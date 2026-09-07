safe_random_effect <- function(model, formula, data, ridge = 1e-8) {
  data_na <- stats::na.omit(data)
  beta <- model$beta
  B <- matrix(0, ncol = sum(model$idea0), nrow = sum(model$idea0))
  B[upper.tri(B, diag = TRUE)] <- model$varcov
  B <- t(B)
  B[upper.tri(B, diag = TRUE)] <- model$varcov
  se <- model$stderr^2
  Z <- stats::model.matrix(formula$random, data_na)
  X <- stats::model.matrix(formula$fixed, data_na)
  Y <- stats::model.matrix(
    stats::reformulate(as.character(formula$fixed)[2], intercept = FALSE), data_na
  )
  if (!nrow(data_na)) return(rep(NA_real_, ncol(B)))
  V <- Z %*% B %*% t(Z) + se * diag(nrow(Z))
  resid <- Y - X %*% beta
  ans <- tryCatch(
    B %*% t(Z) %*% solve(V + diag(ridge, nrow(V))) %*% resid,
    error = function(e) B %*% t(Z) %*% MASS::ginv(V) %*% resid
  )
  as.numeric(ans)
}

route_tree_safe <- function(tree, time_one, fixed_one, model, time_var = "time") {
  var_fact <- vapply(fixed_one[, setdiff(names(fixed_one), "hadm_id"), drop = FALSE],
                     function(x) inherits(x, c("character", "factor")), logical(1))
  var_num <- vapply(fixed_one[, setdiff(names(fixed_one), "hadm_id"), drop = FALSE],
                    function(x) inherits(x, c("numeric", "integer")), logical(1))
  current_node <- 1
  max_steps <- nrow(tree$V_split) + 2L
  steps <- 0L
  while (!is.element(current_node, tree$leaves)) {
    steps <- steps + 1L
    if (steps > max_steps || is.na(current_node)) return(NA_character_)
    hit <- which(as.numeric(as.character(tree$V_split[, 2])) == current_node)
    if (!length(hit)) return(NA_character_)
    hit <- hit[1]
    input_type <- as.character(tree$V_split[hit, 1])
    type <- tolower(input_type)
    var_split <- as.integer(as.character(tree$V_split[hit, 3]))
    var_summary <- as.integer(as.character(tree$V_split[hit, 4]))
    threshold <- as.numeric(as.character(tree$V_split[hit, 5]))
    mean_left <- tree$hist_nodes[[as.character(2 * current_node)]]
    mean_right <- tree$hist_nodes[[as.character(2 * current_node + 1)]]

    if (type == "longitudinal") {
      fml <- model$Longitudinal.model[[var_split]]
      model_var <- unique(c(all.vars(fml$fixed), all.vars(fml$random)))
      data_model <- data.frame(id = as.numeric(time_one$hadm_id), time_one, check.names = FALSE)
      names(data_model)[names(data_model) == "time"] <- time_var
      data_model <- data_model[, c("id", model_var), drop = FALSE]
      re <- safe_random_effect(
        tree$model_param[[as.character(current_node)]][[1]], fml, data_model
      )
      if (var_summary > length(re) || !is.finite(re[var_summary])) return(NA_character_)
      go_left <- re[var_summary] < threshold
    } else if (type == "numeric") {
      nm <- names(var_num)[var_num][var_split]
      value <- as.numeric(fixed_one[[nm]][1])
      if (!is.finite(value)) return(NA_character_)
      go_left <- value < threshold
    } else if (type == "factor") {
      nm <- names(var_fact)[var_fact][var_split]
      value <- fixed_one[[nm]][1]
      dist_left <- -1 * is.element(value, mean_left)
      dist_right <- -1 * is.element(value, mean_right)
      go_left <- dist_left <= dist_right
    } else {
      return(NA_character_)
    }
    current_node <- if (isTRUE(go_left)) 2 * current_node else 2 * current_node + 1
  }
  as.character(current_node)
}

predict_subject_safe <- function(id, model, time_data, fixed_data, grid, landmark = 5) {
  time_one <- time_data[time_data$hadm_id == id & time_data$time <= landmark + 1e-8, , drop = FALSE]
  fixed_one <- fixed_data[fixed_data$hadm_id == id, , drop = FALSE]
  all_times <- model$times
  idx0 <- max(which(all_times <= landmark + 1e-8))
  idx_grid <- vapply(grid, function(tt) max(which(all_times <= tt + 1e-8)), integer(1))
  tree_curves <- matrix(NA_real_, nrow = ncol(model$rf), ncol = length(grid))
  for (tt in seq_len(ncol(model$rf))) {
    tree <- model$rf[, tt]
    leaf <- route_tree_safe(tree, time_one, fixed_one, model, "time")
    if (is.na(leaf)) next
    pred <- tree$Y_pred[[leaf]][[as.character(model$cause)]]
    if (is.null(pred)) next
    traj <- pred$traj
    p0 <- traj[idx0]
    surv0 <- 1 - p0
    if (!is.finite(surv0) || surv0 <= 0) next
    tree_curves[tt, ] <- (traj[idx_grid] - p0) / surv0
  }
  curve <- colMeans(tree_curves, na.rm = TRUE)
  if (any(!is.finite(curve))) stop("患者 ", id, " 没有可用安全路由树。")
  pmin(pmax(cummax(curve), 0), 1)
}

predict_rsflc_safe <- function(model, time_data, fixed_data, ids, grid, landmark = 5,
                               ncores = 1L) {
  worker <- function(id) {
    predict_subject_safe(id, model, time_data, fixed_data, grid, landmark)
  }
  if (ncores <= 1L) {
    curves <- lapply(ids, worker)
  } else {
    cl <- parallel::makeCluster(ncores)
    on.exit(parallel::stopCluster(cl), add = TRUE)
    parallel::clusterEvalQ(cl, library(MASS))
    parallel::clusterExport(
      cl,
      c("model", "time_data", "fixed_data", "grid", "landmark",
        "safe_random_effect", "route_tree_safe", "predict_subject_safe"),
      envir = environment()
    )
    curves <- parallel::parLapply(cl, ids, worker)
  }
  out <- do.call(rbind, curves)
  rownames(out) <- as.character(ids)
  colnames(out) <- format(grid, trim = TRUE, scientific = FALSE)
  out
}
