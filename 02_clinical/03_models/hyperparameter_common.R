# Added integration, 2026-09-07. Original research scripts are not overwritten.
# Design and IPCW helpers are read from ../unified_pipeline/00_config.R.
# Selection uses group=1 five-fold validation only. No group=2 scoring here.

hp_design <- function(config_path) {
  needed <- c("seed_value", "time_scale_days", "landmark_day", "horizon_day",
              "landmark_u", "horizon_u", "fixed11", "long20", "traj3", "day1_17",
              "static31", "dynamic_fixed28", "factor_vars", "rsf_grid", "rsflc_grid",
              "rsflc_par", "km_censor_function", "ipcw_auc", "ipcw_brier")
  env <- new.env(parent = baseenv())
  # Evaluate only named design assignments/functions, never the config's I/O.
  for (ex in parse(config_path, encoding = "UTF-8")) {
    if (is.call(ex) && is.symbol(ex[[1]]) && as.character(ex[[1]]) %in% c("<-", "=") &&
        is.symbol(ex[[2]]) && as.character(ex[[2]]) %in% needed) eval(ex, env)
  }
  missing <- setdiff(needed, ls(env))
  if (length(missing)) stop("Missing config definitions: ", paste(missing, collapse = ", "))
  stopifnot(length(env$static31) == 31L, length(env$dynamic_fixed28) == 28L,
            length(env$traj3) == 3L)
  env
}

hp_require_columns <- function(x, columns) {
  missing <- setdiff(columns, names(x))
  if (length(missing)) stop("Missing columns: ", paste(missing, collapse = ", "))
}

hp_training_data <- function(base, features, time_name, status_name) {
  hp_require_columns(base, c("subject_id", "hadm_id", "group", "fold", features, time_name, status_name))
  if (anyNA(base$group) || any(!base$group %in% c(1, 2))) stop("group must be 1 or 2")
  if (anyNA(base$subject_id) || anyNA(base$hadm_id) || anyDuplicated(base$hadm_id)) stop("Invalid subject/admission IDs")
  if (any(vapply(split(base$group, base$subject_id), function(z) length(unique(z)) > 1L, logical(1))))
    stop("A subject spans group=1 and group=2")
  d <- base[base$group == 1L, , drop = FALSE]
  if (!nrow(d) || anyNA(d$fold) || !setequal(unique(d$fold), 1:5)) stop("Training data must contain folds 1..5")
  if (any(vapply(split(d$fold, d$subject_id), function(z) length(unique(z)) > 1L, logical(1))))
    stop("A training subject spans folds")
  if (any(!is.finite(d[[time_name]])) || any(d[[time_name]] <= 0) ||
      anyNA(d[[status_name]]) || any(!d[[status_name]] %in% c(0, 1))) stop("Invalid survival outcomes")
  for (fold in 1:5) {
    if (length(unique(d[[status_name]][d$fold == fold])) != 2L ||
        length(unique(d[[status_name]][d$fold != fold])) != 2L) stop("Every fold requires events and non-events")
  }
  d
}

hp_preprocess <- function(train, valid, columns, categorical) {
  rules <- list()
  for (nm in columns) {
    if (nm %in% categorical) {
      z <- as.character(train[[nm]])
      levels <- sort(unique(z[!is.na(z) & nzchar(z)]))
      if (length(levels) < 2L) stop("Training fold has fewer than 2 levels: ", nm)
      fill <- names(sort(table(z[z %in% levels]), decreasing = TRUE))[1]
      convert <- function(x) {
        x <- as.character(x); x[is.na(x) | !x %in% levels] <- fill
        factor(x, levels = levels)
      }
      train[[nm]] <- convert(train[[nm]]); valid[[nm]] <- convert(valid[[nm]])
      rules[[nm]] <- list(type = "factor", levels = levels, fill = fill)
    } else {
      if (!is.numeric(train[[nm]]) || !is.numeric(valid[[nm]])) stop("Expected numeric feature: ", nm)
      good <- is.finite(train[[nm]])
      if (!any(good)) stop("No finite training values: ", nm)
      fill <- stats::median(train[[nm]][good])
      train[[nm]][!good] <- fill
      valid[[nm]][!is.finite(valid[[nm]])] <- fill
      rules[[nm]] <- list(type = "numeric", fill = fill)
    }
  }
  list(train = train, valid = valid, rules = rules)
}

hp_step_risk <- function(times, risk_matrix, horizon) {
  if (!length(times) || any(!is.finite(times)) || is.unsorted(times)) stop("Invalid prediction times")
  if (!is.matrix(risk_matrix) || ncol(risk_matrix) != length(times)) stop("Invalid prediction matrix")
  j <- findInterval(horizon, times)
  risk <- if (j == 0L) rep(0, nrow(risk_matrix)) else risk_matrix[, j]
  if (any(!is.finite(risk)) || any(risk < -1e-8 | risk > 1 + 1e-8)) stop("Non-finite or invalid risks")
  pmin(1, pmax(0, risk))
}

hp_scores <- function(train, valid, risk, time_name, status_name, cfg) {
  if (length(risk) != nrow(valid)) stop("Prediction count does not match validation data")
  time <- valid[[time_name]]; status <- valid[[status_name]]
  G <- cfg$km_censor_function(train[[time_name]], train[[status_name]])
  ans <- c(C_index = survival::concordance(survival::Surv(time, status) ~ risk, reverse = TRUE)$concordance,
           AUC = cfg$ipcw_auc(time, status, risk, cfg$horizon_u, G),
           Brier = cfg$ipcw_brier(time, status, risk, cfg$horizon_u, G))
  if (any(!is.finite(ans))) stop("One or more fold metrics are undefined")
  ans
}

hp_rank <- function(raw, grid) {
  result <- lapply(seq_len(nrow(grid)), function(g) {
    d <- raw[raw$grid_id == g, , drop = FALSE]
    ok <- d$status == "ok" & is.finite(d$C_index) & is.finite(d$AUC) & is.finite(d$Brier)
    complete <- nrow(d) == 5L && !anyDuplicated(d$fold) && setequal(d$fold, 1:5) && all(ok)
    data.frame(grid_id = g, grid[g, , drop = FALSE], successful_folds = sum(ok), eligible = complete,
      C_index_mean = if (complete) mean(d$C_index) else NA_real_,
      C_index_sd = if (complete) stats::sd(d$C_index) else NA_real_,
      AUC_mean = if (complete) mean(d$AUC) else NA_real_,
      AUC_sd = if (complete) stats::sd(d$AUC) else NA_real_,
      Brier_mean = if (complete) mean(d$Brier) else NA_real_,
      Brier_sd = if (complete) stats::sd(d$Brier) else NA_real_)
  })
  tab <- do.call(rbind, result)
  tab[order(!tab$eligible, -tab$C_index_mean, tab$Brier_mean, tab$grid_id, na.last = TRUE), , drop = FALSE]
}

hp_fit <- function(model, train, longitudinal, par, cfg, cores, seed) {
  set.seed(seed)
  if (model == "RSF") {
    Surv <- survival::Surv
    f <- stats::reformulate(cfg$static31, response = "Surv(time_u, status28)")
    return(randomForestSRC::rfsrc(f, data = train[, c("time_u", "status28", cfg$static31)],
      ntree = par$ntree, mtry = par$mtry, nodesize = par$nodesize, nsplit = par$nsplit,
      splitrule = "logrank", na.action = "na.omit", importance = FALSE, seed = -abs(seed)))
  }
  td <- longitudinal[longitudinal$hadm_id %in% train$hadm_id, c("hadm_id", "time_u", cfg$traj3), drop = FALSE]
  names(td)[2] <- "time"
  y <- train[, c("hadm_id", "lm_time_u", "lm_status")]; names(y) <- c("hadm_id", "time", "event")
  tm <- stats::setNames(lapply(cfg$traj3, function(nm) list(
    fixed = stats::reformulate("time", response = nm), random = ~time)), cfg$traj3)
  DynForest::dynforest(timeData = td, fixedData = train[, c("hadm_id", cfg$dynamic_fixed28)],
    idVar = "hadm_id", timeVar = "time", timeVarModel = tm, Y = list(type = "surv", Y = y),
    ntree = par$ntree, mtry = par$mtry, nodesize = par$nodesize,
    minsplit = cfg$rsflc_par$minsplit, cause = 1, nsplit_option = "quantile",
    ncores = cores, seed = seed, verbose = FALSE)
}

hp_predict <- function(model, fit, valid, longitudinal, cfg) {
  if (model == "RSF") {
    # Deliberately do not pass validation outcomes to the model prediction API.
    p <- stats::predict(fit, newdata = valid[, cfg$static31], na.action = "na.omit")
    return(hp_step_risk(p$time.interest, 1 - p$survival, cfg$horizon_u))
  }
  td <- longitudinal[longitudinal$hadm_id %in% valid$hadm_id, c("hadm_id", "time_u", cfg$traj3), drop = FALSE]
  names(td)[2] <- "time"
  p <- stats::predict(fit, timeData = td, fixedData = valid[, c("hadm_id", cfg$dynamic_fixed28)],
                      idVar = "hadm_id", timeVar = "time", t0 = cfg$landmark_u)
  ids <- rownames(p$pred_indiv)
  if (is.null(ids) || anyDuplicated(ids) || !setequal(ids, as.character(valid$hadm_id)))
    stop("DynForest prediction IDs do not match the validation admissions; refusing positional fallback")
  hp_step_risk(p$times, p$pred_indiv[match(as.character(valid$hadm_id), ids), , drop = FALSE], cfg$horizon_u)
}

hp_run <- function(model, cache_dir, output_dir, cfg, cores = 1L, grid = NULL) {
  if (is.null(grid)) grid <- if (model == "RSF") cfg$rsf_grid else cfg$rsflc_grid
  pkg <- if (model == "RSF") "randomForestSRC" else "DynForest"
  for (p in c("survival", pkg)) if (!requireNamespace(p, quietly = TRUE)) stop("Install required package: ", p)
  if (!is.finite(cores) || cores < 1) stop("cores must be a positive integer")
  old_options <- options(rf.cores = as.integer(cores)); on.exit(options(old_options), add = TRUE)
  features <- if (model == "RSF") cfg$static31 else cfg$dynamic_fixed28
  time_name <- if (model == "RSF") "time_u" else "lm_time_u"
  status_name <- if (model == "RSF") "status28" else "lm_status"
  base_path <- file.path(cache_dir, if (model == "RSF") "static_data.rds" else "dynamic_base.rds")
  input_paths <- base_path
  base <- readRDS(base_path)
  train <- hp_training_data(base, features, time_name, status_name)
  if (any(train[[time_name]] > cfg$horizon_u)) stop("Time exceeds configured horizon")
  long <- NULL
  if (model == "RSFLC") {
    input_paths <- c(input_paths, file.path(cache_dir, "dynamic_long.rds"))
    long <- readRDS(input_paths[2])
    hp_require_columns(long, c("hadm_id", "time_u", cfg$traj3))
    long <- long[long$hadm_id %in% train$hadm_id, , drop = FALSE]
    if (any(!is.finite(long$time_u)) || any(long$time_u < 0 | long$time_u > cfg$landmark_u) ||
        any(train[[time_name]] <= cfg$landmark_u)) stop("Invalid landmark cohort or future trajectories")
    if (!setequal(long$hadm_id, train$hadm_id) || anyDuplicated(long[c("hadm_id", "time_u")]))
      stop("Missing or duplicate longitudinal admissions/times")
    if (any(!vapply(long[cfg$traj3], is.numeric, logical(1)))) stop("Trajectories must be numeric")
    for (id in train$hadm_id) for (nm in cfg$traj3) {
      if (sum(is.finite(long[[nm]][long$hadm_id == id])) < 2) stop("Fewer than two observations in a trajectory")
    }
    for (nm in cfg$traj3) long[[nm]][!is.finite(long[[nm]])] <- NA_real_
  }
  if (dir.exists(output_dir) && length(list.files(output_dir, all.files = TRUE, no.. = TRUE)))
    stop("Output directory is not empty; choose a new run directory")
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  write <- function(x, name) utils::write.csv(x, file.path(output_dir, name), row.names = FALSE, fileEncoding = "UTF-8")
  write(data.frame(grid_id = seq_len(nrow(grid)), grid), "parameter_grid.csv")
  raw <- list(); counter <- 0L
  for (g in seq_len(nrow(grid))) for (fold in 1:5) {
    seed <- cfg$seed_value + g * 10L + fold
    row <- data.frame(grid_id = g, fold = fold, grid[g, , drop = FALSE], seed = seed,
                      C_index = NA_real_, AUC = NA_real_, Brier = NA_real_, status = "failed", error = "")
    result <- tryCatch({
      pp <- hp_preprocess(train[train$fold != fold, ], train[train$fold == fold, ], features, cfg$factor_vars)
      fit <- hp_fit(model, pp$train, long, grid[g, , drop = FALSE], cfg, cores, seed)
      risk <- hp_predict(model, fit, pp$valid, long, cfg)
      hp_scores(pp$train, pp$valid, risk, time_name, status_name, cfg)
    }, error = function(e) e)
    if (inherits(result, "error")) row$error <- conditionMessage(result) else {
      row[1, c("C_index", "AUC", "Brier")] <- as.list(result); row$status <- "ok"
    }
    counter <- counter + 1L; raw[[counter]] <- row
    write(do.call(rbind, raw), "cv_folds.csv")
    message(sprintf("%s grid %d/%d fold %d/5: %s", model, g, nrow(grid), fold, row$status))
  }
  ranking <- hp_rank(do.call(rbind, raw), grid); write(ranking, "cv_summary.csv")
  if (!any(ranking$eligible)) stop("No parameter combination completed all five folds; see cv_folds.csv")
  best <- ranking[which(ranking$eligible)[1], , drop = FALSE]
  write(best, "best_parameters.csv")
  pp <- hp_preprocess(train, train[0, , drop = FALSE], features, cfg$factor_vars)
  chosen <- grid[best$grid_id, , drop = FALSE]
  fit <- hp_fit(model, pp$train, long, chosen, cfg, cores, cfg$seed_value)
  saveRDS(list(model = fit, model_name = model, selected_parameters = chosen,
    preprocessing = pp$rules, features = features, trajectories = if (model == "RSFLC") cfg$traj3 else NULL,
    time_scale_days = cfg$time_scale_days, horizon_u = cfg$horizon_u,
    landmark_u = if (model == "RSFLC") cfg$landmark_u else NULL,
    training_group = 1L, seed = cfg$seed_value,
    selection_rule = "five complete folds; mean C-index descending, mean Brier ascending, grid_id ascending"),
    file.path(output_dir, "best_model_bundle.rds"))
  write(data.frame(path = normalizePath(input_paths, winslash = "/"), md5 = unname(tools::md5sum(input_paths))), "input_manifest.csv")
  write(data.frame(model = model, training_admissions = nrow(train), training_subjects = length(unique(train$subject_id)),
    heldout_used_for_selection = FALSE, combinations = nrow(grid), eligible_combinations = sum(ranking$eligible)), "run_summary.csv")
  capture.output(utils::sessionInfo(), file = file.path(output_dir, "sessionInfo.txt"))
  message("Saved selected parameters and refitted training model to: ", output_dir)
  invisible(best)
}

hp_cli <- function(model, model_dir, args = commandArgs(trailingOnly = TRUE)) {
  if ("--help" %in% args) {
    cat("Rscript 01_tune_and_refit.R --cache-dir=PATH --output-dir=NEW_PATH [--cores=1]\n",
        "Rscript 01_tune_and_refit.R --dry-run\n", sep = "")
    return(invisible(NULL))
  }
  if (any(!grepl("^--(cache-dir=.+|output-dir=.+|cores=[0-9]+|dry-run)$", args))) stop("Unknown argument; use --help")
  cfg <- hp_design(file.path(model_dir, "..", "unified_pipeline", "00_config.R"))
  grid <- if (model == "RSF") cfg$rsf_grid else cfg$rsflc_grid
  if ("--dry-run" %in% args) {
    cat(model, ": ", nrow(grid), " combinations x 5 folds + 1 final refit; no data read or model fitted\n", sep = "")
    return(invisible(grid))
  }
  arg <- function(name, default = NULL) {
    values <- args[startsWith(args, paste0("--", name, "="))]
    if (length(values) > 1L) stop("Duplicate argument: ", name)
    if (!length(values)) return(default)
    substring(values, nchar(name) + 4L)
  }
  cache <- arg("cache-dir"); out <- arg("output-dir")
  if (is.null(cache) || is.null(out)) stop("Provide --cache-dir and --output-dir; use --help")
  hp_run(model, cache, out, cfg, cores = as.integer(arg("cores", "1")))
}
