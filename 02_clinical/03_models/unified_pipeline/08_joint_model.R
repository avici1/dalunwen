suppressPackageStartupMessages({
  library(dplyr)
  library(survival)
  library(nlme)
  library(JMbayes2)
  library(coda)
  library(ggplot2)
  library(tidyr)
  library(patchwork)
})
script_dir <- Sys.getenv("CH5_SCRIPT_DIR", unset = getwd())
source(file.path(script_dir, "00_config.R"), encoding = "UTF-8")
log_progress("JM_START", "3 trajectories + 28 fixed; Y~time_u; random intercept")

base <- readRDS(file.path(cache_dir, "dynamic_base.rds"))
long <- readRDS(file.path(cache_dir, "dynamic_long.rds"))
train_base <- base %>% filter(group == 1L)
valid_base <- base %>% filter(group == 2L)
train_long <- long %>% filter(hadm_id %in% train_base$hadm_id) %>%
  inner_join(train_base %>% select(hadm_id, all_of(dynamic_fixed28)), by = "hadm_id")
valid_long <- long %>% filter(hadm_id %in% valid_base$hadm_id) %>%
  inner_join(valid_base %>% select(hadm_id, all_of(dynamic_fixed28)), by = "hadm_id")

for (nm in intersect(factor_vars, dynamic_fixed28)) {
  lev <- levels(factor(train_base[[nm]]))
  train_base[[nm]] <- factor(train_base[[nm]], levels = lev)
  valid_base[[nm]] <- factor(valid_base[[nm]], levels = lev)
  train_long[[nm]] <- factor(train_long[[nm]], levels = lev)
  valid_long[[nm]] <- factor(valid_long[[nm]], levels = lev)
}

ctl <- nlme::lmeControl(opt = "optim", msMaxIter = 200L, returnObject = TRUE)
mixed_path <- file.path(cache_dir, "jm_mixed_models_random_intercept.rds")
surv_path <- file.path(cache_dir, "jm_survival_submodel_28fixed.rds")
if (file.exists(mixed_path) && file.exists(surv_path)) {
  mixed_models <- readRDS(mixed_path)
  cox_fit <- readRDS(surv_path)
} else {
  mixed_models <- list(
    nlme::lme(gcs ~ time_u, data = train_long, random = ~ 1 | hadm_id, control = ctl),
    nlme::lme(sofa_24hours ~ time_u, data = train_long, random = ~ 1 | hadm_id, control = ctl),
    nlme::lme(cns_24hours ~ time_u, data = train_long, random = ~ 1 | hadm_id, control = ctl)
  )
  names(mixed_models) <- traj3
  cox_formula <- as.formula(paste("Surv(lm_time_u, lm_status) ~", paste(dynamic_fixed28, collapse = " + ")))
  cox_fit <- survival::coxph(cox_formula, data = train_base, x = TRUE, model = TRUE)
  saveRDS(mixed_models, mixed_path)
  saveRDS(cox_fit, surv_path)
}

fit_joint <- function(n_iter, n_burnin, path) {
  log_progress("JM_MCMC", sprintf("chains=%d; iter=%d; burn=%d", jm_par$n_chains, n_iter, n_burnin))
  fit <- JMbayes2::jm(
    Surv_object = cox_fit, Mixed_objects = mixed_models,
    time_var = "time_u", id_var = "hadm_id",
    n_chains = jm_par$n_chains, n_iter = n_iter, n_burnin = n_burnin,
    cores = jm_par$n_chains, seed = seed_value
  )
  saveRDS(fit, path)
  fit
}

primary_path <- file.path(cache_dir, "jm_3trajectory_28fixed_random_intercept_3000.rds")
extended_path <- file.path(cache_dir, "jm_3trajectory_28fixed_random_intercept_6000.rds")
final_path <- file.path(cache_dir, "jm_3trajectory_28fixed_random_intercept_12000.rds")
if (file.exists(final_path)) {
  jm_fit <- readRDS(final_path)
} else if (file.exists(extended_path)) {
  jm_fit <- readRDS(extended_path)
} else if (file.exists(primary_path)) {
  jm_fit <- readRDS(primary_path)
} else {
  jm_fit <- fit_joint(jm_par$n_iter, jm_par$n_burnin, primary_path)
}

association_diagnostics <- function(fit) {
  chains <- fit$mcmc$alphas
  mat <- do.call(rbind, lapply(chains, as.matrix))
  means <- colMeans(mat)
  ci <- apply(mat, 2, quantile, probs = c(0.025, 0.975))
  rhat <- gelman.diag(chains, autoburnin = FALSE, multivariate = FALSE)$psrf[, 1]
  ess <- effectiveSize(chains)
  list(chains = chains, mat = mat, table = data.frame(
    trajectory = names(means), alpha = as.numeric(means),
    HR_per_unit = exp(as.numeric(means)),
    lower95 = exp(ci[1, ]), upper95 = exp(ci[2, ]),
    Rhat = as.numeric(rhat[names(means)]), ESS = as.numeric(ess[names(means)]),
    row.names = NULL
  ))
}

diag_obj <- association_diagnostics(jm_fit)
auto_extend <- tolower(Sys.getenv("CH5_JM_AUTO_EXTEND", unset = "true")) %in% c("true", "1", "yes")
if (auto_extend && any(diag_obj$table$Rhat > 1.05, na.rm = TRUE) && !file.exists(extended_path)) {
  log_progress("JM_EXTEND", sprintf("max Rhat=%.3f; refit to 6000 iterations", max(diag_obj$table$Rhat, na.rm = TRUE)))
  jm_fit <- fit_joint(6000L, 3000L, extended_path)
  diag_obj <- association_diagnostics(jm_fit)
}
if (auto_extend && any(diag_obj$table$Rhat > 1.05, na.rm = TRUE) && !file.exists(final_path)) {
  log_progress("JM_EXTEND", sprintf("max Rhat=%.3f after 6000; refit to 12000 iterations",
                                    max(diag_obj$table$Rhat, na.rm = TRUE)))
  jm_fit <- fit_joint(12000L, 6000L, final_path)
  diag_obj <- association_diagnostics(jm_fit)
}
write_utf8(diag_obj$table, file.path(artifact_dir, "\u88685-6_JM\u5f53\u524d\u503c\u5173\u8054\u53c2\u6570.csv"))

parse_prediction <- function(pred, ids, eval_times) {
  pdf <- if (is.data.frame(pred)) pred else if (is.list(pred) && all(c("pred", "times", "id") %in% names(pred))) {
    data.frame(pred = as.numeric(pred$pred), times = as.numeric(pred$times), id = as.integer(pred$id))
  } else if (is.list(pred) && length(pred) && is.data.frame(pred[[1L]])) pred[[1L]] else as.data.frame(pred)
  nms <- names(pdf)
  id_col <- nms[tolower(nms) %in% c("id", "hadm_id")][1L]
  time_col <- nms[tolower(nms) %in% c("times", "time")][1L]
  risk_col <- nms[tolower(nms) %in% c("pred", "preds", "pred_mean")][1L]
  if (is.na(id_col) || is.na(risk_col)) stop("Cannot parse JM prediction columns: ", paste(nms, collapse = ", "))
  pdf$hadm_id <- as.integer(as.character(pdf[[id_col]]))
  pdf$risk <- as.numeric(pdf[[risk_col]])
  pdf$eval_time <- if (is.na(time_col)) rep(eval_times[1L], nrow(pdf)) else as.numeric(pdf[[time_col]])
  mat <- matrix(NA_real_, length(ids), length(eval_times), dimnames = list(as.character(ids), as.character(eval_times)))
  for (i in seq_along(ids)) {
    z <- pdf[pdf$hadm_id == ids[i], , drop = FALSE]
    if (!nrow(z)) next
    for (j in seq_along(eval_times)) mat[i, j] <- z$risk[which.min(abs(z$eval_time - eval_times[j]))]
  }
  mat
}

predict_jm_matrix <- function(fit, history, ids, eval_times) {
  chunks <- split(ids, ceiling(seq_along(ids) / 50L))
  predict_chunk <- function(ids_k, seed_offset) {
    nd <- history %>% filter(hadm_id %in% ids_k, time_u <= landmark_u) %>%
      mutate(lm_time_u = landmark_u, lm_status = 0L) %>% arrange(hadm_id, time_u)
    pred <- try(predict(
      fit, newdata = nd, process = "event", times = eval_times,
      type = "subject_specific",
      control = list(n_samples = 50L, n_mcmc = 30L, parallel = "snow",
                     cores = 1L, seed = seed_value + seed_offset)
    ), silent = TRUE)
    if (!inherits(pred, "try-error")) return(parse_prediction(pred, ids_k, eval_times))
    if (length(ids_k) > 1L) {
      cut <- ceiling(length(ids_k) / 2L)
      return(rbind(
        predict_chunk(ids_k[seq_len(cut)], seed_offset + 1000L),
        predict_chunk(ids_k[-seq_len(cut)], seed_offset + 2000L)
      ))
    }
    warning("JM prediction undefined for hadm_id=", ids_k)
    matrix(NA_real_, 1L, length(eval_times), dimnames = list(as.character(ids_k), as.character(eval_times)))
  }
  pieces <- lapply(seq_along(chunks), function(k) {
    log_progress("JM_PREDICT", sprintf("batch %d/%d; n=%d", k, length(chunks), length(chunks[[k]])))
    predict_chunk(chunks[[k]], k)
  })
  do.call(rbind, pieces)
}

prediction_path <- file.path(
  cache_dir,
  if (file.exists(final_path)) "jm_predictions_28fixed_12000.rds" else "jm_predictions_28fixed.rds"
)
if (file.exists(prediction_path)) {
  pp <- readRDS(prediction_path); train_risk <- pp$train; valid_risk <- pp$validation
} else {
  train_risk <- predict_jm_matrix(jm_fit, train_long, train_base$hadm_id, dynamic_eval_u)
  valid_risk <- predict_jm_matrix(jm_fit, valid_long, valid_base$hadm_id, dynamic_eval_u)
  saveRDS(list(train = train_risk, validation = valid_risk), prediction_path)
}

bad_train <- train_base$hadm_id[!apply(is.finite(train_risk), 1L, all)]
bad_valid <- valid_base$hadm_id[!apply(is.finite(valid_risk), 1L, all)]
write_utf8(data.frame(
  hadm_id = c(bad_train, bad_valid),
  split = c(rep("train", length(bad_train)), rep("validation", length(bad_valid)))
), file.path(artifact_dir, "JM_\u65e0\u6cd5\u751f\u6210\u9884\u6d4b\u7684\u4f4f\u9662ID.csv"))

keep_train <- !train_base$hadm_id %in% bad_train
keep_valid <- !valid_base$hadm_id %in% bad_valid
train_eval <- train_base[keep_train, , drop = FALSE]; train_risk_eval <- train_risk[keep_train, , drop = FALSE]
valid_eval <- valid_base[keep_valid, , drop = FALSE]; valid_risk_eval <- valid_risk[keep_valid, , drop = FALSE]
Gfun <- km_censor_function(train_eval$lm_time_u, train_eval$lm_status)
metrics_train <- metric_bundle(train_eval$lm_time_u, train_eval$lm_status, train_risk_eval, dynamic_eval_u, Gfun)
metrics_valid <- metric_bundle(valid_eval$lm_time_u, valid_eval$lm_status, valid_risk_eval, dynamic_eval_u, Gfun)

metric_rows <- bind_rows(
  data.frame(model = "JM", split = "train", C_index = metrics_train$cindex,
             AUC_u1_given_uL = tail(metrics_train$auc, 1L), Brier_u1_given_uL = tail(metrics_train$brier, 1L), IBS_uL_1 = metrics_train$ibs),
  data.frame(model = "JM", split = "validation", C_index = metrics_valid$cindex,
             AUC_u1_given_uL = tail(metrics_valid$auc, 1L), Brier_u1_given_uL = tail(metrics_valid$brier, 1L), IBS_uL_1 = metrics_valid$ibs)
)
write_utf8(metric_rows, file.path(artifact_dir, "JM_\u52a8\u6001\u6027\u80fd.csv"))
write_utf8(transform(
  bootstrap_metrics(valid_eval$lm_time_u, valid_eval$lm_status, valid_risk_eval,
                    dynamic_eval_u, Gfun, cluster_id = valid_eval$subject_id), model = "JM"),
  file.path(artifact_dir, "JM_95CI_\u60a3\u8005\u7c07bootstrap.csv")
)
write_utf8(data.frame(model = "JM", u = dynamic_eval_u, day = dynamic_eval_u * 28,
                      AUC = metrics_valid$auc, Brier = metrics_valid$brier),
           file.path(artifact_dir, "JM_\u65f6\u95f4\u4f9d\u8d56\u6027\u80fd.csv"))

saveRDS(list(train_base = train_eval, validation_base = valid_eval,
             train_risk = train_risk_eval, validation_risk = valid_risk_eval,
             eval_u = dynamic_eval_u, G_train = Gfun), file.path(cache_dir, "jm_evaluation_data.rds"))

trace_df <- bind_rows(lapply(seq_along(diag_obj$chains), function(ch) {
  z <- as.data.frame(as.matrix(diag_obj$chains[[ch]]))
  z$iteration <- seq_len(nrow(z)); z$chain <- factor(ch)
  pivot_longer(z, -c(iteration, chain), names_to = "parameter", values_to = "value")
}))
theme_set(theme_bw(base_size = 10.5, base_family = "Microsoft YaHei"))
p_trace <- ggplot(trace_df, aes(iteration, value, colour = chain)) +
  geom_line(linewidth = 0.25, alpha = 0.75) + facet_wrap(~parameter, scales = "free_y", ncol = 1L) +
  labs(title = "A  MCMC\u8f68\u8ff9", colour = "\u94fe")
p_density <- ggplot(trace_df, aes(value, colour = chain, fill = chain)) +
  geom_density(alpha = 0.12) + facet_wrap(~parameter, scales = "free", ncol = 1L) +
  labs(title = "B  \u540e\u9a8c\u5bc6\u5ea6", colour = "\u94fe", fill = "\u94fe")
ggsave(file.path(artifact_dir, "\u56fe5-9_JM\u5173\u8054\u53c2\u6570MCMC\u8f68\u8ff9\u4e0e\u540e\u9a8c\u5bc6\u5ea6.png"),
       p_trace | p_density, width = 11.0, height = 8.5, dpi = 320, bg = "white")

log_progress("JM_OK", sprintf("validation_C=%.4f; max_Rhat=%.3f; undefined=%d",
                              metrics_valid$cindex, max(diag_obj$table$Rhat, na.rm = TRUE), length(bad_valid)))
cat("JM_OK\n")
