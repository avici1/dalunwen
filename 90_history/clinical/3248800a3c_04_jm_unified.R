suppressPackageStartupMessages({
  library(dplyr)
  library(survival)
  library(nlme)
  library(JMbayes2)
  library(coda)
  library(ggplot2)
  library(tidyr)
})

script_dir <- Sys.getenv("CH5_SCRIPT_DIR", unset = "work/chapter5_0831")
source(file.path(script_dir, "00_config.R"), encoding = "UTF-8")

base <- readRDS(file.path(out_dir, "models", "dynamic_base.rds"))
long <- readRDS(file.path(out_dir, "models", "dynamic_long.rds"))
train_base <- base %>% filter(group == 1L)
valid_base <- base %>% filter(group == 2L)
train_long <- long %>% filter(hadm_id %in% train_base$hadm_id) %>%
  inner_join(train_base %>% select(hadm_id, all_of(dynamic_fixed25)), by = "hadm_id")
valid_long <- long %>% filter(hadm_id %in% valid_base$hadm_id) %>%
  inner_join(valid_base %>% select(hadm_id, all_of(dynamic_fixed25)), by = "hadm_id")

for (nm in intersect(factor_vars, dynamic_fixed25)) {
  lev <- levels(factor(train_base[[nm]]))
  train_base[[nm]] <- factor(train_base[[nm]], levels = lev)
  valid_base[[nm]] <- factor(valid_base[[nm]], levels = lev)
  train_long[[nm]] <- factor(train_long[[nm]], levels = lev)
  valid_long[[nm]] <- factor(valid_long[[nm]], levels = lev)
}

model_path <- file.path(out_dir, "models", "jm_3trajectory_25fixed.rds")
if (file.exists(model_path)) {
  jm_fit <- readRDS(model_path)
} else {
  ctl <- nlme::lmeControl(opt = "optim", msMaxIter = 200)
  fm_gcs <- nlme::lme(gcs ~ time, data = train_long,
                      random = ~ time | hadm_id, control = ctl)
  fm_sofa <- nlme::lme(sofa_24hours ~ time, data = train_long,
                       random = ~ time | hadm_id, control = ctl)
  fm_cns <- nlme::lme(cns_24hours ~ time, data = train_long,
                      random = ~ time | hadm_id, control = ctl)
  cox_formula <- as.formula(paste(
    "Surv(lm_time, lm_status) ~", paste(dynamic_fixed25, collapse = " + ")
  ))
  cox_fit <- survival::coxph(cox_formula, data = train_base, x = TRUE, model = TRUE)
  jm_fit <- JMbayes2::jm(
    Surv_object = cox_fit,
    Mixed_objects = list(fm_gcs, fm_sofa, fm_cns),
    time_var = "time", id_var = "hadm_id",
    n_chains = jm_par$n_chains,
    n_iter = jm_par$n_iter,
    n_burnin = jm_par$n_burnin,
    cores = jm_par$n_chains,
    seed = seed_value
  )
  saveRDS(jm_fit, model_path)
}

parse_prediction <- function(pred, ids, eval_times) {
  pdf <- if (is.data.frame(pred)) {
    pred
  } else if (is.list(pred) && all(c("pred", "times", "id") %in% names(pred))) {
    data.frame(pred = as.numeric(pred$pred), times = as.numeric(pred$times),
               id = as.integer(pred$id))
  } else if (is.list(pred) && is.data.frame(pred[[1]])) {
    pred[[1]]
  } else {
    as.data.frame(pred)
  }
  nms <- names(pdf)
  id_col <- nms[tolower(nms) %in% c("id", "hadm_id")][1]
  time_col <- nms[tolower(nms) %in% c("times", "time")][1]
  risk_col <- nms[tolower(nms) %in% c("pred", "preds", "pred_mean")][1]
  if (is.na(id_col) || is.na(risk_col)) stop("Cannot parse JM prediction columns: ", paste(nms, collapse = ", "))
  pdf$hadm_id <- as.integer(as.character(pdf[[id_col]]))
  pdf$risk <- as.numeric(pdf[[risk_col]])
  if (is.na(time_col)) {
    if (length(eval_times) != 1L) stop("JM prediction omitted time column for multiple evaluation times")
    pdf$eval_time <- eval_times
  } else {
    pdf$eval_time <- as.numeric(pdf[[time_col]])
  }
  mat <- matrix(NA_real_, nrow = length(ids), ncol = length(eval_times),
                dimnames = list(as.character(ids), as.character(eval_times)))
  for (i in seq_along(ids)) {
    z <- pdf[pdf$hadm_id == ids[i], , drop = FALSE]
    if (!nrow(z)) next
    for (j in seq_along(eval_times)) {
      k <- which.min(abs(z$eval_time - eval_times[j]))
      mat[i, j] <- z$risk[k]
    }
  }
  mat
}

predict_jm_matrix <- function(fit, history, ids, eval_times) {
  chunks <- split(ids, ceiling(seq_along(ids) / 50L))
  predict_chunk <- function(ids_k, seed_offset) {
    nd <- history %>% filter(hadm_id %in% ids_k, time <= landmark) %>%
      mutate(lm_time = landmark, lm_status = 0L) %>% arrange(hadm_id, time)
    pred <- try(predict(
      fit, newdata = nd, process = "event", times = eval_times,
      type = "subject_specific",
      control = list(n_samples = 50L, n_mcmc = 30L, parallel = "snow",
                     cores = 1L, seed = seed_value + seed_offset)
    ), silent = TRUE)
    if (!inherits(pred, "try-error")) return(parse_prediction(pred, ids_k, eval_times))
    if (length(ids_k) > 1L) {
      cut <- ceiling(length(ids_k) / 2)
      message("  retrying failed JM batch as ", cut, "+", length(ids_k) - cut)
      return(rbind(
        predict_chunk(ids_k[seq_len(cut)], seed_offset + 1000L),
        predict_chunk(ids_k[-seq_len(cut)], seed_offset + 2000L)
      ))
    }
    warning("JM prediction remained undefined for hadm_id=", ids_k)
    matrix(NA_real_, nrow = 1L, ncol = length(eval_times),
           dimnames = list(as.character(ids_k), as.character(eval_times)))
  }
  pieces <- lapply(seq_along(chunks), function(k) {
    ids_k <- chunks[[k]]
    message("JM prediction batch ", k, "/", length(chunks), " (n=", length(ids_k), ")")
    predict_chunk(ids_k, k)
  })
  do.call(rbind, pieces)
}

train_risk <- predict_jm_matrix(jm_fit, train_long, train_base$hadm_id, dynamic_times)
valid_risk <- predict_jm_matrix(jm_fit, valid_long, valid_base$hadm_id, dynamic_times)
bad_train <- train_base$hadm_id[!apply(is.finite(train_risk), 1, all)]
bad_valid <- valid_base$hadm_id[!apply(is.finite(valid_risk), 1, all)]
write_utf8(data.frame(hadm_id = c(bad_train, bad_valid),
                      split = c(rep("train", length(bad_train)), rep("validation", length(bad_valid)))),
           file.path(out_dir, "tables", "JM_undefined_prediction_ids.csv"))
if (length(bad_train)) {
  keep <- !train_base$hadm_id %in% bad_train
  train_base <- train_base[keep, , drop = FALSE]
  train_risk <- train_risk[keep, , drop = FALSE]
}
if (length(bad_valid)) {
  keep <- !valid_base$hadm_id %in% bad_valid
  valid_base <- valid_base[keep, , drop = FALSE]
  valid_risk <- valid_risk[keep, , drop = FALSE]
}

Gfun <- km_censor_function(train_base$lm_time, train_base$lm_status)
metrics_train <- metric_bundle(train_base$lm_time, train_base$lm_status, train_risk, dynamic_times, Gfun)
metrics_valid <- metric_bundle(valid_base$lm_time, valid_base$lm_status, valid_risk, dynamic_times, Gfun)
ci <- bootstrap_metrics(valid_base$lm_time, valid_base$lm_status, valid_risk,
                        dynamic_times, Gfun, B = bootstrap_repeats)

metric_row <- function(m, split) {
  data.frame(model = "JM", split = split,
             C_index = m$cindex, AUC_28 = tail(m$auc, 1),
             Brier_28 = tail(m$brier, 1), IBS_5_28 = m$ibs)
}
write_utf8(bind_rows(metric_row(metrics_train, "train"), metric_row(metrics_valid, "validation")),
           file.path(out_dir, "tables", "JM_dynamic_metrics.csv"))
write_utf8(ci, file.path(out_dir, "tables", "JM_dynamic_metrics_bootstrap95.csv"))
write_utf8(data.frame(time = dynamic_times, AUC = metrics_valid$auc,
                      Brier = metrics_valid$brier, model = "JM"),
           file.path(out_dir, "tables", "JM_time_metrics.csv"))

alpha_chains <- jm_fit$mcmc$alphas
alpha_mat <- do.call(rbind, lapply(alpha_chains, as.matrix))
alpha_mean <- colMeans(alpha_mat)
alpha_ci <- apply(alpha_mat, 2, quantile, probs = c(0.025, 0.975))
rhat <- gelman.diag(alpha_chains, autoburnin = FALSE, multivariate = FALSE)$psrf[, 1]
ess <- effectiveSize(alpha_chains)
assoc <- data.frame(
  trajectory = names(alpha_mean), alpha = as.numeric(alpha_mean),
  HR_per_unit = exp(as.numeric(alpha_mean)),
  lower95 = exp(alpha_ci[1, ]), upper95 = exp(alpha_ci[2, ]),
  Rhat = as.numeric(rhat[names(alpha_mean)]), ESS = as.numeric(ess[names(alpha_mean)])
)
write_utf8(assoc, file.path(out_dir, "tables", "table_5_6_JM_association.csv"))

valid_outcome <- as.integer(valid_base$lm_status == 1L & valid_base$lm_time <= horizon)
cal <- calibration_table(valid_risk[, ncol(valid_risk)], valid_outcome)
dca <- dca_table(valid_risk[, ncol(valid_risk)], valid_outcome)
write_utf8(cal, file.path(out_dir, "tables", "JM_calibration_28.csv"))
write_utf8(dca, file.path(out_dir, "tables", "JM_DCA_28.csv"))

p_cal <- ggplot(cal, aes(predicted, observed)) +
  geom_abline(slope = 1, intercept = 0, linetype = 2, colour = "grey45") +
  geom_point(size = 2.3, colour = "#0072B2") + geom_line(colour = "#0072B2") +
  coord_equal(xlim = c(0, 1), ylim = c(0, 1)) + theme_bw() +
  labs(x = "Predicted 28-day mortality", y = "Observed 28-day mortality",
       title = "JM validation calibration at landmark day 5")
ggsave(file.path(out_dir, "figures", "JM_01_calibration.png"), p_cal, width = 6.4, height = 5.4, dpi = 300)

dca_long <- dca %>% pivot_longer(c(model, treat_all, treat_none), names_to = "strategy", values_to = "net_benefit")
p_dca <- ggplot(dca_long, aes(threshold, net_benefit, colour = strategy)) +
  geom_line(linewidth = 0.85) + theme_bw() +
  scale_colour_manual(values = c(model = "#0072B2", treat_all = "#D55E00", treat_none = "grey40")) +
  labs(x = "Threshold probability", y = "Net benefit", colour = NULL,
       title = "JM decision curve at landmark day 5")
ggsave(file.path(out_dir, "figures", "JM_02_DCA.png"), p_dca, width = 7, height = 5.2, dpi = 300)

trace_df <- bind_rows(lapply(seq_along(alpha_chains), function(ch) {
  z <- as.data.frame(as.matrix(alpha_chains[[ch]]))
  z$iteration <- seq_len(nrow(z)); z$chain <- factor(ch)
  pivot_longer(z, -c(iteration, chain), names_to = "parameter", values_to = "value")
}))
p_trace <- ggplot(trace_df, aes(iteration, value, colour = chain)) +
  geom_line(linewidth = 0.25, alpha = 0.75) + facet_wrap(~parameter, scales = "free_y", ncol = 1) +
  theme_bw() + labs(title = "JM association-parameter trace plots", colour = "Chain")
ggsave(file.path(out_dir, "figures", "JM_03_MCMC_trace.png"), p_trace, width = 8, height = 8.5, dpi = 300)

p_density <- ggplot(trace_df, aes(value, colour = chain, fill = chain)) +
  geom_density(alpha = 0.12) + facet_wrap(~parameter, scales = "free", ncol = 1) +
  theme_bw() + labs(title = "JM association-parameter posterior densities", colour = "Chain", fill = "Chain")
ggsave(file.path(out_dir, "figures", "JM_04_MCMC_density.png"), p_density, width = 8, height = 8.5, dpi = 300)

cat("JM_OK\n")
