suppressPackageStartupMessages({
  library(dplyr)
  library(survival)
  library(DynForest)
  library(ggplot2)
  library(fastshap)
  library(shapviz)
  library(patchwork)
})
script_dir <- Sys.getenv("CH5_SCRIPT_DIR", unset = "work/chapter5_0831")
source(file.path(script_dir, "00_config.R"), encoding = "UTF-8")

base <- readRDS(file.path(out_dir, "models", "dynamic_base.rds"))
long <- readRDS(file.path(out_dir, "models", "dynamic_long.rds"))
jm_exclusion_path <- file.path(out_dir, "tables", "JM_undefined_prediction_ids.csv")
if (file.exists(jm_exclusion_path)) {
  jm_bad <- read.csv(jm_exclusion_path)$hadm_id
  base <- base %>% filter(!hadm_id %in% jm_bad)
  long <- long %>% filter(!hadm_id %in% jm_bad)
}
train_b <- base %>% filter(group == 1L)
valid_b <- base %>% filter(group == 2L)
train_l <- long %>% filter(hadm_id %in% train_b$hadm_id)
valid_l <- long %>% filter(hadm_id %in% valid_b$hadm_id)

for (nm in intersect(factor_vars, names(train_b))) {
  train_b[[nm]] <- factor(train_b[[nm]])
  valid_b[[nm]] <- factor(valid_b[[nm]], levels = levels(train_b[[nm]]))
}

fixed_train <- train_b %>% select(hadm_id, all_of(dynamic_fixed25)) %>% as.data.frame()
fixed_valid <- valid_b %>% select(hadm_id, all_of(dynamic_fixed25)) %>% as.data.frame()
time_train <- train_l %>% select(hadm_id, time, all_of(traj3)) %>% as.data.frame()
time_valid <- valid_l %>% select(hadm_id, time, all_of(traj3)) %>% as.data.frame()
Y_train <- train_b %>% select(hadm_id, time = lm_time, event = lm_status) %>% as.data.frame()

time_models <- stats::setNames(lapply(traj3, function(nm) {
  list(fixed = stats::as.formula(paste(nm, "~ time")), random = ~ time)
}), traj3)

rsflc_path <- file.path(out_dir, "models", "rsflc_survival.rds")
if (file.exists(rsflc_path)) {
  rsflc_fit <- readRDS(rsflc_path)
} else {
  set.seed(seed_value)
  rsflc_fit <- DynForest::dynforest(
    timeData = time_train, fixedData = fixed_train,
    idVar = "hadm_id", timeVar = "time", timeVarModel = time_models,
    Y = list(type = "surv", Y = Y_train),
    ntree = rsflc_par$ntree, mtry = rsflc_par$mtry,
    nodesize = rsflc_par$nodesize, minsplit = rsflc_par$minsplit,
    cause = 1, nsplit_option = "quantile",
    ncores = min(8L, max(1L, parallel::detectCores(logical = TRUE) - 2L)),
    seed = 2026L, verbose = TRUE
  )
  saveRDS(rsflc_fit, rsflc_path)
}

pred_cl <- parallel::makeCluster(4L)
parallel::clusterEvalQ(pred_cl, suppressPackageStartupMessages(library(DynForest)))
parallel::clusterExport(pred_cl, "rsflc_fit", envir = environment())

predict_dyn <- function(fit, time_data, fixed_data, eval_times = dynamic_times) {
  ids_all <- fixed_data$hadm_id
  chunks <- split(ids_all, cut(seq_along(ids_all), breaks = min(4L, length(ids_all)), labels = FALSE))
  jobs <- lapply(chunks, function(ids) list(
    td = time_data[time_data$hadm_id %in% ids, , drop = FALSE],
    fd = fixed_data[fixed_data$hadm_id %in% ids, , drop = FALSE],
    eval_times = eval_times, landmark = landmark
  ))
  pieces <- parallel::parLapply(pred_cl, jobs, function(z) {
    p <- predict(rsflc_fit, timeData = z$td, fixedData = z$fd,
                 idVar = "hadm_id", timeVar = "time", t0 = z$landmark)
    ids <- suppressWarnings(as.integer(rownames(p$pred_indiv)))
    if (length(ids) != nrow(p$pred_indiv) || anyNA(ids)) ids <- as.integer(z$fd$hadm_id[seq_len(nrow(p$pred_indiv))])
    vv <- vapply(seq_len(nrow(p$pred_indiv)), function(i) {
      idx <- findInterval(z$eval_times, p$times)
      out <- numeric(length(z$eval_times)); keep <- idx > 0L
      out[keep] <- p$pred_indiv[i, pmin(idx[keep], ncol(p$pred_indiv))]
      out
    }, numeric(length(z$eval_times)))
    mat <- if (length(z$eval_times) == 1L) matrix(vv, ncol = 1L) else t(vv)
    rownames(mat) <- ids; colnames(mat) <- z$eval_times
    list(ids = ids, risk = mat)
  })
  ids <- unlist(lapply(pieces, `[[`, "ids"), use.names = FALSE)
  mat <- do.call(rbind, lapply(pieces, `[[`, "risk"))
  oo <- match(ids_all, ids)
  list(ids = ids[oo], risk = mat[oo, , drop = FALSE], raw = NULL)
}

prediction_path <- file.path(out_dir, "models", "rsflc_predictions.rds")
if (file.exists(prediction_path)) {
  cached_prediction <- readRDS(prediction_path)
  pr_train <- cached_prediction$train
  pr_valid <- cached_prediction$valid
  train_eval <- cached_prediction$train_eval
  valid_eval <- cached_prediction$valid_eval
} else {
  pr_train <- predict_dyn(rsflc_fit, time_train, fixed_train)
  pr_valid <- predict_dyn(rsflc_fit, time_valid, fixed_valid)
  train_eval <- train_b[match(pr_train$ids, train_b$hadm_id), ]
  valid_eval <- valid_b[match(pr_valid$ids, valid_b$hadm_id), ]
}
G_train <- km_censor_function(train_eval$lm_time, train_eval$lm_status)
met_train <- metric_bundle(train_eval$lm_time, train_eval$lm_status, pr_train$risk, dynamic_times, G_train)
met_valid <- metric_bundle(valid_eval$lm_time, valid_eval$lm_status, pr_valid$risk, dynamic_times, G_train)

metrics <- data.frame(
  model = "RSFLC_survival", landmark = landmark,
  setting = sprintf("3 trajectories + 25 fixed; ntree=%d,mtry=%d,nodesize=%d,minsplit=%d",
                    rsflc_par$ntree, rsflc_par$mtry, rsflc_par$nodesize, rsflc_par$minsplit),
  train_C_index = met_train$cindex, valid_C_index = met_valid$cindex,
  AUC_28 = tail(met_valid$auc, 1), Brier_28 = tail(met_valid$brier, 1),
  IBS_5_28 = met_valid$ibs
)
write_utf8(metrics, file.path(out_dir, "tables", "rsflc_survival_metrics.csv"))
write_utf8(data.frame(time = dynamic_times, AUC = met_valid$auc, Brier = met_valid$brier),
           file.path(out_dir, "tables", "rsflc_time_metrics.csv"))
write_utf8(transform(bootstrap_metrics(valid_eval$lm_time, valid_eval$lm_status,
                                       pr_valid$risk, dynamic_times, G_train), model = "RSFLC"),
           file.path(out_dir, "tables", "rsflc_metric_bootstrap_CI.csv"))
saveRDS(list(train = pr_train, valid = pr_valid, train_eval = train_eval, valid_eval = valid_eval),
        prediction_path)

oob_path <- file.path(out_dir, "models", "rsflc_oob_error.rds")
if (!file.exists(oob_path)) {
  oob <- try(DynForest::compute_ooberror(rsflc_fit, IBS.min = 5, IBS.max = 28,
                                         ncores = min(8L, max(1L, parallel::detectCores() - 2L))), silent = TRUE)
  if (!inherits(oob, "try-error")) saveRDS(oob, oob_path)
}
vimp_table_path <- file.path(out_dir, "tables", "table_5_7_rsflc_OOB_VIMP.csv")
if (file.exists(vimp_table_path)) {
  vimp_tbl <- read.csv(vimp_table_path)
} else {
  set.seed(seed_value)
  vimp_native <- DynForest::compute_vimp(rsflc_fit, IBS.min = 5, IBS.max = 28,
                                         ncores = min(8L, max(1L, parallel::detectCores() - 2L)), seed = 2026L)
  saveRDS(vimp_native, file.path(out_dir, "models", "rsflc_native_vimp.rds"))
  vimp_values <- as.numeric(unlist(vimp_native$Importance, use.names = FALSE))
  vimp_names <- as.character(unlist(vimp_native$Inputs, use.names = FALSE))
  vimp_tbl <- data.frame(variable = vimp_names, OOB_VIMP = vimp_values) %>% arrange(desc(OOB_VIMP))
  write_utf8(vimp_tbl, vimp_table_path)
}

predict_valid28 <- function(time_data, fixed_data) {
  z <- predict_dyn(rsflc_fit, time_data, fixed_data, horizon)
  out <- z$risk[, 1]
  names(out) <- z$ids
  out
}
base_risk <- pr_valid$risk[, ncol(pr_valid$risk)]
names(base_risk) <- pr_valid$ids
base_c <- met_valid$cindex
base_auc <- tail(met_valid$auc, 1)
base_bs <- tail(met_valid$brier, 1)

# Validation permutation is computed on a reproducible stratified validation subset
# to keep the 30-repeat DynForest analysis computationally tractable.
set.seed(seed_value)
event_ids <- valid_eval$hadm_id[valid_eval$lm_status == 1L]
nonevent_ids <- valid_eval$hadm_id[valid_eval$lm_status == 0L]
n_perm_sample <- min(50L, nrow(valid_eval))
n_event <- min(length(event_ids), max(30L, round(n_perm_sample * mean(valid_eval$lm_status))))
perm_ids <- c(sample(event_ids, n_event), sample(nonevent_ids, n_perm_sample - n_event))
perm_b <- fixed_valid[fixed_valid$hadm_id %in% perm_ids, , drop = FALSE]
perm_l <- time_valid[time_valid$hadm_id %in% perm_ids, , drop = FALSE]
perm_outcome <- valid_eval[match(perm_ids, valid_eval$hadm_id), ]
base_sub <- predict_dyn(rsflc_fit, perm_l, perm_b, horizon)
base_sub_out <- perm_outcome[match(base_sub$ids, perm_outcome$hadm_id), ]
G_sub <- G_train
base_sub_c <- concordance(Surv(base_sub_out$lm_time, base_sub_out$lm_status) ~ base_sub$risk[, 1], reverse = TRUE)$concordance
base_sub_auc <- ipcw_auc(base_sub_out$lm_time, base_sub_out$lm_status, base_sub$risk[, 1], horizon, G_sub)
base_sub_bs <- ipcw_brier(base_sub_out$lm_time, base_sub_out$lm_status, base_sub$risk[, 1], horizon, G_sub)

permute_trajectory <- function(td, variable) {
  pieces <- split(td, td$hadm_id)
  src <- sample(names(pieces))
  target <- names(pieces)
  out <- lapply(seq_along(target), function(i) {
    a <- pieces[[target[i]]]
    b <- pieces[[src[i]]]
    a[[variable]] <- approx(b$time, b[[variable]], xout = a$time, rule = 2)$y
    a
  })
  bind_rows(out)
}

perm_raw_path <- file.path(out_dir, "tables", "rsflc_validation_permutation_raw.csv")
perm_summary_path <- file.path(out_dir, "tables", "rsflc_validation_permutation_VIMP.csv")
if (file.exists(perm_raw_path) && file.exists(perm_summary_path)) {
  perm_raw <- read.csv(perm_raw_path)
  perm_summary <- read.csv(perm_summary_path)
} else {
  perm_rows <- list()
  k <- 0L
  set.seed(seed_value)
  rsflc_permutation_repeats <- 3L
  for (nm in c(traj3, dynamic_fixed25)) {
    for (b in seq_len(rsflc_permutation_repeats)) {
    fb <- perm_b
    tl <- perm_l
    if (nm %in% traj3) {
      tl <- permute_trajectory(tl, nm)
    } else {
      fb[[nm]] <- sample(fb[[nm]])
    }
    z <- try(predict_dyn(rsflc_fit, tl, fb, horizon), silent = TRUE)
    if (inherits(z, "try-error")) next
    oo <- base_sub_out[match(z$ids, base_sub_out$hadm_id), ]
    c_i <- concordance(Surv(oo$lm_time, oo$lm_status) ~ z$risk[, 1], reverse = TRUE)$concordance
    a_i <- ipcw_auc(oo$lm_time, oo$lm_status, z$risk[, 1], horizon, G_sub)
    b_i <- ipcw_brier(oo$lm_time, oo$lm_status, z$risk[, 1], horizon, G_sub)
    k <- k + 1L
    perm_rows[[k]] <- data.frame(variable = nm, permutation_id = b, delta_C = base_sub_c - c_i,
                                 delta_AUC = base_sub_auc - a_i, delta_Brier = b_i - base_sub_bs)
    }
  }
  perm_raw <- bind_rows(perm_rows)
  perm_summary <- perm_raw %>% group_by(variable) %>% summarise(
    mean_delta_C = mean(delta_C), sd_delta_C = sd(delta_C),
    mean_delta_AUC = mean(delta_AUC), mean_delta_Brier = mean(delta_Brier),
    completed_repeats = n(), .groups = "drop"
  ) %>% arrange(desc(mean_delta_C))
  write_utf8(perm_raw, perm_raw_path)
  write_utf8(perm_summary, perm_summary_path)
}

# FastSHAP for fixed covariates; trajectories remain patient-specific and unchanged.
hi <- which.max(ifelse(valid_eval$lm_status == 1L, base_risk[as.character(valid_eval$hadm_id)], -Inf))
lo <- which.min(ifelse(valid_eval$lm_status == 0L, base_risk[as.character(valid_eval$hadm_id)], Inf))
hi_id <- valid_eval$hadm_id[hi]
lo_id <- valid_eval$hadm_id[lo]
set.seed(seed_value)
remain_ids <- setdiff(valid_eval$hadm_id, c(hi_id, lo_id))
rsflc_shap_n <- 12L
rsflc_shap_nsim <- 3L
shap_ids <- unique(c(hi_id, lo_id, sample(remain_ids, min(rsflc_shap_n - 2L, length(remain_ids)))))
shap_x <- fixed_valid[match(shap_ids, fixed_valid$hadm_id), dynamic_fixed25, drop = FALSE]
rownames(shap_x) <- shap_ids
set.seed(seed_value)
bg_ids <- sample(fixed_train$hadm_id, min(rsflc_shap_n, nrow(fixed_train)))
bg <- fixed_train[match(bg_ids, fixed_train$hadm_id), dynamic_fixed25, drop = FALSE]
rownames(bg) <- shap_ids[seq_len(nrow(bg))]

pred_wrapper <- function(object, newdata) {
  ids <- shap_ids[seq_len(nrow(newdata))]
  fd <- data.frame(hadm_id = ids, newdata, check.names = FALSE)
  td <- time_valid[time_valid$hadm_id %in% ids, , drop = FALSE]
  z <- predict_dyn(object, td, fd, horizon)
  setNames(z$risk[, 1], z$ids)[as.character(ids)]
}

set.seed(seed_value)
shap_mat <- fastshap::explain(rsflc_fit, X = bg, pred_wrapper = pred_wrapper,
                              newdata = shap_x, nsim = rsflc_shap_nsim, adjust = TRUE)
sv <- shapviz::shapviz(shap_mat, X = shap_x)
shap_imp <- data.frame(variable = colnames(shap_mat), mean_abs_SHAP = colMeans(abs(shap_mat)), mean_SHAP = colMeans(shap_mat)) %>% arrange(desc(mean_abs_SHAP))
write_utf8(shap_imp, file.path(out_dir, "tables", "rsflc_SHAP_importance_fixed_covariates.csv"))
saveRDS(list(values = shap_mat, X = shap_x, ids = shap_ids), file.path(out_dir, "models", "rsflc_SHAP.rds"))
write_utf8(data.frame(patient = c("high_risk_death", "low_risk_survivor"), hadm_id = c(hi_id, lo_id),
                      time = valid_eval$lm_time[c(hi, lo)], status = valid_eval$lm_status[c(hi, lo)],
                      risk28 = base_risk[as.character(c(hi_id, lo_id))]),
           file.path(out_dir, "tables", "rsflc_typical_patients.csv"))

theme_set(theme_minimal(base_size = 11))
ggsave(file.path(out_dir, "figures", "RSFLC_01_OOB_VIMP.png"),
       ggplot(head(vimp_tbl, 20), aes(reorder(variable, OOB_VIMP), OOB_VIMP)) + geom_col(fill = "#2F75B5") + coord_flip() + labs(x = NULL, y = "OOB VIMP", title = "Landmark survival RSFLC variable importance"), width = 7, height = 5.6, dpi = 300)
ggsave(file.path(out_dir, "figures", "RSFLC_02_validation_permutation_VIMP.png"),
       ggplot(head(perm_summary, 20), aes(reorder(variable, mean_delta_C), mean_delta_C)) + geom_col(fill = "#70AD47") + coord_flip() + labs(x = NULL, y = "Mean decrease in validation C-index", title = "RSFLC validation permutation importance"), width = 7, height = 5.6, dpi = 300)
ggsave(file.path(out_dir, "figures", "RSFLC_03_SHAP_beeswarm.png"), sv_importance(sv, kind = "beeswarm", max_display = 20) + ggtitle(sprintf("RSFLC fixed-covariate SHAP (n=%d, nsim=%d)", length(shap_ids), rsflc_shap_nsim)), width = 7.3, height = 6, dpi = 300)
ggsave(file.path(out_dir, "figures", "RSFLC_04_typical_patient_SHAP.png"), sv_waterfall(sv, row_id = 1, max_display = 12) / sv_waterfall(sv, row_id = 2, max_display = 12), width = 7.3, height = 9, dpi = 300)
ggsave(file.path(out_dir, "figures", "RSFLC_05_SHAP_dependence.png"), sv_dependence(sv, v = shap_imp$variable[1], color_var = "auto") + ggtitle("RSFLC SHAP dependence"), width = 7, height = 5, dpi = 300)

cal <- calibration_table(pr_valid$risk[, ncol(pr_valid$risk)], valid_eval$lm_status)
write_utf8(cal, file.path(out_dir, "tables", "rsflc_calibration.csv"))
dca <- dca_table(pr_valid$risk[, ncol(pr_valid$risk)], valid_eval$lm_status)
write_utf8(dca, file.path(out_dir, "tables", "rsflc_DCA.csv"))
ggsave(file.path(out_dir, "figures", "RSFLC_06_calibration.png"), ggplot(cal, aes(predicted, observed)) + geom_abline(slope = 1, intercept = 0, linetype = 2) + geom_line(color = "#2F75B5") + geom_point(color = "#2F75B5") + coord_equal() + labs(x = "Predicted conditional risk", y = "Observed mortality", title = "RSFLC validation calibration at day 28"), width = 7, height = 5.5, dpi = 300)
ggsave(file.path(out_dir, "figures", "RSFLC_07_DCA.png"), ggplot(dca, aes(threshold, model)) + geom_line(color = "#2F75B5") + geom_line(aes(y = treat_all), color = "grey40", linetype = 2) + geom_hline(yintercept = 0, linetype = 3) + labs(x = "Threshold probability", y = "Net benefit", title = "RSFLC dynamic decision curve"), width = 7, height = 5.5, dpi = 300)
ggsave(file.path(out_dir, "figures", "RSFLC_08_time_metrics.png"), tidyr::pivot_longer(data.frame(time = dynamic_times, AUC = met_valid$auc, Brier = met_valid$brier), c(AUC, Brier), names_to = "metric", values_to = "value") %>% ggplot(aes(time, value)) + geom_line(color = "#2F75B5") + facet_wrap(~metric, scales = "free_y") + labs(x = "Day", y = NULL, title = "RSFLC time-dependent performance"), width = 8, height = 4.8, dpi = 300)

parallel::stopCluster(pred_cl)
cat("RSFLC_OK\n")
