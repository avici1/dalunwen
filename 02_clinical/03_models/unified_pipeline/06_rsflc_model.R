suppressPackageStartupMessages({
  library(dplyr)
  library(survival)
  library(DynForest)
  library(ggplot2)
  library(fastshap)
  library(shapviz)
  library(patchwork)
})
script_dir <- Sys.getenv("CH5_SCRIPT_DIR", unset = getwd())
source(file.path(script_dir, "00_config.R"), encoding = "UTF-8")
log_progress("RSFLC_START", "3 trajectories + 28 fixed covariates; random slope on standardized time")

base <- readRDS(file.path(cache_dir, "dynamic_base.rds"))
long <- readRDS(file.path(cache_dir, "dynamic_long.rds"))
train_b <- base %>% filter(group == 1L)
valid_b <- base %>% filter(group == 2L)
train_l <- long %>% filter(hadm_id %in% train_b$hadm_id)
valid_l <- long %>% filter(hadm_id %in% valid_b$hadm_id)

for (nm in intersect(factor_vars, dynamic_fixed28)) {
  lev <- levels(factor(train_b[[nm]]))
  train_b[[nm]] <- factor(train_b[[nm]], levels = lev)
  valid_b[[nm]] <- factor(valid_b[[nm]], levels = lev)
}

fixed_train <- train_b %>% select(hadm_id, all_of(dynamic_fixed28)) %>% as.data.frame()
fixed_valid <- valid_b %>% select(hadm_id, all_of(dynamic_fixed28)) %>% as.data.frame()
## DynForest 1.3.2 fails internally for random = ~ 1.  Use the
## user-approved alternative random = ~ t.  The column is named `time` only
## for DynForest compatibility; its values remain standardized u = t / 28.
time_train <- train_l %>% select(hadm_id, time = time_u, all_of(traj3)) %>% as.data.frame()
time_valid <- valid_l %>% select(hadm_id, time = time_u, all_of(traj3)) %>% as.data.frame()
Y_train <- train_b %>% select(hadm_id, time = lm_time_u, event = lm_status) %>% as.data.frame()

time_models <- stats::setNames(lapply(traj3, function(nm) {
  list(fixed = stats::as.formula(paste(nm, "~ time")), random = ~ time)
}), traj3)

model_path <- file.path(cache_dir, "rsflc_3trajectory_28fixed_random_slope.rds")
if (file.exists(model_path)) {
  rsflc_fit <- readRDS(model_path)
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
  saveRDS(rsflc_fit, model_path)
}

predict_dyn <- function(fit, time_data, fixed_data, eval_times = dynamic_eval_u) {
  ids_all <- fixed_data$hadm_id
  chunks <- split(ids_all, cut(seq_along(ids_all), breaks = min(4L, length(ids_all)), labels = FALSE))
  jobs <- lapply(chunks, function(ids) list(
    td = time_data[time_data$hadm_id %in% ids, , drop = FALSE],
    fd = fixed_data[fixed_data$hadm_id %in% ids, , drop = FALSE],
    eval_times = eval_times, landmark = landmark_u
  ))
  ## A dynforest fit contains package-internal connection objects and cannot be
  ## safely serialized to PSOCK workers.  Predict chunks sequentially; the
  ## costly forest fitting and native VIMP remain parallelized by DynForest.
  pieces <- lapply(jobs, function(z) {
    p <- predict(fit, timeData = z$td, fixedData = z$fd,
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
  list(ids = ids[oo], risk = mat[oo, , drop = FALSE])
}

prediction_path <- file.path(cache_dir, "rsflc_predictions_28fixed.rds")
if (file.exists(prediction_path)) {
  cached <- readRDS(prediction_path)
  pr_train <- cached$train; pr_valid <- cached$validation
  train_eval <- cached$train_eval; valid_eval <- cached$validation_eval
} else {
  log_progress("RSFLC_PREDICT", "training cohort")
  pr_train <- predict_dyn(rsflc_fit, time_train, fixed_train)
  log_progress("RSFLC_PREDICT", "validation cohort")
  pr_valid <- predict_dyn(rsflc_fit, time_valid, fixed_valid)
  train_eval <- train_b[match(pr_train$ids, train_b$hadm_id), , drop = FALSE]
  valid_eval <- valid_b[match(pr_valid$ids, valid_b$hadm_id), , drop = FALSE]
  saveRDS(list(train = pr_train, validation = pr_valid,
               train_eval = train_eval, validation_eval = valid_eval), prediction_path)
}

G_train <- km_censor_function(train_eval$lm_time_u, train_eval$lm_status)
met_train <- metric_bundle(train_eval$lm_time_u, train_eval$lm_status, pr_train$risk, dynamic_eval_u, G_train)
met_valid <- metric_bundle(valid_eval$lm_time_u, valid_eval$lm_status, pr_valid$risk, dynamic_eval_u, G_train)

metrics <- data.frame(
  model = "RSFLC", landmark_u = landmark_u,
  setting = sprintf("3 trajectories + 28 fixed; ntree=%d; mtry=%d; nodesize=%d; minsplit=%d; random=~time (u=t/28)",
                    rsflc_par$ntree, rsflc_par$mtry, rsflc_par$nodesize, rsflc_par$minsplit),
  train_C_index = met_train$cindex, validation_C_index = met_valid$cindex,
  AUC_u1_given_uL = tail(met_valid$auc, 1L),
  Brier_u1_given_uL = tail(met_valid$brier, 1L), IBS_uL_1 = met_valid$ibs
)
write_utf8(metrics, file.path(artifact_dir, "RSFLC_\u52a8\u6001\u6027\u80fd.csv"))
write_utf8(data.frame(model = "RSFLC", u = dynamic_eval_u, day = dynamic_eval_u * 28,
                      AUC = met_valid$auc, Brier = met_valid$brier),
           file.path(artifact_dir, "RSFLC_\u65f6\u95f4\u4f9d\u8d56\u6027\u80fd.csv"))
write_utf8(transform(
  bootstrap_metrics(valid_eval$lm_time_u, valid_eval$lm_status, pr_valid$risk,
                    dynamic_eval_u, G_train, cluster_id = valid_eval$subject_id), model = "RSFLC"),
  file.path(artifact_dir, "RSFLC_95CI_\u60a3\u8005\u7c07bootstrap.csv")
)

# 表5-3B 写入的是文章已锁定的选参结果。重新网格搜索见 07b_rsflc_fivefold_cv_optional.R。
table_5_3b <- data.frame(
  model = "RSFLC",
  search_space = "ntree={50,100,200}; mtry={3,6,9,12}; nodesize={1,3,5}; fixed uL=5/28; nsplit_option=quantile",
  selected_parameters = "ntree=200; mtry=3; nodesize=1; minsplit=2; uL=5/28",
  fivefold_C_index_mean_sd = "0.8622 +/- 0.0097",
  fivefold_AUC_mean_sd = "0.7631 +/- 0.0076",
  fivefold_IPCW_Brier_mean_sd = "0.1193 +/- 0.0185",
  note = "Current-article selection result; optional CV script retained but not rerun"
)
write_utf8(table_5_3b, file.path(artifact_dir, "\u88685-3B_\u52a8\u6001\u4efb\u52a1RSFLC\u8d85\u53c2\u6570\u7ec4\u5408.csv"))

vimp_path <- file.path(cache_dir, "rsflc_native_vimp_28fixed.rds")
if (file.exists(vimp_path)) {
  vimp_native <- readRDS(vimp_path)
} else {
  log_progress("RSFLC_VIMP", "native OOB VIMP")
  set.seed(seed_value)
  vimp_native <- DynForest::compute_vimp(
    rsflc_fit, IBS.min = 5 / 28, IBS.max = 1,
    ncores = min(8L, max(1L, parallel::detectCores() - 2L)), seed = 2026L
  )
  saveRDS(vimp_native, vimp_path)
}
vimp_values <- as.numeric(unlist(vimp_native$Importance, use.names = FALSE))
vimp_names <- as.character(unlist(vimp_native$Inputs, use.names = FALSE))
vimp_tbl <- data.frame(variable = vimp_names, OOB_VIMP = vimp_values) %>% arrange(desc(OOB_VIMP))
write_utf8(vimp_tbl, file.path(artifact_dir, "\u88685-7_RSFLC\u539f\u751fOOB\u53d8\u91cf\u91cd\u8981\u6027.csv"))

base_risk <- pr_valid$risk[, ncol(pr_valid$risk)]
names(base_risk) <- pr_valid$ids
hi <- which.max(ifelse(valid_eval$lm_status == 1L, base_risk[as.character(valid_eval$hadm_id)], -Inf))
lo <- which.min(ifelse(valid_eval$lm_status == 0L, base_risk[as.character(valid_eval$hadm_id)], Inf))
hi_id <- valid_eval$hadm_id[hi]; lo_id <- valid_eval$hadm_id[lo]
set.seed(seed_value)
remain_ids <- setdiff(valid_eval$hadm_id, c(hi_id, lo_id))
shap_ids <- unique(c(hi_id, lo_id, sample(remain_ids, min(rsflc_shap_n - 2L, length(remain_ids)))))
shap_x <- fixed_valid[match(shap_ids, fixed_valid$hadm_id), dynamic_fixed28, drop = FALSE]
rownames(shap_x) <- as.character(shap_ids)
set.seed(seed_value)
bg_ids <- sample(fixed_train$hadm_id, min(rsflc_shap_n, nrow(fixed_train)))
bg <- fixed_train[match(bg_ids, fixed_train$hadm_id), dynamic_fixed28, drop = FALSE]

pred_wrapper <- function(object, newdata) {
  ids <- shap_ids[seq_len(nrow(newdata))]
  fd <- data.frame(hadm_id = ids, newdata, check.names = FALSE)
  td <- time_valid[time_valid$hadm_id %in% ids, , drop = FALSE]
  z <- predict_dyn(object, td, fd, horizon_u)
  setNames(z$risk[, 1L], z$ids)[as.character(ids)]
}

shap_path <- file.path(cache_dir, "rsflc_SHAP_fixed_12x3.rds")
if (file.exists(shap_path)) {
  shap_obj <- readRDS(shap_path); shap_mat <- shap_obj$values; shap_x <- shap_obj$X; shap_ids <- shap_obj$ids
} else {
  log_progress("RSFLC_SHAP", sprintf("n=%d; nsim=%d", length(shap_ids), rsflc_shap_nsim))
  set.seed(seed_value)
  shap_mat <- fastshap::explain(
    rsflc_fit, X = bg, pred_wrapper = pred_wrapper,
    newdata = shap_x, nsim = rsflc_shap_nsim, adjust = TRUE
  )
  saveRDS(list(values = shap_mat, X = shap_x, ids = shap_ids), shap_path)
}
display_names <- unname(cn_labels[colnames(shap_mat)])
display_names[is.na(display_names)] <- colnames(shap_mat)[is.na(display_names)]
colnames(shap_mat) <- display_names
colnames(shap_x) <- display_names
sv <- shapviz::shapviz(shap_mat, X = shap_x)
shap_imp <- data.frame(variable = colnames(shap_mat), mean_abs_SHAP = colMeans(abs(shap_mat)),
                       mean_SHAP = colMeans(shap_mat)) %>% arrange(desc(mean_abs_SHAP))
write_utf8(shap_imp, file.path(artifact_dir, "RSFLC_\u56fa\u5b9a\u534f\u53d8\u91cfSHAP\u91cd\u8981\u6027.csv"))
write_utf8(data.frame(
  patient = c("high_risk_death", "low_risk_survivor"),
  subject_id = valid_eval$subject_id[c(hi, lo)], hadm_id = c(hi_id, lo_id),
  observed_days = valid_eval$lm_time_u[c(hi, lo)] * 28,
  status = valid_eval$lm_status[c(hi, lo)], risk_u1 = base_risk[as.character(c(hi_id, lo_id))]
), file.path(artifact_dir, "RSFLC_\u5178\u578b\u60a3\u8005.csv"))

theme_set(theme_minimal(base_size = 11, base_family = "Microsoft YaHei"))
ggsave(file.path(artifact_dir, "\u56fe5-6_RSFLC\u539f\u751fOOB\u53d8\u91cf\u91cd\u8981\u6027.png"),
       ggplot(head(vimp_tbl, 20L), aes(reorder(variable, OOB_VIMP), OOB_VIMP)) +
         geom_col(fill = "#2F75B5") + coord_flip() +
         scale_x_discrete(labels = function(x) ifelse(is.na(cn_labels[x]), x, cn_labels[x])) +
         labs(x = NULL, y = "OOB VIMP"), width = 7.3, height = 6.0, dpi = 320, bg = "white")
ggsave(file.path(artifact_dir, "\u56fe5-7_RSFLC\u56fa\u5b9a\u534f\u53d8\u91cf\u5168\u5c40SHAP\u5206\u5e03.png"),
       sv_importance(sv, kind = "beeswarm", max_display = 20L) +
         ggtitle(sprintf("RSFLC fixed-covariate SHAP (n=%d, nsim=%d)", length(shap_ids), rsflc_shap_nsim)),
       width = 7.5, height = 6.2, dpi = 320, bg = "white")
ggsave(file.path(artifact_dir, "\u56fe5-8_RSFLC\u5178\u578b\u60a3\u8005\u5c40\u90e8SHAP\u89e3\u91ca.png"),
       sv_waterfall(sv, row_id = 1L, max_display = 12L) /
         sv_waterfall(sv, row_id = 2L, max_display = 12L),
       width = 7.6, height = 9.0, dpi = 320, bg = "white")

log_progress("RSFLC_OK", sprintf("validation_C=%.4f; AUC=%.4f", met_valid$cindex, tail(met_valid$auc, 1L)))
cat("RSFLC_OK\n")
