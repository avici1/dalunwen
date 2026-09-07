suppressPackageStartupMessages({
  library(dplyr)
  library(survival)
  library(randomForestSRC)
  library(riskRegression)
  library(ggplot2)
  library(fastshap)
  library(shapviz)
  library(patchwork)
})
script_dir <- Sys.getenv("CH5_SCRIPT_DIR", unset = "work/chapter5_0831")
source(file.path(script_dir, "00_config.R"), encoding = "UTF-8")
options(rf.cores = max(1L, parallel::detectCores(logical = TRUE) - 2L))

d <- readRDS(file.path(out_dir, "models", "static_data.rds"))
train0 <- d %>% filter(group == 1L)
valid0 <- d %>% filter(group == 2L)
train_ids <- train0$hadm_id
valid_ids <- valid0$hadm_id
train <- train0 %>% select(-hadm_id, -group) %>% as.data.frame()
valid <- valid0 %>% select(-hadm_id, -group) %>% as.data.frame()

for (nm in intersect(factor_vars, names(train))) {
  train[[nm]] <- factor(train[[nm]])
  valid[[nm]] <- factor(valid[[nm]], levels = levels(train[[nm]]))
}

cox_path <- file.path(out_dir, "models", "cox_static.rds")
rsf_path <- file.path(out_dir, "models", "rsf_static.rds")
if (file.exists(cox_path) && file.exists(rsf_path)) {
  cox_fit <- readRDS(cox_path)
  rsf_fit <- readRDS(rsf_path)
} else {
  set.seed(seed_value)
  cox_fit <- coxph(Surv(time28, status28) ~ ., data = train, x = TRUE, y = TRUE, model = TRUE)
  set.seed(seed_value)
  rsf_fit <- rfsrc(
    Surv(time28, status28) ~ ., data = train,
    ntree = rsf_par$ntree, mtry = rsf_par$mtry,
    nodesize = rsf_par$nodesize, nsplit = rsf_par$nsplit,
    splitrule = "logrank", importance = FALSE,
    block.size = 1L, na.action = "na.impute", seed = seed_value
  )
  saveRDS(cox_fit, cox_path)
  saveRDS(rsf_fit, rsf_path)
}

cox_risk_matrix <- function(newdata, times) {
  as.matrix(riskRegression::predictRisk(cox_fit, newdata = newdata, times = times))
}
rsf_risk_matrix <- function(newdata, times) {
  pr <- predict(rsf_fit, newdata = newdata, na.action = "na.impute")
  out <- t(vapply(seq_len(nrow(newdata)), function(i) {
    1 - step_at(pr$time.interest, pr$survival[i, ], times, initial = 1)
  }, numeric(length(times))))
  colnames(out) <- times
  out
}

cox_train_risk <- cox_risk_matrix(train, static_times)
cox_valid_risk <- cox_risk_matrix(valid, static_times)
rsf_train_risk <- rsf_risk_matrix(train, static_times)
rsf_valid_risk <- rsf_risk_matrix(valid, static_times)
G_train <- km_censor_function(train$time28, train$status28)

cox_train_metrics <- metric_bundle(train$time28, train$status28, cox_train_risk, static_times, G_train)
cox_valid_metrics <- metric_bundle(valid$time28, valid$status28, cox_valid_risk, static_times, G_train)
rsf_train_metrics <- metric_bundle(train$time28, train$status28, rsf_train_risk, static_times, G_train)
rsf_valid_metrics <- metric_bundle(valid$time28, valid$status28, rsf_valid_risk, static_times, G_train)
rsf_oob_c <- 1 - tail(as.numeric(rsf_fit$err.rate), 1)

static_metrics <- data.frame(
  model = c("COX", "RSF"),
  setting = c("31 baseline predictors", sprintf("ntree=%d,mtry=%d,nodesize=%d,nsplit=%d", rsf_par$ntree, rsf_par$mtry, rsf_par$nodesize, rsf_par$nsplit)),
  train_C_index = c(cox_train_metrics$cindex, rsf_oob_c),
  valid_C_index = c(cox_valid_metrics$cindex, rsf_valid_metrics$cindex),
  AUC_28 = c(tail(cox_valid_metrics$auc, 1), tail(rsf_valid_metrics$auc, 1)),
  Brier_28 = c(tail(cox_valid_metrics$brier, 1), tail(rsf_valid_metrics$brier, 1)),
  IBS_0_28 = c(cox_valid_metrics$ibs, rsf_valid_metrics$ibs)
)
write_utf8(static_metrics, file.path(out_dir, "tables", "table_5_4A_static_metrics.csv"))

curve_tbl <- bind_rows(
  data.frame(model = "COX", time = static_times, AUC = cox_valid_metrics$auc, Brier = cox_valid_metrics$brier),
  data.frame(model = "RSF", time = static_times, AUC = rsf_valid_metrics$auc, Brier = rsf_valid_metrics$brier)
)
write_utf8(curve_tbl, file.path(out_dir, "tables", "static_time_metrics.csv"))

ci_static <- bind_rows(
  transform(bootstrap_metrics(valid$time28, valid$status28, cox_valid_risk, static_times, G_train), model = "COX"),
  transform(bootstrap_metrics(valid$time28, valid$status28, rsf_valid_risk, static_times, G_train), model = "RSF")
)
write_utf8(ci_static, file.path(out_dir, "tables", "static_metric_bootstrap_CI.csv"))

sm <- summary(cox_fit)
cox_coef <- data.frame(
  term = rownames(sm$coefficients),
  beta = sm$coefficients[, "coef"],
  HR = sm$coefficients[, "exp(coef)"],
  lower95 = sm$conf.int[, "lower .95"],
  upper95 = sm$conf.int[, "upper .95"],
  p_value = sm$coefficients[, "Pr(>|z|)"],
  row.names = NULL
)
write_utf8(cox_coef, file.path(out_dir, "tables", "table_5_5_cox_coefficients.csv"))
ph <- cox.zph(cox_fit)
ph_tbl <- data.frame(term = rownames(ph$table), ph$table, row.names = NULL)
write_utf8(ph_tbl, file.path(out_dir, "tables", "cox_PH_test.csv"))

set.seed(seed_value)
vimp_obj <- vimp(rsf_fit, importance = "permute", seed = seed_value)
vimp_tbl <- data.frame(variable = names(vimp_obj$importance), OOB_VIMP = as.numeric(vimp_obj$importance)) %>%
  arrange(desc(OOB_VIMP))
write_utf8(vimp_tbl, file.path(out_dir, "tables", "rsf_OOB_VIMP.csv"))

md_obj <- max.subtree(rsf_fit)
md_order <- md_obj[["order"]]
md_tbl <- data.frame(variable = rownames(md_order), minimal_depth = md_order[, 1], second_order_depth = md_order[, 2], row.names = NULL) %>%
  arrange(minimal_depth)
write_utf8(md_tbl, file.path(out_dir, "tables", "rsf_minimal_depth.csv"))

predict_rsf_28 <- function(object, newdata) {
  pr <- predict(object, newdata = newdata, na.action = "na.impute")
  j <- which.min(abs(pr$time.interest - horizon))
  1 - pr$survival[, j]
}
base_valid_risk <- predict_rsf_28(rsf_fit, valid[, static31, drop = FALSE])
base_c <- survival::concordance(Surv(valid$time28, valid$status28) ~ base_valid_risk, reverse = TRUE)$concordance
perm_rows <- vector("list", length(static31) * permutation_repeats)
k <- 0L
set.seed(seed_value)
for (nm in static31) {
  for (b in seq_len(permutation_repeats)) {
    x <- valid[, static31, drop = FALSE]
    x[[nm]] <- sample(x[[nm]])
    r <- predict_rsf_28(rsf_fit, x)
    c_i <- survival::concordance(Surv(valid$time28, valid$status28) ~ r, reverse = TRUE)$concordance
    auc_i <- ipcw_auc(valid$time28, valid$status28, r, horizon, G_train)
    bs_i <- ipcw_brier(valid$time28, valid$status28, r, horizon, G_train)
    k <- k + 1L
    perm_rows[[k]] <- data.frame(variable = nm, permutation_id = b,
                                 delta_C = base_c - c_i,
                                 delta_AUC = tail(rsf_valid_metrics$auc, 1) - auc_i,
                                 delta_Brier = bs_i - tail(rsf_valid_metrics$brier, 1))
  }
}
perm_raw <- bind_rows(perm_rows)
perm_summary <- perm_raw %>% group_by(variable) %>% summarise(
  mean_delta_C = mean(delta_C), sd_delta_C = sd(delta_C),
  mean_delta_AUC = mean(delta_AUC), mean_delta_Brier = mean(delta_Brier), .groups = "drop"
) %>% arrange(desc(mean_delta_C))
write_utf8(perm_raw, file.path(out_dir, "tables", "rsf_validation_permutation_raw.csv"))
write_utf8(perm_summary, file.path(out_dir, "tables", "rsf_validation_permutation_VIMP.csv"))

# The memory-safe, vectorized SHAP/PDP/calibration stage is implemented in
# 02b_static_explain.R and deliberately run as a separate resumable step.
cat("STATIC_CORE_OK\n")
quit(save = "no", status = 0L)

train_x <- train[, static31, drop = FALSE]
valid_x <- valid[, static31, drop = FALSE]
hi <- which.max(ifelse(valid$status28 == 1L, base_valid_risk, -Inf))
lo <- which.min(ifelse(valid$status28 == 0L, base_valid_risk, Inf))
set.seed(seed_value)
remain <- setdiff(seq_len(nrow(valid_x)), c(hi, lo))
idx <- unique(c(hi, lo, sample(remain, min(shap_n - 2L, length(remain)))))
shap_x <- valid_x[idx, , drop = FALSE]
set.seed(seed_value)
bg <- train_x[sample(seq_len(nrow(train_x)), min(shap_background_n, nrow(train_x))), , drop = FALSE]
set.seed(seed_value)
shap_mat <- fastshap::explain(rsf_fit, X = bg, pred_wrapper = predict_rsf_28,
                              newdata = shap_x, nsim = shap_nsim, adjust = TRUE)
sv <- shapviz::shapviz(shap_mat, X = shap_x)
shap_imp <- data.frame(variable = colnames(shap_mat), mean_abs_SHAP = colMeans(abs(shap_mat)), mean_SHAP = colMeans(shap_mat)) %>%
  arrange(desc(mean_abs_SHAP))
write_utf8(shap_imp, file.path(out_dir, "tables", "rsf_SHAP_importance.csv"))
saveRDS(list(values = shap_mat, X = shap_x, indices = idx), file.path(out_dir, "models", "rsf_SHAP.rds"))

typical <- data.frame(
  patient = c("high_risk_death", "low_risk_survivor"),
  hadm_id = valid_ids[c(hi, lo)], observed_time = valid$time28[c(hi, lo)],
  status = valid$status28[c(hi, lo)], risk28 = base_valid_risk[c(hi, lo)]
)
write_utf8(typical, file.path(out_dir, "tables", "rsf_typical_patients.csv"))

top4 <- head(vimp_tbl$variable, 4)
pdp_list <- list()
for (nm in top4) {
  grid <- if (is.numeric(valid_x[[nm]])) {
    unique(as.numeric(quantile(valid_x[[nm]], seq(0.05, 0.95, length.out = 20), na.rm = TRUE)))
  } else levels(valid_x[[nm]])
  pdp_list[[nm]] <- bind_rows(lapply(grid, function(z) {
    x <- valid_x
    x[[nm]] <- if (is.factor(x[[nm]])) factor(z, levels = levels(x[[nm]])) else as.numeric(z)
    data.frame(variable = nm, value = as.character(z), risk28 = mean(predict_rsf_28(rsf_fit, x)))
  }))
}
pdp_tbl <- bind_rows(pdp_list)
write_utf8(pdp_tbl, file.path(out_dir, "tables", "rsf_PDP.csv"))

theme_set(theme_minimal(base_size = 11))
ggsave(file.path(out_dir, "figures", "RSF_01_OOB_convergence.png"),
       ggplot(data.frame(tree = seq_along(rsf_fit$err.rate), C = 1 - as.numeric(rsf_fit$err.rate)), aes(tree, C)) +
         geom_line(color = "#2F75B5") + geom_hline(yintercept = rsf_oob_c, linetype = 2, color = "#C00000") +
         labs(x = "Number of trees", y = "OOB C-index", title = "RSF OOB convergence"), width = 7, height = 4.6, dpi = 300)
ggsave(file.path(out_dir, "figures", "RSF_02_OOB_VIMP.png"),
       ggplot(head(vimp_tbl, 20), aes(reorder(variable, OOB_VIMP), OOB_VIMP)) + geom_col(fill = "#2F75B5") + coord_flip() +
         labs(x = NULL, y = "OOB permutation VIMP", title = "RSF OOB variable importance"), width = 7, height = 5.6, dpi = 300)
ggsave(file.path(out_dir, "figures", "RSF_03_validation_permutation_VIMP.png"),
       ggplot(head(perm_summary, 20), aes(reorder(variable, mean_delta_C), mean_delta_C)) + geom_col(fill = "#70AD47") + coord_flip() +
         labs(x = NULL, y = "Mean decrease in validation C-index", title = "RSF validation permutation importance"), width = 7, height = 5.6, dpi = 300)
ggsave(file.path(out_dir, "figures", "RSF_04_minimal_depth.png"),
       ggplot(head(md_tbl, 20), aes(reorder(variable, -minimal_depth), minimal_depth)) + geom_col(fill = "#A5A5A5") + coord_flip() +
         labs(x = NULL, y = "Minimal depth", title = "RSF minimal depth"), width = 7, height = 5.6, dpi = 300)

pdp_plots <- lapply(split(pdp_tbl, pdp_tbl$variable), function(z) {
  z$value_num <- suppressWarnings(as.numeric(z$value))
  if (all(is.finite(z$value_num))) ggplot(z, aes(value_num, risk28)) + geom_line(color = "#2F75B5") + geom_point(color = "#2F75B5") + labs(x = z$variable[1], y = "Mean 28-day risk")
  else ggplot(z, aes(value, risk28)) + geom_col(fill = "#2F75B5") + labs(x = z$variable[1], y = "Mean 28-day risk")
})
ggsave(file.path(out_dir, "figures", "RSF_05_PDP.png"), wrap_plots(pdp_plots, ncol = 2) + plot_annotation(title = "RSF partial dependence for 28-day mortality"), width = 8, height = 6.5, dpi = 300)
ggsave(file.path(out_dir, "figures", "RSF_06_SHAP_beeswarm.png"), sv_importance(sv, kind = "beeswarm", max_display = 20) + ggtitle(sprintf("RSF SHAP (n=%d, nsim=%d)", length(idx), shap_nsim)), width = 7.3, height = 6, dpi = 300)
ggsave(file.path(out_dir, "figures", "RSF_07_typical_patient_SHAP.png"), sv_waterfall(sv, row_id = 1, max_display = 12) / sv_waterfall(sv, row_id = 2, max_display = 12), width = 7.3, height = 9, dpi = 300)
ggsave(file.path(out_dir, "figures", "RSF_08_SHAP_dependence.png"), sv_dependence(sv, v = shap_imp$variable[1], color_var = "auto") + ggtitle("RSF SHAP dependence"), width = 7, height = 5, dpi = 300)

cal_static <- bind_rows(
  transform(calibration_table(cox_valid_risk[, ncol(cox_valid_risk)], valid$status28), model = "COX"),
  transform(calibration_table(rsf_valid_risk[, ncol(rsf_valid_risk)], valid$status28), model = "RSF")
)
write_utf8(cal_static, file.path(out_dir, "tables", "static_calibration.csv"))
ggsave(file.path(out_dir, "figures", "STATIC_09_calibration.png"),
       ggplot(cal_static, aes(predicted, observed, color = model)) + geom_abline(slope = 1, intercept = 0, linetype = 2) + geom_line() + geom_point() + coord_equal() + labs(x = "Predicted 28-day risk", y = "Observed 28-day mortality", title = "Static-task validation calibration"), width = 7, height = 5.5, dpi = 300)

dca_static <- bind_rows(
  transform(dca_table(cox_valid_risk[, ncol(cox_valid_risk)], valid$status28), model_name = "COX"),
  transform(dca_table(rsf_valid_risk[, ncol(rsf_valid_risk)], valid$status28), model_name = "RSF")
)
write_utf8(dca_static, file.path(out_dir, "tables", "static_DCA.csv"))
ggsave(file.path(out_dir, "figures", "STATIC_10_DCA.png"),
       ggplot(dca_static, aes(threshold, model, color = model_name)) + geom_line() + geom_line(aes(y = treat_all), color = "grey40", linetype = 2) + geom_hline(yintercept = 0, linetype = 3) + labs(x = "Threshold probability", y = "Net benefit", title = "Static-task decision curve analysis"), width = 7, height = 5.5, dpi = 300)

long_curve <- tidyr::pivot_longer(curve_tbl, cols = c(AUC, Brier), names_to = "metric", values_to = "value")
ggsave(file.path(out_dir, "figures", "STATIC_11_time_metrics.png"),
       ggplot(long_curve, aes(time, value, color = model)) + geom_line() + facet_wrap(~metric, scales = "free_y") + labs(x = "Days", y = NULL, title = "Static-task time-dependent performance"), width = 8, height = 4.8, dpi = 300)

cat("STATIC_OK\n")
