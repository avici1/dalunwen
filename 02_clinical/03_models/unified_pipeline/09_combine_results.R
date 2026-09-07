suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(patchwork)
})
script_dir <- Sys.getenv("CH5_SCRIPT_DIR", unset = getwd())
source(file.path(script_dir, "00_config.R"), encoding = "UTF-8")
log_progress("COMBINE_START", "common-cohort dynamic comparison and final figures")

jm <- readRDS(file.path(cache_dir, "jm_evaluation_data.rds"))
rf <- readRDS(file.path(cache_dir, "rsflc_predictions_28fixed.rds"))

common_train <- intersect(jm$train_base$hadm_id, rf$train_eval$hadm_id)
common_valid <- intersect(jm$validation_base$hadm_id, rf$validation_eval$hadm_id)
train_base <- jm$train_base[match(common_train, jm$train_base$hadm_id), , drop = FALSE]
valid_base <- jm$validation_base[match(common_valid, jm$validation_base$hadm_id), , drop = FALSE]
jm_train_risk <- jm$train_risk[match(common_train, jm$train_base$hadm_id), , drop = FALSE]
jm_valid_risk <- jm$validation_risk[match(common_valid, jm$validation_base$hadm_id), , drop = FALSE]
rf_train_risk <- rf$train$risk[match(common_train, rf$train$ids), , drop = FALSE]
rf_valid_risk <- rf$validation$risk[match(common_valid, rf$validation$ids), , drop = FALSE]

Gfun <- km_censor_function(train_base$lm_time_u, train_base$lm_status)
jm_train_met <- metric_bundle(train_base$lm_time_u, train_base$lm_status, jm_train_risk, dynamic_eval_u, Gfun)
jm_valid_met <- metric_bundle(valid_base$lm_time_u, valid_base$lm_status, jm_valid_risk, dynamic_eval_u, Gfun)
rf_train_met <- metric_bundle(train_base$lm_time_u, train_base$lm_status, rf_train_risk, dynamic_eval_u, Gfun)
rf_valid_met <- metric_bundle(valid_base$lm_time_u, valid_base$lm_status, rf_valid_risk, dynamic_eval_u, Gfun)

jm_mcmc_setting <- if (file.exists(file.path(cache_dir, "jm_3trajectory_28fixed_random_intercept_12000.rds"))) {
  "3 trajectories + 28 fixed; Y~time_u; random=~1; 3 chains; 12000 iterations; burn-in 6000 (auto-extended for Rhat)"
} else if (file.exists(file.path(cache_dir, "jm_3trajectory_28fixed_random_intercept_6000.rds"))) {
  "3 trajectories + 28 fixed; Y~time_u; random=~1; 3 chains; 6000 iterations; burn-in 3000 (auto-extended for Rhat)"
} else {
  "3 trajectories + 28 fixed; Y~time_u; random=~1; 3 chains; 3000 iterations; burn-in 1500"
}

table_5_4b <- data.frame(
  model = c("JM", "RSFLC"),
  setting = c(
    jm_mcmc_setting,
    sprintf("3 trajectories + 28 fixed; Y~time; random=~time (u=t/28); ntree=%d; mtry=%d; nodesize=%d; minsplit=%d",
            rsflc_par$ntree, rsflc_par$mtry, rsflc_par$nodesize, rsflc_par$minsplit)
  ),
  train_C_index = c(jm_train_met$cindex, rf_train_met$cindex),
  validation_C_index = c(jm_valid_met$cindex, rf_valid_met$cindex),
  AUC_u1_given_uL = c(tail(jm_valid_met$auc, 1L), tail(rf_valid_met$auc, 1L)),
  Brier_u1_given_uL = c(tail(jm_valid_met$brier, 1L), tail(rf_valid_met$brier, 1L)),
  IBS_uL_1 = c(jm_valid_met$ibs, rf_valid_met$ibs),
  validation_n = length(common_valid)
)
write_utf8(table_5_4b, file.path(artifact_dir, "\u88685-4B_\u52a8\u6001\u4efb\u52a1\u5185\u90e8\u7559\u51fa\u9a8c\u8bc1\u96c6\u6027\u80fd.csv"))

dynamic_ci <- bind_rows(
  transform(bootstrap_metrics(valid_base$lm_time_u, valid_base$lm_status, jm_valid_risk,
                              dynamic_eval_u, Gfun, cluster_id = valid_base$subject_id), model = "JM"),
  transform(bootstrap_metrics(valid_base$lm_time_u, valid_base$lm_status, rf_valid_risk,
                              dynamic_eval_u, Gfun, cluster_id = valid_base$subject_id), model = "RSFLC")
)
write_utf8(dynamic_ci, file.path(artifact_dir, "\u88685-4B_95CI_\u5171\u540c\u9a8c\u8bc1\u961f\u5217bootstrap.csv"))

dynamic_curves <- bind_rows(
  data.frame(model = "JM", u = dynamic_eval_u, day = dynamic_eval_u * 28,
             AUC = jm_valid_met$auc, Brier = jm_valid_met$brier),
  data.frame(model = "RSFLC", u = dynamic_eval_u, day = dynamic_eval_u * 28,
             AUC = rf_valid_met$auc, Brier = rf_valid_met$brier)
)
write_utf8(dynamic_curves, file.path(artifact_dir, "\u52a8\u6001\u4efb\u52a1_\u65f6\u95f4\u4f9d\u8d56\u6027\u80fd.csv"))

cal <- bind_rows(
  transform(calibration_table(jm_valid_risk[, ncol(jm_valid_risk)], valid_base$lm_status), model = "JM"),
  transform(calibration_table(rf_valid_risk[, ncol(rf_valid_risk)], valid_base$lm_status), model = "RSFLC")
)
write_utf8(cal, file.path(artifact_dir, "\u52a8\u6001\u4efb\u52a1_\u6821\u51c6\u6570\u636e.csv"))
dca <- bind_rows(
  transform(dca_table(jm_valid_risk[, ncol(jm_valid_risk)], valid_base$lm_status), model_name = "JM"),
  transform(dca_table(rf_valid_risk[, ncol(rf_valid_risk)], valid_base$lm_status), model_name = "RSFLC")
)
write_utf8(dca, file.path(artifact_dir, "\u52a8\u6001\u4efb\u52a1_DCA\u6570\u636e.csv"))

theme_set(theme_bw(base_size = 11, base_family = "Microsoft YaHei"))
p_cal <- ggplot(cal, aes(predicted, observed, colour = model)) +
  geom_abline(slope = 1, intercept = 0, linetype = 2, colour = "grey50") +
  geom_line(linewidth = 0.8) + geom_point(size = 2) + coord_equal() +
  labs(x = "\u9884\u6d4bu=1\u6761\u4ef6\u6b7b\u4ea1\u98ce\u9669", y = "\u89c2\u5bdf\u6b7b\u4ea1\u7387", colour = NULL)
ggsave(file.path(artifact_dir, "\u56fe5-12_\u52a8\u6001\u4efb\u52a1\u9a8c\u8bc1\u96c6u1\u6761\u4ef6\u98ce\u9669\u6821\u51c6\u56fe.png"),
       p_cal, width = 7.0, height = 5.6, dpi = 320, bg = "white")

dca_long <- dca %>% pivot_longer(c(model, treat_all, treat_none), names_to = "strategy", values_to = "net_benefit")
dca_long$curve <- ifelse(dca_long$strategy == "model", dca_long$model_name, dca_long$strategy)
p_dca <- ggplot(dca_long, aes(threshold, net_benefit, colour = curve,
                              linetype = strategy == "model")) +
  geom_line(linewidth = 0.8) +
  scale_linetype_manual(values = c(`TRUE` = 1, `FALSE` = 2), guide = "none") +
  coord_cartesian(xlim = c(0.01, 0.30), ylim = c(-0.02, 0.10)) +
  labs(x = "\u9608\u503c\u6982\u7387", y = "\u51c0\u53d7\u76ca", colour = NULL)
ggsave(file.path(artifact_dir, "\u56fe5-13_\u52a8\u6001\u4efb\u52a1\u9a8c\u8bc1\u96c6\u51b3\u7b56\u66f2\u7ebf.png"),
       p_dca, width = 7.2, height = 5.6, dpi = 320, bg = "white")

static_curves <- read.csv(file.path(artifact_dir, "\u9759\u6001\u4efb\u52a1_\u65f6\u95f4\u4f9d\u8d56\u6027\u80fd.csv"), check.names = FALSE)
p_static <- static_curves %>% pivot_longer(c(AUC, Brier), names_to = "metric", values_to = "value") %>%
  ggplot(aes(day, value, colour = model)) + geom_line(linewidth = 0.8) +
  facet_wrap(~metric, scales = "free_y") + labs(x = "\u5165\u9662\u540e\u5929\u6570", y = NULL, title = "A  \u9759\u6001\u4efb\u52a1")
p_dynamic <- dynamic_curves %>% pivot_longer(c(AUC, Brier), names_to = "metric", values_to = "value") %>%
  ggplot(aes(day, value, colour = model)) + geom_line(linewidth = 0.8) +
  facet_wrap(~metric, scales = "free_y") + labs(x = "\u5165\u9662\u540e\u5929\u6570", y = NULL, title = "B  \u7b2c5\u65e5landmark\u52a8\u6001\u4efb\u52a1")
ggsave(file.path(artifact_dir, "\u56fe5-14_\u4e24\u7c7b\u4efb\u52a1\u65f6\u95f4\u4f9d\u8d56AUC\u4e0eBrier\u66f2\u7ebf.png"),
       p_static / p_dynamic, width = 9.0, height = 8.2, dpi = 320, bg = "white")

static_perf <- read.csv(file.path(artifact_dir, "\u88685-4A_\u9759\u6001\u4efb\u52a1\u5185\u90e8\u7559\u51fa\u9a8c\u8bc1\u96c6\u6027\u80fd.csv"), check.names = FALSE)
all_perf <- bind_rows(
  static_perf %>% transmute(task = "Static", model, C_index = validation_C_index,
                            AUC = AUC_u1, Brier = Brier_u1, IBS = IBS_0_1),
  table_5_4b %>% transmute(task = "Landmark day 5", model, C_index = validation_C_index,
                           AUC = AUC_u1_given_uL, Brier = Brier_u1_given_uL, IBS = IBS_uL_1)
)
write_utf8(all_perf, file.path(artifact_dir, "\u56db\u6a21\u578b\u5206\u4efb\u52a1\u9a8c\u8bc1\u6027\u80fd\u6c47\u603b.csv"))

log_progress("COMBINE_OK", sprintf("common validation n=%d; JM_C=%.4f; RSFLC_C=%.4f",
                                   length(common_valid), jm_valid_met$cindex, rf_valid_met$cindex))
cat("COMBINE_OK\n")
