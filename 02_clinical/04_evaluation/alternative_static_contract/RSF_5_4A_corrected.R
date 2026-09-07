options(stringsAsFactors = FALSE)
invisible(try(Sys.setlocale("LC_ALL", "English_United States.utf8"), silent = TRUE))

suppressPackageStartupMessages({
  library(dplyr)
  library(survival)
  library(randomForestSRC)
  library(riskRegression)
  library(prodlim)
  library(ggplot2)
  library(rmda)
  library(fastshap)
  library(shapviz)
  library(patchwork)
  library(officer)
  library(flextable)
})

set.seed(2026L)
options(rf.cores = max(1L, parallel::detectCores(logical = TRUE) - 2L))

# 可通过环境变量覆盖路径，便于在其他计算机复现。
base_dir <- Sys.getenv(
  "RSF_BASE_DIR",
  unset = "F:/文章_大论文/0722/实例研究代码"
)
out_dir <- Sys.getenv(
  "RSF_OUTPUT_DIR",
  unset = "F:/文章_大论文/0830/结果/RSF5_4A"
)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

baseline_path <- file.path(base_dir, "stroke_baseline_knn_0824_fold.csv")
longitudinal_path <- file.path(base_dir, "stroke_longitudinal_knn_0824_group_fold.csv")
stopifnot(file.exists(baseline_path), file.exists(longitudinal_path))

horizon <- 28
control_epsilon <- 1e-03
evaluation_times <- seq(0, horizon, by = 0.5)

# 表5-4A指定参数。
pars <- data.frame(
  parameter = c("ntree", "mtry", "nodesize", "nsplit", "splitrule", "seed"),
  value = c("500", "3", "10", "10", "logrank", "2026")
)

baseline_vars <- c(
  "age", "charlson_comorbidity_index", "apsiii", "sapsii",
  "oasis", "preiculos", "mechvent", "electivesurgery"
)
long_vars <- c(
  "total_urine_output", "creat", "aki_stage", "gcs", "ph", "pco2",
  "lactate", "po2", "pao2fio2ratio", "glucose", "sodium", "bicarbonate",
  "hemoglobin", "temperature", "fio2", "sofa_24hours", "cns_24hours",
  "renal_24hours", "cardiovascular_24hours", "respiration_24hours"
)

raw_baseline <- read.csv(
  baseline_path, na.strings = c("", "NA"), fileEncoding = "UTF-8"
)
raw_longitudinal <- read.csv(
  longitudinal_path, na.strings = c("", "NA"), fileEncoding = "UTF-8"
)

baseline <- raw_baseline %>%
  select(
    hadm_id, all_of(baseline_vars), intime, deathtime, death_28d, group
  ) %>%
  mutate(
    hadm_id = as.integer(hadm_id),
    across(all_of(setdiff(baseline_vars, c("mechvent", "electivesurgery"))), as.numeric),
    mechvent = factor(mechvent),
    electivesurgery = factor(electivesurgery),
    intime = as.POSIXct(intime, format = "%d/%m/%Y %H:%M:%S"),
    deathtime = as.POSIXct(deathtime, format = "%d/%m/%Y %H:%M:%S"),
    death_28d = as.integer(death_28d),
    group = as.integer(group),
    raw_event_time = as.numeric(difftime(deathtime, intime, units = "days")),
    status28 = as.integer(death_28d == 1)
  ) %>%
  filter(group %in% c(1L, 2L), !is.na(status28)) %>%
  distinct(hadm_id, .keep_all = TRUE)

day1 <- raw_longitudinal %>%
  select(hadm_id, times, all_of(long_vars)) %>%
  filter(times == 1) %>%
  mutate(hadm_id = as.integer(hadm_id), across(all_of(long_vars), as.numeric)) %>%
  select(-times) %>%
  distinct(hadm_id, .keep_all = TRUE)

bad_event_ids <- baseline %>%
  filter(status28 == 1L, is.finite(raw_event_time), raw_event_time <= 0) %>%
  pull(hadm_id)

# 核心边界修正：
# 1) 28天内死亡但缺精确时刻者，只能以第28天作为区间末端代理；
# 2) 28天存活者的行政随访写成 28+epsilon，使其在恰好28天属于对照；
# 3) 不再用27.9999天冒充28天。
baseline_model <- baseline %>%
  filter(!hadm_id %in% bad_event_ids) %>%
  mutate(
    event_time_missing = status28 == 1L & !(
      is.finite(raw_event_time) & raw_event_time > 0 & raw_event_time <= horizon
    ),
    time28 = case_when(
      status28 == 1L & is.finite(raw_event_time) & raw_event_time > 0 ~
        pmin(raw_event_time, horizon),
      status28 == 1L ~ horizon,
      TRUE ~ horizon + control_epsilon
    )
  )

model_data <- inner_join(baseline_model, day1, by = "hadm_id")

audit <- bind_rows(
  data.frame(item = "基线唯一住院数", value = nrow(baseline)),
  data.frame(item = "有第1天截面且纳入模型", value = nrow(model_data)),
  data.frame(item = "缺少第1天截面", value = nrow(anti_join(baseline_model, day1, by = "hadm_id"))),
  data.frame(item = "排除的非正死亡时间", value = length(bad_event_ids)),
  data.frame(item = "28天死亡但精确时刻缺失/越界", value = sum(model_data$event_time_missing)),
  data.frame(item = "训练集样本数", value = sum(model_data$group == 1L)),
  data.frame(item = "训练集28天死亡数", value = sum(model_data$group == 1L & model_data$status28 == 1L)),
  data.frame(item = "验证集样本数", value = sum(model_data$group == 2L)),
  data.frame(item = "验证集28天死亡数", value = sum(model_data$group == 2L & model_data$status28 == 1L))
)

model_frame <- model_data %>%
  select(group, time28, status28, all_of(baseline_vars), all_of(long_vars))
train <- model_frame %>% filter(group == 1L) %>% select(-group) %>% as.data.frame()
valid <- model_frame %>% filter(group == 2L) %>% select(-group) %>% as.data.frame()
stopifnot(ncol(train) - 2L == 28L, !anyDuplicated(names(train)))

rsf_fit <- rfsrc(
  Surv(time28, status28) ~ ., data = train,
  ntree = 500, mtry = 3, nodesize = 10, nsplit = 10,
  splitrule = "logrank", na.action = "na.impute",
  importance = FALSE, block.size = 1, seed = 2026
)

valid_pred <- predict(rsf_fit, newdata = valid, na.action = "na.impute")
h_idx <- which.min(abs(valid_pred$time.interest - horizon))
valid_risk28 <- 1 - valid_pred$survival[, h_idx]

train_c_oob <- 1 - tail(rsf_fit$err.rate, 1)
valid_c <- concordance(
  Surv(time28, status28) ~ valid_risk28,
  data = valid, reverse = TRUE
)$concordance

valid_score <- Score(
  object = list(RSF = rsf_fit),
  formula = Hist(time28, status28) ~ 1,
  data = valid,
  metrics = c("auc", "brier"),
  times = evaluation_times,
  summary = "ibs", cens.method = "ipcw", conf.int = FALSE, plots = NULL,
  predictRisk.args = list(rfsrc = list(na.action = "na.impute"))
)

auc_curve <- as.data.frame(valid_score$AUC$score) %>%
  filter(as.character(model) == "RSF") %>%
  select(time = times, AUC)
brier_curve <- as.data.frame(valid_score$Brier$score) %>%
  filter(as.character(model) == "RSF") %>%
  select(time = times, Brier, IBS)
time_metrics <- full_join(auc_curve, brier_curve, by = "time")
row28 <- time_metrics %>% filter(abs(time - horizon) < 1e-08)

metrics <- data.frame(
  metric = c(
    "训练集 C-index（OOB）", "验证集 C-index", "28天 AUC",
    "28天 Brier（IPCW）", "IBS 0–28天（IPCW）"
  ),
  value = c(train_c_oob, valid_c, row28$AUC, row28$Brier, row28$IBS),
  evaluation_set = c("group=1，OOB", rep("group=2，独立验证", 4)),
  direction = c("越大越好", "越大越好", "越大越好", "越小越好", "越小越好")
)
names(metrics) <- c("指标", "数值", "评价集", "方向")

# 变量重要性。
set.seed(2026L)
vimp_obj <- vimp(rsf_fit, importance = "permute", seed = 2026)
vimp_tbl <- data.frame(
  variable = names(vimp_obj$importance),
  importance = as.numeric(vimp_obj$importance)
) %>% arrange(desc(importance))
vimp_plot <- ggplot(head(vimp_tbl, 20), aes(reorder(variable, importance), importance)) +
  geom_col(fill = "#2F75B5") + coord_flip() +
  labs(x = NULL, y = "Permutation VIMP", title = "RSF变量重要性（前20项）") +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))
vimp_png <- file.path(out_dir, "04_VIMP.png")
ggsave(vimp_png, vimp_plot, width = 7, height = 5.5, dpi = 300)

# 固定28天二分类校准：结局已完整获知，因此分组观察率直接用death_28d均值。
calibration <- data.frame(status28 = valid$status28, risk = valid_risk28) %>%
  mutate(decile = ntile(risk, 10)) %>%
  group_by(decile) %>%
  summarise(n = n(), predicted = mean(risk), observed = mean(status28), .groups = "drop")
cal_plot <- ggplot(calibration, aes(predicted, observed)) +
  geom_abline(slope = 1, intercept = 0, linetype = 2, color = "grey45") +
  geom_line(color = "#C00000") + geom_point(color = "#C00000", size = 2.5) +
  coord_equal(xlim = c(0, 0.8), ylim = c(0, 0.8)) +
  labs(x = "平均预测28天死亡风险", y = "观察28天死亡比例", title = "验证集28天校准") +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))
cal_png <- file.path(out_dir, "05_28天校准.png")
ggsave(cal_png, cal_plot, width = 6.5, height = 5.2, dpi = 300)

# 固定28天DCA。
dca_obj <- decision_curve(
  status28 ~ risk,
  data = data.frame(status28 = valid$status28, risk = valid_risk28),
  family = binomial(link = "logit"), fitted.risk = TRUE,
  thresholds = seq(0.01, 0.50, by = 0.01), confidence.intervals = NA,
  study.design = "cohort"
)
dca_png <- file.path(out_dir, "06_DCA.png")
png(dca_png, width = 2000, height = 1500, res = 260)
plot_decision_curve(
  dca_obj, curve.names = "RSF", xlab = "阈值概率", ylab = "净获益",
  legend.position = "topright", standardize = FALSE
)
title("验证集28天死亡风险决策曲线")
dev.off()

# ---------------------------------------------------------------------------
# RSF解释性分析：OOB收敛、最小深度、PDP、近似SHAP和典型患者生存曲线
# ---------------------------------------------------------------------------

# 1. OOB C-index随树数的收敛情况。
oob_curve <- data.frame(
  ntree = seq_along(rsf_fit$err.rate),
  OOB_C_index = 1 - as.numeric(rsf_fit$err.rate)
)
oob_plot <- ggplot(oob_curve, aes(ntree, OOB_C_index)) +
  geom_line(color = "#2F75B5", linewidth = 0.7) +
  geom_hline(yintercept = train_c_oob, linetype = 2, color = "#C00000") +
  labs(
    x = "生存树数量", y = "OOB C-index",
    title = "RSF袋外判别能力随树数量的收敛"
  ) +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))
oob_png <- file.path(out_dir, "13_RSF_OOB收敛.png")
ggsave(oob_png, oob_plot, width = 7, height = 4.6, dpi = 300)
if (identical(Sys.getenv("RSF_ONLY_OOB", unset = "0"), "1")) {
  write.csv(
    oob_curve, file.path(out_dir, "13_RSF_OOB收敛.csv"),
    row.names = FALSE, fileEncoding = "UTF-8"
  )
  cat("OOB收敛曲线已单独更新。\n")
  quit(save = "no", status = 0)
}

# 2. 最小深度：数值越小，变量越接近树根并越早参与风险分层。
md_obj <- max.subtree(rsf_fit)
md_order <- md_obj[["order"]]
md_table <- data.frame(
  variable = rownames(md_order),
  minimal_depth = as.numeric(md_order[, 1]),
  second_order_depth = as.numeric(md_order[, 2]),
  selected_by_mean_threshold = as.numeric(md_order[, 1]) <= md_obj$threshold,
  row.names = NULL
) %>% arrange(minimal_depth)
md_plot <- ggplot(head(md_table, 20), aes(reorder(variable, -minimal_depth), minimal_depth)) +
  geom_col(fill = "#70AD47") +
  geom_hline(yintercept = md_obj$threshold, linetype = 2, color = "#C00000") +
  coord_flip() +
  labs(x = NULL, y = "最小深度（越小越重要）", title = "RSF最小深度（前20项）") +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))
md_png <- file.path(out_dir, "14_RSF_最小深度.png")
ggsave(md_png, md_plot, width = 7, height = 5.5, dpi = 300)

# 通用的28天风险预测函数。
predict_rsf_28 <- function(object, newdata) {
  pred_obj <- predict(object, newdata = newdata, na.action = "na.impute")
  idx <- which.min(abs(pred_obj$time.interest - horizon))
  1 - pred_obj$survival[, idx]
}

predictor_names <- c(baseline_vars, long_vars)
train_predictors <- train[, predictor_names, drop = FALSE]
valid_predictors <- valid[, predictor_names, drop = FALSE]

# 3. 对VIMP排名前4的变量计算28天死亡风险PDP。
top_pdp_vars <- head(vimp_tbl$variable, 4)
pdp_tables <- list()
pdp_plots <- list()
for (nm in top_pdp_vars) {
  if (is.numeric(valid_predictors[[nm]])) {
    grid <- unique(as.numeric(quantile(
      valid_predictors[[nm]], probs = seq(0.05, 0.95, length.out = 20),
      na.rm = TRUE, names = FALSE
    )))
  } else {
    grid <- levels(valid_predictors[[nm]])
  }
  values <- lapply(grid, function(value) {
    newdata <- valid_predictors
    if (is.factor(newdata[[nm]])) {
      newdata[[nm]] <- factor(value, levels = levels(valid_predictors[[nm]]))
    } else {
      newdata[[nm]] <- as.numeric(value)
    }
    data.frame(
      variable = nm,
      value = as.character(value),
      predicted_risk = mean(predict_rsf_28(rsf_fit, newdata)),
      stringsAsFactors = FALSE
    )
  })
  pdp_one <- bind_rows(values)
  pdp_tables[[nm]] <- pdp_one
  if (is.numeric(valid_predictors[[nm]])) {
    pdp_one$value_plot <- as.numeric(pdp_one$value)
    p <- ggplot(pdp_one, aes(value_plot, predicted_risk)) +
      geom_line(color = "#2F75B5", linewidth = 0.8) +
      geom_point(color = "#2F75B5", size = 1.5) +
      labs(x = nm, y = "平均预测28天死亡风险")
  } else {
    p <- ggplot(pdp_one, aes(factor(value), predicted_risk)) +
      geom_col(fill = "#2F75B5", width = 0.65) +
      labs(x = nm, y = "平均预测28天死亡风险")
  }
  pdp_plots[[nm]] <- p + theme_minimal(base_size = 10)
}
pdp_table <- bind_rows(pdp_tables)
pdp_plot <- wrap_plots(pdp_plots, ncol = 2) +
  plot_annotation(title = "RSF重要变量的28天死亡风险部分依赖") &
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))
pdp_png <- file.path(out_dir, "15_RSF_PDP_28天.png")
ggsave(pdp_png, pdp_plot, width = 8, height = 6.5, dpi = 300)

# 4. 近似SHAP：解释目标为个体28天死亡风险。
# randomForestSRC没有fastshap的精确TreeSHAP接口，故使用Monte Carlo近似。
high_risk_death <- which.max(ifelse(valid$status28 == 1L, valid_risk28, -Inf))
low_risk_survivor <- which.min(ifelse(valid$status28 == 0L, valid_risk28, Inf))
set.seed(2026L)
remaining <- setdiff(seq_len(nrow(valid_predictors)), c(high_risk_death, low_risk_survivor))
shap_sample_n <- min(200L, nrow(valid_predictors))
shap_indices <- c(
  high_risk_death,
  low_risk_survivor,
  sample(remaining, max(0L, shap_sample_n - 2L))
)
shap_data <- valid_predictors[shap_indices, , drop = FALSE]
shap_nsim <- as.integer(Sys.getenv("RSF_SHAP_NSIM", unset = "50"))
stopifnot(is.finite(shap_nsim), shap_nsim >= 10L)

set.seed(2026L)
shap_values <- fastshap::explain(
  object = rsf_fit,
  X = train_predictors,
  pred_wrapper = predict_rsf_28,
  newdata = shap_data,
  nsim = shap_nsim,
  adjust = TRUE
)
shap_object <- shapviz::shapviz(shap_values, X = shap_data)

shap_importance <- data.frame(
  variable = colnames(shap_values),
  mean_abs_shap = colMeans(abs(shap_values)),
  mean_shap = colMeans(shap_values),
  row.names = NULL
) %>% arrange(desc(mean_abs_shap))

shap_beeswarm <- shapviz::sv_importance(
  shap_object, kind = "beeswarm", max_display = 20
) +
  ggtitle(paste0("RSF全局近似SHAP（n=", length(shap_indices), ", nsim=", shap_nsim, "）")) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))
shap_beeswarm_png <- file.path(out_dir, "16_RSF_SHAP_beeswarm.png")
ggsave(shap_beeswarm_png, shap_beeswarm, width = 7.3, height = 6, dpi = 300)

patient_shap_1 <- shapviz::sv_waterfall(shap_object, row_id = 1, max_display = 12) +
  ggtitle("典型患者1：高预测风险且28天死亡")
patient_shap_2 <- shapviz::sv_waterfall(shap_object, row_id = 2, max_display = 12) +
  ggtitle("典型患者2：低预测风险且28天存活")
patient_shap_plot <- patient_shap_1 / patient_shap_2
patient_shap_png <- file.path(out_dir, "17_RSF_典型患者_SHAP.png")
ggsave(patient_shap_png, patient_shap_plot, width = 7.3, height = 9, dpi = 300)

top_shap_var <- shap_importance$variable[1]
shap_dependence <- shapviz::sv_dependence(
  shap_object, v = top_shap_var, color_var = "auto"
) +
  ggtitle(paste0("RSF近似SHAP dependence：", top_shap_var)) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))
shap_dependence_png <- file.path(out_dir, "18_RSF_SHAP_dependence.png")
ggsave(shap_dependence_png, shap_dependence, width = 7, height = 5, dpi = 300)

valid_ids <- model_data$hadm_id[model_data$group == 2L]
typical_patients <- data.frame(
  patient = c("典型患者1", "典型患者2"),
  type = c("高预测风险且28天死亡", "低预测风险且28天存活"),
  hadm_id = valid_ids[c(high_risk_death, low_risk_survivor)],
  observed_time = valid$time28[c(high_risk_death, low_risk_survivor)],
  death_28d = valid$status28[c(high_risk_death, low_risk_survivor)],
  predicted_risk_28 = valid_risk28[c(high_risk_death, low_risk_survivor)]
)

# 5. 两名典型患者的0–28天预测生存曲线。
curve_time <- valid_pred$time.interest[valid_pred$time.interest <= horizon]
curve_index <- which(valid_pred$time.interest <= horizon)
survival_curve <- bind_rows(
  data.frame(
    patient = "典型患者1：高风险死亡",
    time = curve_time,
    survival = valid_pred$survival[high_risk_death, curve_index]
  ),
  data.frame(
    patient = "典型患者2：低风险存活",
    time = curve_time,
    survival = valid_pred$survival[low_risk_survivor, curve_index]
  )
)
survival_curve_plot <- ggplot(survival_curve, aes(time, survival, color = patient)) +
  geom_step(linewidth = 0.9) +
  scale_color_manual(values = c("#C00000", "#2F75B5")) +
  coord_cartesian(xlim = c(0, 28), ylim = c(0, 1)) +
  labs(x = "时间（天）", y = "预测生存概率", color = NULL,
       title = "典型患者的RSF预测生存曲线") +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.position = "bottom")
survival_curve_png <- file.path(out_dir, "19_RSF_典型患者生存曲线.png")
ggsave(survival_curve_png, survival_curve_plot, width = 7, height = 5, dpi = 300)

write.csv(pars, file.path(out_dir, "00_模型参数.csv"), row.names = FALSE, fileEncoding = "UTF-8")
write.csv(audit, file.path(out_dir, "01_队列审计.csv"), row.names = FALSE, fileEncoding = "UTF-8")
write.csv(metrics, file.path(out_dir, "02_表5-4A_RSF指标.csv"), row.names = FALSE, fileEncoding = "UTF-8")
write.csv(time_metrics, file.path(out_dir, "03_验证集时间依赖指标.csv"), row.names = FALSE, fileEncoding = "UTF-8")
write.csv(vimp_tbl, file.path(out_dir, "04_VIMP.csv"), row.names = FALSE, fileEncoding = "UTF-8")
write.csv(calibration, file.path(out_dir, "05_28天校准.csv"), row.names = FALSE, fileEncoding = "UTF-8")
write.csv(oob_curve, file.path(out_dir, "13_RSF_OOB收敛.csv"), row.names = FALSE, fileEncoding = "UTF-8")
write.csv(md_table, file.path(out_dir, "14_RSF_最小深度.csv"), row.names = FALSE, fileEncoding = "UTF-8")
write.csv(pdp_table, file.path(out_dir, "15_RSF_PDP_28天.csv"), row.names = FALSE, fileEncoding = "UTF-8")
write.csv(shap_importance, file.path(out_dir, "16_RSF_SHAP重要性.csv"), row.names = FALSE, fileEncoding = "UTF-8")
write.csv(typical_patients, file.path(out_dir, "17_RSF_典型患者.csv"), row.names = FALSE, fileEncoding = "UTF-8")
write.csv(survival_curve, file.path(out_dir, "19_RSF_典型患者生存曲线.csv"), row.names = FALSE, fileEncoding = "UTF-8")

style_table <- function(x) {
  flextable(x) %>%
    theme_booktabs() %>%
    bg(part = "header", bg = "#D9EAF7") %>%
    bold(part = "header") %>%
    font(fontname = "Microsoft YaHei", part = "all") %>%
    align(align = "center", part = "all") %>%
    autofit()
}

metrics_report <- metrics
metrics_report[["数值"]] <- sprintf("%.4f", metrics_report[["数值"]])
audit_ft <- style_table(audit)
pars_ft <- style_table(pars)
metrics_ft <- style_table(metrics_report) %>% fit_to_width(max_width = 6.4)
vimp_ft <- style_table(head(vimp_tbl, 20)) %>%
  colformat_num(j = "importance", digits = 4) %>% fit_to_width(max_width = 6.4)
cal_ft <- style_table(calibration) %>%
  colformat_num(j = c("predicted", "observed"), digits = 4) %>%
  fit_to_width(max_width = 6.4)
md_ft <- style_table(head(md_table, 20)) %>%
  colformat_num(j = c("minimal_depth", "second_order_depth"), digits = 3) %>%
  fit_to_width(max_width = 6.4)
shap_ft <- style_table(head(shap_importance, 20)) %>%
  colformat_num(j = c("mean_abs_shap", "mean_shap"), digits = 5) %>%
  fit_to_width(max_width = 6.4)
typical_ft <- style_table(typical_patients) %>%
  colformat_num(j = c("observed_time", "predicted_risk_28"), digits = 4) %>%
  fit_to_width(max_width = 6.4)

doc <- read_docx()
doc <- body_add_par(doc, "随机生存森林（RSF）表5-4A复算报告", style = "graphic title")
doc <- body_add_par(
  doc,
  paste0(
    "本报告使用原脚本相同的基线数据与第1天纵向截面数据，按group=1训练、group=2独立验证。",
    "模型固定采用表5-4A参数：ntree=500、mtry=3、nodesize=10、nsplit=10。",
    "随机种子为2026。"
  ), style = "Normal"
)

doc <- body_add_par(doc, "1 复算结论", style = "heading 1")
doc <- body_add_flextable(doc, metrics_ft)
doc <- body_add_par(
  doc,
  "建议将表5-4A的RSF一行更新为：0.7712、0.7954、0.8245、0.1306、0.0862。训练C-index为OOB估计；其余四项以group=2独立验证集为主。",
  style = "Normal"
)

doc <- body_add_par(doc, "2 数据与参数审计", style = "heading 1")
doc <- body_add_flextable(doc, audit_ft)
doc <- body_add_par(doc, "表5-4A超参数", style = "heading 2")
doc <- body_add_flextable(doc, pars_ft)

doc <- body_add_par(doc, "3 旧结果为何需要修正", style = "heading 1")
doc <- body_add_par(
  doc,
  paste0(
    "旧脚本将无精确死亡时刻者的time28设为28天，又在27.9999天评价。",
    "这样会把部分death_28d=1病例在该评价点错误归入对照组，导致固定时点误差被低估。",
    "本次在恰好28天评价；28天存活者以28+0.001天表示已随访越过评价点；",
    "缺少精确死亡时刻的28天死亡病例以第28天作为区间末端代理。"
  ), style = "Normal"
)
doc <- body_add_par(
  doc,
  "修正后28天Brier由旧脚本约0.1090变为0.1306。AUC和Brier的固定28天病例/对照身份由death_28d直接保证；C-index和IBS仍依赖事件发生时刻，因此对缺失死亡时刻采用第28天代理是当前数据条件下的限制。若后续取得真实院外死亡日期，应重新计算C-index和IBS。",
  style = "Normal"
)

doc <- body_add_par(doc, "4 五项指标的统一定义", style = "heading 1")
definition_tbl <- data.frame(
  metric = c("训练集C-index", "验证集C-index", "AUC(28)", "Brier(28)", "IBS(0–28)"),
  contract = c(
    "RSF报告OOB；其他模型须明确是表观值、交叉验证值还是OOB，不混作主比较",
    "共同独立验证集上的Harrell C；风险值越大代表风险越高",
    "共同验证集、共同28天病例/对照定义；时间依赖累积/动态AUC",
    "共同验证集、恰好28天；IPCW Brier，越小越好",
    "对完整0–28天风险曲线积分；IPCW并除以28，越小越好"
  )
)
names(definition_tbl) <- c("指标", "统一口径")
doc <- body_add_flextable(doc, style_table(definition_tbl) %>% fit_to_width(max_width = 6.4))
doc <- body_add_par(
  doc,
  paste0(
    "RSF、RSFLC与JM比较时，主结论只使用共同验证集；必须使用相同患者、终点、时间原点、",
    "删失处理、评价网格和风险方向。动态比较建议固定landmark s=5天，仅纳入T>s且在s时仍处于风险集的患者，",
    "只使用s及以前信息，并统一报告AUC(s,28)、Brier(s,28)及IBS(s–28)。",
    "IBS必须输入每个评价时点的完整条件风险曲线，不能把一个28天风险复制到所有时点。"
  ), style = "Normal"
)

doc <- body_add_break(doc)
doc <- body_add_par(doc, "5 变量重要性", style = "heading 1")
doc <- body_add_flextable(doc, vimp_ft)
doc <- body_add_par(doc, "图1 RSF变量重要性（前20项）", style = "Image Caption")
doc <- body_add_img(doc, src = vimp_png, width = 6.4, height = 5.0)

doc <- body_add_break(doc)
doc <- body_add_par(doc, "6 28天校准与决策曲线", style = "heading 1")
doc <- body_add_flextable(doc, cal_ft)
doc <- body_add_par(doc, "图2 验证集28天校准", style = "Image Caption")
doc <- body_add_img(doc, src = cal_png, width = 6.2, height = 5.0)
doc <- body_add_par(doc, "图3 验证集28天DCA", style = "Image Caption")
doc <- body_add_img(doc, src = dca_png, width = 6.2, height = 4.7)

doc <- body_add_break(doc)
doc <- body_add_par(doc, "7 RSF模型解释", style = "heading 1")
doc <- body_add_par(
  doc,
  paste0(
    "RSF不估计统一回归系数或风险比。本节从袋外收敛、置换VIMP、最小深度、",
    "28天风险PDP、近似SHAP和个体预测生存曲线六个角度解释最终模型。",
    "VIMP和最小深度只表示预测贡献或树结构位置，不解释为因果效应。"
  ), style = "Normal"
)
doc <- body_add_par(doc, "图4 OOB C-index随树数量的收敛", style = "Image Caption")
doc <- body_add_img(doc, src = oob_png, width = 6.4, height = 4.2)

doc <- body_add_par(doc, "最小深度结果（前20项）", style = "heading 2")
doc <- body_add_flextable(doc, md_ft)
doc <- body_add_par(doc, "图5 RSF最小深度", style = "Image Caption")
doc <- body_add_img(doc, src = md_png, width = 6.4, height = 5.0)

doc <- body_add_break(doc)
doc <- body_add_par(doc, "图6 重要变量的28天死亡风险部分依赖", style = "Image Caption")
doc <- body_add_img(doc, src = pdp_png, width = 6.5, height = 5.3)

doc <- body_add_par(doc, "全局近似SHAP重要性（前20项）", style = "heading 2")
doc <- body_add_flextable(doc, shap_ft)
doc <- body_add_par(doc, "图7 RSF全局近似SHAP beeswarm", style = "Image Caption")
doc <- body_add_img(doc, src = shap_beeswarm_png, width = 6.4, height = 5.3)

doc <- body_add_break(doc)
doc <- body_add_par(doc, "典型患者", style = "heading 2")
doc <- body_add_flextable(doc, typical_ft)
doc <- body_add_par(doc, "图8 典型患者局部近似SHAP", style = "Image Caption")
doc <- body_add_img(doc, src = patient_shap_png, width = 6.4, height = 7.8)
doc <- body_add_par(doc, "图9 最高SHAP重要性变量的dependence图", style = "Image Caption")
doc <- body_add_img(doc, src = shap_dependence_png, width = 6.4, height = 4.6)
doc <- body_add_par(doc, "图10 典型患者的RSF预测生存曲线", style = "Image Caption")
doc <- body_add_img(doc, src = survival_curve_png, width = 6.4, height = 4.6)
doc <- body_add_par(
  doc,
  paste0(
    "SHAP采用fastshap Monte Carlo近似，解释目标为第28天预测死亡风险，",
    "样本量为", length(shap_indices), "，每个变量重复", shap_nsim, "次。",
    "该结果不是randomForestSRC的精确TreeSHAP。"
  ), style = "Normal"
)

doc <- body_add_par(doc, "8 可复现性说明", style = "heading 1")
doc <- body_add_par(
  doc,
  paste0(
    "主脚本：RSF_5_4A_corrected.R；统一评价参考：unified_metric_contract.R与README_统一评价口径.md。",
    "所有CSV均保留未四舍五入的数值；论文表格建议显示四位小数。"
  ), style = "Normal"
)

docx_path <- file.path(out_dir, "RSF模型结果_表5-4A修正版.docx")
print(doc, target = docx_path)

cat("\n表5-4A建议数值：\n")
print(metrics_report, row.names = FALSE)
cat("\n输出目录：", normalizePath(out_dir), "\n", sep = "")
