# =============================================================================
# RSF 结果输出：8 个基线协变量 + 第 1 天 20 条轨迹截面
# group=1 全部训练（约 70%），group=2 全部外验证；fold 不参与本脚本
# 可独立 source，不依赖 0826_RSF_超参数筛选.R
# =============================================================================
library(dplyr)
library(survival)
library(randomForestSRC)
library(riskRegression)
library(prodlim)
library(ggplot2)
library(fastshap)
library(shapviz)
library(rmda)
library(survminer)
library(patchwork)
library(officer)
library(flextable)

thread_number <- max(1L, parallel::detectCores(logical = TRUE) - 2L)
options(rf.cores = thread_number)

base_dir <- "F:/文章_大论文/0722/实例研究代码"
out_dir <- file.path(base_dir, "执行")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
baseline_path <- file.path(base_dir, "stroke_baseline_knn_0824_fold.csv")
longitudinal_path <- file.path(base_dir, "stroke_longitudinal_knn_0824_group_fold.csv")
fig_dir <- file.path(out_dir, "图像_RSF")
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)

# combo 2：ntree=500, mtry=3, nodesize=10, nsplit=10
# 该组合来自 0826_RSF_超参数筛选.R / 表5-3A 五折搜索后锁定，本脚本不再搜网格。
best_ntree <- 500
best_mtry <- 3
best_nodesize <- 10
best_nsplit <- 10

baseline_cols <- c(
  "hadm_id", "age", "charlson_comorbidity_index", "apsiii", "sapsii",
  "oasis", "preiculos", "mechvent", "electivesurgery",
  "intime", "deathtime", "death_28d", "group"
)
long_vars <- c(
  "total_urine_output", "creat", "aki_stage", "gcs", "ph", "pco2",
  "lactate", "po2", "pao2fio2ratio", "glucose", "sodium", "bicarbonate",
  "hemoglobin", "temperature", "fio2", "sofa_24hours", "cns_24hours",
  "renal_24hours", "cardiovascular_24hours", "respiration_24hours"
)
longitudinal_cols <- c("hadm_id", "times", long_vars)

evaluation_horizon <- 28 - 1e-04
evaluation_times <- unique(c(seq(0, 27.5, by = 0.5), evaluation_horizon))

# ---------------------------------------------------------------------------
# 读数并拼成建模表：数据已预处理，不再做入组筛选
# 仅按 times==1 取第 1 天轨迹截面，再与基线按 hadm_id 拼接
# ---------------------------------------------------------------------------
stroke_baseline <- read.csv(
  baseline_path,
  stringsAsFactors = FALSE,
  na.strings = c("", "NA"),
  fileEncoding = "UTF-8"
)
stroke_longitudinal <- read.csv(
  longitudinal_path,
  stringsAsFactors = FALSE,
  na.strings = c("", "NA"),
  fileEncoding = "UTF-8"
)

baseline_data <- stroke_baseline %>%
  select(all_of(baseline_cols)) %>%
  mutate(
    hadm_id = as.integer(hadm_id),
    age = as.numeric(age),
    charlson_comorbidity_index = as.numeric(charlson_comorbidity_index),
    apsiii = as.numeric(apsiii),
    sapsii = as.numeric(sapsii),
    oasis = as.numeric(oasis),
    preiculos = as.numeric(preiculos),
    mechvent = factor(mechvent),
    electivesurgery = factor(electivesurgery),
    intime = as.POSIXct(intime, format = "%d/%m/%Y %H:%M:%S"),
    deathtime = as.POSIXct(deathtime, format = "%d/%m/%Y %H:%M:%S"),
    death_28d = as.integer(death_28d),
    group = as.integer(group),
    time28 = as.numeric(difftime(deathtime, intime, units = "days")),
    time28 = ifelse(is.na(time28), 28, pmin(time28, 28)),
    status28 = as.integer(death_28d == 1)
  )

longitudinal_day1 <- stroke_longitudinal %>%
  select(all_of(longitudinal_cols)) %>%
  filter(times == 1) %>%
  mutate(
    hadm_id = as.integer(hadm_id),
    across(all_of(long_vars), as.numeric)
  ) %>%
  select(-times)

rsf_data <- baseline_data %>%
  left_join(longitudinal_day1, by = "hadm_id") %>%
  select(-hadm_id, -intime, -deathtime, -death_28d) %>%
  as.data.frame()

stopifnot("group" %in% names(rsf_data))
train_data <- rsf_data[rsf_data$group == 1L, ]
test_data <- rsf_data[rsf_data$group == 2L, ]
stopifnot(nrow(train_data) > 0L, nrow(test_data) > 0L)
train_data$group <- NULL
test_data$group <- NULL

n_predictor <- ncol(train_data) - 2L
stopifnot(n_predictor == 28L)

cat(
  "训练集 group=1 n =", nrow(train_data),
  "| 死亡 =", sum(train_data$status28),
  sprintf("（%.1f%%）", 100 * mean(train_data$status28)),
  "| 验证集 group=2 n =", nrow(test_data),
  "| 死亡 =", sum(test_data$status28),
  sprintf("（%.1f%%）", 100 * mean(test_data$status28)),
  "| 协变量数 =", n_predictor, "（8基线+20轨迹截面）\n"
)
flush.console()

# ---------------------------------------------------------------------------
# 全部训练集拟合，全部验证集预测
# ---------------------------------------------------------------------------
rsf_model <- randomForestSRC::rfsrc(
  Surv(time28, status28) ~ .,
  data = train_data,
  ntree = best_ntree,
  mtry = best_mtry,
  nodesize = best_nodesize,
  nsplit = best_nsplit,
  splitrule = "logrank",
  na.action = "na.impute",
  importance = FALSE,
  seed = 2026
)

test_prediction <- predict(
  rsf_model,
  newdata = test_data,
  na.action = "na.impute"
)
test_time_index <- which.min(abs(test_prediction$time.interest - 28))
test_probability <- 1 - test_prediction$survival[, test_time_index]

train_time_index <- which.min(abs(rsf_model$time.interest - 28))
train_probability <- 1 - rsf_model$survival.oob[, train_time_index]
train_cindex <- survival::concordance(
  Surv(time28, status28) ~ probability,
  data = data.frame(
    time28 = train_data$time28,
    status28 = train_data$status28,
    probability = train_probability
  ),
  reverse = TRUE
)$concordance
test_cindex <- survival::concordance(
  Surv(time28, status28) ~ probability,
  data = data.frame(
    time28 = test_data$time28,
    status28 = test_data$status28,
    probability = test_probability
  ),
  reverse = TRUE
)$concordance
oob_cindex <- 1 - tail(rsf_model$err.rate, 1)

train_rsf_score <- riskRegression::Score(
  object = list(RSF = rsf_model),
  formula = Hist(time28, status28) ~ 1,
  data = train_data,
  metrics = c("auc", "brier"),
  times = evaluation_times,
  summary = "ibs",
  cens.method = "ipcw",
  conf.int = FALSE,
  plots = NULL,
  predictRisk.args = list(rfsrc = list(na.action = "na.impute"))
)
test_rsf_score <- riskRegression::Score(
  object = list(RSF = rsf_model),
  formula = Hist(time28, status28) ~ 1,
  data = test_data,
  metrics = c("auc", "brier"),
  times = evaluation_times,
  summary = "ibs",
  cens.method = "ipcw",
  conf.int = FALSE,
  plots = NULL,
  predictRisk.args = list(rfsrc = list(na.action = "na.impute"))
)

train_auc_score_table <- as.data.frame(train_rsf_score$AUC$score)
train_brier_score_table <- as.data.frame(train_rsf_score$Brier$score)
test_auc_score_table <- as.data.frame(test_rsf_score$AUC$score)
test_brier_score_table <- as.data.frame(test_rsf_score$Brier$score)

pick_score <- function(tbl, col) {
  tbl[[col]][
    as.character(tbl$model) == "RSF" &
      abs(tbl$times - evaluation_horizon) < 1e-08
  ]
}

performance_table <- data.frame(
  metric = c(
    "C-index", "28-day AUC",
    "28-day Brier (IPCW)", "IBS 0-28 days (IPCW)"
  ),
  training_set = round(
    c(
      oob_cindex,
      pick_score(train_auc_score_table, "AUC"),
      pick_score(train_brier_score_table, "Brier"),
      pick_score(train_brier_score_table, "IBS")
    ),
    4
  ),
  test_set = round(
    c(
      test_cindex,
      pick_score(test_auc_score_table, "AUC"),
      pick_score(test_brier_score_table, "Brier"),
      pick_score(test_brier_score_table, "IBS")
    ),
    4
  ),
  check.names = FALSE
)
parameter_table <- data.frame(
  parameter = c("ntree", "mtry", "nodesize", "nsplit"),
  value = c(best_ntree, best_mtry, best_nodesize, best_nsplit)
)
print(parameter_table, row.names = FALSE)
print(performance_table, row.names = FALSE)
write.csv(
  parameter_table,
  file.path(fig_dir, "00_parameters.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)
write.csv(
  performance_table,
  file.path(fig_dir, "00_performance.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

# ---------------------------------------------------------------------------
# 1. VIMP
# ---------------------------------------------------------------------------
set.seed(2026)
vimp_result <- randomForestSRC::vimp(
  rsf_model,
  importance = "permute",
  seed = 2026
)

vimp_table <- data.frame(
  variable = names(vimp_result$importance),
  importance = as.numeric(vimp_result$importance),
  row.names = NULL
) %>%
  arrange(desc(importance))

vimp_plot <- ggplot2::ggplot(
  head(vimp_table, 20),
  ggplot2::aes(x = reorder(variable, importance), y = importance)
) +
  ggplot2::geom_col(fill = "#2F75B5") +
  ggplot2::coord_flip() +
  ggplot2::labs(
    x = NULL,
    y = "Permutation VIMP",
    title = "RSF变量重要性（前20项）"
  ) +
  ggplot2::theme_minimal(base_size = 12) +
  ggplot2::theme(
    plot.title = ggplot2::element_text(hjust = 0.5, face = "bold")
  )

vimp_file <- file.path(fig_dir, "01_VIMP.png")
ggplot2::ggsave(vimp_file, vimp_plot, width = 7, height = 5.5, dpi = 300)
write.csv(vimp_table, file.path(fig_dir, "01_VIMP.csv"), row.names = FALSE, fileEncoding = "UTF-8")

# ---------------------------------------------------------------------------
# 2–4. SHAP
# ---------------------------------------------------------------------------
predict_rsf_28 <- function(object, newdata) {
  prediction_object <- predict(
    object,
    newdata = newdata,
    na.action = "na.impute"
  )
  prediction_time_index <- which.min(
    abs(prediction_object$time.interest - 28)
  )
  1 - prediction_object$survival[, prediction_time_index]
}

train_predictors <- as.data.frame(rsf_model$xvar)
test_predictors <- test_data %>%
  select(all_of(names(train_predictors)))

fill_missing_like_train <- function(newdata, template) {
  out <- newdata
  for (nm in names(template)) {
    if (is.numeric(template[[nm]])) {
      fill_value <- median(template[[nm]], na.rm = TRUE)
      out[[nm]][is.na(out[[nm]])] <- fill_value
    } else {
      fill_value <- names(sort(table(template[[nm]]), decreasing = TRUE))[1]
      out[[nm]][is.na(out[[nm]])] <- fill_value
      if (is.factor(template[[nm]])) {
        out[[nm]] <- factor(out[[nm]], levels = levels(template[[nm]]))
      }
    }
  }
  out
}

test_predictors <- fill_missing_like_train(test_predictors, train_predictors)

high_risk_death_index <- which.max(
  ifelse(test_data$status28 == 1L, test_probability, -Inf)
)
low_risk_survivor_index <- which.min(
  ifelse(test_data$status28 == 0L, test_probability, Inf)
)

set.seed(2026)
remaining_indices <- setdiff(
  seq_len(nrow(test_predictors)),
  c(high_risk_death_index, low_risk_survivor_index)
)
shap_indices <- c(
  high_risk_death_index,
  low_risk_survivor_index,
  sample(remaining_indices, min(198, length(remaining_indices)))
)
shap_data <- test_predictors[shap_indices, , drop = FALSE]

set.seed(2026)
shap_values <- fastshap::explain(
  object = rsf_model,
  X = train_predictors,
  pred_wrapper = predict_rsf_28,
  newdata = shap_data,
  nsim = 20,
  adjust = TRUE
)

shap_object <- shapviz::shapviz(shap_values, X = shap_data)

shap_beeswarm_plot <- shapviz::sv_importance(
  shap_object,
  kind = "beeswarm",
  max_display = 20
) +
  ggplot2::ggtitle("RSF全局SHAP beeswarm") +
  ggplot2::theme(
    plot.title = ggplot2::element_text(hjust = 0.5, face = "bold")
  )

shap_beeswarm_file <- file.path(fig_dir, "02_SHAP_beeswarm.png")
ggplot2::ggsave(
  shap_beeswarm_file,
  shap_beeswarm_plot,
  width = 7,
  height = 6,
  dpi = 300
)

patient_shap_plot_1 <- shapviz::sv_waterfall(
  shap_object,
  row_id = 1,
  max_display = 12
) +
  ggplot2::ggtitle("典型患者1：高风险死亡")

patient_shap_plot_2 <- shapviz::sv_waterfall(
  shap_object,
  row_id = 2,
  max_display = 12
) +
  ggplot2::ggtitle("典型患者2：低风险存活")

patient_shap_plot <- patient_shap_plot_1 / patient_shap_plot_2
patient_shap_file <- file.path(fig_dir, "03_典型患者_SHAP.png")
ggplot2::ggsave(
  patient_shap_file,
  patient_shap_plot,
  width = 7,
  height = 9,
  dpi = 300
)

typical_patient_table <- data.frame(
  patient = c("典型患者1", "典型患者2"),
  type = c("高预测风险且28天死亡", "低预测风险且28天存活"),
  test_row = c(high_risk_death_index, low_risk_survivor_index),
  observed_time = round(
    test_data$time28[c(high_risk_death_index, low_risk_survivor_index)],
    2
  ),
  death_28d = test_data$status28[
    c(high_risk_death_index, low_risk_survivor_index)
  ],
  predicted_risk = round(
    test_probability[c(high_risk_death_index, low_risk_survivor_index)],
    4
  )
)
write.csv(
  typical_patient_table,
  file.path(fig_dir, "03_典型患者.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

top_shap_variable <- vimp_table$variable[1]
shap_dependence_plot <- shapviz::sv_dependence(
  shap_object,
  v = top_shap_variable,
  color_var = "auto"
) +
  ggplot2::ggtitle(paste0("SHAP dependence：", top_shap_variable)) +
  ggplot2::theme(
    plot.title = ggplot2::element_text(hjust = 0.5, face = "bold")
  )

shap_dependence_file <- file.path(fig_dir, "04_SHAP_dependence.png")
ggplot2::ggsave(
  shap_dependence_file,
  shap_dependence_plot,
  width = 7,
  height = 5,
  dpi = 300
)

# ---------------------------------------------------------------------------
# 5. 28 天校准图
# ---------------------------------------------------------------------------
calibration_data <- data.frame(
  time28 = test_data$time28,
  status28 = test_data$status28,
  predicted_risk = test_probability
) %>%
  mutate(risk_decile = dplyr::ntile(predicted_risk, 10))

calibration_table <- data.frame(
  risk_decile = 1:10,
  n = NA_integer_,
  predicted_risk = NA_real_,
  observed_risk = NA_real_,
  observed_low = NA_real_,
  observed_high = NA_real_
)

for (i in 1:10) {
  calibration_group <- calibration_data %>%
    filter(risk_decile == i)
  calibration_fit <- survival::survfit(
    Surv(time28, status28) ~ 1,
    data = calibration_group
  )
  calibration_summary <- summary(
    calibration_fit,
    times = evaluation_horizon,
    extend = TRUE
  )

  calibration_table$n[i] <- nrow(calibration_group)
  calibration_table$predicted_risk[i] <- mean(
    calibration_group$predicted_risk
  )
  calibration_table$observed_risk[i] <- 1 - calibration_summary$surv
  calibration_table$observed_low[i] <- 1 - calibration_summary$upper
  calibration_table$observed_high[i] <- 1 - calibration_summary$lower
}

calibration_plot <- ggplot2::ggplot(
  calibration_table,
  ggplot2::aes(x = predicted_risk, y = observed_risk)
) +
  ggplot2::geom_abline(
    intercept = 0, slope = 1, linetype = 2, color = "grey40"
  ) +
  ggplot2::geom_errorbar(
    ggplot2::aes(ymin = observed_low, ymax = observed_high),
    width = 0.01,
    color = "#2F75B5"
  ) +
  ggplot2::geom_point(size = 2.8, color = "#C00000") +
  ggplot2::geom_line(color = "#C00000") +
  ggplot2::coord_equal(xlim = c(0, 1), ylim = c(0, 1)) +
  ggplot2::labs(
    x = "平均预测28天死亡风险",
    y = "KM观察28天死亡风险",
    title = "测试集28天校准图"
  ) +
  ggplot2::theme_minimal(base_size = 12) +
  ggplot2::theme(
    plot.title = ggplot2::element_text(hjust = 0.5, face = "bold")
  )

calibration_file <- file.path(fig_dir, "05_28天校准.png")
ggplot2::ggsave(
  calibration_file,
  calibration_plot,
  width = 6.5,
  height = 5.5,
  dpi = 300
)
write.csv(
  calibration_table,
  file.path(fig_dir, "05_28天校准.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

# ---------------------------------------------------------------------------
# 6. DCA
# ---------------------------------------------------------------------------
dca_data <- data.frame(
  death_28d = test_data$status28,
  rsf_risk = test_probability
)

dca_result <- rmda::decision_curve(
  death_28d ~ rsf_risk,
  data = dca_data,
  family = binomial(link = "logit"),
  fitted.risk = TRUE,
  thresholds = seq(0.01, 0.50, by = 0.01),
  confidence.intervals = NA,
  study.design = "cohort"
)

dca_file <- file.path(fig_dir, "06_DCA.png")
png(dca_file, width = 2000, height = 1500, res = 260)
rmda::plot_decision_curve(
  dca_result,
  curve.names = "RSF",
  xlab = "Threshold probability",
  ylab = "Net benefit",
  legend.position = "topright",
  standardize = FALSE
)
title("测试集28天死亡风险决策曲线")
dev.off()

# ---------------------------------------------------------------------------
# 7. KM 风险分层
# ---------------------------------------------------------------------------
km_data <- data.frame(
  time28 = test_data$time28,
  status28 = test_data$status28,
  predicted_risk = test_probability
)
km_data$risk_group <- factor(
  ifelse(
    km_data$predicted_risk >= median(km_data$predicted_risk),
    "高风险组",
    "低风险组"
  ),
  levels = c("低风险组", "高风险组")
)

km_fit <- survival::survfit(
  Surv(time28, status28) ~ risk_group,
  data = km_data
)

km_plot <- survminer::ggsurvplot(
  km_fit,
  data = km_data,
  risk.table = TRUE,
  pval = TRUE,
  conf.int = FALSE,
  xlim = c(0, 28),
  break.time.by = 7,
  xlab = "时间（天）",
  ylab = "生存概率",
  legend.title = "风险分层",
  legend.labs = c("低风险组", "高风险组"),
  palette = c("#2F75B5", "#C00000"),
  ggtheme = ggplot2::theme_minimal(base_size = 12)
)

km_file <- file.path(fig_dir, "07_KM风险分层.png")
png(km_file, width = 2000, height = 1700, res = 260)
print(km_plot)
dev.off()

# ---------------------------------------------------------------------------
# 生成 Word 报告（结构与 图像_RSF/RSF模型结果.docx 一致）
# ---------------------------------------------------------------------------
style_ft <- function(ft) {
  ft %>%
    flextable::theme_booktabs() %>%
    flextable::bg(part = "header", bg = "#D9EAF7") %>%
    flextable::bold(part = "header") %>%
    flextable::font(fontname = "Microsoft YaHei", part = "all") %>%
    flextable::align(align = "center", part = "all") %>%
    flextable::autofit()
}

parameter_ft <- style_ft(flextable::flextable(parameter_table))
performance_ft <- style_ft(flextable::flextable(performance_table)) %>%
  flextable::fit_to_width(max_width = 6.4)
vimp_ft <- style_ft(flextable::flextable(vimp_table)) %>%
  flextable::colformat_num(j = "importance", digits = 4) %>%
  flextable::autofit() %>%
  flextable::paginate(init = TRUE, hdr_ftr = TRUE)
typical_patient_ft <- style_ft(flextable::flextable(typical_patient_table)) %>%
  flextable::fit_to_width(max_width = 6.4)
calibration_ft <- style_ft(flextable::flextable(calibration_table)) %>%
  flextable::colformat_num(
    j = c("predicted_risk", "observed_risk", "observed_low", "observed_high"),
    digits = 4
  ) %>%
  flextable::fit_to_width(max_width = 6.4)

rsf_doc <- officer::read_docx()
rsf_doc <- officer::body_add_par(
  rsf_doc,
  "随机生存森林（RSF）模型结果",
  style = "graphic title"
)
rsf_doc <- officer::body_add_par(
  rsf_doc,
  paste0(
    "结局为28天全因死亡。模型纳入8个基线协变量与ICU第1天20条轨迹截面。",
    "group=1 全部作为训练集拟合，group=2 全部作为外部验证集预测。",
    "训练集 C-index 采用 OOB 评价，验证集采用外部留出评价。",
    "AUC、28天Brier和IBS均按生存资料定义计算，其中Brier和IBS采用IPCW校正删失。"
  ),
  style = "Normal"
)

rsf_doc <- officer::body_add_par(rsf_doc, "1 最优超参数", style = "heading 1")
rsf_doc <- flextable::body_add_flextable(rsf_doc, parameter_ft)

rsf_doc <- officer::body_add_par(rsf_doc, "2 模型预测性能", style = "heading 1")
rsf_doc <- flextable::body_add_flextable(rsf_doc, performance_ft)
rsf_doc <- officer::body_add_par(
  rsf_doc,
  paste0(
    "注：training_set 为全部 group=1 拟合后的表观/OOB 性能，test_set 为全部 group=2 外验证。",
    "未死亡病例统一在第28天行政删失，因此严格的时间依赖AUC在",
    "27.9999天计算，并作为28天AUC报告；IPCW-Brier和IPCW-IBS也评价至该时点。"
  ),
  style = "Normal"
)

rsf_doc <- officer::body_add_par(rsf_doc, "3 VIMP变量重要性", style = "heading 1")
rsf_doc <- flextable::body_add_flextable(rsf_doc, vimp_ft)
rsf_doc <- officer::body_add_par(rsf_doc, "图1 VIMP变量重要性", style = "Image Caption")
rsf_doc <- officer::body_add_img(rsf_doc, src = vimp_file, width = 6.4, height = 5.0)

rsf_doc <- officer::body_add_break(rsf_doc)
rsf_doc <- officer::body_add_par(rsf_doc, "4 全局SHAP解释", style = "heading 1")
rsf_doc <- officer::body_add_par(
  rsf_doc,
  "SHAP值表示各变量对患者28天预测死亡风险的边际贡献；正值推动风险升高，负值推动风险降低。",
  style = "Normal"
)
rsf_doc <- officer::body_add_par(rsf_doc, "图2 SHAP global beeswarm", style = "Image Caption")
rsf_doc <- officer::body_add_img(
  rsf_doc, src = shap_beeswarm_file, width = 6.4, height = 5.5
)

rsf_doc <- officer::body_add_break(rsf_doc)
rsf_doc <- officer::body_add_par(rsf_doc, "图3 SHAP dependence", style = "Image Caption")
rsf_doc <- officer::body_add_img(
  rsf_doc, src = shap_dependence_file, width = 6.4, height = 4.6
)

rsf_doc <- officer::body_add_break(rsf_doc)
rsf_doc <- officer::body_add_par(rsf_doc, "5 两个典型患者的局部解释", style = "heading 1")
rsf_doc <- flextable::body_add_flextable(rsf_doc, typical_patient_ft)
rsf_doc <- officer::body_add_par(rsf_doc, "图4 两个典型患者的SHAP waterfall", style = "Image Caption")
rsf_doc <- officer::body_add_img(
  rsf_doc, src = patient_shap_file, width = 6.4, height = 8.2
)

rsf_doc <- officer::body_add_break(rsf_doc)
rsf_doc <- officer::body_add_par(rsf_doc, "6 28天校准", style = "heading 1")
rsf_doc <- flextable::body_add_flextable(rsf_doc, calibration_ft)
rsf_doc <- officer::body_add_par(rsf_doc, "图5 测试集28天校准图", style = "Image Caption")
rsf_doc <- officer::body_add_img(
  rsf_doc, src = calibration_file, width = 6.2, height = 5.2
)

rsf_doc <- officer::body_add_break(rsf_doc)
rsf_doc <- officer::body_add_par(rsf_doc, "7 临床决策曲线分析", style = "heading 1")
rsf_doc <- officer::body_add_par(
  rsf_doc,
  "DCA比较不同阈值概率下RSF模型与全部干预、均不干预策略的净获益。",
  style = "Normal"
)
rsf_doc <- officer::body_add_par(rsf_doc, "图6 测试集DCA", style = "Image Caption")
rsf_doc <- officer::body_add_img(rsf_doc, src = dca_file, width = 6.4, height = 4.8)

rsf_doc <- officer::body_add_break(rsf_doc)
rsf_doc <- officer::body_add_par(rsf_doc, "8 风险分层与KM曲线", style = "heading 1")
rsf_doc <- officer::body_add_par(
  rsf_doc,
  "以测试集预测风险中位数划分高、低风险组，并使用log-rank检验比较两组生存曲线。",
  style = "Normal"
)
rsf_doc <- officer::body_add_par(rsf_doc, "图7 高低风险组KM曲线", style = "Image Caption")
rsf_doc <- officer::body_add_img(rsf_doc, src = km_file, width = 6.4, height = 5.4)

docx_path <- file.path(fig_dir, "RSF模型结果.docx")
print(rsf_doc, target = docx_path)
cat("结果已保存至：\n", normalizePath(fig_dir), "\n")
cat("Word报告已保存至：\n", normalizePath(docx_path), "\n")
