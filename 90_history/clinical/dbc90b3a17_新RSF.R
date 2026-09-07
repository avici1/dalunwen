#%%
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
library(rstudioapi)
library(officer)
library(flextable)

#%%


thread_number <- max(1L, parallel::detectCores(logical = TRUE) - 2L)
options(rf.cores = thread_number)


# 同时兼容RStudio的Source运行和命令行Rscript运行。
source_path <- if (rstudioapi::isAvailable()) {
  tryCatch(
    rstudioapi::getSourceEditorContext()$path,
    error = function(e) ""
  )
} else {
  ""
}

script_dir <- if (nzchar(source_path)) {
  dirname(source_path)
} else {
  file_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  command_file <- if (length(file_arg)) {
    sub("^--file=", "", file_arg[1])
  } else {
    ""
  }
  if (nzchar(command_file) && command_file != "-" && file.exists(command_file)) {
    dirname(normalizePath(command_file))
  } else {
    getwd()
  }
}
setwd(script_dir)
rm(source_path, script_dir)

stroke_baseline <- read.csv(
  "stroke_baselinedata_0531.csv",
  stringsAsFactors = FALSE,
  na.strings = c("", "NA")
)

stroke_longitudinal <- read.csv(
  "stroke_longitudinal_inputed_0603.csv",
  stringsAsFactors = FALSE,
  na.strings = c("", "NA")
)

#%% 一、整理数据
####################################################################################################
baseline_cols <- c(
  "hadm_id", "age", "charlson_comorbidity_index", "apsiii", "sapsii",
  "oasis", "preiculos", "mechvent", "electivesurgery",
  "intime", "deathtime", "death_28d"
)

longitudinal_cols <- c(
  "hadm_id", "times", "total_urine_output", "creat", "aki_stage", "gcs",
  "ph", "pco2", "lactate", "po2", "pao2fio2ratio", "glucose", "sodium",
  "bicarbonate", "hemoglobin", "temperature", "fio2", "sofa_24hours",
  "cns_24hours", "renal_24hours", "cardiovascular_24hours",
  "respiration_24hours"
)

baseline_data <- stroke_baseline %>%
  select(all_of(baseline_cols)) %>%
  mutate(
    hadm_id = as.integer(hadm_id),
    intime = as.POSIXct(intime, format = "%d/%m/%Y %H:%M:%S"),
    deathtime = as.POSIXct(deathtime, format = "%d/%m/%Y %H:%M:%S"),
    time28 = as.numeric(difftime(deathtime, intime, units = "days")),
    time28 = ifelse(is.na(time28), 28, pmin(time28, 28)),
    status28 = as.integer(death_28d == 1),
    mechvent = factor(mechvent),
    electivesurgery = factor(electivesurgery)
  ) %>%
  filter(time28 > 0, !is.na(status28)) %>%
  distinct(hadm_id, .keep_all = TRUE)

longitudinal_day1 <- stroke_longitudinal %>%
  select(all_of(longitudinal_cols)) %>%
  filter(times == 1) %>%
  mutate(hadm_id = as.integer(hadm_id)) %>%
  select(-times) %>%
  distinct(hadm_id, .keep_all = TRUE)

rsf_data <- baseline_data %>%
  inner_join(longitudinal_day1, by = "hadm_id") %>%
  select(
    -hadm_id, -intime, -deathtime, -death_28d,
    time28, status28
  ) %>%
  as.data.frame()

#%% 保存建模数据
writexl::write_xlsx(rsf_data, "rsf_data.xlsx")

#%% 二、按 7:3 划分训练集和测试集
####################################################################################################
set.seed(2026)

death_rows <- which(rsf_data$status28 == 1L)
survival_rows <- which(rsf_data$status28 == 0L)

train_rows <- sort(c(
  sample(death_rows, floor(length(death_rows) * 0.7)),
  sample(survival_rows, floor(length(survival_rows) * 0.7))
))

train_data <- rsf_data[train_rows, ]
test_data <- rsf_data[-train_rows, ]


#%% 三、调参
####################################################################################################
thread_number

tuning_results <- expand.grid(
  ntree = c(300, 500, 1000),
  mtry = c(3, 6, 9, 12),
  nodesize = c(10, 20, 30, 40),
  nsplit = c(10, 25, 50)
)
tuning_results$oob_error <- NA_real_

for (i in seq_len(nrow(tuning_results))) {
  tuning_model <- randomForestSRC::rfsrc(
    Surv(time28, status28) ~ .,
    data = train_data,
    ntree = tuning_results$ntree[i],
    mtry = tuning_results$mtry[i],
    nodesize = tuning_results$nodesize[i],
    nsplit = tuning_results$nsplit[i],
    splitrule = "logrank",
    na.action = "na.impute",
    importance = FALSE,
    seed = 2026
  )

  tuning_results$oob_error[i] <- tail(tuning_model$err.rate, 1)
}


#%% 最优超参     ntree=500  mtry=6  nodesize=30  nsplit=10
####################################################################################################
tuning_results <- tuning_results %>%
  arrange(oob_error)

best_ntree <- tuning_results$ntree[1]
best_mtry <- tuning_results$mtry[1]
best_nodesize <- tuning_results$nodesize[1]
best_nsplit <- tuning_results$nsplit[1]

tuning_results

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

#%%  结果
####################################################################################################
train_time_index <- which.min(abs(rsf_model$time.interest - 28))
train_probability <- 1 - rsf_model$survival.oob[, train_time_index]

train_evaluation <- data.frame(
  time28 = train_data$time28,
  status28 = train_data$status28,
  probability = train_probability
)

train_cindex <- survival::concordance(
  Surv(time28, status28) ~ probability,
  data = train_evaluation,
  reverse = TRUE
)$concordance

train_brier <- mean(
  (train_evaluation$status28 - train_evaluation$probability)^2
)

test_prediction <- predict(
  rsf_model,
  newdata = test_data,
  na.action = "na.impute"
)

test_time_index <- which.min(abs(test_prediction$time.interest - 28))
test_probability <- 1 - test_prediction$survival[, test_time_index]

test_evaluation <- data.frame(
  time28 = test_data$time28,
  status28 = test_data$status28,
  probability = test_probability
)

test_cindex <- survival::concordance(
  Surv(time28, status28) ~ probability,
  data = test_evaluation,
  reverse = TRUE
)$concordance

test_brier <- mean(
  (test_evaluation$status28 - test_evaluation$probability)^2
)

metrics_rsf <- data.frame(
  dataset = c("Train_OOB", "Test"),
  n = c(nrow(train_evaluation), nrow(test_evaluation)),
  events = c(sum(train_evaluation$status28), sum(test_evaluation$status28)),
  cindex = round(c(train_cindex, test_cindex), 4),
  brier = round(c(train_brier, test_brier), 4),
  ntree = best_ntree,
  mtry = best_mtry,
  nodesize = best_nodesize,
  nsplit = best_nsplit
)

best_oob_error <- tail(rsf_model$err.rate, 1)

best_oob_error
metrics_rsf


#%%  输出成WORD文件
####################################################################################################

# 本节复用上文已经得到的 rsf_model、train_data、test_data、
# test_probability 和最优超参数，不重复拟合模型。

# 1. 28天时间依赖AUC、IPCW-Brier和IPCW-IBS
# 由于所有未死亡病例都在第28天行政删失，AUC必须在28天前极小距离处评价，
# 否则严格定义下没有 time > 28 的对照病例。
evaluation_horizon <- 28 - 1e-04
evaluation_times <- unique(c(seq(0, 27.5, by = 0.5), evaluation_horizon))

# 训练集评价：反映模型在训练资料上的表观性能。
train_rsf_score <- riskRegression::Score(
  object = list(RSF = rsf_model),
  formula = Hist(time28, status28) ~ 1,
  data = train_data,
  metrics = c("auc", "brier"),
  times = evaluation_times,
  summary = "ibs",
  cens.method = "ipcw",
  conf.int = FALSE,
  plots = NULL
)

# 测试集评价：反映模型在留出测试资料上的泛化性能。
test_rsf_score <- riskRegression::Score(
  object = list(RSF = rsf_model),
  formula = Hist(time28, status28) ~ 1,
  data = test_data,
  metrics = c("auc", "brier"),
  times = evaluation_times,
  summary = "ibs",
  cens.method = "ipcw",
  conf.int = FALSE,
  plots = NULL
)

train_auc_score_table <- as.data.frame(train_rsf_score$AUC$score)
train_brier_score_table <- as.data.frame(train_rsf_score$Brier$score)
test_auc_score_table <- as.data.frame(test_rsf_score$AUC$score)
test_brier_score_table <- as.data.frame(test_rsf_score$Brier$score)

train_auc_28 <- train_auc_score_table$AUC[
  as.character(train_auc_score_table$model) == "RSF" &
    abs(train_auc_score_table$times - evaluation_horizon) < 1e-08
]
train_brier_28_ipcw <- train_brier_score_table$Brier[
  as.character(train_brier_score_table$model) == "RSF" &
    abs(train_brier_score_table$times - evaluation_horizon) < 1e-08
]
train_ibs_ipcw <- train_brier_score_table$IBS[
  as.character(train_brier_score_table$model) == "RSF" &
    abs(train_brier_score_table$times - evaluation_horizon) < 1e-08
]

test_auc_28 <- test_auc_score_table$AUC[
  as.character(test_auc_score_table$model) == "RSF" &
    abs(test_auc_score_table$times - evaluation_horizon) < 1e-08
]
test_brier_28_ipcw <- test_brier_score_table$Brier[
  as.character(test_brier_score_table$model) == "RSF" &
    abs(test_brier_score_table$times - evaluation_horizon) < 1e-08
]
test_ibs_ipcw <- test_brier_score_table$IBS[
  as.character(test_brier_score_table$model) == "RSF" &
    abs(test_brier_score_table$times - evaluation_horizon) < 1e-08
]

oob_cindex <- 1 - best_oob_error

parameter_table <- data.frame(
  parameter = c("ntree", "mtry", "nodesize", "nsplit"),
  value = c(best_ntree, best_mtry, best_nodesize, best_nsplit)
)

performance_table <- data.frame(
  metric = c(
    "C-index", "28-day AUC",
    "28-day Brier (IPCW)", "IBS 0-28 days (IPCW)"
  ),
  training_set = round(
    c(oob_cindex, train_auc_28, train_brier_28_ipcw, train_ibs_ipcw),
    4
  ),
  test_set = round(
    c(test_cindex, test_auc_28, test_brier_28_ipcw, test_ibs_ipcw),
    4
  ),
  check.names = FALSE
)

# 2. VIMP变量重要性
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

dir.create("RSF报告图", showWarnings = FALSE)

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

vimp_file <- file.path("RSF报告图", "01_VIMP.png")
ggplot2::ggsave(vimp_file, vimp_plot, width = 7, height = 5.5, dpi = 300)


# 3. SHAP global beeswarm、两个典型患者局部SHAP和dependence
# fastshap需要一个预测包装函数；这是本节唯一不可避免的辅助函数。
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

train_predictors <- train_data %>%
  select(-time28, -status28)
test_predictors <- test_data %>%
  select(-time28, -status28)

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

shap_beeswarm_file <- file.path("RSF报告图", "02_SHAP_beeswarm.png")
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
patient_shap_file <- file.path("RSF报告图", "03_典型患者_SHAP.png")
ggplot2::ggsave(
  patient_shap_file,
  patient_shap_plot,
  width = 7,
  height = 9,
  dpi = 300
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

shap_dependence_file <- file.path("RSF报告图", "04_SHAP_dependence.png")
ggplot2::ggsave(
  shap_dependence_file,
  shap_dependence_plot,
  width = 7,
  height = 5,
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

 
# 4. 28-day calibration：按预测风险十分位分组，观察风险用KM估计
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

calibration_file <- file.path("RSF报告图", "05_28天校准.png")
ggplot2::ggsave(
  calibration_file,
  calibration_plot,
  width = 6.5,
  height = 5.5,
  dpi = 300
)

# 5. DCA：在测试集上评价28天预测风险的临床净获益
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

dca_file <- file.path("RSF报告图", "06_DCA.png")
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


# 6. KM：按测试集预测风险中位数分为高、低风险组
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

km_file <- file.path("RSF报告图", "07_KM风险分层.png")
png(km_file, width = 2000, height = 1700, res = 260)
print(km_plot)
dev.off()



# 7. 组织Word报告中的表格
parameter_ft <- flextable::flextable(parameter_table) %>%
  flextable::theme_booktabs() %>%
  flextable::bg(part = "header", bg = "#D9EAF7") %>%
  flextable::bold(part = "header") %>%
  flextable::font(fontname = "Microsoft YaHei", part = "all") %>%
  flextable::align(align = "center", part = "all") %>%
  flextable::autofit()

performance_ft <- flextable::flextable(performance_table) %>%
  flextable::theme_booktabs() %>%
  flextable::bg(part = "header", bg = "#D9EAF7") %>%
  flextable::bold(part = "header") %>%
  flextable::font(fontname = "Microsoft YaHei", part = "all") %>%
  flextable::align(align = "center", part = "all") %>%
  flextable::autofit() %>%
  flextable::fit_to_width(max_width = 6.4)

vimp_ft <- flextable::flextable(vimp_table) %>%
  flextable::theme_booktabs() %>%
  flextable::bg(part = "header", bg = "#D9EAF7") %>%
  flextable::bold(part = "header") %>%
  flextable::font(fontname = "Microsoft YaHei", part = "all") %>%
  flextable::colformat_num(j = "importance", digits = 4) %>%
  flextable::autofit() %>%
  flextable::paginate(init = TRUE, hdr_ftr = TRUE)

typical_patient_ft <- flextable::flextable(typical_patient_table) %>%
  flextable::theme_booktabs() %>%
  flextable::bg(part = "header", bg = "#D9EAF7") %>%
  flextable::bold(part = "header") %>%
  flextable::font(fontname = "Microsoft YaHei", part = "all") %>%
  flextable::align(align = "center", part = "all") %>%
  flextable::autofit() %>%
  flextable::fit_to_width(max_width = 6.4)

calibration_ft <- flextable::flextable(calibration_table) %>%
  flextable::theme_booktabs() %>%
  flextable::bg(part = "header", bg = "#D9EAF7") %>%
  flextable::bold(part = "header") %>%
  flextable::font(fontname = "Microsoft YaHei", part = "all") %>%
  flextable::colformat_num(
    j = c("predicted_risk", "observed_risk", "observed_low", "observed_high"),
    digits = 4
  ) %>%
  flextable::autofit() %>%
  flextable::fit_to_width(max_width = 6.4)

#%% 
# 8. 生成“RSF模型结果.docx”
rsf_doc <- officer::read_docx()
rsf_doc <- officer::body_add_par(
  rsf_doc,
  "随机生存森林（RSF）模型结果",
  style = "graphic title"
)
rsf_doc <- officer::body_add_par(
  rsf_doc,
  paste0(
    "结局为28天全因死亡。训练集C-index采用OOB评价，测试集采用外部留出评价。",
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
    "注：未死亡病例统一在第28天行政删失，因此严格的时间依赖AUC在",
    "27.9999天计算，并作为28天AUC报告；IPCW-Brier和IPCW-IBS也评价至该时点。",
    "训练集AUC、Brier和IBS为训练资料上的表观性能，测试集结果用于评价泛化性能。"
  ),
  style = "Normal"
)

rsf_doc <- officer::body_add_par(rsf_doc, "3 VIMP变量重要性", style = "heading 1")
rsf_doc <- flextable::body_add_flextable(rsf_doc, vimp_ft)
rsf_doc <- officer::body_add_par(rsf_doc, "图1 VIMP变量重要性", style = "Image Caption")
rsf_doc <- officer::body_add_img(
  rsf_doc,
  src = vimp_file,
  width = 6.4,
  height = 5.0
)

rsf_doc <- officer::body_add_break(rsf_doc)
rsf_doc <- officer::body_add_par(rsf_doc, "4 全局SHAP解释", style = "heading 1")
rsf_doc <- officer::body_add_par(
  rsf_doc,
  "SHAP值表示各变量对患者28天预测死亡风险的边际贡献；正值推动风险升高，负值推动风险降低。",
  style = "Normal"
)
rsf_doc <- officer::body_add_par(rsf_doc, "图2 SHAP global beeswarm", style = "Image Caption")
rsf_doc <- officer::body_add_img(
  rsf_doc,
  src = shap_beeswarm_file,
  width = 6.4,
  height = 5.5
)

rsf_doc <- officer::body_add_break(rsf_doc)
rsf_doc <- officer::body_add_par(rsf_doc, "图3 SHAP dependence", style = "Image Caption")
rsf_doc <- officer::body_add_img(
  rsf_doc,
  src = shap_dependence_file,
  width = 6.4,
  height = 4.6
)

rsf_doc <- officer::body_add_break(rsf_doc)
rsf_doc <- officer::body_add_par(rsf_doc, "5 两个典型患者的局部解释", style = "heading 1")
rsf_doc <- flextable::body_add_flextable(rsf_doc, typical_patient_ft)
rsf_doc <- officer::body_add_par(rsf_doc, "图4 两个典型患者的SHAP waterfall", style = "Image Caption")
rsf_doc <- officer::body_add_img(
  rsf_doc,
  src = patient_shap_file,
  width = 6.4,
  height = 8.2
)

rsf_doc <- officer::body_add_break(rsf_doc)
rsf_doc <- officer::body_add_par(rsf_doc, "6 28天校准", style = "heading 1")
rsf_doc <- flextable::body_add_flextable(rsf_doc, calibration_ft)
rsf_doc <- officer::body_add_par(rsf_doc, "图5 测试集28天校准图", style = "Image Caption")
rsf_doc <- officer::body_add_img(
  rsf_doc,
  src = calibration_file,
  width = 6.2,
  height = 5.2
)

rsf_doc <- officer::body_add_break(rsf_doc)
rsf_doc <- officer::body_add_par(rsf_doc, "7 临床决策曲线分析", style = "heading 1")
rsf_doc <- officer::body_add_par(
  rsf_doc,
  "DCA比较不同阈值概率下RSF模型与全部干预、均不干预策略的净获益。",
  style = "Normal"
)
rsf_doc <- officer::body_add_par(rsf_doc, "图6 测试集DCA", style = "Image Caption")
rsf_doc <- officer::body_add_img(
  rsf_doc,
  src = dca_file,
  width = 6.4,
  height = 4.8
)

rsf_doc <- officer::body_add_break(rsf_doc)
rsf_doc <- officer::body_add_par(rsf_doc, "8 风险分层与KM曲线", style = "heading 1")
rsf_doc <- officer::body_add_par(
  rsf_doc,
  "以测试集预测风险中位数划分高、低风险组，并使用log-rank检验比较两组生存曲线。",
  style = "Normal"
)
rsf_doc <- officer::body_add_par(rsf_doc, "图7 高低风险组KM曲线", style = "Image Caption")
rsf_doc <- officer::body_add_img(
  rsf_doc,
  src = km_file,
  width = 6.4,
  height = 5.4
)

print(rsf_doc, target = "RSF模型结果.docx")
cat(
  "RSF模型结果已保存至：\n",
  normalizePath("RSF模型结果.docx"),
  "\n"
)
