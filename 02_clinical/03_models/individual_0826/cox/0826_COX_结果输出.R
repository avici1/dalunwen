# =============================================================================
# Cox 比例风险模型：8 个基线协变量 + 第 1 天 20 条轨迹截面
# 变量定义与参考/新COX.R 一致；group=1 训练，group=2 外验证；fold 不参与
# 可独立 source，不依赖 RStudio
# =============================================================================
library(dplyr)
library(survival)
library(riskRegression)
library(prodlim)
library(officer)
library(flextable)

base_dir <- "F:/文章_大论文/0722/实例研究代码"
out_dir <- file.path(base_dir, "执行")
fig_dir <- file.path(out_dir, "图像COX")
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)

baseline_path <- file.path(base_dir, "stroke_baseline_knn_0824_fold.csv")
longitudinal_path <- file.path(base_dir, "stroke_longitudinal_knn_0824_group_fold.csv")

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
# 读数并拼成建模表：8 基线 + times==1 的 20 轨迹截面
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

miss_base <- setdiff(baseline_cols, names(stroke_baseline))
miss_long <- setdiff(longitudinal_cols, names(stroke_longitudinal))
if (length(miss_base) > 0L) {
  stop("基线数据缺少列: ", paste(miss_base, collapse = ", "))
}
if (length(miss_long) > 0L) {
  stop("纵向数据缺少列: ", paste(miss_long, collapse = ", "))
}

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

cox_data <- baseline_data %>%
  left_join(longitudinal_day1, by = "hadm_id") %>%
  select(-hadm_id, -intime, -deathtime, -death_28d) %>%
  as.data.frame()

stopifnot("group" %in% names(cox_data))
train_data <- cox_data[cox_data$group == 1L, ]
test_data <- cox_data[cox_data$group == 2L, ]
stopifnot(nrow(train_data) > 0L, nrow(test_data) > 0L)
train_data$group <- NULL
test_data$group <- NULL

complete_mask_train <- stats::complete.cases(train_data)
complete_mask_test <- stats::complete.cases(test_data)
if (any(!complete_mask_train) || any(!complete_mask_test)) {
  cat(
    "剔除含缺失的样本：训练集", sum(!complete_mask_train),
    "例，验证集", sum(!complete_mask_test), "例\n"
  )
  train_data <- train_data[complete_mask_train, , drop = FALSE]
  test_data <- test_data[complete_mask_test, , drop = FALSE]
}

n_predictor <- ncol(train_data) - 2L
stopifnot(n_predictor == 28L)
stopifnot(nrow(train_data) > 0L, nrow(test_data) > 0L)

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
# 训练集拟合 Cox
# ---------------------------------------------------------------------------
cox_model <- survival::coxph(
  Surv(time28, status28) ~ .,
  data = train_data,
  x = TRUE,
  y = TRUE,
  model = TRUE
)

cox_train_risk <- predict(cox_model, newdata = train_data, type = "lp")
cox_test_risk <- predict(cox_model, newdata = test_data, type = "lp")

cox_train_cindex <- survival::concordance(
  Surv(time28, status28) ~ cox_train_risk,
  data = train_data,
  reverse = TRUE
)$concordance

cox_test_cindex <- survival::concordance(
  Surv(time28, status28) ~ cox_test_risk,
  data = test_data,
  reverse = TRUE
)$concordance

cox_baseline_hazard <- survival::basehaz(cox_model, centered = FALSE)
cox_hazard_28 <- tail(
  cox_baseline_hazard$hazard[cox_baseline_hazard$time <= 28],
  1
)
cox_train_probability <- 1 - exp(-cox_hazard_28 * exp(cox_train_risk))
cox_test_probability <- 1 - exp(-cox_hazard_28 * exp(cox_test_risk))

calculate_cox_brier_metrics <- function(model, evaluation_data) {
  score_object <- riskRegression::Score(
    object = list(Cox = model),
    formula = Hist(time28, status28) ~ 1,
    data = evaluation_data,
    metrics = "brier",
    times = evaluation_times,
    summary = "ibs",
    cens.method = "ipcw",
    conf.int = FALSE,
    plots = NULL
  )

  score_table <- as.data.frame(score_object$Brier$score)
  target_row <- score_table[
    as.character(score_table$model) == "Cox" &
      abs(score_table$times - evaluation_horizon) < 1e-08,
    ,
    drop = FALSE
  ]

  c(
    brier_28 = as.numeric(target_row$Brier),
    ibs_0_28 = as.numeric(target_row$IBS)
  )
}

cox_train_brier_metrics <- calculate_cox_brier_metrics(cox_model, train_data)
cox_test_brier_metrics <- calculate_cox_brier_metrics(cox_model, test_data)

cox_train_brier_28 <- unname(cox_train_brier_metrics["brier_28"])
cox_test_brier_28 <- unname(cox_test_brier_metrics["brier_28"])
cox_train_ibs <- unname(cox_train_brier_metrics["ibs_0_28"])
cox_test_ibs <- unname(cox_test_brier_metrics["ibs_0_28"])

metrics_cox <- data.frame(
  dataset = c("Train", "Test"),
  n = c(nrow(train_data), nrow(test_data)),
  events = c(sum(train_data$status28), sum(test_data$status28)),
  cindex = round(c(cox_train_cindex, cox_test_cindex), 4),
  brier_28 = round(c(cox_train_brier_28, cox_test_brier_28), 4),
  ibs = round(c(cox_train_ibs, cox_test_ibs), 4)
)

print(summary(cox_model))
print(metrics_cox, row.names = FALSE)

# ---------------------------------------------------------------------------
# 回归结果与性能表（Word + CSV）
# ---------------------------------------------------------------------------
cox_summary <- summary(cox_model)
cox_coef_matrix <- cox_summary$coefficients
cox_ci_matrix <- cox_summary$conf.int
cox_results <- data.frame(
  Variable = rownames(cox_coef_matrix),
  Coefficient = cox_coef_matrix[, "coef"],
  Standard_Error = cox_coef_matrix[, "se(coef)"],
  HR = cox_ci_matrix[, "exp(coef)"],
  HR_95CI_Lower = cox_ci_matrix[, "lower .95"],
  HR_95CI_Upper = cox_ci_matrix[, "upper .95"],
  P_value = cox_coef_matrix[, "Pr(>|z|)"],
  row.names = NULL,
  check.names = FALSE
)

write.csv(
  cox_results,
  file.path(fig_dir, "00_coefficients.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)
write.csv(
  metrics_cox,
  file.path(fig_dir, "00_performance.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

cox_results_formatted <- cox_results
cox_results_formatted$Coefficient <- sprintf("%.4f", cox_results$Coefficient)
cox_results_formatted$Standard_Error <- sprintf("%.4f", cox_results$Standard_Error)
cox_results_formatted$HR <- sprintf("%.4f", cox_results$HR)
cox_results_formatted$HR_95CI_Lower <- sprintf("%.4f", cox_results$HR_95CI_Lower)
cox_results_formatted$HR_95CI_Upper <- sprintf("%.4f", cox_results$HR_95CI_Upper)
cox_results_formatted$`HR (95% CI)` <- paste0(
  cox_results_formatted$HR,
  " (",
  cox_results_formatted$HR_95CI_Lower,
  "–",
  cox_results_formatted$HR_95CI_Upper,
  ")"
)
cox_results_formatted$P_value <- ifelse(
  cox_results$P_value < 0.001,
  "<0.001",
  sprintf("%.3f", cox_results$P_value)
)
cox_results_formatted <- cox_results_formatted[
  ,
  c("Variable", "Coefficient", "Standard_Error", "HR (95% CI)", "P_value")
]
names(cox_results_formatted) <- c(
  "变量", "回归系数", "标准误", "HR（95%CI）", "P值"
)

metrics_cox_formatted <- data.frame(
  数据集 = c("训练集", "测试集"),
  样本量 = c(nrow(train_data), nrow(test_data)),
  事件数 = c(
    sum(train_data$status28 == 1, na.rm = TRUE),
    sum(test_data$status28 == 1, na.rm = TRUE)
  ),
  事件率 = sprintf(
    "%.2f%%",
    100 * c(
      mean(train_data$status28 == 1, na.rm = TRUE),
      mean(test_data$status28 == 1, na.rm = TRUE)
    )
  ),
  `C-index` = sprintf("%.4f", c(cox_train_cindex, cox_test_cindex)),
  `28天Brier评分` = sprintf("%.4f", c(cox_train_brier_28, cox_test_brier_28)),
  `0至28天IBS` = sprintf("%.4f", c(cox_train_ibs, cox_test_ibs)),
  check.names = FALSE
)

cox_table <- flextable(cox_results_formatted) %>%
  bold(part = "header") %>%
  bg(part = "header", bg = "#D9EAF7") %>%
  font(fontname = "宋体", part = "all") %>%
  font(fontname = "Times New Roman", part = "all", cs.family = "宋体") %>%
  fontsize(size = 9, part = "body") %>%
  fontsize(size = 10, part = "header") %>%
  align(j = 1, align = "left", part = "all") %>%
  align(j = 2:ncol(cox_results_formatted), align = "center", part = "all") %>%
  autofit() %>%
  add_footer_lines(
    values = paste0(
      "注：HR为风险比；CI为置信区间。",
      "分类变量的回归系数均相对于其参考组。"
    )
  ) %>%
  fontsize(size = 8, part = "footer") %>%
  italic(part = "footer")

metrics_table <- flextable(metrics_cox_formatted) %>%
  bold(part = "header") %>%
  bg(part = "header", bg = "#D9EAF7") %>%
  font(fontname = "宋体", part = "all") %>%
  font(fontname = "Times New Roman", part = "all", cs.family = "宋体") %>%
  fontsize(size = 10, part = "all") %>%
  align(align = "center", part = "all") %>%
  autofit() %>%
  add_footer_lines(
    values = paste0(
      "注：C-index越接近1，模型区分度越高；",
      "28天Brier评分反映第28天的预测误差；",
      "IBS为0至28天Brier评分的时间积分平均，两者均越小越好。",
      "Brier与IBS均采用IPCW校正删失。"
    )
  ) %>%
  fontsize(size = 8, part = "footer") %>%
  italic(part = "footer")

word_document <- read_docx()
word_document <- word_document %>%
  body_add_par("生存分析结果", style = "graphic title")
word_document <- word_document %>%
  body_add_par(
    paste0(
      "采用训练集（group=1）建立Cox比例风险回归模型，",
      "并分别评价模型在训练集和测试集（group=2）中的预测性能。"
    ),
    style = "Normal"
  )
word_document <- word_document %>%
  body_add_par("表1 训练集Cox比例风险回归结果", style = "heading 1")
word_document <- word_document %>%
  body_add_flextable(cox_table)
word_document <- word_document %>%
  body_add_break()
word_document <- word_document %>%
  body_add_par("表2 Cox模型在训练集和测试集中的预测性能", style = "heading 1")
word_document <- word_document %>%
  body_add_flextable(metrics_table)

output_file <- file.path(fig_dir, "COX模型结果.docx")
print(word_document, target = output_file)
cat("结果已保存至：\n", normalizePath(fig_dir), "\n")
cat("Word文件：\n", normalizePath(output_file), "\n")
