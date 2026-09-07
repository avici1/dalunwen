#%%
library(dplyr)
library(survival)
library(randomForestSRC)
library(riskRegression)
library(rstudioapi)
library(officer)
library(flextable)

thread_number <- max(1L, parallel::detectCores(logical = TRUE) - 2L)
options(rf.cores = thread_number)
options(rf.cores = thread_number)

setwd(dirname(getSourceEditorContext()$path))

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



#%% Cox 回归及训练集、测试集验证
####################################################################################################
cox_model <- survival::coxph(
  Surv(time28, status28) ~ .,
  data = train_data,
  x = TRUE,
  y = TRUE,
  model = TRUE
)




#%% C-index \28-day Brier \ IBS 
####################################################################################################
# 根据训练集 Cox 模型的基线累积风险计算 28 天死亡概率。
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


# 同时计算28天单时点Brier评分和0至28天IBS。
# riskRegression::Score()使用IPCW校正删失，两项指标采用相同方法。

cox_baseline_hazard <- survival::basehaz(cox_model, centered = FALSE)
cox_hazard_28 <- tail(
  cox_baseline_hazard$hazard[cox_baseline_hazard$time <= 28],
  1
)
cox_train_probability <- 1 - exp(-cox_hazard_28 * exp(cox_train_risk))
cox_test_probability <- 1 - exp(-cox_hazard_28 * exp(cox_test_risk))

calculate_cox_brier_metrics <- function(model, evaluation_data, horizon = 28) {
  evaluation_times <- seq(0, horizon, by = 0.5)

  score_object <- riskRegression::Score(
    object = list(Cox = model),
    formula = Hist(time28, status28) ~ 1,
    data = evaluation_data,
    metrics = "brier",
    times = evaluation_times,
    summary = "ibs"
  )

  score_table <- as.data.frame(score_object$Brier$score)
  target_row <- score_table[
    as.character(score_table$model) == "Cox" &
      abs(score_table$times - horizon) < sqrt(.Machine$double.eps),
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

summary(cox_model)
metrics_cox




#%%
######################################################保存生存分析内容
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


cox_results_formatted <- cox_results
cox_results_formatted$Coefficient <- sprintf(
  "%.4f",
  cox_results$Coefficient
)
cox_results_formatted$Standard_Error <- sprintf(
  "%.4f",
  cox_results$Standard_Error
)

cox_results_formatted$HR <- sprintf(
  "%.4f",
  cox_results$HR
)
cox_results_formatted$HR_95CI_Lower <- sprintf(
  "%.4f",
  cox_results$HR_95CI_Lower
)
cox_results_formatted$HR_95CI_Upper <- sprintf(
  "%.4f",
  cox_results$HR_95CI_Upper
)
# 合并HR及其95%CI
cox_results_formatted$`HR (95% CI)` <- paste0(
  cox_results_formatted$HR,
  " (",
  cox_results_formatted$HR_95CI_Lower,
  "–",
  cox_results_formatted$HR_95CI_Upper,
  ")"
)

# 格式化P值
cox_results_formatted$P_value <- ifelse(
  cox_results$P_value < 0.001,
  "<0.001",
  sprintf("%.3f", cox_results$P_value)
)

# 保留最终需要展示的列
cox_results_formatted <- cox_results_formatted[
  ,
  c(
    "Variable",
    "Coefficient",
    "Standard_Error",
    "HR (95% CI)",
    "P_value"
  )
]

# 修改为中文列名
names(cox_results_formatted) <- c(
  "变量",
  "回归系数",
  "标准误",
  "HR（95%CI）",
  "P值"
)



metrics_cox_formatted <- data.frame(
  数据集 = c("训练集", "测试集"),
  样本量 = c(
    nrow(train_data),
    nrow(test_data)
  ),
  
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
  
  `C-index` = sprintf(
    "%.4f",
    c(
      cox_train_cindex,
      cox_test_cindex
    )
  ),

  `28天Brier评分` = sprintf(
    "%.4f",
    c(
      cox_train_brier_28,
      cox_test_brier_28
    )
  ),
  
  `0至28天IBS` = sprintf(
    "%.4f",
    c(
      cox_train_ibs,
      cox_test_ibs
    )
  ),
  
  check.names = FALSE
)



# 设置回归结果表格样式

cox_table <- flextable(cox_results_formatted) %>%
  
  # 表头加粗
  bold(part = "header") %>%
  
  # 表头背景色
  bg(
    part = "header",
    bg = "#D9EAF7"
  ) %>%
  
  # 表格字体
  font(
    fontname = "宋体",
    part = "all"
  ) %>%
  
  # 英文字体
  font(
    fontname = "Times New Roman",
    part = "all",
    cs.family = "宋体"
  ) %>%
  
  # 字体大小
  fontsize(
    size = 9,
    part = "body"
  ) %>%
  
  fontsize(
    size = 10,
    part = "header"
  ) %>%
  
  # 对齐
  align(
    j = 1,
    align = "left",
    part = "all"
  ) %>%
  
  align(
    j = 2:ncol(cox_results_formatted),
    align = "center",
    part = "all"
  ) %>%
  
  # 自动调整列宽
  autofit() %>%
  
  # 添加表格脚注
  add_footer_lines(
    values = paste0(
      "注：HR为风险比；CI为置信区间。",
      "分类变量的回归系数均相对于其参考组。"
    )
  ) %>%
  
  fontsize(
    size = 8,
    part = "footer"
  ) %>%
  
  italic(
    part = "footer"
  )


# 5. 设置性能指标表格样式

metrics_table <- flextable(metrics_cox_formatted) %>%
  
  bold(part = "header") %>%
  
  bg(
    part = "header",
    bg = "#D9EAF7"
  ) %>%
  
  font(
    fontname = "宋体",
    part = "all"
  ) %>%
  
  font(
    fontname = "Times New Roman",
    part = "all",
    cs.family = "宋体"
  ) %>%
  
  fontsize(
    size = 10,
    part = "all"
  ) %>%
  
  align(
    align = "center",
    part = "all"
  ) %>%
  
  autofit() %>%
  
  add_footer_lines(
    values = paste0(
      "注：C-index越接近1，模型区分度越高；",
      "28天Brier评分反映第28天的预测误差；",
      "IBS为0至28天Brier评分的时间积分平均，两者均越小越好。"
    )
  ) %>%
  
  fontsize(
    size = 8,
    part = "footer"
  ) %>%
  
  italic(
    part = "footer"
  )


# 创建Word文档


word_document <- read_docx()
word_document <- word_document %>%
  body_add_par(
    "生存分析结果",
    style = "graphic title"
  )
word_document <- word_document %>%
  body_add_par(
    paste0(
      "采用训练集建立Cox比例风险回归模型，",
      "并分别评价模型在训练集和测试集中的预测性能。"
    ),
    style = "Normal"
  )

# 表1标题
word_document <- word_document %>%
  body_add_par(
    "表1 训练集Cox比例风险回归结果",
    style = "heading 1"
  )
word_document <- word_document %>%
  body_add_flextable(cox_table)

# 添加分页
word_document <- word_document %>%
  body_add_break()

# 表2标题
word_document <- word_document %>%
  body_add_par(
    "表2 Cox模型在训练集和测试集中的预测性能",
    style = "heading 1"
  )
word_document <- word_document %>%
  body_add_flextable(metrics_table)
output_file <- file.path(
  getwd(),
  "生存分析结果.docx"
)
print(
  word_document,
  target = output_file
)
cat(
  "Word文件已经保存至：\n",
  normalizePath(output_file),
  "\n"
)
