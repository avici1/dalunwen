options(stringsAsFactors = FALSE)
invisible(try(Sys.setlocale("LC_ALL", "English_United States.utf8"), silent = TRUE))

suppressPackageStartupMessages({
  library(dplyr)
  library(survival)
  library(riskRegression)
  library(prodlim)
  library(officer)
  library(flextable)
})

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
  select(hadm_id, all_of(baseline_vars), intime, deathtime, death_28d, group) %>%
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

# 与RSF完全一致的28天边界处理。
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
model_frame <- model_data %>%
  select(group, time28, status28, all_of(baseline_vars), all_of(long_vars))
train <- model_frame %>% filter(group == 1L) %>% select(-group) %>% as.data.frame()
valid <- model_frame %>% filter(group == 2L) %>% select(-group) %>% as.data.frame()

stopifnot(
  ncol(train) - 2L == 28L,
  all(complete.cases(train)),
  all(complete.cases(valid))
)

cox_fit <- coxph(
  Surv(time28, status28) ~ ., data = train,
  ties = "efron", x = TRUE, y = TRUE, model = TRUE
)

lp_train <- predict(cox_fit, newdata = train, type = "lp")
lp_valid <- predict(cox_fit, newdata = valid, type = "lp")
train_c <- concordance(
  Surv(time28, status28) ~ lp_train, data = train, reverse = TRUE
)$concordance
valid_c <- concordance(
  Surv(time28, status28) ~ lp_valid, data = valid, reverse = TRUE
)$concordance

valid_score <- Score(
  object = list(COX = cox_fit),
  formula = Hist(time28, status28) ~ 1,
  data = valid,
  metrics = c("auc", "brier"),
  times = evaluation_times,
  summary = "ibs", cens.method = "ipcw", conf.int = FALSE, plots = NULL
)

auc_curve <- as.data.frame(valid_score$AUC$score) %>%
  filter(as.character(model) == "COX") %>%
  select(time = times, AUC)
brier_curve <- as.data.frame(valid_score$Brier$score) %>%
  filter(as.character(model) == "COX") %>%
  select(time = times, Brier, IBS)
time_metrics <- full_join(auc_curve, brier_curve, by = "time")
row28 <- time_metrics %>% filter(abs(time - horizon) < 1e-08)

metrics <- data.frame(
  指标 = c(
    "训练集 C-index（表观值）", "验证集 C-index", "28天 AUC",
    "28天 Brier（IPCW）", "IBS 0–28天（IPCW）"
  ),
  数值 = c(train_c, valid_c, row28$AUC, row28$Brier, row28$IBS),
  评价集 = c("group=1，模型拟合集", rep("group=2，独立验证", 4)),
  方向 = c("越大越好", "越大越好", "越大越好", "越小越好", "越小越好")
)

audit <- bind_rows(
  data.frame(item = "训练集样本数", value = nrow(train)),
  data.frame(item = "训练集28天死亡数", value = sum(train$status28)),
  data.frame(item = "验证集样本数", value = nrow(valid)),
  data.frame(item = "验证集28天死亡数", value = sum(valid$status28)),
  data.frame(item = "协变量数", value = ncol(train) - 2L),
  data.frame(item = "排除的非正死亡时间", value = length(bad_event_ids)),
  data.frame(item = "纳入队列中精确死亡时刻缺失/越界", value = sum(model_data$event_time_missing))
)

fit_summary <- summary(cox_fit)
coef_matrix <- fit_summary$coefficients
ci_matrix <- fit_summary$conf.int
coef_table <- data.frame(
  variable = rownames(coef_matrix),
  coefficient = coef_matrix[, "coef"],
  standard_error = coef_matrix[, "se(coef)"],
  HR = ci_matrix[, "exp(coef)"],
  HR_95CI_lower = ci_matrix[, "lower .95"],
  HR_95CI_upper = ci_matrix[, "upper .95"],
  p_value = coef_matrix[, "Pr(>|z|)"],
  row.names = NULL,
  check.names = FALSE
)

ph_obj <- cox.zph(cox_fit)
ph_table <- data.frame(
  variable = rownames(ph_obj$table),
  chisq = ph_obj$table[, "chisq"],
  df = ph_obj$table[, "df"],
  p_value = ph_obj$table[, "p"],
  row.names = NULL
)
global_ph_p <- ph_table$p_value[ph_table$variable == "GLOBAL"]

cox_table54a <- data.frame(
  模型 = "COX",
  模型设定 = "8个基线 + 第1天20个截面；无需调参",
  `训练集 C-index` = train_c,
  `验证集 C-index` = valid_c,
  `28天AUC` = row28$AUC,
  `28天Brier` = row28$Brier,
  `IBS 0–28` = row28$IBS,
  check.names = FALSE
)

rsf_file <- file.path(out_dir, "02_表5-4A_RSF指标.csv")
if (file.exists(rsf_file)) {
  rsf_metrics <- read.csv(rsf_file, check.names = FALSE, fileEncoding = "UTF-8")
  rsf_table54a <- data.frame(
    模型 = "RSF",
    模型设定 = "ntree=500, mtry=3, nodesize=10, nsplit=10",
    `训练集 C-index` = rsf_metrics[["数值"]][1],
    `验证集 C-index` = rsf_metrics[["数值"]][2],
    `28天AUC` = rsf_metrics[["数值"]][3],
    `28天Brier` = rsf_metrics[["数值"]][4],
    `IBS 0–28` = rsf_metrics[["数值"]][5],
    check.names = FALSE
  )
  combined_table54a <- bind_rows(cox_table54a, rsf_table54a)
} else {
  combined_table54a <- cox_table54a
}

write.csv(metrics, file.path(out_dir, "07_表5-4A_COX指标.csv"), row.names = FALSE, fileEncoding = "UTF-8")
write.csv(combined_table54a, file.path(out_dir, "08_表5-4A_COX_RSF合并.csv"), row.names = FALSE, fileEncoding = "UTF-8")
write.csv(coef_table, file.path(out_dir, "09_COX回归系数.csv"), row.names = FALSE, fileEncoding = "UTF-8")
write.csv(ph_table, file.path(out_dir, "10_COX_PH假设检验.csv"), row.names = FALSE, fileEncoding = "UTF-8")
write.csv(time_metrics, file.path(out_dir, "11_COX验证集时间依赖指标.csv"), row.names = FALSE, fileEncoding = "UTF-8")
write.csv(audit, file.path(out_dir, "12_COX队列审计.csv"), row.names = FALSE, fileEncoding = "UTF-8")

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
metrics_report$数值 <- sprintf("%.4f", metrics_report$数值)
table54_report <- combined_table54a
num_cols <- setdiff(names(table54_report), c("模型", "模型设定"))
table54_report[num_cols] <- lapply(table54_report[num_cols], function(x) sprintf("%.4f", x))

coef_report <- coef_table %>%
  transmute(
    变量 = variable,
    HR = sprintf("%.4f", HR),
    `95%CI` = sprintf("%.4f–%.4f", HR_95CI_lower, HR_95CI_upper),
    P值 = ifelse(p_value < 0.001, "<0.001", sprintf("%.3f", p_value))
  )
ph_report <- ph_table %>%
  transmute(
    变量 = variable,
    卡方值 = sprintf("%.3f", chisq),
    自由度 = df,
    P值 = ifelse(p_value < 0.001, "<0.001", sprintf("%.3f", p_value))
  )

doc <- read_docx()
doc <- body_add_par(doc, "COX模型表5-4A复算报告", style = "graphic title")
doc <- body_add_par(
  doc,
  paste0(
    "COX与RSF使用完全相同的静态任务队列、28个预测变量、训练/验证划分、",
    "28天边界和IPCW评价口径。group=1用于拟合，group=2用于独立验证。"
  ), style = "Normal"
)

doc <- body_add_par(doc, "1 可直接填入表5-4A的COX结果", style = "heading 1")
doc <- body_add_flextable(doc, style_table(metrics_report) %>% fit_to_width(max_width = 6.4))
doc <- body_add_par(
  doc,
  "COX一行建议填写：0.7756、0.7737、0.8011、0.1404、0.0918。训练集C-index为模型拟合集上的表观值，不是OOB值；模型间主比较应以独立验证集指标为准。",
  style = "Normal"
)

doc <- body_add_par(doc, "2 COX与RSF表5-4A合并结果", style = "heading 1")
doc <- body_add_flextable(doc, style_table(table54_report) %>% fit_to_width(max_width = 6.4))
doc <- body_add_par(
  doc,
  "在共同验证集上，RSF的C-index和AUC较高，Brier和IBS较低。训练集COX表观C-index与RSF OOB C-index的估计方式不同，不宜据此进行模型优劣排序。",
  style = "Normal"
)

doc <- body_add_par(doc, "3 队列与评价边界", style = "heading 1")
doc <- body_add_flextable(doc, style_table(audit))
doc <- body_add_par(
  doc,
  paste0(
    "评价点为恰好28天。28天存活者以28+0.001天表示随访越过评价点；",
    "缺少精确死亡时刻的28天死亡病例暂以第28天作为区间末端代理。",
    "因此固定28天AUC和Brier的结局身份由death_28d保证，C-index和IBS仍受死亡时刻近似影响。"
  ), style = "Normal"
)

doc <- body_add_break(doc)
doc <- body_add_par(doc, "4 COX回归系数", style = "heading 1")
doc <- body_add_flextable(
  doc,
  style_table(coef_report) %>% fit_to_width(max_width = 6.4) %>%
    paginate(init = TRUE, hdr_ftr = TRUE)
)

doc <- body_add_break(doc)
doc <- body_add_par(doc, "5 比例风险假设检验", style = "heading 1")
doc <- body_add_flextable(
  doc,
  style_table(ph_report) %>% fit_to_width(max_width = 6.4) %>%
    paginate(init = TRUE, hdr_ftr = TRUE)
)
doc <- body_add_par(
  doc,
  paste0(
    "Schoenfeld残差全局检验P值",
    ifelse(global_ph_p < 0.001, "<0.001", paste0("=", sprintf("%.3f", global_ph_p))),
    "，提示比例风险假设整体不成立。该结果不改变本次预先规定的表5-4A预测性能计算，",
    "但论文中解释单一恒定HR时应谨慎；建议将时间交互或分层分析作为敏感性分析。"
  ), style = "Normal"
)

doc <- body_add_par(doc, "6 文件说明", style = "heading 1")
doc <- body_add_par(
  doc,
  paste0(
    "主脚本为COX_5_4A_corrected.R；07文件为COX五项指标，08文件为COX/RSF合并表，",
    "09文件为回归系数，10文件为比例风险假设检验，11文件为验证集完整时间依赖指标。"
  ), style = "Normal"
)

docx_path <- file.path(out_dir, "COX模型结果_表5-4A修正版.docx")
print(doc, target = docx_path)

cat("\n表5-4A COX建议数值：\n")
print(metrics_report, row.names = FALSE)
cat("\n输出目录：", normalizePath(out_dir), "\n", sep = "")

