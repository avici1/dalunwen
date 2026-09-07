library(dplyr)
library(tidyr)
library(DynForest)

## ========== 1. 读入数据 ==========
stroke_baselinedata_0531 <- read.csv(
  "F:/文章_大论文/0521/处理后文件/stroke_baselinedata_0531.csv"
)
stroke_longitudinal_filter_0530 <- read.csv(
  "F:/文章_大论文/0521/处理后文件/stroke_longitudinal_filter_0530.csv"
)
stroke_outcome_0520 <- read.csv(
  "F:/文章_大论文/0521/提取文件/stroke_outcome_0520.csv"
)

## ========== 2. 参数 ==========
landmark <- 7L
long_vars_log <- c("creat", "item_51301", "item_51221")
long_vars_model <- c(
  "gcs", "sofa_24hours",
  paste0("log_", long_vars_log),
  "item_220045"
)

## ========== 3. 构造分析队列 ==========
stroke_cohort <- stroke_baselinedata_0531 %>%
  left_join(
    stroke_outcome_0520 %>% select(hadm_id, death_28d, icu_expire_flag),
    by = "hadm_id"
  ) %>%
  mutate(
    intime_parsed = as.POSIXct(intime, format = "%d/%m/%Y %H:%M:%S"),
    died_before_lm = icu_expire_flag == 1 & icu_los_days <= landmark
  ) %>%
  filter(!is.na(death_28d), icu_los_days > landmark, !died_before_lm) %>%
  distinct(hadm_id, .keep_all = TRUE)

stroke_long <- stroke_longitudinal_filter_0530 %>%
  semi_join(stroke_cohort %>% select(hadm_id), by = "hadm_id") %>%
  left_join(stroke_cohort %>% select(hadm_id, intime_parsed), by = "hadm_id") %>%
  mutate(
    chart_date_parsed = as.Date(chart_date),
    time = as.numeric(
      difftime(chart_date_parsed, as.Date(intime_parsed), units = "days")
    )
  ) %>%
  filter(time >= 0, time <= landmark) %>%
  mutate(across(all_of(long_vars_log), ~ log1p(.x), .names = "log_{.col}"))

## ========== 4. 训练 / 测试划分（按 hadm_id） ==========
set.seed(1234)
id_all <- unique(stroke_cohort$hadm_id)
id_train <- sample(id_all, size = floor(length(id_all) * 2 / 3))
id_test <- setdiff(id_all, id_train)

## ========== 5. 四个 dynforest 输入对象 ==========
timeData_train <- stroke_long %>%
  filter(hadm_id %in% id_train) %>%
  select(hadm_id, time, all_of(long_vars_model))

fixedData_train <- stroke_cohort %>%
  filter(hadm_id %in% id_train) %>%
  mutate(mechvent = factor(mechvent)) %>%
  select(hadm_id, age, charlson_comorbidity_index, mechvent, preiculos) %>%
  distinct(hadm_id, .keep_all = TRUE)

timeVarModel <- list(
  gcs = list(fixed = gcs ~ time, random = ~ time),
  sofa_24hours = list(
    fixed = sofa_24hours ~ time + I(time^2),
    random = ~ time + I(time^2)
  ),
  log_creat = list(fixed = log_creat ~ time, random = ~ time),
  log_item_51301 = list(fixed = log_item_51301 ~ time, random = ~ time),
  log_item_51221 = list(
    fixed = log_item_51221 ~ time + I(time^2),
    random = ~ time + I(time^2)
  ),
  item_220045 = list(fixed = item_220045 ~ time, random = ~ time)
)

Y <- list(
  type = "factor",
  Y = stroke_cohort %>%
    filter(hadm_id %in% id_train) %>%
    transmute(hadm_id, event = death_28d) %>%
    distinct(hadm_id, .keep_all = TRUE)
)

cat("训练集 n =", nrow(fixedData_train),
    "| 事件率 =", round(mean(Y$Y$event), 3), "\n")

## ========== 6. 拟合 dynforest ==========
res_dyn <- dynforest(
  timeData = timeData_train,
  fixedData = fixedData_train,
  timeVar = "time",
  idVar = "hadm_id",
  timeVarModel = timeVarModel,
  Y = Y,
  ntree = 100,
  mtry = 5,
  nodesize = 10,
  minsplit = 20,
  ncores = max(1L, parallel::detectCores() - 1L),
  seed = 1234
)

print(summary(res_dyn))

## ========== 7. 测试集预测 ==========
timeData_test <- stroke_long %>%
  filter(hadm_id %in% id_test) %>%
  select(hadm_id, time, all_of(long_vars_model))

fixedData_test <- stroke_cohort %>%
  filter(hadm_id %in% id_test) %>%
  mutate(mechvent = factor(mechvent)) %>%
  select(hadm_id, age, charlson_comorbidity_index, mechvent, preiculos) %>%
  distinct(hadm_id, .keep_all = TRUE)

pred_dyn <- predict(
  object = res_dyn,
  timeData = timeData_test,
  fixedData = fixedData_test,
  idVar = "hadm_id",
  timeVar = "time",
  t0 = landmark
)

pred_df <- data.frame(
  hadm_id = fixedData_test$hadm_id,
  pred_class = pred_dyn$pred_indiv,
  pred_prob = pred_dyn$pred_indiv_proba,
  event_true = stroke_cohort$death_28d[
    match(fixedData_test$hadm_id, stroke_cohort$hadm_id)
  ]
)

cat("\n测试集预测结果（前 10 行）：\n")
print(head(pred_df, 10))

## ========== 8. 测试集 AUC（可选） ==========
if (requireNamespace("pROC", quietly = TRUE)) {
  roc_obj <- pROC::roc(
    response = pred_df$event_true,
    predictor = pred_df$pred_prob,
    quiet = TRUE
  )
  cat("\n测试集 AUC =", round(as.numeric(pROC::auc(roc_obj)), 3), "\n")
} else {
  cat("\n安装 pROC 包后可计算 AUC: install.packages('pROC')\n")
}

## ========== 9. 变量重要性（可选，耗时） ==========
# res_vimp <- compute_vimp(dynforest_obj = res_dyn, ncores = 2, seed = 1234)
# plot(res_vimp, PCT = TRUE)
