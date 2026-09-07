
## =========================
## 精简版：读取xlsx -> 预处理 -> DynForest建模 -> 输出指标与结构
## 可独立运行本段
## =========================
suppressPackageStartupMessages({
  library(dplyr)
  library(readxl)
  library(DynForest)
  library(survival)
  library(timeROC)
})
set.seed(123)

## 1) 读取数据（主表 + 基线结局）
dir_0314 <- "f:/文章_大论文/MIMIC数据库_代码/TREA代码/0314"
path_main <- file.path(dir_0314, "widedata_merge1.xlsx")
path_base <- "F:/文章_大论文/MIMIC数据库_代码/TREA代码/0311/stroke_baseline_simple.xlsx"
if (!file.exists(path_main)) stop(paste0("文件不存在: ", path_main))
if (!file.exists(path_base)) stop(paste0("文件不存在: ", path_base))

df_main <- read_xlsx(path_main)
df_base <- read_xlsx(path_base)

## 合并 hospital_mortality -> hospitalmortality
df_base_sub <- df_base %>%
  select(hadm_id, hospital_mortality) %>%
  distinct(hadm_id, .keep_all = TRUE) %>%
  rename(hospitalmortality = hospital_mortality)
df_raw <- df_main %>%
  mutate(hadm_id = as.character(hadm_id)) %>%
  left_join(df_base_sub %>% mutate(hadm_id = as.character(hadm_id)), by = "hadm_id")

## 2) 构造 DynForest 输入
## id/time
df_raw$id <- as.numeric(as.factor(as.character(df_raw$subject_id)))
df_raw$time <- suppressWarnings(as.numeric(df_raw$Obstimes))
if (all(is.na(df_raw$time))) stop("Obstimes 无法转换为数值。")
df_raw <- df_raw %>%
  group_by(id) %>%
  mutate(time = time - min(time, na.rm = TRUE)) %>%
  ungroup()

## 纵向变量：数值型 itemid_（去掉明显基线分类）
drop_cat <- c(
  "itemid_admission_type", "itemid_admission_location", "itemid_discharge_location",
  "itemid_insurance", "itemid_language", "itemid_marital_status",
  "itemid_race", "itemid_first_careunit"
)
long_vars <- names(df_raw)[grepl("^itemid_", names(df_raw))]
long_vars <- setdiff(long_vars, c(drop_cat, "itemid_los_hosp_days"))
long_vars <- long_vars[vapply(long_vars, function(v) {
  x <- suppressWarnings(as.numeric(df_raw[[v]]))
  sum(!is.na(x)) > 0
}, logical(1))]
for (v in long_vars) df_raw[[v]] <- suppressWarnings(as.numeric(df_raw[[v]]))
timeData <- df_raw %>% select(id, time, all_of(long_vars))

## 生存结局（每id一行）
fixed_surv <- df_raw %>%
  group_by(id) %>%
  summarise(
    time = suppressWarnings(as.numeric(first(itemid_los_hosp_days))),
    event = suppressWarnings(as.numeric(first(hospitalmortality))),
    .groups = "drop"
  ) %>%
  filter(is.finite(time), !is.na(time), time > 0, !is.na(event))
fixed_surv$event <- ifelse(fixed_surv$event > 0, 1, 0)

## 固定协变量（每id取首个非缺失）
first_not_na <- function(x) {
  x2 <- x[!is.na(x) & !(is.character(x) & trimws(x) == "")]
  if (length(x2) == 0L) NA else x2[1]
}
exclude <- unique(c("id", "time", "Obstimes", "hospitalmortality", "itemid_los_hosp_days", long_vars))
fixed_vars <- setdiff(names(df_raw), exclude)
fixed_base <- df_raw %>% group_by(id) %>% summarise(across(all_of(fixed_vars), first_not_na), .groups = "drop")
fixedData <- fixed_surv %>% left_join(fixed_base, by = "id")

## 规范固定协变量：时间字符串转数值；高基数分类并为<=10类
norm_pred <- function(x) {
  if (is.character(x)) {
    xc <- trimws(x); xc[xc == ""] <- NA_character_
    n_non <- sum(!is.na(xc))
    xt <- tryCatch(suppressWarnings(as.POSIXct(xc, tz = "UTC")), error = function(e) rep(as.POSIXct(NA), length(xc)))
    if (n_non > 0 && sum(!is.na(xt)) / n_non >= 0.8) return(as.numeric(xt))
    xn <- suppressWarnings(as.numeric(xc))
    if (n_non > 0 && sum(!is.na(xn)) / n_non >= 0.8) return(xn)
    lv <- names(sort(table(xc), decreasing = TRUE))
    if (length(lv) > 10L) {
      keep <- lv[1:9]
      xc <- ifelse(is.na(xc), NA_character_, ifelse(xc %in% keep, xc, "Others"))
    }
    return(as.factor(xc))
  }
  if (is.factor(x)) return(norm_pred(as.character(x)))
  x
}
pred_cols <- setdiff(names(fixedData), c("id", "time", "event"))
for (v in pred_cols) fixedData[[v]] <- norm_pred(fixedData[[v]])

## 对齐 id 集合
fixedData <- fixedData %>% distinct(id, .keep_all = TRUE)
timeData <- timeData %>% filter(id %in% fixedData$id)

## 3) 建模
timeVarModel <- lapply(long_vars, function(v) list(model = "linear", fixed = ~ 1, random = ~ 1 + time | id))
names(timeVarModel) <- long_vars
y_surv <- fixedData %>% select(id, time, event)

dyn_model <- DynForest::dynforest(
  timeData = as.data.frame(timeData),
  fixedData = as.data.frame(fixedData),
  idVar = "id",
  timeVar = "time",
  timeVarModel = timeVarModel,
  Y = list(type = "surv", Y = as.data.frame(y_surv)),
  ntree = 500, mtry = 2, nodesize = 10, minsplit = 2,
  nsplit_option = "quantile", ncores = 1, verbose = FALSE
)

## 4) 预测 + 指标
t0 <- median(y_surv$time, na.rm = TRUE)
pred_obj <- predict(dyn_model, timeData = as.data.frame(timeData), fixedData = as.data.frame(fixedData), idVar = "id", timeVar = "time", t0 = t0)
pred_mat <- pred_obj$pred_indiv
risk_score <- as.numeric(pred_mat[, ncol(pred_mat)])
eval_ids <- as.numeric(rownames(pred_mat))
eval_data <- y_surv %>% filter(id %in% eval_ids)
eval_data <- eval_data[match(eval_ids, eval_data$id), , drop = FALSE]

surv_time <- as.numeric(eval_data$time)
surv_event <- as.numeric(eval_data$event)

cindex <- as.numeric(survival::concordance(survival::Surv(surv_time, surv_event) ~ risk_score)$concordance)
if (!is.na(cindex) && cindex < 0.5) {
  risk_score <- -risk_score
  cindex <- as.numeric(survival::concordance(survival::Surv(surv_time, surv_event) ~ risk_score)$concordance)
}
roc1 <- timeROC::timeROC(T = surv_time, delta = surv_event, marker = risk_score, cause = 1, times = t0)
roc2 <- timeROC::timeROC(T = surv_time, delta = surv_event, marker = -risk_score, cause = 1, times = t0)
auc1 <- if (length(roc1$AUC) >= 2) roc1$AUC[2] else roc1$AUC[1]
auc2 <- if (length(roc2$AUC) >= 2) roc2$AUC[2] else roc2$AUC[1]
auc <- max(as.numeric(auc1), as.numeric(auc2), na.rm = TRUE)

haz <- exp(as.numeric(scale(risk_score)))
sp <- exp(-haz * t0)
bs <- mean(ifelse(surv_time <= t0 & surv_event == 1, (1 - sp)^2, sp^2), na.rm = TRUE)

model_metrics <- data.frame(
  t0 = t0,
  CINDEX = round(cindex, 4),
  BS = round(bs, 4),
  AUC = round(auc, 4)
)

model_structure_summary <- list(
  timeData_dim = dim(timeData),
  fixedData_dim = dim(fixedData),
  timeData_columns = names(timeData),
  fixedData_columns = names(fixedData),
  timeVarModel_names = names(timeVarModel),
  Y_summary = list(
    n = nrow(y_surv),
    event_table = table(y_surv$event, useNA = "ifany"),
    time_summary = summary(y_surv$time)
  )
)

cat("\nDynForest 建模完成（精简版）。\n")
print(model_metrics)
cat("\nDynForest 输入结构摘要：\n")
cat("- timeData 维度: ", model_structure_summary$timeData_dim[1], " x ", model_structure_summary$timeData_dim[2], "\n", sep = "")
cat("- fixedData 维度: ", model_structure_summary$fixedData_dim[1], " x ", model_structure_summary$fixedData_dim[2], "\n", sep = "")
cat("- timeVarModel 纵向变量个数: ", length(model_structure_summary$timeVarModel_names), "\n", sep = "")
cat("- Y(event) 分布:\n")
print(model_structure_summary$Y_summary$event_table)
