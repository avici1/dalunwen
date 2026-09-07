library(dplyr)
library(readxl)

options(scipen = 999)


## =========================
## 1) 读取 wide_data_merge1.xlsx
## =========================
script_dir <- "f:/文章_大论文/MIMIC数据库_代码/TREA代码/0314"
input_candidates <- c(
  file.path(script_dir, "wide_data_merge1.xlsx"),
  file.path(script_dir, "widedata_merge1.xlsx"),
  file.path(script_dir, "0314_widedata_merge.xlsx")
)
input_path <- input_candidates[file.exists(input_candidates)][1]
if (is.na(input_path)) {
  stop("未找到输入文件：wide_data_merge1.xlsx / widedata_merge1.xlsx / 0314_widedata_merge.xlsx")
}
dat_raw <- read_xlsx(input_path)



#######################
## 合并 stroke_baseline_simple.xlsx 的 hospitalmortality 到 dat_raw（保持 dat_raw 行数不变）
baseline_path <- "F:/文章_大论文/MIMIC数据库_代码/TREA代码/0311/stroke_baseline_simple.xlsx"
if (!file.exists(baseline_path)) {
  stop(paste0("文件不存在: ", baseline_path))
}

baseline_raw <- read_xlsx(baseline_path)

key_cands <- c("hadm_id", "subject_id", "subjectid", "stay_id")
merge_key <- key_cands[key_cands %in% names(dat_raw) & key_cands %in% names(baseline_raw)][1]
if (is.na(merge_key)) {
  stop("widedata_merge1 与 stroke_baseline_simple 未找到共同连接键（候选：hadm_id/subject_id/subjectid/stay_id）。")
}

hm_col_cands <- c("hospitalmortality", "hospital_mortality", "inhospital_mortality", "mortality_hosp")
hm_col <- hm_col_cands[hm_col_cands %in% names(baseline_raw)][1]
if (is.na(hm_col)) {
  stop("stroke_baseline_simple.xlsx 中未找到 hospitalmortality 列。")
}

baseline_sub <- baseline_raw %>%
  select(all_of(c(merge_key, hm_col))) %>%
  distinct(across(all_of(merge_key)), .keep_all = TRUE) %>%
  rename(hospitalmortality = all_of(hm_col))

dat_raw <- dat_raw %>%
  mutate(.merge_key_tmp = as.character(.data[[merge_key]])) %>%
  left_join(
    baseline_sub %>% mutate(.merge_key_tmp = as.character(.data[[merge_key]])) %>% select(-all_of(merge_key)),
    by = ".merge_key_tmp"
  ) %>%
  select(-.merge_key_tmp)
cat("已合并 hospitalmortality：连接键=", merge_key, "，当前行数=", nrow(dat_raw), "\n", sep = "")

#######################




















## =========================
## 2) 统一 id / time 字段
## =========================
pick_first <- function(cands, nms) {
  hit <- cands[cands %in% nms]
  if (length(hit) == 0L) NA_character_ else hit[1]
}

id_col <- pick_first(c("subject_id", "subjectid", "ID", "id"), names(dat_raw))
if (is.na(id_col)) stop("未识别到 id 列（候选：subject_id/subjectid/ID/id）")

time_col_raw <- pick_first(c("Obstimes", "obstimes", "t", "time", "charttime"), names(dat_raw))
if (is.na(time_col_raw)) stop("未识别到时间列（候选：Obstimes/t/time/charttime）")

dat <- dat_raw %>%
  rename(id = all_of(id_col))

if (time_col_raw %in% c("charttime")) {
  chart_parsed <- as.POSIXct(dat[[time_col_raw]], tz = "UTC")
  if (all(is.na(chart_parsed))) {
    chart_num <- suppressWarnings(as.numeric(dat[[time_col_raw]]))
    if (all(is.na(chart_num))) stop("charttime 无法解析为时间或数值。")
    dat$time <- chart_num
  } else {
    dat$time <- as.numeric(chart_parsed)
  }
} else {
  dat$time <- suppressWarnings(as.numeric(dat[[time_col_raw]]))
  if (all(is.na(dat$time))) stop("时间列无法转换为数值。")
}

## 每个患者内将 time 重标化为从 0 开始
dat <- dat %>%
  group_by(id) %>%
  mutate(time = time - min(time, na.rm = TRUE)) %>%
  ungroup()

## DynForest 通常使用数值 id
dat$id <- as.numeric(as.factor(as.character(dat$id)))

## =========================
## 3) 构造 longitudinal_data
## =========================
long_candidates <- names(dat)[grepl("^itemid_", names(dat))]
## 剔除明显分类列（之前你已做过合并）
drop_cat <- c(
  "itemid_admission_type", "itemid_admission_location", "itemid_discharge_location",
  "itemid_insurance", "itemid_language", "itemid_marital_status",
  "itemid_race", "itemid_first_careunit"
)
long_vars <- setdiff(long_candidates, drop_cat)

## 仅保留可转为数值的纵向变量
is_numeric_like <- vapply(long_vars, function(v) {
  x <- suppressWarnings(as.numeric(dat[[v]]))
  sum(!is.na(x)) > 0
}, logical(1))
long_vars <- long_vars[is_numeric_like]
if (length(long_vars) == 0L) stop("未识别到可用于纵向建模的数值型 itemid_ 变量。")

for (v in long_vars) {
  dat[[v]] <- suppressWarnings(as.numeric(dat[[v]]))
}

longitudinal_data <- dat %>%
  select(id, time, all_of(long_vars))

## =========================
## 4) 构造生存结局 + 固定协变量
## =========================
event_col <- pick_first(
  c("event", "status", "outcome", "death", "死亡", "mortality_28d", "mortality_hosp", "hospitalmortality", "hospital_mortality", "inhospital_mortality"),
  names(dat)
)
time_surv_col <- pick_first(
  c("obs_time", "surv_time", "time_to_event", "followup_time", "los_hospital", "los_icu", "itemid_los_hosp_days"),
  names(dat)
)

if (is.na(time_surv_col)) {
  stop("未识别到生存时间列。请提供 obs_time/surv_time/itemid_los_hosp_days。")
}
if (is.na(event_col)) {
  dat$event_dyn <- 1
  event_col <- "event_dyn"
  warning("未识别到事件列(event/status等)，已暂设 event=1。后续建模前请替换为真实事件指示。")
}

survival_data <- dat %>%
  group_by(id) %>%
  summarise(
    time = suppressWarnings(as.numeric(first(.data[[time_surv_col]]))),
    event = suppressWarnings(as.numeric(first(.data[[event_col]]))),
    .groups = "drop"
  )

## 固定协变量：剔除 id/time/生存列/纵向列后，其余按每个 id 取首个非缺失值
exclude_cols <- unique(c("id", "time", time_surv_col, event_col, long_vars, time_col_raw))
fixed_vars <- setdiff(names(dat), exclude_cols)

first_not_na <- function(x) {
  x2 <- x[!is.na(x) & !(is.character(x) & trimws(x) == "")]
  if (length(x2) == 0L) return(NA)
  x2[1]
}

if (length(fixed_vars) > 0L) {
  baseline_data <- dat %>%
    group_by(id) %>%
    summarise(across(all_of(fixed_vars), first_not_na), .groups = "drop")
  fixed_data <- survival_data %>% left_join(baseline_data, by = "id")
} else {
  fixed_data <- survival_data
}

## 规范 fixed_data 预测变量类型：
## 1) 时间字符串优先转为数值时间戳；2) 高基数字符/因子变量合并到 <=10 类
normalize_fixed_predictor <- function(x) {
  if (is.character(x)) {
    x_chr <- trimws(x)
    x_chr[x_chr == ""] <- NA_character_
    non_na_n <- sum(!is.na(x_chr))

    # 先尝试按时间字符串解析（避免被当作高基数因子）
    x_time <- tryCatch(
      suppressWarnings(as.POSIXct(x_chr, tz = "UTC")),
      error = function(e) rep(as.POSIXct(NA), length(x_chr))
    )
    if (non_na_n > 0 && sum(!is.na(x_time)) / non_na_n >= 0.8) {
      return(as.numeric(x_time))
    }

    # 再尝试数值化
    x_num <- suppressWarnings(as.numeric(x_chr))
    if (non_na_n > 0 && sum(!is.na(x_num)) / non_na_n >= 0.8) {
      return(x_num)
    }

    # 其余作为分类变量，限制最多10类（前9类 + Others）
    lev <- names(sort(table(x_chr), decreasing = TRUE))
    if (length(lev) > 10L) {
      keep <- lev[1:9]
      x_chr <- ifelse(is.na(x_chr), NA_character_, ifelse(x_chr %in% keep, x_chr, "Others"))
    }
    return(as.factor(x_chr))
  }

  if (is.factor(x)) {
    x_chr <- as.character(x)
    x_chr[x_chr == ""] <- NA_character_
    lev <- names(sort(table(x_chr), decreasing = TRUE))
    if (length(lev) > 10L) {
      keep <- lev[1:9]
      x_chr <- ifelse(is.na(x_chr), NA_character_, ifelse(x_chr %in% keep, x_chr, "Others"))
    }
    return(as.factor(x_chr))
  }

  x
}

pred_cols_fixed <- setdiff(names(fixed_data), c("id", "time", "event"))
for (v in pred_cols_fixed) {
  fixed_data[[v]] <- normalize_fixed_predictor(fixed_data[[v]])
}

## =========================
## 5) 组装 DynForest 输入对象
## =========================
timeVarModel <- lapply(long_vars, function(v) {
  list(
    model = "linear",
    fixed = ~ 1,
    random = ~ 1 + time | id
  )
})
names(timeVarModel) <- long_vars

dynforest_input <- list(
  timeData = as.data.frame(longitudinal_data),
  fixedData = as.data.frame(fixed_data),
  idVar = "id",
  timeVar = "time",
  timeVarModel = timeVarModel,
  Y = list(
    type = "surv",
    Y = data.frame(
      id = fixed_data$id,
      time = fixed_data$time,
      event = as.numeric(fixed_data$event)
    )
  )
)

## 可直接用于：
## dyn_model <- DynForest::dynforest(
##   timeData = dynforest_input$timeData,
##   fixedData = dynforest_input$fixedData,
##   idVar = dynforest_input$idVar,
##   timeVar = dynforest_input$timeVar,
##   timeVarModel = dynforest_input$timeVarModel,
##   Y = dynforest_input$Y
## )

cat("数据整理完成：\n")
cat("- 输入文件: ", input_path, "\n", sep = "")
cat("- longitudinal_data 维度: ", nrow(dynforest_input$timeData), " x ", ncol(dynforest_input$timeData), "\n", sep = "")
cat("- fixed_data 维度: ", nrow(dynforest_input$fixedData), " x ", ncol(dynforest_input$fixedData), "\n", sep = "")
cat("- 纵向变量数: ", length(long_vars), "\n", sep = "")











#############################

## DynForest 建模 + 评估（CINDEX / BS / AUC）
if (!requireNamespace("DynForest", quietly = TRUE)) stop("请先安装 DynForest 包")
if (!requireNamespace("survival", quietly = TRUE)) stop("请先安装 survival 包")
if (!requireNamespace("timeROC", quietly = TRUE)) stop("请先安装 timeROC 包")
suppressPackageStartupMessages(library(survival))

## 清理生存结局，避免 NA / 非法值影响建模
fixed_data_model <- dynforest_input$fixedData %>%
  mutate(
    time = suppressWarnings(as.numeric(time)),
    event = suppressWarnings(as.numeric(event))
  ) %>%
  filter(is.finite(time), !is.na(time), time > 0, !is.na(event))
fixed_data_model$event <- ifelse(fixed_data_model$event > 0, 1, 0)

## timeData 仅保留 fixed_data_model 中 id
time_data_model <- dynforest_input$timeData %>%
  filter(id %in% fixed_data_model$id)

## Y（survival）
y_surv <- data.frame(
  id = fixed_data_model$id,
  time = fixed_data_model$time,
  event = fixed_data_model$event
)

set.seed(123)
dyn_model <- DynForest::dynforest(
  timeData      = as.data.frame(time_data_model),
  fixedData     = as.data.frame(fixed_data_model),
  idVar         = "id",
  timeVar       = "time",
  timeVarModel  = dynforest_input$timeVarModel,
  Y             = list(type = "surv", Y = y_surv),
  ntree         = 500,
  mtry          = 2,
  nodesize      = 10,
  minsplit      = 2,
  nsplit_option = "quantile",
  ncores        = 1,
  verbose       = FALSE
)

## 使用 predict.dynforest 提取个体预测，避免 rf 结构差异导致风险分数全 0
t0 <- median(fixed_data_model$time, na.rm = TRUE)

pred_obj <- predict(
  dyn_model,
  timeData = as.data.frame(time_data_model),
  fixedData = as.data.frame(fixed_data_model),
  idVar = "id",
  timeVar = "time",
  t0 = t0
)

pred_mat <- pred_obj$pred_indiv
if (is.null(dim(pred_mat)) || nrow(pred_mat) == 0 || ncol(pred_mat) == 0) {
  stop("predict.dynforest 未返回有效的 pred_indiv，无法计算模型指标。")
}

## 取最后一个时间点的个体预测作为风险分数
risk_score <- as.numeric(pred_mat[, ncol(pred_mat)])
eval_ids <- as.numeric(rownames(pred_mat))
if (anyNA(eval_ids)) {
  stop("pred_indiv 行名无法识别为 id，无法对齐生存结局。")
}

eval_data <- fixed_data_model %>%
  filter(id %in% eval_ids) %>%
  select(id, time, event)
eval_data <- eval_data[match(eval_ids, eval_data$id), , drop = FALSE]

surv_time <- as.numeric(eval_data$time)
surv_event <- as.numeric(eval_data$event)

## C-index（若方向相反则翻转）
cidx_obj <- survival::concordance(survival::Surv(surv_time, surv_event) ~ risk_score)
cindex <- as.numeric(cidx_obj$concordance)
if (!is.na(cindex) && cindex < 0.5) {
  risk_score <- -risk_score
  cidx_obj <- survival::concordance(survival::Surv(surv_time, surv_event) ~ risk_score)
  cindex <- as.numeric(cidx_obj$concordance)
}

## AUC（time-dependent），尝试正反方向，取较大值
roc_obj <- timeROC::timeROC(T = surv_time, delta = surv_event, marker = risk_score, cause = 1, times = t0)
roc_obj_rev <- timeROC::timeROC(T = surv_time, delta = surv_event, marker = -risk_score, cause = 1, times = t0)
auc <- if (length(roc_obj$AUC) >= 2) roc_obj$AUC[2] else roc_obj$AUC[1]
auc_rev <- if (length(roc_obj_rev$AUC) >= 2) roc_obj_rev$AUC[2] else roc_obj_rev$AUC[1]
auc <- max(as.numeric(auc), as.numeric(auc_rev), na.rm = TRUE)

## BS（基于缩放风险分数的简化估计）
hazard <- exp(as.numeric(scale(risk_score)))
surv_prob <- exp(-hazard * t0)
brier <- ifelse(surv_time <= t0 & surv_event == 1, (1 - surv_prob)^2, surv_prob^2)
bs <- mean(brier, na.rm = TRUE)

model_metrics <- data.frame(
  t0 = t0,
  CINDEX = round(cindex, 4),
  BS = round(bs, 4),
  AUC = round(auc, 4)
)

cat("\nDynForest 建模完成。\n")
print(model_metrics)

## 输出模型结构摘要（timeData / fixedData / timeVarModel / Y）
model_structure_summary <- list(
  timeData_dim = dim(time_data_model),
  fixedData_dim = dim(fixed_data_model),
  timeData_columns = names(time_data_model),
  fixedData_columns = names(fixed_data_model),
  timeVarModel_names = names(dynforest_input$timeVarModel),
  Y_summary = list(
    n = nrow(y_surv),
    event_table = table(y_surv$event, useNA = "ifany"),
    time_summary = summary(y_surv$time)
  )
)

cat("\nDynForest 输入结构摘要：\n")
cat("- timeData 维度: ", model_structure_summary$timeData_dim[1], " x ", model_structure_summary$timeData_dim[2], "\n", sep = "")
cat("- fixedData 维度: ", model_structure_summary$fixedData_dim[1], " x ", model_structure_summary$fixedData_dim[2], "\n", sep = "")
cat("- timeVarModel 纵向变量个数: ", length(model_structure_summary$timeVarModel_names), "\n", sep = "")
cat("- Y(event) 分布:\n")
print(model_structure_summary$Y_summary$event_table)
cat("- Y(time) 摘要:\n")
print(model_structure_summary$Y_summary$time_summary)






##############RSFLC代码整合##############


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

#########################JM###################################

## =========================
## JM（参考 cal_3 思路）：读取xlsx -> 数据处理 -> 多次尝试建模 -> 输出AUC/BS/CINDEX
## =========================
suppressPackageStartupMessages({
  library(dplyr)
  library(readxl)
  library(survival)
  library(timeROC)
  library(nlme)
  library(JM)
})
set.seed(123)

## 1) 读取并合并数据
jm_dir <- "f:/文章_大论文/MIMIC数据库_代码/TREA代码/0314"
jm_main_path <- file.path(jm_dir, "widedata_merge1.xlsx")
jm_base_path <- "F:/文章_大论文/MIMIC数据库_代码/TREA代码/0311/stroke_baseline_simple.xlsx"
if (!file.exists(jm_main_path)) stop(paste0("文件不存在: ", jm_main_path))
if (!file.exists(jm_base_path)) stop(paste0("文件不存在: ", jm_base_path))

jm_main <- read_xlsx(jm_main_path)
jm_base <- read_xlsx(jm_base_path)

jm_base_sub <- jm_base %>%
  dplyr::select(hadm_id, hospital_mortality) %>%
  dplyr::distinct(hadm_id, .keep_all = TRUE) %>%
  dplyr::rename(hospitalmortality = hospital_mortality)

jm_raw <- jm_main %>%
  mutate(hadm_id = as.character(hadm_id)) %>%
  left_join(jm_base_sub %>% mutate(hadm_id = as.character(hadm_id)), by = "hadm_id")

## 2) 构造基础字段（ID/t/obs_time/event）
jm_raw$ID <- as.numeric(as.factor(as.character(jm_raw$subject_id)))
jm_raw$obs_time <- suppressWarnings(as.numeric(jm_raw$itemid_los_hosp_days))
jm_raw$event <- suppressWarnings(as.numeric(jm_raw$hospitalmortality))
jm_raw$event <- ifelse(is.na(jm_raw$event), NA, ifelse(jm_raw$event > 0, 1, 0))

## t优先使用 charttime 计算“天”为单位的相对时间；若失败再退回 Obstimes
if ("charttime" %in% names(jm_raw)) {
  ct <- tryCatch(as.POSIXct(jm_raw$charttime, tz = "UTC"), error = function(e) rep(as.POSIXct(NA), nrow(jm_raw)))
  ct_num <- as.numeric(ct)
  if (sum(!is.na(ct_num)) >= max(100, floor(0.5 * nrow(jm_raw)))) {
    jm_raw$chart_num <- ct_num
    jm_raw <- jm_raw %>%
      group_by(ID) %>%
      mutate(t = (chart_num - min(chart_num, na.rm = TRUE)) / (24 * 3600)) %>%
      ungroup() %>%
      dplyr::select(-chart_num)
  } else {
    jm_raw$t <- suppressWarnings(as.numeric(jm_raw$Obstimes)) / 24
  }
} else {
  jm_raw$t <- suppressWarnings(as.numeric(jm_raw$Obstimes)) / 24
}

jm_raw <- jm_raw %>%
  filter(!is.na(ID), !is.na(t), !is.na(obs_time), !is.na(event)) %>%
  filter(t <= obs_time)

## 3) 生成6个基线协变量 V1..V6（与 cal_3 形式一致）
to_num01 <- function(x) {
  if (is.logical(x)) return(as.numeric(x))
  if (is.numeric(x)) return(as.numeric(x))
  xc <- trimws(as.character(x))
  xc[xc == ""] <- NA_character_
  xn <- suppressWarnings(as.numeric(xc))
  ok <- sum(!is.na(xn))
  if (ok >= max(10, floor(0.5 * sum(!is.na(xc))))) return(xn)
  xf <- as.factor(xc)
  as.numeric(xf)
}

baseline_first <- function(df, col) {
  df %>% group_by(ID) %>% summarise(val = dplyr::first(.data[[col]]), .groups = "drop")
}

candidate_covars <- c(
  "itemid_anchor_age", "itemid_gender", "itemid_anchor_year",
  "itemid_insurance", "itemid_marital_status", "itemid_race",
  "itemid_admission_type", "itemid_first_careunit"
)
candidate_covars <- intersect(candidate_covars, names(jm_raw))
if (length(candidate_covars) == 0L) stop("未找到可用基线协变量。")

cov_list <- lapply(candidate_covars, function(v) {
  x <- baseline_first(jm_raw, v)
  x$val <- to_num01(x$val)
  names(x)[2] <- v
  x
})
baseline_cov <- Reduce(function(a, b) left_join(a, b, by = "ID"), cov_list)

## 选择缺失最少的前6个作为 V1..V6
na_rate <- sapply(setdiff(names(baseline_cov), "ID"), function(v) mean(is.na(baseline_cov[[v]])))
pick6 <- names(sort(na_rate))[seq_len(min(6, length(na_rate)))]
for (i in seq_along(pick6)) names(baseline_cov)[names(baseline_cov) == pick6[i]] <- paste0("V", i)
if (length(pick6) < 6) {
  for (k in (length(pick6) + 1):6) baseline_cov[[paste0("V", k)]] <- 0
}
baseline_cov <- baseline_cov %>% dplyr::select(ID, V1, V2, V3, V4, V5, V6)

jm_data0 <- jm_raw %>% left_join(baseline_cov, by = "ID")
for (v in c("V1", "V2", "V3", "V4", "V5", "V6")) {
  jm_data0[[v]][is.na(jm_data0[[v]])] <- median(jm_data0[[v]], na.rm = TRUE)
  if (!is.finite(jm_data0[[v]][1])) jm_data0[[v]] <- 0
}

## 4) 选择候选纵向变量并不断尝试 JM
drop_cat_jm <- c(
  "itemid_admission_type", "itemid_admission_location", "itemid_discharge_location",
  "itemid_insurance", "itemid_language", "itemid_marital_status", "itemid_race",
  "itemid_first_careunit", "itemid_los_hosp_days"
)
long_cands <- names(jm_data0)[grepl("^itemid_", names(jm_data0))]
long_cands <- setdiff(long_cands, drop_cat_jm)
long_cands <- setdiff(long_cands, c("itemid_anchor_age", "itemid_anchor_year", "itemid_gender", "itemid_anchor_year_group"))

score_var <- function(v) {
  y <- suppressWarnings(as.numeric(jm_data0[[v]]))
  not_na <- sum(!is.na(y))
  df_tmp <- data.frame(ID = jm_data0$ID, y = y)
  df_tmp <- df_tmp[!is.na(df_tmp$y), , drop = FALSE]
  n_by_id <- table(df_tmp$ID)
  n_id2 <- sum(n_by_id >= 2)
  not_na + 10 * n_id2
}
if (length(long_cands) == 0L) stop("未找到可用纵向变量（itemid_）。")
long_cands <- long_cands[order(sapply(long_cands, score_var), decreasing = TRUE)]
long_try <- head(long_cands, 8)  # 不断尝试的候选变量

safe_metric <- function(x, digits = 4) ifelse(is.finite(x), round(x, digits), NA_real_)

fit_one_jm <- function(data_all, y_var, t0) {
  d <- data_all %>%
    mutate(Y = suppressWarnings(as.numeric(.data[[y_var]]))) %>%
    filter(!is.na(.data$Y))
  if (sd(d$Y, na.rm = TRUE) <= 1e-8) stop("Y方差接近0，跳过该变量。")
  d <- d %>%
    group_by(ID) %>%
    arrange(t, .by_group = TRUE) %>%
    mutate(
      t = as.numeric(t),
      t = t + (row_number() - 1) * 1e-4
    ) %>%
    ungroup()
  d$Y <- as.numeric(scale(d$Y))
  id_count <- d %>% count(ID, name = "n")
  ids_n2 <- id_count %>% filter(n >= 2) %>% pull(ID)
  use_random_slope <- length(ids_n2) >= 30
  if (use_random_slope) {
    d <- d %>% filter(ID %in% ids_n2)
  } else {
    ids_n1 <- id_count %>% filter(n >= 1) %>% pull(ID)
    d <- d %>% filter(ID %in% ids_n1)
  }
  if (nrow(d) < 80 || length(unique(d$ID)) < 30) stop("样本不足，跳过该变量。")

  form_candidates <- list(
    as.formula("Y ~ t"),
    as.formula("Y ~ t + V1 + V2"),
    as.formula("Y ~ t + V1 + V2 + V3 + V4 + V5 + V6")
  )
  rand_candidates <- if (use_random_slope) list(~ 1 | ID, ~ t | ID) else list(~ 1 | ID)

  fit <- NULL
  lme_fit <- NULL
  cox_fit <- NULL
  used_form <- NULL
  last_err <- NULL
  id_pool <- unique(d$ID)
  sample_sizes <- unique(c(length(id_pool), 300, 200, 150, 100))
  sample_sizes <- sample_sizes[sample_sizes <= length(id_pool)]

  for (ss in sample_sizes) {
    d_fit <- if (ss < length(id_pool)) {
      keep_ids <- sample(id_pool, ss)
      d %>% filter(ID %in% keep_ids)
    } else {
      d
    }

    for (fm in form_candidates) {
      for (rfm in rand_candidates) {
        fit_try <- tryCatch({
          lme_try <- nlme::lme(
            fixed = fm,
            random = rfm,
            data = d_fit,
            na.action = na.omit,
            control = nlme::lmeControl(opt = "optim")
          )
          surv_data_try <- d_fit[!duplicated(d_fit$ID), c("ID", "obs_time", "event")]
          cox_try <- survival::coxph(
            survival::Surv(obs_time, event) ~ 1,
            data = surv_data_try,
            x = TRUE,
            model = TRUE
          )
          jm_try <- JM::jointModel(
            lmeObject = lme_try,
            survObject = cox_try,
            timeVar = "t",
            method = "weibull-PH-aGH"
          )
          list(jm = jm_try, lme = lme_try, cox = cox_try, d_fit = d_fit)
        }, error = function(e) {
          last_err <<- conditionMessage(e)
          NULL
        })

        if (!is.null(fit_try)) {
          fit <- fit_try$jm
          lme_fit <- fit_try$lme
          cox_fit <- fit_try$cox
          d <- fit_try$d_fit
          used_form <- fm
          break
        }
      }
      if (!is.null(fit)) break
    }
    if (!is.null(fit)) break
  }
  if (is.null(fit)) stop(paste0("所有固定/随机效应组合均拟合失败。最后错误: ", last_err))

  surv_data <- d[!duplicated(d$ID), c("ID", "obs_time", "event", "V1", "V2", "V3", "V4", "V5", "V6")]
  beta <- nlme::fixed.effects(lme_fit)
  assoc_candidates <- c("Assoct", "alpha", "AssoctE", "AssoctEV")
  assoc_val <- NA_real_
  for (nm in assoc_candidates) {
    if (!is.null(fit$coefficients[[nm]])) {
      assoc_val <- as.numeric(fit$coefficients[[nm]])[1]
      break
    }
  }
  if (!is.finite(assoc_val)) assoc_val <- 1

  re <- nlme::ranef(lme_fit)
  if (is.null(dim(re))) re <- matrix(re, ncol = 1)
  re_int <- re[, 1]
  re_slope <- if (ncol(re) >= 2) re[, 2] else rep(0, length(re_int))

  surv_data$t <- t0
  rhs_terms <- attr(terms(used_form), "term.labels")
  rhs_form <- if (length(rhs_terms) > 0) {
    as.formula(paste("~", paste(rhs_terms, collapse = " + ")))
  } else {
    ~ 1
  }
  X <- model.matrix(rhs_form, data = surv_data)
  beta_use <- beta[colnames(X)]
  beta_use[is.na(beta_use)] <- 0
  fixed_part <- as.numeric(X %*% beta_use)
  rand_part <- re_int + re_slope * t0
  risk <- assoc_val * (fixed_part + rand_part)

  ## C-index（方向修正）
  cindex <- as.numeric(survival::concordance(Surv(surv_data$obs_time, surv_data$event) ~ risk)$concordance)
  if (!is.na(cindex) && cindex < 0.5) {
    risk <- -risk
    cindex <- as.numeric(survival::concordance(Surv(surv_data$obs_time, surv_data$event) ~ risk)$concordance)
  }

  ## AUC
  roc1 <- timeROC(T = surv_data$obs_time, delta = surv_data$event, marker = risk, cause = 1, times = t0)
  roc2 <- timeROC(T = surv_data$obs_time, delta = surv_data$event, marker = -risk, cause = 1, times = t0)
  auc1 <- if (length(roc1$AUC) >= 2) roc1$AUC[2] else roc1$AUC[1]
  auc2 <- if (length(roc2$AUC) >= 2) roc2$AUC[2] else roc2$AUC[1]
  auc <- max(as.numeric(auc1), as.numeric(auc2), na.rm = TRUE)

  ## BS（与RSFLC段一致的简化计算）
  haz <- exp(as.numeric(scale(risk)))
  sp <- exp(-haz * t0)
  bs <- mean(ifelse(surv_data$obs_time <= t0 & surv_data$event == 1, (1 - sp)^2, sp^2), na.rm = TRUE)

  list(
    fit = fit,
    lme_fit = lme_fit,
    cox_fit = cox_fit,
    y_var = y_var,
    n_id = nrow(surv_data),
    metrics = data.frame(
      AUC = safe_metric(auc),
      BS = safe_metric(bs),
      CINDEX = safe_metric(cindex)
    )
  )
}

t0_jm <- median(jm_data0$obs_time[jm_data0$obs_time > 0], na.rm = TRUE)
jm_try_results <- list()
for (v in long_try) {
  cat("JM尝试变量: ", v, "\n", sep = "")
  one <- tryCatch(fit_one_jm(jm_data0, v, t0_jm), error = function(e) {
    cat("  失败: ", conditionMessage(e), "\n", sep = "")
    NULL
  })
  if (!is.null(one)) jm_try_results[[v]] <- one
}
if (length(jm_try_results) == 0L) stop("JM多次尝试均失败，请检查数据质量或减少变量复杂度。")

## 5) 选择最佳模型并输出
jm_metrics_all <- bind_rows(lapply(names(jm_try_results), function(v) {
  cbind(y_var = v, n_id = jm_try_results[[v]]$n_id, jm_try_results[[v]]$metrics)
}))
jm_best_idx <- which.max(jm_metrics_all$CINDEX)
jm_best <- jm_metrics_all[jm_best_idx, , drop = FALSE]
jm_model <- jm_try_results[[jm_best$y_var]]$fit

cat("\nJM建模完成（多次尝试后最优）。\n")
print(jm_best)
cat("\nJM全部尝试结果：\n")
print(jm_metrics_all)

jm_structure_summary <- list(
  n_rows_raw = nrow(jm_raw),
  n_rows_model = nrow(jm_data0),
  n_ids = length(unique(jm_data0$ID)),
  t0 = t0_jm,
  covariates = c("V1", "V2", "V3", "V4", "V5", "V6"),
  tried_longitudinal_vars = long_try,
  best_longitudinal_var = as.character(jm_best$y_var)
)

cat("\nJM输入结构摘要：\n")
cat("- 原始行数: ", jm_structure_summary$n_rows_raw, "\n", sep = "")
cat("- 建模行数: ", jm_structure_summary$n_rows_model, "\n", sep = "")
cat("- 患者数(ID): ", jm_structure_summary$n_ids, "\n", sep = "")
cat("- t0: ", round(jm_structure_summary$t0, 6), "\n", sep = "")
cat("- 最优纵向变量: ", jm_structure_summary$best_longitudinal_var, "\n", sep = "")







###########RSF建模##########################
## RSF建模（参考 cal_3_RSF）：使用 widedata_merge.xlsx，仅 Obstimes=0/1，单条记录/患者
suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(survival)
  library(timeROC)
  library(randomForestSRC)
})
set.seed(123)

## 1) 数据读取与结局合并（显式 dplyr::，避免 select 被覆盖）
rsf_dir <- "F:/文章_大论文/MIMIC数据库_代码/TREA代码/0314"
rsf_path_candidates <- c(
  file.path(rsf_dir, "widedata_merge.xlsx"),
  file.path(rsf_dir, "0314_widedata_merge.xlsx")
)
rsf_path <- rsf_path_candidates[file.exists(rsf_path_candidates)][1]
if (is.na(rsf_path)) stop("未找到 widedata_merge.xlsx 或 0314_widedata_merge.xlsx")
rsf_raw <- readxl::read_xlsx(rsf_path)

base_path <- "F:/文章_大论文/MIMIC数据库_代码/TREA代码/0311/stroke_baseline_simple.xlsx"
if (!file.exists(base_path)) stop(paste0("文件不存在: ", base_path))
base_dat <- readxl::read_xlsx(base_path) %>%
  dplyr::select(hadm_id, hospital_mortality) %>%
  dplyr::distinct(hadm_id, .keep_all = TRUE) %>%
  dplyr::rename(hospitalmortality = hospital_mortality)

if ("hadm_id" %in% names(rsf_raw)) {
  rsf_raw <- rsf_raw %>%
    dplyr::mutate(hadm_id = as.character(hadm_id)) %>%
    dplyr::left_join(base_dat %>% dplyr::mutate(hadm_id = as.character(hadm_id)), by = "hadm_id")
}

## 2) 调整结构：仅用 Obstimes=0/1，且每患者保留一条（优先 Obstimes=1）
id_col <- if ("subject_id" %in% names(rsf_raw)) "subject_id" else if ("subjectid" %in% names(rsf_raw)) "subjectid" else stop("未找到 subject_id/subjectid")
if (!("Obstimes" %in% names(rsf_raw))) stop("未找到 Obstimes 列")
if (!("itemid_los_hosp_days" %in% names(rsf_raw))) stop("未找到 itemid_los_hosp_days（生存时间）")
if (!("hospitalmortality" %in% names(rsf_raw))) stop("未找到 hospitalmortality（事件）")

to_numeric_safe <- function(x) {
  if (is.list(x)) x <- unlist(x, use.names = FALSE)
  suppressWarnings(as.numeric(x))
}
rsf_raw$Obstimes <- to_numeric_safe(rsf_raw$Obstimes)
rsf_raw$itemid_los_hosp_days <- to_numeric_safe(rsf_raw$itemid_los_hosp_days)
rsf_raw$hospitalmortality <- to_numeric_safe(rsf_raw$hospitalmortality)

rsf_data0 <- rsf_raw %>%
  dplyr::filter(Obstimes %in% c(0, 1)) %>%
  dplyr::mutate(.id = as.character(.data[[id_col]])) %>%
  dplyr::arrange(.id, dplyr::desc(Obstimes)) %>%
  dplyr::distinct(.id, .keep_all = TRUE) %>%
  dplyr::mutate(
    ID = as.numeric(as.factor(.id)),
    obs_time = itemid_los_hosp_days,
    event = ifelse(hospitalmortality > 0, 1, 0)
  ) %>%
  dplyr::filter(is.finite(obs_time), !is.na(obs_time), obs_time > 0, !is.na(event))

## 候选预测变量
exclude_vars <- c(
  ".id", "ID", id_col, "subject_id", "subjectid", "hadm_id", "stay_id",
  "charttime", "Obstimes", "obs_time", "event", "hospitalmortality",
  "itemid_los_hosp_days", "itemid_hadm_id_stroke", "itemid_hadm_id_icu"
)
pred_vars <- setdiff(names(rsf_data0), exclude_vars)
keep_pred <- pred_vars[vapply(pred_vars, function(v) {
  x <- rsf_data0[[v]]
  sum(!is.na(x)) > 0 && length(unique(x[!is.na(x)])) > 1
}, logical(1))]
if (length(keep_pred) == 0L) stop("无可用预测变量，请检查数据。")

for (v in keep_pred) {
  x <- rsf_data0[[v]]
  if (is.character(x)) {
    x <- trimws(x)
    x[x == ""] <- NA_character_
    lv <- names(sort(table(x), decreasing = TRUE))
    if (length(lv) > 15L) {
      keep <- lv[1:14]
      x <- ifelse(is.na(x), NA_character_, ifelse(x %in% keep, x, "Others"))
    }
    rsf_data0[[v]] <- as.factor(x)
  }
}

## 3) 模型构建 + 结果计算（AUC/CINDEX/BS）
cal_3_RSF_mimic <- function(data, t0, keep_pred) {
  rsf_df_raw <- data[, c(keep_pred, "obs_time", "event"), drop = FALSE]
  rsf_df_raw$obs_time <- to_numeric_safe(rsf_df_raw$obs_time)
  rsf_df_raw$event <- ifelse(to_numeric_safe(rsf_df_raw$event) > 0, 1, 0)
  rsf_df_raw <- rsf_df_raw[is.finite(rsf_df_raw$obs_time) & !is.na(rsf_df_raw$obs_time) & rsf_df_raw$obs_time > 0 & !is.na(rsf_df_raw$event), , drop = FALSE]

  ## 清洗预测变量，避免 formula 解析失败
  pred_cols_raw <- setdiff(names(rsf_df_raw), c("obs_time", "event"))
  pred_clean <- list()
  for (v in pred_cols_raw) {
    x <- rsf_df_raw[[v]]
    if (is.list(x)) next
    if (inherits(x, "POSIXt") || inherits(x, "Date")) {
      x <- as.numeric(x)
    } else if (is.logical(x)) {
      x <- as.numeric(x)
    } else if (is.character(x)) {
      x <- trimws(x)
      x[x == ""] <- NA_character_
      x <- as.factor(x)
    } else if (!(is.numeric(x) || is.integer(x) || is.factor(x))) {
      x <- suppressWarnings(as.numeric(x))
    }
    if (sum(!is.na(x)) == 0) next
    if (length(unique(x[!is.na(x)])) <= 1) next
    pred_clean[[v]] <- x
  }
  if (length(pred_clean) == 0L) stop("清洗后无可用预测变量，无法建立 RSF 模型。")

  rsf_df <- data.frame(pred_clean, obs_time = rsf_df_raw$obs_time, event = rsf_df_raw$event, check.names = FALSE)
  names(rsf_df) <- make.names(names(rsf_df), unique = TRUE)
  pred_cols <- setdiff(names(rsf_df), c("obs_time", "event"))
  if (length(pred_cols) == 0L) stop("模型预测变量为空。")

  surv_time <- rsf_df$obs_time
  surv_status <- rsf_df$event
  if (length(unique(surv_status)) < 2L) stop("event 只有一个取值，无法计算生存判别指标。")

  mtry_val <- max(1, floor(length(pred_cols) / 3))
  fit <- randomForestSRC::rfsrc(
    formula = Surv(obs_time, event) ~ .,
    data = rsf_df,
    ntree = 1000,
    mtry = mtry_val,
    nodesize = 10,
    importance = TRUE,
    proximity = FALSE,
    na.action = "na.impute",
    seed = 123
  )

  n_obs <- nrow(rsf_df)
  risk_marker <- NULL
  survival_probs <- NULL
  if (!is.null(fit$survival) && !is.null(fit$time.interest) && length(fit$time.interest) > 0) {
    t_idx <- which.min(abs(fit$time.interest - t0))
    if (is.matrix(fit$survival)) {
      survival_probs <- as.numeric(fit$survival[, t_idx])
    } else {
      survival_probs <- as.numeric(fit$survival)
    }
    if (length(survival_probs) == n_obs) {
      risk_marker <- 1 - survival_probs
    }
  }
  ## 回退：使用 predicted 作为风险分数
  if (is.null(risk_marker) && !is.null(fit$predicted) && length(fit$predicted) == n_obs) {
    risk_marker <- as.numeric(fit$predicted)
    survival_probs <- pmax(0, pmin(1, 1 - risk_marker))
  }
  if (is.null(risk_marker) || length(risk_marker) != n_obs) {
    stop("无法从 RSF 预测结果提取与样本等长的风险分数。")
  }

  roc_obj1 <- timeROC::timeROC(T = surv_time, delta = surv_status, marker = risk_marker, cause = 1, times = t0)
  roc_obj2 <- timeROC::timeROC(T = surv_time, delta = surv_status, marker = -risk_marker, cause = 1, times = t0)
  auc1 <- if (length(roc_obj1$AUC) >= 2) roc_obj1$AUC[2] else roc_obj1$AUC[1]
  auc2 <- if (length(roc_obj2$AUC) >= 2) roc_obj2$AUC[2] else roc_obj2$AUC[1]
  AUC <- max(as.numeric(auc1), as.numeric(auc2), na.rm = TRUE)

  CINDEX <- as.numeric(survival::concordance(survival::Surv(surv_time, surv_status) ~ risk_marker)$concordance)
  if (!is.na(CINDEX) && CINDEX < 0.5) {
    risk_marker <- -risk_marker
    CINDEX <- as.numeric(survival::concordance(survival::Surv(surv_time, surv_status) ~ risk_marker)$concordance)
  }

  Y_obs <- as.numeric(surv_time > t0 | (surv_time <= t0 & surv_status == 0))
  censoring_model <- survival::survfit(survival::Surv(obs_time, 1 - event) ~ 1, data = rsf_df)
  get_weights <- function(time, event, censoring_model, t0) {
    cens_probs <- summary(censoring_model, times = pmin(time, t0))$surv
    if (length(cens_probs) != length(time)) cens_probs <- rep(summary(censoring_model, times = t0)$surv, length(time))
    if (is.null(cens_probs) || length(cens_probs) == 0) cens_probs <- rep(1, length(time))
    ifelse(time <= t0 & event == 1, 1 / pmax(cens_probs, 1e-6),
           ifelse(time > t0, 1 / pmax(summary(censoring_model, times = t0)$surv, 1e-6), 0))
  }
  weights <- get_weights(surv_time, surv_status, censoring_model, t0)
  BS <- mean(weights * (survival_probs - Y_obs)^2, na.rm = TRUE)

  list(
    fit = fit,
    metrics = data.frame(AUC = round(AUC, 4), CINDEX = round(CINDEX, 4), BS = round(BS, 4))
  )
}

t0_rsf <- median(rsf_data0$obs_time, na.rm = TRUE)
rsf_res <- cal_3_RSF_mimic(rsf_data0, t0 = t0_rsf, keep_pred = keep_pred)
rsf_metrics <- rsf_res$metrics

rsf_structure <- list(
  input_file = rsf_path,
  n_rows_raw = nrow(rsf_raw),
  n_rows_model = nrow(rsf_data0),
  n_patients = length(unique(rsf_data0$ID)),
  n_predictors = length(keep_pred),
  t0 = t0_rsf
)

cat("\nRSF建模完成（Obstimes=0/1，单条记录/患者）。\n")
print(rsf_metrics)
cat("\nRSF输入结构摘要：\n")
cat("- 输入文件: ", rsf_structure$input_file, "\n", sep = "")
cat("- 原始行数: ", rsf_structure$n_rows_raw, "\n", sep = "")
cat("- 建模行数: ", rsf_structure$n_rows_model, "\n", sep = "")
cat("- 患者数: ", rsf_structure$n_patients, "\n", sep = "")
cat("- 预测变量数: ", rsf_structure$n_predictors, "\n", sep = "")
cat("- t0: ", round(rsf_structure$t0, 6), "\n", sep = "")



#############COX模型构建###############

## 使用 widedata_merge1.xlsx 构建 COX 生存模型，并输出 AUC / BS / CINDEX
suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(survival)
  library(timeROC)
})
set.seed(123)

## 1) 读取数据
cox_dir <- "F:/文章_大论文/MIMIC数据库_代码/TREA代码/0314"
cox_path <- file.path(cox_dir, "widedata_merge1.xlsx")
if (!file.exists(cox_path)) stop(paste0("文件不存在: ", cox_path))
cox_raw <- readxl::read_xlsx(cox_path)

## 若缺失事件列，尝试从 baseline 文件合并 hospital_mortality
event_col_cands <- c("hospitalmortality", "hospital_mortality", "event", "status", "mortality_hosp")
event_col <- event_col_cands[event_col_cands %in% names(cox_raw)][1]
if (is.na(event_col) && "hadm_id" %in% names(cox_raw)) {
  base_path <- "F:/文章_大论文/MIMIC数据库_代码/TREA代码/0311/stroke_baseline_simple.xlsx"
  if (file.exists(base_path)) {
    base_dat <- readxl::read_xlsx(base_path) %>%
      dplyr::select(hadm_id, hospital_mortality) %>%
      dplyr::distinct(hadm_id, .keep_all = TRUE) %>%
      dplyr::rename(hospitalmortality = hospital_mortality)
    cox_raw <- cox_raw %>%
      dplyr::mutate(hadm_id = as.character(hadm_id)) %>%
      dplyr::left_join(base_dat %>% dplyr::mutate(hadm_id = as.character(hadm_id)), by = "hadm_id")
    event_col <- "hospitalmortality"
  }
}
if (is.na(event_col)) stop("未找到事件列（hospitalmortality/event/status 等）。")

## 2) 结构调整：使用 Obstimes=0/1 且每患者单条（优先 Obstimes=1）
id_col <- if ("subject_id" %in% names(cox_raw)) "subject_id" else if ("subjectid" %in% names(cox_raw)) "subjectid" else stop("未找到 subject_id/subjectid")
time_col <- c("itemid_los_hosp_days", "obs_time", "surv_time")[c("itemid_los_hosp_days", "obs_time", "surv_time") %in% names(cox_raw)][1]
if (is.na(time_col)) stop("未找到生存时间列（itemid_los_hosp_days/obs_time/surv_time）。")
if (!("Obstimes" %in% names(cox_raw))) stop("未找到 Obstimes 列。")

to_num_safe <- function(x) {
  if (is.list(x)) x <- unlist(x, use.names = FALSE)
  suppressWarnings(as.numeric(x))
}
cox_raw$Obstimes <- to_num_safe(cox_raw$Obstimes)
cox_raw[[time_col]] <- to_num_safe(cox_raw[[time_col]])
cox_raw[[event_col]] <- to_num_safe(cox_raw[[event_col]])

cox_data0 <- cox_raw %>%
  dplyr::filter(Obstimes %in% c(0, 1)) %>%
  dplyr::mutate(.id = as.character(.data[[id_col]])) %>%
  dplyr::arrange(.id, dplyr::desc(Obstimes)) %>%
  dplyr::distinct(.id, .keep_all = TRUE) %>%
  dplyr::mutate(
    ID = as.numeric(as.factor(.id)),
    obs_time = .data[[time_col]],
    event = ifelse(.data[[event_col]] > 0, 1, 0)
  ) %>%
  dplyr::filter(is.finite(obs_time), !is.na(obs_time), obs_time > 0, !is.na(event))

## 候选预测变量：仅使用 itemid_ 变量，避免非目标自变量泄露
pred_vars <- grep("^itemid_", names(cox_data0), value = TRUE)
drop_itemid <- c(
  "itemid_los_hosp_days", "itemid_hadm_id_stroke", "itemid_hadm_id_icu",
  "itemid_admittime", "itemid_dischtime", "itemid_edregtime", "itemid_edouttime",
  "itemid_icu_intime", "itemid_icu_outtime", "itemid_hadm_id"
)
pred_vars <- setdiff(pred_vars, drop_itemid)
pred_vars <- pred_vars[vapply(pred_vars, function(v) {
  x <- cox_data0[[v]]
  sum(!is.na(x)) > 0 && length(unique(x[!is.na(x)])) > 1
}, logical(1))]
if (length(pred_vars) == 0L) stop("itemid_ 变量中无可用预测变量。")

## 清洗变量类型
pred_clean <- list()
for (v in pred_vars) {
  x <- cox_data0[[v]]
  if (is.list(x)) next
  if (inherits(x, "POSIXt") || inherits(x, "Date")) {
    x <- as.numeric(x)
  } else if (is.logical(x)) {
    x <- as.numeric(x)
  } else if (is.character(x)) {
    x <- trimws(x)
    x[x == ""] <- NA_character_
    lv <- names(sort(table(x), decreasing = TRUE))
    if (length(lv) > 10L) {
      keep <- lv[1:9]
      x <- ifelse(is.na(x), NA_character_, ifelse(x %in% keep, x, "Others"))
    }
    x <- as.factor(x)
  } else if (!(is.numeric(x) || is.integer(x) || is.factor(x))) {
    x <- to_num_safe(x)
  }
  if (sum(!is.na(x)) == 0) next
  if (length(unique(x[!is.na(x)])) <= 1) next
  pred_clean[[v]] <- x
}
if (length(pred_clean) == 0L) stop("清洗后无可用 itemid_ 预测变量。")

cox_df <- data.frame(pred_clean, obs_time = cox_data0$obs_time, event = cox_data0$event, check.names = FALSE)
names(cox_df) <- make.names(names(cox_df), unique = TRUE)
cox_pred_cols <- setdiff(names(cox_df), c("obs_time", "event"))
if (length(cox_pred_cols) == 0L) stop("COX 预测变量为空。")

## 训练/测试划分，避免训练内评估导致 AUC/CINDEX 异常接近1
n_all <- nrow(cox_df)
if (n_all < 50) stop("样本量过小，不适合进行稳定的训练/测试评估。")
idx_event <- which(cox_df$event == 1)
idx_nonev <- which(cox_df$event == 0)
train_event_n <- max(1, floor(length(idx_event) * 0.7))
train_nonev_n <- max(1, floor(length(idx_nonev) * 0.7))
train_idx <- c(sample(idx_event, train_event_n), sample(idx_nonev, train_nonev_n))
test_idx <- setdiff(seq_len(n_all), train_idx)
if (length(test_idx) < 20) stop("测试集样本过少，请调整划分比例。")
train_df <- cox_df[train_idx, , drop = FALSE]
test_df <- cox_df[test_idx, , drop = FALSE]

## 用训练集参数填补缺失
for (v in cox_pred_cols) {
  if (is.factor(train_df[[v]])) {
    lv <- levels(train_df[[v]])
    train_chr <- as.character(train_df[[v]])
    mode_val <- if (all(is.na(train_chr))) "Others" else names(sort(table(train_chr), decreasing = TRUE))[1]
    train_chr[is.na(train_chr)] <- mode_val
    test_chr <- as.character(test_df[[v]])
    test_chr[is.na(test_chr)] <- mode_val
    test_chr[!(test_chr %in% c(lv, mode_val))] <- mode_val
    all_lv <- unique(c(lv, mode_val))
    train_df[[v]] <- factor(train_chr, levels = all_lv)
    test_df[[v]] <- factor(test_chr, levels = all_lv)
  } else {
    med <- stats::median(train_df[[v]], na.rm = TRUE)
    if (!is.finite(med)) med <- 0
    train_df[[v]][is.na(train_df[[v]])] <- med
    test_df[[v]][is.na(test_df[[v]])] <- med
  }
}

## 控制变量数：按训练集单变量 Cox p 值选前若干个（避免不收敛）
event_n_train <- sum(train_df$event == 1)
max_vars <- max(3, min(12, floor(event_n_train / 5)))
uni_p <- sapply(cox_pred_cols, function(v) {
  fm <- as.formula(paste("survival::Surv(obs_time, event) ~", v))
  p <- tryCatch({
    fit_u <- survival::coxph(fm, data = train_df)
    sm <- summary(fit_u)
    if (nrow(sm$coefficients) >= 1) as.numeric(sm$coefficients[1, "Pr(>|z|)"]) else 1
  }, error = function(e) 1)
  ifelse(is.finite(p), p, 1)
})
sel_vars <- names(sort(uni_p))[seq_len(min(max_vars, length(uni_p)))]

## 3) 模型构建（训练集）
cox_formula <- as.formula(paste("survival::Surv(obs_time, event) ~", paste(sel_vars, collapse = " + ")))
cox_fit <- survival::coxph(
  cox_formula,
  data = train_df,
  x = TRUE,
  model = TRUE,
  ties = "efron",
  control = survival::coxph.control(iter.max = 100)
)

## 4) 结果计算（测试集 AUC / CINDEX / BS）
lp_test <- as.numeric(stats::predict(cox_fit, newdata = test_df, type = "lp"))
surv_time <- test_df$obs_time
surv_status <- test_df$event
cindex <- as.numeric(survival::concordance(survival::Surv(surv_time, surv_status) ~ lp_test)$concordance)
if (!is.na(cindex) && cindex < 0.5) {
  lp_test <- -lp_test
  cindex <- as.numeric(survival::concordance(survival::Surv(surv_time, surv_status) ~ lp_test)$concordance)
}

t0_cox <- median(train_df$obs_time, na.rm = TRUE)
auc <- NA_real_
if (length(unique(surv_status)) >= 2) {
  roc1 <- tryCatch(timeROC::timeROC(T = surv_time, delta = surv_status, marker = lp_test, cause = 1, times = t0_cox), error = function(e) NULL)
  roc2 <- tryCatch(timeROC::timeROC(T = surv_time, delta = surv_status, marker = -lp_test, cause = 1, times = t0_cox), error = function(e) NULL)
  if (!is.null(roc1) && !is.null(roc2)) {
    auc1 <- if (length(roc1$AUC) >= 2) roc1$AUC[2] else roc1$AUC[1]
    auc2 <- if (length(roc2$AUC) >= 2) roc2$AUC[2] else roc2$AUC[1]
    auc <- max(as.numeric(auc1), as.numeric(auc2), na.rm = TRUE)
  }
}

## BS 用测试集概率损失（0-1范围）
bh <- survival::basehaz(cox_fit, centered = FALSE)
H0_t0 <- approx(x = bh$time, y = bh$hazard, xout = t0_cox, method = "linear", rule = 2)$y
survival_probs <- exp(-H0_t0 * exp(lp_test))
survival_probs <- pmin(pmax(survival_probs, 0), 1)
Y_obs <- as.numeric(surv_time > t0_cox | (surv_time <= t0_cox & surv_status == 0))
bs <- mean((survival_probs - Y_obs)^2, na.rm = TRUE)

cox_metrics <- data.frame(
  AUC = round(auc, 4),
  CINDEX = round(cindex, 4),
  BS = round(bs, 4)
)
cox_structure <- list(
  input_file = cox_path,
  n_rows_model = nrow(cox_df),
  n_patients = nrow(cox_data0),
  n_predictors = length(sel_vars),
  n_train = nrow(train_df),
  n_test = nrow(test_df),
  t0 = t0_cox
)

cat("\nCOX建模完成。\n")
print(cox_metrics)
cat("\nCOX输入结构摘要：\n")
cat("- 输入文件: ", cox_structure$input_file, "\n", sep = "")
cat("- 建模样本数: ", cox_structure$n_rows_model, "\n", sep = "")
cat("- 患者数: ", cox_structure$n_patients, "\n", sep = "")
cat("- 训练集/测试集: ", cox_structure$n_train, "/", cox_structure$n_test, "\n", sep = "")
cat("- 入模预测变量数(itemid_): ", cox_structure$n_predictors, "\n", sep = "")
cat("- t0: ", round(cox_structure$t0, 6), "\n", sep = "")










