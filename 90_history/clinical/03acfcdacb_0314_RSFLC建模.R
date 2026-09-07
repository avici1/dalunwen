
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



######################10V尝试建模#####################

## 为保证本区块可独立运行：在区块开头重新引入数据并构建 df_raw/timeData/fixedData/timeVarModel
dir_10v <- "f:/文章_大论文/MIMIC数据库_代码/TREA代码/0314"
path_main_10v <- file.path(dir_10v, "widedata_merge1.xlsx")
path_base_10v <- "F:/文章_大论文/MIMIC数据库_代码/TREA代码/0311/stroke_baseline_simple.xlsx"
if (!file.exists(path_main_10v)) stop(paste0("文件不存在: ", path_main_10v))
if (!file.exists(path_base_10v)) stop(paste0("文件不存在: ", path_base_10v))

df_main_10v <- read_xlsx(path_main_10v)
df_base_10v <- read_xlsx(path_base_10v)
df_base_sub_10v <- df_base_10v %>%
  dplyr::select(hadm_id, hospital_mortality) %>%
  dplyr::distinct(hadm_id, .keep_all = TRUE) %>%
  dplyr::rename(hospitalmortality = hospital_mortality)

df_raw <- df_main_10v %>%
  mutate(hadm_id = as.character(hadm_id)) %>%
  left_join(df_base_sub_10v %>% mutate(hadm_id = as.character(hadm_id)), by = "hadm_id")

df_raw$id <- as.numeric(as.factor(as.character(df_raw$subject_id)))
df_raw$time <- suppressWarnings(as.numeric(df_raw$Obstimes))
if (all(is.na(df_raw$time))) stop("Obstimes 无法转换为数值。")
df_raw <- df_raw %>%
  group_by(id) %>%
  mutate(time = time - min(time, na.rm = TRUE)) %>%
  ungroup()

drop_cat_10v <- c(
  "itemid_admission_type", "itemid_admission_location", "itemid_discharge_location",
  "itemid_insurance", "itemid_language", "itemid_marital_status",
  "itemid_race", "itemid_first_careunit"
)
long_vars_10v <- names(df_raw)[grepl("^itemid_", names(df_raw))]
long_vars_10v <- setdiff(long_vars_10v, c(drop_cat_10v, "itemid_los_hosp_days"))
long_vars_10v <- long_vars_10v[vapply(long_vars_10v, function(v) {
  x <- suppressWarnings(as.numeric(df_raw[[v]]))
  sum(!is.na(x)) > 0
}, logical(1))]
for (v in long_vars_10v) df_raw[[v]] <- suppressWarnings(as.numeric(df_raw[[v]]))
timeData <- df_raw %>% dplyr::select(id, time, dplyr::all_of(long_vars_10v))

first_not_na_10v <- function(x) {
  x2 <- x[!is.na(x) & !(is.character(x) & trimws(x) == "")]
  if (length(x2) == 0L) NA else x2[1]
}
fixed_surv_10v <- df_raw %>%
  group_by(id) %>%
  summarise(
    time = suppressWarnings(as.numeric(first(itemid_los_hosp_days))),
    event = suppressWarnings(as.numeric(first(hospitalmortality))),
    .groups = "drop"
  ) %>%
  filter(is.finite(time), !is.na(time), time > 0, !is.na(event))
fixed_surv_10v$event <- ifelse(fixed_surv_10v$event > 0, 1, 0)

exclude_10v <- unique(c("id", "time", "Obstimes", "hospitalmortality", "itemid_los_hosp_days", long_vars_10v))
fixed_vars_10v <- setdiff(names(df_raw), exclude_10v)
fixed_base_10v <- df_raw %>% group_by(id) %>% summarise(across(all_of(fixed_vars_10v), first_not_na_10v), .groups = "drop")
fixedData <- fixed_surv_10v %>% left_join(fixed_base_10v, by = "id")
fixedData <- fixedData %>% distinct(id, .keep_all = TRUE)
timeData <- timeData %>% filter(id %in% fixedData$id)

timeVarModel <- lapply(long_vars_10v, function(v) list(model = "linear", fixed = ~ 1, random = ~ 1 + time | id))
names(timeVarModel) <- long_vars_10v

## 目标：
## - 将 itemid_ 变量按每10个一组放入 fixedData 尝试建模
## - 每组拟合一个 DynForest 模型并计算 CINDEX / BS / AUC
## - 输出结果到 result_RSFLC_10V.xlsx

if (!requireNamespace("writexl", quietly = TRUE)) {
  stop("请先安装 writexl 包：install.packages('writexl')")
}

to_num_local <- function(x) {
  if (is.logical(x)) return(as.numeric(x))
  if (is.numeric(x)) return(as.numeric(x))
  xc <- trimws(as.character(x))
  xc[xc == ""] <- NA_character_
  xn <- suppressWarnings(as.numeric(xc))
  ok <- sum(!is.na(xn))
  if (ok >= max(10, floor(0.5 * sum(!is.na(xc))))) return(xn)
  as.numeric(as.factor(xc))
}
safe_metric_local <- function(x, digits = 4) ifelse(is.finite(x), round(x, digits), NA_real_)

## 按id聚合出 itemid_ 基线值（每id首个非缺失）
first_not_na2 <- function(x) {
  x2 <- x[!is.na(x) & !(is.character(x) & trimws(x) == "")]
  if (length(x2) == 0L) NA else x2[1]
}

itemid_all <- names(df_raw)[grepl("^itemid_", names(df_raw))]
exclude_itemid_fixed <- c(
  "itemid_los_hosp_days",
  "itemid_admittime", "itemid_dischtime", "itemid_edregtime", "itemid_edouttime",
  "itemid_icu_intime", "itemid_icu_outtime",
  "itemid_admission_type", "itemid_admission_location", "itemid_discharge_location",
  "itemid_insurance", "itemid_language", "itemid_marital_status", "itemid_race",
  "itemid_first_careunit", "itemid_anchor_year", "itemid_anchor_year_group"
)
itemid_fixed_cands <- setdiff(itemid_all, exclude_itemid_fixed)
itemid_fixed_cands <- itemid_fixed_cands[vapply(itemid_fixed_cands, function(v) {
  x <- suppressWarnings(as.numeric(df_raw[[v]]))
  sum(!is.na(x)) > 0
}, logical(1))]

if (length(itemid_fixed_cands) == 0L) stop("未找到可用于10V尝试的 itemid_ 固定变量。")

## 与主流程一致：生存数据与纵向数据
y_surv_10v <- fixedData %>% dplyr::select(id, time, event)
timeData_10v <- timeData %>% filter(id %in% y_surv_10v$id)

## 固定变量每10个一组
batch_size_10v <- 10L
batches_10v <- split(itemid_fixed_cands, ceiling(seq_along(itemid_fixed_cands) / batch_size_10v))
t0_10v <- median(y_surv_10v$time, na.rm = TRUE)

res_10v <- vector("list", length(batches_10v))

for (i in seq_along(batches_10v)) {
  vars_i <- batches_10v[[i]]

  fixed_i <- df_raw %>%
    group_by(id) %>%
    summarise(across(all_of(vars_i), first_not_na2), .groups = "drop")
  fixed_i <- y_surv_10v %>%
    left_join(fixed_i, by = "id") %>%
    distinct(id, .keep_all = TRUE)

  ## 转数值并填补缺失
  for (v in vars_i) {
    xv <- to_num_local(fixed_i[[v]])
    med <- suppressWarnings(median(xv, na.rm = TRUE))
    if (!is.finite(med)) med <- 0
    xv[is.na(xv)] <- med
    xv <- as.numeric(scale(xv))
    xv[!is.finite(xv)] <- 0
    fixed_i[[v]] <- xv
  }

  ## 确保timeData和fixedData id一致
  ids_i <- intersect(unique(timeData_10v$id), unique(fixed_i$id))
  td_i <- timeData_10v %>% filter(id %in% ids_i)
  fd_i <- fixed_i %>% filter(id %in% ids_i)
  y_i <- y_surv_10v %>% filter(id %in% ids_i)

  one <- tryCatch({
    dyn_i <- DynForest::dynforest(
      timeData = as.data.frame(td_i),
      fixedData = as.data.frame(fd_i),
      idVar = "id",
      timeVar = "time",
      timeVarModel = timeVarModel,
      Y = list(type = "surv", Y = as.data.frame(y_i)),
      ntree = 500,
      mtry = max(1, floor(length(vars_i) / 3)),
      nodesize = 10,
      minsplit = 2,
      nsplit_option = "quantile",
      ncores = 1,
      verbose = FALSE
    )

    pred_i <- predict(dyn_i, timeData = as.data.frame(td_i), fixedData = as.data.frame(fd_i), idVar = "id", timeVar = "time", t0 = t0_10v)
    pred_mat_i <- pred_i$pred_indiv
    risk_i <- as.numeric(pred_mat_i[, ncol(pred_mat_i)])
    eval_ids_i <- as.numeric(rownames(pred_mat_i))
    eval_i <- y_i %>% filter(id %in% eval_ids_i)
    eval_i <- eval_i[match(eval_ids_i, eval_i$id), , drop = FALSE]
    st <- as.numeric(eval_i$time)
    se <- as.numeric(eval_i$event)

    cidx <- as.numeric(survival::concordance(survival::Surv(st, se) ~ risk_i)$concordance)
    if (!is.na(cidx) && cidx < 0.5) {
      risk_i <- -risk_i
      cidx <- as.numeric(survival::concordance(survival::Surv(st, se) ~ risk_i)$concordance)
    }

    roc1_i <- timeROC::timeROC(T = st, delta = se, marker = risk_i, cause = 1, times = t0_10v)
    roc2_i <- timeROC::timeROC(T = st, delta = se, marker = -risk_i, cause = 1, times = t0_10v)
    auc1_i <- if (length(roc1_i$AUC) >= 2) roc1_i$AUC[2] else roc1_i$AUC[1]
    auc2_i <- if (length(roc2_i$AUC) >= 2) roc2_i$AUC[2] else roc2_i$AUC[1]
    auc_i <- max(as.numeric(auc1_i), as.numeric(auc2_i), na.rm = TRUE)

    haz_i <- exp(as.numeric(scale(risk_i)))
    sp_i <- exp(-haz_i * t0_10v)
    bs_i <- mean(ifelse(st <= t0_10v & se == 1, (1 - sp_i)^2, sp_i^2), na.rm = TRUE)

    data.frame(
      itemids = paste(vars_i, collapse = ";"),
      cindex = safe_metric_local(cidx),
      bs = safe_metric_local(bs_i),
      auc = safe_metric_local(auc_i),
      stringsAsFactors = FALSE
    )
  }, error = function(e) {
    data.frame(
      itemids = paste(vars_i, collapse = ";"),
      cindex = NA_real_,
      bs = NA_real_,
      auc = NA_real_,
      stringsAsFactors = FALSE
    )
  })

  res_10v[[i]] <- one
  cat("RSFLC 10V进度: ", i, "/", length(batches_10v), " 完成\n", sep = "")
}

result_10v_df <- dplyr::bind_rows(res_10v)
out_10v_path <- "f:/文章_大论文/MIMIC数据库_代码/TREA代码/0315/result_RSFLC_10V.xlsx"
writexl::write_xlsx(result_10v_df, out_10v_path)

cat("\nRSFLC 10V批量建模结束。\n")
cat("结果文件: ", out_10v_path, "\n", sep = "")
print(utils::head(result_10v_df, 10))





###################4V尝试建模#####################

## 目标：
## - 固定协变量每次仅4个：年龄 + 性别 + 另外2个itemid
## - 遍历其余itemid两两组合
## - 输出4列：itemids / cindex / bs / auc
## - 保存到 result_RSFLC_4V.xlsx

if (!exists("df_raw") || !exists("timeData") || !exists("timeVarModel")) {
  stop("请先运行上方10V区块（会构建 df_raw/timeData/timeVarModel）。")
}

pick_first_existing <- function(cands, pool) {
  x <- cands[cands %in% pool]
  if (length(x) == 0L) return(NA_character_)
  x[1]
}

age_var4 <- pick_first_existing(c("itemid_anchor_age", "V1"), names(df_raw))
sex_var4 <- pick_first_existing(c("itemid_gender", "V2"), names(df_raw))
if (is.na(age_var4) || is.na(sex_var4)) {
  stop("未找到年龄或性别变量（支持 itemid_anchor_age/itemid_gender 或 V1/V2）。")
}

## 与10V一致的生存数据
y_surv_4v <- df_raw %>%
  dplyr::group_by(id) %>%
  dplyr::summarise(
    time = suppressWarnings(as.numeric(dplyr::first(itemid_los_hosp_days))),
    event = suppressWarnings(as.numeric(dplyr::first(hospitalmortality))),
    .groups = "drop"
  ) %>%
  dplyr::filter(is.finite(time), !is.na(time), time > 0, !is.na(event))
y_surv_4v$event <- ifelse(y_surv_4v$event > 0, 1, 0)

## 固定候选：保留可数值化且有信息的 itemid，排除时间/分类字段和age/gender
itemid_all_4v <- names(df_raw)[grepl("^itemid_", names(df_raw))]
exclude_itemid_4v <- c(
  age_var4, sex_var4, "itemid_los_hosp_days",
  "itemid_admittime", "itemid_dischtime", "itemid_edregtime", "itemid_edouttime",
  "itemid_icu_intime", "itemid_icu_outtime",
  "itemid_admission_type", "itemid_admission_location", "itemid_discharge_location",
  "itemid_insurance", "itemid_language", "itemid_marital_status", "itemid_race",
  "itemid_first_careunit", "itemid_anchor_year", "itemid_anchor_year_group"
)
extra_cands_4v <- setdiff(itemid_all_4v, exclude_itemid_4v)
extra_cands_4v <- extra_cands_4v[vapply(extra_cands_4v, function(v) {
  x <- suppressWarnings(as.numeric(df_raw[[v]]))
  sum(!is.na(x)) > 0 && length(unique(x[!is.na(x)])) > 1
}, logical(1))]

if (length(extra_cands_4v) < 2) {
  stop("4V建模可用的额外 itemid 候选不足2个。")
}

pair_mat_4v <- utils::combn(extra_cands_4v, 2)
t0_4v <- median(y_surv_4v$time, na.rm = TRUE)

## 对过多组合做上限，避免运行过久
max_pairs_4v <- 300L
if (ncol(pair_mat_4v) > max_pairs_4v) {
  pair_mat_4v <- pair_mat_4v[, seq_len(max_pairs_4v), drop = FALSE]
  cat("提示：4V组合过多，仅尝试前 ", max_pairs_4v, " 组。\n", sep = "")
}

res_4v <- vector("list", ncol(pair_mat_4v))
for (i in seq_len(ncol(pair_mat_4v))) {
  extra2 <- pair_mat_4v[, i]
  vars_i <- c(age_var4, sex_var4, extra2)

  fixed_i <- df_raw %>%
    dplyr::group_by(id) %>%
    dplyr::summarise(across(dplyr::all_of(vars_i), first_not_na2), .groups = "drop")
  fixed_i <- y_surv_4v %>%
    dplyr::left_join(fixed_i, by = "id") %>%
    dplyr::distinct(id, .keep_all = TRUE)

  for (v in vars_i) {
    xv <- to_num_local(fixed_i[[v]])
    med <- suppressWarnings(median(xv, na.rm = TRUE))
    if (!is.finite(med)) med <- 0
    xv[is.na(xv)] <- med
    xv <- as.numeric(scale(xv))
    xv[!is.finite(xv)] <- 0
    fixed_i[[v]] <- xv
  }

  ids_i <- intersect(unique(timeData$id), unique(fixed_i$id))
  td_i <- timeData %>% dplyr::filter(id %in% ids_i)
  fd_i <- fixed_i %>% dplyr::filter(id %in% ids_i)
  y_i <- y_surv_4v %>% dplyr::filter(id %in% ids_i)

  one <- tryCatch({
    dyn_i <- DynForest::dynforest(
      timeData = as.data.frame(td_i),
      fixedData = as.data.frame(fd_i),
      idVar = "id",
      timeVar = "time",
      timeVarModel = timeVarModel,
      Y = list(type = "surv", Y = as.data.frame(y_i)),
      ntree = 500,
      mtry = max(1, floor(length(vars_i) / 3)),
      nodesize = 10,
      minsplit = 2,
      nsplit_option = "quantile",
      ncores = 1,
      verbose = FALSE
    )

    pred_i <- predict(dyn_i, timeData = as.data.frame(td_i), fixedData = as.data.frame(fd_i), idVar = "id", timeVar = "time", t0 = t0_4v)
    pred_mat_i <- pred_i$pred_indiv
    risk_i <- as.numeric(pred_mat_i[, ncol(pred_mat_i)])
    eval_ids_i <- as.numeric(rownames(pred_mat_i))
    eval_i <- y_i %>% dplyr::filter(id %in% eval_ids_i)
    eval_i <- eval_i[match(eval_ids_i, eval_i$id), , drop = FALSE]
    st <- as.numeric(eval_i$time)
    se <- as.numeric(eval_i$event)

    cidx <- as.numeric(survival::concordance(survival::Surv(st, se) ~ risk_i)$concordance)
    if (!is.na(cidx) && cidx < 0.5) {
      risk_i <- -risk_i
      cidx <- as.numeric(survival::concordance(survival::Surv(st, se) ~ risk_i)$concordance)
    }

    roc1_i <- timeROC::timeROC(T = st, delta = se, marker = risk_i, cause = 1, times = t0_4v)
    roc2_i <- timeROC::timeROC(T = st, delta = se, marker = -risk_i, cause = 1, times = t0_4v)
    auc1_i <- if (length(roc1_i$AUC) >= 2) roc1_i$AUC[2] else roc1_i$AUC[1]
    auc2_i <- if (length(roc2_i$AUC) >= 2) roc2_i$AUC[2] else roc2_i$AUC[1]
    auc_i <- max(as.numeric(auc1_i), as.numeric(auc2_i), na.rm = TRUE)

    haz_i <- exp(as.numeric(scale(risk_i)))
    sp_i <- exp(-haz_i * t0_4v)
    bs_i <- mean(ifelse(st <= t0_4v & se == 1, (1 - sp_i)^2, sp_i^2), na.rm = TRUE)

    data.frame(
      itemids = paste(vars_i, collapse = ";"),
      cindex = safe_metric_local(cidx),
      bs = safe_metric_local(bs_i),
      auc = safe_metric_local(auc_i),
      stringsAsFactors = FALSE
    )
  }, error = function(e) {
    data.frame(
      itemids = paste(vars_i, collapse = ";"),
      cindex = NA_real_,
      bs = NA_real_,
      auc = NA_real_,
      stringsAsFactors = FALSE
    )
  })

  res_4v[[i]] <- one
  cat("RSFLC 4V进度: ", i, "/", ncol(pair_mat_4v), " 完成\n", sep = "")
}

result_4v_df <- dplyr::bind_rows(res_4v)
out_4v_path <- "f:/文章_大论文/MIMIC数据库_代码/TREA代码/0315/result_RSFLC_4V.xlsx"
writexl::write_xlsx(result_4v_df, out_4v_path)

cat("\nRSFLC 4V批量建模结束。\n")
cat("年龄变量: ", age_var4, "；性别变量: ", sex_var4, "\n", sep = "")
cat("结果文件: ", out_4v_path, "\n", sep = "")
print(utils::head(result_4v_df, 10))


################5折交叉验证##################

## 目标：
## - 从 10V/4V 结果中自动选择 CINDEX 最优模型
## - 在现有数据上进行 5 折交叉验证（分层event）
## - 输出每折与均值的 CINDEX / BS / AUC

if (!requireNamespace("writexl", quietly = TRUE)) {
  stop("请先安装 writexl 包：install.packages('writexl')")
}
if (!exists("df_raw") || !exists("timeData") || !exists("timeVarModel")) {
  stop("未找到 df_raw/timeData/timeVarModel。请先运行10V或4V区块。")
}
if (!exists("result_10v_df") && !exists("result_4v_df")) {
  stop("未找到 result_10v_df/result_4v_df，请先运行10V或4V区块。")
}

pick_best_r <- function(df, tag) {
  if (is.null(df) || nrow(df) == 0) return(NULL)
  req <- c("itemids", "cindex", "bs", "auc")
  if (!all(req %in% names(df))) return(NULL)
  d <- df %>% dplyr::filter(is.finite(.data$cindex))
  if (nrow(d) == 0) return(NULL)
  d <- d %>% dplyr::arrange(dplyr::desc(.data$cindex), .data$bs, dplyr::desc(.data$auc))
  data.frame(
    model_tag = tag,
    itemids = as.character(d$itemids[1]),
    cindex = as.numeric(d$cindex[1]),
    bs = as.numeric(d$bs[1]),
    auc = as.numeric(d$auc[1]),
    stringsAsFactors = FALSE
  )
}

best_pool <- dplyr::bind_rows(
  if (exists("result_10v_df")) pick_best_r(result_10v_df, "10V") else NULL,
  if (exists("result_4v_df")) pick_best_r(result_4v_df, "4V") else NULL
)
if (nrow(best_pool) == 0) stop("没有可用于5折交叉验证的成功模型。")

best_row <- best_pool %>%
  dplyr::arrange(dplyr::desc(.data$cindex), .data$bs, dplyr::desc(.data$auc)) %>%
  dplyr::slice(1)
best_vars <- unlist(strsplit(as.character(best_row$itemids[1]), ";", fixed = TRUE))
best_vars <- best_vars[best_vars %in% names(df_raw)]
if (length(best_vars) == 0L) stop("最优模型变量在 df_raw 中不存在。")

## 构造生存结局（每id一行）
y_surv_cv <- df_raw %>%
  dplyr::group_by(id) %>%
  dplyr::summarise(
    time = suppressWarnings(as.numeric(dplyr::first(itemid_los_hosp_days))),
    event = suppressWarnings(as.numeric(dplyr::first(hospitalmortality))),
    .groups = "drop"
  ) %>%
  dplyr::filter(is.finite(.data$time), !is.na(.data$time), .data$time > 0, !is.na(.data$event))
y_surv_cv$event <- ifelse(y_surv_cv$event > 0, 1, 0)

fixed_cv <- df_raw %>%
  dplyr::group_by(id) %>%
  dplyr::summarise(across(dplyr::all_of(best_vars), first_not_na2), .groups = "drop") %>%
  dplyr::left_join(y_surv_cv, by = "id") %>%
  dplyr::distinct(id, .keep_all = TRUE)
for (v in best_vars) {
  xv <- to_num_local(fixed_cv[[v]])
  med <- suppressWarnings(median(xv, na.rm = TRUE))
  if (!is.finite(med)) med <- 0
  xv[is.na(xv)] <- med
  xv <- as.numeric(scale(xv))
  xv[!is.finite(xv)] <- 0
  fixed_cv[[v]] <- xv
}

ids_all <- intersect(unique(timeData$id), unique(fixed_cv$id))
time_cv <- timeData %>% dplyr::filter(.data$id %in% ids_all)
fixed_cv <- fixed_cv %>% dplyr::filter(.data$id %in% ids_all)
y_surv_cv <- y_surv_cv %>% dplyr::filter(.data$id %in% ids_all)

## 分层5折
set.seed(2026)
id_e1 <- y_surv_cv$id[y_surv_cv$event == 1]
id_e0 <- y_surv_cv$id[y_surv_cv$event == 0]
fold_df <- data.frame(id = y_surv_cv$id, fold = NA_integer_)
fold_df$fold[match(id_e1, fold_df$id)] <- sample(rep(1:5, length.out = length(id_e1)))
fold_df$fold[match(id_e0, fold_df$id)] <- sample(rep(1:5, length.out = length(id_e0)))

cv_rows <- vector("list", 5)
for (k in 1:5) {
  te_ids <- fold_df$id[fold_df$fold == k]
  tr_ids <- setdiff(fold_df$id, te_ids)
  td_tr <- time_cv %>% dplyr::filter(.data$id %in% tr_ids)
  fd_tr <- fixed_cv %>% dplyr::filter(.data$id %in% tr_ids)
  y_tr  <- y_surv_cv %>% dplyr::filter(.data$id %in% tr_ids)
  td_te <- time_cv %>% dplyr::filter(.data$id %in% te_ids)
  fd_te <- fixed_cv %>% dplyr::filter(.data$id %in% te_ids)
  y_te  <- y_surv_cv %>% dplyr::filter(.data$id %in% te_ids)
  t0_k <- median(y_tr$time, na.rm = TRUE)

  one <- tryCatch({
    fit_k <- DynForest::dynforest(
      timeData = as.data.frame(td_tr),
      fixedData = as.data.frame(fd_tr),
      idVar = "id",
      timeVar = "time",
      timeVarModel = timeVarModel,
      Y = list(type = "surv", Y = as.data.frame(y_tr)),
      ntree = 500,
      mtry = max(1, floor(length(best_vars) / 3)),
      nodesize = 10,
      minsplit = 2,
      nsplit_option = "quantile",
      ncores = 1,
      verbose = FALSE
    )
    pred_k <- predict(fit_k, timeData = as.data.frame(td_te), fixedData = as.data.frame(fd_te), idVar = "id", timeVar = "time", t0 = t0_k)
    pred_mat <- pred_k$pred_indiv
    risk <- as.numeric(pred_mat[, ncol(pred_mat)])
    eid <- as.numeric(rownames(pred_mat))
    e <- y_te %>% dplyr::filter(.data$id %in% eid)
    e <- e[match(eid, e$id), , drop = FALSE]
    st <- as.numeric(e$time); se <- as.numeric(e$event)

    cidx <- as.numeric(survival::concordance(survival::Surv(st, se) ~ risk)$concordance)
    if (!is.na(cidx) && cidx < 0.5) {
      risk <- -risk
      cidx <- as.numeric(survival::concordance(survival::Surv(st, se) ~ risk)$concordance)
    }
    roc1 <- timeROC::timeROC(T = st, delta = se, marker = risk, cause = 1, times = t0_k)
    roc2 <- timeROC::timeROC(T = st, delta = se, marker = -risk, cause = 1, times = t0_k)
    auc1 <- if (length(roc1$AUC) >= 2) roc1$AUC[2] else roc1$AUC[1]
    auc2 <- if (length(roc2$AUC) >= 2) roc2$AUC[2] else roc2$AUC[1]
    auc <- max(as.numeric(auc1), as.numeric(auc2), na.rm = TRUE)
    haz <- exp(as.numeric(scale(risk)))
    sp <- exp(-haz * t0_k)
    bs <- mean(ifelse(st <= t0_k & se == 1, (1 - sp)^2, sp^2), na.rm = TRUE)

    data.frame(
      fold = k, n_train = length(tr_ids), n_test = length(te_ids),
      cindex = safe_metric_local(cidx),
      bs = safe_metric_local(bs),
      auc = safe_metric_local(auc),
      error_msg = "",
      stringsAsFactors = FALSE
    )
  }, error = function(e) {
    data.frame(
      fold = k, n_train = length(tr_ids), n_test = length(te_ids),
      cindex = NA_real_, bs = NA_real_, auc = NA_real_,
      error_msg = conditionMessage(e),
      stringsAsFactors = FALSE
    )
  })

  cv_rows[[k]] <- one
  cat("RSFLC 5折CV进度: ", k, "/5 完成\n", sep = "")
}

cv_detail <- dplyr::bind_rows(cv_rows)
cv_mean <- cv_detail %>%
  dplyr::summarise(
    fold = "mean",
    n_train = round(mean(.data$n_train), 0),
    n_test = round(mean(.data$n_test), 0),
    cindex = round(mean(.data$cindex, na.rm = TRUE), 4),
    bs = round(mean(.data$bs, na.rm = TRUE), 4),
    auc = round(mean(.data$auc, na.rm = TRUE), 4),
    error_msg = ""
  )
meta <- data.frame(
  selected_model_source = best_row$model_tag[1],
  selected_itemids = best_row$itemids[1],
  selected_cindex = as.numeric(best_row$cindex[1]),
  selected_bs = as.numeric(best_row$bs[1]),
  selected_auc = as.numeric(best_row$auc[1]),
  stringsAsFactors = FALSE
)

out_cv_path <- "f:/文章_大论文/MIMIC数据库_代码/TREA代码/0315/result_RSFLC_best_5fold.xlsx"
writexl::write_xlsx(
  list(model_selected = meta, cv_detail = cv_detail, cv_mean = cv_mean),
  out_cv_path
)

cat("\nRSFLC 最优模型5折交叉验证完成。\n")
cat("最优模型来源: ", best_row$model_tag[1], "\n", sep = "")
cat("最优变量: ", best_row$itemids[1], "\n", sep = "")
cat("结果文件: ", out_cv_path, "\n", sep = "")
print(cv_detail)
print(cv_mean)






