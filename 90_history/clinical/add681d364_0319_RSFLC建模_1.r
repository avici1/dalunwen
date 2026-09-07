## =========================
## RSFLC (DynForest) 建模 - 部分变量组合遍历版
## 性别+年龄固定，遍历其他变量的组合
## 4V: age + gender + C(n,2) 组合 -> 0319_resultRSFLC_4v.xlsx
## 10V: age + gender + 文献指定变量（单模型）-> 0319_resultRSFLC_10v.xlsx
## =========================
suppressPackageStartupMessages({
  library(dplyr)
  library(readxl)
  library(DynForest)
  library(survival)
  library(timeROC)
  library(writexl)
})
set.seed(123)

out_dir <- "F:/文章_大论文/MIMIC数据库_代码/TREA代码/0319部分变量建模"

## ====================================================================
## 1) 数据读取与结局合并
## ====================================================================
dir_0314 <- "f:/文章_大论文/MIMIC数据库_代码/TREA代码/0314"
path_main <- file.path(dir_0314, "widedata_merge1.xlsx")
path_base <- "F:/文章_大论文/MIMIC数据库_代码/TREA代码/0311/stroke_baseline_simple.xlsx"
if (!file.exists(path_main)) stop(paste0("文件不存在: ", path_main))
if (!file.exists(path_base)) stop(paste0("文件不存在: ", path_base))

df_main <- read_xlsx(path_main)
df_base <- read_xlsx(path_base)

df_base_sub <- df_base %>%
  select(hadm_id, hospital_mortality) %>%
  distinct(hadm_id, .keep_all = TRUE) %>%
  rename(hospitalmortality = hospital_mortality)
df_raw <- df_main %>%
  mutate(hadm_id = as.character(hadm_id)) %>%
  left_join(df_base_sub %>% mutate(hadm_id = as.character(hadm_id)), by = "hadm_id")

## ====================================================================
## 2) 构造 DynForest 基础输入
## ====================================================================
df_raw$id <- as.numeric(as.factor(as.character(df_raw$subject_id)))
df_raw$time <- suppressWarnings(as.numeric(df_raw$Obstimes))
if (all(is.na(df_raw$time))) stop("Obstimes 无法转换为数值。")
df_raw <- df_raw %>%
  group_by(id) %>%
  mutate(time = time - min(time, na.rm = TRUE)) %>%
  ungroup()

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
timeData <- df_raw %>% dplyr::select(id, time, dplyr::all_of(long_vars))

timeVarModel <- lapply(long_vars, function(v) list(model = "linear", fixed = ~ 1, random = ~ 1 + time | id))
names(timeVarModel) <- long_vars

## 生存结局（每id一行）
y_surv <- df_raw %>%
  group_by(id) %>%
  summarise(
    time = suppressWarnings(as.numeric(first(itemid_los_hosp_days))),
    event = suppressWarnings(as.numeric(first(hospitalmortality))),
    .groups = "drop"
  ) %>%
  filter(is.finite(time), !is.na(time), time > 0, !is.na(event))
y_surv$event <- ifelse(y_surv$event > 0, 1, 0)

## ====================================================================
## 3) 确定固定变量和候选额外变量
## ====================================================================
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

first_not_na <- function(x) {
  x2 <- x[!is.na(x) & !(is.character(x) & trimws(x) == "")]
  if (length(x2) == 0L) NA else x2[1]
}

pick_first_existing <- function(cands, pool) {
  x <- cands[cands %in% pool]
  if (length(x) == 0L) return(NA_character_)
  x[1]
}

age_var <- pick_first_existing(c("itemid_anchor_age", "V1"), names(df_raw))
sex_var <- pick_first_existing(c("itemid_gender", "V2"), names(df_raw))
if (is.na(age_var) || is.na(sex_var)) {
  stop("未找到年龄或性别变量。")
}
cat("固定变量: ", age_var, " + ", sex_var, "\n", sep = "")

itemid_all <- names(df_raw)[grepl("^itemid_", names(df_raw))]
exclude_itemid <- c(
  age_var, sex_var, "itemid_los_hosp_days",
  "itemid_admittime", "itemid_dischtime", "itemid_edregtime", "itemid_edouttime",
  "itemid_icu_intime", "itemid_icu_outtime",
  "itemid_admission_type", "itemid_admission_location", "itemid_discharge_location",
  "itemid_insurance", "itemid_language", "itemid_marital_status", "itemid_race",
  "itemid_first_careunit", "itemid_anchor_year", "itemid_anchor_year_group",
  "itemid_hadm_id_stroke", "itemid_hadm_id_icu"
)
extra_cands <- setdiff(itemid_all, exclude_itemid)
extra_cands <- extra_cands[vapply(extra_cands, function(v) {
  x <- suppressWarnings(as.numeric(df_raw[[v]]))
  sum(!is.na(x)) > 0 && length(unique(x[!is.na(x)])) > 1
}, logical(1))]

cat("可用额外变量 (", length(extra_cands), " 个): ", paste(extra_cands, collapse = ", "), "\n\n", sep = "")

## 对齐 timeData 与 y_surv
timeData <- timeData %>% filter(id %in% y_surv$id)
t0_global <- median(y_surv$time, na.rm = TRUE)

## ====================================================================
## 4) DynForest 单模型拟合 + 评估函数
## ====================================================================
fit_and_eval_rsflc <- function(fixed_vars_i, t0_now) {
  ## 构建本轮的 fixedData
  fixed_i <- df_raw %>%
    dplyr::group_by(id) %>%
    dplyr::summarise(across(dplyr::all_of(fixed_vars_i), first_not_na), .groups = "drop")
  fixed_i <- y_surv %>%
    dplyr::left_join(fixed_i, by = "id") %>%
    dplyr::distinct(id, .keep_all = TRUE)

  for (v in fixed_vars_i) {
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
  y_i <- y_surv %>% dplyr::filter(id %in% ids_i)

  dyn_i <- DynForest::dynforest(
    timeData = as.data.frame(td_i),
    fixedData = as.data.frame(fd_i),
    idVar = "id",
    timeVar = "time",
    timeVarModel = timeVarModel,
    Y = list(type = "surv", Y = as.data.frame(y_i)),
    ntree = 500,
    mtry = max(1, floor(length(fixed_vars_i) / 3)),
    nodesize = 10,
    minsplit = 2,
    nsplit_option = "quantile",
    ncores = 1,
    verbose = FALSE
  )

  pred_i <- predict(dyn_i, timeData = as.data.frame(td_i), fixedData = as.data.frame(fd_i), idVar = "id", timeVar = "time", t0 = t0_now)
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

  roc1_i <- timeROC::timeROC(T = st, delta = se, marker = risk_i, cause = 1, times = t0_now)
  roc2_i <- timeROC::timeROC(T = st, delta = se, marker = -risk_i, cause = 1, times = t0_now)
  auc1_i <- if (length(roc1_i$AUC) >= 2) roc1_i$AUC[2] else roc1_i$AUC[1]
  auc2_i <- if (length(roc2_i$AUC) >= 2) roc2_i$AUC[2] else roc2_i$AUC[1]
  auc_i <- max(as.numeric(auc1_i), as.numeric(auc2_i), na.rm = TRUE)

  haz_i <- exp(as.numeric(scale(risk_i)))
  sp_i <- exp(-haz_i * t0_now)
  bs_i <- mean(ifelse(st <= t0_now & se == 1, (1 - sp_i)^2, sp_i^2), na.rm = TRUE)

  c(cindex = safe_metric_local(cidx), bs = safe_metric_local(bs_i), auc = safe_metric_local(auc_i))
}

## ====================================================================
## 5) 4V 建模：age + gender + C(n,2) 组合遍历
## ====================================================================
cat("========== 4V RSFLC 建模开始 ==========\n")

if (length(extra_cands) < 2L) stop("4V建模可用额外变量不足2个。")

pair_mat_4v <- utils::combn(extra_cands, 2)
n_comb_4v <- ncol(pair_mat_4v)
cat("4V总组合数: C(", length(extra_cands), ",2) = ", n_comb_4v, "\n", sep = "")

max_pairs_4v <- 2000L
if (n_comb_4v > max_pairs_4v) {
  set.seed(42)
  sel_idx <- sort(sample(n_comb_4v, max_pairs_4v))
  pair_mat_4v <- pair_mat_4v[, sel_idx, drop = FALSE]
  cat("组合过多，随机抽取 ", max_pairs_4v, " 组。\n", sep = "")
  n_comb_4v <- max_pairs_4v
}

res_4v <- vector("list", n_comb_4v)
t_start_4v <- Sys.time()

for (i in seq_len(n_comb_4v)) {
  vars_i <- c(age_var, sex_var, pair_mat_4v[, i])
  one <- tryCatch({
    met <- fit_and_eval_rsflc(fixed_vars_i = vars_i, t0_now = t0_global)
    data.frame(
      itemids   = paste(vars_i, collapse = ";"),
      cindex    = as.numeric(met["cindex"]),
      bs        = as.numeric(met["bs"]),
      auc       = as.numeric(met["auc"]),
      error_msg = "",
      stringsAsFactors = FALSE
    )
  }, error = function(e) {
    msg <- conditionMessage(e)
    if (i <= 20) cat("RSFLC 4V失败[", i, "] ", msg, "\n", sep = "")
    data.frame(
      itemids   = paste(vars_i, collapse = ";"),
      cindex    = NA_real_,
      bs        = NA_real_,
      auc       = NA_real_,
      error_msg = msg,
      stringsAsFactors = FALSE
    )
  })
  res_4v[[i]] <- one
  if (i %% 50 == 0 || i == n_comb_4v) {
    cat("RSFLC 4V进度: ", i, "/", n_comb_4v, " 完成\n", sep = "")
  }
}

result_rsflc_4v <- dplyr::bind_rows(res_4v) %>%
  dplyr::arrange(dplyr::desc(cindex), bs, dplyr::desc(auc))

out_4v <- file.path(out_dir, "0319_resultRSFLC_4v.xlsx")
writexl::write_xlsx(result_rsflc_4v, out_4v)

t_end_4v <- Sys.time()
cat("\n4V建模完成，耗时: ", round(difftime(t_end_4v, t_start_4v, units = "secs"), 1), " 秒\n", sep = "")
cat("固定变量: ", age_var, " + ", sex_var, "\n", sep = "")
cat("成功模型数: ", sum(is.finite(result_rsflc_4v$cindex)), "/", nrow(result_rsflc_4v), "\n", sep = "")
cat("结果文件: ", out_4v, "\n", sep = "")
cat("Top 5 模型:\n")
print(utils::head(result_rsflc_4v, 5))

## ====================================================================
## 6) 10V 建模：age + gender + 文献指定变量（不遍历）
## ====================================================================
cat("\n========== 10V RSFLC 建模开始 ==========\n")

## 文献支持的ICU死亡率预测变量（与JM建模一致，按文献证据强度排序）
literature_vars_10v <- c(
  "itemid_50912",  # Creatinine 肌酐
  "itemid_51006",  # BUN 尿素氮
  "itemid_50813",  # Lactate 乳酸
  "itemid_51265",  # PLT 血小板
  "itemid_51222",  # Hb 血红蛋白
  "itemid_51277",  # RDW
  "itemid_51301",  # WBC 白细胞
  "itemid_50882",  # HCO3
  "itemid_50983",  # Na
  "itemid_50971",  # K
  "itemid_51237",  # INR
  "itemid_50868",  # Anion Gap
  "itemid_50902",  # Cl
  "itemid_51274",  # PT
  "itemid_51275",  # APTT
  "itemid_51221",  # HCT
  "itemid_51279",  # RBC
  "itemid_50893"   # Total Calcium
)
extra_10v <- intersect(literature_vars_10v, extra_cands)
if (length(extra_10v) < 8L) {
  need <- 8L - length(extra_10v)
  pad <- setdiff(extra_cands, extra_10v)[seq_len(min(need, length(setdiff(extra_cands, extra_10v))))]
  extra_10v <- c(extra_10v, pad)
}
extra_10v <- extra_10v[seq_len(min(8L, length(extra_10v)))]
vars_10v <- c(age_var, sex_var, extra_10v)
cat("10V 指定变量 (", length(vars_10v), " 个): ", paste(vars_10v, collapse = "; "), "\n", sep = "")

res_10v <- vector("list", 1L)
t_start_10v <- Sys.time()

one_10v <- tryCatch({
  met <- fit_and_eval_rsflc(fixed_vars_i = vars_10v, t0_now = t0_global)
  data.frame(
    itemids   = paste(vars_10v, collapse = ";"),
    cindex    = as.numeric(met["cindex"]),
    bs        = as.numeric(met["bs"]),
    auc       = as.numeric(met["auc"]),
    error_msg = "",
    stringsAsFactors = FALSE
  )
}, error = function(e) {
  msg <- conditionMessage(e)
  cat("RSFLC 10V失败: ", msg, "\n", sep = "")
  data.frame(
    itemids   = paste(vars_10v, collapse = ";"),
    cindex    = NA_real_,
    bs        = NA_real_,
    auc       = NA_real_,
    error_msg = msg,
    stringsAsFactors = FALSE
  )
})
res_10v[[1]] <- one_10v

result_rsflc_10v <- dplyr::bind_rows(res_10v) %>%
  dplyr::arrange(dplyr::desc(cindex), bs, dplyr::desc(auc))

out_10v <- file.path(out_dir, "0319_resultRSFLC_10v.xlsx")
writexl::write_xlsx(result_rsflc_10v, out_10v)

t_end_10v <- Sys.time()
cat("\n10V建模完成，耗时: ", round(difftime(t_end_10v, t_start_10v, units = "secs"), 1), " 秒\n", sep = "")
cat("固定变量: ", age_var, " + ", sex_var, "\n", sep = "")
cat("成功模型数: ", sum(is.finite(result_rsflc_10v$cindex)), "/", nrow(result_rsflc_10v), "\n", sep = "")
cat("结果文件: ", out_10v, "\n", sep = "")
cat("Top 5 模型:\n")
print(utils::head(result_rsflc_10v, 5))

## ====================================================================
## 7) 汇总
## ====================================================================
cat("\n========== RSFLC 汇总 ==========\n")
cat("4V 结果: ", out_4v, "\n", sep = "")
cat("10V 结果: ", out_10v, "\n", sep = "")

best_4v <- result_rsflc_4v %>% dplyr::filter(is.finite(cindex)) %>% dplyr::slice(1)
best_10v <- result_rsflc_10v %>% dplyr::filter(is.finite(cindex)) %>% dplyr::slice(1)

if (nrow(best_4v) > 0) {
  cat("\n4V 最优模型: ", best_4v$itemids, "\n", sep = "")
  cat("  CINDEX=", best_4v$cindex, " AUC=", best_4v$auc, " BS=", best_4v$bs, "\n", sep = "")
}
if (nrow(best_10v) > 0) {
  cat("\n10V 最优模型: ", best_10v$itemids, "\n", sep = "")
  cat("  CINDEX=", best_10v$cindex, " AUC=", best_10v$auc, " BS=", best_10v$bs, "\n", sep = "")
}

cat("\nRSFLC 全部完成。\n")
