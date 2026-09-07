## =========================
## JM建模 - 部分变量组合遍历版
## 性别+年龄固定，遍历其他变量的组合
## 4V: age + gender + 文献变量C(n,2)，最多100组 -> 0319_resultJM_4v.xlsx
## 10V: age + gender + 文献变量C(n,8)，最多100组 -> 0319_resultJM_10v.xlsx
## =========================
suppressPackageStartupMessages({
  library(dplyr)
  library(readxl)
  library(survival)
  library(timeROC)
  library(nlme)
  library(JM)
  library(writexl)
})
set.seed(123)

out_dir <- "F:/文章_大论文/MIMIC数据库_代码/TREA代码/0319部分变量建模"

## ====================================================================
## 1) 数据读取与结局合并
## ====================================================================
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

## ====================================================================
## 2) 构造 ID / t / obs_time / event
## ====================================================================
jm_raw$ID <- as.numeric(as.factor(as.character(jm_raw$subject_id)))
jm_raw$event <- suppressWarnings(as.numeric(jm_raw$hospitalmortality))
jm_raw$event <- ifelse(is.na(jm_raw$event), NA, ifelse(jm_raw$event > 0, 1, 0))
jm_raw$t <- suppressWarnings(as.numeric(jm_raw$Obstimes)) / 24

obs_los <- suppressWarnings(as.numeric(jm_raw$itemid_los_hosp_days))
q95_los <- suppressWarnings(as.numeric(stats::quantile(obs_los, 0.95, na.rm = TRUE)))
prop_pos_los <- mean(obs_los > 0, na.rm = TRUE)
los_like_standardized <- (!is.finite(q95_los)) || (q95_los <= 2) || (!is.finite(prop_pos_los)) || (prop_pos_los < 0.8)

if (!los_like_standardized) {
  jm_raw$obs_time <- obs_los
  obs_time_source <- "itemid_los_hosp_days"
} else {
  jm_raw <- jm_raw %>%
    group_by(ID) %>%
    mutate(obs_time = max(t, na.rm = TRUE)) %>%
    ungroup()
  jm_raw$obs_time <- jm_raw$obs_time + 1e-3
  jm_raw$obs_time[!is.finite(jm_raw$obs_time)] <- NA_real_
  obs_time_source <- "max(Obstimes)/24 by ID"
}
cat("obs_time来源 = ", obs_time_source, "\n", sep = "")

jm_raw <- jm_raw %>%
  filter(!is.na(ID), !is.na(t), !is.na(obs_time), !is.na(event)) %>%
  filter(obs_time > 0) %>%
  filter(t <= obs_time)

to_num01 <- function(x) {
  if (is.logical(x)) return(as.numeric(x))
  if (is.numeric(x)) return(as.numeric(x))
  xc <- trimws(as.character(x))
  xc[xc == ""] <- NA_character_
  xn <- suppressWarnings(as.numeric(xc))
  ok <- sum(!is.na(xn))
  if (ok >= max(10, floor(0.5 * sum(!is.na(xc))))) return(xn)
  as.numeric(as.factor(xc))
}

safe_num <- function(x, digits = 4) ifelse(is.finite(x), round(x, digits), NA_real_)

## ====================================================================
## 3) 选择纵向结局 Y
## ====================================================================
drop_cat_jm <- c(
  "itemid_admission_type", "itemid_admission_location", "itemid_discharge_location",
  "itemid_insurance", "itemid_language", "itemid_marital_status", "itemid_race",
  "itemid_first_careunit", "itemid_los_hosp_days"
)
long_cands <- names(jm_raw)[grepl("^itemid_", names(jm_raw))]
long_cands <- setdiff(long_cands, drop_cat_jm)
long_cands <- setdiff(long_cands, c("itemid_anchor_age", "itemid_anchor_year", "itemid_gender", "itemid_anchor_year_group"))

score_var <- function(v) {
  y <- suppressWarnings(as.numeric(jm_raw[[v]]))
  not_na <- sum(!is.na(y))
  df_tmp <- data.frame(ID = jm_raw$ID, y = y)
  df_tmp <- df_tmp[!is.na(df_tmp$y), , drop = FALSE]
  n_id2 <- sum(table(df_tmp$ID) >= 2)
  not_na + 10 * n_id2
}
if (length(long_cands) == 0L) stop("未找到可用纵向变量（itemid_）。")
long_cands <- long_cands[order(sapply(long_cands, score_var), decreasing = TRUE)]
y_var <- long_cands[1]
cat("纵向Y变量: ", y_var, "\n", sep = "")

## 构建建模数据
data_clean <- jm_raw %>%
  mutate(Y = suppressWarnings(as.numeric(.data[[y_var]]))) %>%
  filter(!is.na(Y)) %>%
  group_by(ID) %>%
  arrange(t, .by_group = TRUE) %>%
  mutate(t = as.numeric(t), t = t + (row_number() - 1) * 1e-4) %>%
  ungroup() %>%
  filter(t < obs_time)
data_clean$Y <- as.numeric(scale(data_clean$Y))

if (nrow(data_clean) < 100 || length(unique(data_clean$ID)) < 30) {
  stop("可用于JM建模的数据不足。")
}

## ====================================================================
## 4) 确定固定变量（年龄+性别）和候选额外变量
## ====================================================================
age_var <- "itemid_anchor_age"
sex_var <- "itemid_gender"
if (!all(c(age_var, sex_var) %in% names(data_clean))) {
  stop("data_clean 中缺少年龄或性别字段。")
}
cat("固定变量: ", age_var, " + ", sex_var, "\n", sep = "")

all_itemid <- names(data_clean)[grepl("^itemid_", names(data_clean))]
exclude_itemid <- c(
  age_var, sex_var,
  "itemid_los_hosp_days", "itemid_admittime", "itemid_dischtime",
  "itemid_edregtime", "itemid_edouttime", "itemid_icu_intime", "itemid_icu_outtime",
  "itemid_admission_type", "itemid_admission_location", "itemid_discharge_location",
  "itemid_insurance", "itemid_language", "itemid_marital_status", "itemid_race",
  "itemid_first_careunit", "itemid_anchor_year", "itemid_anchor_year_group",
  "itemid_hadm_id_stroke", "itemid_hadm_id_icu"
)
extra_vars_all <- setdiff(all_itemid, exclude_itemid)
extra_vars_all <- extra_vars_all[vapply(extra_vars_all, function(v) {
  x <- suppressWarnings(as.numeric(data_clean[[v]]))
  sum(!is.na(x)) > 0 && length(unique(x[!is.na(x)])) > 1
}, logical(1))]

## 文献支持的ICU死亡率预测变量（MIMIC/SOFA相关，按文献证据强度排序）
## 参考: SOFA组分、Lactate、BUN/Cr、PLT、Hb、RDW、WBC、代谢指标等
literature_vars <- c(
  "itemid_50912",  # Creatinine 肌酐 - 肾功/SOFA
  "itemid_51006",  # BUN 尿素氮 - 肾功
  "itemid_50813",  # Lactate 乳酸 - 组织灌注/代谢
  "itemid_51265",  # PLT 血小板 - 凝血/SOFA
  "itemid_51222",  # Hb 血红蛋白
  "itemid_51277",  # RDW - 脓毒症预后
  "itemid_51301",  # WBC 白细胞
  "itemid_50882",  # HCO3 - 代谢
  "itemid_50983",  # Na 钠
  "itemid_50971",  # K 钾
  "itemid_51237",  # INR - 凝血
  "itemid_50868",  # Anion Gap 阴离子间隙
  "itemid_50902",  # Cl 氯
  "itemid_51274",  # PT
  "itemid_51275",  # APTT
  "itemid_51221",  # HCT 红细胞压积
  "itemid_51279",  # RBC 红细胞
  "itemid_50893"   # Total Calcium 血钙
)
extra_vars <- intersect(literature_vars, extra_vars_all)
if (length(extra_vars) < 2L) {
  extra_vars <- extra_vars_all
  cat("文献变量在数据中不足2个，使用全部可用变量。\n", sep = "")
}
cat("可用额外变量 (", length(extra_vars), " 个，文献优先): ", paste(extra_vars, collapse = ", "), "\n\n", sep = "")

## ====================================================================
## 5) 数值预处理辅助函数
## ====================================================================
prep_numeric_var <- function(df, vname) {
  x <- to_num01(df[[vname]])
  med <- suppressWarnings(median(x, na.rm = TRUE))
  if (!is.finite(med)) med <- 0
  x[is.na(x)] <- med
  x <- as.numeric(scale(x))
  x[!is.finite(x)] <- 0
  x
}

## ====================================================================
## 6) JM 单模型拟合 + 评估函数
## ====================================================================
fit_and_eval_jm <- function(d_in, fixed_vars_use, t0_now) {
  d <- d_in
  for (vv in fixed_vars_use) {
    d[[vv]] <- prep_numeric_var(d, vv)
  }

  rhs <- paste(c("t", fixed_vars_use), collapse = " + ")
  fm <- as.formula(paste("Y ~", rhs))

  lme_fit <- nlme::lme(
    fixed = fm,
    random = ~ 1 | ID,
    data = d,
    na.action = na.omit,
    control = nlme::lmeControl(
      opt = "optim", maxIter = 200, msMaxIter = 200, niterEM = 50
    )
  )

  surv_data <- d[!duplicated(d$ID), c("ID", "obs_time", "event"), drop = FALSE]
  cox_fit <- survival::coxph(survival::Surv(obs_time, event) ~ 1, data = surv_data, x = TRUE, model = TRUE)
  jm_fit <- JM::jointModel(
    lmeObject = lme_fit, survObject = cox_fit, timeVar = "t", method = "weibull-PH-aGH"
  )

  ## 评估指标
  rhs_terms <- attr(terms(fm), "term.labels")
  rhs_form <- if (length(rhs_terms) > 0) as.formula(paste("~", paste(rhs_terms, collapse = " + "))) else ~ 1

  need_cols <- unique(c("ID", "obs_time", "event", fixed_vars_use))
  need_cols <- need_cols[need_cols %in% names(d)]
  surv_eval <- d[!duplicated(d$ID), need_cols, drop = FALSE]
  surv_eval$t <- t0_now

  beta <- nlme::fixed.effects(lme_fit)
  X <- model.matrix(rhs_form, data = surv_eval)
  beta_use <- beta[colnames(X)]
  beta_use[is.na(beta_use)] <- 0
  fixed_part <- as.numeric(X %*% beta_use)

  re <- nlme::ranef(lme_fit)
  if (is.null(dim(re))) re <- matrix(re, ncol = 1)
  re_df <- as.data.frame(re)
  re_df$ID <- suppressWarnings(as.numeric(rownames(re_df)))
  re_df <- re_df[match(surv_eval$ID, re_df$ID), , drop = FALSE]
  re_int <- if (ncol(re_df) >= 1) as.numeric(re_df[[1]]) else rep(0, nrow(surv_eval))
  re_int[is.na(re_int)] <- 0

  assoc_val <- NA_real_
  for (nm in c("Assoct", "alpha", "AssoctE", "AssoctEV")) {
    if (!is.null(jm_fit$coefficients[[nm]])) {
      assoc_val <- suppressWarnings(as.numeric(jm_fit$coefficients[[nm]])[1])
      if (is.finite(assoc_val)) break
    }
  }
  if (!is.finite(assoc_val)) assoc_val <- 1

  risk <- assoc_val * (fixed_part + re_int)

  cindex <- tryCatch(
    as.numeric(survival::concordance(Surv(surv_eval$obs_time, surv_eval$event) ~ risk)$concordance),
    error = function(e) NA_real_
  )
  if (!is.na(cindex) && cindex < 0.5) {
    risk <- -risk
    cindex <- tryCatch(
      as.numeric(survival::concordance(Surv(surv_eval$obs_time, surv_eval$event) ~ risk)$concordance),
      error = function(e) NA_real_
    )
  }

  auc <- tryCatch({
    roc1 <- timeROC(T = surv_eval$obs_time, delta = surv_eval$event, marker = risk, cause = 1, times = t0_now)
    roc2 <- timeROC(T = surv_eval$obs_time, delta = surv_eval$event, marker = -risk, cause = 1, times = t0_now)
    auc1 <- if (length(roc1$AUC) >= 2) roc1$AUC[2] else roc1$AUC[1]
    auc2 <- if (length(roc2$AUC) >= 2) roc2$AUC[2] else roc2$AUC[1]
    max(as.numeric(auc1), as.numeric(auc2), na.rm = TRUE)
  }, error = function(e) NA_real_)

  bs <- tryCatch({
    haz <- exp(as.numeric(scale(risk)))
    sp <- exp(-haz * t0_now)
    mean(ifelse(surv_eval$obs_time <= t0_now & surv_eval$event == 1, (1 - sp)^2, sp^2), na.rm = TRUE)
  }, error = function(e) NA_real_)

  c(cindex = safe_num(cindex), bs = safe_num(bs), auc = safe_num(auc))
}

t0_jm <- median(data_clean$obs_time[data_clean$obs_time > 0], na.rm = TRUE)

## ====================================================================
## 7) 4V 建模：age + gender + C(n,2) 组合遍历
## ====================================================================
cat("========== 4V JM 建模开始 ==========\n")

if (length(extra_vars) < 2L) stop("4V建模可用额外变量不足2个。")

pair_mat_4v <- utils::combn(extra_vars, 2)
n_comb_4v <- ncol(pair_mat_4v)
cat("4V总组合数: C(", length(extra_vars), ",2) = ", n_comb_4v, "\n", sep = "")

max_pairs_4v <- 100L
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
    met <- fit_and_eval_jm(data_clean, fixed_vars_use = vars_i, t0_now = t0_jm)
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
    if (i <= 20) cat("JM 4V失败[", i, "] ", msg, "\n", sep = "")
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
    cat("JM 4V进度: ", i, "/", n_comb_4v, " 完成\n", sep = "")
  }
}

result_jm_4v <- dplyr::bind_rows(res_4v) %>%
  dplyr::arrange(dplyr::desc(cindex), bs, dplyr::desc(auc))

out_4v <- file.path(out_dir, "0319_resultJM_4v.xlsx")
writexl::write_xlsx(result_jm_4v, out_4v)

t_end_4v <- Sys.time()
cat("\n4V建模完成，耗时: ", round(difftime(t_end_4v, t_start_4v, units = "secs"), 1), " 秒\n", sep = "")
cat("固定变量: ", age_var, " + ", sex_var, "\n", sep = "")
cat("Y变量: ", y_var, "\n", sep = "")
cat("成功模型数: ", sum(is.finite(result_jm_4v$cindex)), "/", nrow(result_jm_4v), "\n", sep = "")
cat("结果文件: ", out_4v, "\n", sep = "")
cat("Top 5 模型:\n")
print(utils::head(result_jm_4v, 5))

## ====================================================================
## 8) 10V 建模：age + gender + C(n,8) 组合遍历
## ====================================================================
cat("\n========== 10V JM 建模开始 ==========\n")

if (length(extra_vars) < 8L) {
  cat("额外变量仅 ", length(extra_vars), " 个，不足8个，将使用全部额外变量。\n", sep = "")
  n_extra_10v <- length(extra_vars)
} else {
  n_extra_10v <- 8L
}

comb_mat_10v <- utils::combn(extra_vars, n_extra_10v)
n_comb_10v <- ncol(comb_mat_10v)
cat("10V总组合数: C(", length(extra_vars), ",", n_extra_10v, ") = ", n_comb_10v, "\n", sep = "")

max_pairs_10v <- 100L
if (n_comb_10v > max_pairs_10v) {
  set.seed(42)
  sel_idx <- sort(sample(n_comb_10v, max_pairs_10v))
  comb_mat_10v <- comb_mat_10v[, sel_idx, drop = FALSE]
  cat("组合过多，随机抽取 ", max_pairs_10v, " 组。\n", sep = "")
  n_comb_10v <- max_pairs_10v
}

res_10v <- vector("list", n_comb_10v)
t_start_10v <- Sys.time()

for (i in seq_len(n_comb_10v)) {
  vars_i <- c(age_var, sex_var, comb_mat_10v[, i])
  one <- tryCatch({
    met <- fit_and_eval_jm(data_clean, fixed_vars_use = vars_i, t0_now = t0_jm)
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
    if (i <= 20) cat("JM 10V失败[", i, "] ", msg, "\n", sep = "")
    data.frame(
      itemids   = paste(vars_i, collapse = ";"),
      cindex    = NA_real_,
      bs        = NA_real_,
      auc       = NA_real_,
      error_msg = msg,
      stringsAsFactors = FALSE
    )
  })
  res_10v[[i]] <- one
  if (i %% 50 == 0 || i == n_comb_10v) {
    cat("JM 10V进度: ", i, "/", n_comb_10v, " 完成\n", sep = "")
  }
}

result_jm_10v <- dplyr::bind_rows(res_10v) %>%
  dplyr::arrange(dplyr::desc(cindex), bs, dplyr::desc(auc))

out_10v <- file.path(out_dir, "0319_resultJM_10v.xlsx")
writexl::write_xlsx(result_jm_10v, out_10v)

t_end_10v <- Sys.time()
cat("\n10V建模完成，耗时: ", round(difftime(t_end_10v, t_start_10v, units = "secs"), 1), " 秒\n", sep = "")
cat("固定变量: ", age_var, " + ", sex_var, "\n", sep = "")
cat("Y变量: ", y_var, "\n", sep = "")
cat("成功模型数: ", sum(is.finite(result_jm_10v$cindex)), "/", nrow(result_jm_10v), "\n", sep = "")
cat("结果文件: ", out_10v, "\n", sep = "")
cat("Top 5 模型:\n")
print(utils::head(result_jm_10v, 5))

## ====================================================================
## 9) 汇总
## ====================================================================
cat("\n========== JM 汇总 ==========\n")
cat("4V 结果: ", out_4v, "\n", sep = "")
cat("10V 结果: ", out_10v, "\n", sep = "")

best_4v <- result_jm_4v %>% dplyr::filter(is.finite(cindex)) %>% dplyr::slice(1)
best_10v <- result_jm_10v %>% dplyr::filter(is.finite(cindex)) %>% dplyr::slice(1)

if (nrow(best_4v) > 0) {
  cat("\n4V 最优模型: ", best_4v$itemids, "\n", sep = "")
  cat("  CINDEX=", best_4v$cindex, " AUC=", best_4v$auc, " BS=", best_4v$bs, "\n", sep = "")
}
if (nrow(best_10v) > 0) {
  cat("\n10V 最优模型: ", best_10v$itemids, "\n", sep = "")
  cat("  CINDEX=", best_10v$cindex, " AUC=", best_10v$auc, " BS=", best_10v$bs, "\n", sep = "")
}

cat("\nJM 全部完成。\n")
