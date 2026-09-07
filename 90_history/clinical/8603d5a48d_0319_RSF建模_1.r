## =========================
## RSF建模 - 部分变量组合遍历版
## 性别+年龄固定，遍历其他变量的组合
## 4V: age + gender + C(n,2) 组合 -> 0319_resultRSF_4v.xlsx
## 10V: age + gender + C(n,8) 组合 -> 0319_resultRSF_10v.xlsx
## =========================
suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(survival)
  library(timeROC)
  library(randomForestSRC)
  library(writexl)
})
set.seed(123)

out_dir <- "F:/文章_大论文/MIMIC数据库_代码/TREA代码/0319部分变量建模"

## ====================================================================
## 1) 数据读取与结局合并
## ====================================================================
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

## ====================================================================
## 2) 调整结构：仅用 Obstimes=0/1，每患者保留一条（优先 Obstimes=1）
## ====================================================================
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

## ====================================================================
## 3) 泄漏变量过滤 & 固定变量确定
## ====================================================================
is_leak_var <- function(v) {
  grepl("disch|outtime|icu_out|edout|los|mort|death|event|charttime|hadm|stay|subject|id$", v, ignore.case = TRUE)
}
pred_strict <- keep_pred[!vapply(keep_pred, is_leak_var, logical(1))]
if (length(pred_strict) == 0L) stop("严格基线过滤后无可用变量。")

pick_first_existing <- function(cands, pool) {
  x <- cands[cands %in% pool]
  if (length(x) == 0L) return(NA_character_)
  x[1]
}

age_var <- pick_first_existing(c("itemid_anchor_age", "itemid_age", "age"), pred_strict)
gender_var <- pick_first_existing(c("itemid_gender", "gender", "sex"), pred_strict)
if (is.na(age_var) || is.na(gender_var)) {
  stop("严格基线变量中未找到 age 或 gender。请检查变量名。")
}
cat("固定变量: ", age_var, " + ", gender_var, "\n", sep = "")

extra_vars <- setdiff(pred_strict, c(age_var, gender_var))
cat("可用额外变量 (", length(extra_vars), " 个): ", paste(extra_vars, collapse = ", "), "\n\n", sep = "")

## ====================================================================
## 4) 训练-测试拆分（分层 event）
## ====================================================================
set.seed(123)
idx_event1 <- which(rsf_data0$event == 1)
idx_event0 <- which(rsf_data0$event == 0)
n1 <- length(idx_event1)
n0 <- length(idx_event0)
n_tr1 <- if (n1 >= 2) max(1, min(n1 - 1, floor(n1 * 0.7))) else max(1, floor(n1 * 0.7))
n_tr0 <- if (n0 >= 2) max(1, min(n0 - 1, floor(n0 * 0.7))) else max(1, floor(n0 * 0.7))
tr_idx <- c(
  if (length(idx_event1) > 0) sample(idx_event1, n_tr1) else integer(0),
  if (length(idx_event0) > 0) sample(idx_event0, n_tr0) else integer(0)
)
tr_idx <- sort(unique(tr_idx))
te_idx <- setdiff(seq_len(nrow(rsf_data0)), tr_idx)
if (length(te_idx) < 10) stop("测试集样本过少。")
train_df <- rsf_data0[tr_idx, , drop = FALSE]
test_df <- rsf_data0[te_idx, , drop = FALSE]
t0_split <- median(train_df$obs_time, na.rm = TRUE)

cat("训练集: ", nrow(train_df), " 行, 测试集: ", nrow(test_df), " 行\n", sep = "")
cat("t0 = ", round(t0_split, 4), "\n\n", sep = "")

## ====================================================================
## 5) RSF split 建模函数（训练集建模，测试集评估）
## ====================================================================
cal_3_RSF_split <- function(train_data, test_data, t0, vars_use) {
  numify_pair <- function(x_tr, x_te) {
    if (inherits(x_tr, "POSIXt") || inherits(x_tr, "Date")) x_tr <- as.numeric(x_tr)
    if (inherits(x_te, "POSIXt") || inherits(x_te, "Date")) x_te <- as.numeric(x_te)
    if (is.logical(x_tr)) x_tr <- as.numeric(x_tr)
    if (is.logical(x_te)) x_te <- as.numeric(x_te)
    if (is.numeric(x_tr) || is.integer(x_tr)) {
      tr <- as.numeric(x_tr)
      te <- suppressWarnings(as.numeric(x_te))
    } else {
      trc <- trimws(as.character(x_tr)); trc[trc == ""] <- NA_character_
      tec <- trimws(as.character(x_te)); tec[tec == ""] <- NA_character_
      lv <- unique(c(trc, tec))
      tr <- match(trc, lv)
      te <- match(tec, lv)
    }
    list(tr = tr, te = te)
  }

  keep_mat <- list()
  for (v in vars_use) {
    z <- numify_pair(train_data[[v]], test_data[[v]])
    xtr <- z$tr; xte <- z$te
    med <- suppressWarnings(median(xtr, na.rm = TRUE))
    if (!is.finite(med)) med <- 0
    xtr[is.na(xtr)] <- med; xte[is.na(xte)] <- med
    s <- stats::sd(xtr, na.rm = TRUE)
    if (is.finite(s) && s > 1e-8) {
      m <- mean(xtr, na.rm = TRUE)
      xtr <- (xtr - m) / s; xte <- (xte - m) / s
    }
    if (length(unique(xtr)) <= 1) next
    keep_mat[[v]] <- list(tr = xtr, te = xte)
  }
  if (length(keep_mat) == 0L) stop("变量在训练集均无有效变异。")

  tr_pred <- as.data.frame(lapply(keep_mat, function(x) x$tr), check.names = FALSE)
  te_pred <- as.data.frame(lapply(keep_mat, function(x) x$te), check.names = FALSE)
  tr <- data.frame(tr_pred, obs_time = train_data$obs_time, event = as.integer(train_data$event), check.names = FALSE)
  te <- data.frame(te_pred, obs_time = test_data$obs_time, event = as.integer(test_data$event), check.names = FALSE)
  names(tr) <- make.names(names(tr), unique = TRUE)
  names(te) <- make.names(names(te), unique = TRUE)

  if (length(unique(tr$event)) < 2L) stop("训练集event仅单一取值。")
  if (length(unique(te$event)) < 2L) stop("测试集event仅单一取值。")

  mtry_val <- max(1, floor((ncol(tr) - 2) / 3))
  fit <- randomForestSRC::rfsrc(
    formula = stats::as.formula("Surv(obs_time, event) ~ ."),
    data = tr,
    ntree = 1000,
    mtry = mtry_val,
    nodesize = 10,
    importance = FALSE,
    proximity = FALSE,
    na.action = "na.impute",
    seed = 123
  )

  pred <- predict(fit, newdata = te)
  n_obs <- nrow(te)
  risk_marker <- NULL
  survival_probs <- NULL
  if (!is.null(pred$survival) && !is.null(pred$time.interest) && length(pred$time.interest) > 0) {
    t_idx <- which.min(abs(pred$time.interest - t0))
    survival_probs <- if (is.matrix(pred$survival)) as.numeric(pred$survival[, t_idx]) else as.numeric(pred$survival)
    if (length(survival_probs) == n_obs) risk_marker <- 1 - survival_probs
  }
  if (is.null(risk_marker) && !is.null(pred$predicted) && length(pred$predicted) == n_obs) {
    risk_marker <- as.numeric(pred$predicted)
    survival_probs <- pmax(0, pmin(1, 1 - risk_marker))
  }
  if (is.null(risk_marker) || length(risk_marker) != n_obs) stop("测试集风险分数提取失败。")

  surv_time <- te$obs_time
  surv_status <- te$event

  roc_obj1 <- timeROC::timeROC(T = surv_time, delta = surv_status, marker = risk_marker, cause = 1, times = t0)
  roc_obj2 <- timeROC::timeROC(T = surv_time, delta = surv_status, marker = -risk_marker, cause = 1, times = t0)
  auc1 <- if (length(roc_obj1$AUC) >= 2) roc_obj1$AUC[2] else roc_obj1$AUC[1]
  auc2 <- if (length(roc_obj2$AUC) >= 2) roc_obj2$AUC[2] else roc_obj2$AUC[1]
  AUC <- max(as.numeric(auc1), as.numeric(auc2), na.rm = TRUE)

  CINDEX <- as.numeric(survival::concordance(survival::Surv(surv_time, surv_status) ~ risk_marker)$concordance)
  if (!is.na(CINDEX) && CINDEX < 0.5) {
    CINDEX <- as.numeric(survival::concordance(survival::Surv(surv_time, surv_status) ~ I(-risk_marker))$concordance)
  }

  Y_obs <- as.numeric(surv_time > t0 | (surv_time <= t0 & surv_status == 0))
  BS <- mean((survival_probs - Y_obs)^2, na.rm = TRUE)

  data.frame(AUC = round(AUC, 4), CINDEX = round(CINDEX, 4), BS = round(BS, 4))
}

## ====================================================================
## 6) 4V 建模：age + gender + C(n,2) 组合遍历
## ====================================================================
cat("========== 4V RSF 建模开始 ==========\n")

if (length(extra_vars) < 2L) stop("4V建模可用额外变量不足2个。")

pair_mat_4v <- utils::combn(extra_vars, 2)
n_comb_4v <- ncol(pair_mat_4v)
cat("4V总组合数: C(", length(extra_vars), ",2) = ", n_comb_4v, "\n", sep = "")

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
  vars_i <- c(age_var, gender_var, pair_mat_4v[, i])
  one <- tryCatch({
    mt_i <- cal_3_RSF_split(train_df, test_df, t0 = t0_split, vars_use = vars_i)
    data.frame(
      itemids   = paste(vars_i, collapse = ";"),
      cindex    = as.numeric(mt_i$CINDEX[1]),
      bs        = as.numeric(mt_i$BS[1]),
      auc       = as.numeric(mt_i$AUC[1]),
      error_msg = "",
      stringsAsFactors = FALSE
    )
  }, error = function(e) {
    msg <- conditionMessage(e)
    if (i <= 20) cat("RSF 4V失败[", i, "] ", msg, "\n", sep = "")
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
    cat("RSF 4V进度: ", i, "/", n_comb_4v, " 完成\n", sep = "")
  }
}

result_rsf_4v <- dplyr::bind_rows(res_4v) %>%
  dplyr::arrange(dplyr::desc(cindex), bs, dplyr::desc(auc))

out_4v <- file.path(out_dir, "0319_resultRSF_4v.xlsx")
writexl::write_xlsx(result_rsf_4v, out_4v)

t_end_4v <- Sys.time()
cat("\n4V建模完成，耗时: ", round(difftime(t_end_4v, t_start_4v, units = "secs"), 1), " 秒\n", sep = "")
cat("固定变量: ", age_var, " + ", gender_var, "\n", sep = "")
cat("成功模型数: ", sum(is.finite(result_rsf_4v$cindex)), "/", nrow(result_rsf_4v), "\n", sep = "")
cat("结果文件: ", out_4v, "\n", sep = "")
cat("Top 5 模型:\n")
print(utils::head(result_rsf_4v, 5))

## ====================================================================
## 7) 10V 建模：age + gender + C(n,8) 组合遍历
## ====================================================================
cat("\n========== 10V RSF 建模开始 ==========\n")

if (length(extra_vars) < 8L) {
  cat("额外变量仅 ", length(extra_vars), " 个，不足8个，将使用全部额外变量。\n", sep = "")
  n_extra_10v <- length(extra_vars)
} else {
  n_extra_10v <- 8L
}

comb_mat_10v <- utils::combn(extra_vars, n_extra_10v)
n_comb_10v <- ncol(comb_mat_10v)
cat("10V总组合数: C(", length(extra_vars), ",", n_extra_10v, ") = ", n_comb_10v, "\n", sep = "")

max_pairs_10v <- 2000L
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
  vars_i <- c(age_var, gender_var, comb_mat_10v[, i])
  one <- tryCatch({
    mt_i <- cal_3_RSF_split(train_df, test_df, t0 = t0_split, vars_use = vars_i)
    data.frame(
      itemids   = paste(vars_i, collapse = ";"),
      cindex    = as.numeric(mt_i$CINDEX[1]),
      bs        = as.numeric(mt_i$BS[1]),
      auc       = as.numeric(mt_i$AUC[1]),
      error_msg = "",
      stringsAsFactors = FALSE
    )
  }, error = function(e) {
    msg <- conditionMessage(e)
    if (i <= 20) cat("RSF 10V失败[", i, "] ", msg, "\n", sep = "")
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
    cat("RSF 10V进度: ", i, "/", n_comb_10v, " 完成\n", sep = "")
  }
}

result_rsf_10v <- dplyr::bind_rows(res_10v) %>%
  dplyr::arrange(dplyr::desc(cindex), bs, dplyr::desc(auc))

out_10v <- file.path(out_dir, "0319_resultRSF_10v.xlsx")
writexl::write_xlsx(result_rsf_10v, out_10v)

t_end_10v <- Sys.time()
cat("\n10V建模完成，耗时: ", round(difftime(t_end_10v, t_start_10v, units = "secs"), 1), " 秒\n", sep = "")
cat("固定变量: ", age_var, " + ", gender_var, "\n", sep = "")
cat("成功模型数: ", sum(is.finite(result_rsf_10v$cindex)), "/", nrow(result_rsf_10v), "\n", sep = "")
cat("结果文件: ", out_10v, "\n", sep = "")
cat("Top 5 模型:\n")
print(utils::head(result_rsf_10v, 5))

## ====================================================================
## 8) 汇总
## ====================================================================
cat("\n========== RSF 汇总 ==========\n")
cat("4V 结果: ", out_4v, "\n", sep = "")
cat("10V 结果: ", out_10v, "\n", sep = "")

best_4v <- result_rsf_4v %>% dplyr::filter(is.finite(cindex)) %>% dplyr::slice(1)
best_10v <- result_rsf_10v %>% dplyr::filter(is.finite(cindex)) %>% dplyr::slice(1)

if (nrow(best_4v) > 0) {
  cat("\n4V 最优模型: ", best_4v$itemids, "\n", sep = "")
  cat("  CINDEX=", best_4v$cindex, " AUC=", best_4v$auc, " BS=", best_4v$bs, "\n", sep = "")
}
if (nrow(best_10v) > 0) {
  cat("\n10V 最优模型: ", best_10v$itemids, "\n", sep = "")
  cat("  CINDEX=", best_10v$cindex, " AUC=", best_10v$auc, " BS=", best_10v$bs, "\n", sep = "")
}

cat("\nRSF 全部完成。\n")
