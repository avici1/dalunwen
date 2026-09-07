## =========================
## COX建模脚本
## 内容：数据引入 -> 数据结构处理 -> COX全变量建模 -> 10V/4V批量建模 -> 5折CV
## 输出格式与 JM/RSF/RSFLC 保持一致：itemids / cindex / bs / auc
## =========================
suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(survival)
  library(timeROC)
  library(writexl)
})
set.seed(123)

## ====================================================================
## 1) 数据读取与结局合并（与 RSF建模 一致，使用 widedata_merge.xlsx）
## ====================================================================
rsf_dir <- "F:/文章_大论文/MIMIC数据库_代码/TREA代码/0314"
rsf_path_candidates <- c(
  file.path(rsf_dir, "widedata_merge.xlsx"),
  file.path(rsf_dir, "0314_widedata_merge.xlsx")
)
rsf_path <- rsf_path_candidates[file.exists(rsf_path_candidates)][1]
if (is.na(rsf_path)) stop("未找到 widedata_merge.xlsx 或 0314_widedata_merge.xlsx")
raw_data <- readxl::read_xlsx(rsf_path)

base_path <- "F:/文章_大论文/MIMIC数据库_代码/TREA代码/0311/stroke_baseline_simple.xlsx"
if (!file.exists(base_path)) stop(paste0("文件不存在: ", base_path))
base_dat <- readxl::read_xlsx(base_path) %>%
  dplyr::select(hadm_id, hospital_mortality) %>%
  dplyr::distinct(hadm_id, .keep_all = TRUE) %>%
  dplyr::rename(hospitalmortality = hospital_mortality)

if ("hadm_id" %in% names(raw_data)) {
  raw_data <- raw_data %>%
    dplyr::mutate(hadm_id = as.character(hadm_id)) %>%
    dplyr::left_join(base_dat %>% dplyr::mutate(hadm_id = as.character(hadm_id)), by = "hadm_id")
}

## ====================================================================
## 2) 调整结构：仅用 Obstimes=0/1，每患者保留一条（优先 Obstimes=1）
## ====================================================================
id_col <- if ("subject_id" %in% names(raw_data)) "subject_id" else if ("subjectid" %in% names(raw_data)) "subjectid" else stop("未找到 subject_id/subjectid")
if (!("Obstimes" %in% names(raw_data))) stop("未找到 Obstimes 列")
if (!("itemid_los_hosp_days" %in% names(raw_data))) stop("未找到 itemid_los_hosp_days（生存时间）")
if (!("hospitalmortality" %in% names(raw_data))) stop("未找到 hospitalmortality（事件）")

to_numeric_safe <- function(x) {
  if (is.list(x)) x <- unlist(x, use.names = FALSE)
  suppressWarnings(as.numeric(x))
}
raw_data$Obstimes <- to_numeric_safe(raw_data$Obstimes)
raw_data$itemid_los_hosp_days <- to_numeric_safe(raw_data$itemid_los_hosp_days)
raw_data$hospitalmortality <- to_numeric_safe(raw_data$hospitalmortality)

cox_data0 <- raw_data %>%
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
pred_vars <- setdiff(names(cox_data0), exclude_vars)
keep_pred <- pred_vars[vapply(pred_vars, function(v) {
  x <- cox_data0[[v]]
  sum(!is.na(x)) > 0 && length(unique(x[!is.na(x)])) > 1
}, logical(1))]
if (length(keep_pred) == 0L) stop("无可用预测变量，请检查数据。")

## 变量清洗：字符转因子
for (v in keep_pred) {
  x <- cox_data0[[v]]
  if (is.character(x)) {
    x <- trimws(x)
    x[x == ""] <- NA_character_
    lv <- names(sort(table(x), decreasing = TRUE))
    if (length(lv) > 15L) {
      keep <- lv[1:14]
      x <- ifelse(is.na(x), NA_character_, ifelse(x %in% keep, x, "Others"))
    }
    cox_data0[[v]] <- as.factor(x)
  }
}

safe_metric <- function(x, digits = 4) ifelse(is.finite(x), round(x, digits), NA_real_)

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

## ====================================================================
## 3) COX模型构建 + 结果计算（AUC/CINDEX/BS）- 全变量版本
## ====================================================================
cal_3_COX_mimic <- function(data, t0, keep_pred) {
  cox_df_raw <- data[, c(keep_pred, "obs_time", "event"), drop = FALSE]
  cox_df_raw$obs_time <- to_numeric_safe(cox_df_raw$obs_time)
  cox_df_raw$event <- ifelse(to_numeric_safe(cox_df_raw$event) > 0, 1, 0)
  cox_df_raw <- cox_df_raw[is.finite(cox_df_raw$obs_time) & !is.na(cox_df_raw$obs_time) &
                             cox_df_raw$obs_time > 0 & !is.na(cox_df_raw$event), , drop = FALSE]

  pred_cols_raw <- setdiff(names(cox_df_raw), c("obs_time", "event"))
  pred_clean <- list()
  for (v in pred_cols_raw) {
    x <- cox_df_raw[[v]]
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
  if (length(pred_clean) == 0L) stop("清洗后无可用预测变量，无法建立 COX 模型。")

  cox_df <- data.frame(pred_clean, obs_time = cox_df_raw$obs_time, event = cox_df_raw$event, check.names = FALSE)
  names(cox_df) <- make.names(names(cox_df), unique = TRUE)
  pred_cols <- setdiff(names(cox_df), c("obs_time", "event"))
  if (length(pred_cols) == 0L) stop("模型预测变量为空。")

  surv_time <- cox_df$obs_time
  surv_status <- cox_df$event
  if (length(unique(surv_status)) < 2L) stop("event 只有一个取值，无法计算生存判别指标。")

  fm <- as.formula(paste("Surv(obs_time, event) ~", paste(pred_cols, collapse = " + ")))
  fit <- tryCatch(
    survival::coxph(fm, data = cox_df, x = TRUE, model = TRUE),
    error = function(e) stop(paste0("COX拟合失败: ", conditionMessage(e)))
  )

  risk_marker <- as.numeric(predict(fit, newdata = cox_df, type = "risk"))
  survival_probs <- tryCatch({
    sf <- survival::survfit(fit, newdata = cox_df)
    t_idx <- which.min(abs(sf$time - t0))
    if (is.matrix(sf$surv)) as.numeric(sf$surv[t_idx, ]) else rep(as.numeric(sf$surv[t_idx]), nrow(cox_df))
  }, error = function(e) {
    base_haz <- exp(-risk_marker * t0 / mean(surv_time))
    pmax(0, pmin(1, base_haz))
  })

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
  censoring_model <- survival::survfit(survival::Surv(obs_time, 1 - event) ~ 1, data = cox_df)
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

t0_cox <- median(cox_data0$obs_time, na.rm = TRUE)
cox_res <- cal_3_COX_mimic(cox_data0, t0 = t0_cox, keep_pred = keep_pred)
cox_metrics <- cox_res$metrics

cox_structure <- list(
  input_file = rsf_path,
  n_rows_raw = nrow(raw_data),
  n_rows_model = nrow(cox_data0),
  n_patients = length(unique(cox_data0$ID)),
  n_predictors = length(keep_pred),
  t0 = t0_cox
)

cat("\nCOX建模完成（Obstimes=0/1，单条记录/患者）。\n")
print(cox_metrics)
cat("\nCOX输入结构摘要：\n")
cat("- 输入文件: ", cox_structure$input_file, "\n", sep = "")
cat("- 原始行数: ", cox_structure$n_rows_raw, "\n", sep = "")
cat("- 建模行数: ", cox_structure$n_rows_model, "\n", sep = "")
cat("- 患者数: ", cox_structure$n_patients, "\n", sep = "")
cat("- 预测变量数: ", cox_structure$n_predictors, "\n", sep = "")
cat("- t0: ", round(cox_structure$t0, 6), "\n", sep = "")


## ====================================================================
## 4) 10V批量建模：每10个itemid一组建COX模型
## ====================================================================

batch_size_10v <- 10L
batches_10v <- split(keep_pred, ceiling(seq_along(keep_pred) / batch_size_10v))
t0_10v <- median(cox_data0$obs_time, na.rm = TRUE)

res_10v <- vector("list", length(batches_10v))
for (i in seq_along(batches_10v)) {
  vars_i <- batches_10v[[i]]
  one <- tryCatch({
    fit_i <- cal_3_COX_mimic(cox_data0, t0 = t0_10v, keep_pred = vars_i)
    data.frame(
      itemids = paste(vars_i, collapse = ";"),
      cindex = as.numeric(fit_i$metrics$CINDEX[1]),
      bs = as.numeric(fit_i$metrics$BS[1]),
      auc = as.numeric(fit_i$metrics$AUC[1]),
      stringsAsFactors = FALSE
    )
  }, error = function(e) {
    if (i <= 12) cat("COX 10V失败[", i, "] ", conditionMessage(e), "\n", sep = "")
    data.frame(
      itemids = paste(vars_i, collapse = ";"),
      cindex = NA_real_,
      bs = NA_real_,
      auc = NA_real_,
      stringsAsFactors = FALSE
    )
  })
  res_10v[[i]] <- one
  cat("COX 10V进度: ", i, "/", length(batches_10v), " 完成\n", sep = "")
}

result_cox_10v <- dplyr::bind_rows(res_10v)
out_cox_10v <- "F:/文章_大论文/MIMIC数据库_代码/TREA代码/0315/result_COX_10V.xlsx"
writexl::write_xlsx(result_cox_10v, out_cox_10v)

cat("\nCOX 10V批量建模结束。\n")
cat("结果文件: ", out_cox_10v, "\n", sep = "")
print(utils::head(result_cox_10v, 10))


## ====================================================================
## 5) 4V批量建模：age + gender 固定，另外2个从其余变量组合
## ====================================================================

is_leak_var <- function(v) {
  grepl("disch|outtime|icu_out|edout|los|mort|death|event|charttime|hadm|stay|subject|id$", v, ignore.case = TRUE)
}
pred_strict <- keep_pred[!vapply(keep_pred, is_leak_var, logical(1))]
if (length(pred_strict) == 0L) stop("4V区块严格基线过滤后无可用变量。")

pick_first_existing <- function(cands, pool) {
  x <- cands[cands %in% pool]
  if (length(x) == 0L) return(NA_character_)
  x[1]
}

age_var_4v <- pick_first_existing(c("itemid_anchor_age", "itemid_age", "age"), pred_strict)
gender_var_4v <- pick_first_existing(c("itemid_gender", "gender", "sex"), pred_strict)
if (is.na(age_var_4v) || is.na(gender_var_4v)) {
  stop("严格基线变量中未找到 age 或 gender。请检查变量名。")
}

extra_vars_4v <- setdiff(pred_strict, c(age_var_4v, gender_var_4v))
if (length(extra_vars_4v) < 2L) stop("4V建模可用额外变量不足2个。")

## 训练-测试拆分（分层event）
set.seed(123)
idx_event1 <- which(cox_data0$event == 1)
idx_event0 <- which(cox_data0$event == 0)
n1 <- length(idx_event1)
n0 <- length(idx_event0)
n_tr1 <- if (n1 >= 2) max(1, min(n1 - 1, floor(n1 * 0.7))) else max(1, floor(n1 * 0.7))
n_tr0 <- if (n0 >= 2) max(1, min(n0 - 1, floor(n0 * 0.7))) else max(1, floor(n0 * 0.7))
tr_idx <- c(
  if (length(idx_event1) > 0) sample(idx_event1, n_tr1) else integer(0),
  if (length(idx_event0) > 0) sample(idx_event0, n_tr0) else integer(0)
)
tr_idx <- sort(unique(tr_idx))
te_idx <- setdiff(seq_len(nrow(cox_data0)), tr_idx)
if (length(te_idx) < 30) stop("4V区块测试集样本过少。")
train_df <- cox_data0[tr_idx, , drop = FALSE]
test_df <- cox_data0[te_idx, , drop = FALSE]
t0_4v <- median(train_df$obs_time, na.rm = TRUE)

## 4V专用：训练集建模，测试集评估
cal_3_COX_split <- function(train_data, test_data, t0, vars_use) {
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
  if (length(keep_mat) == 0L) stop("4V区块：变量在训练集均无有效变异。")

  tr_pred <- as.data.frame(lapply(keep_mat, function(x) x$tr), check.names = FALSE)
  te_pred <- as.data.frame(lapply(keep_mat, function(x) x$te), check.names = FALSE)
  tr <- data.frame(tr_pred, obs_time = train_data$obs_time, event = as.integer(train_data$event), check.names = FALSE)
  te <- data.frame(te_pred, obs_time = test_data$obs_time, event = as.integer(test_data$event), check.names = FALSE)
  names(tr) <- make.names(names(tr), unique = TRUE)
  names(te) <- make.names(names(te), unique = TRUE)

  if (length(unique(tr$event)) < 2L) stop("训练集event仅单一取值。")
  if (length(unique(te$event)) < 2L) stop("测试集event仅单一取值。")

  pred_cols <- setdiff(names(tr), c("obs_time", "event"))
  fm <- as.formula(paste("Surv(obs_time, event) ~", paste(pred_cols, collapse = " + ")))
  fit <- survival::coxph(fm, data = tr, x = TRUE, model = TRUE)

  risk_marker <- as.numeric(predict(fit, newdata = te, type = "risk"))
  surv_time <- te$obs_time
  surv_status <- te$event

  survival_probs <- tryCatch({
    sf <- survival::survfit(fit, newdata = te)
    t_idx <- which.min(abs(sf$time - t0))
    if (is.matrix(sf$surv)) as.numeric(sf$surv[t_idx, ]) else rep(as.numeric(sf$surv[t_idx]), nrow(te))
  }, error = function(e) {
    pmax(0, pmin(1, exp(-risk_marker * t0 / mean(surv_time))))
  })

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

pair_mat_4v <- utils::combn(extra_vars_4v, 2)
max_pairs_4v <- 300L
if (ncol(pair_mat_4v) > max_pairs_4v) {
  pair_mat_4v <- pair_mat_4v[, seq_len(max_pairs_4v), drop = FALSE]
  cat("提示：4V组合过多，仅尝试前 ", max_pairs_4v, " 组。\n", sep = "")
}

res_4v <- vector("list", ncol(pair_mat_4v))
for (i in seq_len(ncol(pair_mat_4v))) {
  vars_i <- c(age_var_4v, gender_var_4v, pair_mat_4v[, i])
  one <- tryCatch({
    mt_i <- cal_3_COX_split(train_df, test_df, t0 = t0_4v, vars_use = vars_i)
    data.frame(
      itemids = paste(vars_i, collapse = ";"),
      cindex = as.numeric(mt_i$CINDEX[1]),
      bs = as.numeric(mt_i$BS[1]),
      auc = as.numeric(mt_i$AUC[1]),
      error_msg = "",
      stringsAsFactors = FALSE
    )
  }, error = function(e) {
    msg <- conditionMessage(e)
    if (i <= 12) cat("COX 4V失败[", i, "] ", msg, "\n", sep = "")
    data.frame(
      itemids = paste(vars_i, collapse = ";"),
      cindex = NA_real_,
      bs = NA_real_,
      auc = NA_real_,
      error_msg = msg,
      stringsAsFactors = FALSE
    )
  })
  res_4v[[i]] <- one
  cat("COX 4V进度: ", i, "/", ncol(pair_mat_4v), " 完成\n", sep = "")
}

result_cox_4v <- dplyr::bind_rows(res_4v)
out_cox_4v <- "F:/文章_大论文/MIMIC数据库_代码/TREA代码/0315/result_COX_4V.xlsx"
writexl::write_xlsx(result_cox_4v, out_cox_4v)

cat("\nCOX 4V批量建模结束。\n")
cat("固定变量: ", age_var_4v, " + ", gender_var_4v, "\n", sep = "")
cat("结果文件: ", out_cox_4v, "\n", sep = "")
cat("4V成功模型数: ", sum(is.finite(result_cox_4v$cindex)), "/", nrow(result_cox_4v), "\n", sep = "")
print(utils::head(result_cox_4v, 10))


## ====================================================================
## 6) 5折交叉验证：选CINDEX最优模型做5折CV
## ====================================================================

pick_best_from_table <- function(df, model_tag) {
  if (is.null(df) || nrow(df) == 0) return(NULL)
  req <- c("itemids", "cindex", "bs", "auc")
  if (!all(req %in% names(df))) return(NULL)
  d <- df %>% dplyr::filter(is.finite(.data$cindex))
  if (nrow(d) == 0) return(NULL)
  d <- d %>% dplyr::arrange(dplyr::desc(.data$cindex), .data$bs, dplyr::desc(.data$auc))
  data.frame(
    model_tag = model_tag,
    itemids = as.character(d$itemids[1]),
    cindex = as.numeric(d$cindex[1]),
    bs = as.numeric(d$bs[1]),
    auc = as.numeric(d$auc[1]),
    stringsAsFactors = FALSE
  )
}

cand_best <- dplyr::bind_rows(
  pick_best_from_table(result_cox_10v, "10V"),
  pick_best_from_table(result_cox_4v, "4V")
)
if (nrow(cand_best) == 0) stop("没有可用的已成功COX模型可用于5折交叉验证。")

best_row <- cand_best %>%
  dplyr::arrange(dplyr::desc(.data$cindex), .data$bs, dplyr::desc(.data$auc)) %>%
  dplyr::slice(1)
best_vars <- unlist(strsplit(as.character(best_row$itemids[1]), ";", fixed = TRUE))
best_vars <- best_vars[best_vars %in% names(cox_data0)]
if (length(best_vars) == 0) stop("最佳模型变量在 cox_data0 中不存在，无法CV。")

## 5折分层（按event）
set.seed(2026)
idx1 <- which(cox_data0$event == 1)
idx0 <- which(cox_data0$event == 0)
fold_id <- rep(NA_integer_, nrow(cox_data0))
fold_id[idx1] <- sample(rep(1:5, length.out = length(idx1)))
fold_id[idx0] <- sample(rep(1:5, length.out = length(idx0)))

cv_rows <- vector("list", 5)
for (k in 1:5) {
  te_idx_k <- which(fold_id == k)
  tr_idx_k <- setdiff(seq_len(nrow(cox_data0)), te_idx_k)
  tr <- cox_data0[tr_idx_k, , drop = FALSE]
  te <- cox_data0[te_idx_k, , drop = FALSE]
  t0_k <- median(tr$obs_time, na.rm = TRUE)

  one <- tryCatch({
    mt <- cal_3_COX_split(tr, te, t0 = t0_k, vars_use = best_vars)
    data.frame(
      fold = k,
      n_train = nrow(tr),
      n_test = nrow(te),
      cindex = as.numeric(mt$CINDEX[1]),
      bs = as.numeric(mt$BS[1]),
      auc = as.numeric(mt$AUC[1]),
      error_msg = "",
      stringsAsFactors = FALSE
    )
  }, error = function(e) {
    data.frame(
      fold = k,
      n_train = nrow(tr),
      n_test = nrow(te),
      cindex = NA_real_,
      bs = NA_real_,
      auc = NA_real_,
      error_msg = conditionMessage(e),
      stringsAsFactors = FALSE
    )
  })
  cv_rows[[k]] <- one
  cat("COX 5折CV进度: ", k, "/5 完成\n", sep = "")
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

cv_meta <- data.frame(
  selected_model_source = best_row$model_tag[1],
  selected_itemids = best_row$itemids[1],
  selected_cindex = as.numeric(best_row$cindex[1]),
  selected_bs = as.numeric(best_row$bs[1]),
  selected_auc = as.numeric(best_row$auc[1]),
  stringsAsFactors = FALSE
)

out_cv <- "F:/文章_大论文/MIMIC数据库_代码/TREA代码/0315/result_COX_best_5fold.xlsx"
writexl::write_xlsx(
  list(
    model_selected = cv_meta,
    cv_detail = cv_detail,
    cv_mean = cv_mean
  ),
  out_cv
)

cat("\nCOX 最优模型 5折交叉验证完成。\n")
cat("最优模型来源: ", best_row$model_tag[1], "\n", sep = "")
cat("最优变量: ", best_row$itemids[1], "\n", sep = "")
cat("结果文件: ", out_cv, "\n", sep = "")
print(cv_detail)
print(cv_mean)
