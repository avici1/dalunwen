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


###################10V尝试建模#####################

## 目标：
## - 严格基线版：排除明显后验/泄漏变量，并采用训练-测试拆分评估
## - 将候选变量按每10个一组进行RSF建模
## - 每组输出 itemids / cindex / bs / auc
## - 保存到 result_RSF_10V.xlsx

if (!requireNamespace("writexl", quietly = TRUE)) {
  stop("请先安装 writexl 包：install.packages('writexl')")
}

if (!exists("rsf_data0") || !exists("keep_pred") || !exists("cal_3_RSF_mimic")) {
  stop("请先运行上方RSF主流程，确保 rsf_data0 / keep_pred / cal_3_RSF_mimic 已生成。")
}

## 1) 严格基线变量过滤（去掉后验信息与明显标识字段）
is_leak_var <- function(v) {
  grepl("disch|outtime|icu_out|edout|los|mort|death|event|charttime|hadm|stay|subject|id$", v, ignore.case = TRUE)
}
pred_strict <- keep_pred[!vapply(keep_pred, is_leak_var, logical(1))]
if (length(pred_strict) == 0L) stop("严格基线过滤后无可用变量。")

## 2) 按患者做训练-测试拆分（7:3，分层event）
set.seed(123)
idx_event1 <- which(rsf_data0$event == 1)
idx_event0 <- which(rsf_data0$event == 0)
n_tr1 <- max(1, floor(length(idx_event1) * 0.7))
n_tr0 <- max(1, floor(length(idx_event0) * 0.7))
tr_idx <- c(
  if (length(idx_event1) > 0) sample(idx_event1, n_tr1) else integer(0),
  if (length(idx_event0) > 0) sample(idx_event0, n_tr0) else integer(0)
)
tr_idx <- sort(unique(tr_idx))
te_idx <- setdiff(seq_len(nrow(rsf_data0)), tr_idx)
if (length(te_idx) < 30) stop("测试集样本过少，请检查数据拆分。")

train_df <- rsf_data0[tr_idx, , drop = FALSE]
test_df  <- rsf_data0[te_idx, , drop = FALSE]
t0_rsf_10v <- median(train_df$obs_time, na.rm = TRUE)

## 3) 评估函数：在训练集建模，测试集评估
cal_3_RSF_strict_split <- function(train_data, test_data, t0, vars_use) {
  td <- train_data[, c(vars_use, "obs_time", "event"), drop = FALSE]
  vd <- test_data[, c(vars_use, "obs_time", "event"), drop = FALSE]

  clean_predictors <- function(df) {
    pred_cols <- setdiff(names(df), c("obs_time", "event"))
    out <- list()
    for (v in pred_cols) {
      x <- df[[v]]
      if (is.list(x)) next
      if (inherits(x, "POSIXt") || inherits(x, "Date")) {
        x <- as.numeric(x)
      } else if (is.logical(x)) {
        x <- as.numeric(x)
      } else if (is.character(x)) {
        x <- trimws(x); x[x == ""] <- NA_character_
        x <- as.factor(x)
      } else if (!(is.numeric(x) || is.integer(x) || is.factor(x))) {
        x <- suppressWarnings(as.numeric(x))
      }
      if (sum(!is.na(x)) == 0) next
      if (length(unique(x[!is.na(x)])) <= 1) next
      out[[v]] <- x
    }
    out
  }

  train_pred <- clean_predictors(td)
  if (length(train_pred) == 0L) stop("训练集清洗后无可用预测变量。")

  ## 测试集变量按训练集同名字段对齐
  use_cols <- names(train_pred)
  test_pred <- list()
  for (v in use_cols) {
    x <- vd[[v]]
    if (is.character(x)) {
      x <- trimws(x); x[x == ""] <- NA_character_
      x <- as.factor(x)
    } else if (inherits(x, "POSIXt") || inherits(x, "Date")) {
      x <- as.numeric(x)
    } else if (is.logical(x)) {
      x <- as.numeric(x)
    } else if (!(is.numeric(x) || is.integer(x) || is.factor(x))) {
      x <- suppressWarnings(as.numeric(x))
    }
    test_pred[[v]] <- x
  }

  tr <- data.frame(train_pred, obs_time = train_data$obs_time, event = train_data$event, check.names = FALSE)
  te <- data.frame(test_pred,  obs_time = test_data$obs_time,  event = test_data$event,  check.names = FALSE)
  names(tr) <- make.names(names(tr), unique = TRUE)
  names(te) <- make.names(names(te), unique = TRUE)

  ## 再对齐一次列名，保证训练/测试一致
  common_cols <- intersect(names(tr), names(te))
  common_cols <- setdiff(common_cols, c("obs_time", "event"))
  if (length(common_cols) == 0L) stop("训练/测试无共同可用预测变量。")
  tr <- tr[, c(common_cols, "obs_time", "event"), drop = FALSE]
  te <- te[, c(common_cols, "obs_time", "event"), drop = FALSE]

  mtry_val <- max(1, floor(length(common_cols) / 3))
  fit <- randomForestSRC::rfsrc(
    formula = Surv(obs_time, event) ~ .,
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
    if (is.matrix(pred$survival)) {
      survival_probs <- as.numeric(pred$survival[, t_idx])
    } else {
      survival_probs <- as.numeric(pred$survival)
    }
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
    risk_marker <- -risk_marker
    CINDEX <- as.numeric(survival::concordance(survival::Surv(surv_time, surv_status) ~ risk_marker)$concordance)
  }

  Y_obs <- as.numeric(surv_time > t0 | (surv_time <= t0 & surv_status == 0))
  censoring_model <- survival::survfit(survival::Surv(obs_time, 1 - event) ~ 1, data = te)
  get_weights <- function(time, event, censoring_model, t0) {
    cens_probs <- summary(censoring_model, times = pmin(time, t0))$surv
    if (length(cens_probs) != length(time)) cens_probs <- rep(summary(censoring_model, times = t0)$surv, length(time))
    if (is.null(cens_probs) || length(cens_probs) == 0) cens_probs <- rep(1, length(time))
    ifelse(time <= t0 & event == 1, 1 / pmax(cens_probs, 1e-6),
           ifelse(time > t0, 1 / pmax(summary(censoring_model, times = t0)$surv, 1e-6), 0))
  }
  weights <- get_weights(surv_time, surv_status, censoring_model, t0)
  BS <- mean(weights * (survival_probs - Y_obs)^2, na.rm = TRUE)

  data.frame(AUC = round(AUC, 4), CINDEX = round(CINDEX, 4), BS = round(BS, 4))
}

batch_size_10v <- 10L
batches_10v <- split(pred_strict, ceiling(seq_along(pred_strict) / batch_size_10v))

res_10v <- vector("list", length(batches_10v))
for (i in seq_along(batches_10v)) {
  vars_i <- batches_10v[[i]]
  one <- tryCatch({
    mt_i <- cal_3_RSF_strict_split(train_df, test_df, t0 = t0_rsf_10v, vars_use = vars_i)
    data.frame(
      itemids = paste(vars_i, collapse = ";"),
      cindex = as.numeric(mt_i$CINDEX[1]),
      bs = as.numeric(mt_i$BS[1]),
      auc = as.numeric(mt_i$AUC[1]),
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
  cat("RSF 10V进度: ", i, "/", length(batches_10v), " 完成\n", sep = "")
}

result_rsf_10v <- dplyr::bind_rows(res_10v)
out_rsf_10v <- "F:/文章_大论文/MIMIC数据库_代码/TREA代码/0314/result_RSF_10V.xlsx"
writexl::write_xlsx(result_rsf_10v, out_rsf_10v)

cat("\nRSF 10V批量建模结束。\n")
cat("严格基线候选变量数: ", length(pred_strict), "\n", sep = "")
cat("训练集样本: ", nrow(train_df), "；测试集样本: ", nrow(test_df), "\n", sep = "")
cat("结果文件: ", out_rsf_10v, "\n", sep = "")
print(utils::head(result_rsf_10v, 10))






############4V###############

## 严格基线版4V：
## - 固定包含 age + gender
## - 另外从其余严格基线变量中选2个，组成4V模型
## - 训练集建模，测试集评估
## - 输出 itemids / cindex / bs / auc 到 result_RSF_4V.xlsx

## 独立运行支持：若关键对象不存在，则在4V区块内自动构建
if (!exists("rsf_data0") || !exists("keep_pred")) {
  rsf_dir_4v <- "F:/文章_大论文/MIMIC数据库_代码/TREA代码/0314"
  rsf_path_candidates_4v <- c(
    file.path(rsf_dir_4v, "widedata_merge.xlsx"),
    file.path(rsf_dir_4v, "0314_widedata_merge.xlsx"),
    file.path(rsf_dir_4v, "widedata_merge1.xlsx")
  )
  rsf_path_4v <- rsf_path_candidates_4v[file.exists(rsf_path_candidates_4v)][1]
  if (is.na(rsf_path_4v)) stop("4V区块未找到可用主数据文件。")

  base_path_4v <- "F:/文章_大论文/MIMIC数据库_代码/TREA代码/0311/stroke_baseline_simple.xlsx"
  if (!file.exists(base_path_4v)) stop("4V区块未找到 baseline 文件。")

  rsf_raw_4v <- readxl::read_xlsx(rsf_path_4v)
  base_dat_4v <- readxl::read_xlsx(base_path_4v) %>%
    dplyr::select(hadm_id, hospital_mortality) %>%
    dplyr::distinct(hadm_id, .keep_all = TRUE) %>%
    dplyr::rename(hospitalmortality = hospital_mortality)
  if ("hadm_id" %in% names(rsf_raw_4v)) {
    rsf_raw_4v <- rsf_raw_4v %>%
      dplyr::mutate(hadm_id = as.character(hadm_id)) %>%
      dplyr::left_join(base_dat_4v %>% dplyr::mutate(hadm_id = as.character(hadm_id)), by = "hadm_id")
  }

  id_col_4v <- if ("subject_id" %in% names(rsf_raw_4v)) "subject_id" else if ("subjectid" %in% names(rsf_raw_4v)) "subjectid" else stop("4V区块未找到subject_id/subjectid。")
  to_numeric_safe_4v <- function(x) {
    if (is.list(x)) x <- unlist(x, use.names = FALSE)
    suppressWarnings(as.numeric(x))
  }
  rsf_raw_4v$Obstimes <- to_numeric_safe_4v(rsf_raw_4v$Obstimes)
  rsf_raw_4v$itemid_los_hosp_days <- to_numeric_safe_4v(rsf_raw_4v$itemid_los_hosp_days)
  rsf_raw_4v$hospitalmortality <- to_numeric_safe_4v(rsf_raw_4v$hospitalmortality)

  rsf_data0 <- rsf_raw_4v %>%
    dplyr::filter(Obstimes %in% c(0, 1)) %>%
    dplyr::mutate(.id = as.character(.data[[id_col_4v]])) %>%
    dplyr::arrange(.id, dplyr::desc(Obstimes)) %>%
    dplyr::distinct(.id, .keep_all = TRUE) %>%
    dplyr::mutate(
      ID = as.numeric(as.factor(.id)),
      obs_time = itemid_los_hosp_days,
      event = ifelse(hospitalmortality > 0, 1, 0)
    ) %>%
    dplyr::filter(is.finite(obs_time), !is.na(obs_time), obs_time > 0, !is.na(event))

  exclude_vars_4v <- c(
    ".id", "ID", id_col_4v, "subject_id", "subjectid", "hadm_id", "stay_id",
    "charttime", "Obstimes", "obs_time", "event", "hospitalmortality",
    "itemid_los_hosp_days", "itemid_hadm_id_stroke", "itemid_hadm_id_icu"
  )
  pred_vars_4v <- setdiff(names(rsf_data0), exclude_vars_4v)
  keep_pred <- pred_vars_4v[vapply(pred_vars_4v, function(v) {
    x <- rsf_data0[[v]]
    sum(!is.na(x)) > 0 && length(unique(x[!is.na(x)])) > 1
  }, logical(1))]
}

is_leak_var_4v <- function(v) {
  grepl("disch|outtime|icu_out|edout|los|mort|death|event|charttime|hadm|stay|subject|id$", v, ignore.case = TRUE)
}
pred_strict <- keep_pred[!vapply(keep_pred, is_leak_var_4v, logical(1))]
if (length(pred_strict) == 0L) stop("4V区块严格基线过滤后无可用变量。")

set.seed(123)
idx_event1_4v <- which(rsf_data0$event == 1)
idx_event0_4v <- which(rsf_data0$event == 0)
n_tr1_4v <- max(1, floor(length(idx_event1_4v) * 0.7))
n_tr0_4v <- max(1, floor(length(idx_event0_4v) * 0.7))
tr_idx_4v <- c(
  if (length(idx_event1_4v) > 0) sample(idx_event1_4v, n_tr1_4v) else integer(0),
  if (length(idx_event0_4v) > 0) sample(idx_event0_4v, n_tr0_4v) else integer(0)
)
tr_idx_4v <- sort(unique(tr_idx_4v))
te_idx_4v <- setdiff(seq_len(nrow(rsf_data0)), tr_idx_4v)
if (length(te_idx_4v) < 30) stop("4V区块测试集样本过少。")
train_df <- rsf_data0[tr_idx_4v, , drop = FALSE]
test_df <- rsf_data0[te_idx_4v, , drop = FALSE]
t0_rsf_10v <- median(train_df$obs_time, na.rm = TRUE)

if (!exists("cal_3_RSF_strict_split")) {
  cal_3_RSF_strict_split <- function(train_data, test_data, t0, vars_use) {
    td <- train_data[, c(vars_use, "obs_time", "event"), drop = FALSE]
    vd <- test_data[, c(vars_use, "obs_time", "event"), drop = FALSE]

    clean_predictors <- function(df) {
      pred_cols <- setdiff(names(df), c("obs_time", "event"))
      out <- list()
      for (v in pred_cols) {
        x <- df[[v]]
        if (is.list(x)) next
        if (inherits(x, "POSIXt") || inherits(x, "Date")) x <- as.numeric(x)
        else if (is.logical(x)) x <- as.numeric(x)
        else if (is.character(x)) {
          x <- trimws(x); x[x == ""] <- NA_character_
          x <- as.factor(x)
        } else if (!(is.numeric(x) || is.integer(x) || is.factor(x))) x <- suppressWarnings(as.numeric(x))
        if (sum(!is.na(x)) == 0) next
        if (length(unique(x[!is.na(x)])) <= 1) next
        out[[v]] <- x
      }
      out
    }

    train_pred <- clean_predictors(td)
    if (length(train_pred) == 0L) stop("4V区块训练集清洗后无可用预测变量。")
    use_cols <- names(train_pred)

    test_pred <- list()
    for (v in use_cols) {
      x <- vd[[v]]
      if (is.character(x)) {
        x <- trimws(x); x[x == ""] <- NA_character_
        x <- as.factor(x)
      } else if (inherits(x, "POSIXt") || inherits(x, "Date")) x <- as.numeric(x)
      else if (is.logical(x)) x <- as.numeric(x)
      else if (!(is.numeric(x) || is.integer(x) || is.factor(x))) x <- suppressWarnings(as.numeric(x))
      test_pred[[v]] <- x
    }

    tr <- data.frame(train_pred, obs_time = train_data$obs_time, event = train_data$event, check.names = FALSE)
    te <- data.frame(test_pred, obs_time = test_data$obs_time, event = test_data$event, check.names = FALSE)
    names(tr) <- make.names(names(tr), unique = TRUE)
    names(te) <- make.names(names(te), unique = TRUE)

    common_cols <- intersect(names(tr), names(te))
    common_cols <- setdiff(common_cols, c("obs_time", "event"))
    if (length(common_cols) == 0L) stop("4V区块训练/测试无共同可用预测变量。")
    tr <- tr[, c(common_cols, "obs_time", "event"), drop = FALSE]
    te <- te[, c(common_cols, "obs_time", "event"), drop = FALSE]

    mtry_val <- max(1, floor(length(common_cols) / 3))
    fit <- randomForestSRC::rfsrc(
      formula = survival::Surv(obs_time, event) ~ .,
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
      if (is.matrix(pred$survival)) survival_probs <- as.numeric(pred$survival[, t_idx]) else survival_probs <- as.numeric(pred$survival)
      if (length(survival_probs) == n_obs) risk_marker <- 1 - survival_probs
    }
    if (is.null(risk_marker) && !is.null(pred$predicted) && length(pred$predicted) == n_obs) {
      risk_marker <- as.numeric(pred$predicted)
      survival_probs <- pmax(0, pmin(1, 1 - risk_marker))
    }
    if (is.null(risk_marker) || length(risk_marker) != n_obs) stop("4V区块测试集风险分数提取失败。")

    surv_time <- te$obs_time
    surv_status <- te$event
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
    censoring_model <- survival::survfit(survival::Surv(obs_time, 1 - event) ~ 1, data = te)
    get_weights <- function(time, event, censoring_model, t0) {
      cens_probs <- summary(censoring_model, times = pmin(time, t0))$surv
      if (length(cens_probs) != length(time)) cens_probs <- rep(summary(censoring_model, times = t0)$surv, length(time))
      if (is.null(cens_probs) || length(cens_probs) == 0) cens_probs <- rep(1, length(time))
      ifelse(time <= t0 & event == 1, 1 / pmax(cens_probs, 1e-6),
             ifelse(time > t0, 1 / pmax(summary(censoring_model, times = t0)$surv, 1e-6), 0))
    }
    weights <- get_weights(surv_time, surv_status, censoring_model, t0)
    BS <- mean(weights * (survival_probs - Y_obs)^2, na.rm = TRUE)
    data.frame(AUC = round(AUC, 4), CINDEX = round(CINDEX, 4), BS = round(BS, 4))
  }
}

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

pair_mat_4v <- utils::combn(extra_vars_4v, 2)
## 避免组合过多导致运行时间过长
max_pairs_4v <- 300L
if (ncol(pair_mat_4v) > max_pairs_4v) {
  pair_mat_4v <- pair_mat_4v[, seq_len(max_pairs_4v), drop = FALSE]
  cat("提示：4V组合过多，仅尝试前 ", max_pairs_4v, " 组。\n", sep = "")
}

res_4v <- vector("list", ncol(pair_mat_4v))
for (i in seq_len(ncol(pair_mat_4v))) {
  vars_i <- c(age_var_4v, gender_var_4v, pair_mat_4v[, i])
  one <- tryCatch({
    mt_i <- cal_3_RSF_strict_split(train_df, test_df, t0 = t0_rsf_10v, vars_use = vars_i)
    data.frame(
      itemids = paste(vars_i, collapse = ";"),
      cindex = as.numeric(mt_i$CINDEX[1]),
      bs = as.numeric(mt_i$BS[1]),
      auc = as.numeric(mt_i$AUC[1]),
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
  cat("RSF 4V进度: ", i, "/", ncol(pair_mat_4v), " 完成\n", sep = "")
}

result_rsf_4v <- dplyr::bind_rows(res_4v)
out_rsf_4v <- "F:/文章_大论文/MIMIC数据库_代码/TREA代码/0314/result_RSF_4V.xlsx"
writexl::write_xlsx(result_rsf_4v, out_rsf_4v)

cat("\nRSF 4V批量建模结束。\n")
cat("固定变量: ", age_var_4v, " + ", gender_var_4v, "\n", sep = "")
cat("结果文件: ", out_rsf_4v, "\n", sep = "")
print(utils::head(result_rsf_4v, 10))


