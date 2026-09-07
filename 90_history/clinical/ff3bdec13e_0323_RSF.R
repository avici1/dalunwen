## ==========================================
## RSF 建模 - 22变量全纳入，5折交叉验证
## 参考：0314_RSF建模.R
## 数据：0323_DATA.xlsx
## 调参：每折训练集内 20 次随机搜索（mtry / ntree / nodesize），
##       以按患者 20% 验证集 C-index 选优；患者数过少(<8)时改为全训练集 OOB C-index 选优
## 输出：0323_RSF_5fold_results.xlsx（各折明细、折间均值、折间 min / max，便于箱线图须线）
## ==========================================
suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(survival)
  library(timeROC)
  library(randomForestSRC)
  library(writexl)
})

set.seed(123)
options(scipen = 999)

# RSF 随机搜索：迭代次数与搜索空间（每折训练集内）
N_RSF_RANDOM_SEARCH <- 20L
RSF_NTREE_CAND <- c(200L, 300L, 400L, 500L, 600L, 800L, 1000L)
RSF_NODESIZE_CAND <- c(3L, 5L, 7L, 10L, 15L, 20L, 30L)
RSF_VAL_FRAC <- 0.2
RSF_MIN_PATIENTS_VAL <- 8L

# =========================
# 路径
# =========================
work_dir <- "F:/文章_大论文/MIMIC数据库_代码/TREA代码/0323实例比较"
data_path <- file.path(work_dir, "0323_DATA.xlsx")
if (!dir.exists(work_dir)) dir.create(work_dir, recursive = TRUE)

# =========================
# 1) 读取数据并 5 折划分（内嵌，无中间文件）
# =========================
message("读取数据: ", data_path)
if (!file.exists(data_path)) stop("未找到 0323_DATA.xlsx")
dat_raw <- readxl::read_xlsx(data_path)
message("数据维度: ", nrow(dat_raw), " 行 × ", ncol(dat_raw), " 列")

event_col_cv <- NULL
for (cand in c("event", "death", "hospitalmortality", "hospital_mortality")) {
  if (cand %in% names(dat_raw)) { event_col_cv <- cand; break }
}

id_col <- NULL
for (cand in c("subject_id", "subjectid", "id")) {
  if (cand %in% names(dat_raw)) { id_col <- cand; break }
}
if (is.null(id_col)) stop("未找到个体 ID 列（subject_id/subjectid/id）")

# 按个体划分：每个患者一个 fold，同一患者所有行归入同一折
ids_all <- unique(dat_raw[[id_col]])
patient_event <- dat_raw %>%
  dplyr::group_by(!!sym(id_col)) %>%
  dplyr::summarise(event = as.numeric(first(.data[[event_col_cv]])), .groups = "drop")
patient_event$event <- ifelse(is.na(patient_event$event) | patient_event$event > 0, 1, 0)

K <- 5L
id_e1 <- patient_event[[id_col]][patient_event$event == 1]
id_e0 <- patient_event[[id_col]][patient_event$event == 0]
fold_vec <- rep(NA_integer_, length(ids_all))
if (length(id_e1) > 0L)
  fold_vec[match(id_e1, ids_all)] <- sample(rep(1L:K, length.out = length(id_e1)))
if (length(id_e0) > 0L)
  fold_vec[match(id_e0, ids_all)] <- sample(rep(1L:K, length.out = length(id_e0)))

fold_df <- data.frame(id = ids_all, fold = fold_vec, stringsAsFactors = FALSE)
fold_df <- fold_df[!is.na(fold_df$fold), , drop = FALSE]
message("已按个体（患者）分层划分为 5 折，共 ", nrow(fold_df), " 个个体")

# =========================
# 2) 数据准备：合并 fold 到 dat，仅保留有有效 fold 的个体
# =========================
fold_df_join <- fold_df
names(fold_df_join)[names(fold_df_join) == "id"] <- id_col
dat <- dat_raw %>%
  dplyr::inner_join(fold_df_join, by = id_col) %>%
  as.data.frame(stringsAsFactors = FALSE)

# 生存时间与事件列
to_numeric_safe <- function(x) {
  if (is.list(x)) x <- unlist(x, use.names = FALSE)
  suppressWarnings(as.numeric(x))
}

surv_time_col <- NULL
event_col <- NULL
for (cand in c("itemid_los_hosp_days", "los_hosp_days", "obs_time", "los", "surv_time")) {
  if (cand %in% names(dat)) { surv_time_col <- cand; break }
}
for (cand in c("event", "death", "hospitalmortality", "hospital_mortality")) {
  if (cand %in% names(dat)) { event_col <- cand; break }
}
if (is.null(surv_time_col) || is.null(event_col)) {
  stop("未找到生存时间或事件列，请检查 0323_DATA 中是否含 itemid_los_hosp_days/obs_time 与 event/hospitalmortality")
}

dat$obs_time <- to_numeric_safe(dat[[surv_time_col]])
dat$event <- ifelse(to_numeric_safe(dat[[event_col]]) > 0, 1, 0)

if (!("fold" %in% names(dat))) stop("dat 中无 fold 列。")

# =========================
# 22 个预测变量：排除 id/时间/结局/fold 等
# =========================
exclude_vars <- c(
  "subject_id", "subjectid", "hadm_id", "stay_id", "charttime",
  "Obstimes", "obs_time", "event", "fold",
  "hospitalmortality", "hospital_mortality",
  "itemid_los_hosp_days", "los_hosp_days", "itemid_hadm_id_stroke", "itemid_hadm_id_icu",
  "deathtime", "death"
)
pred_vars <- setdiff(names(dat), exclude_vars)
# 只保留有变异且非全缺失的变量
keep_pred <- pred_vars[vapply(pred_vars, function(v) {
  x <- dat[[v]]
  sum(!is.na(x)) > 0 && length(unique(x[!is.na(x)])) > 1
}, logical(1))]

# 若超过 22 个，取前 22 个（按列顺序）；若不足则全部使用
n_vars_target <- 22L
if (length(keep_pred) > n_vars_target) {
  keep_pred <- keep_pred[seq_len(n_vars_target)]
  message("预测变量数超过22，取前22个: ", paste(keep_pred, collapse = ", "))
} else {
  message("使用 ", length(keep_pred), " 个预测变量: ", paste(keep_pred, collapse = ", "))
}
if (length(keep_pred) == 0L) stop("无可用预测变量，请检查数据。")

# =========================
# 辅助函数：给定风险分数与生存数据，计算 AUC、CINDEX、BS
# =========================
calc_metrics <- function(surv_time, surv_status, risk_marker, survival_probs, t0, df_for_cens) {
  roc1 <- tryCatch(timeROC::timeROC(T = surv_time, delta = surv_status, marker = risk_marker, cause = 1, times = t0), error = function(e) NULL)
  roc2 <- tryCatch(timeROC::timeROC(T = surv_time, delta = surv_status, marker = -risk_marker, cause = 1, times = t0), error = function(e) NULL)
  auc1 <- if (!is.null(roc1) && !is.null(roc1$AUC)) { aucv <- as.numeric(roc1$AUC); if (length(aucv) >= 2) aucv[2] else aucv[1] } else NA_real_
  auc2 <- if (!is.null(roc2) && !is.null(roc2$AUC)) { aucv <- as.numeric(roc2$AUC); if (length(aucv) >= 2) aucv[2] else aucv[1] } else NA_real_
  AUC <- max(c(auc1, auc2), na.rm = TRUE)
  if (!is.finite(AUC)) AUC <- NA_real_

  CINDEX <- tryCatch(as.numeric(survival::concordance(survival::Surv(surv_time, surv_status) ~ risk_marker)$concordance), error = function(e) NA_real_)
  if (!is.na(CINDEX) && CINDEX < 0.5) {
    CINDEX <- tryCatch(as.numeric(survival::concordance(survival::Surv(surv_time, surv_status) ~ I(-risk_marker))$concordance), error = function(e) NA_real_)
  }

  Y_obs <- as.numeric(surv_time > t0 | (surv_time <= t0 & surv_status == 0))
  censoring_model <- tryCatch(survival::survfit(survival::Surv(obs_time, 1 - event) ~ 1, data = df_for_cens), error = function(e) NULL)
  if (!is.null(censoring_model)) {
    get_weights <- function(time, event, censoring_model, t0) {
      cens_probs <- tryCatch(summary(censoring_model, times = pmin(time, t0))$surv, error = function(e) NULL)
      if (is.null(cens_probs) || length(cens_probs) != length(time))
        cens_probs <- tryCatch(rep(summary(censoring_model, times = t0)$surv, length(time)), error = function(e) rep(1, length(time)))
      if (is.null(cens_probs) || length(cens_probs) == 0) cens_probs <- rep(1, length(time))
      ifelse(time <= t0 & event == 1, 1 / pmax(cens_probs, 1e-6),
             ifelse(time > t0, 1 / pmax(summary(censoring_model, times = t0)$surv, 1e-6), 0))
    }
    weights <- get_weights(surv_time, surv_status, censoring_model, t0)
    BS <- mean(weights * (survival_probs - Y_obs)^2, na.rm = TRUE)
  } else {
    BS <- mean((survival_probs - Y_obs)^2, na.rm = TRUE)
  }
  if (!is.finite(BS) || is.na(BS)) BS <- NA_real_
  list(AUC = round(AUC, 4), CINDEX = round(CINDEX, 4), BS = round(BS, 4))
}

# =========================
# 从 rfsrc 拟合或 predict 结果提取与 t0 对齐的风险分数（长度须为 n）
# =========================
extract_risk_at_t0 <- function(fit_or_pred, t0, n) {
  risk <- NULL
  surv_probs <- NULL
  s <- fit_or_pred$survival
  ti <- fit_or_pred$time.interest
  if (!is.null(s) && !is.null(ti) && length(ti) > 0) {
    t_idx <- which.min(abs(ti - t0))
    surv_probs <- if (is.matrix(s)) as.numeric(s[, t_idx]) else as.numeric(s)
    if (length(surv_probs) == n) risk <- 1 - surv_probs
  }
  if (is.null(risk) && !is.null(fit_or_pred$predicted) && length(fit_or_pred$predicted) == n) {
    risk <- as.numeric(fit_or_pred$predicted)
    surv_probs <- pmax(0, pmin(1, 1 - risk))
  }
  list(risk = risk, surv_probs = surv_probs)
}

# =========================
# 训练集建模、训练集/测试集评估函数（含随机搜索调参）
# =========================
cal_rsf_cv_fold <- function(train_data, test_data, t0, vars_use, id_col, fold_idx) {
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
    xtr <- z$tr
    xte <- z$te
    med <- suppressWarnings(median(xtr, na.rm = TRUE))
    if (!is.finite(med)) med <- 0
    xtr[is.na(xtr)] <- med
    xte[is.na(xte)] <- med
    s <- stats::sd(xtr, na.rm = TRUE)
    if (is.finite(s) && s > 1e-8) {
      m <- mean(xtr, na.rm = TRUE)
      xtr <- (xtr - m) / s
      xte <- (xte - m) / s
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

  if (length(unique(tr$event)) < 2L) stop("训练集 event 仅单一取值。")
  if (length(unique(te$event)) < 2L) stop("测试集 event 仅单一取值。")

  pred_cols <- setdiff(names(tr), c("obs_time", "event"))
  p <- length(pred_cols)
  default_mtry <- max(1L, floor(p / 3))
  default_ntree <- 500L
  default_nodesize <- 10L

  # ---------- 20 次随机搜索：验证集或 OOB 上最大化 C-index ----------
  set.seed(5000L + as.integer(fold_idx) * 97L)
  pid_tr <- as.character(train_data[[id_col]])
  uids <- unique(pid_tr)
  n_pat <- length(uids)

  idx_row_tr <- seq_len(nrow(tr))
  tune_mode <- "val_holdout"
  idx_fit <- idx_row_tr
  idx_val <- integer(0)

  if (n_pat >= RSF_MIN_PATIENTS_VAL) {
    evt_by_id <- tapply(as.integer(train_data$event), as.character(train_data[[id_col]]), function(x) max(x, na.rm = TRUE))
    id1 <- names(evt_by_id)[evt_by_id == 1L & !is.na(evt_by_id)]
    id0 <- names(evt_by_id)[evt_by_id == 0L & !is.na(evt_by_id)]
    n_v1 <- max(1L, min(length(id1), ceiling(length(id1) * RSF_VAL_FRAC)))
    n_v0 <- max(1L, min(length(id0), ceiling(length(id0) * RSF_VAL_FRAC)))
    if (length(id1) > 0L && length(id0) > 0L) {
      val_ids <- union(sample(id1, n_v1), sample(id0, n_v0))
    } else {
      val_ids <- sample(uids, max(1L, ceiling(n_pat * RSF_VAL_FRAC)))
    }
    train_tune_ids <- setdiff(uids, val_ids)
    if (length(train_tune_ids) < 1L || length(val_ids) < 1L) {
      tune_mode <- "oob"
    } else {
      idx_fit <- idx_row_tr[pid_tr %in% train_tune_ids]
      idx_val <- idx_row_tr[pid_tr %in% val_ids]
      if (length(idx_fit) < 5L || length(idx_val) < 3L ||
          length(unique(tr$event[idx_val])) < 2L || length(unique(tr$event[idx_fit])) < 2L) {
        tune_mode <- "oob"
      }
    }
  } else {
    tune_mode <- "oob"
  }

  if (tune_mode == "oob") {
    idx_fit <- idx_row_tr
    idx_val <- integer(0)
  }

  tr_tune <- tr[idx_fit, , drop = FALSE]
  tr_val <- if (length(idx_val) > 0L) tr[idx_val, , drop = FALSE] else NULL

  best_c <- -Inf
  best_mtry <- default_mtry
  best_ntree <- default_ntree
  best_nodesize <- default_nodesize

  score_cindex <- function(obs_t, ev, rk) {
    if (is.null(rk) || length(rk) != length(obs_t)) return(-Inf)
    if (length(unique(ev)) < 2L) return(-Inf)
    cc <- tryCatch(
      as.numeric(survival::concordance(survival::Surv(obs_t, ev) ~ rk)$concordance),
      error = function(e) NA_real_
    )
    if (!is.finite(cc) || is.na(cc)) return(-Inf)
    if (cc < 0.5) {
      cc2 <- tryCatch(
        as.numeric(survival::concordance(survival::Surv(obs_t, ev) ~ I(-rk))$concordance),
        error = function(e) NA_real_
      )
      if (is.finite(cc2) && !is.na(cc2)) cc <- cc2
    }
    cc
  }

  for (it in seq_len(N_RSF_RANDOM_SEARCH)) {
    mtry_it <- sample.int(p, 1L)
    ntree_it <- sample(RSF_NTREE_CAND, 1L)
    nodesize_it <- sample(RSF_NODESIZE_CAND, 1L)
    fit_it <- tryCatch(
      randomForestSRC::rfsrc(
        formula = survival::Surv(obs_time, event) ~ .,
        data = tr_tune,
        ntree = ntree_it,
        mtry = mtry_it,
        nodesize = nodesize_it,
        importance = FALSE,
        proximity = FALSE,
        na.action = "na.impute",
        seed = 123L + as.integer(fold_idx) * 1000L + as.integer(it)
      ),
      error = function(e) NULL
    )
    if (is.null(fit_it)) next

    if (tune_mode == "val_holdout" && !is.null(tr_val) && nrow(tr_val) >= 3L) {
      pr <- tryCatch(predict(fit_it, newdata = tr_val), error = function(e) NULL)
      if (is.null(pr)) next
      ex <- extract_risk_at_t0(pr, t0, nrow(tr_val))
      c_it <- score_cindex(tr_val$obs_time, tr_val$event, ex$risk)
    } else {
      rk <- NULL
      if (!is.null(fit_it$predicted.oob) && length(fit_it$predicted.oob) == nrow(tr_tune)) {
        rk <- as.numeric(fit_it$predicted.oob)
      }
      if (is.null(rk)) {
        ex <- extract_risk_at_t0(fit_it, t0, nrow(tr_tune))
        rk <- ex$risk
      }
      c_it <- score_cindex(tr_tune$obs_time, tr_tune$event, rk)
    }

    if (is.finite(c_it) && c_it > best_c) {
      best_c <- c_it
      best_mtry <- mtry_it
      best_ntree <- ntree_it
      best_nodesize <- nodesize_it
    }
  }

  if (!is.finite(best_c) || best_c <= -Inf) {
    best_mtry <- default_mtry
    best_ntree <- default_ntree
    best_nodesize <- default_nodesize
    best_c <- NA_real_
  }

  fit <- randomForestSRC::rfsrc(
    formula = survival::Surv(obs_time, event) ~ .,
    data = tr,
    ntree = best_ntree,
    mtry = best_mtry,
    nodesize = best_nodesize,
    importance = FALSE,
    proximity = FALSE,
    na.action = "na.impute",
    seed = 123L + as.integer(fold_idx)
  )

  n_tr <- nrow(tr)
  n_te <- nrow(te)

  # 训练集预测（fit 内含训练集预测）
  risk_tr <- NULL
  surv_probs_tr <- NULL
  if (!is.null(fit$survival) && !is.null(fit$time.interest) && length(fit$time.interest) > 0) {
    t_idx <- which.min(abs(fit$time.interest - t0))
    surv_probs_tr <- if (is.matrix(fit$survival)) as.numeric(fit$survival[, t_idx]) else as.numeric(fit$survival)
    if (length(surv_probs_tr) == n_tr) risk_tr <- 1 - surv_probs_tr
  }
  if (is.null(risk_tr) && !is.null(fit$predicted) && length(fit$predicted) == n_tr) {
    risk_tr <- as.numeric(fit$predicted)
    surv_probs_tr <- pmax(0, pmin(1, 1 - risk_tr))
  }
  if (is.null(risk_tr) || length(risk_tr) != n_tr) risk_tr <- rep(NA_real_, n_tr)
  if (is.null(surv_probs_tr) || length(surv_probs_tr) != n_tr) surv_probs_tr <- rep(NA_real_, n_tr)

  # 测试集预测
  pred <- predict(fit, newdata = te)
  risk_te <- NULL
  surv_probs_te <- NULL
  if (!is.null(pred$survival) && !is.null(pred$time.interest) && length(pred$time.interest) > 0) {
    t_idx <- which.min(abs(pred$time.interest - t0))
    surv_probs_te <- if (is.matrix(pred$survival)) as.numeric(pred$survival[, t_idx]) else as.numeric(pred$survival)
    if (length(surv_probs_te) == n_te) risk_te <- 1 - surv_probs_te
  }
  if (is.null(risk_te) && !is.null(pred$predicted) && length(pred$predicted) == n_te) {
    risk_te <- as.numeric(pred$predicted)
    surv_probs_te <- pmax(0, pmin(1, 1 - risk_te))
  }
  if (is.null(risk_te) || length(risk_te) != n_te) stop("测试集风险分数提取失败。")

  mt_tr <- calc_metrics(tr$obs_time, tr$event, risk_tr, surv_probs_tr, t0, tr)
  mt_te <- calc_metrics(te$obs_time, te$event, risk_te, surv_probs_te, t0, te)

  data.frame(
    train_AUC = mt_tr$AUC, train_CINDEX = mt_tr$CINDEX, train_BS = mt_tr$BS,
    test_AUC = mt_te$AUC, test_CINDEX = mt_te$CINDEX, test_BS = mt_te$BS,
    diff_AUC = mt_tr$AUC - mt_te$AUC,
    diff_CINDEX = mt_tr$CINDEX - mt_te$CINDEX,
    diff_BS = mt_te$BS - mt_tr$BS,
    tune_mtry = as.integer(best_mtry),
    tune_ntree = as.integer(best_ntree),
    tune_nodesize = as.integer(best_nodesize),
    tune_score_C = round(as.numeric(best_c), 4),
    tune_mode = tune_mode,
    stringsAsFactors = FALSE
  )
}

# =========================
# 5 折交叉验证
# =========================
K <- 5L
cv_results <- vector("list", K)

for (k in 1L:K) {
  # 按个体划分：同一患者所有行归入训练集或测试集
  tr_ids <- fold_df$id[fold_df$fold != k]
  te_ids <- fold_df$id[fold_df$fold == k]
  tr_dat <- dat[dat[[id_col]] %in% tr_ids, , drop = FALSE]
  te_dat <- dat[dat[[id_col]] %in% te_ids, , drop = FALSE]
  t0_k <- median(tr_dat$obs_time, na.rm = TRUE)

  one <- tryCatch({
    mt <- cal_rsf_cv_fold(tr_dat, te_dat, t0 = t0_k, vars_use = keep_pred, id_col = id_col, fold_idx = k)
    data.frame(
      fold = k,
      n_train = length(tr_ids),
      n_test = length(te_ids),
      train_AUC = as.numeric(mt$train_AUC[1]),
      train_CINDEX = as.numeric(mt$train_CINDEX[1]),
      train_BS = as.numeric(mt$train_BS[1]),
      test_AUC = as.numeric(mt$test_AUC[1]),
      test_CINDEX = as.numeric(mt$test_CINDEX[1]),
      test_BS = as.numeric(mt$test_BS[1]),
      diff_AUC = as.numeric(mt$diff_AUC[1]),
      diff_CINDEX = as.numeric(mt$diff_CINDEX[1]),
      diff_BS = as.numeric(mt$diff_BS[1]),
      tune_mtry = as.integer(mt$tune_mtry[1]),
      tune_ntree = as.integer(mt$tune_ntree[1]),
      tune_nodesize = as.integer(mt$tune_nodesize[1]),
      tune_score_C = as.numeric(mt$tune_score_C[1]),
      tune_mode = as.character(mt$tune_mode[1]),
      error_msg = "",
      stringsAsFactors = FALSE
    )
  }, error = function(e) {
    data.frame(
      fold = k,
      n_train = length(tr_ids),
      n_test = length(te_ids),
      train_AUC = NA_real_, train_CINDEX = NA_real_, train_BS = NA_real_,
      test_AUC = NA_real_, test_CINDEX = NA_real_, test_BS = NA_real_,
      diff_AUC = NA_real_, diff_CINDEX = NA_real_, diff_BS = NA_real_,
      tune_mtry = NA_integer_, tune_ntree = NA_integer_, tune_nodesize = NA_integer_,
      tune_score_C = NA_real_, tune_mode = NA_character_,
      error_msg = conditionMessage(e),
      stringsAsFactors = FALSE
    )
  })
  cv_results[[k]] <- one
  message("5折 CV 进度: ", k, "/5 完成")
}

cv_detail <- dplyr::bind_rows(cv_results)
cv_mean <- cv_detail %>%
  dplyr::summarise(
    fold = "mean",
    n_train = round(mean(n_train, na.rm = TRUE), 0),
    n_test = round(mean(n_test, na.rm = TRUE), 0),
    train_AUC = round(mean(train_AUC, na.rm = TRUE), 4),
    train_CINDEX = round(mean(train_CINDEX, na.rm = TRUE), 4),
    train_BS = round(mean(train_BS, na.rm = TRUE), 4),
    test_AUC = round(mean(test_AUC, na.rm = TRUE), 4),
    test_CINDEX = round(mean(test_CINDEX, na.rm = TRUE), 4),
    test_BS = round(mean(test_BS, na.rm = TRUE), 4),
    diff_AUC = round(mean(diff_AUC, na.rm = TRUE), 4),
    diff_CINDEX = round(mean(diff_CINDEX, na.rm = TRUE), 4),
    diff_BS = round(mean(diff_BS, na.rm = TRUE), 4),
    tune_mtry = round(mean(tune_mtry, na.rm = TRUE), 2),
    tune_ntree = round(mean(tune_ntree, na.rm = TRUE), 0),
    tune_nodesize = round(mean(tune_nodesize, na.rm = TRUE), 2),
    tune_score_C = round(mean(tune_score_C, na.rm = TRUE), 4),
    tune_mode = paste(unique(na.omit(tune_mode)), collapse = ";"),
    error_msg = "",
    .groups = "drop"
  )

# =========================
# 输出与保存
# =========================
out_xlsx <- file.path(work_dir, "0323_RSF_5fold_results.xlsx")
writexl::write_xlsx(
  list(
    cv_detail = cv_detail,
    cv_mean = cv_mean
  ),
  out_xlsx
)

cat("\n========== RSF 5折交叉验证完成 ==========\n")
cat("预测变量数: ", length(keep_pred), "\n", sep = "")
cat("输出文件: ", out_xlsx, "\n\n", sep = "")
cat("各折结果 (训练集/测试集/差值):\n")
print(cv_detail)
cat("\n均值:\n")
print(cv_mean)
