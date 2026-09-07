## ==========================================
## 0428 实例数据分析：RSF + RSFLC 五折交叉验证
## 数据：data_0423_wide_inputed（宽表）
## 变量：全部 itemid* 变量进入模型
## 输出：每折 test_CINDEX/test_AUC/test_BS + 均值
## ==========================================

suppressPackageStartupMessages({
  library(dplyr)
  library(survival)
  library(timeROC)
  library(randomForestSRC)
  library(DynForest)
  library(writexl)
})

set.seed(123)
options(scipen = 999)


######### 1 读取数据#######

data_0423_wide_inputed <- read.csv(
  "F:/文章_大论文/0417/stroke_patients_inputed_0428_1.csv",
  stringsAsFactors = FALSE
)
dat_raw <- as.data.frame(data_0423_wide_inputed, stringsAsFactors = FALSE)

######### 2 数据准备#######

to_numeric_safe <- function(x) {
  if (is.list(x)) x <- unlist(x, use.names = FALSE)
  suppressWarnings(as.numeric(x))
}

# 固定字段映射（按当前数据结构）
names(dat_raw) <- trimws(names(dat_raw))
id_col <- "subject_id"
event_col <- "survival"
surv_time_col <- "survival_time_days"

# 基础清洗
dat <- dat_raw
names(dat) <- trimws(names(dat))

dat$obs_time <- to_numeric_safe(dat[[surv_time_col]])
dat$event <- ifelse(to_numeric_safe(dat[[event_col]]) > 0, 1L, 0L)
# 为了避免把某一类事件整体过滤掉：只过滤非数值时间，不再过滤 <=0 时间
dat <- dat[is.finite(dat$obs_time) & !is.na(dat$event), , drop = FALSE]
# RSF/Surv 要求时间为正，非正时间压到极小正数，保留样本用于debug与建模
dat$obs_time[dat$obs_time <= 0] <- 1e-6

# 仅纳入 item* 变量
pred_vars <- grep("^item", names(dat), value = TRUE)
pred_vars <- setdiff(pred_vars, c(event_col, "obs_time", "event", id_col, surv_time_col))

# 去除全缺失/无变异变量
keep_pred <- pred_vars[vapply(pred_vars, function(v) {
  x <- dat[[v]]
  sum(!is.na(x)) > 0 && length(unique(x[!is.na(x)])) > 1
}, logical(1))]

# 按患者分层划分5折：同一患者全部记录进入同一折
patient_event <- dat %>%
  dplyr::group_by(.data[[id_col]]) %>%
  dplyr::summarise(event = as.integer(max(event, na.rm = TRUE)), .groups = "drop")

K <- 5L
set.seed(123)
fold_df <- data.frame(id = patient_event[[id_col]], fold = NA_integer_, stringsAsFactors = FALSE)
id_e1 <- fold_df$id[patient_event$event == 1L]
id_e0 <- fold_df$id[patient_event$event == 0L]
if (length(id_e1) > 0L) fold_df$fold[match(id_e1, fold_df$id)] <- sample(rep(1L:K, length.out = length(id_e1)))
if (length(id_e0) > 0L) fold_df$fold[match(id_e0, fold_df$id)] <- sample(rep(1L:K, length.out = length(id_e0)))

######### 3 载入程序#######

# 与 0323_RSF.R 一致：随机搜索调参设置
N_RSF_RANDOM_SEARCH <- 20L
RSF_NTREE_CAND <- c(200L, 300L, 400L, 500L, 600L, 800L, 1000L)
RSF_NODESIZE_CAND <- c(3L, 5L, 7L, 10L, 15L, 20L, 30L)
RSF_VAL_FRAC <- 0.2
RSF_MIN_PATIENTS_VAL <- 8L

# 与 0323_RSF.R 一致：AUC/CINDEX/BS 计算
calc_metrics <- function(surv_time, surv_status, risk_marker, survival_probs, t0, df_for_cens) {
  roc1 <- timeROC::timeROC(T = surv_time, delta = surv_status, marker = risk_marker, cause = 1, times = t0)
  roc2 <- timeROC::timeROC(T = surv_time, delta = surv_status, marker = -risk_marker, cause = 1, times = t0)
  auc1 <- { aucv <- as.numeric(roc1$AUC); if (length(aucv) >= 2) aucv[2] else aucv[1] }
  auc2 <- { aucv <- as.numeric(roc2$AUC); if (length(aucv) >= 2) aucv[2] else aucv[1] }
  AUC <- max(c(auc1, auc2), na.rm = TRUE)

  CINDEX <- as.numeric(survival::concordance(survival::Surv(surv_time, surv_status) ~ risk_marker)$concordance)
  if (!is.na(CINDEX) && CINDEX < 0.5) {
    CINDEX <- as.numeric(survival::concordance(survival::Surv(surv_time, surv_status) ~ I(-risk_marker))$concordance)
  }

  Y_obs <- as.numeric(surv_time > t0 | (surv_time <= t0 & surv_status == 0))
  censoring_model <- survival::survfit(survival::Surv(obs_time, 1 - event) ~ 1, data = df_for_cens)
  get_weights <- function(time, event, censoring_model, t0) {
    cens_probs <- summary(censoring_model, times = pmin(time, t0))$surv
    ifelse(time <= t0 & event == 1, 1 / pmax(cens_probs, 1e-6),
           ifelse(time > t0, 1 / pmax(summary(censoring_model, times = t0)$surv, 1e-6), 0))
  }
  weights <- get_weights(surv_time, surv_status, censoring_model, t0)
  BS <- mean(weights * (survival_probs - Y_obs)^2, na.rm = TRUE)
  list(AUC = round(AUC, 4), CINDEX = round(CINDEX, 4), BS = round(BS, 4))
}

# 与 0323_RSF.R 一致：从 rfsrc 结果提取 t0 风险
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

build_xy <- function(train_data, test_data, vars_use) {
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
  if (length(keep_mat) == 0L) stop("训练集特征无有效变异。")

  tr <- as.data.frame(lapply(keep_mat, function(x) x$tr), check.names = FALSE)
  te <- as.data.frame(lapply(keep_mat, function(x) x$te), check.names = FALSE)
  tr$obs_time <- train_data$obs_time
  tr$event <- as.integer(train_data$event)
  te$obs_time <- test_data$obs_time
  te$event <- as.integer(test_data$event)
  names(tr) <- make.names(names(tr), unique = TRUE)
  names(te) <- make.names(names(te), unique = TRUE)
  list(tr = tr, te = te)
}

# 与 0323_RSF.R 一致：每折调参 + 建模 + 训练/测试评估
cal_rsf_cv_fold <- function(train_data, test_data, t0, vars_use, id_col, fold_idx) {
  xy <- build_xy(train_data, test_data, vars_use)
  tr <- xy$tr
  te <- xy$te

  if (length(unique(tr$event)) < 2L) stop("训练集 event 仅单一取值。")
  if (length(unique(te$event)) < 2L) stop("测试集 event 仅单一取值。")

  pred_cols <- setdiff(names(tr), c("obs_time", "event"))
  p <- length(pred_cols)
  default_mtry <- max(1L, floor(p / 3))
  default_ntree <- 500L
  default_nodesize <- 10L

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
    cc <- as.numeric(survival::concordance(survival::Surv(obs_t, ev) ~ rk)$concordance)
    if (cc < 0.5) {
      cc2 <- as.numeric(survival::concordance(survival::Surv(obs_t, ev) ~ I(-rk))$concordance)
      cc <- cc2
    }
    cc
  }

  rsf_formula <- as.formula(paste0("Surv(obs_time, event) ~ ", paste(pred_cols, collapse = " + ")))

  for (it in seq_len(N_RSF_RANDOM_SEARCH)) {
    mtry_it <- sample.int(p, 1L)
    ntree_it <- sample(RSF_NTREE_CAND, 1L)
    nodesize_it <- sample(RSF_NODESIZE_CAND, 1L)
    fit_it <- randomForestSRC::rfsrc(
      formula = rsf_formula,
      data = tr_tune,
      ntree = ntree_it,
      mtry = mtry_it,
      nodesize = nodesize_it,
      importance = FALSE,
      proximity = FALSE,
      na.action = "na.impute",
      seed = 123L + as.integer(fold_idx) * 1000L + as.integer(it)
    )

    if (tune_mode == "val_holdout" && !is.null(tr_val) && nrow(tr_val) >= 3L) {
      pr <- predict(fit_it, newdata = tr_val)
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

  fit <- randomForestSRC::rfsrc(
    formula = rsf_formula,
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
  ex_tr <- extract_risk_at_t0(fit, t0, n_tr)
  pred <- predict(fit, newdata = te)
  ex_te <- extract_risk_at_t0(pred, t0, n_te)
  if (is.null(ex_te$risk) || length(ex_te$risk) != n_te) stop("测试集风险分数提取失败。")

  mt_tr <- calc_metrics(tr$obs_time, tr$event, ex_tr$risk, ex_tr$surv_probs, t0, tr)
  mt_te <- calc_metrics(te$obs_time, te$event, ex_te$risk, ex_te$surv_probs, t0, te)

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

##### 4 交叉验证 #########

cv_results <- vector("list", K)

for (k in 1L:K) {
  tr_ids <- fold_df$id[fold_df$fold != k]
  te_ids <- fold_df$id[fold_df$fold == k]
  tr_dat <- dat[dat[[id_col]] %in% tr_ids, , drop = FALSE]
  te_dat <- dat[dat[[id_col]] %in% te_ids, , drop = FALSE]
  t0_k <- median(tr_dat$obs_time, na.rm = TRUE)

  mt <- cal_rsf_cv_fold(tr_dat, te_dat, t0 = t0_k, vars_use = keep_pred, id_col = id_col, fold_idx = k)
  one <- data.frame(
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
    stringsAsFactors = FALSE
  )

  cv_results[[k]] <- one
  message("5折CV进度: ", k, "/", K)
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
    .groups = "drop"
  )

###### 5  输出与保存#####

out_dir <- "F:/文章_大论文/MIMIC数据库_代码/TREA代码"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

out_xlsx <- file.path(out_dir, "0428_RSF_5fold_results.xlsx")
writexl::write_xlsx(
  list(
    cv_detail = cv_detail,
    cv_mean = cv_mean
  ),
  out_xlsx
)

cat("\n========== RSF 5折交叉验证完成 ==========\n")
cat("可用样本行数: ", nrow(dat), "\n", sep = "")
cat("预测变量数(itemid*): ", length(keep_pred), "\n", sep = "")
cat("输出文件: ", out_xlsx, "\n\n", sep = "")
cat("各折结果 (训练集/测试集/差值):\n")
print(cv_detail)
cat("\n均值:\n")
print(cv_mean)
 



