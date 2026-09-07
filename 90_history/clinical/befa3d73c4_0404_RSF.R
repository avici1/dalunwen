  library(readxl)
  library(dplyr)
  library(survival)
  library(timeROC)
  library(randomForestSRC)


set.seed(123)


work_dir <- "F:/文章_大论文/MIMIC数据库_代码/TREA代码/0404新设计"
fold_xlsx <- file.path(work_dir, "DATA", "0407_DATA_fold1.xlsx")
if (!file.exists(fold_xlsx)) stop("未找到 DATA/0407_DATA_fold1.xlsx")

raw <- readxl::read_excel(fold_xlsx)
fold_col <- if ("fold1" %in% names(raw)) "fold1" else if ("fold_1" %in% names(raw)) "fold_1" else stop("数据中缺少 fold1 或 fold_1 列")
fv <- suppressWarnings(as.integer(round(as.numeric(raw[[fold_col]]))))
# data_train / data_vali 由下方外层 5 折循环按 fold1 逐折划分







############iter_search(data_train, data_vali) 3######################

iter_search <- function(data_train, data_vali) {
  N_RSF_RANDOM_SEARCH <- 20L
  RSF_NTREE_CAND <- c(50L, 100L, 150L, 200L, 250L, 300L, 400L)
  RSF_NODESIZE_CAND <- c(3L, 5L, 7L, 10L, 15L, 20L, 30L)
  fold_drop <- c("fold", "fold1", "fold_1", "fold2", "fold3", "fold4", "fold5", "fold6")

  dat_train <- dplyr::select(
    as.data.frame(data_train, stringsAsFactors = FALSE),
    -dplyr::any_of(fold_drop)
  ) %>%
    as.data.frame(stringsAsFactors = FALSE)

  dat_vali <- dplyr::select(
    as.data.frame(data_vali, stringsAsFactors = FALSE),
    -dplyr::any_of(fold_drop)
  ) %>%
    as.data.frame(stringsAsFactors = FALSE)

  for (nm in c("los_hosp_days", "death")) {
    if (nm %in% names(dat_train) && is.character(dat_train[[nm]])) {
      dat_train[[nm]] <- suppressWarnings(as.numeric(dat_train[[nm]]))
    }
    if (nm %in% names(dat_vali) && is.character(dat_vali[[nm]])) {
      dat_vali[[nm]] <- suppressWarnings(as.numeric(dat_vali[[nm]]))
    }
  }
  dat_train[] <- lapply(dat_train, function(x) if (is.character(x)) factor(x) else x)
  dat_vali[] <- lapply(dat_vali, function(x) if (is.character(x)) factor(x) else x)

  for (nm in names(dat_vali)) {
    if (is.factor(dat_vali[[nm]]) && nm %in% names(dat_train) && is.factor(dat_train[[nm]])) {
      dat_vali[[nm]] <- factor(as.character(dat_vali[[nm]]), levels = levels(dat_train[[nm]]))
    }
  }

  p_pred <- ncol(dat_train) - 2L
  if (p_pred < 1L) stop("预测变量数为 0，无法调参。")

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

  calc_metrics_vali <- function(surv_time, surv_status, risk_marker, survival_probs, t0, df_for_cens) {
    roc_v <- tryCatch(
      timeROC::timeROC(T = surv_time, delta = surv_status, marker = risk_marker, cause = 1, times = t0),
      error = function(e) NULL
    )
    AUC <- NA_real_
    if (!is.null(roc_v) && !is.null(roc_v$AUC)) {
      aucv <- as.numeric(roc_v$AUC)
      AUC <- if (length(aucv) >= 2L) aucv[[2L]] else aucv[[1L]]
    }
    if (!is.finite(AUC)) AUC <- NA_real_

    CINDEX <- tryCatch(
      as.numeric(survival::concordance(survival::Surv(surv_time, surv_status) ~ risk_marker)$concordance),
      error = function(e) NA_real_
    )
    if (!is.na(CINDEX) && CINDEX < 0.5) {
      CINDEX <- tryCatch(
        as.numeric(survival::concordance(survival::Surv(surv_time, surv_status) ~ I(-risk_marker))$concordance),
        error = function(e) NA_real_
      )
    }

    Y_obs <- as.numeric(surv_time > t0 | (surv_time <= t0 & surv_status == 0))
    censoring_model <- tryCatch(
      survival::survfit(survival::Surv(obs_time, 1 - event) ~ 1, data = df_for_cens),
      error = function(e) NULL
    )
    if (!is.null(censoring_model)) {
      get_weights <- function(time, event, censoring_model, t0) {
        cens_probs <- tryCatch(summary(censoring_model, times = pmin(time, t0))$surv, error = function(e) NULL)
        if (is.null(cens_probs) || length(cens_probs) != length(time)) {
          cens_probs <- tryCatch(
            rep(summary(censoring_model, times = t0)$surv, length(time)),
            error = function(e) rep(1, length(time))
          )
        }
        if (is.null(cens_probs) || length(cens_probs) == 0) cens_probs <- rep(1, length(time))
        ifelse(
          time <= t0 & event == 1, 1 / pmax(cens_probs, 1e-6),
          ifelse(time > t0, 1 / pmax(summary(censoring_model, times = t0)$surv, 1e-6), 0)
        )
      }
      weights <- get_weights(surv_time, surv_status, censoring_model, t0)
      BS <- mean(weights * (survival_probs - Y_obs)^2, na.rm = TRUE)
    } else {
      BS <- mean((survival_probs - Y_obs)^2, na.rm = TRUE)
    }
    if (!is.finite(BS) || is.na(BS)) BS <- NA_real_
    list(AUC = AUC, CINDEX = CINDEX, BS = BS)
  }

  # 验证集 DCA：固定时间 t0 下，以 risk≈t0 前事件概率 与 是否观察到 t0 内死亡 做二分类决策曲线
  calc_dca_vali <- function(surv_time, event, risk, t0, thresholds) {
    p_hat <- pmax(0, pmin(1, as.numeric(risk)))
    y_evt <- as.integer(surv_time <= t0 & as.integer(event) == 1L)
    n <- length(y_evt)
    if (n < 1L) {
      return(list(mean_nb = NA_real_, auc_nb = NA_real_, nb_by_pt = NA_real_))
    }
    nb_by_pt <- vapply(thresholds, function(pt) {
      if (!(is.finite(pt) && pt > 0 && pt < 1)) return(NA_real_)
      pred_hi <- p_hat >= pt
      tp <- sum(pred_hi & y_evt == 1L)
      fp <- sum(pred_hi & y_evt == 0L)
      (tp / n) - (fp / n) * (pt / (1 - pt))
    }, numeric(1))
    fin <- is.finite(nb_by_pt)
    mean_nb <- if (any(fin)) mean(nb_by_pt[fin]) else NA_real_
    auc_nb <- if (sum(fin) >= 2L) {
      th <- thresholds[fin]
      nb <- nb_by_pt[fin]
      ord <- order(th)
      th <- th[ord]
      nb <- nb[ord]
      sum(diff(th) * (head(nb, -1L) + tail(nb, -1L)) / 2)
    } else NA_real_
    list(mean_nb = mean_nb, auc_nb = auc_nb, nb_by_pt = nb_by_pt)
  }

  # 评估时刻 t0 仅用训练集，避免用验证集分布定 t0 造成信息泄露
  t0_train <- stats::median(dat_train[["los_hosp_days"]], na.rm = TRUE)
  death_v <- as.integer(dat_vali[["death"]])
  df_cens_v <- data.frame(obs_time = dat_vali[["los_hosp_days"]], event = death_v)
  dca_thresholds <- seq(0.05, 0.95, by = 0.1)

  set.seed(5000L)
  best_tier <- 0L
  best_val <- -Inf
  best_ntree <- 500L
  best_mtry <- max(1L, min(p_pred, as.integer(ceiling(sqrt(p_pred)))))
  best_nodesize <- 10L
  tune_rsf_log <- vector("list", N_RSF_RANDOM_SEARCH)
  vali_metrics_rows <- vector("list", N_RSF_RANDOM_SEARCH)
  t_run <- proc.time()

  for (it in seq_len(N_RSF_RANDOM_SEARCH)) {
    ntree_it <- sample(RSF_NTREE_CAND, 1L)
    nodesize_it <- sample(RSF_NODESIZE_CAND, 1L)
    mtry_it <- sample.int(p_pred, 1L)

    fit_it <- tryCatch(
      randomForestSRC::rfsrc(
        as.formula("Surv(los_hosp_days, death) ~ . - los_hosp_days - death"),
        data = dat_train,
        ntree = ntree_it,
        mtry = mtry_it,
        nodesize = nodesize_it,
        importance = FALSE,
        proximity = FALSE,
        na.action = "na.impute",
        seed = 7000L + it
      ),
      error = function(e) NULL
    )

    elapsed <- unname((proc.time() - t_run)["elapsed"])
    stamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    n_v <- nrow(dat_vali)

    if (is.null(fit_it)) {
      message(
        stamp, " | 进度 ", it, "/", N_RSF_RANDOM_SEARCH,
        " | 训练拟合失败 | 累计用时 ", round(elapsed, 1), " s"
      )
      tune_rsf_log[[it]] <- data.frame(
        iter = it,
        ntree = ntree_it,
        mtry = mtry_it,
        nodesize = nodesize_it,
        ok = FALSE,
        stringsAsFactors = FALSE
      )
      vali_metrics_rows[[it]] <- data.frame(
        iter = it,
        ntree = ntree_it,
        mtry = mtry_it,
        nodesize = nodesize_it,
        cindex = NA_real_,
        brier = NA_real_,
        auc = NA_real_,
        dca_mean_nb = NA_real_,
        dca_auc_nb = NA_real_,
        t0 = t0_train,
        ok = FALSE,
        stringsAsFactors = FALSE
      )
      next
    }

    pred_v <- tryCatch(
      randomForestSRC::predict.rfsrc(fit_it, newdata = dat_vali, na.action = "na.impute"),
      error = function(e) NULL
    )

    ex_v <- if (!is.null(pred_v)) extract_risk_at_t0(pred_v, t0_train, n_v) else list(risk = NULL, surv_probs = NULL)
    metrics_ok <- !is.null(ex_v$risk) && length(ex_v$risk) == n_v &&
      !is.null(ex_v$surv_probs) && length(ex_v$surv_probs) == n_v && !anyNA(ex_v$surv_probs)

    cix <- NA_real_
    bs_v <- NA_real_
    auc_v <- NA_real_
    dmn <- NA_real_
    dau <- NA_real_
    if (metrics_ok) {
      mt <- calc_metrics_vali(
        dat_vali[["los_hosp_days"]],
        death_v,
        ex_v$risk,
        ex_v$surv_probs,
        t0_train,
        df_cens_v
      )
      cix <- mt$CINDEX
      bs_v <- mt$BS
      auc_v <- mt$AUC
      dca_r <- calc_dca_vali(
        dat_vali[["los_hosp_days"]],
        death_v,
        ex_v$risk,
        t0_train,
        dca_thresholds
      )
      dmn <- dca_r$mean_nb
      dau <- dca_r$auc_nb
    }

    message(
      stamp, " | 进度 ", it, "/", N_RSF_RANDOM_SEARCH,
      " | 训练完成，验证集指标已算 | 累计用时 ", round(elapsed, 1), " s"
    )

    tune_rsf_log[[it]] <- data.frame(
      iter = it,
      ntree = ntree_it,
      mtry = mtry_it,
      nodesize = nodesize_it,
      ok = TRUE,
      stringsAsFactors = FALSE
    )
    vali_metrics_rows[[it]] <- data.frame(
      iter = it,
      ntree = ntree_it,
      mtry = mtry_it,
      nodesize = nodesize_it,
      cindex = cix,
      brier = bs_v,
      auc = auc_v,
      dca_mean_nb = dmn,
      dca_auc_nb = dau,
      t0 = t0_train,
      ok = metrics_ok,
      stringsAsFactors = FALSE
    )

    if (metrics_ok) {
      tier_it <- 0L
      val_it <- -Inf
      if (is.finite(dau) && !is.na(dau)) {
        tier_it <- 3L
        val_it <- dau
      } else if (is.finite(dmn) && !is.na(dmn)) {
        tier_it <- 2L
        val_it <- dmn
      } else if (is.finite(cix) && !is.na(cix)) {
        tier_it <- 1L
        val_it <- cix
      }
      if (tier_it > 0L && (best_tier == 0L || tier_it > best_tier || (tier_it == best_tier && val_it > best_val))) {
        best_tier <- tier_it
        best_val <- val_it
        best_ntree <- ntree_it
        best_mtry <- mtry_it
        best_nodesize <- nodesize_it
      }
    }
  }

  tune_rsf_results <- do.call(rbind, tune_rsf_log)
  rownames(tune_rsf_results) <- NULL
  vali_tune_metrics <- do.call(rbind, vali_metrics_rows)
  rownames(vali_tune_metrics) <- NULL

  if (best_tier == 0L) {
    message("验证集 DCA / C-index 均未有效，退回默认超参数。")
  }

  t_final <- proc.time()
  fit <- randomForestSRC::rfsrc(
    as.formula("Surv(los_hosp_days, death) ~ . - los_hosp_days - death"),
    data = dat_train,
    ntree = best_ntree,
    mtry = best_mtry,
    nodesize = best_nodesize,
    importance = FALSE,
    proximity = FALSE,
    na.action = "na.impute",
    seed = 123L
  )
  message(
    format(Sys.time(), "%Y-%m-%d %H:%M:%S"), " | 最终模型拟合完成 | 本次用时 ",
    round(unname((proc.time() - t_final)["elapsed"]), 1), " s"
  )

  result <- data.frame(
    ntree = best_ntree,
    mtry = best_mtry,
    nodesize = best_nodesize,
    stringsAsFactors = FALSE
  )
  attr(result, "tune_rsf_results") <- tune_rsf_results
  attr(result, "t0_train") <- t0_train
  attr(result, "dca_thresholds") <- dca_thresholds
  attr(result, "tune_best_tier") <- best_tier
  attr(result, "tune_best_score") <- if (best_tier > 0L) best_val else NA_real_

  crit_lab <- switch(
    as.character(best_tier),
    "3" = "dca_auc_nb",
    "2" = "dca_mean_nb",
    "1" = "cindex",
    "0" = "无（默认超参）",
    "无（默认超参）"
  )
  message(
    "RSF 随机搜索结束。选参依据（分层）：dca_auc_nb > dca_mean_nb > cindex。",
    " 本次为 ", crit_lab, " | 最优 ntree=", best_ntree, ", mtry=", best_mtry, ", nodesize=", best_nodesize
  )

  attr(vali_tune_metrics, "dca_note") <- paste0(
    "DCA：t0 为训练集 los_hosp_days 中位数（验证集评估亦用该 t0，避免泄露）；y=1 为 t0 内观察到死亡；",
    "risk 为 t0 时刻预测事件概率；阈值 ", paste(format(dca_thresholds, digits = 2), collapse = ", ")
  )

  list(fit, result, vali_tune_metrics)
}

###########################

####################
# 外层 5 折嵌套 CV：轮番 hold out fold1=k（k=1..5）；开发集内按 0407 数据的 fold(7-k)（k=5→fold2…k=1→fold6）做 A/B/C 三折 iter_search，
# 汇总超参后 modeling(全开发集, 外层测试折)。外层测试折不参与内层选参。

######## modeling：固定 RSF 超参，训练 + 测试集评估（与 iter_search 同源预处理与指标）########
modeling <- function(ntree, mtry, nodesize, data_train, data_test) {
  fold_drop <- c("fold", "fold1", "fold_1", "fold2", "fold3", "fold4", "fold5", "fold6")
  dat_train <- dplyr::select(
    as.data.frame(data_train, stringsAsFactors = FALSE),
    -dplyr::any_of(fold_drop)
  ) %>%
    as.data.frame(stringsAsFactors = FALSE)
  dat_test <- dplyr::select(
    as.data.frame(data_test, stringsAsFactors = FALSE),
    -dplyr::any_of(fold_drop)
  ) %>%
    as.data.frame(stringsAsFactors = FALSE)

  for (nm in c("los_hosp_days", "death")) {
    if (nm %in% names(dat_train) && is.character(dat_train[[nm]])) {
      dat_train[[nm]] <- suppressWarnings(as.numeric(dat_train[[nm]]))
    }
    if (nm %in% names(dat_test) && is.character(dat_test[[nm]])) {
      dat_test[[nm]] <- suppressWarnings(as.numeric(dat_test[[nm]]))
    }
  }
  dat_train[] <- lapply(dat_train, function(x) if (is.character(x)) factor(x) else x)
  dat_test[] <- lapply(dat_test, function(x) if (is.character(x)) factor(x) else x)
  for (nm in names(dat_test)) {
    if (is.factor(dat_test[[nm]]) && nm %in% names(dat_train) && is.factor(dat_train[[nm]])) {
      dat_test[[nm]] <- factor(as.character(dat_test[[nm]]), levels = levels(dat_train[[nm]]))
    }
  }

  p_pred <- ncol(dat_train) - 2L
  if (p_pred < 1L) stop("modeling：预测变量数为 0。")

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

  calc_metrics_test <- function(surv_time, surv_status, risk_marker, survival_probs, t0, df_for_cens) {
    roc_v <- tryCatch(
      timeROC::timeROC(T = surv_time, delta = surv_status, marker = risk_marker, cause = 1, times = t0),
      error = function(e) NULL
    )
    AUC <- NA_real_
    if (!is.null(roc_v) && !is.null(roc_v$AUC)) {
      aucv <- as.numeric(roc_v$AUC)
      AUC <- if (length(aucv) >= 2L) aucv[[2L]] else aucv[[1L]]
    }
    if (!is.finite(AUC)) AUC <- NA_real_

    CINDEX <- tryCatch(
      as.numeric(survival::concordance(survival::Surv(surv_time, surv_status) ~ risk_marker)$concordance),
      error = function(e) NA_real_
    )
    if (!is.na(CINDEX) && CINDEX < 0.5) {
      CINDEX <- tryCatch(
        as.numeric(survival::concordance(survival::Surv(surv_time, surv_status) ~ I(-risk_marker))$concordance),
        error = function(e) NA_real_
      )
    }

    Y_obs <- as.numeric(surv_time > t0 | (surv_time <= t0 & surv_status == 0))
    censoring_model <- tryCatch(
      survival::survfit(survival::Surv(obs_time, 1 - event) ~ 1, data = df_for_cens),
      error = function(e) NULL
    )
    if (!is.null(censoring_model)) {
      get_weights <- function(time, event, censoring_model, t0) {
        cens_probs <- tryCatch(summary(censoring_model, times = pmin(time, t0))$surv, error = function(e) NULL)
        if (is.null(cens_probs) || length(cens_probs) != length(time)) {
          cens_probs <- tryCatch(
            rep(summary(censoring_model, times = t0)$surv, length(time)),
            error = function(e) rep(1, length(time))
          )
        }
        if (is.null(cens_probs) || length(cens_probs) == 0) cens_probs <- rep(1, length(time))
        ifelse(
          time <= t0 & event == 1, 1 / pmax(cens_probs, 1e-6),
          ifelse(time > t0, 1 / pmax(summary(censoring_model, times = t0)$surv, 1e-6), 0)
        )
      }
      weights <- get_weights(surv_time, surv_status, censoring_model, t0)
      BS <- mean(weights * (survival_probs - Y_obs)^2, na.rm = TRUE)
    } else {
      BS <- mean((survival_probs - Y_obs)^2, na.rm = TRUE)
    }
    if (!is.finite(BS) || is.na(BS)) BS <- NA_real_
    list(AUC = AUC, CINDEX = CINDEX, BS = BS)
  }

  t0_train <- stats::median(dat_train[["los_hosp_days"]], na.rm = TRUE)
  death_te <- as.integer(dat_test[["death"]])
  df_cens_te <- data.frame(obs_time = dat_test[["los_hosp_days"]], event = death_te)
  n_te <- nrow(dat_test)

  fit <- tryCatch(
    randomForestSRC::rfsrc(
      as.formula("Surv(los_hosp_days, death) ~ . - los_hosp_days - death"),
      data = dat_train,
      ntree = as.integer(ntree),
      mtry = as.integer(mtry),
      nodesize = as.integer(nodesize),
      importance = FALSE,
      proximity = FALSE,
      na.action = "na.impute",
      seed = 123L
    ),
    error = function(e) NULL
  )
  if (is.null(fit)) stop("modeling：训练集拟合失败。")

  pred_te <- tryCatch(
    randomForestSRC::predict.rfsrc(fit, newdata = dat_test, na.action = "na.impute"),
    error = function(e) NULL
  )
  ex_te <- if (!is.null(pred_te)) extract_risk_at_t0(pred_te, t0_train, n_te) else list(risk = NULL, surv_probs = NULL)
  ok <- !is.null(ex_te$risk) && length(ex_te$risk) == n_te &&
    !is.null(ex_te$surv_probs) && length(ex_te$surv_probs) == n_te && !anyNA(ex_te$surv_probs)

  cix <- NA_real_
  auc_v <- NA_real_
  bs_v <- NA_real_
  if (ok) {
    mt <- calc_metrics_test(
      dat_test[["los_hosp_days"]],
      death_te,
      ex_te$risk,
      ex_te$surv_probs,
      t0_train,
      df_cens_te
    )
    cix <- mt$CINDEX
    auc_v <- mt$AUC
    bs_v <- mt$BS
  }

  metrics <- data.frame(
    cindex = cix,
    auc = auc_v,
    brier = bs_v,
    stringsAsFactors = FALSE
  )
  attr(metrics, "t0") <- t0_train
  attr(metrics, "t0_note") <- "t0 为 data_train 的 los_hosp_days 中位数（与 iter_search 一致，避免用测试集定 t0）"
  attr(metrics, "ntree") <- as.integer(ntree)
  attr(metrics, "mtry") <- as.integer(mtry)
  attr(metrics, "nodesize") <- as.integer(nodesize)
  attr(metrics, "metrics_ok") <- ok

  list(fit, metrics)
}

##############实现5折嵌套CV#################
outer_folds <- 1L:5L
cv_MODEL_LIST <- vector("list", length(outer_folds))
names(cv_MODEL_LIST) <- paste0("fold1_test_", outer_folds)
cv_summary_rows <- vector("list", length(outer_folds))

for (idx in seq_along(outer_folds)) {
  outer_fold <- outer_folds[[idx]]
  data_vali <- raw[!is.na(fv) & fv == outer_fold, , drop = FALSE]
  data_train <- raw[!is.na(fv) & fv != outer_fold & fv %in% 1L:5L, , drop = FALSE]

  ## 外层测试 fold1=k 时，内层 A/B/C 存在 fold(7-k) 列（k=5→fold2，4→fold3，…，1→fold6）
  inner_col <- paste0("fold", 7L - as.integer(outer_fold))

  message(
    "\n========== 外层 ", idx, "/5 | 外层测试 fold1=", outer_fold,
    " | 开发集 n=", nrow(data_train), " | 测试集 n=", nrow(data_vali),
    " | 内层列=", inner_col, " =========="
  )

  if (!inner_col %in% names(data_train)) {
    stop(
      "data_train 缺少 ", inner_col, "（内层 A/B/C）。外层 fold1=", outer_fold,
      " 应对应 0407_DATA_fold1 中的该列。"
    )
  }

  set.seed(5000L + outer_fold)

  inner_abc_chr <- as.character(data_train[[inner_col]])

  idx_vali_a <- !is.na(inner_abc_chr) & inner_abc_chr == "A"
  idx_train_a <- !is.na(inner_abc_chr) & inner_abc_chr != "A"
  d_in_tr_a <- data_train[idx_train_a, , drop = FALSE]
  d_in_va_a <- data_train[idx_vali_a, , drop = FALSE]
  message(
    "内部三折 | 外层测试=", outer_fold, " | 验证折=A | 训练 n=", nrow(d_in_tr_a), " | 验证 n=", nrow(d_in_va_a)
  )
  fold_list1 <- iter_search(d_in_tr_a, d_in_va_a)

  idx_vali_b <- !is.na(inner_abc_chr) & inner_abc_chr == "B"
  idx_train_b <- !is.na(inner_abc_chr) & inner_abc_chr != "B"
  d_in_tr_b <- data_train[idx_train_b, , drop = FALSE]
  d_in_va_b <- data_train[idx_vali_b, , drop = FALSE]
  message(
    "内部三折 | 外层测试=", outer_fold, " | 验证折=B | 训练 n=", nrow(d_in_tr_b), " | 验证 n=", nrow(d_in_va_b)
  )
  fold_list2 <- iter_search(d_in_tr_b, d_in_va_b)

  idx_vali_c <- !is.na(inner_abc_chr) & inner_abc_chr == "C"
  idx_train_c <- !is.na(inner_abc_chr) & inner_abc_chr != "C"
  d_in_tr_c <- data_train[idx_train_c, , drop = FALSE]
  d_in_va_c <- data_train[idx_vali_c, , drop = FALSE]
  message(
    "内部三折 | 外层测试=", outer_fold, " | 验证折=C | 训练 n=", nrow(d_in_tr_c), " | 验证 n=", nrow(d_in_va_c)
  )
  fold_list3 <- iter_search(d_in_tr_c, d_in_va_c)

  fl_inner <- stats::setNames(
    list(fold_list1, fold_list2, fold_list3),
    c("内折_验证=A", "内折_验证=B", "内折_验证=C")
  )
  best_j <- 1L
  best_tier_pick <- -1L
  best_score_pick <- -Inf
  for (j in seq_along(fl_inner)) {
    res_j <- fl_inner[[j]][[2]]
    ti <- attr(res_j, "tune_best_tier")
    sc <- attr(res_j, "tune_best_score")
    if (is.null(ti) || length(ti) != 1L || is.na(ti)) {
      ti <- 0L
    } else {
      ti <- as.integer(ti)
    }
    if (is.null(sc) || length(sc) != 1L || is.na(sc)) {
      sc <- -Inf
    } else {
      sc <- as.numeric(sc)
    }
    if (ti > best_tier_pick || (ti == best_tier_pick && sc > best_score_pick)) {
      best_tier_pick <- ti
      best_score_pick <- sc
      best_j <- j
    }
  }
  if (best_tier_pick == 0L) {
    max_dca <- vapply(fl_inner, function(L) {
      m <- L[[3]]
      if (!is.data.frame(m) || !"dca_auc_nb" %in% names(m)) return(-Inf)
      suppressWarnings(mx <- max(m[["dca_auc_nb"]], na.rm = TRUE))
      if (!is.finite(mx)) -Inf else mx
    }, numeric(1))
    if (any(is.finite(max_dca))) {
      best_j <- which.max(max_dca)
      message(
        "外层 ", outer_fold, "：三折 tune_best_tier 均为 0，改按 dca_auc_nb 最大选折：",
        names(fl_inner)[best_j]
      )
    }
  }
  win_nm <- names(fl_inner)[best_j]
  win_res <- fl_inner[[best_j]][[2]]
  ntree_o <- as.integer(win_res$ntree[[1L]])
  mtry_o <- as.integer(win_res$mtry[[1L]])
  nodesize_o <- as.integer(win_res$nodesize[[1L]])
  message(
    "外层 ", outer_fold, " 内三折汇总：", win_nm,
    " | 内层列=", inner_col,
    " | tier=", best_tier_pick,
    " score=", ifelse(is.finite(best_score_pick), round(best_score_pick, 6), NA),
    " | ntree=", ntree_o, " mtry=", mtry_o, " nodesize=", nodesize_o
  )

  MODEL_LIST <- modeling(ntree_o, mtry_o, nodesize_o, data_train, data_vali)
  attr(MODEL_LIST[[2]], "chosen_from_inner_fold") <- win_nm
  attr(MODEL_LIST[[2]], "inner_tune_best_tier") <- best_tier_pick
  attr(MODEL_LIST[[2]], "inner_tune_best_score") <- best_score_pick
  attr(MODEL_LIST[[2]], "outer_fold1_test") <- outer_fold
  attr(MODEL_LIST[[2]], "inner_abc_column") <- inner_col

  cv_MODEL_LIST[[idx]] <- MODEL_LIST
  m2 <- MODEL_LIST[[2]]
  cv_summary_rows[[idx]] <- data.frame(
    outer_fold1_test = outer_fold,
    inner_abc_col = inner_col,
    n_dev = nrow(data_train),
    n_test = nrow(data_vali),
    cindex = m2$cindex,
    auc = m2$auc,
    brier = m2$brier,
    ntree = ntree_o,
    mtry = mtry_o,
    nodesize = nodesize_o,
    chosen_from_inner_fold = win_nm,
    inner_tune_best_tier = best_tier_pick,
    inner_tune_best_score = ifelse(is.finite(best_score_pick), best_score_pick, NA_real_),
    stringsAsFactors = FALSE
  )

  message(
    "外层 ", outer_fold, " 测试集 | cindex=", round(m2$cindex, 4),
    " auc=", round(m2$auc, 4), " brier=", round(m2$brier, 6)
  )
}

CV_5FOLD_SUMMARY <- do.call(rbind, cv_summary_rows)
rownames(CV_5FOLD_SUMMARY) <- NULL

message("\n========== 5 折嵌套 CV 汇总表 ==========")
print(CV_5FOLD_SUMMARY)
message(
  "5 折均值（na.rm） | cindex=", round(mean(CV_5FOLD_SUMMARY$cindex, na.rm = TRUE), 4),
  " auc=", round(mean(CV_5FOLD_SUMMARY$auc, na.rm = TRUE), 4),
  " brier=", round(mean(CV_5FOLD_SUMMARY$brier, na.rm = TRUE), 6)
)

cv_out_dir <- file.path(work_dir, "DATA")
utils::write.csv(
  CV_5FOLD_SUMMARY,
  file.path(cv_out_dir, "RSF_nestedCV_5fold_summary.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)
message("已写入 ", file.path(cv_out_dir, "RSF_nestedCV_5fold_summary.csv"))

# 与原先「单折：fold1=5 为测试」对齐，便于后续交互使用
data_vali <- raw[!is.na(fv) & fv == 5L, , drop = FALSE]
data_train <- raw[!is.na(fv) & fv %in% 1L:4L, , drop = FALSE]
MODEL_LIST <- cv_MODEL_LIST[["fold1_test_5"]]

#############################

