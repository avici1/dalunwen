
work_dir <- "F:/文章_大论文/MIMIC数据库_代码/TREA代码/0404新设计"
fold_xlsx <- file.path(work_dir, "DATA", "0407_DATA_fold1.xlsx")
if (!file.exists(fold_xlsx)) stop("未找到 DATA/0407_DATA_fold1.xlsx")

raw <- readxl::read_excel(fold_xlsx)
fold_col <- if ("fold1" %in% names(raw)) "fold1" else if ("fold_1" %in% names(raw)) "fold_1" else stop("数据中缺少 fold1 或 fold_1 列")
fv <- suppressWarnings(as.integer(round(as.numeric(raw[[fold_col]]))))
# data_train / data_vali 由下方外层 5 折循环按 fold1 逐折划分

suppressPackageStartupMessages({
  library(dplyr)
  library(readxl)
  library(DynForest)
  library(survival)
  library(timeROC)
})

# 与 DynForest:::predict.dynforest（生存）内部一致：先筛 time<=t0、纵向各标记均有观测的 id、去掉 fixedData 任一为 NA 的行，再取 timeData/fixedData 的 id 交集。
# 若不预先对齐，nrow(fixedData) 会大于 predict 实际返回的 pred 行数，导致 length(risk)==n 永假，cindex/auc/brier 全为 NA。
rsflc_filter_predict_cohort <- function(timeData, fixedData, idVar = "id", timeVar = "time", t0) {
  if (!is.null(timeData) && NROW(timeData) > 0L && !is.null(t0) && is.finite(t0)) {
    timeData <- timeData[timeData[[timeVar]] <= t0, , drop = FALSE]
  }
  if (!is.null(timeData) && NROW(timeData) > 0L) {
    long_cols <- names(timeData)[!names(timeData) %in% c(idVar, timeVar)]
    if (length(long_cols) > 0L) {
      timeData_id_noNA_list <- lapply(long_cols, function(x) {
        df <- timeData[, c(idVar, x), drop = FALSE]
        names(df) <- c("ID", "var")
        stats::aggregate(var ~ ID, data = df, FUN = length)[, 1]
      })
      timeData_id_noNA <- Reduce(intersect, timeData_id_noNA_list)
      if (length(timeData_id_noNA) > 0L) {
        timeData <- timeData[timeData[[idVar]] %in% timeData_id_noNA, , drop = FALSE]
      }
    }
  }
  if (!is.null(fixedData) && nrow(fixedData) > 0L) {
    fd_narow <- which(rowSums(is.na(fixedData)) > 0L)
    if (length(fd_narow) > 0L) {
      fixedData <- fixedData[-fd_narow, , drop = FALSE]
    }
  }
  parts <- list()
  if (!is.null(timeData) && NROW(timeData) > 0L) {
    parts <- c(parts, list(unique(timeData[[idVar]])))
  }
  if (!is.null(fixedData) && nrow(fixedData) > 0L) {
    parts <- c(parts, list(unique(fixedData[[idVar]])))
  }
  if (length(parts) == 0L) {
    return(list(timeData = timeData, fixedData = fixedData))
  }
  idnoNA <- if (length(parts) == 1L) parts[[1L]] else Reduce(intersect, parts)
  if (!is.null(timeData) && NROW(timeData) > 0L) {
    timeData <- timeData[timeData[[idVar]] %in% idnoNA, , drop = FALSE]
  }
  if (!is.null(fixedData) && nrow(fixedData) > 0L) {
    fixedData <- fixedData[fixedData[[idVar]] %in% idnoNA, , drop = FALSE]
  }
  list(timeData = timeData, fixedData = fixedData)
}

############ iter_search_RSFLC：DynForest 调参（与 0404_RSF iter_search 相同 DCA 分层规则）############
# 纵向子模型写死：每个纵向变量 linear + fixed~1 + random~1+time|id；timeData/fixedData 纳入当前表内全部可用自变量（除折列与结局/时间键）

iter_search_RSFLC <- function(data_train, data_vali) {
  N_RANDOM_SEARCH <- 20L
  RSFLC_NTREE_CAND <- c(50L, 100L, 150L, 200L, 250L, 300L, 400L)
  RSFLC_NODESIZE_CAND <- c(3L, 5L, 7L, 10L, 15L, 20L, 30L)
  RSFLC_MINSPLIT_FIXED <- 2L
  RSFLC_NSPLIT <- "quantile"
  fold_drop <- c("fold", "fold1", "fold_1", "fold2", "fold3", "fold4", "fold5", "fold6")

  first_not_na <- function(x) {
    if (length(x) == 0L) return(NA)
    ok <- !is.na(x) & !(is.character(x) & trimws(as.character(x)) == "")
    if (!any(ok)) return(NA)
    x[which(ok)[1L]]
  }

  # 由训练集构造 DynForest 输入；验证集沿用训练集 longitudinal 列名并补 NA
  build_rsflc_inputs <- function(df, fold_drop_cols, long_vars_template = NULL, gender_levels = NULL) {
    d <- dplyr::select(as.data.frame(df, stringsAsFactors = FALSE), -dplyr::any_of(fold_drop_cols)) %>%
      as.data.frame(stringsAsFactors = FALSE)

    if (!"subject_id" %in% names(d)) stop("iter_search_RSFLC：缺少 subject_id。")
    if (!all(c("los_hosp_days", "death", "Obstimes") %in% names(d))) {
      stop("iter_search_RSFLC：缺少 los_hosp_days、death 或 Obstimes。")
    }

    d$id <- as.numeric(as.factor(as.character(d$subject_id)))
    d$death <- suppressWarnings(as.integer(round(as.numeric(d$death))))
    d$los_hosp_days <- suppressWarnings(as.numeric(d$los_hosp_days))
    d$Obstimes <- suppressWarnings(as.numeric(d$Obstimes))

    d <- d %>%
      dplyr::group_by(.data$id) %>%
      dplyr::mutate(time = .data$Obstimes - min(.data$Obstimes, na.rm = TRUE)) %>%
      dplyr::ungroup()

    # 基线进 fixedData，不进 timeData（与 0314_RSFLC 思路一致）
    baseline_in_fixed <- intersect(c("anchor_age", "gender"), names(d))

    exclude_from_long <- unique(c(
      "id", "subject_id", "Obstimes", "charttime", "los_hosp_days", "death", "deathtime",
      baseline_in_fixed
    ))
    cand <- setdiff(names(d), exclude_from_long)

    coerce_num <- function(v, x) {
      if (is.numeric(x)) return(x)
      if (is.logical(x)) return(as.numeric(x))
      if (is.integer(x)) return(as.numeric(x))
      suppressWarnings(as.numeric(as.character(x)))
    }

    long_vars <- cand[vapply(cand, function(nm) {
      x <- d[[nm]]
      xn <- coerce_num(nm, x)
      sum(is.finite(xn)) >= max(30L, as.integer(0.2 * nrow(d)))
    }, logical(1))]

    if (is.null(long_vars_template)) {
      if (length(long_vars) < 1L) stop("iter_search_RSFLC：未找到可用的纵向数值预测列。")
    } else {
      for (v in long_vars_template) {
        if (!v %in% names(d)) d[[v]] <- NA_real_
        else d[[v]] <- coerce_num(v, d[[v]])
      }
      long_vars <- long_vars_template
    }

    timeData <- d %>%
      dplyr::select("id", "time", dplyr::all_of(long_vars)) %>%
      as.data.frame(stringsAsFactors = FALSE)

    fixed_surv <- d %>%
      dplyr::group_by(.data$id) %>%
      dplyr::summarise(
        time = suppressWarnings(as.numeric(dplyr::first(stats::na.omit(.data$los_hosp_days)))),
        event = as.integer(dplyr::first(stats::na.omit(.data$death))),
        .groups = "drop"
      ) %>%
      dplyr::filter(is.finite(.data$time), .data$time > 0, !is.na(.data$event))

    if (length(baseline_in_fixed) > 0L) {
      fd0 <- d %>%
        dplyr::group_by(.data$id) %>%
        dplyr::summarise(dplyr::across(dplyr::all_of(baseline_in_fixed), first_not_na), .groups = "drop")
      fixedData <- dplyr::left_join(fixed_surv, fd0, by = "id")
    } else {
      fixedData <- fixed_surv
    }

    if ("gender" %in% names(fixedData)) {
      g <- trimws(as.character(fixedData$gender))
      g[g == ""] <- NA_character_
      if (is.null(gender_levels)) {
        fixedData$gender <- as.factor(g)
      } else {
        fixedData$gender <- factor(g, levels = gender_levels)
      }
    }
    if ("anchor_age" %in% names(fixedData)) {
      fixedData$anchor_age <- suppressWarnings(as.numeric(fixedData$anchor_age))
    }

    fixedData <- fixedData %>% dplyr::distinct(.data$id, .keep_all = TRUE)
    timeData <- timeData %>% dplyr::filter(.data$id %in% fixedData$id)

    timeVarModel <- stats::setNames(
      lapply(long_vars, function(v) {
        list(model = "linear", fixed = ~1, random = ~1 + time | id)
      }),
      long_vars
    )

    y_surv <- fixedData %>% dplyr::select("id", "time", "event")

    p_mtry <- length(long_vars) + max(0L, ncol(fixedData) - 3L)
    p_mtry <- max(1L, as.integer(p_mtry))

    list(
      timeData = as.data.frame(timeData, stringsAsFactors = FALSE),
      fixedData = as.data.frame(fixedData, stringsAsFactors = FALSE),
      timeVarModel = timeVarModel,
      y_surv = as.data.frame(y_surv, stringsAsFactors = FALSE),
      long_vars = long_vars,
      p_mtry = p_mtry,
      gender_levels = if ("gender" %in% names(fixedData)) levels(as.factor(trimws(as.character(fixedData$gender)))) else NULL
    )
  }

  pred_rsflc_risk_t0 <- function(fit, td, fd, t0) {
    pr <- tryCatch(
      predict(
        fit,
        timeData = as.data.frame(td),
        fixedData = as.data.frame(fd),
        idVar = "id",
        timeVar = "time",
        t0 = t0
      ),
      error = function(e) NULL
    )
    if (is.null(pr) || is.null(pr$pred_indiv)) return(list(risk = NULL, surv_probs = NULL))
    pm <- as.matrix(pr$pred_indiv)
    if (ncol(pm) < 1L) return(list(risk = NULL, surv_probs = NULL))
    ids <- suppressWarnings(as.numeric(rownames(pm)))
    rk <- as.numeric(pm[, ncol(pm)])
    if (length(ids) != length(rk) || anyNA(ids)) return(list(risk = NULL, surv_probs = NULL))
    sp <- pmax(0, pmin(1, 1 - rk))
    list(risk = rk, surv_probs = sp, pred_ids = ids)
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

  tr <- build_rsflc_inputs(data_train, fold_drop, NULL, NULL)
  timeData_tr <- tr$timeData
  fixedData_tr <- tr$fixedData
  timeVarModel <- tr$timeVarModel
  y_surv_tr <- tr$y_surv
  p_mtry <- tr$p_mtry
  glev <- tr$gender_levels

  va <- build_rsflc_inputs(data_vali, fold_drop, tr$long_vars, glev)
  timeData_va <- va$timeData
  fixedData_va <- va$fixedData
  y_surv_va <- va$y_surv

  t0_train <- stats::median(fixedData_tr$time, na.rm = TRUE)
  flt_va <- rsflc_filter_predict_cohort(timeData_va, fixedData_va, "id", "time", t0_train)
  timeData_va <- flt_va$timeData
  fixedData_va <- flt_va$fixedData
  death_v <- as.integer(fixedData_va$event)
  df_cens_v <- data.frame(obs_time = fixedData_va$time, event = death_v)
  dca_thresholds <- seq(0.05, 0.95, by = 0.1)

  set.seed(5000L)
  best_tier <- 0L
  best_val <- -Inf
  best_ntree <- 500L
  best_mtry <- max(1L, min(p_mtry, as.integer(ceiling(sqrt(p_mtry)))))
  best_nodesize <- 10L
  tune_pick_reason <- "default"
  pick_brier_score <- NA_real_
  tune_log <- vector("list", N_RANDOM_SEARCH)
  vali_metrics_rows <- vector("list", N_RANDOM_SEARCH)
  t_run <- proc.time()

  for (it in seq_len(N_RANDOM_SEARCH)) {
    ntree_it <- sample(RSFLC_NTREE_CAND, 1L)
    nodesize_it <- sample(RSFLC_NODESIZE_CAND, 1L)
    mtry_it <- sample.int(p_mtry, 1L)

    fit_it <- tryCatch(
      DynForest::dynforest(
        timeData = timeData_tr,
        fixedData = fixedData_tr,
        idVar = "id",
        timeVar = "time",
        timeVarModel = timeVarModel,
        Y = list(type = "surv", Y = y_surv_tr),
        ntree = ntree_it,
        mtry = mtry_it,
        nodesize = nodesize_it,
        minsplit = RSFLC_MINSPLIT_FIXED,
        nsplit_option = RSFLC_NSPLIT,
        ncores = 1L,
        verbose = FALSE
      ),
      error = function(e) NULL
    )

    elapsed <- unname((proc.time() - t_run)["elapsed"])
    stamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    n_v <- nrow(fixedData_va)

    if (is.null(fit_it)) {
      message(stamp, " | RSFLC 进度 ", it, "/", N_RANDOM_SEARCH, " | dynforest 失败 | 累计 ", round(elapsed, 1), " s")
      tune_log[[it]] <- data.frame(
        iter = it, ntree = ntree_it, mtry = mtry_it, nodesize = nodesize_it,
        ok = FALSE, fit_ok = FALSE, pred_ok = FALSE,
        stringsAsFactors = FALSE
      )
      vali_metrics_rows[[it]] <- data.frame(
        iter = it, ntree = ntree_it, mtry = mtry_it, nodesize = nodesize_it,
        cindex = NA_real_, brier = NA_real_, auc = NA_real_,
        dca_mean_nb = NA_real_, dca_auc_nb = NA_real_, t0 = t0_train,
        fit_ok = FALSE, ok = FALSE,
        stringsAsFactors = FALSE
      )
      next
    }

    ex <- pred_rsflc_risk_t0(fit_it, timeData_va, fixedData_va, t0_train)
    pred_ids <- ex$pred_ids
    risk_va <- ex$risk
    sp_va <- ex$surv_probs
    metrics_ok <- !is.null(risk_va) && !is.null(pred_ids) && length(risk_va) == n_v &&
      !is.null(sp_va) && length(sp_va) == n_v && !anyNA(sp_va)
    if (metrics_ok) {
      o <- match(fixedData_va$id, pred_ids)
      if (anyNA(o)) {
        metrics_ok <- FALSE
      } else {
        risk_ord <- risk_va[o]
        sp_ord <- sp_va[o]
      }
    }

    cix <- NA_real_
    bs_v <- NA_real_
    auc_v <- NA_real_
    dmn <- NA_real_
    dau <- NA_real_
    if (isTRUE(metrics_ok)) {
      mt <- calc_metrics_vali(
        fixedData_va$time, death_v, risk_ord, sp_ord, t0_train, df_cens_v
      )
      cix <- mt$CINDEX
      bs_v <- mt$BS
      auc_v <- mt$AUC
      dca_r <- calc_dca_vali(fixedData_va$time, death_v, risk_ord, t0_train, dca_thresholds)
      dmn <- dca_r$mean_nb
      dau <- dca_r$auc_nb
    }

    if (isTRUE(metrics_ok)) {
      message(
        stamp, " | RSFLC 进度 ", it, "/", N_RANDOM_SEARCH,
        " | 预测对齐 | C=", ifelse(is.finite(cix), format(round(cix, 4), nsmall = 4), "NA"),
        " AUC_t=", ifelse(is.finite(auc_v), format(round(auc_v, 4), nsmall = 4), "NA"),
        " Brier=", ifelse(is.finite(bs_v), format(round(bs_v, 4), nsmall = 4), "NA"),
        " DCA_auc_nb=", ifelse(is.finite(dau), format(round(dau, 4), nsmall = 4), "NA"),
        " | 累计 ", round(elapsed, 1), " s"
      )
    } else {
      message(
        stamp, " | RSFLC 进度 ", it, "/", N_RANDOM_SEARCH,
        " | 预测未通过（长度/NA/id 不匹配）；本迭代不调参打分 | 累计 ", round(elapsed, 1), " s"
      )
    }

    tune_log[[it]] <- data.frame(
      iter = it, ntree = ntree_it, mtry = mtry_it, nodesize = nodesize_it,
      ok = TRUE, fit_ok = TRUE, pred_ok = isTRUE(metrics_ok),
      stringsAsFactors = FALSE
    )
    vali_metrics_rows[[it]] <- data.frame(
      iter = it, ntree = ntree_it, mtry = mtry_it, nodesize = nodesize_it,
      cindex = cix, brier = bs_v, auc = auc_v,
      dca_mean_nb = dmn, dca_auc_nb = dau, t0 = t0_train,
      fit_ok = TRUE, ok = isTRUE(metrics_ok),
      stringsAsFactors = FALSE
    )

    if (isTRUE(metrics_ok)) {
      tier_it <- 0L
      val_it <- -Inf
      pick_this <- NA_character_
      if (is.finite(dau) && !is.na(dau)) {
        tier_it <- 3L
        val_it <- dau
        pick_this <- "dca_auc_nb"
      } else if (is.finite(dmn) && !is.na(dmn)) {
        tier_it <- 2L
        val_it <- dmn
        pick_this <- "dca_mean_nb"
      } else if (is.finite(cix) && !is.na(cix)) {
        tier_it <- 1L
        val_it <- cix
        pick_this <- "cindex"
      } else if (is.finite(auc_v) && !is.na(auc_v)) {
        tier_it <- 1L
        val_it <- auc_v
        pick_this <- "auc_timeROC"
      }
      if (tier_it > 0L && (best_tier == 0L || tier_it > best_tier || (tier_it == best_tier && val_it > best_val))) {
        best_tier <- tier_it
        best_val <- val_it
        best_ntree <- ntree_it
        best_mtry <- mtry_it
        best_nodesize <- nodesize_it
        tune_pick_reason <- pick_this
      }
    }
  }

  tune_rsf_results <- do.call(rbind, tune_log)
  rownames(tune_rsf_results) <- NULL
  vali_tune_metrics <- do.call(rbind, vali_metrics_rows)
  rownames(vali_tune_metrics) <- NULL

  if (best_tier == 0L) {
    vm <- vali_tune_metrics
    w_b <- which(vm$ok & is.finite(vm$brier))
    if (length(w_b) > 0L) {
      j <- w_b[which.min(vm$brier[w_b])]
      best_ntree <- as.integer(vm$ntree[j])
      best_mtry <- as.integer(vm$mtry[j])
      best_nodesize <- as.integer(vm$nodesize[j])
      pick_brier_score <- as.numeric(vm$brier[j])
      tune_pick_reason <- "brier_min"
      message(
        "RSFLC：DCA/C-index/timeROC-AUC 均未进入分层；已按验证集 IPCW-Brier 最小选参 | iter=",
        vm$iter[j], " | ntree=", best_ntree, " mtry=", best_mtry, " nodesize=", best_nodesize,
        " | Brier=", round(pick_brier_score, 5)
      )
    } else {
      wf <- which(vm$fit_ok %in% TRUE)
      if (length(wf) > 0L) {
        j <- wf[[1L]]
        best_ntree <- as.integer(vm$ntree[j])
        best_mtry <- as.integer(vm$mtry[j])
        best_nodesize <- as.integer(vm$nodesize[j])
        tune_pick_reason <- "first_fit"
        message(
          "RSFLC：验证预测均未对齐或指标全 NA；保留首次 dynforest 成功迭代的超参 | iter=",
          vm$iter[j], " | ntree=", best_ntree, " mtry=", best_mtry, " nodesize=", best_nodesize
        )
      } else {
        message("RSFLC：全部迭代拟合失败，退回默认超参数。")
      }
    }
  }

  t_final <- proc.time()
  fit <- tryCatch(
    DynForest::dynforest(
      timeData = timeData_tr,
      fixedData = fixedData_tr,
      idVar = "id",
      timeVar = "time",
      timeVarModel = timeVarModel,
      Y = list(type = "surv", Y = y_surv_tr),
      ntree = best_ntree,
      mtry = best_mtry,
      nodesize = best_nodesize,
      minsplit = RSFLC_MINSPLIT_FIXED,
      nsplit_option = RSFLC_NSPLIT,
      ncores = 1L,
      verbose = FALSE
    ),
    error = function(e) NULL
  )
  message(
    format(Sys.time(), "%Y-%m-%d %H:%M:%S"), " | RSFLC 最终 dynforest 完成 | 用时 ",
    round(unname((proc.time() - t_final)["elapsed"]), 1), " s"
  )
  if (is.null(fit)) stop("iter_search_RSFLC：最终 dynforest 拟合失败。")

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
  attr(result, "tune_best_score") <- if (best_tier > 0L) {
    best_val
  } else if (is.finite(pick_brier_score)) {
    pick_brier_score
  } else {
    NA_real_
  }
  attr(result, "tune_pick_reason") <- tune_pick_reason
  attr(result, "rsflc_long_vars") <- tr$long_vars
  attr(result, "rsflc_p_mtry") <- p_mtry

  crit_lab <- if (nzchar(tune_pick_reason) && tune_pick_reason != "default") {
    switch(
      tune_pick_reason,
      dca_auc_nb = "dca_auc_nb",
      dca_mean_nb = "dca_mean_nb",
      cindex = "cindex",
      auc_timeROC = "auc_timeROC（timeROC，与 C/DCA 并列兜底）",
      brier_min = "brier_min（IPCW-Brier 最小）",
      first_fit = "first_fit（仅拟合成功）",
      default = "默认超参",
      tune_pick_reason
    )
  } else {
    "默认超参"
  }
  message(
    "RSFLC 随机搜索结束。主分层：dca_auc_nb > dca_mean_nb > cindex > auc_timeROC；",
    "若无则 Brier 最小，再则首次拟合成功。",
    " 本次依据：", crit_lab, " | ntree=", best_ntree, " mtry=", best_mtry, " nodesize=", best_nodesize
  )

  attr(vali_tune_metrics, "dca_note") <- paste0(
    "RSFLC/DynForest；t0 为训练集 fixedData$time 中位数；纵向列数=", length(tr$long_vars),
    "；阈值 ", paste(format(dca_thresholds, digits = 2), collapse = ", ")
  )

  list(fit, result, vali_tune_metrics)
}







#####测试#############

data_v <- raw[!is.na(fv) & fv == 5L, , drop = FALSE]
data_t <- raw[!is.na(fv) & fv %in% 1L:4L, , drop = FALSE]


test<-iter_search_RSFLC(data_t,data_v)
################









######## modeling_RSFLC：固定 DynForest 超参，训练 + 测试集评估（与 iter_search_RSFLC 同源预处理与指标；本函数内嵌副本，不依赖外部新增代码）########
modeling_RSFLC <- function(
  ntree,
  mtry,
  nodesize,
  data_train,
  data_test,
  minsplit = 2L,
  nsplit_option = "quantile",
  ncores = 1L,
  verbose = FALSE,
  seed = 123L
) {
  fold_drop <- c("fold", "fold1", "fold_1", "fold2", "fold3", "fold4", "fold5", "fold6")

  first_not_na <- function(x) {
    if (length(x) == 0L) return(NA)
    ok <- !is.na(x) & !(is.character(x) & trimws(as.character(x)) == "")
    if (!any(ok)) return(NA)
    x[which(ok)[1L]]
  }

  build_rsflc_inputs <- function(df, fold_drop_cols, long_vars_template = NULL, gender_levels = NULL) {
    d <- dplyr::select(as.data.frame(df, stringsAsFactors = FALSE), -dplyr::any_of(fold_drop_cols)) %>%
      as.data.frame(stringsAsFactors = FALSE)

    if (!"subject_id" %in% names(d)) stop("modeling_RSFLC：缺少 subject_id。")
    if (!all(c("los_hosp_days", "death", "Obstimes") %in% names(d))) {
      stop("modeling_RSFLC：缺少 los_hosp_days、death 或 Obstimes。")
    }

    d$id <- as.numeric(as.factor(as.character(d$subject_id)))
    d$death <- suppressWarnings(as.integer(round(as.numeric(d$death))))
    d$los_hosp_days <- suppressWarnings(as.numeric(d$los_hosp_days))
    d$Obstimes <- suppressWarnings(as.numeric(d$Obstimes))

    d <- d %>%
      dplyr::group_by(.data$id) %>%
      dplyr::mutate(time = .data$Obstimes - min(.data$Obstimes, na.rm = TRUE)) %>%
      dplyr::ungroup()

    baseline_in_fixed <- intersect(c("anchor_age", "gender"), names(d))

    exclude_from_long <- unique(c(
      "id", "subject_id", "Obstimes", "charttime", "los_hosp_days", "death", "deathtime",
      baseline_in_fixed
    ))
    cand <- setdiff(names(d), exclude_from_long)

    coerce_num <- function(v, x) {
      if (is.numeric(x)) return(x)
      if (is.logical(x)) return(as.numeric(x))
      if (is.integer(x)) return(as.numeric(x))
      suppressWarnings(as.numeric(as.character(x)))
    }

    long_vars <- cand[vapply(cand, function(nm) {
      x <- d[[nm]]
      xn <- coerce_num(nm, x)
      sum(is.finite(xn)) >= max(30L, as.integer(0.2 * nrow(d)))
    }, logical(1))]

    if (is.null(long_vars_template)) {
      if (length(long_vars) < 1L) stop("modeling_RSFLC：未找到可用的纵向数值预测列。")
    } else {
      for (v in long_vars_template) {
        if (!v %in% names(d)) d[[v]] <- NA_real_
        else d[[v]] <- coerce_num(v, d[[v]])
      }
      long_vars <- long_vars_template
    }

    timeData <- d %>%
      dplyr::select("id", "time", dplyr::all_of(long_vars)) %>%
      as.data.frame(stringsAsFactors = FALSE)

    fixed_surv <- d %>%
      dplyr::group_by(.data$id) %>%
      dplyr::summarise(
        time = suppressWarnings(as.numeric(dplyr::first(stats::na.omit(.data$los_hosp_days)))),
        event = as.integer(dplyr::first(stats::na.omit(.data$death))),
        .groups = "drop"
      ) %>%
      dplyr::filter(is.finite(.data$time), .data$time > 0, !is.na(.data$event))

    if (length(baseline_in_fixed) > 0L) {
      fd0 <- d %>%
        dplyr::group_by(.data$id) %>%
        dplyr::summarise(dplyr::across(dplyr::all_of(baseline_in_fixed), first_not_na), .groups = "drop")
      fixedData <- dplyr::left_join(fixed_surv, fd0, by = "id")
    } else {
      fixedData <- fixed_surv
    }

    if ("gender" %in% names(fixedData)) {
      g <- trimws(as.character(fixedData$gender))
      g[g == ""] <- NA_character_
      if (is.null(gender_levels)) {
        fixedData$gender <- as.factor(g)
      } else {
        fixedData$gender <- factor(g, levels = gender_levels)
      }
    }
    if ("anchor_age" %in% names(fixedData)) {
      fixedData$anchor_age <- suppressWarnings(as.numeric(fixedData$anchor_age))
    }

    fixedData <- fixedData %>% dplyr::distinct(.data$id, .keep_all = TRUE)
    timeData <- timeData %>% dplyr::filter(.data$id %in% fixedData$id)

    timeVarModel <- stats::setNames(
      lapply(long_vars, function(v) {
        list(model = "linear", fixed = ~1, random = ~1 + time | id)
      }),
      long_vars
    )

    y_surv <- fixedData %>% dplyr::select("id", "time", "event")

    p_mtry <- length(long_vars) + max(0L, ncol(fixedData) - 3L)
    p_mtry <- max(1L, as.integer(p_mtry))

    list(
      timeData = as.data.frame(timeData, stringsAsFactors = FALSE),
      fixedData = as.data.frame(fixedData, stringsAsFactors = FALSE),
      timeVarModel = timeVarModel,
      y_surv = as.data.frame(y_surv, stringsAsFactors = FALSE),
      long_vars = long_vars,
      p_mtry = p_mtry,
      gender_levels = if ("gender" %in% names(fixedData)) levels(as.factor(trimws(as.character(fixedData$gender)))) else NULL
    )
  }

  pred_rsflc_risk_t0 <- function(fit, td, fd, t0) {
    pr <- tryCatch(
      predict(
        fit,
        timeData = as.data.frame(td),
        fixedData = as.data.frame(fd),
        idVar = "id",
        timeVar = "time",
        t0 = t0
      ),
      error = function(e) NULL
    )
    if (is.null(pr) || is.null(pr$pred_indiv)) return(list(risk = NULL, surv_probs = NULL))
    pm <- as.matrix(pr$pred_indiv)
    if (ncol(pm) < 1L) return(list(risk = NULL, surv_probs = NULL))
    ids <- suppressWarnings(as.numeric(rownames(pm)))
    rk <- as.numeric(pm[, ncol(pm)])
    if (length(ids) != length(rk) || anyNA(ids)) return(list(risk = NULL, surv_probs = NULL))
    sp <- pmax(0, pmin(1, 1 - rk))
    list(risk = rk, surv_probs = sp, pred_ids = ids)
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

  tr <- build_rsflc_inputs(data_train, fold_drop, NULL, NULL)
  timeData_tr <- tr$timeData
  fixedData_tr <- tr$fixedData
  timeVarModel <- tr$timeVarModel
  y_surv_tr <- tr$y_surv

  te <- build_rsflc_inputs(data_test, fold_drop, tr$long_vars, tr$gender_levels)
  timeData_te <- te$timeData
  fixedData_te <- te$fixedData

  t0_train <- stats::median(fixedData_tr$time, na.rm = TRUE)
  flt_te <- rsflc_filter_predict_cohort(timeData_te, fixedData_te, "id", "time", t0_train)
  timeData_te <- flt_te$timeData
  fixedData_te <- flt_te$fixedData
  death_te <- as.integer(fixedData_te$event)
  df_cens_te <- data.frame(obs_time = fixedData_te$time, event = death_te)
  n_te <- nrow(fixedData_te)

  set.seed(as.integer(seed))
  fit <- tryCatch(
    DynForest::dynforest(
      timeData = timeData_tr,
      fixedData = fixedData_tr,
      idVar = "id",
      timeVar = "time",
      timeVarModel = timeVarModel,
      Y = list(type = "surv", Y = y_surv_tr),
      ntree = as.integer(ntree),
      mtry = as.integer(mtry),
      nodesize = as.integer(nodesize),
      minsplit = as.integer(minsplit),
      nsplit_option = nsplit_option,
      ncores = as.integer(ncores),
      verbose = isTRUE(verbose)
    ),
    error = function(e) NULL
  )
  if (is.null(fit)) stop("modeling_RSFLC：训练集 dynforest 拟合失败。")

  ex_te <- pred_rsflc_risk_t0(fit, timeData_te, fixedData_te, t0_train)
  ok <- !is.null(ex_te$risk) && !is.null(ex_te$pred_ids) && length(ex_te$risk) == n_te &&
    !is.null(ex_te$surv_probs) && length(ex_te$surv_probs) == n_te && !anyNA(ex_te$surv_probs)
  if (isTRUE(ok)) {
    o <- match(fixedData_te$id, ex_te$pred_ids)
    if (anyNA(o)) {
      ok <- FALSE
    } else {
      risk_ord <- ex_te$risk[o]
      sp_ord <- ex_te$surv_probs[o]
    }
  }

  cix <- NA_real_
  auc_v <- NA_real_
  bs_v <- NA_real_
  if (isTRUE(ok)) {
    mt <- calc_metrics_test(
      fixedData_te$time,
      death_te,
      risk_ord,
      sp_ord,
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
  attr(metrics, "t0_note") <- "t0 为 data_train 的 fixedData$time 中位数（与 iter_search_RSFLC 一致，避免用测试集定 t0）"
  attr(metrics, "ntree") <- as.integer(ntree)
  attr(metrics, "mtry") <- as.integer(mtry)
  attr(metrics, "nodesize") <- as.integer(nodesize)
  attr(metrics, "minsplit") <- as.integer(minsplit)
  attr(metrics, "nsplit_option") <- nsplit_option
  attr(metrics, "ncores") <- as.integer(ncores)
  attr(metrics, "rsflc_long_vars") <- tr$long_vars
  attr(metrics, "rsflc_p_mtry") <- tr$p_mtry
  attr(metrics, "metrics_ok") <- ok

  list(fit, metrics)
}

####################


############## 实现 5 折嵌套 CV（RSFLC / DynForest，与 0404_RSF 结构一致；0407_DATA_fold1 内层 fold2~fold6）##############
# 外层：fold1=k 为测试；开发集内按 fold(7-k) 列做 A/B/C 三折 iter_search_RSFLC；再 modeling_RSFLC(全开发集, 外层测试)

outer_folds <- 1L:5L
cv_MODEL_LIST_RSFLC <- vector("list", length(outer_folds))
names(cv_MODEL_LIST_RSFLC) <- paste0("fold1_test_", outer_folds)
cv_summary_rows_rsflc <- vector("list", length(outer_folds))

for (idx in seq_along(outer_folds)) {
  outer_fold <- outer_folds[[idx]]
  data_vali <- raw[!is.na(fv) & fv == outer_fold, , drop = FALSE]
  data_train <- raw[!is.na(fv) & fv != outer_fold & fv %in% 1L:5L, , drop = FALSE]

  inner_col <- paste0("fold", 7L - as.integer(outer_fold))

  message(
    "\n========== RSFLC 外层 ", idx, "/5 | 外层测试 fold1=", outer_fold,
    " | 开发集 n=", nrow(data_train), " | 测试集 n=", nrow(data_vali),
    " | 内层列=", inner_col, " =========="
  )

  if (!inner_col %in% names(data_train)) {
    stop(
      "data_train 缺少 ", inner_col, "（内层 A/B/C）。外层 fold1=", outer_fold,
      " 请使用 DATA/0407_DATA_fold1.xlsx。"
    )
  }

  set.seed(5000L + outer_fold)

  inner_abc_chr <- as.character(data_train[[inner_col]])

  idx_vali_a <- !is.na(inner_abc_chr) & inner_abc_chr == "A"
  idx_train_a <- !is.na(inner_abc_chr) & inner_abc_chr != "A"
  d_in_tr_a <- data_train[idx_train_a, , drop = FALSE]
  d_in_va_a <- data_train[idx_vali_a, , drop = FALSE]
  message(
    "RSFLC 内部三折 | 外层测试=", outer_fold, " | 验证折=A | 训练 n=", nrow(d_in_tr_a), " | 验证 n=", nrow(d_in_va_a)
  )
  fold_list1 <- iter_search_RSFLC(d_in_tr_a, d_in_va_a)

  idx_vali_b <- !is.na(inner_abc_chr) & inner_abc_chr == "B"
  idx_train_b <- !is.na(inner_abc_chr) & inner_abc_chr != "B"
  d_in_tr_b <- data_train[idx_train_b, , drop = FALSE]
  d_in_va_b <- data_train[idx_vali_b, , drop = FALSE]
  message(
    "RSFLC 内部三折 | 外层测试=", outer_fold, " | 验证折=B | 训练 n=", nrow(d_in_tr_b), " | 验证 n=", nrow(d_in_va_b)
  )
  fold_list2 <- iter_search_RSFLC(d_in_tr_b, d_in_va_b)

  idx_vali_c <- !is.na(inner_abc_chr) & inner_abc_chr == "C"
  idx_train_c <- !is.na(inner_abc_chr) & inner_abc_chr != "C"
  d_in_tr_c <- data_train[idx_train_c, , drop = FALSE]
  d_in_va_c <- data_train[idx_vali_c, , drop = FALSE]
  message(
    "RSFLC 内部三折 | 外层测试=", outer_fold, " | 验证折=C | 训练 n=", nrow(d_in_tr_c), " | 验证 n=", nrow(d_in_va_c)
  )
  fold_list3 <- iter_search_RSFLC(d_in_tr_c, d_in_va_c)

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
        "RSFLC 外层 ", outer_fold, "：三折 tune_best_tier 均为 0，改按 dca_auc_nb 最大选折：",
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
    "RSFLC 外层 ", outer_fold, " 内三折汇总：", win_nm,
    " | 内层列=", inner_col,
    " | tier=", best_tier_pick,
    " score=", ifelse(is.finite(best_score_pick), round(best_score_pick, 6), NA),
    " | ntree=", ntree_o, " mtry=", mtry_o, " nodesize=", nodesize_o
  )

  MODEL_LIST_RSFLC <- modeling_RSFLC(ntree_o, mtry_o, nodesize_o, data_train, data_vali)
  attr(MODEL_LIST_RSFLC[[2]], "chosen_from_inner_fold") <- win_nm
  attr(MODEL_LIST_RSFLC[[2]], "inner_tune_best_tier") <- best_tier_pick
  attr(MODEL_LIST_RSFLC[[2]], "inner_tune_best_score") <- best_score_pick
  attr(MODEL_LIST_RSFLC[[2]], "outer_fold1_test") <- outer_fold
  attr(MODEL_LIST_RSFLC[[2]], "inner_abc_column") <- inner_col

  cv_MODEL_LIST_RSFLC[[idx]] <- MODEL_LIST_RSFLC
  m2 <- MODEL_LIST_RSFLC[[2]]
  cv_summary_rows_rsflc[[idx]] <- data.frame(
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
    "RSFLC 外层 ", outer_fold, " 测试集 | cindex=", round(m2$cindex, 4),
    " auc=", round(m2$auc, 4), " brier=", round(m2$brier, 6)
  )
}

CV_5FOLD_SUMMARY_RSFLC <- do.call(rbind, cv_summary_rows_rsflc)
rownames(CV_5FOLD_SUMMARY_RSFLC) <- NULL

message("\n========== RSFLC 5 折嵌套 CV 汇总表 ==========")
print(CV_5FOLD_SUMMARY_RSFLC)
message(
  "RSFLC 5 折均值（na.rm） | cindex=", round(mean(CV_5FOLD_SUMMARY_RSFLC$cindex, na.rm = TRUE), 4),
  " auc=", round(mean(CV_5FOLD_SUMMARY_RSFLC$auc, na.rm = TRUE), 4),
  " brier=", round(mean(CV_5FOLD_SUMMARY_RSFLC$brier, na.rm = TRUE), 6)
)

cv_out_dir <- file.path(work_dir, "DATA")
utils::write.csv(
  CV_5FOLD_SUMMARY_RSFLC,
  file.path(cv_out_dir, "RSFLC_nestedCV_5fold_summary.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)
message("已写入 ", file.path(cv_out_dir, "RSFLC_nestedCV_5fold_summary.csv"))

# 与「单折：fold1=5 为测试」对齐，便于后续交互
data_vali <- raw[!is.na(fv) & fv == 5L, , drop = FALSE]
data_train <- raw[!is.na(fv) & fv %in% 1L:4L, , drop = FALSE]
MODEL_LIST_RSFLC <- cv_MODEL_LIST_RSFLC[["fold1_test_5"]]

########################

