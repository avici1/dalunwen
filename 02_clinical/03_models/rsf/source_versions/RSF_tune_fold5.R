# =============================================================================
# RSF 5 折交叉验证调参
# 每组超参轮流用第 k 折预测、其余 4 折训练；按验证集 C-index 五折均值选优
# 流程对齐 RSFLC_20V_tune_fold5；指标口径对齐 RSF_baseline_0824
# 不含 VIMP / SHAP / 作图 / Word，不用 group=2
# =============================================================================
library(dplyr)
library(survival)
library(randomForestSRC)
library(prodlim)
library(riskRegression)

# ---------------------------------------------------------------------------
# 开关与设定
# ---------------------------------------------------------------------------
seed_value <- 2026L
evaluation_horizon <- 28 - 1e-04
evaluation_times <- unique(c(seq(0, 27.5, by = 0.5), evaluation_horizon))
thread_number <- max(1L, parallel::detectCores(logical = TRUE) - 2L)
options(rf.cores = thread_number)

base_dir <- "F:/文章_大论文/0722/实例研究代码"
out_dir <- file.path(base_dir, "5折RSF")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
baseline_path <- file.path(base_dir, "stroke_baseline_knn_0824_fold.csv")
result_path <- file.path(out_dir, "results_RSF_cv.csv")
fold_result_path <- file.path(out_dir, "results_RSF_cv_folds.csv")
best_perf_path <- file.path(out_dir, "best_performance.csv")
best_param_path <- file.path(out_dir, "best_parameters.csv")

fixed_covars <- c(
  "age", "charlson_comorbidity_index", "apsiii", "sapsii", "oasis",
  "preiculos", "mechvent", "electivesurgery", "gender", "bmi", "stroke_type"
)
model_cols <- c(fixed_covars, "time28", "status28")

# ---------------------------------------------------------------------------
# 工具函数
# ---------------------------------------------------------------------------
mean_na <- function(x) {
  if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE)
}

prob_at_28 <- function(surv_mat, time_interest) {
  time_index <- which.min(abs(time_interest - 28))
  as.numeric(1 - surv_mat[, time_index])
}

extract_score_at_horizon <- function(score_obj, model_name, horizon) {
  auc_tbl <- as.data.frame(score_obj$AUC$score)
  brier_tbl <- as.data.frame(score_obj$Brier$score)
  auc_val <- auc_tbl$AUC[
    as.character(auc_tbl$model) == model_name &
      abs(auc_tbl$times - horizon) < 1e-08
  ]
  brier_val <- brier_tbl$Brier[
    as.character(brier_tbl$model) == model_name &
      abs(brier_tbl$times - horizon) < 1e-08
  ]
  ibs_val <- brier_tbl$IBS[
    as.character(brier_tbl$model) == model_name &
      abs(brier_tbl$times - horizon) < 1e-08
  ]
  list(
    auc = if (length(auc_val)) as.numeric(auc_val[1]) else NA_real_,
    brier = if (length(brier_val)) as.numeric(brier_val[1]) else NA_real_,
    ibs = if (length(ibs_val)) as.numeric(ibs_val[1]) else NA_real_
  )
}

score_rsf <- function(model, data) {
  out <- list(auc = NA_real_, brier = NA_real_, ibs = NA_real_)
  score_obj <- tryCatch(
    riskRegression::Score(
      object = list(RSF = model),
      formula = Hist(time28, status28) ~ 1,
      data = data,
      metrics = c("auc", "brier"),
      times = evaluation_times,
      summary = "ibs",
      cens.method = "ipcw",
      conf.int = FALSE,
      plots = NULL,
      predictRisk.args = list(rfsrc = list(na.action = "na.impute"))
    ),
    error = function(e) NULL
  )
  if (!is.null(score_obj)) {
    out <- extract_score_at_horizon(score_obj, "RSF", evaluation_horizon)
  }
  out
}

cindex_from_prob <- function(time28, status28, probability) {
  keep <- is.finite(probability) & !is.na(status28)
  if (!any(keep)) return(NA_real_)
  as.numeric(survival::concordance(
    survival::Surv(time28[keep], status28[keep]) ~ probability[keep],
    reverse = TRUE
  )$concordance)
}

ordinary_brier <- function(status28, probability) {
  keep <- is.finite(probability) & !is.na(status28)
  if (!any(keep)) return(NA_real_)
  mean((as.numeric(status28[keep]) - probability[keep])^2)
}

# ---------------------------------------------------------------------------
# 读数：仅 group=1，按 fold 划分
# ---------------------------------------------------------------------------
stroke_baseline <- read.csv(
  baseline_path,
  stringsAsFactors = FALSE,
  na.strings = c("", "NA"),
  fileEncoding = "UTF-8"
)
if (!"fold" %in% names(stroke_baseline)) {
  stop("基线数据缺少 fold 列")
}

rsf_all <- stroke_baseline %>%
  dplyr::transmute(
    hadm_id = as.integer(.data$hadm_id),
    age = as.numeric(.data$age),
    charlson_comorbidity_index = as.numeric(.data$charlson_comorbidity_index),
    apsiii = as.numeric(.data$apsiii),
    sapsii = as.numeric(.data$sapsii),
    oasis = as.numeric(.data$oasis),
    preiculos = as.numeric(.data$preiculos),
    mechvent = factor(.data$mechvent),
    electivesurgery = factor(.data$electivesurgery),
    gender = factor(.data$gender),
    bmi = as.numeric(.data$bmi),
    stroke_type = factor(.data$stroke_type),
    group = as.integer(.data$group),
    fold = as.integer(.data$fold),
    intime = as.POSIXct(.data$intime, format = "%d/%m/%Y %H:%M:%S"),
    deathtime = as.POSIXct(.data$deathtime, format = "%d/%m/%Y %H:%M:%S"),
    death_28d = as.integer(.data$death_28d)
  ) %>%
  dplyr::mutate(
    time28 = as.numeric(difftime(.data$deathtime, .data$intime, units = "days")),
    time28 = ifelse(is.na(.data$time28), 28, pmin(.data$time28, 28)),
    status28 = as.integer(.data$death_28d == 1)
  ) %>%
  dplyr::filter(
    .data$time28 > 0,
    !is.na(.data$status28),
    .data$group == 1L,
    .data$fold %in% 1:5
  ) %>%
  dplyr::distinct(.data$hadm_id, .keep_all = TRUE) %>%
  as.data.frame()

miss_cov <- setdiff(fixed_covars, names(rsf_all))
if (length(miss_cov) > 0) {
  stop("基线数据缺少列: ", paste(miss_cov, collapse = ", "))
}

p_mtry <- length(fixed_covars)
fold_ids <- 1:5
fold_n_valid <- integer(5)
for (k in fold_ids) {
  fold_n_valid[k] <- sum(rsf_all$fold == k)
  if (fold_n_valid[k] == 0L) {
    stop("缺少 fold = ", k, " 的患者")
  }
}

cat(
  "合格训练队列 group=1 n =", nrow(rsf_all),
  "| 各折验证 n =", paste(fold_n_valid, collapse = "/"),
  "| p =", p_mtry,
  "| rf.cores =", thread_number, "\n"
)
flush.console()

# ---------------------------------------------------------------------------
# 网格（与 RSF_baseline_0824 一致，mtry 不超过 p）
# ---------------------------------------------------------------------------
tuning_grid <- expand.grid(
  ntree = c(300, 500, 1000),
  mtry = c(3, 6, 9, 12),
  nodesize = c(10, 20, 30, 40),
  nsplit = c(10, 25, 50),
  KEEP.OUT.ATTRS = FALSE,
  stringsAsFactors = FALSE
)
tuning_grid <- tuning_grid[tuning_grid$mtry <= p_mtry, , drop = FALSE]
n_grid <- nrow(tuning_grid)
tuning_metric_rows <- vector("list", n_grid)
fold_metric_rows <- list()
cat(
  "网格组合数 =", n_grid,
  "| 每组 5 折，总拟合次数 =", n_grid * 5L, "\n"
)
flush.console()

na_fold_row <- function(i, k, ntree_i, mtry_i, nodesize_i, nsplit_i) {
  data.frame(
    combo = i,
    ntree = ntree_i,
    mtry = mtry_i,
    nodesize = nodesize_i,
    nsplit = nsplit_i,
    ncores = thread_number,
    seed = seed_value,
    fold = k,
    fold5_cindex = NA_real_,
    fold5_auc = NA_real_,
    fold5_brier_ipcw = NA_real_,
    fold5_ibs = NA_real_,
    fold5_brier = NA_real_,
    train_cindex = NA_real_,
    train_auc = NA_real_,
    train_brier_ipcw = NA_real_,
    train_ibs = NA_real_,
    train_brier = NA_real_,
    stringsAsFactors = FALSE
  )
}

na_mean_row <- function(i, ntree_i, mtry_i, nodesize_i, nsplit_i) {
  data.frame(
    combo = i,
    ntree = ntree_i,
    mtry = mtry_i,
    nodesize = nodesize_i,
    nsplit = nsplit_i,
    ncores = thread_number,
    seed = seed_value,
    fold5_cindex = NA_real_,
    fold5_auc = NA_real_,
    fold5_brier_ipcw = NA_real_,
    fold5_ibs = NA_real_,
    fold5_brier = NA_real_,
    train_cindex = NA_real_,
    train_auc = NA_real_,
    train_brier_ipcw = NA_real_,
    train_ibs = NA_real_,
    train_brier = NA_real_,
    stringsAsFactors = FALSE
  )
}

tune_start <- Sys.time()
for (i in seq_len(n_grid)) {
  iter_start <- Sys.time()
  ntree_i <- as.integer(tuning_grid$ntree[i])
  mtry_i <- as.integer(tuning_grid$mtry[i])
  nodesize_i <- as.integer(tuning_grid$nodesize[i])
  nsplit_i <- as.integer(tuning_grid$nsplit[i])
  combo_fold_rows <- vector("list", 5)

  for (k in fold_ids) {
    row_k <- na_fold_row(i, k, ntree_i, mtry_i, nodesize_i, nsplit_i)
    train_data <- rsf_all[rsf_all$fold != k, model_cols, drop = FALSE]
    valid_data <- rsf_all[rsf_all$fold == k, model_cols, drop = FALSE]

    cat(sprintf("  combo %d fold %d 开始拟合...\n", i, k))
    flush.console()
    rsf_i <- tryCatch(
      randomForestSRC::rfsrc(
        Surv(time28, status28) ~ .,
        data = train_data,
        ntree = ntree_i,
        mtry = mtry_i,
        nodesize = nodesize_i,
        nsplit = nsplit_i,
        splitrule = "logrank",
        na.action = "na.impute",
        importance = FALSE,
        seed = seed_value
      ),
      error = function(e) {
        cat("  rfsrc 失败:", conditionMessage(e), "\n")
        flush.console()
        NULL
      }
    )

    if (!is.null(rsf_i)) {
      train_probability <- tryCatch(
        prob_at_28(rsf_i$survival.oob, rsf_i$time.interest),
        error = function(e) NULL
      )
      valid_prediction <- tryCatch(
        predict(rsf_i, newdata = valid_data, na.action = "na.impute"),
        error = function(e) {
          cat("  验证 predict 失败:", conditionMessage(e), "\n")
          flush.console()
          NULL
        }
      )
      valid_probability <- if (is.null(valid_prediction)) {
        NULL
      } else {
        prob_at_28(valid_prediction$survival, valid_prediction$time.interest)
      }

      if (!is.null(train_probability)) {
        row_k$train_cindex <- cindex_from_prob(
          train_data$time28, train_data$status28, train_probability
        )
        row_k$train_brier <- ordinary_brier(train_data$status28, train_probability)
      }
      if (!is.null(valid_probability)) {
        row_k$fold5_cindex <- cindex_from_prob(
          valid_data$time28, valid_data$status28, valid_probability
        )
        row_k$fold5_brier <- ordinary_brier(valid_data$status28, valid_probability)
      }

      train_score_vals <- score_rsf(rsf_i, train_data)
      valid_score_vals <- score_rsf(rsf_i, valid_data)
      row_k$train_auc <- train_score_vals$auc
      row_k$train_brier_ipcw <- train_score_vals$brier
      row_k$train_ibs <- train_score_vals$ibs
      row_k$fold5_auc <- valid_score_vals$auc
      row_k$fold5_brier_ipcw <- valid_score_vals$brier
      row_k$fold5_ibs <- valid_score_vals$ibs
    }

    combo_fold_rows[[k]] <- row_k
    fold_metric_rows[[length(fold_metric_rows) + 1L]] <- row_k
    fold_so_far <- do.call(rbind, fold_metric_rows)
    write.csv(fold_so_far, fold_result_path, row.names = FALSE, fileEncoding = "UTF-8")
  }

  combo_fold_tab <- do.call(rbind, combo_fold_rows)
  row_i <- na_mean_row(i, ntree_i, mtry_i, nodesize_i, nsplit_i)
  row_i$fold5_cindex <- mean_na(combo_fold_tab$fold5_cindex)
  row_i$fold5_auc <- mean_na(combo_fold_tab$fold5_auc)
  row_i$fold5_brier_ipcw <- mean_na(combo_fold_tab$fold5_brier_ipcw)
  row_i$fold5_ibs <- mean_na(combo_fold_tab$fold5_ibs)
  row_i$fold5_brier <- mean_na(combo_fold_tab$fold5_brier)
  row_i$train_cindex <- mean_na(combo_fold_tab$train_cindex)
  row_i$train_auc <- mean_na(combo_fold_tab$train_auc)
  row_i$train_brier_ipcw <- mean_na(combo_fold_tab$train_brier_ipcw)
  row_i$train_ibs <- mean_na(combo_fold_tab$train_ibs)
  row_i$train_brier <- mean_na(combo_fold_tab$train_brier)

  tuning_metric_rows[[i]] <- row_i
  tuning_so_far <- do.call(rbind, tuning_metric_rows[seq_len(i)])
  write.csv(tuning_so_far, result_path, row.names = FALSE, fileEncoding = "UTF-8")

  elapsed_min <- as.numeric(difftime(Sys.time(), tune_start, units = "mins"))
  remain_min <- elapsed_min / i * (n_grid - i)
  cat(sprintf(
    "[%d/%d] ntree=%d mtry=%d nodesize=%d nsplit=%d | 用时 %.1f 秒 | 已用 %.1f 分钟 | 预计剩余 %.1f 分钟 | mean valid C-index=%.4f AUC=%.4f\n",
    i, n_grid, ntree_i, mtry_i, nodesize_i, nsplit_i,
    as.numeric(difftime(Sys.time(), iter_start, units = "secs")),
    elapsed_min,
    remain_min,
    ifelse(is.na(row_i$fold5_cindex), NaN, row_i$fold5_cindex),
    ifelse(is.na(row_i$fold5_auc), NaN, row_i$fold5_auc)
  ))
  flush.console()
}

tuning_results <- do.call(rbind, tuning_metric_rows)
print(tuning_results, digits = 4, row.names = FALSE)
write.csv(tuning_results, result_path, row.names = FALSE, fileEncoding = "UTF-8")
cat("五折均值表已写入:", result_path, "\n")
cat("五折明细表已写入:", fold_result_path, "\n")
flush.console()

# ---------------------------------------------------------------------------
# 按验证集 C-index 五折均值选优
# ---------------------------------------------------------------------------
if (all(is.na(tuning_results$fold5_cindex))) {
  warning("全部组合 fold5_cindex 均为 NA，未写出最优表")
} else {
  best_row <- which.max(tuning_results$fold5_cindex)
  best_ntree <- as.integer(tuning_results$ntree[best_row])
  best_mtry <- as.integer(tuning_results$mtry[best_row])
  best_nodesize <- as.integer(tuning_results$nodesize[best_row])
  best_nsplit <- as.integer(tuning_results$nsplit[best_row])
  cat(sprintf(
    "按五折验证 C-index 均值选定 combo %d：ntree=%d mtry=%d nodesize=%d nsplit=%d\n",
    as.integer(tuning_results$combo[best_row]),
    best_ntree, best_mtry, best_nodesize, best_nsplit
  ))
  flush.console()

  best_parameters <- data.frame(
    parameter = c("combo", "ntree", "mtry", "nodesize", "nsplit"),
    value = c(
      as.integer(tuning_results$combo[best_row]),
      best_ntree, best_mtry, best_nodesize, best_nsplit
    ),
    stringsAsFactors = FALSE
  )
  best_performance <- data.frame(
    metric = c(
      "C-index",
      "28-day AUC",
      "28-day Brier (IPCW)",
      "IBS 0-28 days (IPCW)",
      "28-day Brier (ordinary)"
    ),
    training_set = round(
      c(
        tuning_results$train_cindex[best_row],
        tuning_results$train_auc[best_row],
        tuning_results$train_brier_ipcw[best_row],
        tuning_results$train_ibs[best_row],
        tuning_results$train_brier[best_row]
      ),
      4
    ),
    test_set = round(
      c(
        tuning_results$fold5_cindex[best_row],
        tuning_results$fold5_auc[best_row],
        tuning_results$fold5_brier_ipcw[best_row],
        tuning_results$fold5_ibs[best_row],
        tuning_results$fold5_brier[best_row]
      ),
      4
    ),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  write.csv(best_parameters, best_param_path, row.names = FALSE, fileEncoding = "UTF-8")
  write.csv(best_performance, best_perf_path, row.names = FALSE, fileEncoding = "UTF-8")
  cat("最优超参：\n")
  print(best_parameters, row.names = FALSE)
  cat("最优组合 5 指标（test_set = 五折验证均值）：\n")
  print(best_performance, row.names = FALSE)
  cat("已写入:\n ", best_param_path, "\n ", best_perf_path, "\n")
  flush.console()
}
