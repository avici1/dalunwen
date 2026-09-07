# =============================================================================
# RSFLC 20 轨迹调参：完整 5 折交叉验证
# 每组超参轮流用第 k 折预测、其余 4 折训练；按验证集 C-index 五折均值选优
# 指标口径对齐试运行 RSFLC_20V；不含 VIMP / SHAP / 作图 / Word，不用 group=2
# =============================================================================
library(dplyr)
library(survival)
library(DynForest)
library(prodlim)
library(riskRegression)

# ---------------------------------------------------------------------------
# 开关与设定
# ---------------------------------------------------------------------------
t0 <- 5
times_max <- 10
seed_value <- 2026L
evaluation_horizon <- 28 - 1e-04
evaluation_times <- unique(c(seq(0, 27.5, by = 0.5), evaluation_horizon))
ncores_use <- 8L
nsplit_option <- "quantile"

base_dir <- "F:/文章_大论文/0722/实例研究代码"
out_dir <- file.path(base_dir, "5折RSFLC")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
baseline_path <- file.path(base_dir, "stroke_baseline_knn_0824_fold.csv")
longitudinal_path <- file.path(base_dir, "stroke_longitudinal_knn_0824_group_fold.csv")
result_path <- file.path(out_dir, "results_RSFLC_20V_cv.csv")
fold_result_path <- file.path(out_dir, "results_RSFLC_20V_cv_folds.csv")
best_perf_path <- file.path(out_dir, "best_performance.csv")
best_param_path <- file.path(out_dir, "best_parameters.csv")

long_vars <- c(
  "total_urine_output", "creat", "aki_stage", "gcs", "ph", "pco2",
  "lactate", "po2", "pao2fio2ratio", "glucose", "sodium", "bicarbonate",
  "hemoglobin", "temperature", "fio2", "sofa_24hours", "cns_24hours",
  "renal_24hours", "cardiovascular_24hours", "respiration_24hours"
)
fixed_covars <- c(
  "age", "charlson_comorbidity_index", "apsiii", "sapsii", "oasis",
  "preiculos", "mechvent", "electivesurgery", "gender", "bmi", "stroke_type"
)

timeVarModel_20 <- stats::setNames(
  lapply(long_vars, function(v) {
    list(fixed = stats::as.formula(paste(v, "~ 1")), random = ~ time)
  }),
  long_vars
)

# ---------------------------------------------------------------------------
# 工具函数
# ---------------------------------------------------------------------------
fill_missing_like_train <- function(newdata, template) {
  out <- newdata
  for (nm in names(template)) {
    if (is.numeric(template[[nm]])) {
      fill_value <- median(template[[nm]], na.rm = TRUE)
      if (!is.finite(fill_value)) fill_value <- 0
      out[[nm]][is.na(out[[nm]])] <- fill_value
    } else {
      fill_value <- names(sort(table(template[[nm]]), decreasing = TRUE))[1]
      if (length(fill_value) == 0 || is.na(fill_value)) {
        fill_value <- if (is.factor(template[[nm]])) levels(template[[nm]])[1] else "missing"
      }
      if (is.factor(template[[nm]])) {
        out[[nm]] <- factor(as.character(out[[nm]]), levels = levels(template[[nm]]))
      }
      out[[nm]][is.na(out[[nm]])] <- fill_value
      if (is.factor(template[[nm]])) {
        out[[nm]] <- factor(out[[nm]], levels = levels(template[[nm]]))
      }
    }
  }
  out
}

subset_by_ids <- function(timeData, fixedData, y_df, outcome_df, ids) {
  ids <- unique(as.integer(ids))
  list(
    timeData = timeData[timeData$hadm_id %in% ids, , drop = FALSE],
    fixedData = fixedData[fixedData$hadm_id %in% ids, , drop = FALSE],
    y_df = y_df[y_df$hadm_id %in% ids, , drop = FALSE],
    outcome = outcome_df[outcome_df$hadm_id %in% ids, , drop = FALSE]
  )
}

build_cv_packs <- function(train_ids, valid_ids) {
  stopifnot(length(train_ids) > 0L, length(valid_ids) > 0L)
  stopifnot(length(intersect(train_ids, valid_ids)) == 0L)
  train_raw <- subset_by_ids(timeData_all, fixedData_all, y_all, outcome_all, train_ids)
  valid_raw <- subset_by_ids(timeData_all, fixedData_all, y_all, outcome_all, valid_ids)
  train_template <- train_raw$fixedData[, fixed_covars, drop = FALSE]
  train_fixed <- train_raw$fixedData
  train_fixed[, fixed_covars] <- fill_missing_like_train(
    train_fixed[, fixed_covars, drop = FALSE],
    train_template
  )
  valid_fixed <- valid_raw$fixedData
  valid_fixed[, fixed_covars] <- fill_missing_like_train(
    valid_fixed[, fixed_covars, drop = FALSE],
    train_fixed[, fixed_covars, drop = FALSE]
  )
  list(
    train_pack = list(
      timeData = train_raw$timeData,
      fixedData = train_fixed,
      y_df = train_raw$y_df,
      outcome = train_raw$outcome
    ),
    valid_pack = list(
      timeData = valid_raw$timeData,
      fixedData = valid_fixed,
      y_df = valid_raw$y_df,
      outcome = valid_raw$outcome
    )
  )
}

mean_na <- function(x) {
  if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE)
}

fit_dynforest <- function(timeData, fixedData, y_df, ntree, mtry, nodesize, ncores, seed) {
  do.call(DynForest::dynforest, list(
    timeData      = timeData,
    fixedData     = fixedData,
    timeVar       = "time",
    idVar         = "hadm_id",
    timeVarModel  = timeVarModel_20,
    Y             = list(type = "factor", Y = y_df),
    mtry          = as.integer(mtry),
    nodesize      = as.integer(nodesize),
    ntree         = as.integer(ntree),
    nsplit_option = nsplit_option,
    ncores        = as.integer(ncores),
    seed          = as.integer(seed),
    verbose       = TRUE
  ))
}

predict_prob_dead <- function(model, timeData, fixedData, t0_value = t0) {
  time_var <- if (!is.null(model$timeVar)) model$timeVar else "time"
  pred_dyn <- predict(
    object    = model,
    timeData  = timeData,
    fixedData = fixedData,
    idVar     = "hadm_id",
    timeVar   = time_var,
    t0        = t0_value
  )
  dead_label <- if (!is.null(model$levels) && "dead" %in% model$levels) {
    "dead"
  } else if (!is.null(model$levels)) {
    model$levels[length(model$levels)]
  } else {
    "dead"
  }
  pred_class <- unname(pred_dyn$pred_indiv)
  proba <- as.numeric(unname(pred_dyn$pred_indiv_proba))
  data.frame(
    hadm_id = as.integer(names(pred_dyn$pred_indiv)),
    probability = ifelse(pred_class == dead_label, proba, 1 - proba),
    stringsAsFactors = FALSE
  )
}

eval_metrics <- function(pred_df, outcome_df) {
  merged <- merge(pred_df, outcome_df, by = "hadm_id", sort = FALSE)
  merged <- merged[is.finite(merged$probability) & !is.na(merged$status28), , drop = FALSE]
  if (nrow(merged) == 0) {
    return(list(cindex = NA_real_, brier = NA_real_, n = 0L, data = merged))
  }
  cindex_val <- survival::concordance(
    survival::Surv(merged$time28, merged$status28) ~ merged$probability,
    reverse = TRUE
  )$concordance
  brier_val <- mean((as.numeric(merged$status28) - merged$probability)^2)
  list(
    cindex = as.numeric(cindex_val),
    brier = brier_val,
    n = nrow(merged),
    data = merged
  )
}

build_constant_risk_matrix <- function(probability, times) {
  n <- length(probability)
  mat <- matrix(
    rep(as.numeric(probability), times = length(times)),
    nrow = n,
    ncol = length(times),
    byrow = FALSE
  )
  colnames(mat) <- as.character(times)
  mat
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

empty_metrics <- function() {
  list(
    cindex = NA_real_,
    auc = NA_real_,
    brier_ipcw = NA_real_,
    ibs = NA_real_,
    brier_ordinary = NA_real_,
    n = 0L
  )
}

score_pack <- function(pred_eval) {
  out <- empty_metrics()
  if (is.null(pred_eval) || nrow(pred_eval) == 0) return(out)
  out$n <- nrow(pred_eval)

  ### 1. C-index
  # 与 新RSF.R 测试集 / 试运行一致：concordance(Surv ~ probability, reverse=TRUE)
  out$cindex <- as.numeric(survival::concordance(
    survival::Surv(pred_eval$time28, pred_eval$status28) ~ pred_eval$probability,
    reverse = TRUE
  )$concordance)

  score_data <- data.frame(
    time28 = pred_eval$time28,
    status28 = pred_eval$status28,
    stringsAsFactors = FALSE
  )
  risk_mat <- build_constant_risk_matrix(pred_eval$probability, evaluation_times)
  score_obj <- tryCatch(
    riskRegression::Score(
      object = list(RSFLC = risk_mat),
      formula = Hist(time28, status28) ~ 1,
      data = score_data,
      metrics = c("auc", "brier"),
      times = evaluation_times,
      summary = "ibs",
      cens.method = "ipcw",
      conf.int = FALSE,
      plots = NULL
    ),
    error = function(e) NULL
  )
  if (!is.null(score_obj)) {
    vals <- extract_score_at_horizon(score_obj, "RSFLC", evaluation_horizon)
    ### 2. 28-day AUC（Score 时间依赖 AUC，t=27.9999；失败保持 NA，不回退 pROC）
    out$auc <- vals$auc
    ### 3. 28-day Brier (IPCW)
    out$brier_ipcw <- vals$brier
    ### 4. IBS 0-28 days (IPCW)
    out$ibs <- vals$ibs
  }

  ### 5. 28-day Brier (ordinary)
  out$brier_ordinary <- mean(
    (as.numeric(pred_eval$status28) - pred_eval$probability)^2
  )

  out
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
stroke_longitudinal <- read.csv(
  longitudinal_path,
  stringsAsFactors = FALSE,
  na.strings = c("", "NA"),
  fileEncoding = "UTF-8"
)

miss_long <- setdiff(long_vars, names(stroke_longitudinal))
if (length(miss_long) > 0) {
  stop("纵向数据缺少列: ", paste(miss_long, collapse = ", "))
}
if (!"fold" %in% names(stroke_baseline)) {
  stop("基线数据缺少 fold 列")
}

timeData_all <- stroke_longitudinal %>%
  dplyr::filter(.data$times <= times_max) %>%
  dplyr::transmute(
    hadm_id = as.integer(.data$hadm_id),
    time = as.integer(.data$times),
    dplyr::across(dplyr::all_of(long_vars), as.numeric)
  ) %>%
  dplyr::filter(dplyr::if_all(dplyr::all_of(long_vars), is.finite)) %>%
  dplyr::group_by(.data$hadm_id) %>%
  dplyr::filter(dplyr::n() >= 2) %>%
  dplyr::ungroup() %>%
  as.data.frame()

valid_ids <- unique(timeData_all$hadm_id)

baseline_all <- stroke_baseline %>%
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
    .data$hadm_id %in% valid_ids,
    .data$time28 > 0,
    !is.na(.data$status28),
    .data$group == 1L,
    .data$fold %in% 1:5
  ) %>%
  dplyr::distinct(.data$hadm_id, .keep_all = TRUE) %>%
  as.data.frame()

keep_ids <- unique(baseline_all$hadm_id)
timeData_all <- timeData_all[timeData_all$hadm_id %in% keep_ids, , drop = FALSE]
keep_ids <- intersect(unique(timeData_all$hadm_id), keep_ids)
baseline_all <- baseline_all[baseline_all$hadm_id %in% keep_ids, , drop = FALSE]
timeData_all <- timeData_all[timeData_all$hadm_id %in% keep_ids, , drop = FALSE]

fixedData_all <- baseline_all[, c("hadm_id", fixed_covars), drop = FALSE]
y_all <- baseline_all %>%
  dplyr::transmute(
    hadm_id = as.integer(.data$hadm_id),
    event = factor(.data$death_28d, levels = c(0, 1), labels = c("alive", "dead"))
  ) %>%
  as.data.frame()
outcome_all <- baseline_all[, c("hadm_id", "time28", "status28", "death_28d", "fold"), drop = FALSE]

stopifnot(
  identical(sort(unique(timeData_all$hadm_id)), sort(fixedData_all$hadm_id)),
  identical(sort(fixedData_all$hadm_id), sort(y_all$hadm_id)),
  all(colSums(is.na(timeData_all[, long_vars, drop = FALSE])) == 0)
)

p_mtry <- length(long_vars) + length(fixed_covars)
fold_ids <- 1:5
fold_packs <- vector("list", 5)
fold_n_valid <- integer(5)
for (k in fold_ids) {
  train_ids_k <- sort(unique(outcome_all$hadm_id[outcome_all$fold != k]))
  valid_ids_k <- sort(unique(outcome_all$hadm_id[outcome_all$fold == k]))
  if (length(valid_ids_k) == 0L) {
    stop("缺少 fold = ", k, " 的患者")
  }
  fold_packs[[k]] <- build_cv_packs(train_ids_k, valid_ids_k)
  fold_n_valid[k] <- nrow(fold_packs[[k]]$valid_pack$fixedData)
}

cat(
  "合格训练队列 group=1 n =", nrow(outcome_all),
  "| 各折验证 n =", paste(fold_n_valid, collapse = "/"),
  "| p =", p_mtry, "\n"
)
flush.console()

# ---------------------------------------------------------------------------
# 网格
# ---------------------------------------------------------------------------
tuning_grid <- expand.grid(
  ntree = c(50, 100, 200),
  mtry = c(3, 6, 9, 12),
  nodesize = c(1, 3, 5),
  KEEP.OUT.ATTRS = FALSE,
  stringsAsFactors = FALSE
)
tuning_grid <- tuning_grid[tuning_grid$mtry <= p_mtry, , drop = FALSE]
n_grid <- nrow(tuning_grid)
tuning_metric_rows <- vector("list", n_grid)
fold_metric_rows <- list()
cat(
  "网格组合数 =", n_grid,
  "| 每组 5 折，总拟合次数 =", n_grid * 5L,
  "| ncores =", ncores_use, "\n"
)
flush.console()

na_fold_row <- function(i, k, ntree_i, mtry_i, nodesize_i) {
  data.frame(
    combo = i,
    ntree = ntree_i,
    mtry = mtry_i,
    nodesize = nodesize_i,
    t0 = t0,
    nsplit_option = nsplit_option,
    ncores = ncores_use,
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

na_mean_row <- function(i, ntree_i, mtry_i, nodesize_i) {
  data.frame(
    combo = i,
    ntree = ntree_i,
    mtry = mtry_i,
    nodesize = nodesize_i,
    t0 = t0,
    nsplit_option = nsplit_option,
    ncores = ncores_use,
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
  combo_fold_rows <- vector("list", 5)

  for (k in fold_ids) {
    row_k <- na_fold_row(i, k, ntree_i, mtry_i, nodesize_i)
    train_pack <- fold_packs[[k]]$train_pack
    valid_pack <- fold_packs[[k]]$valid_pack

    cat(sprintf("  combo %d fold %d 开始拟合...\n", i, k))
    flush.console()
    dyn_i <- tryCatch(
      fit_dynforest(
        timeData = train_pack$timeData,
        fixedData = train_pack$fixedData,
        y_df = train_pack$y_df,
        ntree = ntree_i,
        mtry = mtry_i,
        nodesize = nodesize_i,
        ncores = ncores_use,
        seed = seed_value
      ),
      error = function(e) {
        cat("  dynforest 失败:", conditionMessage(e), "\n")
        flush.console()
        NULL
      }
    )

    if (!is.null(dyn_i)) {
      train_pred <- tryCatch(
        predict_prob_dead(dyn_i, train_pack$timeData, train_pack$fixedData, t0),
        error = function(e) {
          cat("  训练 predict 失败:", conditionMessage(e), "\n")
          flush.console()
          NULL
        }
      )
      valid_pred <- tryCatch(
        predict_prob_dead(dyn_i, valid_pack$timeData, valid_pack$fixedData, t0),
        error = function(e) {
          cat("  验证 predict 失败:", conditionMessage(e), "\n")
          flush.console()
          NULL
        }
      )
      train_eval <- if (is.null(train_pred)) {
        NULL
      } else {
        eval_metrics(train_pred, train_pack$outcome)$data
      }
      valid_eval <- if (is.null(valid_pred)) {
        NULL
      } else {
        eval_metrics(valid_pred, valid_pack$outcome)$data
      }
      train_scores <- score_pack(train_eval)
      valid_scores <- score_pack(valid_eval)
      row_k$fold5_cindex <- valid_scores$cindex
      row_k$fold5_auc <- valid_scores$auc
      row_k$fold5_brier_ipcw <- valid_scores$brier_ipcw
      row_k$fold5_ibs <- valid_scores$ibs
      row_k$fold5_brier <- valid_scores$brier_ordinary
      row_k$train_cindex <- train_scores$cindex
      row_k$train_auc <- train_scores$auc
      row_k$train_brier_ipcw <- train_scores$brier_ipcw
      row_k$train_ibs <- train_scores$ibs
      row_k$train_brier <- train_scores$brier_ordinary
    }

    combo_fold_rows[[k]] <- row_k
    fold_metric_rows[[length(fold_metric_rows) + 1L]] <- row_k
    fold_so_far <- do.call(rbind, fold_metric_rows)
    write.csv(fold_so_far, fold_result_path, row.names = FALSE, fileEncoding = "UTF-8")
  }

  combo_fold_tab <- do.call(rbind, combo_fold_rows)
  row_i <- na_mean_row(i, ntree_i, mtry_i, nodesize_i)
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
    "[%d/%d] ntree=%d mtry=%d nodesize=%d | 用时 %.1f 秒 | 已用 %.1f 分钟 | 预计剩余 %.1f 分钟 | mean valid C-index=%.4f AUC=%.4f\n",
    i, n_grid, ntree_i, mtry_i, nodesize_i,
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
  cat(sprintf(
    "按五折验证 C-index 均值选定 combo %d：ntree=%d mtry=%d nodesize=%d\n",
    as.integer(tuning_results$combo[best_row]),
    best_ntree, best_mtry, best_nodesize
  ))
  flush.console()

  best_parameters <- data.frame(
    parameter = c("combo", "ntree", "mtry", "nodesize", "t0"),
    value = c(
      as.integer(tuning_results$combo[best_row]),
      best_ntree, best_mtry, best_nodesize, t0
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
