# =============================================================================
# RSFLC 超参数筛选（20 条纵向轨迹 + 11 个基线协变量）
# 纵向：stroke_longitudinal_knn_0824_group_fold.csv
# 划分以该文件 group 列为准：group=1 全体训练，group=2（约 30%）外验证
# 按验证集 C-index 选优
# 与 0826_RSFLC结果输出.R 共用同一套纵向定义、划分与混合模型设定
#
# 注意：本脚本用外验证集选参。完整五折网格（不用 group=2）见同目录：
#   RSFLC_20V_tune_fold5.R
# 文章最终口径（3 条轨迹 + 28 个固定变量）的五折搜索见：
#   ../../unified_pipeline/07b_rsflc_fivefold_cv_optional.R
# =============================================================================
library(dplyr)
library(survival)
library(DynForest)
library(pROC)

# ---------------------------------------------------------------------------
# 开关
# RUN_OOB：每组调用 compute_ooberror（较慢，失败则记 NA）
# TUNING_MAX_N：调参时训练集最大人数，NULL 为全队列；粗搜可设 1200
# ---------------------------------------------------------------------------
RUN_OOB <- TRUE
TUNING_MAX_N <- NULL

t0 <- 5
seed_value <- 2026L
ncores_use <- 8L

base_dir <- "F:/文章_大论文/0722/实例研究代码"
out_dir <- file.path(base_dir, "执行")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
baseline_path <- file.path(base_dir, "stroke_baseline_knn_0824_fold.csv")
longitudinal_path <- file.path(base_dir, "stroke_longitudinal_knn_0824_group_fold.csv")
result_path <- file.path(out_dir, "results_RSFLC.csv")
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

# random=~1 会触发 DynForest 内部 bug；固定截距 + 随机时间斜率
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
    nsplit_option = "quantile",
    ncores        = as.integer(ncores),
    seed          = as.integer(seed),
    verbose       = TRUE
  ))
}

as_dead_num <- function(y) {
  if (is.null(y)) {
    return(numeric(0))
  }
  if (is.factor(y)) {
    lev <- levels(y)
    if ("dead" %in% lev) {
      return(as.numeric(y == "dead"))
    }
    if (length(lev) >= 2L) {
      return(as.numeric(y == lev[length(lev)]))
    }
    return(as.numeric(as.integer(y) == max(as.integer(y), na.rm = TRUE)))
  }
  y_chr <- as.character(y)
  out <- rep(NA_real_, length(y_chr))
  out[y_chr %in% c("dead", "Dead", "TRUE", "true")] <- 1
  out[y_chr %in% c("alive", "Alive", "FALSE", "false")] <- 0
  num <- suppressWarnings(as.numeric(y_chr))
  fill <- is.na(out) & is.finite(num)
  out[fill] <- as.numeric(num[fill] > 0)
  out
}

leaf_death_rate <- function(tree) {
  if (is.null(tree$leaves) || is.null(tree$idY) || is.null(tree$Y$Y) || is.null(tree$Y$id)) {
    return(NULL)
  }
  y_boot <- tree$Y$Y[match(tree$idY, tree$Y$id)]
  dead <- as_dead_num(y_boot)
  leaves <- as.character(tree$leaves)
  ok <- !is.na(leaves) & leaves != "0" & !is.na(dead)
  if (!any(ok)) {
    return(NULL)
  }
  tapply(dead[ok], leaves[ok], mean)
}

predict_prob_dead <- function(model, timeData, fixedData, t0_value = t0, verbose = FALSE) {
  time_var <- if (!is.null(model$timeVar)) model$timeVar else "time"
  pred_dyn <- predict(
    object    = model,
    timeData  = timeData,
    fixedData = fixedData,
    idVar     = "hadm_id",
    timeVar   = time_var,
    t0        = t0_value
  )

  pred_leaf <- pred_dyn$pred_leaf
  ids <- as.integer(names(pred_dyn$pred_indiv))
  probability <- NULL

  if (!is.null(pred_leaf) && !is.null(model$rf)) {
    n_tree <- ncol(model$rf)
    if (ncol(pred_leaf) == n_tree && nrow(pred_leaf) != n_tree) {
      pred_leaf <- t(pred_leaf)
    }
    n_id <- ncol(pred_leaf)
    if (length(ids) != n_id) {
      ids <- as.integer(unique(fixedData$hadm_id))
      if (length(ids) != n_id) {
        ids <- seq_len(n_id)
      }
    }
    pmat <- matrix(NA_real_, n_tree, n_id)
    for (t in seq_len(n_tree)) {
      rates <- leaf_death_rate(model$rf[, t])
      if (is.null(rates) || length(rates) == 0) {
        next
      }
      pmat[t, ] <- unname(rates[as.character(pred_leaf[t, ])])
    }
    probability <- colMeans(pmat, na.rm = TRUE)
    probability[!is.finite(probability)] <- NA_real_
  }

  if (is.null(probability) || all(!is.finite(probability))) {
    dead_label <- if (!is.null(model$levels) && "dead" %in% model$levels) {
      "dead"
    } else if (!is.null(model$levels)) {
      model$levels[length(model$levels)]
    } else {
      "dead"
    }
    pred_class <- as.character(unname(pred_dyn$pred_indiv))
    pred_class[pred_class %in% c("0", "NA")] <- NA_character_
    proba <- as.numeric(unname(pred_dyn$pred_indiv_proba))
    ids <- as.integer(names(pred_dyn$pred_indiv))
    probability <- ifelse(pred_class == dead_label, proba, 1 - proba)
  }

  if (isTRUE(verbose)) {
    okp <- is.finite(probability)
    cat(sprintf(
      "死亡概率: n=%d | min=%.4f median=%.4f max=%.4f | 唯一值个数=%d\n",
      sum(okp),
      if (any(okp)) min(probability[okp]) else NA_real_,
      if (any(okp)) stats::median(probability[okp]) else NA_real_,
      if (any(okp)) max(probability[okp]) else NA_real_,
      length(unique(round(probability[okp], 6)))
    ))
    flush.console()
  }

  data.frame(
    hadm_id = ids,
    probability = as.numeric(probability),
    stringsAsFactors = FALSE
  )
}

eval_metrics <- function(pred_df, outcome_df) {
  merged <- merge(pred_df, outcome_df, by = "hadm_id", sort = FALSE)
  merged <- merged[is.finite(merged$probability) & !is.na(merged$status28), , drop = FALSE]
  if (nrow(merged) == 0) {
    return(list(auc = NA_real_, cindex = NA_real_, brier = NA_real_, n = 0L, data = merged))
  }
  auc_val <- NA_real_
  if (length(unique(merged$status28)) >= 2L) {
    auc_val <- as.numeric(pROC::auc(pROC::roc(
      response = merged$status28,
      predictor = merged$probability,
      levels = c(0, 1),
      direction = "<",
      quiet = TRUE
    )))
  }
  cindex_val <- survival::concordance(
    survival::Surv(merged$time28, merged$status28) ~ merged$probability,
    reverse = TRUE
  )$concordance
  brier_val <- mean((as.numeric(merged$status28) - merged$probability)^2)
  list(
    auc = auc_val,
    cindex = as.numeric(cindex_val),
    brier = brier_val,
    n = nrow(merged),
    data = merged
  )
}

# ---------------------------------------------------------------------------
# 读数：数据已预先处理好，此处只做格式转换与 ID 对齐
# 划分以纵向表 group 列为准（不用 fold）
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
if (!"group" %in% names(stroke_longitudinal)) {
  stop("纵向数据缺少 group 列: ", longitudinal_path)
}

long_group <- stroke_longitudinal %>%
  dplyr::transmute(
    hadm_id = as.integer(.data$hadm_id),
    group = as.integer(.data$group)
  ) %>%
  dplyr::filter(!is.na(.data$hadm_id), .data$group %in% c(1L, 2L)) %>%
  dplyr::distinct(.data$hadm_id, .data$group)
dup_group <- long_group %>%
  dplyr::count(.data$hadm_id) %>%
  dplyr::filter(.data$n > 1L)
if (nrow(dup_group) > 0) {
  stop("纵向数据中存在同一 hadm_id 对应多个 group")
}

timeData_all <- stroke_longitudinal %>%
  dplyr::transmute(
    hadm_id = as.integer(.data$hadm_id),
    time = as.integer(.data$times),
    dplyr::across(dplyr::all_of(long_vars), as.numeric)
  ) %>%
  as.data.frame()

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
    intime = as.POSIXct(.data$intime, format = "%d/%m/%Y %H:%M:%S"),
    deathtime = as.POSIXct(.data$deathtime, format = "%d/%m/%Y %H:%M:%S"),
    death_28d = as.integer(.data$death_28d)
  ) %>%
  dplyr::inner_join(long_group, by = "hadm_id") %>%
  dplyr::mutate(
    time28 = as.numeric(difftime(.data$deathtime, .data$intime, units = "days")),
    time28 = ifelse(is.na(.data$time28), 28, pmin(.data$time28, 28)),
    status28 = as.integer(.data$death_28d == 1)
  ) %>%
  dplyr::distinct(.data$hadm_id, .keep_all = TRUE) %>%
  as.data.frame()

ids_both <- intersect(unique(timeData_all$hadm_id), unique(baseline_all$hadm_id))
timeData_all <- timeData_all[timeData_all$hadm_id %in% ids_both, , drop = FALSE]
baseline_all <- baseline_all[baseline_all$hadm_id %in% ids_both, , drop = FALSE]

fixedData_all <- baseline_all[, c("hadm_id", fixed_covars), drop = FALSE]
y_all <- baseline_all %>%
  dplyr::transmute(
    hadm_id = as.integer(.data$hadm_id),
    event = factor(.data$death_28d, levels = c(0, 1), labels = c("alive", "dead"))
  ) %>%
  as.data.frame()
outcome_all <- baseline_all[, c("hadm_id", "time28", "status28", "death_28d"), drop = FALSE]

stopifnot(
  identical(sort(unique(timeData_all$hadm_id)), sort(fixedData_all$hadm_id)),
  identical(sort(fixedData_all$hadm_id), sort(y_all$hadm_id))
)

cat(
  "患者 n =", nrow(fixedData_all),
  "| 纵向行数 =", nrow(timeData_all),
  "| 28d 死亡率 =", round(mean(outcome_all$status28), 3), "\n"
)
flush.console()

# ---------------------------------------------------------------------------
# 划分：纵向表 group=1 全体训练；group=2（约 30%）验证
# ---------------------------------------------------------------------------
stopifnot("group" %in% names(baseline_all))
train_ids <- sort(unique(baseline_all$hadm_id[baseline_all$group == 1L]))
test_ids <- sort(unique(baseline_all$hadm_id[baseline_all$group == 2L]))
stopifnot(length(train_ids) > 0L, length(test_ids) > 0L)
stopifnot(length(intersect(train_ids, test_ids)) == 0L)

train_raw <- subset_by_ids(timeData_all, fixedData_all, y_all, outcome_all, train_ids)
test_raw <- subset_by_ids(timeData_all, fixedData_all, y_all, outcome_all, test_ids)

train_template <- train_raw$fixedData[, fixed_covars, drop = FALSE]
train_fixed <- train_raw$fixedData
train_fixed[, fixed_covars] <- fill_missing_like_train(
  train_fixed[, fixed_covars, drop = FALSE],
  train_template
)
test_fixed <- test_raw$fixedData
test_fixed[, fixed_covars] <- fill_missing_like_train(
  test_fixed[, fixed_covars, drop = FALSE],
  train_fixed[, fixed_covars, drop = FALSE]
)

train_pack <- list(
  timeData = train_raw$timeData,
  fixedData = train_fixed,
  y_df = train_raw$y_df,
  outcome = train_raw$outcome
)
test_pack <- list(
  timeData = test_raw$timeData,
  fixedData = test_fixed,
  y_df = test_raw$y_df,
  outcome = test_raw$outcome
)

n_train_dead <- sum(train_pack$outcome$status28 == 1L)
n_test_dead <- sum(test_pack$outcome$status28 == 1L)
cat(sprintf(
  paste0(
    "数据: %s | 划分: group=1 全体训练，group=2 验证 | ncores=%d\n",
    "训练集 n=%d 死亡=%d (%.1f%%) | 验证集 n=%d 死亡=%d (%.1f%%)\n"
  ),
  basename(longitudinal_path),
  ncores_use,
  nrow(train_pack$fixedData), n_train_dead,
  100 * mean(train_pack$outcome$status28),
  nrow(test_pack$fixedData), n_test_dead,
  100 * mean(test_pack$outcome$status28)
))
flush.console()

tune_train <- train_pack
if (!is.null(TUNING_MAX_N) && nrow(train_pack$fixedData) > TUNING_MAX_N) {
  set.seed(seed_value)
  tune_outcome <- train_pack$outcome
  ids0 <- tune_outcome$hadm_id[tune_outcome$status28 == 0L]
  ids1 <- tune_outcome$hadm_id[tune_outcome$status28 == 1L]
  n1 <- min(length(ids1), max(50L, as.integer(round(TUNING_MAX_N * mean(tune_outcome$status28)))))
  n0 <- min(length(ids0), as.integer(TUNING_MAX_N) - n1)
  tune_ids <- c(sample(ids0, n0), sample(ids1, n1))
  tune_train <- subset_by_ids(
    train_pack$timeData,
    train_pack$fixedData,
    train_pack$y_df,
    train_pack$outcome,
    tune_ids
  )
  cat("调参子样 n =", nrow(tune_train$fixedData), "\n")
  flush.console()
}

p_mtry <- length(long_vars) + length(fixed_covars)
cat(
  "预测变量数 p =", p_mtry,
  "（", length(long_vars), "条轨迹 +", length(fixed_covars), "个固定）\n"
)
flush.console()

# ---------------------------------------------------------------------------
# 网格调参
# ntree：包默认 200，vignette/已跑通脚本常用 50
# mtry：文献要求在 1…p 中选；p=31 时 sqrt(p)≈5.6
# nodesize：默认 1；方法文常用 3；已跑通脚本用 5
# nsplit_option 固定 quantile
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
cat("网格组合数 =", n_grid, "\n")
flush.console()

tune_start <- Sys.time()
for (i in seq_len(n_grid)) {
  iter_start <- Sys.time()
  ntree_i <- tuning_grid$ntree[i]
  mtry_i <- tuning_grid$mtry[i]
  nodesize_i <- tuning_grid$nodesize[i]

  dyn_i <- tryCatch(
    fit_dynforest(
      timeData = tune_train$timeData,
      fixedData = tune_train$fixedData,
      y_df = tune_train$y_df,
      ntree = ntree_i,
      mtry = mtry_i,
      nodesize = nodesize_i,
      ncores = ncores_use,
      seed = seed_value
    ),
    error = function(e) {
      cat("  dynforest 失败:", conditionMessage(e), "\n")
      NULL
    }
  )
  fit_sec <- as.numeric(difftime(Sys.time(), iter_start, units = "secs"))
  cat(sprintf(
    "[%d/%d] 建模完成  ntree=%d mtry=%d nodesize=%d  用时 %.1f 秒\n",
    i, n_grid, ntree_i, mtry_i, nodesize_i, fit_sec
  ))
  flush.console()

  oob_error <- NA_real_
  testci <- NA_real_
  auc28 <- NA_real_
  brier28 <- NA_real_

  if (!is.null(dyn_i)) {
    if (isTRUE(RUN_OOB)) {
      oob_error <- tryCatch({
        oob_obj <- DynForest::compute_ooberror(dyn_i, ncores = ncores_use)
        mean(as.numeric(oob_obj$oob.err), na.rm = TRUE)
      }, error = function(e) {
        cat("  compute_ooberror 失败:", conditionMessage(e), "\n")
        NA_real_
      })
    }

    pred_test <- tryCatch(
      predict_prob_dead(dyn_i, test_pack$timeData, test_pack$fixedData, t0),
      error = function(e) {
        cat("  predict 失败:", conditionMessage(e), "\n")
        NULL
      }
    )
    if (!is.null(pred_test)) {
      metrics_test <- eval_metrics(pred_test, test_pack$outcome)
      testci <- metrics_test$cindex
      auc28 <- metrics_test$auc
      brier28 <- metrics_test$brier
    }
  }

  tuning_metric_rows[[i]] <- data.frame(
    combo = i,
    ntree = ntree_i,
    mtry = mtry_i,
    nodesize = nodesize_i,
    oob_error = oob_error,
    oobci = ifelse(is.na(oob_error), NA_real_, 1 - oob_error),
    testci = testci,
    AUC28 = auc28,
    Brier28 = brier28,
    stringsAsFactors = FALSE
  )

  elapsed_min <- as.numeric(difftime(Sys.time(), tune_start, units = "mins"))
  remain_min <- elapsed_min / i * (n_grid - i)
  cat(sprintf(
    "  本组合计 %.1f 秒 | 已用 %.1f 分钟 | 预计剩余 %.1f 分钟 | testci=%.4f AUC28=%.4f\n",
    as.numeric(difftime(Sys.time(), iter_start, units = "secs")),
    elapsed_min,
    remain_min,
    ifelse(is.na(testci), NaN, testci),
    ifelse(is.na(auc28), NaN, auc28)
  ))
  flush.console()
}

tuning_results <- do.call(rbind, tuning_metric_rows)
print(tuning_results, digits = 4, row.names = FALSE)
write.csv(tuning_results, result_path, row.names = FALSE)
cat("调参结果已写入:", result_path, "\n")
flush.console()

if (all(is.na(tuning_results$testci))) {
  stop("全部组合 testci 均为 NA，无法选定超参")
}

best_row <- which.max(tuning_results$testci)
best_ntree <- as.integer(tuning_results$ntree[best_row])
best_mtry <- as.integer(tuning_results$mtry[best_row])
best_nodesize <- as.integer(tuning_results$nodesize[best_row])
best_combo <- as.integer(tuning_results$combo[best_row])

best_param_table <- data.frame(
  parameter = c("combo", "ntree", "mtry", "nodesize", "t0", "testci", "AUC28", "Brier28"),
  value = c(
    best_combo, best_ntree, best_mtry, best_nodesize, t0,
    round(tuning_results$testci[best_row], 6),
    round(tuning_results$AUC28[best_row], 6),
    round(tuning_results$Brier28[best_row], 6)
  ),
  stringsAsFactors = FALSE
)
write.csv(best_param_table, best_param_path, row.names = FALSE)
cat(sprintf(
  "按测试集 C-index 选定 combo %d：ntree=%d mtry=%d nodesize=%d | testci=%.4f\n",
  best_combo, best_ntree, best_mtry, best_nodesize,
  tuning_results$testci[best_row]
))
cat("最优超参已写入:", best_param_path, "\n")
flush.console()
