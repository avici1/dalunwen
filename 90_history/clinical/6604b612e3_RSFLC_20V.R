# =============================================================================
# RSFLC 试运行：20 个纵向轨迹（与 JM 对齐）+ 现有 11 个基线协变量
# 小样本约 200 人（group=1 抽 140，group=2 抽 60），超参沿用 combo 3
# 不调参、不作 SHAP/七图/Word；输出样本表、观测次数、性能指标
# =============================================================================
library(dplyr)
library(survival)
library(DynForest)
library(prodlim)
library(riskRegression)

# ---------------------------------------------------------------------------
# 开关与超参
# ---------------------------------------------------------------------------
RUN_VIMP <- FALSE

t0 <- 5
times_max <- 10
seed_value <- 2026L
evaluation_horizon <- 28 - 1e-04
evaluation_times <- unique(c(seq(0, 27.5, by = 0.5), evaluation_horizon))

ncores_use <- 1L
best_ntree <- 200L
best_mtry <- 3L
best_nodesize <- 1L

n_train_target <- 140L
n_test_target <- 60L

base_dir <- "F:/文章_大论文/0722/实例研究代码"
trial_dir <- file.path(base_dir, "试运行")
dir.create(trial_dir, showWarnings = FALSE, recursive = TRUE)
baseline_path <- file.path(base_dir, "stroke_baseline_knn_0824.csv")
longitudinal_path <- file.path(base_dir, "stroke_longitudinal_knn_0824.csv")

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
factor_covars <- c("mechvent", "electivesurgery", "gender", "stroke_type")
numeric_covars <- setdiff(fixed_covars, factor_covars)

# random=~1 会触发 DynForest 内部 bug；沿用已跑通的 GCS 写法
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

sample_stratified_ids <- function(outcome_df, n_target) {
  ids0 <- outcome_df$hadm_id[outcome_df$status28 == 0L]
  ids1 <- outcome_df$hadm_id[outcome_df$status28 == 1L]
  n_avail <- nrow(outcome_df)
  n_use <- min(as.integer(n_target), n_avail)
  p <- mean(outcome_df$status28)
  n1 <- min(length(ids1), max(1L, as.integer(round(n_use * p))))
  n0 <- min(length(ids0), n_use - n1)
  remain <- n_use - n0 - n1
  if (remain > 0L) {
    extra0 <- min(length(ids0) - n0, remain)
    n0 <- n0 + extra0
    remain <- n_use - n0 - n1
    extra1 <- min(length(ids1) - n1, remain)
    n1 <- n1 + extra1
  }
  c(sample(ids0, n0), sample(ids1, n1))
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

permute_long_marker <- function(time_data, marker) {
  split_time <- split(time_data, time_data$hadm_id)
  perm_ids <- sample(names(split_time))
  time_perm_list <- lapply(seq_along(split_time), function(i) {
    dst <- split_time[[i]]
    src <- split_time[[perm_ids[i]]]
    n_dst <- nrow(dst)
    n_src <- nrow(src)
    dst[[marker]] <- src[[marker]][rep_len(seq_len(n_src), n_dst)]
    dst
  })
  out <- do.call(rbind, time_perm_list)
  rownames(out) <- NULL
  out
}

permutation_vimp_fallback <- function(
  model, time_data, fixed_data, outcome_df,
  long_names, numeric_names, factor_names, seed = 2026L
) {
  set.seed(seed)
  base_pred <- predict_prob_dead(model, time_data, fixed_data, t0)
  base_metrics <- eval_metrics(base_pred, outcome_df)
  base_err <- 1 - base_metrics$cindex
  rows <- list()

  for (nm in long_names) {
    time_perm <- permute_long_marker(time_data, nm)
    pred_i <- tryCatch(
      predict_prob_dead(model, time_perm, fixed_data, t0),
      error = function(e) NULL
    )
    if (is.null(pred_i)) next
    err_i <- 1 - eval_metrics(pred_i, outcome_df)$cindex
    rows[[length(rows) + 1]] <- data.frame(
      variable = nm,
      importance = as.numeric(err_i - base_err),
      group = "Longitudinal",
      stringsAsFactors = FALSE
    )
  }

  for (nm in numeric_names) {
    fd <- fixed_data
    fd[[nm]] <- sample(fd[[nm]])
    pred_i <- tryCatch(
      predict_prob_dead(model, time_data, fd, t0),
      error = function(e) NULL
    )
    if (is.null(pred_i)) next
    err_i <- 1 - eval_metrics(pred_i, outcome_df)$cindex
    rows[[length(rows) + 1]] <- data.frame(
      variable = nm,
      importance = as.numeric(err_i - base_err),
      group = "Numeric",
      stringsAsFactors = FALSE
    )
  }
  for (nm in factor_names) {
    fd <- fixed_data
    fd[[nm]] <- sample(fd[[nm]])
    pred_i <- tryCatch(
      predict_prob_dead(model, time_data, fd, t0),
      error = function(e) NULL
    )
    if (is.null(pred_i)) next
    err_i <- 1 - eval_metrics(pred_i, outcome_df)$cindex
    rows[[length(rows) + 1]] <- data.frame(
      variable = nm,
      importance = as.numeric(err_i - base_err),
      group = "Factor",
      stringsAsFactors = FALSE
    )
  }

  if (length(rows) == 0) {
    return(data.frame(
      variable = character(0),
      importance = numeric(0),
      group = character(0),
      stringsAsFactors = FALSE
    ))
  }
  out <- dplyr::bind_rows(rows)
  out[order(-out$importance, na.last = TRUE), , drop = FALSE]
}

count_obs <- function(time_data, vars) {
  vapply(vars, function(v) as.integer(sum(is.finite(time_data[[v]]))), integer(1))
}

# ---------------------------------------------------------------------------
# 读数与整理
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
    .data$group %in% c(1L, 2L)
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
outcome_all <- baseline_all[, c("hadm_id", "time28", "status28", "death_28d", "group"), drop = FALSE]

stopifnot(
  identical(sort(unique(timeData_all$hadm_id)), sort(fixedData_all$hadm_id)),
  identical(sort(fixedData_all$hadm_id), sort(y_all$hadm_id)),
  all(colSums(is.na(timeData_all[, long_vars, drop = FALSE])) == 0)
)

p_mtry <- length(long_vars) + length(fixed_covars)
stopifnot(best_mtry <= p_mtry)

cat(
  "合格全队列 n =", nrow(fixedData_all),
  "| 纵向行数 =", nrow(timeData_all),
  "| 28d 死亡率 =", round(mean(outcome_all$status28), 3),
  "| p =", p_mtry, "（", length(long_vars), "轨迹 +", length(fixed_covars), "基线）\n"
)
flush.console()

# ---------------------------------------------------------------------------
# 小样本：保持 group 划分，层内按 status28 分层
# ---------------------------------------------------------------------------
set.seed(seed_value)
pool_train <- outcome_all[outcome_all$group == 1L, , drop = FALSE]
pool_test <- outcome_all[outcome_all$group == 2L, , drop = FALSE]
stopifnot(nrow(pool_train) > 0L, nrow(pool_test) > 0L)

train_ids <- sort(sample_stratified_ids(pool_train, n_train_target))
test_ids <- sort(sample_stratified_ids(pool_test, n_test_target))
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

sample_table <- data.frame(
  dataset = c("训练集", "测试集"),
  sample_size = c(nrow(train_pack$fixedData), nrow(test_pack$fixedData)),
  events = c(sum(train_pack$outcome$status28), sum(test_pack$outcome$status28)),
  event_rate = round(
    c(mean(train_pack$outcome$status28), mean(test_pack$outcome$status28)),
    3
  ),
  stringsAsFactors = FALSE
)

obs_table <- data.frame(
  longitudinal_outcome = long_vars,
  training_observations = as.integer(count_obs(train_pack$timeData, long_vars)),
  testing_observations = as.integer(count_obs(test_pack$timeData, long_vars)),
  stringsAsFactors = FALSE
)

write.csv(
  sample_table,
  file.path(trial_dir, "20V_sample.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)
write.csv(
  obs_table,
  file.path(trial_dir, "20V_observations.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

cat("样本与事件概况：\n")
print(sample_table, row.names = FALSE)
cat("纵向观测次数（前 5 行）：\n")
print(head(obs_table, 5), row.names = FALSE)
cat(sprintf(
  "试运行超参 ntree=%d mtry=%d nodesize=%d t0=%s ncores=%d seed=%d\n",
  best_ntree, best_mtry, best_nodesize, t0, ncores_use, seed_value
))
flush.console()

# ---------------------------------------------------------------------------
# 拟合 + 预测
# ---------------------------------------------------------------------------
cat("开始拟合 DynForest（20 轨迹）...\n")
flush.console()
fit_start <- Sys.time()
dyn_model <- fit_dynforest(
  timeData = train_pack$timeData,
  fixedData = train_pack$fixedData,
  y_df = train_pack$y_df,
  ntree = best_ntree,
  mtry = best_mtry,
  nodesize = best_nodesize,
  ncores = ncores_use,
  seed = seed_value
)
attr(dyn_model, "model_name") <- "RSFLC_20V"
fit_sec <- as.numeric(difftime(Sys.time(), fit_start, units = "secs"))
cat(sprintf("建模完成，用时 %.1f 秒（%.1f 分钟）\n", fit_sec, fit_sec / 60))
flush.console()

# ---------------------------------------------------------------------------
# 预测（五项指标共用）：P(dead)
# ---------------------------------------------------------------------------
test_pred <- predict_prob_dead(
  dyn_model,
  test_pack$timeData,
  test_pack$fixedData,
  t0
)
test_metrics <- eval_metrics(test_pred, test_pack$outcome)
test_eval <- test_metrics$data

train_pred <- predict_prob_dead(
  dyn_model,
  train_pack$timeData,
  train_pack$fixedData,
  t0
)
train_metrics <- eval_metrics(train_pred, train_pack$outcome)
train_eval <- train_metrics$data

train_score_data <- data.frame(
  time28 = train_eval$time28,
  status28 = train_eval$status28,
  stringsAsFactors = FALSE
)
test_score_data <- data.frame(
  time28 = test_eval$time28,
  status28 = test_eval$status28,
  stringsAsFactors = FALSE
)
train_risk_mat <- build_constant_risk_matrix(train_eval$probability, evaluation_times)
test_risk_mat <- build_constant_risk_matrix(test_eval$probability, evaluation_times)

# Score 一次算出 AUC / IPCW-Brier / IBS，供下面 2–4 取用
cat("计算训练集/测试集 Score（AUC / IPCW-Brier / IBS）...\n")
flush.console()
train_score_vals <- list(auc = NA_real_, brier = NA_real_, ibs = NA_real_)
test_score_vals <- list(auc = NA_real_, brier = NA_real_, ibs = NA_real_)
train_rsflc_score <- tryCatch(
  riskRegression::Score(
    object = list(RSFLC = train_risk_mat),
    formula = Hist(time28, status28) ~ 1,
    data = train_score_data,
    metrics = c("auc", "brier"),
    times = evaluation_times,
    summary = "ibs",
    cens.method = "ipcw",
    conf.int = FALSE,
    plots = NULL
  ),
  error = function(e) {
    cat("训练集 Score 失败:", conditionMessage(e), "\n")
    flush.console()
    NULL
  }
)
test_rsflc_score <- tryCatch(
  riskRegression::Score(
    object = list(RSFLC = test_risk_mat),
    formula = Hist(time28, status28) ~ 1,
    data = test_score_data,
    metrics = c("auc", "brier"),
    times = evaluation_times,
    summary = "ibs",
    cens.method = "ipcw",
    conf.int = FALSE,
    plots = NULL
  ),
  error = function(e) {
    cat("测试集 Score 失败:", conditionMessage(e), "\n")
    flush.console()
    NULL
  }
)
if (!is.null(train_rsflc_score)) {
  train_score_vals <- extract_score_at_horizon(train_rsflc_score, "RSFLC", evaluation_horizon)
}
if (!is.null(test_rsflc_score)) {
  test_score_vals <- extract_score_at_horizon(test_rsflc_score, "RSFLC", evaluation_horizon)
}

### 1. C-index
# 与 新RSF.R 测试集一致：concordance(Surv ~ probability, reverse=TRUE)
train_cindex <- train_metrics$cindex
test_cindex <- test_metrics$cindex
cat(sprintf(
  "1 C-index | 训练=%.4f | 测试=%.4f\n",
  train_cindex, test_cindex
))
flush.console()

### 2. 28-day AUC
# 与 新RSF.R 一致：Score 时间依赖 AUC，t=27.9999，IPCW；不回退 pROC
train_auc_28 <- train_score_vals$auc
test_auc_28 <- test_score_vals$auc
cat(sprintf(
  "2 28-day AUC | 训练=%.4f | 测试=%.4f\n",
  train_auc_28, test_auc_28
))
flush.console()

### 3. 28-day Brier (IPCW)
train_brier_28_ipcw <- train_score_vals$brier
test_brier_28_ipcw <- test_score_vals$brier
cat(sprintf(
  "3 28-day Brier (IPCW) | 训练=%.4f | 测试=%.4f\n",
  train_brier_28_ipcw, test_brier_28_ipcw
))
flush.console()

### 4. IBS 0-28 days (IPCW)
train_ibs_ipcw <- train_score_vals$ibs
test_ibs_ipcw <- test_score_vals$ibs
cat(sprintf(
  "4 IBS 0-28 days (IPCW) | 训练=%.4f | 测试=%.4f\n",
  train_ibs_ipcw, test_ibs_ipcw
))
flush.console()

### 5. 28-day Brier (ordinary)
train_brier_ordinary <- train_metrics$brier
test_brier_ordinary <- test_metrics$brier
cat(sprintf(
  "5 28-day Brier (ordinary) | 训练=%.4f | 测试=%.4f\n",
  train_brier_ordinary, test_brier_ordinary
))
flush.console()

# ---------------------------------------------------------------------------
# 汇总写出
# ---------------------------------------------------------------------------
parameter_table <- data.frame(
  parameter = c("ntree", "mtry", "nodesize", "t0", "n_long", "n_fixed"),
  value = c(
    best_ntree, best_mtry, best_nodesize, t0,
    length(long_vars), length(fixed_covars)
  ),
  stringsAsFactors = FALSE
)

performance_table <- data.frame(
  metric = c(
    "C-index",
    "28-day AUC",
    "28-day Brier (IPCW)",
    "IBS 0-28 days (IPCW)",
    "28-day Brier (ordinary)"
  ),
  training_set = round(
    c(
      train_cindex, train_auc_28, train_brier_28_ipcw, train_ibs_ipcw,
      train_brier_ordinary
    ),
    4
  ),
  test_set = round(
    c(
      test_cindex, test_auc_28, test_brier_28_ipcw, test_ibs_ipcw,
      test_brier_ordinary
    ),
    4
  ),
  check.names = FALSE,
  stringsAsFactors = FALSE
)

write.csv(
  parameter_table,
  file.path(trial_dir, "20V_parameters.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)
write.csv(
  performance_table,
  file.path(trial_dir, "20V_performance.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

cat("性能表：\n")
print(performance_table, row.names = FALSE)
flush.console()

# ---------------------------------------------------------------------------
# 可选 VIMP（默认关闭）
# ---------------------------------------------------------------------------
if (isTRUE(RUN_VIMP)) {
  cat("计算置换 VIMP（20 轨迹 + 11 基线）...\n")
  flush.console()
  vimp_table <- tryCatch({
    permutation_vimp_fallback(
      model = dyn_model,
      time_data = test_pack$timeData,
      fixed_data = test_pack$fixedData,
      outcome_df = test_pack$outcome,
      long_names = long_vars,
      numeric_names = numeric_covars,
      factor_names = factor_covars,
      seed = seed_value
    )
  }, error = function(e) {
    cat("置换 VIMP 失败:", conditionMessage(e), "\n")
    flush.console()
    data.frame(
      variable = character(0),
      importance = numeric(0),
      group = character(0),
      stringsAsFactors = FALSE
    )
  })
  write.csv(
    vimp_table,
    file.path(trial_dir, "20V_VIMP.csv"),
    row.names = FALSE,
    fileEncoding = "UTF-8"
  )
  cat("VIMP 已写入:", file.path(trial_dir, "20V_VIMP.csv"), "\n")
  flush.console()
} else {
  cat("跳过 VIMP（RUN_VIMP=FALSE）\n")
  flush.console()
}

cat(
  "试运行结果已写入:\n",
  normalizePath(trial_dir),
  "\n  20V_sample.csv / 20V_observations.csv / 20V_parameters.csv / 20V_performance.csv\n"
)
flush.console()
