# =============================================================================
# RSFLC 最优模型输出：可独立 source
# 数据读入、工具函数、group=1 训练 / group=2 验证 与 RSFLC_baseline_0825 同款
# 其后为 combo 3（20 轨迹 + 11 基线）最终拟合与 8 项结果
# =============================================================================
library(dplyr)
library(survival)
library(DynForest)
library(ggplot2)
library(fastshap)
library(shapviz)
library(rmda)
library(survminer)
library(patchwork)
library(pROC)
library(prodlim)
library(riskRegression)
library(officer)
library(flextable)

# ---------------------------------------------------------------------------
# 开关与设定（与 RSFLC_baseline_0825 同款）
# ---------------------------------------------------------------------------
RUN_SHAP <- TRUE

t0 <- 5
times_max <- 10
seed_value <- 2026L
evaluation_horizon <- 28 - 1e-04
evaluation_times <- unique(c(seq(0, 27.5, by = 0.5), evaluation_horizon))
ncores_use <- 8L

base_dir <- "F:/文章_大论文/0722/实例研究代码"
baseline_path <- file.path(base_dir, "stroke_baseline_knn_0824.csv")
longitudinal_path <- file.path(base_dir, "stroke_longitudinal_knn_0824.csv")
fig_dir <- file.path(base_dir, "图像_RSFLC")
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)

fixed_covars <- c(
  "age", "charlson_comorbidity_index", "apsiii", "sapsii", "oasis",
  "preiculos", "mechvent", "electivesurgery", "gender", "bmi", "stroke_type"
)
factor_covars <- c("mechvent", "electivesurgery", "gender", "stroke_type")
numeric_covars <- setdiff(fixed_covars, factor_covars)

timeVarModel_gcs <- list(
  gcs = list(fixed = gcs ~ 1, random = ~ time)
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
    timeVarModel  = timeVarModel_gcs,
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

permutation_vimp_fallback <- function(
  model, time_data, fixed_data, outcome_df,
  numeric_names, factor_names, seed = 2026L
) {
  set.seed(seed)
  base_pred <- predict_prob_dead(model, time_data, fixed_data, t0)
  base_metrics <- eval_metrics(base_pred, outcome_df)
  base_err <- 1 - base_metrics$cindex
  rows <- list()

  split_time <- split(time_data, time_data$hadm_id)
  perm_ids <- sample(names(split_time))
  time_perm_list <- lapply(seq_along(split_time), function(i) {
    src <- split_time[[perm_ids[i]]]
    dst_id <- as.integer(names(split_time)[i])
    out_i <- src
    out_i$hadm_id <- dst_id
    out_i
  })
  time_perm <- do.call(rbind, time_perm_list)
  rownames(time_perm) <- NULL
  pred_gcs <- tryCatch(
    predict_prob_dead(model, time_perm, fixed_data, t0),
    error = function(e) NULL
  )
  if (!is.null(pred_gcs)) {
    err_gcs <- 1 - eval_metrics(pred_gcs, outcome_df)$cindex
    rows[[length(rows) + 1]] <- data.frame(
      variable = "gcs",
      importance = as.numeric(err_gcs - base_err),
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

style_ft <- function(ft) {
  ft %>%
    flextable::theme_booktabs() %>%
    flextable::bg(part = "header", bg = "#D9EAF7") %>%
    flextable::bold(part = "header") %>%
    flextable::font(fontname = "Microsoft YaHei", part = "all") %>%
    flextable::align(align = "center", part = "all") %>%
    flextable::autofit()
}

safe_body_add_img <- function(doc, path, width, height, caption) {
  if (!is.null(path) && file.exists(path)) {
    doc <- officer::body_add_par(doc, caption, style = "Image Caption")
    doc <- officer::body_add_img(doc, src = path, width = width, height = height)
  } else {
    warning("缺少图片，跳过: ", caption, " | ", path)
    doc <- officer::body_add_par(
      doc,
      paste0(caption, "（图片未生成，已跳过）"),
      style = "Normal"
    )
  }
  doc
}

replicate_time_data <- function(time_i, n) {
  n_i <- nrow(time_i)
  out <- time_i[rep(seq_len(n_i), times = n), , drop = FALSE]
  out$hadm_id <- rep(seq_len(n), each = n_i)
  rownames(out) <- NULL
  out
}

make_shap_pred_wrapper <- function(time_i, template_fixed, t0_value = t0) {
  covars <- names(template_fixed)
  function(object, newdata) {
    nd <- as.data.frame(newdata, stringsAsFactors = FALSE)
    n <- nrow(nd)
    for (nm in covars) {
      if (!nm %in% names(nd)) {
        stop("SHAP newdata 缺少列: ", nm)
      }
      if (is.factor(template_fixed[[nm]])) {
        nd[[nm]] <- factor(as.character(nd[[nm]]), levels = levels(template_fixed[[nm]]))
      } else {
        nd[[nm]] <- as.numeric(nd[[nm]])
      }
    }
    nd$hadm_id <- seq_len(n)
    time_rep <- replicate_time_data(time_i, n)
    pred_df <- predict_prob_dead(
      object,
      time_rep,
      nd[, c("hadm_id", covars), drop = FALSE],
      t0_value
    )
    as.numeric(pred_df$probability[match(seq_len(n), pred_df$hadm_id)])
  }
}

# ---------------------------------------------------------------------------
# 读数与整理（与 RSFLC_baseline_0825 同款）
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

timeData_all <- stroke_longitudinal %>%
  dplyr::filter(.data$times <= times_max) %>%
  dplyr::transmute(
    hadm_id = as.integer(.data$hadm_id),
    time = as.integer(.data$times),
    gcs = as.numeric(.data$gcs)
  ) %>%
  dplyr::filter(is.finite(.data$gcs)) %>%
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
outcome_all <- baseline_all[, c("hadm_id", "time28", "status28", "death_28d"), drop = FALSE]

stopifnot(
  identical(sort(unique(timeData_all$hadm_id)), sort(fixedData_all$hadm_id)),
  identical(sort(fixedData_all$hadm_id), sort(y_all$hadm_id)),
  sum(is.na(timeData_all$gcs)) == 0
)

cat(
  "合格患者 n =", nrow(fixedData_all),
  "| 纵向行数 =", nrow(timeData_all),
  "| 28d 死亡率 =", round(mean(outcome_all$status28), 3), "\n"
)
flush.console()

# ---------------------------------------------------------------------------
# 使用数据中已有分组：group=1 训练（70%），group=2 测试（30%）
# ---------------------------------------------------------------------------
stopifnot("group" %in% names(baseline_all))
train_ids <- sort(unique(baseline_all$hadm_id[baseline_all$group == 1L]))
test_ids <- sort(unique(baseline_all$hadm_id[baseline_all$group == 2L]))
stopifnot(length(train_ids) > 0L, length(test_ids) > 0L)

cat(
  "训练集 n =", length(train_ids),
  "| 验证集 n =", length(test_ids), "\n"
)
flush.console()

# ---------------------------------------------------------------------------
# 最终模型：combo 3；纵向改为与试运行/五折相同的 20 条轨迹
# 基线仍为原来的 11 个固定协变量。全部 group=1 拟合，group=2 外验证。
# ---------------------------------------------------------------------------

long_vars <- c(
  "total_urine_output", "creat", "aki_stage", "gcs", "ph", "pco2",
  "lactate", "po2", "pao2fio2ratio", "glucose", "sodium", "bicarbonate",
  "hemoglobin", "temperature", "fio2", "sofa_24hours", "cns_24hours",
  "renal_24hours", "cardiovascular_24hours", "respiration_24hours"
)
miss_long <- setdiff(long_vars, names(stroke_longitudinal))
if (length(miss_long) > 0) {
  stop("纵向数据缺少列: ", paste(miss_long, collapse = ", "))
}

timeData_20 <- stroke_longitudinal %>%
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

ids_20 <- unique(timeData_20$hadm_id)
train_ids_20 <- sort(intersect(train_ids, ids_20))
test_ids_20 <- sort(intersect(test_ids, ids_20))
stopifnot(length(train_ids_20) > 0L, length(test_ids_20) > 0L)

train_raw_20 <- subset_by_ids(timeData_20, fixedData_all, y_all, outcome_all, train_ids_20)
test_raw_20 <- subset_by_ids(timeData_20, fixedData_all, y_all, outcome_all, test_ids_20)
train_template_20 <- train_raw_20$fixedData[, fixed_covars, drop = FALSE]
train_fixed_20 <- train_raw_20$fixedData
train_fixed_20[, fixed_covars] <- fill_missing_like_train(
  train_fixed_20[, fixed_covars, drop = FALSE],
  train_template_20
)
test_fixed_20 <- test_raw_20$fixedData
test_fixed_20[, fixed_covars] <- fill_missing_like_train(
  test_fixed_20[, fixed_covars, drop = FALSE],
  train_fixed_20[, fixed_covars, drop = FALSE]
)
train_pack <- list(
  timeData = train_raw_20$timeData,
  fixedData = train_fixed_20,
  y_df = train_raw_20$y_df,
  outcome = train_raw_20$outcome
)
test_pack <- list(
  timeData = test_raw_20$timeData,
  fixedData = test_fixed_20,
  y_df = test_raw_20$y_df,
  outcome = test_raw_20$outcome
)

timeVarModel_gcs <- stats::setNames(
  lapply(long_vars, function(v) {
    list(fixed = stats::as.formula(paste(v, "~ 1")), random = ~ time)
  }),
  long_vars
)

best_ntree <- 200L
best_mtry <- 3L
best_nodesize <- 1L

cat(sprintf(
  "指定最优超参 ntree=%d mtry=%d nodesize=%d | 20轨迹+11基线 | 训练集 n=%d | 验证集 n=%d\n",
  best_ntree, best_mtry, best_nodesize,
  nrow(train_pack$fixedData),
  nrow(test_pack$fixedData)
))
flush.console()

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
attr(dyn_model, "model_name") <- "RSFLC"

test_pred <- predict_prob_dead(
  dyn_model,
  test_pack$timeData,
  test_pack$fixedData,
  t0
)
test_metrics <- eval_metrics(test_pred, test_pack$outcome)
test_eval <- test_metrics$data
test_probability <- test_eval$probability

cat(sprintf(
  "验证集 n=%d | C-index=%.4f | AUC28=%.4f | Brier28=%.4f\n",
  test_metrics$n,
  test_metrics$cindex,
  test_metrics$auc,
  test_metrics$brier
))
flush.console()

# ---------------------------------------------------------------------------
# OOB + IPCW 性能表（对齐 RSF 报告口径）
# ---------------------------------------------------------------------------
cat("计算最终模型 OOB C-index...\n")
flush.console()
# 注意：compute_ooberror 返回 class=dynforestoob，且 is(dynforest)=FALSE；
# 不可写回 dyn_model，否则后续 compute_vimp / predict 都会失败。
oob_cindex <- tryCatch({
  oob_obj <- DynForest::compute_ooberror(dyn_model, ncores = 1L)
  1 - mean(as.numeric(oob_obj$oob.err), na.rm = TRUE)
}, error = function(e) {
  cat("compute_ooberror 失败:", conditionMessage(e), "\n")
  flush.console()
  NA_real_
})

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

cat("计算训练集/测试集 IPCW-Brier 与 IBS...\n")
flush.console()
train_rsflc_score <- riskRegression::Score(
  object = list(RSFLC = train_risk_mat),
  formula = Hist(time28, status28) ~ 1,
  data = train_score_data,
  metrics = c("auc", "brier"),
  times = evaluation_times,
  summary = "ibs",
  cens.method = "ipcw",
  conf.int = FALSE,
  plots = NULL
)
test_rsflc_score <- riskRegression::Score(
  object = list(RSFLC = test_risk_mat),
  formula = Hist(time28, status28) ~ 1,
  data = test_score_data,
  metrics = c("auc", "brier"),
  times = evaluation_times,
  summary = "ibs",
  cens.method = "ipcw",
  conf.int = FALSE,
  plots = NULL
)

train_score_vals <- extract_score_at_horizon(train_rsflc_score, "RSFLC", evaluation_horizon)
test_score_vals <- extract_score_at_horizon(test_rsflc_score, "RSFLC", evaluation_horizon)

# 训练集 C-index 用 OOB；测试集用留出 C-index
# AUC 优先用 Score 时间依赖 AUC；若 NA 则回退 pROC
train_auc_28 <- if (is.finite(train_score_vals$auc)) train_score_vals$auc else train_metrics$auc
test_auc_28 <- if (is.finite(test_score_vals$auc)) test_score_vals$auc else test_metrics$auc
train_brier_28_ipcw <- train_score_vals$brier
test_brier_28_ipcw <- test_score_vals$brier
train_ibs_ipcw <- train_score_vals$ibs
test_ibs_ipcw <- test_score_vals$ibs

parameter_table <- data.frame(
  parameter = c("ntree", "mtry", "nodesize", "t0"),
  value = c(best_ntree, best_mtry, best_nodesize, t0),
  stringsAsFactors = FALSE
)

performance_table <- data.frame(
  metric = c(
    "C-index",
    "28-day AUC",
    "28-day Brier (IPCW)",
    "IBS 0-28 days (IPCW)"
  ),
  training_set = round(
    c(oob_cindex, train_auc_28, train_brier_28_ipcw, train_ibs_ipcw),
    4
  ),
  test_set = round(
    c(test_metrics$cindex, test_auc_28, test_brier_28_ipcw, test_ibs_ipcw),
    4
  ),
  check.names = FALSE,
  stringsAsFactors = FALSE
)

write.csv(
  performance_table,
  file.path(fig_dir, "00_performance.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)
write.csv(
  parameter_table,
  file.path(fig_dir, "00_parameters.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

cat("性能表：\n")
print(performance_table, row.names = FALSE)
flush.console()

# ---------------------------------------------------------------------------
# 01 VIMP
# DynForest::compute_vimp 在 Windows 上即便 ncores=1 仍走 PSOCK+foreach，
# 置换阶段 OOB.tree 常得到全 NaN Importance（已用小样本复现）。
# 因此默认直接使用基于 predict 的置换重要性，不换其他 R 包。
# ---------------------------------------------------------------------------
set.seed(seed_value)
vimp_file <- file.path(fig_dir, "01_VIMP.png")

cat("计算置换 VIMP（predict 兜底，跳过 DynForest::compute_vimp）...\n")
flush.console()
vimp_table <- tryCatch({
  permutation_vimp_fallback(
    model = dyn_model,
    time_data = test_pack$timeData,
    fixed_data = test_pack$fixedData,
    outcome_df = test_pack$outcome,
    numeric_names = numeric_covars,
    factor_names = factor_covars,
    seed = seed_value
  )
}, error = function(e) {
  cat("置换 VIMP 失败:", conditionMessage(e), "\n")
  flush.console()
  NULL
})

if (!is.null(vimp_table) && nrow(vimp_table) > 0 && any(is.finite(vimp_table$importance))) {
  vimp_plot <- ggplot2::ggplot(
    head(vimp_table, 20),
    ggplot2::aes(x = reorder(variable, importance), y = importance)
  ) +
    ggplot2::geom_col(fill = "#2F75B5") +
    ggplot2::coord_flip() +
    ggplot2::labs(
      x = NULL,
      y = "Permutation VIMP (1 - C-index delta)",
      title = "RSFLC变量重要性（GCS + 固定协变量）"
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, face = "bold")
    )
  ggplot2::ggsave(vimp_file, vimp_plot, width = 7, height = 5.5, dpi = 300)
} else {
  vimp_table <- data.frame(
    variable = character(0),
    importance = numeric(0),
    group = character(0),
    stringsAsFactors = FALSE
  )
  warning("VIMP 为空，跳过 01_VIMP.png")
}

write.csv(vimp_table, file.path(fig_dir, "01_VIMP.csv"), row.names = FALSE, fileEncoding = "UTF-8")
cat("VIMP 已写入:", file.path(fig_dir, "01_VIMP.csv"), "\n")
flush.console()

# ---------------------------------------------------------------------------
# 02–04 SHAP（仅固定协变量；GCS 轨迹不扰动）
# ---------------------------------------------------------------------------
train_predictors <- train_pack$fixedData[, fixed_covars, drop = FALSE]
test_predictors <- test_pack$fixedData[, fixed_covars, drop = FALSE]
test_hadm <- test_pack$fixedData$hadm_id

shap_beeswarm_file <- file.path(fig_dir, "02_SHAP_beeswarm.png")
patient_shap_file <- file.path(fig_dir, "03_典型患者_SHAP.png")
shap_dependence_file <- file.path(fig_dir, "04_SHAP_dependence.png")
shap_importance_table <- data.frame(
  variable = character(0),
  mean_abs_shap = numeric(0),
  stringsAsFactors = FALSE
)

eval_index <- match(test_eval$hadm_id, test_hadm)
high_risk_death_index <- eval_index[which.max(
  ifelse(test_eval$status28 == 1L, test_eval$probability, -Inf)
)]
low_risk_survivor_index <- eval_index[which.min(
  ifelse(test_eval$status28 == 0L, test_eval$probability, Inf)
)]

typical_patient_table <- data.frame(
  patient = c("典型患者1", "典型患者2"),
  type = c("高预测风险且28天死亡", "低预测风险且28天存活"),
  hadm_id = as.integer(test_hadm[c(high_risk_death_index, low_risk_survivor_index)]),
  observed_time = round(
    test_pack$outcome$time28[
      match(test_hadm[c(high_risk_death_index, low_risk_survivor_index)], test_pack$outcome$hadm_id)
    ],
    2
  ),
  death_28d = as.integer(
    test_pack$outcome$status28[
      match(test_hadm[c(high_risk_death_index, low_risk_survivor_index)], test_pack$outcome$hadm_id)
    ]
  ),
  predicted_risk = round(
    test_eval$probability[
      match(test_hadm[c(high_risk_death_index, low_risk_survivor_index)], test_eval$hadm_id)
    ],
    4
  ),
  stringsAsFactors = FALSE
)
write.csv(
  typical_patient_table,
  file.path(fig_dir, "03_典型患者.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

if (isTRUE(RUN_SHAP) && nrow(test_eval) >= 2L) {
  set.seed(seed_value)
  remaining_indices <- setdiff(
    seq_len(nrow(test_predictors)),
    c(high_risk_death_index, low_risk_survivor_index)
  )
  shap_indices <- c(
    high_risk_death_index,
    low_risk_survivor_index,
    sample(remaining_indices, min(198L, length(remaining_indices)))
  )
  shap_indices <- unique(shap_indices[!is.na(shap_indices)])
  if (length(shap_indices) < 2L) {
    stop("SHAP 抽样失败：有效测试患者不足")
  }
  shap_data <- test_predictors[shap_indices, , drop = FALSE]
  shap_ids <- test_hadm[shap_indices]
  shap_time_by_id <- split(test_pack$timeData, test_pack$timeData$hadm_id)
  set.seed(seed_value)
  shap_bg_n <- min(100L, nrow(train_predictors))
  shap_bg <- train_predictors[sample(seq_len(nrow(train_predictors)), shap_bg_n), , drop = FALSE]
  
  shap_rows <- vector("list", length(shap_ids))
  cat("开始 SHAP：n =", length(shap_ids), " nsim = 20（较慢）\n")
  flush.console()
  for (s in seq_along(shap_ids)) {
    hid <- shap_ids[s]
    time_i <- shap_time_by_id[[as.character(hid)]]
    if (is.null(time_i) || nrow(time_i) == 0) {
      time_i <- test_pack$timeData[test_pack$timeData$hadm_id == hid, , drop = FALSE]
    }
    wrap_i <- make_shap_pred_wrapper(time_i, train_predictors, t0)
    shap_rows[[s]] <- tryCatch(
      fastshap::explain(
        object = dyn_model,
        X = shap_bg,
        pred_wrapper = wrap_i,
        newdata = shap_data[s, , drop = FALSE],
        nsim = 20,
        adjust = TRUE
      ),
      error = function(e) {
        cat("  SHAP 患者", hid, "失败:", conditionMessage(e), "\n")
        NULL
      }
    )
    if (s %% 10 == 0 || s == length(shap_ids)) {
      cat(sprintf("  SHAP 进度 %d/%d\n", s, length(shap_ids)))
      flush.console()
    }
  }
  
  shap_ok <- !vapply(shap_rows, is.null, logical(1))
  if (sum(shap_ok) >= 2L) {
    shap_mat <- do.call(rbind, lapply(shap_rows[shap_ok], as.matrix))
    shap_x <- shap_data[shap_ok, , drop = FALSE]
    shap_object <- shapviz::shapviz(shap_mat, X = shap_x)
    
    shap_importance_table <- data.frame(
      variable = colnames(shap_mat),
      mean_abs_shap = as.numeric(colMeans(abs(shap_mat))),
      stringsAsFactors = FALSE
    )
    shap_importance_table <- shap_importance_table[
      order(-shap_importance_table$mean_abs_shap),
      ,
      drop = FALSE
    ]
    write.csv(
      shap_importance_table,
      file.path(fig_dir, "02_SHAP_importance.csv"),
      row.names = FALSE,
      fileEncoding = "UTF-8"
    )
    
    shap_beeswarm_plot <- shapviz::sv_importance(
      shap_object,
      kind = "beeswarm",
      max_display = 20
    ) +
      ggplot2::ggtitle("RSFLC全局SHAP beeswarm（固定协变量）") +
      ggplot2::theme(
        plot.title = ggplot2::element_text(hjust = 0.5, face = "bold")
      )
    ggplot2::ggsave(shap_beeswarm_file, shap_beeswarm_plot, width = 7, height = 6, dpi = 300)
    
    patient_shap_plot_1 <- shapviz::sv_waterfall(
      shap_object,
      row_id = 1,
      max_display = 12
    ) +
      ggplot2::ggtitle("典型患者1：高风险死亡")
    patient_shap_plot_2 <- shapviz::sv_waterfall(
      shap_object,
      row_id = min(2L, sum(shap_ok)),
      max_display = 12
    ) +
      ggplot2::ggtitle("典型患者2：低风险存活")
    patient_shap_plot <- patient_shap_plot_1 / patient_shap_plot_2
    ggplot2::ggsave(patient_shap_file, patient_shap_plot, width = 7, height = 9, dpi = 300)
    
    top_shap_variable <- setdiff(vimp_table$variable, "gcs")[1]
    if (
      length(top_shap_variable) == 0 ||
      is.na(top_shap_variable) ||
      !top_shap_variable %in% names(shap_x)
    ) {
      if (nrow(shap_importance_table) > 0) {
        top_shap_variable <- shap_importance_table$variable[1]
      } else {
        top_shap_variable <- names(shap_x)[1]
      }
    }
    shap_dependence_plot <- shapviz::sv_dependence(
      shap_object,
      v = top_shap_variable,
      color_var = "auto"
    ) +
      ggplot2::ggtitle(paste0("SHAP dependence：", top_shap_variable)) +
      ggplot2::theme(
        plot.title = ggplot2::element_text(hjust = 0.5, face = "bold")
      )
    ggplot2::ggsave(shap_dependence_file, shap_dependence_plot, width = 7, height = 5, dpi = 300)
  } else {
    warning("有效 SHAP 结果不足，跳过 02–04 图")
  }
} else {
  cat("跳过 SHAP 三图（RUN_SHAP=FALSE 或测试集过小）\n")
  flush.console()
}

# ---------------------------------------------------------------------------
# 05 测试集 28 天校准
# ---------------------------------------------------------------------------
calibration_data <- test_eval %>%
  dplyr::mutate(risk_decile = dplyr::ntile(probability, 10))

calibration_table <- data.frame(
  risk_decile = 1:10,
  n = NA_integer_,
  predicted_risk = NA_real_,
  observed_risk = NA_real_,
  observed_low = NA_real_,
  observed_high = NA_real_
)

for (i in 1:10) {
  calibration_group <- calibration_data %>%
    dplyr::filter(risk_decile == i)
  if (nrow(calibration_group) == 0) next
  calibration_fit <- survival::survfit(
    Surv(time28, status28) ~ 1,
    data = calibration_group
  )
  calibration_summary <- summary(
    calibration_fit,
    times = evaluation_horizon,
    extend = TRUE
  )
  calibration_table$n[i] <- nrow(calibration_group)
  calibration_table$predicted_risk[i] <- mean(calibration_group$probability)
  calibration_table$observed_risk[i] <- 1 - calibration_summary$surv
  calibration_table$observed_low[i] <- 1 - calibration_summary$upper
  calibration_table$observed_high[i] <- 1 - calibration_summary$lower
}

calibration_plot <- ggplot2::ggplot(
  calibration_table,
  ggplot2::aes(x = predicted_risk, y = observed_risk)
) +
  ggplot2::geom_abline(intercept = 0, slope = 1, linetype = 2, color = "grey40") +
  ggplot2::geom_errorbar(
    ggplot2::aes(ymin = observed_low, ymax = observed_high),
    width = 0.01,
    color = "#2F75B5"
  ) +
  ggplot2::geom_point(size = 2.8, color = "#C00000") +
  ggplot2::geom_line(color = "#C00000") +
  ggplot2::coord_equal(xlim = c(0, 1), ylim = c(0, 1)) +
  ggplot2::labs(
    x = "平均预测28天死亡风险",
    y = "KM观察28天死亡风险",
    title = "测试集28天校准图"
  ) +
  ggplot2::theme_minimal(base_size = 12) +
  ggplot2::theme(
    plot.title = ggplot2::element_text(hjust = 0.5, face = "bold")
  )

calibration_file <- file.path(fig_dir, "05_28天校准.png")
ggplot2::ggsave(calibration_file, calibration_plot, width = 6.5, height = 5.5, dpi = 300)
write.csv(
  calibration_table,
  file.path(fig_dir, "05_校准十分位.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

# ---------------------------------------------------------------------------
# 06 DCA
# ---------------------------------------------------------------------------
dca_data <- data.frame(
  death_28d = test_eval$status28,
  rsflc_risk = test_eval$probability
)
dca_result <- rmda::decision_curve(
  death_28d ~ rsflc_risk,
  data = dca_data,
  family = binomial(link = "logit"),
  fitted.risk = TRUE,
  thresholds = seq(0.01, 0.50, by = 0.01),
  confidence.intervals = NA,
  study.design = "cohort"
)

dca_file <- file.path(fig_dir, "06_DCA.png")
png(dca_file, width = 2000, height = 1500, res = 260)
rmda::plot_decision_curve(
  dca_result,
  curve.names = "RSFLC",
  xlab = "Threshold probability",
  ylab = "Net benefit",
  legend.position = "topright",
  standardize = FALSE
)
title("测试集28天死亡风险决策曲线")
dev.off()

# ---------------------------------------------------------------------------
# 07 KM 风险分层
# ---------------------------------------------------------------------------
km_data <- data.frame(
  time28 = test_eval$time28,
  status28 = test_eval$status28,
  predicted_risk = test_eval$probability
)
km_data$risk_group <- factor(
  ifelse(
    km_data$predicted_risk >= median(km_data$predicted_risk),
    "高风险组",
    "低风险组"
  ),
  levels = c("低风险组", "高风险组")
)

km_fit <- survival::survfit(
  Surv(time28, status28) ~ risk_group,
  data = km_data
)

km_plot <- survminer::ggsurvplot(
  km_fit,
  data = km_data,
  risk.table = TRUE,
  pval = TRUE,
  conf.int = FALSE,
  xlim = c(0, 28),
  break.time.by = 7,
  xlab = "时间（天）",
  ylab = "生存概率",
  legend.title = "风险分层",
  legend.labs = c("低风险组", "高风险组"),
  palette = c("#2F75B5", "#C00000"),
  ggtheme = ggplot2::theme_minimal(base_size = 12)
)

km_file <- file.path(fig_dir, "07_KM风险分层.png")
png(km_file, width = 2000, height = 1700, res = 260)
print(km_plot)
dev.off()

cat("七张图已保存至：\n", normalizePath(fig_dir), "\n")
flush.console()

# ---------------------------------------------------------------------------
# 08 生成 Word 报告（对标 新RSF.R）
# ---------------------------------------------------------------------------
parameter_ft <- style_ft(flextable::flextable(parameter_table))
performance_ft <- style_ft(flextable::flextable(performance_table)) %>%
  flextable::fit_to_width(max_width = 6.4)

vimp_ft <- if (nrow(vimp_table) > 0) {
  style_ft(flextable::flextable(vimp_table)) %>%
    flextable::colformat_num(j = "importance", digits = 4) %>%
    flextable::fit_to_width(max_width = 6.4)
} else {
  NULL
}

typical_patient_ft <- if (nrow(typical_patient_table) > 0) {
  style_ft(flextable::flextable(typical_patient_table)) %>%
    flextable::fit_to_width(max_width = 6.4)
} else {
  NULL
}

calibration_ft <- style_ft(flextable::flextable(calibration_table)) %>%
  flextable::colformat_num(
    j = c("predicted_risk", "observed_risk", "observed_low", "observed_high"),
    digits = 4
  ) %>%
  flextable::fit_to_width(max_width = 6.4)

top_shap_text <- if (nrow(shap_importance_table) > 0) {
  paste0(
    "按 mean(|SHAP|) 排序，固定协变量中贡献最大的前3项为：",
    paste(head(shap_importance_table$variable, 3), collapse = "、"),
    "。"
  )
} else {
  "SHAP 汇总表未生成时，请结合 beeswarm 图解读变量贡献方向与幅度。"
}

rsflc_doc <- officer::read_docx()
rsflc_doc <- officer::body_add_par(
  rsflc_doc,
  "动态随机森林（RSFLC）模型结果",
  style = "graphic title"
)
rsflc_doc <- officer::body_add_par(
  rsflc_doc,
  paste0(
    "结局为28天全因死亡。模型为 landmark 时刻 t0=5 天的 DynForest 分类森林（纳入 GCS 纵向轨迹与基线固定协变量）。",
    "训练集 C-index 采用 OOB 评价，测试集采用外部留出评价。",
    "AUC、28天 Brier 和 IBS 均按生存资料定义计算，其中 Brier 和 IBS 采用 IPCW 校正删失；",
    "因 RSFLC 输出为28天死亡概率，IPCW 风险矩阵在各评价时点使用同一预测概率作为累积风险近似。"
  ),
  style = "Normal"
)

rsflc_doc <- officer::body_add_par(rsflc_doc, "1 最优超参数", style = "heading 1")
rsflc_doc <- flextable::body_add_flextable(rsflc_doc, parameter_ft)

rsflc_doc <- officer::body_add_par(rsflc_doc, "2 模型预测性能", style = "heading 1")
rsflc_doc <- flextable::body_add_flextable(rsflc_doc, performance_ft)
rsflc_doc <- officer::body_add_par(
  rsflc_doc,
  paste0(
    "注：未死亡病例统一在第28天行政删失，因此严格的时间依赖 AUC 在 ",
    "27.9999 天计算，并作为28天 AUC 报告；IPCW-Brier 和 IPCW-IBS 也评价至该时点。",
    "训练集 AUC、Brier 和 IBS 为训练资料上的表观性能，测试集结果用于评价泛化性能。"
  ),
  style = "Normal"
)

rsflc_doc <- officer::body_add_par(rsflc_doc, "3 VIMP变量重要性", style = "heading 1")
if (!is.null(vimp_ft)) {
  rsflc_doc <- flextable::body_add_flextable(rsflc_doc, vimp_ft)
}
rsflc_doc <- officer::body_add_par(
  rsflc_doc,
  "VIMP 同时评价纵向 GCS 与固定协变量；数值越大表示置换后预测性能下降越多。",
  style = "Normal"
)
rsflc_doc <- safe_body_add_img(
  rsflc_doc, vimp_file, 6.4, 5.0, "图1 VIMP变量重要性"
)

rsflc_doc <- officer::body_add_break(rsflc_doc)
rsflc_doc <- officer::body_add_par(rsflc_doc, "4 全局SHAP解释", style = "heading 1")
rsflc_doc <- officer::body_add_par(
  rsflc_doc,
  paste0(
    "SHAP 值表示各固定协变量对患者28天预测死亡风险的边际贡献；正值推动风险升高，负值推动风险降低。",
    "本分析仅扰动固定协变量，GCS 纵向轨迹保持原值。",
    top_shap_text
  ),
  style = "Normal"
)
rsflc_doc <- safe_body_add_img(
  rsflc_doc, shap_beeswarm_file, 6.4, 5.5, "图2 SHAP global beeswarm"
)
rsflc_doc <- officer::body_add_break(rsflc_doc)
rsflc_doc <- safe_body_add_img(
  rsflc_doc, shap_dependence_file, 6.4, 4.6, "图3 SHAP dependence"
)

rsflc_doc <- officer::body_add_break(rsflc_doc)
rsflc_doc <- officer::body_add_par(rsflc_doc, "5 两个典型患者的局部解释", style = "heading 1")
if (!is.null(typical_patient_ft)) {
  rsflc_doc <- flextable::body_add_flextable(rsflc_doc, typical_patient_ft)
}
rsflc_doc <- safe_body_add_img(
  rsflc_doc, patient_shap_file, 6.4, 8.2, "图4 两个典型患者的SHAP waterfall"
)

rsflc_doc <- officer::body_add_break(rsflc_doc)
rsflc_doc <- officer::body_add_par(rsflc_doc, "6 28天校准", style = "heading 1")
rsflc_doc <- flextable::body_add_flextable(rsflc_doc, calibration_ft)
rsflc_doc <- officer::body_add_par(
  rsflc_doc,
  "按测试集预测风险十分位分组，组内平均预测风险与 KM 估计的28天观察死亡风险对照。",
  style = "Normal"
)
rsflc_doc <- safe_body_add_img(
  rsflc_doc, calibration_file, 6.2, 5.2, "图5 测试集28天校准图"
)

rsflc_doc <- officer::body_add_break(rsflc_doc)
rsflc_doc <- officer::body_add_par(rsflc_doc, "7 临床决策曲线分析", style = "heading 1")
rsflc_doc <- officer::body_add_par(
  rsflc_doc,
  "DCA 比较不同阈值概率下 RSFLC 模型与全部干预、均不干预策略的净获益。",
  style = "Normal"
)
rsflc_doc <- safe_body_add_img(
  rsflc_doc, dca_file, 6.4, 4.8, "图6 测试集DCA"
)

rsflc_doc <- officer::body_add_break(rsflc_doc)
rsflc_doc <- officer::body_add_par(rsflc_doc, "8 风险分层与KM曲线", style = "heading 1")
rsflc_doc <- officer::body_add_par(
  rsflc_doc,
  "以测试集预测风险中位数划分高、低风险组，并使用 log-rank 检验比较两组生存曲线。",
  style = "Normal"
)
rsflc_doc <- safe_body_add_img(
  rsflc_doc, km_file, 6.4, 5.4, "图7 高低风险组KM曲线"
)

docx_path <- file.path(base_dir, "RSFLC模型结果.docx")
print(rsflc_doc, target = docx_path)
cat("RSFLC模型结果已保存至：\n", normalizePath(docx_path), "\n")
flush.console()