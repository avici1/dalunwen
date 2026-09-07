# =============================================================================
# RSFLC 最优模型结果输出（20 条纵向轨迹 + 11 个基线协变量）
# 纵向：stroke_longitudinal_knn_0824_group_fold.csv
# 划分以该文件 group 列为准：group=1 全体训练，group=2（约 30%）外验证
# 与 0826_RSFLC超参数筛选.R 共用同一套纵向定义、划分与混合模型设定
# 超参写死为已选定组合，不再读取调参结果
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
# 开关与设定
# ---------------------------------------------------------------------------
RUN_SHAP <- TRUE

t0 <- 5
seed_value <- 2026L
evaluation_horizon <- 28 - 1e-04
evaluation_times <- unique(c(seq(0, 27.5, by = 0.5), evaluation_horizon))
ncores_use <- 8L

# 已选择最优超参数组合
best_ntree <- 200L
best_mtry <- 3L
best_nodesize <- 1L

base_dir <- "F:/文章_大论文/0722/实例研究代码"
out_dir <- file.path(base_dir, "执行")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
baseline_path <- file.path(base_dir, "stroke_baseline_knn_0824_fold.csv")
longitudinal_path <- file.path(base_dir, "stroke_longitudinal_knn_0824_group_fold.csv")
fig_dir <- file.path(out_dir, "图像_RSFLC")
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)

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

# DynForest 分类叶子存的是多数票（稀有事件下几乎全是 alive）。
# predict() 又把类别写入数值矩阵，未赋值的 0 会变成字符 "0"，
# 再用 1-proba 会得到全 0。这里改用各叶子的实际死亡率。
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

p_mtry <- length(long_vars) + length(fixed_covars)
n_train_dead <- sum(train_pack$outcome$status28 == 1L)
n_test_dead <- sum(test_pack$outcome$status28 == 1L)
cat(sprintf(
  paste0(
    "数据: %s | 划分: group=1 全体训练，group=2 验证\n",
    "最优超参 ntree=%d mtry=%d nodesize=%d | 20轨迹+11基线 p=%d\n",
    "训练集 n=%d 死亡=%d (%.1f%%) | 验证集 n=%d 死亡=%d (%.1f%%)\n"
  ),
  basename(longitudinal_path),
  best_ntree, best_mtry, best_nodesize, p_mtry,
  nrow(train_pack$fixedData), n_train_dead,
  100 * mean(train_pack$outcome$status28),
  nrow(test_pack$fixedData), n_test_dead,
  100 * mean(test_pack$outcome$status28)
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
cat("拟合模型摘要:\n")
print(dyn_model)
flush.console()

test_pred <- predict_prob_dead(
  dyn_model,
  test_pack$timeData,
  test_pack$fixedData,
  t0,
  verbose = TRUE
)
test_metrics <- eval_metrics(test_pred, test_pack$outcome)
test_eval <- test_metrics$data

cat(sprintf(
  "验证集 n=%d | C-index=%.4f | AUC28=%.4f | Brier28=%.4f\n",
  test_metrics$n,
  test_metrics$cindex,
  test_metrics$auc,
  test_metrics$brier
))
flush.console()

# ---------------------------------------------------------------------------
# OOB + IPCW 性能表
# ---------------------------------------------------------------------------
cat("计算最终模型 OOB C-index...\n")
flush.console()
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
# 01 VIMP：可单独选中运行（用全局 dyn_model / test_pack，不重新拟合）
# 轨迹列以会话为准（优先 traj_vars，否则从 timeData 推断），避免磁盘上的 20 轨迹名单
# ---------------------------------------------------------------------------
if (!exists("dyn_model")) {
  stop("找不到 dyn_model。请在已拟合的同一 R 会话中运行本段，不要从头 source 整份脚本。")
}
if (!exists("test_pack")) {
  stop("找不到 test_pack。请在读数/划分之后的同一会话中运行本段。")
}
if (!exists("predict_prob_dead") || !exists("eval_metrics") || !exists("permute_long_marker")) {
  stop("找不到 predict_prob_dead / eval_metrics / permute_long_marker。请在原会话运行，不要清环境。")
}
if (!exists("t0")) t0 <- 5
if (!exists("seed_value")) seed_value <- 2026L
if (!exists("fig_dir")) {
  fig_dir <- "F:/文章_大论文/0722/实例研究代码/执行/图像_RSFLC"
}
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)

vimp_long_names <- if (exists("traj_vars")) {
  as.character(traj_vars)
} else {
  setdiff(names(test_pack$timeData), c("hadm_id", "time", "times"))
}
vimp_fixed_names <- setdiff(names(test_pack$fixedData), "hadm_id")
vimp_factor_names <- if (exists("factor_covars")) {
  intersect(as.character(factor_covars), vimp_fixed_names)
} else {
  vimp_fixed_names[vapply(test_pack$fixedData[vimp_fixed_names], is.factor, logical(1))]
}
vimp_numeric_names <- if (exists("numeric_covars")) {
  intersect(as.character(numeric_covars), vimp_fixed_names)
} else {
  setdiff(vimp_fixed_names, vimp_factor_names)
}
vimp_long_names <- intersect(vimp_long_names, names(test_pack$timeData))
stopifnot(length(vimp_long_names) > 0L)

vimp_job_names <- c(
  vimp_long_names,
  vimp_numeric_names,
  vimp_factor_names
)
vimp_n_job <- length(vimp_job_names)
vimp_file <- file.path(fig_dir, "01_VIMP.png")

cat(sprintf(
  "计算置换 VIMP（不重新拟合）：%d 条轨迹 + %d 个固定协变量，共 %d 次预测\n",
  length(vimp_long_names),
  length(vimp_numeric_names) + length(vimp_factor_names),
  vimp_n_job + 1L
))
flush.console()

set.seed(seed_value)
vimp_table <- tryCatch({
  cat("  [1/", vimp_n_job + 1L, "] 基准预测\n", sep = "")
  flush.console()
  base_pred <- predict_prob_dead(
    dyn_model, test_pack$timeData, test_pack$fixedData, t0
  )
  base_metrics <- eval_metrics(base_pred, test_pack$outcome)
  base_err <- 1 - base_metrics$cindex
  rows <- list()
  step_i <- 1L

  for (nm in vimp_long_names) {
    step_i <- step_i + 1L
    cat(sprintf("  [%d/%d] 置换轨迹 %s\n", step_i, vimp_n_job + 1L, nm))
    flush.console()
    time_perm <- permute_long_marker(test_pack$timeData, nm)
    pred_i <- tryCatch(
      predict_prob_dead(dyn_model, time_perm, test_pack$fixedData, t0),
      error = function(e) NULL
    )
    if (is.null(pred_i)) next
    err_i <- 1 - eval_metrics(pred_i, test_pack$outcome)$cindex
    rows[[length(rows) + 1]] <- data.frame(
      variable = nm,
      importance = as.numeric(err_i - base_err),
      group = "Longitudinal",
      stringsAsFactors = FALSE
    )
  }
  for (nm in vimp_numeric_names) {
    step_i <- step_i + 1L
    cat(sprintf("  [%d/%d] 置换数值 %s\n", step_i, vimp_n_job + 1L, nm))
    flush.console()
    fd <- test_pack$fixedData
    fd[[nm]] <- sample(fd[[nm]])
    pred_i <- tryCatch(
      predict_prob_dead(dyn_model, test_pack$timeData, fd, t0),
      error = function(e) NULL
    )
    if (is.null(pred_i)) next
    err_i <- 1 - eval_metrics(pred_i, test_pack$outcome)$cindex
    rows[[length(rows) + 1]] <- data.frame(
      variable = nm,
      importance = as.numeric(err_i - base_err),
      group = "Numeric",
      stringsAsFactors = FALSE
    )
  }
  for (nm in vimp_factor_names) {
    step_i <- step_i + 1L
    cat(sprintf("  [%d/%d] 置换因子 %s\n", step_i, vimp_n_job + 1L, nm))
    flush.console()
    fd <- test_pack$fixedData
    fd[[nm]] <- sample(fd[[nm]])
    pred_i <- tryCatch(
      predict_prob_dead(dyn_model, test_pack$timeData, fd, t0),
      error = function(e) NULL
    )
    if (is.null(pred_i)) next
    err_i <- 1 - eval_metrics(pred_i, test_pack$outcome)$cindex
    rows[[length(rows) + 1]] <- data.frame(
      variable = nm,
      importance = as.numeric(err_i - base_err),
      group = "Factor",
      stringsAsFactors = FALSE
    )
  }

  if (length(rows) == 0) {
    data.frame(
      variable = character(0),
      importance = numeric(0),
      group = character(0),
      stringsAsFactors = FALSE
    )
  } else {
    out <- dplyr::bind_rows(rows)
    out[order(-out$importance, na.last = TRUE), , drop = FALSE]
  }
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
      title = sprintf(
        "RSFLC变量重要性（%d条轨迹 + 固定协变量）",
        length(vimp_long_names)
      )
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, face = "bold")
    )
  ggplot2::ggsave(vimp_file, vimp_plot, width = 7, height = 7, dpi = 300)
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
# 02–04 SHAP：可单独选中运行（用已有 dyn_model / train_pack / test_pack / test_eval）
# 扰动全部固定协变量；纵向轨迹保持原值。人数 50（2 典型 + 48 随机），nsim = 5
# ---------------------------------------------------------------------------
if (!exists("dyn_model") || !exists("train_pack") || !exists("test_pack") || !exists("test_eval")) {
  stop("找不到 dyn_model / train_pack / test_pack / test_eval。请在原会话从本段运行，不要从头 source。")
}
if (!exists("make_shap_pred_wrapper") || !exists("t0")) {
  stop("找不到 make_shap_pred_wrapper 或 t0。请在原会话运行。")
}
if (!exists("RUN_SHAP")) RUN_SHAP <- TRUE
if (!exists("seed_value")) seed_value <- 2026L
if (!exists("fig_dir")) {
  fig_dir <- "F:/文章_大论文/0722/实例研究代码/执行/图像_RSFLC"
}
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)

shap_covars <- setdiff(names(train_pack$fixedData), "hadm_id")
train_predictors <- train_pack$fixedData[, shap_covars, drop = FALSE]
test_predictors <- test_pack$fixedData[, shap_covars, drop = FALSE]
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
    sample(remaining_indices, min(48L, length(remaining_indices)))
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
  cat("开始 SHAP：n =", length(shap_ids), " nsim = 5\n")
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
        nsim = 5,
        adjust = TRUE
      ),
      error = function(e) {
        cat("  SHAP 患者", hid, "失败:", conditionMessage(e), "\n")
        NULL
      }
    )
    if (s %% 5 == 0 || s == length(shap_ids)) {
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

    fixed_vimp <- vimp_table$variable[vimp_table$group %in% c("Numeric", "Factor")]
    top_shap_variable <- fixed_vimp[1]
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
# 08 生成 Word 报告
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
    "结局为28天全因死亡。模型为 landmark 时刻 t0=5 天的 DynForest 分类森林",
    "（纳入 20 条纵向轨迹与 11 个基线固定协变量）。",
    "训练使用 stroke_longitudinal_knn_0824_group_fold.csv 中 group=1 的全体患者，",
    "验证使用 group=2（约 30% 留出）。",
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
  "VIMP 同时评价 20 条纵向轨迹与 11 个固定协变量；数值越大表示置换后预测性能下降越多。",
  style = "Normal"
)
rsflc_doc <- safe_body_add_img(
  rsflc_doc, vimp_file, 6.4, 6.0, "图1 VIMP变量重要性"
)

rsflc_doc <- officer::body_add_break(rsflc_doc)
rsflc_doc <- officer::body_add_par(rsflc_doc, "4 全局SHAP解释", style = "heading 1")
rsflc_doc <- officer::body_add_par(
  rsflc_doc,
  paste0(
    "SHAP 值表示各固定协变量对患者28天预测死亡风险的边际贡献；正值推动风险升高，负值推动风险降低。",
    "本分析仅扰动固定协变量，纵向轨迹（GCS、SOFA、CNS）保持原值。",
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

docx_path <- file.path(out_dir, "RSFLC模型结果.docx")
print(rsflc_doc, target = docx_path)
cat("RSFLC模型结果已保存至：\n", normalizePath(docx_path), "\n")
flush.console()
