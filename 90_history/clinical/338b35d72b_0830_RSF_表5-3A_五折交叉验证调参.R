# =============================================================================
# 表5-3A：静态预测任务 RSF 五折交叉验证调参
#
# 任务：使用 8 个入院基线变量 + 第1天 20 个截面变量预测入院后28天死亡。
# 调参：ntree × mtry × nodesize × nsplit，共 108 组；按患者分组的5折CV。
# 选优：最大化五折验证 C-index 均值；并报告最优组合的28天AUC、
#       28天IPCW-Brier、IBS及普通Brier。
#
# 重要数据规则：
# 1. group=1 仅用于调参；group=2 完全不参与表5-3A的参数选择。
# 2. fold仅从既有fold文件读取ID和折号，不读取其中已插补的协变量。
# 3. 使用未插补的基线/纵向数据；每折用训练折中位数/众数填补训练与验证数据，
#    避免先全队列插补造成信息泄漏。
# 4. 主要结局起点统一为 admittime。death_28d=1但缺少deathtime的记录因无法
#    确定生存时间而从本次时间-事件调参中排除，并在队列审计表中报告。
# =============================================================================

suppressPackageStartupMessages({
  library(survival)
  library(randomForestSRC)
  library(prodlim)
  library(riskRegression)
})

# -----------------------------------------------------------------------------
# 1. 路径与运行参数
# -----------------------------------------------------------------------------
default_base_dir <- "F:/文章_大论文/0722/实例研究代码"
base_dir <- Sys.getenv("RSF_INPUT_DIR", unset = default_base_dir)
default_out_dir <- "F:/文章_大论文/0830/结果/RSF"
out_dir <- Sys.getenv("RSF_OUTPUT_DIR", unset = default_out_dir)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

baseline_path <- file.path(base_dir, "stroke_baselinedata_0824.csv")
longitudinal_path <- file.path(base_dir, "stroke_longitudinal_filterP1P99_0824.csv")
fold_path <- file.path(base_dir, "stroke_baseline_knn_0824_fold.csv")

seed_value <- 20260831L
evaluation_horizon <- 28 - 1e-04
evaluation_times <- unique(c(seq(1, 27, by = 1), evaluation_horizon))
thread_default <- max(1L, parallel::detectCores(logical = TRUE) - 2L)
thread_number <- as.integer(Sys.getenv("RSF_THREADS", unset = thread_default))
if (!is.finite(thread_number) || thread_number < 1L) thread_number <- 1L
options(rf.cores = thread_number)

max_combos_env <- Sys.getenv("RSF_MAX_COMBOS", unset = "")
max_combos <- if (nzchar(max_combos_env)) as.integer(max_combos_env) else NA_integer_

path_queue_audit <- file.path(out_dir, "01_cohort_audit.csv")
path_missing <- file.path(out_dir, "02_variable_missingness.csv")
path_fold_distribution <- file.path(out_dir, "03_fold_distribution.csv")
path_imputation <- file.path(out_dir, "04_fold_imputation_parameters.csv")
path_grid_folds <- file.path(out_dir, "05_grid_fold_cindex.csv")
path_grid_summary <- file.path(out_dir, "06_grid_summary_ranked.csv")
path_best_folds <- file.path(out_dir, "07_best_combo_fold_metrics.csv")
path_best_parameters <- file.path(out_dir, "08_best_parameters.csv")
path_table53a <- file.path(out_dir, "09_table_5_3A_RSF_tuning.csv")
path_best_model <- file.path(out_dir, "10_RSF_best_model_group1.rds")
path_imputer <- file.path(out_dir, "11_RSF_final_imputation_rules.rds")
path_log <- file.path(out_dir, "12_run_information.txt")

write_utf8_csv <- function(x, path) {
  data.table::fwrite(x, path, bom = TRUE, na = "")
}

append_log <- function(...) {
  line <- paste0(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), " | ", paste0(..., collapse = ""))
  cat(line, "\n")
  cat(line, "\n", file = path_log, append = TRUE)
  flush.console()
}

safe_mean <- function(x) if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE)
safe_sd <- function(x) if (sum(is.finite(x)) <= 1L) NA_real_ else sd(x, na.rm = TRUE)

# -----------------------------------------------------------------------------
# 2. 变量定义
# -----------------------------------------------------------------------------
baseline_vars <- c(
  "age", "charlson_comorbidity_index", "apsiii", "sapsii",
  "oasis", "preiculos", "mechvent", "electivesurgery"
)

day1_vars <- c(
  "total_urine_output", "creat", "aki_stage", "gcs", "ph", "pco2",
  "lactate", "po2", "pao2fio2ratio", "glucose", "sodium", "bicarbonate",
  "hemoglobin", "temperature", "fio2", "sofa_24hours", "cns_24hours",
  "renal_24hours", "cardiovascular_24hours", "respiration_24hours"
)

predictor_vars <- c(baseline_vars, day1_vars)
factor_vars <- c("mechvent", "electivesurgery")
numeric_vars <- setdiff(predictor_vars, factor_vars)
model_vars <- c(predictor_vars, "time28", "status28")

# -----------------------------------------------------------------------------
# 3. 读取原始数据并构造队列
# -----------------------------------------------------------------------------
required_files <- c(baseline_path, longitudinal_path, fold_path)
if (!all(file.exists(required_files))) {
  stop("缺少输入文件: ", paste(required_files[!file.exists(required_files)], collapse = "; "))
}

baseline <- data.table::fread(
  baseline_path, encoding = "UTF-8", na.strings = c("", "NA"),
  data.table = FALSE, check.names = FALSE
)
longitudinal <- data.table::fread(
  longitudinal_path, encoding = "UTF-8", na.strings = c("", "NA"),
  data.table = FALSE, check.names = FALSE
)
fold_key <- data.table::fread(
  fold_path, encoding = "UTF-8", na.strings = c("", "NA"),
  data.table = FALSE, check.names = FALSE
)

required_baseline <- c(
  "subject_id", "hadm_id", "admittime", "deathtime", "death_28d", baseline_vars
)
required_long <- c("subject_id", "hadm_id", "times", day1_vars)
required_fold <- c("subject_id", "hadm_id", "group", "fold")
for (check in list(
  baseline = setdiff(required_baseline, names(baseline)),
  longitudinal = setdiff(required_long, names(longitudinal)),
  fold = setdiff(required_fold, names(fold_key))
)) {
  if (length(check) > 0L) stop("输入数据缺少字段: ", paste(check, collapse = ", "))
}

fold_key <- unique(fold_key[required_fold])
if (anyDuplicated(fold_key$hadm_id)) stop("fold文件中hadm_id不唯一")

baseline <- merge(
  baseline,
  fold_key,
  by = c("subject_id", "hadm_id"),
  all.x = TRUE,
  suffixes = c("", "_foldkey")
)
if ("group_foldkey" %in% names(baseline)) baseline$group <- baseline$group_foldkey
if ("fold_foldkey" %in% names(baseline)) baseline$fold <- baseline$fold_foldkey

parse_datetime <- function(x) {
  as.POSIXct(x, format = "%d/%m/%Y %H:%M:%S", tz = "UTC")
}
baseline$admittime_parsed <- parse_datetime(baseline$admittime)
baseline$deathtime_parsed <- parse_datetime(baseline$deathtime)
baseline$death_28d <- as.integer(baseline$death_28d)
baseline$group <- as.integer(baseline$group)
baseline$fold <- as.integer(baseline$fold)
baseline$event_days_exact <- as.numeric(
  difftime(baseline$deathtime_parsed, baseline$admittime_parsed, units = "days")
)

baseline$outcome_issue <- "eligible"
baseline$outcome_issue[is.na(baseline$death_28d)] <- "missing_death_28d"
baseline$outcome_issue[
  baseline$death_28d == 1L & is.na(baseline$deathtime_parsed)
] <- "death28_missing_event_time"
baseline$outcome_issue[
  baseline$death_28d == 1L & is.finite(baseline$event_days_exact) & baseline$event_days_exact <= 0
] <- "nonpositive_event_time"

baseline$status28 <- ifelse(baseline$death_28d == 1L, 1L, 0L)
baseline$time28 <- ifelse(
  baseline$status28 == 1L,
  pmin(baseline$event_days_exact, evaluation_horizon),
  28
)

day1 <- longitudinal[longitudinal$times == 1, c("subject_id", "hadm_id", day1_vars)]
if (anyDuplicated(day1$hadm_id)) stop("纵向数据中times=1的hadm_id不唯一")
analysis_all <- merge(
  baseline,
  day1,
  by = c("subject_id", "hadm_id"),
  all.x = TRUE,
  suffixes = c("", "_day1")
)

for (v in numeric_vars) analysis_all[[v]] <- as.numeric(analysis_all[[v]])
for (v in factor_vars) analysis_all[[v]] <- factor(analysis_all[[v]])

analysis_all$has_day1_record <- analysis_all$hadm_id %in% day1$hadm_id
eligible <- analysis_all$outcome_issue == "eligible" &
  analysis_all$group == 1L & analysis_all$fold %in% 1:5 &
  is.finite(analysis_all$time28) & analysis_all$time28 > 0
analysis <- analysis_all[eligible, , drop = FALSE]

if (nrow(analysis) == 0L) stop("没有符合条件的group=1调参样本")
if (any(analysis$subject_id %in% analysis_all$subject_id[analysis_all$group == 2L], na.rm = TRUE)) {
  stop("同一subject_id同时出现在group=1和group=2")
}
if (any(tapply(analysis$fold, analysis$subject_id, function(x) length(unique(x))) > 1L)) {
  stop("同一subject_id被分配到多个CV折")
}

queue_audit <- data.frame(
  item = c(
    "原始住院记录", "独立患者", "group=1原始记录", "group=2保留记录",
    "28天死亡总数", "28天死亡但缺少精确死亡时间", "非正事件时间",
    "group=1最终调参记录", "group=1最终调参事件", "group=1缺少第1天记录"
  ),
  n = c(
    nrow(analysis_all), length(unique(analysis_all$subject_id)),
    sum(analysis_all$group == 1L, na.rm = TRUE), sum(analysis_all$group == 2L, na.rm = TRUE),
    sum(analysis_all$death_28d == 1L, na.rm = TRUE),
    sum(analysis_all$outcome_issue == "death28_missing_event_time", na.rm = TRUE),
    sum(analysis_all$outcome_issue == "nonpositive_event_time", na.rm = TRUE),
    nrow(analysis), sum(analysis$status28 == 1L), sum(!analysis$has_day1_record)
  ),
  note = c(
    "baseline全部住院记录", "subject_id去重", "仅用于CV调参", "不参与本次调参",
    "death_28d=1", "主要时间-事件分析排除", "主要时间-事件分析排除",
    "group=1且有可用事件时间", "精确时间可用的28天死亡", "20个第1天变量由训练折规则填补"
  ),
  stringsAsFactors = FALSE
)
write_utf8_csv(queue_audit, path_queue_audit)

missing_table <- data.frame(
  variable = predictor_vars,
  role = c(rep("入院基线", length(baseline_vars)), rep("第1天截面", length(day1_vars))),
  missing_n = vapply(analysis[predictor_vars], function(x) sum(is.na(x)), integer(1)),
  missing_rate = vapply(analysis[predictor_vars], function(x) mean(is.na(x)), numeric(1)),
  stringsAsFactors = FALSE
)
write_utf8_csv(missing_table, path_missing)

fold_distribution <- do.call(rbind, lapply(1:5, function(k) {
  d <- analysis[analysis$fold == k, ]
  data.frame(
    fold = k,
    n_admissions = nrow(d),
    n_subjects = length(unique(d$subject_id)),
    events = sum(d$status28 == 1L),
    event_rate = mean(d$status28 == 1L),
    stringsAsFactors = FALSE
  )
}))
write_utf8_csv(fold_distribution, path_fold_distribution)

append_log("输入基线记录=", nrow(analysis_all), "; group1最终CV记录=", nrow(analysis),
           "; events=", sum(analysis$status28), "; threads=", thread_number)

# -----------------------------------------------------------------------------
# 4. 每折只基于训练折拟合插补规则
# -----------------------------------------------------------------------------
mode_value <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0L) return(NA_character_)
  tab <- sort(table(as.character(x)), decreasing = TRUE)
  names(tab)[1]
}

fit_imputer <- function(train, predictors, factor_names) {
  rules <- vector("list", length(predictors))
  names(rules) <- predictors
  for (v in predictors) {
    if (v %in% factor_names) {
      lev <- levels(droplevels(factor(train[[v]])))
      fill <- mode_value(train[[v]])
      if (is.na(fill)) fill <- "0"
      if (!fill %in% lev) lev <- c(lev, fill)
      rules[[v]] <- list(type = "factor", fill = fill, levels = lev)
    } else {
      fill <- suppressWarnings(median(as.numeric(train[[v]]), na.rm = TRUE))
      if (!is.finite(fill)) fill <- 0
      rules[[v]] <- list(type = "numeric", fill = as.numeric(fill))
    }
  }
  rules
}

apply_imputer <- function(data, rules) {
  out <- data
  for (v in names(rules)) {
    rule <- rules[[v]]
    if (rule$type == "numeric") {
      out[[v]] <- as.numeric(out[[v]])
      out[[v]][!is.finite(out[[v]])] <- rule$fill
    } else {
      raw <- as.character(out[[v]])
      raw[is.na(raw) | !raw %in% rule$levels] <- rule$fill
      out[[v]] <- factor(raw, levels = rule$levels)
    }
  }
  out
}

fold_cache <- vector("list", 5)
imputation_rows <- list()
for (k in 1:5) {
  train_raw <- analysis[analysis$fold != k, model_vars, drop = FALSE]
  valid_raw <- analysis[analysis$fold == k, model_vars, drop = FALSE]
  rules <- fit_imputer(train_raw, predictor_vars, factor_vars)
  train_imp <- apply_imputer(train_raw, rules)
  valid_imp <- apply_imputer(valid_raw, rules)
  fold_cache[[k]] <- list(train = train_imp, valid = valid_imp, rules = rules)
  for (v in predictor_vars) {
    imputation_rows[[length(imputation_rows) + 1L]] <- data.frame(
      fold = k, variable = v, type = rules[[v]]$type,
      fill_value = as.character(rules[[v]]$fill), stringsAsFactors = FALSE
    )
  }
}
write_utf8_csv(do.call(rbind, imputation_rows), path_imputation)

# -----------------------------------------------------------------------------
# 5. 指标函数
# -----------------------------------------------------------------------------
probability_at_horizon <- function(survival_matrix, time_interest) {
  index <- which.min(abs(time_interest - evaluation_horizon))
  as.numeric(1 - survival_matrix[, index])
}

cindex_from_risk <- function(time, status, risk) {
  keep <- is.finite(time) & !is.na(status) & is.finite(risk)
  if (sum(keep) < 2L) return(NA_real_)
  as.numeric(survival::concordance(
    survival::Surv(time[keep], status[keep]) ~ risk[keep], reverse = TRUE
  )$concordance)
}

score_rsf <- function(model, data) {
  result <- list(auc = NA_real_, brier_ipcw = NA_real_, ibs = NA_real_)
  score_object <- tryCatch(
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
    error = function(e) {
      append_log("Score失败: ", conditionMessage(e))
      NULL
    }
  )
  if (is.null(score_object)) return(result)
  auc_table <- as.data.frame(score_object$AUC$score)
  brier_table <- as.data.frame(score_object$Brier$score)
  auc_row <- auc_table[as.character(auc_table$model) == "RSF" &
                         abs(auc_table$times - evaluation_horizon) < 1e-06, , drop = FALSE]
  brier_row <- brier_table[as.character(brier_table$model) == "RSF" &
                             abs(brier_table$times - evaluation_horizon) < 1e-06, , drop = FALSE]
  if (nrow(auc_row)) result$auc <- as.numeric(auc_row$AUC[1])
  if (nrow(brier_row)) {
    result$brier_ipcw <- as.numeric(brier_row$Brier[1])
  }
  brier_curve <- brier_table[as.character(brier_table$model) == "RSF" &
                               is.finite(brier_table$times) &
                               is.finite(brier_table$Brier),
                             c("times", "Brier"), drop = FALSE]
  brier_curve <- brier_curve[order(brier_curve$times), , drop = FALSE]
  if (nrow(brier_curve)) {
    curve_time <- c(0, as.numeric(brier_curve$times))
    curve_brier <- c(0, as.numeric(brier_curve$Brier))
    horizon <- max(curve_time)
    if (is.finite(horizon) && horizon > 0) {
      result$ibs <- sum(diff(curve_time) *
                          (head(curve_brier, -1L) + tail(curve_brier, -1L)) / 2) / horizon
    }
  }
  result
}

# -----------------------------------------------------------------------------
# 6. 完整108组网格：所有组合先完成五折验证C-index
# -----------------------------------------------------------------------------
tuning_grid <- expand.grid(
  ntree = c(300L, 500L, 1000L),
  mtry = c(3L, 6L, 9L),
  nodesize = c(10L, 20L, 30L, 40L),
  nsplit = c(10L, 25L, 50L),
  KEEP.OUT.ATTRS = FALSE,
  stringsAsFactors = FALSE
)
tuning_grid$combo <- seq_len(nrow(tuning_grid))
if (is.finite(max_combos) && max_combos > 0L) {
  tuning_grid <- head(tuning_grid, max_combos)
  append_log("测试模式：仅运行前", nrow(tuning_grid), "组参数")
}

existing <- if (file.exists(path_grid_folds)) {
  read.csv(path_grid_folds, stringsAsFactors = FALSE, check.names = FALSE)
} else {
  data.frame()
}

grid_start <- Sys.time()
total_fits <- nrow(tuning_grid) * 5L
fit_counter <- 0L

for (i in seq_len(nrow(tuning_grid))) {
  g <- tuning_grid[i, ]
  for (k in 1:5) {
    fit_counter <- fit_counter + 1L
    already_done <- nrow(existing) > 0L && any(
      existing$combo == g$combo & existing$fold == k & existing$status == "ok"
    )
    if (already_done) {
      append_log("跳过已完成 combo=", g$combo, ", fold=", k)
      next
    }

    train_data <- fold_cache[[k]]$train
    valid_data <- fold_cache[[k]]$valid
    fit_seed <- seed_value + as.integer(g$combo) * 100L + k
    set.seed(fit_seed)
    fit_begin <- Sys.time()
    error_message <- ""
    model <- tryCatch(
      randomForestSRC::rfsrc(
        Surv(time28, status28) ~ .,
        data = train_data,
        ntree = as.integer(g$ntree),
        mtry = as.integer(g$mtry),
        nodesize = as.integer(g$nodesize),
        nsplit = as.integer(g$nsplit),
        splitrule = "logrank",
        na.action = "na.omit",
        importance = FALSE,
        forest = TRUE,
        seed = fit_seed
      ),
      error = function(e) {
        error_message <<- conditionMessage(e)
        NULL
      }
    )

    validation_cindex <- NA_real_
    train_oob_cindex <- NA_real_
    if (!is.null(model)) {
      valid_prediction <- tryCatch(
        predict(model, newdata = valid_data, na.action = "na.omit"),
        error = function(e) {
          error_message <<- conditionMessage(e)
          NULL
        }
      )
      if (!is.null(valid_prediction)) {
        valid_risk <- probability_at_horizon(
          valid_prediction$survival, valid_prediction$time.interest
        )
        validation_cindex <- cindex_from_risk(
          valid_data$time28, valid_data$status28, valid_risk
        )
      }
      if (!is.null(model$survival.oob)) {
        train_risk <- probability_at_horizon(model$survival.oob, model$time.interest)
        train_oob_cindex <- cindex_from_risk(
          train_data$time28, train_data$status28, train_risk
        )
      }
    }

    elapsed_seconds <- as.numeric(difftime(Sys.time(), fit_begin, units = "secs"))
    new_row <- data.frame(
      combo = as.integer(g$combo), ntree = as.integer(g$ntree),
      mtry = as.integer(g$mtry), nodesize = as.integer(g$nodesize),
      nsplit = as.integer(g$nsplit), fold = k,
      n_train = nrow(train_data), n_valid = nrow(valid_data),
      events_train = sum(train_data$status28), events_valid = sum(valid_data$status28),
      train_oob_cindex = train_oob_cindex,
      validation_cindex = validation_cindex,
      seed = fit_seed, elapsed_seconds = elapsed_seconds,
      status = if (is.finite(validation_cindex)) "ok" else "failed",
      error = error_message,
      stringsAsFactors = FALSE
    )

    if (nrow(existing) > 0L) {
      existing <- existing[!(existing$combo == g$combo & existing$fold == k), , drop = FALSE]
    }
    existing <- rbind(existing, new_row)
    existing <- existing[order(existing$combo, existing$fold), ]
    write_utf8_csv(existing, path_grid_folds)

    elapsed_total <- as.numeric(difftime(Sys.time(), grid_start, units = "mins"))
    expected_remaining <- if (fit_counter > 0L) elapsed_total / fit_counter * (total_fits - fit_counter) else NA_real_
    append_log(
      sprintf("[%d/%d] combo=%d fold=%d ntree=%d mtry=%d nodesize=%d nsplit=%d | C-index=%.4f | %.1fs | 剩余约%.1fmin",
              fit_counter, total_fits, g$combo, k, g$ntree, g$mtry, g$nodesize, g$nsplit,
              validation_cindex, elapsed_seconds, expected_remaining)
    )
  }
}

grid_folds <- existing
grid_summary_rows <- lapply(split(grid_folds, grid_folds$combo), function(d) {
  data.frame(
    combo = d$combo[1], ntree = d$ntree[1], mtry = d$mtry[1],
    nodesize = d$nodesize[1], nsplit = d$nsplit[1],
    successful_folds = sum(d$status == "ok" & is.finite(d$validation_cindex)),
    validation_cindex_mean = safe_mean(d$validation_cindex[d$status == "ok"]),
    validation_cindex_sd = safe_sd(d$validation_cindex[d$status == "ok"]),
    train_oob_cindex_mean = safe_mean(d$train_oob_cindex[d$status == "ok"]),
    train_oob_cindex_sd = safe_sd(d$train_oob_cindex[d$status == "ok"]),
    mean_fit_seconds = safe_mean(d$elapsed_seconds),
    stringsAsFactors = FALSE
  )
})
grid_summary <- do.call(rbind, grid_summary_rows)
grid_summary <- grid_summary[order(
  -grid_summary$successful_folds,
  -grid_summary$validation_cindex_mean,
  grid_summary$validation_cindex_sd,
  grid_summary$ntree,
  grid_summary$mtry,
  grid_summary$nodesize,
  grid_summary$nsplit
), ]
grid_summary$rank <- seq_len(nrow(grid_summary))
grid_summary <- grid_summary[c("rank", setdiff(names(grid_summary), "rank"))]
write_utf8_csv(grid_summary, path_grid_summary)

eligible_best <- grid_summary[
  grid_summary$successful_folds == 5L & is.finite(grid_summary$validation_cindex_mean),
]
if (nrow(eligible_best) == 0L) stop("没有完成全部5折的有效超参数组合")
best <- eligible_best[1, ]
append_log("最优组合 combo=", best$combo, ": ntree=", best$ntree,
           ", mtry=", best$mtry, ", nodesize=", best$nodesize,
           ", nsplit=", best$nsplit, ", mean C-index=", round(best$validation_cindex_mean, 6))

# -----------------------------------------------------------------------------
# 7. 最优组合重新五折拟合并计算完整指标
# -----------------------------------------------------------------------------
best_fold_rows <- list()
for (k in 1:5) {
  train_data <- fold_cache[[k]]$train
  valid_data <- fold_cache[[k]]$valid
  fit_seed <- seed_value + as.integer(best$combo) * 100L + k
  set.seed(fit_seed)
  fit_begin <- Sys.time()
  model <- randomForestSRC::rfsrc(
    Surv(time28, status28) ~ .,
    data = train_data,
    ntree = as.integer(best$ntree), mtry = as.integer(best$mtry),
    nodesize = as.integer(best$nodesize), nsplit = as.integer(best$nsplit),
    splitrule = "logrank", na.action = "na.omit",
    importance = FALSE, forest = TRUE, seed = fit_seed
  )
  prediction <- predict(model, newdata = valid_data, na.action = "na.omit")
  valid_risk <- probability_at_horizon(prediction$survival, prediction$time.interest)
  validation_cindex <- cindex_from_risk(valid_data$time28, valid_data$status28, valid_risk)
  score_values <- score_rsf(model, valid_data)
  ordinary_brier <- mean((valid_data$status28 - valid_risk)^2)
  train_risk <- probability_at_horizon(model$survival.oob, model$time.interest)
  train_oob_cindex <- cindex_from_risk(train_data$time28, train_data$status28, train_risk)
  best_fold_rows[[k]] <- data.frame(
    fold = k, n_train = nrow(train_data), n_valid = nrow(valid_data),
    events_train = sum(train_data$status28), events_valid = sum(valid_data$status28),
    train_oob_cindex = train_oob_cindex,
    validation_cindex = validation_cindex,
    validation_auc28 = score_values$auc,
    validation_brier28_ipcw = score_values$brier_ipcw,
    validation_ibs_0_28 = score_values$ibs,
    validation_brier28_ordinary = ordinary_brier,
    elapsed_seconds = as.numeric(difftime(Sys.time(), fit_begin, units = "secs")),
    stringsAsFactors = FALSE
  )
  append_log("最优组合完整评价 fold=", k, "; C-index=", round(validation_cindex, 5),
             "; AUC=", round(score_values$auc, 5), "; Brier=", round(score_values$brier_ipcw, 5))
}
best_folds <- do.call(rbind, best_fold_rows)
write_utf8_csv(best_folds, path_best_folds)

metric_names <- c(
  "train_oob_cindex", "validation_cindex", "validation_auc28",
  "validation_brier28_ipcw", "validation_ibs_0_28", "validation_brier28_ordinary"
)
best_metric_summary <- data.frame(
  metric = metric_names,
  mean = vapply(best_folds[metric_names], safe_mean, numeric(1)),
  sd = vapply(best_folds[metric_names], safe_sd, numeric(1)),
  stringsAsFactors = FALSE
)

best_parameters <- data.frame(
  parameter = c(
    "combo", "ntree", "mtry", "nodesize", "nsplit", "selection_metric",
    "predictor_count", "cv_folds", "group_used", "event_time_policy"
  ),
  value = c(
    best$combo, best$ntree, best$mtry, best$nodesize, best$nsplit,
    "max mean validation C-index", length(predictor_vars), 5, 1,
    "exclude death_28d=1 with missing deathtime"
  ),
  stringsAsFactors = FALSE
)
write_utf8_csv(best_parameters, path_best_parameters)

fmt_mean_sd <- function(mean_value, sd_value) {
  if (!is.finite(mean_value)) return("")
  sprintf("%.4f ± %.4f", mean_value, sd_value)
}
metric_lookup <- function(metric) {
  best_metric_summary[best_metric_summary$metric == metric, , drop = FALSE]
}

m_ci <- metric_lookup("validation_cindex")
m_auc <- metric_lookup("validation_auc28")
m_brier <- metric_lookup("validation_brier28_ipcw")
table53a <- data.frame(
  model = "RSF",
  search_space = "ntree={300,500,1000}; mtry={3,6,9}; nodesize={10,20,30,40}; nsplit={10,25,50}",
  best_parameters = sprintf("ntree=%d, mtry=%d, nodesize=%d, nsplit=%d",
                            best$ntree, best$mtry, best$nodesize, best$nsplit),
  cv_cindex = fmt_mean_sd(m_ci$mean, m_ci$sd),
  cv_auc = fmt_mean_sd(m_auc$mean, m_auc$sd),
  cv_brier = fmt_mean_sd(m_brier$mean, m_brier$sd),
  check.names = FALSE,
  stringsAsFactors = FALSE
)
names(table53a) <- c(
  "模型", "搜索空间", "最优超参数", "五折验证 C-index",
  "五折验证 AUC", "五折验证 Brier"
)
write_utf8_csv(table53a, path_table53a)

# -----------------------------------------------------------------------------
# 8. 用全部group=1调参队列拟合并保存最优模型（不评估group=2）
# -----------------------------------------------------------------------------
final_raw <- analysis[model_vars]
final_rules <- fit_imputer(final_raw, predictor_vars, factor_vars)
final_data <- apply_imputer(final_raw, final_rules)
set.seed(seed_value + 999999L)
final_model <- randomForestSRC::rfsrc(
  Surv(time28, status28) ~ .,
  data = final_data,
  ntree = as.integer(best$ntree), mtry = as.integer(best$mtry),
  nodesize = as.integer(best$nodesize), nsplit = as.integer(best$nsplit),
  splitrule = "logrank", na.action = "na.omit",
  importance = TRUE, forest = TRUE, seed = seed_value + 999999L
)
saveRDS(final_model, path_best_model)
saveRDS(
  list(rules = final_rules, predictors = predictor_vars, factors = factor_vars,
       evaluation_horizon = evaluation_horizon, outcome_origin = "admittime"),
  path_imputer
)

append_log("全部完成；输出目录=", normalizePath(out_dir, winslash = "/", mustWork = FALSE))
append_log("表5-3A: ", paste(unlist(table53a), collapse = " | "))
print(table53a, row.names = FALSE)
