# =============================================================================
# RSF 超参数筛选：8 个基线协变量 + 第 1 天 20 条轨迹截面
# 变量定义与参考/新RSF.R 一致；group=1 训练，group=2 外验证
# 可独立 source，不依赖 0826_RSF_结果输出.R
# =============================================================================
library(dplyr)
library(survival)
library(randomForestSRC)
library(riskRegression)
library(prodlim)

thread_number <- max(1L, parallel::detectCores(logical = TRUE) - 2L)
options(rf.cores = thread_number)

base_dir <- "F:/文章_大论文/0722/实例研究代码"
out_dir <- file.path(base_dir, "执行")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
baseline_path <- file.path(base_dir, "stroke_baselinedata_0824.csv")
longitudinal_path <- file.path(base_dir, "stroke_longitudinal_knn_0824.csv")
result_path <- file.path(out_dir, "results_RSF.csv")

baseline_cols <- c(
  "hadm_id", "age", "charlson_comorbidity_index", "apsiii", "sapsii",
  "oasis", "preiculos", "mechvent", "electivesurgery",
  "intime", "deathtime", "death_28d", "group"
)
long_vars <- c(
  "total_urine_output", "creat", "aki_stage", "gcs", "ph", "pco2",
  "lactate", "po2", "pao2fio2ratio", "glucose", "sodium", "bicarbonate",
  "hemoglobin", "temperature", "fio2", "sofa_24hours", "cns_24hours",
  "renal_24hours", "cardiovascular_24hours", "respiration_24hours"
)
longitudinal_cols <- c("hadm_id", "times", long_vars)

evaluation_horizon <- 28 - 1e-04
evaluation_times <- unique(c(seq(0, 27.5, by = 0.5), evaluation_horizon))

# ---------------------------------------------------------------------------
# 读数并拼成建模表：8 基线 + times==1 的 20 轨迹截面
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

miss_base <- setdiff(baseline_cols, names(stroke_baseline))
miss_long <- setdiff(longitudinal_cols, names(stroke_longitudinal))
if (length(miss_base) > 0L) {
  stop("基线数据缺少列: ", paste(miss_base, collapse = ", "))
}
if (length(miss_long) > 0L) {
  stop("纵向数据缺少列: ", paste(miss_long, collapse = ", "))
}

baseline_data <- stroke_baseline %>%
  select(all_of(baseline_cols)) %>%
  mutate(
    hadm_id = as.integer(hadm_id),
    age = as.numeric(age),
    charlson_comorbidity_index = as.numeric(charlson_comorbidity_index),
    apsiii = as.numeric(apsiii),
    sapsii = as.numeric(sapsii),
    oasis = as.numeric(oasis),
    preiculos = as.numeric(preiculos),
    mechvent = factor(mechvent),
    electivesurgery = factor(electivesurgery),
    intime = as.POSIXct(intime, format = "%d/%m/%Y %H:%M:%S"),
    deathtime = as.POSIXct(deathtime, format = "%d/%m/%Y %H:%M:%S"),
    death_28d = as.integer(death_28d),
    group = as.integer(group),
    time28 = as.numeric(difftime(deathtime, intime, units = "days")),
    time28 = ifelse(is.na(time28), 28, pmin(time28, 28)),
    status28 = as.integer(death_28d == 1)
  ) 

longitudinal_day1 <- stroke_longitudinal %>%
  select(all_of(longitudinal_cols)) %>%
  filter(times == 1) %>%
  mutate(
    hadm_id = as.integer(hadm_id),
    across(all_of(long_vars), as.numeric)
  ) %>%
  select(-times)

rsf_data <- baseline_data %>%
  left_join(longitudinal_day1, by = "hadm_id") %>%
  select(-hadm_id, -intime, -deathtime, -death_28d) %>%
  as.data.frame()

stopifnot("group" %in% names(rsf_data))
train_data <- rsf_data[rsf_data$group == 1L, ]
test_data <- rsf_data[rsf_data$group == 2L, ]
stopifnot(nrow(train_data) > 0L, nrow(test_data) > 0L)
train_data$group <- NULL
test_data$group <- NULL

n_predictor <- ncol(train_data) - 2L
stopifnot(n_predictor == 28L)

cat(
  "训练集 n =", nrow(train_data),
  "| 测试集 n =", nrow(test_data),
  "| 协变量数 =", n_predictor, "（8基线+20轨迹截面）\n"
)
flush.console()

# ---------------------------------------------------------------------------
# 网格搜索
# ---------------------------------------------------------------------------
tuning_grid <- expand.grid(
  ntree = c(300, 500, 1000),
  mtry = c(3, 6, 9, 12),
  nodesize = c(10, 20, 30, 40),
  nsplit = c(10, 25, 50)
)

n_grid <- nrow(tuning_grid)
tuning_metric_rows <- vector("list", n_grid)
cat("网格组合数 =", n_grid, "\n")
tune_start <- Sys.time()

for (i in seq_len(n_grid)) {
  iter_start <- Sys.time()
  rsf_model <- randomForestSRC::rfsrc(
    Surv(time28, status28) ~ .,
    data = train_data,
    ntree = tuning_grid$ntree[i],
    mtry = tuning_grid$mtry[i],
    nodesize = tuning_grid$nodesize[i],
    nsplit = tuning_grid$nsplit[i],
    splitrule = "logrank",
    na.action = "na.impute",
    importance = FALSE,
    seed = 2026
  )
  fit_sec <- as.numeric(difftime(Sys.time(), iter_start, units = "secs"))
  cat(sprintf(
    "[%d/%d] 建模完成  ntree=%d mtry=%d nodesize=%d nsplit=%d  用时 %.1f 秒\n",
    i, n_grid,
    tuning_grid$ntree[i], tuning_grid$mtry[i],
    tuning_grid$nodesize[i], tuning_grid$nsplit[i],
    fit_sec
  ))
  flush.console()

  train_time_index <- which.min(abs(rsf_model$time.interest - 28))
  train_probability <- 1 - rsf_model$survival.oob[, train_time_index]
  train_evaluation <- data.frame(
    time28 = train_data$time28,
    status28 = train_data$status28,
    probability = train_probability
  )
  train_cindex <- survival::concordance(
    Surv(time28, status28) ~ probability,
    data = train_evaluation,
    reverse = TRUE
  )$concordance

  test_prediction <- predict(
    rsf_model,
    newdata = test_data,
    na.action = "na.impute"
  )
  test_time_index <- which.min(abs(test_prediction$time.interest - 28))
  test_probability <- 1 - test_prediction$survival[, test_time_index]
  test_evaluation <- data.frame(
    time28 = test_data$time28,
    status28 = test_data$status28,
    probability = test_probability
  )
  test_cindex <- survival::concordance(
    Surv(time28, status28) ~ probability,
    data = test_evaluation,
    reverse = TRUE
  )$concordance

  best_oob_error <- tail(rsf_model$err.rate, 1)
  oob_cindex <- 1 - best_oob_error

  train_rsf_score <- riskRegression::Score(
    object = list(RSF = rsf_model),
    formula = Hist(time28, status28) ~ 1,
    data = train_data,
    metrics = c("auc", "brier"),
    times = evaluation_times,
    summary = "ibs",
    cens.method = "ipcw",
    conf.int = FALSE,
    plots = NULL,
    predictRisk.args = list(rfsrc = list(na.action = "na.impute"))
  )
  test_rsf_score <- riskRegression::Score(
    object = list(RSF = rsf_model),
    formula = Hist(time28, status28) ~ 1,
    data = test_data,
    metrics = c("auc", "brier"),
    times = evaluation_times,
    summary = "ibs",
    cens.method = "ipcw",
    conf.int = FALSE,
    plots = NULL,
    predictRisk.args = list(rfsrc = list(na.action = "na.impute"))
  )

  train_auc_score_table <- as.data.frame(train_rsf_score$AUC$score)
  train_brier_score_table <- as.data.frame(train_rsf_score$Brier$score)
  test_auc_score_table <- as.data.frame(test_rsf_score$AUC$score)
  test_brier_score_table <- as.data.frame(test_rsf_score$Brier$score)

  train_auc_28 <- train_auc_score_table$AUC[
    as.character(train_auc_score_table$model) == "RSF" &
      abs(train_auc_score_table$times - evaluation_horizon) < 1e-08
  ]
  train_brier_28_ipcw <- train_brier_score_table$Brier[
    as.character(train_brier_score_table$model) == "RSF" &
      abs(train_brier_score_table$times - evaluation_horizon) < 1e-08
  ]
  train_ibs_ipcw <- train_brier_score_table$IBS[
    as.character(train_brier_score_table$model) == "RSF" &
      abs(train_brier_score_table$times - evaluation_horizon) < 1e-08
  ]
  test_auc_28 <- test_auc_score_table$AUC[
    as.character(test_auc_score_table$model) == "RSF" &
      abs(test_auc_score_table$times - evaluation_horizon) < 1e-08
  ]
  test_brier_28_ipcw <- test_brier_score_table$Brier[
    as.character(test_brier_score_table$model) == "RSF" &
      abs(test_brier_score_table$times - evaluation_horizon) < 1e-08
  ]
  test_ibs_ipcw <- test_brier_score_table$IBS[
    as.character(test_brier_score_table$model) == "RSF" &
      abs(test_brier_score_table$times - evaluation_horizon) < 1e-08
  ]

  tuning_metric_rows[[i]] <- data.frame(
    combo = i,
    ntree = tuning_grid$ntree[i],
    mtry = tuning_grid$mtry[i],
    nodesize = tuning_grid$nodesize[i],
    nsplit = tuning_grid$nsplit[i],
    oobci = oob_cindex,
    trainci = train_cindex,
    testci = test_cindex,
    train_AUC28 = train_auc_28,
    train_Brier28_IPCW = train_brier_28_ipcw,
    train_IBS_0_28_IPCW = train_ibs_ipcw,
    AUC28 = test_auc_28,
    Brier28_IPCW = test_brier_28_ipcw,
    IBS_0_28_IPCW = test_ibs_ipcw
  )

  elapsed_min <- as.numeric(difftime(Sys.time(), tune_start, units = "mins"))
  remain_min <- elapsed_min / i * (n_grid - i)
  cat(sprintf(
    "  本组合计 %.1f 秒 | 已用 %.1f 分钟 | 预计剩余 %.1f 分钟\n",
    as.numeric(difftime(Sys.time(), iter_start, units = "secs")),
    elapsed_min,
    remain_min
  ))
  flush.console()
}

tuning_results <- do.call(rbind, tuning_metric_rows)
print(tuning_results, digits = 4, row.names = FALSE)

write.csv(tuning_results, result_path, row.names = FALSE, fileEncoding = "UTF-8")

best_row <- tuning_results %>%
  arrange(desc(testci), desc(AUC28), Brier28_IPCW)
best_param_path <- file.path(out_dir, "best_parameters_RSF.csv")
write.csv(best_row[1, , drop = FALSE], best_param_path, row.names = FALSE, fileEncoding = "UTF-8")

cat(
  "调参结果已保存至：\n",
  normalizePath(result_path),
  "\n最优组合（按测试集 C-index）：ntree=", best_row$ntree[1],
  " mtry=", best_row$mtry[1],
  " nodesize=", best_row$nodesize[1],
  " nsplit=", best_row$nsplit[1],
  " testci=", round(best_row$testci[1], 4),
  "\n",
  sep = ""
)
