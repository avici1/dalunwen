library(dplyr)
library(survival)
library(randomForestSRC)
library(riskRegression)
library(prodlim)

thread_number <- max(1L, parallel::detectCores(logical = TRUE) - 2L)
options(rf.cores = thread_number)

stroke_baseline <- read.csv(
  "F:/文章_大论文/0722/实例研究代码/stroke_baselinedata_0824.csv",
  stringsAsFactors = FALSE,
  na.strings = c("", "NA"),
  fileEncoding = "UTF-8"
)

rsf_data <- stroke_baseline %>%
  transmute(
    age = as.numeric(age),
    charlson_comorbidity_index = as.numeric(charlson_comorbidity_index),
    apsiii = as.numeric(apsiii),
    sapsii = as.numeric(sapsii),
    oasis = as.numeric(oasis),
    preiculos = as.numeric(preiculos),
    mechvent = factor(mechvent),
    electivesurgery = factor(electivesurgery),
    gender = factor(gender),
    bmi = as.numeric(bmi),
    stroke_type = factor(stroke_type),
    group = as.integer(group),
    intime = as.POSIXct(intime, format = "%d/%m/%Y %H:%M:%S"),
    deathtime = as.POSIXct(deathtime, format = "%d/%m/%Y %H:%M:%S"),
    death_28d = as.integer(death_28d)
  ) %>%
  mutate(
    time28 = as.numeric(difftime(deathtime, intime, units = "days")),
    time28 = ifelse(is.na(time28), 28, pmin(time28, 28)),
    status28 = as.integer(death_28d == 1)
  ) %>%
  select(-intime, -deathtime, -death_28d) %>%
  filter(time28 > 0, !is.na(status28)) %>%
  as.data.frame()

train_data <- rsf_data[rsf_data$group == 1L, ]
train_data$group <- NULL

evaluation_horizon <- 28 - 1e-04
evaluation_times <- unique(c(seq(0, 27.5, by = 0.5), evaluation_horizon))

set.seed(2026)
n_death_train <- sum(train_data$status28 == 1L)
n_surv_train <- sum(train_data$status28 == 0L)
fold_id <- integer(nrow(train_data))
fold_id[train_data$status28 == 1L] <- sample(
  rep(1:5, length.out = n_death_train)
)
fold_id[train_data$status28 == 0L] <- sample(
  rep(1:5, length.out = n_surv_train)
)

tuning_grid <- expand.grid(
  ntree = c(300, 500, 1000),
  mtry = c(3, 6, 9, 12),
  nodesize = c(10, 20, 30, 40),
  nsplit = c(10, 25, 50)
)

n_grid <- nrow(tuning_grid)
n_fold <- 5L
n_total <- n_fold * n_grid
cv_metric_rows <- vector("list", n_total)
row_index <- 0L

cat(
  "训练集 n =", nrow(train_data),
  "死亡 =", n_death_train, "\n"
)
print(table(fold = fold_id, death = train_data$status28))
cat("总建模次数 =", n_total, "\n")
flush.console()

cv_start <- Sys.time()

for (k in seq_len(n_fold)) {
  fold_train <- train_data[fold_id != k, , drop = FALSE]
  fold_valid <- train_data[fold_id == k, , drop = FALSE]
  n_train_fold <- nrow(fold_train)
  n_valid_fold <- nrow(fold_valid)
  death_train_fold <- sum(fold_train$status28)
  death_valid_fold <- sum(fold_valid$status28)

  for (i in seq_len(n_grid)) {
    row_index <- row_index + 1L
    iter_start <- Sys.time()

    rsf_model <- randomForestSRC::rfsrc(
      Surv(time28, status28) ~ .,
      data = fold_train,
      ntree = tuning_grid$ntree[i],
      mtry = tuning_grid$mtry[i],
      nodesize = tuning_grid$nodesize[i],
      nsplit = tuning_grid$nsplit[i],
      splitrule = "logrank",
      na.action = "na.impute",
      importance = FALSE,
      seed = 2026
    )

    valid_prediction <- predict(
      rsf_model,
      newdata = fold_valid,
      na.action = "na.impute"
    )
    valid_time_index <- which.min(abs(valid_prediction$time.interest - 28))
    valid_probability <- 1 - valid_prediction$survival[, valid_time_index]
    valid_evaluation <- data.frame(
      time28 = fold_valid$time28,
      status28 = fold_valid$status28,
      probability = valid_probability
    )
    test_cindex <- survival::concordance(
      Surv(time28, status28) ~ probability,
      data = valid_evaluation,
      reverse = TRUE
    )$concordance

    oob_cindex <- 1 - tail(rsf_model$err.rate, 1)

    valid_rsf_score <- riskRegression::Score(
      object = list(RSF = rsf_model),
      formula = Hist(time28, status28) ~ 1,
      data = fold_valid,
      metrics = c("auc", "brier"),
      times = evaluation_times,
      summary = "ibs",
      cens.method = "ipcw",
      conf.int = FALSE,
      plots = NULL,
      predictRisk.args = list(rfsrc = list(na.action = "na.impute"))
    )
    valid_auc_score_table <- as.data.frame(valid_rsf_score$AUC$score)
    valid_brier_score_table <- as.data.frame(valid_rsf_score$Brier$score)

    test_auc_28 <- valid_auc_score_table$AUC[
      as.character(valid_auc_score_table$model) == "RSF" &
        abs(valid_auc_score_table$times - evaluation_horizon) < 1e-08
    ]
    test_brier_28_ipcw <- valid_brier_score_table$Brier[
      as.character(valid_brier_score_table$model) == "RSF" &
        abs(valid_brier_score_table$times - evaluation_horizon) < 1e-08
    ]
    test_ibs_ipcw <- valid_brier_score_table$IBS[
      as.character(valid_brier_score_table$model) == "RSF" &
        abs(valid_brier_score_table$times - evaluation_horizon) < 1e-08
    ]

    cv_metric_rows[[row_index]] <- data.frame(
      fold = k,
      combo = i,
      ntree = tuning_grid$ntree[i],
      mtry = tuning_grid$mtry[i],
      nodesize = tuning_grid$nodesize[i],
      nsplit = tuning_grid$nsplit[i],
      n_train = n_train_fold,
      n_valid = n_valid_fold,
      death_train = death_train_fold,
      death_valid = death_valid_fold,
      oobci = oob_cindex,
      testci = test_cindex,
      AUC28 = test_auc_28,
      Brier28_IPCW = test_brier_28_ipcw,
      IBS_0_28_IPCW = test_ibs_ipcw
    )

    elapsed_min <- as.numeric(difftime(Sys.time(), cv_start, units = "mins"))
    remain_min <- elapsed_min / row_index * (n_total - row_index)
    cat(sprintf(
      "[折 %d/5 | 组合 %d/%d] ntree=%d mtry=%d nodesize=%d nsplit=%d  本组合 %.1f 秒 | 已用 %.1f 分钟 | 预计剩余 %.1f 分钟\n",
      k, i, n_grid,
      tuning_grid$ntree[i], tuning_grid$mtry[i],
      tuning_grid$nodesize[i], tuning_grid$nsplit[i],
      as.numeric(difftime(Sys.time(), iter_start, units = "secs")),
      elapsed_min,
      remain_min
    ))
    flush.console()
  }
}

cv_results <- do.call(rbind, cv_metric_rows)
print(cv_results, digits = 4, row.names = FALSE)

out_dir <- "F:/文章_大论文/0722/实例研究代码/文件"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
out_file <- file.path(out_dir, "5_fold.csv")
write.csv(cv_results, out_file, row.names = FALSE, fileEncoding = "UTF-8")
cat("5折结果已保存至：\n", normalizePath(out_file), "\n")
