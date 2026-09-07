# =============================================================================
# RSF 网格搜索（至少 500 组超参）
# 数据：stroke_baselinedata_0824.csv
# 准则：训练集 OOB error（err.rate 末值，越小越好）
# =============================================================================

library(dplyr)
library(survival)
library(randomForestSRC)

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

set.seed(2026)
death_rows <- which(rsf_data$status28 == 1L)
survival_rows <- which(rsf_data$status28 == 0L)
train_rows <- sort(c(
  sample(death_rows, floor(length(death_rows) * 0.7)),
  sample(survival_rows, floor(length(survival_rows) * 0.7))
))
train_data <- rsf_data[train_rows, ]
test_data <- rsf_data[-train_rows, ]

n_predictor <- ncol(train_data) - 2L

# ---------------------------------------------------------------------------
# 网格：文献常用范围 × 当前数据 p 个协变量
#   ntree    Ishwaran 默认 500；Cavus 等 (2025) 搜 [100, 2000]
#   mtry     从 2 到 p；sqrt(p)≈3，Wang 等脑出血 RSF 网格后取较大 mtry
#   nodesize 生存默认 15；tune.rfsrc 搜 1–100；Wang 等最优 8；Cavus 搜 [5, 100]
#   nsplit   默认 10；Cavus 搜 [5, 15]；你原先还包含 25、50
# 组合数：4 × 10 × 7 × 4 = 1120
# ---------------------------------------------------------------------------
tuning_results <- expand.grid(
  ntree    = c(200, 300, 500, 1000),
  mtry     = 2:n_predictor,
  nodesize = c(5, 8, 10, 15, 20, 30, 40),
  nsplit   = c(5, 10, 15, 25)
)
tuning_results$mtry <- pmin(tuning_results$mtry, n_predictor)
tuning_results$oob_error <- NA_real_
tuning_results$elapsed_sec <- NA_real_

n_grid <- nrow(tuning_results)
cat("协变量数 p =", n_predictor, "；网格组合数 =", n_grid, "\n")

tune_start <- Sys.time()
for (i in seq_len(n_grid)) {
  iter_start <- Sys.time()
  tuning_model <- randomForestSRC::rfsrc(
    Surv(time28, status28) ~ .,
    data = train_data,
    ntree = tuning_results$ntree[i],
    mtry = tuning_results$mtry[i],
    nodesize = tuning_results$nodesize[i],
    nsplit = tuning_results$nsplit[i],
    splitrule = "logrank",
    na.action = "na.impute",
    importance = FALSE,
    seed = 2026
  )
  tuning_results$oob_error[i] <- tail(tuning_model$err.rate, 1)
  tuning_results$elapsed_sec[i] <- as.numeric(difftime(Sys.time(), iter_start, units = "secs"))

  if (i %% 10L == 0L || i == n_grid) {
    elapsed <- as.numeric(difftime(Sys.time(), tune_start, units = "mins"))
    eta <- elapsed / i * (n_grid - i)
    cat(
      sprintf(
        "[%d/%d] oob=%.4f | 已用 %.1f min | 预计剩余 %.1f min\n",
        i, n_grid, tuning_results$oob_error[i], elapsed, eta
      )
    )
  }
}

tuning_results <- tuning_results %>% arrange(oob_error)
best_ntree <- tuning_results$ntree[1]
best_mtry <- tuning_results$mtry[1]
best_nodesize <- tuning_results$nodesize[1]
best_nsplit <- tuning_results$nsplit[1]

write.csv(
  tuning_results,
  "F:/文章_大论文/0722/实例研究代码/RSF_tuning_results_0824.csv",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

cat(
  "最优: ntree=", best_ntree,
  " mtry=", best_mtry,
  " nodesize=", best_nodesize,
  " nsplit=", best_nsplit,
  " oob_error=", round(tuning_results$oob_error[1], 4), "\n",
  sep = ""
)

rsf_model <- randomForestSRC::rfsrc(
  Surv(time28, status28) ~ .,
  data = train_data,
  ntree = best_ntree,
  mtry = best_mtry,
  nodesize = best_nodesize,
  nsplit = best_nsplit,
  splitrule = "logrank",
  na.action = "na.impute",
  importance = FALSE,
  seed = 2026
)

tuning_metric_rows[[1]]
