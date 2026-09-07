library(dplyr)
library(survival)
library(randomForestSRC)
library(riskRegression)
library(prodlim)
library(ggplot2)
library(fastshap)
library(shapviz)
library(rmda)
library(survminer)
library(patchwork)

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
  filter(time28 > 0, !is.na(status28), group %in% c(1L, 2L)) %>%
  as.data.frame()

# 使用数据中已有分组：group=1 训练（70%），group=2 测试（30%）
stopifnot("group" %in% names(rsf_data))
train_data <- rsf_data[rsf_data$group == 1L, ]
test_data <- rsf_data[rsf_data$group == 2L, ]
stopifnot(nrow(train_data) > 0L, nrow(test_data) > 0L)
train_data$group <- NULL
test_data$group <- NULL

evaluation_horizon <- 28 - 1e-04
evaluation_times <- unique(c(seq(0, 27.5, by = 0.5), evaluation_horizon))

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
    testci = test_cindex,
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

write.csv(tuning_results,"F:/文章_大论文/0722/实例研究代码/results_RSF.CSV")


















#############作图############
# combo 2：ntree=500, mtry=3, nodesize=10, nsplit=10
best_ntree <- 500
best_mtry <- 3
best_nodesize <- 10
best_nsplit <- 10

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

test_prediction <- predict(
  rsf_model,
  newdata = test_data,
  na.action = "na.impute"
)
test_time_index <- which.min(abs(test_prediction$time.interest - 28))
test_probability <- 1 - test_prediction$survival[, test_time_index]

fig_dir <- "F:/文章_大论文/0722/实例研究代码/图像_RSF"
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)

set.seed(2026)
vimp_result <- randomForestSRC::vimp(
  rsf_model,
  importance = "permute",
  seed = 2026
)

vimp_table <- data.frame(
  variable = names(vimp_result$importance),
  importance = as.numeric(vimp_result$importance),
  row.names = NULL
) %>%
  arrange(desc(importance))

vimp_plot <- ggplot2::ggplot(
  head(vimp_table, 10),
  ggplot2::aes(x = reorder(variable, importance), y = importance)
) +
  ggplot2::geom_col(fill = "#2F75B5") +
  ggplot2::coord_flip() +
  ggplot2::labs(
    x = NULL,
    y = "Permutation VIMP",
    title = "RSF变量重要性（前10项）"
  ) +
  ggplot2::theme_minimal(base_size = 12) +
  ggplot2::theme(
    plot.title = ggplot2::element_text(hjust = 0.5, face = "bold")
  )

vimp_file <- file.path(fig_dir, "01_VIMP.png")
ggplot2::ggsave(vimp_file, vimp_plot, width = 7, height = 5.5, dpi = 300)
write.csv(vimp_table, file.path(fig_dir, "01_VIMP.csv"), row.names = FALSE, fileEncoding = "UTF-8")

predict_rsf_28 <- function(object, newdata) {
  prediction_object <- predict(
    object,
    newdata = newdata,
    na.action = "na.impute"
  )
  prediction_time_index <- which.min(
    abs(prediction_object$time.interest - 28)
  )
  1 - prediction_object$survival[, prediction_time_index]
}

train_predictors <- as.data.frame(rsf_model$xvar)
test_predictors <- test_data %>%
  select(all_of(names(train_predictors)))

fill_missing_like_train <- function(newdata, template) {
  out <- newdata
  for (nm in names(template)) {
    if (is.numeric(template[[nm]])) {
      fill_value <- median(template[[nm]], na.rm = TRUE)
      out[[nm]][is.na(out[[nm]])] <- fill_value
    } else {
      fill_value <- names(sort(table(template[[nm]]), decreasing = TRUE))[1]
      out[[nm]][is.na(out[[nm]])] <- fill_value
      if (is.factor(template[[nm]])) {
        out[[nm]] <- factor(out[[nm]], levels = levels(template[[nm]]))
      }
    }
  }
  out
}

test_predictors <- fill_missing_like_train(test_predictors, train_predictors)

high_risk_death_index <- which.max(
  ifelse(test_data$status28 == 1L, test_probability, -Inf)
)
low_risk_survivor_index <- which.min(
  ifelse(test_data$status28 == 0L, test_probability, Inf)
)

set.seed(2026)
remaining_indices <- setdiff(
  seq_len(nrow(test_predictors)),
  c(high_risk_death_index, low_risk_survivor_index)
)
shap_indices <- c(
  high_risk_death_index,
  low_risk_survivor_index,
  sample(remaining_indices, min(198, length(remaining_indices)))
)
shap_data <- test_predictors[shap_indices, , drop = FALSE]

set.seed(2026)
shap_values <- fastshap::explain(
  object = rsf_model,
  X = train_predictors,
  pred_wrapper = predict_rsf_28,
  newdata = shap_data,
  nsim = 20,
  adjust = TRUE
)

shap_object <- shapviz::shapviz(shap_values, X = shap_data)

shap_beeswarm_plot <- shapviz::sv_importance(
  shap_object,
  kind = "beeswarm",
  max_display = 20
) +
  ggplot2::ggtitle("RSF全局SHAP beeswarm") +
  ggplot2::theme(
    plot.title = ggplot2::element_text(hjust = 0.5, face = "bold")
  )

shap_beeswarm_file <- file.path(fig_dir, "02_SHAP_beeswarm.png")
ggplot2::ggsave(
  shap_beeswarm_file,
  shap_beeswarm_plot,
  width = 7,
  height = 6,
  dpi = 300
)

patient_shap_plot_1 <- shapviz::sv_waterfall(
  shap_object,
  row_id = 1,
  max_display = 12
) +
  ggplot2::ggtitle("典型患者1：高风险死亡")

patient_shap_plot_2 <- shapviz::sv_waterfall(
  shap_object,
  row_id = 2,
  max_display = 12
) +
  ggplot2::ggtitle("典型患者2：低风险存活")

patient_shap_plot <- patient_shap_plot_1 / patient_shap_plot_2
patient_shap_file <- file.path(fig_dir, "03_典型患者_SHAP.png")
ggplot2::ggsave(
  patient_shap_file,
  patient_shap_plot,
  width = 7,
  height = 9,
  dpi = 300
)

top_shap_variable <- vimp_table$variable[1]
shap_dependence_plot <- shapviz::sv_dependence(
  shap_object,
  v = top_shap_variable,
  color_var = "auto"
) +
  ggplot2::ggtitle(paste0("SHAP dependence：", top_shap_variable)) +
  ggplot2::theme(
    plot.title = ggplot2::element_text(hjust = 0.5, face = "bold")
  )

shap_dependence_file <- file.path(fig_dir, "04_SHAP_dependence.png")
ggplot2::ggsave(
  shap_dependence_file,
  shap_dependence_plot,
  width = 7,
  height = 5,
  dpi = 300
)

calibration_data <- data.frame(
  time28 = test_data$time28,
  status28 = test_data$status28,
  predicted_risk = test_probability
) %>%
  mutate(risk_decile = dplyr::ntile(predicted_risk, 10))

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
    filter(risk_decile == i)
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
  calibration_table$predicted_risk[i] <- mean(
    calibration_group$predicted_risk
  )
  calibration_table$observed_risk[i] <- 1 - calibration_summary$surv
  calibration_table$observed_low[i] <- 1 - calibration_summary$upper
  calibration_table$observed_high[i] <- 1 - calibration_summary$lower
}

calibration_plot <- ggplot2::ggplot(
  calibration_table,
  ggplot2::aes(x = predicted_risk, y = observed_risk)
) +
  ggplot2::geom_abline(
    intercept = 0, slope = 1, linetype = 2, color = "grey40"
  ) +
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
ggplot2::ggsave(
  calibration_file,
  calibration_plot,
  width = 6.5,
  height = 5.5,
  dpi = 300
)

dca_data <- data.frame(
  death_28d = test_data$status28,
  rsf_risk = test_probability
)

dca_result <- rmda::decision_curve(
  death_28d ~ rsf_risk,
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
  curve.names = "RSF",
  xlab = "Threshold probability",
  ylab = "Net benefit",
  legend.position = "topright",
  standardize = FALSE
)
title("测试集28天死亡风险决策曲线")
dev.off()

km_data <- data.frame(
  time28 = test_data$time28,
  status28 = test_data$status28,
  predicted_risk = test_probability
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
