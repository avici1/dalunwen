library(dplyr)
library(survival)
library(randomForestSRC)
library(timeROC)
library(parallel)

options(rf.cores = parallel::detectCores() - 1L)

stroke_baselinedata_0531 <- read.csv(
  "F:/文章_大论文/0722/重新构筑代码/stroke_baselinedata_0531.csv"
)

stroke_longitudinal_inputed_0603 <- read.csv(
  "F:/文章_大论文/0722/重新构筑代码/stroke_longitudinal_inputed_0603.csv"
)

prep_rsf_inputs <- function(baseline_data, longitude_data) {
  stopifnot(is.data.frame(baseline_data), is.data.frame(longitude_data))
  
  base_need <- c(
    "hadm_id", "age", "charlson_comorbidity_index", "apsiii", "sapsii", "oasis",
    "preiculos", "mechvent", "electivesurgery",
    "intime", "deathtime", "death_28d"
  )
  if (!all(base_need %in% names(baseline_data))) {
    stop(
      "baseline_data 缺少必要列: ",
      paste(setdiff(base_need, names(baseline_data)), collapse = ", ")
    )
  }
  if (!all(c("hadm_id", "times") %in% names(longitude_data))) {
    stop("longitude_data 需含 hadm_id, times")
  }
  
  base_df <- baseline_data %>%
    dplyr::transmute(
      hadm_id = as.integer(hadm_id),
      age = as.numeric(age),
      charlson_comorbidity_index = as.integer(charlson_comorbidity_index),
      apsiii = as.integer(apsiii),
      sapsii = as.integer(sapsii),
      oasis = as.integer(oasis),
      preiculos = as.numeric(preiculos),
      mechvent = factor(mechvent, levels = c(0, 1), labels = c("no", "yes")),
      electivesurgery = factor(electivesurgery, levels = c(0, 1), labels = c("no", "yes")),
      intime = intime,
      deathtime = deathtime,
      death_28d = death_28d
    )
  
  intime_parsed <- as.POSIXct(base_df$intime, format = "%d/%m/%Y %H:%M:%S")
  deathtime_chr <- ifelse(
    base_df$deathtime == "" | is.na(base_df$deathtime),
    NA_character_,
    base_df$deathtime
  )
  deathtime_parsed <- as.POSIXct(deathtime_chr, format = "%d/%m/%Y %H:%M:%S")
  time_to_death <- as.numeric(difftime(deathtime_parsed, intime_parsed, units = "days"))
  time_to_death[is.na(time_to_death)] <- 28
  
  base_df$time28   <- pmin(time_to_death, 28)
  base_df$status28 <- as.integer(as.numeric(as.character(base_df$death_28d)) == 1)
  
  long_t1 <- longitude_data %>%
    dplyr::filter(times == 1) %>%
    dplyr::mutate(hadm_id = as.integer(hadm_id))
  
  long_drop <- c("X", "subject_id", "hadm_id", "chart_date", "times")
  long_pred_cols <- setdiff(names(long_t1), long_drop)
  
  long_t1 <- long_t1 %>%
    dplyr::select(dplyr::all_of(c("hadm_id", long_pred_cols)))
  
  for (col in long_pred_cols) {
    if (is.character(long_t1[[col]])) {
      long_t1[[col]] <- as.numeric(long_t1[[col]])
    }
  }
  
  merged <- base_df %>%
    dplyr::inner_join(long_t1, by = "hadm_id")
  
  if (nrow(merged) == 0) {
    stop("prep_rsf_inputs：合并后无患者，请检查 times==1 的 longitudinal 数据")
  }
  
  rsf_data <- merged %>%
    dplyr::select(
      age, charlson_comorbidity_index, apsiii, sapsii, oasis, preiculos,
      mechvent, electivesurgery,
      dplyr::all_of(long_pred_cols),
      time28, status28
    ) %>%
    as.data.frame()
  
  meta <- merged %>%
    dplyr::select(hadm_id, time28, status28, death_28d) %>%
    as.data.frame()
  
  stopifnot(
    nrow(rsf_data) == nrow(meta),
    nrow(rsf_data) == nrow(merged)
  )
  
  list(
    rsf_data = rsf_data,
    meta     = meta,
    long_pred_cols = long_pred_cols
  )
}

cal_3_rsf_test <- function(model, t0, eval_data = NULL) {
  stopifnot(inherits(model, "rfsrc"))
  if (!requireNamespace("pROC", quietly = TRUE)) {
    stop("请先安装 pROC: install.packages('pROC')")
  }
  if (!requireNamespace("survival", quietly = TRUE)) {
    stop("请先安装 survival")
  }
  
  model_name <- attr(model, "model_name", exact = TRUE)
  if (is.null(model_name)) model_name <- "RSF"
  
  if (!is.null(eval_data)) {
    rsf_data <- as.data.frame(eval_data)
    if (!all(c("time28", "status28") %in% names(rsf_data))) {
      stop("eval_data 需含 time28 与 status28")
    }
  } else if (!is.null(model$data) &&
             all(c("time28", "status28") %in% names(model$data))) {
    rsf_data <- as.data.frame(model$data)
  } else if (!is.null(model$yvar) && !is.null(model$xvar)) {
    rsf_data <- as.data.frame(model$xvar)
    y_nm <- model$yvar.names
    if (!all(c("time28", "status28") %in% y_nm)) {
      stop("model$yvar 需含 time28/status28，当前为: ",
           paste(y_nm, collapse = ", "))
    }
    rsf_data$time28   <- model$yvar[["time28"]]
    rsf_data$status28 <- model$yvar[["status28"]]
  } else {
    stop("无法从 model 获取评估数据；请传入 eval_data")
  }
  
  pred_cols <- model$xvar.names
  if (is.null(pred_cols)) {
    pred_cols <- attr(model, "pred_cols", exact = TRUE)
  }
  if (is.null(pred_cols) || !all(pred_cols %in% names(rsf_data))) {
    stop("评估数据缺少模型预测变量列")
  }
  
  cat(
    "[cal_3_rsf_test] 模型:", model_name,
    "| t0 =", t0,
    "| n =", nrow(rsf_data), "\n"
  )
  
  cat("[cal_3_rsf_test] 正在 predict...\n")
  x_new <- rsf_data[, pred_cols, drop = FALSE]
  pred  <- predict(model, newdata = x_new)
  
  time_points <- pred$time.interest
  t0_idx      <- which.min(abs(time_points - t0))
  prob_dead   <- 1 - pred$survival[, t0_idx]
  
  pred_df <- data.frame(
    time28    = rsf_data$time28,
    status28  = rsf_data$status28,
    y_dead    = as.integer(rsf_data$status28),
    prob_dead = prob_dead,
    stringsAsFactors = FALSE
  )
  
  n_na   <- sum(is.na(pred_df$prob_dead))
  n_inf  <- sum(pred_df$prob_dead == Inf,  na.rm = TRUE)
  n_ninf <- sum(pred_df$prob_dead == -Inf, na.rm = TRUE)
  pred_df_eval <- pred_df[is.finite(pred_df$prob_dead), , drop = FALSE]
  
  cat(
    "[cal_3_rsf_test] prob_dead NA 数:", n_na,
    "| Inf 数:", n_inf,
    "| -Inf 数:", n_ninf,
    "| 用于指标计算样本数:", nrow(pred_df_eval),
    "（剔除非有限值后）\n"
  )
  
  if (nrow(pred_df_eval) == 0) {
    stop("cal_3_rsf_test：剔除 NA/Inf 后无有效 prob_dead，无法计算指标")
  }
  if (length(unique(pred_df_eval$y_dead)) < 2) {
    stop("cal_3_rsf_test：有效样本中 y_dead 仅单一类别，无法计算 AUC")
  }
  
  roc_obj <- pROC::roc(
    response  = pred_df_eval$y_dead,
    predictor = pred_df_eval$prob_dead,
    levels    = c(0, 1),
    direction = "<",
    quiet     = TRUE
  )
  auc_val <- as.numeric(pROC::auc(roc_obj))
  cat("[cal_3_rsf_test] 完成计算 AUC =", round(auc_val, 4), "\n")
  
  cindex_obj <- survival::concordance(
    survival::Surv(pred_df_eval$time28, pred_df_eval$status28) ~ pred_df_eval$prob_dead,
    reverse = TRUE
  )
  cindex_val <- cindex_obj$concordance
  cat("[cal_3_rsf_test] 完成计算 C-index =", round(cindex_val, 4), "\n")
  
  bs_val <- mean((pred_df_eval$y_dead - pred_df_eval$prob_dead)^2)
  cat("[cal_3_rsf_test] 完成计算 Brier Score =", round(bs_val, 4), "\n")
  
  out_df <- data.frame(
    model   = model_name,
    auc     = round(auc_val, 4),
    cindex  = round(cindex_val, 4),
    bs      = round(bs_val, 4),
    n       = nrow(pred_df_eval),
    n_total = nrow(pred_df),
    stringsAsFactors = FALSE
  )
  cat("[cal_3_rsf_test] 全部指标计算完成\n")
  out_df
}

RSF_1 <- function(data) {
  stopifnot(is.list(data), !is.null(data$rsf_data))
  if (!requireNamespace("survival", quietly = TRUE)) {
    stop("请先安装 survival")
  }
  rsf_data <- data$rsf_data
  
  if (!all(c("time28", "status28") %in% names(rsf_data))) {
    stop("rsf_data 需含 time28 与 status28")
  }
  
  n_event <- sum(rsf_data$status28, na.rm = TRUE)
  if (nrow(rsf_data) < 3L) {
    stop("rsf_data 行数过少，无法建模")
  }
  if (n_event == 0L) {
    stop("rsf_data 中无死亡事件(status28=1)，无法拟合 survival 模型")
  }
  
  drop_cols <- c("time28", "status28", "X.1", "X")
  pred_cols <- setdiff(names(rsf_data), drop_cols)
  n_pred <- length(pred_cols)
  
  if (n_pred < 1L) {
    stop("无可用预测变量")
  }
  
  cat("[RSF_1] 开始 rfsrc 建模 | n =", nrow(rsf_data),
      "| 预测变量数 =", n_pred,
      "| 事件数 =", n_event, "\n")
  
  fml <- as.formula(
    paste("Surv(time28, status28) ~", paste(pred_cols, collapse = " + "))
  )
  
  rfsrc_fit <- randomForestSRC::rfsrc(
    fml,
    data       = rsf_data,
    ntree      = 100,
    mtry       = max(1L, floor(n_pred / 3)),
    nodesize   = 10,
    importance = F,
    proximity  = FALSE,
    seed       = 123
  )
  
  attr(rfsrc_fit, "model_name") <- "RSF_1"
  attr(rfsrc_fit, "pred_cols")  <- pred_cols
  attr(rfsrc_fit, "prep_meta")  <- data$meta
  if (!is.null(data$long_pred_cols)) {
    attr(rfsrc_fit, "long_pred_cols") <- data$long_pred_cols
  }
  
  oob_err <- if (!is.null(rfsrc_fit$err.rate)) {
    as.numeric(rfsrc_fit$err.rate[length(rfsrc_fit$err.rate)])
  } else {
    NA_real_
  }
  
  cat("[RSF_1] 建模完成 | ntree =", rfsrc_fit$ntree,
      "| OOB err =", round(oob_err, 4), "\n")
  
  rfsrc_fit
}

############### 全量建模 + 指标 #######################

t0 <- 5

inputs_rsf <- prep_rsf_inputs(
  baseline_data  = stroke_baselinedata_0531,
  longitude_data = stroke_longitudinal_inputed_0603
)

rfsrc_fit <- RSF_1(inputs_rsf)

metrics_rsf <- cal_3_rsf_test(
  model     = rfsrc_fit,
  t0        = t0,
  eval_data = inputs_rsf$rsf_data
)

print(metrics_rsf)
