library(dplyr)
library(survival)
library(timeROC)
library(pROC)

stroke_baselinedata_filter1_0531 <- read.csv2(
  "F:/文章_大论文/0521/处理后文件/stroke_baselinedata_filter1_0531.csv"
)

stroke_longitudinal_inputed_0603 <- read.csv(
  "F:/文章_大论文/0521/处理后文件/stroke_longitudinal_inputed_0603.csv"
)


# 按 hadm_id 分为 5 折（同一患者所有记录进入同一折，两表折号一致）
set.seed(1234)
unique_hadm_id <- unique(c(
  stroke_baselinedata_filter1_0531$hadm_id,
  stroke_longitudinal_inputed_0603$hadm_id
))
fold_labels <- sample(rep(1:5, length.out = length(unique_hadm_id)))
id_fold_map <- setNames(fold_labels, unique_hadm_id)
make_fold_list <- function(data, id_col = "hadm_id") {
  lapply(1:5, function(k) {
    ids_k <- as.integer(names(id_fold_map)[id_fold_map == k])
    data[data[[id_col]] %in% ids_k, , drop = FALSE]
  })
}
Data_baseline_5fold <- make_fold_list(stroke_baselinedata_filter1_0531)
Data_longitude_5fold <- make_fold_list(stroke_longitudinal_inputed_0603)














###############程序#######################


prep_cox_inputs <- function(baseline_data, longitude_data) {
  stopifnot(is.data.frame(baseline_data), is.data.frame(longitude_data))
  
  base_need <- c(
    "hadm_id", "age",
    "intime", "deathtime", "death_28d"
  )
  if (!all(base_need %in% names(baseline_data))) {
    stop(
      "baseline_data 缺少必要列: ",
      paste(setdiff(base_need, names(baseline_data)), collapse = ", ")
    )
  }
  if (!all(c("hadm_id", "times", "gcs", "sofa_24hours") %in% names(longitude_data))) {
    stop("longitude_data 需含 hadm_id, times, gcs, sofa_24hours")
  }
  
  static_cols <- c("age")
  
  long_pred_cols <- c(
    "gcs",
    "sofa_24hours"
  )
  
  base_df <- baseline_data %>%
    dplyr::transmute(
      hadm_id = as.integer(hadm_id),
      age = as.numeric(age),
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
  
  bad_time_ids <- base_df$hadm_id[is.na(base_df$time28) | base_df$time28 <= 0]
  if (length(bad_time_ids) > 0) {
    warning(
      "prep_cox_inputs：剔除 time28 <= 0 的患者 n = ", length(bad_time_ids),
      " | 示例 hadm_id: ", paste(head(bad_time_ids, 5), collapse = ", ")
    )
    base_df <- base_df[!base_df$hadm_id %in% bad_time_ids, , drop = FALSE]
  }
  
  bad_event_ids <- base_df$hadm_id[is.na(base_df$status28)]
  if (length(bad_event_ids) > 0) {
    warning("prep_cox_inputs：剔除 status28 缺失的患者 n = ", length(bad_event_ids))
    base_df <- base_df[!base_df$hadm_id %in% bad_event_ids, , drop = FALSE]
  }
  
  long_t1 <- longitude_data %>%
    dplyr::filter(times == 1) %>%
    dplyr::transmute(
      hadm_id = as.integer(hadm_id),
      gcs = as.numeric(gcs),
      sofa_24hours = as.numeric(sofa_24hours)
    )
  
  if (anyNA(long_t1[, long_pred_cols, drop = FALSE])) {
    na_cnt <- colSums(is.na(long_t1[, long_pred_cols, drop = FALSE]))
    warning(
      "prep_cox_inputs：day-1 纵向变量存在 NA | ",
      paste(names(na_cnt), na_cnt, sep = "=", collapse = ", ")
    )
  }
  
  merged <- base_df %>%
    dplyr::inner_join(long_t1, by = "hadm_id")
  
  if (nrow(merged) == 0) {
    stop("prep_cox_inputs：合并后无患者，请检查 times==1 的 longitudinal 数据")
  }
  
  cox_data <- merged %>%
    dplyr::select(
      age,
      gcs,
      sofa_24hours,
      time28,
      status28
    ) %>%
    as.data.frame()
  
  if (!all(cox_data$time28 > 0, na.rm = TRUE)) {
    stop("prep_cox_inputs：仍有 time28 <= 0")
  }
  if (any(is.na(cox_data$status28)) || !all(cox_data$status28 %in% c(0L, 1L))) {
    stop("prep_cox_inputs：status28 非 0/1 或含 NA")
  }
  
  attr(cox_data, "static_cols")  <- static_cols
  attr(cox_data, "long_pred_cols") <- long_pred_cols
  attr(cox_data, "pred_cols")      <- c(static_cols, long_pred_cols)
  cox_data
}




cal_3_COX <- function(model, t0, eval_data = NULL) {
  stopifnot(inherits(model, "coxph"))
  if (!requireNamespace("pROC", quietly = TRUE)) {
    stop("请先安装 pROC: install.packages('pROC')")
  }
  if (!requireNamespace("survival", quietly = TRUE)) {
    stop("请先安装 survival")
  }
  
  model_name <- attr(model, "model_name", exact = TRUE)
  if (is.null(model_name)) model_name <- "COX"
  
  pred_cols <- attr(model, "pred_cols", exact = TRUE)
  
  # ---- 组装评估数据 ----
  if (!is.null(eval_data)) {
    cox_eval <- as.data.frame(eval_data)
    if (!all(c("time28", "status28") %in% names(cox_eval))) {
      stop("eval_data 需含 time28 与 status28")
    }
    if (is.null(pred_cols)) {
      pred_cols <- setdiff(names(cox_eval), c("time28", "status28", "X.1", "X"))
    }
    if (!all(pred_cols %in% names(cox_eval))) {
      stop("eval_data 缺少模型预测变量: ",
           paste(setdiff(pred_cols, names(cox_eval)), collapse = ", "))
    }
    newdata  <- cox_eval[, pred_cols, drop = FALSE]
    time28   <- cox_eval$time28
    status28 <- cox_eval$status28
  } else {
    mf <- stats::model.frame(model)
    if (is.null(mf) || nrow(mf) == 0) {
      stop("cal_3_COX：无 eval_data 时需 coxph(..., model = TRUE) 拟合")
    }
    tt <- stats::terms(model)
    resp_idx <- attr(tt, "response")
    newdata <- if (!is.null(resp_idx) && resp_idx > 0) {
      mf[, -resp_idx, drop = FALSE]
    } else {
      mf
    }
    y_surv <- stats::model.response(mf)
    time28   <- y_surv[, 1L]
    status28 <- y_surv[, 2L]
  }
  
  n_total <- length(time28)
  
  cat(
    "[cal_3_COX] 模型:", model_name,
    "| t0 =", t0,
    "| n =", n_total, "\n"
  )
  
  # ---- 预测 prob_dead = 1 - S(t0 | x) ----
  cat("[cal_3_COX] 正在 predict（survfit + summary）...\n")
  
  sf <- survival::survfit(
    model,
    newdata  = newdata,
    se.fit   = FALSE,
    conf.int = FALSE
  )
  
  ss <- summary(sf, times = t0, extend = TRUE)
  prob_dead <- 1 - as.numeric(ss$surv)
  
  if (length(prob_dead) != n_total) {
    stop(
      "cal_3_COX：prob_dead 长度 (", length(prob_dead),
      ") 与样本数 (", n_total, ") 不一致，请检查 newdata"
    )
  }
  
  prob_dead <- pmin(pmax(prob_dead, 0), 1)
  
  pred_df <- data.frame(
    time28    = time28,
    status28  = as.integer(status28),
    y_dead    = as.integer(status28),
    prob_dead = prob_dead,
    stringsAsFactors = FALSE
  )
  
  n_na   <- sum(is.na(pred_df$prob_dead))
  n_inf  <- sum(pred_df$prob_dead == Inf,  na.rm = TRUE)
  n_ninf <- sum(pred_df$prob_dead == -Inf, na.rm = TRUE)
  pred_df_eval <- pred_df[is.finite(pred_df$prob_dead), , drop = FALSE]
  
  cat(
    "[cal_3_COX] prob_dead NA 数:", n_na,
    "| Inf 数:", n_inf,
    "| -Inf 数:", n_ninf,
    "| 用于指标计算样本数:", nrow(pred_df_eval),
    "（剔除非有限值后）\n"
  )
  
  if (nrow(pred_df_eval) == 0) {
    stop("cal_3_COX：剔除 NA/Inf 后无有效 prob_dead，无法计算指标")
  }
  if (length(unique(pred_df_eval$y_dead)) < 2) {
    stop("cal_3_COX：有效样本中 y_dead 仅单一类别，无法计算 AUC")
  }
  
  roc_obj <- pROC::roc(
    response  = pred_df_eval$y_dead,
    predictor = pred_df_eval$prob_dead,
    levels    = c(0, 1),
    direction = "<",
    quiet     = TRUE
  )
  auc_val <- as.numeric(pROC::auc(roc_obj))
  cat("[cal_3_COX] 完成计算 AUC =", round(auc_val, 4), "\n")
  
  cindex_obj <- survival::concordance(
    survival::Surv(pred_df_eval$time28, pred_df_eval$status28) ~ pred_df_eval$prob_dead,
    reverse = TRUE
  )
  cindex_val <- cindex_obj$concordance
  cat("[cal_3_COX] 完成计算 C-index =", round(cindex_val, 4), "\n")
  
  bs_val <- mean((pred_df_eval$y_dead - pred_df_eval$prob_dead)^2)
  cat("[cal_3_COX] 完成计算 Brier Score =", round(bs_val, 4), "\n")
  
  out_df <- data.frame(
    model   = model_name,
    auc     = round(auc_val, 4),
    cindex  = round(cindex_val, 4),
    bs      = round(bs_val, 4),
    n       = nrow(pred_df_eval),
    n_total = n_total,
    stringsAsFactors = FALSE
  )
  cat("[cal_3_COX] 全部指标计算完成\n")
  out_df
}


COX_1 <- function(cox_data) {
  stopifnot(is.data.frame(cox_data))
  if (!requireNamespace("survival", quietly = TRUE)) {
    stop("请先安装 survival")
  }
  if (!all(c("time28", "status28") %in% names(cox_data))) {
    stop("cox_data 需含 time28 与 status28")
  }
  
  pred_cols <- attr(cox_data, "pred_cols", exact = TRUE)
  if (is.null(pred_cols)) {
    pred_cols <- setdiff(names(cox_data), c("time28", "status28", "X.1", "X"))
  }
  
  n_event <- sum(cox_data$status28, na.rm = TRUE)
  if (nrow(cox_data) < 3L) stop("cox_data 行数过少")
  if (n_event == 0L) stop("cox_data 中无死亡事件，无法拟合 Cox")
  
  cat(
    "[COX_1] 开始 coxph | n =", nrow(cox_data),
    "| 预测变量数 =", length(pred_cols),
    "| 事件数 =", n_event, "\n"
  )
  
  fml <- as.formula(
    paste("Surv(time28, status28) ~", paste(pred_cols, collapse = " + "))
  )
  
  cox_fit <- survival::coxph(
    formula = fml,
    data    = cox_data,
    x       = TRUE,
    y       = TRUE,
    model   = TRUE
  )
  
  attr(cox_fit, "model_name") <- "COX_1"
  attr(cox_fit, "pred_cols")  <- pred_cols
  
  cat(
    "[COX_1] 建模完成 | 实际 n =", cox_fit$n,
    "| 事件数 =", cox_fit$nevent,
    "| concordance =", round(summary(cox_fit)$concordance[1], 4), "\n"
  )
  
  cox_fit
}


COX_fold <- function(data, t0, predictiondata) {
  stopifnot(
    is.list(data),
    is.list(predictiondata),
    is.data.frame(data$baselinedata),
    is.data.frame(data$longitudedata),
    is.data.frame(predictiondata$baselinedata),
    is.data.frame(predictiondata$longitudedata)
  )
  
  train_cox <- prep_cox_inputs(
    baseline_data  = data$baselinedata,
    longitude_data  = data$longitudedata
  )
  val_cox <- prep_cox_inputs(
    baseline_data  = predictiondata$baselinedata,
    longitude_data  = predictiondata$longitudedata
  )
  
  cat(
    "[COX_fold] 训练 n =", nrow(train_cox),
    "| 验证 n =", nrow(val_cox), "\n"
  )
  
  cox_fit <- COX_1(train_cox)
  attr(cox_fit, "model_name") <- "COX_fold"
  
  metrics_cox <- cal_3_COX(
    model     = cox_fit,
    t0        = t0,
    eval_data = val_cox
  )
  
  list(
    metrics_cox = metrics_cox,
    cox_fit     = cox_fit,
    train_cox   = train_cox,
    val_cox     = val_cox
  )
}



#################5折交叉验证#########################

t0_cv <- 28   # 与 28 天结局对齐；若与 RSF 比 t0=5 则改为 5

metrics_cox_cv <- NULL
models_cox_cv  <- vector("list", 5)

for (k in 1:5) {
  cat("\n========== 5折 CV COX | fold", k, "==========\n")
  train_idx <- setdiff(1:5, k)
  
  out_k <- COX_fold(
    data = list(
      baselinedata  = dplyr::bind_rows(Data_baseline_5fold[train_idx]),
      longitudedata = dplyr::bind_rows(Data_longitude_5fold[train_idx])
    ),
    t0 = t0_cv,
    predictiondata = list(
      baselinedata  = Data_baseline_5fold[[k]],
      longitudedata = Data_longitude_5fold[[k]]
    )
  )
  
  metrics_k <- out_k$metrics_cox
  metrics_k$fold <- k
  metrics_cox_cv <- rbind(metrics_cox_cv, metrics_k)
  models_cox_cv[[k]] <- out_k$cox_fit
}

metrics_cox_cv
write.csv(metrics_cox_cv,"F:/文章_大论文/0521/处理后文件/结果/RSFLC交叉验证结果/metrics_cox_cv.csv")



###############调试#######################










