library(dplyr)
library(survival)
library(timeROC)
library(joineRML)      # mjoint()


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


















t0_cv <- 5
metrics_jm_cv <- NULL
models_jm_cv  <- vector("list", 5)
for (k in 1:5) {
  cat("\n========== 5折 CV JM GCS (简化) | fold", k, "==========\n")
  train_idx <- setdiff(1:5, k)
  out_k <- JM_gcs_fold1(
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
  metrics_k <- out_k$metrics_gcs
  metrics_k$fold <- k
  metrics_jm_cv <- rbind(metrics_jm_cv, metrics_k)
  models_jm_cv[[k]] <- out_k$fit_jm_gcs
}
metrics_jm_cv



write.csv(metrics_jm_cv,"F:/文章_大论文/0521/处理后文件/结果/RSFLC交叉验证结果/metrics_jm_cv.csv")













t0_cv <- 5
metrics_jm_cv <- NULL
models_jm_cv  <- vector("list", 5)
for (k in 1:5) {
  train_k <- k
  test_k  <- if (k == 5L) 1L else k + 1L
  cat(
    "\n========== JM GCS | 训练集", train_k,
    "→ 预测集", test_k, "==========\n"
  )
  out_k <- JM_gcs_fold1(
    data = list(
      baselinedata  = Data_baseline_5fold[[train_k]],
      longitudedata = Data_longitude_5fold[[train_k]]
    ),
    t0 = t0_cv,
    predictiondata = list(
      baselinedata  = Data_baseline_5fold[[test_k]],
      longitudedata = Data_longitude_5fold[[test_k]]
    )
  )
  metrics_k <- out_k$metrics_gcs
  metrics_k$train_fold <- train_k
  metrics_k$test_fold  <- test_k
  metrics_jm_cv <- rbind(metrics_jm_cv, metrics_k)
  models_jm_cv[[k]] <- out_k$fit_jm_gcs
}
metrics_jm_cv

#########调试################
prep_jm_gcs_inputs <- function(baselinedata, longitudedata) {
  stopifnot(is.data.frame(baselinedata), is.data.frame(longitudedata))
  
  base_need <- c(
    "hadm_id", "age", "charlson_comorbidity_index", "apsiii", "sapsii", "oasis",
    "preiculos", "mechvent", "electivesurgery",
    "intime", "deathtime", "death_28d"
  )
  if (!all(base_need %in% names(baselinedata))) {
    stop(
      "baselinedata 缺少必要列: ",
      paste(setdiff(base_need, names(baselinedata)), collapse = ", ")
    )
  }
  if (!all(c("hadm_id", "times", "gcs") %in% names(longitudedata))) {
    stop("longitudedata 需含 hadm_id, times, gcs")
  }
  
  long_gcs <- longitudedata %>%
    dplyr::filter(times <= 10) %>%
    dplyr::transmute(
      hadm_id = as.integer(hadm_id),
      t       = as.integer(times),
      gcs     = as.numeric(gcs)
    ) %>%
    dplyr::group_by(hadm_id) %>%
    dplyr::filter(dplyr::n() >= 2) %>%
    dplyr::ungroup() %>%
    as.data.frame()
  
  if (nrow(long_gcs) == 0) {
    stop("prep_jm_gcs_inputs：筛选后无有效纵向 GCS 记录")
  }
  
  valid_patients <- unique(long_gcs$hadm_id)
  
  base_df <- baselinedata %>%
    dplyr::filter(hadm_id %in% valid_patients) %>%
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
    ) %>%
    dplyr::distinct(hadm_id, .keep_all = TRUE) %>%
    as.data.frame()
  
  intime_parsed <- as.POSIXct(base_df$intime, format = "%d/%m/%Y %H:%M:%S")
  deathtime_chr <- ifelse(
    base_df$deathtime == "" | is.na(base_df$deathtime),
    NA_character_,
    base_df$deathtime
  )
  deathtime_parsed <- as.POSIXct(deathtime_chr, format = "%d/%m/%Y %H:%M:%S")
  time_to_death <- as.numeric(difftime(deathtime_parsed, intime_parsed, units = "days"))
  time_to_death[is.na(time_to_death)] <- 28
  
  base_df$obs_time <- pmin(time_to_death, 28)
  base_df$event    <- as.integer(as.numeric(as.character(base_df$death_28d)) == 1)
  
  # ---- 剔除异常生存时间 ----
  bad_time_ids <- base_df$hadm_id[is.na(base_df$obs_time) | base_df$obs_time <= 0]
  if (length(bad_time_ids) > 0) {
    warning(
      "prep_jm_gcs_inputs：剔除 obs_time <= 0 的患者 n = ", length(bad_time_ids),
      " | 示例 hadm_id: ", paste(head(bad_time_ids, 5), collapse = ", ")
    )
    base_df  <- base_df[!base_df$hadm_id %in% bad_time_ids, , drop = FALSE]
    long_gcs <- long_gcs[!long_gcs$hadm_id %in% bad_time_ids, , drop = FALSE]
  }
  
  bad_event_ids <- base_df$hadm_id[is.na(base_df$event)]
  if (length(bad_event_ids) > 0) {
    warning("prep_jm_gcs_inputs：剔除 death_28d 缺失的患者 n = ", length(bad_event_ids))
    base_df  <- base_df[!base_df$hadm_id %in% bad_event_ids, , drop = FALSE]
    long_gcs <- long_gcs[!long_gcs$hadm_id %in% bad_event_ids, , drop = FALSE]
  }
  
  missing_base_ids <- setdiff(unique(long_gcs$hadm_id), base_df$hadm_id)
  if (length(missing_base_ids) > 0) {
    warning("prep_jm_gcs_inputs：纵向有但 baseline 无，已剔除 n = ", length(missing_base_ids))
    long_gcs <- long_gcs[!long_gcs$hadm_id %in% missing_base_ids, , drop = FALSE]
  }
  
  surv_df <- base_df %>%
    dplyr::select(
      hadm_id, obs_time, event,
      age, charlson_comorbidity_index, apsiii, sapsii, oasis, preiculos,
      mechvent, electivesurgery
    )
  
  jm_long <- long_gcs %>%
    dplyr::inner_join(surv_df, by = "hadm_id") %>%
    dplyr::arrange(hadm_id, t) %>%
    as.data.frame()
  
  if (!identical(sort(unique(long_gcs$hadm_id)), sort(unique(surv_df$hadm_id)))) {
    stop("prep_jm_gcs_inputs：long_gcs 与 surv_df 的 hadm_id 不一致")
  }
  if (sum(is.na(jm_long$gcs)) > 0) {
    stop("prep_jm_gcs_inputs：gcs 含 NA，n = ", sum(is.na(jm_long$gcs)))
  }
  if (!all(jm_long$obs_time > 0)) {
    stop("prep_jm_gcs_inputs：仍有 obs_time <= 0")
  }
  if (any(is.na(jm_long$event)) || !all(jm_long$event %in% c(0L, 1L))) {
    stop("prep_jm_gcs_inputs：event 非 0/1 或含 NA")
  }
  if (nrow(jm_long) == 0) {
    stop("prep_jm_gcs_inputs：合并后无患者")
  }
  
  jm_long
}

cal_3_JM1 <- function(model, t0, data) {
  stopifnot(inherits(model, "mjoint"))
  stopifnot(is.data.frame(data))
  if (!requireNamespace("pROC", quietly = TRUE)) stop("请先安装 pROC")
  if (!requireNamespace("survival", quietly = TRUE)) stop("请先安装 survival")
  
  need_cols <- c("hadm_id", "obs_time", "event", "age", "gcs", "t")
  if (!all(need_cols %in% names(data))) {
    stop("data 缺少必要列: ", paste(setdiff(need_cols, names(data)), collapse = ", "))
  }
  
  model_name <- attr(model, "model_name", exact = TRUE)
  if (is.null(model_name)) model_name <- "JM1"
  
  clip <- function(x, lim = 20) pmax(pmin(x, lim), -lim)
  
  surv_data <- data[!duplicated(data$hadm_id),
                    c("hadm_id", "obs_time", "event", "age"), drop = FALSE]
  surv_data$hadm_id <- as.integer(surv_data$hadm_id)
  
  cat("[cal_3_JM1] 模型:", model_name, "| t0 =", t0, "| n =", nrow(surv_data), "\n")
  
  beta  <- model$coefficients$beta
  gamma <- model$coefficients$gamma
  if (!"gamma_1" %in% names(gamma)) stop("未找到 gamma_1")
  gamma_alpha <- unname(gamma["gamma_1"])
  
  b0 <- unname(beta["(Intercept)_1"])
  b1 <- unname(beta["t_1"])
  
  cat("[cal_3_JM1] 正在计算 prob_dead...\n")
  
  re <- ranef(model)
  re_df <- data.frame(
    hadm_id  = as.integer(rownames(re)),
    re_int   = re[, "(Intercept)_1"],
    re_slope = re[, "t_1"],
    stringsAsFactors = FALSE
  )
  
  # 验证集：用 t <= t0 的 gcs 残差估计随机效应
  long_t0 <- data[data$t <= t0, c("hadm_id", "t", "gcs"), drop = FALSE]
  impute_re <- function(id) {
    obs <- long_t0[long_t0$hadm_id == id, , drop = FALSE]
    if (nrow(obs) == 0) return(c(re_int = 0, re_slope = 0))
    pop <- b0 + b1 * obs$t
    resid <- obs$gcs - pop
    re_int <- mean(resid)
    re_slope <- if (nrow(obs) >= 2L) {
      unname(coef(lm(gcs ~ t, data = obs))["t"]) - b1
    } else {
      0
    }
    c(re_int = re_int, re_slope = re_slope)
  }
  
  all_ids <- surv_data$hadm_id
  missing_ids <- setdiff(all_ids, re_df$hadm_id)
  if (length(missing_ids) > 0) {
    cat("[cal_3_JM1] 对", length(missing_ids), "例验证集患者用纵向数据估计随机效应\n")
    re_imputed <- t(vapply(missing_ids, impute_re, numeric(2)))
    re_imputed <- data.frame(
      hadm_id  = missing_ids,
      re_int   = re_imputed[, "re_int"],
      re_slope = re_imputed[, "re_slope"]
    )
    re_df <- rbind(re_df, re_imputed)
  }
  
  pred_df <- surv_data
  pred_df$t <- t0
  pred_df <- merge(pred_df, re_df, by = "hadm_id", sort = FALSE, all.x = TRUE)
  
  X_long <- model.matrix(~ t, data = pred_df)
  fixed_part  <- clip(as.numeric(X_long %*% beta))
  random_part <- clip(pred_df$re_int + pred_df$re_slope * t0)
  lp_t0 <- clip(fixed_part + random_part)
  
  S0_28 <- summary(survfit(Surv(obs_time, event) ~ 1, data = surv_data), times = 28)$surv
  if (length(S0_28) == 0 || is.na(S0_28)) {
    S0_28 <- tail(summary(survfit(Surv(obs_time, event) ~ 1, data = surv_data))$surv, 1)
  }
  
  prob_dead <- 1 - S0_28 ^ exp(clip(as.numeric(gamma_alpha) * lp_t0))
  prob_dead <- pmin(pmax(prob_dead, 0), 1)
  
  pred_df$prob_dead <- prob_dead
  pred_df$y_dead    <- as.integer(pred_df$event)
  pred_df$time28    <- pred_df$obs_time
  pred_df$status28  <- as.integer(pred_df$event)
  
  pred_df_eval <- pred_df[is.finite(pred_df$prob_dead), , drop = FALSE]
  if (nrow(pred_df_eval) == 0) stop("无有效 prob_dead")
  if (length(unique(pred_df_eval$y_dead)) < 2) stop("y_dead 仅单一类别")
  
  roc_obj <- pROC::roc(
    response  = pred_df_eval$y_dead,
    predictor = pred_df_eval$prob_dead,
    levels    = c(0, 1), direction = "<", quiet = TRUE
  )
  auc_val <- as.numeric(pROC::auc(roc_obj))
  
  cindex_obj <- survival::concordance(
    survival::Surv(pred_df_eval$time28, pred_df_eval$status28) ~ pred_df_eval$prob_dead,
    reverse = TRUE
  )
  
  bs_val <- mean((pred_df_eval$y_dead - pred_df_eval$prob_dead)^2)
  
  cat("[cal_3_JM1] AUC =", round(auc_val, 4),
      "| C-index =", round(cindex_obj$concordance, 4),
      "| BS =", round(bs_val, 4), "\n")
  
  data.frame(
    model   = model_name,
    auc     = round(auc_val, 4),
    cindex  = round(cindex_obj$concordance, 4),
    bs      = round(bs_val, 4),
    n       = nrow(pred_df_eval),
    n_total = nrow(pred_df),
    stringsAsFactors = FALSE
  )
}


JM_gcs_fold1 <- function(data, t0, predictiondata) {
  stopifnot(
    is.list(data),
    is.list(predictiondata),
    is.data.frame(data$baselinedata),
    is.data.frame(data$longitudedata),
    is.data.frame(predictiondata$baselinedata),
    is.data.frame(predictiondata$longitudedata)
  )
  if (!requireNamespace("joineRML", quietly = TRUE)) {
    stop("请先安装 joineRML")
  }
  if (!requireNamespace("survival", quietly = TRUE)) {
    stop("请先安装 survival")
  }
  
  # ---- 1) 训练集预处理 ----
  train_long <- prep_jm_gcs_inputs(
    baselinedata  = data$baselinedata,
    longitudedata = data$longitudedata
  )
  train_data <- train_long[train_long$t <= train_long$obs_time, , drop = FALSE]
  
  cat(
    "[JM_gcs_fold1] 训练 n =", length(unique(train_data$hadm_id)),
    "| 纵向行数 =", nrow(train_data), "\n"
  )
  
  # ---- 2) 拟合简化 mjoint ----
  # 纵向固定效应：gcs ~ t
  # 随机效应：~ t | hadm_id
  # 生存：仅 age
  cat("[JM_gcs_fold1] 开始 mjoint 建模...\n")
  fit_jm_gcs <- joineRML::mjoint(
    formLongFixed = list(
      "gcs" = gcs ~ t
    ),
    formLongRandom = list(
      "gcs" = ~ t | hadm_id
    ),
    formSurv = Surv(obs_time, event) ~ age,
    data    = train_data,
    timeVar = "t"
  )
  attr(fit_jm_gcs, "model_name") <- "JM_gcs_fold1"
  cat("[JM_gcs_fold1] 建模完成\n")
  
  # ---- 3) 验证集预处理 ----
  pred_long <- prep_jm_gcs_inputs(
    baselinedata  = predictiondata$baselinedata,
    longitudedata = predictiondata$longitudedata
  )
  pred_data <- pred_long[pred_long$t <= pred_long$obs_time, , drop = FALSE]
  
  cat(
    "[JM_gcs_fold1] 验证 n =", length(unique(pred_data$hadm_id)),
    "| 纵向行数 =", nrow(pred_data), "\n"
  )
  
  # ---- 4) 计算 AUC / C-index / Brier ----
  metrics_gcs <- cal_3_JM1(
    model = fit_jm_gcs,
    t0    = t0,
    data  = pred_data
  )
  
  list(
    metrics_gcs = metrics_gcs,
    fit_jm_gcs  = fit_jm_gcs
  )
}


















set.seed(1234)
train_baseline  <- dplyr::bind_rows(Data_baseline_5fold[2:5])
train_longitude <- dplyr::bind_rows(Data_longitude_5fold[2:5])
ids_train <- sample(unique(train_baseline$hadm_id), 500)
system.time({
  JM_gcs_fold1(
    data = list(
      baselinedata  = train_baseline  |>
        dplyr::filter(hadm_id %in% ids_train),
      longitudedata = train_longitude |>
        dplyr::filter(hadm_id %in% ids_train)
    ),
    t0 = 5,
    predictiondata = list(
      baselinedata  = Data_baseline_5fold[[1]],
      longitudedata = Data_longitude_5fold[[1]]
      # 验证用整折 fold 1 即可；评估很快，不必再 subsample
    )
  )
})



 


##################### fold 1 试跑：prep + mjoint #####################
# ---- 1) 取第 1 折数据 ----
baseline_fold1  <- Data_baseline_5fold[[1]]
longitude_fold1 <- Data_longitude_5fold[[1]]
cat("fold1 baseline 行数:", nrow(baseline_fold1),
    "| longitude 行数:", nrow(longitude_fold1), "\n")
# ---- 2) 预处理 → mjoint 长表 ----
jm_long <- prep_jm_gcs_inputs(
  baselinedata   = baseline_fold1,
  longitudedata  = longitude_fold1
)
cat(
  "prep 完成 | 患者数:", length(unique(jm_long$hadm_id)),
  "| 纵向行数:", nrow(jm_long),
  "| 事件数:", sum(!duplicated(jm_long$hadm_id) & jm_long$event == 1),
  "\n"
)
# mjoint 要求：纵向观测时间不晚于生存时间（与参考 cal_3 一致）
jm_data <- jm_long[jm_long$t <= jm_long$obs_time, , drop = FALSE]
cat(
  "landmark 过滤后 | 患者数:", length(unique(jm_data$hadm_id)),
  "| 纵向行数:", nrow(jm_data), "\n"
)
# ---- 3) 拟合 joint model ----
cat("[mjoint] 开始拟合（样本量大时可能较慢）...\n")
fit_jm_gcs <- mjoint(
  formLongFixed = list(
    "gcs" = gcs ~ t + age + charlson_comorbidity_index + apsiii + sapsii +
      oasis + preiculos + mechvent + electivesurgery
  ),
  formLongRandom = list(
    "gcs" = ~ t | hadm_id
  ),
  formSurv = Surv(obs_time, event) ~ age + charlson_comorbidity_index + apsiii +
    sapsii + oasis + preiculos + mechvent + electivesurgery,
  data    = jm_data,
  timeVar = "t"
)
cat("[mjoint] 拟合完成\n")
# ---- 4) 查看结果 ----
summary(fit_jm_gcs)
# 系数（纵向 + 生存 + 关联参数 alpha）
fit_jm_gcs$coefficients
# 随机效应（EB）
head(ranef(fit_jm_gcs))








metrics_jm <- cal_3_JM(
  model = fit_jm_gcs,
  t0    = 5,
  data  = jm_data
)
metrics_jm



