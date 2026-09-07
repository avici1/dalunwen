library(dplyr)
library(tidyr)
library(readxl)
library(purrr)
library(DynForest)
library(missForest)
library(pROC)
library(survival)

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












prep_sofa_inputs <- function(baseline_data, longitude_data) {
  timeData <- longitude_data %>%
    dplyr::filter(times <= 10) %>%
    dplyr::transmute(
      hadm_id      = as.integer(hadm_id),
      time         = as.integer(times),
      sofa_24hours = as.numeric(sofa_24hours)
    ) %>%
    dplyr::group_by(hadm_id) %>%
    dplyr::filter(dplyr::n() >= 2) %>%
    dplyr::ungroup() %>%
    as.data.frame()
  valid_patients <- unique(timeData$hadm_id)
  fixedData <- baseline_data %>%
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
      electivesurgery = factor(electivesurgery, levels = c(0, 1), labels = c("no", "yes"))
    ) %>%
    dplyr::distinct(hadm_id, .keep_all = TRUE) %>%
    as.data.frame()
  Y <- list(
    type = "factor",
    Y = baseline_data %>%
      dplyr::filter(hadm_id %in% valid_patients) %>%
      dplyr::transmute(
        hadm_id = as.integer(hadm_id),
        event = factor(death_28d, levels = c(0, 1), labels = c("alive", "dead"))
      ) %>%
      dplyr::distinct(hadm_id, .keep_all = TRUE) %>%
      as.data.frame()
  )
  stopifnot(
    identical(sort(unique(timeData$hadm_id)), sort(fixedData$hadm_id)),
    identical(sort(fixedData$hadm_id), sort(Y$Y$hadm_id)),
    sum(is.na(timeData$sofa_24hours)) == 0
  )
  list(timeData = timeData, fixedData = fixedData, Y = Y)
}


cal_3_test <- function(model, t0, timeData, fixedData, baseline_data) {
  stopifnot(inherits(model, "dynforest"))
  stopifnot(is.data.frame(timeData), is.data.frame(fixedData), is.data.frame(baseline_data))
  if (model$type != "factor") {
    stop("cal_3_test 仅支持 dynforest 分类结局 (type = 'factor')")
  }
  if (!requireNamespace("pROC", quietly = TRUE)) {
    stop("请先安装 pROC: install.packages('pROC')")
  }
  if (!requireNamespace("survival", quietly = TRUE)) {
    stop("请先安装 survival")
  }
  
  idVar <- "hadm_id"
  timeVar <- model$timeVar
  model_name <- attr(model, "model_name", exact = TRUE)
  if (is.null(model_name)) model_name <- "dynforest"
  
  dead_label <- "dead"
  if (!dead_label %in% model$levels) {
    dead_label <- model$levels[length(model$levels)]
  }
  
  eval_ids <- unique(as.integer(fixedData[[idVar]]))
  y_df <- baseline_data %>%
    dplyr::filter(hadm_id %in% eval_ids) %>%
    dplyr::transmute(
      hadm_id = as.integer(hadm_id),
      y_dead  = as.integer(death_28d == 1)
    ) %>%
    dplyr::distinct(hadm_id, .keep_all = TRUE) %>%
    as.data.frame()
  
  if (!all(c(idVar, "intime", "deathtime", "death_28d") %in% names(baseline_data))) {
    stop("baseline_data 需含 hadm_id, intime, deathtime, death_28d")
  }
  
  cat(
    "[cal_3_test] 模型:", model_name,
    "| t0 =", t0,
    "| n =", length(eval_ids), "\n"
  )
  
  cat("[cal_3_test] 正在 predict（样本量大时耗时较长）...\n")
  pred_dyn <- predict(
    object    = model,
    timeData  = timeData,
    fixedData = fixedData,
    idVar     = idVar,
    timeVar   = timeVar,
    t0        = t0
  )
  
  pred_hadm_id <- as.integer(names(pred_dyn$pred_indiv))
  pred_class   <- unname(pred_dyn$pred_indiv)
  prob_dead    <- ifelse(
    pred_class == dead_label,
    unname(pred_dyn$pred_indiv_proba),
    1 - unname(pred_dyn$pred_indiv_proba)
  )
  
  pred_df <- data.frame(
    hadm_id   = pred_hadm_id,
    prob_dead = prob_dead,
    stringsAsFactors = FALSE
  )
  pred_df <- merge(pred_df, y_df, by = "hadm_id", sort = FALSE)
  cat("[cal_3_test] 完成预测与 pred_df 整理 | nrow =", nrow(pred_df), "\n")
  
  # 剔除非有限 prob_dead（NA / Inf / -Inf），与调试代码一致
  n_na   <- sum(is.na(pred_df$prob_dead))
  n_inf  <- sum(pred_df$prob_dead == Inf,  na.rm = TRUE)
  n_ninf <- sum(pred_df$prob_dead == -Inf, na.rm = TRUE)
  pred_df_eval <- pred_df[is.finite(pred_df$prob_dead), , drop = FALSE]
  
  cat(
    "[cal_3_test] prob_dead NA 数:", n_na,
    "| Inf 数:", n_inf,
    "| -Inf 数:", n_ninf,
    "| 用于指标计算样本数:", nrow(pred_df_eval),
    "（剔除非有限值后）\n"
  )
  
  if (nrow(pred_df_eval) == 0) {
    stop("cal_3_test：剔除 NA/Inf 后无有效 prob_dead，无法计算指标")
  }
  if (length(unique(pred_df_eval$y_dead)) < 2) {
    stop("cal_3_test：有效样本中 y_dead 仅单一类别，无法计算 AUC")
  }
  
  roc_obj <- pROC::roc(
    response  = pred_df_eval$y_dead,
    predictor = pred_df_eval$prob_dead,
    levels    = c(0, 1),
    direction = "<",
    quiet     = TRUE
  )
  auc_val <- as.numeric(pROC::auc(roc_obj))
  cat("[cal_3_test] 完成计算 AUC =", round(auc_val, 4), "\n")
  
  eval_cindex <- pred_df
  base_sub <- baseline_data[, c(idVar, "intime", "deathtime", "death_28d")]
  base_sub <- base_sub[!duplicated(base_sub[[idVar]]), , drop = FALSE]
  eval_cindex <- merge(eval_cindex, base_sub, by = idVar, all.x = TRUE, sort = FALSE)
  
  intime_parsed <- as.POSIXct(eval_cindex$intime, format = "%d/%m/%Y %H:%M:%S")
  deathtime_chr <- ifelse(
    eval_cindex$deathtime == "" | is.na(eval_cindex$deathtime),
    NA_character_,
    eval_cindex$deathtime
  )
  deathtime_parsed <- as.POSIXct(deathtime_chr, format = "%d/%m/%Y %H:%M:%S")
  time_to_death <- as.numeric(difftime(deathtime_parsed, intime_parsed, units = "days"))
  time_to_death[is.na(time_to_death)] <- 28
  eval_cindex$time28   <- pmin(time_to_death, 28)
  eval_cindex$status28 <- as.integer(eval_cindex$death_28d == 1)
  
  eval_cindex_ci <- eval_cindex[is.finite(eval_cindex$prob_dead), , drop = FALSE]
  cindex_obj <- survival::concordance(
    survival::Surv(eval_cindex_ci$time28, eval_cindex_ci$status28) ~ eval_cindex_ci$prob_dead,
    reverse = TRUE
  )
  cindex_val <- cindex_obj$concordance
  cat("[cal_3_test] 完成计算 C-index =", round(cindex_val, 4), "\n")
  
  bs_val <- mean((pred_df_eval$y_dead - pred_df_eval$prob_dead)^2)
  cat("[cal_3_test] 完成计算 Brier Score =", round(bs_val, 4), "\n")
  
  out_df <- data.frame(
    model   = model_name,
    auc     = round(auc_val, 4),
    cindex  = round(cindex_val, 4),
    bs      = round(bs_val, 4),
    n       = nrow(pred_df_eval),
    n_total = nrow(pred_df),
    stringsAsFactors = FALSE
  )
  cat("[cal_3_test] 全部指标计算完成\n")
  out_df
}


RSFLC_sofa_fold <- function(baseline_data,
                            longitude_data,
                            t0,
                            prediction_baseline,
                            prediction_longitude) {
  stopifnot(
    is.data.frame(baseline_data),
    is.data.frame(longitude_data),
    is.data.frame(prediction_baseline),
    is.data.frame(prediction_longitude)
  )
  train_inputs <- prep_sofa_inputs(baseline_data, longitude_data)
  pred_inputs  <- prep_sofa_inputs(prediction_baseline, prediction_longitude)
  timeVar_sofa <- "time"
  timeVarModel_sofa <- list(
    sofa_24hours = list(fixed = sofa_24hours ~ 1, random = ~ time)
  )
  res_dyn_sofa <- DynForest::dynforest(
    timeData     = train_inputs$timeData,
    fixedData    = train_inputs$fixedData,
    timeVar      = timeVar_sofa,
    idVar        = "hadm_id",
    timeVarModel = timeVarModel_sofa,
    Y            = train_inputs$Y,
    mtry         = 3,
    nodesize     = 5,
    ncores       = 1,
    ntree        = 50,
    seed         = 1234
  )
  attr(res_dyn_sofa, "model_name") <- "RSFLC_sofa_fold"
  metrics_sofa <- cal_3_test(
    model         = res_dyn_sofa,
    t0            = t0,
    timeData      = pred_inputs$timeData,
    fixedData     = pred_inputs$fixedData,
    baseline_data = prediction_baseline
  )
  list(
    metrics_sofa = metrics_sofa,
    res_dyn_sofa = res_dyn_sofa
  )
}
prep_gcs_inputs <- function(baseline_data, longitude_data) {
  timeData <- longitude_data %>%
    dplyr::filter(times <= 10) %>%
    dplyr::transmute(
      hadm_id = as.integer(hadm_id),
      time    = as.integer(times),
      gcs     = as.numeric(gcs)
    ) %>%
    dplyr::group_by(hadm_id) %>%
    dplyr::filter(dplyr::n() >= 2) %>%
    dplyr::ungroup() %>%
    as.data.frame()
  
  valid_patients <- unique(timeData$hadm_id)
  
  fixedData <- baseline_data %>%
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
      electivesurgery = factor(electivesurgery, levels = c(0, 1), labels = c("no", "yes"))
    ) %>%
    dplyr::distinct(hadm_id, .keep_all = TRUE) %>%
    as.data.frame()
  
  Y <- list(
    type = "factor",
    Y = baseline_data %>%
      dplyr::filter(hadm_id %in% valid_patients) %>%
      dplyr::transmute(
        hadm_id = as.integer(hadm_id),
        event = factor(death_28d, levels = c(0, 1), labels = c("alive", "dead"))
      ) %>%
      dplyr::distinct(hadm_id, .keep_all = TRUE) %>%
      as.data.frame()
  )
  
  stopifnot(
    identical(sort(unique(timeData$hadm_id)), sort(fixedData$hadm_id)),
    identical(sort(fixedData$hadm_id), sort(Y$Y$hadm_id)),
    sum(is.na(timeData$gcs)) == 0
  )
  
  list(timeData = timeData, fixedData = fixedData, Y = Y)
}

RSFLC_gcs_fold <- function(baseline_data,
                           longitude_data,
                           t0,
                           prediction_baseline,
                           prediction_longitude) {
  stopifnot(
    is.data.frame(baseline_data),
    is.data.frame(longitude_data),
    is.data.frame(prediction_baseline),
    is.data.frame(prediction_longitude)
  )
  
  train_inputs <- prep_gcs_inputs(baseline_data, longitude_data)
  pred_inputs  <- prep_gcs_inputs(prediction_baseline, prediction_longitude)
  
  timeVar_gcs <- "time"
  timeVarModel_gcs <- list(
    gcs = list(fixed = gcs ~ 1, random = ~ time)
  )
  
  res_dyn_gcs <- DynForest::dynforest(
    timeData     = train_inputs$timeData,
    fixedData    = train_inputs$fixedData,
    timeVar      = timeVar_gcs,
    idVar        = "hadm_id",
    timeVarModel = timeVarModel_gcs,
    Y            = train_inputs$Y,
    mtry         = 3,
    nodesize     = 5,
    ncores       = 1,
    ntree        = 50,
    seed         = 1234
  )
  
  attr(res_dyn_gcs, "model_name") <- "RSFLC_gcs_fold"
  
  metrics_gcs <- cal_3_test(
    model         = res_dyn_gcs,
    t0            = t0,
    timeData      = pred_inputs$timeData,
    fixedData     = pred_inputs$fixedData,
    baseline_data = prediction_baseline
  )
  
  list(
    metrics_gcs = metrics_gcs,
    res_dyn_gcs = res_dyn_gcs
  )
}



#####################5折实现##############

t0_cv <- 5
metrics_gcs_cv <- NULL
models_gcs_cv <- vector("list", 5)

for (k in 1:5) {
  cat("\n========== 5折 CV GCS | fold", k, "==========\n")
  train_idx <- setdiff(1:5, k)

  train_baseline  <- dplyr::bind_rows(Data_baseline_5fold[train_idx])
  train_longitude <- dplyr::bind_rows(Data_longitude_5fold[train_idx])
  val_baseline    <- Data_baseline_5fold[[k]]
  val_longitude   <- Data_longitude_5fold[[k]]

  out_k <- RSFLC_gcs_fold(
    baseline_data        = train_baseline,
    longitude_data       = train_longitude,
    t0                   = t0_cv,
    prediction_baseline  = val_baseline,
    prediction_longitude = val_longitude
  )

  metrics_k <- out_k$metrics_gcs
  metrics_k$fold <- k
  metrics_gcs_cv <- rbind(metrics_gcs_cv, metrics_k)
  models_gcs_cv[[k]] <- out_k$res_dyn_gcs
}

metrics_gcs_cv

write.csv(metrics_gcs_cv,"F:/文章_大论文/0521/处理后文件/结果/RSFLC交叉验证结果/metrics_gcs_cv.csv")








######代码调试###########################
# RSFLC_sofa_fold 内部调用 cal_3_test（验证集评估用，定义在同目录 cal_3_test.R）

# ---------- 单折试跑：fold 1 作验证集，fold 2-5 作训练集 ----------
t0_try    <- 5
val_k     <- 1
train_idx <- 2:5

train_baseline  <- dplyr::bind_rows(Data_baseline_5fold[train_idx])
train_longitude <- dplyr::bind_rows(Data_longitude_5fold[train_idx])
val_baseline    <- Data_baseline_5fold[[val_k]]
val_longitude   <- Data_longitude_5fold[[val_k]]

cat("\n========== 单折试跑 | 验证 fold", val_k,
    "| 训练 fold", paste(train_idx, collapse = ","), "==========\n")

out_fold1 <- RSFLC_sofa_fold(
  baseline_data        = train_baseline,
  longitude_data       = train_longitude,
  t0                   = t0_try,
  prediction_baseline  = val_baseline,
  prediction_longitude = val_longitude
)
metrics_fold1 <- out_fold1$metrics_sofa
metrics_fold1$fold <- val_k
metrics_fold1







# ---- 第 0 步：准备训练/验证原始数据 ----
t0_try    <- 5
val_k     <- 1
train_idx <- 2:5

train_baseline  <- dplyr::bind_rows(Data_baseline_5fold[train_idx])
train_longitude <- dplyr::bind_rows(Data_longitude_5fold[train_idx])
val_baseline    <- Data_baseline_5fold[[val_k]]
val_longitude   <- Data_longitude_5fold[[val_k]]

cat("\n[步骤0] 训练集 baseline 行数:", nrow(train_baseline),
    "| 训练集 longitude 行数:", nrow(train_longitude), "\n")
cat("[步骤0] 验证集 baseline 行数:", nrow(val_baseline),
    "| 验证集 longitude 行数:", nrow(val_longitude), "\n")


# ---- 第 1 步：训练集数据预处理（prep_sofa_inputs）----
train_inputs <- prep_sofa_inputs(train_baseline, train_longitude)

cat("\n[步骤1] 训练集预处理完成\n")
cat("  timeData  行数:", nrow(train_inputs$timeData),
    "| 患者数:", length(unique(train_inputs$timeData$hadm_id)), "\n")
cat("  fixedData 行数:", nrow(train_inputs$fixedData), "\n")
cat("  Y         行数:", nrow(train_inputs$Y$Y), "\n")
cat("  死亡人数:", sum(train_inputs$Y$Y$event == "dead"), "\n")


# ---- 第 2 步：DynForest 建模（训练集）----
timeVar_sofa <- "time"
timeVarModel_sofa <- list(
  sofa_24hours = list(fixed = sofa_24hours ~ 1, random = ~ time)
)

res_dyn_sofa <- DynForest::dynforest(
  timeData     = train_inputs$timeData,
  fixedData    = train_inputs$fixedData,
  timeVar      = timeVar_sofa,
  idVar        = "hadm_id",
  timeVarModel = timeVarModel_sofa,
  Y            = train_inputs$Y,
  mtry         = 3,
  nodesize     = 5,
  ncores       = 1,
  ntree        = 50,
  seed         = 1234
)
attr(res_dyn_sofa, "model_name") <- "RSFLC_sofa_fold"

cat("\n[步骤2] DynForest 建模完成 | 类型:", res_dyn_sofa$type,
    "| 训练样本数:", length(res_dyn_sofa$data$Y$id), "\n")


# ---- 第 3 步：验证集数据预处理（prep_sofa_inputs）----
pred_inputs <- prep_sofa_inputs(val_baseline, val_longitude)

cat("\n[步骤3] 验证集预处理完成\n")
cat("  timeData  行数:", nrow(pred_inputs$timeData),
    "| 患者数:", length(unique(pred_inputs$timeData$hadm_id)), "\n")
cat("  fixedData 行数:", nrow(pred_inputs$fixedData), "\n")


# ---- 第 4 步：验证集预测 + 计算 AUC（到此步为止）----
idVar   <- "hadm_id"
timeVar <- res_dyn_sofa$timeVar
dead_label <- "dead"
if (!dead_label %in% res_dyn_sofa$levels) {
  dead_label <- res_dyn_sofa$levels[length(res_dyn_sofa$levels)]
}

# 4a) 整理验证集真实结局
eval_ids <- unique(as.integer(pred_inputs$fixedData[[idVar]]))
y_df <- val_baseline %>%
  dplyr::filter(hadm_id %in% eval_ids) %>%
  dplyr::transmute(
    hadm_id = as.integer(hadm_id),
    y_dead  = as.integer(death_28d == 1)
  ) %>%
  dplyr::distinct(hadm_id, .keep_all = TRUE) %>%
  as.data.frame()

cat("\n[步骤4] 验证集患者数:", length(eval_ids),
    "| 死亡人数:", sum(y_df$y_dead), "\n")
cat("[步骤4] 正在 predict（耗时较长）...\n")

# 4b) 在验证集上预测
pred_dyn <- predict(
  object    = res_dyn_sofa,
  timeData  = pred_inputs$timeData,
  fixedData = pred_inputs$fixedData,
  idVar     = idVar,
  timeVar   = timeVar,
  t0        = t0_try
)

pred_hadm_id <- as.integer(names(pred_dyn$pred_indiv))
pred_class   <- unname(pred_dyn$pred_indiv)
prob_dead    <- ifelse(
  pred_class == dead_label,
  unname(pred_dyn$pred_indiv_proba),
  1 - unname(pred_dyn$pred_indiv_proba)
)

pred_df <- data.frame(
  hadm_id   = pred_hadm_id,
  prob_dead = prob_dead,
  stringsAsFactors = FALSE
)
pred_df <- merge(pred_df, y_df, by = "hadm_id", sort = FALSE)

cat("[步骤4] pred_df 整理完成 | nrow =", nrow(pred_df), "\n")
cat("[步骤4] prob_dead NA 数:", sum(is.na(pred_df$prob_dead)),
    "| y_dead 分布:\n")
print(table(pred_df$y_dead, useNA = "ifany"))

# 4c) 计算 AUC（pROC 不接受 Inf，需先剔除非有限值）
pred_df_auc <- pred_df[is.finite(pred_df$prob_dead), , drop = FALSE]
cat("[步骤4] prob_dead Inf 数:", sum(is.infinite(pred_df$prob_dead)),
    "| 用于 AUC 样本数:", nrow(pred_df_auc), "（剔除 Inf/NA 后）\n")

roc_obj <- pROC::roc(
  response  = pred_df_auc$y_dead,
  predictor = pred_df_auc$prob_dead,
  levels    = c(0, 1),
  direction = "<",
  quiet     = TRUE
)
auc_val <- as.numeric(pROC::auc(roc_obj))

cat("\n[步骤4] AUC =", round(auc_val, 4), "\n")


# ---- 第 5 步：计算 C-index（与 cal_3_test 一致）----
eval_cindex <- pred_df
base_sub <- val_baseline[, c(idVar, "intime", "deathtime", "death_28d")]
base_sub <- base_sub[!duplicated(base_sub[[idVar]]), , drop = FALSE]
eval_cindex <- merge(eval_cindex, base_sub, by = idVar, all.x = TRUE, sort = FALSE)

intime_parsed <- as.POSIXct(eval_cindex$intime, format = "%d/%m/%Y %H:%M:%S")
deathtime_chr <- ifelse(
  eval_cindex$deathtime == "" | is.na(eval_cindex$deathtime),
  NA_character_,
  eval_cindex$deathtime
)
deathtime_parsed <- as.POSIXct(deathtime_chr, format = "%d/%m/%Y %H:%M:%S")
time_to_death <- as.numeric(difftime(deathtime_parsed, intime_parsed, units = "days"))
time_to_death[is.na(time_to_death)] <- 28
eval_cindex$time28   <- pmin(time_to_death, 28)
eval_cindex$status28 <- as.integer(eval_cindex$death_28d == 1)

# C-index 剔除非有限 prob_dead
eval_cindex_ci <- eval_cindex[is.finite(eval_cindex$prob_dead), , drop = FALSE]
cindex_obj <- survival::concordance(
  survival::Surv(eval_cindex_ci$time28, eval_cindex_ci$status28) ~ eval_cindex_ci$prob_dead,
  reverse = TRUE
)
cindex_val <- cindex_obj$concordance
cat("\n[步骤5] C-index =", round(cindex_val, 4),
    "| n =", nrow(eval_cindex_ci), "\n")


# ---- 第 6 步：计算 Brier Score（与 cal_3_test 一致）----
pred_df_bs <- pred_df[is.finite(pred_df$prob_dead), , drop = FALSE]
bs_val <- mean((pred_df_bs$y_dead - pred_df_bs$prob_dead)^2)
cat("[步骤6] Brier Score =", round(bs_val, 4),
    "| n =", nrow(pred_df_bs), "\n")

metrics_fold1 <- data.frame(
  model  = "RSFLC_sofa_fold",
  auc    = round(auc_val, 4),
  cindex = round(cindex_val, 4),
  bs     = round(bs_val, 4),
  n      = nrow(pred_df_bs),
  fold   = val_k,
  stringsAsFactors = FALSE
)
cat("\n[调试] 全部指标计算完成\n")
metrics_fold1





#############调试gcs模型###############
prep_gcs_inputs <- function(baseline_data, longitude_data) {
  timeData <- longitude_data %>%
    dplyr::filter(times <= 10) %>%
    dplyr::transmute(
      hadm_id = as.integer(hadm_id),
      time    = as.integer(times),
      gcs     = as.numeric(gcs)
    ) %>%
    dplyr::group_by(hadm_id) %>%
    dplyr::filter(dplyr::n() >= 2) %>%
    dplyr::ungroup() %>%
    as.data.frame()
  
  valid_patients <- unique(timeData$hadm_id)
  
  fixedData <- baseline_data %>%
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
      electivesurgery = factor(electivesurgery, levels = c(0, 1), labels = c("no", "yes"))
    ) %>%
    dplyr::distinct(hadm_id, .keep_all = TRUE) %>%
    as.data.frame()
  
  Y <- list(
    type = "factor",
    Y = baseline_data %>%
      dplyr::filter(hadm_id %in% valid_patients) %>%
      dplyr::transmute(
        hadm_id = as.integer(hadm_id),
        event = factor(death_28d, levels = c(0, 1), labels = c("alive", "dead"))
      ) %>%
      dplyr::distinct(hadm_id, .keep_all = TRUE) %>%
      as.data.frame()
  )
  
  stopifnot(
    identical(sort(unique(timeData$hadm_id)), sort(fixedData$hadm_id)),
    identical(sort(fixedData$hadm_id), sort(Y$Y$hadm_id)),
    sum(is.na(timeData$gcs)) == 0
  )
  
  list(timeData = timeData, fixedData = fixedData, Y = Y)
}

RSFLC_gcs_fold <- function(baseline_data,
                           longitude_data,
                           t0,
                           prediction_baseline,
                           prediction_longitude) {
  stopifnot(
    is.data.frame(baseline_data),
    is.data.frame(longitude_data),
    is.data.frame(prediction_baseline),
    is.data.frame(prediction_longitude)
  )
  
  train_inputs <- prep_gcs_inputs(baseline_data, longitude_data)
  pred_inputs  <- prep_gcs_inputs(prediction_baseline, prediction_longitude)
  
  timeVar_gcs <- "time"
  timeVarModel_gcs <- list(
    gcs = list(fixed = gcs ~ 1, random = ~ time)
  )
  
  res_dyn_gcs <- DynForest::dynforest(
    timeData     = train_inputs$timeData,
    fixedData    = train_inputs$fixedData,
    timeVar      = timeVar_gcs,
    idVar        = "hadm_id",
    timeVarModel = timeVarModel_gcs,
    Y            = train_inputs$Y,
    mtry         = 3,
    nodesize     = 5,
    ncores       = 1,
    ntree        = 50,
    seed         = 1234
  )
  
  attr(res_dyn_gcs, "model_name") <- "RSFLC_gcs_fold"
  
  metrics_gcs <- cal_3_test(
    model         = res_dyn_gcs,
    t0            = t0,
    timeData      = pred_inputs$timeData,
    fixedData     = pred_inputs$fixedData,
    baseline_data = prediction_baseline
  )
  
  list(
    metrics_gcs = metrics_gcs,
    res_dyn_gcs = res_dyn_gcs
  )
}




######代码调试 GCS###########################
# RSFLC_gcs_fold 内部调用 cal_3_test（验证集评估用）
# 前提：环境中已有 prep_gcs_inputs、RSFLC_gcs_fold、cal_3_test
# ---------- 单折试跑：fold 1 作验证集，fold 2-5 作训练集 ----------
t0_try    <- 5
val_k     <- 1
train_idx <- 2:5
train_baseline  <- dplyr::bind_rows(Data_baseline_5fold[train_idx])
train_longitude <- dplyr::bind_rows(Data_longitude_5fold[train_idx])
val_baseline    <- Data_baseline_5fold[[val_k]]
val_longitude   <- Data_longitude_5fold[[val_k]]
cat("\n========== GCS 单折试跑 | 验证 fold", val_k,
    "| 训练 fold", paste(train_idx, collapse = ","), "==========\n")
out_fold1_gcs <- RSFLC_gcs_fold(
  baseline_data        = train_baseline,
  longitude_data        = train_longitude,
  t0                   = t0_try,
  prediction_baseline  = val_baseline,
  prediction_longitude = val_longitude
)
metrics_fold1_gcs <- out_fold1_gcs$metrics_gcs
metrics_fold1_gcs$fold <- val_k
metrics_fold1_gcs
# ---- 第 0 步：准备训练/验证原始数据 ----
t0_try    <- 5
val_k     <- 1
train_idx <- 2:5
train_baseline  <- dplyr::bind_rows(Data_baseline_5fold[train_idx])
train_longitude <- dplyr::bind_rows(Data_longitude_5fold[train_idx])
val_baseline    <- Data_baseline_5fold[[val_k]]
val_longitude   <- Data_longitude_5fold[[val_k]]
cat("\n[步骤0] 训练集 baseline 行数:", nrow(train_baseline),
    "| 训练集 longitude 行数:", nrow(train_longitude), "\n")
cat("[步骤0] 验证集 baseline 行数:", nrow(val_baseline),
    "| 验证集 longitude 行数:", nrow(val_longitude), "\n")
# ---- 第 1 步：训练集数据预处理（prep_gcs_inputs）----
train_inputs_gcs <- prep_gcs_inputs(train_baseline, train_longitude)
cat("\n[步骤1] 训练集预处理完成\n")
cat("  timeData  行数:", nrow(train_inputs_gcs$timeData),
    "| 患者数:", length(unique(train_inputs_gcs$timeData$hadm_id)), "\n")
cat("  fixedData 行数:", nrow(train_inputs_gcs$fixedData), "\n")
cat("  Y         行数:", nrow(train_inputs_gcs$Y$Y), "\n")
cat("  死亡人数:", sum(train_inputs_gcs$Y$Y$event == "dead"), "\n")
# ---- 第 2 步：DynForest 建模（训练集）----
timeVar_gcs <- "time"
timeVarModel_gcs <- list(
  gcs = list(fixed = gcs ~ 1, random = ~ time)
)
res_dyn_gcs <- DynForest::dynforest(
  timeData     = train_inputs_gcs$timeData,
  fixedData    = train_inputs_gcs$fixedData,
  timeVar      = timeVar_gcs,
  idVar        = "hadm_id",
  timeVarModel = timeVarModel_gcs,
  Y            = train_inputs_gcs$Y,
  mtry         = 3,
  nodesize     = 5,
  ncores       = 1,
  ntree        = 50,
  seed         = 1234
)
attr(res_dyn_gcs, "model_name") <- "RSFLC_gcs_fold"
cat("\n[步骤2] DynForest 建模完成 | 类型:", res_dyn_gcs$type,
    "| 训练样本数:", length(res_dyn_gcs$data$Y$id), "\n")
# ---- 第 3 步：验证集数据预处理（prep_gcs_inputs）----
pred_inputs_gcs <- prep_gcs_inputs(val_baseline, val_longitude)
cat("\n[步骤3] 验证集预处理完成\n")
cat("  timeData  行数:", nrow(pred_inputs_gcs$timeData),
    "| 患者数:", length(unique(pred_inputs_gcs$timeData$hadm_id)), "\n")
cat("  fixedData 行数:", nrow(pred_inputs_gcs$fixedData), "\n")
# ---- 第 4 步：验证集预测 + 计算 AUC ----
idVar   <- "hadm_id"
timeVar <- res_dyn_gcs$timeVar
dead_label <- "dead"
if (!dead_label %in% res_dyn_gcs$levels) {
  dead_label <- res_dyn_gcs$levels[length(res_dyn_gcs$levels)]
}
# 4a) 整理验证集真实结局
eval_ids <- unique(as.integer(pred_inputs_gcs$fixedData[[idVar]]))
y_df_gcs <- val_baseline %>%
  dplyr::filter(hadm_id %in% eval_ids) %>%
  dplyr::transmute(
    hadm_id = as.integer(hadm_id),
    y_dead  = as.integer(death_28d == 1)
  ) %>%
  dplyr::distinct(hadm_id, .keep_all = TRUE) %>%
  as.data.frame()
cat("\n[步骤4] 验证集患者数:", length(eval_ids),
    "| 死亡人数:", sum(y_df_gcs$y_dead), "\n")
cat("[步骤4] 正在 predict（耗时较长）...\n")
# 4b) 在验证集上预测
pred_dyn_gcs <- predict(
  object    = res_dyn_gcs,
  timeData  = pred_inputs_gcs$timeData,
  fixedData = pred_inputs_gcs$fixedData,
  idVar     = idVar,
  timeVar   = timeVar,
  t0        = t0_try
)
pred_hadm_id <- as.integer(names(pred_dyn_gcs$pred_indiv))
pred_class   <- unname(pred_dyn_gcs$pred_indiv)
prob_dead    <- ifelse(
  pred_class == dead_label,
  unname(pred_dyn_gcs$pred_indiv_proba),
  1 - unname(pred_dyn_gcs$pred_indiv_proba)
)
pred_df_gcs <- data.frame(
  hadm_id   = pred_hadm_id,
  prob_dead = prob_dead,
  stringsAsFactors = FALSE
)
pred_df_gcs <- merge(pred_df_gcs, y_df_gcs, by = "hadm_id", sort = FALSE)
cat("[步骤4] pred_df 整理完成 | nrow =", nrow(pred_df_gcs), "\n")
cat("[步骤4] prob_dead NA 数:", sum(is.na(pred_df_gcs$prob_dead)),
    "| y_dead 分布:\n")
print(table(pred_df_gcs$y_dead, useNA = "ifany"))
# 4c) 计算 AUC（pROC 不接受 Inf，需先剔除非有限值）
pred_df_auc_gcs <- pred_df_gcs[is.finite(pred_df_gcs$prob_dead), , drop = FALSE]
cat("[步骤4] prob_dead Inf 数:", sum(is.infinite(pred_df_gcs$prob_dead)),
    "| 用于 AUC 样本数:", nrow(pred_df_auc_gcs), "（剔除 Inf/NA 后）\n")
roc_obj_gcs <- pROC::roc(
  response  = pred_df_auc_gcs$y_dead,
  predictor = pred_df_auc_gcs$prob_dead,
  levels    = c(0, 1),
  direction = "<",
  quiet     = TRUE
)
auc_val_gcs <- as.numeric(pROC::auc(roc_obj_gcs))
cat("\n[步骤4] AUC =", round(auc_val_gcs, 4), "\n")
# ---- 第 5 步：计算 C-index（与 cal_3_test 一致）----
eval_cindex_gcs <- pred_df_gcs
base_sub <- val_baseline[, c(idVar, "intime", "deathtime", "death_28d")]
base_sub <- base_sub[!duplicated(base_sub[[idVar]]), , drop = FALSE]
eval_cindex_gcs <- merge(eval_cindex_gcs, base_sub, by = idVar, all.x = TRUE, sort = FALSE)
intime_parsed <- as.POSIXct(eval_cindex_gcs$intime, format = "%d/%m/%Y %H:%M:%S")
deathtime_chr <- ifelse(
  eval_cindex_gcs$deathtime == "" | is.na(eval_cindex_gcs$deathtime),
  NA_character_,
  eval_cindex_gcs$deathtime
)
deathtime_parsed <- as.POSIXct(deathtime_chr, format = "%d/%m/%Y %H:%M:%S")
time_to_death <- as.numeric(difftime(deathtime_parsed, intime_parsed, units = "days"))
time_to_death[is.na(time_to_death)] <- 28
eval_cindex_gcs$time28   <- pmin(time_to_death, 28)
eval_cindex_gcs$status28 <- as.integer(eval_cindex_gcs$death_28d == 1)
# C-index 剔除非有限 prob_dead
eval_cindex_ci_gcs <- eval_cindex_gcs[is.finite(eval_cindex_gcs$prob_dead), , drop = FALSE]
cindex_obj_gcs <- survival::concordance(
  survival::Surv(eval_cindex_ci_gcs$time28, eval_cindex_ci_gcs$status28) ~ eval_cindex_ci_gcs$prob_dead,
  reverse = TRUE
)
cindex_val_gcs <- cindex_obj_gcs$concordance
cat("\n[步骤5] C-index =", round(cindex_val_gcs, 4),
    "| n =", nrow(eval_cindex_ci_gcs), "\n")
# ---- 第 6 步：计算 Brier Score（与 cal_3_test 一致）----
pred_df_bs_gcs <- pred_df_gcs[is.finite(pred_df_gcs$prob_dead), , drop = FALSE]
bs_val_gcs <- mean((pred_df_bs_gcs$y_dead - pred_df_bs_gcs$prob_dead)^2)
cat("[步骤6] Brier Score =", round(bs_val_gcs, 4),
    "| n =", nrow(pred_df_bs_gcs), "\n")
metrics_fold1_gcs <- data.frame(
  model  = "RSFLC_gcs_fold",
  auc    = round(auc_val_gcs, 4),
  cindex = round(cindex_val_gcs, 4),
  bs     = round(bs_val_gcs, 4),
  n      = nrow(pred_df_bs_gcs),
  fold   = val_k,
  stringsAsFactors = FALSE
)
cat("\n[调试] GCS 全部指标计算完成\n")
metrics_fold1_gcs
























