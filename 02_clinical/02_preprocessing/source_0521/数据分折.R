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









######################################################









RSFLC_sofa <- function(baseline_data, longitude_data, t0 = 5) {
  stopifnot(is.data.frame(baseline_data), is.data.frame(longitude_data))
  
  # 1) timeData：前 10 天 SOFA，每人 >= 2 条
  timeData_train_sofa <- longitude_data %>%
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
  
  valid_patients_sofa <- unique(timeData_train_sofa$hadm_id)
  
  # 2) fixedData：8 个静态基线变量
  fixedData_train_sofa <- baseline_data %>%
    dplyr::filter(hadm_id %in% valid_patients_sofa) %>%
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
  
  # 3) timeVarModel：SOFA 轨迹 fixed ~ 1, random ~ time
  timeVar_sofa <- "time"
  timeVarModel_sofa <- list(
    sofa_24hours = list(fixed = sofa_24hours ~ 1, random = ~ time)
  )
  
  # 4) Y：28 天死亡
  Y_sofa <- list(
    type = "factor",
    Y = baseline_data %>%
      dplyr::filter(hadm_id %in% valid_patients_sofa) %>%
      dplyr::transmute(
        hadm_id = as.integer(hadm_id),
        event = factor(death_28d, levels = c(0, 1), labels = c("alive", "dead"))
      ) %>%
      dplyr::distinct(hadm_id, .keep_all = TRUE) %>%
      as.data.frame()
  )
  
  stopifnot(
    identical(sort(unique(timeData_train_sofa$hadm_id)), sort(fixedData_train_sofa$hadm_id)),
    identical(sort(fixedData_train_sofa$hadm_id), sort(Y_sofa$Y$hadm_id)),
    sum(is.na(timeData_train_sofa$sofa_24hours)) == 0
  )
  
  res_dyn_sofa <- DynForest::dynforest(
    timeData     = timeData_train_sofa,
    fixedData    = fixedData_train_sofa,
    timeVar      = timeVar_sofa,
    idVar        = "hadm_id",
    timeVarModel = timeVarModel_sofa,
    Y            = Y_sofa,
    mtry         = 3,
    nodesize     = 5,
    ncores       = 1,
    ntree        = 50,
    seed         = 1234
  )
  
  attr(res_dyn_sofa, "baseline") <- baseline_data
  attr(res_dyn_sofa, "model_name") <- "RSFLC_sofa"
  
  metrics_sofa <- cal_3(res_dyn_sofa, t0 = t0)
  
  list(
    metrics_sofa = metrics_sofa,
    res_dyn_sofa = res_dyn_sofa
  )
}
cal_3 <- function(model, t0) {
  stopifnot(inherits(model, "dynforest"))
  if (model$type != "factor") {
    stop("cal_3 仅支持 dynforest 分类结局 (type = 'factor')")
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
  baseline <- attr(model, "baseline", exact = TRUE)
  
  dead_label <- "dead"
  if (!dead_label %in% model$levels) {
    dead_label <- model$levels[length(model$levels)]
  }
  
  cat("[cal_3] 模型:", model_name, "| t0 =", t0, "| n =", length(model$data$Y$id), "\n")
  
  rebuild_timeData <- function(model, idVar, timeVar) {
    L <- model$data$Longitudinal
    if (is.null(L)) return(NULL)
    out <- data.frame(L$id, L$time, L$X, check.names = FALSE)
    names(out)[1:2] <- c(idVar, timeVar)
    out
  }
  
  rebuild_fixedData <- function(model, idVar) {
    ids <- model$data$Y$id
    out <- data.frame(x = ids, stringsAsFactors = FALSE)
    names(out)[1] <- idVar
    if (!is.null(model$data$Numeric)) {
      nd <- model$data$Numeric
      num_df <- data.frame(nd$id, nd$X, check.names = FALSE)
      names(num_df)[1] <- idVar
      out <- merge(out, num_df, by = idVar, all.x = TRUE, sort = FALSE)
    }
    if (!is.null(model$data$Factor)) {
      fd <- model$data$Factor
      fac_df <- data.frame(fd$id, fd$X, check.names = FALSE)
      names(fac_df)[1] <- idVar
      out <- merge(out, fac_df, by = idVar, all.x = TRUE, sort = FALSE)
    }
    out[match(ids, out[[idVar]]), , drop = FALSE]
  }
  
  timeData  <- rebuild_timeData(model, idVar, timeVar)
  fixedData <- rebuild_fixedData(model, idVar)
  
  cat("[cal_3] 正在 predict...\n")
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
  
  y_df <- data.frame(
    hadm_id = model$data$Y$id,
    y_dead  = as.integer(model$data$Y$Y == dead_label),
    stringsAsFactors = FALSE
  )
  
  pred_df <- data.frame(
    hadm_id   = pred_hadm_id,
    prob_dead = prob_dead,
    stringsAsFactors = FALSE
  )
  pred_df <- merge(pred_df, y_df, by = "hadm_id", sort = FALSE)
  cat("[cal_3] 完成预测与 pred_df 整理\n")
  
  roc_obj <- pROC::roc(
    response  = pred_df$y_dead,
    predictor = pred_df$prob_dead,
    levels    = c(0, 1),
    direction = "<",
    quiet     = TRUE
  )
  auc_val <- as.numeric(pROC::auc(roc_obj))
  cat("[cal_3] 完成计算 AUC =", round(auc_val, 4), "\n")
  
  if (!is.null(baseline)) {
    if (!all(c(idVar, "intime", "deathtime", "death_28d") %in% names(baseline))) {
      stop("baseline 需含 hadm_id, intime, deathtime, death_28d")
    }
    eval_cindex <- pred_df
    base_sub <- baseline[, c(idVar, "intime", "deathtime", "death_28d")]
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
    
    cindex_obj <- survival::concordance(
      survival::Surv(eval_cindex$time28, eval_cindex$status28) ~ eval_cindex$prob_dead,
      reverse = TRUE
    )
  } else {
    eval_cindex <- pred_df
    eval_cindex$time28   <- 28
    eval_cindex$status28 <- eval_cindex$y_dead
    cindex_obj <- survival::concordance(
      survival::Surv(eval_cindex$time28, eval_cindex$status28) ~ eval_cindex$prob_dead,
      reverse = TRUE
    )
  }
  cindex_val <- cindex_obj$concordance
  cat("[cal_3] 完成计算 C-index =", round(cindex_val, 4), "\n")
  
  bs_val <- mean((pred_df$y_dead - pred_df$prob_dead)^2)
  cat("[cal_3] 完成计算 Brier Score =", round(bs_val, 4), "\n")
  
  out_df <- data.frame(
    model  = model_name,
    auc    = round(auc_val, 4),
    cindex = round(cindex_val, 4),
    bs     = round(bs_val, 4),
    stringsAsFactors = FALSE
  )
  cat("[cal_3] 全部指标计算完成\n")
  out_df
}
RSFLC_gcs <- function(baseline_data, longitude_data, t0 = 5) {
  stopifnot(is.data.frame(baseline_data), is.data.frame(longitude_data))
  
  # 1) timeData：前 10 天 GCS，每人 >= 2 条
  timeData_train_gcs <- longitude_data %>%
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
  
  valid_patients_gcs <- unique(timeData_train_gcs$hadm_id)
  
  # 2) fixedData：8 个静态基线变量
  fixedData_train_gcs <- baseline_data %>%
    dplyr::filter(hadm_id %in% valid_patients_gcs) %>%
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
  
  # 3) timeVarModel：GCS 轨迹 fixed ~ 1, random ~ time
  timeVar_gcs <- "time"
  timeVarModel_gcs <- list(
    gcs = list(fixed = gcs ~ 1, random = ~ time)
  )
  
  # 4) Y：28 天死亡
  Y_gcs <- list(
    type = "factor",
    Y = baseline_data %>%
      dplyr::filter(hadm_id %in% valid_patients_gcs) %>%
      dplyr::transmute(
        hadm_id = as.integer(hadm_id),
        event = factor(death_28d, levels = c(0, 1), labels = c("alive", "dead"))
      ) %>%
      dplyr::distinct(hadm_id, .keep_all = TRUE) %>%
      as.data.frame()
  )
  
  stopifnot(
    identical(sort(unique(timeData_train_gcs$hadm_id)), sort(fixedData_train_gcs$hadm_id)),
    identical(sort(fixedData_train_gcs$hadm_id), sort(Y_gcs$Y$hadm_id)),
    sum(is.na(timeData_train_gcs$gcs)) == 0
  )
  
  res_dyn_gcs <- DynForest::dynforest(
    timeData     = timeData_train_gcs,
    fixedData    = fixedData_train_gcs,
    timeVar      = timeVar_gcs,
    idVar        = "hadm_id",
    timeVarModel = timeVarModel_gcs,
    Y            = Y_gcs,
    mtry         = 3,
    nodesize     = 5,
    ncores       = 1,
    ntree        = 50,
    seed         = 1234
  )
  
  attr(res_dyn_gcs, "baseline") <- baseline_data
  attr(res_dyn_gcs, "model_name") <- "RSFLC_gcs"
  
  metrics_gcs <- cal_3(res_dyn_gcs, t0 = t0)
  
  list(
    metrics_gcs = metrics_gcs,
    res_dyn_gcs = res_dyn_gcs
  )
}




out <- RSFLC_sofa(
  baseline_data  = Data_baseline_5fold[[5]],
  longitude_data = Data_longitude_5fold[[5]],
  t0             = 5
)
metrics_sofa <- out$metrics_sofa   # AUC / C-index / BS 表格
res_dyn_sofa <- out$res_dyn_sofa     # DynForest 模型对象


out_gcs <- RSFLC_gcs(
  baseline_data  = Data_baseline_5fold[[1]],
  longitude_data  = Data_longitude_5fold[[1]],
  t0             = 5
)
metrics_gcs <- out_gcs$metrics_gcs
res_dyn_gcs <- out_gcs$res_dyn_gcs







