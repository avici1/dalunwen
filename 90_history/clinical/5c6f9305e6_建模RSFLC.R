library(dplyr)
library(tidyr)
library(readxl)
library(purrr)
library(DynForest)
library(missForest)

stroke_baselinedata_filter1_0531 <- read.csv2(
  "F:/文章_大论文/0521/处理后文件/stroke_baselinedata_filter1_0531.csv"
)

stroke_longitudinal_inputed_0603 <- read.csv(
  "F:/文章_大论文/0521/处理后文件/stroke_longitudinal_inputed_0603.csv"
)

##### 数据添补（已有 CSV 时设 RUN_IMPUTE <- FALSE 跳过）#####
RUN_IMPUTE <- FALSE
if (RUN_IMPUTE) {
  stroke_longitudinal_filter_0530 <- read.csv(
    "F:/文章_大论文/0521/处理后文件/stroke_longitudinal_filter_0530.csv"
  )
  id_cols <- c("X", "subject_id", "hadm_id", "chart_date", "times")
  impute_df <- stroke_longitudinal_filter_0530 %>%
    select(where(is.numeric)) %>%
    select(-any_of(id_cols))
  set.seed(1234)
  mf_out <- missForest(
    impute_df,
    maxiter = 5,
    ntree = 100,
    parallelize = "forests",
    verbose = TRUE
  )
  stroke_longitudinal_inputed_0603 <- stroke_longitudinal_filter_0530 %>%
    select(all_of(id_cols)) %>%
    bind_cols(mf_out$ximp)
  write.csv(
    stroke_longitudinal_inputed_0603,
    "F:/文章_大论文/0521/处理后文件/stroke_longitudinal_inputed_0603.csv",
    row.names = FALSE
  )
}
#############
# ---------------------------------------------------------------------------
# DynForest 建模（最小可跑版）
# ---------------------------------------------------------------------------

glimpse(stroke_baselinedata_filter1_0531)
glimpse(stroke_longitudinal_inputed_0603)

# 1) timeData_train：前 5 天 GCS，每人 >= 2 条纵向记录
timeData_train <- stroke_longitudinal_inputed_0603 %>%
  filter(times <= 5) %>%
  transmute(hadm_id = as.integer(hadm_id), time = as.integer(times), gcs = as.numeric(gcs)) %>%
  group_by(hadm_id) %>%
  filter(n() >= 2) %>%
  ungroup() %>%
  as.data.frame()

valid_patients <- unique(timeData_train$hadm_id)

# 2) fixedData_train：静态基线（宽表）
fixedData_train <- stroke_baselinedata_filter1_0531 %>%
  filter(hadm_id %in% valid_patients) %>%
  transmute(
    hadm_id = as.integer(hadm_id),
    age = as.numeric(age),
    apsiii = as.integer(apsiii)
  ) %>%
  distinct(hadm_id, .keep_all = TRUE) %>%
  as.data.frame()

# 3) timeVarModel：须用 random slope（random=~1 会触发 DynForest 内部 bug）
timeVar <- "time"
timeVarModel <- list(
  gcs = list(fixed = gcs ~ time, random = ~ time)
)

# 4) Y：28 天死亡
Y <- list(
  type = "factor",
  Y = stroke_baselinedata_filter1_0531 %>%
    filter(hadm_id %in% valid_patients) %>%
    transmute(
      hadm_id = as.integer(hadm_id),
      event = factor(death_28d, levels = c(0, 1), labels = c("alive", "dead"))
    ) %>%
    distinct(hadm_id, .keep_all = TRUE) %>%
    as.data.frame()
)

stopifnot(
  identical(sort(unique(timeData_train$hadm_id)), sort(fixedData_train$hadm_id)),
  identical(sort(fixedData_train$hadm_id), sort(Y$Y$hadm_id)),
  sum(is.na(timeData_train$gcs)) == 0
)

cat(
  "DynForest 输入 | n =", nrow(fixedData_train),
  "| 纵向行数 =", nrow(timeData_train),
  "| 28d 死亡率 =", round(mean(Y$Y$event == "dead"), 3), "\n"
)

res_dyn <- dynforest(
  timeData = timeData_train,
  fixedData = fixedData_train,
  timeVar = timeVar,
  idVar = "hadm_id",
  timeVarModel = timeVarModel,
  Y = Y,
  mtry = 2,
  nodesize = 5,
  ncores = 1,
  ntree = 50,
  seed = 1234
)

print(summary(res_dyn))



# 1) 预测（t0=5 与建模时“前5天 GCS”一致）
pred_dyn <- predict(
  object = res_dyn,
  timeData = timeData_train,
  fixedData = fixedData_train,
  idVar = "hadm_id",
  timeVar = "time",
  t0 = 5
)


# pred_indiv_proba = 预测类别的票比例；若预测 alive，则 P(dead)=1-proba
pred_df <- data.frame(
  hadm_id = as.integer(names(pred_dyn$pred_indiv)),
  pred_class = unname(pred_dyn$pred_indiv),
  prob_dead = ifelse(
    unname(pred_dyn$pred_indiv) == "dead",
    unname(pred_dyn$pred_indiv_proba),
    1 - unname(pred_dyn$pred_indiv_proba)
  )
)
# 3) 合并真实结局
pred_df <- merge(
  pred_df,
  transform(Y$Y, y_dead = as.integer(event == "dead")),
  by = "hadm_id"
)
# 4) 计算 AUC
roc_obj <- roc(
  response = pred_df$y_dead,
  predictor = pred_df$prob_dead,
  levels = c(0, 1),
  direction = "<",   # 概率越高越可能死亡
  quiet = TRUE
)
auc(roc_obj)                 # AUC 点估计
ci.auc(roc_obj)              # 95% CI（可选）
plot(roc_obj, main = "Apparent ROC (training data)")
















# timeData_train：前 5 天 GCS，每人 >= 2 条
timeData_train <- stroke_longitudinal_inputed_0603 %>%
  filter(times <= 5) %>%
  transmute(
    hadm_id = as.integer(hadm_id),
    time    = as.integer(times),
    gcs     = as.numeric(gcs)
  ) %>%
  group_by(hadm_id) %>%
  filter(n() >= 2) %>%
  ungroup() %>%
  as.data.frame()
valid_patients <- unique(timeData_train$hadm_id)
# fixedData_train
fixedData_train <- stroke_baselinedata_filter1_0531 %>%
  filter(hadm_id %in% valid_patients) %>%
  transmute(
    hadm_id = as.integer(hadm_id),
    age     = as.numeric(age),
    apsiii  = as.integer(apsiii)
  ) %>%
  distinct(hadm_id, .keep_all = TRUE) %>%
  as.data.frame()
# Y：28 天死亡
Y <- list(
  type = "factor",
  Y = stroke_baselinedata_filter1_0531 %>%
    filter(hadm_id %in% valid_patients) %>%
    transmute(
      hadm_id = as.integer(hadm_id),
      event   = factor(death_28d, levels = c(0, 1), labels = c("alive", "dead"))
    ) %>%
    distinct(hadm_id, .keep_all = TRUE) %>%
    as.data.frame()
)


timeVar <- "time"
stopifnot(
  identical(sort(unique(timeData_train$hadm_id)), sort(fixedData_train$hadm_id)),
  identical(sort(fixedData_train$hadm_id), sort(Y$Y$hadm_id)),
  sum(is.na(timeData_train$gcs)) == 0
)

timeVarModel <- list(
  gcs = list(
    fixed  = ~ 1,
    random = ~ 1
  )
)
res_dyn_simple <- dynforest(
  timeData      = timeData_train,
  fixedData     = fixedData_train,
  timeVar       = timeVar,
  idVar         = "hadm_id",
  timeVarModel  = timeVarModel,
  Y             = Y,
  mtry          = 2,
  nodesize      = 5,
  ncores        = 1,
  ntree         = 50,
  seed          = 1234
)




# 1) 预测（t0 = 5，要与建模时使用的前5天数据一致）
pred_dyn_simple <- predict(
  object     = res_dyn_simple,
  timeData   = timeData_train,
  fixedData  = fixedData_train,
  idVar      = "hadm_id",
  timeVar    = "time",
  t0         = 5
)

# 2) 整理预测结果
# pred_indiv_proba = 预测类别的票比例
# 如果预测 alive，则死亡概率 = 1 - proba
pred_df_simple <- data.frame(
  hadm_id = as.integer(names(pred_dyn_simple$pred_indiv)),
  pred_class = unname(pred_dyn_simple$pred_indiv),
  prob_dead = ifelse(
    unname(pred_dyn_simple$pred_indiv) == "dead",
    unname(pred_dyn_simple$pred_indiv_proba),
    1 - unname(pred_dyn_simple$pred_indiv_proba)
  )
)

# 3) 合并真实结局
pred_df_simple <- merge(
  pred_df_simple,
  transform(
    Y$Y,
    y_dead = as.integer(event == "dead")
  ),
  by = "hadm_id"
)

# 4) 计算 ROC 与 AUC
roc_obj_simple <- roc(
  response  = pred_df_simple$y_dead,
  predictor = pred_df_simple$prob_dead,
  levels    = c(0, 1),
  direction = "<",   # 概率越高越可能死亡
  quiet     = TRUE
)

# 5) 输出 AUC
auc(roc_obj_simple)

# 95% CI
ci.auc(roc_obj_simple)

# 6) 绘制 ROC 曲线
plot(
  roc_obj_simple,
  main = "Apparent ROC (training data)"
)







# 1) 从基线表取入 ICU 时间、死亡时间
eval_cindex <- pred_df_simple %>%
  left_join(
    stroke_baselinedata_filter1_0531 %>%
      select(hadm_id, intime, deathtime, death_28d) %>%
      distinct(hadm_id, .keep_all = TRUE),
    by = "hadm_id"
  ) %>%
  mutate(
    intime_parsed = as.POSIXct(intime, format = "%d/%m/%Y %H:%M:%S"),
    deathtime_chr = ifelse(deathtime == "" | is.na(deathtime), NA_character_, deathtime),
    deathtime_parsed = as.POSIXct(deathtime_chr, format = "%d/%m/%Y %H:%M:%S"),
    time_to_death = as.numeric(difftime(deathtime_parsed, intime_parsed, units = "days")),
    # 28 天内死亡：用实际死亡时间；否则在 28 天删失
    time28   = pmin(replace(time_to_death, is.na(time_to_death), 28), 28),
    status28 = as.integer(death_28d == 1)
  )
# 2) Harrell C-index（prob_dead 越高，风险越大 → reverse = TRUE）
cindex_obj <- concordance(
  Surv(time28, status28) ~ prob_dead,
  data = eval_cindex,
  reverse = TRUE
)
# 3) 输出
cat("C-index =", round(cindex_obj$concordance, 3), "\n")
cat("SE        =", round(sqrt(cindex_obj$var), 3), "\n")
cat("95% CI    = [",
    round(cindex_obj$concordance - 1.96 * sqrt(cindex_obj$var), 3), ", ",
    round(cindex_obj$concordance + 1.96 * sqrt(cindex_obj$var), 3), "]\n",
    sep = ""
)



brier_score <- mean((pred_df_simple$y_dead - pred_df_simple$prob_dead)^2)
cat("Brier Score =", round(brier_score, 4), "\n")





################程序#######################
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
  
  cat("[cal_3] 正在 predict（样本量大时耗时较长）...\n")
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



metrics_simple <- cal_3(res_dyn_simple, t0 = 5)

metrics_1 <-  cal_3(res_dyn, t0 = 5)







#####################################
# 1) timeData_train：前 5 天 GCS，每人 >= 2 条（不变）
timeData_train <- stroke_longitudinal_inputed_0603 %>%
  filter(times <= 10) %>%
  transmute(
    hadm_id = as.integer(hadm_id),
    time    = as.integer(times),
    gcs     = as.numeric(gcs)
  ) %>%
  group_by(hadm_id) %>%
  filter(n() >= 2) %>%
  ungroup() %>%
  as.data.frame()
valid_patients <- unique(timeData_train$hadm_id)
# 2) fixedData_train：8 个静态基线变量
fixedData_train <- stroke_baselinedata_filter1_0531 %>%
  filter(hadm_id %in% valid_patients) %>%
  transmute(
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
  distinct(hadm_id, .keep_all = TRUE) %>%
  as.data.frame()
# 3) timeVarModel：GCS 轨迹 fixed ~ 1, random ~ time
timeVar <- "time"
timeVarModel <- list(
  gcs = list(fixed = gcs ~ 1, random = ~ time)
)
# 4) Y：28 天死亡（不变）
Y <- list(
  type = "factor",
  Y = stroke_baselinedata_filter1_0531 %>%
    filter(hadm_id %in% valid_patients) %>%
    transmute(
      hadm_id = as.integer(hadm_id),
      event = factor(death_28d, levels = c(0, 1), labels = c("alive", "dead"))
    ) %>%
    distinct(hadm_id, .keep_all = TRUE) %>%
    as.data.frame()
)
res_dyn_8baseline <- dynforest(
  timeData     = timeData_train,
  fixedData    = fixedData_train,
  timeVar      = timeVar,
  idVar        = "hadm_id",
  timeVarModel = timeVarModel,
  Y            = Y,
  mtry         = 3,      # 原 2 → 3（1 纵向 + 8 静态，默认 sqrt≈3）
  nodesize     = 5,
  ncores       = 1,
  ntree        = 50,
  seed         = 1234
)


############## SOFA 轨迹版（8 静态变量 + sofa_24hours 纵向，结构同 GCS 版）############
# 1) timeData_train：前 10 天 SOFA，每人 >= 2 条
timeData_train_sofa <- stroke_longitudinal_inputed_0603 %>%
  filter(times <= 10) %>%
  transmute(
    hadm_id      = as.integer(hadm_id),
    time         = as.integer(times),
    sofa_24hours = as.numeric(sofa_24hours)
  ) %>%
  group_by(hadm_id) %>%
  filter(n() >= 2) %>%
  ungroup() %>%
  as.data.frame()
valid_patients_sofa <- unique(timeData_train_sofa$hadm_id)
# 2) fixedData_train：8 个静态基线变量（与 GCS 版相同）
fixedData_train_sofa <- stroke_baselinedata_filter1_0531 %>%
  filter(hadm_id %in% valid_patients_sofa) %>%
  transmute(
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
  distinct(hadm_id, .keep_all = TRUE) %>%
  as.data.frame()
# 3) timeVarModel：SOFA 轨迹 fixed ~ 1, random ~ time
timeVar_sofa <- "time"
timeVarModel_sofa <- list(
  sofa_24hours = list(fixed = sofa_24hours ~ 1, random = ~ time)
)
# 4) Y：28 天死亡（不变）
Y_sofa <- list(
  type = "factor",
  Y = stroke_baselinedata_filter1_0531 %>%
    filter(hadm_id %in% valid_patients_sofa) %>%
    transmute(
      hadm_id = as.integer(hadm_id),
      event = factor(death_28d, levels = c(0, 1), labels = c("alive", "dead"))
    ) %>%
    distinct(hadm_id, .keep_all = TRUE) %>%
    as.data.frame()
)
stopifnot(
  identical(sort(unique(timeData_train_sofa$hadm_id)), sort(fixedData_train_sofa$hadm_id)),
  identical(sort(fixedData_train_sofa$hadm_id), sort(Y_sofa$Y$hadm_id)),
  sum(is.na(timeData_train_sofa$sofa_24hours)) == 0
)
cat(
  "DynForest SOFA | n =", nrow(fixedData_train_sofa),
  "| 纵向行数 =", nrow(timeData_train_sofa),
  "| 28d 死亡率 =", round(mean(Y_sofa$Y$event == "dead"), 3), "\n"
)
res_dyn_sofa <- dynforest(
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



metrics_gcs <- cal_3(res_dyn_8baseline, t0 = 5)
metrics_sofa      <- cal_3(res_dyn_sofa,      t0 = 5)
















################################################
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























