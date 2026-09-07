# =============================================================================
# RSFLC_sofa_fold 分段运行（单折试跑：fold1验证，fold2-5训练）
# 前提：已运行 5折交叉验证RSF_LC.R 中的函数定义与数据分折部分
#       （prep_sofa_inputs、Data_baseline_5fold、Data_longitude_5fold 等已在环境中）
# 本文件不修改任何现有代码，仅供逐步调试
# =============================================================================

library(dplyr)
library(DynForest)
library(pROC)

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

# 4c) 计算 AUC
roc_obj <- pROC::roc(
  response  = pred_df$y_dead,
  predictor = pred_df$prob_dead,
  levels    = c(0, 1),
  direction = "<",
  quiet     = TRUE
)
auc_val <- as.numeric(pROC::auc(roc_obj))

cat("\n[步骤4] AUC =", round(auc_val, 4), "\n")

# 可供后续继续计算 C-index / Brier Score 的中间结果
list(
  res_dyn_sofa = res_dyn_sofa,
  pred_df      = pred_df,
  auc          = auc_val
)

