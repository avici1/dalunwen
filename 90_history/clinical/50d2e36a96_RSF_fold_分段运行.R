# =============================================================================
# RSF 分段运行（最小调试：fold1验证，fold2-5训练 | 仅 baseline + rfsrc 建模）
# 前提：已运行 5折交叉验证RSF.R 中的数据读取与 5 折划分
#       （Data_baseline_5fold 已在环境中）
# =============================================================================

library(dplyr)
library(survival)
library(randomForestSRC)

# ---- prep_rsf_inputs：baseline → rfsrc 用数据（不去重，调用方保证无重复）----
prep_rsf_inputs <- function(baseline_data) {
  stopifnot(is.data.frame(baseline_data))
  need_cols <- c(
    "hadm_id", "age", "charlson_comorbidity_index", "apsiii", "sapsii", "oasis",
    "preiculos", "mechvent", "electivesurgery",
    "intime", "deathtime", "death_28d"
  )
  if (!all(need_cols %in% names(baseline_data))) {
    stop("baseline_data 缺少必要列: ", paste(setdiff(need_cols, names(baseline_data)), collapse = ", "))
  }

  out <- baseline_data %>%
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

  intime_parsed <- as.POSIXct(out$intime, format = "%d/%m/%Y %H:%M:%S")
  deathtime_chr <- ifelse(
    out$deathtime == "" | is.na(out$deathtime),
    NA_character_,
    out$deathtime
  )
  deathtime_parsed <- as.POSIXct(deathtime_chr, format = "%d/%m/%Y %H:%M:%S")
  time_to_death <- as.numeric(difftime(deathtime_parsed, intime_parsed, units = "days"))
  time_to_death[is.na(time_to_death)] <- 28

  out$time28   <- pmin(time_to_death, 28)
  out$status28 <- as.integer(out$death_28d == 1)

  list(
    rsf_data = out %>%
      dplyr::select(
        age, charlson_comorbidity_index, apsiii, sapsii, oasis, preiculos,
        mechvent, electivesurgery, time28, status28
      ) %>%
      as.data.frame(),
    meta = out %>% dplyr::select(hadm_id, time28, status28, death_28d)
  )
}


# ---- 第 0 步：准备训练/验证原始数据 ----
val_k     <- 1
train_idx <- 2:5

train_baseline <- dplyr::bind_rows(Data_baseline_5fold[train_idx])
val_baseline   <- Data_baseline_5fold[[val_k]]

cat("\n[步骤0] 训练集 baseline 行数:", nrow(train_baseline), "\n")
cat("[步骤0] 验证集 baseline 行数:", nrow(val_baseline), "\n")


# ---- 第 1 步：训练集数据预处理 ----
train_inputs_rsf <- prep_rsf_inputs(train_baseline)

cat("\n[步骤1] 训练集预处理完成\n")
cat("  rsf_data 行数:", nrow(train_inputs_rsf$rsf_data), "\n")
cat("  死亡人数 status28=1:", sum(train_inputs_rsf$rsf_data$status28), "\n")
cat("  time28 范围:", paste(range(train_inputs_rsf$rsf_data$time28), collapse = " ~ "), "\n")


# ---- 第 2 步：rfsrc 建模（训练集）----
n_pred <- 8L
rfsrc_fit <- randomForestSRC::rfsrc(
  survival::Surv(time28, status28) ~ .,
  data = train_inputs_rsf$rsf_data,
  ntree      = 1000,
  mtry       = max(1L, floor(n_pred / 3)),
  nodesize   = 10,
  importance = TRUE,
  proximity  = FALSE,
  seed       = 123
)

cat("\n[步骤2] rfsrc 建模完成\n")
cat("  训练样本数:", rfsrc_fit$n,
    "| 事件数:", sum(train_inputs_rsf$rsf_data$status28), "\n")
cat("  OOB 误差:", round(rfsrc_fit$err.rate[rfsrc_fit$ntree], 4), "\n")

# 中间结果（后续步骤 3 验证集预处理、步骤 4 predict + 指标 可接着用）
list(
  rfsrc_fit        = rfsrc_fit,
  train_inputs_rsf = train_inputs_rsf,
  train_baseline   = train_baseline,
  val_baseline     = val_baseline
)
