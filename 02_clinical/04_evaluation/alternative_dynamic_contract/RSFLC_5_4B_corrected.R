options(stringsAsFactors = FALSE)
suppressPackageStartupMessages({
  library(DynForest)
  library(survival)
  library(pROC)
})

args_all <- commandArgs(trailingOnly = FALSE)
file_hits <- grep("^--file=", args_all, value = TRUE)
file_arg <- if (length(file_hits)) sub("^--file=", "", file_hits[1]) else NA_character_
source_file <- tryCatch(sys.frame(1)$ofile, error = function(e) NULL)
script_dir <- if (!is.null(source_file) && nzchar(source_file)) {
  dirname(normalizePath(source_file))
} else if (!is.na(file_arg) && nzchar(file_arg)) {
  dirname(normalizePath(file_arg))
} else {
  Sys.getenv("RSF5_4B_CODE", unset = getwd())
}
source(file.path(script_dir, "common_dynamic_metric_contract.R"), encoding = "UTF-8")
source(file.path(script_dir, "rsflc_safe_predict.R"), encoding = "UTF-8")

base_dir <- Sys.getenv(
  "RSF5_4B_DATA_ROOT", unset = "F:/文章_大论文/0722/实例研究代码"
)
baseline_path <- file.path(base_dir, "stroke_baseline_knn_0824_fold.csv")
longitudinal_path <- file.path(base_dir, "stroke_longitudinal_knn_0824_group_fold.csv")
result_root <- Sys.getenv("RSF5_4B_OUT", unset = "F:/文章_大论文/0830/结果/RSF5_4B")
out_dir <- file.path(result_root, "RSFLC")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# 按用户要求，暂借用表5-4A RSF的超参数；这不是RSFLC重新调参结果。
ntree_use <- 500L
mtry_use <- 3L
nodesize_use <- 10L
minsplit_use <- 2L
ncores_use <- min(8L, max(1L, parallel::detectCores(logical = TRUE) - 1L))
set.seed(SEED_VALUE)

cohort <- make_common_dynamic_cohort(baseline_path, longitudinal_path)
base <- cohort$baseline
long <- cohort$longitudinal

make_pack <- function(ids) {
  b <- base[match(ids, base$hadm_id), , drop = FALSE]
  fixed <- b[, c("hadm_id", RSFLC_FIXED_COVARS), drop = FALSE]
  for (nm in c("mechvent", "electivesurgery", "gender", "stroke_type")) {
    fixed[[nm]] <- factor(fixed[[nm]])
  }
  td <- long[long$hadm_id %in% ids, c("hadm_id", "times", LONG_VARS), drop = FALSE]
  names(td)[names(td) == "times"] <- "time"
  outcome <- b[, c("hadm_id", "time28", "status28", "exact_death_time_missing"), drop = FALSE]
  y <- outcome[, c("hadm_id", "time28", "status28")]
  list(fixed = fixed, time = td, outcome = outcome, y = y)
}

train <- make_pack(cohort$train_ids)
test <- make_pack(cohort$test_ids)
time_var_model <- stats::setNames(lapply(LONG_VARS, function(v) {
  list(fixed = stats::as.formula(paste(v, "~ 1")), random = ~ time)
}), LONG_VARS)

cat(sprintf(
  "RSFLC共同landmark队列：训练 n=%d，事件=%d；验证 n=%d，事件=%d\n",
  nrow(train$outcome), sum(train$outcome$status28),
  nrow(test$outcome), sum(test$outcome$status28)
))
cat(sprintf(
  "拟合 survival DynForest：ntree=%d, mtry=%d, nodesize=%d, t0=%d\n",
  ntree_use, mtry_use, nodesize_use, LANDMARK_DAY
))

fit_rds <- file.path(out_dir, "RSFLC_5_4B_fit.rds")
if (file.exists(fit_rds) && identical(Sys.getenv("REFIT_RSFLC", unset = "0"), "0")) {
  cat("读取已有RSFLC拟合对象：", fit_rds, "\n")
  fit <- readRDS(fit_rds)
} else {
  fit <- DynForest::dynforest(
    timeData = train$time,
    fixedData = train$fixed,
    idVar = "hadm_id",
    timeVar = "time",
    timeVarModel = time_var_model,
    Y = list(type = "surv", Y = train$y),
    ntree = ntree_use,
    mtry = mtry_use,
    nodesize = nodesize_use,
    minsplit = minsplit_use,
    cause = 1,
    nsplit_option = "quantile",
    ncores = as.integer(ncores_use),
    seed = 2026L,
    verbose = TRUE
  )
  saveRDS(fit, fit_rds)
}

predict_curve <- function(fit, pack) {
  # DynForest 1.2.0 在部分节点的随机效应矩阵奇异时会把新患者叶节点置0，
  # 从而把整棵树风险错误补为0。安全路由对奇异矩阵加小岭/广义逆。
  predict_rsflc_safe(
    model = fit,
    time_data = pack$time,
    fixed_data = pack$fixed,
    ids = pack$outcome$hadm_id,
    grid = EVAL_GRID,
    landmark = LANDMARK_DAY,
    ncores = min(4L, ncores_use)
  )
}

test_risk <- predict_curve(fit, test)
test_eval <- evaluate_dynamic_risk(test_risk, test$outcome)

# 训练集只需表5-4B的C-index。直接汇总每棵树已保存的袋内叶节点28天风险，
# 避免对3994名训练患者重新做一次极慢的纵向混合模型路由。
inbag_risk28 <- function(model, ids) {
  total <- stats::setNames(numeric(length(ids)), as.character(ids))
  count <- stats::setNames(integer(length(ids)), as.character(ids))
  for (tt in seq_len(ncol(model$rf))) {
    tree <- model$rf[, tt]
    id_t <- as.character(tree$idY)
    leaf_t <- as.character(tree$leaves)
    leaf_risk <- vapply(unique(leaf_t), function(leaf) {
      pred <- tree$Y_pred[[leaf]][[as.character(model$cause)]]
      if (is.null(pred) || !nrow(pred)) return(NA_real_)
      as.numeric(tail(pred$traj, 1L))
    }, numeric(1))
    val <- unname(leaf_risk[leaf_t])
    ok <- id_t %in% names(total) & is.finite(val)
    total[id_t[ok]] <- total[id_t[ok]] + val[ok]
    count[id_t[ok]] <- count[id_t[ok]] + 1L
  }
  out <- total / count
  if (any(!is.finite(out))) stop("部分训练患者没有可用袋内预测。")
  out
}
train_risk28 <- inbag_risk28(fit, train$outcome$hadm_id)
train_out <- train$outcome[match(as.integer(names(train_risk28)), train$outcome$hadm_id), ]
train_cindex <- survival::concordance(
  survival::Surv(train_out$time28 - LANDMARK_DAY, train_out$status28) ~ train_risk28,
  reverse = TRUE
)$concordance
train_eval <- list(
  n = nrow(train_out), events = sum(train_out$status28), cindex = as.numeric(train_cindex)
)

setting <- sprintf(
  "survival DynForest; ntree=%d, mtry=%d, nodesize=%d, t0=%d天；训练C-index为袋内表观值",
  ntree_use, mtry_use, nodesize_use, LANDMARK_DAY
)
metrics <- metric_rows("RSFLC", setting, train_eval, test_eval)
cohort_audit <- data.frame(
  数据集 = c("训练集", "验证集"),
  group = c(1L, 2L),
  样本量 = c(train_eval$n, test_eval$n),
  `5至28天事件数` = c(train_eval$events, test_eval$events),
  精确死亡时刻缺失数 = c(
    sum(train$outcome$exact_death_time_missing),
    sum(test$outcome$exact_death_time_missing)
  ),
  stringsAsFactors = FALSE
)
params <- data.frame(
  参数 = c("ntree", "mtry", "nodesize", "minsplit", "landmark", "horizon", "seed"),
  数值 = c(ntree_use, mtry_use, nodesize_use, minsplit_use, LANDMARK_DAY, HORIZON_DAY, SEED_VALUE),
  来源 = c(rep("表5-4A RSF临时借用", 3), "固定", "论文预设", "论文预设", "固定"),
  stringsAsFactors = FALSE
)

write_utf8_csv(params, file.path(out_dir, "00_模型参数.csv"))
write_utf8_csv(cohort_audit, file.path(out_dir, "01_共同队列审计.csv"))
write_utf8_csv(metrics, file.path(out_dir, "02_表5-4B_RSFLC指标.csv"))
write_utf8_csv(
  data.frame(hadm_id = names(train_risk28), risk28_inbag = as.numeric(train_risk28)),
  file.path(out_dir, "03_训练集28天袋内预测.csv")
)
write_utf8_csv(cbind(hadm_id = rownames(test_risk), as.data.frame(test_risk)), file.path(out_dir, "04_验证集条件风险曲线.csv"))
write_utf8_csv(test_eval$brier_curve, file.path(out_dir, "05_验证集Brier曲线.csv"))
write_utf8_csv(test_eval$horizon, file.path(out_dir, "06_验证集28天预测.csv"))

cat("\nRSFLC 表5-4B指标：\n")
print(metrics, row.names = FALSE)
