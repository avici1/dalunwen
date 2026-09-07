options(stringsAsFactors = FALSE)
suppressPackageStartupMessages({
  library(dplyr)
  library(survival)
  library(nlme)
  library(JMbayes2)
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

base_dir <- Sys.getenv(
  "RSF5_4B_DATA_ROOT", unset = "F:/文章_大论文/0722/实例研究代码"
)
baseline_path <- file.path(base_dir, "stroke_baseline_knn_0824_fold.csv")
longitudinal_path <- file.path(base_dir, "stroke_longitudinal_knn_0824_group_fold.csv")
result_root <- Sys.getenv("RSF5_4B_OUT", unset = "F:/文章_大论文/0830/结果/RSF5_4B")
out_dir <- file.path(result_root, "JM")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

n_chains_use <- 3L
n_iter_use <- 6000L
n_burnin_use <- 3000L
n_cores_fit <- min(n_chains_use, max(1L, parallel::detectCores(logical = TRUE) - 1L))
prediction_chunk_size <- 200L
prediction_samples <- 100L
prediction_mcmc <- 10L
jm_traj_use <- c("sofa_24hours")
set.seed(SEED_VALUE)

cohort <- make_common_dynamic_cohort(baseline_path, longitudinal_path)
base <- cohort$baseline
long <- cohort$longitudinal
day1 <- cohort$day1

build_long_data <- function(ids) {
  td <- long[long$hadm_id %in% ids, c("hadm_id", "times", jm_traj_use), drop = FALSE]
  names(td)[names(td) == "times"] <- "t"
  b <- base[base$hadm_id %in% ids, c(
    "hadm_id", CLINICAL_COVARS, "group", "time28", "status28",
    "exact_death_time_missing"
  ), drop = FALSE]
  names(b)[names(b) == "time28"] <- "obs_time"
  names(b)[names(b) == "status28"] <- "event"
  d1 <- day1[day1$hadm_id %in% ids, c("hadm_id", JM_DAY1_VARS), drop = FALSE]
  out <- merge(td, b, by = "hadm_id", sort = FALSE)
  out <- merge(out, d1, by = "hadm_id", sort = FALSE)
  out$mechvent <- factor(out$mechvent)
  out$electivesurgery <- factor(out$electivesurgery)
  out <- out[order(out$hadm_id, out$t), , drop = FALSE]
  rownames(out) <- NULL
  out
}

train_long <- build_long_data(cohort$train_ids)
test_long <- build_long_data(cohort$test_ids)
train_surv <- train_long[!duplicated(train_long$hadm_id), , drop = FALSE]
test_surv <- test_long[!duplicated(test_long$hadm_id), , drop = FALSE]
cox_covars <- c(CLINICAL_COVARS, JM_DAY1_VARS)

cat(sprintf(
  "JM共同landmark队列：训练 n=%d，事件=%d；验证 n=%d，事件=%d\n",
  nrow(train_surv), sum(train_surv$event), nrow(test_surv), sum(test_surv$event)
))

jm_rds <- file.path(out_dir, "JM_5_4B_fit.rds")
if (file.exists(jm_rds) && identical(Sys.getenv("REFIT_JM", unset = "0"), "0")) {
  cat("读取已有修正版JM拟合对象：", jm_rds, "\n")
  jm_fit <- readRDS(jm_rds)
} else {
  lme_ctrl <- nlme::lmeControl(opt = "optim", msMaxIter = 200)
  cat("拟合3条0至5天纵向轨迹混合模型...\n")
  # 三轨迹JM的GCS/CNS关联参数在多轮延长链后仍未收敛；
  # 最终采用临床综合性更强、关联参数稳定的SOFA单轨迹JM。
  fm_sofa <- nlme::lme(sofa_24hours ~ t, data = train_long, random = ~ t | hadm_id, control = lme_ctrl)

  cox_var_ok <- vapply(cox_covars, function(nm) {
    x <- train_surv[[nm]]
    if (is.factor(x)) nlevels(droplevels(x)) >= 2L else length(unique(x)) >= 2L
  }, logical(1))
  cox_use <- cox_covars[cox_var_ok]
  cox_formula <- stats::as.formula(
    paste("Surv(obs_time, event) ~", paste(cox_use, collapse = " + "))
  )
  cox_fit <- survival::coxph(cox_formula, data = train_surv, x = TRUE, model = TRUE)
  cat(sprintf(
    "拟合JMbayes2：chains=%d, iter=%d, burnin=%d, cores=%d\n",
    n_chains_use, n_iter_use, n_burnin_use, n_cores_fit
  ))
  jm_fit <- JMbayes2::jm(
    Surv_object = cox_fit,
    Mixed_objects = list(fm_sofa),
    time_var = "t",
    id_var = "hadm_id",
    n_chains = n_chains_use,
    n_iter = n_iter_use,
    n_burnin = n_burnin_use,
    base_hazard_segments = 5L,
    MALA = TRUE,
    cores = n_cores_fit,
    seed = 2026L
  )
  saveRDS(jm_fit, jm_rds)
}

predict_jm_curve <- function(fit, dat, ids) {
  chunks <- split(ids, ceiling(seq_along(ids) / prediction_chunk_size))
  risk_parts <- vector("list", length(chunks))
  for (k in seq_along(chunks)) {
    chunk_ids <- chunks[[k]]
    nd <- dat[dat$hadm_id %in% chunk_ids & dat$t <= LANDMARK_DAY + 1e-8, , drop = FALSE]
    nd$obs_time <- LANDMARK_DAY
    nd$event <- 0L
    pr <- predict(
      fit,
      newdata = nd,
      process = "event",
      times = EVAL_GRID,
      type = "subject_specific",
      return_newdata = TRUE,
      control = list(
        all_times = TRUE,
        n_samples = prediction_samples,
        n_mcmc = prediction_mcmc,
        cores = 1L,
        seed = 2026L + k
      )
    )
    if (!all(c("hadm_id", "t", "pred_CIF") %in% names(pr))) {
      stop("JM predict()输出缺少 hadm_id/t/pred_CIF。")
    }
    wide <- matrix(NA_real_, nrow = length(chunk_ids), ncol = length(EVAL_GRID))
    rownames(wide) <- as.character(chunk_ids)
    colnames(wide) <- format(EVAL_GRID, trim = TRUE, scientific = FALSE)
    for (i in seq_along(chunk_ids)) {
      one <- pr[pr$hadm_id == chunk_ids[i], c("t", "pred_CIF"), drop = FALSE]
      one <- one[order(one$t), , drop = FALSE]
      hit <- match(EVAL_GRID, one$t)
      if (anyNA(hit)) stop("JM预测未返回完整评价网格。")
      wide[i, ] <- one$pred_CIF[hit]
    }
    risk_parts[[k]] <- wide
    cat(sprintf("JM预测 %d/%d 批完成\n", k, length(chunks)))
  }
  risk <- do.call(rbind, risk_parts)
  risk <- risk[match(as.character(ids), rownames(risk)), , drop = FALSE]
  risk <- t(apply(risk, 1L, cummax))
  rownames(risk) <- as.character(ids)
  colnames(risk) <- format(EVAL_GRID, trim = TRUE, scientific = FALSE)
  pmin(pmax(risk, 0), 1)
}

train_risk <- predict_jm_curve(jm_fit, train_long, cohort$train_ids)
test_risk <- predict_jm_curve(jm_fit, test_long, cohort$test_ids)
train_outcome <- train_surv[, c("hadm_id", "obs_time", "event", "exact_death_time_missing")]
test_outcome <- test_surv[, c("hadm_id", "obs_time", "event", "exact_death_time_missing")]
names(train_outcome)[names(train_outcome) == "obs_time"] <- "time28"
names(train_outcome)[names(train_outcome) == "event"] <- "status28"
names(test_outcome)[names(test_outcome) == "obs_time"] <- "time28"
names(test_outcome)[names(test_outcome) == "event"] <- "status28"

train_eval <- evaluate_dynamic_risk(train_risk, train_outcome)
test_eval <- evaluate_dynamic_risk(test_risk, test_outcome)
setting <- "JMbayes2；SOFA单轨迹个体随机截距+随机斜率；25项生存协变量；t0=5天"
metrics <- metric_rows("JM", setting, train_eval, test_eval)

flatten_rhat <- function(x) {
  if (is.null(x)) return(NULL)
  if (is.matrix(x) || (is.numeric(x) && !is.null(dim(x)))) {
    rn <- rownames(x)
    if (is.null(rn)) rn <- as.character(seq_len(nrow(x)))
    return(stats::setNames(as.numeric(x[, 1]), rn))
  }
  if (is.numeric(x) && !is.null(names(x))) return(x)
  if (is.list(x)) {
    z <- lapply(x, flatten_rhat)
    z <- z[!vapply(z, is.null, logical(1))]
    if (length(z)) return(unlist(z, use.names = TRUE))
  }
  NULL
}
rhat <- flatten_rhat(jm_fit$statistics$Rhat)
# 个体frailty的Rhat为NA且不属于总体参数收敛表；只汇总有限的总体参数。
rhat <- rhat[is.finite(rhat) & !grepl("^frailty\\.[0-9]+$", names(rhat))]
mcmc_diag <- data.frame(
  参数数 = length(rhat),
  最大_Rhat = if (length(rhat)) max(rhat, na.rm = TRUE) else NA_real_,
  Rhat_大于_1_01 = if (length(rhat)) sum(rhat > 1.01, na.rm = TRUE) else NA_integer_,
  Rhat_大于_1_05 = if (length(rhat)) sum(rhat > 1.05, na.rm = TRUE) else NA_integer_,
  stringsAsFactors = FALSE
)
cohort_audit <- data.frame(
  数据集 = c("训练集", "验证集"),
  group = c(1L, 2L),
  样本量 = c(train_eval$n, test_eval$n),
  `5至28天事件数` = c(train_eval$events, test_eval$events),
  精确死亡时刻缺失数 = c(
    sum(train_outcome$exact_death_time_missing),
    sum(test_outcome$exact_death_time_missing)
  ),
  stringsAsFactors = FALSE
)
params <- data.frame(
  参数 = c("landmark", "horizon", "chains", "iter", "burnin", "prediction_samples", "seed"),
  数值 = c(LANDMARK_DAY, HORIZON_DAY, n_chains_use, n_iter_use, n_burnin_use, prediction_samples, SEED_VALUE),
  stringsAsFactors = FALSE
)

write_utf8_csv(params, file.path(out_dir, "00_模型参数.csv"))
write_utf8_csv(cohort_audit, file.path(out_dir, "01_共同队列审计.csv"))
write_utf8_csv(metrics, file.path(out_dir, "02_表5-4B_JM指标.csv"))
write_utf8_csv(mcmc_diag, file.path(out_dir, "03_MCMC收敛诊断.csv"))
write_utf8_csv(cbind(hadm_id = rownames(train_risk), as.data.frame(train_risk)), file.path(out_dir, "04_训练集条件风险曲线.csv"))
write_utf8_csv(cbind(hadm_id = rownames(test_risk), as.data.frame(test_risk)), file.path(out_dir, "05_验证集条件风险曲线.csv"))
write_utf8_csv(train_eval$brier_curve, file.path(out_dir, "06_训练集Brier曲线.csv"))
write_utf8_csv(test_eval$brier_curve, file.path(out_dir, "07_验证集Brier曲线.csv"))
write_utf8_csv(train_eval$horizon, file.path(out_dir, "08_训练集28天预测.csv"))
write_utf8_csv(test_eval$horizon, file.path(out_dir, "09_验证集28天预测.csv"))

cat("\nJM 表5-4B指标：\n")
print(metrics, row.names = FALSE)
cat("\nMCMC诊断：\n")
print(mcmc_diag, row.names = FALSE)
