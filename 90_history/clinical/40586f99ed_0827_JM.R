# =============================================================================
# JM 小样本试跑：3 条纵向轨迹 + 17 个第 1 日截面 + 8 个临床基线
# 轨迹：gcs, sofa_24hours, cns_24hours
# 拟合对齐 5_jointmodel_10V3C.R（JMbayes2::jm，随机截距）
# group=1 分层抽 140 训练，group=2 分层抽 60 外验证
# C-index：event-process 最后时刻风险 + concordance(reverse=TRUE)
# AUC / IPCW-Brier / IBS：与 RSF、RSFLC 相同，riskRegression::Score
#   times = seq(0,27.5,0.5) 并在 27.9999 取 28 天指标，cens.method = "ipcw"
# Word 第3–8节：系数 / α / 随机效应 / MCMC / DIC-WAIC-LPML（预测性能只写 CSV）
# =============================================================================
library(dplyr)
library(survival)
library(nlme)
library(JMbayes2)
library(prodlim)
library(riskRegression)
library(officer)
library(flextable)
library(coda)

set.seed(2026L)

base_dir <- "F:/文章_大论文/0722/实例研究代码"
out_dir <- file.path(base_dir, "试运行", "JM")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
baseline_path <- file.path(base_dir, "stroke_baseline_knn_0824_fold.csv")
longitudinal_path <- file.path(base_dir, "stroke_longitudinal_knn_0824_group_fold.csv")

n_train_target <- 140L
n_test_target <- 60L
# 单链不计算 Gelman Rhat（表中 Rhat 为空属正常）；要 Rhat 需 n_chains >= 2
n_chains_use <- 1L
n_iter_use <- 1500L
n_burnin_use <- 500L

evaluation_horizon <- 28 - 1e-04
evaluation_times <- unique(c(seq(0, 27.5, by = 0.5), evaluation_horizon))

marker_all <- c(
  "total_urine_output", "creat", "aki_stage", "gcs", "ph", "pco2",
  "lactate", "po2", "pao2fio2ratio", "glucose", "sodium", "bicarbonate",
  "hemoglobin", "temperature", "fio2", "sofa_24hours", "cns_24hours",
  "renal_24hours", "cardiovascular_24hours", "respiration_24hours"
)
traj_vars <- c("gcs", "sofa_24hours", "cns_24hours")
day1_vars <- setdiff(marker_all, traj_vars)
clinical_covars <- c(
  "age", "charlson_comorbidity_index", "apsiii", "sapsii", "oasis",
  "preiculos", "mechvent", "electivesurgery"
)
factor_covars <- c("mechvent", "electivesurgery")
cox_covars <- c(clinical_covars, day1_vars)

stopifnot(
  length(traj_vars) == 3L,
  length(day1_vars) == 17L,
  length(clinical_covars) == 8L,
  length(cox_covars) == 25L,
  length(intersect(traj_vars, day1_vars)) == 0L
)

sample_stratified_ids <- function(outcome_df, n_target) {
  ids0 <- outcome_df$hadm_id[outcome_df$event == 0L]
  ids1 <- outcome_df$hadm_id[outcome_df$event == 1L]
  n_avail <- nrow(outcome_df)
  n_use <- min(as.integer(n_target), n_avail)
  p <- mean(outcome_df$event)
  n1 <- min(length(ids1), max(1L, as.integer(round(n_use * p))))
  n0 <- min(length(ids0), n_use - n1)
  remain <- n_use - n0 - n1
  if (remain > 0L) {
    extra0 <- min(length(ids0) - n0, remain)
    n0 <- n0 + extra0
    remain <- n_use - n0 - n1
    extra1 <- min(length(ids1) - n1, remain)
    n1 <- n1 + extra1
  }
  c(sample(ids0, n0), sample(ids1, n1))
}

pred_event_last <- function(fit, dat, surv) {
  pred <- predict(fit, newdata = dat, process = "event")
  pred_patient <- data.frame(
    hadm_id = as.integer(as.character(pred$id)),
    time = pred$times,
    risk = as.numeric(pred$pred),
    stringsAsFactors = FALSE
  )
  risk_last <- pred_patient %>%
    dplyr::group_by(hadm_id) %>%
    dplyr::summarise(risk = dplyr::last(risk), .groups = "drop")
  surv_use <- surv
  surv_use$hadm_id <- as.integer(surv_use$hadm_id)
  cdata <- merge(surv_use, risk_last, by = "hadm_id", sort = FALSE)
  cdata <- cdata[match(surv_use$hadm_id, cdata$hadm_id), , drop = FALSE]
  if (nrow(cdata) == 0L || any(!is.finite(cdata$risk))) {
    stop("预测风险与生存表未能对齐，或存在非有限 risk")
  }
  cdata$probability <- pmin(pmax(as.numeric(cdata$risk), 0), 1)
  cdata
}

build_constant_risk_matrix <- function(probability, times) {
  n <- length(probability)
  mat <- matrix(
    rep(as.numeric(probability), times = length(times)),
    nrow = n,
    ncol = length(times),
    byrow = FALSE
  )
  colnames(mat) <- as.character(times)
  mat
}

extract_score_at_horizon <- function(score_obj, model_name, horizon) {
  auc_tbl <- as.data.frame(score_obj$AUC$score)
  brier_tbl <- as.data.frame(score_obj$Brier$score)
  auc_val <- auc_tbl$AUC[
    as.character(auc_tbl$model) == model_name &
      abs(auc_tbl$times - horizon) < 1e-08
  ]
  brier_val <- brier_tbl$Brier[
    as.character(brier_tbl$model) == model_name &
      abs(brier_tbl$times - horizon) < 1e-08
  ]
  ibs_val <- brier_tbl$IBS[
    as.character(brier_tbl$model) == model_name &
      abs(brier_tbl$times - horizon) < 1e-08
  ]
  list(
    auc = if (length(auc_val)) as.numeric(auc_val[1]) else NA_real_,
    brier = if (length(brier_val)) as.numeric(brier_val[1]) else NA_real_,
    ibs = if (length(ibs_val)) as.numeric(ibs_val[1]) else NA_real_
  )
}

score_ipcw <- function(probability, time, status, label) {
  score_data <- data.frame(
    time28 = as.numeric(time),
    status28 = as.integer(status),
    stringsAsFactors = FALSE
  )
  risk_mat <- build_constant_risk_matrix(probability, evaluation_times)
  score_obj <- tryCatch(
    riskRegression::Score(
      object = list(JM = risk_mat),
      formula = Hist(time28, status28) ~ 1,
      data = score_data,
      metrics = c("auc", "brier"),
      times = evaluation_times,
      summary = "ibs",
      cens.method = "ipcw",
      conf.int = FALSE,
      plots = NULL
    ),
    error = function(e) {
      cat(label, "Score 失败:", conditionMessage(e), "\n")
      flush.console()
      NULL
    }
  )
  if (is.null(score_obj)) {
    return(list(auc = NA_real_, brier = NA_real_, ibs = NA_real_))
  }
  extract_score_at_horizon(score_obj, "JM", evaluation_horizon)
}

# ---------------------------------------------------------------------------
# 读数
# ---------------------------------------------------------------------------
stroke_baseline <- read.csv(
  baseline_path,
  stringsAsFactors = FALSE,
  na.strings = c("", "NA"),
  fileEncoding = "UTF-8"
)
stroke_longitudinal <- read.csv(
  longitudinal_path,
  stringsAsFactors = FALSE,
  na.strings = c("", "NA"),
  fileEncoding = "UTF-8"
)

miss_base <- setdiff(c("hadm_id", clinical_covars, "intime", "deathtime", "death_28d", "group"), names(stroke_baseline))
miss_long <- setdiff(c("hadm_id", "times", marker_all), names(stroke_longitudinal))
if (length(miss_base) > 0L) {
  stop("基线数据缺少列: ", paste(miss_base, collapse = ", "))
}
if (length(miss_long) > 0L) {
  stop("纵向数据缺少列: ", paste(miss_long, collapse = ", "))
}

base_df <- stroke_baseline %>%
  dplyr::transmute(
    hadm_id = as.integer(.data$hadm_id),
    age = as.numeric(.data$age),
    charlson_comorbidity_index = as.numeric(.data$charlson_comorbidity_index),
    apsiii = as.numeric(.data$apsiii),
    sapsii = as.numeric(.data$sapsii),
    oasis = as.numeric(.data$oasis),
    preiculos = as.numeric(.data$preiculos),
    mechvent = factor(.data$mechvent, levels = c(0, 1)),
    electivesurgery = factor(.data$electivesurgery, levels = c(0, 1)),
    group = as.integer(.data$group),
    intime = as.POSIXct(.data$intime, format = "%d/%m/%Y %H:%M:%S"),
    deathtime = as.POSIXct(.data$deathtime, format = "%d/%m/%Y %H:%M:%S"),
    death_28d = as.integer(.data$death_28d)
  ) %>%
  dplyr::mutate(
    obs_time = as.numeric(difftime(.data$deathtime, .data$intime, units = "days")),
    obs_time = ifelse(is.na(.data$obs_time), 28, pmin(.data$obs_time, 28)),
    event = as.integer(.data$death_28d == 1)
  ) %>%
  dplyr::select(-"intime", -"deathtime", -"death_28d") %>%
  dplyr::filter(.data$obs_time > 0, !is.na(.data$event), .data$group %in% c(1L, 2L)) %>%
  dplyr::distinct(.data$hadm_id, .keep_all = TRUE)

day1_df <- stroke_longitudinal %>%
  dplyr::select(dplyr::all_of(c("hadm_id", "times", day1_vars))) %>%
  dplyr::filter(.data$times == 1) %>%
  dplyr::mutate(
    hadm_id = as.integer(.data$hadm_id),
    dplyr::across(dplyr::all_of(day1_vars), as.numeric)
  ) %>%
  dplyr::select(-"times") %>%
  dplyr::distinct(.data$hadm_id, .keep_all = TRUE)

long_df <- stroke_longitudinal %>%
  dplyr::select(dplyr::all_of(c("hadm_id", "times", traj_vars))) %>%
  dplyr::mutate(
    hadm_id = as.integer(.data$hadm_id),
    t = as.numeric(.data$times),
    dplyr::across(dplyr::all_of(traj_vars), as.numeric)
  ) %>%
  dplyr::filter(.data$t <= 10, dplyr::if_all(dplyr::all_of(traj_vars), is.finite)) %>%
  dplyr::select(-"times") %>%
  dplyr::inner_join(base_df, by = "hadm_id") %>%
  dplyr::inner_join(day1_df, by = "hadm_id") %>%
  dplyr::filter(.data$t <= .data$obs_time) %>%
  dplyr::group_by(.data$hadm_id) %>%
  dplyr::filter(dplyr::n() >= 2L) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(.data$hadm_id, .data$t) %>%
  as.data.frame()

cox_ok <- stats::complete.cases(long_df[, cox_covars, drop = FALSE])
if (any(!cox_ok)) {
  drop_ids <- unique(long_df$hadm_id[!cox_ok])
  cat("剔除 Cox 协变量含缺失的患者 n =", length(drop_ids), "\n")
  long_df <- long_df[!long_df$hadm_id %in% drop_ids, , drop = FALSE]
}

surv_all <- long_df[!duplicated(long_df$hadm_id), , drop = FALSE]
pool_train <- surv_all[surv_all$group == 1L, , drop = FALSE]
pool_test <- surv_all[surv_all$group == 2L, , drop = FALSE]
if (nrow(pool_train) == 0L || nrow(pool_test) == 0L) {
  stop("合格过滤后 group=1 或 group=2 为空")
}

cat(
  "合格患者: 训练池 group=1 n =", nrow(pool_train),
  "| 死亡 =", sum(pool_train$event),
  sprintf("（%.1f%%）", 100 * mean(pool_train$event)),
  "| 验证池 group=2 n =", nrow(pool_test),
  "| 死亡 =", sum(pool_test$event),
  sprintf("（%.1f%%）", 100 * mean(pool_test$event)), "\n"
)
flush.console()

train_ids <- sort(sample_stratified_ids(pool_train, n_train_target))
test_ids <- sort(sample_stratified_ids(pool_test, n_test_target))

train_long <- long_df[long_df$hadm_id %in% train_ids, , drop = FALSE]
test_long <- long_df[long_df$hadm_id %in% test_ids, , drop = FALSE]
train_surv <- train_long[!duplicated(train_long$hadm_id), , drop = FALSE]
test_surv <- test_long[!duplicated(test_long$hadm_id), , drop = FALSE]
rownames(train_long) <- NULL
rownames(test_long) <- NULL
rownames(train_surv) <- NULL
rownames(test_surv) <- NULL

cat(
  "抽中训练集 n =", nrow(train_surv),
  "| 死亡 =", sum(train_surv$event),
  sprintf("（%.1f%%）", 100 * mean(train_surv$event)),
  "| 纵向行 =", nrow(train_long), "\n"
)
cat(
  "抽中验证集 n =", nrow(test_surv),
  "| 死亡 =", sum(test_surv$event),
  sprintf("（%.1f%%）", 100 * mean(test_surv$event)),
  "| 纵向行 =", nrow(test_long), "\n"
)
flush.console()

# ---------------------------------------------------------------------------
# 纵向 lme + Cox + jm
# ---------------------------------------------------------------------------
lme_ctrl <- nlme::lmeControl(opt = "optim", msMaxIter = 200)
cat("[lme] 开始拟合 3 条轨迹...\n")
flush.console()
t_lme <- Sys.time()
fm_gcs <- nlme::lme(
  gcs ~ t,
  data = train_long,
  random = ~ 1 | hadm_id,
  control = lme_ctrl
)
fm_sofa <- nlme::lme(
  sofa_24hours ~ t,
  data = train_long,
  random = ~ 1 | hadm_id,
  control = lme_ctrl
)
fm_cns <- nlme::lme(
  cns_24hours ~ t,
  data = train_long,
  random = ~ 1 | hadm_id,
  control = lme_ctrl
)
cat(
  "[lme] 完成 |",
  round(as.numeric(difftime(Sys.time(), t_lme, units = "secs")), 1),
  "秒\n"
)
flush.console()

cox_var_ok <- vapply(cox_covars, function(nm) {
  x <- train_surv[[nm]]
  if (is.factor(x)) {
    nlevels(droplevels(x)) >= 2L
  } else {
    length(unique(x[is.finite(as.numeric(x))])) >= 2L
  }
}, logical(1))
cox_covars_dropped <- cox_covars[!cox_var_ok]
cox_covars_use <- cox_covars[cox_var_ok]
if (length(cox_covars_use) == 0L) {
  cox_fml <- Surv(obs_time, event) ~ 1
} else {
  cox_fml <- stats::as.formula(
    paste("Surv(obs_time, event) ~", paste(cox_covars_use, collapse = " + "))
  )
}
if (length(cox_covars_dropped) > 0L) {
  cat(
    "[cox] 训练集无变异，已从公式剔除:",
    paste(cox_covars_dropped, collapse = ", "), "\n"
  )
  flush.console()
}

coxFit <- survival::coxph(cox_fml, data = train_surv, x = TRUE, model = TRUE)

cat(
  "[jm] 开始拟合 | n_chains =", n_chains_use,
  "| n_iter =", n_iter_use,
  "| n_burnin =", n_burnin_use, "\n"
)
flush.console()
t_jm <- Sys.time()
jmFit <- tryCatch(
  JMbayes2::jm(
    Surv_object = coxFit,
    Mixed_objects = list(fm_gcs, fm_sofa, fm_cns),
    time_var = "t",
    id_var = "hadm_id",
    n_chains = n_chains_use,
    n_iter = n_iter_use,
    n_burnin = n_burnin_use
  ),
  error = function(e) {
    stop("jm() 拟合失败: ", conditionMessage(e), call. = FALSE)
  }
)
cat(
  "[jm] 完成 |",
  round(as.numeric(difftime(Sys.time(), t_jm, units = "mins")), 2),
  "分钟\n"
)
flush.console()
jm_rds <- file.path(out_dir, "0827_JM_fit.rds")
saveRDS(jmFit, jm_rds)
cat("已保存拟合对象: ", normalizePath(jm_rds), "\n", sep = "")
flush.console()

# ---------------------------------------------------------------------------
# C-index + Score（28-day AUC / IPCW-Brier / IBS）
# ---------------------------------------------------------------------------
cat("[predict] 训练集 event-process...\n")
flush.console()
train_pred <- pred_event_last(jmFit, train_long, train_surv)
cat("[predict] 验证集 event-process...\n")
flush.console()
test_pred <- pred_event_last(jmFit, test_long, test_surv)

train_cindex <- as.numeric(
  survival::concordance(
    Surv(obs_time, event) ~ risk,
    reverse = TRUE,
    data = train_pred
  )$concordance
)
test_cindex <- as.numeric(
  survival::concordance(
    Surv(obs_time, event) ~ risk,
    reverse = TRUE,
    data = test_pred
  )$concordance
)

cat("计算训练集/测试集 Score（AUC / IPCW-Brier / IBS）...\n")
flush.console()
train_score_vals <- score_ipcw(
  train_pred$probability, train_pred$obs_time, train_pred$event, "训练集"
)
test_score_vals <- score_ipcw(
  test_pred$probability, test_pred$obs_time, test_pred$event, "测试集"
)

cat(sprintf("1 C-index | 训练=%.4f | 测试=%.4f\n", train_cindex, test_cindex))
cat(sprintf("2 28-day AUC | 训练=%.4f | 测试=%.4f\n", train_score_vals$auc, test_score_vals$auc))
cat(sprintf(
  "3 28-day Brier (IPCW) | 训练=%.4f | 测试=%.4f\n",
  train_score_vals$brier, test_score_vals$brier
))
cat(sprintf(
  "4 IBS 0-28 days (IPCW) | 训练=%.4f | 测试=%.4f\n",
  train_score_vals$ibs, test_score_vals$ibs
))
flush.console()

metrics <- data.frame(
  dataset = c("Train", "Test"),
  n = c(nrow(train_surv), nrow(test_surv)),
  events = c(sum(train_surv$event), sum(test_surv$event)),
  cindex = round(c(train_cindex, test_cindex), 4),
  auc_28 = round(c(train_score_vals$auc, test_score_vals$auc), 4),
  brier_28_ipcw = round(c(train_score_vals$brier, test_score_vals$brier), 4),
  ibs_0_28_ipcw = round(c(train_score_vals$ibs, test_score_vals$ibs), 4),
  stringsAsFactors = FALSE
)
performance_table <- data.frame(
  metric = c(
    "C-index",
    "28-day AUC",
    "28-day Brier (IPCW)",
    "IBS 0-28 days (IPCW)"
  ),
  training_set = round(
    c(train_cindex, train_score_vals$auc, train_score_vals$brier, train_score_vals$ibs),
    4
  ),
  test_set = round(
    c(test_cindex, test_score_vals$auc, test_score_vals$brier, test_score_vals$ibs),
    4
  ),
  check.names = FALSE,
  stringsAsFactors = FALSE
)
print(performance_table, row.names = FALSE)

out_wide <- file.path(out_dir, "0827_JM_cindex.csv")
out_perf <- file.path(out_dir, "0827_JM_performance.csv")
write.csv(metrics, out_wide, row.names = FALSE, fileEncoding = "UTF-8")
write.csv(performance_table, out_perf, row.names = FALSE, fileEncoding = "UTF-8")
cat("已写入: ", normalizePath(out_wide), "\n", sep = "")
cat("已写入: ", normalizePath(out_perf), "\n", sep = "")

############################
# 第 3–6 节：纵向固定效应 / 生存子模型 / 关联参数 α / 随机效应
# 对齐《JM模型总体结果报告》；数值来自当前 JMbayes2::jm 后验
# ---------------------------------------------------------------------------
cat("\n========== 3–6 节：联合模型参数（后验） ==========\n")
flush.console()

flatten_jm_stat <- function(x) {
  if (is.null(x)) {
    return(NULL)
  }
  if (is.numeric(x) && !is.null(names(x))) {
    return(x)
  }
  if (is.matrix(x)) {
    rn <- rownames(x)
    if (is.null(rn)) rn <- as.character(seq_len(nrow(x)))
    return(stats::setNames(as.numeric(x[, 1]), rn))
  }
  if (is.list(x)) {
    return(unlist(x, use.names = TRUE, recursive = TRUE))
  }
  NULL
}

align_stat <- function(v, nms) {
  if (is.null(v) || length(nms) == 0L) {
    return(rep(NA_real_, length(nms)))
  }
  out <- unname(v[nms])
  if (length(out) != length(nms)) {
    out <- rep(NA_real_, length(nms))
    hit <- intersect(nms, names(v))
    out[match(hit, nms)] <- unname(v[hit])
  }
  as.numeric(out)
}

fmt_num <- function(x, digits = 4) {
  ifelse(is.finite(x), sprintf(paste0("%.", digits, "f"), x), NA_character_)
}

fmt_p <- function(x) {
  ifelse(
    !is.finite(x),
    NA_character_,
    ifelse(x < 0.001, "<0.001", sprintf("%.3f", x))
  )
}

style_jm_ft <- function(x) {
  flextable::flextable(x) %>%
    flextable::bold(part = "header") %>%
    flextable::bg(part = "header", bg = "#D9EAF7") %>%
    flextable::font(fontname = "宋体", part = "all") %>%
    flextable::font(fontname = "Times New Roman", part = "all", cs.family = "宋体") %>%
    flextable::fontsize(size = 9, part = "body") %>%
    flextable::fontsize(size = 10, part = "header") %>%
    flextable::align(align = "center", part = "all") %>%
    flextable::autofit()
}

all_csv <- file.path(out_dir, "03to06_全部后验参数.csv")
jm_rds <- file.path(out_dir, "0827_JM_fit.rds")
has_jmfit <- exists("jmFit") && inherits(jmFit, "jm") &&
  !is.null(jmFit$statistics) && !is.null(jmFit$statistics$Mean)
if (!has_jmfit && file.exists(jm_rds)) {
  cat("从 RDS 读取 jmFit，用于第 7–8 节诊断与拟合指标\n")
  jmFit <- readRDS(jm_rds)
  has_jmfit <- inherits(jmFit, "jm") && !is.null(jmFit$statistics$Mean)
}
if (has_jmfit) {
  print(summary(jmFit))
  jm_stat <- jmFit$statistics
  mean_v <- flatten_jm_stat(jm_stat$Mean)
  sd_v <- flatten_jm_stat(jm_stat$SD)
  lo_v <- flatten_jm_stat(jm_stat$CI_low)
  hi_v <- flatten_jm_stat(jm_stat$CI_upp)
  p_v <- flatten_jm_stat(jm_stat$P)
  rhat_v <- flatten_jm_stat(jm_stat$Rhat)
  ess_v <- flatten_jm_stat(jm_stat$Effective_Size)
  param_names <- names(mean_v)
  if (is.null(param_names) || length(param_names) == 0L) {
    stop("后验均值没有参数名，无法拆分纵向/生存/关联/随机效应")
  }
  jm_all <- data.frame(
    parameter = param_names,
    Mean = as.numeric(mean_v),
    SD = align_stat(sd_v, param_names),
    CI_low = align_stat(lo_v, param_names),
    CI_upp = align_stat(hi_v, param_names),
    P = align_stat(p_v, param_names),
    Rhat = align_stat(rhat_v, param_names),
    ESS = align_stat(ess_v, param_names),
    stringsAsFactors = FALSE
  )
} else {
  if (!file.exists(all_csv)) {
    stop("无 jmFit，且找不到 03to06_全部后验参数.csv，无法导出第 3–6 节")
  }
  cat("未找到 jmFit，从已保存后验 CSV 重拆表 3–6（不重跑 MCMC）\n")
  jm_all <- utils::read.csv(
    all_csv,
    stringsAsFactors = FALSE,
    check.names = FALSE,
    fileEncoding = "UTF-8"
  )
  stopifnot(all(c("parameter", "Mean") %in% names(jm_all)))
  if (!"ESS" %in% names(jm_all)) {
    jm_all$ESS <- NA_real_
  }
  if (!"Rhat" %in% names(jm_all)) {
    jm_all$Rhat <- NA_real_
  }
}

# 个体随机效应 b_i 不进参数表/表7（模板按参数块：alphas、gammas、D、frailty 等）
jm_all <- jm_all[!grepl("^b[0-9]+$", jm_all$parameter) & !grepl("^b\\.[0-9]", jm_all$parameter), , drop = FALSE]

outcome_labels <- traj_vars
label_outcome <- function(idx) {
  i <- suppressWarnings(as.integer(idx))
  if (is.finite(i) && i >= 1L && i <= length(outcome_labels)) {
    return(outcome_labels[[i]])
  }
  paste0("outcome", idx)
}

parse_long_row <- function(nm) {
  m_beta <- regexec("^betas([0-9]+)\\.(.*)$", nm)
  hit <- regmatches(nm, m_beta)[[1]]
  if (length(hit) >= 3L) {
    term <- hit[3]
    if (!nzchar(term)) term <- "(Intercept)"
    return(c(outcome = label_outcome(hit[2]), term = term))
  }
  c(outcome = NA_character_, term = nm)
}

parse_alpha_row <- function(nm) {
  m <- regexec("^alphas\\.value\\(([^)]+)\\)$", nm)
  hit <- regmatches(nm, m)[[1]]
  if (length(hit) >= 2L) {
    return(c(轨迹 = hit[2], 关联 = paste0("当前值：", hit[2])))
  }
  c(轨迹 = NA_character_, 关联 = nm)
}

parse_re_row <- function(nm) {
  m_d <- regexec("^D\\.D\\[\\s*([0-9]+)\\s*,\\s*([0-9]+)\\s*\\]$", nm)
  hit <- regmatches(nm, m_d)[[1]]
  if (length(hit) >= 3L) {
    i <- as.integer(hit[2])
    j <- as.integer(hit[3])
    oi <- label_outcome(i)
    oj <- label_outcome(j)
    if (identical(i, j)) {
      return(c(
        类型 = "随机截距方差",
        结局 = oi,
        参数 = paste0("D[", i, ",", i, "]")
      ))
    }
    return(c(
      类型 = "随机截距协方差",
      结局 = paste0(oi, " 与 ", oj),
      参数 = paste0("D[", i, ",", j, "]")
    ))
  }
  m_s <- regexec("^sigmas\\.sigmas_([0-9]+)$", nm)
  hit_s <- regmatches(nm, m_s)[[1]]
  if (length(hit_s) >= 2L) {
    k <- as.integer(hit_s[2])
    return(c(
      类型 = "残差标准差",
      结局 = label_outcome(k),
      参数 = paste0("sigma_", k)
    ))
  }
  c(类型 = NA_character_, 结局 = NA_character_, 参数 = nm)
}

dir_prob <- function(p) {
  ifelse(is.finite(p), 1 - as.numeric(p) / 2, NA_real_)
}

make_hr_block <- function(raw, param_lab) {
  if (nrow(raw) == 0L) {
    return(data.frame(
      parameter = character(0),
      posterior_mean = numeric(0),
      posterior_sd = numeric(0),
      credible_interval_low = numeric(0),
      credible_interval_high = numeric(0),
      posterior_tail_probability = numeric(0),
      posterior_direction_probability = numeric(0),
      Rhat = numeric(0),
      hazard_ratio = numeric(0),
      hazard_ratio_low = numeric(0),
      hazard_ratio_high = numeric(0),
      stringsAsFactors = FALSE
    ))
  }
  data.frame(
    parameter = param_lab,
    posterior_mean = raw$Mean,
    posterior_sd = raw$SD,
    credible_interval_low = raw$CI_low,
    credible_interval_high = raw$CI_upp,
    posterior_tail_probability = raw$P,
    posterior_direction_probability = dir_prob(raw$P),
    Rhat = raw$Rhat,
    hazard_ratio = exp(raw$Mean),
    hazard_ratio_low = exp(raw$CI_low),
    hazard_ratio_high = exp(raw$CI_upp),
    stringsAsFactors = FALSE
  )
}

fmt_eng_ft <- function(df, digits = 4) {
  out <- df
  for (cc in names(out)) {
    if (is.numeric(df[[cc]])) {
      out[[cc]] <- fmt_num(df[[cc]], digits)
    }
  }
  out
}

add_jm_table <- function(doc, title, df, note) {
  doc <- officer::body_add_par(doc, title, style = "heading 1")
  if (!is.data.frame(df) || nrow(df) == 0L) {
    return(officer::body_add_par(doc, "无数据。", style = "Normal"))
  }
  ft <- style_jm_ft(df)
  if (!is.null(note) && nzchar(note)) {
    ft <- ft %>%
      flextable::add_footer_lines(note) %>%
      flextable::fontsize(size = 8, part = "footer") %>%
      flextable::italic(part = "footer")
  }
  flextable::body_add_flextable(doc, ft)
}

jm_component <- function(nm) {
  nm <- as.character(nm)
  if (grepl("^D(\\.|$)", nm) || grepl("^D\\.D\\[", nm)) {
    return("D")
  }
  if (grepl("^frailty", nm, ignore.case = TRUE)) {
    return("frailty")
  }
  if (grepl("^alphas(\\.|$)", nm)) {
    return("alphas")
  }
  if (!grepl("\\.", nm)) {
    return(nm)
  }
  sub("\\..*$", "", nm)
}

make_mcmc_comp <- function(all_df) {
  if (!is.data.frame(all_df) || nrow(all_df) == 0L) {
    return(data.frame(
      component = character(0),
      parameter_count = integer(0),
      maximum_Rhat = numeric(0),
      Rhat_above_1.01 = numeric(0),
      Rhat_above_1.05 = numeric(0),
      stringsAsFactors = FALSE
    ))
  }
  rhat_col <- if ("Rhat" %in% names(all_df)) all_df$Rhat else rep(NA_real_, nrow(all_df))
  comp_id <- vapply(all_df$parameter, jm_component, character(1))
  do.call(rbind, lapply(sort(unique(comp_id), method = "radix"), function(cc) {
    hit <- which(comp_id == cc)
    rh <- as.numeric(rhat_col[hit])
    ok <- is.finite(rh)
    data.frame(
      component = cc,
      parameter_count = length(hit),
      maximum_Rhat = if (any(ok)) max(rh[ok]) else NA_real_,
      Rhat_above_1.01 = if (any(ok)) as.numeric(sum(rh[ok] > 1.01)) else NA_real_,
      Rhat_above_1.05 = if (any(ok)) as.numeric(sum(rh[ok] > 1.05)) else NA_real_,
      stringsAsFactors = FALSE
    )
  }))
}

make_fit_stats <- function(fit, csv_path) {
  empty <- data.frame(
    likelihood_type = character(0),
    DIC = numeric(0),
    effective_parameters_pD = numeric(0),
    WAIC = numeric(0),
    LPML = numeric(0),
    stringsAsFactors = FALSE
  )
  rows <- list()
  if (!is.null(fit) && !is.null(fit$fit_stats)) {
    for (typ in c("conditional", "marginal")) {
      x <- fit$fit_stats[[typ]]
      if (is.null(x)) {
        next
      }
      rows[[typ]] <- data.frame(
        likelihood_type = typ,
        DIC = as.numeric(x$DIC)[1],
        effective_parameters_pD = as.numeric(x$pD)[1],
        WAIC = as.numeric(x$WAIC)[1],
        LPML = as.numeric(x$LPML)[1],
        stringsAsFactors = FALSE
      )
    }
  }
  if (length(rows)) {
    out <- do.call(rbind, rows)
    rownames(out) <- NULL
    return(out)
  }
  if (file.exists(csv_path)) {
    tmp <- utils::read.csv(
      csv_path, stringsAsFactors = FALSE, check.names = FALSE, fileEncoding = "UTF-8"
    )
    if (all(c("likelihood_type", "DIC") %in% names(tmp))) {
      want <- c("likelihood_type", "DIC", "effective_parameters_pD", "WAIC", "LPML")
      for (cc in want) {
        if (!cc %in% names(tmp)) tmp[[cc]] <- NA
      }
      ord <- match(c("conditional", "marginal"), tmp$likelihood_type)
      ord <- ord[is.finite(ord)]
      if (length(ord)) tmp <- tmp[ord, want, drop = FALSE]
      return(tmp)
    }
    if ("类型" %in% names(tmp) && "DIC" %in% names(tmp)) {
      typ <- ifelse(
        tmp[["类型"]] %in% c("条件", "conditional"),
        "conditional",
        ifelse(
          tmp[["类型"]] %in% c("边际", "marginal"),
          "marginal",
          as.character(tmp[["类型"]])
        )
      )
      pD <- if ("effective_parameters_pD" %in% names(tmp)) {
        tmp$effective_parameters_pD
      } else {
        tmp$pD
      }
      out <- data.frame(
        likelihood_type = typ,
        DIC = as.numeric(tmp$DIC),
        effective_parameters_pD = as.numeric(pD),
        WAIC = as.numeric(tmp$WAIC),
        LPML = as.numeric(tmp$LPML),
        stringsAsFactors = FALSE
      )
      ord <- match(c("conditional", "marginal"), out$likelihood_type)
      ord <- ord[is.finite(ord)]
      if (length(ord)) out <- out[ord, , drop = FALSE]
      return(out)
    }
  }
  empty
}

fmt_mcmc_ft <- function(df) {
  if (nrow(df) == 0L) {
    return(df)
  }
  out <- df
  out$parameter_count <- as.character(df$parameter_count)
  out$maximum_Rhat <- fmt_num(df$maximum_Rhat, 4)
  out$Rhat_above_1.01 <- ifelse(
    is.finite(df$Rhat_above_1.01), as.character(as.integer(df$Rhat_above_1.01)), NA_character_
  )
  out$Rhat_above_1.05 <- ifelse(
    is.finite(df$Rhat_above_1.05), as.character(as.integer(df$Rhat_above_1.05)), NA_character_
  )
  out
}

# 表3：纵向固定效应 betas1/2/3
# 表4：仅 Cox 回归 γ（gammas.*）；不含基线风险 B 样条
# 表5：仅 current-value 关联 alphas.value(*)
# 表6：每条轨迹一行，随机截距标准差；当前模型无随机斜率
is_betas <- grepl("^betas[0-9]+\\.", jm_all$parameter)
is_gamma <- grepl("^gammas\\.", jm_all$parameter)
is_alpha <- grepl("^alphas\\.value\\(", jm_all$parameter)
is_dmat <- grepl("^D\\.D\\[", jm_all$parameter)
is_sigma <- grepl("^sigmas\\.sigmas_[0-9]+$", jm_all$parameter)

long_raw <- jm_all[is_betas, , drop = FALSE]
if (nrow(long_raw) == 0L) {
  warning("未从后验中匹配到纵向 betas，第 3 节表为空")
}
long_parsed <- if (nrow(long_raw) > 0L) {
  t(vapply(long_raw$parameter, parse_long_row, character(2)))
} else {
  matrix(character(0), ncol = 2, dimnames = list(NULL, c("outcome", "term")))
}
long_table_out <- data.frame(
  结局 = if (nrow(long_raw)) as.character(long_parsed[, 1]) else character(0),
  参数 = if (nrow(long_raw)) as.character(long_parsed[, 2]) else character(0),
  后验均值 = long_raw$Mean,
  标准差 = long_raw$SD,
  `2.5%` = long_raw$CI_low,
  `97.5%` = long_raw$CI_upp,
  P = long_raw$P,
  Rhat = long_raw$Rhat,
  原始参数名 = long_raw$parameter,
  stringsAsFactors = FALSE,
  check.names = FALSE
)

surv_raw <- jm_all[is_gamma, , drop = FALSE]
surv_table_out <- make_hr_block(surv_raw, sub("^gammas\\.", "", surv_raw$parameter))

alpha_raw <- jm_all[is_alpha, , drop = FALSE]
alpha_table_out <- make_hr_block(alpha_raw, sub("^alphas\\.", "", alpha_raw$parameter))

re_table_out <- do.call(rbind, lapply(seq_along(traj_vars), function(k) {
  pat <- sprintf("^D\\.D\\[\\s*%d\\s*,\\s*%d\\s*\\]$", k, k)
  hit <- jm_all[grepl(pat, jm_all$parameter), , drop = FALSE]
  sd_i <- if (nrow(hit) > 0L && is.finite(hit$Mean[[1]])) {
    sqrt(as.numeric(hit$Mean[[1]]))
  } else {
    NA_real_
  }
  data.frame(
    longitudinal_outcome = traj_vars[[k]],
    random_intercept_sd = sd_i,
    random_slope_sd = NA_real_,
    intercept_slope_correlation = NA_real_,
    stringsAsFactors = FALSE
  )
}))
rownames(re_table_out) <- NULL

write.csv(long_table_out, file.path(out_dir, "03_纵向固定效应.csv"), row.names = FALSE, fileEncoding = "UTF-8")
write.csv(surv_table_out, file.path(out_dir, "04_生存子模型.csv"), row.names = FALSE, fileEncoding = "UTF-8")
write.csv(alpha_table_out, file.path(out_dir, "05_关联参数.csv"), row.names = FALSE, fileEncoding = "UTF-8")
write.csv(re_table_out, file.path(out_dir, "06_随机效应.csv"), row.names = FALSE, fileEncoding = "UTF-8")
write.csv(jm_all, file.path(out_dir, "03to06_全部后验参数.csv"), row.names = FALSE, fileEncoding = "UTF-8")

cat("\n--- 3. 纵向子模型参数 ---\n")
print(long_table_out, row.names = FALSE)
cat("\n--- 4. 生存子模型参数 ---\n")
print(surv_table_out, row.names = FALSE)
cat("\n--- 5. 纵向-生存关联参数 ---\n")
print(alpha_table_out, row.names = FALSE)
cat("\n--- 6. 随机效应 ---\n")
print(re_table_out, row.names = FALSE)
flush.console()

long_ft <- long_table_out
if (nrow(long_ft) > 0L) {
  long_ft$后验均值 <- fmt_num(long_table_out$后验均值)
  long_ft$标准差 <- fmt_num(long_table_out$标准差)
  long_ft$`2.5%` <- fmt_num(long_table_out$`2.5%`)
  long_ft$`97.5%` <- fmt_num(long_table_out$`97.5%`)
  long_ft$P <- fmt_p(long_table_out$P)
  long_ft$Rhat <- fmt_num(long_table_out$Rhat, 3)
  long_ft$原始参数名 <- NULL
}

surv_ft <- fmt_eng_ft(surv_table_out)
alpha_ft <- fmt_eng_ft(alpha_table_out)
re_ft <- fmt_eng_ft(re_table_out)

rhat_note <- if (exists("n_chains_use") && is.finite(n_chains_use) && n_chains_use < 2L) {
  "当前为单链 MCMC，Gelman Rhat 需至少 2 条链，故表中 Rhat 为空。"
} else {
  "Rhat 为 Gelman–Rubin 收敛诊断。"
}

# ---------------------------------------------------------------------------
# 第 7–8 节：MCMC 按参数块汇总 / 拟合指标（Word 不含预测性能）
# ---------------------------------------------------------------------------
mcmc_comp_out <- make_mcmc_comp(jm_all)
fit_obj <- if (isTRUE(has_jmfit) && exists("jmFit")) jmFit else NULL
fit_stats_out <- make_fit_stats(fit_obj, file.path(out_dir, "08_拟合指标.csv"))

write.csv(mcmc_comp_out, file.path(out_dir, "07_MCMC诊断.csv"), row.names = FALSE, fileEncoding = "UTF-8")
if (nrow(fit_stats_out) > 0L) {
  write.csv(fit_stats_out, file.path(out_dir, "08_拟合指标.csv"), row.names = FALSE, fileEncoding = "UTF-8")
}

cat("\n--- 7. MCMC收敛诊断 ---\n")
print(mcmc_comp_out, row.names = FALSE)
cat("\n--- 8. 模型拟合指标 ---\n")
if (nrow(fit_stats_out) > 0L) {
  print(fit_stats_out, row.names = FALSE)
} else {
  cat("无 DIC/WAIC/LPML（需要 jmFit 或 08_拟合指标.csv）\n")
}
flush.console()

mcmc_ft <- fmt_mcmc_ft(mcmc_comp_out)
fit_stats_ft <- fmt_eng_ft(fit_stats_out, digits = 2)

note4 <- "解释：风险比及其95%可信区间由生存系数指数转换得到，风险比大于1表示死亡风险升高。"
note5 <- "解释：关联风险比表示纵向指标潜在真实水平增加1个原始计量单位时，死亡瞬时风险的相对变化。"
note6 <- "解释：随机截距和随机斜率标准差刻画患者间异质性，相关系数刻画同一结局基线水平与变化速度的关系。"
note7 <- "解释：Rhat越接近1越好，所有参数Rhat不超过1.01较理想；超过1.05时应增加迭代并检查轨迹图。各主要参数表中同时给出了逐参数Rhat。"
note8 <- "解释：比较相同数据上的候选模型时，DIC和WAIC越小越好，LPML越大越好。条件指标和边际指标应分别解释。"

jm_doc <- officer::read_docx()
jm_doc <- officer::body_add_par(jm_doc, "JM模型总体结果（第3–8节）", style = "graphic title")
jm_doc <- officer::body_add_par(
  jm_doc,
  paste0(
    "基于当前试跑 JMbayes2 联合模型（轨迹：",
    paste(traj_vars, collapse = "、"),
    "；生存子模型为 8 个临床基线 + 17 个第 1 日截面，已剔除训练集无变异协变量）。",
    "表内为后验均值、标准差与 95% 可信区间。",
    rhat_note
  ),
  style = "Normal"
)

jm_doc <- officer::body_add_par(jm_doc, "3. 纵向子模型参数", style = "heading 1")
if (nrow(long_ft) > 0L) {
  jm_doc <- flextable::body_add_flextable(
    jm_doc,
    style_jm_ft(long_ft) %>%
      flextable::add_footer_lines("注：每个纵向结局为 Y ~ t，随机截距 | hadm_id。参数为贝叶斯后验，不是频率派标准误。") %>%
      flextable::fontsize(size = 8, part = "footer") %>%
      flextable::italic(part = "footer")
  )
} else {
  jm_doc <- officer::body_add_par(jm_doc, "未提取到纵向固定效应。", style = "Normal")
}

jm_doc <- add_jm_table(jm_doc, "4. 生存子模型参数", surv_ft, note4)
jm_doc <- add_jm_table(jm_doc, "5. 纵向-生存关联参数", alpha_ft, note5)
jm_doc <- add_jm_table(jm_doc, "6. 随机效应", re_ft, note6)

jm_doc <- officer::body_add_break(jm_doc)
jm_doc <- add_jm_table(jm_doc, "7. MCMC收敛诊断", mcmc_ft, note7)

jm_doc <- officer::body_add_break(jm_doc)
if (nrow(fit_stats_ft) > 0L) {
  jm_doc <- add_jm_table(jm_doc, "8. 模型拟合指标", fit_stats_ft, note8)
} else {
  jm_doc <- officer::body_add_par(jm_doc, "8. 模型拟合指标", style = "heading 1")
  jm_doc <- officer::body_add_par(
    jm_doc,
    "当前会话没有 jmFit，无法计算 DIC/WAIC/LPML。完整运行 0827_JM.R 拟合后会写入本表。",
    style = "Normal"
  )
}

out_docx <- file.path(out_dir, "JM模型总体结果_第3至8节.docx")
print(jm_doc, target = out_docx)
out_docx_alt <- file.path(out_dir, "JM模型总体结果_第3至6节.docx")
tryCatch(
  print(jm_doc, target = out_docx_alt),
  error = function(e) {
    cat("未能覆盖旧 Word（可能正被打开）：", conditionMessage(e), "\n")
  }
)
cat("已写入第 3–8 节 CSV 与 Word：\n", normalizePath(out_dir), "\n", sep = "")
cat("Word：", normalizePath(out_docx, mustWork = FALSE), "\n", sep = "")


