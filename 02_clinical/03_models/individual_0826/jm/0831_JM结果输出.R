# =============================================================================
# JM 全量拟合：3 条纵向轨迹 + 17 个第 1 日截面 + 8 个临床基线
# 轨迹：gcs, sofa_24hours, cns_24hours
# 拟合对齐试跑 0827_JM.R / 老师报告（JMbayes2::jm，随机截距+随机斜率 ~ t | hadm_id）
# group=1 全部训练，group=2 全部外验证；fold 不参与
# 正式 MCMC：4 链，每链 12000 次，burn-in 3000；启用 MALA 改善 D 块采样
# C-index / AUC / Brier：landmark=5（仅 t=5 仍存活者，用 0–5 天轨迹预测 28 天风险）
# Word：按《JM模型总体结果报告》第1–10节，只写 执行/图像_JM
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

if (identical(Sys.getenv("JM_PARSE_ONLY", unset = "0"), "1")) {
  cat("PARSE_OK\n")
  quit(save = "no", status = 0)
}

set.seed(2026L)

base_dir <- Sys.getenv(
  "JM_BASE_DIR",
  unset = "F:/文章_大论文/0722/实例研究代码"
)
exec_dir <- Sys.getenv("JM_EXEC_DIR", unset = file.path(base_dir, "执行"))
dir.create(exec_dir, showWarnings = FALSE, recursive = TRUE)
out_dir <- Sys.getenv("JM_OUT_DIR", unset = file.path(exec_dir, "图像_JM"))
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
baseline_path <- file.path(base_dir, "stroke_baseline_knn_0824_fold.csv")
longitudinal_path <- file.path(base_dir, "stroke_longitudinal_knn_0824_group_fold.csv")

if (identical(Sys.getenv("JM_PATH_TEST", unset = "0"), "1")) {
  path_test_rds <- file.path(out_dir, "JM_path_test.rds")
  path_test_csv <- file.path(out_dir, "JM_path_test.csv")
  saveRDS(list(ok = TRUE, time = Sys.time()), path_test_rds)
  write.csv(data.frame(ok = TRUE), path_test_csv, row.names = FALSE)
  stopifnot(file.exists(path_test_rds), file.exists(path_test_csv))
  unlink(c(path_test_rds, path_test_csv))
  cat("PATH_WRITE_OK\n")
  quit(save = "no", status = 0)
}

# 4 条链用于正式收敛诊断；Windows 上 JMbayes2 用 PSOCK 并行
n_chains_use <- 4L
n_iter_use <- 12000L
n_burnin_use <- 3000L
n_thin_use <- 1L
use_mala <- TRUE
force_refit <- !identical(Sys.getenv("JM_FORCE_REFIT", unset = "1"), "0")
n_cores_use <- min(n_chains_use, max(1L, parallel::detectCores(logical = TRUE) - 1L))

landmark_t <- 5
evaluation_horizon <- 28 - 1e-04
evaluation_times <- unique(c(seq(landmark_t, 27.5, by = 0.5), evaluation_horizon))

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

pred_landmark <- function(fit, dat, surv, landmark, horizon) {
  surv_use <- surv
  surv_use$hadm_id <- as.integer(surv_use$hadm_id)
  dat_use <- dat
  dat_use$hadm_id <- as.integer(dat_use$hadm_id)
  at_risk <- unique(surv_use$hadm_id[is.finite(surv_use$obs_time) & surv_use$obs_time > landmark])
  nd <- dat_use[dat_use$hadm_id %in% at_risk & dat_use$t <= landmark + 1e-08, , drop = FALSE]
  keep <- sort(unique(nd$hadm_id))
  if (length(keep) == 0L) {
    stop("landmark=", landmark, " 后无可用患者（obs_time > landmark 且有 t<=landmark 的纵向）")
  }
  surv_lm <- surv_use[match(keep, surv_use$hadm_id), , drop = FALSE]
  nd <- nd[order(nd$hadm_id, nd$t), , drop = FALSE]
  rownames(nd) <- NULL
  pred <- tryCatch(
    predict(
      fit,
      newdata = nd,
      process = "event",
      times = horizon,
      type = "subject_specific"
    ),
    error = function(e) {
      cat("predict(times, subject_specific) 失败，尝试降级：", conditionMessage(e), "\n")
      flush.console()
      tryCatch(
        predict(fit, newdata = nd, process = "event", times = horizon),
        error = function(e2) {
          predict(fit, newdata = nd, process = "event")
        }
      )
    }
  )
  pdf <- if (is.data.frame(pred)) {
    pred
  } else if (is.list(pred) && is.data.frame(pred[[1]])) {
    pred[[1]]
  } else {
    as.data.frame(pred)
  }
  nms <- names(pdf)
  id_col <- nms[tolower(nms) %in% c("id", "hadm_id")][1]
  time_col <- nms[tolower(nms) %in% c("times", "time")][1]
  risk_col <- nms[tolower(nms) %in% c("pred", "preds", "pred_mean")][1]
  if (is.na(id_col) || is.na(risk_col)) {
    stop("无法解析 predict 输出列: ", paste(nms, collapse = ", "))
  }
  pdf$hadm_id <- as.integer(as.character(pdf[[id_col]]))
  pdf$risk_raw <- as.numeric(pdf[[risk_col]])
  if (!is.na(time_col)) {
    pdf$time <- as.numeric(pdf[[time_col]])
    risk_last <- pdf %>%
      dplyr::group_by(hadm_id) %>%
      dplyr::summarise(
        risk = {
                  hit <- which.min(abs(time - horizon))
                  risk_raw[[hit]]
        },
        .groups = "drop"
      )
  } else {
    risk_last <- pdf %>%
      dplyr::group_by(hadm_id) %>%
      dplyr::summarise(risk = dplyr::last(.data$risk_raw), .groups = "drop")
  }
  cdata <- merge(surv_lm, risk_last, by = "hadm_id", sort = FALSE)
  cdata <- cdata[match(surv_lm$hadm_id, cdata$hadm_id), , drop = FALSE]
  if (nrow(cdata) == 0L || any(!is.finite(cdata$risk))) {
    stop("landmark 预测风险与生存表未能对齐，或存在非有限 risk")
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

score_ipcw <- function(probability, time, status, label, times_use = evaluation_times) {
  score_data <- data.frame(
    time28 = as.numeric(time),
    status28 = as.integer(status),
    stringsAsFactors = FALSE
  )
  risk_mat <- build_constant_risk_matrix(probability, times_use)
  score_obj <- tryCatch(
    riskRegression::Score(
      object = list(JM = risk_mat),
      formula = Hist(time28, status28) ~ 1,
      data = score_data,
      metrics = c("auc", "brier"),
      times = times_use,
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

train_ids <- sort(unique(pool_train$hadm_id))
test_ids <- sort(unique(pool_test$hadm_id))

train_long <- long_df[long_df$hadm_id %in% train_ids, , drop = FALSE]
test_long <- long_df[long_df$hadm_id %in% test_ids, , drop = FALSE]
train_surv <- train_long[!duplicated(train_long$hadm_id), , drop = FALSE]
test_surv <- test_long[!duplicated(test_long$hadm_id), , drop = FALSE]
rownames(train_long) <- NULL
rownames(test_long) <- NULL
rownames(train_surv) <- NULL
rownames(test_surv) <- NULL

cat(
  "训练集 group=1 全部 n =", nrow(train_surv),
  "| 死亡 =", sum(train_surv$event),
  sprintf("（%.1f%%）", 100 * mean(train_surv$event)),
  "| 纵向行 =", nrow(train_long), "\n"
)
cat(
  "验证集 group=2 全部 n =", nrow(test_surv),
  "| 死亡 =", sum(test_surv$event),
  sprintf("（%.1f%%）", 100 * mean(test_surv$event)),
  "| 纵向行 =", nrow(test_long), "\n"
)
flush.console()

# ---------------------------------------------------------------------------
# 纵向 lme + Cox + jm（本正式脚本强制重拟合；成功后才原子式覆盖旧 RDS）
# ---------------------------------------------------------------------------
jm_rds <- file.path(out_dir, "0827_JM_fit.rds")
if (file.exists(jm_rds) && !isTRUE(force_refit)) {
  cat("发现已有拟合对象，跳过 lme/Cox/jm：", jm_rds, "\n")
  flush.console()
  jmFit <- readRDS(jm_rds)
  if (!inherits(jmFit, "jm")) {
    stop("RDS 不是 JMbayes2::jm 对象，请删除后重跑拟合: ", jm_rds)
  }
} else {
  lme_ctrl <- nlme::lmeControl(opt = "optim", msMaxIter = 200)
  cat("[lme] 开始拟合 3 条轨迹...\n")
  flush.console()
  t_lme <- Sys.time()
  fm_gcs <- nlme::lme(
    gcs ~ t,
    data = train_long,
    random = ~ t | hadm_id,
    control = lme_ctrl
  )
  fm_sofa <- nlme::lme(
    sofa_24hours ~ t,
    data = train_long,
    random = ~ t | hadm_id,
    control = lme_ctrl
  )
  fm_cns <- nlme::lme(
    cns_24hours ~ t,
    data = train_long,
    random = ~ t | hadm_id,
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
    "| n_burnin =", n_burnin_use,
    "| cores =", n_cores_use, "\n"
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
      n_burnin = n_burnin_use,
      n_thin = n_thin_use,
      MALA = use_mala,
      cores = n_cores_use,
      seed = 2026L
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
  jm_rds_new <- paste0(jm_rds, ".new")
  saveRDS(jmFit, jm_rds_new)
  copied_ok <- file.copy(jm_rds_new, jm_rds, overwrite = TRUE)
  if (!isTRUE(copied_ok)) {
    stop("新拟合已完成，但覆盖旧 RDS 失败；新对象保留在: ", jm_rds_new)
  }
  unlink(jm_rds_new)
  cat("已保存拟合对象: ", normalizePath(jm_rds), "\n", sep = "")
  flush.console()
}

# 每次正式拟合均覆盖 MCMC 设置与轨迹图，避免保留旧的 1500 次结果。
mcmc_settings <- data.frame(
  项目 = c(
    "链数 n_chains", "总迭代 n_iter", "预烧 n_burnin", "稀释 n_thin",
    "MALA", "并行核数 cores", "每链保留样本", "合计后验样本"
  ),
  取值 = c(
    n_chains_use, n_iter_use, n_burnin_use, n_thin_use,
    use_mala, n_cores_use,
    (n_iter_use - n_burnin_use) / n_thin_use,
    n_chains_use * (n_iter_use - n_burnin_use) / n_thin_use
  ),
  check.names = FALSE,
  stringsAsFactors = FALSE
)
write.csv(
  mcmc_settings, file.path(out_dir, "07_MCMC设置.csv"),
  row.names = FALSE, fileEncoding = "UTF-8"
)

png(file.path(out_dir, "07_trace_alphas.png"), width = 1800, height = 1200, res = 180)
JMbayes2::traceplot(jmFit, parm = "alphas")
dev.off()
png(file.path(out_dir, "07_trace_betas.png"), width = 1800, height = 1600, res = 180)
JMbayes2::traceplot(jmFit, parm = "betas")
dev.off()
png(file.path(out_dir, "07_trace_D_sigma.png"), width = 2000, height = 2000, res = 180)
JMbayes2::traceplot(jmFit, parm = "D")
dev.off()
png(file.path(out_dir, "07_trace_sigmas.png"), width = 1800, height = 1200, res = 180)
JMbayes2::traceplot(jmFit, parm = "sigmas")
dev.off()

# ---------------------------------------------------------------------------
# C-index + Score：landmark=5，预测 28 天死亡
# ---------------------------------------------------------------------------
prediction_error <- NULL
skip_prediction <- identical(Sys.getenv("JM_SKIP_PREDICTION", unset = "0"), "1")
if (skip_prediction) {
  prediction_error <- "已按 JM_SKIP_PREDICTION=1 跳过预测；正式 MCMC 与参数结果照常导出。"
  train_pred <- NULL
} else {
  cat("[predict] landmark=", landmark_t, " 训练集...\n", sep = "")
  flush.console()
  train_pred <- tryCatch(
    pred_landmark(jmFit, train_long, train_surv, landmark_t, evaluation_horizon),
    error = function(e) {
      prediction_error <<- paste("训练集预测失败:", conditionMessage(e))
      NULL
    }
  )
}
test_pred <- NULL
if (!is.null(train_pred)) {
  cat("[predict] landmark=", landmark_t, " 验证集...\n", sep = "")
  flush.console()
  test_pred <- tryCatch(
    pred_landmark(jmFit, test_long, test_surv, landmark_t, evaluation_horizon),
    error = function(e) {
      prediction_error <<- paste("验证集预测失败:", conditionMessage(e))
      NULL
    }
  )
}

out_wide <- file.path(out_dir, "0827_JM_cindex.csv")
out_perf <- file.path(out_dir, "0827_JM_performance.csv")
if (is.null(train_pred) || is.null(test_pred)) {
  warning(prediction_error, "；继续导出 MCMC 与参数结果，预测指标记为 NA。")
  writeLines(
    c(
      "正式 JM 已成功拟合并保存，但旧预测解析器无法处理本次返回对象。",
      prediction_error,
      "MCMC/参数结果有效；预测性能需要单独修订接口后重算。"
    ),
    file.path(out_dir, "0827_JM_prediction_error.txt"),
    useBytes = TRUE
  )
  metrics <- data.frame(
    dataset = c("Train", "Test"), landmark = landmark_t,
    n = NA_integer_, events = NA_integer_, cindex = NA_real_, auc_28 = NA_real_,
    brier_28_ipcw = NA_real_, ibs_0_28_ipcw = NA_real_,
    stringsAsFactors = FALSE
  )
  performance_table <- data.frame(
    metric = c("C-index", "28-day AUC", "28-day Brier (IPCW)", "IBS 0-28 days (IPCW)"),
    training_set = NA_real_, test_set = NA_real_,
    check.names = FALSE, stringsAsFactors = FALSE
  )
} else {
  cat("landmark 训练集 n =", nrow(train_pred), "| 死亡 =", sum(train_pred$event), "\n")
  cat("landmark 验证集 n =", nrow(test_pred), "| 死亡 =", sum(test_pred$event), "\n")
  flush.console()
  train_cindex <- as.numeric(survival::concordance(
    Surv(obs_time, event) ~ risk, reverse = TRUE, data = train_pred
  )$concordance)
  test_cindex <- as.numeric(survival::concordance(
    Surv(obs_time, event) ~ risk, reverse = TRUE, data = test_pred
  )$concordance)
  cat("计算训练集/测试集 Score（AUC / IPCW-Brier / IBS）...\n")
  flush.console()
  train_score_vals <- score_ipcw(
    train_pred$probability, train_pred$obs_time, train_pred$event, "训练集"
  )
  test_score_vals <- score_ipcw(
    test_pred$probability, test_pred$obs_time, test_pred$event, "测试集"
  )
  metrics <- data.frame(
    dataset = c("Train", "Test"), landmark = landmark_t,
    n = c(nrow(train_pred), nrow(test_pred)),
    events = c(sum(train_pred$event), sum(test_pred$event)),
    cindex = round(c(train_cindex, test_cindex), 4),
    auc_28 = round(c(train_score_vals$auc, test_score_vals$auc), 4),
    brier_28_ipcw = round(c(train_score_vals$brier, test_score_vals$brier), 4),
    ibs_0_28_ipcw = round(c(train_score_vals$ibs, test_score_vals$ibs), 4),
    stringsAsFactors = FALSE
  )
  performance_table <- data.frame(
    metric = c("C-index", "28-day AUC", "28-day Brier (IPCW)", "IBS 0-28 days (IPCW)"),
    training_set = round(c(
      train_cindex, train_score_vals$auc, train_score_vals$brier, train_score_vals$ibs
    ), 4),
    test_set = round(c(
      test_cindex, test_score_vals$auc, test_score_vals$brier, test_score_vals$ibs
    ), 4),
    check.names = FALSE, stringsAsFactors = FALSE
  )
}
print(performance_table, row.names = FALSE)
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

# Rhat 在 n_chains>=2 时是各参数一组 2 列矩阵（Point est. / Upper C.I.），
# 不能 recursive unlist，否则名字对不上 Mean。
flatten_rhat <- function(x) {
  if (is.null(x)) {
    return(NULL)
  }
  if (is.matrix(x) || (is.numeric(x) && !is.null(dim(x)))) {
    rn <- rownames(x)
    if (is.null(rn)) rn <- as.character(seq_len(nrow(x)))
    return(stats::setNames(as.numeric(x[, 1]), rn))
  }
  if (is.numeric(x) && !is.null(names(x))) {
    return(x)
  }
  if (is.list(x)) {
    parts <- lapply(x, flatten_rhat)
    parts <- parts[!vapply(parts, is.null, logical(1))]
    if (!length(parts)) {
      return(NULL)
    }
    return(unlist(parts, use.names = TRUE, recursive = TRUE))
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
  rhat_v <- flatten_rhat(jm_stat$Rhat)
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
# 表6：每条轨迹一行；D 中顺序为 (截距, 斜率) × 3 条轨迹
is_betas <- grepl("^betas[0-9]+\\.", jm_all$parameter)
is_gamma <- grepl("^gammas\\.", jm_all$parameter)
is_alpha <- grepl("^alphas\\.value\\(", jm_all$parameter)
is_dmat <- grepl("^D\\.D\\[", jm_all$parameter)
is_sigma <- grepl("^sigmas\\.sigmas_[0-9]+$", jm_all$parameter)

long_rows <- list()
for (k in seq_along(traj_vars)) {
  oc <- traj_vars[[k]]
  bpat <- sprintf("^betas%d\\.", k)
  br <- jm_all[grepl(bpat, jm_all$parameter), , drop = FALSE]
  if (nrow(br) > 0L) {
    parsed <- t(vapply(br$parameter, parse_long_row, character(2)))
    long_rows[[length(long_rows) + 1L]] <- data.frame(
      longitudinal_outcome = oc,
      parameter = as.character(parsed[, 2]),
      posterior_mean = br$Mean,
      posterior_sd = br$SD,
      credible_interval_low = br$CI_low,
      credible_interval_high = br$CI_upp,
      posterior_tail_probability = br$P,
      posterior_direction_probability = dir_prob(br$P),
      Rhat = br$Rhat,
      stringsAsFactors = FALSE
    )
  }
  spat <- sprintf("^sigmas\\.sigmas_%d$", k)
  sr <- jm_all[grepl(spat, jm_all$parameter), , drop = FALSE]
  if (nrow(sr) > 0L) {
    long_rows[[length(long_rows) + 1L]] <- data.frame(
      longitudinal_outcome = oc,
      parameter = "sigma",
      posterior_mean = sr$Mean[[1]],
      posterior_sd = sr$SD[[1]],
      credible_interval_low = sr$CI_low[[1]],
      credible_interval_high = sr$CI_upp[[1]],
      posterior_tail_probability = sr$P[[1]],
      posterior_direction_probability = dir_prob(sr$P[[1]]),
      Rhat = sr$Rhat[[1]],
      stringsAsFactors = FALSE
    )
  }
}
long_table_out <- if (length(long_rows)) {
  out <- do.call(rbind, long_rows)
  rownames(out) <- NULL
  out
} else {
  data.frame(
    longitudinal_outcome = character(0),
    parameter = character(0),
    posterior_mean = numeric(0),
    posterior_sd = numeric(0),
    credible_interval_low = numeric(0),
    credible_interval_high = numeric(0),
    posterior_tail_probability = numeric(0),
    posterior_direction_probability = numeric(0),
    Rhat = numeric(0),
    stringsAsFactors = FALSE
  )
}
if (nrow(long_table_out) == 0L) {
  warning("未从后验中匹配到纵向 betas/sigma，第 3 节表为空")
}

surv_raw <- jm_all[is_gamma, , drop = FALSE]
surv_table_out <- make_hr_block(surv_raw, sub("^gammas\\.", "", surv_raw$parameter))

alpha_raw <- jm_all[is_alpha, , drop = FALSE]
alpha_table_out <- make_hr_block(alpha_raw, sub("^alphas\\.", "", alpha_raw$parameter))

d_elem <- function(i, j) {
  ii <- max(as.integer(i), as.integer(j))
  jj <- min(as.integer(i), as.integer(j))
  pat <- sprintf("^D\\.D\\[\\s*%d\\s*,\\s*%d\\s*\\]$", ii, jj)
  hit <- jm_all[grepl(pat, jm_all$parameter), , drop = FALSE]
  if (nrow(hit) == 0L) {
    pat2 <- sprintf("^D\\.D\\[\\s*%d\\s*,\\s*%d\\s*\\]$", as.integer(i), as.integer(j))
    hit <- jm_all[grepl(pat2, jm_all$parameter), , drop = FALSE]
  }
  if (nrow(hit) > 0L && is.finite(hit$Mean[[1]])) {
    as.numeric(hit$Mean[[1]])
  } else {
    NA_real_
  }
}

re_table_out <- do.call(rbind, lapply(seq_along(traj_vars), function(k) {
  i0 <- 2L * k - 1L
  i1 <- 2L * k
  v0 <- d_elem(i0, i0)
  v1 <- d_elem(i1, i1)
  cv <- d_elem(i1, i0)
  sd0 <- if (is.finite(v0) && v0 >= 0) sqrt(v0) else NA_real_
  sd1 <- if (is.finite(v1) && v1 >= 0) sqrt(v1) else NA_real_
  corr <- if (is.finite(cv) && is.finite(sd0) && is.finite(sd1) && sd0 > 0 && sd1 > 0) {
    cv / (sd0 * sd1)
  } else {
    NA_real_
  }
  data.frame(
    longitudinal_outcome = traj_vars[[k]],
    random_intercept_sd = sd0,
    random_slope_sd = sd1,
    intercept_slope_correlation = corr,
    stringsAsFactors = FALSE
  )
}))
rownames(re_table_out) <- NULL

write.csv(long_table_out, file.path(out_dir, "03_纵向固定效应.csv"), row.names = FALSE, fileEncoding = "UTF-8")
write.csv(surv_table_out, file.path(out_dir, "04_生存子模型.csv"), row.names = FALSE, fileEncoding = "UTF-8")
write.csv(alpha_table_out, file.path(out_dir, "05_关联参数.csv"), row.names = FALSE, fileEncoding = "UTF-8")
write.csv(re_table_out, file.path(out_dir, "06_随机效应.csv"), row.names = FALSE, fileEncoding = "UTF-8")
write.csv(jm_all, file.path(out_dir, "03to06_全部后验参数.csv"), row.names = FALSE, fileEncoding = "UTF-8")

# 覆盖旧的 MCMC 摘要，并额外输出未达阈值的参数明细。
finite_rhat <- is.finite(jm_all$Rhat)
finite_ess <- is.finite(jm_all$ESS)
mcmc_summary <- data.frame(
  项目 = c(
    "诊断参数个数", "有 Rhat 的参数个数", "最大 Rhat",
    "Rhat > 1.01 的参数个数", "Rhat > 1.05 的参数个数",
    "Rhat > 1.10 的参数个数", "有 ESS 的参数个数",
    "ESS 最小值", "ESS < 400 的参数个数"
  ),
  取值 = c(
    nrow(jm_all), sum(finite_rhat),
    if (any(finite_rhat)) max(jm_all$Rhat[finite_rhat]) else NA_real_,
    sum(jm_all$Rhat > 1.01, na.rm = TRUE),
    sum(jm_all$Rhat > 1.05, na.rm = TRUE),
    sum(jm_all$Rhat > 1.10, na.rm = TRUE),
    sum(finite_ess),
    if (any(finite_ess)) min(jm_all$ESS[finite_ess]) else NA_real_,
    sum(jm_all$ESS < 400, na.rm = TRUE)
  ),
  check.names = FALSE,
  stringsAsFactors = FALSE
)
write.csv(
  mcmc_summary, file.path(out_dir, "07_MCMC摘要.csv"),
  row.names = FALSE, fileEncoding = "UTF-8"
)
mcmc_attention <- jm_all[
  (is.finite(jm_all$Rhat) & jm_all$Rhat > 1.01) |
    (is.finite(jm_all$ESS) & jm_all$ESS < 400),
  c("parameter", "Mean", "SD", "CI_low", "CI_upp", "Rhat", "ESS"),
  drop = FALSE
]
if (nrow(mcmc_attention)) {
  mcmc_attention <- mcmc_attention[order(mcmc_attention$Rhat, decreasing = TRUE, na.last = TRUE), , drop = FALSE]
}
write.csv(
  mcmc_attention, file.path(out_dir, "07_MCMC需关注参数.csv"),
  row.names = FALSE, fileEncoding = "UTF-8"
)

sample_table <- data.frame(
  dataset = c("训练集", "测试集"),
  sample_size = c(nrow(train_surv), nrow(test_surv)),
  events = c(sum(train_surv$event), sum(test_surv$event)),
  event_rate = round(c(mean(train_surv$event), mean(test_surv$event)), 4),
  stringsAsFactors = FALSE
)
obs_table <- data.frame(
  longitudinal_outcome = traj_vars,
  training_observations = as.integer(vapply(traj_vars, function(v) sum(is.finite(train_long[[v]])), numeric(1))),
  testing_observations = as.integer(vapply(traj_vars, function(v) sum(is.finite(test_long[[v]])), numeric(1))),
  stringsAsFactors = FALSE
)
write.csv(sample_table, file.path(out_dir, "01_样本概况.csv"), row.names = FALSE, fileEncoding = "UTF-8")
write.csv(obs_table, file.path(out_dir, "02_纵向观测次数.csv"), row.names = FALSE, fileEncoding = "UTF-8")

cat("\n--- 1. 样本与事件概况 ---\n")
print(sample_table, row.names = FALSE)
cat("\n--- 2. 纵向结局观测次数 ---\n")
print(obs_table, row.names = FALSE)
cat("\n--- 3. 纵向子模型参数 ---\n")
print(long_table_out, row.names = FALSE)
cat("\n--- 4. 生存子模型参数 ---\n")
print(surv_table_out, row.names = FALSE)
cat("\n--- 5. 纵向-生存关联参数 ---\n")
print(alpha_table_out, row.names = FALSE)
cat("\n--- 6. 随机效应 ---\n")
print(re_table_out, row.names = FALSE)
flush.console()

long_ft <- fmt_eng_ft(long_table_out)
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

note1 <- "解释：联合模型使用训练集估计参数，测试集仅用于验证；事件定义为28天内死亡。"
note2 <- "解释：表中为每个动态指标进入模型的重复测量数量，第1天定义为t=0。"
note3 <- "解释：每个纵向结局均报告固定截距、时间斜率和残差标准差，以及相应的后验统计量和Rhat。"
note4 <- "解释：风险比及其95%可信区间由生存系数指数转换得到，风险比大于1表示死亡风险升高。"
note5 <- "解释：关联风险比表示纵向指标潜在真实水平增加1个原始计量单位时，死亡瞬时风险的相对变化。"
note6 <- "解释：随机截距和随机斜率标准差刻画患者间异质性，相关系数刻画同一结局基线水平与变化速度的关系。"
note7 <- "解释：Rhat越接近1越好，所有参数Rhat不超过1.01较理想；超过1.05时应增加迭代并检查轨迹图。各主要参数表中同时给出了逐参数Rhat。"
note8 <- "解释：比较相同数据上的候选模型时，DIC和WAIC越小越好，LPML越大越好。条件指标和边际指标应分别解释。"
note9 <- paste0(
  "解释：C-index越大表示风险排序能力越强，Brier越小表示28天概率预测越准确；泛化能力主要查看测试集。",
  "本表在 landmark=", landmark_t, " 仍存活、且有 t≤", landmark_t, " 纵向记录的患者中计算。"
)

perf_word <- data.frame(
  dataset = metrics$dataset,
  n = metrics$n,
  events = metrics$events,
  cindex = metrics$cindex,
  brier = metrics$brier_28_ipcw,
  stringsAsFactors = FALSE
)
write.csv(perf_word, file.path(out_dir, "09_预测性能.csv"), row.names = FALSE, fileEncoding = "UTF-8")

explain_table <- data.frame(
  item = c(
    "训练集样本数和事件数",
    "纵向结局观测次数",
    "纵向子模型参数",
    "生存子模型参数",
    "纵向-生存关联参数",
    "随机效应标准差",
    "随机截距-随机斜率相关系数",
    "后验均值与后验标准差",
    "95%可信区间",
    "后验方向概率",
    "Rhat",
    "DIC和WAIC",
    "LPML",
    "C-index和Brier"
  ),
  detailed_explanation = c(
    "样本数是进入相应数据集的独立患者数；事件数是28天内死亡人数。事件率用于判断结局是否不平衡，并用于比较训练集和测试集的结局构成。",
    "观测次数是每个纵向指标实际进入模型的重复测量总数，不等于患者数。同一患者可以贡献多个时间点，观测越多通常越有利于估计个体轨迹。",
    "截距表示t=0时纵向指标的总体平均水平，t表示随时间变化的平均斜率，sigma表示观测值围绕个体轨迹的残差标准差。纵向指标未进行标化，因此参数均按照各指标的原始单位解释。",
    "生存参数表示基线变量对28天死亡瞬时风险的影响。系数大于0或风险比大于1表示风险升高；系数小于0或风险比小于1表示风险降低。",
    "关联参数连接纵向轨迹与生存风险。正值表示该纵向指标的当前潜在真实水平越高，死亡风险越高；负值表示水平越高，死亡风险越低。风险比按纵向指标增加1个原始计量单位解释。",
    "随机截距标准差反映患者基线水平的个体差异；随机斜率标准差反映患者随时间变化速度的个体差异。数值越大，患者之间的异质性越明显。",
    "相关系数描述同一纵向结局中患者基线水平与变化速度的关系。正相关表示基线较高者倾向于上升更快，负相关表示基线较高者倾向于下降更快。不同纵向结局之间的随机效应在本模型中设为独立。",
    "后验均值是参数的主要点估计；后验标准差反映估计的不确定性。后验标准差相对于均值越小，参数估计通常越稳定。",
    "在当前数据与先验条件下，参数有95%的后验概率位于该区间。参数区间不跨0通常表示效应方向较明确；风险比区间不跨1通常表示风险效应较明确。",
    "本报告使用1-P/2计算参数与后验均值同方向的概率。数值越接近1，参数方向的后验证据越强，但解释时仍应结合效应大小及95%可信区间。",
    "Rhat比较多条MCMC链的链内与链间变异。Rhat接近1表示收敛良好；通常不超过1.01较理想，超过1.05提示应增加迭代并检查轨迹图。",
    "DIC和WAIC同时考虑拟合度与模型复杂度，只适合在相同数据上的候选模型之间比较，通常越小越好。条件指标包含个体随机效应，边际指标对随机效应进行积分。",
    "LPML反映模型对观测数据的整体预测支持度。在相同数据上的候选模型之间比较时，LPML越大，即越不负，通常表示模型越好。",
    "C-index衡量风险排序能力，越接近1区分度越好，0.5约等于随机排序；Brier衡量28天预测概率与实际结局的均方误差，越小越好。测试集指标主要反映泛化能力。"
  ),
  stringsAsFactors = FALSE
)

sample_ft <- sample_table
sample_ft$sample_size <- as.character(sample_table$sample_size)
sample_ft$events <- as.character(sample_table$events)
sample_ft$event_rate <- fmt_num(sample_table$event_rate, 4)
obs_ft <- obs_table
obs_ft$training_observations <- as.character(obs_table$training_observations)
obs_ft$testing_observations <- as.character(obs_table$testing_observations)
perf_ft <- perf_word
perf_ft$n <- as.character(perf_word$n)
perf_ft$events <- as.character(perf_word$events)
perf_ft$cindex <- fmt_num(perf_word$cindex, 4)
perf_ft$brier <- fmt_num(perf_word$brier, 4)

jm_doc <- officer::read_docx()
jm_doc <- officer::body_add_par(jm_doc, "多变量联合模型总体结果报告", style = "graphic title")
jm_doc <- officer::body_add_par(
  jm_doc,
  paste0(
    "模型：3个纵向结局（",
    paste(traj_vars, collapse = "、"),
    "）与28天生存结局的联合模型；随机效应为截距+斜率（~ t | hadm_id）；",
    "生存子模型为8个临床基线 + 17个第1日截面；数据集划分用 group=1/2。",
    "预测性能采用 landmark=", landmark_t, "。",
    "报告生成时间：", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "。",
    rhat_note
  ),
  style = "Normal"
)

jm_doc <- add_jm_table(jm_doc, "1. 样本与事件概况", sample_ft, note1)
jm_doc <- add_jm_table(jm_doc, "2. 纵向结局观测次数", obs_ft, note2)
jm_doc <- add_jm_table(jm_doc, "3. 纵向子模型参数", long_ft, note3)
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
    "当前没有 jmFit 的 DIC/WAIC/LPML。完整运行拟合后会写入本表。",
    style = "Normal"
  )
}

jm_doc <- officer::body_add_break(jm_doc)
jm_doc <- add_jm_table(jm_doc, "9. 训练集与测试集预测性能", perf_ft, note9)

jm_doc <- officer::body_add_break(jm_doc)
jm_doc <- add_jm_table(
  jm_doc,
  "10. 各项结果详细解释",
  explain_table,
  NULL
)

out_docx <- file.path(out_dir, "JM模型总体结果报告.docx")
print(jm_doc, target = out_docx)
cat("已写入第 1–10 节 CSV 与 Word：\n", normalizePath(out_dir), "\n", sep = "")
cat("Word：", normalizePath(out_docx, mustWork = FALSE), "\n", sep = "")
