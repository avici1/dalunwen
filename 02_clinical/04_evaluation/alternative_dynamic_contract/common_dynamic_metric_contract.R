options(stringsAsFactors = FALSE)

LANDMARK_DAY <- 5
HORIZON_DAY <- 28
EVAL_GRID <- unique(c(seq(LANDMARK_DAY, HORIZON_DAY, by = 0.5), HORIZON_DAY))
SEED_VALUE <- 2026L

LONG_VARS <- c(
  "total_urine_output", "creat", "aki_stage", "gcs", "ph", "pco2",
  "lactate", "po2", "pao2fio2ratio", "glucose", "sodium", "bicarbonate",
  "hemoglobin", "temperature", "fio2", "sofa_24hours", "cns_24hours",
  "renal_24hours", "cardiovascular_24hours", "respiration_24hours"
)
JM_TRAJ_VARS <- c("gcs", "sofa_24hours", "cns_24hours")
JM_DAY1_VARS <- setdiff(LONG_VARS, JM_TRAJ_VARS)
CLINICAL_COVARS <- c(
  "age", "charlson_comorbidity_index", "apsiii", "sapsii", "oasis",
  "preiculos", "mechvent", "electivesurgery"
)
RSFLC_FIXED_COVARS <- c(
  CLINICAL_COVARS, "gender", "bmi", "stroke_type"
)

parse_datetime <- function(x) {
  as.POSIXct(x, format = "%d/%m/%Y %H:%M:%S", tz = "UTC")
}

make_common_dynamic_cohort <- function(baseline_path, longitudinal_path) {
  baseline <- read.csv(
    baseline_path, stringsAsFactors = FALSE, na.strings = c("", "NA"),
    fileEncoding = "UTF-8"
  )
  long <- read.csv(
    longitudinal_path, stringsAsFactors = FALSE, na.strings = c("", "NA"),
    fileEncoding = "UTF-8"
  )

  req_base <- c(
    "hadm_id", RSFLC_FIXED_COVARS, "intime", "deathtime", "death_28d", "group"
  )
  req_long <- c("hadm_id", "times", "group", LONG_VARS)
  miss_base <- setdiff(req_base, names(baseline))
  miss_long <- setdiff(req_long, names(long))
  if (length(miss_base)) stop("基线数据缺少列: ", paste(miss_base, collapse = ", "))
  if (length(miss_long)) stop("纵向数据缺少列: ", paste(miss_long, collapse = ", "))

  baseline$hadm_id <- as.integer(baseline$hadm_id)
  baseline$group <- as.integer(baseline$group)
  baseline$death_28d <- as.integer(baseline$death_28d)
  baseline$intime_parsed <- parse_datetime(baseline$intime)
  baseline$deathtime_parsed <- parse_datetime(baseline$deathtime)
  baseline$exact_death_time_missing <- baseline$death_28d == 1L & is.na(baseline$deathtime_parsed)
  raw_time <- as.numeric(difftime(
    baseline$deathtime_parsed, baseline$intime_parsed, units = "days"
  ))
  baseline$time28 <- ifelse(is.finite(raw_time), pmin(raw_time, HORIZON_DAY), HORIZON_DAY)
  baseline$status28 <- as.integer(baseline$death_28d == 1L)
  baseline <- baseline[
    !duplicated(baseline$hadm_id) & baseline$group %in% c(1L, 2L) &
      is.finite(baseline$time28) & baseline$time28 > LANDMARK_DAY &
      !is.na(baseline$status28), , drop = FALSE
  ]

  long$hadm_id <- as.integer(long$hadm_id)
  long$times <- as.numeric(long$times)
  long$group <- as.integer(long$group)
  for (nm in LONG_VARS) long[[nm]] <- as.numeric(long[[nm]])
  long <- long[
    long$group %in% c(1L, 2L) & is.finite(long$times) &
      long$times <= LANDMARK_DAY + 1e-8, , drop = FALSE
  ]

  long_group <- unique(long[, c("hadm_id", "group")])
  group_counts <- table(long_group$hadm_id)
  if (any(group_counts > 1L)) stop("同一 hadm_id 在纵向数据中对应多个 group。")
  common <- merge(
    baseline, long_group, by = "hadm_id", suffixes = c("", "_long"), sort = FALSE
  )
  common <- common[common$group == common$group_long, , drop = FALSE]

  day1 <- long[abs(long$times - 1) < 1e-8, c("hadm_id", JM_DAY1_VARS), drop = FALSE]
  day1 <- day1[!duplicated(day1$hadm_id), , drop = FALSE]
  day1_ok <- day1$hadm_id[complete.cases(day1)]

  history_ok <- vapply(split(long, long$hadm_id), function(x) {
    if (nrow(x) < 2L) return(FALSE)
    all(vapply(LONG_VARS, function(nm) any(is.finite(x[[nm]])), logical(1))) &&
      all(vapply(JM_TRAJ_VARS, function(nm) sum(is.finite(x[[nm]])) >= 2L, logical(1)))
  }, logical(1))
  history_ids <- as.integer(names(history_ok)[history_ok])

  fixed_ok <- complete.cases(common[, RSFLC_FIXED_COVARS, drop = FALSE])
  keep_ids <- intersect(common$hadm_id[fixed_ok], intersect(day1_ok, history_ids))
  common <- common[common$hadm_id %in% keep_ids, , drop = FALSE]
  common <- common[order(common$group, common$hadm_id), , drop = FALSE]
  long <- long[long$hadm_id %in% common$hadm_id, , drop = FALSE]
  long <- long[order(long$hadm_id, long$times), , drop = FALSE]
  day1 <- day1[day1$hadm_id %in% common$hadm_id, , drop = FALSE]

  if (!identical(sort(unique(long$hadm_id)), sort(common$hadm_id))) {
    stop("共同队列与纵向数据 ID 未完全对齐。")
  }
  if (length(intersect(
    common$hadm_id[common$group == 1L], common$hadm_id[common$group == 2L]
  ))) stop("训练集与验证集 ID 重叠。")

  list(
    baseline = common,
    longitudinal = long,
    day1 = day1,
    train_ids = common$hadm_id[common$group == 1L],
    test_ids = common$hadm_id[common$group == 2L]
  )
}

step_risk_to_grid <- function(risk, source_times, target_times = EVAL_GRID) {
  risk <- as.matrix(risk)
  source_times <- as.numeric(source_times)
  if (ncol(risk) != length(source_times) && nrow(risk) == length(source_times)) {
    risk <- t(risk)
  }
  if (ncol(risk) != length(source_times)) stop("预测矩阵列数与时间点数不一致。")
  ord <- order(source_times)
  source_times <- source_times[ord]
  risk <- risk[, ord, drop = FALSE]
  out <- vapply(target_times, function(tt) {
    idx <- max(which(source_times <= tt + 1e-8))
    risk[, idx]
  }, numeric(nrow(risk)))
  out <- as.matrix(out)
  rownames(out) <- rownames(risk)
  colnames(out) <- format(target_times, trim = TRUE, scientific = FALSE)
  out <- pmin(pmax(out, 0), 1)
  out
}

assert_risk_matrix <- function(risk, ids, grid = EVAL_GRID, tol = 1e-6) {
  risk <- as.matrix(risk)
  if (nrow(risk) != length(ids) || ncol(risk) != length(grid)) {
    stop("风险矩阵维度错误。")
  }
  if (is.null(rownames(risk)) || !identical(as.character(ids), rownames(risk))) {
    stop("风险矩阵行名必须按 outcome 的 hadm_id 排列。")
  }
  if (any(!is.finite(risk))) stop("风险矩阵含 NA/Inf。")
  if (any(risk < -tol | risk > 1 + tol)) stop("风险不在 [0,1]。")
  if (any(apply(risk, 1L, function(x) any(diff(x) < -tol)))) {
    stop("累计风险曲线存在明显非单调下降。")
  }
  invisible(TRUE)
}

km_censor_survival <- function(time, status) {
  censor_event <- as.integer(status == 0L)
  u <- sort(unique(time[censor_event == 1L]))
  if (!length(u)) {
    return(list(times = numeric(0), surv = numeric(0)))
  }
  surv <- cumprod(vapply(u, function(tt) {
    n_risk <- sum(time >= tt)
    d_cens <- sum(time == tt & censor_event == 1L)
    if (n_risk > 0) 1 - d_cens / n_risk else 1
  }, numeric(1)))
  list(times = u, surv = surv)
}

eval_km_step <- function(km, x, left_limit = FALSE) {
  if (!length(km$times)) return(rep(1, length(x)))
  vapply(x, function(xx) {
    idx <- if (left_limit) which(km$times < xx) else which(km$times <= xx)
    if (!length(idx)) 1 else km$surv[max(idx)]
  }, numeric(1))
}

ipcw_brier_curve <- function(risk, time, status, grid = EVAL_GRID) {
  risk <- as.matrix(risk)
  n <- length(time)
  km <- km_censor_survival(time, status)
  out <- numeric(length(grid))
  for (j in seq_along(grid)) {
    tt <- grid[j]
    if (abs(tt - HORIZON_DAY) < 1e-8) {
      # 28天结局在原数据中完整给出；精确端点评价按二分类结局定义。
      y <- as.numeric(status == 1L & time <= HORIZON_DAY + 1e-8)
      out[j] <- mean((y - risk[, j])^2)
      next
    }
    is_case <- status == 1L & time <= tt
    is_control <- time > tt
    w <- numeric(n)
    if (any(is_case)) {
      g_event <- eval_km_step(km, time[is_case], left_limit = TRUE)
      w[is_case] <- ifelse(g_event > 0, 1 / g_event, 0)
    }
    g_t <- eval_km_step(km, tt, left_limit = FALSE)
    if (g_t > 0) w[is_control] <- 1 / g_t
    y <- as.numeric(is_case)
    out[j] <- sum(w * (y - risk[, j])^2) / n
  }
  out
}

binary_auc <- function(y, score) {
  y <- as.integer(y)
  if (length(unique(y)) < 2L) return(NA_real_)
  as.numeric(pROC::auc(pROC::roc(
    response = y, predictor = score, levels = c(0, 1), direction = "<", quiet = TRUE
  )))
}

evaluate_dynamic_risk <- function(risk, outcome, grid = EVAL_GRID) {
  outcome <- outcome[match(as.integer(rownames(risk)), outcome$hadm_id), , drop = FALSE]
  if (anyNA(outcome$hadm_id)) stop("风险矩阵与结局 ID 无法对齐。")
  assert_risk_matrix(risk, outcome$hadm_id, grid)
  horizon_risk <- risk[, ncol(risk)]
  horizon_y <- as.integer(outcome$status28 == 1L & outcome$time28 <= HORIZON_DAY + 1e-8)
  cindex <- survival::concordance(
    survival::Surv(outcome$time28 - LANDMARK_DAY, outcome$status28) ~ horizon_risk,
    reverse = TRUE
  )$concordance
  brier_curve <- ipcw_brier_curve(
    risk, outcome$time28, outcome$status28, grid
  )
  ibs <- sum(diff(grid) * (head(brier_curve, -1L) + tail(brier_curve, -1L)) / 2) /
    (HORIZON_DAY - LANDMARK_DAY)
  list(
    n = nrow(outcome),
    events = sum(horizon_y),
    cindex = as.numeric(cindex),
    auc_28 = binary_auc(horizon_y, horizon_risk),
    brier_28 = mean((horizon_y - horizon_risk)^2),
    ibs_5_28 = as.numeric(ibs),
    brier_curve = data.frame(time = grid, brier_ipcw = brier_curve),
    horizon = data.frame(
      hadm_id = outcome$hadm_id, time28 = outcome$time28,
      status28 = outcome$status28, risk28 = horizon_risk
    )
  )
}

metric_rows <- function(model, setting, train_eval, test_eval) {
  data.frame(
    模型 = model,
    模型设定 = setting,
    训练集_C_index = train_eval$cindex,
    验证集_C_index = test_eval$cindex,
    验证集_28天_AUC = test_eval$auc_28,
    验证集_28天_Brier = test_eval$brier_28,
    验证集_IBS_5_28天 = test_eval$ibs_5_28,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

write_utf8_csv <- function(x, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  write.csv(x, path, row.names = FALSE, fileEncoding = "UTF-8")
}
