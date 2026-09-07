options(stringsAsFactors = FALSE)
invisible(try(Sys.setlocale("LC_ALL", "English_United States.utf8"), silent = TRUE))

seed_value <- 2026L
set.seed(seed_value)

data_dir <- Sys.getenv("CH5_DATA_DIR", unset = "data")
out_dir <- Sys.getenv("CH5_OUTPUT_DIR", unset = "outputs")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(out_dir, "tables"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(out_dir, "figures"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(out_dir, "models"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(out_dir, "logs"), recursive = TRUE, showWarnings = FALSE)

baseline_path <- file.path(data_dir, "baseline.csv")
longitudinal_path <- file.path(data_dir, "longitudinal.csv")

landmark <- 5
horizon <- 28
static_times <- unique(c(seq(0, 27.5, by = 0.5), horizon))
dynamic_times <- seq(landmark, horizon, by = 1)

clinical8 <- c(
  "age", "charlson_comorbidity_index", "apsiii", "sapsii", "oasis",
  "preiculos", "mechvent", "electivesurgery"
)
fixed11 <- c(clinical8, "gender", "bmi", "stroke_type")
long20 <- c(
  "total_urine_output", "creat", "aki_stage", "gcs", "ph", "pco2",
  "lactate", "po2", "pao2fio2ratio", "glucose", "sodium", "bicarbonate",
  "hemoglobin", "temperature", "fio2", "sofa_24hours", "cns_24hours",
  "renal_24hours", "cardiovascular_24hours", "respiration_24hours"
)
traj3 <- c("gcs", "sofa_24hours", "cns_24hours")
day1_17 <- setdiff(long20, traj3)
static31 <- c(fixed11, long20)
dynamic_fixed25 <- c(clinical8, day1_17)
factor_vars <- c("mechvent", "electivesurgery", "gender", "stroke_type")

rsf_par <- list(ntree = 500L, mtry = 3L, nodesize = 10L, nsplit = 10L)
rsflc_par <- list(ntree = 200L, mtry = 3L, nodesize = 1L, minsplit = 2L)
jm_par <- list(n_chains = 3L, n_iter = 1500L, n_burnin = 500L)

shap_n <- 200L
shap_nsim <- 50L
shap_background_n <- 200L
permutation_repeats <- 30L
bootstrap_repeats <- 1000L

write_utf8 <- function(x, path) {
  utils::write.csv(x, path, row.names = FALSE, fileEncoding = "UTF-8")
}

step_at <- function(times, values, query, initial = 0) {
  idx <- findInterval(query, times)
  out <- rep(initial, length(query))
  keep <- idx > 0
  out[keep] <- values[pmin(idx[keep], length(values))]
  out
}

km_censor_function <- function(time, status) {
  fit <- survival::survfit(survival::Surv(time, 1L - status) ~ 1)
  function(t, left = FALSE) {
    tt <- if (left) pmax(0, t - 1e-08) else t
    idx <- findInterval(tt, fit$time)
    out <- rep(1, length(tt))
    keep <- idx > 0
    out[keep] <- fit$surv[pmin(idx[keep], length(fit$surv))]
    pmax(out, 1e-06)
  }
}

ipcw_auc <- function(time, status, risk, eval_time, Gfun) {
  case <- status == 1L & time <= eval_time
  # Administrative survivors are recorded with time == horizon. They are
  # known event-free through that day and therefore belong to the control set.
  ctrl <- !case & time >= eval_time
  if (sum(case) == 0L || sum(ctrl) == 0L) return(NA_real_)
  wc <- 1 / Gfun(time[case], left = TRUE)
  wn <- rep(1 / Gfun(eval_time, left = TRUE), sum(ctrl))
  rc <- risk[case]
  rn <- risk[ctrl]
  # Weighted Mann-Whitney statistic, implemented without the O(n_case*n_control)
  # comparison matrix. This is exactly the same IPCW AUC, including 0.5 ties.
  ctrl_weights <- tapply(wn, rn, sum)
  ctrl_values <- as.numeric(names(ctrl_weights))
  oo <- order(ctrl_values)
  ctrl_values <- ctrl_values[oo]
  ctrl_weights <- as.numeric(ctrl_weights[oo])
  cum_weights <- cumsum(ctrl_weights)
  le_idx <- findInterval(rc, ctrl_values)
  eq_idx <- match(rc, ctrl_values, nomatch = 0L)
  less_weight <- ifelse(le_idx > 0L, cum_weights[pmax(le_idx, 1L)], 0)
  has_tie <- eq_idx > 0L
  less_weight[has_tie] <- less_weight[has_tie] - ctrl_weights[eq_idx[has_tie]]
  tie_weight <- numeric(length(rc))
  tie_weight[has_tie] <- ctrl_weights[eq_idx[has_tie]]
  sum(wc * (less_weight + 0.5 * tie_weight)) / (sum(wc) * sum(wn))
}

ipcw_brier <- function(time, status, risk, eval_time, Gfun) {
  event_before <- status == 1L & time <= eval_time
  y_surv <- as.numeric(!event_before & time >= eval_time)
  pred_surv <- 1 - risk
  w <- numeric(length(time))
  at_risk <- !event_before & time >= eval_time
  w[event_before] <- 1 / Gfun(time[event_before], left = TRUE)
  w[at_risk] <- 1 / Gfun(eval_time, left = TRUE)
  mean(w * (y_surv - pred_surv)^2)
}

metric_bundle <- function(time, status, risk_matrix, eval_times, Gfun) {
  stopifnot(nrow(risk_matrix) == length(time), ncol(risk_matrix) == length(eval_times))
  cindex <- survival::concordance(
    survival::Surv(time, status) ~ risk_matrix[, ncol(risk_matrix)],
    reverse = TRUE
  )$concordance
  auc <- vapply(seq_along(eval_times), function(j) {
    ipcw_auc(time, status, risk_matrix[, j], eval_times[j], Gfun)
  }, numeric(1))
  brier <- vapply(seq_along(eval_times), function(j) {
    ipcw_brier(time, status, risk_matrix[, j], eval_times[j], Gfun)
  }, numeric(1))
  span <- max(eval_times) - min(eval_times)
  ibs <- if (span > 0) {
    sum(diff(eval_times) * (head(brier, -1) + tail(brier, -1)) / 2) / span
  } else {
    brier[length(brier)]
  }
  list(cindex = as.numeric(cindex), auc = auc, brier = brier, ibs = as.numeric(ibs))
}

bootstrap_metrics <- function(time, status, risk_matrix, eval_times, Gfun, B = bootstrap_repeats) {
  set.seed(seed_value)
  vals <- matrix(NA_real_, nrow = B, ncol = 4,
                 dimnames = list(NULL, c("C_index", "AUC", "Brier", "IBS")))
  n <- length(time)
  for (b in seq_len(B)) {
    ii <- sample.int(n, n, replace = TRUE)
    z <- try({
      tb <- time[ii]
      sb <- status[ii]
      rb <- risk_matrix[ii, , drop = FALSE]
      c_b <- survival::concordance(survival::Surv(tb, sb) ~ rb[, ncol(rb)], reverse = TRUE)$concordance
      a_b <- ipcw_auc(tb, sb, rb[, ncol(rb)], tail(eval_times, 1), Gfun)
      bs <- vapply(seq_along(eval_times), function(j) {
        ipcw_brier(tb, sb, rb[, j], eval_times[j], Gfun)
      }, numeric(1))
      span <- max(eval_times) - min(eval_times)
      ibs_b <- if (span > 0) sum(diff(eval_times) * (head(bs, -1) + tail(bs, -1)) / 2) / span else tail(bs, 1)
      c(c_b, a_b, tail(bs, 1), ibs_b)
    }, silent = TRUE)
    if (!inherits(z, "try-error")) {
      vals[b, ] <- z
    }
  }
  data.frame(
    metric = colnames(vals),
    lower95 = apply(vals, 2, stats::quantile, probs = 0.025, na.rm = TRUE),
    upper95 = apply(vals, 2, stats::quantile, probs = 0.975, na.rm = TRUE),
    valid_bootstrap = colSums(is.finite(vals)),
    row.names = NULL
  )
}

calibration_table <- function(risk, outcome, groups = 10L) {
  br <- unique(stats::quantile(risk, probs = seq(0, 1, length.out = groups + 1), na.rm = TRUE))
  if (length(br) < 3L) br <- seq(min(risk), max(risk) + 1e-08, length.out = 3L)
  grp <- cut(risk, breaks = br, include.lowest = TRUE, labels = FALSE)
  d <- data.frame(risk = risk, outcome = outcome, group = grp)
  dplyr::summarise(dplyr::group_by(d, group), n = dplyr::n(),
                   predicted = mean(risk), observed = mean(outcome), .groups = "drop")
}

dca_table <- function(risk, outcome, thresholds = seq(0.01, 0.50, by = 0.01)) {
  n <- length(outcome)
  prevalence <- mean(outcome)
  out <- lapply(thresholds, function(pt) {
    pred <- risk >= pt
    tp <- sum(pred & outcome == 1L)
    fp <- sum(pred & outcome == 0L)
    data.frame(
      threshold = pt,
      model = tp / n - fp / n * pt / (1 - pt),
      treat_all = prevalence - (1 - prevalence) * pt / (1 - pt),
      treat_none = 0
    )
  })
  dplyr::bind_rows(out)
}
