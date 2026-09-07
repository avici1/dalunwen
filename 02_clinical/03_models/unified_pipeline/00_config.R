options(stringsAsFactors = FALSE, width = 220)
invisible(try(Sys.setlocale("LC_CTYPE", ".UTF-8"), silent = TRUE))

seed_value <- 2026L
set.seed(seed_value)

script_dir <- Sys.getenv("CH5_SCRIPT_DIR", unset = getwd())
project_dir <- Sys.getenv(
  "CH5_PROJECT_ROOT",
  unset = normalizePath(file.path(script_dir, ".."), winslash = "/", mustWork = FALSE)
)
data_dir <- Sys.getenv("CH5_DATA_DIR", unset = file.path(project_dir, "data"))
artifact_dir <- Sys.getenv(
  "CH5_ARTIFACT_DIR",
  unset = file.path(project_dir, "\u56fe\u50cf\u8868\u683c")
)
cache_dir <- Sys.getenv("CH5_CACHE_DIR", unset = file.path(project_dir, "cache"))
log_dir <- Sys.getenv("CH5_LOG_DIR", unset = file.path(project_dir, "logs"))

for (d in c(data_dir, artifact_dir, cache_dir, log_dir)) {
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
}

baseline_path <- file.path(data_dir, "stroke_baseline_knn_0824_fold.csv")
longitudinal_path <- file.path(data_dir, "stroke_longitudinal_knn_0824_group_fold.csv")

# Locked study definitions.
time_scale_days <- 28
landmark_day <- 5
horizon_day <- 28
landmark_u <- landmark_day / time_scale_days
horizon_u <- 1
static_eval_u <- unique(c(seq(0, 27.5 / 28, by = 0.5 / 28), 1))
dynamic_eval_u <- seq(landmark_u, horizon_u, by = 1 / 28)

fixed11 <- c(
  "age", "charlson_comorbidity_index", "apsiii", "sapsii", "oasis",
  "preiculos", "mechvent", "electivesurgery", "gender", "bmi", "stroke_type"
)
long20 <- c(
  "total_urine_output", "creat", "aki_stage", "gcs", "ph", "pco2",
  "lactate", "po2", "pao2fio2ratio", "glucose", "sodium", "bicarbonate",
  "hemoglobin", "temperature", "fio2", "sofa_24hours", "cns_24hours",
  "renal_24hours", "cardiovascular_24hours", "respiration_24hours"
)
traj3 <- c("gcs", "sofa_24hours", "cns_24hours")
day1_17 <- setdiff(long20, traj3)
static31 <- c(fixed11, long20)
dynamic_fixed28 <- c(fixed11, day1_17)
factor_vars <- c("mechvent", "electivesurgery", "gender", "stroke_type")

stopifnot(length(static31) == 31L, length(dynamic_fixed28) == 28L, length(traj3) == 3L)

# Locked hyperparameters used by 04_static_models.R / 06_rsflc_model.R.
# Search grids are used by 07a / 07b (group=1 five-fold only; default off).
rsf_par <- list(ntree = 500L, mtry = 3L, nodesize = 10L, nsplit = 10L)
rsflc_par <- list(ntree = 200L, mtry = 3L, nodesize = 1L, minsplit = 2L)
jm_par <- list(n_chains = 3L, n_iter = 3000L, n_burnin = 1500L)

rsf_grid <- expand.grid(
  ntree = c(300L, 500L, 1000L), mtry = c(3L, 6L, 9L),
  nodesize = c(10L, 20L, 30L, 40L), nsplit = c(10L, 25L, 50L)
)
rsflc_grid <- expand.grid(
  ntree = c(50L, 100L, 200L), mtry = c(3L, 6L, 9L, 12L),
  nodesize = c(1L, 3L, 5L)
)

bootstrap_repeats <- 1000L
rsf_permutation_repeats <- 30L
rsf_shap_n <- 200L
rsf_shap_nsim <- 50L
rsf_shap_background_n <- 200L
rsflc_shap_n <- 12L
rsflc_shap_nsim <- 3L

cn_labels <- c(
  age = "\u5e74\u9f84", charlson_comorbidity_index = "Charlson\u5408\u5e76\u75c7\u6307\u6570",
  apsiii = "APSIII", sapsii = "SAPSII", oasis = "OASIS",
  preiculos = "ICU\u524d\u4f4f\u9662\u65f6\u957f", mechvent = "\u673a\u68b0\u901a\u6c14",
  electivesurgery = "\u62e9\u671f\u624b\u672f", gender = "\u6027\u522b",
  bmi = "BMI", stroke_type = "\u5352\u4e2d\u5206\u578b",
  total_urine_output = "24 h\u5c3f\u91cf", creat = "\u8840\u808c\u9150",
  aki_stage = "AKI\u5206\u671f", gcs = "GCS", ph = "pH", pco2 = "PaCO2",
  lactate = "\u4e73\u9178", po2 = "PaO2", pao2fio2ratio = "PaO2/FiO2",
  glucose = "\u8840\u7cd6", sodium = "\u8840\u94a0", bicarbonate = "\u78b3\u9178\u6c22\u76d0",
  hemoglobin = "\u8840\u7ea2\u86cb\u767d", temperature = "\u4f53\u6e29", fio2 = "FiO2",
  sofa_24hours = "SOFA\u603b\u5206", cns_24hours = "CNS\u8bc4\u5206",
  renal_24hours = "\u80be\u810fSOFA", cardiovascular_24hours = "\u5fc3\u8840\u7ba1SOFA",
  respiration_24hours = "\u547c\u5438SOFA"
)

write_utf8 <- function(x, path) {
  utils::write.csv(x, path, row.names = FALSE, fileEncoding = "UTF-8")
  invisible(path)
}

log_progress <- function(stage, detail = "") {
  line <- sprintf("%s\t%s\t%s", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), stage, detail)
  cat(line, "\n", file = file.path(log_dir, "progress.log"), append = TRUE)
  message("[", stage, "] ", detail)
}

step_at <- function(times, values, query, initial = 0) {
  idx <- findInterval(query, times)
  out <- rep(initial, length(query))
  keep <- idx > 0L
  out[keep] <- values[pmin(idx[keep], length(values))]
  out
}

km_censor_function <- function(time, status) {
  fit <- survival::survfit(survival::Surv(time, 1L - status) ~ 1)
  function(t, left = FALSE) {
    tt <- if (left) pmax(0, t - 1e-10) else t
    idx <- findInterval(tt, fit$time)
    out <- rep(1, length(tt))
    keep <- idx > 0L
    out[keep] <- fit$surv[pmin(idx[keep], length(fit$surv))]
    pmax(out, 1e-6)
  }
}

ipcw_auc <- function(time, status, risk, eval_time, Gfun) {
  case <- status == 1L & time <= eval_time
  ctrl <- !case & time >= eval_time
  if (!any(case) || !any(ctrl)) return(NA_real_)
  wc <- 1 / Gfun(time[case], left = TRUE)
  wn <- rep(1 / Gfun(eval_time, left = TRUE), sum(ctrl))
  rc <- risk[case]
  rn <- risk[ctrl]
  ctrl_weights <- tapply(wn, rn, sum)
  ctrl_values <- as.numeric(names(ctrl_weights))
  oo <- order(ctrl_values)
  ctrl_values <- ctrl_values[oo]
  ctrl_weights <- as.numeric(ctrl_weights[oo])
  cum_weights <- cumsum(ctrl_weights)
  le_idx <- findInterval(rc, ctrl_values)
  eq_idx <- match(rc, ctrl_values, nomatch = 0L)
  less_weight <- ifelse(le_idx > 0L, cum_weights[pmax(le_idx, 1L)], 0)
  tied <- eq_idx > 0L
  less_weight[tied] <- less_weight[tied] - ctrl_weights[eq_idx[tied]]
  tie_weight <- numeric(length(rc))
  tie_weight[tied] <- ctrl_weights[eq_idx[tied]]
  sum(wc * (less_weight + 0.5 * tie_weight)) / (sum(wc) * sum(wn))
}

ipcw_brier <- function(time, status, risk, eval_time, Gfun) {
  event_before <- status == 1L & time <= eval_time
  at_risk <- !event_before & time >= eval_time
  y_surv <- as.numeric(at_risk)
  pred_surv <- 1 - risk
  w <- numeric(length(time))
  w[event_before] <- 1 / Gfun(time[event_before], left = TRUE)
  w[at_risk] <- 1 / Gfun(eval_time, left = TRUE)
  mean(w * (y_surv - pred_surv)^2)
}

metric_bundle <- function(time, status, risk_matrix, eval_times, Gfun) {
  stopifnot(nrow(risk_matrix) == length(time), ncol(risk_matrix) == length(eval_times))
  cindex <- survival::concordance(
    survival::Surv(time, status) ~ risk_matrix[, ncol(risk_matrix)], reverse = TRUE
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
  } else tail(brier, 1)
  list(cindex = as.numeric(cindex), auc = auc, brier = brier, ibs = as.numeric(ibs))
}

cluster_bootstrap_indices <- function(cluster_id) {
  clusters <- unique(cluster_id)
  draws <- sample(clusters, length(clusters), replace = TRUE)
  unlist(lapply(draws, function(z) which(cluster_id == z)), use.names = FALSE)
}

bootstrap_metrics <- function(time, status, risk_matrix, eval_times, Gfun,
                              cluster_id = seq_along(time), B = bootstrap_repeats) {
  set.seed(seed_value)
  vals <- matrix(NA_real_, B, 4L, dimnames = list(NULL, c("C_index", "AUC", "Brier", "IBS")))
  for (b in seq_len(B)) {
    ii <- cluster_bootstrap_indices(cluster_id)
    z <- try({
      tb <- time[ii]; sb <- status[ii]; rb <- risk_matrix[ii, , drop = FALSE]
      c_b <- survival::concordance(survival::Surv(tb, sb) ~ rb[, ncol(rb)], reverse = TRUE)$concordance
      a_b <- ipcw_auc(tb, sb, rb[, ncol(rb)], tail(eval_times, 1), Gfun)
      bs <- vapply(seq_along(eval_times), function(j) {
        ipcw_brier(tb, sb, rb[, j], eval_times[j], Gfun)
      }, numeric(1))
      span <- max(eval_times) - min(eval_times)
      ibs_b <- if (span > 0) sum(diff(eval_times) * (head(bs, -1) + tail(bs, -1)) / 2) / span else tail(bs, 1)
      c(c_b, a_b, tail(bs, 1), ibs_b)
    }, silent = TRUE)
    if (!inherits(z, "try-error")) vals[b, ] <- z
  }
  data.frame(
    metric = colnames(vals),
    lower95 = apply(vals, 2, stats::quantile, probs = 0.025, na.rm = TRUE),
    upper95 = apply(vals, 2, stats::quantile, probs = 0.975, na.rm = TRUE),
    valid_bootstrap = colSums(is.finite(vals)), row.names = NULL
  )
}

calibration_table <- function(risk, outcome, groups = 10L) {
  br <- unique(stats::quantile(risk, probs = seq(0, 1, length.out = groups + 1L), na.rm = TRUE))
  if (length(br) < 3L) br <- seq(min(risk), max(risk) + 1e-8, length.out = 3L)
  grp <- cut(risk, breaks = br, include.lowest = TRUE, labels = FALSE)
  d <- data.frame(risk = risk, outcome = outcome, group = grp)
  dplyr::summarise(dplyr::group_by(d, group), n = dplyr::n(),
                   predicted = mean(risk), observed = mean(outcome), .groups = "drop")
}

dca_table <- function(risk, outcome, thresholds = seq(0.01, 0.50, by = 0.01)) {
  n <- length(outcome)
  prevalence <- mean(outcome)
  dplyr::bind_rows(lapply(thresholds, function(pt) {
    pred <- risk >= pt
    tp <- sum(pred & outcome == 1L)
    fp <- sum(pred & outcome == 0L)
    data.frame(
      threshold = pt,
      model = tp / n - fp / n * pt / (1 - pt),
      treat_all = prevalence - (1 - prevalence) * pt / (1 - pt),
      treat_none = 0
    )
  }))
}

format_mean_sd <- function(x, digits = 2L) {
  sprintf(paste0("%.", digits, "f ", intToUtf8(177L), " %.", digits, "f"),
          mean(x, na.rm = TRUE), stats::sd(x, na.rm = TRUE))
}

format_n_pct <- function(n, total, digits = 1L) {
  sprintf(paste0("%d (%.", digits, "f%%)"), n, 100 * n / total)
}
