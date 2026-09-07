# =============================================================================
# RSFLC：DynForest（与 0323_RSFLC.R 一致：纵向 itemid + 固定 anchor_age/gender）
# SHAP 目标：predict(..., t0)$pred_indiv 最后一列（与 5 折脚本中 risk 一致）
# 说明：fastshap 扰动基线协变量；每人构造 1 条 timeData(time=0)；fixedData 中
#       生存时间/事件用训练集的中位数占位。
# =============================================================================
to_num_rsflc <- function(x) {
  if (is.list(x) && !inherits(x, "data.frame")) x <- unlist(x, use.names = FALSE)
  suppressWarnings(as.numeric(x))
}
first_not_na_rsflc <- function(x) {
  x2 <- x[!is.na(x) & !(is.character(x) & trimws(as.character(x)) == "")]
  if (length(x2) == 0L) NA else x2[1]
}

sid_keep <- as.character(dat$subject_id)
raw_long <- as.data.frame(readxl::read_excel(DATA_PATH), stringsAsFactors = FALSE)
raw_long <- raw_long[as.character(raw_long$subject_id) %in% sid_keep, , drop = FALSE]
raw_long$id <- as.numeric(as.factor(as.character(raw_long$subject_id)))
raw_long$time <- to_num_rsflc(raw_long$Obstimes)
raw_long <- raw_long %>%
  dplyr::group_by(id) %>%
  dplyr::mutate(time = time - min(time, na.rm = TRUE)) %>%
  dplyr::ungroup()

itemid_cols_lc <- grep("^itemid_", names(raw_long), value = TRUE)
itemid_cols_lc <- intersect(itemid_cols_lc, names(dat))
itemid_cols_lc <- itemid_cols_lc[vapply(itemid_cols_lc, function(v) {
  sum(!is.na(to_num_rsflc(raw_long[[v]]))) > 0L
}, logical(1))]
for (v in itemid_cols_lc) raw_long[[v]] <- to_num_rsflc(raw_long[[v]])

y_surv_lc <- raw_long %>%
  dplyr::group_by(id) %>%
  dplyr::summarise(
    time = to_num_rsflc(dplyr::first(.data[[time_var]])),
    event = to_num_rsflc(dplyr::first(.data[[event_var]])),
    .groups = "drop"
  )
y_surv_lc$event <- ifelse(!is.na(y_surv_lc$event) & y_surv_lc$event > 0, 1, 0)

fixed_vars_lc <- c()
if ("anchor_age" %in% names(raw_long)) fixed_vars_lc <- c(fixed_vars_lc, "anchor_age")
if ("gender" %in% names(raw_long)) fixed_vars_lc <- c(fixed_vars_lc, "gender")

fixed_base_lc <- raw_long %>%
  dplyr::group_by(id) %>%
  dplyr::summarise(dplyr::across(dplyr::all_of(fixed_vars_lc), first_not_na_rsflc), .groups = "drop")
fixedData_lc <- y_surv_lc %>%
  dplyr::left_join(fixed_base_lc, by = "id") %>%
  dplyr::distinct(id, .keep_all = TRUE)

to_num_local_lc <- function(x) {
  if (is.logical(x)) return(as.numeric(x))
  if (is.numeric(x)) return(as.numeric(x))
  xc <- trimws(as.character(x))
  xc[xc == ""] <- NA_character_
  suppressWarnings(as.numeric(xc))
}
for (v in fixed_vars_lc) {
  xv <- to_num_local_lc(fixedData_lc[[v]])
  if (is.character(fixedData_lc[[v]]) || is.factor(fixedData_lc[[v]])) {
    xv <- as.numeric(as.factor(fixedData_lc[[v]]))
  }
  med <- suppressWarnings(median(xv, na.rm = TRUE))
  if (!is.finite(med)) med <- 0
  xv[is.na(xv)] <- med
  xv <- as.numeric(scale(xv))
  xv[!is.finite(xv)] <- 0
  fixedData_lc[[v]] <- xv
}

fixedData_lc <- fixedData_lc %>%
  dplyr::filter(is.finite(time), !is.na(time), time > 0, !is.na(event))
timeData_lc <- raw_long %>%
  dplyr::filter(id %in% fixedData_lc$id) %>%
  dplyr::select(id, time, dplyr::all_of(itemid_cols_lc))

n_vars_target_lc <- 22L
long_cands_lc <- itemid_cols_lc
if (length(long_cands_lc) + length(fixed_vars_lc) > n_vars_target_lc) {
  n_long <- n_vars_target_lc - length(fixed_vars_lc)
  long_cands_lc <- long_cands_lc[seq_len(min(n_long, length(long_cands_lc)))]
}
timeData_lc <- timeData_lc %>% dplyr::select(id, time, dplyr::all_of(long_cands_lc))
timeVarModel_lc <- lapply(long_cands_lc, function(v) {
  list(model = "linear", fixed = ~1, random = ~1 + time | id)
})
names(timeVarModel_lc) <- long_cands_lc

y_surv_lc <- fixedData_lc %>% dplyr::select(id, time, event)
t0_rsflc <- stats::median(y_surv_lc$time, na.rm = TRUE)
med_surv_rsflc <- stats::median(y_surv_lc$time, na.rm = TRUE)
med_event_rsflc <- as.integer(round(stats::median(y_surv_lc$event, na.rm = TRUE)))

rsflc_shap_vars <- c(long_cands_lc, fixed_vars_lc)
rsflc_shap_vars <- rsflc_shap_vars[rsflc_shap_vars %in% names(dat)]
X_bg_rsflc <- as.data.frame(dat[bg_idx, rsflc_shap_vars, drop = FALSE])
X_ex_rsflc <- as.data.frame(dat[ex_idx, rsflc_shap_vars, drop = FALSE])

fix_scale_lc <- vector("list", length(fixed_vars_lc))
names(fix_scale_lc) <- fixed_vars_lc
fb0 <- raw_long %>%
  dplyr::group_by(id) %>%
  dplyr::summarise(dplyr::across(dplyr::all_of(fixed_vars_lc), first_not_na_rsflc), .groups = "drop") %>%
  dplyr::filter(id %in% fixedData_lc$id)
for (v in fixed_vars_lc) {
  xv0 <- to_num_local_lc(fb0[[v]])
  if (is.character(fb0[[v]]) || is.factor(fb0[[v]])) xv0 <- as.numeric(as.factor(fb0[[v]]))
  med <- suppressWarnings(median(xv0, na.rm = TRUE))
  if (!is.finite(med)) med <- 0
  xv0[is.na(xv0)] <- med
  mu <- mean(xv0, na.rm = TRUE)
  sdv <- stats::sd(xv0, na.rm = TRUE)
  if (!is.finite(sdv) || sdv < 1e-8) sdv <- 1
  fix_scale_lc[[v]] <- list(mean = mu, sd = sdv)
}

fit_rsflc <- tryCatch(
  {
    mtry_lc <- max(1L, floor((length(long_cands_lc) + length(fixed_vars_lc)) / 3))
    DynForest::dynforest(
      timeData = as.data.frame(timeData_lc),
      fixedData = as.data.frame(fixedData_lc),
      idVar = "id",
      timeVar = "time",
      timeVarModel = timeVarModel_lc,
      Y = list(type = "surv", Y = as.data.frame(y_surv_lc)),
      ntree = RSFLC_NTREE,
      mtry = mtry_lc,
      nodesize = 10L,
      minsplit = 2L,
      nsplit_option = "quantile",
      ncores = 1L,
      verbose = FALSE
    )
  },
  error = function(e) e
)

if (inherits(fit_rsflc, "error")) {
  message("RSFLC(DynForest) 拟合失败: ", conditionMessage(fit_rsflc))
  openxlsx::write.xlsx(
    data.frame(error = conditionMessage(fit_rsflc), stringsAsFactors = FALSE),
    file.path(OUT_DIR, "RSFLC_shap_error.xlsx"),
    overwrite = TRUE
  )
} else {
  rsflc_bundle <- list(
    fit = fit_rsflc,
    long_cands = long_cands_lc,
    fixed_vars = fixed_vars_lc,
    t0 = t0_rsflc,
    med_surv = med_surv_rsflc,
    med_event = med_event_rsflc,
    fix_scale = fix_scale_lc
  )

  pred_wrapper_rsflc <- function(object, newdata) {
    nd <- as.data.frame(newdata)
    n <- nrow(nd)
    syn <- seq_len(n)
    td <- data.frame(id = syn, time = 0)
    for (v in object$long_cands) td[[v]] <- nd[[v]]
    fd <- data.frame(id = syn, time = object$med_surv, event = object$med_event)
    for (v in object$fixed_vars) {
      xv <- to_num_local_lc(nd[[v]])
      if (v == "gender" && (is.factor(nd[[v]]) || is.character(nd[[v]]))) {
        xv <- as.numeric(as.factor(nd[[v]]))
      }
      sp <- object$fix_scale[[v]]
      xv[is.na(xv)] <- sp$mean
      fd[[v]] <- (xv - sp$mean) / sp$sd
      fd[[v]][!is.finite(fd[[v]])] <- 0
    }
    pr <- predict(
      object$fit,
      timeData = td,
      fixedData = fd,
      idVar = "id",
      timeVar = "time",
      t0 = object$t0
    )
    pm <- pr$pred_indiv
    as.numeric(pm[, ncol(pm)])
  }

  message(sprintf(
    "RSFLC SHAP: 特征 %d 个, 背景 %d, 解释 %d, nsim=%d",
    length(rsflc_shap_vars), bg_n, ex_n, NSIM
  ))
  shap_rsflc <- fastshap::explain(
    rsflc_bundle,
    X = X_bg_rsflc,
    newdata = X_ex_rsflc,
    nsim = NSIM,
    pred_wrapper = pred_wrapper_rsflc,
    adjust = TRUE
  )
  colnames(shap_rsflc) <- rsflc_shap_vars

  mean_abs_rsflc <- sort(colMeans(abs(shap_rsflc)), decreasing = TRUE)
  imp_rsflc_df <- data.frame(
    feature = names(mean_abs_rsflc),
    mean_abs_shap = as.numeric(mean_abs_rsflc),
    row.names = NULL
  )
  shap_rsflc_df <- data.frame(
    subject_id = dat[["subject_id"]][ex_idx],
    Obstimes = dat[["Obstimes"]][ex_idx],
    shap_rsflc,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  shap_rsflc_df <- shap_rsflc_df[order(shap_rsflc_df[["subject_id"]]), , drop = FALSE]
  rownames(shap_rsflc_df) <- NULL

  out_rsflc_v <- file.path(OUT_DIR, "RSFLC_shap_values.xlsx")
  out_rsflc_i <- file.path(OUT_DIR, "RSFLC_shap_mean_abs_importance.xlsx")
  openxlsx::write.xlsx(shap_rsflc_df, out_rsflc_v, overwrite = TRUE)
  openxlsx::write.xlsx(imp_rsflc_df, out_rsflc_i, overwrite = TRUE)
  message("已写出: ", normalizePath(out_rsflc_v, winslash = "/"))
  message("已写出: ", normalizePath(out_rsflc_i, winslash = "/"))
  cat("\n=== RSFLC: 各变量平均 |SHAP| ===\n")
  print(imp_rsflc_df, row.names = FALSE)
}

# =============================================================================
# JM：joineRML::mjoint（与 0323_JM.R 一致：单纵向 Y + t + 固定协变量；生存 ~1）
# SHAP 目标：gamma * clip(X %*% beta)，与 0323_JM.R 测试集风险一致（随机效应=0）
# =============================================================================
jm_raw <- as.data.frame(readxl::read_excel(DATA_PATH), stringsAsFactors = FALSE)
jm_raw <- jm_raw[as.character(jm_raw$subject_id) %in% sid_keep, , drop = FALSE]
jm_raw$ID <- as.numeric(as.factor(as.character(jm_raw$subject_id)))
jm_raw$event <- to_num_rsflc(jm_raw[[event_var]])
jm_raw$event <- ifelse(is.na(jm_raw$event), NA, ifelse(jm_raw$event > 0, 1, 0))
jm_raw$obs_time <- to_num_rsflc(jm_raw[[time_var]])
jm_raw$t <- to_num_rsflc(jm_raw$Obstimes) / 24
if (all(is.na(jm_raw$t))) jm_raw$t <- to_num_rsflc(jm_raw$Obstimes)

obs_los <- jm_raw$obs_time
q95 <- stats::quantile(obs_los, 0.95, na.rm = TRUE)
if (!is.finite(q95) || q95 <= 2 || mean(obs_los > 0, na.rm = TRUE) < 0.8) {
  jm_raw <- jm_raw %>%
    dplyr::group_by(ID) %>%
    dplyr::mutate(obs_time = max(t, na.rm = TRUE)) %>%
    dplyr::ungroup()
  jm_raw$obs_time <- jm_raw$obs_time + 1e-3
}

jm_raw <- jm_raw %>%
  dplyr::filter(!is.na(ID), !is.na(t), !is.na(obs_time), !is.na(event)) %>%
  dplyr::filter(obs_time > 0, t <= obs_time)

drop_cat_jm <- c(
  "itemid_admission_type", "itemid_admission_location", "itemid_discharge_location",
  "itemid_insurance", "itemid_language", "itemid_marital_status", "itemid_race",
  "itemid_first_careunit", "itemid_los_hosp_days"
)
long_cands_jm <- grep("^itemid_", names(jm_raw), value = TRUE)
long_cands_jm <- setdiff(long_cands_jm, c(
  drop_cat_jm, "itemid_anchor_age", "itemid_anchor_year",
  "itemid_gender", "itemid_anchor_year_group"
))
score_var_jm <- function(v) {
  y <- to_num_rsflc(jm_raw[[v]])
  df_tmp <- data.frame(ID = jm_raw$ID, y = y)
  df_tmp <- df_tmp[!is.na(df_tmp$y), , drop = FALSE]
  n_id2 <- sum(table(df_tmp$ID) >= 2)
  sum(!is.na(y)) + 10 * n_id2
}
long_cands_jm <- long_cands_jm[order(vapply(long_cands_jm, score_var_jm, numeric(1)), decreasing = TRUE)]
y_var_jm <- long_cands_jm[1]
age_var_jm <- c("itemid_anchor_age", "anchor_age")[match(TRUE, c("itemid_anchor_age", "anchor_age") %in% names(jm_raw))]
sex_var_jm <- c("itemid_gender", "gender")[match(TRUE, c("itemid_gender", "gender") %in% names(jm_raw))]
if (is.na(age_var_jm)) age_var_jm <- long_cands_jm[1]
if (is.na(sex_var_jm)) sex_var_jm <- long_cands_jm[min(2, length(long_cands_jm))]
extra_jm <- setdiff(long_cands_jm, c(y_var_jm, age_var_jm, sex_var_jm))
extra_jm <- extra_jm[vapply(extra_jm, function(v) {
  x <- to_num_rsflc(jm_raw[[v]])
  sum(!is.na(x)) > 0 && length(unique(x[!is.na(x)])) > 1
}, logical(1))]
n_target_jm <- 22L
n_extra_jm <- min(n_target_jm - 2L, length(extra_jm))
extra_vars_jm <- extra_jm[seq_len(max(1L, n_extra_jm))]
fixed_vars_jm <- c(age_var_jm, sex_var_jm, extra_vars_jm)

to_num01_jm <- function(x) {
  if (is.logical(x)) return(as.numeric(x))
  if (is.numeric(x)) return(as.numeric(x))
  xc <- trimws(as.character(x))
  xc[xc == ""] <- NA_character_
  xn <- suppressWarnings(as.numeric(xc))
  if (sum(!is.na(xn)) >= max(10, floor(0.5 * sum(!is.na(xc))))) return(xn)
  as.numeric(as.factor(xc))
}

data_jm <- jm_raw %>%
  dplyr::mutate(Y = to_num_rsflc(.data[[y_var_jm]])) %>%
  dplyr::filter(!is.na(Y)) %>%
  dplyr::group_by(ID) %>%
  dplyr::arrange(t, .by_group = TRUE) %>%
  dplyr::mutate(t = as.numeric(t) + (dplyr::row_number() - 1) * 1e-4) %>%
  dplyr::ungroup() %>%
  dplyr::filter(t < obs_time)
data_jm$Y <- as.numeric(scale(data_jm$Y))

prep_numeric_var_jm <- function(df, vname, ref_mean = NULL, ref_sd = NULL) {
  x <- to_num01_jm(df[[vname]])
  med <- suppressWarnings(median(x, na.rm = TRUE))
  if (!is.finite(med)) med <- 0
  x[is.na(x)] <- med
  if (!is.null(ref_mean) && !is.null(ref_sd) && is.finite(ref_sd) && ref_sd > 1e-8) {
    (x - ref_mean) / ref_sd
  } else {
    as.numeric(scale(x))
  }
}

jm_d <- data_jm
scale_params_jm <- list()
for (vv in fixed_vars_jm) {
  if (vv %in% names(jm_d)) {
    xv <- to_num01_jm(jm_d[[vv]])
    med <- suppressWarnings(median(xv, na.rm = TRUE))
    if (!is.finite(med)) med <- 0
    xv[is.na(xv)] <- med
    scale_params_jm[[vv]] <- list(mean = mean(xv, na.rm = TRUE), sd = {
      s <- stats::sd(xv, na.rm = TRUE)
      if (is.finite(s) && s > 1e-8) s else 1
    })
    jm_d[[vv]] <- prep_numeric_var_jm(jm_d, vv)
  }
}
lme_fixed_use_jm <- c("t", fixed_vars_jm[fixed_vars_jm %in% names(jm_d)])
form_fixed_jm <- as.formula(paste("Y ~", paste(lme_fixed_use_jm, collapse = " + ")))
t0_jm <- stats::median(jm_d$obs_time[jm_d$obs_time > 0], na.rm = TRUE)
if (!is.finite(t0_jm)) t0_jm <- stats::median(jm_d$obs_time, na.rm = TRUE)

fit_jm <- tryCatch(
  joineRML::mjoint(
    formLongFixed = list("Y" = form_fixed_jm),
    formLongRandom = list("Y" = ~1 | ID),
    formSurv = Surv(obs_time, event) ~ 1,
    data = jm_d,
    timeVar = "t",
    control = list(
      tol = 0.01,
      maxit = JM_MJOINT_MAXIT,
      verbose = FALSE
    )
  ),
  error = function(e) e
)

if (inherits(fit_jm, "error")) {
  message("JM(mjoint) 拟合失败: ", conditionMessage(fit_jm))
  openxlsx::write.xlsx(
    data.frame(error = conditionMessage(fit_jm), stringsAsFactors = FALSE),
    file.path(OUT_DIR, "JM_shap_error.xlsx"),
    overwrite = TRUE
  )
} else {
  beta_raw <- fit_jm$coefficients$beta
  beta_jm <- as.numeric(beta_raw)
  bn_jm <- names(beta_raw)
  if (length(bn_jm) == 0) bn_jm <- paste0("b", seq_along(beta_jm))
  gamma_jm <- as.numeric(fit_jm$coefficients$gamma)
  if (length(gamma_jm) > 1) gamma_jm <- gamma_jm[length(gamma_jm)]
  rhs_form_jm <- as.formula(paste("~", paste(lme_fixed_use_jm, collapse = " + ")))

  align_beta_jm <- function(X, bet) {
    cx <- colnames(X)
    if (length(bet) == length(cx)) return(bet)
    out <- numeric(length(cx))
    b_alt <- gsub("^Y\\.", "", bn_jm)
    for (i in seq_along(cx)) {
      j <- match(cx[i], bn_jm)
      if (is.na(j)) j <- match(cx[i], b_alt)
      if (!is.na(j) && j <= length(bet)) out[i] <- bet[j] else out[i] <- 0
    }
    out
  }

  jm_shap_vars <- setdiff(lme_fixed_use_jm, "t")
  jm_shap_vars <- jm_shap_vars[jm_shap_vars %in% names(dat)]
  X_bg_jm <- as.data.frame(dat[bg_idx, jm_shap_vars, drop = FALSE])
  X_ex_jm <- as.data.frame(dat[ex_idx, jm_shap_vars, drop = FALSE])

  jm_bundle <- list(
    gamma = gamma_jm,
    beta_named = beta_raw,
    bn = bn_jm,
    rhs_form = rhs_form_jm,
    fixed_vars = fixed_vars_jm,
    scale_params = scale_params_jm,
    t0 = t0_jm,
    align_beta = align_beta_jm
  )

  pred_wrapper_jm <- function(object, newdata) {
    nd <- as.data.frame(newdata)
    n <- nrow(nd)
    nd$t <- rep(object$t0, n)
    for (vv in object$fixed_vars) {
      if (!vv %in% names(nd)) next
      sp <- object$scale_params[[vv]]
      xv <- to_num01_jm(nd[[vv]])
      med <- suppressWarnings(median(xv, na.rm = TRUE))
      if (!is.finite(med)) med <- sp$mean
      xv[is.na(xv)] <- med
      nd[[vv]] <- (xv - sp$mean) / sp$sd
      nd[[vv]][!is.finite(nd[[vv]])] <- 0
    }
    X <- stats::model.matrix(object$rhs_form, data = nd)
    b_use <- object$align_beta(X, as.numeric(object$beta_named))
    eta <- as.numeric(X %*% b_use)
    eta <- pmax(pmin(eta, 20), -20)
    as.numeric(object$gamma * eta)
  }

  message(sprintf(
    "JM SHAP: 纵向Y=%s; 解释特征 %d 个; 背景 %d, 解释 %d, nsim=%d",
    y_var_jm, length(jm_shap_vars), bg_n, ex_n, NSIM
  ))
  shap_jm <- fastshap::explain(
    jm_bundle,
    X = X_bg_jm,
    newdata = X_ex_jm,
    nsim = NSIM,
    pred_wrapper = pred_wrapper_jm,
    adjust = TRUE
  )
  colnames(shap_jm) <- jm_shap_vars

  mean_abs_jm <- sort(colMeans(abs(shap_jm)), decreasing = TRUE)
  imp_jm_df <- data.frame(
    feature = names(mean_abs_jm),
    mean_abs_shap = as.numeric(mean_abs_jm),
    row.names = NULL
  )
  shap_jm_df <- data.frame(
    subject_id = dat[["subject_id"]][ex_idx],
    Obstimes = dat[["Obstimes"]][ex_idx],
    shap_jm,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  shap_jm_df <- shap_jm_df[order(shap_jm_df[["subject_id"]]), , drop = FALSE]
  rownames(shap_jm_df) <- NULL

  out_jm_v <- file.path(OUT_DIR, "JM_shap_values.xlsx")
  out_jm_i <- file.path(OUT_DIR, "JM_shap_mean_abs_importance.xlsx")
  meta_jm <- data.frame(
    item = c("longitudinal_Y", "t0_median", "gamma", "maxit"),
    value = c(y_var_jm, as.character(t0_jm), as.character(gamma_jm), as.character(JM_MJOINT_MAXIT)),
    stringsAsFactors = FALSE
  )
  openxlsx::write.xlsx(
    list(shap_values = shap_jm_df, mean_abs = imp_jm_df, meta = meta_jm),
    out_jm_v,
    overwrite = TRUE
  )
  openxlsx::write.xlsx(imp_jm_df, out_jm_i, overwrite = TRUE)
  message("已写出: ", normalizePath(out_jm_v, winslash = "/"))
  message("已写出: ", normalizePath(out_jm_i, winslash = "/"))
  cat("\n=== JM: 各变量平均 |SHAP| ===\n")
  print(imp_jm_df, row.names = FALSE)
}
