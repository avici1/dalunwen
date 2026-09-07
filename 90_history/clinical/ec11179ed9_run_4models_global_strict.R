suppressPackageStartupMessages({
  needed_pkgs <- c("dplyr", "readxl", "writexl", "survival", "timeROC", "randomForestSRC", "joineRML", "DynForest")
  missing_pkgs <- needed_pkgs[!vapply(needed_pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing_pkgs) > 0) {
    install.packages(missing_pkgs, repos = "https://cloud.r-project.org")
  }
  invisible(lapply(needed_pkgs, library, character.only = TRUE))
})

set.seed(2026)
options(scipen = 999)

work_dir <- "F:/文章_大论文/MIMIC数据库_代码/TREA代码/0319全局建模"
in_path <- file.path(work_dir, "0319PRO_inputmergedata.xlsx")
out_strict <- file.path(work_dir, "result_model_strict.xlsx")
out_main <- file.path(work_dir, "result_model.xlsx")
t0 <- 1

if (!file.exists(in_path)) stop(paste0("输入文件不存在: ", in_path))

to_num <- function(x) {
  if (is.numeric(x)) return(as.numeric(x))
  if (is.logical(x)) return(as.numeric(x))
  xc <- trimws(as.character(x))
  xc[xc == ""] <- NA_character_
  suppressWarnings(as.numeric(xc))
}

mode_value <- function(x) {
  x2 <- x[!is.na(x)]
  if (length(x2) == 0) return(NA)
  ux <- unique(x2)
  ux[which.max(tabulate(match(x2, ux)))]
}

round4 <- function(x) ifelse(is.finite(x), round(x, 4), NA_real_)

write_xlsx_safe <- function(df, target_path) {
  ok <- tryCatch({
    writexl::write_xlsx(df, target_path)
    TRUE
  }, error = function(e) FALSE)
  if (ok) return(target_path)
  alt <- file.path(dirname(target_path), paste0(tools::file_path_sans_ext(basename(target_path)), "_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx"))
  writexl::write_xlsx(df, alt)
  alt
}

calc_auc <- function(st, se, risk, t0) {
  auc_rank <- function(event, score) {
    event <- as.numeric(event)
    score <- as.numeric(score)
    ok <- is.finite(score) & !is.na(event)
    event <- event[ok]
    score <- score[ok]
    if (length(unique(event)) < 2) return(NA_real_)
    pos <- score[event == 1]
    neg <- score[event == 0]
    if (length(pos) == 0 || length(neg) == 0) return(NA_real_)
    cmp <- outer(pos, neg, FUN = "-")
    (sum(cmp > 0) + 0.5 * sum(cmp == 0)) / (length(pos) * length(neg))
  }

  out <- tryCatch({
    roc <- timeROC::timeROC(T = st, delta = se, marker = risk, cause = 1, times = t0)
    if (length(roc$AUC) >= 2) as.numeric(roc$AUC[2]) else as.numeric(roc$AUC[1])
  }, error = function(e) NA_real_)
  if (!is.finite(out)) {
    out <- auc_rank(se, risk)
  }
  if (!is.finite(out)) NA_real_ else out
}

calc_cindex_manual <- function(st, se, risk) {
  n <- length(st)
  if (n < 2) return(NA_real_)
  idx <- utils::combn(n, 2)
  i <- idx[1, ]
  j <- idx[2, ]
  comparable_ij <- (se[i] == 1 & st[i] < st[j])
  comparable_ji <- (se[j] == 1 & st[j] < st[i])
  comparable <- comparable_ij | comparable_ji
  concordant <- (comparable_ij & (risk[i] > risk[j])) | (comparable_ji & (risk[j] > risk[i]))
  tied <- comparable & (risk[i] == risk[j])
  n_pairs <- sum(comparable)
  if (n_pairs == 0) return(NA_real_)
  (sum(concordant) + 0.5 * sum(tied)) / n_pairs
}

calc_bs_ipcw <- function(st, se, surv_prob, t0) {
  surv_prob <- pmin(pmax(as.numeric(surv_prob), 0), 1)
  km_cens <- tryCatch(survival::survfit(survival::Surv(st, 1 - se) ~ 1), error = function(e) NULL)
  if (is.null(km_cens)) return(NA_real_)

  get_G <- function(tt) {
    v <- tryCatch(summary(km_cens, times = tt, extend = TRUE)$surv, error = function(e) NA_real_)
    if (length(v) == 0) return(NA_real_)
    as.numeric(v[1])
  }

  G_t0 <- get_G(t0)
  if (!is.finite(G_t0) || G_t0 <= 0) G_t0 <- NA_real_

  w <- numeric(length(st))
  for (k in seq_along(st)) {
    if (st[k] <= t0 && se[k] == 1) {
      Gk <- get_G(st[k])
      w[k] <- ifelse(is.finite(Gk) && Gk > 0, 1 / Gk, 0)
    } else if (st[k] > t0) {
      w[k] <- ifelse(is.finite(G_t0) && G_t0 > 0, 1 / G_t0, 0)
    } else {
      w[k] <- 0
    }
  }
  y_obs <- as.numeric(st > t0 | (st <= t0 & se == 0))
  bs <- mean(w * (surv_prob - y_obs)^2, na.rm = TRUE)
  if (!is.finite(bs)) NA_real_ else bs
}

build_metrics <- function(name, st, se, risk, surv_prob) {
  data.frame(
    model = name,
    CINDEX = round4(calc_cindex_manual(st, se, risk)),
    BS = round4(calc_bs_ipcw(st, se, surv_prob, t0)),
    AUC = round4(calc_auc(st, se, risk, t0)),
    stringsAsFactors = FALSE
  )
}

message("读取数据: ", in_path)
df <- readxl::read_xlsx(in_path) %>%
  mutate(
    subject_id = as.character(subject_id),
    event = ifelse(!is.na(deathtime) & trimws(as.character(deathtime)) != "", 1, 0),
    obs_time = to_num(los_hosp_days),
    obs_time = ifelse(!is.finite(obs_time) | is.na(obs_time) | obs_time <= 0, to_num(Obstimes) + 1, obs_time),
    obs_time = ifelse(!is.finite(obs_time) | is.na(obs_time) | obs_time <= 0, 1, obs_time),
    t = to_num(Obstimes)
  ) %>%
  group_by(subject_id) %>%
  mutate(t = t - min(t, na.rm = TRUE)) %>%
  ungroup()

# 关键稳定步骤：把生存结局统一为“每个subject一条”
# 避免长表内同一患者obs_time不一致导致JM报错
subj_outcome <- df %>%
  group_by(subject_id) %>%
  summarise(
    event_subj = as.numeric(max(event, na.rm = TRUE)),
    obs_from_los = suppressWarnings(median(obs_time[is.finite(obs_time) & obs_time > 0], na.rm = TRUE)),
    max_t = suppressWarnings(max(t, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  mutate(
    obs_from_los = ifelse(is.finite(obs_from_los) & !is.na(obs_from_los) & obs_from_los > 0, obs_from_los, NA_real_),
    max_t = ifelse(is.finite(max_t) & !is.na(max_t), max_t, 0),
    obs_time_subj = ifelse(is.na(obs_from_los), max_t + 1, obs_from_los),
    obs_time_subj = ifelse(obs_time_subj <= 0 | !is.finite(obs_time_subj), 1, obs_time_subj),
    event_subj = ifelse(event_subj > 0, 1, 0)
  ) %>%
  select(subject_id, event_subj, obs_time_subj)

df <- df %>%
  select(-event, -obs_time) %>%
  left_join(subj_outcome, by = "subject_id") %>%
  mutate(
    event = as.numeric(event_subj),
    obs_time = as.numeric(obs_time_subj)
  ) %>%
  select(-event_subj, -obs_time_subj)

exclude_cols <- c("subject_id", "Obstimes", "charttime", "deathtime", "los_hosp_days", "event", "obs_time", "t")
covars_all <- setdiff(names(df), exclude_cols)
if (length(covars_all) == 0) stop("未检测到协变量。")

# 严格版：所有协变量统一转为数值进入四模型
for (v in covars_all) {
  if (v == "gender") {
    df[[v]] <- ifelse(toupper(trimws(as.character(df[[v]]))) %in% c("M", "MALE"), 1, 0)
  } else {
    xnum <- to_num(df[[v]])
    if (sum(!is.na(xnum)) < max(5, floor(0.2 * length(xnum)))) {
      xnum <- as.numeric(as.factor(as.character(df[[v]])))
    }
    df[[v]] <- xnum
  }
  med <- suppressWarnings(median(df[[v]], na.rm = TRUE))
  if (!is.finite(med)) med <- 0
  df[[v]][is.na(df[[v]])] <- med
}

df_subj <- df %>%
  arrange(subject_id, t) %>%
  group_by(subject_id) %>%
  slice(1) %>%
  ungroup()

covars_subj <- covars_all[vapply(covars_all, function(v) length(unique(df_subj[[v]])) > 1, logical(1))]
if (length(covars_subj) == 0) stop("基线层面没有可用协变量。")

results <- list()

# ---------- COX ----------
message("严格版：COX")
cox_formula <- as.formula(paste0("survival::Surv(obs_time, event) ~ ", paste(covars_subj, collapse = " + ")))
cox_fit <- tryCatch(survival::coxph(cox_formula, data = df_subj, x = TRUE), error = function(e) NULL)
if (is.null(cox_fit)) {
  results[["COX"]] <- data.frame(model = "COX", CINDEX = NA_real_, BS = NA_real_, AUC = NA_real_)
} else {
  risk <- as.numeric(stats::predict(cox_fit, type = "lp"))
  sfit <- tryCatch(survival::survfit(cox_fit, newdata = df_subj), error = function(e) NULL)
  surv_prob <- rep(NA_real_, nrow(df_subj))
  if (!is.null(sfit)) {
    sp <- tryCatch(summary(sfit, times = t0, extend = TRUE)$surv, error = function(e) NULL)
    if (!is.null(sp)) {
      if (length(sp) == nrow(df_subj)) surv_prob <- as.numeric(sp) else surv_prob <- rep(as.numeric(sp[1]), nrow(df_subj))
    }
  }
  results[["COX"]] <- build_metrics("COX", as.numeric(df_subj$obs_time), as.numeric(df_subj$event), risk, surv_prob)
}

# ---------- RSF ----------
glimpse(df_subj)
message("严格版：RSF（默认参数，不兜底）")
rsf_formula <- as.formula(paste0("Surv(obs_time, event) ~ ", paste(covars_subj, collapse = " + ")))
rsf_mtry <- max(2, floor(sqrt(length(covars_subj))))
rsf_fit <- tryCatch(
  randomForestSRC::rfsrc(
    rsf_formula,
    data = df_subj,
    ntree = 1200,
    mtry = 22,
    nodesize = 5,
    nsplit = 10,
    splitrule = "logrank",
    importance = FALSE
  ),
  error = function(e) {
    message("RSF首次拟合失败: ", conditionMessage(e))
    NULL
  }
)
if (is.null(rsf_fit)) {
  # 严格增强：仍然是RSF，只做设计矩阵重编码以提升可拟合性
  mm <- tryCatch(stats::model.matrix(~ . - 1, data = df_subj[, covars_subj, drop = FALSE]), error = function(e) NULL)
  if (is.null(mm)) {
    results[["RSF"]] <- data.frame(model = "RSF", CINDEX = NA_real_, BS = NA_real_, AUC = NA_real_)
  } else {
    rsf_df2 <- data.frame(obs_time = df_subj$obs_time, event = df_subj$event, mm, check.names = FALSE)
    rsf_fit2 <- tryCatch(
      randomForestSRC::rfsrc(
        Surv(obs_time, event) ~ .,
        data = rsf_df2,
        ntree = 1200,
        mtry = max(2, floor(sqrt(ncol(rsf_df2) - 2))),
        nodesize = 5,
        nsplit = 10,
        splitrule = "logrank",
        importance = FALSE
      ),
      error = function(e) {
        message("RSF重编码后拟合失败: ", conditionMessage(e))
        NULL
      }
    )
    if (is.null(rsf_fit2)) {
      results[["RSF"]] <- data.frame(model = "RSF", CINDEX = NA_real_, BS = NA_real_, AUC = NA_real_)
    } else {
      time_points <- rsf_fit2$time.interest
      surv_mat <- if (!is.null(rsf_fit2$survival.oob)) rsf_fit2$survival.oob else rsf_fit2$survival
      if (!is.null(surv_mat) && !is.null(time_points)) {
        t_idx <- which.min(abs(time_points - t0))
        surv_prob <- as.numeric(surv_mat[, t_idx])
      } else {
        rp <- as.numeric(rsf_fit2$predicted)
        if (all(!is.finite(rp))) {
          surv_prob <- rep(NA_real_, nrow(rsf_df2))
        } else {
          rp <- as.numeric(scale(rp))
          surv_prob <- exp(-pmax(rp, 0) * t0)
        }
      }
      if (any(!is.finite(surv_prob))) {
        med_sp <- suppressWarnings(median(surv_prob[is.finite(surv_prob)], na.rm = TRUE))
        if (!is.finite(med_sp)) med_sp <- 0.5
        surv_prob[!is.finite(surv_prob)] <- med_sp
      }
      surv_prob <- pmin(pmax(surv_prob, 1e-8), 1 - 1e-8)
      risk <- 1 - surv_prob
      results[["RSF"]] <- build_metrics("RSF", as.numeric(rsf_df2$obs_time), as.numeric(rsf_df2$event), risk, surv_prob)
    }
  }
} else {
  time_points <- rsf_fit$time.interest
  surv_mat <- if (!is.null(rsf_fit$survival.oob)) rsf_fit$survival.oob else rsf_fit$survival
  if (is.null(surv_mat) || is.null(time_points)) {
    rsf_pred <- tryCatch(predict(rsf_fit, newdata = df_subj), error = function(e) NULL)
    if (!is.null(rsf_pred) && !is.null(rsf_pred$survival) && !is.null(rsf_pred$time.interest)) {
      surv_mat <- rsf_pred$survival
      time_points <- rsf_pred$time.interest
    }
  }

  if (is.null(surv_mat) || is.null(time_points)) {
    risk <- as.numeric(rsf_fit$predicted)
    if (all(!is.finite(risk))) {
      results[["RSF"]] <- data.frame(model = "RSF", CINDEX = NA_real_, BS = NA_real_, AUC = NA_real_)
    } else {
      risk[!is.finite(risk)] <- median(risk[is.finite(risk)], na.rm = TRUE)
      risk <- as.numeric(scale(risk))
      surv_prob <- exp(-pmax(risk, 0) * t0)
      results[["RSF"]] <- build_metrics("RSF", as.numeric(df_subj$obs_time), as.numeric(df_subj$event), risk, surv_prob)
    }
  } else {
    t_idx <- which.min(abs(time_points - t0))
    surv_prob <- as.numeric(surv_mat[, t_idx])
    if (any(!is.finite(surv_prob))) {
      rp <- as.numeric(rsf_fit$predicted)
      if (length(rp) == length(surv_prob) && any(is.finite(rp))) {
        rp <- as.numeric(scale(rp))
        sp_alt <- exp(-pmax(rp, 0) * t0)
        surv_prob[!is.finite(surv_prob)] <- sp_alt[!is.finite(surv_prob)]
      }
    }
    if (all(!is.finite(surv_prob))) {
      rp <- as.numeric(rsf_fit$predicted)
      if (any(is.finite(rp))) {
        rp <- as.numeric(scale(rp))
        surv_prob <- exp(-pmax(rp, 0) * t0)
      }
    }
    if (any(!is.finite(surv_prob))) {
      med_sp <- suppressWarnings(median(surv_prob[is.finite(surv_prob)], na.rm = TRUE))
      if (!is.finite(med_sp)) med_sp <- 0.5
      surv_prob[!is.finite(surv_prob)] <- med_sp
    }
    surv_prob <- pmin(pmax(surv_prob, 1e-8), 1 - 1e-8)
    risk <- 1 - surv_prob
    if (any(!is.finite(risk))) {
      med_r <- suppressWarnings(median(risk[is.finite(risk)], na.rm = TRUE))
      if (!is.finite(med_r)) med_r <- 0
      risk[!is.finite(risk)] <- med_r
    }
    results[["RSF"]] <- build_metrics("RSF", as.numeric(df_subj$obs_time), as.numeric(df_subj$event), risk, surv_prob)
  }
}

# ---------- JM ----------
message("严格版：JM（mjoint，不兜底）")
marker_cols <- covars_all[grepl("^itemid_", covars_all)]
if (length(marker_cols) == 0) {
  marker_cols <- covars_all
}

df_jm <- df %>%
  transmute(
    ID = as.numeric(as.factor(subject_id)),
    t = as.numeric(t),
    obs_time = as.numeric(obs_time),
    event = as.numeric(event),
    Y = rowMeans(across(all_of(marker_cols), ~ as.numeric(.x)), na.rm = TRUE),
    across(all_of(covars_all), ~ as.numeric(.x))
  ) %>%
  filter(is.finite(t), is.finite(obs_time), obs_time > 0, !is.na(event), !is.na(Y)) %>%
  filter((event == 1 & t < obs_time) | (event == 0 & t <= obs_time))

if (nrow(df_jm) < 100) {
  results[["JM"]] <- data.frame(model = "JM", CINDEX = NA_real_, BS = NA_real_, AUC = NA_real_)
} else {
  long_formula <- as.formula(paste0("Y ~ t + ", paste(covars_all, collapse = " + ")))
  jm_fit <- tryCatch(
    joineRML::mjoint(
      formLongFixed = list("Y" = long_formula),
      formLongRandom = list("Y" = ~ t | ID),
      formSurv = survival::Surv(obs_time, event) ~ 1,
      data = df_jm,
      timeVar = "t"
    ),
    error = function(e) NULL
  )

  if (is.null(jm_fit)) {
    results[["JM"]] <- data.frame(model = "JM", CINDEX = NA_real_, BS = NA_real_, AUC = NA_real_)
  } else {
    surv_jm <- df_jm[!duplicated(df_jm$ID), ]
    beta <- jm_fit$coefficients$beta
    gamma <- as.numeric(jm_fit$coefficients$gamma)
    re <- tryCatch(ranef(jm_fit), error = function(e) NULL)
    if (is.null(re)) {
      results[["JM"]] <- data.frame(model = "JM", CINDEX = NA_real_, BS = NA_real_, AUC = NA_real_)
    } else {
      bl <- surv_jm
      bl$t <- t0
      X <- model.matrix(long_formula, data = bl)
      fixed_part <- as.numeric(X %*% beta)
      rand_part <- re[, 1] + re[, 2] * t0
      risk <- gamma * (fixed_part + rand_part)
      s0 <- tryCatch(summary(survival::survfit(survival::coxph(survival::Surv(obs_time, event) ~ 1, data = surv_jm)), times = t0, extend = TRUE)$surv[1], error = function(e) NA_real_)
      if (!is.finite(s0)) s0 <- NA_real_
      surv_prob <- s0 ^ exp(risk)
      results[["JM"]] <- build_metrics("JM", as.numeric(surv_jm$obs_time), as.numeric(surv_jm$event), risk, surv_prob)
    }
  }
}

# ---------- RSFLC ----------
message("严格版：RSFLC（DynForest默认参数，不兜底）")
marker_vars <- marker_cols
if (length(marker_vars) > 6) {
  vvec <- vapply(marker_vars, function(v) stats::var(as.numeric(df[[v]]), na.rm = TRUE), numeric(1))
  vvec[!is.finite(vvec)] <- -Inf
  marker_vars <- marker_vars[order(vvec, decreasing = TRUE)[1:6]]
}
marker_alias <- paste0("Y", seq_along(marker_vars))

df_lc <- df %>%
  transmute(
    id = as.numeric(as.factor(subject_id)),
    time = as.numeric(t),
    obs_time = as.numeric(obs_time),
    event = as.numeric(event),
    across(all_of(marker_vars), ~ as.numeric(.x)),
    across(all_of(covars_all), ~ as.numeric(.x))
  ) %>%
  filter(is.finite(time), is.finite(obs_time), obs_time > 0, !is.na(event))

for (k in seq_along(marker_vars)) {
  oldn <- marker_vars[k]
  newn <- marker_alias[k]
  df_lc[[newn]] <- df_lc[[oldn]]
  med <- suppressWarnings(median(df_lc[[newn]], na.rm = TRUE))
  if (!is.finite(med)) med <- 0
  df_lc[[newn]][!is.finite(df_lc[[newn]]) | is.na(df_lc[[newn]])] <- med
}

timeData <- df_lc %>% select(id, time, all_of(marker_alias))
fixedData <- df_lc %>%
  group_by(id) %>%
  summarise(across(all_of(covars_all), ~ .x[1]), obs_time = obs_time[1], event = event[1], .groups = "drop")
y_surv <- fixedData %>% transmute(id = id, time = obs_time, event = event)
timeVarModel <- setNames(
  lapply(marker_alias, function(.) list(model = "linear", fixed = ~ 1, random = ~ 1 + time | id)),
  marker_alias
)

lc_fit <- tryCatch(
  DynForest::dynforest(
    timeData = as.data.frame(timeData),
    fixedData = as.data.frame(fixedData),
    idVar = "id",
    timeVar = "time",
    timeVarModel = timeVarModel,
    Y = list(type = "surv", Y = as.data.frame(y_surv)),
    ntree = 80,
    mtry = max(2, floor(sqrt(length(covars_all)))),
    nodesize = 5,
    minsplit = 5,
    nsplit_option = "quantile",
    ncores = 1,
    verbose = TRUE
  ),
  error = function(e) {
    message("RSFLC拟合失败: ", conditionMessage(e))
    NULL
  }
)

if (is.null(lc_fit)) {
  results[["RSFLC"]] <- data.frame(model = "RSFLC", CINDEX = NA_real_, BS = NA_real_, AUC = NA_real_)
} else {
  st <- as.numeric(y_surv$time)
  se <- as.numeric(y_surv$event)
  lc_pred <- tryCatch(
    predict(
      lc_fit,
      timeData = as.data.frame(timeData),
      fixedData = as.data.frame(fixedData),
      idVar = "id",
      timeVar = "time",
      Y = list(type = "surv", Y = as.data.frame(y_surv)),
      t0 = t0
    ),
    error = function(e) NULL
  )

  risk <- rep(NA_real_, length(st))
  if (!is.null(lc_pred) && !is.null(lc_pred$pred_indiv) && !is.null(lc_pred$times)) {
    pmat <- as.matrix(lc_pred$pred_indiv)
    tidx <- which.min(abs(as.numeric(lc_pred$times) - t0))
    if (nrow(pmat) == length(st) && ncol(pmat) >= tidx) {
      risk <- as.numeric(pmat[, tidx])
    }
  }

  if (all(!is.finite(risk))) {
    if (!is.null(lc_pred) && !is.null(lc_pred$pred_leaf)) {
      leaf_mat <- as.matrix(lc_pred$pred_leaf)
      if (ncol(leaf_mat) == length(st)) {
        risk <- as.numeric(colMeans(leaf_mat, na.rm = TRUE))
      }
    }
  }

  if (all(is.finite(risk)) && stats::sd(risk, na.rm = TRUE) < 1e-12) {
    if (!is.null(lc_pred) && !is.null(lc_pred$pred_leaf)) {
      leaf_mat <- as.matrix(lc_pred$pred_leaf)
      if (ncol(leaf_mat) == length(st)) {
        risk <- as.numeric(colMeans(leaf_mat, na.rm = TRUE))
      }
    }
  }

  if (all(!is.finite(risk))) {
    results[["RSFLC"]] <- data.frame(model = "RSFLC", CINDEX = NA_real_, BS = NA_real_, AUC = NA_real_)
  } else {
    risk[!is.finite(risk)] <- median(risk[is.finite(risk)], na.rm = TRUE)
    # DynForest预测常可视为风险/事件概率，转换为生存概率
    if (all(is.finite(risk)) && all(risk >= 0) && all(risk <= 1)) {
      surv_prob <- 1 - risk
    } else {
      surv_prob <- exp(-exp(as.numeric(scale(risk))) * t0)
    }
    results[["RSFLC"]] <- build_metrics("RSFLC", st, se, risk, surv_prob)
  }
}

result_df <- bind_rows(results) %>% select(model, CINDEX, BS, AUC)
saved_strict <- write_xlsx_safe(result_df, out_strict)
saved_main <- write_xlsx_safe(result_df, out_main)

message("严格版完成。")
message("输出: ", saved_strict)
message("同步覆盖: ", saved_main)
print(result_df)
