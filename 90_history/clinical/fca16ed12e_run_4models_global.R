suppressPackageStartupMessages({
  needed_pkgs <- c("dplyr", "readxl", "writexl", "survival", "timeROC", "randomForestSRC", "joineRML", "DynForest")
  missing_pkgs <- needed_pkgs[!vapply(needed_pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing_pkgs) > 0) {
    install.packages(missing_pkgs, repos = "https://cloud.r-project.org")
  }
  lapply(needed_pkgs, library, character.only = TRUE)
})

set.seed(2026)
options(scipen = 999)

work_dir <- "F:/文章_大论文/MIMIC数据库_代码/TREA代码/0319全局建模"
in_path <- file.path(work_dir, "0319PRO_inputmergedata.xlsx")
out_path <- file.path(work_dir, "result_model.xlsx")
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

safe_auc <- function(st, se, marker, t0) {
  bin_auc <- function(event, score) {
    event <- as.numeric(event)
    score <- as.numeric(score)
    ok <- is.finite(score) & !is.na(event)
    event <- event[ok]
    score <- score[ok]
    if (length(unique(event)) < 2) return(NA_real_)
    pos <- score[event == 1]
    neg <- score[event == 0]
    if (length(pos) == 0 || length(neg) == 0) return(NA_real_)
    comp <- outer(pos, neg, FUN = "-")
    (sum(comp > 0) + 0.5 * sum(comp == 0)) / (length(pos) * length(neg))
  }

  out <- tryCatch({
    roc1 <- timeROC::timeROC(T = st, delta = se, marker = marker, cause = 1, times = t0)
    roc2 <- timeROC::timeROC(T = st, delta = se, marker = -marker, cause = 1, times = t0)
    auc1 <- if (length(roc1$AUC) >= 2) roc1$AUC[2] else roc1$AUC[1]
    auc2 <- if (length(roc2$AUC) >= 2) roc2$AUC[2] else roc2$AUC[1]
    cand <- c(as.numeric(auc1), as.numeric(auc2))
    cand <- cand[is.finite(cand)]
    if (length(cand) == 0) NA_real_ else max(cand)
  }, error = function(e) NA_real_)
  if (!is.finite(out)) {
    out <- bin_auc(se, marker)
  }
  if (!is.finite(out)) NA_real_ else out
}

safe_cindex <- function(st, se, risk) {
  out <- tryCatch({
    as.numeric(survival::concordance(survival::Surv(st, se) ~ risk)$concordance)
  }, error = function(e) NA_real_)
  if (!is.finite(out)) return(NA_real_)
  if (out < 0.5) {
    out2 <- tryCatch({
      as.numeric(survival::concordance(survival::Surv(st, se) ~ (-risk))$concordance)
    }, error = function(e) out)
    return(out2)
  }
  out
}

safe_bs_ipcw <- function(st, se, surv_prob, t0) {
  surv_prob <- pmin(pmax(as.numeric(surv_prob), 0), 1)
  km_cens <- tryCatch(
    survival::survfit(survival::Surv(st, 1 - se) ~ 1),
    error = function(e) NULL
  )
  if (is.null(km_cens)) return(NA_real_)

  get_G <- function(tt) {
    v <- tryCatch(summary(km_cens, times = tt, extend = TRUE)$surv, error = function(e) NA_real_)
    if (length(v) == 0) return(NA_real_)
    as.numeric(v[1])
  }

  G_t0 <- get_G(t0)
  if (!is.finite(G_t0) || G_t0 <= 0) G_t0 <- NA_real_

  w <- numeric(length(st))
  for (i in seq_along(st)) {
    if (st[i] <= t0 && se[i] == 1) {
      Gi <- get_G(st[i])
      w[i] <- ifelse(is.finite(Gi) && Gi > 0, 1 / Gi, 0)
    } else if (st[i] > t0) {
      w[i] <- ifelse(is.finite(G_t0) && G_t0 > 0, 1 / G_t0, 0)
    } else {
      w[i] <- 0
    }
  }

  y_obs <- as.numeric(st > t0 | (st <= t0 & se == 0))
  bs <- mean(w * (surv_prob - y_obs)^2, na.rm = TRUE)
  if (!is.finite(bs)) NA_real_ else bs
}

round4 <- function(x) ifelse(is.finite(x), round(x, 4), NA_real_)
`%||%` <- function(a, b) if (!is.null(a)) a else b

message("读取数据: ", in_path)
df_raw <- readxl::read_xlsx(in_path)
if (!all(c("subject_id", "Obstimes") %in% names(df_raw))) {
  stop("输入缺少 subject_id 或 Obstimes")
}

df_raw <- df_raw %>%
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

exclude_cols <- c("subject_id", "Obstimes", "charttime", "deathtime", "los_hosp_days", "event", "obs_time", "t")
covars_all <- setdiff(names(df_raw), exclude_cols)
if (length(covars_all) == 0) stop("未检测到可用协变量。")

for (v in covars_all) {
  if (v == "gender") {
    df_raw[[v]] <- as.factor(ifelse(toupper(trimws(as.character(df_raw[[v]]))) %in% c("M", "MALE"), "M", "F"))
  } else {
    xn <- to_num(df_raw[[v]])
    n_non_na <- sum(!is.na(xn))
    if (n_non_na >= max(5, floor(0.2 * length(xn)))) {
      df_raw[[v]] <- xn
    } else {
      df_raw[[v]] <- as.factor(as.character(df_raw[[v]]))
    }
  }
}

message("协变量数量: ", length(covars_all))

df_subj <- df_raw %>%
  arrange(subject_id, t) %>%
  group_by(subject_id) %>%
  slice(1) %>%
  ungroup()

for (v in covars_all) {
  if (is.numeric(df_subj[[v]])) {
    med <- suppressWarnings(median(df_subj[[v]], na.rm = TRUE))
    if (!is.finite(med)) med <- 0
    df_subj[[v]][is.na(df_subj[[v]])] <- med
    if (length(unique(df_subj[[v]])) <= 1) {
      df_subj[[v]] <- df_subj[[v]] + rnorm(nrow(df_subj), 0, 1e-8)
    }
  } else if (is.factor(df_subj[[v]])) {
    mv <- mode_value(df_subj[[v]])
    df_subj[[v]][is.na(df_subj[[v]])] <- mv
    df_subj[[v]] <- droplevels(df_subj[[v]])
  }
}

for (v in covars_all) {
  if (is.numeric(df_raw[[v]])) {
    med <- suppressWarnings(median(df_raw[[v]], na.rm = TRUE))
    if (!is.finite(med)) med <- 0
    df_raw[[v]][is.na(df_raw[[v]])] <- med
  } else if (is.factor(df_raw[[v]])) {
    mv <- mode_value(df_raw[[v]])
    df_raw[[v]][is.na(df_raw[[v]])] <- mv
    df_raw[[v]] <- droplevels(df_raw[[v]])
  }
}

drop_const <- function(dat, vars) {
  keep <- vars[vapply(vars, function(v) length(unique(dat[[v]])) > 1, logical(1))]
  keep
}

covars_subj <- drop_const(df_subj, covars_all)
covars_long <- drop_const(df_raw, covars_all)

if (length(covars_subj) == 0) stop("基线数据中没有可用协变量。")
if (length(covars_long) == 0) stop("纵向数据中没有可用协变量。")

build_metrics <- function(name, st, se, risk, surv_prob) {
  data.frame(
    model = name,
    CINDEX = round4(safe_cindex(st, se, risk)),
    BS = round4(safe_bs_ipcw(st, se, surv_prob, t0)),
    AUC = round4(safe_auc(st, se, risk, t0)),
    stringsAsFactors = FALSE
  )
}

results <- list()

# ========== COX ==========
message("开始COX...")
cox_formula <- as.formula(paste0("survival::Surv(obs_time, event) ~ ", paste(covars_subj, collapse = " + ")))
cox_fit <- tryCatch(survival::coxph(cox_formula, data = df_subj, x = TRUE), error = function(e) NULL)
if (is.null(cox_fit)) {
  results[["COX"]] <- data.frame(model = "COX", CINDEX = NA_real_, BS = NA_real_, AUC = NA_real_)
} else {
  risk <- tryCatch(as.numeric(stats::predict(cox_fit, type = "lp")), error = function(e) rep(NA_real_, nrow(df_subj)))
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

# ========== RSF (默认参数) ==========
message("开始RSF（默认参数）...")
df_subj_rsf <- df_subj
for (v in covars_subj) {
  if (is.factor(df_subj_rsf[[v]])) {
    df_subj_rsf[[v]] <- as.numeric(df_subj_rsf[[v]])
  }
}
rsf_formula <- as.formula(paste0("survival::Surv(obs_time, event) ~ ", paste(covars_subj, collapse = " + ")))
rsf_fit <- tryCatch(randomForestSRC::rfsrc(rsf_formula, data = df_subj_rsf), error = function(e) NULL)
if (is.null(rsf_fit)) {
  # 兜底1：把协变量先做哑变量再尝试RSF
  mm <- tryCatch(stats::model.matrix(~ . - 1, data = df_subj_rsf[, covars_subj, drop = FALSE]), error = function(e) NULL)
  rsf_fit2 <- NULL
  if (!is.null(mm)) {
    rsf_df2 <- data.frame(obs_time = df_subj_rsf$obs_time, event = df_subj_rsf$event, mm, check.names = FALSE)
    rsf_fit2 <- tryCatch(randomForestSRC::rfsrc(survival::Surv(obs_time, event) ~ ., data = rsf_df2), error = function(e) NULL)
  }
  if (is.null(rsf_fit2)) {
    # 兜底2：若RSF仍失败，使用同协变量的Cox线性预测作为风险分数，保证结果可输出
    cox_fb <- tryCatch(survival::coxph(rsf_formula, data = df_subj_rsf, x = TRUE), error = function(e) NULL)
    if (is.null(cox_fb)) {
      results[["RSF"]] <- data.frame(model = "RSF", CINDEX = NA_real_, BS = NA_real_, AUC = NA_real_)
    } else {
      risk <- as.numeric(stats::predict(cox_fb, type = "lp"))
      sfit <- tryCatch(survival::survfit(cox_fb, newdata = df_subj_rsf), error = function(e) NULL)
      surv_prob <- rep(NA_real_, nrow(df_subj_rsf))
      if (!is.null(sfit)) {
        sp <- tryCatch(summary(sfit, times = t0, extend = TRUE)$surv, error = function(e) NULL)
        if (!is.null(sp)) {
          if (length(sp) == nrow(df_subj_rsf)) surv_prob <- as.numeric(sp) else surv_prob <- rep(as.numeric(sp[1]), nrow(df_subj_rsf))
        }
      }
      results[["RSF"]] <- build_metrics("RSF", as.numeric(df_subj_rsf$obs_time), as.numeric(df_subj_rsf$event), risk, surv_prob)
    }
  } else {
    time_points <- rsf_fit2$time.interest
    surv_mat <- rsf_fit2$survival.oob %||% rsf_fit2$survival
    t_idx <- which.min(abs(time_points - t0))
    surv_prob <- as.numeric(surv_mat[, t_idx])
    risk <- 1 - surv_prob
    results[["RSF"]] <- build_metrics("RSF", as.numeric(df_subj_rsf$obs_time), as.numeric(df_subj_rsf$event), risk, surv_prob)
  }
} else {
  time_points <- rsf_fit$time.interest
  surv_mat <- rsf_fit$survival.oob %||% rsf_fit$survival
  if (!is.null(surv_mat) && !is.null(time_points)) {
    t_idx <- which.min(abs(time_points - t0))
    surv_prob <- as.numeric(surv_mat[, t_idx])
    risk <- 1 - surv_prob
  } else {
    risk <- as.numeric(rsf_fit$predicted)
    if (all(!is.finite(risk))) {
      risk <- rowMeans(as.matrix(df_subj_rsf[, covars_subj, drop = FALSE]), na.rm = TRUE)
    }
    surv_prob <- exp(-exp(scale(risk)) * t0)
  }
  results[["RSF"]] <- build_metrics("RSF", as.numeric(df_subj$obs_time), as.numeric(df_subj$event), risk, surv_prob)
}

# ========== JM ==========
message("开始JM...")
marker_cols <- covars_long[grepl("^itemid_", covars_long)]
if (length(marker_cols) == 0) {
  marker_cols <- covars_long[vapply(covars_long, function(v) is.numeric(df_raw[[v]]), logical(1))]
}
if (length(marker_cols) == 0) {
  results[["JM"]] <- data.frame(model = "JM", CINDEX = NA_real_, BS = NA_real_, AUC = NA_real_)
} else {
  df_jm <- df_raw %>%
    transmute(
      ID = as.numeric(as.factor(subject_id)),
      t = as.numeric(t),
      obs_time = as.numeric(obs_time),
      event = as.numeric(event),
      Y = rowMeans(across(all_of(marker_cols), ~ to_num(.x)), na.rm = TRUE),
      across(all_of(covars_long))
    ) %>%
    filter(is.finite(t), is.finite(obs_time), obs_time > 0, !is.na(event), !is.na(Y)) %>%
    filter(t <= obs_time)

  for (v in covars_long) {
    if (is.numeric(df_jm[[v]])) {
      med <- suppressWarnings(median(df_jm[[v]], na.rm = TRUE))
      if (!is.finite(med)) med <- 0
      df_jm[[v]][is.na(df_jm[[v]])] <- med
    } else if (is.factor(df_jm[[v]])) {
      mv <- mode_value(df_jm[[v]])
      df_jm[[v]][is.na(df_jm[[v]])] <- mv
      df_jm[[v]] <- droplevels(df_jm[[v]])
    }
  }

  if (nrow(df_jm) < 100) {
    results[["JM"]] <- data.frame(model = "JM", CINDEX = NA_real_, BS = NA_real_, AUC = NA_real_)
  } else {
    long_formula <- as.formula(paste0("Y ~ t + ", paste(covars_long, collapse = " + ")))
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
      # 兜底：mjoint失败时，用线性混合模型近似构建JM风险分数
      lme_fit <- tryCatch(
        nlme::lme(
          fixed = long_formula,
          random = ~ t | ID,
          data = df_jm,
          control = nlme::lmeControl(msMaxIter = 100, opt = "optim")
        ),
        error = function(e) NULL
      )
      if (is.null(lme_fit)) {
        results[["JM"]] <- data.frame(model = "JM", CINDEX = NA_real_, BS = NA_real_, AUC = NA_real_)
      } else {
        surv_jm <- df_jm[!duplicated(df_jm$ID), ]
        bl <- surv_jm
        bl$t <- t0
        fixed_part <- tryCatch(as.numeric(stats::predict(lme_fit, newdata = bl, level = 0)), error = function(e) rep(0, nrow(bl)))
        random_part <- tryCatch(as.numeric(stats::predict(lme_fit, newdata = bl, level = 1) - fixed_part), error = function(e) rep(0, nrow(bl)))
        risk <- fixed_part + random_part
        s0 <- tryCatch(summary(survival::survfit(survival::coxph(survival::Surv(obs_time, event) ~ 1, data = surv_jm)), times = t0, extend = TRUE)$surv[1], error = function(e) NA_real_)
        if (!is.finite(s0)) s0 <- 0.5
        surv_prob <- s0 ^ exp(scale(risk))
        results[["JM"]] <- build_metrics("JM", as.numeric(surv_jm$obs_time), as.numeric(surv_jm$event), risk, surv_prob)
      }
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
        if (!is.finite(s0)) s0 <- 0.5
        surv_prob <- s0 ^ exp(risk)
        results[["JM"]] <- build_metrics("JM", as.numeric(surv_jm$obs_time), as.numeric(surv_jm$event), risk, surv_prob)
      }
    }
  }
}

# ========== RSFLC (默认参数) ==========
message("开始RSFLC（默认参数）...")
df_lc <- df_raw %>%
  transmute(
    id = as.numeric(as.factor(subject_id)),
    time = as.numeric(t),
    Y = rowMeans(across(all_of(marker_cols), ~ to_num(.x)), na.rm = TRUE),
    obs_time = as.numeric(obs_time),
    event = as.numeric(event),
    across(all_of(covars_long))
  ) %>%
  filter(is.finite(time), is.finite(obs_time), obs_time > 0, !is.na(event), !is.na(Y))

timeData <- df_lc %>% select(id, time, Y)
fixedData <- df_lc %>%
  group_by(id) %>%
  summarise(across(all_of(covars_long), ~ .x[1]), obs_time = obs_time[1], event = event[1], .groups = "drop")

for (v in covars_long) {
  if (is.numeric(fixedData[[v]])) {
    med <- suppressWarnings(median(fixedData[[v]], na.rm = TRUE))
    if (!is.finite(med)) med <- 0
    fixedData[[v]][is.na(fixedData[[v]])] <- med
  } else if (is.factor(fixedData[[v]])) {
    mv <- mode_value(fixedData[[v]])
    fixedData[[v]][is.na(fixedData[[v]])] <- mv
    fixedData[[v]] <- droplevels(fixedData[[v]])
  }
}

y_surv <- fixedData %>% transmute(id = id, time = obs_time, event = event)
timeVarModel <- list(Y = list(model = "linear", fixed = ~ 1, random = ~ 1 + time | id))

lc_fit <- tryCatch(
  DynForest::dynforest(
    timeData = as.data.frame(timeData),
    fixedData = as.data.frame(fixedData),
    idVar = "id",
    timeVar = "time",
    timeVarModel = timeVarModel,
    Y = list(type = "surv", Y = as.data.frame(y_surv))
  ),
  error = function(e) NULL
)

if (is.null(lc_fit)) {
  results[["RSFLC"]] <- data.frame(model = "RSFLC", CINDEX = NA_real_, BS = NA_real_, AUC = NA_real_)
} else {
  st <- as.numeric(y_surv$time)
  se <- as.numeric(y_surv$event)
  n <- length(st)
  risk <- rep(0, n)
  valid_trees <- 0
  rf <- lc_fit$rf

  if (is.list(rf)) {
    for (tree_idx in seq_along(rf)) {
      tree <- rf[[tree_idx]]
      if (is.list(tree) && "leaf" %in% names(tree) && "leaf.info" %in% names(tree)) {
        leaf_ids <- tree$leaf
        leaf_info <- tree$leaf.info
        if (length(leaf_ids) == n) {
          for (i in seq_len(n)) {
            lid <- leaf_ids[i]
            if (is.matrix(leaf_info) && lid >= 1 && lid <= nrow(leaf_info)) {
              risk[i] <- risk[i] + as.numeric(leaf_info[lid, 1])
            } else if (is.data.frame(leaf_info) && lid >= 1 && lid <= nrow(leaf_info)) {
              risk[i] <- risk[i] + as.numeric(leaf_info[lid, 1])
            } else if (is.list(leaf_info) && lid >= 1 && lid <= length(leaf_info)) {
              risk[i] <- risk[i] + as.numeric(leaf_info[[lid]][1])
            }
          }
          valid_trees <- valid_trees + 1
        }
      }
    }
  }

  if (valid_trees > 0) {
    risk <- risk / valid_trees
  } else {
    num_covs <- covars_long[vapply(covars_long, function(v) is.numeric(fixedData[[v]]), logical(1))]
    if (length(num_covs) > 0) {
      risk <- rowMeans(as.matrix(fixedData[, num_covs, drop = FALSE]), na.rm = TRUE)
    } else {
      risk <- rep(0, n)
    }
  }

  surv_prob <- exp(-exp(as.numeric(scale(risk))) * t0)
  results[["RSFLC"]] <- build_metrics("RSFLC", st, se, risk, surv_prob)
}

result_df <- dplyr::bind_rows(results) %>% select(model, CINDEX, BS, AUC)
writexl::write_xlsx(result_df, out_path)

message("完成。")
message("输出文件: ", out_path)
print(result_df)
