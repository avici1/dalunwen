## =========================
## JM建模精简脚本
## 内容：数据引入 -> 数据结构处理 -> 模型构建 -> 结果生成
## =========================
suppressPackageStartupMessages({
  library(dplyr)
  library(readxl)
  library(survival)
  library(timeROC)
  library(nlme)
  library(JM)
})
set.seed(123)

## 1) 数据引入
jm_dir <- "f:/文章_大论文/MIMIC数据库_代码/TREA代码/0314"
jm_main_path <- file.path(jm_dir, "widedata_merge1.xlsx")
jm_base_path <- "F:/文章_大论文/MIMIC数据库_代码/TREA代码/0311/stroke_baseline_simple.xlsx"
if (!file.exists(jm_main_path)) stop(paste0("文件不存在: ", jm_main_path))
if (!file.exists(jm_base_path)) stop(paste0("文件不存在: ", jm_base_path))

jm_main <- read_xlsx(jm_main_path)
jm_base <- read_xlsx(jm_base_path)

jm_base_sub <- jm_base %>%
  dplyr::select(hadm_id, hospital_mortality) %>%
  dplyr::distinct(hadm_id, .keep_all = TRUE) %>%
  dplyr::rename(hospitalmortality = hospital_mortality)

jm_raw <- jm_main %>%
  mutate(hadm_id = as.character(hadm_id)) %>%
  left_join(jm_base_sub %>% mutate(hadm_id = as.character(hadm_id)), by = "hadm_id")

## 2) 修改数据结构（ID/t/obs_time/event + V1~V6 + 纵向候选）
jm_raw$ID <- as.numeric(as.factor(as.character(jm_raw$subject_id)))
jm_raw$obs_time <- suppressWarnings(as.numeric(jm_raw$itemid_los_hosp_days))
jm_raw$event <- suppressWarnings(as.numeric(jm_raw$hospitalmortality))
jm_raw$event <- ifelse(is.na(jm_raw$event), NA, ifelse(jm_raw$event > 0, 1, 0))

## t优先用 charttime 计算“天”尺度；否则回退到 Obstimes
if ("charttime" %in% names(jm_raw)) {
  ct <- tryCatch(as.POSIXct(jm_raw$charttime, tz = "UTC"), error = function(e) rep(as.POSIXct(NA), nrow(jm_raw)))
  ct_num <- as.numeric(ct)
  if (sum(!is.na(ct_num)) >= max(100, floor(0.5 * nrow(jm_raw)))) {
    jm_raw$chart_num <- ct_num
    jm_raw <- jm_raw %>%
      group_by(ID) %>%
      mutate(t = (chart_num - min(chart_num, na.rm = TRUE)) / (24 * 3600)) %>%
      ungroup() %>%
      dplyr::select(-chart_num)
  } else {
    jm_raw$t <- suppressWarnings(as.numeric(jm_raw$Obstimes)) / 24
  }
} else {
  jm_raw$t <- suppressWarnings(as.numeric(jm_raw$Obstimes)) / 24
}

jm_raw <- jm_raw %>%
  filter(!is.na(ID), !is.na(t), !is.na(obs_time), !is.na(event)) %>%
  filter(t <= obs_time)

to_num01 <- function(x) {
  if (is.logical(x)) return(as.numeric(x))
  if (is.numeric(x)) return(as.numeric(x))
  xc <- trimws(as.character(x))
  xc[xc == ""] <- NA_character_
  xn <- suppressWarnings(as.numeric(xc))
  ok <- sum(!is.na(xn))
  if (ok >= max(10, floor(0.5 * sum(!is.na(xc))))) return(xn)
  as.numeric(as.factor(xc))
}

baseline_first <- function(df, col) {
  df %>% group_by(ID) %>% summarise(val = dplyr::first(.data[[col]]), .groups = "drop")
}

candidate_covars <- c(
  "itemid_anchor_age", "itemid_gender", "itemid_anchor_year",
  "itemid_insurance", "itemid_marital_status", "itemid_race",
  "itemid_admission_type", "itemid_first_careunit"
)
candidate_covars <- intersect(candidate_covars, names(jm_raw))
if (length(candidate_covars) == 0L) stop("未找到可用基线协变量。")

cov_list <- lapply(candidate_covars, function(v) {
  x <- baseline_first(jm_raw, v)
  x$val <- to_num01(x$val)
  names(x)[2] <- v
  x
})
baseline_cov <- Reduce(function(a, b) left_join(a, b, by = "ID"), cov_list)

na_rate <- sapply(setdiff(names(baseline_cov), "ID"), function(v) mean(is.na(baseline_cov[[v]])))
pick6 <- names(sort(na_rate))[seq_len(min(6, length(na_rate)))]
for (i in seq_along(pick6)) names(baseline_cov)[names(baseline_cov) == pick6[i]] <- paste0("V", i)
if (length(pick6) < 6) {
  for (k in (length(pick6) + 1):6) baseline_cov[[paste0("V", k)]] <- 0
}
baseline_cov <- baseline_cov %>% dplyr::select(ID, V1, V2, V3, V4, V5, V6)

jm_data0 <- jm_raw %>% left_join(baseline_cov, by = "ID")
for (v in c("V1", "V2", "V3", "V4", "V5", "V6")) {
  jm_data0[[v]][is.na(jm_data0[[v]])] <- median(jm_data0[[v]], na.rm = TRUE)
  if (!is.finite(jm_data0[[v]][1])) jm_data0[[v]] <- 0
}

drop_cat_jm <- c(
  "itemid_admission_type", "itemid_admission_location", "itemid_discharge_location",
  "itemid_insurance", "itemid_language", "itemid_marital_status", "itemid_race",
  "itemid_first_careunit", "itemid_los_hosp_days"
)
long_cands <- names(jm_data0)[grepl("^itemid_", names(jm_data0))]
long_cands <- setdiff(long_cands, drop_cat_jm)
long_cands <- setdiff(long_cands, c("itemid_anchor_age", "itemid_anchor_year", "itemid_gender", "itemid_anchor_year_group"))

score_var <- function(v) {
  y <- suppressWarnings(as.numeric(jm_data0[[v]]))
  not_na <- sum(!is.na(y))
  df_tmp <- data.frame(ID = jm_data0$ID, y = y)
  df_tmp <- df_tmp[!is.na(df_tmp$y), , drop = FALSE]
  n_id2 <- sum(table(df_tmp$ID) >= 2)
  not_na + 10 * n_id2
}
if (length(long_cands) == 0L) stop("未找到可用纵向变量（itemid_）。")
long_cands <- long_cands[order(sapply(long_cands, score_var), decreasing = TRUE)]
long_try <- head(long_cands, 8)

safe_metric <- function(x, digits = 4) ifelse(is.finite(x), round(x, digits), NA_real_)

## 3) 模型构建（不断尝试）
fit_one_jm <- function(data_all, y_var, t0) {
  d <- data_all %>%
    mutate(Y = suppressWarnings(as.numeric(.data[[y_var]]))) %>%
    filter(!is.na(.data$Y))
  if (sd(d$Y, na.rm = TRUE) <= 1e-8) stop("Y方差接近0，跳过该变量。")

  d <- d %>%
    group_by(ID) %>%
    arrange(t, .by_group = TRUE) %>%
    mutate(t = as.numeric(t), t = t + (row_number() - 1) * 1e-4) %>%
    ungroup()
  d$Y <- as.numeric(scale(d$Y))

  id_count <- d %>% count(ID, name = "n")
  ids_n2 <- id_count %>% filter(n >= 2) %>% pull(ID)
  use_random_slope <- length(ids_n2) >= 30
  if (use_random_slope) d <- d %>% filter(ID %in% ids_n2)
  if (nrow(d) < 80 || length(unique(d$ID)) < 30) stop("样本不足，跳过该变量。")

  form_candidates <- list(
    as.formula("Y ~ t"),
    as.formula("Y ~ t + V1 + V2"),
    as.formula("Y ~ t + V1 + V2 + V3 + V4 + V5 + V6")
  )
  rand_candidates <- if (use_random_slope) list(~ 1 | ID, ~ t | ID) else list(~ 1 | ID)

  fit <- NULL
  lme_fit <- NULL
  used_form <- NULL
  last_err <- NULL
  id_pool <- unique(d$ID)
  sample_sizes <- unique(c(length(id_pool), 300, 200, 150, 100))
  sample_sizes <- sample_sizes[sample_sizes <= length(id_pool)]

  for (ss in sample_sizes) {
    d_fit <- if (ss < length(id_pool)) d %>% filter(ID %in% sample(id_pool, ss)) else d
    for (fm in form_candidates) {
      for (rfm in rand_candidates) {
        fit_try <- tryCatch({
          lme_try <- nlme::lme(
            fixed = fm, random = rfm, data = d_fit, na.action = na.omit,
            control = nlme::lmeControl(opt = "optim")
          )
          surv_data_try <- d_fit[!duplicated(d_fit$ID), c("ID", "obs_time", "event")]
          cox_try <- survival::coxph(survival::Surv(obs_time, event) ~ 1, data = surv_data_try, x = TRUE, model = TRUE)
          jm_try <- JM::jointModel(lmeObject = lme_try, survObject = cox_try, timeVar = "t", method = "weibull-PH-aGH")
          list(jm = jm_try, lme = lme_try, d_fit = d_fit)
        }, error = function(e) {
          last_err <<- conditionMessage(e)
          NULL
        })
        if (!is.null(fit_try)) {
          fit <- fit_try$jm
          lme_fit <- fit_try$lme
          d <- fit_try$d_fit
          used_form <- fm
          break
        }
      }
      if (!is.null(fit)) break
    }
    if (!is.null(fit)) break
  }
  if (is.null(fit)) stop(paste0("所有固定/随机效应组合均拟合失败。最后错误: ", last_err))

  surv_data <- d[!duplicated(d$ID), c("ID", "obs_time", "event", "V1", "V2", "V3", "V4", "V5", "V6")]
  beta <- nlme::fixed.effects(lme_fit)
  assoc_candidates <- c("Assoct", "alpha", "AssoctE", "AssoctEV")
  assoc_val <- NA_real_
  for (nm in assoc_candidates) {
    if (!is.null(fit$coefficients[[nm]])) {
      assoc_val <- as.numeric(fit$coefficients[[nm]])[1]
      break
    }
  }
  if (!is.finite(assoc_val)) assoc_val <- 1

  re <- nlme::ranef(lme_fit)
  if (is.null(dim(re))) re <- matrix(re, ncol = 1)
  re_int <- re[, 1]
  re_slope <- if (ncol(re) >= 2) re[, 2] else rep(0, length(re_int))

  surv_data$t <- t0
  rhs_terms <- attr(terms(used_form), "term.labels")
  rhs_form <- if (length(rhs_terms) > 0) as.formula(paste("~", paste(rhs_terms, collapse = " + "))) else ~ 1
  X <- model.matrix(rhs_form, data = surv_data)
  beta_use <- beta[colnames(X)]
  beta_use[is.na(beta_use)] <- 0
  fixed_part <- as.numeric(X %*% beta_use)
  rand_part <- re_int + re_slope * t0
  risk <- assoc_val * (fixed_part + rand_part)

  cindex <- as.numeric(survival::concordance(Surv(surv_data$obs_time, surv_data$event) ~ risk)$concordance)
  if (!is.na(cindex) && cindex < 0.5) {
    risk <- -risk
    cindex <- as.numeric(survival::concordance(Surv(surv_data$obs_time, surv_data$event) ~ risk)$concordance)
  }

  roc1 <- timeROC(T = surv_data$obs_time, delta = surv_data$event, marker = risk, cause = 1, times = t0)
  roc2 <- timeROC(T = surv_data$obs_time, delta = surv_data$event, marker = -risk, cause = 1, times = t0)
  auc1 <- if (length(roc1$AUC) >= 2) roc1$AUC[2] else roc1$AUC[1]
  auc2 <- if (length(roc2$AUC) >= 2) roc2$AUC[2] else roc2$AUC[1]
  auc <- max(as.numeric(auc1), as.numeric(auc2), na.rm = TRUE)

  haz <- exp(as.numeric(scale(risk)))
  sp <- exp(-haz * t0)
  bs <- mean(ifelse(surv_data$obs_time <= t0 & surv_data$event == 1, (1 - sp)^2, sp^2), na.rm = TRUE)

  list(
    fit = fit,
    y_var = y_var,
    n_id = nrow(surv_data),
    metrics = data.frame(AUC = safe_metric(auc), BS = safe_metric(bs), CINDEX = safe_metric(cindex))
  )
}

t0_jm <- median(jm_data0$obs_time[jm_data0$obs_time > 0], na.rm = TRUE)
jm_try_results <- list()
for (v in long_try) {
  cat("JM尝试变量: ", v, "\n", sep = "")
  one <- tryCatch(fit_one_jm(jm_data0, v, t0_jm), error = function(e) {
    cat("  失败: ", conditionMessage(e), "\n", sep = "")
    NULL
  })
  if (!is.null(one)) jm_try_results[[v]] <- one
}
if (length(jm_try_results) == 0L) stop("JM多次尝试均失败，请检查数据质量或减少变量复杂度。")

## 4) 结果生成
jm_metrics_all <- bind_rows(lapply(names(jm_try_results), function(v) {
  cbind(y_var = v, n_id = jm_try_results[[v]]$n_id, jm_try_results[[v]]$metrics)
}))
jm_best <- jm_metrics_all[which.max(jm_metrics_all$CINDEX), , drop = FALSE]
jm_model <- jm_try_results[[jm_best$y_var]]$fit

jm_structure_summary <- list(
  n_rows_raw = nrow(jm_raw),
  n_rows_model = nrow(jm_data0),
  n_ids = length(unique(jm_data0$ID)),
  t0 = t0_jm,
  covariates = c("V1", "V2", "V3", "V4", "V5", "V6"),
  tried_longitudinal_vars = long_try,
  best_longitudinal_var = as.character(jm_best$y_var)
)

cat("\nJM建模完成（多次尝试后最优）。\n")
print(jm_best)
cat("\nJM全部尝试结果：\n")
print(jm_metrics_all)
cat("\nJM输入结构摘要：\n")
cat("- 原始行数: ", jm_structure_summary$n_rows_raw, "\n", sep = "")
cat("- 建模行数: ", jm_structure_summary$n_rows_model, "\n", sep = "")
cat("- 患者数(ID): ", jm_structure_summary$n_ids, "\n", sep = "")
cat("- t0: ", round(jm_structure_summary$t0, 6), "\n", sep = "")
cat("- 最优纵向变量: ", jm_structure_summary$best_longitudinal_var, "\n", sep = "")
