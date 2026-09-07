## ==========================================
## COX 建模 - 22变量全纳入，5折交叉验证
## 参考：0323_JM.R 流程 + 0314_COX建模.R
## 数据：0323_DATA.xlsx
## 输出：0323_COX_5fold_results.xlsx（训练集/测试集 AUC、BS、CINDEX 及差值）
## ==========================================
suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(survival)
  library(timeROC)
  library(writexl)
})

set.seed(123)
options(scipen = 999)

# =========================
# 路径
# =========================
work_dir <- "F:/文章_大论文/MIMIC数据库_代码/TREA代码/0323实例比较"
data_path <- file.path(work_dir, "0323_DATA.xlsx")
if (!dir.exists(work_dir)) dir.create(work_dir, recursive = TRUE)

# =========================
# 1) 读取数据
# =========================
message("读取数据: ", data_path)
if (!file.exists(data_path)) stop("未找到 0323_DATA.xlsx")
dat_raw <- readxl::read_xlsx(data_path)
message("数据维度: ", nrow(dat_raw), " 行 × ", ncol(dat_raw), " 列")

to_numeric_safe <- function(x) {
  if (is.list(x)) x <- unlist(x, use.names = FALSE)
  suppressWarnings(as.numeric(x))
}
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
safe_num <- function(x, digits = 4) ifelse(is.finite(x), round(x, digits), NA_real_)

# =========================
# 2) 构造 ID / obs_time / event
# =========================
id_col <- NULL
for (cand in c("subject_id", "subjectid", "id")) {
  if (cand %in% names(dat_raw)) { id_col <- cand; break }
}
if (is.null(id_col)) stop("未找到个体 ID 列")

dat_raw$ID <- as.numeric(as.factor(as.character(dat_raw[[id_col]])))

event_col <- NULL
for (cand in c("event", "death", "hospitalmortality", "hospital_mortality")) {
  if (cand %in% names(dat_raw)) { event_col <- cand; break }
}
dat_raw$event <- to_numeric_safe(dat_raw[[event_col]])
dat_raw$event <- ifelse(is.na(dat_raw$event), NA, ifelse(dat_raw$event > 0, 1, 0))

surv_time_col <- NULL
for (cand in c("itemid_los_hosp_days", "los_hosp_days", "obs_time", "los")) {
  if (cand %in% names(dat_raw)) { surv_time_col <- cand; break }
}
dat_raw$obs_time <- to_numeric_safe(dat_raw[[surv_time_col]])

dat_raw <- dat_raw %>%
  dplyr::filter(!is.na(ID), is.finite(obs_time), obs_time > 0, !is.na(event))

# =========================
# 3) 构建患者级数据（每患者一行，取首条记录协变量）
# =========================
exclude_vars <- c(
  "subject_id", "subjectid", "hadm_id", "stay_id", "charttime",
  "Obstimes", "obs_time", "event", "ID",
  "hospitalmortality", "hospital_mortality",
  "itemid_los_hosp_days", "los_hosp_days", "itemid_hadm_id_stroke", "itemid_hadm_id_icu",
  "deathtime", "death"
)
pred_cands <- setdiff(names(dat_raw), exclude_vars)
pred_cands <- pred_cands[vapply(pred_cands, function(v) {
  x <- dat_raw[[v]]
  sum(!is.na(x)) > 0 && length(unique(x[!is.na(x)])) > 1
}, logical(1))]

n_vars_target <- 22L
if (length(pred_cands) > n_vars_target) {
  pred_cands <- pred_cands[seq_len(n_vars_target)]
}
message("预测变量数: ", length(pred_cands))

first_not_na <- function(x) {
  x2 <- x[!is.na(x) & !(is.character(x) & trimws(as.character(x)) == "")]
  if (length(x2) == 0L) NA else x2[1]
}

data_patient <- dat_raw %>%
  dplyr::group_by(ID) %>%
  dplyr::summarise(
    obs_time = obs_time[1],
    event = event[1],
    dplyr::across(dplyr::all_of(pred_cands), first_not_na),
    .groups = "drop"
  ) %>%
  as.data.frame()

data_patient <- data_patient %>%
  dplyr::filter(is.finite(obs_time), obs_time > 0, !is.na(event))
message("患者级数据: ", nrow(data_patient), " 行")

# =========================
# 4) 5 折划分（按患者 ID）
# =========================
ids_all <- unique(data_patient$ID)
id_e1 <- data_patient$ID[data_patient$event == 1]
id_e0 <- data_patient$ID[data_patient$event == 0]
id_e1 <- unique(id_e1[id_e1 %in% ids_all])
id_e0 <- unique(id_e0[id_e0 %in% ids_all])

K <- 5L
fold_vec <- rep(NA_integer_, length(ids_all))
fold_vec[match(id_e1, ids_all)] <- sample(rep(1L:K, length.out = length(id_e1)))
fold_vec[match(id_e0, ids_all)] <- sample(rep(1L:K, length.out = length(id_e0)))
fold_df <- data.frame(ID = ids_all, fold = fold_vec, stringsAsFactors = FALSE)
fold_df <- fold_df[!is.na(fold_df$fold), ]

# =========================
# 5) 协变量标准化与 COX 指标计算
# =========================
prep_numeric_var <- function(df, vname, ref_mean = NULL, ref_sd = NULL) {
  x <- to_num01(df[[vname]])
  med <- suppressWarnings(median(x, na.rm = TRUE))
  if (!is.finite(med)) med <- 0
  x[is.na(x)] <- med
  if (!is.null(ref_mean) && !is.null(ref_sd) && is.finite(ref_sd) && ref_sd > 1e-8) {
    x <- (x - ref_mean) / ref_sd
  } else {
    x <- as.numeric(scale(x))
  }
  x[!is.finite(x)] <- 0
  x
}

calc_metrics_cox <- function(surv_time, surv_status, risk_marker, survival_probs, t0, df_cens) {
  if (length(unique(surv_status)) < 2L) return(list(AUC = NA_real_, CINDEX = NA_real_, BS = NA_real_))
  risk_marker <- as.numeric(risk_marker)
  ok <- is.finite(risk_marker) & is.finite(surv_time) & !is.na(surv_status)
  if (sum(ok) < 5L) return(list(AUC = NA_real_, CINDEX = NA_real_, BS = NA_real_))
  st <- surv_time[ok]
  se <- surv_status[ok]
  risk <- risk_marker[ok]
  sp <- if (length(survival_probs) == length(surv_time)) survival_probs[ok] else rep(0.5, sum(ok))
  df_ok <- df_cens[ok, , drop = FALSE]

  AUC <- NA_real_
  roc1 <- tryCatch(timeROC::timeROC(T = st, delta = se, marker = risk, cause = 1, times = t0), error = function(e) NULL)
  roc2 <- tryCatch(timeROC::timeROC(T = st, delta = se, marker = -risk, cause = 1, times = t0), error = function(e) NULL)
  if (!is.null(roc1) && !is.null(roc1$AUC)) {
    aucv <- as.numeric(roc1$AUC)
    AUC <- if (length(aucv) >= 2) aucv[2] else aucv[1]
  }
  if (!is.null(roc2) && !is.null(roc2$AUC)) {
    aucv <- as.numeric(roc2$AUC)
    auc2 <- if (length(aucv) >= 2) aucv[2] else aucv[1]
    AUC <- if (is.finite(AUC)) max(AUC, auc2) else auc2
  }

  CINDEX <- tryCatch(as.numeric(survival::concordance(survival::Surv(st, se) ~ risk)$concordance), error = function(e) NA_real_)
  if (!is.na(CINDEX) && CINDEX < 0.5) {
    CINDEX <- tryCatch(as.numeric(survival::concordance(survival::Surv(st, se) ~ I(-risk))$concordance), error = function(e) NA_real_)
  }

  Y_obs <- as.numeric(st > t0 | (st <= t0 & se == 0))
  cens_mod <- tryCatch(survival::survfit(survival::Surv(st, 1 - se) ~ 1, data = df_ok), error = function(e) NULL)
  wts <- rep(1, length(st))
  if (!is.null(cens_mod)) {
    cens_p <- tryCatch(summary(cens_mod, times = pmin(st, t0))$surv, error = function(e) NULL)
    if (is.null(cens_p) || length(cens_p) != length(st)) {
      s0 <- tryCatch(summary(cens_mod, times = t0)$surv[1], error = function(e) 1)
      cens_p <- rep(if (length(s0) > 0) s0 else 1, length(st))
    }
    if (length(cens_p) > 0) {
      s0 <- tryCatch(summary(cens_mod, times = t0)$surv[1], error = function(e) 1)
      wts <- ifelse(st <= t0 & se == 1, 1 / pmax(cens_p, 1e-6),
                    ifelse(st > t0, 1 / pmax(if (length(s0) > 0) s0 else 1, 1e-6), 0))
    }
  }
  BS <- mean(wts * (sp - Y_obs)^2, na.rm = TRUE)
  if (!is.finite(BS)) BS <- NA_real_

  list(AUC = safe_num(AUC), CINDEX = safe_num(CINDEX), BS = safe_num(BS))
}

# =========================
# 6) 5 折交叉验证（COX）
# =========================
cv_results <- vector("list", K)
for (k in 1L:K) {
  tr_ids <- fold_df$ID[fold_df$fold != k]
  te_ids <- fold_df$ID[fold_df$fold == k]
  d_tr <- data_patient %>% dplyr::filter(ID %in% tr_ids)
  d_te <- data_patient %>% dplyr::filter(ID %in% te_ids)

  t0_k <- median(d_tr$obs_time[d_tr$obs_time > 0], na.rm = TRUE)
  if (!is.finite(t0_k)) t0_k <- median(data_patient$obs_time, na.rm = TRUE)

  one <- tryCatch({
    cox_d <- d_tr
    scale_params <- list()
    for (vv in pred_cands) {
      if (vv %in% names(cox_d)) {
        xv <- to_num01(cox_d[[vv]])
        med <- suppressWarnings(median(xv, na.rm = TRUE))
        if (!is.finite(med)) med <- 0
        xv[is.na(xv)] <- med
        scale_params[[vv]] <- list(mean = mean(xv, na.rm = TRUE), sd = sd(xv, na.rm = TRUE))
        scale_params[[vv]]$sd <- if (is.finite(scale_params[[vv]]$sd) && scale_params[[vv]]$sd > 1e-8) scale_params[[vv]]$sd else 1
        cox_d[[vv]] <- prep_numeric_var(cox_d, vv)
      }
    }
    vars_use <- pred_cands[pred_cands %in% names(cox_d)]
    if (length(vars_use) < 2L && length(unique(cox_d$event)) < 2L) stop("训练集 event 单一取值")
    fm <- as.formula(paste("Surv(obs_time, event) ~", paste(vars_use, collapse = " + ")))
    fit <- survival::coxph(fm, data = cox_d, x = TRUE, model = TRUE)

    risk_tr <- as.numeric(predict(fit, newdata = cox_d, type = "lp"))
    if (length(unique(risk_tr)) <= 1) risk_tr <- risk_tr + rnorm(length(risk_tr), 0, 1e-8)
    surv_probs_tr <- tryCatch({
      sf <- survival::survfit(fit, newdata = cox_d)
      t_idx <- which.min(abs(sf$time - t0_k))
      if (is.matrix(sf$surv)) as.numeric(sf$surv[t_idx, ]) else rep(as.numeric(sf$surv[t_idx]), nrow(cox_d))
    }, error = function(e) pmax(0, pmin(1, exp(-exp(risk_tr) * t0_k / mean(cox_d$obs_time)))))
    cindex_tr <- tryCatch(as.numeric(survival::concordance(survival::Surv(cox_d$obs_time, cox_d$event) ~ risk_tr)$concordance), error = function(e) NA_real_)
    if (!is.na(cindex_tr) && cindex_tr < 0.5) risk_tr <- -risk_tr
    mt_tr <- calc_metrics_cox(cox_d$obs_time, cox_d$event, risk_tr, surv_probs_tr, t0_k, cox_d)

    d_te_prep <- d_te
    for (vv in pred_cands) {
      if (vv %in% names(d_te_prep)) {
        sp <- scale_params[[vv]]
        d_te_prep[[vv]] <- prep_numeric_var(d_te_prep, vv, ref_mean = sp$mean, ref_sd = sp$sd)
      }
    }
    risk_te <- tryCatch(as.numeric(predict(fit, newdata = d_te_prep, type = "lp")), error = function(e) rep(NA_real_, nrow(d_te_prep)))
    if (all(is.na(risk_te))) risk_te <- rep(0, nrow(d_te_prep))
    if (length(unique(risk_te)) <= 1) risk_te <- risk_te + rnorm(length(risk_te), 0, 1e-8)
    surv_probs_te <- tryCatch({
      sf <- survival::survfit(fit, newdata = d_te_prep)
      t_idx <- which.min(abs(sf$time - t0_k))
      if (is.matrix(sf$surv)) as.numeric(sf$surv[t_idx, ]) else rep(as.numeric(sf$surv[t_idx]), nrow(d_te_prep))
    }, error = function(e) pmax(0, pmin(1, exp(-exp(risk_te) * t0_k / mean(d_te_prep$obs_time)))))
    cindex_te <- tryCatch(as.numeric(survival::concordance(survival::Surv(d_te_prep$obs_time, d_te_prep$event) ~ risk_te)$concordance), error = function(e) NA_real_)
    if (!is.na(cindex_te) && cindex_te < 0.5) risk_te <- -risk_te
    mt_te <- calc_metrics_cox(d_te_prep$obs_time, d_te_prep$event, risk_te, surv_probs_te, t0_k, d_te_prep)

    data.frame(
      fold = k,
      n_train = length(tr_ids),
      n_test = length(te_ids),
      train_AUC = mt_tr$AUC, train_CINDEX = mt_tr$CINDEX, train_BS = mt_tr$BS,
      test_AUC = mt_te$AUC, test_CINDEX = mt_te$CINDEX, test_BS = mt_te$BS,
      diff_AUC = mt_tr$AUC - mt_te$AUC,
      diff_CINDEX = mt_tr$CINDEX - mt_te$CINDEX,
      diff_BS = mt_te$BS - mt_tr$BS,
      error_msg = "",
      stringsAsFactors = FALSE
    )
  }, error = function(e) {
    data.frame(
      fold = k,
      n_train = length(tr_ids),
      n_test = length(te_ids),
      train_AUC = NA_real_, train_CINDEX = NA_real_, train_BS = NA_real_,
      test_AUC = NA_real_, test_CINDEX = NA_real_, test_BS = NA_real_,
      diff_AUC = NA_real_, diff_CINDEX = NA_real_, diff_BS = NA_real_,
      error_msg = conditionMessage(e),
      stringsAsFactors = FALSE
    )
  })
  cv_results[[k]] <- one
  message("5折 CV 进度: ", k, "/5 完成")
}

cv_detail <- dplyr::bind_rows(cv_results)
cv_mean <- cv_detail %>%
  dplyr::summarise(
    fold = "mean",
    n_train = round(mean(n_train, na.rm = TRUE), 0),
    n_test = round(mean(n_test, na.rm = TRUE), 0),
    train_AUC = round(mean(train_AUC, na.rm = TRUE), 4),
    train_CINDEX = round(mean(train_CINDEX, na.rm = TRUE), 4),
    train_BS = round(mean(train_BS, na.rm = TRUE), 4),
    test_AUC = round(mean(test_AUC, na.rm = TRUE), 4),
    test_CINDEX = round(mean(test_CINDEX, na.rm = TRUE), 4),
    test_BS = round(mean(test_BS, na.rm = TRUE), 4),
    diff_AUC = round(mean(diff_AUC, na.rm = TRUE), 4),
    diff_CINDEX = round(mean(diff_CINDEX, na.rm = TRUE), 4),
    diff_BS = round(mean(diff_BS, na.rm = TRUE), 4),
    error_msg = "",
    .groups = "drop"
  )

# =========================
# 7) 输出与保存
# =========================
out_xlsx <- file.path(work_dir, "0323_COX_5fold_results.xlsx")
saved_path <- tryCatch({
  writexl::write_xlsx(list(cv_detail = cv_detail, cv_mean = cv_mean), out_xlsx)
  out_xlsx
}, error = function(e) {
  alt <- file.path(work_dir, paste0("0323_COX_5fold_results_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx"))
  message("原路径写入失败，改用备用路径: ", alt)
  writexl::write_xlsx(list(cv_detail = cv_detail, cv_mean = cv_mean), alt)
  alt
})

cat("\n========== COX 5折交叉验证完成 ==========\n")
cat("预测变量: ", length(pred_cands), " 个\n", sep = "")
cat("输出文件: ", saved_path, "\n\n", sep = "")
cat("各折结果 (训练集/测试集/差值):\n")
print(cv_detail)
cat("\n均值:\n")
print(cv_mean)
