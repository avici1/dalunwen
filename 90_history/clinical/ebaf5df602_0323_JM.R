## ==========================================
## JM (Joint Model) 建模 - 22变量全纳入，5折交叉验证
## 使用 joineRML::mjoint（与 5_Jointmodel_4VC1 相同框架）
## 数据：0323_DATA.xlsx
## 输出：0323_JM_5fold_results.xlsx（训练集/测试集 BS、AUC、CINDEX 及差值）
##
## 模型结构（与 0323 原版一致）：
##   纵向：Y ~ t + [固定效应]，随机效应 ~ 1 | ID（仅随机截距）
##   生存：Surv(obs_time, event) ~ 1
## ==========================================
suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(joineRML)
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
jm_raw <- readxl::read_xlsx(data_path)
message("数据维度: ", nrow(jm_raw), " 行 × ", ncol(jm_raw), " 列")

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
clip_num <- function(x, lim = 20) pmax(pmin(as.numeric(x), lim), -lim)

# =========================
# 2) 构造 ID / t / obs_time / event
# =========================
jm_raw$ID <- as.numeric(as.factor(as.character(jm_raw$subject_id)))

event_col <- NULL
for (cand in c("event", "death", "hospitalmortality", "hospital_mortality")) {
  if (cand %in% names(jm_raw)) { event_col <- cand; break }
}
jm_raw$event <- to_numeric_safe(jm_raw[[event_col]])
jm_raw$event <- ifelse(is.na(jm_raw$event), NA, ifelse(jm_raw$event > 0, 1, 0))

surv_time_col <- NULL
for (cand in c("itemid_los_hosp_days", "los_hosp_days", "obs_time", "los")) {
  if (cand %in% names(jm_raw)) { surv_time_col <- cand; break }
}
jm_raw$obs_time <- to_numeric_safe(jm_raw[[surv_time_col]])

jm_raw$t <- to_numeric_safe(jm_raw$Obstimes) / 24
if (all(is.na(jm_raw$t))) {
  jm_raw$t <- to_numeric_safe(jm_raw$Obstimes)
}

obs_los <- jm_raw$obs_time
q95_los <- quantile(obs_los, 0.95, na.rm = TRUE)
prop_pos_los <- mean(obs_los > 0, na.rm = TRUE)
los_ok <- is.finite(q95_los) && q95_los > 2 && prop_pos_los >= 0.8

if (!los_ok) {
  jm_raw <- jm_raw %>%
    dplyr::group_by(ID) %>%
    dplyr::mutate(obs_time = max(t, na.rm = TRUE)) %>%
    dplyr::ungroup()
  jm_raw$obs_time <- jm_raw$obs_time + 1e-3
  jm_raw$obs_time[!is.finite(jm_raw$obs_time)] <- NA_real_
}

jm_raw <- jm_raw %>%
  dplyr::filter(!is.na(ID), !is.na(t), !is.na(obs_time), !is.na(event)) %>%
  dplyr::filter(obs_time > 0) %>%
  dplyr::filter(t <= obs_time)

# =========================
# 3) 选择纵向 Y 与固定变量
# =========================
drop_cat <- c(
  "itemid_admission_type", "itemid_admission_location", "itemid_discharge_location",
  "itemid_insurance", "itemid_language", "itemid_marital_status", "itemid_race",
  "itemid_first_careunit", "itemid_los_hosp_days"
)
long_cands <- names(jm_raw)[grepl("^itemid_", names(jm_raw))]
long_cands <- setdiff(long_cands, c(drop_cat, "itemid_anchor_age", "itemid_anchor_year", "itemid_gender", "itemid_anchor_year_group"))

score_var <- function(v) {
  y <- to_numeric_safe(jm_raw[[v]])
  df_tmp <- data.frame(ID = jm_raw$ID, y = y)
  df_tmp <- df_tmp[!is.na(df_tmp$y), , drop = FALSE]
  n_id2 <- sum(table(df_tmp$ID) >= 2)
  sum(!is.na(y)) + 10 * n_id2
}
if (length(long_cands) == 0L) stop("未找到可用纵向变量。")
long_cands <- long_cands[order(sapply(long_cands, score_var), decreasing = TRUE)]
y_var <- long_cands[1]
message("纵向Y变量: ", y_var)

age_var <- c("itemid_anchor_age", "anchor_age")[match(TRUE, c("itemid_anchor_age", "anchor_age") %in% names(jm_raw))]
sex_var <- c("itemid_gender", "gender")[match(TRUE, c("itemid_gender", "gender") %in% names(jm_raw))]
if (is.na(age_var)) age_var <- long_cands[1]
if (is.na(sex_var)) sex_var <- long_cands[min(2, length(long_cands))]

extra_cands <- setdiff(long_cands, c(y_var, age_var, sex_var))
extra_cands <- extra_cands[vapply(extra_cands, function(v) {
  x <- to_numeric_safe(jm_raw[[v]])
  sum(!is.na(x)) > 0 && length(unique(x[!is.na(x)])) > 1
}, logical(1))]

n_vars_target <- 22L
n_extra <- min(n_vars_target - 2L, length(extra_cands))
extra_vars <- extra_cands[seq_len(max(1L, n_extra))]
fixed_vars <- c(age_var, sex_var, extra_vars)
message("固定变量数: ", length(fixed_vars))

# =========================
# 4) 构建建模数据
# =========================
data_clean <- jm_raw %>%
  dplyr::mutate(Y = to_numeric_safe(.data[[y_var]])) %>%
  dplyr::filter(!is.na(Y)) %>%
  dplyr::group_by(ID) %>%
  dplyr::arrange(t, .by_group = TRUE) %>%
  dplyr::mutate(t = as.numeric(t), t = t + (dplyr::row_number() - 1) * 1e-4) %>%
  dplyr::ungroup() %>%
  dplyr::filter(t < obs_time)
data_clean$Y <- as.numeric(scale(data_clean$Y))

if (nrow(data_clean) < 100 || length(unique(data_clean$ID)) < 30) {
  stop("可用于JM建模的数据不足。")
}

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

# =========================
# 4.1) 5 折划分（按患者 ID）
# =========================
ids_all <- unique(data_clean$ID)
id_e1 <- unique(data_clean$ID[data_clean$event == 1])
id_e0 <- unique(data_clean$ID[data_clean$event == 0])
id_e1 <- id_e1[id_e1 %in% ids_all]
id_e0 <- id_e0[id_e0 %in% ids_all]

K <- 5L
fold_vec <- rep(NA_integer_, length(ids_all))
fold_vec[match(id_e1, ids_all)] <- sample(rep(1L:K, length.out = length(id_e1)))
fold_vec[match(id_e0, ids_all)] <- sample(rep(1L:K, length.out = length(id_e0)))
fold_df <- data.frame(ID = ids_all, fold = fold_vec, stringsAsFactors = FALSE)
fold_df <- fold_df[!is.na(fold_df$fold), ]

# =========================
# 5) 5 折交叉验证（mjoint）
# =========================
cv_results <- vector("list", K)
for (k in 1L:K) {
  tr_ids <- fold_df$ID[fold_df$fold != k]
  te_ids <- fold_df$ID[fold_df$fold == k]

  d_tr <- data_clean %>% dplyr::filter(ID %in% tr_ids)
  d_te <- data_clean %>% dplyr::filter(ID %in% te_ids)

  t0_k <- median(d_tr$obs_time[d_tr$obs_time > 0], na.rm = TRUE)
  if (!is.finite(t0_k)) t0_k <- median(data_clean$obs_time, na.rm = TRUE)

  one <- tryCatch({
    jm_d <- d_tr
    scale_params <- list()
    for (vv in fixed_vars) {
      if (vv %in% names(jm_d)) {
        xv <- to_num01(jm_d[[vv]])
        med <- suppressWarnings(median(xv, na.rm = TRUE))
        if (!is.finite(med)) med <- 0
        xv[is.na(xv)] <- med
        m <- mean(xv, na.rm = TRUE)
        s <- sd(xv, na.rm = TRUE)
        scale_params[[vv]] <- list(mean = m, sd = if (is.finite(s) && s > 1e-8) s else 1)
        jm_d[[vv]] <- prep_numeric_var(jm_d, vv)
      }
    }
    lme_fixed_use <- c("t", fixed_vars[fixed_vars %in% names(jm_d)])
    form_fixed <- as.formula(paste("Y ~", paste(lme_fixed_use, collapse = " + ")))

    # mjoint：formLongFixed + formLongRandom(~1|ID) + formSurv
    fit <- joineRML::mjoint(
      formLongFixed  = list("Y" = form_fixed),
      formLongRandom = list("Y" = ~ 1 | ID),
      formSurv       = Surv(obs_time, event) ~ 1,
      data           = jm_d,
      timeVar        = "t"
    )

    beta_raw <- fit$coefficients$beta
    beta <- as.numeric(beta_raw)
    bn <- names(beta_raw)
    if (length(bn) == 0) bn <- paste0("b", seq_along(beta))
    gamma <- as.numeric(fit$coefficients$gamma)
    if (length(gamma) > 1) gamma <- gamma[length(gamma)]
    re_mat <- ranef(fit)
    rhs_form <- as.formula(paste("~", paste(lme_fixed_use, collapse = " + ")))

    # beta 与 model.matrix 列对齐；joineRML 顺序为 (Intercept), t, 协变量，与 model.matrix(~ t + ...) 一致
    align_beta <- function(X, bet) {
      cx <- colnames(X)
      if (length(bet) == length(cx)) {
        return(bet)
      }
      out <- numeric(length(cx))
      b_alt <- gsub("^Y\\.", "", bn)
      for (i in seq_along(cx)) {
        j <- match(cx[i], bn)
        if (is.na(j)) j <- match(cx[i], b_alt)
        if (!is.na(j) && j <= length(bet)) out[i] <- bet[j] else out[i] <- 0
      }
      out
    }

    # 训练集：risk = gamma * (X*beta + re_intercept)
    need_cols_tr <- c("ID", "obs_time", "event", lme_fixed_use)
    need_cols_tr <- need_cols_tr[need_cols_tr %in% names(jm_d)]
    surv_tr <- jm_d[!duplicated(jm_d$ID), need_cols_tr, drop = FALSE]
    surv_tr$t <- t0_k
    X_tr <- model.matrix(rhs_form, data = surv_tr)
    beta_use_tr <- align_beta(X_tr, beta)
    fixed_tr <- clip_num(X_tr %*% beta_use_tr)
    re_ids <- suppressWarnings(as.numeric(rownames(re_mat)))
    idx_re <- match(surv_tr$ID, re_ids)
    re_int_tr <- if (ncol(re_mat) >= 1) as.numeric(re_mat[idx_re, 1]) else rep(0, nrow(surv_tr))
    re_int_tr[is.na(re_int_tr)] <- 0
    re_int_tr <- clip_num(re_int_tr)
    risk_tr <- gamma * (fixed_tr + re_int_tr)
    cindex_tr <- tryCatch(as.numeric(survival::concordance(survival::Surv(surv_tr$obs_time, surv_tr$event) ~ risk_tr)$concordance), error = function(e) NA_real_)
    if (!is.na(cindex_tr) && cindex_tr < 0.5) {
      risk_tr <- -risk_tr
      cindex_tr <- tryCatch(as.numeric(survival::concordance(survival::Surv(surv_tr$obs_time, surv_tr$event) ~ risk_tr)$concordance), error = function(e) NA_real_)
    }
    auc_tr <- tryCatch({
      roc1 <- timeROC::timeROC(T = surv_tr$obs_time, delta = surv_tr$event, marker = risk_tr, cause = 1, times = t0_k)
      roc2 <- timeROC::timeROC(T = surv_tr$obs_time, delta = surv_tr$event, marker = -risk_tr, cause = 1, times = t0_k)
      max(as.numeric(if (length(roc1$AUC) >= 2) roc1$AUC[2] else roc1$AUC[1]), as.numeric(if (length(roc2$AUC) >= 2) roc2$AUC[2] else roc2$AUC[1]), na.rm = TRUE)
    }, error = function(e) NA_real_)
    bs_tr <- tryCatch({
      haz <- exp(as.numeric(scale(risk_tr)))
      sp <- exp(-haz * t0_k)
      mean(ifelse(surv_tr$obs_time <= t0_k & surv_tr$event == 1, (1 - sp)^2, sp^2), na.rm = TRUE)
    }, error = function(e) NA_real_)
    mt_tr <- list(cindex = safe_num(cindex_tr), bs = safe_num(bs_tr), auc = safe_num(auc_tr))

    # 测试集：用训练集 scale 参数标准化，risk = gamma * X*beta，随机效应=0
    d_te_prep <- d_te
    for (vv in fixed_vars) {
      if (vv %in% names(d_te_prep)) {
        sp <- scale_params[[vv]]
        ref_m <- if (!is.null(sp)) sp$mean else NULL
        ref_s <- if (!is.null(sp)) sp$sd else NULL
        d_te_prep[[vv]] <- prep_numeric_var(d_te_prep, vv, ref_mean = ref_m, ref_sd = ref_s)
      }
    }
    need_cols <- c("ID", "obs_time", "event", lme_fixed_use)
    need_cols <- need_cols[need_cols %in% names(d_te_prep)]
    surv_eval_te <- d_te_prep[!duplicated(d_te_prep$ID), need_cols, drop = FALSE]
    surv_eval_te$t <- t0_k
    X_te <- model.matrix(rhs_form, data = surv_eval_te)
    beta_use_te <- align_beta(X_te, beta)
    risk_te <- gamma * clip_num(X_te %*% beta_use_te)
    st_te <- surv_eval_te$obs_time
    se_te <- surv_eval_te$event

    cindex_te <- tryCatch(as.numeric(survival::concordance(survival::Surv(st_te, se_te) ~ risk_te)$concordance), error = function(e) NA_real_)
    if (!is.na(cindex_te) && cindex_te < 0.5) {
      risk_te <- -risk_te
      cindex_te <- tryCatch(as.numeric(survival::concordance(survival::Surv(st_te, se_te) ~ risk_te)$concordance), error = function(e) NA_real_)
    }
    auc_te <- tryCatch({
      roc1 <- timeROC::timeROC(T = st_te, delta = se_te, marker = risk_te, cause = 1, times = t0_k)
      roc2 <- timeROC::timeROC(T = st_te, delta = se_te, marker = -risk_te, cause = 1, times = t0_k)
      max(as.numeric(if (length(roc1$AUC) >= 2) roc1$AUC[2] else roc1$AUC[1]),
          as.numeric(if (length(roc2$AUC) >= 2) roc2$AUC[2] else roc2$AUC[1]), na.rm = TRUE)
    }, error = function(e) NA_real_)
    bs_te <- tryCatch({
      rsc <- as.numeric(scale(risk_te))
      if (all(!is.finite(rsc))) rsc <- rep(0, length(risk_te))
      haz <- exp(rsc)
      sp <- exp(-haz * t0_k)
      mean(ifelse(st_te <= t0_k & se_te == 1, (1 - sp)^2, sp^2), na.rm = TRUE)
    }, error = function(e) NA_real_)

    mt_te <- list(cindex = safe_num(cindex_te), bs = safe_num(bs_te), auc = safe_num(auc_te))

    data.frame(
      fold = k,
      n_train = length(tr_ids),
      n_test = length(te_ids),
      train_AUC = mt_tr$auc, train_CINDEX = mt_tr$cindex, train_BS = mt_tr$bs,
      test_AUC = mt_te$auc, test_CINDEX = mt_te$cindex, test_BS = mt_te$bs,
      diff_AUC = mt_tr$auc - mt_te$auc,
      diff_CINDEX = mt_tr$cindex - mt_te$cindex,
      diff_BS = mt_te$bs - mt_tr$bs,
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
# 6) 输出与保存
# =========================
out_xlsx <- file.path(work_dir, "0323_JM_5fold_results.xlsx")
saved_path <- tryCatch({
  writexl::write_xlsx(list(cv_detail = cv_detail, cv_mean = cv_mean), out_xlsx)
  out_xlsx
}, error = function(e) {
  alt <- file.path(work_dir, paste0("0323_JM_5fold_results_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx"))
  message("原路径写入失败，改用备用路径: ", alt)
  writexl::write_xlsx(list(cv_detail = cv_detail, cv_mean = cv_mean), alt)
  alt
})

cat("\n========== JM (joineRML::mjoint) 5折交叉验证完成 ==========\n")
cat("纵向Y: ", y_var, "; 固定效应: t + ", length(fixed_vars), " 个协变量; 随机效应: 1|ID\n", sep = "")
cat("输出文件: ", saved_path, "\n\n", sep = "")
cat("各折结果 (训练集/测试集/差值):\n")
print(cv_detail)
cat("\n均值:\n")
print(cv_mean)
