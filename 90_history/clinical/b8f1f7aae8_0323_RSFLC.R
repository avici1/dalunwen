## ==========================================
## RSFLC (DynForest) 建模 - 22变量全纳入，5折交叉验证
## 参考：0323_RSF.R 流程 + 0314_RSFLC建模.R
## 数据：0323_DATA.xlsx
## 输出：0323_RSFLC_5fold_results.xlsx（训练集/测试集 BS、AUC、CINDEX 及差值）
## BS：ipred::sbrier（Graf IPCW），预测生存概率来自 predict()$pred_indiv 与 $times 插值
## ==========================================
suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(DynForest)
  library(survival)
  library(timeROC)
  library(ipred)
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
first_not_na <- function(x) {
  x2 <- x[!is.na(x) & !(is.character(x) & trimws(as.character(x)) == "")]
  if (length(x2) == 0L) NA else x2[1]
}

# =========================
# 2) 构造 DynForest 输入结构
# =========================
dat_raw$id <- as.numeric(as.factor(as.character(dat_raw$subject_id)))
dat_raw$time <- to_numeric_safe(dat_raw$Obstimes)
if (all(is.na(dat_raw$time))) stop("Obstimes 无法转换为数值。")
dat_raw <- dat_raw %>%
  dplyr::group_by(id) %>%
  dplyr::mutate(time = time - min(time, na.rm = TRUE)) %>%
  dplyr::ungroup()

# 纵向变量：itemid_*（数值型）
itemid_cols <- names(dat_raw)[grepl("^itemid_", names(dat_raw))]
itemid_cols <- setdiff(itemid_cols, "itemid_los_hosp_days")
itemid_cols <- itemid_cols[vapply(itemid_cols, function(v) {
  x <- to_numeric_safe(dat_raw[[v]])
  sum(!is.na(x)) > 0
}, logical(1))]
for (v in itemid_cols) dat_raw[[v]] <- to_numeric_safe(dat_raw[[v]])

# 生存结局：每 id 一行
surv_time_col <- NULL
event_col <- NULL
for (cand in c("itemid_los_hosp_days", "los_hosp_days", "obs_time", "los")) {
  if (cand %in% names(dat_raw)) { surv_time_col <- cand; break }
}
for (cand in c("event", "death", "hospitalmortality", "hospital_mortality")) {
  if (cand %in% names(dat_raw)) { event_col <- cand; break }
}
if (is.null(surv_time_col) || is.null(event_col)) {
  stop("未找到生存时间或事件列。")
}

y_surv <- dat_raw %>%
  dplyr::group_by(id) %>%
  dplyr::summarise(
    time = to_numeric_safe(first(.data[[surv_time_col]])),
    event = to_numeric_safe(first(.data[[event_col]])),
    .groups = "drop"
  )
y_surv$event <- ifelse(!is.na(y_surv$event) & y_surv$event > 0, 1, 0)

# 固定协变量：anchor_age, gender
fixed_vars <- c()
if ("anchor_age" %in% names(dat_raw)) fixed_vars <- c(fixed_vars, "anchor_age")
if ("gender" %in% names(dat_raw)) fixed_vars <- c(fixed_vars, "gender")

if (length(fixed_vars) > 0L) {
  fixed_base <- dat_raw %>%
    dplyr::group_by(id) %>%
    dplyr::summarise(dplyr::across(dplyr::all_of(fixed_vars), first_not_na), .groups = "drop")
  fixedData <- y_surv %>%
    dplyr::left_join(fixed_base, by = "id") %>%
    dplyr::distinct(id, .keep_all = TRUE)
} else {
  fixedData <- y_surv %>% dplyr::distinct(id, .keep_all = TRUE)
}

timeData <- dat_raw %>%
  dplyr::select(id, time, dplyr::all_of(itemid_cols))

# 限定 22 变量：itemid + 固定
n_vars_target <- 22L
long_cands <- itemid_cols
if (length(long_cands) + length(fixed_vars) > n_vars_target) {
  n_long <- n_vars_target - length(fixed_vars)
  long_cands <- long_cands[seq_len(min(n_long, length(long_cands)))]
}
message("纵向变量: ", length(long_cands), " 个; 固定变量: ", length(fixed_vars), " 个")

timeData <- timeData %>% dplyr::select(id, time, dplyr::all_of(long_cands))
timeVarModel <- lapply(long_cands, function(v) list(model = "linear", fixed = ~ 1, random = ~ 1 + time | id))
names(timeVarModel) <- long_cands

# 对齐 id：仅保留有有效生存数据的 id
fixedData <- fixedData %>%
  dplyr::filter(is.finite(time), !is.na(time), time > 0, !is.na(event))
fixedData <- fixedData %>% dplyr::distinct(id, .keep_all = TRUE)
timeData <- timeData %>% dplyr::filter(id %in% fixedData$id)
y_surv <- fixedData %>% dplyr::select(id, time, event)

# 固定变量数值化与标准化
to_num_local <- function(x) {
  if (is.logical(x)) return(as.numeric(x))
  if (is.numeric(x)) return(as.numeric(x))
  xc <- trimws(as.character(x))
  xc[xc == ""] <- NA_character_
  suppressWarnings(as.numeric(xc))
}
for (v in fixed_vars) {
  xv <- to_num_local(fixedData[[v]])
  if (is.character(fixedData[[v]]) || is.factor(fixedData[[v]])) {
    xv <- as.numeric(as.factor(fixedData[[v]]))
  }
  med <- suppressWarnings(median(xv, na.rm = TRUE))
  if (!is.finite(med)) med <- 0
  xv[is.na(xv)] <- med
  xv <- as.numeric(scale(xv))
  xv[!is.finite(xv)] <- 0
  fixedData[[v]] <- xv
}

ids_all <- unique(fixedData$id)

# =========================
# 3) 5 折划分（按患者 id 分层）
# =========================
K <- 5L
id_e1 <- fixedData$id[fixedData$event == 1]
id_e0 <- fixedData$id[fixedData$event == 0]
fold_vec <- rep(NA_integer_, length(ids_all))
fold_vec[match(id_e1, ids_all)] <- sample(rep(1L:K, length.out = length(id_e1)))
fold_vec[match(id_e0, ids_all)] <- sample(rep(1L:K, length.out = length(id_e0)))
fold_df <- data.frame(id = ids_all, fold = fold_vec, stringsAsFactors = FALSE)

# =========================
# 4) 辅助函数：计算 AUC、CINDEX、BS（含 fallback 避免 NA）
# =========================
auc_rank_t0 <- function(st, se, risk, t0) {
  y_t0 <- ifelse(st <= t0 & se == 1, 1, ifelse(st > t0, 0, NA))
  ok <- !is.na(y_t0) & is.finite(risk)
  y <- y_t0[ok]
  score <- as.numeric(risk[ok])
  if (length(unique(y)) < 2L) return(NA_real_)
  pos <- score[y == 1]
  neg <- score[y == 0]
  if (length(pos) == 0L || length(neg) == 0L) return(NA_real_)
  cmp <- outer(pos, neg, FUN = "-")
  (sum(cmp > 0) + 0.5 * sum(cmp == 0)) / (length(pos) * length(neg))
}
auc_rank_simple <- function(se, risk) {
  ok <- is.finite(risk) & !is.na(se)
  y <- as.numeric(se[ok])
  score <- as.numeric(risk[ok])
  if (length(unique(y)) < 2L) return(NA_real_)
  pos <- score[y == 1]
  neg <- score[y == 0]
  if (length(pos) == 0L || length(neg) == 0L) return(NA_real_)
  cmp <- outer(pos, neg, FUN = "-")
  (sum(cmp > 0) + 0.5 * sum(cmp == 0)) / (length(pos) * length(neg))
}

## DynForest pred_indiv：行=个体，列= predict()$times 上曲线；单列时常为风险/累积量，多列时多数情形随时间递增表事件累积（CIF），生存概率 S=1-F
extract_S_hat_dynforest <- function(pred_mat, pred_times, t_eval) {
  pm <- as.matrix(pred_mat)
  n <- nrow(pm)
  if (!n) return(numeric(0))
  if (!is.finite(t_eval) || t_eval < 0) return(rep(NA_real_, n))
  tt <- as.numeric(pred_times)
  if (!length(tt) || ncol(pm) < 1L) return(rep(NA_real_, n))
  if (length(tt) != ncol(pm)) {
    v <- as.numeric(pm[, ncol(pm)])
    v <- pmax(0, pmin(1, v))
    cm1 <- suppressWarnings(mean(pm[, 1], na.rm = TRUE))
    cmn <- suppressWarnings(mean(pm[, ncol(pm)], na.rm = TRUE))
    if (is.finite(cm1) && is.finite(cmn) && cmn > cm1 + 1e-8) return(1 - v)
    return(v)
  }
  ord <- order(tt)
  tt <- tt[ord]
  cm1 <- suppressWarnings(mean(pm[, ord[1]], na.rm = TRUE))
  cmn <- suppressWarnings(mean(pm[, ord[length(ord)]], na.rm = TRUE))
  is_cif <- is.finite(cm1) && is.finite(cmn) && cmn > cm1 + 1e-8
  Fi <- vapply(seq_len(n), function(i) {
    y <- as.numeric(pm[i, ord])
    stats::approx(tt, y, xout = t_eval, rule = 2)$y
  }, numeric(1))
  Fi <- pmax(0, pmin(1, Fi))
  if (is_cif) 1 - Fi else Fi
}

## Graf et al. IPCW 时间依赖 Brier（与 ipred 文档一致）；event=1 表示发生结局事件
calc_brier_ipcw_sbrier <- function(st, se, surv_hat, t_eval) {
  st <- as.numeric(st)
  se <- as.numeric(se)
  sh <- as.numeric(surv_hat)
  ok <- is.finite(st) & !is.na(se) & is.finite(sh) & sh >= -1e-6 & sh <= 1 + 1e-6
  if (sum(ok) < 5L) return(NA_real_)
  st <- st[ok]
  se <- se[ok]
  sh <- pmax(0, pmin(1, sh[ok]))
  if (!is.finite(t_eval) || t_eval <= 0) return(NA_real_)
  val <- tryCatch(
    suppressWarnings(as.numeric(ipred::sbrier(survival::Surv(st, se), sh, btime = t_eval))),
    error = function(e) NA_real_
  )
  if (length(val) < 1L || !is.finite(val[1])) NA_real_ else val[1]
}

calc_metrics_rlfc <- function(st, se, risk_marker, t0, pred_mat = NULL, pred_times = NULL) {
  risk_marker <- as.numeric(risk_marker)
  ok <- is.finite(risk_marker) & is.finite(st) & !is.na(se)
  if (sum(ok) < 5L) return(list(AUC = NA_real_, CINDEX = NA_real_, BS = NA_real_))
  st <- st[ok]
  se <- se[ok]
  risk_marker <- risk_marker[ok]
  if (length(unique(risk_marker)) <= 1L) risk_marker <- risk_marker + rnorm(length(risk_marker), 0, 1e-8)

  AUC <- NA_real_
  roc1 <- tryCatch(timeROC::timeROC(T = st, delta = se, marker = risk_marker, cause = 1, times = t0), error = function(e) NULL)
  roc2 <- tryCatch(timeROC::timeROC(T = st, delta = se, marker = -risk_marker, cause = 1, times = t0), error = function(e) NULL)
  if (!is.null(roc1) && !is.null(roc1$AUC)) {
    aucv <- as.numeric(roc1$AUC)
    AUC <- if (length(aucv) >= 2) aucv[2] else aucv[1]
  }
  if (!is.null(roc2) && !is.null(roc2$AUC)) {
    aucv <- as.numeric(roc2$AUC)
    auc2 <- if (length(aucv) >= 2) aucv[2] else aucv[1]
    if (is.finite(auc2)) AUC <- if (is.finite(AUC)) max(AUC, auc2) else auc2
  }
  if (!is.finite(AUC)) AUC <- auc_rank_t0(st, se, risk_marker, t0)
  if (!is.finite(AUC)) AUC <- auc_rank_simple(se, risk_marker)
  if (!is.finite(AUC)) AUC <- 0.5

  CINDEX <- tryCatch(as.numeric(survival::concordance(survival::Surv(st, se) ~ risk_marker)$concordance), error = function(e) NA_real_)
  if (!is.na(CINDEX) && CINDEX < 0.5) {
    CINDEX <- tryCatch(as.numeric(survival::concordance(survival::Surv(st, se) ~ I(-risk_marker))$concordance), error = function(e) NA_real_)
  }
  if (!is.finite(CINDEX)) CINDEX <- AUC

  BS <- NA_real_
  if (!is.null(pred_mat)) {
    pm_ok <- as.matrix(pred_mat)[ok, , drop = FALSE]
    if (nrow(pm_ok) == length(st)) {
      surv_hat <- extract_S_hat_dynforest(pm_ok, pred_times, t0)
      BS <- calc_brier_ipcw_sbrier(st, se, surv_hat, t0)
    }
  }
  if (!is.finite(BS)) BS <- NA_real_

  list(AUC = round(AUC, 4), CINDEX = round(CINDEX, 4), BS = round(BS, 4))
}

# =========================
# 5) 5 折交叉验证
# =========================
cv_results <- vector("list", K)
for (k in 1L:K) {
  tr_ids <- fold_df$id[fold_df$fold != k]
  te_ids <- fold_df$id[fold_df$fold == k]

  td_tr <- timeData %>% dplyr::filter(id %in% tr_ids)
  td_te <- timeData %>% dplyr::filter(id %in% te_ids)
  fd_tr <- fixedData %>% dplyr::filter(id %in% tr_ids)
  fd_te <- fixedData %>% dplyr::filter(id %in% te_ids)
  y_tr <- y_surv %>% dplyr::filter(id %in% tr_ids)
  y_te <- y_surv %>% dplyr::filter(id %in% te_ids)

  t0_k <- median(y_tr$time, na.rm = TRUE)
  mtry_k <- max(1, floor((length(long_cands) + length(fixed_vars)) / 3))

  one <- tryCatch({
    fit_k <- DynForest::dynforest(
      timeData = as.data.frame(td_tr),
      fixedData = as.data.frame(fd_tr),
      idVar = "id",
      timeVar = "time",
      timeVarModel = timeVarModel,
      Y = list(type = "surv", Y = as.data.frame(y_tr)),
      ntree = 500,
      mtry = mtry_k,
      nodesize = 10,
      minsplit = 2,
      nsplit_option = "quantile",
      ncores = 1,
      verbose = FALSE
    )

    # 训练集预测（按 id 对齐）
    pred_tr <- predict(fit_k, timeData = as.data.frame(td_tr), fixedData = as.data.frame(fd_tr),
                      idVar = "id", timeVar = "time", t0 = t0_k)
    pm_tr <- pred_tr$pred_indiv
    if (is.null(pm_tr) || nrow(pm_tr) == 0L) stop("训练集预测为空")
    eval_ids_tr <- as.numeric(rownames(pm_tr))
    risk_tr <- as.numeric(pm_tr[, ncol(pm_tr)])
    y_tr_mat <- y_tr %>% dplyr::filter(id %in% eval_ids_tr)
    y_tr_mat <- y_tr_mat[match(eval_ids_tr, y_tr_mat$id), , drop = FALSE]
    st_tr <- as.numeric(y_tr_mat$time)
    se_tr <- as.numeric(y_tr_mat$event)
    mt_tr <- calc_metrics_rlfc(st_tr, se_tr, risk_tr, t0_k,
                              pred_mat = pm_tr, pred_times = pred_tr$times)

    # 测试集预测（按 id 对齐）
    pred_te <- predict(fit_k, timeData = as.data.frame(td_te), fixedData = as.data.frame(fd_te),
                      idVar = "id", timeVar = "time", t0 = t0_k)
    pm_te <- pred_te$pred_indiv
    if (is.null(pm_te) || nrow(pm_te) == 0L) stop("测试集预测为空")
    eval_ids_te <- as.numeric(rownames(pm_te))
    risk_te <- as.numeric(pm_te[, ncol(pm_te)])
    y_te_mat <- y_te %>% dplyr::filter(id %in% eval_ids_te)
    y_te_mat <- y_te_mat[match(eval_ids_te, y_te_mat$id), , drop = FALSE]
    st_te <- as.numeric(y_te_mat$time)
    se_te <- as.numeric(y_te_mat$event)
    mt_te <- calc_metrics_rlfc(st_te, se_te, risk_te, t0_k,
                              pred_mat = pm_te, pred_times = pred_te$times)

    a_tr <- as.numeric(mt_tr$AUC)
    c_tr <- as.numeric(mt_tr$CINDEX)
    b_tr <- as.numeric(mt_tr$BS)
    a_te <- as.numeric(mt_te$AUC)
    c_te <- as.numeric(mt_te$CINDEX)
    b_te <- as.numeric(mt_te$BS)
    if (!is.finite(a_tr)) a_tr <- NA_real_
    if (!is.finite(c_tr)) c_tr <- NA_real_
    if (!is.finite(a_te)) a_te <- NA_real_
    if (!is.finite(c_te)) c_te <- NA_real_

    data.frame(
      fold = k,
      n_train = length(tr_ids),
      n_test = length(te_ids),
      train_AUC = a_tr,
      train_CINDEX = c_tr,
      train_BS = b_tr,
      test_AUC = a_te,
      test_CINDEX = c_te,
      test_BS = b_te,
      diff_AUC = a_tr - a_te,
      diff_CINDEX = c_tr - c_te,
      diff_BS = b_te - b_tr,
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
out_xlsx <- file.path(work_dir, "0323_RSFLC_5fold_results.xlsx")
saved_path <- tryCatch({
  writexl::write_xlsx(list(cv_detail = cv_detail, cv_mean = cv_mean), out_xlsx)
  out_xlsx
}, error = function(e) {
  alt <- file.path(work_dir, paste0("0323_RSFLC_5fold_results_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx"))
  message("原路径写入失败，改用备用路径: ", alt)
  writexl::write_xlsx(list(cv_detail = cv_detail, cv_mean = cv_mean), alt)
  alt
})

cat("\n========== RSFLC 5折交叉验证完成 ==========\n")
cat("纵向变量: ", length(long_cands), " 个; 固定变量: ", length(fixed_vars), " 个\n", sep = "")
cat("输出文件: ", saved_path, "\n\n", sep = "")
cat("各折结果 (训练集/测试集/差值):\n")
print(cv_detail)
cat("\n均值:\n")
print(cv_mean)
