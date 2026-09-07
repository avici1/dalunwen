# =============================================================================
# 0323_DATA.xlsx -> 每患者仅保留基线(第一条观测: Obstimes 最小) ->
#    Cox 比例风险模型 -> 基于线性预测器(type="lp")的 SHAP(Shapley)
#    随机生存森林(RSF, randomForestSRC) -> 对集成死亡率预测值的 SHAP
#    RSFLC (DynForest) -> 对 predict(..., t0) 末列风险指标的 SHAP
#    JM (joineRML::mjoint) -> 对 gamma*(X*beta) 线性风险(随机效应=0)的 SHAP
# 依赖: readxl, survival, fastshap, randomForestSRC, openxlsx, dplyr,
#       DynForest, joineRML
# =============================================================================

suppressPackageStartupMessages({
  if (!requireNamespace("readxl", quietly = TRUE)) {
    install.packages("readxl", repos = "https://cloud.r-project.org")
  }
  if (!requireNamespace("survival", quietly = TRUE)) {
    install.packages("survival", repos = "https://cloud.r-project.org")
  }
  if (!requireNamespace("fastshap", quietly = TRUE)) {
    install.packages("fastshap", repos = "https://cloud.r-project.org")
  }
  if (!requireNamespace("randomForestSRC", quietly = TRUE)) {
    install.packages("randomForestSRC", repos = "https://cloud.r-project.org")
  }
  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    install.packages("openxlsx", repos = "https://cloud.r-project.org")
  }
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    install.packages("dplyr", repos = "https://cloud.r-project.org")
  }
  if (!requireNamespace("DynForest", quietly = TRUE)) {
    install.packages("DynForest", repos = "https://cloud.r-project.org")
  }
  if (!requireNamespace("joineRML", quietly = TRUE)) {
    install.packages("joineRML", repos = "https://cloud.r-project.org")
  }
  library(readxl)
  library(survival)
  library(fastshap)
  library(randomForestSRC)
  library(openxlsx)
  library(dplyr)
  library(DynForest)
  library(joineRML)
})

# ------------ 路径与可调参数 -------------------------------------------------
args <- commandArgs(trailingOnly = FALSE)
file_arg <- sub("^--file=", "", grep("^--file=", args, value = TRUE)[1])
if (!is.na(file_arg) && nzchar(file_arg)) {
  SCRIPT_DIR <- dirname(normalizePath(file_arg))
} else {
  SCRIPT_DIR <- getwd()
}

DATA_PATH <- file.path(SCRIPT_DIR, "0323_DATA.xlsx")
if (!file.exists(DATA_PATH)) {
  DATA_PATH <- file.path(getwd(), "0323_DATA.xlsx")
}
stopifnot(file.exists(DATA_PATH))
OUT_DIR <- dirname(normalizePath(DATA_PATH))
NSIM <- 40L
BG_N <- 300L
# 解释样本数上限；基线一人一行时常设为不小于样本量以输出全员 SHAP
N_EXPLAIN <- 1000L
RNG_SEED <- 42L
RSF_NTREE <- 400L
RSFLC_NTREE <- 120L
JM_MJOINT_MAXIT <- 25L

# ------------ 读入数据 -------------------------------------------------------
raw <- readxl::read_excel(DATA_PATH)
n_in <- nrow(raw)

# 基线：按 subject_id、Obstimes(及原行序) 排序后，每患者保留第一条
ord <- order(
  raw[["subject_id"]],
  raw[["Obstimes"]],
  seq_len(nrow(raw))
)
raw <- raw[ord, , drop = FALSE]
raw <- raw[!duplicated(raw[["subject_id"]]), , drop = FALSE]
raw[["gender"]] <- factor(raw[["gender"]])

message(sprintf(
  "基线数据: 每患者 1 条, 共 %d 人 (原始行数 %d)",
  nrow(raw), n_in
))

id_vars <- c("subject_id", "Obstimes", "charttime", "deathtime")
time_var <- "los_hosp_days"
event_var <- "death"

pred_vars <- setdiff(names(raw), c(id_vars, time_var, event_var))
need <- c(time_var, event_var, pred_vars)
dat <- as.data.frame(raw[stats::complete.cases(raw[, need]), , drop = FALSE])
rownames(dat) <- NULL

message(sprintf(
  "用于建模的完整病例: %d / %d (基线行数)",
  nrow(dat), nrow(raw)
))

# ------------ Cox 模型 -------------------------------------------------------
cox_form <- as.formula(
  paste0("survival::Surv(", time_var, ", ", event_var, ") ~ ",
         paste(pred_vars, collapse = " + "))
)
fit <- survival::coxph(cox_form, data = dat, model = TRUE, x = TRUE, y = TRUE)
print(summary(fit))

pred_wrapper <- function(object, newdata) {
  as.numeric(stats::predict(object, newdata = newdata, type = "lp"))
}

# ------------ Cox / RSF 共用的背景集与解释集 --------------------------------
set.seed(RNG_SEED)
bg_n <- min(BG_N, nrow(dat))
ex_n <- min(N_EXPLAIN, nrow(dat))
bg_idx <- sample.int(nrow(dat), bg_n)
ex_idx <- sample.int(nrow(dat), ex_n)

X_bg <- as.data.frame(dat[bg_idx, pred_vars, drop = FALSE])
X_ex <- as.data.frame(dat[ex_idx, pred_vars, drop = FALSE])

# ------------ SHAP(Cox)：对线性预测器 g(X)=lp -------------------------------
message(sprintf(
  "Cox SHAP: 背景 %d 行, 解释 %d 行, nsim=%d (Monte Carlo 近似)",
  bg_n, ex_n, NSIM
))

shap_mat <- fastshap::explain(
  fit,
  X = X_bg,
  newdata = X_ex,
  nsim = NSIM,
  pred_wrapper = pred_wrapper,
  adjust = TRUE
)

colnames(shap_mat) <- pred_vars

mean_abs <- sort(colMeans(abs(shap_mat)), decreasing = TRUE)
imp_df <- data.frame(
  feature = names(mean_abs),
  mean_abs_shap = as.numeric(mean_abs),
  row.names = NULL
)

shap_df <- data.frame(
  subject_id = dat[["subject_id"]][ex_idx],
  Obstimes = dat[["Obstimes"]][ex_idx],
  shap_mat,
  check.names = FALSE,
  stringsAsFactors = FALSE
)
shap_df <- shap_df[order(shap_df[["subject_id"]]), , drop = FALSE]
rownames(shap_df) <- NULL

out_shap <- file.path(OUT_DIR, "cox_shap_values_sample.csv")
out_imp <- file.path(OUT_DIR, "cox_shap_mean_abs_importance.csv")
utils::write.csv(shap_df, out_shap, row.names = FALSE, fileEncoding = "UTF-8")
utils::write.csv(imp_df, out_imp, row.names = FALSE, fileEncoding = "UTF-8")

message("已写出: ", normalizePath(out_shap, winslash = "/"))
message("已写出: ", normalizePath(out_imp, winslash = "/"))

cat("\n=== Cox: 各变量平均 |SHAP| (解释子样本) ===\n")
print(imp_df, row.names = FALSE)

# =============================================================================
# RSF（randomForestSRC）+ SHAP
# 解释目标：predict() 返回的集成死亡率预测值 predicted（RSF 文档中的 mortality）
# =============================================================================
rsf_form <- as.formula(
  paste0("Surv(", time_var, ", ", event_var, ") ~ ",
         paste(pred_vars, collapse = " + "))
)

message(sprintf("拟合 RSF: ntree=%d ...", RSF_NTREE))
fit_rsf <- randomForestSRC::rfsrc(
  rsf_form,
  data = dat,
  ntree = RSF_NTREE,
  splitrule = "logrank",
  importance = "none",
  block.size = 1L,
  na.action = "na.omit"
)

pred_wrapper_rsf <- function(object, newdata) {
  pv <- stats::predict(object, newdata = as.data.frame(newdata))
  as.numeric(pv[["predicted"]])
}

message(sprintf(
  "RSF SHAP: 背景 %d 行, 解释 %d 行, nsim=%d (与 Cox 相同抽样)",
  bg_n, ex_n, NSIM
))

shap_rsf <- fastshap::explain(
  fit_rsf,
  X = X_bg,
  newdata = X_ex,
  nsim = NSIM,
  pred_wrapper = pred_wrapper_rsf,
  adjust = TRUE
)
colnames(shap_rsf) <- pred_vars

mean_abs_rsf <- sort(colMeans(abs(shap_rsf)), decreasing = TRUE)
imp_rsf_df <- data.frame(
  feature = names(mean_abs_rsf),
  mean_abs_shap = as.numeric(mean_abs_rsf),
  row.names = NULL
)

shap_rsf_df <- data.frame(
  subject_id = dat[["subject_id"]][ex_idx],
  Obstimes = dat[["Obstimes"]][ex_idx],
  shap_rsf,
  check.names = FALSE,
  stringsAsFactors = FALSE
)
shap_rsf_df <- shap_rsf_df[order(shap_rsf_df[["subject_id"]]), , drop = FALSE]
rownames(shap_rsf_df) <- NULL

out_rsf_val <- file.path(OUT_DIR, "RSF_shap_values.xlsx")
out_rsf_imp <- file.path(OUT_DIR, "RSF_shap_mean_abs_importance.xlsx")
openxlsx::write.xlsx(shap_rsf_df, out_rsf_val, overwrite = TRUE)
openxlsx::write.xlsx(imp_rsf_df, out_rsf_imp, overwrite = TRUE)

message("已写出: ", normalizePath(out_rsf_val, winslash = "/"))
message("已写出: ", normalizePath(out_rsf_imp, winslash = "/"))

cat("\n=== RSF: 各变量平均 |SHAP| (解释子样本) ===\n")
print(imp_rsf_df, row.names = FALSE)




#######RSFLC###########
# 数据：0323_DATA1.xlsx（与 0323_RSFLC.R 相同纵向+生存结构）
# 解释变量：各 itemid 纵向均值（按 id 聚合）+ 固定效应 anchor_age、gender（与 DynForest fixedData 一致）
# 解释目标：predict(..., t0)$pred_indiv 最后一列（与 0323_RSFLC.R 中 risk 指标一致）
# 输出：RSFLC_shap_values.xlsx、RSFLC_shap_mean_abs_importance.xlsx

RSFLC_DATA_PATH <- file.path(SCRIPT_DIR, "0323_DATA1.xlsx")
if (!file.exists(RSFLC_DATA_PATH)) {
  RSFLC_DATA_PATH <- file.path(OUT_DIR, "0323_DATA1.xlsx")
}
if (!file.exists(RSFLC_DATA_PATH)) {
  RSFLC_DATA_PATH <- file.path(getwd(), "0323_DATA1.xlsx")
}
if (!file.exists(RSFLC_DATA_PATH)) {
  fb <- file.path(SCRIPT_DIR, "0323_DATA.xlsx")
  if (!file.exists(fb)) fb <- file.path(OUT_DIR, "0323_DATA.xlsx")
  if (file.exists(fb)) {
    message("提示: 未找到 0323_DATA1.xlsx，RSFLC 暂用同结构文件: ", fb)
    RSFLC_DATA_PATH <- fb
  } else {
    stop(
      "未找到 0323_DATA1.xlsx（亦无 0323_DATA.xlsx 可回退）。请将文件放在: ",
      normalizePath(SCRIPT_DIR, winslash = "/")
    )
  }
}

to_numeric_safe_rsflc <- function(x) {
  if (is.list(x)) x <- unlist(x, use.names = FALSE)
  suppressWarnings(as.numeric(x))
}
first_not_na_rsflc <- function(x) {
  x2 <- x[!is.na(x) & !(is.character(x) & trimws(as.character(x)) == "")]
  if (length(x2) == 0L) NA else x2[1]
}

message("RSFLC: 读取 ", RSFLC_DATA_PATH)
dat_raw_lc <- readxl::read_excel(RSFLC_DATA_PATH)
dat_raw_lc$id <- as.numeric(as.factor(as.character(dat_raw_lc$subject_id)))
dat_raw_lc$time <- to_numeric_safe_rsflc(dat_raw_lc$Obstimes)
if (all(is.na(dat_raw_lc$time))) {
  stop("RSFLC: Obstimes 无法转换为数值。")
}
dat_raw_lc <- dat_raw_lc %>%
  dplyr::group_by(id) %>%
  dplyr::mutate(time = time - min(time, na.rm = TRUE)) %>%
  dplyr::ungroup()

id_like_lc <- c("subject_id", "Obstimes", "charttime", "deathtime")
fixed_names_lc <- c("anchor_age", "gender")

surv_time_col_lc <- NULL
event_col_lc <- NULL
for (cand in c("itemid_los_hosp_days", "los_hosp_days", "obs_time", "los")) {
  if (cand %in% names(dat_raw_lc)) {
    surv_time_col_lc <- cand
    break
  }
}
for (cand in c("event", "death", "hospitalmortality", "hospital_mortality")) {
  if (cand %in% names(dat_raw_lc)) {
    event_col_lc <- cand
    break
  }
}
if (is.null(surv_time_col_lc) || is.null(event_col_lc)) {
  stop("RSFLC: 未找到生存时间或事件列。")
}

# 纵向变量：优先 itemid_*；若无（如 0323_DATA1 为化验项英文名），则用除 id/生存/固定协变量外的数值列
itemid_cols_lc <- names(dat_raw_lc)[grepl("^itemid_", names(dat_raw_lc))]
itemid_cols_lc <- setdiff(itemid_cols_lc, "itemid_los_hosp_days")
if (length(itemid_cols_lc) == 0L) {
  excl <- c(id_like_lc, "id", "time", surv_time_col_lc, event_col_lc, fixed_names_lc)
  excl <- unique(excl[excl %in% names(dat_raw_lc)])
  itemid_cols_lc <- setdiff(names(dat_raw_lc), excl)
}
itemid_cols_lc <- itemid_cols_lc[vapply(itemid_cols_lc, function(v) {
  x <- to_numeric_safe_rsflc(dat_raw_lc[[v]])
  sum(!is.na(x)) > 0
}, logical(1))]
if (length(itemid_cols_lc) < 1L) {
  stop("RSFLC: 未找到可用的纵向数值变量（无 itemid_* 且无其他数值候选列）。")
}
for (v in itemid_cols_lc) dat_raw_lc[[v]] <- to_numeric_safe_rsflc(dat_raw_lc[[v]])

y_surv_lc <- dat_raw_lc %>%
  dplyr::group_by(id) %>%
  dplyr::summarise(
    time = to_numeric_safe_rsflc(first_not_na_rsflc(.data[[surv_time_col_lc]])),
    event = to_numeric_safe_rsflc(first_not_na_rsflc(.data[[event_col_lc]])),
    .groups = "drop"
  )
y_surv_lc$event <- ifelse(!is.na(y_surv_lc$event) & y_surv_lc$event > 0, 1, 0)

fixed_vars_lc <- c()
if ("anchor_age" %in% names(dat_raw_lc)) fixed_vars_lc <- c(fixed_vars_lc, "anchor_age")
if ("gender" %in% names(dat_raw_lc)) fixed_vars_lc <- c(fixed_vars_lc, "gender")

if (length(fixed_vars_lc) > 0L) {
  fixed_base_lc <- dat_raw_lc %>%
    dplyr::group_by(id) %>%
    dplyr::summarise(dplyr::across(dplyr::all_of(fixed_vars_lc), first_not_na_rsflc), .groups = "drop")
  fixedData_lc <- y_surv_lc %>%
    dplyr::left_join(fixed_base_lc, by = "id") %>%
    dplyr::distinct(id, .keep_all = TRUE)
} else {
  fixedData_lc <- y_surv_lc %>% dplyr::distinct(id, .keep_all = TRUE)
}

timeData_lc <- dat_raw_lc %>% dplyr::select(id, time, dplyr::all_of(itemid_cols_lc))

n_vars_target_lc <- 22L
long_cands_lc <- itemid_cols_lc
if (length(long_cands_lc) + length(fixed_vars_lc) > n_vars_target_lc) {
  n_long_lc <- n_vars_target_lc - length(fixed_vars_lc)
  long_cands_lc <- long_cands_lc[seq_len(min(n_long_lc, length(long_cands_lc)))]
}
timeData_lc <- timeData_lc %>% dplyr::select(id, time, dplyr::all_of(long_cands_lc))
timeVarModel_lc <- lapply(long_cands_lc, function(v) {
  list(model = "linear", fixed = ~1, random = ~1 + time | id)
})
names(timeVarModel_lc) <- long_cands_lc

fixedData_lc <- fixedData_lc %>%
  dplyr::filter(is.finite(time), !is.na(time), time > 0, !is.na(event))
fixedData_lc <- fixedData_lc %>% dplyr::distinct(id, .keep_all = TRUE)
timeData_lc <- timeData_lc %>% dplyr::filter(id %in% fixedData_lc$id)
y_surv_lc <- fixedData_lc %>% dplyr::select(id, time, event)

to_num_fixed_lc <- function(x) {
  if (is.logical(x)) return(as.numeric(x))
  if (is.numeric(x)) return(as.numeric(x))
  xc <- trimws(as.character(x))
  xc[xc == ""] <- NA_character_
  suppressWarnings(as.numeric(xc))
}
for (v in fixed_vars_lc) {
  xv <- to_num_fixed_lc(fixedData_lc[[v]])
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

id_subj_lc <- dat_raw_lc %>%
  dplyr::group_by(id) %>%
  dplyr::summarise(
    subject_id = first_not_na_rsflc(.data[["subject_id"]]),
    Obstimes = first_not_na_rsflc(.data[["Obstimes"]]),
    .groups = "drop"
  )

ag_long_lc <- timeData_lc %>%
  dplyr::group_by(id) %>%
  dplyr::summarise(dplyr::across(dplyr::all_of(long_cands_lc), ~ mean(.x, na.rm = TRUE)), .groups = "drop")
for (v in long_cands_lc) {
  m <- suppressWarnings(median(ag_long_lc[[v]], na.rm = TRUE))
  if (!is.finite(m)) m <- 0
  ag_long_lc[[v]][!is.finite(ag_long_lc[[v]])] <- m
}

X_tab_lc <- fixedData_lc %>%
  dplyr::left_join(ag_long_lc, by = "id") %>%
  dplyr::left_join(id_subj_lc, by = "id")
shap_features_lc <- c(long_cands_lc, fixed_vars_lc)
X_tab_lc <- as.data.frame(X_tab_lc)
for (v in shap_features_lc) {
  if (!v %in% names(X_tab_lc)) stop("RSFLC: 缺少 SHAP 特征列: ", v)
  mv <- suppressWarnings(median(X_tab_lc[[v]], na.rm = TRUE))
  if (!is.finite(mv)) mv <- 0
  X_tab_lc[[v]][!is.finite(X_tab_lc[[v]])] <- mv
}

time_template_lc <- as.data.frame(timeData_lc)
fixed_template_lc <- as.data.frame(fixedData_lc)
time_template_lc$id <- as.numeric(time_template_lc$id)
fixed_template_lc$id <- as.numeric(fixed_template_lc$id)
y_surv_lc$id <- as.numeric(y_surv_lc$id)

# 仅保留 timeData 中至少有一条观测且纵向 marker 存在有限值的 id（满足 predict 要求）
uids_lc <- sort(unique(time_template_lc$id))
ok_id_rsflc <- vapply(uids_lc, function(ii) {
  s <- time_template_lc[time_template_lc$id == ii, long_cands_lc, drop = FALSE]
  if (!nrow(s)) return(FALSE)
  any(is.finite(as.matrix(s)))
}, logical(1))
keep_ids_lc <- uids_lc[ok_id_rsflc]
if (length(keep_ids_lc) < length(uids_lc)) {
  message(sprintf(
    "RSFLC: 剔除无有效纵向观测的 id: %d -> %d",
    length(uids_lc),
    length(keep_ids_lc)
  ))
}
time_template_lc <- time_template_lc[time_template_lc$id %in% keep_ids_lc, , drop = FALSE]
fixed_template_lc <- fixed_template_lc[fixed_template_lc$id %in% keep_ids_lc, , drop = FALSE]
y_surv_lc <- y_surv_lc[y_surv_lc$id %in% keep_ids_lc, , drop = FALSE]
X_tab_lc <- X_tab_lc[X_tab_lc$id %in% keep_ids_lc, , drop = FALSE]
rownames(X_tab_lc) <- NULL
n_id_lc <- nrow(X_tab_lc)
if (n_id_lc < 5L) stop("RSFLC: 有效样本过少，无法拟合 DynForest。")

bg_n_lc <- min(BG_N, n_id_lc)
ex_n_lc <- min(N_EXPLAIN, n_id_lc)
set.seed(RNG_SEED)
bg_idx_lc <- sample.int(n_id_lc, bg_n_lc)
ex_idx_lc <- sample.int(n_id_lc, ex_n_lc)

X_bg_lc <- as.data.frame(X_tab_lc[bg_idx_lc, shap_features_lc, drop = FALSE])
X_ex_lc <- as.data.frame(X_tab_lc[ex_idx_lc, shap_features_lc, drop = FALSE])

t0_rsflc <- stats::median(fixed_template_lc$time, na.rm = TRUE)
if (!is.finite(t0_rsflc) || t0_rsflc <= 0) {
  t0_rsflc <- stats::quantile(fixed_template_lc$time, 0.5, na.rm = TRUE, names = FALSE)
}
mtry_lc <- max(1L, floor((length(long_cands_lc) + length(fixed_vars_lc)) / 3))

message(sprintf(
  "RSFLC: 拟合 DynForest (ntree=%d, mtry=%d, t0=%g) ...",
  RSFLC_NTREE, mtry_lc, t0_rsflc
))
fit_rsflc <- DynForest::dynforest(
  timeData = time_template_lc,
  fixedData = fixed_template_lc,
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

ids_ex_lc <- as.numeric(X_tab_lc$id[ex_idx_lc])

feat_med_rsflc <- vapply(shap_features_lc, function(v) {
  suppressWarnings(stats::median(X_tab_lc[[v]], na.rm = TRUE))
}, numeric(1))
names(feat_med_rsflc) <- shap_features_lc
feat_med_rsflc[!is.finite(feat_med_rsflc)] <- 0

pred_wrapper_rsflc <- function(object, newdata) {
  newdata <- as.data.frame(newdata)
  n <- nrow(newdata)
  if (n != length(ids_ex_lc)) {
    stop("RSFLC pred_wrapper: nrow(newdata) 与解释样本数不一致。")
  }
  td_list <- vector("list", n)
  fd_list <- vector("list", n)
  for (i in seq_len(n)) {
    id_i <- ids_ex_lc[i]
    td <- time_template_lc[time_template_lc$id == id_i, , drop = FALSE]
    fd <- fixed_template_lc[fixed_template_lc$id == id_i, , drop = FALSE]
    if (!nrow(td) || !nrow(fd)) {
      return(rep(NA_real_, n))
    }
    for (v in long_cands_lc) {
      val <- as.numeric(newdata[[v]][i])
      if (!is.finite(val)) val <- feat_med_rsflc[[v]]
      td[[v]] <- rep(val, nrow(td))
    }
    for (v in fixed_vars_lc) {
      val <- as.numeric(newdata[[v]][i])
      if (!is.finite(val)) val <- feat_med_rsflc[[v]]
      fd[[v]] <- val
    }
    td_list[[i]] <- td
    fd_list[[i]] <- fd
  }
  time_all <- dplyr::bind_rows(td_list)
  fixed_all <- dplyr::bind_rows(fd_list)
  pr <- stats::predict(
    object,
    timeData = time_all,
    fixedData = fixed_all,
    idVar = "id",
    timeVar = "time",
    t0 = t0_rsflc
  )
  pm <- pr$pred_indiv
  if (is.null(pm) || nrow(pm) < 1L) return(rep(NA_real_, n))
  ri <- as.numeric(rownames(pm))
  out <- vapply(seq_len(n), function(i) {
    w <- which(ri == ids_ex_lc[i])
    if (length(w) < 1L) NA_real_ else as.numeric(pm[w[1], ncol(pm)])
  }, numeric(1))
  as.numeric(out)
}

message(sprintf(
  "RSFLC SHAP: 背景 %d 人, 解释 %d 人, nsim=%d",
  bg_n_lc, ex_n_lc, NSIM
))

shap_rsflc <- fastshap::explain(
  fit_rsflc,
  X = X_bg_lc,
  newdata = X_ex_lc,
  nsim = NSIM,
  pred_wrapper = pred_wrapper_rsflc,
  adjust = TRUE
)
colnames(shap_rsflc) <- shap_features_lc

mean_abs_rsflc <- sort(colMeans(abs(shap_rsflc), na.rm = TRUE), decreasing = TRUE)
imp_rsflc_df <- data.frame(
  feature = names(mean_abs_rsflc),
  mean_abs_shap = as.numeric(mean_abs_rsflc),
  row.names = NULL
)

shap_rsflc_df <- data.frame(
  subject_id = X_tab_lc$subject_id[ex_idx_lc],
  Obstimes = X_tab_lc$Obstimes[ex_idx_lc],
  shap_rsflc,
  check.names = FALSE,
  stringsAsFactors = FALSE
)
shap_rsflc_df <- shap_rsflc_df[order(shap_rsflc_df$subject_id), , drop = FALSE]
rownames(shap_rsflc_df) <- NULL

out_rsflc_val <- file.path(OUT_DIR, "RSFLC_shap_values.xlsx")
out_rsflc_imp <- file.path(OUT_DIR, "RSFLC_shap_mean_abs_importance.xlsx")
openxlsx::write.xlsx(shap_rsflc_df, out_rsflc_val, overwrite = TRUE)
openxlsx::write.xlsx(imp_rsflc_df, out_rsflc_imp, overwrite = TRUE)

message("已写出: ", normalizePath(out_rsflc_val, winslash = "/"))
message("已写出: ", normalizePath(out_rsflc_imp, winslash = "/"))

cat("\n=== RSFLC (DynForest): 各变量平均 |SHAP| (解释子样本) ===\n")
print(imp_rsflc_df, row.names = FALSE)


#####JM#########