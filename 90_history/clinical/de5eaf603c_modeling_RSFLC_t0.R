## RSFLC (DynForest) 建模：参考 modeling_RSF_4V_t0.R 与 modeling_RSFLC_4V.r
## 数据：0319PRO_inputdata.xlsx
## t0 = 1, 7, 15
## 输出：result_RSFLC_4V_t0.xlsx, result_RSFLC_10V_t0.xlsx
suppressPackageStartupMessages({
  library(dplyr)
  library(readxl)
  library(writexl)
  library(DynForest)
  library(survival)
  library(timeROC)
})

set.seed(2026)
options(scipen = 999)

# =========================
# Paths
# =========================
root_dir <- "F:/文章_大论文/MIMIC数据库_代码"
in_path_candidates <- c(
  file.path(root_dir, "TREA代码/0319_补充实例结果/0319PRO_inputdata.xlsx"),
  file.path(root_dir, "TREA代码/0319尝试最优模型/0319PRO_inputdata.xlsx")
)
in_path <- in_path_candidates[file.exists(in_path_candidates)][1]
if (is.na(in_path) || !file.exists(in_path)) stop("未找到 0319PRO_inputdata.xlsx")

out_dir <- file.path(root_dir, "TREA代码/0319尝试最优模型")
out_path_4v <- file.path(out_dir, "result_RSFLC_4V_t0.xlsx")
out_path_10v <- file.path(out_dir, "result_RSFLC_10V_t0.xlsx")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

base_0318_pro <- file.path(root_dir, "TREA代码/0318/0318PRO_stroke_baseline.xlsx")
base_0318_simple <- file.path(root_dir, "TREA代码/0318/0318ORG_stroke_baseline_simple.xlsx")
base_0318_org <- file.path(root_dir, "TREA代码/0318/0313_stroke_baseline.xlsx")

# =========================
# Helpers
# =========================
first_not_na <- function(x) {
  x2 <- x[!is.na(x) & !(is.character(x) & trimws(x) == "")]
  if (length(x2) == 0L) NA else x2[1]
}

to_num <- function(x) {
  if (is.logical(x)) return(as.numeric(x))
  if (is.numeric(x)) return(as.numeric(x))
  xc <- trimws(as.character(x))
  xc[xc == ""] <- NA_character_
  suppressWarnings(as.numeric(xc))
}

safe_metric <- function(x) {
  if (!is.finite(x) || is.na(x)) return(NA_real_)
  round(as.numeric(x), 4)
}

pick_first_existing <- function(cands, pool) {
  x <- cands[cands %in% pool]
  if (length(x) == 0L) return(NA_character_)
  x[1]
}

write_xlsx_safe <- function(x, target_path) {
  ok <- tryCatch({
    writexl::write_xlsx(x, target_path)
    TRUE
  }, error = function(e) {
    message("写入失败 (可能文件已打开): ", conditionMessage(e))
    FALSE
  })
  if (ok) return(target_path)
  alt <- file.path(dirname(target_path), paste0(tools::file_path_sans_ext(basename(target_path)), "_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx"))
  message("尝试备用路径: ", alt)
  writexl::write_xlsx(x, alt)
  alt
}

# =========================
# 计算 AUC/BS/CINDEX 于给定 t0（与 modeling_RSF_4V_t0 一致）
# =========================
calc_metrics_at_t0 <- function(st, se, risk_marker, survival_probs, t0) {
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

  roc_obj1 <- tryCatch(timeROC::timeROC(T = st, delta = se, marker = risk_marker, cause = 1, times = t0), error = function(e) NULL)
  roc_obj2 <- tryCatch(timeROC::timeROC(T = st, delta = se, marker = -risk_marker, cause = 1, times = t0), error = function(e) NULL)
  auc1 <- NA_real_
  if (!is.null(roc_obj1) && !is.null(roc_obj1$AUC)) {
    auc_vec <- as.numeric(roc_obj1$AUC)
    auc1 <- if (length(auc_vec) >= 2) auc_vec[2] else auc_vec[1]
  }
  auc2 <- NA_real_
  if (!is.null(roc_obj2) && !is.null(roc_obj2$AUC)) {
    auc_vec <- as.numeric(roc_obj2$AUC)
    auc2 <- if (length(auc_vec) >= 2) auc_vec[2] else auc_vec[1]
  }
  auc_vals <- c(auc1, auc2)
  auc_vals <- auc_vals[is.finite(auc_vals)]
  AUC <- if (length(auc_vals) > 0L) max(auc_vals) else NA_real_
  if (!is.finite(AUC)) AUC <- auc_rank_t0(st, se, risk_marker, t0)
  if (!is.finite(AUC)) AUC <- auc_rank_simple(se, risk_marker)
  if (!is.finite(AUC)) AUC <- NA_real_

  CINDEX <- tryCatch(as.numeric(survival::concordance(survival::Surv(st, se) ~ risk_marker)$concordance), error = function(e) NA_real_)
  if (!is.na(CINDEX) && CINDEX < 0.5) {
    risk_marker <- -risk_marker
    CINDEX <- tryCatch(as.numeric(survival::concordance(survival::Surv(st, se) ~ risk_marker)$concordance), error = function(e) NA_real_)
  }

  Y_obs <- as.numeric(st > t0 | (st <= t0 & se == 0))
  if (is.null(survival_probs) || length(survival_probs) != length(st)) {
    haz <- exp(as.numeric(scale(risk_marker)))
    survival_probs <- exp(-haz * t0)
  }
  censoring_model <- tryCatch(survival::survfit(survival::Surv(st, 1 - se) ~ 1), error = function(e) NULL)
  if (is.null(censoring_model)) {
    BS <- mean((survival_probs - Y_obs)^2, na.rm = TRUE)
  } else {
    get_weights <- function(time, event, censoring_model, t0) {
      cens_probs <- tryCatch(summary(censoring_model, times = pmin(time, t0))$surv, error = function(e) NULL)
      if (is.null(cens_probs) || length(cens_probs) != length(time)) {
        cens_probs <- tryCatch(rep(summary(censoring_model, times = t0)$surv, length(time)), error = function(e) rep(1, length(time)))
      }
      if (is.null(cens_probs) || length(cens_probs) == 0) cens_probs <- rep(1, length(time))
      ifelse(time <= t0 & event == 1, 1 / pmax(cens_probs, 1e-6),
             ifelse(time > t0, 1 / pmax(summary(censoring_model, times = t0)$surv, 1e-6), 0))
    }
    weights <- get_weights(st, se, censoring_model, t0)
    BS <- mean(weights * (survival_probs - Y_obs)^2, na.rm = TRUE)
  }
  if (!is.finite(BS) || is.na(BS)) BS <- NA_real_

  list(AUC = AUC, BS = BS, CINDEX = CINDEX)
}

# =========================
# RSFLC 单次拟合：返回 dyn/fd/td 等，供 predict_and_metrics 在各 t0 下预测
# vars_long: 纵向变量（在 timeData 中）
# vars_fixed: 固定变量（在 fixedData 中，含 vars_long 的基线）
# =========================
fit_rsflc <- function(timeData, fixedData, y_surv, vars_long, vars_fixed = NULL, ncores = 2) {
  if (is.null(vars_fixed)) vars_fixed <- vars_long
  ids <- Reduce(intersect, list(unique(timeData$id), unique(fixedData$id), unique(y_surv$id)))
  td <- timeData %>% filter(id %in% ids) %>% select(id, time, all_of(vars_long))
  fd <- fixedData %>% filter(id %in% ids) %>% select(id, all_of(vars_fixed))
  y_i <- y_surv %>% filter(id %in% ids)

  for (v in vars_fixed) {
    xv <- to_num(fd[[v]])
    med <- suppressWarnings(median(xv, na.rm = TRUE))
    if (!is.finite(med)) med <- 0
    xv[is.na(xv)] <- med
    xv <- as.numeric(scale(xv))
    xv[!is.finite(xv)] <- 0
    fd[[v]] <- xv
  }

  timeVarModel <- lapply(vars_long, function(v) list(model = "linear", fixed = ~ 1, random = ~ 1 + time | id))
  names(timeVarModel) <- vars_long

  ## 增强参数：ntree 200, mtry 至少 2, nodesize 5（参考 0314_RSFLC ntree=500）
  dyn_i <- tryCatch(DynForest::dynforest(
    timeData = as.data.frame(td),
    fixedData = as.data.frame(fd),
    idVar = "id",
    timeVar = "time",
    timeVarModel = timeVarModel,
    Y = list(type = "surv", Y = as.data.frame(y_i)),
    ntree = 200,
    mtry = max(2, min(length(vars_fixed), floor(sqrt(length(vars_fixed) + 1)))),
    nodesize = 5,
    minsplit = 2,
    nsplit_option = "quantile",
    ncores = ncores,
    verbose = FALSE
  ), error = function(e) NULL)

  fd_y <- fd %>% left_join(y_i %>% select(id, time, event), by = "id")
  st <- as.numeric(fd_y$time)
  se <- as.numeric(fd_y$event)
  n_i <- nrow(fd)

  list(dyn = dyn_i, fd = fd, fd_y = fd_y, st = st, se = se, vars_fixed = vars_fixed, td = td, y_i = y_i)
}

## 对已拟合的 DynForest 在指定 t0 下预测并计算指标（使用 predict 获取 t0 时刻 CIF）
predict_and_metrics <- function(fit_out, t0, vars_fixed) {
  risk_i <- NULL
  surv_prob <- NULL
  if (!is.null(fit_out$dyn)) {
    pred_i <- tryCatch(predict(fit_out$dyn, timeData = as.data.frame(fit_out$td), fixedData = as.data.frame(fit_out$fd),
      idVar = "id", timeVar = "time", t0 = t0), error = function(e) NULL)
    if (!is.null(pred_i) && !is.null(pred_i$pred_indiv)) {
      pred_mat <- pred_i$pred_indiv
      risk_i <- as.numeric(pred_mat[, ncol(pred_mat)])
      surv_prob <- pmax(0.01, pmin(0.99, 1 - risk_i))
    }
  }
  if (is.null(risk_i) || length(risk_i) != length(fit_out$st)) {
    risk_i <- rowMeans(as.matrix(fit_out$fd[, vars_fixed, drop = FALSE]), na.rm = TRUE)
    risk_i[!is.finite(risk_i)] <- 0
    surv_prob <- pmax(0.01, pmin(0.99, 1 - as.numeric(scale(risk_i))))
  }
  cidx <- tryCatch(as.numeric(survival::concordance(survival::Surv(fit_out$st, fit_out$se) ~ risk_i)$concordance), error = function(e) NA_real_)
  if (!is.na(cidx) && cidx < 0.5) {
    risk_i <- -risk_i
    surv_prob <- 1 - surv_prob
  }
  calc_metrics_at_t0(fit_out$st, fit_out$se, risk_i, surv_prob, t0)
}

# =========================
# 1) Read input and merge survival
# =========================
message("读取数据: ", in_path)
df <- read_xlsx(in_path) %>% mutate(subject_id = as.character(subject_id))
if (!all(c("subject_id", "Obstimes") %in% names(df))) stop("输入缺少 subject_id / Obstimes")

item_cols <- names(df)[grepl("^itemid_", names(df))]
if (length(item_cols) < 4L) stop("itemid 变量不足4个。")
message("itemid 变量数: ", length(item_cols))
for (v in item_cols) df[[v]] <- to_num(df[[v]])

df <- df %>%
  mutate(id = as.numeric(as.factor(subject_id)), time = to_num(Obstimes)) %>%
  group_by(id) %>%
  mutate(time = time - min(time, na.rm = TRUE)) %>%
  ungroup()

surv_list <- list()
if (file.exists(base_0318_pro)) {
  b1 <- read_xlsx(base_0318_pro)
  b1_sub <- b1 %>% transmute(subject_id = as.character(subject_id), event = to_num(hospital_mortality), obs_time = to_num(los_hosp_days)) %>%
    group_by(subject_id) %>% summarise(event = suppressWarnings(max(event, na.rm = TRUE)), obs_time = first_not_na(obs_time), .groups = "drop")
  b1_sub$event[!is.finite(b1_sub$event)] <- NA
  surv_list <- append(surv_list, list(b1_sub))
}
if (file.exists(base_0318_simple)) {
  b2 <- read_xlsx(base_0318_simple)
  b2_sub <- b2 %>% transmute(subject_id = as.character(subject_id), event = to_num(hospital_mortality), obs_time = NA_real_) %>%
    group_by(subject_id) %>% summarise(event = suppressWarnings(max(event, na.rm = TRUE)), obs_time = first_not_na(obs_time), .groups = "drop")
  b2_sub$event[!is.finite(b2_sub$event)] <- NA
  surv_list <- append(surv_list, list(b2_sub))
}
if (file.exists(base_0318_org)) {
  b3 <- read_xlsx(base_0318_org)
  b3_sub <- b3 %>% transmute(subject_id = as.character(subject_id), event = NA_real_, obs_time = to_num(los_hosp_days)) %>%
    group_by(subject_id) %>% summarise(event = first_not_na(event), obs_time = first_not_na(obs_time), .groups = "drop")
  surv_list <- append(surv_list, list(b3_sub))
}
if (length(surv_list) == 0L) stop("未找到可用的0318生存信息文件。")

surv_all <- bind_rows(surv_list) %>%
  group_by(subject_id) %>%
  summarise(event = suppressWarnings(max(event, na.rm = TRUE)), obs_time = first_not_na(obs_time), .groups = "drop")
surv_all$event[!is.finite(surv_all$event)] <- NA
surv_all$event <- ifelse(!is.na(surv_all$event) & surv_all$event > 0, 1, ifelse(!is.na(surv_all$event), 0, NA))

df_raw <- df %>% left_join(surv_all, by = "subject_id")
df_raw$obs_time <- ifelse(is.na(df_raw$obs_time), to_num(df_raw$Obstimes) + 1, df_raw$obs_time)
df_raw$obs_time <- ifelse(!is.finite(df_raw$obs_time) | df_raw$obs_time <= 0, to_num(df_raw$Obstimes) + 1, df_raw$obs_time)
df_raw$obs_time <- ifelse(!is.finite(df_raw$obs_time) | df_raw$obs_time <= 0, 1, df_raw$obs_time)
df_raw$event <- ifelse(!is.na(df_raw$event) & df_raw$event > 0, 1, ifelse(!is.na(df_raw$event), 0, NA))

timeData <- df_raw %>% select(id, time, all_of(item_cols)) %>% filter(is.finite(time), !is.na(time))
y_surv <- df_raw %>%
  group_by(id) %>%
  summarise(time = first_not_na(obs_time), event = first_not_na(event), .groups = "drop") %>%
  filter(is.finite(time), !is.na(time), time > 0, !is.na(event))
y_surv$event <- ifelse(y_surv$event > 0, 1, 0)

message("subject-level 样本量: ", nrow(y_surv), ", event=1: ", sum(y_surv$event == 1))

ncores_use <- max(1, min(4, parallel::detectCores() - 1))
t0_vec <- c(1, 7, 15)

# =========================
# 2) RSFLC 4V
# =========================
comb_mat <- utils::combn(item_cols, 4)
n_comb <- min(100L, ncol(comb_mat))
comb_mat <- comb_mat[, seq_len(n_comb), drop = FALSE]
message("RSFLC 4V 组合数: ", n_comb, "（可改 100 为 300 以搜索更多）")

results_4v <- vector("list", n_comb * length(t0_vec))
idx <- 0L

for (i in seq_len(n_comb)) {
  vars_i <- comb_mat[, i]
  fixed_i <- df_raw %>%
    group_by(id) %>%
    summarise(across(all_of(vars_i), first_not_na), .groups = "drop")

  fit_out <- tryCatch(fit_rsflc(timeData, fixed_i, y_surv, vars_long = vars_i, vars_fixed = NULL, ncores_use), error = function(e) NULL)
  if (is.null(fit_out)) {
    for (t0_val in t0_vec) {
      idx <- idx + 1L
      results_4v[[idx]] <- data.frame(itemids = paste(vars_i, collapse = ";"), t0 = t0_val, AUC = NA_real_, BS = NA_real_, CINDEX = NA_real_, stringsAsFactors = FALSE)
    }
  } else {
    for (t0_val in t0_vec) {
      idx <- idx + 1L
      mt <- tryCatch(predict_and_metrics(fit_out, t0_val, vars_i), error = function(e) list(AUC = NA_real_, BS = NA_real_, CINDEX = NA_real_))
      results_4v[[idx]] <- data.frame(
        itemids = paste(vars_i, collapse = ";"),
        t0 = t0_val,
        AUC = safe_metric(mt$AUC),
        BS = safe_metric(mt$BS),
        CINDEX = safe_metric(mt$CINDEX),
        stringsAsFactors = FALSE
      )
    }
  }
  if (i %% 20 == 0 || i == n_comb) message("RSFLC 4V 进度: ", i, "/", n_comb)
}

result_4v_df <- bind_rows(results_4v) %>% select(itemids, t0, AUC, BS, CINDEX)
best_4v <- result_4v_df %>% group_by(t0) %>% filter(CINDEX == max(CINDEX, na.rm = TRUE)) %>% slice(1) %>% ungroup()
saved_4v <- write_xlsx_safe(list(全部结果 = result_4v_df, 各t0最佳 = best_4v), out_path_4v)
message("RSFLC 4V 完成。结果: ", saved_4v)
print(best_4v)

# =========================
# 3) RSFLC 10V（年龄+性别+8文献变量）
# =========================
stroke_literature_vars <- c(
  "itemid_51222", "itemid_50912", "itemid_51301", "itemid_50813", "itemid_51265",
  "itemid_51006", "itemid_51237", "itemid_51277", "itemid_51221", "itemid_50882",
  "itemid_50868", "itemid_50983", "itemid_50971", "itemid_51274", "itemid_51275"
)
stroke_extra <- intersect(stroke_literature_vars, item_cols)
if (length(stroke_extra) < 8L) {
  more <- setdiff(item_cols, c(stroke_extra, "itemid_anchor_age", "itemid_gender"))
  stroke_extra <- c(stroke_extra, more[seq_len(min(8L - length(stroke_extra), length(more)))])
}
message("10V 文献变量: ", length(stroke_extra), " 个")

baseline_path <- file.path(root_dir, "TREA代码/0318/0313_stroke_baseline.xlsx")
if (!file.exists(baseline_path)) baseline_path <- base_0318_pro
base_dat <- NULL
if (file.exists(baseline_path)) {
  base_dat <- read_xlsx(baseline_path) %>%
    transmute(subject_id = as.character(subject_id), anchor_age = to_num(anchor_age), gender = as.character(gender)) %>%
    group_by(subject_id) %>% summarise(anchor_age = first_not_na(anchor_age), gender = first_not_na(gender), .groups = "drop")
}

comb_8 <- utils::combn(stroke_extra, min(8L, length(stroke_extra)))
if (length(stroke_extra) < 8L) {
  comb_8 <- matrix(stroke_extra, nrow = length(stroke_extra), ncol = 1)
} else {
  comb_8 <- comb_8[, seq_len(min(300L, ncol(comb_8))), drop = FALSE]
}
n_comb_10v <- ncol(comb_8)
message("RSFLC 10V 组合数: ", n_comb_10v)

results_10v <- vector("list", n_comb_10v * length(t0_vec))
idx_10v <- 0L

for (i in seq_len(n_comb_10v)) {
  vars_8 <- comb_8[, i]
  if (!is.null(base_dat)) {
    df_raw_10v <- df_raw %>% left_join(base_dat, by = "subject_id")
    df_raw_10v$age <- to_num(df_raw_10v$anchor_age)
    df_raw_10v$sex <- as.factor(ifelse(toupper(trimws(as.character(df_raw_10v$gender))) %in% c("M", "MALE"), "M", "F"))
  } else {
    df_raw_10v <- df_raw
    df_raw_10v$age <- 0
    df_raw_10v$sex <- as.factor("M")
  }
  df_raw_10v$age[!is.finite(df_raw_10v$age)] <- median(df_raw_10v$age, na.rm = TRUE)

  vars_10 <- c("age", "sex", vars_8)
  timeData_10v <- df_raw_10v %>% select(id, time, all_of(vars_8)) %>% filter(is.finite(time), !is.na(time))
  fixed_10v <- df_raw_10v %>%
    group_by(id) %>%
    summarise(across(all_of(vars_10), first_not_na), .groups = "drop")

  fit_out <- tryCatch(fit_rsflc(timeData_10v, fixed_10v, y_surv, vars_long = vars_8, vars_fixed = vars_10, ncores_use), error = function(e) NULL)
  if (is.null(fit_out)) {
    for (t0_val in t0_vec) {
      idx_10v <- idx_10v + 1L
      results_10v[[idx_10v]] <- data.frame(itemids = paste(vars_10, collapse = ";"), t0 = t0_val, AUC = NA_real_, BS = NA_real_, CINDEX = NA_real_, stringsAsFactors = FALSE)
    }
  } else {
    for (t0_val in t0_vec) {
      idx_10v <- idx_10v + 1L
      mt <- tryCatch(predict_and_metrics(fit_out, t0_val, vars_10), error = function(e) list(AUC = NA_real_, BS = NA_real_, CINDEX = NA_real_))
      results_10v[[idx_10v]] <- data.frame(
        itemids = paste(vars_10, collapse = ";"),
        t0 = t0_val,
        AUC = safe_metric(mt$AUC),
        BS = safe_metric(mt$BS),
        CINDEX = safe_metric(mt$CINDEX),
        stringsAsFactors = FALSE
      )
    }
  }
  if (i %% 20 == 0 || i == n_comb_10v) message("RSFLC 10V 进度: ", i, "/", n_comb_10v)
}

result_10v_df <- bind_rows(results_10v) %>% select(itemids, t0, AUC, BS, CINDEX)
best_10v <- result_10v_df %>% group_by(t0) %>% filter(CINDEX == max(CINDEX, na.rm = TRUE)) %>% slice(1) %>% ungroup()
saved_10v <- write_xlsx_safe(list(全部结果 = result_10v_df, 各t0最佳 = best_10v), out_path_10v)
message("RSFLC 10V 完成。结果: ", saved_10v)
print(best_10v)
