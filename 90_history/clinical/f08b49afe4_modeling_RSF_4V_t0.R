## 4变量RSF建模：参考 cal_3_RSF_mimic (0314_RSF建模.R)
## 数据：0319PRO_inputdata.xlsx（与 modeling_RSFLC_4V.r 一致）
## t0 = 1, 7, 15
## 输出：result_RSF_4V_0320.xlsx
suppressPackageStartupMessages({
  library(dplyr)
  library(readxl)
  library(writexl)
  library(survival)
  library(timeROC)
  library(randomForestSRC)
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
out_path <- file.path(out_dir, "result_RSF_4V_0320.xlsx")
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

to_numeric_safe <- function(x) {
  if (is.list(x)) x <- unlist(x, use.names = FALSE)
  suppressWarnings(as.numeric(x))
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

## 安全写入 xlsx：若目标文件被占用则写入带时间戳的备用路径
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
# cal_3_RSF_mimic：参考 0314_RSF建模.R:87-88
# =========================
cal_3_RSF_mimic <- function(data, t0, keep_pred) {
  rsf_df_raw <- data[, c(keep_pred, "obs_time", "event"), drop = FALSE]
  rsf_df_raw$obs_time <- to_numeric_safe(rsf_df_raw$obs_time)
  rsf_df_raw$event <- ifelse(to_numeric_safe(rsf_df_raw$event) > 0, 1, 0)
  rsf_df_raw <- rsf_df_raw[is.finite(rsf_df_raw$obs_time) & !is.na(rsf_df_raw$obs_time) & rsf_df_raw$obs_time > 0 & !is.na(rsf_df_raw$event), , drop = FALSE]

  pred_cols_raw <- setdiff(names(rsf_df_raw), c("obs_time", "event"))
  pred_clean <- list()
  for (v in pred_cols_raw) {
    x <- rsf_df_raw[[v]]
    if (is.list(x)) next
    if (inherits(x, "POSIXt") || inherits(x, "Date")) {
      x <- as.numeric(x)
    } else if (is.logical(x)) {
      x <- as.numeric(x)
    } else if (is.character(x)) {
      x <- trimws(x)
      x[x == ""] <- NA_character_
      x <- as.factor(x)
    } else if (!(is.numeric(x) || is.integer(x) || is.factor(x))) {
      x <- suppressWarnings(as.numeric(x))
    }
    if (sum(!is.na(x)) == 0) next
    if (length(unique(x[!is.na(x)])) <= 1) next
    pred_clean[[v]] <- x
  }
  if (length(pred_clean) == 0L) return(list(metrics = data.frame(AUC = NA_real_, CINDEX = NA_real_, BS = NA_real_)))

  rsf_df <- data.frame(pred_clean, obs_time = rsf_df_raw$obs_time, event = rsf_df_raw$event, check.names = FALSE)
  names(rsf_df) <- make.names(names(rsf_df), unique = TRUE)
  pred_cols <- setdiff(names(rsf_df), c("obs_time", "event"))
  if (length(pred_cols) == 0L) return(list(metrics = data.frame(AUC = NA_real_, CINDEX = NA_real_, BS = NA_real_)))

  surv_time <- rsf_df$obs_time
  surv_status <- rsf_df$event
  if (length(unique(surv_status)) < 2L) return(list(metrics = data.frame(AUC = NA_real_, CINDEX = NA_real_, BS = NA_real_)))

  mtry_val <- max(1, floor(length(pred_cols) / 3))
  fit <- tryCatch({
    randomForestSRC::rfsrc(
      formula = Surv(obs_time, event) ~ .,
      data = rsf_df,
      ntree = 500,
      mtry = mtry_val,
      nodesize = 10,
      importance = FALSE,
      proximity = FALSE,
      na.action = "na.impute",
      seed = 123
    )
  }, error = function(e) NULL)

  if (is.null(fit)) return(list(metrics = data.frame(AUC = NA_real_, CINDEX = NA_real_, BS = NA_real_)))

  n_obs <- nrow(rsf_df)
  risk_marker <- NULL
  survival_probs <- NULL
  if (!is.null(fit$survival) && !is.null(fit$time.interest) && length(fit$time.interest) > 0) {
    t_idx <- which.min(abs(fit$time.interest - t0))
    if (is.matrix(fit$survival)) {
      survival_probs <- as.numeric(fit$survival[, t_idx])
    } else {
      survival_probs <- as.numeric(fit$survival)
    }
    if (length(survival_probs) == n_obs) risk_marker <- 1 - survival_probs
  }
  if (is.null(risk_marker) && !is.null(fit$predicted) && length(fit$predicted) == n_obs) {
    risk_marker <- as.numeric(fit$predicted)
    survival_probs <- pmax(0, pmin(1, 1 - risk_marker))
  }
  if (is.null(risk_marker) || length(risk_marker) != n_obs) {
    return(list(metrics = data.frame(AUC = NA_real_, CINDEX = NA_real_, BS = NA_real_)))
  }

  ## AUC at t0：timeROC -> 秩次法(t0) -> 简单事件AUC 三级兜底
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

  roc_obj1 <- tryCatch(timeROC::timeROC(T = surv_time, delta = surv_status, marker = risk_marker, cause = 1, times = t0), error = function(e) NULL)
  roc_obj2 <- tryCatch(timeROC::timeROC(T = surv_time, delta = surv_status, marker = -risk_marker, cause = 1, times = t0), error = function(e) NULL)
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
  if (!is.finite(AUC)) {
    AUC <- auc_rank_t0(surv_time, surv_status, risk_marker, t0)
  }
  if (!is.finite(AUC)) {
    AUC <- auc_rank_simple(surv_status, risk_marker)
  }
  if (!is.finite(AUC)) AUC <- NA_real_

  CINDEX <- tryCatch(as.numeric(survival::concordance(survival::Surv(surv_time, surv_status) ~ risk_marker)$concordance), error = function(e) NA_real_)
  if (!is.na(CINDEX) && CINDEX < 0.5) {
    risk_marker <- -risk_marker
    CINDEX <- tryCatch(as.numeric(survival::concordance(survival::Surv(surv_time, surv_status) ~ risk_marker)$concordance), error = function(e) NA_real_)
  }

  Y_obs <- as.numeric(surv_time > t0 | (surv_time <= t0 & surv_status == 0))
  censoring_model <- tryCatch(survival::survfit(survival::Surv(obs_time, 1 - event) ~ 1, data = rsf_df), error = function(e) NULL)
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
    weights <- get_weights(surv_time, surv_status, censoring_model, t0)
    BS <- mean(weights * (survival_probs - Y_obs)^2, na.rm = TRUE)
  }
  if (!is.finite(BS) || is.na(BS)) BS <- NA_real_

  list(metrics = data.frame(AUC = round(AUC, 4), CINDEX = round(CINDEX, 4), BS = round(BS, 4)))
}

# =========================
# 1) Read input and merge survival (from modeling_RSFLC_4V.r)
# =========================
message("读取数据: ", in_path)
df <- read_xlsx(in_path) %>% mutate(subject_id = as.character(subject_id))
if (!all(c("subject_id", "Obstimes") %in% names(df))) stop("输入缺少 subject_id / Obstimes")

item_cols <- names(df)[grepl("^itemid_", names(df))]
if (length(item_cols) < 4L) stop("itemid 变量不足4个，无法组合。")
message("itemid 变量数: ", length(item_cols))
for (v in item_cols) df[[v]] <- to_num(df[[v]])

surv_list <- list()
if (file.exists(base_0318_pro)) {
  b1 <- read_xlsx(base_0318_pro)
  b1_sub <- b1 %>%
    transmute(subject_id = as.character(subject_id), event = to_num(hospital_mortality), obs_time = to_num(los_hosp_days)) %>%
    group_by(subject_id) %>% summarise(event = suppressWarnings(max(event, na.rm = TRUE)), obs_time = first_not_na(obs_time), .groups = "drop")
  b1_sub$event[!is.finite(b1_sub$event)] <- NA
  surv_list <- append(surv_list, list(b1_sub))
}
if (file.exists(base_0318_simple)) {
  b2 <- read_xlsx(base_0318_simple)
  b2_sub <- b2 %>%
    transmute(subject_id = as.character(subject_id), event = to_num(hospital_mortality), obs_time = NA_real_) %>%
    group_by(subject_id) %>% summarise(event = suppressWarnings(max(event, na.rm = TRUE)), obs_time = first_not_na(obs_time), .groups = "drop")
  b2_sub$event[!is.finite(b2_sub$event)] <- NA
  surv_list <- append(surv_list, list(b2_sub))
}
if (file.exists(base_0318_org)) {
  b3 <- read_xlsx(base_0318_org)
  b3_sub <- b3 %>%
    transmute(subject_id = as.character(subject_id), event = NA_real_, obs_time = to_num(los_hosp_days)) %>%
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

# 每 subject 一行（取第一条）
df_subj <- df_raw %>%
  group_by(subject_id) %>%
  summarise(across(all_of(item_cols), first_not_na), obs_time = first_not_na(obs_time), event = first_not_na(event), .groups = "drop") %>%
  filter(is.finite(obs_time), !is.na(obs_time), obs_time > 0, !is.na(event))
df_subj$event <- ifelse(df_subj$event > 0, 1, 0)

# 中位数填补
for (v in item_cols) {
  xv <- to_num(df_subj[[v]])
  med <- suppressWarnings(median(xv, na.rm = TRUE))
  if (!is.finite(med)) med <- 0
  xv[is.na(xv)] <- med
  df_subj[[v]] <- xv
}

message("subject-level 样本量: ", nrow(df_subj), ", event=1: ", sum(df_subj$event == 1))

# =========================
# 2) 4变量组合，t0 = 1, 7, 15
# =========================
comb_mat <- utils::combn(item_cols, 4)
n_comb <- ncol(comb_mat)
max_pairs <- 300L
if (n_comb > max_pairs) {
  comb_mat <- comb_mat[, seq_len(max_pairs), drop = FALSE]
  n_comb <- ncol(comb_mat)
  message("仅运行前 ", n_comb, " 组4变量组合。")
}

t0_vec <- c(1, 7, 15)
results <- vector("list", n_comb * length(t0_vec))
idx <- 0L

for (i in seq_len(n_comb)) {
  vars_i <- comb_mat[, i]
  for (t0_val in t0_vec) {
    idx <- idx + 1L
    res <- tryCatch(cal_3_RSF_mimic(df_subj, t0 = t0_val, keep_pred = vars_i), error = function(e) list(metrics = data.frame(AUC = NA_real_, CINDEX = NA_real_, BS = NA_real_)))
    mt <- res$metrics
    results[[idx]] <- data.frame(
      itemids = paste(vars_i, collapse = ";"),
      t0 = t0_val,
      AUC = safe_metric(mt$AUC[1]),
      BS = safe_metric(mt$BS[1]),
      CINDEX = safe_metric(mt$CINDEX[1]),
      stringsAsFactors = FALSE
    )
  }
  if (i %% 20 == 0 || i == n_comb) message("RSF 4V 进度: ", i, "/", n_comb)
}

result_df <- bind_rows(results)

# 输出格式参考 result_RSF_4V.xlsx：itemids, t0, AUC, BS, CINDEX
result_df <- result_df %>% select(itemids, t0, AUC, BS, CINDEX)

# 汇总：每个 t0 的最佳组合
best_per_t0 <- result_df %>%
  group_by(t0) %>%
  filter(CINDEX == max(CINDEX, na.rm = TRUE)) %>%
  slice(1) %>%
  ungroup()

saved_4v <- write_xlsx_safe(list(全部结果 = result_df, 各t0最佳 = best_per_t0), out_path)

message("建模完成。结果文件: ", saved_4v)
message("各 t0 最佳 4 变量组合 (按 CINDEX): ")
print(best_per_t0)


###########10V尝试建模#####################
## 10变量 = 年龄 + 性别 + 8个脑卒中预后相关变量（文献支持）
## 文献来源：Hemoglobin, Creatinine, WBC, Lactate, Platelet, BUN, INR, GCS等与脑卒中院内死亡/预后显著相关
## 不遍历全部组合，仅从文献支持的变量中选取8个进行组合尝试，最多300次建模
## 输出：result_RSF_10V_0320.xlsx

out_path_10v <- file.path(out_dir, "result_RSF_10V_0320.xlsx")

# 文献支持的脑卒中预后相关变量（MIMIC itemid，按证据强度排序）
# 参考：Hemoglobin(Hb), Creatinine, WBC, Lactate, Platelet, BUN, INR, HCT, RDW, HCO3, Anion Gap, Na, K
stroke_literature_vars <- c(
  "itemid_51222",  # Hb 血红蛋白 - 低Hb增加死亡风险
  "itemid_50912",  # Creatinine 肌酐 - 升高增加死亡风险
  "itemid_51301",  # WBC 白细胞 - 与院内死亡相关
  "itemid_50813",  # Lactate 乳酸 - 组织灌注/代谢
  "itemid_51265",  # PLT 血小板 - HALP评分成分
  "itemid_51006",  # BUN 尿素氮 - 肾功
  "itemid_51237",  # INR - 凝血
  "itemid_51277",  # RDW - 预后相关
  "itemid_51221",  # HCT 红细胞压积
  "itemid_50882",  # HCO3 - 代谢
  "itemid_50868",  # Anion Gap 阴离子间隙
  "itemid_50983",  # Na 钠
  "itemid_50971",  # K 钾
  "itemid_51274",  # PT
  "itemid_51275"   # APTT
)
stroke_extra <- intersect(stroke_literature_vars, item_cols)
if (length(stroke_extra) < 8L) {
  more <- setdiff(item_cols, c(stroke_extra, "itemid_anchor_age", "itemid_gender"))
  stroke_extra <- c(stroke_extra, more[seq_len(min(8L - length(stroke_extra), length(more)))])
}
message("10V 文献变量（数据中存在）: ", length(stroke_extra), " 个")

# 合并基线获取年龄、性别（若数据中无）
baseline_path <- file.path(root_dir, "TREA代码/0318/0313_stroke_baseline.xlsx")
if (!file.exists(baseline_path)) baseline_path <- base_0318_pro
df_subj_10v <- df_subj
if (file.exists(baseline_path)) {
  base <- read_xlsx(baseline_path) %>%
    transmute(
      subject_id = as.character(subject_id),
      anchor_age = to_num(anchor_age),
      gender = as.character(gender)
    ) %>%
    group_by(subject_id) %>%
    summarise(anchor_age = first_not_na(anchor_age), gender = first_not_na(gender), .groups = "drop")
  df_subj_10v <- df_subj %>%
    left_join(base %>% mutate(subject_id = as.character(subject_id)), by = "subject_id")
}
# 年龄、性别变量：优先 baseline，否则从 itemid 中找
age_var <- NA_character_
sex_var <- NA_character_
if ("anchor_age" %in% names(df_subj_10v)) {
  age_var <- "anchor_age"
  df_subj_10v$age <- to_num(df_subj_10v$anchor_age)
} else if ("itemid_anchor_age" %in% names(df_subj_10v)) {
  age_var <- "itemid_anchor_age"
  df_subj_10v$age <- to_num(df_subj_10v$itemid_anchor_age)
}
if ("gender" %in% names(df_subj_10v)) {
  sex_var <- "gender"
  df_subj_10v$sex <- as.factor(ifelse(toupper(trimws(as.character(df_subj_10v$gender))) %in% c("M", "MALE"), "M", "F"))
} else if ("itemid_gender" %in% names(df_subj_10v)) {
  sex_var <- "itemid_gender"
  df_subj_10v$sex <- as.factor(ifelse(toupper(trimws(as.character(df_subj_10v$itemid_gender))) %in% c("M", "MALE"), "M", "F"))
}
if (is.na(age_var) || is.na(sex_var)) {
  age_var <- pick_first_existing(c("itemid_anchor_age", "anchor_age", "age"), names(df_subj_10v))
  sex_var <- pick_first_existing(c("itemid_gender", "gender", "sex"), names(df_subj_10v))
  if (is.na(age_var) || is.na(sex_var)) stop("10V 未找到年龄或性别变量。")
  df_subj_10v$age <- to_num(df_subj_10v[[age_var]])
  df_subj_10v$sex <- as.factor(as.character(df_subj_10v[[sex_var]]))
}
df_subj_10v$age[!is.finite(df_subj_10v$age)] <- median(df_subj_10v$age, na.rm = TRUE)
df_subj_10v$sex <- as.factor(df_subj_10v$sex)

# 10V = age + sex + 8 from stroke_extra
comb_8 <- utils::combn(stroke_extra, min(8L, length(stroke_extra)))
if (length(stroke_extra) < 8L) {
  comb_8 <- matrix(stroke_extra, nrow = length(stroke_extra), ncol = 1)
} else {
  comb_8 <- comb_8[, seq_len(min(300L, ncol(comb_8))), drop = FALSE]
}
n_comb_10v <- ncol(comb_8)
message("10V 组合数: ", n_comb_10v)

results_10v <- vector("list", n_comb_10v * length(t0_vec))
idx_10v <- 0L
for (i in seq_len(n_comb_10v)) {
  vars_8 <- comb_8[, i]
  vars_10 <- c("age", "sex", vars_8)
  for (t0_val in t0_vec) {
    idx_10v <- idx_10v + 1L
    res <- tryCatch(cal_3_RSF_mimic(df_subj_10v, t0 = t0_val, keep_pred = vars_10), error = function(e) list(metrics = data.frame(AUC = NA_real_, CINDEX = NA_real_, BS = NA_real_)))
    mt <- res$metrics
    results_10v[[idx_10v]] <- data.frame(
      itemids = paste(vars_10, collapse = ";"),
      t0 = t0_val,
      AUC = safe_metric(mt$AUC[1]),
      BS = safe_metric(mt$BS[1]),
      CINDEX = safe_metric(mt$CINDEX[1]),
      stringsAsFactors = FALSE
    )
  }
  if (i %% 20 == 0 || i == n_comb_10v) message("RSF 10V 进度: ", i, "/", n_comb_10v)
}

result_10v_df <- bind_rows(results_10v) %>% select(itemids, t0, AUC, BS, CINDEX)
best_10v_per_t0 <- result_10v_df %>%
  group_by(t0) %>%
  filter(CINDEX == max(CINDEX, na.rm = TRUE)) %>%
  slice(1) %>%
  ungroup()

saved_10v <- write_xlsx_safe(list(全部结果 = result_10v_df, 各t0最佳 = best_10v_per_t0), out_path_10v)
message("10V 建模完成。结果文件: ", saved_10v)
message("各 t0 最佳 10 变量组合:")
print(best_10v_per_t0)

