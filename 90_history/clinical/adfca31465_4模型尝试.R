suppressPackageStartupMessages({
  library(dplyr)
  library(readxl)
  library(writexl)
  library(survival)
  library(timeROC)
  library(randomForestSRC)
  library(joineRML)
})

set.seed(2026)
options(scipen = 999)

root_dir <- "F:/文章_大论文/MIMIC数据库_代码"
work_dir <- file.path(root_dir, "TREA代码/0319尝试最优模型")

in_path <- file.path(work_dir, "0319PRO_inputdata.xlsx")
baseline_path <- file.path(root_dir, "TREA代码/0318/0313_stroke_baseline.xlsx")

merged_out <- file.path(work_dir, "0319PRO_inputmergedata.xlsx")

out_rsf_4v <- file.path(work_dir, "result_RSF_4V.xlsx")
out_rsf_10v <- file.path(work_dir, "result_RSF_10V.xlsx")
out_jm_4v <- file.path(work_dir, "result_JM_4V.xlsx")
out_jm_10v <- file.path(work_dir, "result_JM_10V.xlsx")
out_all <- file.path(work_dir, "0319PRO_result_4model_R.xlsx")

t0 <- 30

if (!file.exists(in_path)) stop(paste0("输入文件不存在: ", in_path))
if (!file.exists(baseline_path)) stop(paste0("基线文件不存在: ", baseline_path))

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

safe_auc <- function(st, se, marker, t0) {
  out <- tryCatch({
    roc1 <- timeROC::timeROC(T = st, delta = se, marker = marker, cause = 1, times = t0)
    roc2 <- timeROC::timeROC(T = st, delta = se, marker = -marker, cause = 1, times = t0)
    auc1 <- if (length(roc1$AUC) >= 2) roc1$AUC[2] else roc1$AUC[1]
    auc2 <- if (length(roc2$AUC) >= 2) roc2$AUC[2] else roc2$AUC[1]
    max(as.numeric(auc1), as.numeric(auc2), na.rm = TRUE)
  }, error = function(e) NA_real_)
  if (!is.finite(out) || is.na(out)) return(NA_real_)
  out
}

safe_cindex <- function(st, se, risk) {
  out <- tryCatch({
    as.numeric(survival::concordance(survival::Surv(st, se) ~ risk)$concordance)
  }, error = function(e) NA_real_)
  if (!is.finite(out) || is.na(out)) return(NA_real_)
  if (out < 0.5) {
    out2 <- tryCatch({
      as.numeric(survival::concordance(survival::Surv(st, se) ~ (-risk))$concordance)
    }, error = function(e) out)
    return(out2)
  }
  out
}

safe_bs <- function(st, se, surv_prob, t0) {
  # 简化BS，与既往脚本风格一致
  y_obs <- as.numeric(st > t0 | (st <= t0 & se == 0))
  bs <- mean((surv_prob - y_obs)^2, na.rm = TRUE)
  if (!is.finite(bs) || is.na(bs)) return(NA_real_)
  bs
}

round4 <- function(x) ifelse(is.finite(x), round(x, 4), NA_real_)

# =========================
# 1) Merge age/sex from baseline
# =========================
df <- read_xlsx(in_path)
base <- read_xlsx(baseline_path) %>%
  transmute(
    subject_id = as.character(subject_id),
    anchor_age = to_num(anchor_age),
    gender = as.character(gender),
    los_hosp_days = to_num(los_hosp_days),
    deathtime = as.character(deathtime)
  ) %>%
  group_by(subject_id) %>%
  summarise(
    anchor_age = first_not_na(anchor_age),
    gender = first_not_na(gender),
    los_hosp_days = first_not_na(los_hosp_days),
    deathtime = first_not_na(deathtime),
    .groups = "drop"
  )

df <- df %>%
  mutate(subject_id = as.character(subject_id)) %>%
  left_join(base, by = "subject_id")

write_xlsx(df, merged_out)

# outcome
df$event <- ifelse(!is.na(df$deathtime) & trimws(df$deathtime) != "", 1, 0)
df$obs_time <- to_num(df$los_hosp_days)
df$obs_time <- ifelse(!is.finite(df$obs_time) | is.na(df$obs_time) | df$obs_time <= 0, to_num(df$Obstimes) + 1, df$obs_time)
df$obs_time <- ifelse(!is.finite(df$obs_time) | is.na(df$obs_time) | df$obs_time <= 0, 1, df$obs_time)

# one row per subject for RSF
df_subj <- df %>%
  mutate(charttime_dt = suppressWarnings(as.POSIXct(charttime))) %>%
  arrange(subject_id, charttime_dt, Obstimes) %>%
  group_by(subject_id) %>%
  slice(1) %>%
  ungroup()

item_cols <- names(df_subj)[grepl("^itemid_", names(df_subj))]
if (length(item_cols) != 22L) {
  message("提示：itemid_变量数=", length(item_cols), "（非22）")
}
for (v in item_cols) {
  df_subj[[v]] <- to_num(df_subj[[v]])
  df[[v]] <- to_num(df[[v]])
}

df_subj$age <- to_num(df_subj$anchor_age)
df_subj$sex <- as.factor(ifelse(toupper(trimws(as.character(df_subj$gender))) %in% c("M", "MALE"), "M", "F"))

# =========================
# 2) RSF function (reference cal_3_RSF spirit)
# =========================
run_rsf <- function(data_subj, vars_all, t0 = 30) {
  d <- data_subj %>% select(all_of(c(vars_all, "age", "sex", "obs_time", "event")))
  for (v in vars_all) {
    xv <- to_num(d[[v]])
    med <- suppressWarnings(median(xv, na.rm = TRUE))
    if (!is.finite(med)) med <- 0
    xv[is.na(xv)] <- med
    d[[v]] <- xv
  }
  d$age <- to_num(d$age)
  d$age[is.na(d$age)] <- median(d$age, na.rm = TRUE)
  d$sex <- as.factor(d$sex)
  d$event <- ifelse(d$event > 0, 1, 0)

  form <- as.formula(paste0("Surv(obs_time, event) ~ ", paste(c("age", "sex", vars_all), collapse = " + ")))

  fit <- tryCatch({
    randomForestSRC::rfsrc(
      formula = form,
      data = d,
      ntree = 300,
      mtry = max(1, floor((length(vars_all) + 2) / 3)),
      nodesize = 10,
      importance = FALSE,
      proximity = FALSE,
      seed = 123
    )
  }, error = function(e) NULL)

  if (is.null(fit)) return(c(AUC = NA_real_, BS = NA_real_, CINDEX = NA_real_))

  pred <- tryCatch(predict(fit, newdata = d), error = function(e) NULL)
  if (is.null(pred) || is.null(pred$survival) || is.null(pred$time.interest)) {
    return(c(AUC = NA_real_, BS = NA_real_, CINDEX = NA_real_))
  }

  t_idx <- which.min(abs(pred$time.interest - t0))
  surv_prob <- pred$survival[, t_idx]
  risk <- 1 - surv_prob

  st <- as.numeric(d$obs_time)
  se <- as.numeric(d$event)
  auc <- safe_auc(st, se, risk, t0)
  cidx <- safe_cindex(st, se, risk)
  bs <- safe_bs(st, se, surv_prob, t0)
  c(AUC = auc, BS = bs, CINDEX = cidx)
}

# =========================
# 3) JM function (reference cal_3 spirit)
# =========================
run_jm <- function(data_long, data_subj, vars_all, t0 = 30) {
  # Longitudinal data with Y constructed from selected vars (mean of selected)
  long0 <- data_long %>%
    transmute(
      ID = as.numeric(as.factor(subject_id)),
      t = to_num(Obstimes),
      obs_time = to_num(obs_time),
      event = as.numeric(ifelse(event > 0, 1, 0)),
      age = to_num(anchor_age),
      sex = as.factor(ifelse(toupper(trimws(as.character(gender))) %in% c("M", "MALE"), "M", "F")),
      Y = rowMeans(across(all_of(vars_all), ~ to_num(.x)), na.rm = TRUE)
    )

  # add two auxiliary covariates to mimic cal_3 with V-like vars
  long0$V3 <- to_num(data_long[[vars_all[1]]])
  long0$V4 <- to_num(data_long[[vars_all[2]]])
  long0$Y[!is.finite(long0$Y)] <- NA
  long0 <- long0 %>% filter(!is.na(Y), !is.na(t), is.finite(obs_time), obs_time > 0)
  if (nrow(long0) < 50) return(c(AUC = NA_real_, BS = NA_real_, CINDEX = NA_real_))
  long0 <- long0 %>% filter(t <= obs_time)
  if (nrow(long0) < 50) return(c(AUC = NA_real_, BS = NA_real_, CINDEX = NA_real_))

  # median impute
  for (v in c("age", "V3", "V4")) {
    xv <- to_num(long0[[v]])
    med <- suppressWarnings(median(xv, na.rm = TRUE))
    if (!is.finite(med)) med <- 0
    xv[is.na(xv)] <- med
    long0[[v]] <- xv
  }
  long0$sex <- as.factor(long0$sex)

  fit <- tryCatch({
    joineRML::mjoint(
      formLongFixed = list("Y" = Y ~ t + age + sex + V3 + V4),
      formLongRandom = list("Y" = ~ t | ID),
      formSurv = Surv(obs_time, event) ~ 1,
      data = long0,
      timeVar = "t"
    )
  }, error = function(e) NULL)
  if (is.null(fit)) return(c(AUC = NA_real_, BS = NA_real_, CINDEX = NA_real_))

  surv_data <- long0[!duplicated(long0$ID), ]
  st <- as.numeric(surv_data$obs_time)
  se <- as.numeric(surv_data$event)
  if (length(unique(se)) < 2) return(c(AUC = NA_real_, BS = NA_real_, CINDEX = NA_real_))

  beta <- fit$coefficients$beta
  gamma <- fit$coefficients$gamma
  re <- tryCatch(ranef(fit), error = function(e) NULL)
  if (is.null(re)) return(c(AUC = NA_real_, BS = NA_real_, CINDEX = NA_real_))

  bl <- surv_data
  bl$t <- t0
  X <- model.matrix(~ t + age + sex + V3 + V4, data = bl)
  fixed_part <- as.numeric(X %*% beta)
  rand_part <- re[, 1] + re[, 2] * t0
  risk <- as.numeric(gamma) * (fixed_part + rand_part)

  auc <- safe_auc(st, se, risk, t0)
  cidx <- safe_cindex(st, se, risk)

  # BS
  lp <- model.matrix(~ t + age + sex + V3 + V4, data = long0) %*% beta
  s0 <- tryCatch(summary(survfit(coxph(Surv(obs_time, event) ~ 1, data = long0)), times = t0)$surv, error = function(e) NA_real_)
  if (!is.finite(s0) || is.na(s0)) s0 <- 0.5
  s_pred <- s0 ^ exp(as.numeric(gamma) * as.numeric(lp))
  bs <- safe_bs(as.numeric(long0$obs_time), as.numeric(long0$event), s_pred, t0)
  c(AUC = auc, BS = bs, CINDEX = cidx)
}

# =========================
# 4) Define 4V and 10V sets (age/sex always included)
# =========================
if (length(item_cols) < 2L) stop("可用itemid变量不足")

# 4V: age+sex + choose 2 from itemids
pair_mat <- utils::combn(item_cols, 2)
n_4v <- ncol(pair_mat)

res_rsf_4v <- vector("list", n_4v)
res_jm_4v <- vector("list", n_4v)

for (i in seq_len(n_4v)) {
  vars_i <- pair_mat[, i]
  met_rsf <- run_rsf(df_subj, vars_i, t0 = t0)
  met_jm <- run_jm(df, df_subj, vars_i, t0 = t0)

  res_rsf_4v[[i]] <- data.frame(
    itemids = paste(c("age", "sex", vars_i), collapse = ";"),
    AUC = round4(met_rsf["AUC"]),
    BS = round4(met_rsf["BS"]),
    CINDEX = round4(met_rsf["CINDEX"]),
    stringsAsFactors = FALSE
  )
  res_jm_4v[[i]] <- data.frame(
    itemids = paste(c("age", "sex", vars_i), collapse = ";"),
    AUC = round4(met_jm["AUC"]),
    BS = round4(met_jm["BS"]),
    CINDEX = round4(met_jm["CINDEX"]),
    stringsAsFactors = FALSE
  )

  if (i %% 20 == 0 || i == n_4v) {
    message("4V进度: ", i, "/", n_4v)
  }
}

result_rsf_4v <- bind_rows(res_rsf_4v)
result_jm_4v <- bind_rows(res_jm_4v)

# 10V: age+sex + 8 itemids（分组遍历，参考你之前10V输出风格）
idx_10 <- split(seq_along(item_cols), ceiling(seq_along(item_cols) / 8))
groups_10 <- lapply(idx_10, function(ix) item_cols[ix])
n_10v <- length(groups_10)

res_rsf_10v <- vector("list", n_10v)
res_jm_10v <- vector("list", n_10v)

for (i in seq_along(groups_10)) {
  vars_i <- groups_10[[i]]
  met_rsf <- run_rsf(df_subj, vars_i, t0 = t0)
  met_jm <- run_jm(df, df_subj, vars_i, t0 = t0)

  res_rsf_10v[[i]] <- data.frame(
    itemids = paste(c("age", "sex", vars_i), collapse = ";"),
    AUC = round4(met_rsf["AUC"]),
    BS = round4(met_rsf["BS"]),
    CINDEX = round4(met_rsf["CINDEX"]),
    stringsAsFactors = FALSE
  )
  res_jm_10v[[i]] <- data.frame(
    itemids = paste(c("age", "sex", vars_i), collapse = ";"),
    AUC = round4(met_jm["AUC"]),
    BS = round4(met_jm["BS"]),
    CINDEX = round4(met_jm["CINDEX"]),
    stringsAsFactors = FALSE
  )
  message("10V进度: ", i, "/", n_10v)
}

result_rsf_10v <- bind_rows(res_rsf_10v)
result_jm_10v <- bind_rows(res_jm_10v)

# overwrite required files
write_xlsx(result_rsf_4v, out_rsf_4v)
write_xlsx(result_rsf_10v, out_rsf_10v)
write_xlsx(result_jm_4v, out_jm_4v)
write_xlsx(result_jm_10v, out_jm_10v)

# combined summary workbook
write_xlsx(
  list(
    RSF_4V = result_rsf_4v,
    RSF_10V = result_rsf_10v,
    JM_4V = result_jm_4v,
    JM_10V = result_jm_10v
  ),
  out_all
)

message("完成。")
message("Merged data: ", merged_out)
message("RSF_4V: ", out_rsf_4v)
message("RSF_10V: ", out_rsf_10v)
message("JM_4V: ", out_jm_4v)
message("JM_10V: ", out_jm_10v)
message("All-in-one: ", out_all)
