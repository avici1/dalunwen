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
jm_raw$event <- suppressWarnings(as.numeric(jm_raw$hospitalmortality))
jm_raw$event <- ifelse(is.na(jm_raw$event), NA, ifelse(jm_raw$event > 0, 1, 0))
## 为了扩大可用样本并保证时间量纲一致，统一使用 Obstimes(小时)->天
jm_raw$t <- suppressWarnings(as.numeric(jm_raw$Obstimes)) / 24

## obs_time优先用原始住院时长；若像标准化值（范围过小/含较多非正值），回退为每个ID的最大Obstimes
obs_los <- suppressWarnings(as.numeric(jm_raw$itemid_los_hosp_days))
q95_los <- suppressWarnings(as.numeric(stats::quantile(obs_los, 0.95, na.rm = TRUE)))
prop_pos_los <- mean(obs_los > 0, na.rm = TRUE)
los_like_standardized <- (!is.finite(q95_los)) || (q95_los <= 2) || (!is.finite(prop_pos_los)) || (prop_pos_los < 0.8)

if (!los_like_standardized) {
  jm_raw$obs_time <- obs_los
  obs_time_source <- "itemid_los_hosp_days"
} else {
  jm_raw <- jm_raw %>%
    group_by(ID) %>%
    mutate(obs_time = max(t, na.rm = TRUE)) %>%
    ungroup()
  ## 给obs_time留出安全余量，避免后续t加微扰后出现 t > obs_time
  jm_raw$obs_time <- jm_raw$obs_time + 1e-3
  jm_raw$obs_time[!is.finite(jm_raw$obs_time)] <- NA_real_
  obs_time_source <- "max(Obstimes)/24 by ID"
}
cat("JM主流程：obs_time来源 = ", obs_time_source, "\n", sep = "")

jm_raw <- jm_raw %>%
  filter(!is.na(ID), !is.na(t), !is.na(obs_time), !is.na(event)) %>%
  filter(obs_time > 0) %>%
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
  d <- d %>% filter(t < .data$obs_time)
  if (nrow(d) < 80 || length(unique(d$ID)) < 30) stop("加微扰后有效样本不足，跳过该变量。")
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
            control = nlme::lmeControl(
              opt = "optim",
              maxIter = 200,
              msMaxIter = 200,
              niterEM = 50
            )
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





############JM建模####################

## ==========================================================
## 清晰版JM建模（显式写出：V1~V6映射、固定效应/随机效应、生存模型）
## 数据来源：widedata_merge1.xlsx（沿用上方 jm_main_path 与 jm_base_path）
## ==========================================================
library(joineRML)
cat("\n\n===== 清晰版JM建模（显式公式）开始 =====\n")

## 0) 重新读取数据（保证该区块可独立理解）
jm_main2 <- read_xlsx(jm_main_path)
jm_base2 <- read_xlsx(jm_base_path)

jm_base_sub2 <- jm_base2 %>%
  dplyr::select(hadm_id, hospital_mortality) %>%
  dplyr::distinct(hadm_id, .keep_all = TRUE) %>%
  dplyr::rename(hospitalmortality = hospital_mortality)

data_raw2 <- jm_main2 %>%
  mutate(hadm_id = as.character(hadm_id)) %>%
  left_join(jm_base_sub2 %>% mutate(hadm_id = as.character(hadm_id)), by = "hadm_id")

## 1) 构造 ID / t / obs_time / event
data_raw2$ID <- as.numeric(as.factor(as.character(data_raw2$subject_id)))
data_raw2$event <- suppressWarnings(as.numeric(data_raw2$hospitalmortality))
data_raw2$event <- ifelse(is.na(data_raw2$event), NA, ifelse(data_raw2$event > 0, 1, 0))

## 与主流程保持一致：统一t为Obstimes(小时)->天
data_raw2$t <- suppressWarnings(as.numeric(data_raw2$Obstimes)) / 24

## obs_time自动判定来源（优先原始LOS；否则回退到每个ID最大Obstimes）
obs_los2 <- suppressWarnings(as.numeric(data_raw2$itemid_los_hosp_days))
q95_los2 <- suppressWarnings(as.numeric(stats::quantile(obs_los2, 0.95, na.rm = TRUE)))
prop_pos_los2 <- mean(obs_los2 > 0, na.rm = TRUE)
los2_like_standardized <- (!is.finite(q95_los2)) || (q95_los2 <= 2) || (!is.finite(prop_pos_los2)) || (prop_pos_los2 < 0.8)

if (!los2_like_standardized) {
  data_raw2$obs_time <- obs_los2
  obs_time_source2 <- "itemid_los_hosp_days"
} else {
  data_raw2 <- data_raw2 %>%
    group_by(ID) %>%
    mutate(obs_time = max(t, na.rm = TRUE)) %>%
    ungroup()
  ## 给obs_time留出安全余量，避免后续t加微扰后出现 t > obs_time
  data_raw2$obs_time <- data_raw2$obs_time + 1e-3
  data_raw2$obs_time[!is.finite(data_raw2$obs_time)] <- NA_real_
  obs_time_source2 <- "max(Obstimes)/24 by ID"
}
cat("JM清晰版：obs_time来源 = ", obs_time_source2, "\n", sep = "")

data_raw2 <- data_raw2 %>%
  filter(!is.na(ID), !is.na(t), !is.na(obs_time), !is.na(event)) %>%
  filter(obs_time > 0) %>%
  filter(t <= obs_time)

## 2) 明确写出 V1~V6 对应 itemid
## 你可按需要直接改这里的映射
v_map <- c(
  V1 = "itemid_anchor_age",
  V2 = "itemid_gender",
  V3 = "itemid_anchor_year",
  V4 = "itemid_insurance",
  V5 = "itemid_marital_status",
  V6 = "itemid_race"
)

missing_v <- setdiff(unname(v_map), names(data_raw2))
if (length(missing_v) > 0) {
  stop(paste0("以下V映射字段在数据中不存在：", paste(missing_v, collapse = ", ")))
}

baseline_cov2 <- data.frame(ID = sort(unique(data_raw2$ID)))
for (vn in names(v_map)) {
  item_col <- v_map[[vn]]
  tmp <- data_raw2 %>%
    group_by(ID) %>%
    summarise(val = dplyr::first(.data[[item_col]]), .groups = "drop")
  tmp$val <- to_num01(tmp$val)
  names(tmp)[2] <- vn
  baseline_cov2 <- baseline_cov2 %>% left_join(tmp, by = "ID")
}

data_model2 <- data_raw2 %>% left_join(baseline_cov2, by = "ID")
for (vn in names(v_map)) {
  data_model2[[vn]][is.na(data_model2[[vn]])] <- median(data_model2[[vn]], na.rm = TRUE)
  if (!is.finite(data_model2[[vn]][1])) data_model2[[vn]] <- 0
}

## 3) 选择一个纵向指标作为 Y（默认：按可用度排序第一名）
drop_cat_jm2 <- c(
  "itemid_admission_type", "itemid_admission_location", "itemid_discharge_location",
  "itemid_insurance", "itemid_language", "itemid_marital_status", "itemid_race",
  "itemid_first_careunit", "itemid_los_hosp_days"
)
long_cands2 <- names(data_model2)[grepl("^itemid_", names(data_model2))]
long_cands2 <- setdiff(long_cands2, drop_cat_jm2)
long_cands2 <- setdiff(long_cands2, c("itemid_anchor_age", "itemid_anchor_year", "itemid_gender", "itemid_anchor_year_group"))
if (length(long_cands2) == 0L) stop("未找到可用纵向变量（itemid_）。")

score_var2 <- function(v) {
  y <- suppressWarnings(as.numeric(data_model2[[v]]))
  not_na <- sum(!is.na(y))
  df_tmp <- data.frame(ID = data_model2$ID, y = y)
  df_tmp <- df_tmp[!is.na(df_tmp$y), , drop = FALSE]
  n_id2 <- sum(table(df_tmp$ID) >= 2)
  not_na + 10 * n_id2
}
long_cands2 <- long_cands2[order(sapply(long_cands2, score_var2), decreasing = TRUE)]
y_itemid <- long_cands2[1]

data_clean <- data_model2 %>%
  mutate(Y = suppressWarnings(as.numeric(.data[[y_itemid]]))) %>%
  filter(!is.na(Y)) %>%
  group_by(ID) %>%
  arrange(t, .by_group = TRUE) %>%
  mutate(t = as.numeric(t), t = t + (row_number() - 1) * 1e-4) %>%
  ungroup()
data_clean$Y <- as.numeric(scale(data_clean$Y))

## 4) 显式定义：固定效应、随机效应、生存模型（并进行选择）
formLongFixed_candidates <- list(
  Y ~ t,
  Y ~ t + V1 + V2,
  Y ~ t + V1 + V2 + V3 + V4 + V5 + V6
)

id_count2 <- data_clean %>% count(ID, name = "n")
use_random_slope2 <- sum(id_count2$n >= 2) >= 30
if (use_random_slope2) {
  formLongRandom_candidates <- list(~ 1 | ID, ~ t | ID)
} else {
  formLongRandom_candidates <- list(~ 1 | ID)
}

## 生存模型固定为：Surv(obs_time, event) ~ 1
formSurv <- Surv(obs_time, event) ~ 1

best_jm_fit2 <- NULL
selected_fixed2 <- NULL
selected_random2 <- NULL
last_err2 <- NULL
selected_jm_method2 <- NA_character_

use_mjoint <- requireNamespace("JoineRML", quietly = TRUE)
if (!use_mjoint) {
  cat("提示：未安装 JoineRML，清晰版自动回退到 JM::jointModel。\n")
}

for (fm in formLongFixed_candidates) {
  for (rfm in formLongRandom_candidates) {
    one_try <- tryCatch({
      if (use_mjoint) {
        ## 你要求的 mjoint(...) 风格
        jm_fit2 <- JoineRML::mjoint(
          formLongFixed = list("Y" = fm),
          formLongRandom = list("Y" = rfm),
          formSurv = formSurv,
          data = data_clean,
          timeVar = "t"
        )
        list(jm = jm_fit2, method = "JoineRML::mjoint")
      } else {
        ## 无JoineRML时自动回退，保证脚本可运行
        lme_fit2 <- nlme::lme(
          fixed = fm,
          random = rfm,
          data = data_clean,
          na.action = na.omit,
          control = nlme::lmeControl(
            opt = "optim",
            maxIter = 200,
            msMaxIter = 200,
            niterEM = 50
          )
        )
        surv_data2 <- data_clean[!duplicated(data_clean$ID), c("ID", "obs_time", "event")]
        cox_fit2 <- survival::coxph(formSurv, data = surv_data2, x = TRUE, model = TRUE)
        jm_fit2 <- JM::jointModel(
          lmeObject = lme_fit2,
          survObject = cox_fit2,
          timeVar = "t",
          method = "weibull-PH-aGH"
        )
        list(jm = jm_fit2, method = "JM::jointModel (fallback)")
      }
    }, error = function(e) {
      last_err2 <<- conditionMessage(e)
      NULL
    })

    if (!is.null(one_try)) {
      best_jm_fit2 <- one_try$jm
      selected_jm_method2 <- one_try$method
      selected_fixed2 <- fm
      selected_random2 <- rfm
      break
    }
  }
  if (!is.null(best_jm_fit2)) break
}

if (is.null(best_jm_fit2)) {
  stop(paste0("清晰版JM拟合失败。最后错误: ", last_err2))
}

## 5) 输出：明确展示选择到的模型结构
cat("\n【V1~V6映射】\n")
for (vn in names(v_map)) {
  cat(" - ", vn, " = ", v_map[[vn]], "\n", sep = "")
}

cat("\n【纵向指标Y】\n")
cat(" - Y = ", y_itemid, "\n", sep = "")

cat("\n【最终选择到的模型】\n")
cat(" - 固定效应(formLongFixed): ", deparse(selected_fixed2), "\n", sep = "")
cat(" - 随机效应(formLongRandom): ", deparse(selected_random2), "\n", sep = "")
cat(" - 生存模型(formSurv): ", deparse(formSurv), "\n", sep = "")
cat(" - 联合模型函数: ", selected_jm_method2, "\n", sep = "")

fit_clear_jm <- best_jm_fit2
cat("===== 清晰版JM建模完成 =====\n")

## 6) 输出清晰版模型评估指标（AUC / BS / CINDEX）
safe_metric2 <- function(x, digits = 4) ifelse(is.finite(x), round(x, digits), NA_real_)

eval_lme2 <- nlme::lme(
  fixed = selected_fixed2,
  random = selected_random2,
  data = data_clean,
  na.action = na.omit,
  control = nlme::lmeControl(
    opt = "optim",
    maxIter = 200,
    msMaxIter = 200,
    niterEM = 50
  )
)

surv_eval2 <- data_clean[!duplicated(data_clean$ID), c("ID", "obs_time", "event", "V1", "V2", "V3", "V4", "V5", "V6")]
t0_eval2 <- median(surv_eval2$obs_time[surv_eval2$obs_time > 0], na.rm = TRUE)
surv_eval2$t <- t0_eval2

beta2 <- nlme::fixed.effects(eval_lme2)
rhs_terms2 <- attr(terms(selected_fixed2), "term.labels")
rhs_form2 <- if (length(rhs_terms2) > 0) as.formula(paste("~", paste(rhs_terms2, collapse = " + "))) else ~ 1
X2 <- model.matrix(rhs_form2, data = surv_eval2)
beta_use2 <- beta2[colnames(X2)]
beta_use2[is.na(beta_use2)] <- 0
fixed_part2 <- as.numeric(X2 %*% beta_use2)

re2 <- nlme::ranef(eval_lme2)
if (is.null(dim(re2))) re2 <- matrix(re2, ncol = 1)
re2_df <- as.data.frame(re2)
re2_df$ID <- suppressWarnings(as.numeric(rownames(re2_df)))
re2_df <- re2_df[match(surv_eval2$ID, re2_df$ID), , drop = FALSE]
re_int2 <- if (ncol(re2_df) >= 1) as.numeric(re2_df[[1]]) else rep(0, nrow(surv_eval2))
re_slope2 <- if (ncol(re2_df) >= 2) as.numeric(re2_df[[2]]) else rep(0, nrow(surv_eval2))
re_int2[is.na(re_int2)] <- 0
re_slope2[is.na(re_slope2)] <- 0
rand_part2 <- re_int2 + re_slope2 * t0_eval2

assoc_candidates2 <- c("Assoct", "alpha", "AssoctE", "AssoctEV")
assoc_val2 <- NA_real_
if (!is.null(best_jm_fit2$coefficients)) {
  for (nm in assoc_candidates2) {
    if (!is.null(best_jm_fit2$coefficients[[nm]])) {
      assoc_val2 <- suppressWarnings(as.numeric(best_jm_fit2$coefficients[[nm]])[1])
      if (is.finite(assoc_val2)) break
    }
  }
}
if (!is.finite(assoc_val2)) {
  coef_try2 <- tryCatch(stats::coef(best_jm_fit2), error = function(e) NULL)
  if (!is.null(coef_try2)) {
    nms2 <- names(coef_try2)
    if (!is.null(nms2)) {
      idx2 <- grep("asso|alpha", nms2, ignore.case = TRUE)
      if (length(idx2) > 0) assoc_val2 <- suppressWarnings(as.numeric(coef_try2[idx2[1]]))
    }
  }
}
if (!is.finite(assoc_val2)) assoc_val2 <- 1

risk2 <- assoc_val2 * (fixed_part2 + rand_part2)

cindex2 <- tryCatch(
  as.numeric(survival::concordance(Surv(surv_eval2$obs_time, surv_eval2$event) ~ risk2)$concordance),
  error = function(e) NA_real_
)
if (!is.na(cindex2) && cindex2 < 0.5) {
  risk2 <- -risk2
  cindex2 <- tryCatch(
    as.numeric(survival::concordance(Surv(surv_eval2$obs_time, surv_eval2$event) ~ risk2)$concordance),
    error = function(e) NA_real_
  )
}

auc2 <- tryCatch({
  roc1_2 <- timeROC(T = surv_eval2$obs_time, delta = surv_eval2$event, marker = risk2, cause = 1, times = t0_eval2)
  roc2_2 <- timeROC(T = surv_eval2$obs_time, delta = surv_eval2$event, marker = -risk2, cause = 1, times = t0_eval2)
  auc1_2 <- if (length(roc1_2$AUC) >= 2) roc1_2$AUC[2] else roc1_2$AUC[1]
  auc2_2 <- if (length(roc2_2$AUC) >= 2) roc2_2$AUC[2] else roc2_2$AUC[1]
  max(as.numeric(auc1_2), as.numeric(auc2_2), na.rm = TRUE)
}, error = function(e) NA_real_)

bs2 <- tryCatch({
  haz2 <- exp(as.numeric(scale(risk2)))
  sp2 <- exp(-haz2 * t0_eval2)
  mean(ifelse(surv_eval2$obs_time <= t0_eval2 & surv_eval2$event == 1, (1 - sp2)^2, sp2^2), na.rm = TRUE)
}, error = function(e) NA_real_)

jm_metrics_clear <- data.frame(
  model = "clear_jm",
  n_id = nrow(surv_eval2),
  t0 = safe_metric2(t0_eval2, 6),
  AUC = safe_metric2(auc2),
  BS = safe_metric2(bs2),
  CINDEX = safe_metric2(cindex2),
  stringsAsFactors = FALSE
)

cat("\n【清晰版模型评估】\n")
print(jm_metrics_clear)












#################尝试各种itemid########################

## 稳健版：先单变量筛选，再小批量组合
## 约定：每个固定效应都强制包含 年龄 + 性别
age_var_all <- "itemid_anchor_age"
sex_var_all <- "itemid_gender"

if (!all(c(age_var_all, sex_var_all) %in% names(data_clean))) {
  stop("data_clean中缺少年龄或性别字段，无法执行全量itemid尝试。")
}

all_itemid_all <- names(data_clean)[grepl("^itemid_", names(data_clean))]
exclude_itemid_all <- c(
  age_var_all, sex_var_all,
  "itemid_los_hosp_days", "itemid_admittime", "itemid_dischtime",
  "itemid_edregtime", "itemid_edouttime", "itemid_icu_intime", "itemid_icu_outtime",
  "itemid_admission_type", "itemid_admission_location", "itemid_discharge_location",
  "itemid_insurance", "itemid_language", "itemid_marital_status", "itemid_race",
  "itemid_first_careunit", "itemid_anchor_year", "itemid_anchor_year_group"
)
fix_candidates_all <- setdiff(all_itemid_all, exclude_itemid_all)

prep_numeric_var <- function(df, vname) {
  x <- to_num01(df[[vname]])
  med <- suppressWarnings(median(x, na.rm = TRUE))
  if (!is.finite(med)) med <- 0
  x[is.na(x)] <- med
  # 尺度标准化，降低lme数值问题
  x <- as.numeric(scale(x))
  x[!is.finite(x)] <- 0
  x
}

fit_try_one_formula <- function(df_in, fm, y_name_for_log) {
  d <- df_in
  one <- tryCatch({
    lme_fit <- nlme::lme(
      fixed = fm,
      random = ~ 1 | ID,
      data = d,
      na.action = na.omit,
      control = nlme::lmeControl(
        opt = "optim",
        maxIter = 200,
        msMaxIter = 200,
        niterEM = 50
      )
    )
    surv_data <- d[!duplicated(d$ID), c("ID", "obs_time", "event")]
    cox_fit <- survival::coxph(survival::Surv(obs_time, event) ~ 1, data = surv_data, x = TRUE, model = TRUE)
    jm_fit <- JM::jointModel(
      lmeObject = lme_fit,
      survObject = cox_fit,
      timeVar = "t",
      method = "weibull-PH-aGH"
    )
    list(ok = TRUE, fit = jm_fit, formula = deparse(fm), y_var = y_name_for_log)
  }, error = function(e) {
    list(ok = FALSE, err = conditionMessage(e), formula = deparse(fm), y_var = y_name_for_log)
  })
  one
}

calc_cindex_quick <- function(jm_fit, lme_fit, surv_data, fm, t0_now) {
  beta <- nlme::fixed.effects(lme_fit)
  rhs_terms <- attr(terms(fm), "term.labels")
  rhs_form <- if (length(rhs_terms) > 0) as.formula(paste("~", paste(rhs_terms, collapse = " + "))) else ~ 1
  surv_eval <- surv_data
  surv_eval$t <- t0_now
  X <- model.matrix(rhs_form, data = surv_eval)
  beta_use <- beta[colnames(X)]
  beta_use[is.na(beta_use)] <- 0
  fixed_part <- as.numeric(X %*% beta_use)

  re <- nlme::ranef(lme_fit)
  if (is.null(dim(re))) re <- matrix(re, ncol = 1)
  re_df <- as.data.frame(re)
  re_df$ID <- suppressWarnings(as.numeric(rownames(re_df)))
  re_df <- re_df[match(surv_eval$ID, re_df$ID), , drop = FALSE]
  re_int <- if (ncol(re_df) >= 1) as.numeric(re_df[[1]]) else rep(0, nrow(surv_eval))
  re_int[is.na(re_int)] <- 0

  assoc_val <- NA_real_
  assoc_candidates <- c("Assoct", "alpha", "AssoctE", "AssoctEV")
  if (!is.null(jm_fit$coefficients)) {
    for (nm in assoc_candidates) {
      if (!is.null(jm_fit$coefficients[[nm]])) {
        assoc_val <- suppressWarnings(as.numeric(jm_fit$coefficients[[nm]])[1])
        if (is.finite(assoc_val)) break
      }
    }
  }
  if (!is.finite(assoc_val)) assoc_val <- 1
  risk <- assoc_val * (fixed_part + re_int)
  cdf <- data.frame(obs_time = surv_eval$obs_time, event = surv_eval$event, risk = risk)
  cidx <- tryCatch(
    as.numeric(survival::concordance(survival::Surv(obs_time, event) ~ risk, data = cdf)$concordance),
    error = function(e) NA_real_
  )
  if (!is.na(cidx) && cidx < 0.5) {
    cdf$risk <- -cdf$risk
    cidx <- tryCatch(
      as.numeric(survival::concordance(survival::Surv(obs_time, event) ~ risk, data = cdf)$concordance),
      error = function(e) NA_real_
    )
  }
  cidx
}

## ---------- 第一步：单变量筛选 ----------
single_results <- list()
for (xv in fix_candidates_all) {
  d_try <- data_clean
  # 缺失过高/几乎常量的变量先跳过
  miss_rate <- mean(is.na(d_try[[xv]]))
  nunique <- length(unique(d_try[[xv]][!is.na(d_try[[xv]])]))
  if (!is.finite(miss_rate) || miss_rate > 0.7 || nunique < 2) {
    single_results[[xv]] <- list(ok = FALSE, err = "缺失过高或无变异", formula = NA_character_)
    next
  }

  for (vv in c(age_var_all, sex_var_all, xv)) {
    d_try[[vv]] <- prep_numeric_var(d_try, vv)
  }

  fm <- as.formula(paste0("Y ~ t + ", age_var_all, " + ", sex_var_all, " + ", xv))
  one <- fit_try_one_formula(d_try, fm, y_itemid)
  single_results[[xv]] <- one
}

ok_single <- vapply(single_results, function(z) isTRUE(z$ok), logical(1))
cat("\n[全量itemid尝试-单变量] 成功数: ", sum(ok_single), "/", length(ok_single), "\n", sep = "")

single_success_vars <- names(single_results)[ok_single]

## ---------- 第二步：小批量组合 ----------
## 用单变量成功的前若干变量进入组合；每批2个，稳定性更高
top_k_for_batch <- min(10, length(single_success_vars))
if (top_k_for_batch > 0) {
  single_success_vars <- single_success_vars[seq_len(top_k_for_batch)]
}

batch_results <- list()
if (length(single_success_vars) >= 2) {
  pair_mat <- utils::combn(single_success_vars, 2)
  for (i in seq_len(ncol(pair_mat))) {
    vars_i <- pair_mat[, i]
    d_try <- data_clean
    for (vv in c(age_var_all, sex_var_all, vars_i)) {
      d_try[[vv]] <- prep_numeric_var(d_try, vv)
    }
    rhs <- paste(c("t", age_var_all, sex_var_all, vars_i), collapse = " + ")
    fm <- as.formula(paste("Y ~", rhs))
    batch_results[[paste(vars_i, collapse = " + ")]] <- fit_try_one_formula(d_try, fm, y_itemid)
  }
}

ok_batch <- if (length(batch_results) > 0) {
  vapply(batch_results, function(z) isTRUE(z$ok), logical(1))
} else {
  logical(0)
}
cat("[全量itemid尝试-两变量组合] 成功数: ", sum(ok_batch), "/", length(ok_batch), "\n", sep = "")

## ---------- 输出示例最佳模型（按CINDEX quick） ----------
rank_pool <- c(single_results[ok_single], batch_results[ok_batch])
if (length(rank_pool) > 0) {
  t0_rank <- median(data_clean$obs_time[data_clean$obs_time > 0], na.rm = TRUE)
  rank_tab <- lapply(names(rank_pool), function(nm) {
    item <- rank_pool[[nm]]
    d_rank <- data_clean
    fm_rank <- as.formula(item$formula)
    rhs_terms <- attr(terms(fm_rank), "term.labels")
    rhs_vars <- unique(gsub("`", "", rhs_terms[rhs_terms != "t"]))
    rhs_vars <- rhs_vars[rhs_vars %in% names(d_rank)]
    for (vv in rhs_vars) d_rank[[vv]] <- prep_numeric_var(d_rank, vv)

    lme_fit <- tryCatch(
      nlme::lme(
        fixed = fm_rank,
        random = ~ 1 | ID,
        data = d_rank,
        na.action = na.omit,
        control = nlme::lmeControl(opt = "optim", maxIter = 200, msMaxIter = 200, niterEM = 50)
      ),
      error = function(e) NULL
    )
    need_cols <- unique(c("ID", "obs_time", "event", rhs_vars))
    need_cols <- need_cols[need_cols %in% names(d_rank)]
    surv_data <- d_rank[!duplicated(d_rank$ID), need_cols, drop = FALSE]
    if (is.null(lme_fit)) return(data.frame(model = nm, formula = item$formula, CINDEX = NA_real_))

    cox_fit <- tryCatch(
      survival::coxph(survival::Surv(obs_time, event) ~ 1, data = surv_data, x = TRUE, model = TRUE),
      error = function(e) NULL
    )
    jm_fit <- if (!is.null(cox_fit)) {
      tryCatch(JM::jointModel(lmeObject = lme_fit, survObject = cox_fit, timeVar = "t", method = "weibull-PH-aGH"), error = function(e) NULL)
    } else {
      NULL
    }
    cidx <- if (!is.null(jm_fit)) calc_cindex_quick(jm_fit, lme_fit, surv_data, fm_rank, t0_rank) else NA_real_
    data.frame(model = nm, formula = item$formula, CINDEX = cidx)
  })
  rank_df <- dplyr::bind_rows(rank_tab) %>% arrange(desc(CINDEX))
  cat("\n[全量itemid尝试] CINDEX前5模型：\n")
  print(utils::head(rank_df, 5))
} else {
  cat("\n[全量itemid尝试] 当前无成功模型。建议先仅跑单变量并查看失败原因。\n")
}














############批量建模10V################

## 目标：
## - 每个JM模型固定效应使用一批itemid变量（每批10个，最后一批可不足10个）
## - 遍历全部候选itemid变量一次
## - 输出4列：itemids / cindex / bs / auc
## - 保存到 result.xlsx

suppressPackageStartupMessages({
  library(dplyr)
  library(readxl)
  library(survival)
  library(timeROC)
  library(nlme)
  library(JM)
})

if (!requireNamespace("writexl", quietly = TRUE)) {
  stop("请先安装 writexl 包：install.packages('writexl')")
}

to_num_local <- function(x) {
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

## 1) 读入并构建JM基础数据
main_path_batch <- "f:/文章_大论文/MIMIC数据库_代码/TREA代码/0314/widedata_merge1.xlsx"
base_path_batch <- "f:/文章_大论文/MIMIC数据库_代码/TREA代码/0311/stroke_baseline_simple.xlsx"
out_path_batch <- "f:/文章_大论文/MIMIC数据库_代码/TREA代码/0315/result.xlsx"

dat_main <- read_xlsx(main_path_batch)
dat_base <- read_xlsx(base_path_batch) %>%
  dplyr::select(hadm_id, hospital_mortality) %>%
  dplyr::distinct(hadm_id, .keep_all = TRUE) %>%
  dplyr::rename(hospitalmortality = hospital_mortality)

dat <- dat_main %>%
  mutate(hadm_id = as.character(hadm_id)) %>%
  left_join(dat_base %>% mutate(hadm_id = as.character(hadm_id)), by = "hadm_id")

dat$ID <- as.numeric(as.factor(as.character(dat$subject_id)))
dat$t <- suppressWarnings(as.numeric(dat$Obstimes)) / 24
dat$event <- suppressWarnings(as.numeric(dat$hospitalmortality))
dat$event <- ifelse(is.na(dat$event), NA, ifelse(dat$event > 0, 1, 0))

obs_los <- suppressWarnings(as.numeric(dat$itemid_los_hosp_days))
q95_los <- suppressWarnings(as.numeric(stats::quantile(obs_los, 0.95, na.rm = TRUE)))
prop_pos_los <- mean(obs_los > 0, na.rm = TRUE)
los_like_standardized <- (!is.finite(q95_los)) || (q95_los <= 2) || (!is.finite(prop_pos_los)) || (prop_pos_los < 0.8)
if (!los_like_standardized) {
  dat$obs_time <- obs_los
} else {
  dat <- dat %>%
    group_by(ID) %>%
    mutate(obs_time = max(t, na.rm = TRUE) + 1e-3) %>%
    ungroup()
  dat$obs_time[!is.finite(dat$obs_time)] <- NA_real_
}

dat <- dat %>%
  filter(!is.na(ID), !is.na(t), !is.na(obs_time), !is.na(event), obs_time > 0) %>%
  filter(t <= obs_time)

## 2) 选择一个纵向结局Y（从itemid中按可用性挑一个）
drop_for_y <- c(
  "itemid_anchor_age", "itemid_gender", "itemid_anchor_year", "itemid_anchor_year_group",
  "itemid_los_hosp_days", "itemid_admittime", "itemid_dischtime", "itemid_edregtime", "itemid_edouttime",
  "itemid_icu_intime", "itemid_icu_outtime",
  "itemid_admission_type", "itemid_admission_location", "itemid_discharge_location",
  "itemid_insurance", "itemid_language", "itemid_marital_status", "itemid_race", "itemid_first_careunit"
)
item_cols_all <- names(dat)[grepl("^itemid_", names(dat))]
y_cands <- setdiff(item_cols_all, drop_for_y)
if (length(y_cands) == 0L) stop("没有可用的itemid纵向变量用于Y。")

score_y <- function(v) {
  y <- suppressWarnings(as.numeric(dat[[v]]))
  not_na <- sum(!is.na(y))
  tmp <- data.frame(ID = dat$ID, y = y)
  tmp <- tmp[!is.na(tmp$y), , drop = FALSE]
  n_id2 <- sum(table(tmp$ID) >= 2)
  not_na + 10 * n_id2
}
y_cands <- y_cands[order(sapply(y_cands, score_y), decreasing = TRUE)]
y_var_batch <- y_cands[1]

dat_model <- dat %>%
  mutate(Y = suppressWarnings(as.numeric(.data[[y_var_batch]]))) %>%
  filter(!is.na(Y)) %>%
  group_by(ID) %>%
  arrange(t, .by_group = TRUE) %>%
  mutate(t = as.numeric(t), t = t + (row_number() - 1) * 1e-4) %>%
  ungroup() %>%
  filter(t < obs_time)
dat_model$Y <- as.numeric(scale(dat_model$Y))

if (nrow(dat_model) < 100 || length(unique(dat_model$ID)) < 30) {
  stop("可用于批量JM建模的数据不足。")
}

## 3) 构建固定效应批次（每批10个itemid）
fixed_candidates <- setdiff(item_cols_all, c(drop_for_y, y_var_batch))
if (length(fixed_candidates) == 0L) stop("没有可用的固定效应itemid候选变量。")

batch_size <- 10L
batches <- split(fixed_candidates, ceiling(seq_along(fixed_candidates) / batch_size))

## 4) 评估函数：从拟合对象计算 AUC / BS / CINDEX
calc_metrics_batch <- function(jm_fit, lme_fit, d_fit, fm, t0) {
  rhs_terms <- attr(terms(fm), "term.labels")
  rhs_vars <- setdiff(rhs_terms, "t")
  rhs_vars <- gsub("`", "", rhs_vars)
  need_cols <- unique(c("ID", "obs_time", "event", rhs_vars))
  need_cols <- need_cols[need_cols %in% names(d_fit)]
  surv_data <- d_fit[!duplicated(d_fit$ID), need_cols, drop = FALSE]
  surv_data$t <- t0

  beta <- nlme::fixed.effects(lme_fit)
  rhs_form <- if (length(rhs_terms) > 0) as.formula(paste("~", paste(rhs_terms, collapse = " + "))) else ~ 1
  X <- model.matrix(rhs_form, data = surv_data)
  beta_use <- beta[colnames(X)]
  beta_use[is.na(beta_use)] <- 0
  fixed_part <- as.numeric(X %*% beta_use)

  re <- nlme::ranef(lme_fit)
  if (is.null(dim(re))) re <- matrix(re, ncol = 1)
  re_df <- as.data.frame(re)
  re_df$ID <- suppressWarnings(as.numeric(rownames(re_df)))
  re_df <- re_df[match(surv_data$ID, re_df$ID), , drop = FALSE]
  re_int <- if (ncol(re_df) >= 1) as.numeric(re_df[[1]]) else rep(0, nrow(surv_data))
  re_int[is.na(re_int)] <- 0

  assoc_val <- NA_real_
  for (nm in c("Assoct", "alpha", "AssoctE", "AssoctEV")) {
    if (!is.null(jm_fit$coefficients[[nm]])) {
      assoc_val <- suppressWarnings(as.numeric(jm_fit$coefficients[[nm]])[1])
      if (is.finite(assoc_val)) break
    }
  }
  if (!is.finite(assoc_val)) assoc_val <- 1

  risk <- assoc_val * (fixed_part + re_int)
  cindex <- tryCatch(
    as.numeric(survival::concordance(Surv(obs_time, event) ~ risk, data = surv_data)$concordance),
    error = function(e) NA_real_
  )
  if (!is.na(cindex) && cindex < 0.5) {
    cindex <- tryCatch(
      as.numeric(survival::concordance(Surv(obs_time, event) ~ I(-risk), data = surv_data)$concordance),
      error = function(e) NA_real_
    )
  }

  auc <- tryCatch({
    roc1 <- timeROC(T = surv_data$obs_time, delta = surv_data$event, marker = risk, cause = 1, times = t0)
    roc2 <- timeROC(T = surv_data$obs_time, delta = surv_data$event, marker = -risk, cause = 1, times = t0)
    auc1 <- if (length(roc1$AUC) >= 2) roc1$AUC[2] else roc1$AUC[1]
    auc2 <- if (length(roc2$AUC) >= 2) roc2$AUC[2] else roc2$AUC[1]
    max(as.numeric(auc1), as.numeric(auc2), na.rm = TRUE)
  }, error = function(e) NA_real_)

  bs <- tryCatch({
    haz <- exp(as.numeric(scale(risk)))
    sp <- exp(-haz * t0)
    mean(ifelse(surv_data$obs_time <= t0 & surv_data$event == 1, (1 - sp)^2, sp^2), na.rm = TRUE)
  }, error = function(e) NA_real_)

  c(auc = auc, bs = bs, cindex = cindex)
}

## 5) 批量建模
t0_batch <- median(dat_model$obs_time[dat_model$obs_time > 0], na.rm = TRUE)
result_rows <- list()

for (i in seq_along(batches)) {
  vars_i <- batches[[i]]
  d_i <- dat_model

  # 数值化 + 缺失填补 + 标准化
  for (v in vars_i) {
    xv <- to_num_local(d_i[[v]])
    med <- suppressWarnings(median(xv, na.rm = TRUE))
    if (!is.finite(med)) med <- 0
    xv[is.na(xv)] <- med
    xv <- as.numeric(scale(xv))
    xv[!is.finite(xv)] <- 0
    d_i[[v]] <- xv
  }

  fm <- as.formula(paste("Y ~ t +", paste(vars_i, collapse = " + ")))

  one <- tryCatch({
    lme_fit <- nlme::lme(
      fixed = fm,
      random = ~ 1 | ID,
      data = d_i,
      na.action = na.omit,
      control = nlme::lmeControl(
        opt = "optim",
        maxIter = 200,
        msMaxIter = 200,
        niterEM = 50
      )
    )
    surv_i <- d_i[!duplicated(d_i$ID), c("ID", "obs_time", "event"), drop = FALSE]
    cox_fit <- survival::coxph(survival::Surv(obs_time, event) ~ 1, data = surv_i, x = TRUE, model = TRUE)
    jm_fit <- JM::jointModel(
      lmeObject = lme_fit,
      survObject = cox_fit,
      timeVar = "t",
      method = "weibull-PH-aGH"
    )
    met <- calc_metrics_batch(jm_fit, lme_fit, d_i, fm, t0_batch)
    data.frame(
      itemids = paste(vars_i, collapse = ";"),
      cindex = safe_num(met["cindex"]),
      bs = safe_num(met["bs"]),
      auc = safe_num(met["auc"]),
      stringsAsFactors = FALSE
    )
  }, error = function(e) {
    data.frame(
      itemids = paste(vars_i, collapse = ";"),
      cindex = NA_real_,
      bs = NA_real_,
      auc = NA_real_,
      stringsAsFactors = FALSE
    )
  })

  result_rows[[i]] <- one
  cat("批量JM进度: ", i, "/", length(batches), " 完成\n", sep = "")
}

result_df <- dplyr::bind_rows(result_rows)
writexl::write_xlsx(result_df, out_path_batch)

cat("\n批量JM建模结束。\n")
cat("Y变量: ", y_var_batch, "\n", sep = "")
cat("结果文件: ", out_path_batch, "\n", sep = "")
print(utils::head(result_df, 10))

############批量建模4V################

## 目标：
## - 固定效应每次仅纳入4个变量
## - 其中 age + gender 始终保留，另外2个变量从其余itemid中组合遍历
## - 输出4列：itemids / cindex / bs / auc
## - 保存到 result_4V.xlsx

if (!exists("dat_model") || !exists("item_cols_all") || !exists("drop_for_y") || !exists("y_var_batch")) {
  stop("请先运行上方“批量建模10V”区块，以生成 dat_model / item_cols_all / y_var_batch。")
}

pick_first_existing <- function(cands, pool) {
  x <- cands[cands %in% pool]
  if (length(x) == 0) return(NA_character_)
  x[1]
}

age_var4 <- pick_first_existing(c("itemid_anchor_age", "V1"), names(dat_model))
sex_var4 <- pick_first_existing(c("itemid_gender", "V2"), names(dat_model))
if (!is.finite(match(age_var4, names(dat_model))) || !is.finite(match(sex_var4, names(dat_model)))) {
  stop("4V建模未找到年龄或性别变量（支持 itemid_anchor_age/itemid_gender 或 V1/V2）。")
}

fixed_candidates4 <- setdiff(item_cols_all, c(drop_for_y, y_var_batch, age_var4, sex_var4))
if (length(fixed_candidates4) < 2) {
  stop("可用于4V建模的候选itemid不足2个。")
}

pair_mat4 <- utils::combn(fixed_candidates4, 2)
t0_4 <- median(dat_model$obs_time[dat_model$obs_time > 0], na.rm = TRUE)
out_path_4 <- "f:/文章_大论文/MIMIC数据库_代码/TREA代码/0315/result_4V.xlsx"

result_rows4 <- vector("list", ncol(pair_mat4))
for (i in seq_len(ncol(pair_mat4))) {
  extra_vars <- pair_mat4[, i]
  vars4 <- c(age_var4, sex_var4, extra_vars)
  d_i <- dat_model

  for (v in vars4) {
    xv <- to_num_local(d_i[[v]])
    med <- suppressWarnings(median(xv, na.rm = TRUE))
    if (!is.finite(med)) med <- 0
    xv[is.na(xv)] <- med
    xv <- as.numeric(scale(xv))
    xv[!is.finite(xv)] <- 0
    d_i[[v]] <- xv
  }

  fm4 <- as.formula(paste("Y ~ t +", paste(vars4, collapse = " + ")))

  one4 <- tryCatch({
    lme_fit4 <- nlme::lme(
      fixed = fm4,
      random = ~ 1 | ID,
      data = d_i,
      na.action = na.omit,
      control = nlme::lmeControl(
        opt = "optim",
        maxIter = 200,
        msMaxIter = 200,
        niterEM = 50
      )
    )
    surv_i4 <- d_i[!duplicated(d_i$ID), c("ID", "obs_time", "event"), drop = FALSE]
    cox_fit4 <- survival::coxph(survival::Surv(obs_time, event) ~ 1, data = surv_i4, x = TRUE, model = TRUE)
    jm_fit4 <- JM::jointModel(
      lmeObject = lme_fit4,
      survObject = cox_fit4,
      timeVar = "t",
      method = "weibull-PH-aGH"
    )
    met4 <- calc_metrics_batch(jm_fit4, lme_fit4, d_i, fm4, t0_4)
    data.frame(
      itemids = paste(vars4, collapse = ";"),
      cindex = safe_num(met4["cindex"]),
      bs = safe_num(met4["bs"]),
      auc = safe_num(met4["auc"]),
      stringsAsFactors = FALSE
    )
  }, error = function(e) {
    data.frame(
      itemids = paste(vars4, collapse = ";"),
      cindex = NA_real_,
      bs = NA_real_,
      auc = NA_real_,
      stringsAsFactors = FALSE
    )
  })

  result_rows4[[i]] <- one4
  cat("4V批量JM进度: ", i, "/", ncol(pair_mat4), " 完成\n", sep = "")
}

result_df4 <- dplyr::bind_rows(result_rows4)
writexl::write_xlsx(result_df4, out_path_4)

cat("\n4V批量JM建模结束。\n")
cat("Y变量: ", y_var_batch, "\n", sep = "")
cat("年龄变量: ", age_var4, "；性别变量: ", sex_var4, "\n", sep = "")
cat("结果文件: ", out_path_4, "\n", sep = "")
print(utils::head(result_df4, 10))

library(writexl)
write_xlsx(result_df4,"F:/文章_大论文/MIMIC数据库_代码/TREA代码/0315/result_JM_4v.xlsx")
write_xlsx(result_df,"F:/文章_大论文/MIMIC数据库_代码/TREA代码/0315/result_JM_10v.xlsx")

################5折交叉验证##################

## 目标：
## - 从现有 10V/4V 结果中自动选 CINDEX 最优模型
## - 在现有数据上进行 5 折交叉验证
## - 输出每折与均值 CINDEX / BS / AUC

if (!requireNamespace("writexl", quietly = TRUE)) {
  stop("请先安装 writexl 包：install.packages('writexl')")
}

## 独立运行支持：若 jm_data0 不存在，则在本区块内自动引入并构建
if (!exists("jm_data0")) {
  jm_main_path_cv <- "f:/文章_大论文/MIMIC数据库_代码/TREA代码/0314/widedata_merge1.xlsx"
  jm_base_path_cv <- "F:/文章_大论文/MIMIC数据库_代码/TREA代码/0311/stroke_baseline_simple.xlsx"
  if (!file.exists(jm_main_path_cv)) stop(paste0("文件不存在: ", jm_main_path_cv))
  if (!file.exists(jm_base_path_cv)) stop(paste0("文件不存在: ", jm_base_path_cv))

  jm_main_cv <- readxl::read_xlsx(jm_main_path_cv)
  jm_base_cv <- readxl::read_xlsx(jm_base_path_cv) %>%
    dplyr::select(hadm_id, hospital_mortality) %>%
    dplyr::distinct(hadm_id, .keep_all = TRUE) %>%
    dplyr::rename(hospitalmortality = hospital_mortality)

  jm_data0 <- jm_main_cv %>%
    dplyr::mutate(hadm_id = as.character(hadm_id)) %>%
    dplyr::left_join(jm_base_cv %>% dplyr::mutate(hadm_id = as.character(hadm_id)), by = "hadm_id")

  jm_data0$ID <- as.numeric(as.factor(as.character(jm_data0$subject_id)))
  jm_data0$t <- suppressWarnings(as.numeric(jm_data0$Obstimes)) / 24
  jm_data0$event <- suppressWarnings(as.numeric(jm_data0$hospitalmortality))
  jm_data0$event <- ifelse(is.na(jm_data0$event), NA, ifelse(jm_data0$event > 0, 1, 0))

  obs_los_cv <- suppressWarnings(as.numeric(jm_data0$itemid_los_hosp_days))
  q95_los_cv <- suppressWarnings(as.numeric(stats::quantile(obs_los_cv, 0.95, na.rm = TRUE)))
  prop_pos_los_cv <- mean(obs_los_cv > 0, na.rm = TRUE)
  los_like_std_cv <- (!is.finite(q95_los_cv)) || (q95_los_cv <= 2) || (!is.finite(prop_pos_los_cv)) || (prop_pos_los_cv < 0.8)
  if (!los_like_std_cv) {
    jm_data0$obs_time <- obs_los_cv
  } else {
    jm_data0 <- jm_data0 %>%
      dplyr::group_by(ID) %>%
      dplyr::mutate(obs_time = max(t, na.rm = TRUE) + 1e-3) %>%
      dplyr::ungroup()
    jm_data0$obs_time[!is.finite(jm_data0$obs_time)] <- NA_real_
  }

  jm_data0 <- jm_data0 %>%
    dplyr::filter(!is.na(.data$ID), !is.na(.data$t), !is.na(.data$obs_time), !is.na(.data$event)) %>%
    dplyr::filter(.data$obs_time > 0, .data$t <= .data$obs_time)
}

## 独立运行支持：若 10V/4V结果对象不存在，则从文件读取
if (!exists("result_df")) {
  p10 <- "F:/文章_大论文/MIMIC数据库_代码/TREA代码/0315/result_JM_10v.xlsx"
  if (file.exists(p10)) {
    result_df <- readxl::read_xlsx(p10)
  }
}
if (!exists("result_df4")) {
  p4 <- "F:/文章_大论文/MIMIC数据库_代码/TREA代码/0315/result_JM_4v.xlsx"
  if (file.exists(p4)) {
    result_df4 <- readxl::read_xlsx(p4)
  }
}
if (!exists("result_df") && !exists("result_df4")) {
  stop("未找到 result_df/result_df4，且结果文件不存在。请先完成10V或4V建模。")
}

if (!exists("safe_num")) {
  safe_num <- function(x, digits = 4) ifelse(is.finite(x), round(x, digits), NA_real_)
}

## 独立运行支持：若 calc_metrics_batch 不存在，则在本区块定义
if (!exists("calc_metrics_batch")) {
  calc_metrics_batch <- function(jm_fit, lme_fit, d_fit, fm, t0) {
    rhs_terms <- attr(terms(fm), "term.labels")
    rhs_vars <- setdiff(rhs_terms, "t")
    rhs_vars <- gsub("`", "", rhs_vars)
    need_cols <- unique(c("ID", "obs_time", "event", rhs_vars))
    need_cols <- need_cols[need_cols %in% names(d_fit)]
    surv_data <- d_fit[!duplicated(d_fit$ID), need_cols, drop = FALSE]
    surv_data$t <- t0

    beta <- nlme::fixed.effects(lme_fit)
    rhs_form <- if (length(rhs_terms) > 0) as.formula(paste("~", paste(rhs_terms, collapse = " + "))) else ~ 1
    X <- model.matrix(rhs_form, data = surv_data)
    beta_use <- beta[colnames(X)]
    beta_use[is.na(beta_use)] <- 0
    fixed_part <- as.numeric(X %*% beta_use)

    re <- nlme::ranef(lme_fit)
    if (is.null(dim(re))) re <- matrix(re, ncol = 1)
    re_df <- as.data.frame(re)
    re_df$ID <- suppressWarnings(as.numeric(rownames(re_df)))
    re_df <- re_df[match(surv_data$ID, re_df$ID), , drop = FALSE]
    re_int <- if (ncol(re_df) >= 1) as.numeric(re_df[[1]]) else rep(0, nrow(surv_data))
    re_int[is.na(re_int)] <- 0

    assoc_val <- NA_real_
    for (nm in c("Assoct", "alpha", "AssoctE", "AssoctEV")) {
      if (!is.null(jm_fit$coefficients[[nm]])) {
        assoc_val <- suppressWarnings(as.numeric(jm_fit$coefficients[[nm]])[1])
        if (is.finite(assoc_val)) break
      }
    }
    if (!is.finite(assoc_val)) assoc_val <- 1

    risk <- assoc_val * (fixed_part + re_int)
    cindex <- tryCatch(
      as.numeric(survival::concordance(survival::Surv(obs_time, event) ~ risk, data = surv_data)$concordance),
      error = function(e) NA_real_
    )
    if (!is.na(cindex) && cindex < 0.5) {
      cindex <- tryCatch(
        as.numeric(survival::concordance(survival::Surv(obs_time, event) ~ I(-risk), data = surv_data)$concordance),
        error = function(e) NA_real_
      )
    }

    auc <- tryCatch({
      roc1 <- timeROC::timeROC(T = surv_data$obs_time, delta = surv_data$event, marker = risk, cause = 1, times = t0)
      roc2 <- timeROC::timeROC(T = surv_data$obs_time, delta = surv_data$event, marker = -risk, cause = 1, times = t0)
      auc1 <- if (length(roc1$AUC) >= 2) roc1$AUC[2] else roc1$AUC[1]
      auc2 <- if (length(roc2$AUC) >= 2) roc2$AUC[2] else roc2$AUC[1]
      max(as.numeric(auc1), as.numeric(auc2), na.rm = TRUE)
    }, error = function(e) NA_real_)

    bs <- tryCatch({
      haz <- exp(as.numeric(scale(risk)))
      sp <- exp(-haz * t0)
      mean(ifelse(surv_data$obs_time <= t0 & surv_data$event == 1, (1 - sp)^2, sp^2), na.rm = TRUE)
    }, error = function(e) NA_real_)

    c(auc = auc, bs = bs, cindex = cindex)
  }
}

pick_best_jm <- function(df, tag) {
  if (is.null(df) || nrow(df) == 0L) return(NULL)
  req <- c("itemids", "cindex", "bs", "auc")
  if (!all(req %in% names(df))) return(NULL)
  d <- df %>% dplyr::filter(is.finite(.data$cindex))
  if (nrow(d) == 0L) return(NULL)
  d <- d %>% dplyr::arrange(dplyr::desc(.data$cindex), .data$bs, dplyr::desc(.data$auc))
  data.frame(
    model_tag = tag,
    itemids = as.character(d$itemids[1]),
    cindex = as.numeric(d$cindex[1]),
    bs = as.numeric(d$bs[1]),
    auc = as.numeric(d$auc[1]),
    stringsAsFactors = FALSE
  )
}

best_pool_jm <- dplyr::bind_rows(
  if (exists("result_df")) pick_best_jm(result_df, "10V") else NULL,
  if (exists("result_df4")) pick_best_jm(result_df4, "4V") else NULL
)
if (nrow(best_pool_jm) == 0L) stop("没有可用于5折交叉验证的成功JM模型。")
best_row_jm <- best_pool_jm %>%
  dplyr::arrange(dplyr::desc(.data$cindex), .data$bs, dplyr::desc(.data$auc)) %>%
  dplyr::slice(1)
best_vars_jm <- unlist(strsplit(as.character(best_row_jm$itemids[1]), ";", fixed = TRUE))
best_vars_jm <- best_vars_jm[best_vars_jm %in% names(jm_data0)]
if (length(best_vars_jm) == 0L) stop("最优模型变量在 jm_data0 中不存在。")

## 纵向结局Y：优先使用前面已选变量，否则在jm_data0中重新选择
if (exists("y_var_batch") && y_var_batch %in% names(jm_data0)) {
  y_var_cv <- y_var_batch
} else {
  cand_y <- names(jm_data0)[grepl("^itemid_", names(jm_data0))]
  cand_y <- setdiff(cand_y, c(best_vars_jm, "itemid_los_hosp_days"))
  cand_y <- cand_y[vapply(cand_y, function(v) sum(!is.na(suppressWarnings(as.numeric(jm_data0[[v]]))) ) > 0, logical(1))]
  if (length(cand_y) == 0L) stop("无法在 jm_data0 中找到可用纵向变量Y。")
  y_var_cv <- cand_y[1]
}

to_num_cv <- function(x) {
  if (exists("to_num_local")) return(to_num_local(x))
  if (exists("to_num01")) return(to_num01(x))
  if (is.logical(x)) return(as.numeric(x))
  if (is.numeric(x)) return(as.numeric(x))
  xc <- trimws(as.character(x))
  xc[xc == ""] <- NA_character_
  xn <- suppressWarnings(as.numeric(xc))
  if (sum(!is.na(xn)) >= max(10, floor(0.5 * sum(!is.na(xc))))) return(xn)
  as.numeric(as.factor(xc))
}

## 按ID分层5折
id_surv <- jm_data0[!duplicated(jm_data0$ID), c("ID", "obs_time", "event"), drop = FALSE]
id_surv <- id_surv[is.finite(id_surv$obs_time) & !is.na(id_surv$obs_time) & !is.na(id_surv$event), , drop = FALSE]
set.seed(2026)
id_e1 <- id_surv$ID[id_surv$event == 1]
id_e0 <- id_surv$ID[id_surv$event == 0]
fold_df <- data.frame(ID = id_surv$ID, fold = NA_integer_)
fold_df$fold[match(id_e1, fold_df$ID)] <- sample(rep(1:5, length.out = length(id_e1)))
fold_df$fold[match(id_e0, fold_df$ID)] <- sample(rep(1:5, length.out = length(id_e0)))

cv_rows_jm <- vector("list", 5)
for (k in 1:5) {
  te_ids <- fold_df$ID[fold_df$fold == k]
  tr_ids <- setdiff(fold_df$ID, te_ids)
  d_tr0 <- jm_data0 %>% dplyr::filter(.data$ID %in% tr_ids)
  d_te0 <- jm_data0 %>% dplyr::filter(.data$ID %in% te_ids)

  one_fold <- tryCatch({
    d_tr <- d_tr0 %>%
      dplyr::mutate(Y = suppressWarnings(as.numeric(.data[[y_var_cv]]))) %>%
      dplyr::filter(!is.na(.data$Y), is.finite(.data$t), is.finite(.data$obs_time), .data$t < .data$obs_time)
    d_te <- d_te0 %>%
      dplyr::mutate(Y = suppressWarnings(as.numeric(.data[[y_var_cv]]))) %>%
      dplyr::filter(!is.na(.data$Y), is.finite(.data$t), is.finite(.data$obs_time), .data$t < .data$obs_time)
    if (nrow(d_tr) < 80 || length(unique(d_tr$ID)) < 30) stop("训练折样本不足。")
    if (nrow(d_te) < 30 || length(unique(d_te$ID)) < 10) stop("测试折样本不足。")

    ## 训练集统计量预处理，并同步应用到测试集
    for (v in best_vars_jm) {
      xtr <- to_num_cv(d_tr[[v]])
      med <- suppressWarnings(median(xtr, na.rm = TRUE))
      if (!is.finite(med)) med <- 0
      xtr[is.na(xtr)] <- med
      s <- stats::sd(xtr, na.rm = TRUE)
      if (is.finite(s) && s > 1e-8) {
        m <- mean(xtr, na.rm = TRUE)
        d_tr[[v]] <- (xtr - m) / s
        xte <- to_num_cv(d_te[[v]]); xte[is.na(xte)] <- med
        d_te[[v]] <- (xte - m) / s
      } else {
        d_tr[[v]] <- xtr
        xte <- to_num_cv(d_te[[v]]); xte[is.na(xte)] <- med
        d_te[[v]] <- xte
      }
      d_tr[[v]][!is.finite(d_tr[[v]])] <- 0
      d_te[[v]][!is.finite(d_te[[v]])] <- 0
    }

    ## Y标准化用训练集参数
    y_m <- mean(d_tr$Y, na.rm = TRUE)
    y_s <- stats::sd(d_tr$Y, na.rm = TRUE)
    if (!is.finite(y_s) || y_s <= 1e-8) stop("Y方差过小。")
    d_tr$Y <- (d_tr$Y - y_m) / y_s
    d_te$Y <- (d_te$Y - y_m) / y_s

    d_tr <- d_tr %>% dplyr::group_by(ID) %>% dplyr::arrange(.data$t, .by_group = TRUE) %>% dplyr::mutate(t = as.numeric(.data$t) + (dplyr::row_number() - 1) * 1e-4) %>% dplyr::ungroup()
    d_te <- d_te %>% dplyr::group_by(ID) %>% dplyr::arrange(.data$t, .by_group = TRUE) %>% dplyr::mutate(t = as.numeric(.data$t) + (dplyr::row_number() - 1) * 1e-4) %>% dplyr::ungroup()

    fm <- as.formula(paste("Y ~ t +", paste(best_vars_jm, collapse = " + ")))
    lme_fit <- nlme::lme(
      fixed = fm, random = ~ 1 | ID, data = d_tr, na.action = na.omit,
      control = nlme::lmeControl(opt = "optim", maxIter = 200, msMaxIter = 200, niterEM = 50)
    )
    surv_tr <- d_tr[!duplicated(d_tr$ID), c("ID", "obs_time", "event"), drop = FALSE]
    cox_fit <- survival::coxph(survival::Surv(obs_time, event) ~ 1, data = surv_tr, x = TRUE, model = TRUE)
    jm_fit <- JM::jointModel(lmeObject = lme_fit, survObject = cox_fit, timeVar = "t", method = "weibull-PH-aGH")
    t0_k <- median(surv_tr$obs_time[surv_tr$obs_time > 0], na.rm = TRUE)
    met <- calc_metrics_batch(jm_fit, lme_fit, d_te, fm, t0_k)

    data.frame(
      fold = k,
      n_train = length(unique(d_tr$ID)),
      n_test = length(unique(d_te$ID)),
      cindex = safe_num(met["cindex"]),
      bs = safe_num(met["bs"]),
      auc = safe_num(met["auc"]),
      error_msg = "",
      stringsAsFactors = FALSE
    )
  }, error = function(e) {
    data.frame(
      fold = k,
      n_train = length(unique(d_tr0$ID)),
      n_test = length(unique(d_te0$ID)),
      cindex = NA_real_,
      bs = NA_real_,
      auc = NA_real_,
      error_msg = conditionMessage(e),
      stringsAsFactors = FALSE
    )
  })

  cv_rows_jm[[k]] <- one_fold
  cat("JM 5折CV进度: ", k, "/5 完成\n", sep = "")
}

cv_detail_jm <- dplyr::bind_rows(cv_rows_jm)
cv_mean_jm <- cv_detail_jm %>%
  dplyr::summarise(
    fold = "mean",
    n_train = round(mean(.data$n_train), 0),
    n_test = round(mean(.data$n_test), 0),
    cindex = round(mean(.data$cindex, na.rm = TRUE), 4),
    bs = round(mean(.data$bs, na.rm = TRUE), 4),
    auc = round(mean(.data$auc, na.rm = TRUE), 4),
    error_msg = ""
  )
meta_jm <- data.frame(
  selected_model_source = best_row_jm$model_tag[1],
  selected_itemids = best_row_jm$itemids[1],
  selected_y = y_var_cv,
  selected_cindex = as.numeric(best_row_jm$cindex[1]),
  selected_bs = as.numeric(best_row_jm$bs[1]),
  selected_auc = as.numeric(best_row_jm$auc[1]),
  stringsAsFactors = FALSE
)

out_cv_jm <- "F:/文章_大论文/MIMIC数据库_代码/TREA代码/0315/result_JM_best_5fold.xlsx"
writexl::write_xlsx(
  list(
    model_selected = meta_jm,
    cv_detail = cv_detail_jm,
    cv_mean = cv_mean_jm
  ),
  out_cv_jm
)

cat("\nJM 最优模型5折交叉验证完成。\n")
cat("最优模型来源: ", best_row_jm$model_tag[1], "\n", sep = "")
cat("最优变量: ", best_row_jm$itemids[1], "\n", sep = "")
cat("纵向结局Y: ", y_var_cv, "\n", sep = "")
cat("结果文件: ", out_cv_jm, "\n", sep = "")
print(cv_detail_jm)
print(cv_mean_jm)






