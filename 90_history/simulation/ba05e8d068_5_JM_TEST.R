# =============================================================================
# 5_JM_TEST.R
# 对 4_TEST_模拟数据_50样本.xlsx 进行 Joint Model 建模
# 参考 5_Jointmodel_4VC1.R
# =============================================================================

library(survival)
library(timeROC)
library(dplyr)
library(joineRML)
library(readxl)
library(writexl)
library(tidyverse)

set.seed(123)
options(scipen = 999)
options(pillar.width = Inf)


# ==================== 路径设置 ====================

# 数据文件（与 4_TEST_模拟数据.r 输出同目录）
DATA_FILE <- "F:/文章_大论文/0319大改/代码/TEST/4_TEST_模拟数据_50样本.xlsx"
OUT_DIR   <- "F:/文章_大论文/0319大改/代码/TEST"
dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)


# ==================== 读取数据 ====================

message("读取数据: ", DATA_FILE)
sim_test_50 <- read_xlsx(DATA_FILE)


# ==================== cal_3：Joint Model + AUC/C-index/Brier ====================

cal_3 <- function(data, t0 = 1) {

  clip <- function(x, lim = 20) {
    pmax(pmin(x, lim), -lim)
  }

  tryCatch({

    # 1. 数据清理：仅保留 t <= obs_time 的观测
    data_clean <- data[data$t <= data$obs_time, ]

    # 2. 拟合 joint model
    fit <- mjoint(
      formLongFixed = list(
        "Y" = Y ~ t + V1 + V2 + V3 + V4
      ),
      formLongRandom = list(
        "Y" = ~ t | ID
      ),
      formSurv = Surv(obs_time, event) ~ 1,
      data = data_clean,
      timeVar = "t"
    )

    # 3. 生存数据（每人一条）
    surv_data <- data_clean[!duplicated(data_clean$ID), ]
    surv_time <- surv_data$obs_time
    surv_status <- surv_data$event

    # 4. 提取参数
    beta <- fit$coefficients$beta
    gamma <- fit$coefficients$gamma
    random_effects <- ranef(fit)

    # 5. 风险得分（固定 + 随机）
    baseline_data <- surv_data
    baseline_data$t <- t0

    X_matrix <- model.matrix(
      ~ t + V1 + V2 + V3 + V4,
      data = baseline_data
    )

    fixed_part <- as.numeric(X_matrix %*% beta)
    fixed_part <- clip(fixed_part)

    random_part <- random_effects[, 1] + random_effects[, 2] * t0
    random_part <- clip(random_part)

    comprehensive_risk <- as.numeric(gamma) * (fixed_part + random_part)

    # 6. AUC
    roc_obj <- timeROC(
      T = surv_time,
      delta = surv_status,
      marker = comprehensive_risk,
      cause = 1,
      times = t0
    )
    AUC <- round(roc_obj$AUC[2], 4)

    # 7. C-index
    n <- length(surv_time)
    idx <- combn(n, 2)

    i <- idx[1, ]
    j <- idx[2, ]

    comparable_ij <- (surv_status[i] == 1 & surv_time[i] < surv_time[j])
    comparable_ji <- (surv_status[j] == 1 & surv_time[j] < surv_time[i])
    comparable <- comparable_ij | comparable_ji

    concordant <- (comparable_ij & (comprehensive_risk[i] > comprehensive_risk[j])) |
      (comparable_ji & (comprehensive_risk[j] > comprehensive_risk[i]))

    tied <- comparable & (comprehensive_risk[i] == comprehensive_risk[j])

    n_pairs <- sum(comparable)
    n_concordant <- sum(concordant)
    n_tied <- sum(tied)

    Cindex <- ifelse(
      n_pairs > 0,
      (n_concordant + 0.5 * n_tied) / n_pairs,
      NA
    )

    # 8. Brier Score
    lp_long_full <- model.matrix(
      ~ t + V1 + V2 + V3 + V4,
      data = data_clean
    ) %*% beta

    lp_long_full <- clip(as.numeric(lp_long_full))

    S0_t0 <- summary(
      survfit(coxph(Surv(obs_time, event) ~ 1, data = data_clean)),
      times = t0
    )$surv

    S_pred <- S0_t0 ^ exp(as.numeric(gamma) * lp_long_full)

    censoring_model <- survfit(
      Surv(obs_time, 1 - event) ~ 1,
      data = data_clean
    )

    get_weights <- function(time, event, censoring_model, t0) {
      cens_probs <- summary(
        censoring_model,
        times = pmin(time, t0)
      )$surv

      weights <- ifelse(
        time <= t0 & event == 1,
        1 / cens_probs,
        ifelse(
          time > t0,
          1 / summary(censoring_model, times = t0)$surv,
          0
        )
      )
      weights
    }

    weights <- get_weights(
      data_clean$obs_time,
      data_clean$event,
      censoring_model,
      t0
    )

    Y_obs <- as.numeric(
      data_clean$obs_time > t0 |
        (data_clean$obs_time <= t0 & data_clean$event == 0)
    )

    BS <- mean(weights * (S_pred - Y_obs)^2, na.rm = TRUE)

    # 9. 返回结果
    data.frame(
      AUC = AUC,
      BS = BS,
      Cindex = Cindex
    )

  }, error = function(e) {

    message("⚠️ cal_3 failed: ", e$message)

    data.frame(
      AUC = NA,
      BS = NA,
      Cindex = NA
    )
  })
}


# ==================== 多时间点计算 ====================

cal_3_multi <- function(data, t0_vec = c(0.5, 1.3, 2)) {

  clip <- function(x, lim = 20) pmax(pmin(x, lim), -lim)

  tryCatch({

    # 1. 数据清理
    data_clean <- data[data$t <= data$obs_time, ]

    # 2. 拟合 joint model（只拟合一次）
    fit <- mjoint(
      formLongFixed = list("Y" = Y ~ t + V1 + V2 + V3 + V4),
      formLongRandom = list("Y" = ~ t | ID),
      formSurv = Surv(obs_time, event) ~ 1,
      data = data_clean,
      timeVar = "t"
    )

    surv_data <- data_clean[!duplicated(data_clean$ID), ]
    surv_time <- surv_data$obs_time
    surv_status <- surv_data$event
    beta <- fit$coefficients$beta
    gamma <- fit$coefficients$gamma
    random_effects <- ranef(fit)

    censoring_model <- survfit(Surv(obs_time, 1 - event) ~ 1, data = data_clean)
    lp_long_full <- model.matrix(~ t + V1 + V2 + V3 + V4, data = data_clean) %*% beta
    lp_long_full <- clip(as.numeric(lp_long_full))

    get_weights <- function(time, event, censoring_model, t0) {
      cens_probs <- summary(censoring_model, times = pmin(time, t0))$surv
      ifelse(
        time <= t0 & event == 1, 1 / cens_probs,
        ifelse(time > t0, 1 / summary(censoring_model, times = t0)$surv, 0)
      )
    }

    res_list <- lapply(t0_vec, function(t0) {

      baseline_data <- surv_data
      baseline_data$t <- t0
      X_matrix <- model.matrix(~ t + V1 + V2 + V3 + V4, data = baseline_data)
      fixed_part <- clip(as.numeric(X_matrix %*% beta))
      random_part <- clip(random_effects[, 1] + random_effects[, 2] * t0)
      comprehensive_risk <- as.numeric(gamma) * (fixed_part + random_part)

      # AUC
      roc_obj <- timeROC(T = surv_time, delta = surv_status, marker = comprehensive_risk,
                         cause = 1, times = t0)
      AUC <- round(roc_obj$AUC[2], 4)

      # C-index
      n <- length(surv_time)
      idx <- combn(n, 2)
      i <- idx[1, ]; j <- idx[2, ]
      comparable_ij <- (surv_status[i] == 1 & surv_time[i] < surv_time[j])
      comparable_ji <- (surv_status[j] == 1 & surv_time[j] < surv_time[i])
      comparable <- comparable_ij | comparable_ji
      concordant <- (comparable_ij & (comprehensive_risk[i] > comprehensive_risk[j])) |
        (comparable_ji & (comprehensive_risk[j] > comprehensive_risk[i]))
      tied <- comparable & (comprehensive_risk[i] == comprehensive_risk[j])
      n_pairs <- sum(comparable)
      Cindex <- ifelse(n_pairs > 0, (sum(concordant) + 0.5 * sum(tied)) / n_pairs, NA)

      # Brier Score
      S0_t0 <- summary(survfit(coxph(Surv(obs_time, event) ~ 1, data = data_clean)), times = t0)$surv
      S_pred <- S0_t0 ^ exp(as.numeric(gamma) * lp_long_full)
      weights <- get_weights(data_clean$obs_time, data_clean$event, censoring_model, t0)
      Y_obs <- as.numeric(data_clean$obs_time > t0 | (data_clean$obs_time <= t0 & data_clean$event == 0))
      BS <- mean(weights * (S_pred - Y_obs)^2, na.rm = TRUE)

      data.frame(t0 = t0, AUC = AUC, BS = BS, Cindex = Cindex)
    })

    do.call(rbind, res_list)

  }, error = function(e) {
    message("⚠️ cal_3_multi failed: ", e$message)
    data.frame(t0 = t0_vec, AUC = NA, BS = NA, Cindex = NA)
  })
}


# ==================== 建模并保存 ====================

t0_vec <- c(0.5, 1.3, 2)

message("【Joint Model】对 50 样本数据建模，计算 t0 = ", paste(t0_vec, collapse = ", "), " 的 AUC/BS/Cindex...")
result_test <- cal_3_multi(sim_test_50, t0_vec = t0_vec)

# 保存结果到 TEST 目录
out_xlsx <- file.path(OUT_DIR, "5_JM_TEST_结果_50样本.xlsx")
write_xlsx(result_test, path = out_xlsx)

message("✅ 建模完成！结果已保存: ", out_xlsx)
print(result_test)
