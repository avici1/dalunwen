# =============================================================================
# 5_RSF_TEST.R
# 对 4_TEST_模拟数据_50样本.xlsx 进行 RSF 建模
# 参考 5_RSF模型_4V1C.R，结果保存格式参考 5_JM_TEST.R
# =============================================================================

library(survival)
library(rms)
library(timeROC)
library(dplyr)
library(randomForestSRC)
library(readxl)
library(writexl)
library(tidyverse)

set.seed(123)
options(scipen = 999)
options(pillar.width = Inf)


# ==================== 路径设置 ====================

OUT_DIR <- "F:/文章_大论文/0319大改/代码/TEST"
possible_data <- c(
  file.path(OUT_DIR, "4_TEST_模拟数据_50样本.xlsx"),
  file.path(dirname(OUT_DIR), "4_TEST_模拟数据_50样本.xlsx"),
  "F:/文章_大论文/0319大改/代码/4_TEST_模拟数据_50样本.xlsx",
  file.path(getwd(), "4_TEST_模拟数据_50样本.xlsx"),
  file.path(getwd(), "..", "4_TEST_模拟数据_50样本.xlsx")
)
DATA_FILE <- possible_data[file.exists(possible_data)][1]

if (is.na(DATA_FILE) || !file.exists(DATA_FILE)) {
  stop("未找到数据文件 4_TEST_模拟数据_50样本.xlsx\n",
       "请先运行 4_TEST_模拟数据.r 生成数据，或将该 xlsx 放在 TEST 目录下")
}

dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)


# ==================== 读取数据 ====================

message("读取数据: ", DATA_FILE)
sim_test_50 <- read_xlsx(DATA_FILE)


# ==================== cal_3_RSF：RSF + AUC/C-index/Brier ====================

cal_3_RSF <- function(data, t0 = 1) {

  tryCatch({

    # 1. 数据准备（每患者一条生存数据）
    surv_data <- data[!duplicated(data$ID), ]
    surv_time <- surv_data$obs_time
    surv_status <- surv_data$event

    # 只保留生存相关的变量（V1-V4, obs_time, event）
    v_cols <- grep("^V[0-9]", names(surv_data), value = TRUE)
    rsf_data <- surv_data[, c(v_cols, "obs_time", "event")]
    rsf_data <- as.data.frame(rsf_data)

    message("  rsf_data: ", nrow(rsf_data), " 人, event数=", sum(surv_status))

    # 2. 建立 RSF 模型（50 样本时 nodesize 调小）
    ntree_use <- 500
    mtry_use <- max(1, floor(length(v_cols) / 3))
    nodesize_use <- max(3, min(10, floor(nrow(rsf_data) / 5)))

    rfsrc_fit <- rfsrc(
      Surv(obs_time, event) ~ .,
      data = rsf_data,
      ntree      = ntree_use,
      mtry       = mtry_use,
      nodesize   = nodesize_use,
      importance = TRUE,
      proximity  = FALSE,
      seed       = 123
    )

    # 3. 预测生存概率
    pred <- predict(rfsrc_fit, newdata = rsf_data)
    time_points <- pred$time.interest
    t0_idx <- which.min(abs(time_points - t0))
    survival_probs <- pred$survival[, t0_idx]

    # 4. 计算 AUC（风险标记 = 1 - 生存概率）
    risk_marker <- 1 - survival_probs

    roc_obj <- timeROC(
      T = surv_time,
      delta = surv_status,
      marker = risk_marker,
      cause = 1,
      times = t0
    )
    AUC <- round(roc_obj$AUC[2], 4)

    # 5. 计算 C-index
    n <- length(surv_time)
    idx <- combn(n, 2)
    i <- idx[1, ]
    j <- idx[2, ]

    comparable_ij <- (surv_status[i] == 1 & surv_time[i] < surv_time[j])
    comparable_ji <- (surv_status[j] == 1 & surv_time[j] < surv_time[i])
    comparable <- comparable_ij | comparable_ji

    concordant <- (comparable_ij & (risk_marker[i] > risk_marker[j])) |
      (comparable_ji & (risk_marker[j] > risk_marker[i]))
    tied <- comparable & (risk_marker[i] == risk_marker[j])

    n_pairs <- sum(comparable)
    n_concordant <- sum(concordant)
    n_tied <- sum(tied)
    Cindex <- ifelse(n_pairs > 0, (n_concordant + 0.5 * n_tied) / n_pairs, NA)

    # 6. 计算 Brier Score
    Y_obs <- as.numeric(surv_time > t0 | (surv_time <= t0 & surv_status == 0))
    censoring_model <- survfit(Surv(obs_time, 1 - event) ~ 1, data = rsf_data)

    get_weights <- function(time, event, censoring_model, t0) {
      cens_probs <- summary(censoring_model, times = pmin(time, t0))$surv
      weights <- ifelse(
        time <= t0 & event == 1, 1 / cens_probs,
        ifelse(time > t0, 1 / summary(censoring_model, times = t0)$surv, 0)
      )
      weights
    }

    weights <- get_weights(surv_time, surv_status, censoring_model, t0)
    BS <- mean(weights * (survival_probs - Y_obs)^2, na.rm = TRUE)

    # 7. 返回结果
    data.frame(
      AUC = AUC,
      BS = BS,
      Cindex = Cindex
    )

  }, error = function(e) {

    message("⚠️ cal_3_RSF failed: ", e$message)
    data.frame(
      AUC = NA,
      BS = NA,
      Cindex = NA
    )
  })
}


# ==================== 多时间点计算 ====================

cal_3_RSF_multi <- function(data, t0_vec = c(0.5, 1.3, 2)) {

  tryCatch({

    # 1. 数据准备
    surv_data <- data[!duplicated(data$ID), ]
    surv_time <- unlist(surv_data$obs_time)
    surv_status <- unlist(surv_data$event)
    surv_time <- as.numeric(surv_time)
    surv_status <- as.numeric(surv_status)

    v_cols <- grep("^V[0-9]", names(surv_data), value = TRUE)
    rsf_data <- as.data.frame(surv_data[, c(v_cols, "obs_time", "event")])
    rsf_data$obs_time <- as.numeric(unlist(rsf_data$obs_time))
    rsf_data$event <- as.numeric(unlist(rsf_data$event))

    message("  rsf_data: ", nrow(rsf_data), " 人, event数=", sum(surv_status))

    # 2. 建立 RSF 模型（只拟合一次）
    ntree_use <- 500
    mtry_use <- max(1, floor(length(v_cols) / 3))
    nodesize_use <- max(3, min(10, floor(nrow(rsf_data) / 5)))

    rfsrc_fit <- rfsrc(
      Surv(obs_time, event) ~ .,
      data = rsf_data,
      ntree = ntree_use, mtry = mtry_use, nodesize = nodesize_use,
      importance = TRUE, proximity = FALSE, seed = 123
    )

    pred <- predict(rfsrc_fit, newdata = rsf_data)
    time_points <- pred$time.interest
    censoring_model <- survfit(Surv(obs_time, 1 - event) ~ 1, data = rsf_data)

    get_weights <- function(time, event, censoring_model, tau) {
      cens_probs <- summary(censoring_model, times = pmin(time, tau))$surv
      ifelse(
        time <= tau & event == 1, 1 / cens_probs,
        ifelse(time > tau, 1 / summary(censoring_model, times = tau)$surv, 0)
      )
    }

    res_list <- lapply(t0_vec, function(a) {

      a_idx <- which.min(abs(time_points - a))
      survival_probs <- pred$survival[, a_idx]
      risk_marker <- 1 - survival_probs

      # AUC
      roc_obj <- timeROC(T = surv_time, delta = surv_status, marker = risk_marker,
                         cause = 1, times = a)
      AUC <- round(roc_obj$AUC[2], 4)

      # C-index
      n <- length(surv_time)
      idx <- combn(n, 2)
      i <- idx[1, ]; j <- idx[2, ]
      comparable_ij <- (surv_status[i] == 1 & surv_time[i] < surv_time[j])
      comparable_ji <- (surv_status[j] == 1 & surv_time[j] < surv_time[i])
      comparable <- comparable_ij | comparable_ji
      concordant <- (comparable_ij & (risk_marker[i] > risk_marker[j])) |
        (comparable_ji & (risk_marker[j] > risk_marker[i]))
      tied <- comparable & (risk_marker[i] == risk_marker[j])
      n_pairs <- sum(comparable)
      Cindex <- ifelse(n_pairs > 0, (sum(concordant) + 0.5 * sum(tied)) / n_pairs, NA)

      # Brier Score
      Y_obs <- as.numeric(surv_time > a | (surv_time <= a & surv_status == 0))
      weights <- get_weights(surv_time, surv_status, censoring_model, tau = a)
      BS <- mean(weights * (survival_probs - Y_obs)^2, na.rm = TRUE)

      data.frame(t0 = a, AUC = AUC, BS = BS, Cindex = Cindex)
    })

    do.call(rbind, res_list)

  }, error = function(e) {
    message("⚠️ cal_3_RSF_multi failed: ", e$message)
    data.frame(t0 = t0_vec, AUC = NA, BS = NA, Cindex = NA)
  })
}


# ==================== 建模并保存 ====================

t0_vec <- c(0.5, 1.3, 2)

message("【RSF】对 50 样本数据建模，计算 t0 = ", paste(t0_vec, collapse = ", "), " 的 AUC/BS/Cindex...")
result_test <- cal_3_RSF_multi(sim_test_50, t0_vec = t0_vec)

# 保存结果到 TEST 目录
out_xlsx <- file.path(OUT_DIR, "5_RSF_TEST_结果_50样本.xlsx")
write_xlsx(result_test, path = out_xlsx)

message("✅ RSF 建模完成！结果已保存: ", out_xlsx)
print(result_test)
