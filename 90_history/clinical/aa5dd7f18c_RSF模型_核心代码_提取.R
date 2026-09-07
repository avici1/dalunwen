###############################################################
# RSF模型 (Random Survival Forest) 核心代码
# 提取自: 5_RSF模型_10VC1.R
# 功能: 建立RSF模型, 计算 C-index, BS(Brier Score), AUC
###############################################################

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

###############################################################
# 核心函数: cal_3_RSF
# 输入: data (含 ID, V1~V6, obs_time, event 等列), t0 (预测时间点)
# 输出: data.frame(AUC, BS, Cindex)
###############################################################
cal_3_RSF <- function(data, t0 = 1) {

  # 1. 数据准备（每患者一条生存数据）
  surv_data <- data[!duplicated(data$ID), ]
  surv_time <- surv_data$obs_time
  surv_status <- surv_data$event

  rsf_data <- surv_data[, c(grep("^V[0-9]", names(surv_data), value = TRUE),
                            "obs_time", "event")]

  # 2. 建立RSF模型
  rfsrc_fit <- rfsrc(
    Surv(obs_time, event) ~ .,
    data       = rsf_data,
    ntree      = 1000,
    mtry       = floor(length(grep("^V[0-9]", names(rsf_data), value = TRUE)) / 3),
    nodesize   = 10,
    importance = TRUE,
    proximity  = FALSE,
    seed       = 123
  )

  # 3. 预测生存概率
  pred <- predict(rfsrc_fit, newdata = rsf_data)
  time_points <- pred$time.interest
  t0_idx <- which.min(abs(time_points - t0))
  survival_probs <- pred$survival[, t0_idx]

  # 4. 计算AUC
  risk_marker <- 1 - survival_probs

  roc_obj <- timeROC(
    T     = surv_time,
    delta = surv_status,
    marker = risk_marker,
    cause  = 1,
    times  = t0
  )
  AUC <- round(roc_obj$AUC[2], 4)

  # 5. 计算C-index
  n <- length(surv_time)
  idx <- combn(n, 2)
  i <- idx[1, ]
  j <- idx[2, ]

  comparable_ij <- (surv_status[i] == 1 & surv_time[i] < surv_time[j])
  comparable_ji <- (surv_status[j] == 1 & surv_time[j] < surv_time[i])
  comparable <- comparable_ij | comparable_ji

  concordant <- (comparable_ij & (risk_marker[i] > risk_marker[j])) |
    (comparable_ji & (risk_marker[j] > risk_marker[i]))
  tied <- (comparable & (risk_marker[i] == risk_marker[j]))

  n_pairs      <- sum(comparable)
  n_concordant <- sum(concordant)
  n_tied       <- sum(tied)
  Cindex <- ifelse(n_pairs > 0, (n_concordant + 0.5 * n_tied) / n_pairs, NA)

  # 6. 计算BS（Brier Score）
  Y_obs <- as.numeric(surv_time > t0 | (surv_time <= t0 & surv_status == 0))
  censoring_model <- survfit(Surv(obs_time, 1 - event) ~ 1, data = rsf_data)

  get_weights <- function(time, event, censoring_model, t0) {
    cens_probs <- summary(censoring_model, times = pmin(time, t0))$surv
    weights <- ifelse(time <= t0 & event == 1, 1/cens_probs,
                      ifelse(time > t0, 1/summary(censoring_model, times = t0)$surv, 0))
    return(weights)
  }
  weights <- get_weights(surv_time, surv_status, censoring_model, t0)
  BS <- mean(weights * (survival_probs - Y_obs)^2, na.rm = TRUE)

  # 7. 返回结果
  result <- data.frame(
    AUC    = AUC,
    BS     = BS,
    Cindex = Cindex
  )
  return(result)
}

###############################################################
# 循环函数: circle_cal_3
# 对 data_list 中每个数据集分别建RSF模型并计算三指标
###############################################################
circle_cal_3 <- function(data_list, t0 = 1, verbose = TRUE) {
  if (!is.list(data_list) || length(data_list) == 0)
    stop("data_list 必须是一个非空 list")

  res <- lapply(seq_along(data_list), function(i) {
    if (verbose) message(sprintf("【%s】Processing sim%g ...", Sys.time(), i))
    tmp <- cal_3_RSF(data_list[[i]], t0 = t0)
    cbind(sim = paste0("sim", i), tmp)
  })
  do.call(rbind, res)
}

###############################################################
# 使用示例 (t0=1):
#   result <- circle_cal_3(your_data_list, t0 = 1)
#   # result 是 data.frame, 列: sim, AUC, BS, Cindex
#
# 原始代码中对以下场景分别运行:
#   - 低相关性: lowINTER / lowBTW
#   - 中相关性: midINTER / midBTW
#   - 高相关性: highINTER / highBTW
#   - 样本量: 500 / 1000
#   - 删失率: 30% / 70%
#   - 纵向测量次数: L1~L5
#
# 结果保存路径: F:/文章/大论文/程序Trae/数据_result_C1_RSF/
###############################################################
