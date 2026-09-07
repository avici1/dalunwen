# =============================================================================
# 5_RSF_10V1C_测试集.R
# RSF 模型在 10V1C 测试集上的评估
# 数据来源：模拟数据_10V1C（测试集）
# 结果保存：result_RSF*.xlsx
# =============================================================================

library(survival)
library(timeROC)
library(dplyr)
library(randomForestSRC)
library(readxl)
library(writexl)
library(tidyverse)
library(purrr)
set.seed(123)
options(scipen = 999)
options(pillar.width = Inf)

# 测试集数据路径（模拟数据_10V1C 的 生成Y 目录）
input_dir <- "F:/文章_大论文/0319大改/测试集/模拟数据_10V1C/生成Y"
# 结果保存路径
output_dir <- "F:/文章_大论文/0319大改/结果"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# 辅助函数：读取 xlsx 多 sheet 为 list
read_xlsx_list <- function(fname) {
  path <- file.path(input_dir, fname)
  if (!file.exists(path)) {
    warning("文件不存在: ", path)
    return(NULL)
  }
  sheets <- excel_sheets(path)
  setNames(map(sheets, ~ read_xlsx(path, sheet = .x)), sheets)
}

###### 读取测试集数据（10V1C：INTER + BTW）######
message("【读取测试集】", input_dir)

sim500_30_10V_lowINTER_1c_L1   <- read_xlsx_list("sim500_30_10V_lowINTER_1c_L1.xlsx")
sim500_30_10V_lowINTER_1c_L2   <- read_xlsx_list("sim500_30_10V_lowINTER_1c_L2.xlsx")
sim500_30_10V_lowINTER_1c_L3   <- read_xlsx_list("sim500_30_10V_lowINTER_1c_L3.xlsx")
sim500_30_10V_lowINTER_1c_L4   <- read_xlsx_list("sim500_30_10V_lowINTER_1c_L4.xlsx")
sim500_30_10V_lowINTER_1c_L5   <- read_xlsx_list("sim500_30_10V_lowINTER_1c_L5.xlsx")
sim500_70_10V_lowINTER_1c_L1   <- read_xlsx_list("sim500_70_10V_lowINTER_1c_L1.xlsx")
sim500_70_10V_lowINTER_1c_L2   <- read_xlsx_list("sim500_70_10V_lowINTER_1c_L2.xlsx")
sim500_70_10V_lowINTER_1c_L3   <- read_xlsx_list("sim500_70_10V_lowINTER_1c_L3.xlsx")
sim500_70_10V_lowINTER_1c_L4   <- read_xlsx_list("sim500_70_10V_lowINTER_1c_L4.xlsx")
sim500_70_10V_lowINTER_1c_L5   <- read_xlsx_list("sim500_70_10V_lowINTER_1c_L5.xlsx")
sim1000_30_10V_lowINTER_1c_L1  <- read_xlsx_list("sim1000_30_10V_lowINTER_1c_L1.xlsx")
sim1000_30_10V_lowINTER_1c_L2  <- read_xlsx_list("sim1000_30_10V_lowINTER_1c_L2.xlsx")
sim1000_30_10V_lowINTER_1c_L3  <- read_xlsx_list("sim1000_30_10V_lowINTER_1c_L3.xlsx")
sim1000_30_10V_lowINTER_1c_L4  <- read_xlsx_list("sim1000_30_10V_lowINTER_1c_L4.xlsx")
sim1000_30_10V_lowINTER_1c_L5  <- read_xlsx_list("sim1000_30_10V_lowINTER_1c_L5.xlsx")
sim1000_70_10V_lowINTER_1c_L1  <- read_xlsx_list("sim1000_70_10V_lowINTER_1c_L1.xlsx")
sim1000_70_10V_lowINTER_1c_L2  <- read_xlsx_list("sim1000_70_10V_lowINTER_1c_L2.xlsx")
sim1000_70_10V_lowINTER_1c_L3  <- read_xlsx_list("sim1000_70_10V_lowINTER_1c_L3.xlsx")
sim1000_70_10V_lowINTER_1c_L4  <- read_xlsx_list("sim1000_70_10V_lowINTER_1c_L4.xlsx")
sim1000_70_10V_lowINTER_1c_L5  <- read_xlsx_list("sim1000_70_10V_lowINTER_1c_L5.xlsx")

sim500_30_10V_midINTER_1c_L1   <- read_xlsx_list("sim500_30_10V_midINTER_1c_L1.xlsx")
sim500_30_10V_midINTER_1c_L2   <- read_xlsx_list("sim500_30_10V_midINTER_1c_L2.xlsx")
sim500_30_10V_midINTER_1c_L3   <- read_xlsx_list("sim500_30_10V_midINTER_1c_L3.xlsx")
sim500_30_10V_midINTER_1c_L4   <- read_xlsx_list("sim500_30_10V_midINTER_1c_L4.xlsx")
sim500_30_10V_midINTER_1c_L5   <- read_xlsx_list("sim500_30_10V_midINTER_1c_L5.xlsx")
sim500_70_10V_midINTER_1c_L1   <- read_xlsx_list("sim500_70_10V_midINTER_1c_L1.xlsx")
sim500_70_10V_midINTER_1c_L2   <- read_xlsx_list("sim500_70_10V_midINTER_1c_L2.xlsx")
sim500_70_10V_midINTER_1c_L3   <- read_xlsx_list("sim500_70_10V_midINTER_1c_L3.xlsx")
sim500_70_10V_midINTER_1c_L4   <- read_xlsx_list("sim500_70_10V_midINTER_1c_L4.xlsx")
sim500_70_10V_midINTER_1c_L5   <- read_xlsx_list("sim500_70_10V_midINTER_1c_L5.xlsx")
sim1000_30_10V_midINTER_1c_L1  <- read_xlsx_list("sim1000_30_10V_midINTER_1c_L1.xlsx")
sim1000_30_10V_midINTER_1c_L2  <- read_xlsx_list("sim1000_30_10V_midINTER_1c_L2.xlsx")
sim1000_30_10V_midINTER_1c_L3  <- read_xlsx_list("sim1000_30_10V_midINTER_1c_L3.xlsx")
sim1000_30_10V_midINTER_1c_L4  <- read_xlsx_list("sim1000_30_10V_midINTER_1c_L4.xlsx")
sim1000_30_10V_midINTER_1c_L5  <- read_xlsx_list("sim1000_30_10V_midINTER_1c_L5.xlsx")
sim1000_70_10V_midINTER_1c_L1  <- read_xlsx_list("sim1000_70_10V_midINTER_1c_L1.xlsx")
sim1000_70_10V_midINTER_1c_L2  <- read_xlsx_list("sim1000_70_10V_midINTER_1c_L2.xlsx")
sim1000_70_10V_midINTER_1c_L3  <- read_xlsx_list("sim1000_70_10V_midINTER_1c_L3.xlsx")
sim1000_70_10V_midINTER_1c_L4  <- read_xlsx_list("sim1000_70_10V_midINTER_1c_L4.xlsx")
sim1000_70_10V_midINTER_1c_L5  <- read_xlsx_list("sim1000_70_10V_midINTER_1c_L5.xlsx")

sim500_30_10V_highINTER_1c_L1  <- read_xlsx_list("sim500_30_10V_highINTER_1c_L1.xlsx")
sim500_30_10V_highINTER_1c_L2  <- read_xlsx_list("sim500_30_10V_highINTER_1c_L2.xlsx")
sim500_30_10V_highINTER_1c_L3  <- read_xlsx_list("sim500_30_10V_highINTER_1c_L3.xlsx")
sim500_30_10V_highINTER_1c_L4  <- read_xlsx_list("sim500_30_10V_highINTER_1c_L4.xlsx")
sim500_30_10V_highINTER_1c_L5  <- read_xlsx_list("sim500_30_10V_highINTER_1c_L5.xlsx")
sim500_70_10V_highINTER_1c_L1  <- read_xlsx_list("sim500_70_10V_highINTER_1c_L1.xlsx")
sim500_70_10V_highINTER_1c_L2  <- read_xlsx_list("sim500_70_10V_highINTER_1c_L2.xlsx")
sim500_70_10V_highINTER_1c_L3  <- read_xlsx_list("sim500_70_10V_highINTER_1c_L3.xlsx")
sim500_70_10V_highINTER_1c_L4  <- read_xlsx_list("sim500_70_10V_highINTER_1c_L4.xlsx")
sim500_70_10V_highINTER_1c_L5  <- read_xlsx_list("sim500_70_10V_highINTER_1c_L5.xlsx")
sim1000_30_10V_highINTER_1c_L1 <- read_xlsx_list("sim1000_30_10V_highINTER_1c_L1.xlsx")
sim1000_30_10V_highINTER_1c_L2 <- read_xlsx_list("sim1000_30_10V_highINTER_1c_L2.xlsx")
sim1000_30_10V_highINTER_1c_L3 <- read_xlsx_list("sim1000_30_10V_highINTER_1c_L3.xlsx")
sim1000_30_10V_highINTER_1c_L4 <- read_xlsx_list("sim1000_30_10V_highINTER_1c_L4.xlsx")
sim1000_30_10V_highINTER_1c_L5 <- read_xlsx_list("sim1000_30_10V_highINTER_1c_L5.xlsx")
sim1000_70_10V_highINTER_1c_L1 <- read_xlsx_list("sim1000_70_10V_highINTER_1c_L1.xlsx")
sim1000_70_10V_highINTER_1c_L2 <- read_xlsx_list("sim1000_70_10V_highINTER_1c_L2.xlsx")
sim1000_70_10V_highINTER_1c_L3 <- read_xlsx_list("sim1000_70_10V_highINTER_1c_L3.xlsx")
sim1000_70_10V_highINTER_1c_L4 <- read_xlsx_list("sim1000_70_10V_highINTER_1c_L4.xlsx")
sim1000_70_10V_highINTER_1c_L5 <- read_xlsx_list("sim1000_70_10V_highINTER_1c_L5.xlsx")

# BTW 数据
sim500_30_10V_lowBTW_1c_L1   <- read_xlsx_list("sim500_30_10V_lowBTW_1c_L1.xlsx")
sim500_30_10V_lowBTW_1c_L2   <- read_xlsx_list("sim500_30_10V_lowBTW_1c_L2.xlsx")
sim500_30_10V_lowBTW_1c_L3   <- read_xlsx_list("sim500_30_10V_lowBTW_1c_L3.xlsx")
sim500_30_10V_lowBTW_1c_L4   <- read_xlsx_list("sim500_30_10V_lowBTW_1c_L4.xlsx")
sim500_30_10V_lowBTW_1c_L5   <- read_xlsx_list("sim500_30_10V_lowBTW_1c_L5.xlsx")
sim500_70_10V_lowBTW_1c_L1   <- read_xlsx_list("sim500_70_10V_lowBTW_1c_L1.xlsx")
sim500_70_10V_lowBTW_1c_L2   <- read_xlsx_list("sim500_70_10V_lowBTW_1c_L2.xlsx")
sim500_70_10V_lowBTW_1c_L3   <- read_xlsx_list("sim500_70_10V_lowBTW_1c_L3.xlsx")
sim500_70_10V_lowBTW_1c_L4   <- read_xlsx_list("sim500_70_10V_lowBTW_1c_L4.xlsx")
sim500_70_10V_lowBTW_1c_L5   <- read_xlsx_list("sim500_70_10V_lowBTW_1c_L5.xlsx")
sim1000_30_10V_lowBTW_1c_L1  <- read_xlsx_list("sim1000_30_10V_lowBTW_1c_L1.xlsx")
sim1000_30_10V_lowBTW_1c_L2  <- read_xlsx_list("sim1000_30_10V_lowBTW_1c_L2.xlsx")
sim1000_30_10V_lowBTW_1c_L3  <- read_xlsx_list("sim1000_30_10V_lowBTW_1c_L3.xlsx")
sim1000_30_10V_lowBTW_1c_L4  <- read_xlsx_list("sim1000_30_10V_lowBTW_1c_L4.xlsx")
sim1000_30_10V_lowBTW_1c_L5  <- read_xlsx_list("sim1000_30_10V_lowBTW_1c_L5.xlsx")
sim1000_70_10V_lowBTW_1c_L1  <- read_xlsx_list("sim1000_70_10V_lowBTW_1c_L1.xlsx")
sim1000_70_10V_lowBTW_1c_L2  <- read_xlsx_list("sim1000_70_10V_lowBTW_1c_L2.xlsx")
sim1000_70_10V_lowBTW_1c_L3  <- read_xlsx_list("sim1000_70_10V_lowBTW_1c_L3.xlsx")
sim1000_70_10V_lowBTW_1c_L4  <- read_xlsx_list("sim1000_70_10V_lowBTW_1c_L4.xlsx")
sim1000_70_10V_lowBTW_1c_L5  <- read_xlsx_list("sim1000_70_10V_lowBTW_1c_L5.xlsx")

sim500_30_10V_midBTW_1c_L1   <- read_xlsx_list("sim500_30_10V_midBTW_1c_L1.xlsx")
sim500_30_10V_midBTW_1c_L2   <- read_xlsx_list("sim500_30_10V_midBTW_1c_L2.xlsx")
sim500_30_10V_midBTW_1c_L3   <- read_xlsx_list("sim500_30_10V_midBTW_1c_L3.xlsx")
sim500_30_10V_midBTW_1c_L4   <- read_xlsx_list("sim500_30_10V_midBTW_1c_L4.xlsx")
sim500_30_10V_midBTW_1c_L5   <- read_xlsx_list("sim500_30_10V_midBTW_1c_L5.xlsx")
sim500_70_10V_midBTW_1c_L1   <- read_xlsx_list("sim500_70_10V_midBTW_1c_L1.xlsx")
sim500_70_10V_midBTW_1c_L2   <- read_xlsx_list("sim500_70_10V_midBTW_1c_L2.xlsx")
sim500_70_10V_midBTW_1c_L3   <- read_xlsx_list("sim500_70_10V_midBTW_1c_L3.xlsx")
sim500_70_10V_midBTW_1c_L4   <- read_xlsx_list("sim500_70_10V_midBTW_1c_L4.xlsx")
sim500_70_10V_midBTW_1c_L5   <- read_xlsx_list("sim500_70_10V_midBTW_1c_L5.xlsx")
sim1000_30_10V_midBTW_1c_L1  <- read_xlsx_list("sim1000_30_10V_midBTW_1c_L1.xlsx")
sim1000_30_10V_midBTW_1c_L2  <- read_xlsx_list("sim1000_30_10V_midBTW_1c_L2.xlsx")
sim1000_30_10V_midBTW_1c_L3  <- read_xlsx_list("sim1000_30_10V_midBTW_1c_L3.xlsx")
sim1000_30_10V_midBTW_1c_L4  <- read_xlsx_list("sim1000_30_10V_midBTW_1c_L4.xlsx")
sim1000_30_10V_midBTW_1c_L5  <- read_xlsx_list("sim1000_30_10V_midBTW_1c_L5.xlsx")
sim1000_70_10V_midBTW_1c_L1  <- read_xlsx_list("sim1000_70_10V_midBTW_1c_L1.xlsx")
sim1000_70_10V_midBTW_1c_L2  <- read_xlsx_list("sim1000_70_10V_midBTW_1c_L2.xlsx")
sim1000_70_10V_midBTW_1c_L3  <- read_xlsx_list("sim1000_70_10V_midBTW_1c_L3.xlsx")
sim1000_70_10V_midBTW_1c_L4  <- read_xlsx_list("sim1000_70_10V_midBTW_1c_L4.xlsx")
sim1000_70_10V_midBTW_1c_L5  <- read_xlsx_list("sim1000_70_10V_midBTW_1c_L5.xlsx")

sim500_30_10V_highBTW_1c_L1  <- read_xlsx_list("sim500_30_10V_highBTW_1c_L1.xlsx")
sim500_30_10V_highBTW_1c_L2  <- read_xlsx_list("sim500_30_10V_highBTW_1c_L2.xlsx")
sim500_30_10V_highBTW_1c_L3  <- read_xlsx_list("sim500_30_10V_highBTW_1c_L3.xlsx")
sim500_30_10V_highBTW_1c_L4  <- read_xlsx_list("sim500_30_10V_highBTW_1c_L4.xlsx")
sim500_30_10V_highBTW_1c_L5  <- read_xlsx_list("sim500_30_10V_highBTW_1c_L5.xlsx")
sim500_70_10V_highBTW_1c_L1  <- read_xlsx_list("sim500_70_10V_highBTW_1c_L1.xlsx")
sim500_70_10V_highBTW_1c_L2  <- read_xlsx_list("sim500_70_10V_highBTW_1c_L2.xlsx")
sim500_70_10V_highBTW_1c_L3  <- read_xlsx_list("sim500_70_10V_highBTW_1c_L3.xlsx")
sim500_70_10V_highBTW_1c_L4  <- read_xlsx_list("sim500_70_10V_highBTW_1c_L4.xlsx")
sim500_70_10V_highBTW_1c_L5  <- read_xlsx_list("sim500_70_10V_highBTW_1c_L5.xlsx")
sim1000_30_10V_highBTW_1c_L1 <- read_xlsx_list("sim1000_30_10V_highBTW_1c_L1.xlsx")
sim1000_30_10V_highBTW_1c_L2 <- read_xlsx_list("sim1000_30_10V_highBTW_1c_L2.xlsx")
sim1000_30_10V_highBTW_1c_L3 <- read_xlsx_list("sim1000_30_10V_highBTW_1c_L3.xlsx")
sim1000_30_10V_highBTW_1c_L4 <- read_xlsx_list("sim1000_30_10V_highBTW_1c_L4.xlsx")
sim1000_30_10V_highBTW_1c_L5 <- read_xlsx_list("sim1000_30_10V_highBTW_1c_L5.xlsx")
sim1000_70_10V_highBTW_1c_L1 <- read_xlsx_list("sim1000_70_10V_highBTW_1c_L1.xlsx")
sim1000_70_10V_highBTW_1c_L2 <- read_xlsx_list("sim1000_70_10V_highBTW_1c_L2.xlsx")
sim1000_70_10V_highBTW_1c_L3 <- read_xlsx_list("sim1000_70_10V_highBTW_1c_L3.xlsx")
sim1000_70_10V_highBTW_1c_L4 <- read_xlsx_list("sim1000_70_10V_highBTW_1c_L4.xlsx")
sim1000_70_10V_highBTW_1c_L5 <- read_xlsx_list("sim1000_70_10V_highBTW_1c_L5.xlsx")

####### 程序：cal_3_RSF 与 circle_cal_3（与 5_RSF模型_10VC1 相同）############
cal_3_RSF <- function(data, t0 = 1) {
  tryCatch({
    surv_data <- data[!duplicated(data$ID), ]
    surv_time <- surv_data$obs_time
    surv_status <- surv_data$event
    rsf_data <- surv_data[, c(grep("^V[0-9]", names(surv_data), value = TRUE),
                              "obs_time", "event")]
    rfsrc_fit <- rfsrc(
      Surv(obs_time, event) ~ .,
      data = rsf_data,
      ntree      = 1000,
      mtry       = floor(length(grep("^V[0-9]", names(rsf_data), value = TRUE)) / 3),
      nodesize   = 10,
      importance = TRUE,
      proximity  = FALSE,
      seed       = 123
    )
    pred <- predict(rfsrc_fit, newdata = rsf_data)
    time_points <- pred$time.interest
    t0_idx <- which.min(abs(time_points - t0))
    survival_probs <- pred$survival[, t0_idx]
    risk_marker <- 1 - survival_probs
    roc_obj <- timeROC(
      T = surv_time,
      delta = surv_status,
      marker = risk_marker,
      cause = 1,
      times = t0
    )
    AUC <- round(roc_obj$AUC[2], 4)
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
    n_pairs <- sum(comparable)
    n_concordant <- sum(concordant)
    n_tied <- sum(tied)
    Cindex <- ifelse(n_pairs > 0, (n_concordant + 0.5 * n_tied) / n_pairs, NA)
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
    result <- data.frame(AUC = AUC, BS = BS, Cindex = Cindex)
    return(result)
  }, error = function(e) {
    message("⚠️ cal_3_RSF failed: ", e$message)
    return(data.frame(AUC = 0.5, BS = 0.25, Cindex = 0.5))
  })
}

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

############# 运行评估循环 ##########
t0 <- 1

message("【低相关性】测试集 RSF 评估...")
result_RSF_sim500_30_10V_lowINTER_1c_L1  <- circle_cal_3(sim500_30_10V_lowINTER_1c_L1,  t0 = t0)
result_RSF_sim500_30_10V_lowINTER_1c_L2  <- circle_cal_3(sim500_30_10V_lowINTER_1c_L2,  t0 = t0)
result_RSF_sim500_30_10V_lowINTER_1c_L3  <- circle_cal_3(sim500_30_10V_lowINTER_1c_L3,  t0 = t0)
result_RSF_sim500_30_10V_lowINTER_1c_L4  <- circle_cal_3(sim500_30_10V_lowINTER_1c_L4,  t0 = t0)
result_RSF_sim500_30_10V_lowINTER_1c_L5  <- circle_cal_3(sim500_30_10V_lowINTER_1c_L5,  t0 = t0)
result_RSF_sim500_70_10V_lowINTER_1c_L1  <- circle_cal_3(sim500_70_10V_lowINTER_1c_L1,  t0 = t0)
result_RSF_sim500_70_10V_lowINTER_1c_L2  <- circle_cal_3(sim500_70_10V_lowINTER_1c_L2,  t0 = t0)
result_RSF_sim500_70_10V_lowINTER_1c_L3  <- circle_cal_3(sim500_70_10V_lowINTER_1c_L3,  t0 = t0)
result_RSF_sim500_70_10V_lowINTER_1c_L4  <- circle_cal_3(sim500_70_10V_lowINTER_1c_L4,  t0 = t0)
result_RSF_sim500_70_10V_lowINTER_1c_L5  <- circle_cal_3(sim500_70_10V_lowINTER_1c_L5,  t0 = t0)
result_RSF_sim1000_30_10V_lowINTER_1c_L1 <- circle_cal_3(sim1000_30_10V_lowINTER_1c_L1, t0 = t0)
result_RSF_sim1000_30_10V_lowINTER_1c_L2 <- circle_cal_3(sim1000_30_10V_lowINTER_1c_L2, t0 = t0)
result_RSF_sim1000_30_10V_lowINTER_1c_L3 <- circle_cal_3(sim1000_30_10V_lowINTER_1c_L3, t0 = t0)
result_RSF_sim1000_30_10V_lowINTER_1c_L4 <- circle_cal_3(sim1000_30_10V_lowINTER_1c_L4, t0 = t0)
result_RSF_sim1000_30_10V_lowINTER_1c_L5 <- circle_cal_3(sim1000_30_10V_lowINTER_1c_L5, t0 = t0)
result_RSF_sim1000_70_10V_lowINTER_1c_L1 <- circle_cal_3(sim1000_70_10V_lowINTER_1c_L1, t0 = t0)
result_RSF_sim1000_70_10V_lowINTER_1c_L2 <- circle_cal_3(sim1000_70_10V_lowINTER_1c_L2, t0 = t0)
result_RSF_sim1000_70_10V_lowINTER_1c_L3 <- circle_cal_3(sim1000_70_10V_lowINTER_1c_L3, t0 = t0)
result_RSF_sim1000_70_10V_lowINTER_1c_L4 <- circle_cal_3(sim1000_70_10V_lowINTER_1c_L4, t0 = t0)
result_RSF_sim1000_70_10V_lowINTER_1c_L5 <- circle_cal_3(sim1000_70_10V_lowINTER_1c_L5, t0 = t0)

message("【中相关性】测试集 RSF 评估...")
result_RSF_sim500_30_10V_midINTER_1c_L1  <- circle_cal_3(sim500_30_10V_midINTER_1c_L1,  t0 = t0)
result_RSF_sim500_30_10V_midINTER_1c_L2  <- circle_cal_3(sim500_30_10V_midINTER_1c_L2,  t0 = t0)
result_RSF_sim500_30_10V_midINTER_1c_L3  <- circle_cal_3(sim500_30_10V_midINTER_1c_L3,  t0 = t0)
result_RSF_sim500_30_10V_midINTER_1c_L4  <- circle_cal_3(sim500_30_10V_midINTER_1c_L4,  t0 = t0)
result_RSF_sim500_30_10V_midINTER_1c_L5  <- circle_cal_3(sim500_30_10V_midINTER_1c_L5,  t0 = t0)
result_RSF_sim500_70_10V_midINTER_1c_L1  <- circle_cal_3(sim500_70_10V_midINTER_1c_L1,  t0 = t0)
result_RSF_sim500_70_10V_midINTER_1c_L2  <- circle_cal_3(sim500_70_10V_midINTER_1c_L2,  t0 = t0)
result_RSF_sim500_70_10V_midINTER_1c_L3  <- circle_cal_3(sim500_70_10V_midINTER_1c_L3,  t0 = t0)
result_RSF_sim500_70_10V_midINTER_1c_L4  <- circle_cal_3(sim500_70_10V_midINTER_1c_L4,  t0 = t0)
result_RSF_sim500_70_10V_midINTER_1c_L5  <- circle_cal_3(sim500_70_10V_midINTER_1c_L5,  t0 = t0)
result_RSF_sim1000_30_10V_midINTER_1c_L1 <- circle_cal_3(sim1000_30_10V_midINTER_1c_L1, t0 = t0)
result_RSF_sim1000_30_10V_midINTER_1c_L2 <- circle_cal_3(sim1000_30_10V_midINTER_1c_L2, t0 = t0)
result_RSF_sim1000_30_10V_midINTER_1c_L3 <- circle_cal_3(sim1000_30_10V_midINTER_1c_L3, t0 = t0)
result_RSF_sim1000_30_10V_midINTER_1c_L4 <- circle_cal_3(sim1000_30_10V_midINTER_1c_L4, t0 = t0)
result_RSF_sim1000_30_10V_midINTER_1c_L5 <- circle_cal_3(sim1000_30_10V_midINTER_1c_L5, t0 = t0)
result_RSF_sim1000_70_10V_midINTER_1c_L1 <- circle_cal_3(sim1000_70_10V_midINTER_1c_L1, t0 = t0)
result_RSF_sim1000_70_10V_midINTER_1c_L2 <- circle_cal_3(sim1000_70_10V_midINTER_1c_L2, t0 = t0)
result_RSF_sim1000_70_10V_midINTER_1c_L3 <- circle_cal_3(sim1000_70_10V_midINTER_1c_L3, t0 = t0)
result_RSF_sim1000_70_10V_midINTER_1c_L4 <- circle_cal_3(sim1000_70_10V_midINTER_1c_L4, t0 = t0)
result_RSF_sim1000_70_10V_midINTER_1c_L5 <- circle_cal_3(sim1000_70_10V_midINTER_1c_L5, t0 = t0)

message("【高相关性】测试集 RSF 评估...")
result_RSF_sim500_30_10V_highINTER_1c_L1  <- circle_cal_3(sim500_30_10V_highINTER_1c_L1,  t0 = t0)
result_RSF_sim500_30_10V_highINTER_1c_L2  <- circle_cal_3(sim500_30_10V_highINTER_1c_L2,  t0 = t0)
result_RSF_sim500_30_10V_highINTER_1c_L3  <- circle_cal_3(sim500_30_10V_highINTER_1c_L3,  t0 = t0)
result_RSF_sim500_30_10V_highINTER_1c_L4  <- circle_cal_3(sim500_30_10V_highINTER_1c_L4,  t0 = t0)
result_RSF_sim500_30_10V_highINTER_1c_L5  <- circle_cal_3(sim500_30_10V_highINTER_1c_L5,  t0 = t0)
result_RSF_sim500_70_10V_highINTER_1c_L1  <- circle_cal_3(sim500_70_10V_highINTER_1c_L1,  t0 = t0)
result_RSF_sim500_70_10V_highINTER_1c_L2  <- circle_cal_3(sim500_70_10V_highINTER_1c_L2,  t0 = t0)
result_RSF_sim500_70_10V_highINTER_1c_L3  <- circle_cal_3(sim500_70_10V_highINTER_1c_L3,  t0 = t0)
result_RSF_sim500_70_10V_highINTER_1c_L4  <- circle_cal_3(sim500_70_10V_highINTER_1c_L4,  t0 = t0)
result_RSF_sim500_70_10V_highINTER_1c_L5  <- circle_cal_3(sim500_70_10V_highINTER_1c_L5,  t0 = t0)
result_RSF_sim1000_30_10V_highINTER_1c_L1 <- circle_cal_3(sim1000_30_10V_highINTER_1c_L1, t0 = t0)
result_RSF_sim1000_30_10V_highINTER_1c_L2 <- circle_cal_3(sim1000_30_10V_highINTER_1c_L2, t0 = t0)
result_RSF_sim1000_30_10V_highINTER_1c_L3 <- circle_cal_3(sim1000_30_10V_highINTER_1c_L3, t0 = t0)
result_RSF_sim1000_30_10V_highINTER_1c_L4 <- circle_cal_3(sim1000_30_10V_highINTER_1c_L4, t0 = t0)
result_RSF_sim1000_30_10V_highINTER_1c_L5 <- circle_cal_3(sim1000_30_10V_highINTER_1c_L5, t0 = t0)
result_RSF_sim1000_70_10V_highINTER_1c_L1 <- circle_cal_3(sim1000_70_10V_highINTER_1c_L1, t0 = t0)
result_RSF_sim1000_70_10V_highINTER_1c_L2 <- circle_cal_3(sim1000_70_10V_highINTER_1c_L2, t0 = t0)
result_RSF_sim1000_70_10V_highINTER_1c_L3 <- circle_cal_3(sim1000_70_10V_highINTER_1c_L3, t0 = t0)
result_RSF_sim1000_70_10V_highINTER_1c_L4 <- circle_cal_3(sim1000_70_10V_highINTER_1c_L4, t0 = t0)
result_RSF_sim1000_70_10V_highINTER_1c_L5 <- circle_cal_3(sim1000_70_10V_highINTER_1c_L5, t0 = t0)

message("【低相关性-BTW】测试集 RSF 评估...")
result_RSF_sim500_30_10V_lowBTW_1c_L1  <- circle_cal_3(sim500_30_10V_lowBTW_1c_L1,  t0 = t0)
result_RSF_sim500_30_10V_lowBTW_1c_L2  <- circle_cal_3(sim500_30_10V_lowBTW_1c_L2,  t0 = t0)
result_RSF_sim500_30_10V_lowBTW_1c_L3  <- circle_cal_3(sim500_30_10V_lowBTW_1c_L3,  t0 = t0)
result_RSF_sim500_30_10V_lowBTW_1c_L4  <- circle_cal_3(sim500_30_10V_lowBTW_1c_L4,  t0 = t0)
result_RSF_sim500_30_10V_lowBTW_1c_L5  <- circle_cal_3(sim500_30_10V_lowBTW_1c_L5,  t0 = t0)
result_RSF_sim500_70_10V_lowBTW_1c_L1  <- circle_cal_3(sim500_70_10V_lowBTW_1c_L1,  t0 = t0)
result_RSF_sim500_70_10V_lowBTW_1c_L2  <- circle_cal_3(sim500_70_10V_lowBTW_1c_L2,  t0 = t0)
result_RSF_sim500_70_10V_lowBTW_1c_L3  <- circle_cal_3(sim500_70_10V_lowBTW_1c_L3,  t0 = t0)
result_RSF_sim500_70_10V_lowBTW_1c_L4  <- circle_cal_3(sim500_70_10V_lowBTW_1c_L4,  t0 = t0)
result_RSF_sim500_70_10V_lowBTW_1c_L5  <- circle_cal_3(sim500_70_10V_lowBTW_1c_L5,  t0 = t0)
result_RSF_sim1000_30_10V_lowBTW_1c_L1 <- circle_cal_3(sim1000_30_10V_lowBTW_1c_L1, t0 = t0)
result_RSF_sim1000_30_10V_lowBTW_1c_L2 <- circle_cal_3(sim1000_30_10V_lowBTW_1c_L2, t0 = t0)
result_RSF_sim1000_30_10V_lowBTW_1c_L3 <- circle_cal_3(sim1000_30_10V_lowBTW_1c_L3, t0 = t0)
result_RSF_sim1000_30_10V_lowBTW_1c_L4 <- circle_cal_3(sim1000_30_10V_lowBTW_1c_L4, t0 = t0)
result_RSF_sim1000_30_10V_lowBTW_1c_L5 <- circle_cal_3(sim1000_30_10V_lowBTW_1c_L5, t0 = t0)
result_RSF_sim1000_70_10V_lowBTW_1c_L1 <- circle_cal_3(sim1000_70_10V_lowBTW_1c_L1, t0 = t0)
result_RSF_sim1000_70_10V_lowBTW_1c_L2 <- circle_cal_3(sim1000_70_10V_lowBTW_1c_L2, t0 = t0)
result_RSF_sim1000_70_10V_lowBTW_1c_L3 <- circle_cal_3(sim1000_70_10V_lowBTW_1c_L3, t0 = t0)
result_RSF_sim1000_70_10V_lowBTW_1c_L4 <- circle_cal_3(sim1000_70_10V_lowBTW_1c_L4, t0 = t0)
result_RSF_sim1000_70_10V_lowBTW_1c_L5 <- circle_cal_3(sim1000_70_10V_lowBTW_1c_L5, t0 = t0)

message("【中相关性-BTW】测试集 RSF 评估...")
result_RSF_sim500_30_10V_midBTW_1c_L1  <- circle_cal_3(sim500_30_10V_midBTW_1c_L1,  t0 = t0)
result_RSF_sim500_30_10V_midBTW_1c_L2  <- circle_cal_3(sim500_30_10V_midBTW_1c_L2,  t0 = t0)
result_RSF_sim500_30_10V_midBTW_1c_L3  <- circle_cal_3(sim500_30_10V_midBTW_1c_L3,  t0 = t0)
result_RSF_sim500_30_10V_midBTW_1c_L4  <- circle_cal_3(sim500_30_10V_midBTW_1c_L4,  t0 = t0)
result_RSF_sim500_30_10V_midBTW_1c_L5  <- circle_cal_3(sim500_30_10V_midBTW_1c_L5,  t0 = t0)
result_RSF_sim500_70_10V_midBTW_1c_L1  <- circle_cal_3(sim500_70_10V_midBTW_1c_L1,  t0 = t0)
result_RSF_sim500_70_10V_midBTW_1c_L2  <- circle_cal_3(sim500_70_10V_midBTW_1c_L2,  t0 = t0)
result_RSF_sim500_70_10V_midBTW_1c_L3  <- circle_cal_3(sim500_70_10V_midBTW_1c_L3,  t0 = t0)
result_RSF_sim500_70_10V_midBTW_1c_L4  <- circle_cal_3(sim500_70_10V_midBTW_1c_L4,  t0 = t0)
result_RSF_sim500_70_10V_midBTW_1c_L5  <- circle_cal_3(sim500_70_10V_midBTW_1c_L5,  t0 = t0)
result_RSF_sim1000_30_10V_midBTW_1c_L1 <- circle_cal_3(sim1000_30_10V_midBTW_1c_L1, t0 = t0)
result_RSF_sim1000_30_10V_midBTW_1c_L2 <- circle_cal_3(sim1000_30_10V_midBTW_1c_L2, t0 = t0)
result_RSF_sim1000_30_10V_midBTW_1c_L3 <- circle_cal_3(sim1000_30_10V_midBTW_1c_L3, t0 = t0)
result_RSF_sim1000_30_10V_midBTW_1c_L4 <- circle_cal_3(sim1000_30_10V_midBTW_1c_L4, t0 = t0)
result_RSF_sim1000_30_10V_midBTW_1c_L5 <- circle_cal_3(sim1000_30_10V_midBTW_1c_L5, t0 = t0)
result_RSF_sim1000_70_10V_midBTW_1c_L1 <- circle_cal_3(sim1000_70_10V_midBTW_1c_L1, t0 = t0)
result_RSF_sim1000_70_10V_midBTW_1c_L2 <- circle_cal_3(sim1000_70_10V_midBTW_1c_L2, t0 = t0)
result_RSF_sim1000_70_10V_midBTW_1c_L3 <- circle_cal_3(sim1000_70_10V_midBTW_1c_L3, t0 = t0)
result_RSF_sim1000_70_10V_midBTW_1c_L4 <- circle_cal_3(sim1000_70_10V_midBTW_1c_L4, t0 = t0)
result_RSF_sim1000_70_10V_midBTW_1c_L5 <- circle_cal_3(sim1000_70_10V_midBTW_1c_L5, t0 = t0)

message("【高相关性-BTW】测试集 RSF 评估...")
result_RSF_sim500_30_10V_highBTW_1c_L1  <- circle_cal_3(sim500_30_10V_highBTW_1c_L1,  t0 = t0)
result_RSF_sim500_30_10V_highBTW_1c_L2  <- circle_cal_3(sim500_30_10V_highBTW_1c_L2,  t0 = t0)
result_RSF_sim500_30_10V_highBTW_1c_L3  <- circle_cal_3(sim500_30_10V_highBTW_1c_L3,  t0 = t0)
result_RSF_sim500_30_10V_highBTW_1c_L4  <- circle_cal_3(sim500_30_10V_highBTW_1c_L4,  t0 = t0)
result_RSF_sim500_30_10V_highBTW_1c_L5  <- circle_cal_3(sim500_30_10V_highBTW_1c_L5,  t0 = t0)
result_RSF_sim500_70_10V_highBTW_1c_L1  <- circle_cal_3(sim500_70_10V_highBTW_1c_L1,  t0 = t0)
result_RSF_sim500_70_10V_highBTW_1c_L2  <- circle_cal_3(sim500_70_10V_highBTW_1c_L2,  t0 = t0)
result_RSF_sim500_70_10V_highBTW_1c_L3  <- circle_cal_3(sim500_70_10V_highBTW_1c_L3,  t0 = t0)
result_RSF_sim500_70_10V_highBTW_1c_L4  <- circle_cal_3(sim500_70_10V_highBTW_1c_L4,  t0 = t0)
result_RSF_sim500_70_10V_highBTW_1c_L5  <- circle_cal_3(sim500_70_10V_highBTW_1c_L5,  t0 = t0)
result_RSF_sim1000_30_10V_highBTW_1c_L1 <- circle_cal_3(sim1000_30_10V_highBTW_1c_L1, t0 = t0)
result_RSF_sim1000_30_10V_highBTW_1c_L2 <- circle_cal_3(sim1000_30_10V_highBTW_1c_L2, t0 = t0)
result_RSF_sim1000_30_10V_highBTW_1c_L3 <- circle_cal_3(sim1000_30_10V_highBTW_1c_L3, t0 = t0)
result_RSF_sim1000_30_10V_highBTW_1c_L4 <- circle_cal_3(sim1000_30_10V_highBTW_1c_L4, t0 = t0)
result_RSF_sim1000_30_10V_highBTW_1c_L5 <- circle_cal_3(sim1000_30_10V_highBTW_1c_L5, t0 = t0)
result_RSF_sim1000_70_10V_highBTW_1c_L1 <- circle_cal_3(sim1000_70_10V_highBTW_1c_L1, t0 = t0)
result_RSF_sim1000_70_10V_highBTW_1c_L2 <- circle_cal_3(sim1000_70_10V_highBTW_1c_L2, t0 = t0)
result_RSF_sim1000_70_10V_highBTW_1c_L3 <- circle_cal_3(sim1000_70_10V_highBTW_1c_L3, t0 = t0)
result_RSF_sim1000_70_10V_highBTW_1c_L4 <- circle_cal_3(sim1000_70_10V_highBTW_1c_L4, t0 = t0)
result_RSF_sim1000_70_10V_highBTW_1c_L5 <- circle_cal_3(sim1000_70_10V_highBTW_1c_L5, t0 = t0)

########### 保存结果（result_RSF*）#################
message("【保存结果】", output_dir)

result_nms <- c(
  "result_RSF_sim500_30_10V_lowINTER_1c_L1", "result_RSF_sim500_30_10V_lowINTER_1c_L2",
  "result_RSF_sim500_30_10V_lowINTER_1c_L3", "result_RSF_sim500_30_10V_lowINTER_1c_L4",
  "result_RSF_sim500_30_10V_lowINTER_1c_L5", "result_RSF_sim500_70_10V_lowINTER_1c_L1",
  "result_RSF_sim500_70_10V_lowINTER_1c_L2", "result_RSF_sim500_70_10V_lowINTER_1c_L3",
  "result_RSF_sim500_70_10V_lowINTER_1c_L4", "result_RSF_sim500_70_10V_lowINTER_1c_L5",
  "result_RSF_sim1000_30_10V_lowINTER_1c_L1", "result_RSF_sim1000_30_10V_lowINTER_1c_L2",
  "result_RSF_sim1000_30_10V_lowINTER_1c_L3", "result_RSF_sim1000_30_10V_lowINTER_1c_L4",
  "result_RSF_sim1000_30_10V_lowINTER_1c_L5", "result_RSF_sim1000_70_10V_lowINTER_1c_L1",
  "result_RSF_sim1000_70_10V_lowINTER_1c_L2", "result_RSF_sim1000_70_10V_lowINTER_1c_L3",
  "result_RSF_sim1000_70_10V_lowINTER_1c_L4", "result_RSF_sim1000_70_10V_lowINTER_1c_L5",
  "result_RSF_sim500_30_10V_midINTER_1c_L1", "result_RSF_sim500_30_10V_midINTER_1c_L2",
  "result_RSF_sim500_30_10V_midINTER_1c_L3", "result_RSF_sim500_30_10V_midINTER_1c_L4",
  "result_RSF_sim500_30_10V_midINTER_1c_L5", "result_RSF_sim500_70_10V_midINTER_1c_L1",
  "result_RSF_sim500_70_10V_midINTER_1c_L2", "result_RSF_sim500_70_10V_midINTER_1c_L3",
  "result_RSF_sim500_70_10V_midINTER_1c_L4", "result_RSF_sim500_70_10V_midINTER_1c_L5",
  "result_RSF_sim1000_30_10V_midINTER_1c_L1", "result_RSF_sim1000_30_10V_midINTER_1c_L2",
  "result_RSF_sim1000_30_10V_midINTER_1c_L3", "result_RSF_sim1000_30_10V_midINTER_1c_L4",
  "result_RSF_sim1000_30_10V_midINTER_1c_L5", "result_RSF_sim1000_70_10V_midINTER_1c_L1",
  "result_RSF_sim1000_70_10V_midINTER_1c_L2", "result_RSF_sim1000_70_10V_midINTER_1c_L3",
  "result_RSF_sim1000_70_10V_midINTER_1c_L4", "result_RSF_sim1000_70_10V_midINTER_1c_L5",
  "result_RSF_sim500_30_10V_highINTER_1c_L1", "result_RSF_sim500_30_10V_highINTER_1c_L2",
  "result_RSF_sim500_30_10V_highINTER_1c_L3", "result_RSF_sim500_30_10V_highINTER_1c_L4",
  "result_RSF_sim500_30_10V_highINTER_1c_L5", "result_RSF_sim500_70_10V_highINTER_1c_L1",
  "result_RSF_sim500_70_10V_highINTER_1c_L2", "result_RSF_sim500_70_10V_highINTER_1c_L3",
  "result_RSF_sim500_70_10V_highINTER_1c_L4", "result_RSF_sim500_70_10V_highINTER_1c_L5",
  "result_RSF_sim1000_30_10V_highINTER_1c_L1", "result_RSF_sim1000_30_10V_highINTER_1c_L2",
  "result_RSF_sim1000_30_10V_highINTER_1c_L3", "result_RSF_sim1000_30_10V_highINTER_1c_L4",
  "result_RSF_sim1000_30_10V_highINTER_1c_L5", "result_RSF_sim1000_70_10V_highINTER_1c_L1",
  "result_RSF_sim1000_70_10V_highINTER_1c_L2", "result_RSF_sim1000_70_10V_highINTER_1c_L3",
  "result_RSF_sim1000_70_10V_highINTER_1c_L4", "result_RSF_sim1000_70_10V_highINTER_1c_L5",
  "result_RSF_sim500_30_10V_lowBTW_1c_L1", "result_RSF_sim500_30_10V_lowBTW_1c_L2",
  "result_RSF_sim500_30_10V_lowBTW_1c_L3", "result_RSF_sim500_30_10V_lowBTW_1c_L4",
  "result_RSF_sim500_30_10V_lowBTW_1c_L5", "result_RSF_sim500_70_10V_lowBTW_1c_L1",
  "result_RSF_sim500_70_10V_lowBTW_1c_L2", "result_RSF_sim500_70_10V_lowBTW_1c_L3",
  "result_RSF_sim500_70_10V_lowBTW_1c_L4", "result_RSF_sim500_70_10V_lowBTW_1c_L5",
  "result_RSF_sim1000_30_10V_lowBTW_1c_L1", "result_RSF_sim1000_30_10V_lowBTW_1c_L2",
  "result_RSF_sim1000_30_10V_lowBTW_1c_L3", "result_RSF_sim1000_30_10V_lowBTW_1c_L4",
  "result_RSF_sim1000_30_10V_lowBTW_1c_L5", "result_RSF_sim1000_70_10V_lowBTW_1c_L1",
  "result_RSF_sim1000_70_10V_lowBTW_1c_L2", "result_RSF_sim1000_70_10V_lowBTW_1c_L3",
  "result_RSF_sim1000_70_10V_lowBTW_1c_L4", "result_RSF_sim1000_70_10V_lowBTW_1c_L5",
  "result_RSF_sim500_30_10V_midBTW_1c_L1", "result_RSF_sim500_30_10V_midBTW_1c_L2",
  "result_RSF_sim500_30_10V_midBTW_1c_L3", "result_RSF_sim500_30_10V_midBTW_1c_L4",
  "result_RSF_sim500_30_10V_midBTW_1c_L5", "result_RSF_sim500_70_10V_midBTW_1c_L1",
  "result_RSF_sim500_70_10V_midBTW_1c_L2", "result_RSF_sim500_70_10V_midBTW_1c_L3",
  "result_RSF_sim500_70_10V_midBTW_1c_L4", "result_RSF_sim500_70_10V_midBTW_1c_L5",
  "result_RSF_sim1000_30_10V_midBTW_1c_L1", "result_RSF_sim1000_30_10V_midBTW_1c_L2",
  "result_RSF_sim1000_30_10V_midBTW_1c_L3", "result_RSF_sim1000_30_10V_midBTW_1c_L4",
  "result_RSF_sim1000_30_10V_midBTW_1c_L5", "result_RSF_sim1000_70_10V_midBTW_1c_L1",
  "result_RSF_sim1000_70_10V_midBTW_1c_L2", "result_RSF_sim1000_70_10V_midBTW_1c_L3",
  "result_RSF_sim1000_70_10V_midBTW_1c_L4", "result_RSF_sim1000_70_10V_midBTW_1c_L5",
  "result_RSF_sim500_30_10V_highBTW_1c_L1", "result_RSF_sim500_30_10V_highBTW_1c_L2",
  "result_RSF_sim500_30_10V_highBTW_1c_L3", "result_RSF_sim500_30_10V_highBTW_1c_L4",
  "result_RSF_sim500_30_10V_highBTW_1c_L5", "result_RSF_sim500_70_10V_highBTW_1c_L1",
  "result_RSF_sim500_70_10V_highBTW_1c_L2", "result_RSF_sim500_70_10V_highBTW_1c_L3",
  "result_RSF_sim500_70_10V_highBTW_1c_L4", "result_RSF_sim500_70_10V_highBTW_1c_L5",
  "result_RSF_sim1000_30_10V_highBTW_1c_L1", "result_RSF_sim1000_30_10V_highBTW_1c_L2",
  "result_RSF_sim1000_30_10V_highBTW_1c_L3", "result_RSF_sim1000_30_10V_highBTW_1c_L4",
  "result_RSF_sim1000_30_10V_highBTW_1c_L5", "result_RSF_sim1000_70_10V_highBTW_1c_L1",
  "result_RSF_sim1000_70_10V_highBTW_1c_L2", "result_RSF_sim1000_70_10V_highBTW_1c_L3",
  "result_RSF_sim1000_70_10V_highBTW_1c_L4", "result_RSF_sim1000_70_10V_highBTW_1c_L5"
)

for (nm in result_nms) {
  obj <- get(nm, envir = .GlobalEnv)
  out_path <- file.path(output_dir, paste0(nm, ".xlsx"))
  write_xlsx(obj, out_path)
  message("  已保存: ", out_path)
}

message("✅ 10V1C 测试集 RSF 评估完成（INTER + BTW）！结果目录: ", output_dir)
