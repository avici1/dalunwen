# =============================================================================
# 5_JM_10V3C_测试集.R
# JM（Joint Model）在 10V3C 测试集上的评估
# 数据来源：模拟数据_10V3C（测试集）
# 结果保存：result_JM*.xlsx
# 说明：每个 sheet 为全样本单次拟合（无 K 折）；指标为样本内评估，非独立验证集。
# 保存：每条结果计算后立刻 write_xlsx（同 5_Jointmodel_4VC1.R）；可分段运行，跑多少存多少。
# =============================================================================

library(survival)
library(timeROC)
library(dplyr)
library(joineRML)
library(readxl)
library(writexl)
library(tidyverse)
library(purrr)
set.seed(123)
options(scipen = 999)
options(pillar.width = Inf)

# 测试集数据路径（模拟数据_10V3C 的 生成Y 目录）
input_dir <- "F:/文章_大论文/0319大改/测试集/模拟数据_10V3C/生成Y"
# 结果保存路径
output_dir <- "F:/文章_大论文/0319大改/结果"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# 与 5_Jointmodel_4VC1.R 一致：每算完一个结果立即写出到 output_dir（分段运行也随时落盘）
save_jm_result <- function(obj, name) {
  out_path <- file.path(output_dir, paste0(name, ".xlsx"))
  write_xlsx(obj, out_path)
  message("  已保存: ", out_path)
}

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

###### 读取测试集数据（10V3C：INTER + BTW）######
message("【读取测试集】", input_dir)

sim500_30_10V_lowINTER_3c_L1   <- read_xlsx_list("sim500_30_10V_lowINTER_3c_L1.xlsx")
#sim500_30_10V_lowINTER_3c_L2   <- read_xlsx_list("sim500_30_10V_lowINTER_3c_L2.xlsx")
#sim500_30_10V_lowINTER_3c_L3   <- read_xlsx_list("sim500_30_10V_lowINTER_3c_L3.xlsx")
#sim500_30_10V_lowINTER_3c_L4   <- read_xlsx_list("sim500_30_10V_lowINTER_3c_L4.xlsx")
#sim500_30_10V_lowINTER_3c_L5   <- read_xlsx_list("sim500_30_10V_lowINTER_3c_L5.xlsx")
sim500_70_10V_lowINTER_3c_L1   <- read_xlsx_list("sim500_70_10V_lowINTER_3c_L1.xlsx")
#sim500_70_10V_lowINTER_3c_L2   <- read_xlsx_list("sim500_70_10V_lowINTER_3c_L2.xlsx")
#sim500_70_10V_lowINTER_3c_L3   <- read_xlsx_list("sim500_70_10V_lowINTER_3c_L3.xlsx")
#sim500_70_10V_lowINTER_3c_L4   <- read_xlsx_list("sim500_70_10V_lowINTER_3c_L4.xlsx")
#sim500_70_10V_lowINTER_3c_L5   <- read_xlsx_list("sim500_70_10V_lowINTER_3c_L5.xlsx")
sim1000_30_10V_lowINTER_3c_L1  <- read_xlsx_list("sim1000_30_10V_lowINTER_3c_L1.xlsx")
#sim1000_30_10V_lowINTER_3c_L2  <- read_xlsx_list("sim1000_30_10V_lowINTER_3c_L2.xlsx")
#sim1000_30_10V_lowINTER_3c_L3  <- read_xlsx_list("sim1000_30_10V_lowINTER_3c_L3.xlsx")
#sim1000_30_10V_lowINTER_3c_L4  <- read_xlsx_list("sim1000_30_10V_lowINTER_3c_L4.xlsx")
#sim1000_30_10V_lowINTER_3c_L5  <- read_xlsx_list("sim1000_30_10V_lowINTER_3c_L5.xlsx")
sim1000_70_10V_lowINTER_3c_L1  <- read_xlsx_list("sim1000_70_10V_lowINTER_3c_L1.xlsx")
#sim1000_70_10V_lowINTER_3c_L2  <- read_xlsx_list("sim1000_70_10V_lowINTER_3c_L2.xlsx")
#sim1000_70_10V_lowINTER_3c_L3  <- read_xlsx_list("sim1000_70_10V_lowINTER_3c_L3.xlsx")
#sim1000_70_10V_lowINTER_3c_L4  <- read_xlsx_list("sim1000_70_10V_lowINTER_3c_L4.xlsx")
#sim1000_70_10V_lowINTER_3c_L5  <- read_xlsx_list("sim1000_70_10V_lowINTER_3c_L5.xlsx")

sim500_30_10V_midINTER_3c_L1   <- read_xlsx_list("sim500_30_10V_midINTER_3c_L1.xlsx")
#sim500_30_10V_midINTER_3c_L2   <- read_xlsx_list("sim500_30_10V_midINTER_3c_L2.xlsx")
#sim500_30_10V_midINTER_3c_L3   <- read_xlsx_list("sim500_30_10V_midINTER_3c_L3.xlsx")
#sim500_30_10V_midINTER_3c_L4   <- read_xlsx_list("sim500_30_10V_midINTER_3c_L4.xlsx")
#sim500_30_10V_midINTER_3c_L5   <- read_xlsx_list("sim500_30_10V_midINTER_3c_L5.xlsx")
sim500_70_10V_midINTER_3c_L1   <- read_xlsx_list("sim500_70_10V_midINTER_3c_L1.xlsx")
#sim500_70_10V_midINTER_3c_L2   <- read_xlsx_list("sim500_70_10V_midINTER_3c_L2.xlsx")
#sim500_70_10V_midINTER_3c_L3   <- read_xlsx_list("sim500_70_10V_midINTER_3c_L3.xlsx")
#sim500_70_10V_midINTER_3c_L4   <- read_xlsx_list("sim500_70_10V_midINTER_3c_L4.xlsx")
#sim500_70_10V_midINTER_3c_L5   <- read_xlsx_list("sim500_70_10V_midINTER_3c_L5.xlsx")
sim1000_30_10V_midINTER_3c_L1  <- read_xlsx_list("sim1000_30_10V_midINTER_3c_L1.xlsx")
#sim1000_30_10V_midINTER_3c_L2  <- read_xlsx_list("sim1000_30_10V_midINTER_3c_L2.xlsx")
#sim1000_30_10V_midINTER_3c_L3  <- read_xlsx_list("sim1000_30_10V_midINTER_3c_L3.xlsx")
#sim1000_30_10V_midINTER_3c_L4  <- read_xlsx_list("sim1000_30_10V_midINTER_3c_L4.xlsx")
#sim1000_30_10V_midINTER_3c_L5  <- read_xlsx_list("sim1000_30_10V_midINTER_3c_L5.xlsx")
sim1000_70_10V_midINTER_3c_L1  <- read_xlsx_list("sim1000_70_10V_midINTER_3c_L1.xlsx")
#sim1000_70_10V_midINTER_3c_L2  <- read_xlsx_list("sim1000_70_10V_midINTER_3c_L2.xlsx")
#sim1000_70_10V_midINTER_3c_L3  <- read_xlsx_list("sim1000_70_10V_midINTER_3c_L3.xlsx")
#sim1000_70_10V_midINTER_3c_L4  <- read_xlsx_list("sim1000_70_10V_midINTER_3c_L4.xlsx")
#sim1000_70_10V_midINTER_3c_L5  <- read_xlsx_list("sim1000_70_10V_midINTER_3c_L5.xlsx")

sim500_30_10V_highINTER_3c_L1  <- read_xlsx_list("sim500_30_10V_highINTER_3c_L1.xlsx")
#sim500_30_10V_highINTER_3c_L2  <- read_xlsx_list("sim500_30_10V_highINTER_3c_L2.xlsx")
#sim500_30_10V_highINTER_3c_L3  <- read_xlsx_list("sim500_30_10V_highINTER_3c_L3.xlsx")
#sim500_30_10V_highINTER_3c_L4  <- read_xlsx_list("sim500_30_10V_highINTER_3c_L4.xlsx")
#sim500_30_10V_highINTER_3c_L5  <- read_xlsx_list("sim500_30_10V_highINTER_3c_L5.xlsx")
sim500_70_10V_highINTER_3c_L1  <- read_xlsx_list("sim500_70_10V_highINTER_3c_L1.xlsx")
#sim500_70_10V_highINTER_3c_L2  <- read_xlsx_list("sim500_70_10V_highINTER_3c_L2.xlsx")
#sim500_70_10V_highINTER_3c_L3  <- read_xlsx_list("sim500_70_10V_highINTER_3c_L3.xlsx")
#sim500_70_10V_highINTER_3c_L4  <- read_xlsx_list("sim500_70_10V_highINTER_3c_L4.xlsx")
#sim500_70_10V_highINTER_3c_L5  <- read_xlsx_list("sim500_70_10V_highINTER_3c_L5.xlsx")
sim1000_30_10V_highINTER_3c_L1 <- read_xlsx_list("sim1000_30_10V_highINTER_3c_L1.xlsx")
#sim1000_30_10V_highINTER_3c_L2 <- read_xlsx_list("sim1000_30_10V_highINTER_3c_L2.xlsx")
#sim1000_30_10V_highINTER_3c_L3 <- read_xlsx_list("sim1000_30_10V_highINTER_3c_L3.xlsx")
#sim1000_30_10V_highINTER_3c_L4 <- read_xlsx_list("sim1000_30_10V_highINTER_3c_L4.xlsx")
#sim1000_30_10V_highINTER_3c_L5 <- read_xlsx_list("sim1000_30_10V_highINTER_3c_L5.xlsx")
sim1000_70_10V_highINTER_3c_L1 <- read_xlsx_list("sim1000_70_10V_highINTER_3c_L1.xlsx")
#sim1000_70_10V_highINTER_3c_L2 <- read_xlsx_list("sim1000_70_10V_highINTER_3c_L2.xlsx")
#sim1000_70_10V_highINTER_3c_L3 <- read_xlsx_list("sim1000_70_10V_highINTER_3c_L3.xlsx")
#sim1000_70_10V_highINTER_3c_L4 <- read_xlsx_list("sim1000_70_10V_highINTER_3c_L4.xlsx")
#sim1000_70_10V_highINTER_3c_L5 <- read_xlsx_list("sim1000_70_10V_highINTER_3c_L5.xlsx")

# BTW 数据
sim500_30_10V_lowBTW_3c_L1   <- read_xlsx_list("sim500_30_10V_lowBTW_3c_L1.xlsx")
#sim500_30_10V_lowBTW_3c_L2   <- read_xlsx_list("sim500_30_10V_lowBTW_3c_L2.xlsx")
#sim500_30_10V_lowBTW_3c_L3   <- read_xlsx_list("sim500_30_10V_lowBTW_3c_L3.xlsx")
#sim500_30_10V_lowBTW_3c_L4   <- read_xlsx_list("sim500_30_10V_lowBTW_3c_L4.xlsx")
#sim500_30_10V_lowBTW_3c_L5   <- read_xlsx_list("sim500_30_10V_lowBTW_3c_L5.xlsx")
sim500_70_10V_lowBTW_3c_L1   <- read_xlsx_list("sim500_70_10V_lowBTW_3c_L1.xlsx")
#sim500_70_10V_lowBTW_3c_L2   <- read_xlsx_list("sim500_70_10V_lowBTW_3c_L2.xlsx")
#sim500_70_10V_lowBTW_3c_L3   <- read_xlsx_list("sim500_70_10V_lowBTW_3c_L3.xlsx")
#sim500_70_10V_lowBTW_3c_L4   <- read_xlsx_list("sim500_70_10V_lowBTW_3c_L4.xlsx")
#sim500_70_10V_lowBTW_3c_L5   <- read_xlsx_list("sim500_70_10V_lowBTW_3c_L5.xlsx")
sim1000_30_10V_lowBTW_3c_L1  <- read_xlsx_list("sim1000_30_10V_lowBTW_3c_L1.xlsx")
#sim1000_30_10V_lowBTW_3c_L2  <- read_xlsx_list("sim1000_30_10V_lowBTW_3c_L2.xlsx")
#sim1000_30_10V_lowBTW_3c_L3  <- read_xlsx_list("sim1000_30_10V_lowBTW_3c_L3.xlsx")
#sim1000_30_10V_lowBTW_3c_L4  <- read_xlsx_list("sim1000_30_10V_lowBTW_3c_L4.xlsx")
#sim1000_30_10V_lowBTW_3c_L5  <- read_xlsx_list("sim1000_30_10V_lowBTW_3c_L5.xlsx")
sim1000_70_10V_lowBTW_3c_L1  <- read_xlsx_list("sim1000_70_10V_lowBTW_3c_L1.xlsx")
#sim1000_70_10V_lowBTW_3c_L2  <- read_xlsx_list("sim1000_70_10V_lowBTW_3c_L2.xlsx")
#sim1000_70_10V_lowBTW_3c_L3  <- read_xlsx_list("sim1000_70_10V_lowBTW_3c_L3.xlsx")
#sim1000_70_10V_lowBTW_3c_L4  <- read_xlsx_list("sim1000_70_10V_lowBTW_3c_L4.xlsx")
#sim1000_70_10V_lowBTW_3c_L5  <- read_xlsx_list("sim1000_70_10V_lowBTW_3c_L5.xlsx")

sim500_30_10V_midBTW_3c_L1   <- read_xlsx_list("sim500_30_10V_midBTW_3c_L1.xlsx")
#sim500_30_10V_midBTW_3c_L2   <- read_xlsx_list("sim500_30_10V_midBTW_3c_L2.xlsx")
#sim500_30_10V_midBTW_3c_L3   <- read_xlsx_list("sim500_30_10V_midBTW_3c_L3.xlsx")
#sim500_30_10V_midBTW_3c_L4   <- read_xlsx_list("sim500_30_10V_midBTW_3c_L4.xlsx")
#sim500_30_10V_midBTW_3c_L5   <- read_xlsx_list("sim500_30_10V_midBTW_3c_L5.xlsx")
sim500_70_10V_midBTW_3c_L1   <- read_xlsx_list("sim500_70_10V_midBTW_3c_L1.xlsx")
#sim500_70_10V_midBTW_3c_L2   <- read_xlsx_list("sim500_70_10V_midBTW_3c_L2.xlsx")
#sim500_70_10V_midBTW_3c_L3   <- read_xlsx_list("sim500_70_10V_midBTW_3c_L3.xlsx")
#sim500_70_10V_midBTW_3c_L4   <- read_xlsx_list("sim500_70_10V_midBTW_3c_L4.xlsx")
#sim500_70_10V_midBTW_3c_L5   <- read_xlsx_list("sim500_70_10V_midBTW_3c_L5.xlsx")
sim1000_30_10V_midBTW_3c_L1  <- read_xlsx_list("sim1000_30_10V_midBTW_3c_L1.xlsx")
#sim1000_30_10V_midBTW_3c_L2  <- read_xlsx_list("sim1000_30_10V_midBTW_3c_L2.xlsx")
#sim1000_30_10V_midBTW_3c_L3  <- read_xlsx_list("sim1000_30_10V_midBTW_3c_L3.xlsx")
#sim1000_30_10V_midBTW_3c_L4  <- read_xlsx_list("sim1000_30_10V_midBTW_3c_L4.xlsx")
#sim1000_30_10V_midBTW_3c_L5  <- read_xlsx_list("sim1000_30_10V_midBTW_3c_L5.xlsx")
sim1000_70_10V_midBTW_3c_L1  <- read_xlsx_list("sim1000_70_10V_midBTW_3c_L1.xlsx")
#sim1000_70_10V_midBTW_3c_L2  <- read_xlsx_list("sim1000_70_10V_midBTW_3c_L2.xlsx")
#sim1000_70_10V_midBTW_3c_L3  <- read_xlsx_list("sim1000_70_10V_midBTW_3c_L3.xlsx")
#sim1000_70_10V_midBTW_3c_L4  <- read_xlsx_list("sim1000_70_10V_midBTW_3c_L4.xlsx")
#sim1000_70_10V_midBTW_3c_L5  <- read_xlsx_list("sim1000_70_10V_midBTW_3c_L5.xlsx")

sim500_30_10V_highBTW_3c_L1  <- read_xlsx_list("sim500_30_10V_highBTW_3c_L1.xlsx")
#sim500_30_10V_highBTW_3c_L2  <- read_xlsx_list("sim500_30_10V_highBTW_3c_L2.xlsx")
#sim500_30_10V_highBTW_3c_L3  <- read_xlsx_list("sim500_30_10V_highBTW_3c_L3.xlsx")
#sim500_30_10V_highBTW_3c_L4  <- read_xlsx_list("sim500_30_10V_highBTW_3c_L4.xlsx")
#sim500_30_10V_highBTW_3c_L5  <- read_xlsx_list("sim500_30_10V_highBTW_3c_L5.xlsx")
sim500_70_10V_highBTW_3c_L1  <- read_xlsx_list("sim500_70_10V_highBTW_3c_L1.xlsx")
#sim500_70_10V_highBTW_3c_L2  <- read_xlsx_list("sim500_70_10V_highBTW_3c_L2.xlsx")
#sim500_70_10V_highBTW_3c_L3  <- read_xlsx_list("sim500_70_10V_highBTW_3c_L3.xlsx")
#sim500_70_10V_highBTW_3c_L4  <- read_xlsx_list("sim500_70_10V_highBTW_3c_L4.xlsx")
#sim500_70_10V_highBTW_3c_L5  <- read_xlsx_list("sim500_70_10V_highBTW_3c_L5.xlsx")
sim1000_30_10V_highBTW_3c_L1 <- read_xlsx_list("sim1000_30_10V_highBTW_3c_L1.xlsx")
#sim1000_30_10V_highBTW_3c_L2 <- read_xlsx_list("sim1000_30_10V_highBTW_3c_L2.xlsx")
#sim1000_30_10V_highBTW_3c_L3 <- read_xlsx_list("sim1000_30_10V_highBTW_3c_L3.xlsx")
#sim1000_30_10V_highBTW_3c_L4 <- read_xlsx_list("sim1000_30_10V_highBTW_3c_L4.xlsx")
#sim1000_30_10V_highBTW_3c_L5 <- read_xlsx_list("sim1000_30_10V_highBTW_3c_L5.xlsx")
sim1000_70_10V_highBTW_3c_L1 <- read_xlsx_list("sim1000_70_10V_highBTW_3c_L1.xlsx")
#sim1000_70_10V_highBTW_3c_L2 <- read_xlsx_list("sim1000_70_10V_highBTW_3c_L2.xlsx")
#sim1000_70_10V_highBTW_3c_L3 <- read_xlsx_list("sim1000_70_10V_highBTW_3c_L3.xlsx")
#sim1000_70_10V_highBTW_3c_L4 <- read_xlsx_list("sim1000_70_10V_highBTW_3c_L4.xlsx")
#sim1000_70_10V_highBTW_3c_L5 <- read_xlsx_list("sim1000_70_10V_highBTW_3c_L5.xlsx")

###### 与 10V 模拟数据 / 5_cox模型_10VC1.R 一致：纵向固定效应含 V1–V10（勿仅用 V1–V6）########
long_fixed_formula <- Y ~ t + V1 + V2 + V3 + V4 + V5 + V6 

####### 程序：cal_3_JM 与 circle_cal_3（测试集：全样本单次拟合，不做 K 折）############
cal_3_JM <- function(data, t0 = 1) {
  tryCatch({
    #----------------#
    # 1. 统一数据清理
    #----------------#
    data_clean <- data[data$t <= data$obs_time, ]
    
    #----------------#
    # 2. 统一建立mjoint模型
    #----------------#
    fit <- mjoint(
      formLongFixed = list(
        "Y" = Y ~ t + V1 + V2 + V3 + V4 + V5 + V6
      ),
      formLongRandom = list(
        "Y" = ~ t | ID
      ),
      formSurv = Surv(obs_time, event) ~ 1,
      data = data_clean,
      timeVar = "t"
    )
    
    #----------------#
    # 3. 提取生存数据（每患者一条）
    #----------------#
    surv_data <- data_clean[!duplicated(data_clean$ID), ]
    surv_time <- surv_data$obs_time
    surv_status <- surv_data$event
    
    #----------------#
    # 4. 提取模型参数
    #----------------#
    beta <- fit$coefficients$beta   # 纵向固定效应
    gamma <- fit$coefficients$gamma # 关联参数
    random_effects <- ranef(fit)    # 随机效应
    
    #----------------#
    # 5. 计算完整的风险得分（固定效应 + 随机效应）
    #----------------#
    # 获取每个个体的基线数据
    baseline_data <- data_clean[!duplicated(data_clean$ID), ]
    baseline_data$t <- t0  # 设置时间为t0
    
    # 计算固定效应部分
    X_matrix <- model.matrix(~ t + V1 + V2 + V3 + V4 + V5 + V6, 
                             data = baseline_data)
    fixed_part <- as.numeric(X_matrix %*% beta)
    
    # 计算随机效应部分（截距 + 斜率×t0）
    random_part <- random_effects[, 1] + random_effects[, 2] * t0
    
    # 完整风险得分 = γ × (固定效应 + 随机效应)
    comprehensive_risk <- as.numeric(gamma) * (fixed_part + random_part)
    
    #----------------#
    # 6. 计算AUC（使用完整风险得分）
    #----------------#
    roc_obj <- timeROC(
      T = surv_time,
      delta = surv_status,
      marker = comprehensive_risk,
      cause = 1,
      times = t0
    )
    AUC <- round(roc_obj$AUC[2], 4)
    
    #----------------#
    # 7. 计算C-index（使用完整风险得分）
    #----------------#
    n <- length(surv_time)
    idx <- combn(n, 2)  # 所有pair的索引
    
    i <- idx[1, ]
    j <- idx[2, ]
    
    # 判断可比对pair（短时间的个体必须发生事件）
    comparable_ij <- (surv_status[i] == 1 & surv_time[i] < surv_time[j])
    comparable_ji <- (surv_status[j] == 1 & surv_time[j] < surv_time[i])
    comparable <- comparable_ij | comparable_ji
    
    # Concordant判断
    concordant <- (comparable_ij & (comprehensive_risk[i] > comprehensive_risk[j])) |
      (comparable_ji & (comprehensive_risk[j] > comprehensive_risk[i]))
    
    tied <- (comparable & (comprehensive_risk[i] == comprehensive_risk[j]))
    
    n_pairs <- sum(comparable)
    n_concordant <- sum(concordant)
    n_tied <- sum(tied)
    
    Cindex <- ifelse(n_pairs > 0, (n_concordant + 0.5 * n_tied) / n_pairs, NA)
    
    #----------------#
    # 8. 计算BS（保持原有方法）
    #----------------#
    # 线性预测子（纵向部分）- 使用完整数据
    lp_long_full <- model.matrix(~ t + V1 + V2 + V3 + V4 + V5 + V6, 
                                 data = data_clean) %*% beta
    
    # 基线生存函数估计
    S0_t0 <- summary(survfit(coxph(Surv(obs_time, event) ~ 1, data = data_clean)), 
                     times = t0)$surv
    
    # 个体预测生存概率
    S_pred <- S0_t0 ^ exp(as.numeric(gamma) * as.numeric(lp_long_full))
    
    # 计算删失权重
    censoring_model <- survfit(Surv(obs_time, 1 - event) ~ 1, data = data_clean)
    
    get_weights <- function(time, event, censoring_model, t0) {
      cens_probs <- summary(censoring_model, times = pmin(time, t0))$surv
      weights <- ifelse(time <= t0 & event == 1, 1/cens_probs, 
                        ifelse(time > t0, 1/summary(censoring_model, times = t0)$surv, 0))
      return(weights)
    }
    
    weights <- get_weights(data_clean$obs_time, data_clean$event, censoring_model, t0)
    
    # 构造观测指标
    Y_obs <- as.numeric(data_clean$obs_time > t0 | 
                          (data_clean$obs_time <= t0 & data_clean$event == 0))
    
    # 计算加权Brier Score
    BS <- mean(weights * (S_pred - Y_obs)^2, na.rm = TRUE)
    
    #----------------#
    # 9. 返回结果
    #----------------#
    result <- data.frame(
      AUC = AUC,
      BS = BS,
      Cindex = Cindex
    )
    
    return(result)
    
  }, error = function(e) {
    # 当模型拟合失败时返回NA值
    # 可选：输出错误信息以便追踪哪些模拟失败了
    message("Model fitting failed: ", conditionMessage(e))
    
    return(data.frame(
      AUC = NA,
      BS = NA,
      Cindex = NA
    ))
  })
}


circle_cal_3 <- function(data_list, t0 = 1, verbose = TRUE) {
  if (!is.list(data_list) || length(data_list) == 0)
    stop("data_list 必须是一个非空 list")
  res <- lapply(seq_along(data_list), function(i) {
    if (verbose) message(sprintf("【%s】Processing sim%g ...", Sys.time(), i))
    tmp <- cal_3_JM(data_list[[i]], t0 = t0)
    cbind(sim = paste0("sim", i), tmp)
  })
  do.call(rbind, res)
}




############# 运行评估循环（测试集全样本单次拟合，无 K 折）##########
t0 <- 1

message("【低相关性】测试集 JM 评估...")
result_JM_sim500_30_10V_lowINTER_3c_L1  <- circle_cal_3(sim500_30_10V_lowINTER_3c_L1,  t0 = t0)
save_jm_result(result_JM_sim500_30_10V_lowINTER_3c_L1, "result_JM_sim500_30_10V_lowINTER_3c_L1")
#result_JM_sim500_30_10V_lowINTER_3c_L2  <- circle_cal_3(sim500_30_10V_lowINTER_3c_L2,  t0 = t0)
#save_jm_result(result_JM_sim500_30_10V_lowINTER_3c_L2, "result_JM_sim500_30_10V_lowINTER_3c_L2")
#result_JM_sim500_30_10V_lowINTER_3c_L3  <- circle_cal_3(sim500_30_10V_lowINTER_3c_L3,  t0 = t0)
#save_jm_result(result_JM_sim500_30_10V_lowINTER_3c_L3, "result_JM_sim500_30_10V_lowINTER_3c_L3")
#result_JM_sim500_30_10V_lowINTER_3c_L4  <- circle_cal_3(sim500_30_10V_lowINTER_3c_L4,  t0 = t0)
#save_jm_result(result_JM_sim500_30_10V_lowINTER_3c_L4, "result_JM_sim500_30_10V_lowINTER_3c_L4")
#result_JM_sim500_30_10V_lowINTER_3c_L5  <- circle_cal_3(sim500_30_10V_lowINTER_3c_L5,  t0 = t0)
#save_jm_result(result_JM_sim500_30_10V_lowINTER_3c_L5, "result_JM_sim500_30_10V_lowINTER_3c_L5")
result_JM_sim500_70_10V_lowINTER_3c_L1  <- circle_cal_3(sim500_70_10V_lowINTER_3c_L1,  t0 = t0)
save_jm_result(result_JM_sim500_70_10V_lowINTER_3c_L1, "result_JM_sim500_70_10V_lowINTER_3c_L1")
#result_JM_sim500_70_10V_lowINTER_3c_L2  <- circle_cal_3(sim500_70_10V_lowINTER_3c_L2,  t0 = t0)
#save_jm_result(result_JM_sim500_70_10V_lowINTER_3c_L2, "result_JM_sim500_70_10V_lowINTER_3c_L2")
#result_JM_sim500_70_10V_lowINTER_3c_L3  <- circle_cal_3(sim500_70_10V_lowINTER_3c_L3,  t0 = t0)
#save_jm_result(result_JM_sim500_70_10V_lowINTER_3c_L3, "result_JM_sim500_70_10V_lowINTER_3c_L3")
#result_JM_sim500_70_10V_lowINTER_3c_L4  <- circle_cal_3(sim500_70_10V_lowINTER_3c_L4,  t0 = t0)
#save_jm_result(result_JM_sim500_70_10V_lowINTER_3c_L4, "result_JM_sim500_70_10V_lowINTER_3c_L4")
#result_JM_sim500_70_10V_lowINTER_3c_L5  <- circle_cal_3(sim500_70_10V_lowINTER_3c_L5,  t0 = t0)
#save_jm_result(result_JM_sim500_70_10V_lowINTER_3c_L5, "result_JM_sim500_70_10V_lowINTER_3c_L5")
result_JM_sim1000_30_10V_lowINTER_3c_L1 <- circle_cal_3(sim1000_30_10V_lowINTER_3c_L1, t0 = t0)
save_jm_result(result_JM_sim1000_30_10V_lowINTER_3c_L1, "result_JM_sim1000_30_10V_lowINTER_3c_L1")
#result_JM_sim1000_30_10V_lowINTER_3c_L2 <- circle_cal_3(sim1000_30_10V_lowINTER_3c_L2, t0 = t0)
#save_jm_result(result_JM_sim1000_30_10V_lowINTER_3c_L2, "result_JM_sim1000_30_10V_lowINTER_3c_L2")
#result_JM_sim1000_30_10V_lowINTER_3c_L3 <- circle_cal_3(sim1000_30_10V_lowINTER_3c_L3, t0 = t0)
#save_jm_result(result_JM_sim1000_30_10V_lowINTER_3c_L3, "result_JM_sim1000_30_10V_lowINTER_3c_L3")
#result_JM_sim1000_30_10V_lowINTER_3c_L4 <- circle_cal_3(sim1000_30_10V_lowINTER_3c_L4, t0 = t0)
#save_jm_result(result_JM_sim1000_30_10V_lowINTER_3c_L4, "result_JM_sim1000_30_10V_lowINTER_3c_L4")
#result_JM_sim1000_30_10V_lowINTER_3c_L5 <- circle_cal_3(sim1000_30_10V_lowINTER_3c_L5, t0 = t0)
#save_jm_result(result_JM_sim1000_30_10V_lowINTER_3c_L5, "result_JM_sim1000_30_10V_lowINTER_3c_L5")
result_JM_sim1000_70_10V_lowINTER_3c_L1 <- circle_cal_3(sim1000_70_10V_lowINTER_3c_L1, t0 = t0)
save_jm_result(result_JM_sim1000_70_10V_lowINTER_3c_L1, "result_JM_sim1000_70_10V_lowINTER_3c_L1")
#result_JM_sim1000_70_10V_lowINTER_3c_L2 <- circle_cal_3(sim1000_70_10V_lowINTER_3c_L2, t0 = t0)
#save_jm_result(result_JM_sim1000_70_10V_lowINTER_3c_L2, "result_JM_sim1000_70_10V_lowINTER_3c_L2")
#result_JM_sim1000_70_10V_lowINTER_3c_L3 <- circle_cal_3(sim1000_70_10V_lowINTER_3c_L3, t0 = t0)
#save_jm_result(result_JM_sim1000_70_10V_lowINTER_3c_L3, "result_JM_sim1000_70_10V_lowINTER_3c_L3")
#result_JM_sim1000_70_10V_lowINTER_3c_L4 <- circle_cal_3(sim1000_70_10V_lowINTER_3c_L4, t0 = t0)
#save_jm_result(result_JM_sim1000_70_10V_lowINTER_3c_L4, "result_JM_sim1000_70_10V_lowINTER_3c_L4")
#result_JM_sim1000_70_10V_lowINTER_3c_L5 <- circle_cal_3(sim1000_70_10V_lowINTER_3c_L5, t0 = t0)
#save_jm_result(result_JM_sim1000_70_10V_lowINTER_3c_L5, "result_JM_sim1000_70_10V_lowINTER_3c_L5")

message("【中相关性】测试集 JM 评估...")
result_JM_sim500_30_10V_midINTER_3c_L1  <- circle_cal_3(sim500_30_10V_midINTER_3c_L1,  t0 = t0)
save_jm_result(result_JM_sim500_30_10V_midINTER_3c_L1, "result_JM_sim500_30_10V_midINTER_3c_L1")
#result_JM_sim500_30_10V_midINTER_3c_L2  <- circle_cal_3(sim500_30_10V_midINTER_3c_L2,  t0 = t0)
#save_jm_result(result_JM_sim500_30_10V_midINTER_3c_L2, "result_JM_sim500_30_10V_midINTER_3c_L2")
#result_JM_sim500_30_10V_midINTER_3c_L3  <- circle_cal_3(sim500_30_10V_midINTER_3c_L3,  t0 = t0)
#save_jm_result(result_JM_sim500_30_10V_midINTER_3c_L3, "result_JM_sim500_30_10V_midINTER_3c_L3")
#result_JM_sim500_30_10V_midINTER_3c_L4  <- circle_cal_3(sim500_30_10V_midINTER_3c_L4,  t0 = t0)
#save_jm_result(result_JM_sim500_30_10V_midINTER_3c_L4, "result_JM_sim500_30_10V_midINTER_3c_L4")
#result_JM_sim500_30_10V_midINTER_3c_L5  <- circle_cal_3(sim500_30_10V_midINTER_3c_L5,  t0 = t0)
#save_jm_result(result_JM_sim500_30_10V_midINTER_3c_L5, "result_JM_sim500_30_10V_midINTER_3c_L5")
result_JM_sim500_70_10V_midINTER_3c_L1  <- circle_cal_3(sim500_70_10V_midINTER_3c_L1,  t0 = t0)
save_jm_result(result_JM_sim500_70_10V_midINTER_3c_L1, "result_JM_sim500_70_10V_midINTER_3c_L1")
#result_JM_sim500_70_10V_midINTER_3c_L2  <- circle_cal_3(sim500_70_10V_midINTER_3c_L2,  t0 = t0)
#save_jm_result(result_JM_sim500_70_10V_midINTER_3c_L2, "result_JM_sim500_70_10V_midINTER_3c_L2")
#result_JM_sim500_70_10V_midINTER_3c_L3  <- circle_cal_3(sim500_70_10V_midINTER_3c_L3,  t0 = t0)
#save_jm_result(result_JM_sim500_70_10V_midINTER_3c_L3, "result_JM_sim500_70_10V_midINTER_3c_L3")
#result_JM_sim500_70_10V_midINTER_3c_L4  <- circle_cal_3(sim500_70_10V_midINTER_3c_L4,  t0 = t0)
#save_jm_result(result_JM_sim500_70_10V_midINTER_3c_L4, "result_JM_sim500_70_10V_midINTER_3c_L4")
#result_JM_sim500_70_10V_midINTER_3c_L5  <- circle_cal_3(sim500_70_10V_midINTER_3c_L5,  t0 = t0)
#save_jm_result(result_JM_sim500_70_10V_midINTER_3c_L5, "result_JM_sim500_70_10V_midINTER_3c_L5")
result_JM_sim1000_30_10V_midINTER_3c_L1 <- circle_cal_3(sim1000_30_10V_midINTER_3c_L1, t0 = t0)
save_jm_result(result_JM_sim1000_30_10V_midINTER_3c_L1, "result_JM_sim1000_30_10V_midINTER_3c_L1")
#result_JM_sim1000_30_10V_midINTER_3c_L2 <- circle_cal_3(sim1000_30_10V_midINTER_3c_L2, t0 = t0)
#save_jm_result(result_JM_sim1000_30_10V_midINTER_3c_L2, "result_JM_sim1000_30_10V_midINTER_3c_L2")
#result_JM_sim1000_30_10V_midINTER_3c_L3 <- circle_cal_3(sim1000_30_10V_midINTER_3c_L3, t0 = t0)
#save_jm_result(result_JM_sim1000_30_10V_midINTER_3c_L3, "result_JM_sim1000_30_10V_midINTER_3c_L3")
#result_JM_sim1000_30_10V_midINTER_3c_L4 <- circle_cal_3(sim1000_30_10V_midINTER_3c_L4, t0 = t0)
#save_jm_result(result_JM_sim1000_30_10V_midINTER_3c_L4, "result_JM_sim1000_30_10V_midINTER_3c_L4")
#result_JM_sim1000_30_10V_midINTER_3c_L5 <- circle_cal_3(sim1000_30_10V_midINTER_3c_L5, t0 = t0)
#save_jm_result(result_JM_sim1000_30_10V_midINTER_3c_L5, "result_JM_sim1000_30_10V_midINTER_3c_L5")
result_JM_sim1000_70_10V_midINTER_3c_L1 <- circle_cal_3(sim1000_70_10V_midINTER_3c_L1, t0 = t0)
save_jm_result(result_JM_sim1000_70_10V_midINTER_3c_L1, "result_JM_sim1000_70_10V_midINTER_3c_L1")
#result_JM_sim1000_70_10V_midINTER_3c_L2 <- circle_cal_3(sim1000_70_10V_midINTER_3c_L2, t0 = t0)
#save_jm_result(result_JM_sim1000_70_10V_midINTER_3c_L2, "result_JM_sim1000_70_10V_midINTER_3c_L2")
#result_JM_sim1000_70_10V_midINTER_3c_L3 <- circle_cal_3(sim1000_70_10V_midINTER_3c_L3, t0 = t0)
#save_jm_result(result_JM_sim1000_70_10V_midINTER_3c_L3, "result_JM_sim1000_70_10V_midINTER_3c_L3")
#result_JM_sim1000_70_10V_midINTER_3c_L4 <- circle_cal_3(sim1000_70_10V_midINTER_3c_L4, t0 = t0)
#save_jm_result(result_JM_sim1000_70_10V_midINTER_3c_L4, "result_JM_sim1000_70_10V_midINTER_3c_L4")
#result_JM_sim1000_70_10V_midINTER_3c_L5 <- circle_cal_3(sim1000_70_10V_midINTER_3c_L5, t0 = t0)
#save_jm_result(result_JM_sim1000_70_10V_midINTER_3c_L5, "result_JM_sim1000_70_10V_midINTER_3c_L5")

message("【高相关性】测试集 JM 评估...")
result_JM_sim500_30_10V_highINTER_3c_L1  <- circle_cal_3(sim500_30_10V_highINTER_3c_L1,  t0 = t0)
save_jm_result(result_JM_sim500_30_10V_highINTER_3c_L1, "result_JM_sim500_30_10V_highINTER_3c_L1")
#result_JM_sim500_30_10V_highINTER_3c_L2  <- circle_cal_3(sim500_30_10V_highINTER_3c_L2,  t0 = t0)
#save_jm_result(result_JM_sim500_30_10V_highINTER_3c_L2, "result_JM_sim500_30_10V_highINTER_3c_L2")
#result_JM_sim500_30_10V_highINTER_3c_L3  <- circle_cal_3(sim500_30_10V_highINTER_3c_L3,  t0 = t0)
#save_jm_result(result_JM_sim500_30_10V_highINTER_3c_L3, "result_JM_sim500_30_10V_highINTER_3c_L3")
#result_JM_sim500_30_10V_highINTER_3c_L4  <- circle_cal_3(sim500_30_10V_highINTER_3c_L4,  t0 = t0)
#save_jm_result(result_JM_sim500_30_10V_highINTER_3c_L4, "result_JM_sim500_30_10V_highINTER_3c_L4")
#result_JM_sim500_30_10V_highINTER_3c_L5  <- circle_cal_3(sim500_30_10V_highINTER_3c_L5,  t0 = t0)
#save_jm_result(result_JM_sim500_30_10V_highINTER_3c_L5, "result_JM_sim500_30_10V_highINTER_3c_L5")
result_JM_sim500_70_10V_highINTER_3c_L1  <- circle_cal_3(sim500_70_10V_highINTER_3c_L1,  t0 = t0)
save_jm_result(result_JM_sim500_70_10V_highINTER_3c_L1, "result_JM_sim500_70_10V_highINTER_3c_L1")
#result_JM_sim500_70_10V_highINTER_3c_L2  <- circle_cal_3(sim500_70_10V_highINTER_3c_L2,  t0 = t0)
#save_jm_result(result_JM_sim500_70_10V_highINTER_3c_L2, "result_JM_sim500_70_10V_highINTER_3c_L2")
#result_JM_sim500_70_10V_highINTER_3c_L3  <- circle_cal_3(sim500_70_10V_highINTER_3c_L3,  t0 = t0)
#save_jm_result(result_JM_sim500_70_10V_highINTER_3c_L3, "result_JM_sim500_70_10V_highINTER_3c_L3")
#result_JM_sim500_70_10V_highINTER_3c_L4  <- circle_cal_3(sim500_70_10V_highINTER_3c_L4,  t0 = t0)
#save_jm_result(result_JM_sim500_70_10V_highINTER_3c_L4, "result_JM_sim500_70_10V_highINTER_3c_L4")
#result_JM_sim500_70_10V_highINTER_3c_L5  <- circle_cal_3(sim500_70_10V_highINTER_3c_L5,  t0 = t0)
#save_jm_result(result_JM_sim500_70_10V_highINTER_3c_L5, "result_JM_sim500_70_10V_highINTER_3c_L5")
result_JM_sim1000_30_10V_highINTER_3c_L1 <- circle_cal_3(sim1000_30_10V_highINTER_3c_L1, t0 = t0)
save_jm_result(result_JM_sim1000_30_10V_highINTER_3c_L1, "result_JM_sim1000_30_10V_highINTER_3c_L1")
#result_JM_sim1000_30_10V_highINTER_3c_L2 <- circle_cal_3(sim1000_30_10V_highINTER_3c_L2, t0 = t0)
#save_jm_result(result_JM_sim1000_30_10V_highINTER_3c_L2, "result_JM_sim1000_30_10V_highINTER_3c_L2")
#result_JM_sim1000_30_10V_highINTER_3c_L3 <- circle_cal_3(sim1000_30_10V_highINTER_3c_L3, t0 = t0)
#save_jm_result(result_JM_sim1000_30_10V_highINTER_3c_L3, "result_JM_sim1000_30_10V_highINTER_3c_L3")
#result_JM_sim1000_30_10V_highINTER_3c_L4 <- circle_cal_3(sim1000_30_10V_highINTER_3c_L4, t0 = t0)
#save_jm_result(result_JM_sim1000_30_10V_highINTER_3c_L4, "result_JM_sim1000_30_10V_highINTER_3c_L4")
#result_JM_sim1000_30_10V_highINTER_3c_L5 <- circle_cal_3(sim1000_30_10V_highINTER_3c_L5, t0 = t0)
#save_jm_result(result_JM_sim1000_30_10V_highINTER_3c_L5, "result_JM_sim1000_30_10V_highINTER_3c_L5")
result_JM_sim1000_70_10V_highINTER_3c_L1 <- circle_cal_3(sim1000_70_10V_highINTER_3c_L1, t0 = t0)
save_jm_result(result_JM_sim1000_70_10V_highINTER_3c_L1, "result_JM_sim1000_70_10V_highINTER_3c_L1")
#result_JM_sim1000_70_10V_highINTER_3c_L2 <- circle_cal_3(sim1000_70_10V_highINTER_3c_L2, t0 = t0)
#save_jm_result(result_JM_sim1000_70_10V_highINTER_3c_L2, "result_JM_sim1000_70_10V_highINTER_3c_L2")
#result_JM_sim1000_70_10V_highINTER_3c_L3 <- circle_cal_3(sim1000_70_10V_highINTER_3c_L3, t0 = t0)
#save_jm_result(result_JM_sim1000_70_10V_highINTER_3c_L3, "result_JM_sim1000_70_10V_highINTER_3c_L3")
#result_JM_sim1000_70_10V_highINTER_3c_L4 <- circle_cal_3(sim1000_70_10V_highINTER_3c_L4, t0 = t0)
#save_jm_result(result_JM_sim1000_70_10V_highINTER_3c_L4, "result_JM_sim1000_70_10V_highINTER_3c_L4")
#result_JM_sim1000_70_10V_highINTER_3c_L5 <- circle_cal_3(sim1000_70_10V_highINTER_3c_L5, t0 = t0)
#save_jm_result(result_JM_sim1000_70_10V_highINTER_3c_L5, "result_JM_sim1000_70_10V_highINTER_3c_L5")

message("【低相关性-BTW】测试集 JM 评估...")
result_JM_sim500_30_10V_lowBTW_3c_L1  <- circle_cal_3(sim500_30_10V_lowBTW_3c_L1,  t0 = t0)
save_jm_result(result_JM_sim500_30_10V_lowBTW_3c_L1, "result_JM_sim500_30_10V_lowBTW_3c_L1")
#result_JM_sim500_30_10V_lowBTW_3c_L2  <- circle_cal_3(sim500_30_10V_lowBTW_3c_L2,  t0 = t0)
#save_jm_result(result_JM_sim500_30_10V_lowBTW_3c_L2, "result_JM_sim500_30_10V_lowBTW_3c_L2")
#result_JM_sim500_30_10V_lowBTW_3c_L3  <- circle_cal_3(sim500_30_10V_lowBTW_3c_L3,  t0 = t0)
#save_jm_result(result_JM_sim500_30_10V_lowBTW_3c_L3, "result_JM_sim500_30_10V_lowBTW_3c_L3")
#result_JM_sim500_30_10V_lowBTW_3c_L4  <- circle_cal_3(sim500_30_10V_lowBTW_3c_L4,  t0 = t0)
#save_jm_result(result_JM_sim500_30_10V_lowBTW_3c_L4, "result_JM_sim500_30_10V_lowBTW_3c_L4")
#result_JM_sim500_30_10V_lowBTW_3c_L5  <- circle_cal_3(sim500_30_10V_lowBTW_3c_L5,  t0 = t0)
#save_jm_result(result_JM_sim500_30_10V_lowBTW_3c_L5, "result_JM_sim500_30_10V_lowBTW_3c_L5")
result_JM_sim500_70_10V_lowBTW_3c_L1  <- circle_cal_3(sim500_70_10V_lowBTW_3c_L1,  t0 = t0)
save_jm_result(result_JM_sim500_70_10V_lowBTW_3c_L1, "result_JM_sim500_70_10V_lowBTW_3c_L1")
#result_JM_sim500_70_10V_lowBTW_3c_L2  <- circle_cal_3(sim500_70_10V_lowBTW_3c_L2,  t0 = t0)
#save_jm_result(result_JM_sim500_70_10V_lowBTW_3c_L2, "result_JM_sim500_70_10V_lowBTW_3c_L2")
#result_JM_sim500_70_10V_lowBTW_3c_L3  <- circle_cal_3(sim500_70_10V_lowBTW_3c_L3,  t0 = t0)
#save_jm_result(result_JM_sim500_70_10V_lowBTW_3c_L3, "result_JM_sim500_70_10V_lowBTW_3c_L3")
#result_JM_sim500_70_10V_lowBTW_3c_L4  <- circle_cal_3(sim500_70_10V_lowBTW_3c_L4,  t0 = t0)
#save_jm_result(result_JM_sim500_70_10V_lowBTW_3c_L4, "result_JM_sim500_70_10V_lowBTW_3c_L4")
#result_JM_sim500_70_10V_lowBTW_3c_L5  <- circle_cal_3(sim500_70_10V_lowBTW_3c_L5,  t0 = t0)
#save_jm_result(result_JM_sim500_70_10V_lowBTW_3c_L5, "result_JM_sim500_70_10V_lowBTW_3c_L5")
result_JM_sim1000_30_10V_lowBTW_3c_L1 <- circle_cal_3(sim1000_30_10V_lowBTW_3c_L1, t0 = t0)
save_jm_result(result_JM_sim1000_30_10V_lowBTW_3c_L1, "result_JM_sim1000_30_10V_lowBTW_3c_L1")
#result_JM_sim1000_30_10V_lowBTW_3c_L2 <- circle_cal_3(sim1000_30_10V_lowBTW_3c_L2, t0 = t0)
#save_jm_result(result_JM_sim1000_30_10V_lowBTW_3c_L2, "result_JM_sim1000_30_10V_lowBTW_3c_L2")
#result_JM_sim1000_30_10V_lowBTW_3c_L3 <- circle_cal_3(sim1000_30_10V_lowBTW_3c_L3, t0 = t0)
#save_jm_result(result_JM_sim1000_30_10V_lowBTW_3c_L3, "result_JM_sim1000_30_10V_lowBTW_3c_L3")
#result_JM_sim1000_30_10V_lowBTW_3c_L4 <- circle_cal_3(sim1000_30_10V_lowBTW_3c_L4, t0 = t0)
#save_jm_result(result_JM_sim1000_30_10V_lowBTW_3c_L4, "result_JM_sim1000_30_10V_lowBTW_3c_L4")
#result_JM_sim1000_30_10V_lowBTW_3c_L5 <- circle_cal_3(sim1000_30_10V_lowBTW_3c_L5, t0 = t0)
#save_jm_result(result_JM_sim1000_30_10V_lowBTW_3c_L5, "result_JM_sim1000_30_10V_lowBTW_3c_L5")
result_JM_sim1000_70_10V_lowBTW_3c_L1 <- circle_cal_3(sim1000_70_10V_lowBTW_3c_L1, t0 = t0)
save_jm_result(result_JM_sim1000_70_10V_lowBTW_3c_L1, "result_JM_sim1000_70_10V_lowBTW_3c_L1")
#result_JM_sim1000_70_10V_lowBTW_3c_L2 <- circle_cal_3(sim1000_70_10V_lowBTW_3c_L2, t0 = t0)
#save_jm_result(result_JM_sim1000_70_10V_lowBTW_3c_L2, "result_JM_sim1000_70_10V_lowBTW_3c_L2")
#result_JM_sim1000_70_10V_lowBTW_3c_L3 <- circle_cal_3(sim1000_70_10V_lowBTW_3c_L3, t0 = t0)
#save_jm_result(result_JM_sim1000_70_10V_lowBTW_3c_L3, "result_JM_sim1000_70_10V_lowBTW_3c_L3")
#result_JM_sim1000_70_10V_lowBTW_3c_L4 <- circle_cal_3(sim1000_70_10V_lowBTW_3c_L4, t0 = t0)
#save_jm_result(result_JM_sim1000_70_10V_lowBTW_3c_L4, "result_JM_sim1000_70_10V_lowBTW_3c_L4")
#result_JM_sim1000_70_10V_lowBTW_3c_L5 <- circle_cal_3(sim1000_70_10V_lowBTW_3c_L5, t0 = t0)
#save_jm_result(result_JM_sim1000_70_10V_lowBTW_3c_L5, "result_JM_sim1000_70_10V_lowBTW_3c_L5")

message("【中相关性-BTW】测试集 JM 评估...")
result_JM_sim500_30_10V_midBTW_3c_L1  <- circle_cal_3(sim500_30_10V_midBTW_3c_L1,  t0 = t0)
save_jm_result(result_JM_sim500_30_10V_midBTW_3c_L1, "result_JM_sim500_30_10V_midBTW_3c_L1")
#result_JM_sim500_30_10V_midBTW_3c_L2  <- circle_cal_3(sim500_30_10V_midBTW_3c_L2,  t0 = t0)
#save_jm_result(result_JM_sim500_30_10V_midBTW_3c_L2, "result_JM_sim500_30_10V_midBTW_3c_L2")
#result_JM_sim500_30_10V_midBTW_3c_L3  <- circle_cal_3(sim500_30_10V_midBTW_3c_L3,  t0 = t0)
#save_jm_result(result_JM_sim500_30_10V_midBTW_3c_L3, "result_JM_sim500_30_10V_midBTW_3c_L3")
#result_JM_sim500_30_10V_midBTW_3c_L4  <- circle_cal_3(sim500_30_10V_midBTW_3c_L4,  t0 = t0)
#save_jm_result(result_JM_sim500_30_10V_midBTW_3c_L4, "result_JM_sim500_30_10V_midBTW_3c_L4")
#result_JM_sim500_30_10V_midBTW_3c_L5  <- circle_cal_3(sim500_30_10V_midBTW_3c_L5,  t0 = t0)
#save_jm_result(result_JM_sim500_30_10V_midBTW_3c_L5, "result_JM_sim500_30_10V_midBTW_3c_L5")
result_JM_sim500_70_10V_midBTW_3c_L1  <- circle_cal_3(sim500_70_10V_midBTW_3c_L1,  t0 = t0)
save_jm_result(result_JM_sim500_70_10V_midBTW_3c_L1, "result_JM_sim500_70_10V_midBTW_3c_L1")
#result_JM_sim500_70_10V_midBTW_3c_L2  <- circle_cal_3(sim500_70_10V_midBTW_3c_L2,  t0 = t0)
#save_jm_result(result_JM_sim500_70_10V_midBTW_3c_L2, "result_JM_sim500_70_10V_midBTW_3c_L2")
#result_JM_sim500_70_10V_midBTW_3c_L3  <- circle_cal_3(sim500_70_10V_midBTW_3c_L3,  t0 = t0)
#save_jm_result(result_JM_sim500_70_10V_midBTW_3c_L3, "result_JM_sim500_70_10V_midBTW_3c_L3")
#result_JM_sim500_70_10V_midBTW_3c_L4  <- circle_cal_3(sim500_70_10V_midBTW_3c_L4,  t0 = t0)
#save_jm_result(result_JM_sim500_70_10V_midBTW_3c_L4, "result_JM_sim500_70_10V_midBTW_3c_L4")
#result_JM_sim500_70_10V_midBTW_3c_L5  <- circle_cal_3(sim500_70_10V_midBTW_3c_L5,  t0 = t0)
#save_jm_result(result_JM_sim500_70_10V_midBTW_3c_L5, "result_JM_sim500_70_10V_midBTW_3c_L5")
result_JM_sim1000_30_10V_midBTW_3c_L1 <- circle_cal_3(sim1000_30_10V_midBTW_3c_L1, t0 = t0)
save_jm_result(result_JM_sim1000_30_10V_midBTW_3c_L1, "result_JM_sim1000_30_10V_midBTW_3c_L1")
#result_JM_sim1000_30_10V_midBTW_3c_L2 <- circle_cal_3(sim1000_30_10V_midBTW_3c_L2, t0 = t0)
#save_jm_result(result_JM_sim1000_30_10V_midBTW_3c_L2, "result_JM_sim1000_30_10V_midBTW_3c_L2")
#result_JM_sim1000_30_10V_midBTW_3c_L3 <- circle_cal_3(sim1000_30_10V_midBTW_3c_L3, t0 = t0)
#save_jm_result(result_JM_sim1000_30_10V_midBTW_3c_L3, "result_JM_sim1000_30_10V_midBTW_3c_L3")
#result_JM_sim1000_30_10V_midBTW_3c_L4 <- circle_cal_3(sim1000_30_10V_midBTW_3c_L4, t0 = t0)
#save_jm_result(result_JM_sim1000_30_10V_midBTW_3c_L4, "result_JM_sim1000_30_10V_midBTW_3c_L4")
#result_JM_sim1000_30_10V_midBTW_3c_L5 <- circle_cal_3(sim1000_30_10V_midBTW_3c_L5, t0 = t0)
#save_jm_result(result_JM_sim1000_30_10V_midBTW_3c_L5, "result_JM_sim1000_30_10V_midBTW_3c_L5")
result_JM_sim1000_70_10V_midBTW_3c_L1 <- circle_cal_3(sim1000_70_10V_midBTW_3c_L1, t0 = t0)
save_jm_result(result_JM_sim1000_70_10V_midBTW_3c_L1, "result_JM_sim1000_70_10V_midBTW_3c_L1")
#result_JM_sim1000_70_10V_midBTW_3c_L2 <- circle_cal_3(sim1000_70_10V_midBTW_3c_L2, t0 = t0)
#save_jm_result(result_JM_sim1000_70_10V_midBTW_3c_L2, "result_JM_sim1000_70_10V_midBTW_3c_L2")
#result_JM_sim1000_70_10V_midBTW_3c_L3 <- circle_cal_3(sim1000_70_10V_midBTW_3c_L3, t0 = t0)
#save_jm_result(result_JM_sim1000_70_10V_midBTW_3c_L3, "result_JM_sim1000_70_10V_midBTW_3c_L3")
#result_JM_sim1000_70_10V_midBTW_3c_L4 <- circle_cal_3(sim1000_70_10V_midBTW_3c_L4, t0 = t0)
#save_jm_result(result_JM_sim1000_70_10V_midBTW_3c_L4, "result_JM_sim1000_70_10V_midBTW_3c_L4")
#result_JM_sim1000_70_10V_midBTW_3c_L5 <- circle_cal_3(sim1000_70_10V_midBTW_3c_L5, t0 = t0)
#save_jm_result(result_JM_sim1000_70_10V_midBTW_3c_L5, "result_JM_sim1000_70_10V_midBTW_3c_L5")

message("【高相关性-BTW】测试集 JM 评估...")
result_JM_sim500_30_10V_highBTW_3c_L1  <- circle_cal_3(sim500_30_10V_highBTW_3c_L1,  t0 = t0)
save_jm_result(result_JM_sim500_30_10V_highBTW_3c_L1, "result_JM_sim500_30_10V_highBTW_3c_L1")
#result_JM_sim500_30_10V_highBTW_3c_L2  <- circle_cal_3(sim500_30_10V_highBTW_3c_L2,  t0 = t0)
#save_jm_result(result_JM_sim500_30_10V_highBTW_3c_L2, "result_JM_sim500_30_10V_highBTW_3c_L2")
#result_JM_sim500_30_10V_highBTW_3c_L3  <- circle_cal_3(sim500_30_10V_highBTW_3c_L3,  t0 = t0)
#save_jm_result(result_JM_sim500_30_10V_highBTW_3c_L3, "result_JM_sim500_30_10V_highBTW_3c_L3")
#result_JM_sim500_30_10V_highBTW_3c_L4  <- circle_cal_3(sim500_30_10V_highBTW_3c_L4,  t0 = t0)
#save_jm_result(result_JM_sim500_30_10V_highBTW_3c_L4, "result_JM_sim500_30_10V_highBTW_3c_L4")
#result_JM_sim500_30_10V_highBTW_3c_L5  <- circle_cal_3(sim500_30_10V_highBTW_3c_L5,  t0 = t0)
#save_jm_result(result_JM_sim500_30_10V_highBTW_3c_L5, "result_JM_sim500_30_10V_highBTW_3c_L5")
result_JM_sim500_70_10V_highBTW_3c_L1  <- circle_cal_3(sim500_70_10V_highBTW_3c_L1,  t0 = t0)
save_jm_result(result_JM_sim500_70_10V_highBTW_3c_L1, "result_JM_sim500_70_10V_highBTW_3c_L1")
#result_JM_sim500_70_10V_highBTW_3c_L2  <- circle_cal_3(sim500_70_10V_highBTW_3c_L2,  t0 = t0)
#save_jm_result(result_JM_sim500_70_10V_highBTW_3c_L2, "result_JM_sim500_70_10V_highBTW_3c_L2")
#result_JM_sim500_70_10V_highBTW_3c_L3  <- circle_cal_3(sim500_70_10V_highBTW_3c_L3,  t0 = t0)
#save_jm_result(result_JM_sim500_70_10V_highBTW_3c_L3, "result_JM_sim500_70_10V_highBTW_3c_L3")
#result_JM_sim500_70_10V_highBTW_3c_L4  <- circle_cal_3(sim500_70_10V_highBTW_3c_L4,  t0 = t0)
#save_jm_result(result_JM_sim500_70_10V_highBTW_3c_L4, "result_JM_sim500_70_10V_highBTW_3c_L4")
#result_JM_sim500_70_10V_highBTW_3c_L5  <- circle_cal_3(sim500_70_10V_highBTW_3c_L5,  t0 = t0)
#save_jm_result(result_JM_sim500_70_10V_highBTW_3c_L5, "result_JM_sim500_70_10V_highBTW_3c_L5")
result_JM_sim1000_30_10V_highBTW_3c_L1 <- circle_cal_3(sim1000_30_10V_highBTW_3c_L1, t0 = t0)
save_jm_result(result_JM_sim1000_30_10V_highBTW_3c_L1, "result_JM_sim1000_30_10V_highBTW_3c_L1")
#result_JM_sim1000_30_10V_highBTW_3c_L2 <- circle_cal_3(sim1000_30_10V_highBTW_3c_L2, t0 = t0)
#save_jm_result(result_JM_sim1000_30_10V_highBTW_3c_L2, "result_JM_sim1000_30_10V_highBTW_3c_L2")
#result_JM_sim1000_30_10V_highBTW_3c_L3 <- circle_cal_3(sim1000_30_10V_highBTW_3c_L3, t0 = t0)
#save_jm_result(result_JM_sim1000_30_10V_highBTW_3c_L3, "result_JM_sim1000_30_10V_highBTW_3c_L3")
#result_JM_sim1000_30_10V_highBTW_3c_L4 <- circle_cal_3(sim1000_30_10V_highBTW_3c_L4, t0 = t0)
#save_jm_result(result_JM_sim1000_30_10V_highBTW_3c_L4, "result_JM_sim1000_30_10V_highBTW_3c_L4")
#result_JM_sim1000_30_10V_highBTW_3c_L5 <- circle_cal_3(sim1000_30_10V_highBTW_3c_L5, t0 = t0)
#save_jm_result(result_JM_sim1000_30_10V_highBTW_3c_L5, "result_JM_sim1000_30_10V_highBTW_3c_L5")
result_JM_sim1000_70_10V_highBTW_3c_L1 <- circle_cal_3(sim1000_70_10V_highBTW_3c_L1, t0 = t0)
save_jm_result(result_JM_sim1000_70_10V_highBTW_3c_L1, "result_JM_sim1000_70_10V_highBTW_3c_L1")
#result_JM_sim1000_70_10V_highBTW_3c_L2 <- circle_cal_3(sim1000_70_10V_highBTW_3c_L2, t0 = t0)
#save_jm_result(result_JM_sim1000_70_10V_highBTW_3c_L2, "result_JM_sim1000_70_10V_highBTW_3c_L2")
#result_JM_sim1000_70_10V_highBTW_3c_L3 <- circle_cal_3(sim1000_70_10V_highBTW_3c_L3, t0 = t0)
#save_jm_result(result_JM_sim1000_70_10V_highBTW_3c_L3, "result_JM_sim1000_70_10V_highBTW_3c_L3")
#result_JM_sim1000_70_10V_highBTW_3c_L4 <- circle_cal_3(sim1000_70_10V_highBTW_3c_L4, t0 = t0)
#save_jm_result(result_JM_sim1000_70_10V_highBTW_3c_L4, "result_JM_sim1000_70_10V_highBTW_3c_L4")
#result_JM_sim1000_70_10V_highBTW_3c_L5 <- circle_cal_3(sim1000_70_10V_highBTW_3c_L5, t0 = t0)
#save_jm_result(result_JM_sim1000_70_10V_highBTW_3c_L5, "result_JM_sim1000_70_10V_highBTW_3c_L5")

########### 补保存：仅写出环境中仍存在的 result_JM*（分段运行时只保存已算出的）#################
message("【补保存/检查】", output_dir, "（已即时保存的可跳过）")

result_nms <- c(
#  "result_JM_sim500_30_10V_lowINTER_3c_L1", "result_JM_sim500_30_10V_lowINTER_3c_L2",
#  "result_JM_sim500_30_10V_lowINTER_3c_L3", "result_JM_sim500_30_10V_lowINTER_3c_L4",
#  "result_JM_sim500_30_10V_lowINTER_3c_L5", "result_JM_sim500_70_10V_lowINTER_3c_L1",
#  "result_JM_sim500_70_10V_lowINTER_3c_L2", "result_JM_sim500_70_10V_lowINTER_3c_L3",
#  "result_JM_sim500_70_10V_lowINTER_3c_L4", "result_JM_sim500_70_10V_lowINTER_3c_L5",
#  "result_JM_sim1000_30_10V_lowINTER_3c_L1", "result_JM_sim1000_30_10V_lowINTER_3c_L2",
#  "result_JM_sim1000_30_10V_lowINTER_3c_L3", "result_JM_sim1000_30_10V_lowINTER_3c_L4",
#  "result_JM_sim1000_30_10V_lowINTER_3c_L5", "result_JM_sim1000_70_10V_lowINTER_3c_L1",
#  "result_JM_sim1000_70_10V_lowINTER_3c_L2", "result_JM_sim1000_70_10V_lowINTER_3c_L3",
#  "result_JM_sim1000_70_10V_lowINTER_3c_L4", "result_JM_sim1000_70_10V_lowINTER_3c_L5",
#  "result_JM_sim500_30_10V_midINTER_3c_L1", "result_JM_sim500_30_10V_midINTER_3c_L2",
#  "result_JM_sim500_30_10V_midINTER_3c_L3", "result_JM_sim500_30_10V_midINTER_3c_L4",
#  "result_JM_sim500_30_10V_midINTER_3c_L5", "result_JM_sim500_70_10V_midINTER_3c_L1",
#  "result_JM_sim500_70_10V_midINTER_3c_L2", "result_JM_sim500_70_10V_midINTER_3c_L3",
#  "result_JM_sim500_70_10V_midINTER_3c_L4", "result_JM_sim500_70_10V_midINTER_3c_L5",
#  "result_JM_sim1000_30_10V_midINTER_3c_L1", "result_JM_sim1000_30_10V_midINTER_3c_L2",
#  "result_JM_sim1000_30_10V_midINTER_3c_L3", "result_JM_sim1000_30_10V_midINTER_3c_L4",
#  "result_JM_sim1000_30_10V_midINTER_3c_L5", "result_JM_sim1000_70_10V_midINTER_3c_L1",
#  "result_JM_sim1000_70_10V_midINTER_3c_L2", "result_JM_sim1000_70_10V_midINTER_3c_L3",
#  "result_JM_sim1000_70_10V_midINTER_3c_L4", "result_JM_sim1000_70_10V_midINTER_3c_L5",
#  "result_JM_sim500_30_10V_highINTER_3c_L1", "result_JM_sim500_30_10V_highINTER_3c_L2",
#  "result_JM_sim500_30_10V_highINTER_3c_L3", "result_JM_sim500_30_10V_highINTER_3c_L4",
#  "result_JM_sim500_30_10V_highINTER_3c_L5", "result_JM_sim500_70_10V_highINTER_3c_L1",
#  "result_JM_sim500_70_10V_highINTER_3c_L2", "result_JM_sim500_70_10V_highINTER_3c_L3",
#  "result_JM_sim500_70_10V_highINTER_3c_L4", "result_JM_sim500_70_10V_highINTER_3c_L5",
#  "result_JM_sim1000_30_10V_highINTER_3c_L1", "result_JM_sim1000_30_10V_highINTER_3c_L2",
#  "result_JM_sim1000_30_10V_highINTER_3c_L3", "result_JM_sim1000_30_10V_highINTER_3c_L4",
#  "result_JM_sim1000_30_10V_highINTER_3c_L5", "result_JM_sim1000_70_10V_highINTER_3c_L1",
#  "result_JM_sim1000_70_10V_highINTER_3c_L2", "result_JM_sim1000_70_10V_highINTER_3c_L3",
#  "result_JM_sim1000_70_10V_highINTER_3c_L4", "result_JM_sim1000_70_10V_highINTER_3c_L5",
#  "result_JM_sim500_30_10V_lowBTW_3c_L1", "result_JM_sim500_30_10V_lowBTW_3c_L2",
#  "result_JM_sim500_30_10V_lowBTW_3c_L3", "result_JM_sim500_30_10V_lowBTW_3c_L4",
#  "result_JM_sim500_30_10V_lowBTW_3c_L5", "result_JM_sim500_70_10V_lowBTW_3c_L1",
#  "result_JM_sim500_70_10V_lowBTW_3c_L2", "result_JM_sim500_70_10V_lowBTW_3c_L3",
#  "result_JM_sim500_70_10V_lowBTW_3c_L4", "result_JM_sim500_70_10V_lowBTW_3c_L5",
#  "result_JM_sim1000_30_10V_lowBTW_3c_L1", "result_JM_sim1000_30_10V_lowBTW_3c_L2",
#  "result_JM_sim1000_30_10V_lowBTW_3c_L3", "result_JM_sim1000_30_10V_lowBTW_3c_L4",
#  "result_JM_sim1000_30_10V_lowBTW_3c_L5", "result_JM_sim1000_70_10V_lowBTW_3c_L1",
#  "result_JM_sim1000_70_10V_lowBTW_3c_L2", "result_JM_sim1000_70_10V_lowBTW_3c_L3",
#  "result_JM_sim1000_70_10V_lowBTW_3c_L4", "result_JM_sim1000_70_10V_lowBTW_3c_L5",
#  "result_JM_sim500_30_10V_midBTW_3c_L1", "result_JM_sim500_30_10V_midBTW_3c_L2",
#  "result_JM_sim500_30_10V_midBTW_3c_L3", "result_JM_sim500_30_10V_midBTW_3c_L4",
#  "result_JM_sim500_30_10V_midBTW_3c_L5", "result_JM_sim500_70_10V_midBTW_3c_L1",
#  "result_JM_sim500_70_10V_midBTW_3c_L2", "result_JM_sim500_70_10V_midBTW_3c_L3",
#  "result_JM_sim500_70_10V_midBTW_3c_L4", "result_JM_sim500_70_10V_midBTW_3c_L5",
#  "result_JM_sim1000_30_10V_midBTW_3c_L1", "result_JM_sim1000_30_10V_midBTW_3c_L2",
#  "result_JM_sim1000_30_10V_midBTW_3c_L3", "result_JM_sim1000_30_10V_midBTW_3c_L4",
#  "result_JM_sim1000_30_10V_midBTW_3c_L5", "result_JM_sim1000_70_10V_midBTW_3c_L1",
#  "result_JM_sim1000_70_10V_midBTW_3c_L2", "result_JM_sim1000_70_10V_midBTW_3c_L3",
#  "result_JM_sim1000_70_10V_midBTW_3c_L4", "result_JM_sim1000_70_10V_midBTW_3c_L5",
#  "result_JM_sim500_30_10V_highBTW_3c_L1", "result_JM_sim500_30_10V_highBTW_3c_L2",
#  "result_JM_sim500_30_10V_highBTW_3c_L3", "result_JM_sim500_30_10V_highBTW_3c_L4",
#  "result_JM_sim500_30_10V_highBTW_3c_L5", "result_JM_sim500_70_10V_highBTW_3c_L1",
#  "result_JM_sim500_70_10V_highBTW_3c_L2", "result_JM_sim500_70_10V_highBTW_3c_L3",
#  "result_JM_sim500_70_10V_highBTW_3c_L4", "result_JM_sim500_70_10V_highBTW_3c_L5",
#  "result_JM_sim1000_30_10V_highBTW_3c_L1", "result_JM_sim1000_30_10V_highBTW_3c_L2",
#  "result_JM_sim1000_30_10V_highBTW_3c_L3", "result_JM_sim1000_30_10V_highBTW_3c_L4",
#  "result_JM_sim1000_30_10V_highBTW_3c_L5", "result_JM_sim1000_70_10V_highBTW_3c_L1",
#  "result_JM_sim1000_70_10V_highBTW_3c_L2", "result_JM_sim1000_70_10V_highBTW_3c_L3",
#  "result_JM_sim1000_70_10V_highBTW_3c_L4", "result_JM_sim1000_70_10V_highBTW_3c_L5"
)

for (nm in result_nms) {
  if (!exists(nm, envir = .GlobalEnv, inherits = FALSE)) {
    message("  跳过（未运行或无对象）: ", nm)
    next
  }
  obj <- get(nm, envir = .GlobalEnv)
  out_path <- file.path(output_dir, paste0(nm, ".xlsx"))
  write_xlsx(obj, out_path)
  message("  已补存: ", out_path)
}

message("✅ 10V3C 测试集 JM 评估完成（INTER + BTW）！结果目录: ", output_dir)
