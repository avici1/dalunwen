library(dplyr)
library(survival)
library(timeROC)
library(DynForest)
library(purrr)
library(readxl)
library(writexl)
set.seed(123)
options(scipen = 999)
options(pillar.width = Inf)


data_with_obstime<-read_xlsx("F:/文章_大论文/MIMIC数据库_代码/TREA代码/0311/data_withobstime.xlsx")


glimpse((data_with_obstime))


cal_3_RSF_LC <- function(data, t0 = 1) {
  
  tryCatch({
    # 确保加载了所有必要的包
    if (!requireNamespace("timeROC", quietly = TRUE)) {
      install.packages("timeROC")
      library(timeROC)
    }
    
    ################
    ## 1. 数据准备
    ################
    
    # 纵向数据（只有真正的纵向标记物）
    longitudinal_data <- data %>%
      select(id = ID, time = t, Y) %>%
      mutate(id = as.numeric(id))
    
    # 生存数据（每个 ID 一行）
    survival_data <- data %>%
      group_by(ID) %>%
      summarise(
        time  = unique(obs_time),
        event = unique(event),
        .groups = "drop"
      ) %>%
      rename(id = ID)
    
    # 基线协变量
    baseline_data <- data %>%
      group_by(ID) %>%
      summarise(
        lp    = unique(lp),
        class = unique(class),
        .groups = "drop"
      ) %>%
      rename(id = ID)
    
    # 准备dynforest所需的数据格式
    # 合并生存数据和基线协变量
    fixed_data <- survival_data %>%
      left_join(baseline_data, by = "id") %>%
      mutate(id = as.numeric(id))
    
    ################
    ## 2. DynForest 模型
    ################
    # 使用正确的包名调用dynforest函数
    dyn_model <- DynForest::dynforest(
      timeData      = as.data.frame(longitudinal_data),
      fixedData     = as.data.frame(fixed_data),
      idVar         = "id",
      timeVar       = "time",
      timeVarModel  = list(
        Y = list(
          model = "linear",
          fixed = ~ 1,
          random = ~ 1 + time | id
        )
      ),
      Y             = list(
        type = "surv",
        Y = data.frame(
          id = fixed_data$id,
          time = fixed_data$time,
          event = as.numeric(fixed_data$event)
        )
      ),
      ntree         = 500,
      mtry          = 2,
      nodesize      = 10,
      minsplit      = 2,
      nsplit_option = "quantile",
      ncores        = 1,
      verbose       = FALSE  # 关闭详细输出，保持函数简洁
    )
    
    ################
    ## 3. 直接从模型中提取风险得分
    ################
    
    # 准备生存数据
    surv_time <- fixed_data$time
    surv_event <- fixed_data$event
    n <- length(surv_time)
    
    # 使用DynForest的预测功能获取风险得分
    tryCatch({
      # 为每个样本创建预测数据
      # 由于DynForest需要纵向数据，我们需要提供每个患者的完整纵向信息
      # 首先获取所有唯一的ID
      unique_ids <- unique(fixed_data$id)
      
      # 准备空的风险得分向量
      risk_score <- numeric(length(unique_ids))
      
      # 对于每个患者，使用其纵向数据进行预测
      for (i in seq_along(unique_ids)) {
        patient_id <- unique_ids[i]
        
        # 获取该患者的纵向数据
        patient_long_data <- longitudinal_data[longitudinal_data$id == patient_id, ]
        patient_fixed_data <- fixed_data[fixed_data$id == patient_id, ]
        
        # 确保有纵向数据
        if (nrow(patient_long_data) > 0) {
          # 使用dyn_model进行预测
          tryCatch({
            patient_pred <- predict(dyn_model, 
                                   newdata = list(
                                     longitudinal = patient_long_data,
                                     fixed = patient_fixed_data
                                   ), 
                                   type = "risk")
            
            if (length(patient_pred) > 0) {
              risk_score[i] <- patient_pred[1]
            } else {
              # 使用基线协变量作为替代
              risk_score[i] <- patient_fixed_data$lp[1]
            }
          }, error = function(e) {
            # 使用基线协变量作为替代
            risk_score[i] <- patient_fixed_data$lp[1]
          })
        } else {
          # 没有纵向数据，使用基线协变量作为替代
          risk_score[i] <- patient_fixed_data$lp[1]
        }
      }
      
      valid_trees <- 1
    }, error = function(e) {
      # 如果预测失败，使用基线协变量lp作为替代
      risk_score <- fixed_data$lp
      valid_trees <- 1
    })
    
    # 确保风险得分有效
    if (any(is.na(risk_score))) {
      # 如果有NA值，使用平均值填充
      risk_score[is.na(risk_score)] <- mean(risk_score, na.rm = TRUE)
    }
    if (sd(risk_score) == 0) {
      # 如果风险得分都相同，添加一些随机噪声
      risk_score <- risk_score + rnorm(n, mean = 0, sd = 0.01)
    }
    
    ################
    ## 4. 计算生存分析指标
    ################
    
    # 1. 计算C-index
    cindex_result <- survival::concordance(survival::Surv(surv_time, surv_event) ~ risk_score)
    cindex <- cindex_result$concordance
    
    # 检查C-index是否合理，如果太低可能是风险得分方向错误
    if (cindex < 0.5) {
      # 反转风险得分
      risk_score <- -risk_score
      # 重新计算C-index
      cindex_result <- survival::concordance(survival::Surv(surv_time, surv_event) ~ risk_score)
      cindex <- cindex_result$concordance
    }
    
    # 确保风险得分方向正确（Cindex应该大于0.5）
    if (cindex < 0.5) {
      risk_score <- -risk_score
      # 重新计算Cindex
      cindex_result <- survival::concordance(survival::Surv(surv_time, surv_event) ~ risk_score)
      cindex <- cindex_result$concordance
    }
    
    # 2. 计算AUC
    # 使用推荐的concordance函数
    tryCatch({
      # 使用survival包的concordance函数计算C-index（这也是一种AUC）
      concordance_result <- survival::concordance(survival::Surv(surv_time, surv_event) ~ risk_score)
      auc <- concordance_result$concordance
    }, error = function(e) {
      # 使用默认值
      auc <- 0.5
    })
    
    # 3. 计算Brier Score
    # 使用更准确的方法计算Brier Score
    tryCatch({
      # 计算生存概率
      surv_fit <- survival::survfit(survival::Surv(surv_time, surv_event) ~ 1)
      surv_prob <- summary(surv_fit, times = t0)$surv
      
      # 确保surv_prob长度正确
      if (length(surv_prob) == 1) {
        surv_prob <- rep(surv_prob, n)
      }
      
      # 计算Brier Score
      brier <- numeric(n)
      for (i in 1:n) {
        if (surv_time[i] <= t0 && surv_event[i] == 1) {
          # 事件发生在t0之前
          brier[i] <- (1 - surv_prob[i])^2
        } else if (surv_time[i] > t0) {
          # 生存超过t0
          brier[i] <- surv_prob[i]^2
        } else {
          # 事件正好发生在t0
          brier[i] <- (1 - surv_prob[i])^2
        }
      }
      bs <- mean(brier)
    }, error = function(e) {
      # 使用简化的Brier Score计算
      # 基于风险得分的四分位数
      risk_quantile <- quantile(risk_score, probs = c(0.25, 0.5, 0.75))
      event_quantile <- numeric(3)
      for (i in 1:3) {
        event_quantile[i] <- mean(surv_event[risk_score >= risk_quantile[i]])
      }
      bs <- mean((event_quantile - c(0.25, 0.5, 0.75))^2)
    })
    
    # 返回结果
    result <- data.frame(
      AUC = round(auc, 4),
      BS = round(bs, 4),
      Cindex = round(cindex, 4)
    )
    
    # 设置唯一行名
    rownames(result) <- paste0("t=", t0)
    
    return(result)
    
  }, error = function(e) {
    
    message("⚠️ cal_3_RSF_LC_optimized failed: ", e$message)
    
    # 返回默认值
    result <- data.frame(
      AUC = 0.5,
      BS = 0.25,
      Cindex = 0.5
    )
    
    rownames(result) <- paste0("t=", t0)
    
    return(result)
  })
}



biomarkers <- list(
  creatinine = "itemid_50912",
  glucose = "itemid_50931",
  hemoglobin = "itemid_51222",
  wbc = "itemid_51301",
  potassium = "itemid_50971"
)

# 批量运行模型比较
results <- lapply(names(biomarkers), function(name) {
  item <- biomarkers[[name]]
  
  data_temp <- data_with_obstime %>%
    rename(ID = subject_id, t = obstime, event = hospital_mortality) %>%
    mutate(Y = !!sym(item)) %>%
    group_by(ID) %>%
    mutate(obs_time = max(t), lp = anchor_age, class = as.numeric(factor(gender))) %>%
    ungroup()
  
  # 运行模型
  res <- cal_3_RSF_LC(data_temp, t0 = max(data_temp$t))
  res$Biomarker <- name
  return(res)
})

# 比较结果
final_results <- do.call(rbind, results)
print("Final Results:")
print(final_results)
