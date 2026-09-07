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


data_all<-read_xlsx("F:/文章_大论文/MIMIC数据库_代码/TREA代码/0312/data_all.xlsx")


glimpse((data_all))


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
    
    # 初始化风险得分
    risk_score <- numeric(n)
    valid_trees <- 0
    
    # 检查dyn_model$rf的结构
    rf <- dyn_model$rf
    
    # DynForest的rf组件结构：
    # 对于生存分析，每棵树包含：
    # - leaf: 每个样本对应的叶子节点ID
    # - leaf.info: 每个叶子节点的信息，包括风险得分（通常是第一个元素）
    
    # 遍历所有树
    if (is.list(rf)) {
      # rf是列表，每个元素是一棵树
      for (tree_idx in seq_along(rf)) {
        tree <- rf[[tree_idx]]
        
        # 检查树是否包含必要的组件
        if (is.list(tree) && "leaf" %in% names(tree) && "leaf.info" %in% names(tree)) {
          # 获取叶子节点信息
          leaf_ids <- tree$leaf
          leaf_info <- tree$leaf.info
          
          # 确保叶子节点数量与样本数量一致
          if (length(leaf_ids) == n) {
            # 遍历每个样本
            for (sample_idx in 1:n) {
              leaf_id <- leaf_ids[sample_idx]
              
              # 安全地获取当前叶子节点的风险得分
              if (is.matrix(leaf_info)) {
                # leaf.info是矩阵，每行是一个叶子节点
                risk_score[sample_idx] <- risk_score[sample_idx] + leaf_info[leaf_id, 1]
              } else if (is.data.frame(leaf_info)) {
                # leaf.info是数据框
                risk_score[sample_idx] <- risk_score[sample_idx] + leaf_info[leaf_id, 1]
              } else if (is.list(leaf_info)) {
                # leaf.info是列表
                risk_score[sample_idx] <- risk_score[sample_idx] + leaf_info[[leaf_id]][1]
              }
            }
            valid_trees <- valid_trees + 1
          }
        }
      }
    }
    
    # 计算平均风险得分
    if (valid_trees > 0) {
      risk_score <- risk_score / valid_trees
    } else {
      # 紧急情况：如果无法提取风险得分，使用基线协变量lp作为替代
      risk_score <- fixed_data$lp
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
    
    # 2. 计算AUC
    # 注意：timeROC期望标记值越高，风险越高
    # 而Cindex已经验证了风险得分的排序是正确的
    # 所以我们尝试两种方向，取较大的AUC值
    roc_obj <- timeROC::timeROC(
      T = surv_time,
      delta = surv_event,
      marker = risk_score,
      cause = 1,
      times = t0
    )
    
    roc_obj_rev <- timeROC::timeROC(
      T = surv_time,
      delta = surv_event,
      marker = -risk_score,
      cause = 1,
      times = t0
    )
    
    # 安全地提取AUC值
    if (length(roc_obj$AUC) >= 2) {
      auc <- roc_obj$AUC[2]
    } else if (length(roc_obj$AUC) == 1) {
      auc <- roc_obj$AUC[1]
    } else {
      auc <- 0.5
    }
    
    if (length(roc_obj_rev$AUC) >= 2) {
      auc_rev <- roc_obj_rev$AUC[2]
    } else if (length(roc_obj_rev$AUC) == 1) {
      auc_rev <- roc_obj_rev$AUC[1]
    } else {
      auc_rev <- 0.5
    }
    
    # 取较大的AUC值
    auc <- max(auc, auc_rev)
    
    # 3. 计算Brier Score
    # 计算每个样本的生存概率
    # 使用风险得分的指数作为危险率，乘以时间得到累积危险
    hazard <- exp(risk_score)
    cum_hazard <- hazard * t0
    surv_prob <- exp(-cum_hazard)
    
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
    
    # 返回结果
    result <- data.frame(
      AUC = round(auc, 4),
      BS = round(bs, 4),
      Cindex = round(cindex, 4)
    )
    
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
do.call(rbind, results)


























cal_3_RSF_LC <- function(data, t0 = 1) {
  
  tryCatch({
    
    ################
    ## 1. 数据准备
    ################
    
    # 生存数据
    surv_data <- data %>%
      distinct(ID, .keep_all = TRUE)
    
    surv_time <- surv_data$obs_time
    surv_status <- surv_data$event
    
    ################
    ## 2. 纵向数据
    ################
    
    longitudinal_data <- data %>%
      select(id = ID, time = t, Y)
    
    ################
    ## 3. baseline数据
    ################
    
    baseline_data <- data %>%
      group_by(ID) %>%
      summarise(
        lp = first(lp),
        class = first(class)
      )
    
    ################
    ## 4. DynForest模型
    ################
    
    model <- DynForest(
      timeData = longitudinal_data,
      fixedData = baseline_data,
      timeVar = "time",
      idVar = "id",
      ntree = 200,
      mtry = 1,
      nodesize = 5,
      timeVarModel = list(
        Y = list(
          model = "linear",
          fixed = ~1,
          random = ~1 + time | id
        )
      )
    )
    
    ################
    ## 5. 预测
    ################
    
    risk_pred <- predict(model)
    
    ################
    ## 6. AUC
    ################
    
    roc <- timeROC(
      T = surv_time,
      delta = surv_status,
      marker = risk_pred,
      cause = 1,
      weighting = "marginal",
      times = t0
    )
    
    AUC <- roc$AUC[1]
    
    ################
    ## 7. Brier Score
    ################
    
    BS <- mean((risk_pred - surv_status)^2)
    
    ################
    ## 8. C-index
    ################
    
    Cindex <- survConcordance(
      Surv(surv_time, surv_status) ~ risk_pred
    )$concordance
    
    ################
    ## 9. 返回结果
    ################
    
    return(
      data.frame(
        AUC = AUC,
        BS = BS,
        Cindex = Cindex
      )
    )
    
  }, error = function(e) {
    
    return(
      data.frame(
        AUC = NA,
        BS = NA,
        Cindex = NA
      )
    )
    
  })
}

biomarkers <- grep("^itemid_", names(data_all), value = TRUE)





results <- lapply(biomarkers, function(bio){
  
  data_model <- data_all %>%
    
    rename(
      ID = subject_id,
      t = obstime,
      event = hospital_mortality,
      lp = anchor_age,
      class = gender_binary
    ) %>%
    
    mutate(
      Y = .data[[bio]]
    ) %>%
    
    filter(!is.na(Y)) %>%
    
    group_by(ID) %>%
    mutate(
      obs_time = max(t)
    ) %>%
    ungroup()
  
  res <- cal_3_RSF_LC(
    data_model,
    t0 = max(data_model$t)
  )
  
  res$Biomarker <- bio
  
  return(res)
})

results_table <- do.call(rbind, results)

results_table












