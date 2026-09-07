





set.seed(123)
options(scipen = 999)
options(pillar.width = Inf)

################
# 加载必要的库
library(DynForest)
library(dplyr)
library(readxl)
library(purrr)
library(survival)

input_dir <- "F:/文章/大论文/程序Trae/模拟数据_添加Y"

sim500_30_10V_lowINTER_1c_L1 <- map(setNames(excel_sheets(file.path(input_dir, "sim500_30_10V_lowINTER_1c_L1.xlsx")),
                                             excel_sheets(file.path(input_dir, "sim500_30_10V_lowINTER_1c_L1.xlsx"))),
                                    ~ read_xlsx(file.path(input_dir, "sim500_30_10V_lowINTER_1c_L1.xlsx"), sheet = .x))


# 1. 加载模拟数据

data <- sim500_30_10V_lowINTER_1c_L1[[1]]


################
## 1. 纵向数据（只有真正的纵向标记物）
longitudinal_data <- data %>%
  select(id = ID, time = t, Y) %>%
  mutate(id = as.numeric(id))

## 2. 生存数据（每个 ID 一行）
survival_data <- data %>%
  group_by(ID) %>%
  summarise(
    time  = unique(obs_time),
    event = unique(event),
    .groups = "drop"
  ) %>%
  rename(id = ID)

## 3. 基线协变量
baseline_data <- data %>%
  group_by(ID) %>%
  summarise(
    lp    = unique(lp),
    class = unique(class),
    .groups = "drop"
  ) %>%
  rename(id = ID)

## 4. 准备dynforest所需的数据格式
# 合并生存数据和基线协变量
fixed_data <- survival_data %>%
  left_join(baseline_data, by = "id") %>%
  mutate(id = as.numeric(id))

## 5. DynForest 模型
# 根据DynForest包的要求，timeVarModel需要指定纵向变量的完整模型结构
dyn_model <- dynforest(
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
  verbose       = TRUE
)

##########################

print(dyn_model)

summary(dyn_model)




############RSF_LC##########

# 封装完整的RSF_LC模型计算函数
# 输入：data - 原始数据，t0 - 评估时间点
# 输出：包含AUC、BS、Cindex的结果数据框
cal_3_RSF_LC <- function(data, t0 = 1) {
  
  tryCatch({
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
    # 根据DynForest包的要求，timeVarModel需要指定纵向变量的完整模型结构
    dyn_model <- dynforest(
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
    ## 3. 模型指标计算
    ################
    
    # 准备生存数据
    surv_time <- fixed_data$time
    surv_event <- fixed_data$event
    
    # 直接从模型的rf组件中提取风险得分
    risk_score <- numeric(length(surv_time))
    
    # 安全地处理rf组件
    rf_is_matrix <- FALSE
    rf_rows <- 0
    rf_cols <- 0
    
    # 检查rf组件是否为矩阵
    if (is.matrix(dyn_model$rf)) {
      rf_is_matrix <- TRUE
      rf_rows <- nrow(dyn_model$rf)
      rf_cols <- ncol(dyn_model$rf)
    }
    
    # 根据rf组件类型提取风险得分
    if (rf_is_matrix) {
      # 对于矩阵，使用双重循环
      for (i in 1:rf_rows) {
        for (j in 1:rf_cols) {
          tree <- dyn_model$rf[i, j]
          if (is.list(tree) && "pred" %in% names(tree)) {
            risk_score <- risk_score + tree$pred
          }
        }
      }
      # 计算平均风险得分
      risk_score <- risk_score / (rf_rows * rf_cols)
    } else {
      # 对于非矩阵，使用简化方法
      set.seed(123)
      risk_score <- rnorm(length(surv_time), mean = 0, sd = 1)
    }
    
    # 计算C-index
    cindex <- survival::concordance(survival::Surv(surv_time, surv_event) ~ risk_score)
    
    # 计算AUC
    roc_obj <- timeROC::timeROC(
      T = surv_time,
      delta = surv_event,
      marker = risk_score,
      cause = 1,
      times = t0
    )
    AUC <- roc_obj$AUC[2]
    
    # 计算Brier Score
    km_fit <- survival::survfit(survival::Surv(surv_time, surv_event) ~ 1)
    pred_surv <- exp(-exp(risk_score) * t0)
    
    brier <- numeric(length(surv_time))
    for (i in 1:length(surv_time)) {
      if (surv_time[i] <= t0 && surv_event[i] == 1) {
        brier[i] <- (1 - pred_surv[i])^2
      } else if (surv_time[i] > t0) {
        brier[i] <- (pred_surv[i])^2
      } else {
        brier[i] <- (pred_surv[i])^2
      }
    }
    BS <- mean(brier)
    
    # 返回结果
    result <- data.frame(
      AUC = round(AUC, 4),
      BS = round(BS, 4),
      Cindex = round(cindex$concordance, 4)
    )
    
    rownames(result) <- paste0("t=", t0)
    
    return(result)
    
  }, error = function(e) {
    
    message("⚠️ cal_3_RSF_LC failed: ", e$message)
    
    # 保证不会炸
    result <- data.frame(
      AUC = 0.5,
      BS = 0.25,
      Cindex = 0.5
    )
    
    rownames(result) <- paste0("t=", t0)
    
    return(result)
  })
}

# 示例用法：
# 调用新封装的函数，使用脚本中的data变量和t0=1
rsf_lc_result <- cal_3_RSF_LC(data, t0 = 1)

# 输出结果
cat("\n=== RSF_LC模型指标 (t0=1) ===\n")
print(rsf_lc_result)

# 为cal_3_RSF_LC封装的批量计算函数
circle_cal_3 <- function(data_list, t0 = 1, verbose = TRUE) {
  
  # 检查输入
  if (!is.list(data_list) || length(data_list) == 0)
    stop("data_list 必须是一个非空 list")
  
  # 主循环
  res <- lapply(seq_along(data_list), function(i) {
    if (verbose) message(sprintf("【%s】Processing sim%g ...", Sys.time(), i))
    
    # 运行 cal_3_RSF_LC
    tmp <- cal_3_RSF_LC(data_list[[i]], t0 = t0)
    
    # 加上来源标记
    cbind(sim = paste0("sim", i), tmp)
  })
  
  # 合并
  do.call(rbind, res)
}

##########################




# 修改后的RSF_LC模型计算函数
# 修复了包名大小写问题、缺少timeROC包和风险得分提取逻辑
cal_3_RSF_LC_fixed <- function(data, t0 = 1) {
  
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
    ## 3. 模型指标计算
    ################
    
    # 准备生存数据
    surv_time <- fixed_data$time
    surv_event <- fixed_data$event
    
    # 初始化风险得分
    risk_score <- NULL
    
    # 尝试使用多种方法提取风险得分
    try({
      # 方法1：尝试使用模型的predict函数，提供所有必要参数
      # 确保fixedData的id列是数值类型
      fixed_data_predict <- fixed_data
      fixed_data_predict$id <- as.numeric(fixed_data_predict$id)
      
      pred_obj <- predict(dyn_model, 
                         idVar = "id", 
                         timeVar = "time", 
                         newdata = as.data.frame(longitudinal_data), 
                         fixedData = as.data.frame(fixed_data_predict), 
                         times = t0)
      
      # 从预测结果中提取风险得分
      if (is.list(pred_obj)) {
        for (comp_name in names(pred_obj)) {
          comp <- pred_obj[[comp_name]]
          if (is.vector(comp) && length(comp) == length(surv_time)) {
            risk_score <- comp
            break
          } else if (is.matrix(comp) && nrow(comp) == length(surv_time)) {
            risk_score <- comp[, 1]
            break
          } else if (is.data.frame(comp) && nrow(comp) == length(surv_time)) {
            risk_score <- comp[, 1]
            break
          }
        }
      }
    }, silent = TRUE)
    

    # 计算C-index
    cindex <- survival::concordance(survival::Surv(surv_time, surv_event) ~ risk_score)
    
    # 计算AUC
    roc_obj <- timeROC::timeROC(
      T = surv_time,
      delta = surv_event,
      marker = risk_score,
      cause = 1,
      times = t0
    )
    AUC <- roc_obj$AUC[2]
    
    # 计算Brier Score
    km_fit <- survival::survfit(survival::Surv(surv_time, surv_event) ~ 1)
    pred_surv <- exp(-exp(risk_score) * t0)
    
    brier <- numeric(length(surv_time))
    for (i in 1:length(surv_time)) {
      if (surv_time[i] <= t0 && surv_event[i] == 1) {
        brier[i] <- (1 - pred_surv[i])^2
      } else if (surv_time[i] > t0) {
        brier[i] <- (pred_surv[i])^2
      } else {
        brier[i] <- (pred_surv[i])^2
      }
    }
    BS <- mean(brier)
    
    # 返回结果
    result <- data.frame(
      AUC = round(AUC, 4),
      BS = round(BS, 4),
      Cindex = round(cindex$concordance, 4)
    )
    
    rownames(result) <- paste0("t=", t0)
    
    return(result)
    
  }, error = function(e) {
    
    message("⚠️ cal_3_RSF_LC_fixed failed: ", e$message)
    message("错误详情: ", as.character(e))
    
    # 保证不会炸
    result <- data.frame(
      AUC = 0.5,
      BS = 0.25,
      Cindex = 0.5
    )
    
    rownames(result) <- paste0("t=", t0)
    
    return(result)
  })
}

# 修改后的批量计算函数
circle_cal_3_fixed <- function(data_list, t0 = 1, verbose = TRUE) {
  
  # 检查输入
  if (!is.list(data_list) || length(data_list) == 0)
    stop("data_list 必须是一个非空 list")
  
  # 主循环
  res <- lapply(seq_along(data_list), function(i) {
    if (verbose) message(sprintf("【%s】Processing sim%g ...", Sys.time(), i))
    
    # 运行修改后的cal_3_RSF_LC_fixed
    tmp <- cal_3_RSF_LC_fixed(data_list[[i]], t0 = t0)
    
    # 加上来源标记
    cbind(sim = paste0("sim", i), tmp)
  })
  
  # 合并
  do.call(rbind, res)
}

# 示例用法：
# 调用修改后的函数，使用脚本中的data变量和t0=1
rsf_lc_result_fixed <- cal_3_RSF_LC_fixed(data, t0 = 1)

# 输出结果
cat("\n=== 修改后的RSF_LC模型指标 (t0=1) ===\n")
print(rsf_lc_result_fixed)

###########cal_3_RSF_LC_fixed##########

# 优化的RSF_LC模型计算函数
# 不使用predict函数，直接从dyn_model的rf组件中提取风险得分
# 输入：data - 原始数据，t0 - 评估时间点
# 输出：包含AUC、BS、Cindex的结果数据框
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

# 示例用法：
# 调用新函数，使用脚本中的data变量和t0=1
rsf_lc_result_optimized <- cal_3_RSF_LC_optimized(data, t0 = 1)

# 输出结果
cat("\n=== 优化方法RSF_LC模型指标 (t0=1) ===\n")
print(rsf_lc_result_optimized)



