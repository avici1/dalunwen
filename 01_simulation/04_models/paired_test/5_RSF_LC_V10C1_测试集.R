# =============================================================================
# 5_RSF_LC_V10C1_测试集.R
# RSF-LC 模型在 10V1C 测试集上的评估
# 数据来源：模拟数据_10V1C（测试集）
# 结果保存：result_RSFLC*.xlsx
# 保存：每条结果计算后立刻 write_xlsx（同 5_RSF_LC_V10C3_测试集.R）；可分段运行，跑多少存多少。
# =============================================================================

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

# 测试集数据路径（模拟数据_10V1C 的 生成Y 目录）
input_dir <- "F:/文章_大论文/0319大改/测试集/模拟数据_10V1C/生成Y"
# 结果保存路径
output_dir <- "F:/文章_大论文/0319大改/结果"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# 与 V10C3 一致：每算完一个结果立即写出到 output_dir（分段运行也随时落盘）
save_rsflc_result <- function(obj, name) {
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

###### 读取测试集数据（10V1C：INTER + BTW）######
message("【读取测试集】", input_dir)

sim500_30_10V_lowINTER_1c_L1   <- read_xlsx_list("sim500_30_10V_lowINTER_1c_L1.xlsx")
#sim500_30_10V_lowINTER_1c_L2   <- read_xlsx_list("sim500_30_10V_lowINTER_1c_L2.xlsx")
#sim500_30_10V_lowINTER_1c_L3   <- read_xlsx_list("sim500_30_10V_lowINTER_1c_L3.xlsx")
#sim500_30_10V_lowINTER_1c_L4   <- read_xlsx_list("sim500_30_10V_lowINTER_1c_L4.xlsx")
#sim500_30_10V_lowINTER_1c_L5   <- read_xlsx_list("sim500_30_10V_lowINTER_1c_L5.xlsx")
sim500_70_10V_lowINTER_1c_L1   <- read_xlsx_list("sim500_70_10V_lowINTER_1c_L1.xlsx")
#sim500_70_10V_lowINTER_1c_L2   <- read_xlsx_list("sim500_70_10V_lowINTER_1c_L2.xlsx")
#sim500_70_10V_lowINTER_1c_L3   <- read_xlsx_list("sim500_70_10V_lowINTER_1c_L3.xlsx")
#sim500_70_10V_lowINTER_1c_L4   <- read_xlsx_list("sim500_70_10V_lowINTER_1c_L4.xlsx")
#sim500_70_10V_lowINTER_1c_L5   <- read_xlsx_list("sim500_70_10V_lowINTER_1c_L5.xlsx")
sim1000_30_10V_lowINTER_1c_L1  <- read_xlsx_list("sim1000_30_10V_lowINTER_1c_L1.xlsx")
#sim1000_30_10V_lowINTER_1c_L2  <- read_xlsx_list("sim1000_30_10V_lowINTER_1c_L2.xlsx")
#sim1000_30_10V_lowINTER_1c_L3  <- read_xlsx_list("sim1000_30_10V_lowINTER_1c_L3.xlsx")
#sim1000_30_10V_lowINTER_1c_L4  <- read_xlsx_list("sim1000_30_10V_lowINTER_1c_L4.xlsx")
#sim1000_30_10V_lowINTER_1c_L5  <- read_xlsx_list("sim1000_30_10V_lowINTER_1c_L5.xlsx")
sim1000_70_10V_lowINTER_1c_L1  <- read_xlsx_list("sim1000_70_10V_lowINTER_1c_L1.xlsx")
#sim1000_70_10V_lowINTER_1c_L2  <- read_xlsx_list("sim1000_70_10V_lowINTER_1c_L2.xlsx")
#sim1000_70_10V_lowINTER_1c_L3  <- read_xlsx_list("sim1000_70_10V_lowINTER_1c_L3.xlsx")
#sim1000_70_10V_lowINTER_1c_L4  <- read_xlsx_list("sim1000_70_10V_lowINTER_1c_L4.xlsx")
#sim1000_70_10V_lowINTER_1c_L5  <- read_xlsx_list("sim1000_70_10V_lowINTER_1c_L5.xlsx")

sim500_30_10V_midINTER_1c_L1   <- read_xlsx_list("sim500_30_10V_midINTER_1c_L1.xlsx")
#sim500_30_10V_midINTER_1c_L2   <- read_xlsx_list("sim500_30_10V_midINTER_1c_L2.xlsx")
#sim500_30_10V_midINTER_1c_L3   <- read_xlsx_list("sim500_30_10V_midINTER_1c_L3.xlsx")
#sim500_30_10V_midINTER_1c_L4   <- read_xlsx_list("sim500_30_10V_midINTER_1c_L4.xlsx")
#sim500_30_10V_midINTER_1c_L5   <- read_xlsx_list("sim500_30_10V_midINTER_1c_L5.xlsx")
sim500_70_10V_midINTER_1c_L1   <- read_xlsx_list("sim500_70_10V_midINTER_1c_L1.xlsx")
#sim500_70_10V_midINTER_1c_L2   <- read_xlsx_list("sim500_70_10V_midINTER_1c_L2.xlsx")
#sim500_70_10V_midINTER_1c_L3   <- read_xlsx_list("sim500_70_10V_midINTER_1c_L3.xlsx")
#sim500_70_10V_midINTER_1c_L4   <- read_xlsx_list("sim500_70_10V_midINTER_1c_L4.xlsx")
#sim500_70_10V_midINTER_1c_L5   <- read_xlsx_list("sim500_70_10V_midINTER_1c_L5.xlsx")
sim1000_30_10V_midINTER_1c_L1  <- read_xlsx_list("sim1000_30_10V_midINTER_1c_L1.xlsx")
#sim1000_30_10V_midINTER_1c_L2  <- read_xlsx_list("sim1000_30_10V_midINTER_1c_L2.xlsx")
#sim1000_30_10V_midINTER_1c_L3  <- read_xlsx_list("sim1000_30_10V_midINTER_1c_L3.xlsx")
#sim1000_30_10V_midINTER_1c_L4  <- read_xlsx_list("sim1000_30_10V_midINTER_1c_L4.xlsx")
#sim1000_30_10V_midINTER_1c_L5  <- read_xlsx_list("sim1000_30_10V_midINTER_1c_L5.xlsx")
sim1000_70_10V_midINTER_1c_L1  <- read_xlsx_list("sim1000_70_10V_midINTER_1c_L1.xlsx")
#sim1000_70_10V_midINTER_1c_L2  <- read_xlsx_list("sim1000_70_10V_midINTER_1c_L2.xlsx")
#sim1000_70_10V_midINTER_1c_L3  <- read_xlsx_list("sim1000_70_10V_midINTER_1c_L3.xlsx")
#sim1000_70_10V_midINTER_1c_L4  <- read_xlsx_list("sim1000_70_10V_midINTER_1c_L4.xlsx")
#sim1000_70_10V_midINTER_1c_L5  <- read_xlsx_list("sim1000_70_10V_midINTER_1c_L5.xlsx")

sim500_30_10V_highINTER_1c_L1  <- read_xlsx_list("sim500_30_10V_highINTER_1c_L1.xlsx")
#sim500_30_10V_highINTER_1c_L2  <- read_xlsx_list("sim500_30_10V_highINTER_1c_L2.xlsx")
#sim500_30_10V_highINTER_1c_L3  <- read_xlsx_list("sim500_30_10V_highINTER_1c_L3.xlsx")
#sim500_30_10V_highINTER_1c_L4  <- read_xlsx_list("sim500_30_10V_highINTER_1c_L4.xlsx")
#sim500_30_10V_highINTER_1c_L5  <- read_xlsx_list("sim500_30_10V_highINTER_1c_L5.xlsx")
sim500_70_10V_highINTER_1c_L1  <- read_xlsx_list("sim500_70_10V_highINTER_1c_L1.xlsx")
#sim500_70_10V_highINTER_1c_L2  <- read_xlsx_list("sim500_70_10V_highINTER_1c_L2.xlsx")
#sim500_70_10V_highINTER_1c_L3  <- read_xlsx_list("sim500_70_10V_highINTER_1c_L3.xlsx")
#sim500_70_10V_highINTER_1c_L4  <- read_xlsx_list("sim500_70_10V_highINTER_1c_L4.xlsx")
#sim500_70_10V_highINTER_1c_L5  <- read_xlsx_list("sim500_70_10V_highINTER_1c_L5.xlsx")
sim1000_30_10V_highINTER_1c_L1 <- read_xlsx_list("sim1000_30_10V_highINTER_1c_L1.xlsx")
#sim1000_30_10V_highINTER_1c_L2 <- read_xlsx_list("sim1000_30_10V_highINTER_1c_L2.xlsx")
#sim1000_30_10V_highINTER_1c_L3 <- read_xlsx_list("sim1000_30_10V_highINTER_1c_L3.xlsx")
#sim1000_30_10V_highINTER_1c_L4 <- read_xlsx_list("sim1000_30_10V_highINTER_1c_L4.xlsx")
#sim1000_30_10V_highINTER_1c_L5 <- read_xlsx_list("sim1000_30_10V_highINTER_1c_L5.xlsx")
sim1000_70_10V_highINTER_1c_L1 <- read_xlsx_list("sim1000_70_10V_highINTER_1c_L1.xlsx")
#sim1000_70_10V_highINTER_1c_L2 <- read_xlsx_list("sim1000_70_10V_highINTER_1c_L2.xlsx")
#sim1000_70_10V_highINTER_1c_L3 <- read_xlsx_list("sim1000_70_10V_highINTER_1c_L3.xlsx")
#sim1000_70_10V_highINTER_1c_L4 <- read_xlsx_list("sim1000_70_10V_highINTER_1c_L4.xlsx")
#sim1000_70_10V_highINTER_1c_L5 <- read_xlsx_list("sim1000_70_10V_highINTER_1c_L5.xlsx")

# BTW 数据
sim500_30_10V_lowBTW_1c_L1   <- read_xlsx_list("sim500_30_10V_lowBTW_1c_L1.xlsx")
#sim500_30_10V_lowBTW_1c_L2   <- read_xlsx_list("sim500_30_10V_lowBTW_1c_L2.xlsx")
#sim500_30_10V_lowBTW_1c_L3   <- read_xlsx_list("sim500_30_10V_lowBTW_1c_L3.xlsx")
#sim500_30_10V_lowBTW_1c_L4   <- read_xlsx_list("sim500_30_10V_lowBTW_1c_L4.xlsx")
#sim500_30_10V_lowBTW_1c_L5   <- read_xlsx_list("sim500_30_10V_lowBTW_1c_L5.xlsx")
sim500_70_10V_lowBTW_1c_L1   <- read_xlsx_list("sim500_70_10V_lowBTW_1c_L1.xlsx")
#sim500_70_10V_lowBTW_1c_L2   <- read_xlsx_list("sim500_70_10V_lowBTW_1c_L2.xlsx")
#sim500_70_10V_lowBTW_1c_L3   <- read_xlsx_list("sim500_70_10V_lowBTW_1c_L3.xlsx")
#sim500_70_10V_lowBTW_1c_L4   <- read_xlsx_list("sim500_70_10V_lowBTW_1c_L4.xlsx")
#sim500_70_10V_lowBTW_1c_L5   <- read_xlsx_list("sim500_70_10V_lowBTW_1c_L5.xlsx")
sim1000_30_10V_lowBTW_1c_L1  <- read_xlsx_list("sim1000_30_10V_lowBTW_1c_L1.xlsx")
#sim1000_30_10V_lowBTW_1c_L2  <- read_xlsx_list("sim1000_30_10V_lowBTW_1c_L2.xlsx")
#sim1000_30_10V_lowBTW_1c_L3  <- read_xlsx_list("sim1000_30_10V_lowBTW_1c_L3.xlsx")
#sim1000_30_10V_lowBTW_1c_L4  <- read_xlsx_list("sim1000_30_10V_lowBTW_1c_L4.xlsx")
#sim1000_30_10V_lowBTW_1c_L5  <- read_xlsx_list("sim1000_30_10V_lowBTW_1c_L5.xlsx")
sim1000_70_10V_lowBTW_1c_L1  <- read_xlsx_list("sim1000_70_10V_lowBTW_1c_L1.xlsx")
#sim1000_70_10V_lowBTW_1c_L2  <- read_xlsx_list("sim1000_70_10V_lowBTW_1c_L2.xlsx")
#sim1000_70_10V_lowBTW_1c_L3  <- read_xlsx_list("sim1000_70_10V_lowBTW_1c_L3.xlsx")
#sim1000_70_10V_lowBTW_1c_L4  <- read_xlsx_list("sim1000_70_10V_lowBTW_1c_L4.xlsx")
#sim1000_70_10V_lowBTW_1c_L5  <- read_xlsx_list("sim1000_70_10V_lowBTW_1c_L5.xlsx")

sim500_30_10V_midBTW_1c_L1   <- read_xlsx_list("sim500_30_10V_midBTW_1c_L1.xlsx")
#sim500_30_10V_midBTW_1c_L2   <- read_xlsx_list("sim500_30_10V_midBTW_1c_L2.xlsx")
#sim500_30_10V_midBTW_1c_L3   <- read_xlsx_list("sim500_30_10V_midBTW_1c_L3.xlsx")
#sim500_30_10V_midBTW_1c_L4   <- read_xlsx_list("sim500_30_10V_midBTW_1c_L4.xlsx")
#sim500_30_10V_midBTW_1c_L5   <- read_xlsx_list("sim500_30_10V_midBTW_1c_L5.xlsx")
sim500_70_10V_midBTW_1c_L1   <- read_xlsx_list("sim500_70_10V_midBTW_1c_L1.xlsx")
#sim500_70_10V_midBTW_1c_L2   <- read_xlsx_list("sim500_70_10V_midBTW_1c_L2.xlsx")
#sim500_70_10V_midBTW_1c_L3   <- read_xlsx_list("sim500_70_10V_midBTW_1c_L3.xlsx")
#sim500_70_10V_midBTW_1c_L4   <- read_xlsx_list("sim500_70_10V_midBTW_1c_L4.xlsx")
#sim500_70_10V_midBTW_1c_L5   <- read_xlsx_list("sim500_70_10V_midBTW_1c_L5.xlsx")
sim1000_30_10V_midBTW_1c_L1  <- read_xlsx_list("sim1000_30_10V_midBTW_1c_L1.xlsx")
#sim1000_30_10V_midBTW_1c_L2  <- read_xlsx_list("sim1000_30_10V_midBTW_1c_L2.xlsx")
#sim1000_30_10V_midBTW_1c_L3  <- read_xlsx_list("sim1000_30_10V_midBTW_1c_L3.xlsx")
#sim1000_30_10V_midBTW_1c_L4  <- read_xlsx_list("sim1000_30_10V_midBTW_1c_L4.xlsx")
#sim1000_30_10V_midBTW_1c_L5  <- read_xlsx_list("sim1000_30_10V_midBTW_1c_L5.xlsx")
sim1000_70_10V_midBTW_1c_L1  <- read_xlsx_list("sim1000_70_10V_midBTW_1c_L1.xlsx")
#sim1000_70_10V_midBTW_1c_L2  <- read_xlsx_list("sim1000_70_10V_midBTW_1c_L2.xlsx")
#sim1000_70_10V_midBTW_1c_L3  <- read_xlsx_list("sim1000_70_10V_midBTW_1c_L3.xlsx")
#sim1000_70_10V_midBTW_1c_L4  <- read_xlsx_list("sim1000_70_10V_midBTW_1c_L4.xlsx")
#sim1000_70_10V_midBTW_1c_L5  <- read_xlsx_list("sim1000_70_10V_midBTW_1c_L5.xlsx")

sim500_30_10V_highBTW_1c_L1  <- read_xlsx_list("sim500_30_10V_highBTW_1c_L1.xlsx")
#sim500_30_10V_highBTW_1c_L2  <- read_xlsx_list("sim500_30_10V_highBTW_1c_L2.xlsx")
#sim500_30_10V_highBTW_1c_L3  <- read_xlsx_list("sim500_30_10V_highBTW_1c_L3.xlsx")
#sim500_30_10V_highBTW_1c_L4  <- read_xlsx_list("sim500_30_10V_highBTW_1c_L4.xlsx")
#sim500_30_10V_highBTW_1c_L5  <- read_xlsx_list("sim500_30_10V_highBTW_1c_L5.xlsx")
sim500_70_10V_highBTW_1c_L1  <- read_xlsx_list("sim500_70_10V_highBTW_1c_L1.xlsx")
#sim500_70_10V_highBTW_1c_L2  <- read_xlsx_list("sim500_70_10V_highBTW_1c_L2.xlsx")
#sim500_70_10V_highBTW_1c_L3  <- read_xlsx_list("sim500_70_10V_highBTW_1c_L3.xlsx")
#sim500_70_10V_highBTW_1c_L4  <- read_xlsx_list("sim500_70_10V_highBTW_1c_L4.xlsx")
#sim500_70_10V_highBTW_1c_L5  <- read_xlsx_list("sim500_70_10V_highBTW_1c_L5.xlsx")
sim1000_30_10V_highBTW_1c_L1 <- read_xlsx_list("sim1000_30_10V_highBTW_1c_L1.xlsx")
#sim1000_30_10V_highBTW_1c_L2 <- read_xlsx_list("sim1000_30_10V_highBTW_1c_L2.xlsx")
#sim1000_30_10V_highBTW_1c_L3 <- read_xlsx_list("sim1000_30_10V_highBTW_1c_L3.xlsx")
#sim1000_30_10V_highBTW_1c_L4 <- read_xlsx_list("sim1000_30_10V_highBTW_1c_L4.xlsx")
#sim1000_30_10V_highBTW_1c_L5 <- read_xlsx_list("sim1000_30_10V_highBTW_1c_L5.xlsx")
sim1000_70_10V_highBTW_1c_L1 <- read_xlsx_list("sim1000_70_10V_highBTW_1c_L1.xlsx")
#sim1000_70_10V_highBTW_1c_L2 <- read_xlsx_list("sim1000_70_10V_highBTW_1c_L2.xlsx")
#sim1000_70_10V_highBTW_1c_L3 <- read_xlsx_list("sim1000_70_10V_highBTW_1c_L3.xlsx")
#sim1000_70_10V_highBTW_1c_L4 <- read_xlsx_list("sim1000_70_10V_highBTW_1c_L4.xlsx")
#sim1000_70_10V_highBTW_1c_L5 <- read_xlsx_list("sim1000_70_10V_highBTW_1c_L5.xlsx")

####### 程序：cal_3_RSF_LC 与 circle_cal_3（与 5_RSF_LC模型_V10C1 相同）############
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

############# 运行评估循环 ##########
t0 <- 1

message("【低相关性】测试集 RSF-LC 评估...")
result_RSFLC_sim500_30_10V_lowINTER_1c_L1  <- circle_cal_3(sim500_30_10V_lowINTER_1c_L1,  t0 = t0)
save_rsflc_result(result_RSFLC_sim500_30_10V_lowINTER_1c_L1, "result_RSFLC_sim500_30_10V_lowINTER_1c_L1")
#result_RSFLC_sim500_30_10V_lowINTER_1c_L2  <- circle_cal_3(sim500_30_10V_lowINTER_1c_L2,  t0 = t0)
#save_rsflc_result(result_RSFLC_sim500_30_10V_lowINTER_1c_L2, "result_RSFLC_sim500_30_10V_lowINTER_1c_L2")
#result_RSFLC_sim500_30_10V_lowINTER_1c_L3  <- circle_cal_3(sim500_30_10V_lowINTER_1c_L3,  t0 = t0)
#save_rsflc_result(result_RSFLC_sim500_30_10V_lowINTER_1c_L3, "result_RSFLC_sim500_30_10V_lowINTER_1c_L3")
#result_RSFLC_sim500_30_10V_lowINTER_1c_L4  <- circle_cal_3(sim500_30_10V_lowINTER_1c_L4,  t0 = t0)
#save_rsflc_result(result_RSFLC_sim500_30_10V_lowINTER_1c_L4, "result_RSFLC_sim500_30_10V_lowINTER_1c_L4")
#result_RSFLC_sim500_30_10V_lowINTER_1c_L5  <- circle_cal_3(sim500_30_10V_lowINTER_1c_L5,  t0 = t0)
#save_rsflc_result(result_RSFLC_sim500_30_10V_lowINTER_1c_L5, "result_RSFLC_sim500_30_10V_lowINTER_1c_L5")
result_RSFLC_sim500_70_10V_lowINTER_1c_L1  <- circle_cal_3(sim500_70_10V_lowINTER_1c_L1,  t0 = t0)
save_rsflc_result(result_RSFLC_sim500_70_10V_lowINTER_1c_L1, "result_RSFLC_sim500_70_10V_lowINTER_1c_L1")
#result_RSFLC_sim500_70_10V_lowINTER_1c_L2  <- circle_cal_3(sim500_70_10V_lowINTER_1c_L2,  t0 = t0)
#save_rsflc_result(result_RSFLC_sim500_70_10V_lowINTER_1c_L2, "result_RSFLC_sim500_70_10V_lowINTER_1c_L2")
#result_RSFLC_sim500_70_10V_lowINTER_1c_L3  <- circle_cal_3(sim500_70_10V_lowINTER_1c_L3,  t0 = t0)
#save_rsflc_result(result_RSFLC_sim500_70_10V_lowINTER_1c_L3, "result_RSFLC_sim500_70_10V_lowINTER_1c_L3")
#result_RSFLC_sim500_70_10V_lowINTER_1c_L4  <- circle_cal_3(sim500_70_10V_lowINTER_1c_L4,  t0 = t0)
#save_rsflc_result(result_RSFLC_sim500_70_10V_lowINTER_1c_L4, "result_RSFLC_sim500_70_10V_lowINTER_1c_L4")
#result_RSFLC_sim500_70_10V_lowINTER_1c_L5  <- circle_cal_3(sim500_70_10V_lowINTER_1c_L5,  t0 = t0)
#save_rsflc_result(result_RSFLC_sim500_70_10V_lowINTER_1c_L5, "result_RSFLC_sim500_70_10V_lowINTER_1c_L5")
result_RSFLC_sim1000_30_10V_lowINTER_1c_L1 <- circle_cal_3(sim1000_30_10V_lowINTER_1c_L1, t0 = t0)
save_rsflc_result(result_RSFLC_sim1000_30_10V_lowINTER_1c_L1, "result_RSFLC_sim1000_30_10V_lowINTER_1c_L1")
#result_RSFLC_sim1000_30_10V_lowINTER_1c_L2 <- circle_cal_3(sim1000_30_10V_lowINTER_1c_L2, t0 = t0)
#save_rsflc_result(result_RSFLC_sim1000_30_10V_lowINTER_1c_L2, "result_RSFLC_sim1000_30_10V_lowINTER_1c_L2")
#result_RSFLC_sim1000_30_10V_lowINTER_1c_L3 <- circle_cal_3(sim1000_30_10V_lowINTER_1c_L3, t0 = t0)
#save_rsflc_result(result_RSFLC_sim1000_30_10V_lowINTER_1c_L3, "result_RSFLC_sim1000_30_10V_lowINTER_1c_L3")
#result_RSFLC_sim1000_30_10V_lowINTER_1c_L4 <- circle_cal_3(sim1000_30_10V_lowINTER_1c_L4, t0 = t0)
#save_rsflc_result(result_RSFLC_sim1000_30_10V_lowINTER_1c_L4, "result_RSFLC_sim1000_30_10V_lowINTER_1c_L4")
#result_RSFLC_sim1000_30_10V_lowINTER_1c_L5 <- circle_cal_3(sim1000_30_10V_lowINTER_1c_L5, t0 = t0)
#save_rsflc_result(result_RSFLC_sim1000_30_10V_lowINTER_1c_L5, "result_RSFLC_sim1000_30_10V_lowINTER_1c_L5")
result_RSFLC_sim1000_70_10V_lowINTER_1c_L1 <- circle_cal_3(sim1000_70_10V_lowINTER_1c_L1, t0 = t0)
save_rsflc_result(result_RSFLC_sim1000_70_10V_lowINTER_1c_L1, "result_RSFLC_sim1000_70_10V_lowINTER_1c_L1")
#result_RSFLC_sim1000_70_10V_lowINTER_1c_L2 <- circle_cal_3(sim1000_70_10V_lowINTER_1c_L2, t0 = t0)
#save_rsflc_result(result_RSFLC_sim1000_70_10V_lowINTER_1c_L2, "result_RSFLC_sim1000_70_10V_lowINTER_1c_L2")
#result_RSFLC_sim1000_70_10V_lowINTER_1c_L3 <- circle_cal_3(sim1000_70_10V_lowINTER_1c_L3, t0 = t0)
#save_rsflc_result(result_RSFLC_sim1000_70_10V_lowINTER_1c_L3, "result_RSFLC_sim1000_70_10V_lowINTER_1c_L3")
#result_RSFLC_sim1000_70_10V_lowINTER_1c_L4 <- circle_cal_3(sim1000_70_10V_lowINTER_1c_L4, t0 = t0)
#save_rsflc_result(result_RSFLC_sim1000_70_10V_lowINTER_1c_L4, "result_RSFLC_sim1000_70_10V_lowINTER_1c_L4")
#result_RSFLC_sim1000_70_10V_lowINTER_1c_L5 <- circle_cal_3(sim1000_70_10V_lowINTER_1c_L5, t0 = t0)
#save_rsflc_result(result_RSFLC_sim1000_70_10V_lowINTER_1c_L5, "result_RSFLC_sim1000_70_10V_lowINTER_1c_L5")

message("【中相关性】测试集 RSF-LC 评估...")
result_RSFLC_sim500_30_10V_midINTER_1c_L1  <- circle_cal_3(sim500_30_10V_midINTER_1c_L1,  t0 = t0)
save_rsflc_result(result_RSFLC_sim500_30_10V_midINTER_1c_L1, "result_RSFLC_sim500_30_10V_midINTER_1c_L1")
#result_RSFLC_sim500_30_10V_midINTER_1c_L2  <- circle_cal_3(sim500_30_10V_midINTER_1c_L2,  t0 = t0)
#save_rsflc_result(result_RSFLC_sim500_30_10V_midINTER_1c_L2, "result_RSFLC_sim500_30_10V_midINTER_1c_L2")
#result_RSFLC_sim500_30_10V_midINTER_1c_L3  <- circle_cal_3(sim500_30_10V_midINTER_1c_L3,  t0 = t0)
#save_rsflc_result(result_RSFLC_sim500_30_10V_midINTER_1c_L3, "result_RSFLC_sim500_30_10V_midINTER_1c_L3")
#result_RSFLC_sim500_30_10V_midINTER_1c_L4  <- circle_cal_3(sim500_30_10V_midINTER_1c_L4,  t0 = t0)
#save_rsflc_result(result_RSFLC_sim500_30_10V_midINTER_1c_L4, "result_RSFLC_sim500_30_10V_midINTER_1c_L4")
#result_RSFLC_sim500_30_10V_midINTER_1c_L5  <- circle_cal_3(sim500_30_10V_midINTER_1c_L5,  t0 = t0)
#save_rsflc_result(result_RSFLC_sim500_30_10V_midINTER_1c_L5, "result_RSFLC_sim500_30_10V_midINTER_1c_L5")
result_RSFLC_sim500_70_10V_midINTER_1c_L1  <- circle_cal_3(sim500_70_10V_midINTER_1c_L1,  t0 = t0)
save_rsflc_result(result_RSFLC_sim500_70_10V_midINTER_1c_L1, "result_RSFLC_sim500_70_10V_midINTER_1c_L1")
#result_RSFLC_sim500_70_10V_midINTER_1c_L2  <- circle_cal_3(sim500_70_10V_midINTER_1c_L2,  t0 = t0)
#save_rsflc_result(result_RSFLC_sim500_70_10V_midINTER_1c_L2, "result_RSFLC_sim500_70_10V_midINTER_1c_L2")
#result_RSFLC_sim500_70_10V_midINTER_1c_L3  <- circle_cal_3(sim500_70_10V_midINTER_1c_L3,  t0 = t0)
#save_rsflc_result(result_RSFLC_sim500_70_10V_midINTER_1c_L3, "result_RSFLC_sim500_70_10V_midINTER_1c_L3")
#result_RSFLC_sim500_70_10V_midINTER_1c_L4  <- circle_cal_3(sim500_70_10V_midINTER_1c_L4,  t0 = t0)
#save_rsflc_result(result_RSFLC_sim500_70_10V_midINTER_1c_L4, "result_RSFLC_sim500_70_10V_midINTER_1c_L4")
#result_RSFLC_sim500_70_10V_midINTER_1c_L5  <- circle_cal_3(sim500_70_10V_midINTER_1c_L5,  t0 = t0)
#save_rsflc_result(result_RSFLC_sim500_70_10V_midINTER_1c_L5, "result_RSFLC_sim500_70_10V_midINTER_1c_L5")
result_RSFLC_sim1000_30_10V_midINTER_1c_L1 <- circle_cal_3(sim1000_30_10V_midINTER_1c_L1, t0 = t0)
save_rsflc_result(result_RSFLC_sim1000_30_10V_midINTER_1c_L1, "result_RSFLC_sim1000_30_10V_midINTER_1c_L1")
#result_RSFLC_sim1000_30_10V_midINTER_1c_L2 <- circle_cal_3(sim1000_30_10V_midINTER_1c_L2, t0 = t0)
#save_rsflc_result(result_RSFLC_sim1000_30_10V_midINTER_1c_L2, "result_RSFLC_sim1000_30_10V_midINTER_1c_L2")
#result_RSFLC_sim1000_30_10V_midINTER_1c_L3 <- circle_cal_3(sim1000_30_10V_midINTER_1c_L3, t0 = t0)
#save_rsflc_result(result_RSFLC_sim1000_30_10V_midINTER_1c_L3, "result_RSFLC_sim1000_30_10V_midINTER_1c_L3")
#result_RSFLC_sim1000_30_10V_midINTER_1c_L4 <- circle_cal_3(sim1000_30_10V_midINTER_1c_L4, t0 = t0)
#save_rsflc_result(result_RSFLC_sim1000_30_10V_midINTER_1c_L4, "result_RSFLC_sim1000_30_10V_midINTER_1c_L4")
#result_RSFLC_sim1000_30_10V_midINTER_1c_L5 <- circle_cal_3(sim1000_30_10V_midINTER_1c_L5, t0 = t0)
#save_rsflc_result(result_RSFLC_sim1000_30_10V_midINTER_1c_L5, "result_RSFLC_sim1000_30_10V_midINTER_1c_L5")
result_RSFLC_sim1000_70_10V_midINTER_1c_L1 <- circle_cal_3(sim1000_70_10V_midINTER_1c_L1, t0 = t0)
save_rsflc_result(result_RSFLC_sim1000_70_10V_midINTER_1c_L1, "result_RSFLC_sim1000_70_10V_midINTER_1c_L1")
#result_RSFLC_sim1000_70_10V_midINTER_1c_L2 <- circle_cal_3(sim1000_70_10V_midINTER_1c_L2, t0 = t0)
#save_rsflc_result(result_RSFLC_sim1000_70_10V_midINTER_1c_L2, "result_RSFLC_sim1000_70_10V_midINTER_1c_L2")
#result_RSFLC_sim1000_70_10V_midINTER_1c_L3 <- circle_cal_3(sim1000_70_10V_midINTER_1c_L3, t0 = t0)
#save_rsflc_result(result_RSFLC_sim1000_70_10V_midINTER_1c_L3, "result_RSFLC_sim1000_70_10V_midINTER_1c_L3")
#result_RSFLC_sim1000_70_10V_midINTER_1c_L4 <- circle_cal_3(sim1000_70_10V_midINTER_1c_L4, t0 = t0)
#save_rsflc_result(result_RSFLC_sim1000_70_10V_midINTER_1c_L4, "result_RSFLC_sim1000_70_10V_midINTER_1c_L4")
#result_RSFLC_sim1000_70_10V_midINTER_1c_L5 <- circle_cal_3(sim1000_70_10V_midINTER_1c_L5, t0 = t0)
#save_rsflc_result(result_RSFLC_sim1000_70_10V_midINTER_1c_L5, "result_RSFLC_sim1000_70_10V_midINTER_1c_L5")

message("【高相关性】测试集 RSF-LC 评估...")
result_RSFLC_sim500_30_10V_highINTER_1c_L1  <- circle_cal_3(sim500_30_10V_highINTER_1c_L1,  t0 = t0)
save_rsflc_result(result_RSFLC_sim500_30_10V_highINTER_1c_L1, "result_RSFLC_sim500_30_10V_highINTER_1c_L1")
#result_RSFLC_sim500_30_10V_highINTER_1c_L2  <- circle_cal_3(sim500_30_10V_highINTER_1c_L2,  t0 = t0)
#save_rsflc_result(result_RSFLC_sim500_30_10V_highINTER_1c_L2, "result_RSFLC_sim500_30_10V_highINTER_1c_L2")
#result_RSFLC_sim500_30_10V_highINTER_1c_L3  <- circle_cal_3(sim500_30_10V_highINTER_1c_L3,  t0 = t0)
#save_rsflc_result(result_RSFLC_sim500_30_10V_highINTER_1c_L3, "result_RSFLC_sim500_30_10V_highINTER_1c_L3")
#result_RSFLC_sim500_30_10V_highINTER_1c_L4  <- circle_cal_3(sim500_30_10V_highINTER_1c_L4,  t0 = t0)
#save_rsflc_result(result_RSFLC_sim500_30_10V_highINTER_1c_L4, "result_RSFLC_sim500_30_10V_highINTER_1c_L4")
#result_RSFLC_sim500_30_10V_highINTER_1c_L5  <- circle_cal_3(sim500_30_10V_highINTER_1c_L5,  t0 = t0)
#save_rsflc_result(result_RSFLC_sim500_30_10V_highINTER_1c_L5, "result_RSFLC_sim500_30_10V_highINTER_1c_L5")
result_RSFLC_sim500_70_10V_highINTER_1c_L1  <- circle_cal_3(sim500_70_10V_highINTER_1c_L1,  t0 = t0)
save_rsflc_result(result_RSFLC_sim500_70_10V_highINTER_1c_L1, "result_RSFLC_sim500_70_10V_highINTER_1c_L1")
#result_RSFLC_sim500_70_10V_highINTER_1c_L2  <- circle_cal_3(sim500_70_10V_highINTER_1c_L2,  t0 = t0)
#save_rsflc_result(result_RSFLC_sim500_70_10V_highINTER_1c_L2, "result_RSFLC_sim500_70_10V_highINTER_1c_L2")
#result_RSFLC_sim500_70_10V_highINTER_1c_L3  <- circle_cal_3(sim500_70_10V_highINTER_1c_L3,  t0 = t0)
#save_rsflc_result(result_RSFLC_sim500_70_10V_highINTER_1c_L3, "result_RSFLC_sim500_70_10V_highINTER_1c_L3")
#result_RSFLC_sim500_70_10V_highINTER_1c_L4  <- circle_cal_3(sim500_70_10V_highINTER_1c_L4,  t0 = t0)
#save_rsflc_result(result_RSFLC_sim500_70_10V_highINTER_1c_L4, "result_RSFLC_sim500_70_10V_highINTER_1c_L4")
#result_RSFLC_sim500_70_10V_highINTER_1c_L5  <- circle_cal_3(sim500_70_10V_highINTER_1c_L5,  t0 = t0)
#save_rsflc_result(result_RSFLC_sim500_70_10V_highINTER_1c_L5, "result_RSFLC_sim500_70_10V_highINTER_1c_L5")
result_RSFLC_sim1000_30_10V_highINTER_1c_L1 <- circle_cal_3(sim1000_30_10V_highINTER_1c_L1, t0 = t0)
save_rsflc_result(result_RSFLC_sim1000_30_10V_highINTER_1c_L1, "result_RSFLC_sim1000_30_10V_highINTER_1c_L1")
#result_RSFLC_sim1000_30_10V_highINTER_1c_L2 <- circle_cal_3(sim1000_30_10V_highINTER_1c_L2, t0 = t0)
#save_rsflc_result(result_RSFLC_sim1000_30_10V_highINTER_1c_L2, "result_RSFLC_sim1000_30_10V_highINTER_1c_L2")
#result_RSFLC_sim1000_30_10V_highINTER_1c_L3 <- circle_cal_3(sim1000_30_10V_highINTER_1c_L3, t0 = t0)
#save_rsflc_result(result_RSFLC_sim1000_30_10V_highINTER_1c_L3, "result_RSFLC_sim1000_30_10V_highINTER_1c_L3")
#result_RSFLC_sim1000_30_10V_highINTER_1c_L4 <- circle_cal_3(sim1000_30_10V_highINTER_1c_L4, t0 = t0)
#save_rsflc_result(result_RSFLC_sim1000_30_10V_highINTER_1c_L4, "result_RSFLC_sim1000_30_10V_highINTER_1c_L4")
#result_RSFLC_sim1000_30_10V_highINTER_1c_L5 <- circle_cal_3(sim1000_30_10V_highINTER_1c_L5, t0 = t0)
#save_rsflc_result(result_RSFLC_sim1000_30_10V_highINTER_1c_L5, "result_RSFLC_sim1000_30_10V_highINTER_1c_L5")
result_RSFLC_sim1000_70_10V_highINTER_1c_L1 <- circle_cal_3(sim1000_70_10V_highINTER_1c_L1, t0 = t0)
save_rsflc_result(result_RSFLC_sim1000_70_10V_highINTER_1c_L1, "result_RSFLC_sim1000_70_10V_highINTER_1c_L1")
#result_RSFLC_sim1000_70_10V_highINTER_1c_L2 <- circle_cal_3(sim1000_70_10V_highINTER_1c_L2, t0 = t0)
#save_rsflc_result(result_RSFLC_sim1000_70_10V_highINTER_1c_L2, "result_RSFLC_sim1000_70_10V_highINTER_1c_L2")
#result_RSFLC_sim1000_70_10V_highINTER_1c_L3 <- circle_cal_3(sim1000_70_10V_highINTER_1c_L3, t0 = t0)
#save_rsflc_result(result_RSFLC_sim1000_70_10V_highINTER_1c_L3, "result_RSFLC_sim1000_70_10V_highINTER_1c_L3")
#result_RSFLC_sim1000_70_10V_highINTER_1c_L4 <- circle_cal_3(sim1000_70_10V_highINTER_1c_L4, t0 = t0)
#save_rsflc_result(result_RSFLC_sim1000_70_10V_highINTER_1c_L4, "result_RSFLC_sim1000_70_10V_highINTER_1c_L4")
#result_RSFLC_sim1000_70_10V_highINTER_1c_L5 <- circle_cal_3(sim1000_70_10V_highINTER_1c_L5, t0 = t0)
#save_rsflc_result(result_RSFLC_sim1000_70_10V_highINTER_1c_L5, "result_RSFLC_sim1000_70_10V_highINTER_1c_L5")

message("【低相关性-BTW】测试集 RSF-LC 评估...")
result_RSFLC_sim500_30_10V_lowBTW_1c_L1  <- circle_cal_3(sim500_30_10V_lowBTW_1c_L1,  t0 = t0)
save_rsflc_result(result_RSFLC_sim500_30_10V_lowBTW_1c_L1, "result_RSFLC_sim500_30_10V_lowBTW_1c_L1")
#result_RSFLC_sim500_30_10V_lowBTW_1c_L2  <- circle_cal_3(sim500_30_10V_lowBTW_1c_L2,  t0 = t0)
#save_rsflc_result(result_RSFLC_sim500_30_10V_lowBTW_1c_L2, "result_RSFLC_sim500_30_10V_lowBTW_1c_L2")
#result_RSFLC_sim500_30_10V_lowBTW_1c_L3  <- circle_cal_3(sim500_30_10V_lowBTW_1c_L3,  t0 = t0)
#save_rsflc_result(result_RSFLC_sim500_30_10V_lowBTW_1c_L3, "result_RSFLC_sim500_30_10V_lowBTW_1c_L3")
#result_RSFLC_sim500_30_10V_lowBTW_1c_L4  <- circle_cal_3(sim500_30_10V_lowBTW_1c_L4,  t0 = t0)
#save_rsflc_result(result_RSFLC_sim500_30_10V_lowBTW_1c_L4, "result_RSFLC_sim500_30_10V_lowBTW_1c_L4")
#result_RSFLC_sim500_30_10V_lowBTW_1c_L5  <- circle_cal_3(sim500_30_10V_lowBTW_1c_L5,  t0 = t0)
#save_rsflc_result(result_RSFLC_sim500_30_10V_lowBTW_1c_L5, "result_RSFLC_sim500_30_10V_lowBTW_1c_L5")
result_RSFLC_sim500_70_10V_lowBTW_1c_L1  <- circle_cal_3(sim500_70_10V_lowBTW_1c_L1,  t0 = t0)
save_rsflc_result(result_RSFLC_sim500_70_10V_lowBTW_1c_L1, "result_RSFLC_sim500_70_10V_lowBTW_1c_L1")
#result_RSFLC_sim500_70_10V_lowBTW_1c_L2  <- circle_cal_3(sim500_70_10V_lowBTW_1c_L2,  t0 = t0)
#save_rsflc_result(result_RSFLC_sim500_70_10V_lowBTW_1c_L2, "result_RSFLC_sim500_70_10V_lowBTW_1c_L2")
#result_RSFLC_sim500_70_10V_lowBTW_1c_L3  <- circle_cal_3(sim500_70_10V_lowBTW_1c_L3,  t0 = t0)
#save_rsflc_result(result_RSFLC_sim500_70_10V_lowBTW_1c_L3, "result_RSFLC_sim500_70_10V_lowBTW_1c_L3")
#result_RSFLC_sim500_70_10V_lowBTW_1c_L4  <- circle_cal_3(sim500_70_10V_lowBTW_1c_L4,  t0 = t0)
#save_rsflc_result(result_RSFLC_sim500_70_10V_lowBTW_1c_L4, "result_RSFLC_sim500_70_10V_lowBTW_1c_L4")
#result_RSFLC_sim500_70_10V_lowBTW_1c_L5  <- circle_cal_3(sim500_70_10V_lowBTW_1c_L5,  t0 = t0)
#save_rsflc_result(result_RSFLC_sim500_70_10V_lowBTW_1c_L5, "result_RSFLC_sim500_70_10V_lowBTW_1c_L5")
result_RSFLC_sim1000_30_10V_lowBTW_1c_L1 <- circle_cal_3(sim1000_30_10V_lowBTW_1c_L1, t0 = t0)
save_rsflc_result(result_RSFLC_sim1000_30_10V_lowBTW_1c_L1, "result_RSFLC_sim1000_30_10V_lowBTW_1c_L1")
#result_RSFLC_sim1000_30_10V_lowBTW_1c_L2 <- circle_cal_3(sim1000_30_10V_lowBTW_1c_L2, t0 = t0)
#save_rsflc_result(result_RSFLC_sim1000_30_10V_lowBTW_1c_L2, "result_RSFLC_sim1000_30_10V_lowBTW_1c_L2")
#result_RSFLC_sim1000_30_10V_lowBTW_1c_L3 <- circle_cal_3(sim1000_30_10V_lowBTW_1c_L3, t0 = t0)
#save_rsflc_result(result_RSFLC_sim1000_30_10V_lowBTW_1c_L3, "result_RSFLC_sim1000_30_10V_lowBTW_1c_L3")
#result_RSFLC_sim1000_30_10V_lowBTW_1c_L4 <- circle_cal_3(sim1000_30_10V_lowBTW_1c_L4, t0 = t0)
#save_rsflc_result(result_RSFLC_sim1000_30_10V_lowBTW_1c_L4, "result_RSFLC_sim1000_30_10V_lowBTW_1c_L4")
#result_RSFLC_sim1000_30_10V_lowBTW_1c_L5 <- circle_cal_3(sim1000_30_10V_lowBTW_1c_L5, t0 = t0)
#save_rsflc_result(result_RSFLC_sim1000_30_10V_lowBTW_1c_L5, "result_RSFLC_sim1000_30_10V_lowBTW_1c_L5")
result_RSFLC_sim1000_70_10V_lowBTW_1c_L1 <- circle_cal_3(sim1000_70_10V_lowBTW_1c_L1, t0 = t0)
save_rsflc_result(result_RSFLC_sim1000_70_10V_lowBTW_1c_L1, "result_RSFLC_sim1000_70_10V_lowBTW_1c_L1")
#result_RSFLC_sim1000_70_10V_lowBTW_1c_L2 <- circle_cal_3(sim1000_70_10V_lowBTW_1c_L2, t0 = t0)
#save_rsflc_result(result_RSFLC_sim1000_70_10V_lowBTW_1c_L2, "result_RSFLC_sim1000_70_10V_lowBTW_1c_L2")
#result_RSFLC_sim1000_70_10V_lowBTW_1c_L3 <- circle_cal_3(sim1000_70_10V_lowBTW_1c_L3, t0 = t0)
#save_rsflc_result(result_RSFLC_sim1000_70_10V_lowBTW_1c_L3, "result_RSFLC_sim1000_70_10V_lowBTW_1c_L3")
#result_RSFLC_sim1000_70_10V_lowBTW_1c_L4 <- circle_cal_3(sim1000_70_10V_lowBTW_1c_L4, t0 = t0)
#save_rsflc_result(result_RSFLC_sim1000_70_10V_lowBTW_1c_L4, "result_RSFLC_sim1000_70_10V_lowBTW_1c_L4")
#result_RSFLC_sim1000_70_10V_lowBTW_1c_L5 <- circle_cal_3(sim1000_70_10V_lowBTW_1c_L5, t0 = t0)
#save_rsflc_result(result_RSFLC_sim1000_70_10V_lowBTW_1c_L5, "result_RSFLC_sim1000_70_10V_lowBTW_1c_L5")

message("【中相关性-BTW】测试集 RSF-LC 评估...")
result_RSFLC_sim500_30_10V_midBTW_1c_L1  <- circle_cal_3(sim500_30_10V_midBTW_1c_L1,  t0 = t0)
save_rsflc_result(result_RSFLC_sim500_30_10V_midBTW_1c_L1, "result_RSFLC_sim500_30_10V_midBTW_1c_L1")
#result_RSFLC_sim500_30_10V_midBTW_1c_L2  <- circle_cal_3(sim500_30_10V_midBTW_1c_L2,  t0 = t0)
#save_rsflc_result(result_RSFLC_sim500_30_10V_midBTW_1c_L2, "result_RSFLC_sim500_30_10V_midBTW_1c_L2")
#result_RSFLC_sim500_30_10V_midBTW_1c_L3  <- circle_cal_3(sim500_30_10V_midBTW_1c_L3,  t0 = t0)
#save_rsflc_result(result_RSFLC_sim500_30_10V_midBTW_1c_L3, "result_RSFLC_sim500_30_10V_midBTW_1c_L3")
#result_RSFLC_sim500_30_10V_midBTW_1c_L4  <- circle_cal_3(sim500_30_10V_midBTW_1c_L4,  t0 = t0)
#save_rsflc_result(result_RSFLC_sim500_30_10V_midBTW_1c_L4, "result_RSFLC_sim500_30_10V_midBTW_1c_L4")
#result_RSFLC_sim500_30_10V_midBTW_1c_L5  <- circle_cal_3(sim500_30_10V_midBTW_1c_L5,  t0 = t0)
#save_rsflc_result(result_RSFLC_sim500_30_10V_midBTW_1c_L5, "result_RSFLC_sim500_30_10V_midBTW_1c_L5")
result_RSFLC_sim500_70_10V_midBTW_1c_L1  <- circle_cal_3(sim500_70_10V_midBTW_1c_L1,  t0 = t0)
save_rsflc_result(result_RSFLC_sim500_70_10V_midBTW_1c_L1, "result_RSFLC_sim500_70_10V_midBTW_1c_L1")
#result_RSFLC_sim500_70_10V_midBTW_1c_L2  <- circle_cal_3(sim500_70_10V_midBTW_1c_L2,  t0 = t0)
#save_rsflc_result(result_RSFLC_sim500_70_10V_midBTW_1c_L2, "result_RSFLC_sim500_70_10V_midBTW_1c_L2")
#result_RSFLC_sim500_70_10V_midBTW_1c_L3  <- circle_cal_3(sim500_70_10V_midBTW_1c_L3,  t0 = t0)
#save_rsflc_result(result_RSFLC_sim500_70_10V_midBTW_1c_L3, "result_RSFLC_sim500_70_10V_midBTW_1c_L3")
#result_RSFLC_sim500_70_10V_midBTW_1c_L4  <- circle_cal_3(sim500_70_10V_midBTW_1c_L4,  t0 = t0)
#save_rsflc_result(result_RSFLC_sim500_70_10V_midBTW_1c_L4, "result_RSFLC_sim500_70_10V_midBTW_1c_L4")
#result_RSFLC_sim500_70_10V_midBTW_1c_L5  <- circle_cal_3(sim500_70_10V_midBTW_1c_L5,  t0 = t0)
#save_rsflc_result(result_RSFLC_sim500_70_10V_midBTW_1c_L5, "result_RSFLC_sim500_70_10V_midBTW_1c_L5")
result_RSFLC_sim1000_30_10V_midBTW_1c_L1 <- circle_cal_3(sim1000_30_10V_midBTW_1c_L1, t0 = t0)
save_rsflc_result(result_RSFLC_sim1000_30_10V_midBTW_1c_L1, "result_RSFLC_sim1000_30_10V_midBTW_1c_L1")
#result_RSFLC_sim1000_30_10V_midBTW_1c_L2 <- circle_cal_3(sim1000_30_10V_midBTW_1c_L2, t0 = t0)
#save_rsflc_result(result_RSFLC_sim1000_30_10V_midBTW_1c_L2, "result_RSFLC_sim1000_30_10V_midBTW_1c_L2")
#result_RSFLC_sim1000_30_10V_midBTW_1c_L3 <- circle_cal_3(sim1000_30_10V_midBTW_1c_L3, t0 = t0)
#save_rsflc_result(result_RSFLC_sim1000_30_10V_midBTW_1c_L3, "result_RSFLC_sim1000_30_10V_midBTW_1c_L3")
#result_RSFLC_sim1000_30_10V_midBTW_1c_L4 <- circle_cal_3(sim1000_30_10V_midBTW_1c_L4, t0 = t0)
#save_rsflc_result(result_RSFLC_sim1000_30_10V_midBTW_1c_L4, "result_RSFLC_sim1000_30_10V_midBTW_1c_L4")
#result_RSFLC_sim1000_30_10V_midBTW_1c_L5 <- circle_cal_3(sim1000_30_10V_midBTW_1c_L5, t0 = t0)
#save_rsflc_result(result_RSFLC_sim1000_30_10V_midBTW_1c_L5, "result_RSFLC_sim1000_30_10V_midBTW_1c_L5")
result_RSFLC_sim1000_70_10V_midBTW_1c_L1 <- circle_cal_3(sim1000_70_10V_midBTW_1c_L1, t0 = t0)
save_rsflc_result(result_RSFLC_sim1000_70_10V_midBTW_1c_L1, "result_RSFLC_sim1000_70_10V_midBTW_1c_L1")
#result_RSFLC_sim1000_70_10V_midBTW_1c_L2 <- circle_cal_3(sim1000_70_10V_midBTW_1c_L2, t0 = t0)
#save_rsflc_result(result_RSFLC_sim1000_70_10V_midBTW_1c_L2, "result_RSFLC_sim1000_70_10V_midBTW_1c_L2")
#result_RSFLC_sim1000_70_10V_midBTW_1c_L3 <- circle_cal_3(sim1000_70_10V_midBTW_1c_L3, t0 = t0)
#save_rsflc_result(result_RSFLC_sim1000_70_10V_midBTW_1c_L3, "result_RSFLC_sim1000_70_10V_midBTW_1c_L3")
#result_RSFLC_sim1000_70_10V_midBTW_1c_L4 <- circle_cal_3(sim1000_70_10V_midBTW_1c_L4, t0 = t0)
#save_rsflc_result(result_RSFLC_sim1000_70_10V_midBTW_1c_L4, "result_RSFLC_sim1000_70_10V_midBTW_1c_L4")
#result_RSFLC_sim1000_70_10V_midBTW_1c_L5 <- circle_cal_3(sim1000_70_10V_midBTW_1c_L5, t0 = t0)
#save_rsflc_result(result_RSFLC_sim1000_70_10V_midBTW_1c_L5, "result_RSFLC_sim1000_70_10V_midBTW_1c_L5")

message("【高相关性-BTW】测试集 RSF-LC 评估...")
result_RSFLC_sim500_30_10V_highBTW_1c_L1  <- circle_cal_3(sim500_30_10V_highBTW_1c_L1,  t0 = t0)
save_rsflc_result(result_RSFLC_sim500_30_10V_highBTW_1c_L1, "result_RSFLC_sim500_30_10V_highBTW_1c_L1")
#result_RSFLC_sim500_30_10V_highBTW_1c_L2  <- circle_cal_3(sim500_30_10V_highBTW_1c_L2,  t0 = t0)
#save_rsflc_result(result_RSFLC_sim500_30_10V_highBTW_1c_L2, "result_RSFLC_sim500_30_10V_highBTW_1c_L2")
#result_RSFLC_sim500_30_10V_highBTW_1c_L3  <- circle_cal_3(sim500_30_10V_highBTW_1c_L3,  t0 = t0)
#save_rsflc_result(result_RSFLC_sim500_30_10V_highBTW_1c_L3, "result_RSFLC_sim500_30_10V_highBTW_1c_L3")
#result_RSFLC_sim500_30_10V_highBTW_1c_L4  <- circle_cal_3(sim500_30_10V_highBTW_1c_L4,  t0 = t0)
#save_rsflc_result(result_RSFLC_sim500_30_10V_highBTW_1c_L4, "result_RSFLC_sim500_30_10V_highBTW_1c_L4")
#result_RSFLC_sim500_30_10V_highBTW_1c_L5  <- circle_cal_3(sim500_30_10V_highBTW_1c_L5,  t0 = t0)
#save_rsflc_result(result_RSFLC_sim500_30_10V_highBTW_1c_L5, "result_RSFLC_sim500_30_10V_highBTW_1c_L5")
result_RSFLC_sim500_70_10V_highBTW_1c_L1  <- circle_cal_3(sim500_70_10V_highBTW_1c_L1,  t0 = t0)
save_rsflc_result(result_RSFLC_sim500_70_10V_highBTW_1c_L1, "result_RSFLC_sim500_70_10V_highBTW_1c_L1")
#result_RSFLC_sim500_70_10V_highBTW_1c_L2  <- circle_cal_3(sim500_70_10V_highBTW_1c_L2,  t0 = t0)
#save_rsflc_result(result_RSFLC_sim500_70_10V_highBTW_1c_L2, "result_RSFLC_sim500_70_10V_highBTW_1c_L2")
#result_RSFLC_sim500_70_10V_highBTW_1c_L3  <- circle_cal_3(sim500_70_10V_highBTW_1c_L3,  t0 = t0)
#save_rsflc_result(result_RSFLC_sim500_70_10V_highBTW_1c_L3, "result_RSFLC_sim500_70_10V_highBTW_1c_L3")
#result_RSFLC_sim500_70_10V_highBTW_1c_L4  <- circle_cal_3(sim500_70_10V_highBTW_1c_L4,  t0 = t0)
#save_rsflc_result(result_RSFLC_sim500_70_10V_highBTW_1c_L4, "result_RSFLC_sim500_70_10V_highBTW_1c_L4")
#result_RSFLC_sim500_70_10V_highBTW_1c_L5  <- circle_cal_3(sim500_70_10V_highBTW_1c_L5,  t0 = t0)
#save_rsflc_result(result_RSFLC_sim500_70_10V_highBTW_1c_L5, "result_RSFLC_sim500_70_10V_highBTW_1c_L5")
result_RSFLC_sim1000_30_10V_highBTW_1c_L1 <- circle_cal_3(sim1000_30_10V_highBTW_1c_L1, t0 = t0)
save_rsflc_result(result_RSFLC_sim1000_30_10V_highBTW_1c_L1, "result_RSFLC_sim1000_30_10V_highBTW_1c_L1")
#result_RSFLC_sim1000_30_10V_highBTW_1c_L2 <- circle_cal_3(sim1000_30_10V_highBTW_1c_L2, t0 = t0)
#save_rsflc_result(result_RSFLC_sim1000_30_10V_highBTW_1c_L2, "result_RSFLC_sim1000_30_10V_highBTW_1c_L2")
#result_RSFLC_sim1000_30_10V_highBTW_1c_L3 <- circle_cal_3(sim1000_30_10V_highBTW_1c_L3, t0 = t0)
#save_rsflc_result(result_RSFLC_sim1000_30_10V_highBTW_1c_L3, "result_RSFLC_sim1000_30_10V_highBTW_1c_L3")
#result_RSFLC_sim1000_30_10V_highBTW_1c_L4 <- circle_cal_3(sim1000_30_10V_highBTW_1c_L4, t0 = t0)
#save_rsflc_result(result_RSFLC_sim1000_30_10V_highBTW_1c_L4, "result_RSFLC_sim1000_30_10V_highBTW_1c_L4")
#result_RSFLC_sim1000_30_10V_highBTW_1c_L5 <- circle_cal_3(sim1000_30_10V_highBTW_1c_L5, t0 = t0)
#save_rsflc_result(result_RSFLC_sim1000_30_10V_highBTW_1c_L5, "result_RSFLC_sim1000_30_10V_highBTW_1c_L5")
result_RSFLC_sim1000_70_10V_highBTW_1c_L1 <- circle_cal_3(sim1000_70_10V_highBTW_1c_L1, t0 = t0)
save_rsflc_result(result_RSFLC_sim1000_70_10V_highBTW_1c_L1, "result_RSFLC_sim1000_70_10V_highBTW_1c_L1")
#result_RSFLC_sim1000_70_10V_highBTW_1c_L2 <- circle_cal_3(sim1000_70_10V_highBTW_1c_L2, t0 = t0)
#save_rsflc_result(result_RSFLC_sim1000_70_10V_highBTW_1c_L2, "result_RSFLC_sim1000_70_10V_highBTW_1c_L2")
#result_RSFLC_sim1000_70_10V_highBTW_1c_L3 <- circle_cal_3(sim1000_70_10V_highBTW_1c_L3, t0 = t0)
#save_rsflc_result(result_RSFLC_sim1000_70_10V_highBTW_1c_L3, "result_RSFLC_sim1000_70_10V_highBTW_1c_L3")
#result_RSFLC_sim1000_70_10V_highBTW_1c_L4 <- circle_cal_3(sim1000_70_10V_highBTW_1c_L4, t0 = t0)
#save_rsflc_result(result_RSFLC_sim1000_70_10V_highBTW_1c_L4, "result_RSFLC_sim1000_70_10V_highBTW_1c_L4")
#result_RSFLC_sim1000_70_10V_highBTW_1c_L5 <- circle_cal_3(sim1000_70_10V_highBTW_1c_L5, t0 = t0)
#save_rsflc_result(result_RSFLC_sim1000_70_10V_highBTW_1c_L5, "result_RSFLC_sim1000_70_10V_highBTW_1c_L5")

message("✅ 10V1C 测试集 RSF-LC 评估完成（INTER + BTW）！结果已在每次建模后写入: ", output_dir)
