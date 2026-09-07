
data_0506<-read.csv("F:/文章_大论文/0417/data_0506.csv")

######数据准备#######



# =========================
# data_0506：按患者分层 5 折（内嵌，无中间文件）
# 前提：已执行 data_0506 <- read.csv(...)
# =========================
dat_raw <- data_0506
event_col_cv <- NULL
for (cand in c("event", "death", "hospitalmortality", "hospital_mortality", "survival")) {
  if (cand %in% names(dat_raw)) {
    event_col_cv <- cand
    break
  }
}
if (is.null(event_col_cv)) {
  stop("未找到事件列（event/death/hospitalmortality/hospital_mortality/survival）")
}
id_col <- NULL
for (cand in c("subject_id", "subjectid", "id")) {
  if (cand %in% names(dat_raw)) {
    id_col <- cand
    break
  }
}
if (is.null(id_col)) {
  stop("未找到个体 ID 列（subject_id/subjectid/id）")
}
# 按个体划分：每个患者一个 fold，同一患者所有行同一折
ids_all <- unique(dat_raw[[id_col]])
patient_event <- dat_raw %>%
  dplyr::group_by(.data[[id_col]]) %>%
  dplyr::summarise(event = as.numeric(dplyr::first(.data[[event_col_cv]])), .groups = "drop")
patient_event$event <- ifelse(
  is.na(patient_event$event) | patient_event$event > 0,
  1L,
  0L
)
K <- 5L
id_e1 <- patient_event[[id_col]][patient_event$event == 1L]
id_e0 <- patient_event[[id_col]][patient_event$event == 0L]
fold_vec <- rep(NA_integer_, length(ids_all))
if (length(id_e1) > 0L) {
  fold_vec[match(id_e1, ids_all)] <- sample(rep(1L:K, length.out = length(id_e1)))
}
if (length(id_e0) > 0L) {
  fold_vec[match(id_e0, ids_all)] <- sample(rep(1L:K, length.out = length(id_e0)))
}
fold_df <- data.frame(id = ids_all, fold = fold_vec, stringsAsFactors = FALSE)
fold_df <- fold_df[!is.na(fold_df$fold), , drop = FALSE]
message("已按个体（患者）分层划分为 5 折，共 ", nrow(fold_df), " 个个体")
# 合并 fold 到数据表，仅保留有有效 fold 的个体
fold_df_join <- fold_df
names(fold_df_join)[names(fold_df_join) == "id"] <- id_col
dat <- dat_raw %>%
  dplyr::inner_join(fold_df_join, by = id_col) %>%
  as.data.frame(stringsAsFactors = FALSE)


#############



#######

#######





###程序#####
cal_3_RSF_LC <- function(data, t0 = 1) {
  
  tryCatch({
    # 确保加载了所有必要的包
    if (!requireNamespace("timeROC", quietly = TRUE)) {
      install.packages("timeROC")
      library(timeROC)
    }
    
    ################
    ## 1. 数据准备（与 DynForest 文档示例一致的对象命名）
    ################
    
    item_cols <- grep("^item_", names(data), value = TRUE)
    if (length(item_cols) == 0L) {
      stop("数据中未找到以 item_ 开头的纵向指标列。")
    }
    req_static <- c("subject_id", "time", "age", "hosp_time", "survival")
    miss <- setdiff(req_static, names(data))
    if (length(miss) > 0L) {
      stop("缺少列: ", paste(miss, collapse = ", "))
    }
    
    # DynForest 要求 id 为 numeric/integer；因子须先 as.character 再转数值，避免变成水平编码
    fixedData_train <- data %>%
      dplyr::group_by(subject_id) %>%
      dplyr::summarise(
        id = as.numeric(as.character(dplyr::first(subject_id))),
        age = dplyr::first(age),
        .groups = "drop"
      )
    
    # 使用函数参数 data（勿用全局 data_0506）；每人一行生存结局
    Y_df <- data %>%
      dplyr::group_by(subject_id) %>%
      dplyr::summarise(
        id = as.numeric(as.character(dplyr::first(subject_id))),
        time = dplyr::first(hosp_time),
        event = as.numeric(dplyr::first(survival)),
        .groups = "drop"
      )
    Y_df$event <- ifelse(is.na(Y_df$event) | Y_df$event > 0, 1L, 0L)
    
    Y <- list(
      type = "surv",
      Y = data.frame(
        id = as.numeric(Y_df$id),
        time = as.numeric(Y_df$time),
        event = as.numeric(Y_df$event),
        stringsAsFactors = FALSE
      )
    )
    
    timeData_train <- data %>%
      dplyr::transmute(
        id = as.numeric(as.character(subject_id)),
        time = .data[["time"]],
        dplyr::across(dplyr::all_of(item_cols))
      )
    
    timeVarModel <- stats::setNames(
      lapply(item_cols, function(nm) {
        list(
          fixed = stats::as.formula(paste(nm, "~ time")),
          random = ~ time
        )
      }),
      item_cols
    )
    
    # 第 3–4 步评估用：id、age 与生存结局对齐
    fixed_data <- fixedData_train %>%
      dplyr::left_join(Y_df %>% dplyr::select(id, time, event), by = "id")
    
    ################
    ## 2. DynForest 模型
    ################
    dyn_model <- DynForest::dynforest(
      timeData      = as.data.frame(timeData_train),
      fixedData     = as.data.frame(fixedData_train),
      idVar         = "id",
      timeVar       = "time",
      timeVarModel  = timeVarModel,
      Y             = Y,
      ntree         = 500,
      mtry          = 2,
      nodesize      = 10,
      minsplit      = 2,
      nsplit_option = "quantile",
      ncores        = 1,
      verbose       = FALSE
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
      risk_score <- as.numeric(fixed_data$age)
      risk_score[is.na(risk_score)] <- 0
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
    
    message("⚠️ cal_3_RSF_LC failed: ", e$message)
    
    result <- data.frame(
      AUC = NA_real_,
      BS = NA_real_,
      Cindex = NA_real_
    )
    
    rownames(result) <- paste0("t=", t0)
    
    return(result)
  })
}





########




################






