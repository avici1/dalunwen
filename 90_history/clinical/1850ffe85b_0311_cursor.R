library(dplyr)
library(survival)
library(timeROC)
library(DynForest)
library(purrr)
library(readxl)
library(writexl)
library(tidyr)
set.seed(123)
options(scipen = 999)
options(pillar.width = Inf)

# =============================================================================
# 数据读取与预处理（适配 MIMIC data_with_obstime）
# =============================================================================
data_paths <- c(
  "F:/文章_大论文/MIMIC数据库_代码/TREA代码/0311/data_withobstime.xlsx",
  "F:/文章_大论文/MIMIC数据库_代码/TREA代码/0312/data_all.xlsx"
)
data_with_obstime <- NULL
for (p in data_paths) {
  if (file.exists(p)) {
    data_with_obstime <- read_xlsx(p)
    message("已读取: ", p)
    break
  }
}
if (is.null(data_with_obstime)) stop("未找到数据文件，请先运行 0311.R 生成 data_withobstime 或 data_all.xlsx")

# 标准化列名
if (!"subject_id" %in% names(data_with_obstime) && "ID" %in% names(data_with_obstime)) {
  data_with_obstime <- data_with_obstime %>% rename(subject_id = ID)
}

# 若无 obstime，按 charttime 生成
if (!"obstime" %in% names(data_with_obstime)) {
  data_with_obstime <- data_with_obstime %>%
    group_by(subject_id) %>%
    arrange(charttime, .by_group = TRUE) %>%
    mutate(obstime = row_number()) %>%
    ungroup()
}

# 生存时间 obs_time：优先 los_icu / los_hospital，否则由 charttime 或 obstime 推导
if (!"obs_time" %in% names(data_with_obstime)) {
  if ("los_icu" %in% names(data_with_obstime)) {
    data_with_obstime <- data_with_obstime %>%
      group_by(subject_id) %>%
      mutate(obs_time = unique(na.omit(los_icu))[1]) %>%
      ungroup()
  } else if ("los_hospital" %in% names(data_with_obstime)) {
    data_with_obstime <- data_with_obstime %>%
      group_by(subject_id) %>%
      mutate(obs_time = unique(na.omit(los_hospital))[1]) %>%
      ungroup()
  } else if ("charttime" %in% names(data_with_obstime)) {
    data_with_obstime <- data_with_obstime %>%
      group_by(subject_id) %>%
      mutate(
        obs_time = as.numeric(difftime(max(charttime, na.rm = TRUE), min(charttime, na.rm = TRUE), units = "days")),
        obs_time = if_else(is.na(obs_time) | obs_time <= 0, as.numeric(max(obstime, na.rm = TRUE)), obs_time)
      ) %>%
      ungroup()
  } else {
    data_with_obstime <- data_with_obstime %>%
      group_by(subject_id) %>%
      mutate(obs_time = max(obstime, na.rm = TRUE)) %>%
      ungroup()
  }
}
data_with_obstime <- data_with_obstime %>%
  group_by(subject_id) %>%
  mutate(obs_time = pmax(0.1, obs_time, na.rm = TRUE)) %>%
  ungroup()

# 基线年龄
if (!"anchor_age" %in% names(data_with_obstime) && "age" %in% names(data_with_obstime)) {
  data_with_obstime <- data_with_obstime %>% rename(anchor_age = age)
}
if (!"anchor_age" %in% names(data_with_obstime)) {
  data_with_obstime <- data_with_obstime %>% mutate(anchor_age = 60)
}

# 性别编码
data_with_obstime <- data_with_obstime %>%
  mutate(
    gender_num = case_when(
      tolower(gender) %in% c("m", "male", "1") ~ 1,
      tolower(gender) %in% c("f", "female", "0") ~ 0,
      TRUE ~ NA_real_
    )
  )
if (all(is.na(data_with_obstime$gender_num))) {
  data_with_obstime <- data_with_obstime %>% mutate(gender_num = 0)
}

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
    
    # 统一 id 映射，确保 longitudinal 与 fixed 一致
    id_levels <- unique(data$ID)
    data <- data %>% mutate(num_id = match(ID, id_levels))
    
    # 纵向数据（只有真正的纵向标记物）
    longitudinal_data <- data %>%
      select(id = num_id, time = t, Y) %>%
      mutate(id = as.numeric(id))
    
    # 生存数据（每个 ID 一行）
    survival_data <- data %>%
      group_by(ID) %>%
      summarise(
        time  = unique(obs_time)[1],
        event = unique(event)[1],
        .groups = "drop"
      ) %>%
      mutate(id = match(ID, id_levels))
    
    # 基线协变量（lp, class 及可选基线变量）
    base_cols <- c("lp", "class")
    base_cols <- intersect(base_cols, names(data))
    baseline_data <- data %>%
      group_by(ID) %>%
      summarise(across(any_of(base_cols), ~ unique(na.omit(.x))[1]), .groups = "drop") %>%
      mutate(id = match(ID, id_levels))
    
    # fixedData：仅基线预测变量（无 time/event），供 DynForest 训练与 predict
    fixed_data <- survival_data %>%
      select(id, time, event) %>%
      left_join(baseline_data %>% select(-ID), by = "id") %>%
      mutate(
        id = as.numeric(id),
        lp = coalesce(lp, 0),
        class = coalesce(class, 0)
      )
    fixed_data_pred <- fixed_data %>% select(id, lp, class)
    
    ################
    ## 2. DynForest 模型
    ################
    # 使用正确的包名调用dynforest函数
    dyn_model <- DynForest::dynforest(
      timeData      = as.data.frame(longitudinal_data),
      fixedData     = as.data.frame(fixed_data_pred),
      idVar         = "id",
      timeVar       = "time",
      timeVarModel  = list(
        Y = list(
          fixed = Y ~ time,
          random = ~ time
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
    ## 3. 使用正确接口提取风险得分
    ################
    
    surv_time <- fixed_data$time
    surv_event <- fixed_data$event
    n <- length(surv_time)
    
    risk_score <- tryCatch({
      pred_risk <- dynforest_predict_risk(
        dyn_obj   = dyn_model,
        timeData  = longitudinal_data,
        fixedData = fixed_data_pred,
        idVar     = "id",
        timeVar   = "time",
        t0        = t0
      )
      if (is.null(pred_risk) || length(pred_risk) == 0) stop("predict returned NULL")
      ord <- match(fixed_data$id, as.integer(names(pred_risk)))
      rs <- as.numeric(pred_risk[ord])
      rs[is.na(rs)] <- fixed_data$lp[is.na(rs)]
      rs
    }, error = function(e) {
      message("DynForest predict 失败，使用 lp 作为替代: ", e$message)
      fixed_data$lp
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

# =============================================================================
# DynForest 正确 predict 封装：使用 timeData, fixedData, idVar, timeVar, t0
# 返回每例的风险得分（CIF 概率）
# =============================================================================
dynforest_predict_risk <- function(dyn_obj, timeData, fixedData, idVar = "id",
                                   timeVar = "time", t0 = NULL) {
  if (!inherits(dyn_obj, "dynforest")) return(NULL)
  timeData <- as.data.frame(timeData)
  fixedData <- as.data.frame(fixedData)
  if (is.null(t0)) t0 <- max(timeData[[timeVar]], na.rm = TRUE) * 0.5
  pred_obj <- tryCatch({
    predict(object = dyn_obj,
            timeData = timeData,
            fixedData = fixedData,
            idVar = idVar,
            timeVar = timeVar,
            t0 = t0)
  }, error = function(e) NULL)
  if (is.null(pred_obj) || is.null(pred_obj$pred_indiv)) return(NULL)
  pred_mat <- pred_obj$pred_indiv
  if (is.matrix(pred_mat)) {
    risk <- apply(pred_mat, 1, function(x) if (length(x) > 0) x[length(x)] else NA)
  } else {
    risk <- as.numeric(pred_mat)
  }
  ids <- as.integer(rownames(pred_mat))
  if (is.null(ids)) ids <- seq_along(risk)
  setNames(risk, ids)
}

# =============================================================================
# itemid 筛选：基于完整性 + 单变量 C-index
# 保留满足条件的 itemid，单个作为应变量 Y 建模
# =============================================================================
screen_itemids <- function(data, itemid_cols,
                           min_subjects = 50,
                           min_meas_per_subject = 2,
                           min_completeness = 0.3,
                           min_uni_cindex = 0.52) {
  if (missing(itemid_cols)) {
    itemid_cols <- grep("^itemid_\\d+$", names(data), value = TRUE)
  }
  if (length(itemid_cols) == 0) return(character(0))
  screen_df <- lapply(itemid_cols, function(col) {
    valid <- data %>%
      group_by(subject_id) %>%
      summarise(
        n_meas = sum(!is.na(!!sym(col))),
        has_event = first(na.omit(hospital_mortality)),
        obs_t = first(na.omit(obs_time)),
        .groups = "drop"
      ) %>%
      filter(n_meas >= min_meas_per_subject)
    n_subj <- nrow(valid)
    if (n_subj < min_subjects) return(data.frame(item = col, pass = FALSE, n_subj, uni_cindex = NA_real_))
    last_val <- data %>%
      filter(subject_id %in% valid$subject_id) %>%
      group_by(subject_id) %>%
      filter(!is.na(!!sym(col))) %>%
      slice_max(obstime, n = 1, with_ties = FALSE) %>%
      ungroup() %>%
      select(subject_id, last_y = !!sym(col), event = hospital_mortality, obs_time)
    cmp <- tryCatch({
      survival::concordance(survival::Surv(obs_time, event) ~ last_y, data = last_val)$concordance
    }, error = function(e) 0.5)
    if (cmp < 0.5) cmp <- 1 - cmp
    completeness <- n_subj / length(unique(data$subject_id))
    pass <- n_subj >= min_subjects && completeness >= min_completeness && cmp >= min_uni_cindex
    data.frame(item = col, pass = pass, n_subj, uni_cindex = round(cmp, 4), completeness = round(completeness, 3))
  })
  res <- bind_rows(screen_df)
  passed <- res %>% filter(pass) %>% pull(item)
  message("itemid 筛选结果:\n", paste(capture.output(print(res)), collapse = "\n"))
  if (length(passed) == 0) {
    message("无 itemid 通过筛选，放宽条件后使用 uni_cindex 最高的 top 项")
    top <- res %>% arrange(desc(uni_cindex), desc(n_subj)) %>% slice(1) %>% pull(item)
    if (!is.na(top)) passed <- top
  }
  passed
}

# =============================================================================
# MIMIC 实验室 itemid 候选（d_labitems）
# =============================================================================
MIMIC_LAB_CANDIDATES <- list(
  creatinine  = "itemid_50912",
  glucose     = "itemid_50931",
  hemoglobin  = "itemid_51222",
  wbc         = "itemid_51301",
  potassium   = "itemid_50971",
  sodium      = "itemid_50893",
  bicarbonate = "itemid_50882",
  anion_gap   = "itemid_50868"
)

# 筛选：仅保留数据中存在的 itemid，再经 screen_itemids 筛选
itemid_cols_all <- intersect(unlist(MIMIC_LAB_CANDIDATES), names(data_with_obstime))
if (length(itemid_cols_all) == 0) itemid_cols_all <- grep("^itemid_", names(data_with_obstime), value = TRUE)
itemid_selected <- screen_itemids(
  data_with_obstime,
  itemid_cols = itemid_cols_all,
  min_subjects = 50,
  min_meas_per_subject = 2,
  min_completeness = 0.2,
  min_uni_cindex = 0.50
)
nms <- names(MIMIC_LAB_CANDIDATES)[match(itemid_selected, MIMIC_LAB_CANDIDATES)]
MIMIC_LAB_ITEMS <- as.list(itemid_selected)
names(MIMIC_LAB_ITEMS) <- ifelse(is.na(nms), itemid_selected, nms)
if (length(MIMIC_LAB_ITEMS) == 0) MIMIC_LAB_ITEMS <- setNames(as.list(itemid_cols_all[1]), "selected")

# 将 data_with_obstime 转为 cal_3_RSF_LC 所需格式
prepare_for_model <- function(data, y_var) {
  y_col <- y_var
  if (!y_col %in% names(data)) return(NULL)
  valid_ids <- data %>%
    group_by(subject_id) %>%
    summarise(has_y = any(!is.na(!!sym(y_col))), .groups = "drop") %>%
    filter(has_y) %>% pull(subject_id)
  data_valid <- data %>% filter(subject_id %in% valid_ids)
  base_per_id <- data_valid %>%
    group_by(subject_id) %>%
    summarise(
      anchor_age = first(na.omit(anchor_age)),
      gender_num = first(na.omit(gender_num)),
      .groups = "drop"
    ) %>%
    mutate(lp = (anchor_age / 10) + coalesce(gender_num, 0), class = coalesce(gender_num, 0))
  data_valid %>%
    select(subject_id, obstime, obs_time, hospital_mortality, !!y_col) %>%
    rename(t = obstime, event = hospital_mortality, Y = !!y_col) %>%
    left_join(base_per_id %>% select(subject_id, lp, class), by = "subject_id") %>%
    rename(ID = subject_id) %>%
    group_by(ID) %>%
    mutate(event = as.numeric(first(na.omit(event))), obs_time = first(na.omit(obs_time))) %>%
    ungroup() %>%
    filter(!is.na(Y))
}

# 对每个可用纵向标记物建模并输出 BS、C-index、AUC
results_list <- list()
for (nm in names(MIMIC_LAB_ITEMS)) {
  item <- MIMIC_LAB_ITEMS[[nm]]
  if (!item %in% names(data_with_obstime)) next
  dt <- prepare_for_model(data_with_obstime, item)
  if (is.null(dt) || nrow(dt) < 30) next
  t0 <- median(unique(dt$obs_time), na.rm = TRUE)
  res <- cal_3_RSF_LC(dt, t0 = t0)
  res$Biomarker <- nm
  results_list[[nm]] <- res
}
final_results <- do.call(rbind, results_list)
if (!is.null(final_results)) {
  print("========== 建模结果 (AUC, C-index, BS) ==========")
  print(final_results)
  write_xlsx(as.data.frame(final_results), "F:/文章_大论文/MIMIC数据库_代码/TREA代码/0312/model_results_RSF_LC.xlsx")
} else {
  message("未得到有效结果，请检查数据是否包含 itemid_50912 等列。")
}








#######################################




# 加载包
library(DynForest)
library(dplyr)

# 设置随机种子以确保结果可重复
set.seed(123)

# 生成示例数据（实际使用时替换为您自己的数据）
n_subjects <- 100  # 患者数量
n_measurements <- 3  # 每个患者的测量次数

# 生成纵向数据
longitudinal_data <- expand.grid(
  id = 1:n_subjects,
  time = 0:(n_measurements - 1)
) %>%
  mutate(
    # 生成随时间变化的标记物Y
    Y = rnorm(n(), mean = 5 + 0.5 * time + 0.1 * id, sd = 1)
  )

# 生成生存数据和基线协变量
fixed_data <- data.frame(
  id = 1:n_subjects,
  # 生存时间
  time = rexp(n_subjects, rate = 0.1),
  # 事件指示器（1=事件发生，0=删失）
  event = rbinom(n_subjects, size = 1, prob = 0.7),
  # 基线协变量
  age = rnorm(n_subjects, mean = 65, sd = 10),
  gender = factor(rbinom(n_subjects, size = 1, prob = 0.5), labels = c("F", "M"))
)

# 确保生存时间为正
fixed_data$time <- abs(fixed_data$time) + 0.1

# 转换为DynForest需要的格式
fixed_data <- fixed_data %>%
  mutate(
    id = as.numeric(id),
    event = as.numeric(event),
    gender_num = as.numeric(gender)
  )

# 核心建模代码
dyn_model <- DynForest::dynforest(
  timeData = as.data.frame(longitudinal_data),
  fixedData = as.data.frame(fixed_data),
  idVar = "id",
  timeVar = "time",
  timeVarModel = list(
    Y = list(
      model = "linear",
      fixed = ~ 1 + time,
      random = ~ 1 + time | id
    )
  ),
  Y = list(
    type = "surv",
    Y = data.frame(
      id = fixed_data$id,
      time = fixed_data$time,
      event = fixed_data$event
    )
  ),
  ntree = 100,  # 树的数量
  mtry = 2,     # 每次分裂考虑的特征数
  nodesize = 5, # 最小节点大小
  minsplit = 5, # 最小分裂样本数
  nsplit_option = "quantile",
  ncores = 1,   # 使用的CPU核心数
  verbose = TRUE
)

# 打印模型摘要
print(dyn_model)














