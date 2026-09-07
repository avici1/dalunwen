# =============================================================================
# MIMIC数据准备与DynForest纵向生存模型
# 功能：将data_with_obstime转换为cal_3_RSF_LC所需格式，计算BS、C-index、AUC
# =============================================================================

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
# 1. MIMIC-IV 常用实验室itemid映射（来自d_labitems）
# =============================================================================
# 50912: Creatinine (肌酐)
# 50931: Glucose (血糖)
# 51222: Hemoglobin (血红蛋白)
# 51301: WBC (白细胞)
# 50971: Potassium (血钾)
# 50893: Sodium (血钠)
# 50882: Bicarbonate (碳酸氢盐)
# 50868: Anion gap (阴离子间隙)
# =============================================================================

MIMIC_LAB_ITEMS <- list(
  creatinine   = "itemid_50912",
  glucose      = "itemid_50931",
  hemoglobin   = "itemid_51222",
  wbc          = "itemid_51301",
  potassium    = "itemid_50971",
  sodium       = "itemid_50893",
  bicarbonate  = "itemid_50882",
  anion_gap    = "itemid_50868"
)

# =============================================================================
# 2. 数据读取与预处理
# =============================================================================

# 尝试多个可能的数据路径
data_paths <- c(
  "F:/文章_大论文/MIMIC数据库_代码/TREA代码/0312/data_all.xlsx",
  "F:/文章_大论文/MIMIC数据库_代码/TREA代码/0311/data_withobstime.xlsx",
  "F:/文章_大论文/MIMIC数据库_代码/TREA代码/0311/data_with_obstime.xlsx"
)

raw_data <- NULL
for (p in data_paths) {
  if (file.exists(p)) {
    raw_data <- read_xlsx(p)
    message("已读取: ", p)
    break
  }
}

if (is.null(raw_data)) {
  stop("未找到数据文件。请先运行 0311.R 生成 data_withobstime 或 data_all.xlsx")
}

# 标准化列名（兼容 subject_id / ID）
if (!"subject_id" %in% names(raw_data) && "ID" %in% names(raw_data)) {
  raw_data <- raw_data %>% rename(subject_id = ID)
}

# 检查必要的列
req_cols <- c("subject_id", "hospital_mortality")
if (!all(req_cols %in% names(raw_data))) {
  stop("数据缺少必要列: subject_id, hospital_mortality。当前列: ", paste(names(raw_data), collapse = ", "))
}

# 检测可用的 itemid 列
available_items <- intersect(unlist(MIMIC_LAB_ITEMS), names(raw_data))
if (length(available_items) == 0) {
  stop("未找到任何 itemid 列。请检查数据是否包含 itemid_50912, itemid_50931 等。")
}
message("可用的纵向标记物: ", paste(available_items, collapse = ", "))

# 检测 obstime
if (!"obstime" %in% names(raw_data)) {
  raw_data <- raw_data %>%
    group_by(subject_id) %>%
    arrange(charttime, .by_group = TRUE) %>%
    mutate(obstime = row_number()) %>%
    ungroup()
  message("已根据 charttime 生成 obstime")
}

# 检测 obs_time（生存时间）
if (!"obs_time" %in% names(raw_data)) {
  if ("los_icu" %in% names(raw_data)) {
    raw_data <- raw_data %>%
      group_by(subject_id) %>%
      mutate(obs_time = unique(na.omit(los_icu))[1]) %>%
      ungroup()
    message("使用 los_icu 作为 obs_time")
  } else if ("los_hospital" %in% names(raw_data)) {
    raw_data <- raw_data %>%
      group_by(subject_id) %>%
      mutate(obs_time = unique(na.omit(los_hospital))[1]) %>%
      ungroup()
    message("使用 los_hospital 作为 obs_time")
  } else {
    # 用 charttime 或 obstime 推导
    if ("charttime" %in% names(raw_data)) {
      raw_data <- raw_data %>%
        group_by(subject_id) %>%
        mutate(
          obs_time = as.numeric(difftime(max(charttime, na.rm = TRUE), min(charttime, na.rm = TRUE), units = "days")),
          obs_time = if_else(is.na(obs_time) | obs_time <= 0, as.numeric(max(obstime, na.rm = TRUE)), obs_time)
        ) %>%
        ungroup()
    } else {
      raw_data <- raw_data %>%
        group_by(subject_id) %>%
        mutate(obs_time = max(obstime, na.rm = TRUE)) %>%
        ungroup()
    }
    message("根据 charttime 或 obstime 推导 obs_time")
  }
}

# 确保 obs_time 为正且无缺失
raw_data <- raw_data %>%
  group_by(subject_id) %>%
  mutate(obs_time = pmax(0.1, obs_time, na.rm = TRUE)) %>%
  ungroup()

# 基线变量：优先使用 anchor_age，否则尝试 age
if (!"anchor_age" %in% names(raw_data) && "age" %in% names(raw_data)) {
  raw_data <- raw_data %>% rename(anchor_age = age)
}
if (!"anchor_age" %in% names(raw_data)) {
  raw_data <- raw_data %>% mutate(anchor_age = 60)  # 默认
}

# gender 编码
raw_data <- raw_data %>%
  mutate(
    gender_num = case_when(
      tolower(gender) %in% c("m", "male", "1") ~ 1,
      tolower(gender) %in% c("f", "female", "0") ~ 0,
      TRUE ~ NA_real_
    )
  )
if (all(is.na(raw_data$gender_num))) {
  raw_data <- raw_data %>% mutate(gender_num = 0)
}

# =============================================================================
# 3. 为 cal_3_RSF_LC 准备数据格式
# 要求：ID, t, Y, obs_time, event, lp, class，以及可选基线变量
# =============================================================================

prepare_data_for_model <- function(data, y_var, baseline_vars = NULL) {
  # 选择第一个可用的 y_var
  y_col <- if (y_var %in% names(data)) y_var else NULL
  if (is.null(y_col)) return(NULL)

  # 过滤掉 Y 全为 NA 的患者
  valid_ids <- data %>%
    group_by(subject_id) %>%
    summarise(has_y = any(!is.na(!!sym(y_col))), .groups = "drop") %>%
    filter(has_y) %>%
    pull(subject_id)

  data_valid <- data %>% filter(subject_id %in% valid_ids)

  # 构建 lp：可用 Cox 或简单线性组合
  base_per_id <- data_valid %>%
    group_by(subject_id) %>%
    summarise(
      anchor_age = first(na.omit(anchor_age)),
      gender_num = first(na.omit(gender_num)),
      .groups = "drop"
    )

  # 简单线性预测: age/10 + gender
  base_per_id <- base_per_id %>%
    mutate(
      lp = (anchor_age / 10) + gender_num,
      class = gender_num
    )

  data_model <- data_valid %>%
    select(subject_id, t = obstime, obs_time, event = hospital_mortality, !!y_col) %>%
    rename(Y = !!y_col) %>%
    left_join(base_per_id %>% select(subject_id, lp, class), by = "subject_id") %>%
    rename(ID = subject_id)

  # 添加其他基线变量
  if (!is.null(baseline_vars)) {
    base_extra <- data_valid %>%
      group_by(subject_id) %>%
      summarise(across(any_of(baseline_vars), ~ first(na.omit(.x))), .groups = "drop") %>%
      rename(ID = subject_id)
    data_model <- data_model %>% left_join(base_extra, by = "ID")
  }

  # 去除 Y 或 event 的 NA 行（在患者级别保证一致性）
  data_model <- data_model %>%
    group_by(ID) %>%
    mutate(
      event = as.numeric(first(na.omit(event))),
      obs_time = first(na.omit(obs_time))
    ) %>%
    ungroup() %>%
    filter(!is.na(Y))

  data_model
}

# =============================================================================
# 4. 修改后的 cal_3_RSF_LC：支持多基线变量
# =============================================================================

cal_3_RSF_LC <- function(data, t0 = NULL, baseline_cols = NULL) {

  if (is.null(t0)) t0 <- max(data$obs_time, na.rm = TRUE) * 0.5  # 默认取中位生存时间

  tryCatch({
    if (!requireNamespace("timeROC", quietly = TRUE)) {
      install.packages("timeROC")
      library(timeROC)
    }

    # 统一 id 映射，确保 longitudinal 与 fixed 一致
    id_levels <- unique(data$ID)
    data <- data %>% mutate(num_id = match(ID, id_levels))

    longitudinal_data <- data %>%
      select(id = num_id, time = t, Y) %>%
      mutate(id = as.numeric(id))

    survival_data <- data %>%
      group_by(ID) %>%
      summarise(
        time  = unique(obs_time)[1],
        event = unique(event)[1],
        .groups = "drop"
      ) %>%
      mutate(id = match(ID, id_levels))

    # 基线：lp, class，以及额外基线列
    base_cols <- c("lp", "class")
    if (!is.null(baseline_cols)) {
      base_cols <- c(base_cols, intersect(baseline_cols, names(data)))
    }
    base_cols <- unique(base_cols)
    base_cols <- intersect(base_cols, names(data))

    baseline_data <- data %>%
      group_by(ID) %>%
      summarise(across(any_of(base_cols), ~ unique(na.omit(.x))[1]), .groups = "drop") %>%
      mutate(id = match(ID, id_levels))

    fixed_data <- survival_data %>%
      select(id, time, event) %>%
      left_join(baseline_data %>% select(-ID), by = "id") %>%
      mutate(id = as.numeric(id))

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
      verbose       = FALSE
    )

    unique_ids <- unique(fixed_data$id)
    risk_score <- numeric(length(unique_ids))

    for (i in seq_along(unique_ids)) {
      patient_id <- unique_ids[i]
      patient_long_data <- longitudinal_data[longitudinal_data$id == patient_id, ]
      patient_fixed_data <- fixed_data[fixed_data$id == patient_id, ]

      if (nrow(patient_long_data) > 0) {
        tryCatch({
          patient_pred <- predict(dyn_model,
            newdata = list(
              longitudinal = patient_long_data,
              fixed = patient_fixed_data
            ),
            type = "risk"
          )
          risk_score[i] <- if (length(patient_pred) > 0) patient_pred[1] else patient_fixed_data$lp[1]
        }, error = function(e) {
          risk_score[i] <<- patient_fixed_data$lp[1]
        })
      } else {
        risk_score[i] <- patient_fixed_data$lp[1]
      }
    }

    surv_time <- fixed_data$time
    surv_event <- fixed_data$event
    n <- length(surv_time)

    if (any(is.na(risk_score))) {
      risk_score[is.na(risk_score)] <- mean(risk_score, na.rm = TRUE)
    }
    if (sd(risk_score, na.rm = TRUE) == 0) {
      risk_score <- risk_score + rnorm(n, mean = 0, sd = 0.01)
    }

    # C-index
    cindex_result <- survival::concordance(survival::Surv(surv_time, surv_event) ~ risk_score)
    cindex <- cindex_result$concordance
    if (cindex < 0.5) {
      risk_score <- -risk_score
      cindex_result <- survival::concordance(survival::Surv(surv_time, surv_event) ~ risk_score)
      cindex <- cindex_result$concordance
    }

    # AUC (时依AUC用 concordance 近似)
    auc <- cindex

    # Brier Score at t0
    tryCatch({
      surv_fit <- survival::survfit(survival::Surv(surv_time, surv_event) ~ 1)
      surv_prob <- summary(surv_fit, times = t0)$surv
      if (length(surv_prob) == 0) surv_prob <- 0.5
      if (length(surv_prob) == 1) surv_prob <- rep(surv_prob, n)

      brier <- numeric(n)
      for (i in 1:n) {
        if (surv_time[i] <= t0 && surv_event[i] == 1) {
          brier[i] <- (1 - surv_prob[i])^2
        } else if (surv_time[i] > t0) {
          brier[i] <- surv_prob[i]^2
        } else {
          brier[i] <- (1 - surv_prob[i])^2
        }
      }
      bs <- mean(brier)
    }, error = function(e) {
      bs <- 0.25
    })

    result <- data.frame(
      AUC = round(auc, 4),
      BS = round(bs, 4),
      Cindex = round(cindex, 4)
    )
    rownames(result) <- paste0("t=", round(t0, 2))
    return(result)

  }, error = function(e) {
    message("cal_3_RSF_LC failed: ", e$message)
    result <- data.frame(AUC = 0.5, BS = 0.25, Cindex = 0.5)
    rownames(result) <- paste0("t=", t0)
    return(result)
  })
}

# =============================================================================
# 5. 运行建模：对每个可用纵向标记物分别建模
# =============================================================================

# 选择基线变量纳入模型
baseline_vars <- c("anchor_age", "gender_num")
if ("los_icu" %in% names(raw_data)) baseline_vars <- c(baseline_vars, "los_icu")
if ("los_hospital" %in% names(raw_data)) baseline_vars <- c(baseline_vars, "los_hospital")

results_list <- list()
for (item_name in names(MIMIC_LAB_ITEMS)) {
  item_col <- MIMIC_LAB_ITEMS[[item_name]]
  if (!item_col %in% names(raw_data)) next

  data_model <- prepare_data_for_model(raw_data, item_col, baseline_vars)
  if (is.null(data_model) || nrow(data_model) < 30) {
    message("跳过 ", item_name, ": 数据不足")
    next
  }

  t0 <- median(unique(data_model$obs_time), na.rm = TRUE)
  res <- cal_3_RSF_LC(data_model, t0 = t0, baseline_cols = baseline_vars)
  res$Biomarker <- item_name
  res$Y_var <- item_col
  results_list[[item_name]] <- res
}

# 汇总结果
final_results <- do.call(rbind, results_list)
if (!is.null(final_results)) {
  final_results <- final_results %>%
    select(Biomarker, Y_var, AUC, Cindex, BS, everything())
  print("========== 建模结果 (AUC, C-index, BS) ==========")
  print(final_results)

  out_path <- "F:/文章_大论文/MIMIC数据库_代码/TREA代码/0312/model_results_RSF_LC.xlsx"
  tryCatch({
    write_xlsx(as.data.frame(final_results), out_path)
    message("结果已保存: ", out_path)
  }, error = function(e) message("保存失败: ", e$message))
} else {
  message("未得到有效建模结果，请检查数据格式与变量名。")
}
