# ==============================================================================
# 生存分析模型比较：分块代码（Friedman + 删失率Wilcoxon）
# ==============================================================================

library(readxl)
library(tidyr)
library(dplyr)
library(PMCMRplus)
library(writexl)

# 公用数据读取与基础因子化 ------------------------------------------------
input_file <- "F:/文章_大论文/结果整理/代码/RESULT_ALL2.XLSX"
df_raw <- read_xlsx(input_file)

df_processed <- df_raw %>%
  select(var, 模型, 样本量, 删失率, 变量数, 相关性, 相关特性, 潜类别,
         AUC_mean, Cindex_mean, BS_mean) %>%
  mutate(
    删失率 = factor(删失率, levels = c(30, 70), labels = c("Cens30", "Cens70")),
    样本量 = factor(样本量, levels = c(500, 1000), labels = c("N500", "N1000")),
    变量数 = factor(变量数, levels = c(4, 10), labels = c("V4", "V10")),
    模型   = factor(模型, levels = c("cox", "jm", "rsf", "rsflc"), 
                  labels = c("COX", "JM", "RSF", "RSFLC"))
  )

models_order <- c("COX", "JM", "RSF", "RSFLC")


# #############################################################
# ##### 1. 总体 Friedman 检验（四模型比较，基于删失率差值）#####
# #############################################################

### 1.1 数据预处理：构造删失率差值宽表 ###
group_vars_censor <- c("样本量", "变量数", "相关性", "相关特性", "潜类别")

df_censor_paired <- df_processed %>%
  arrange(!!!syms(group_vars_censor), 模型, 删失率) %>%
  group_by(!!!syms(group_vars_censor), 模型) %>%
  summarise(
    AUC_Cens30    = AUC_mean[删失率 == "Cens30"],
    AUC_Cens70    = AUC_mean[删失率 == "Cens70"],
    Cindex_Cens30 = Cindex_mean[删失率 == "Cens30"],
    Cindex_Cens70 = Cindex_mean[删失率 == "Cens70"],
    BS_Cens30     = BS_mean[删失率 == "Cens30"],
    BS_Cens70     = BS_mean[删失率 == "Cens70"],
    .groups = "drop"
  ) %>%
  mutate(
    AUC_Diff    = AUC_Cens30 - AUC_Cens70,
    Cindex_Diff = Cindex_Cens30 - Cindex_Cens70,
    BS_Diff     = BS_Cens30 - BS_Cens70
  ) %>%
  group_by(!!!syms(group_vars_censor)) %>%
  mutate(Pair_ID = cur_group_id()) %>%
  ungroup()

# 转换为Friedman检验需要的宽格式（每行一个配对组，列是四个模型的差值）
df_auc_diff_wide <- df_censor_paired %>%
  select(Pair_ID, 模型, AUC_Diff) %>%
  pivot_wider(id_cols = Pair_ID, names_from = 模型, values_from = AUC_Diff,
              names_prefix = "AUC_Diff_")

df_cindex_diff_wide <- df_censor_paired %>%
  select(Pair_ID, 模型, Cindex_Diff) %>%
  pivot_wider(id_cols = Pair_ID, names_from = 模型, values_from = Cindex_Diff,
              names_prefix = "Cindex_Diff_")

df_bs_diff_wide <- df_censor_paired %>%
  select(Pair_ID, 模型, BS_Diff) %>%
  pivot_wider(id_cols = Pair_ID, names_from = 模型, values_from = BS_Diff,
              names_prefix = "BS_Diff_")

### 1.2 执行 Friedman 检验与 Nemenyi 事后检验 ###
run_friedman <- function(df_wide, metric_name) {
  mat <- as.matrix(df_wide[, -1])
  fried <- friedman.test(mat)
  n <- nrow(mat)
  k <- ncol(mat)
  W <- fried$statistic / (n * (k - 1))
  
  # 长格式数据，用于事后检验
  df_long <- df_wide %>%
    pivot_longer(cols = -Pair_ID, names_to = "模型", values_to = "Diff",
                 names_prefix = paste0(metric_name, "_Diff_")) %>%
    mutate(模型 = factor(模型, levels = models_order))
  
  # Nemenyi 事后检验
  nemeyi <- frdAllPairsNemenyiTest(y = df_long$Diff, groups = df_long$模型, 
                                   blocks = df_long$Pair_ID)
  p_mat <- as.matrix(nemeyi$p.value)
  
  # 补全为4x4对称矩阵
  full_p <- matrix(NA, 4, 4, dimnames = list(models_order, models_order))
  rn <- rownames(p_mat)
  cn <- colnames(p_mat)
  for (i in 1:4) {
    for (j in 1:4) {
      if (i == j) {
        full_p[i, j] <- 1
      } else {
        val <- NA
        if (models_order[i] %in% rn && models_order[j] %in% cn) {
          val <- p_mat[models_order[i], models_order[j]]
        } else if (models_order[j] %in% rn && models_order[i] %in% cn) {
          val <- p_mat[models_order[j], models_order[i]]
        }
        full_p[i, j] <- val
      }
    }
  }
  # 对称补全
  for (i in 1:4) {
    for (j in 1:4) {
      if (i != j && is.na(full_p[i, j]) && !is.na(full_p[j, i])) {
        full_p[i, j] <- full_p[j, i]
      }
    }
  }
  
  list(friedman = fried, W = W, p_matrix = full_p, df_long = df_long)
}

res_auc    <- run_friedman(df_auc_diff_wide,    "AUC")
res_cindex <- run_friedman(df_cindex_diff_wide, "Cindex")
res_bs     <- run_friedman(df_bs_diff_wide,     "BS")

### 1.3 输出：Friedman主效应表 + 事后检验表 ###
table_friedman_main <- data.frame(
  Metric = c("AUC", "C-index", "Brier Score"),
  Chi_square = round(c(res_auc$friedman$statistic, 
                       res_cindex$friedman$statistic, 
                       res_bs$friedman$statistic), 2),
  df = c(res_auc$friedman$parameter, 
         res_cindex$friedman$parameter, 
         res_bs$friedman$parameter),
  p_value = format.pval(c(res_auc$friedman$p.value,
                          res_cindex$friedman$p.value,
                          res_bs$friedman$p.value), digits = 3),
  Kendall_W = round(c(res_auc$W, res_cindex$W, res_bs$W), 3)
)

# 事后两两比较表（含p值与显著性）
comparisons <- c("COX vs JM", "COX vs RSF", "COX vs RSFLC", 
                 "JM vs RSF", "JM vs RSFLC", "RSF vs RSFLC")

get_p <- function(mat, m1, m2) {
  if (m1 %in% rownames(mat) && m2 %in% colnames(mat)) return(mat[m1, m2])
  if (m2 %in% rownames(mat) && m1 %in% colnames(mat)) return(mat[m2, m1])
  return(NA)
}
format_p_stars <- function(p) {
  if (is.na(p)) return(NA)
  stars <- if(p < 0.001) "***" else if(p < 0.01) "**" else if(p < 0.05) "*" else "ns"
  p_str <- if(p < 0.001) "<0.001" else sprintf("%.3f", p)
  paste0(p_str, " (", stars, ")")
}

table_posthoc <- data.frame(
  Comparison = comparisons,
  AUC = sapply(comparisons, function(s) {
    m <- strsplit(s, " vs ")[[1]]
    format_p_stars(get_p(res_auc$p_matrix, m[1], m[2]))
  }),
  Cindex = sapply(comparisons, function(s) {
    m <- strsplit(s, " vs ")[[1]]
    format_p_stars(get_p(res_cindex$p_matrix, m[1], m[2]))
  }),
  BS = sapply(comparisons, function(s) {
    m <- strsplit(s, " vs ")[[1]]
    format_p_stars(get_p(res_bs$p_matrix, m[1], m[2]))
  })
)


# #############################################################
# ##### 2. 删失率配对比较（Wilcoxon 检验，30% vs 70%）#####
# #############################################################

### 2.1 数据预处理：构造删失率配对宽表（原始值）###
# 直接使用之前生成的 df_censor_paired，提取原始值宽表
df_wide_censor <- df_censor_paired %>%
  select(Pair_ID, 模型,
         AUC_mean_Cens30 = AUC_Cens30, AUC_mean_Cens70 = AUC_Cens70,
         Cindex_mean_Cens30 = Cindex_Cens30, Cindex_mean_Cens70 = Cindex_Cens70,
         BS_mean_Cens30 = BS_Cens30, BS_mean_Cens70 = BS_Cens70)

### 2.2 执行 Wilcoxon 配对检验 ###
wilcoxon_results <- data.frame(
  Metric = character(), Model = character(),
  Median_diff = numeric(), p_value = character()
)

for(m in models_order) {
  data_m <- df_wide_censor %>% filter(模型 == m)
  
  # AUC
  wt <- wilcox.test(data_m$AUC_mean_Cens30, data_m$AUC_mean_Cens70, paired = TRUE, exact = FALSE)
  wilcoxon_results <- rbind(wilcoxon_results, data.frame(
    Metric = "AUC", Model = m,
    Median_diff = round(median(data_m$AUC_mean_Cens30 - data_m$AUC_mean_Cens70, na.rm = TRUE), 4),
    p_value = format.pval(wt$p.value, digits = 3)
  ))
  
  # C-index
  wt <- wilcox.test(data_m$Cindex_mean_Cens30, data_m$Cindex_mean_Cens70, paired = TRUE, exact = FALSE)
  wilcoxon_results <- rbind(wilcoxon_results, data.frame(
    Metric = "C-index", Model = m,
    Median_diff = round(median(data_m$Cindex_mean_Cens30 - data_m$Cindex_mean_Cens70, na.rm = TRUE), 4),
    p_value = format.pval(wt$p.value, digits = 3)
  ))
  
  # Brier Score
  wt <- wilcox.test(data_m$BS_mean_Cens30, data_m$BS_mean_Cens70, paired = TRUE, exact = FALSE)
  wilcoxon_results <- rbind(wilcoxon_results, data.frame(
    Metric = "BS", Model = m,
    Median_diff = round(median(data_m$BS_mean_Cens30 - data_m$BS_mean_Cens70, na.rm = TRUE), 4),
    p_value = format.pval(wt$p.value, digits = 3)
  ))
}

### 2.3 输出：删失率配对比较表 ###
table_wilcoxon_censor <- wilcoxon_results %>%
  mutate(
    diff = sprintf("%+.4f", Median_diff),
    P = p_value
  ) %>%
  select(Model, 指标 = Metric, diff, P)


# #############################################################
# ##### 3.样本量配对比较（N500 vs N1000） #####
# #############################################################
group_vars_sample <- c("删失率", "变量数", "相关性", "相关特性", "潜类别")

df_sample_paired <- df_processed %>%
  arrange(!!!syms(group_vars_sample), 模型, 样本量) %>%
  group_by(!!!syms(group_vars_sample), 模型) %>%
  summarise(
    AUC_N500    = AUC_mean[样本量 == "N500"],
    AUC_N1000   = AUC_mean[样本量 == "N1000"],
    Cindex_N500 = Cindex_mean[样本量 == "N500"],
    Cindex_N1000= Cindex_mean[样本量 == "N1000"],
    BS_N500     = BS_mean[样本量 == "N500"],
    BS_N1000    = BS_mean[样本量 == "N1000"],
    .groups = "drop"
  ) %>%
  group_by(!!!syms(group_vars_sample)) %>%
  mutate(Pair_ID_sample = cur_group_id()) %>%
  ungroup()

df_wide_sample <- df_sample_paired %>%
  select(Pair_ID_sample, 模型,
         AUC_mean_N500 = AUC_N500, AUC_mean_N1000 = AUC_N1000,
         Cindex_mean_N500 = Cindex_N500, Cindex_mean_N1000 = Cindex_N1000,
         BS_mean_N500 = BS_N500, BS_mean_N1000 = BS_N1000)

# Wilcoxon检验
wilcoxon_sample_results <- data.frame(
  Metric = character(), Model = character(),
  Median_diff = numeric(), p_value = character()
)

for(m in models_order) {
  data_m <- df_wide_sample %>% filter(模型 == m)
  if(nrow(data_m) == 0) next
  
  # AUC
  wt <- wilcox.test(data_m$AUC_mean_N500, data_m$AUC_mean_N1000, paired = TRUE, exact = FALSE)
  wilcoxon_sample_results <- rbind(wilcoxon_sample_results, data.frame(
    Metric = "AUC", Model = m,
    Median_diff = round(median(data_m$AUC_mean_N500 - data_m$AUC_mean_N1000, na.rm = TRUE), 4),
    p_value = format.pval(wt$p.value, digits = 3)
  ))
  
  # C-index
  wt <- wilcox.test(data_m$Cindex_mean_N500, data_m$Cindex_mean_N1000, paired = TRUE, exact = FALSE)
  wilcoxon_sample_results <- rbind(wilcoxon_sample_results, data.frame(
    Metric = "C-index", Model = m,
    Median_diff = round(median(data_m$Cindex_mean_N500 - data_m$Cindex_mean_N1000, na.rm = TRUE), 4),
    p_value = format.pval(wt$p.value, digits = 3)
  ))
  
  # BS
  wt <- wilcox.test(data_m$BS_mean_N500, data_m$BS_mean_N1000, paired = TRUE, exact = FALSE)
  wilcoxon_sample_results <- rbind(wilcoxon_sample_results, data.frame(
    Metric = "BS", Model = m,
    Median_diff = round(median(data_m$BS_mean_N500 - data_m$BS_mean_N1000, na.rm = TRUE), 4),
    p_value = format.pval(wt$p.value, digits = 3)
  ))
}

# 输出表格
table_wilcoxon_sample <- wilcoxon_sample_results %>%
  mutate(
    diff = sprintf("%+.4f", Median_diff),
    P = p_value
  ) %>%
  select(Model, 指标 = Metric, diff, P)




# #############################################################
# ##### 4.相关特性配对比较（BTW vs INTER） #####
# #############################################################

### 相关特性配对比较（BTW vs INTER）###
# 数据预处理：控制其他变量，配对 BTW 与 INTER
group_vars_cortype <- c("样本量", "删失率", "变量数", "相关性", "潜类别")

df_cortype_paired <- df_processed %>%
  arrange(!!!syms(group_vars_cortype), 模型, 相关特性) %>%
  group_by(!!!syms(group_vars_cortype), 模型) %>%
  summarise(
    AUC_BTW    = AUC_mean[相关特性 == "BTW"],
    AUC_INTER  = AUC_mean[相关特性 == "INTER"],
    Cindex_BTW = Cindex_mean[相关特性 == "BTW"],
    Cindex_INTER = Cindex_mean[相关特性 == "INTER"],
    BS_BTW     = BS_mean[相关特性 == "BTW"],
    BS_INTER   = BS_mean[相关特性 == "INTER"],
    .groups = "drop"
  ) %>%
  group_by(!!!syms(group_vars_cortype)) %>%
  mutate(Pair_ID_cortype = cur_group_id()) %>%
  ungroup()

df_wide_cortype <- df_cortype_paired %>%
  select(Pair_ID_cortype, 模型,
         AUC_mean_BTW = AUC_BTW, AUC_mean_INTER = AUC_INTER,
         Cindex_mean_BTW = Cindex_BTW, Cindex_mean_INTER = Cindex_INTER,
         BS_mean_BTW = BS_BTW, BS_mean_INTER = BS_INTER)

# Wilcoxon检验
wilcoxon_cortype_results <- data.frame(
  Metric = character(), Model = character(),
  Median_diff = numeric(), p_value = character()
)

for(m in models_order) {
  data_m <- df_wide_cortype %>% filter(模型 == m)
  if(nrow(data_m) == 0) next
  
  # AUC
  wt <- wilcox.test(data_m$AUC_mean_BTW, data_m$AUC_mean_INTER, paired = TRUE, exact = FALSE)
  wilcoxon_cortype_results <- rbind(wilcoxon_cortype_results, data.frame(
    Metric = "AUC", Model = m,
    Median_diff = round(median(data_m$AUC_mean_BTW - data_m$AUC_mean_INTER, na.rm = TRUE), 4),
    p_value = format.pval(wt$p.value, digits = 3)
  ))
  
  # C-index
  wt <- wilcox.test(data_m$Cindex_mean_BTW, data_m$Cindex_mean_INTER, paired = TRUE, exact = FALSE)
  wilcoxon_cortype_results <- rbind(wilcoxon_cortype_results, data.frame(
    Metric = "C-index", Model = m,
    Median_diff = round(median(data_m$Cindex_mean_BTW - data_m$Cindex_mean_INTER, na.rm = TRUE), 4),
    p_value = format.pval(wt$p.value, digits = 3)
  ))
  
  # BS
  wt <- wilcox.test(data_m$BS_mean_BTW, data_m$BS_mean_INTER, paired = TRUE, exact = FALSE)
  wilcoxon_cortype_results <- rbind(wilcoxon_cortype_results, data.frame(
    Metric = "BS", Model = m,
    Median_diff = round(median(data_m$BS_mean_BTW - data_m$BS_mean_INTER, na.rm = TRUE), 4),
    p_value = format.pval(wt$p.value, digits = 3)
  ))
}

# 输出表格
table_wilcoxon_cortype <- wilcoxon_cortype_results %>%
  mutate(
    diff = sprintf("%+.4f", Median_diff),
    P = p_value
  ) %>%
  select(Model, 指标 = Metric, diff, P)






# #############################################################
# ##### 5.潜类别配对比较（1C vs 3C） #####
# #############################################################
### 潜类别配对比较（1C vs 3C）###
# 数据预处理：控制其他变量，配对 1C 与 3C
group_vars_latent <- c("样本量", "删失率", "变量数", "相关性", "相关特性")

df_latent_paired <- df_processed %>%
  arrange(!!!syms(group_vars_latent), 模型, 潜类别) %>%
  group_by(!!!syms(group_vars_latent), 模型) %>%
  summarise(
    AUC_1C    = AUC_mean[潜类别 == "1C"],
    AUC_3C    = AUC_mean[潜类别 == "3C"],
    Cindex_1C = Cindex_mean[潜类别 == "1C"],
    Cindex_3C = Cindex_mean[潜类别 == "3C"],
    BS_1C     = BS_mean[潜类别 == "1C"],
    BS_3C     = BS_mean[潜类别 == "3C"],
    .groups = "drop"
  ) %>%
  group_by(!!!syms(group_vars_latent)) %>%
  mutate(Pair_ID_latent = cur_group_id()) %>%
  ungroup()

df_wide_latent <- df_latent_paired %>%
  select(Pair_ID_latent, 模型,
         AUC_mean_1C = AUC_1C, AUC_mean_3C = AUC_3C,
         Cindex_mean_1C = Cindex_1C, Cindex_mean_3C = Cindex_3C,
         BS_mean_1C = BS_1C, BS_mean_3C = BS_3C)

# Wilcoxon检验
wilcoxon_latent_results <- data.frame(
  Metric = character(), Model = character(),
  Median_diff = numeric(), p_value = character()
)

for(m in models_order) {
  data_m <- df_wide_latent %>% filter(模型 == m)
  if(nrow(data_m) == 0) next
  
  # AUC
  wt <- wilcox.test(data_m$AUC_mean_1C, data_m$AUC_mean_3C, paired = TRUE, exact = FALSE)
  wilcoxon_latent_results <- rbind(wilcoxon_latent_results, data.frame(
    Metric = "AUC", Model = m,
    Median_diff = round(median(data_m$AUC_mean_1C - data_m$AUC_mean_3C, na.rm = TRUE), 4),
    p_value = format.pval(wt$p.value, digits = 3)
  ))
  
  # C-index
  wt <- wilcox.test(data_m$Cindex_mean_1C, data_m$Cindex_mean_3C, paired = TRUE, exact = FALSE)
  wilcoxon_latent_results <- rbind(wilcoxon_latent_results, data.frame(
    Metric = "C-index", Model = m,
    Median_diff = round(median(data_m$Cindex_mean_1C - data_m$Cindex_mean_3C, na.rm = TRUE), 4),
    p_value = format.pval(wt$p.value, digits = 3)
  ))
  
  # BS
  wt <- wilcox.test(data_m$BS_mean_1C, data_m$BS_mean_3C, paired = TRUE, exact = FALSE)
  wilcoxon_latent_results <- rbind(wilcoxon_latent_results, data.frame(
    Metric = "BS", Model = m,
    Median_diff = round(median(data_m$BS_mean_1C - data_m$BS_mean_3C, na.rm = TRUE), 4),
    p_value = format.pval(wt$p.value, digits = 3)
  ))
}

# 输出表格：长格式（Model, 指标, diff, P）
table_wilcoxon_latent <- wilcoxon_latent_results %>%
  mutate(
    diff = sprintf("%+.4f", Median_diff),
    P = p_value
  ) %>%
  select(Model, 指标 = Metric, diff, P)

# #############################################################
# ##### 6.相关性(low mid high) #####
# #############################################################
### 相关性三水平比较（low / mid / high）- 修正版 ###
# 数据预处理：控制其他变量，构造三水平宽表
group_vars_corr <- c("样本量", "删失率", "变量数", "相关特性", "潜类别", "模型")

df_corr_wide <- df_processed %>%
  select(!!!syms(group_vars_corr), 相关性, AUC_mean, Cindex_mean, BS_mean) %>%
  pivot_wider(
    id_cols = all_of(group_vars_corr),
    names_from = 相关性,
    values_from = c(AUC_mean, Cindex_mean, BS_mean),
    names_sep = "_"
  ) %>%
  group_by(样本量, 删失率, 变量数, 相关特性, 潜类别, 模型) %>%
  mutate(Pair_ID_corr = cur_group_id()) %>%
  ungroup()

# 检验函数：对单个模型在三个相关性水平上进行 Friedman 检验
run_friedman_corr_model <- function(data, model_name, metric_prefix) {
  df_model <- data %>% filter(模型 == model_name)
  if(nrow(df_model) < 3) return(NULL)
  
  # 三列数据
  col_low  <- paste0(metric_prefix, "_low")
  col_mid  <- paste0(metric_prefix, "_mid")
  col_high <- paste0(metric_prefix, "_high")
  
  mat <- as.matrix(df_model[, c(col_low, col_mid, col_high)])
  fried <- friedman.test(mat)
  n <- nrow(mat)
  W <- fried$statistic / (n * 2)   # k=3, 自由度 k-1=2
  
  # 长格式用于事后检验
  df_long <- df_model %>%
    select(Pair_ID_corr, 
           low  = all_of(col_low),
           mid  = all_of(col_mid),
           high = all_of(col_high)) %>%
    pivot_longer(cols = c(low, mid, high), 
                 names_to = "相关性", values_to = "Value") %>%
    mutate(相关性 = factor(相关性, levels = c("low", "mid", "high")))
  
  # Nemenyi 事后检验
  nemeyi <- frdAllPairsNemenyiTest(y = df_long$Value, groups = df_long$相关性, 
                                   blocks = df_long$Pair_ID_corr)
  p_mat <- as.matrix(nemeyi$p.value)
  
  list(friedman = fried, W = W, p_matrix = p_mat)
}

# 执行检验（分模型、分指标）
models_order <- c("COX", "JM", "RSF", "RSFLC")
metrics <- c("AUC_mean", "Cindex_mean", "BS_mean")

corr_list <- list()

for(m in models_order) {
  for(met in metrics) {
    res <- run_friedman_corr_model(df_corr_wide, m, met)
    if(is.null(res)) next
    
    p_mat <- res$p_matrix
    
    # 安全提取 p 值：行列名可能是 "low","mid","high"
    get_p <- function(mat, r, c) {
      if(r %in% rownames(mat) && c %in% colnames(mat)) return(mat[r, c])
      else if(c %in% rownames(mat) && r %in% colnames(mat)) return(mat[c, r])
      else return(NA)
    }
    
    p_low_mid  <- get_p(p_mat, "low", "mid")
    p_low_high <- get_p(p_mat, "low", "high")
    p_mid_high <- get_p(p_mat, "mid", "high")
    
    one_row <- data.frame(
      Model = m,
      Metric = met,
      Friedman_Chi = round(res$friedman$statistic, 2),
      Friedman_df = res$friedman$parameter,
      Friedman_p = format.pval(res$friedman$p.value, digits = 3),
      Kendall_W = round(res$W, 3),
      p_low_vs_mid = p_low_mid,
      p_low_vs_high = p_low_high,
      p_mid_vs_high = p_mid_high,
      stringsAsFactors = FALSE
    )
    corr_list[[length(corr_list) + 1]] <- one_row
  }
}

corr_results <- do.call(rbind, corr_list)

# 格式化 p 值并添加星号
format_p_stars <- function(p) {
  if(is.na(p)) return(NA)
  stars <- if(p < 0.001) "***" else if(p < 0.01) "**" else if(p < 0.05) "*" else "ns"
  p_str <- if(p < 0.001) "<0.001" else sprintf("%.3f", p)
  paste0(p_str, " (", stars, ")")
}

corr_results$`low vs mid`  <- sapply(corr_results$p_low_vs_mid,  format_p_stars)
corr_results$`low vs high` <- sapply(corr_results$p_low_vs_high, format_p_stars)
corr_results$`mid vs high` <- sapply(corr_results$p_mid_vs_high, format_p_stars)

# 整理最终表格（长格式分指标，或保持宽格式随你）
table_corr_friedman <- corr_results %>%
  select(Model, Metric, 
         Chi_square = Friedman_Chi, df = Friedman_df, 
         p_value = Friedman_p, Kendall_W, 
         `low vs mid`, `low vs high`, `mid vs high`)





# #############################################################
# ##### 7. 导出所有结果至 Excel #####
# #############################################################

write_xlsx(
  list(
    # Friedman 检验相关
    Friedman_主效应       = table_friedman_main,
    Friedman_事后检验     = table_posthoc,
    
    # Wilcoxon 配对比较
    Wilcoxon_删失率比较   = table_wilcoxon_censor,
    Wilcoxon_样本量比较   = table_wilcoxon_sample,
    Wilcoxon_相关特性比较 = table_wilcoxon_cortype,
    Wilcoxon_潜类别比较   = table_wilcoxon_latent,
    
    # 相关性三水平 Friedman 检验
    Friedman_相关性三水平 = table_corr_friedman
  ),
  path = "F:/文章_大论文/结果整理/论文核心结果表.xlsx"
)

cat("所有结果表已输出至：论文核心结果表.xlsx\n")
