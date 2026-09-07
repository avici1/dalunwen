library(readxl)
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
lab<-read_xlsx("F:/文章_大论文/MIMIC数据库_代码/TREA代码/0310/stroke_lab2.xlsx")
baseline<-read_xlsx("F:/文章_大论文/MIMIC数据库_代码/TREA代码/0310/stroke_baseline_simple.xlsx")

head(lab)
head(baseline)

lab_wide <- lab %>%
  pivot_wider(
    id_cols = labevent_id,
    names_from = itemid,
    values_from = value
  )



lab_wide <- lab %>%
  pivot_wider(
    id_cols = c(labevent_id, subject_id, charttime, storetime),
    names_from = itemid,
    values_from = value,
    names_prefix = "itemid_",
    values_fn = list(value = first)  # 保留第一个值
  )

head(lab_wide)





lab_wide2 <- lab_wide %>%
  group_by(subject_id, charttime) %>%
  summarise(
    across(starts_with("itemid_"), ~first(na.omit(.))),
    .groups = "drop"
  )

head(lab_wide2)


lab_wide_summary <- lab_wide2 %>%
  pivot_longer(
    cols = starts_with("itemid_"),
    names_to = "item",
    values_to = "value"
  ) %>%
  filter(!is.na(value)) %>%  # 只保留有值的检查
  group_by(subject_id) %>%
  summarise(
    exam_count = n(),  # 总检查次数
    unique_exams = n_distinct(item),  # 不同的检查类型数量
    .groups = "drop"
  )

head(lab_wide2)

########################################
missing_rate <- lab_wide2 %>%
  summarise(across(starts_with("itemid_"), ~mean(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "itemid", values_to = "missing_rate") %>%
  arrange(desc(missing_rate))

# 查看缺失率分布
print(missing_rate)

# 可视化缺失率
missing_rate %>%
  ggplot(aes(x = missing_rate)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "black") +
  labs(title = "指标缺失率分布",
       x = "缺失率",
       y = "指标数量")

# 定义筛选阈值
high_missing_threshold <- 0.8  # 缺失率 > 80% 的指标考虑删除
medium_missing_threshold <- 0.5 # 缺失率 50%-80% 的指标可能需要填补
low_missing_threshold <- 0.3    # 缺失率 < 30% 的指标保留

# 识别不同缺失程度的指标
item_missing_class <- missing_rate %>%
  mutate(
    missing_class = case_when(
      missing_rate > high_missing_threshold ~ "drop",           # 丢弃
      missing_rate > medium_missing_threshold ~ "consider_fill", # 考虑填补
      missing_rate > low_missing_threshold ~ "fill",            # 可以填补
      TRUE ~ "keep"                                             # 保留
    )
  )















patient_completeness <- lab_wide2 %>%
  rowwise() %>%
  mutate(
    non_na_count = sum(!is.na(c_across(starts_with("itemid_")))),
    total_possible = ncol(select(., starts_with("itemid_"))),
    completeness_rate = non_na_count / total_possible
  ) %>%
  ungroup() %>%
  group_by(subject_id) %>%
  summarise(
    avg_completeness = mean(completeness_rate),
    min_completeness = min(completeness_rate),
    records_count = n(),
    .groups = "drop"
  )

# 查看患者完整度分布
summary(patient_completeness$avg_completeness)

# 定义患者筛选阈值
patient_threshold <- 0.3  # 平均完整度 < 30% 的患者考虑排除



patients_to_keep <- patient_completeness %>%
  filter(avg_completeness >= 0.3) %>%
  pull(subject_id)

lab_wide_filtered <- lab_wide2 %>%
  filter(subject_id %in% patients_to_keep)

head(lab_wide_filtered)


####################数据添补#####################

# 安装并加载必要的包
# install.packages("missForest")
# install.packages("doParallel")  # 可选，用于并行计算

library(missForest)
library(dplyr)
library(doParallel)

# 步骤1：准备数据 - 只保留需要填补的itemid列
# 将字符型转换为数值型
item_cols <- grep("^itemid_", names(lab_wide_filtered), value = TRUE)

# 创建只包含数值列的数据框
data_for_imputation <- lab_wide_filtered %>%
  select(all_of(item_cols)) %>%
  mutate(across(everything(), as.numeric))  # 将字符转换为数值

# 查看转换后的数据结构
str(data_for_imputation)
summary(data_for_imputation)

# 步骤2：检查缺失率
missing_rate <- colMeans(is.na(data_for_imputation))
cat("各列缺失率：\n")
print(sort(missing_rate, decreasing = TRUE))

# 步骤3：执行随机森林填补
# 设置并行计算（可选，加快速度）
# registerDoParallel(cores = detectCores() - 1)










all_na_cols <- names(which(sapply(data_for_imputation, function(x) all(is.na(x)))))
cat("全为NA的列：", paste(all_na_cols, collapse = ", "), "\n")

# 找出近全为NA的列（缺失率 > 95%）
high_missing_cols <- names(which(sapply(data_for_imputation, function(x) mean(is.na(x)) > 0.95)))
cat("\n缺失率 > 95%的列：", paste(high_missing_cols, collapse = ", "), "\n")



# 删除全为NA的列
cleaned_data <- data_for_imputation %>%
  select(-all_of(all_na_cols))

cat("原始列数：", ncol(data_for_imputation), "\n")
cat("清理后列数：", ncol(cleaned_data), "\n")

# 再次尝试填补
set.seed(123)
imp_result <- missForest(
  cleaned_data,
  maxiter = 10,
  ntree = 100,
  variablewise = FALSE,
  decreasing = FALSE,
  verbose = TRUE
)





# install.packages("missRanger")
library(missRanger)
library(dplyr)
# 准备数据
item_cols <- grep("^itemid_", names(lab_wide_filtered), value = TRUE)

# 转换为数值型
data_for_imp <- lab_wide_filtered %>%
  select(all_of(item_cols)) %>%
  mutate(across(everything(), as.numeric))

# 检查数据结构
cat("数据维度：", dim(data_for_imp), "\n")
cat("每列数据类型：\n")
print(sapply(data_for_imp, class))

set.seed(123)
imputed_data <- missRanger(
  data_for_imp,
  pmm.k = 3,              # 预测均值匹配的邻居数
  num.trees = 100,        # 树的数量
  maxiter = 5,            # 最大迭代次数
  seed = 123,
  verbose = 1,            # 显示进度
  # 关键参数：明确指定数据类型
  data_only = FALSE
)
lab_wide_imputedMF <- imputed_data$data

head(lab_wide_imputedMF)




lab_wide_final <- lab_wide_filtered

# 获取itemid列名
item_cols <- names(lab_wide_imputedMF)

# 直接替换这些列的值
lab_wide_final[, item_cols] <- lab_wide_imputedMF


################标准化数值######################

glimpse(data_with_obstime)


# 找到所有 itemid 变量
item_cols <- c(
  grep("^itemid_", names(data_with_obstime), value = TRUE),
  "anchor_age"
)


# 计算均值
item_means <- data_with_obstime %>%
  summarise(across(all_of(item_cols), ~mean(.x, na.rm = TRUE)))

# 标准化
data_scaled <- data_with_obstime %>%
  mutate(
    across(
      all_of(item_cols),
      ~ .x / item_means[[cur_column()]]
    )
  )

























##############################################

library(writexl)
write_xlsx(lab_wide_final,"F:/文章_大论文/MIMIC数据库_代码/TREA代码/0310/data_all.xlsx")


