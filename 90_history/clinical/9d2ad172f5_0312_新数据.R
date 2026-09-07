# 数据处理脚本：保留有数据的变量并保存新文件
# 输入：stroke_sup_icu2.xlsx
# 输出：stroke_sup_icu3.xlsx（只包含ID列和有数据的5个变量）

# 加载必要的库
library(readxl)
library(dplyr)
library(openxlsx)

# 设置工作目录
setwd("f:\\文章_大论文\\MIMIC数据库_代码\\TREA代码\\0312")

# 检查stroke_sup_icu2.xlsx是否存在，如果存在则处理
if (file.exists("stroke_sup_icu2.xlsx")) {
  # 1. 读取原始数据
  cat("读取原始数据...\n")
  original_data <- read_xlsx("stroke_sup_icu2.xlsx")

  # 2. 显示原始数据的基本信息
  cat("原始数据信息：\n")
  cat("- 总行数：", nrow(original_data), "\n")
  cat("- 总列数：", ncol(original_data), "\n")
  cat("- 所有变量：", paste(names(original_data), collapse = ", "), "\n\n")

  # 3. 识别有数据的变量（非空变量）
  # 计算每个变量的非空值数量
  non_empty_counts <- sapply(original_data, function(x) sum(!is.na(x)))

  # 筛选出有数据的变量（非空值数量 > 0）
  variables_with_data <- names(non_empty_counts[non_empty_counts > 0])

  cat("有数据的变量：\n")
  for (var in variables_with_data) {
    cat("- ", var, ": ", non_empty_counts[var], "个非空值\n")
  }
  cat("\n")

  # 4. 保留所有三个ID列
  id_columns <- c("subject_id", "hadm_id", "stay_id")

  # 检查ID列是否都存在
  missing_ids <- setdiff(id_columns, names(original_data))
  if (length(missing_ids) > 0) {
    cat("警告：以下ID列在数据中不存在：", paste(missing_ids, collapse = ", "), "\n")
    cat("将只使用存在的ID列\n")
  }

  # 选择存在的ID列
  available_id_cols <- intersect(id_columns, names(original_data))
  cat("保留的ID列：", paste(available_id_cols, collapse = ", "), "\n\n")

  # 5. 选择要保留的变量：所有ID列 + 指定的5个变量
  # 确保只保留用户指定的5个变量
  required_variables <- c("hypertension", "diabetes", "atrial_fibrillation", "heart_failure", "renal_disease")

  # 检查指定的变量是否都存在于数据中
  missing_vars <- setdiff(required_variables, names(original_data))
  if (length(missing_vars) > 0) {
    cat("警告：以下指定变量在数据中不存在：", paste(missing_vars, collapse = ", "), "\n")
    cat("将只使用存在的指定变量\n")
  }

  # 选择存在的指定变量
  available_required_vars <- intersect(required_variables, names(original_data))

  # 构建最终选择的变量列表：所有ID列 + 存在的指定变量
  selected_variables <- c(available_id_cols, available_required_vars)

  cat("\n选择保留的变量：\n")
  cat(paste(selected_variables, collapse = ", "), "\n\n")

  # 6. 创建新数据框
  new_data <- original_data %>% select(all_of(selected_variables))

  # 7. 显示新数据的基本信息
  cat("新数据信息：\n")
  cat("- 总行数：", nrow(new_data), "\n")
  cat("- 总列数：", ncol(new_data), "\n")
  cat("- 保留的变量：", paste(names(new_data), collapse = ", "), "\n\n")

  # 8. 保存新数据
  output_file <- "stroke_sup_icu3.xlsx"
  write.xlsx(new_data, file = output_file, rowNames = FALSE)

  cat("✅ 数据处理完成！\n")
  cat("新文件已保存为：", output_file, "\n")
  cat("\n处理总结：\n")
  cat("- 从原始数据中筛选出", length(selected_variables), "个变量\n")
  cat("- 保留了ID列和有数据的5个变量\n")
  cat("- 数据行数保持不变：", nrow(new_data), "行\n")
} else {
  cat("跳过数据处理部分：stroke_sup_icu2.xlsx不存在\n")
}



# ------------------------- 数据合并部分 ------------------------- #
# 将stroke_sup_icu3.xlsx中的5个变量合并到data_all.xlsx中
# 合并基础：subjectid/subject_id
# 注意：先对stroke_sup去重，避免多对多关系

cat("\n\n开始数据合并操作...\n")

# 1. 读取data_all.xlsx
cat("1. 读取基础数据集 data_all.xlsx...\n")
data_all <- read_xlsx("data_all.xlsx")
cat("- data_all总行数：", nrow(data_all), "\n")
cat("- data_all总列数：", ncol(data_all), "\n")

# 2. 读取stroke_sup_icu3.xlsx
cat("\n2. 读取附加数据集 stroke_sup_icu3.xlsx...\n")
stroke_sup <- read_xlsx("stroke_sup_icu3.xlsx")
cat("- stroke_sup原始行数：", nrow(stroke_sup), "\n")
cat("- stroke_sup原始列数：", ncol(stroke_sup), "\n")

# 3. 对stroke_sup按subject_id去重，避免多对多关系
cat("\n3. 对stroke_sup按subject_id去重...\n")
stroke_sup_unique <- stroke_sup %>% distinct(subject_id, .keep_all = TRUE)
cat("- 去重后stroke_sup行数：", nrow(stroke_sup_unique), "\n")

# 4. 准备合并的变量
# 只保留5个需要合并的变量和subject_id
merge_variables <- c("subject_id", "hypertension", "diabetes", "atrial_fibrillation", "heart_failure", "renal_disease")
stroke_sup_filtered <- stroke_sup_unique %>% select(all_of(merge_variables))

# 5. 检查data_all中的ID列名
id_column_data_all <- "subjectid" # data_all.xlsx中的ID列名
if (!(id_column_data_all %in% names(data_all))) {
  # 如果不存在，尝试查找其他可能的ID列名
  possible_id_cols <- grep("id|ID|subject_id|Subject_ID", names(data_all), value = TRUE)
  if (length(possible_id_cols) > 0) {
    id_column_data_all <- possible_id_cols[1]
    cat("4. 使用", id_column_data_all, "作为data_all的ID列\n")
  } else {
    stop("在data_all.xlsx中未找到ID列")
  }
}

# 6. 进行数据合并
cat("\n5. 执行数据合并（以data_all为基础）...\n")
# 使用left_join，以data_all为基础
merged_data <- left_join(data_all, 
                         stroke_sup_filtered, 
                         by = setNames("subject_id", id_column_data_all))

# 7. 显示合并后的数据信息
cat("\n6. 合并后数据信息：\n")
cat("- 合并后总行数：", nrow(merged_data), "\n")
cat("- 合并后总列数：", ncol(merged_data), "\n")
cat("- 新增变量：", paste(merge_variables[-1], collapse = ", "), "\n")

# 8. 删除没有对应数据的记录（即所有5个变量都缺失的记录）
cat("\n7. 删除没有对应数据的记录...\n")
filtered_data <- merged_data %>%
  filter(if_all(merge_variables[-1], ~ !is.na(.)))

cat("- 筛选后总行数：", nrow(filtered_data), "\n")
cat("- 删除的行数：", nrow(merged_data) - nrow(filtered_data), "\n")

# 9. 保存合并后的数据
output_merge_file <- "data_all_sup.xlsx"
write.xlsx(filtered_data, file = output_merge_file, rowNames = FALSE)

cat("\n✅ 数据合并完成！\n")
cat("合并后文件已保存为：", output_merge_file, "\n")
cat("\n合并总结：\n")
cat("- 基于", id_column_data_all, "/subject_id合并了两个数据集\n")
cat("- 先对stroke_sup去重避免多对多关系\n")
cat("- 删除了所有5个变量都没有数据的记录\n")
cat("- 最终保留了", nrow(filtered_data), "行数据\n")
cat("- 合并后数据行数 <= 原始data_all行数\n")



