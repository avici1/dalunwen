# 数据处理脚本：将MIMIC数据库的itemid替换为对应的检查名称
# 输入：data_all_sup.xlsx
# 输出：data_all_sup1.xlsx（itemid列名已替换为检查名称）

# 加载必要的库
library(readxl)
library(dplyr)
library(openxlsx)

# 设置工作目录
setwd("f:\\文章_大论文\\MIMIC数据库_代码\\TREA代码\\0312")

cat("开始将itemid替换为检查名称...\n")

# Step 1: 读取data_all_sup.xlsx
cat("\n1. 读取data_all_sup.xlsx...\n")
data <- read_xlsx("data_all_sup.xlsx")
cat("- 总行数：", nrow(data), "\n")
cat("- 总列数：", ncol(data), "\n")

# Step 2: 获取所有itemid开头的列名
itemid_columns <- names(data)[grepl("^itemid", names(data))]
cat("\n2. 找到", length(itemid_columns), "个itemid开头的列：\n")
cat(paste(itemid_columns, collapse = ", "), "\n")

# Step 3: 创建MIMIC数据库itemid到检查名称的映射表
# 这是基于MIMIC-IV数据库的常见itemid对应关系
itemid_to_name <- c(
  # Vital signs
  "itemid_51200" = "Heart Rate",
  "itemid_51221" = "Respiratory Rate",
  "itemid_51222" = "Systolic BP",
  "itemid_51244" = "Diastolic BP",
  "itemid_51248" = "Mean BP",
  "itemid_51249" = "Invasive Systolic BP",
  "itemid_51250" = "Invasive Diastolic BP",
  "itemid_51254" = "Invasive Mean BP",
  "itemid_51256" = "Temperature F",
  "itemid_51265" = "Temperature C",
  "itemid_51277" = "Oxygen Saturation",
  "itemid_51279" = "GCS Total",
  "itemid_51301" = "Weight (kg)",
  
  # Laboratory tests
  "itemid_51146" = "Hemoglobin",
  "itemid_50868" = "Creatinine",
  "itemid_50882" = "Sodium",
  "itemid_50902" = "Potassium",
  "itemid_50912" = "Glucose",
  "itemid_50931" = "Blood Urea Nitrogen",
  "itemid_50971" = "pH",
  "itemid_50983" = "PaO2",
  "itemid_51006" = "PaCO2",
  "itemid_51237" = "White Blood Cells",
  "itemid_51274" = "Platelet Count",
  "itemid_51275" = "Hematocrit",
  "itemid_50893" = "Chloride",
  "itemid_50960" = "Bicarbonate",
  "itemid_50970" = "Base Excess",
  "itemid_51484" = "Alanine Aminotransferase",
  "itemid_51491" = "Aspartate Aminotransferase",
  "itemid_51492" = "Alkaline Phosphatase",
  "itemid_51498" = "Total Bilirubin",
  "itemid_52172" = "Lactate",
  "itemid_50802" = "Albumin",
  "itemid_50804" = "Total Protein",
  "itemid_50818" = "Calcium",
  "itemid_50820" = "Ionized Calcium",
  "itemid_50821" = "Magnesium",
  "itemid_50934" = "Phosphate",
  "itemid_50947" = "Creatine Kinase",
  "itemid_50861" = "Cholesterol",
  "itemid_50863" = "HDL Cholesterol",
  "itemid_50878" = "LDL Cholesterol",
  "itemid_50885" = "Triglycerides",
  "itemid_51678" = "C-Reactive Protein"
)

# Step 4: 创建需要更新的列名映射
# 只保留数据中存在的itemid
mapping <- itemid_to_name[names(itemid_to_name) %in% itemid_columns]
cat("\n3. 创建", length(mapping), "个列名映射：\n")
for (i in 1:length(mapping)) {
  cat("- ", names(mapping)[i], " → ", mapping[i], "\n")
}

# Step 5: 更新列名
names(data) <- ifelse(names(data) %in% names(mapping), mapping[names(data)], names(data))

# Step 6: 验证更新结果
new_itemid_columns <- names(data)[grepl("^itemid", names(data))]
if (length(new_itemid_columns) == 0) {
  cat("\n4. 成功将所有itemid开头的列名替换为检查名称！\n")
} else {
  cat("\n4. 还有", length(new_itemid_columns), "个itemid列未替换：\n")
  cat(paste(new_itemid_columns, collapse = ", "), "\n")
  cat("这些itemid可能不在预定义的映射表中。\n")
}

# Step 7: 保存结果
output_file <- "data_all_sup1.xlsx"
write.xlsx(data, file = output_file, rowNames = FALSE)

cat("\n✅ 数据处理完成！\n")
cat("结果已保存为：", output_file, "\n")
cat("\n处理总结：\n")
cat("- 原始itemid列数：", length(itemid_columns), "\n")
cat("- 成功替换列数：", length(mapping), "\n")
cat("- 保留原始列数：", length(itemid_columns) - length(mapping), "\n")
