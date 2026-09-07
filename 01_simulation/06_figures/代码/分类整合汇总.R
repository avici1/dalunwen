library(readxl)
library(openxlsx)
library(dplyr)
library(stringr)

summary_dir <- "f:/文章_大论文/结果/all_summary"

# 4 个目标分类目录（不存在则创建）
dir_compare_4v_10v1c <- file.path(summary_dir, "4V1C vs  10V1C")
dir_compare_10v3c_10v1c <- file.path(summary_dir, "10V3C  vs 10V1C")
dir_by_model <- file.path(summary_dir, "按模型类型")
dir_by_data <- file.path(summary_dir, "按数据类型")

dir.create(dir_compare_4v_10v1c, recursive = TRUE, showWarnings = FALSE)
dir.create(dir_compare_10v3c_10v1c, recursive = TRUE, showWarnings = FALSE)
dir.create(dir_by_model, recursive = TRUE, showWarnings = FALSE)
dir.create(dir_by_data, recursive = TRUE, showWarnings = FALSE)

# 读取 12 个汇总文件
all_files <- list.files(
  summary_dir,
  pattern = "^RESULT_ALL_.*\\.xlsx$",
  full.names = TRUE
)

if (length(all_files) == 0) {
  stop("all_summary 中未找到 RESULT_ALL_*.xlsx 文件。")
}

data_files <- all_files[!grepl("^ALL_RESLT\\.xlsx$", basename(all_files))]

read_one_file <- function(xlsx_path) {
  file_name <- basename(xlsx_path)
  key <- str_remove(file_name, "^RESULT_ALL_") |>
    str_remove("\\.xlsx$")

  model_type <- str_match(key, "^(.+?)_(4V|10V1C|10V3C)$")[, 2]
  data_type <- str_match(key, "^(.+?)_(4V|10V1C|10V3C)$")[, 3]

  if (is.na(model_type) || is.na(data_type)) {
    stop(paste0("文件名无法解析模型和数据类型：", file_name))
  }

  df <- read.xlsx(xlsx_path)

  # 某些文件可能多出无效列（如 X8），只保留标准指标列
  keep_cols <- c("var", "AUC_mean", "AUC_var", "BS_mean", "BS_var", "Cindex_mean", "Cindex_var")
  df <- df[, intersect(names(df), keep_cols), drop = FALSE]

  # 补齐缺失列，保证结构一致
  for (nm in setdiff(keep_cols, names(df))) {
    df[[nm]] <- NA
  }
  df <- df[, keep_cols]
  df$var <- as.character(df$var)
  num_cols <- c("AUC_mean", "AUC_var", "BS_mean", "BS_var", "Cindex_mean", "Cindex_var")
  for (nm in num_cols) {
    df[[nm]] <- suppressWarnings(as.numeric(df[[nm]]))
  }

  df |>
    mutate(
      Model_Type = model_type,
      Data_Type = data_type,
      Source_File = file_name
    ) |>
    select(Model_Type, Data_Type, Source_File, everything())
}

all_data <- bind_rows(lapply(data_files, read_one_file))

# 统一排序
all_data <- all_data |>
  mutate(
    Model_Type = factor(Model_Type, levels = c("COX", "JM", "RSF", "RSFLC")),
    Data_Type = factor(Data_Type, levels = c("4V", "10V1C", "10V3C"))
  ) |>
  arrange(Model_Type, Data_Type, var)

# 1) 按模型类型分类整合保存
for (m in unique(as.character(all_data$Model_Type))) {
  one_model <- all_data |> filter(as.character(Model_Type) == m)
  out_file <- file.path(dir_by_model, paste0("RESULT_BY_MODEL_", m, ".xlsx"))
  write.xlsx(one_model, out_file, overwrite = TRUE)
}

# 2) 按数据类型分类整合保存
for (d in unique(as.character(all_data$Data_Type))) {
  one_type <- all_data |> filter(as.character(Data_Type) == d)
  out_file <- file.path(dir_by_data, paste0("RESULT_BY_DATA_", d, ".xlsx"))
  write.xlsx(one_type, out_file, overwrite = TRUE)
}

# 3) 4V(4V1C) vs 10V1C：按模型分别保存
cmp_4v_10v1c <- all_data |>
  filter(as.character(Data_Type) %in% c("4V", "10V1C")) |>
  mutate(Compare_Group = "4V_vs_10V1C")

for (m in unique(as.character(cmp_4v_10v1c$Model_Type))) {
  one_model <- cmp_4v_10v1c |> filter(as.character(Model_Type) == m)
  out_file <- file.path(dir_compare_4v_10v1c, paste0("COMPARE_4V_vs_10V1C_", m, ".xlsx"))
  write.xlsx(one_model, out_file, overwrite = TRUE)
}

# 4) 10V3C vs 10V1C：按模型分别保存
cmp_10v3c_10v1c <- all_data |>
  filter(as.character(Data_Type) %in% c("10V1C", "10V3C")) |>
  mutate(Compare_Group = "10V3C_vs_10V1C")

for (m in unique(as.character(cmp_10v3c_10v1c$Model_Type))) {
  one_model <- cmp_10v3c_10v1c |> filter(as.character(Model_Type) == m)
  out_file <- file.path(dir_compare_10v3c_10v1c, paste0("COMPARE_10V3C_vs_10V1C_", m, ".xlsx"))
  write.xlsx(one_model, out_file, overwrite = TRUE)
}

# 可选：保存一个全量分类总表，便于后续筛选
write.xlsx(all_data, file.path(summary_dir, "ALL_RESULT_CLASSIFIED.xlsx"), overwrite = TRUE)

cat("分类整合完成。\n")
cat("按模型类型：", dir_by_model, "\n", sep = "")
cat("按数据类型：", dir_by_data, "\n", sep = "")
cat("4V1C vs 10V1C：", dir_compare_4v_10v1c, "\n", sep = "")
cat("10V3C vs 10V1C：", dir_compare_10v3c_10v1c, "\n", sep = "")
