library(readxl)
library(openxlsx)
library(dplyr)
library(stringr)

# 目标目录：数据_result_RSFLC_3C
base_dir <- "f:/文章_大论文/结果/数据_result_RSFLC_3C"
output_file <- file.path(base_dir, "RESULT_ALL_RSFLC_3C.xlsx")

# 读取全部 xlsx 数据文件（排除已存在的汇总文件）
all_files <- list.files(base_dir, pattern = "\\.xlsx$", full.names = TRUE)
data_files <- all_files[!grepl("^RESULT_ALL_RSFLC_3C\\.xlsx$", basename(all_files))]

if (length(data_files) == 0) {
  stop("未找到可整合的 xlsx 数据文件。")
}

# 提取分组键：
# result_sim1000_30_10V_highBTW_3c_L1.xlsx -> sim1000_30_10V_highBTW_3c
group_keys <- basename(data_files) |>
  str_remove("^result_") |>
  str_remove("\\.xlsx$") |>
  str_remove("_L\\d+$")

split_files <- split(data_files, group_keys)

summary_list <- lapply(names(split_files), function(key) {
  one_group_files <- split_files[[key]]
  one_group_data <- lapply(one_group_files, read_xlsx) |> bind_rows()

  tibble(
    var = key,
    AUC_mean = mean(one_group_data$AUC, na.rm = TRUE),
    AUC_var = var(one_group_data$AUC, na.rm = TRUE),
    BS_mean = mean(one_group_data$BS, na.rm = TRUE),
    BS_var = var(one_group_data$BS, na.rm = TRUE),
    Cindex_mean = mean(one_group_data$Cindex, na.rm = TRUE),
    Cindex_var = var(one_group_data$Cindex, na.rm = TRUE)
  )
})

result_df <- bind_rows(summary_list)

parsed_num <- str_match(result_df$var, "^sim(\\d+)_(\\d+)_")

# 按 1C 汇总文件同风格排序：
# INTER 在前，BTW 在后；low -> mid -> high；
# 每个相关性下 sim500_30 -> sim500_70 -> sim1000_30 -> sim1000_70
result_df <- result_df |>
  mutate(
    rel_type = case_when(
      str_detect(var, "INTER") ~ "INTER",
      str_detect(var, "BTW") ~ "BTW",
      TRUE ~ "OTHER"
    ),
    corr_level = case_when(
      str_detect(var, "low") ~ "low",
      str_detect(var, "mid") ~ "mid",
      str_detect(var, "high") ~ "high",
      TRUE ~ "other"
    ),
    n_level = parsed_num[, 2],
    p_level = parsed_num[, 3]
  ) |>
  mutate(
    rel_type = factor(rel_type, levels = c("INTER", "BTW", "OTHER")),
    corr_level = factor(corr_level, levels = c("low", "mid", "high", "other")),
    n_level = as.numeric(n_level),
    p_level = as.numeric(p_level)
  ) |>
  arrange(rel_type, corr_level, n_level, p_level) |>
  select(var, AUC_mean, AUC_var, BS_mean, BS_var, Cindex_mean, Cindex_var)

write.xlsx(result_df, output_file, overwrite = TRUE)

cat("整合完成，已输出：", output_file, "\n", sep = "")
cat("共整合条件数：", nrow(result_df), "\n", sep = "")
