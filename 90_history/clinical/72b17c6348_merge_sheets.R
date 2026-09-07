library(readxl)
library(dplyr)
library(tidyr)
# 设置文件路径
file_path <- "f:/文章_大论文/MIMIC数据库_代码/TREA代码/0309/summary_stroke_lab100_value.xlsx"

# 获取所有sheet名称
sheet_names <- excel_sheets(file_path)

# 读取所有sheet并合并为一个数据框
data <- bind_rows(
  lapply(sheet_names, function(sheet) {
    read_excel(file_path, sheet = sheet)
  })
)



head(data)




# 在第4data# 在第46-47行实现用户需求：labevent_id和subjectid同时作为变量，itemid展开为列
# 解决方案：按时间点合并数据，同时保留labevent_id和生成真正的subject_id
data_wide1 <- data %>%
  # 第一步：按时间点分组，为每个时间点生成subject_id
  group_by(charttime, storetime) %>%
  mutate(
    subject_id = cur_group_id(),
    labevent_id = first(labevent_id)  # 每个时间点取第一个labevent_id
  ) %>%
  ungroup() %>%
  # 第二步：将itemid展开为列
  pivot_wider(
    id_cols = c(subject_id, labevent_id, charttime, storetime),  # labevent_id和subjectid同时作为变量
    names_from = itemid,                           # 将itemid作为列名
    values_from = value,                           # 将value作为列值
    values_fn = first                              # 如果有重复，取第一个值
  )

head(data_wide1)


# 计算data_wide1中每个变量的有效值百分比
data_wide1_summary <- data_wide1 %>%
  summarize(
    across(everything(), function(x) {
      valid_count <- sum(!is.na(x))
      total_count <- n()
      percentage <- round((valid_count / total_count) * 100, 1)
      paste0(valid_count, "/", total_count, "(", percentage, "%)")
    })
  )

# 查看汇总结果 - 显示前20个变量作为示例
print("\ndata_wide1变量有效值汇总（前20个变量）:")
summary_df <- data_wide1_summary %>% select(1:20)
# 使用基础R函数处理转置和行名
result <- as.data.frame(t(summary_df))
colnames(result) <- "有效值统计"
result$变量名 <- rownames(result)
result <- result[, c("变量名", "有效值统计")]
print(result, row.names = FALSE)

# 筛选出所有itemid变量（数字列名）中有非缺失值的行
data_wide_filter1 <- data_wide1 %>%
  # 识别所有itemid变量（列名是数字的列）
  filter(
    # 使用if_any检查任意itemid变量是否有非缺失值
    if_any(
      # 选择所有列名是数字的列（即原本的itemid）
      matches("^\\d+$"),
      ~ !is.na(.x)
    )
  )

# 查看筛选后的结果
dim_filter <- dim(data_wide_filter1)




#######################################################

# 统计data_wide1中itemid变量（数字列名）的有效值数量分布
# 第一步：识别所有itemid变量（列名是数字的列）
itemid_cols <- data_wide1 %>% select(matches("^\\d+$")) %>% colnames()
total_itemid <- length(itemid_cols)
total_obs <- nrow(data_wide1)

# 第二步：计算每行itemid变量的有效值数量，并统计分布
data_wide2 <- data_wide1 %>%
  # 计算每行itemid变量的有效值数量
  rowwise() %>%
  mutate(
    valid_count = sum(!is.na(c_across(all_of(itemid_cols)))),
    valid_rate = paste0(valid_count, "/", total_itemid)
  ) %>%
  ungroup()

# 第三步：统计每个有效值数量的记录数和百分比
data_wide_filter2 <- data_wide2 %>%
  group_by(valid_rate, valid_count) %>%
  summarise(
    obs_count = n(),
    obs_percentage = round((n() / total_obs) * 100, 1)
  ) %>%
  ungroup() %>%
  # 按有效值数量从高到低排序
  arrange(desc(valid_count)) %>%
  # 只显示至少有一个有效值的行
  filter(valid_count > 0) %>%
  # 格式化显示
  mutate(
    result = paste0(obs_count, "/", total_obs, "(", obs_percentage, "%)")
  ) %>%
  # 选择需要的列
  select(valid_rate, obs_count, total_obs, obs_percentage, result)

# 查看结果
print("\nitemid变量有效值数量分布统计:")
print(data_wide_filter2, row.names = FALSE)

# 保存结果到文件
write.csv(data_wide_filter2, file = "data_wide_filter2.csv", row.names = FALSE)
print("\n统计结果已保存到 data_wide_filter2.csv 文件")



