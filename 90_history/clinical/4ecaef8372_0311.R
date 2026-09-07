library(readxl)
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(writexl)
library(stringr)


data_MIMIC<-read_xlsx("F:/文章_大论文/MIMIC数据库_代码/TREA代码/0311/data_MIMIC.xlsx")
data_baseline<-read_xlsx("F:/文章_大论文/MIMIC数据库_代码/TREA代码/0311/stroke_baseline_simple.xlsx")

data_baseline_unique <- data_baseline %>%
  group_by(subject_id) %>%
  slice(1) %>%
  ungroup()

head(data_MIMIC)
head(data_baseline)



data_longitude <- data_MIMIC %>%
  left_join(data_baseline_unique, by = "subject_id")


data_longitude <- data_longitude %>%
  select(-c(48,49))



vars <- c("gender",
          "race",
          "insurance",
          "marital_status",
          "admission_type",
          "hospital_mortality",
          "race_group",
          "obstime",
          "admission_type_4class",
          "insurance_updated")



##########################################################
glimpse(data_longitude)
data_obs_summary <- data_with_obstime %>%
  select(all_of(vars)) %>%
  mutate(across(everything(), as.character)) %>%   # 关键一步
  pivot_longer(cols = everything(),
               names_to = "variable",
               values_to = "value") %>%
  group_by(variable, value) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(variable) %>%
  mutate(percent = round(n / sum(n) * 100, 2)) %>%
  ungroup()

#修正race_group
data_longitude <- data_longitude %>%
  mutate(
    race_group = case_when(
      
      str_detect(race, "WHITE") ~ "WHITE",
      
      str_detect(race, "BLACK") ~ "BLACK",
      
      str_detect(race, "ASIAN") ~ "ASIAN",
      
      str_detect(race, "HISPANIC") ~ "HISPANIC",
      
      race %in% c("UNKNOWN",
                  "UNABLE TO OBTAIN",
                  "PATIENT DECLINED TO ANSWER") ~ "UNKNOWN",
      
      TRUE ~ "OTHER"
    )
  )

# 添加obstime列 - 对于每个subject_id，按charttime排序，从1开始编号
data_with_obstime <- data_longitude %>%
  # 先按subject_id分组
  group_by(subject_id) %>%
  # 按charttime排序（假设charttime是日期格式）
  arrange(charttime, .by_group = TRUE) %>%
  # 添加obstime列，从1开始编号
  mutate(obstime = row_number()) %>%
  # 取消分组
  ungroup()



##修正admissiontype
# 将admission_type转换为4个新类别
data_with_obstime <- data_with_obstime %>%
  mutate(admission_type_4class = case_when(
    # 1. 观察/短期留观 - 合并4种观察类入院
    admission_type %in% c("AMBULATORY OBSERVATION", 
                          "DIRECT OBSERVATION", 
                          "EU OBSERVATION", 
                          "OBSERVATION ADMIT") ~ "Observation/Short Stay",
    
    # 2. 急诊/紧急入院 - 合并3种紧急入院
    admission_type %in% c("DIRECT EMER.", 
                          "EW EMER.", 
                          "URGENT") ~ "Emergency/Urgent",
    
    # 3. 日间手术 - 单独保留
    admission_type == "SURGICAL SAME DAY ADMISSION" ~ "Surgical Same Day",
    
    # 4. 择期入院 - 单独保留
    admission_type == "ELECTIVE" ~ "Elective",
    
    # 如果有其他未匹配的类别（理论上不应该有），设置为NA
    TRUE ~ NA_character_
  ))


##insurance
data_with_obstime <- data_with_obstime %>%
  mutate(insurance_updated = case_when(
    # 将NA和Other都转换为"Other"
    is.na(insurance) | insurance == "Other" ~ "Other",
    # 保留其他类别不变
    TRUE ~ insurance
  ))

data_with_obstime <- data_with_obstime %>%
  mutate(marital_status_3class = case_when(
    # 已婚：包括MARRIED
    marital_status == "MARRIED" ~ "MARRIED",
    
    # 未婚：包括SINGLE、DIVORCED、WIDOWED
    marital_status %in% c("SINGLE", "DIVORCED", "WIDOWED") ~ "SINGLE",
    
    # 未知：包括NA和其他可能的未知状态
    is.na(marital_status) | marital_status == "NA" ~ "UNKNOWN",
    
    # 如果有其他未匹配的类别，也归为未知
    TRUE ~ "未知"
  ))


#########字符变量改数字###################
glimpse(data_with_obstime)


data_with_obstime1<-data_with_obstime[,-c(50,51,52,53)]
glimpse(data_with_obstime1)

data_with_obstime1 <- data_with_obstime1 %>%
  mutate(gender_binary = case_when(
    gender == "M" ~ 1,  # 男为1
    gender == "F" ~ 0,  # 女为0
    TRUE ~ NA_real_     # 如果有其他值，设为NA
  ))


#转为哑变量
library(fastDummies)
data_with_obstime2 <- dummy_cols(
  data_with_obstime1,
  select_columns = c(
    "race_group",
    "admission_type_4class",
    "insurance_updated",
    "marital_status_3class"
  ),
  remove_first_dummy = FALSE,
  remove_selected_columns = TRUE
)
glimpse(data_with_obstime2)

data_with_obstime3<-data_with_obstime2[,-c(49)]
glimpse(data_with_obstime3)



###############标准化数据##############


# 更稳健的标准化版本
data_with_obstime4 <- data_with_obstime3 %>%
  mutate(
    # 标准化年龄
    anchor_age = anchor_age / mean(anchor_age, na.rm = TRUE),
    
    # 标准化所有item变量
    across(starts_with("itemid_"), 
           function(x) {
             mean_val <- mean(x, na.rm = TRUE)
             if(is.na(mean_val) || mean_val == 0) {
               # 如果均值为NA或0，返回原值
               return(x)
             } else {
               return(x / mean_val)
             }
           })
  )

# 验证所有item变量的均值
cat("\n=== Summary of standardized variables ===\n")
summary_stats <- data_with_obstime4 %>%
  select(starts_with("itemid_"), anchor_age) %>%
  summarise(across(everything(), 
                   list(mean = ~ mean(.x, na.rm = TRUE),
                        sd = ~ sd(.x, na.rm = TRUE),
                        min = ~ min(.x, na.rm = TRUE),
                        max = ~ max(.x, na.rm = TRUE))))

# 查看均值的分布
means <- summary_stats[1, grepl("_mean", names(summary_stats))]
cat("\nMeans of standardized variables (should all be close to 1):\n")
print(unlist(means))


write_xlsx(data_with_obstime4,"F:/文章_大论文/MIMIC数据库_代码/TREA代码/0312/data_all.xlsx")












