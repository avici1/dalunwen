library(dplyr)
library(tidyr)
library(missForest)
library(readxl)

stroke_patients_0417 <- read_excel("F:/文章_大论文/0417/stroke_patients_0417.xlsx")
stroke_admission_0417 <- read_excel("F:/文章_大论文/0417/stroke_admission_0417.xlsx")


labevent_0423_filter <- read.csv("F:/文章_大论文/0417/代码/新建文件夹/labevent_0423_filter.csv")
chartevent_0423_filter <- read.csv("F:/文章_大论文/0417/代码/新建文件夹/chartevent_0423_filter.csv")

cat("labevent_0423_filter 行数:", nrow(labevent_0423_filter), "\n")
cat("chartevent_0423_filter 行数:", nrow(chartevent_0423_filter), "\n")

##############################


# 1) 统一格式并合并 lab + chart
lab_long <- labevent_0423_filter %>%
  transmute(
    subject_id,
    hadm_id,
    charttime = as.POSIXct(charttime),
    itemid = as.character(itemid),
    valuenum = suppressWarnings(as.numeric(valuenum))
  )
chart_long <- chartevent_0423_filter %>%
  transmute(
    subject_id,
    hadm_id,
    charttime = as.POSIXct(charttime),
    itemid = as.character(itemid),
    valuenum = suppressWarnings(as.numeric(valuenum))
  )
all_long <- bind_rows(lab_long, chart_long) %>%
  filter(!is.na(subject_id), !is.na(hadm_id), !is.na(itemid), !is.na(valuenum)) %>%
  arrange(subject_id, hadm_id, itemid, charttime)
# 2) 在每个患者-住院-变量(itemid)内，生成第几次测量 visit
all_long_visit <- all_long %>%
  group_by(subject_id, hadm_id, itemid) %>%
  mutate(visit = row_number()) %>%
  ungroup()
# 3) 转宽：itemid 在列里，visit 为重复测量序号
data_0423_wide <- all_long_visit %>%
  select(subject_id, hadm_id, visit, itemid, valuenum) %>%
  pivot_wider(
    id_cols = c(subject_id, hadm_id, visit),
    names_from = itemid,
    values_from = valuenum,
    values_fn = ~ first(.x),  # 防止同键重复时报错
    names_prefix = "item_"
  ) %>%
  arrange(subject_id, hadm_id, visit)
# 结果查看
print(dim(data_0423_wide))
print(head(data_0423_wide, 5))
##################





#######################
# 只计算 item_ 开头变量的缺失率
item_cols <- names(data_0423_wide)[grepl("^item_", names(data_0423_wide))]
missing_rate_item_0423 <- tibble(
  variable = item_cols,
  missing_n = sapply(data_0423_wide[item_cols], function(x) sum(is.na(x))),
  total_n = nrow(data_0423_wide)
) %>%
  mutate(
    missing_rate = missing_n / total_n,
    missing_rate_pct = round(missing_rate * 100, 2)
  ) %>%
  arrange(desc(missing_rate))
print(missing_rate_item_0423, n = nrow(missing_rate_item_0423))




#####################
#转换华氏度为摄氏度
data_0423_wide <- data_0423_wide %>%
  mutate(
    item_223761_c = (item_223761 - 32) / 1.8,
    item_223762 = dplyr::coalesce(item_223762, item_223761_c)
  ) %>%
  select(-item_223761_c, -item_223761)

##################


# install.packages(c("missRanger", "ranger"))  # 如未安装
library(dplyr)
library(missRanger)

# 1) item 列
item_cols <- names(data_0423_wide)[grepl("^item_", names(data_0423_wide))]

# 2) 仅取 item 列并强制数值化
impute_df <- data_0423_wide %>%
  select(all_of(item_cols)) %>%
  mutate(across(everything(), ~ suppressWarnings(as.numeric(.x))))

# 3) 处理极端列：全缺失列无法用RF学习，先记录
all_na_cols <- names(impute_df)[colSums(!is.na(impute_df)) == 0]
use_cols <- setdiff(names(impute_df), all_na_cols)

# 4) 随机森林插补（missRanger）
set.seed(20260423)
imputed_core <- missRanger(
  data = impute_df[, use_cols, drop = FALSE],
  formula = . ~ .,
  num.trees = 200,
  maxiter = 10,
  pmm.k = 5,          # 预测均值匹配，避免不合理极端值
  verbose = TRUE
)

# 5) 回填：可插补列用结果，全缺失列先保留NA（或你可改成0/中位数）
imputed_items <- impute_df
imputed_items[, use_cols] <- imputed_core
# 若你想把全缺失列设为0，可取消注释：
# imputed_items[, all_na_cols] <- 0

# 6) 合并回原数据
data_0423_wide_inputed <- data_0423_wide
data_0423_wide_inputed[, item_cols] <- imputed_items

# 7) 检查
na_after <- sapply(data_0423_wide_inputed[, item_cols], function(x) sum(is.na(x)))
cat("插补后 item 列剩余缺失总数:", sum(na_after), "\n")
cat("仍全缺失列数:", length(all_na_cols), "\n")
print(all_na_cols)
####################






##################
# 你的插补后数据
dat <- data_0423_wide_inputed
# 1) 计算你数据的均值/SD
my_stats <- dat %>%
  summarise(across(starts_with("item_"), list(mean = ~mean(.x, na.rm = TRUE),
                                              sd   = ~sd(.x, na.rm = TRUE)))) %>%
  pivot_longer(everything(),
               names_to = c("item", ".value"),
               names_pattern = "(item_\\d+)_(mean|sd)")
# 2) 文献参考表（可按你最终采用文献再细调）
ref_stats <- tibble::tribble(
  ~item,       ~ref_mean, ~ref_sd,   ~source,
  "item_220045", 84.8,     15.2,     "MIMIC-IV vitals table",
  "item_220210", 25.3,      9.4,     "MIMIC-IV vitals table",
  "item_223762", 36.8,      0.51,    "MIMIC-IV vitals table (C)",
  "item_50821", 109.0,     50.4,     "MIMIC SOFA summary (IQR->SD)",
  "item_50885",   1.1,      2.37,    "MIMIC SOFA summary (IQR->SD)",
  "item_50912",   1.0,      0.74,    "MIMIC SOFA summary (IQR->SD)",
  "item_51265", 186.0,    114.8,     "MIMIC SOFA summary (IQR->SD)",
  "item_50983", 139.0,      3.70,    "MIMIC lab summary (IQR->SD)",
  "item_50971",   4.1,      0.52,    "MIMIC lab summary (IQR->SD)",
  "item_50882",  25.0,      4.44,    "MIMIC lab summary (IQR->SD)",
  "item_51221",  30.1,      5.04,    "MIMIC lab summary (IQR->SD)",
  "item_51301",   9.7,      4.74,    "MIMIC lab summary (IQR->SD)"
)
# 3) 对照与告警
compare_stats <- my_stats %>%
  inner_join(ref_stats, by = "item") %>%
  mutate(
    mean_z = (mean - ref_mean) / ref_sd,    # 均值偏移(单位=参考SD)
    sd_ratio = sd / ref_sd,                 # 方差尺度比
    flag = case_when(
      abs(mean_z) > 1.5 | sd_ratio < 0.5 | sd_ratio > 2.0 ~ "check",
      TRUE ~ "ok"
    )
  ) %>%
  arrange(desc(abs(mean_z)))
print(compare_stats, n = nrow(compare_stats))







# 口径 A：按 subject_id（跨所有住院）
by_patient <- data_0423_wide_inputed %>%
  count(subject_id, name = "n_visits")
cat("患者人数:", n_distinct(data_0423_wide_inputed$subject_id), "\n")
cat("人均重复测量次数(行数):", mean(by_patient$n_visits), "\n")
cat("中位数:", median(by_patient$n_visits), "\n")
# 口径 B：按 subject_id + hadm_id（单次住院内）
by_stay <- data_0423_wide_inputed %>%
  count(subject_id, hadm_id, name = "n_visits")
cat("\n住院 episode 数:", nrow(by_stay), "\n")
cat("单次住院人均 visit 数:", mean(by_stay$n_visits), "\n")
cat("中位数:", median(by_stay$n_visits), "\n")




write.csv(data_0423_wide_inputed,"F:/文章_大论文/0417/stroke_patients_inputed_0428.csv")





################
data_0423_wide_inputed<- read.csv("F:/文章_大论文/0417/stroke_patients_inputed_0428.csv")
# 6.1) 合并基线变量：年龄 + 生存时间 + 是否死亡
# 说明：
# - 年龄使用 patients 中的 anchor_age
# - 生存时间使用 patients 中的 dod 相对 baseline_date(anchor_year-01-01) 的天数
# - survival: 1=已死亡（有生存时间），0=未死亡（无生存时间）
baseline_age_surv <- stroke_patients_0417 %>%
  transmute(
    subject_id,
    anchor_age = suppressWarnings(as.numeric(anchor_age)),
    baseline_date = as.Date(paste0(anchor_year, "-01-01")),
    dod_date = as.Date(dod),
    survival_time_days = dplyr::case_when(
      !is.na(dod_date) & !is.na(baseline_date) ~ as.numeric(difftime(dod_date, baseline_date, units = "days")),
      TRUE ~ NA_real_
    )
  )

baseline_all <- stroke_admission_0417 %>%
  distinct(subject_id, hadm_id) %>%
  left_join(
    baseline_age_surv %>% select(subject_id, anchor_age, survival_time_days),
    by = "subject_id"
  ) %>%
  transmute(
    subject_id,
    hadm_id,
    age = anchor_age,
    survival_time_days,
    survival = if_else(!is.na(survival_time_days), 1L, 0L)
  )

data_0423_wide_inputed <- data_0423_wide_inputed %>%
  left_join(baseline_all, by = c("subject_id", "hadm_id"))
write.csv(data_0423_wide_inputed,"F:/文章_大论文/0417/stroke_patients_inputed_0428_1.csv")



#################

