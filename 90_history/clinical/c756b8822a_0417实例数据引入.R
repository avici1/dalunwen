library(readxl)
library(dplyr)

# =========================================================
# 0) 读取数据
# =========================================================
stroke_admission_0417 <- read_excel("F:/文章_大论文/0417/stroke_admission_0417.xlsx")
stroke_chartevents_0417 <- read.csv("F:/文章_大论文/0417/stroke_chartevents_0417.csv")
stroke_labevent_0417 <- read.csv("F:/文章_大论文/0417/stroke_labevent_0417.csv")
stroke_patients_0417 <- read_excel("F:/文章_大论文/0417/stroke_patients_0417.xlsx")
stroke_icustays_0417 <- read_excel("F:/文章_大论文/0417/stroke_icustays_0417.xlsx")

# =========================================================
# 1) 定义 SOFA + APACHE II 相关 itemid
# =========================================================
labevent_itemids_0423 <- c(
  # SOFA
  50821,  # PaO2
  51265,  # Platelet
  50885,  # Total Bilirubin
  50912,  # Creatinine
  # APACHE II
  50820,  # pH
  50882,  # HCO3
  50983,  # Sodium
  50971,  # Potassium
  51221,  # Hematocrit
  51300,  # WBC Count
  51301   # White Blood Cells
)

charttime_itemids_0423 <- c(
  # SOFA
  223835, # O2 Flow（可用于 FiO2 折算）
  3420,   # FiO2
  3422,   # FiO2 [Meas]
  220052, # MAP (arterial)
  220181, # MAP (NIBP)
  225312, # ART BP mean
  224322, # BP mean
  220739, # GCS eye
  223900, # GCS verbal
  223901, # GCS motor
  # APACHE II 常用生命体征
  220045, # Heart Rate
  220210, # Respiratory Rate
  223761, # Temperature Fahrenheit
  223762  # Temperature Celsius
)

# =========================================================
# 2) 模拟 d_time / dod（仅死亡患者）
# =========================================================
set.seed(20260420)
target_mean_days <- 12
weibull_shape <- 1.6
weibull_scale <- target_mean_days / gamma(1 + 1 / weibull_shape)

n_pat <- nrow(stroke_patients_0417)
d_time_sim <- rweibull(n_pat, shape = weibull_shape, scale = weibull_scale)

stroke_patients_0417 <- stroke_patients_0417 %>%
  mutate(
    died_flag = !is.na(dod),
    d_time = if_else(died_flag, round(pmax(d_time_sim, 1), 1), NA_real_),
    baseline_date = as.Date(paste0(anchor_year, "-01-01")),
    dod = if_else(
      died_flag & !is.na(baseline_date),
      baseline_date + as.integer(round(d_time)),
      as.Date(NA)
    )
  ) %>%
  select(-died_flag, -baseline_date)

# =========================================================
# 3) 模拟 ICU LOS / Hospital LOS，并做死亡约束
# =========================================================
set.seed(20260421)
hosp_mean_target <- 13.91
hosp_sd_target <- 12.44
icu_mean_target <- 4.49
icu_sd_target <- 5.10

icu_hadm <- stroke_icustays_0417 %>%
  filter(!is.na(hadm_id)) %>%
  distinct(subject_id, hadm_id) %>%
  mutate(
    icu_los_days_sim = round(
      pmax(rnorm(n(), mean = icu_mean_target, sd = icu_sd_target), 0.1),
      2
    )
  )

stroke_admission_0417 <- stroke_admission_0417 %>%
  left_join(icu_hadm, by = c("subject_id", "hadm_id")) %>%
  mutate(icu_flag = !is.na(icu_los_days_sim))

hosp_raw <- pmax(
  rnorm(nrow(stroke_admission_0417), mean = hosp_mean_target, sd = hosp_sd_target),
  0.1
)
hosp_raw <- ifelse(
  stroke_admission_0417$icu_flag,
  pmax(hosp_raw, stroke_admission_0417$icu_los_days_sim),
  hosp_raw
)

hosp_adj <- hosp_raw
for (i in 1:3) {
  cur_mean <- mean(hosp_adj, na.rm = TRUE)
  cur_sd <- sd(hosp_adj, na.rm = TRUE)
  if (!is.na(cur_sd) && cur_sd > 0) {
    hosp_adj <- (hosp_adj - cur_mean) / cur_sd * hosp_sd_target + hosp_mean_target
  }
  hosp_adj <- pmax(hosp_adj, 0.1)
  hosp_adj <- ifelse(
    stroke_admission_0417$icu_flag,
    pmax(hosp_adj, stroke_admission_0417$icu_los_days_sim),
    hosp_adj
  )
}

stroke_admission_0417 <- stroke_admission_0417 %>%
  mutate(hosp_los_days = round(hosp_adj, 2))

stroke_icustays_0417 <- stroke_icustays_0417 %>%
  left_join(icu_hadm %>% select(hadm_id, icu_los_days_sim), by = "hadm_id")

death_time_df <- stroke_patients_0417 %>%
  select(subject_id, dod, d_time) %>%
  mutate(is_dead = !is.na(dod) & !is.na(d_time))

stroke_admission_0417 <- stroke_admission_0417 %>%
  left_join(death_time_df, by = "subject_id") %>%
  mutate(
    hosp_los_days = if_else(is_dead, pmin(hosp_los_days, d_time), hosp_los_days)
  ) %>%
  select(-dod, -d_time, -is_dead)

stroke_icustays_0417 <- stroke_icustays_0417 %>%
  left_join(death_time_df, by = "subject_id") %>%
  mutate(
    icu_los_days_sim = if_else(
      is_dead & !is.na(icu_los_days_sim),
      pmin(icu_los_days_sim, d_time),
      icu_los_days_sim
    )
  ) %>%
  select(-dod, -d_time, -is_dead)

# =========================================================
# 4) 根据 hosp_los_days 重建 dischtime
# =========================================================
stroke_admission_0417 <- stroke_admission_0417 %>%
  mutate(
    admittime = as.POSIXct(admittime),
    dischtime = if_else(
      !is.na(admittime) & !is.na(hosp_los_days),
      admittime + hosp_los_days * 24 * 3600,
      as.POSIXct(NA)
    )
  )

# =========================================================
# 5) 重建 labevent/chartevent 的 charttime
#    规则：每个 subject_id + hadm_id 内
#          单条 -> admittime
#          多条 -> 首条=admittime，末条=dischtime，中间等间隔
# =========================================================
time_ref <- stroke_admission_0417 %>%
  select(subject_id, hadm_id, admittime, dischtime) %>%
  mutate(
    admittime = as.POSIXct(admittime),
    dischtime = as.POSIXct(dischtime)
  )

stroke_labevent_0417 <- stroke_labevent_0417 %>%
  mutate(charttime_raw = as.POSIXct(charttime)) %>%
  left_join(time_ref, by = c("subject_id", "hadm_id")) %>%
  group_by(subject_id, hadm_id) %>%
  arrange(charttime_raw, .by_group = TRUE) %>%
  mutate(
    n_rec = n(),
    rec_idx = row_number(),
    charttime = case_when(
      is.na(admittime) ~ as.POSIXct(NA),
      n_rec == 1 ~ admittime,
      is.na(dischtime) | dischtime <= admittime ~ admittime,
      TRUE ~ admittime + (rec_idx - 1) *
        (as.numeric(difftime(dischtime, admittime, units = "secs")) / (n_rec - 1))
    )
  ) %>%
  ungroup() %>%
  select(-charttime_raw, -admittime, -dischtime, -n_rec, -rec_idx)

stroke_chartevents_0417 <- stroke_chartevents_0417 %>%
  mutate(charttime_raw = as.POSIXct(charttime)) %>%
  left_join(time_ref, by = c("subject_id", "hadm_id")) %>%
  group_by(subject_id, hadm_id) %>%
  arrange(charttime_raw, .by_group = TRUE) %>%
  mutate(
    n_rec = n(),
    rec_idx = row_number(),
    charttime = case_when(
      is.na(admittime) ~ as.POSIXct(NA),
      n_rec == 1 ~ admittime,
      is.na(dischtime) | dischtime <= admittime ~ admittime,
      TRUE ~ admittime + (rec_idx - 1) *
        (as.numeric(difftime(dischtime, admittime, units = "secs")) / (n_rec - 1))
    )
  ) %>%
  ungroup() %>%
  select(-charttime_raw, -admittime, -dischtime, -n_rec, -rec_idx)

# =========================================================
# 6) 日内去重：同一患者同一天同一 itemid 仅保留第一条
# =========================================================
stroke_labevent_0423 <- stroke_labevent_0417 %>%
  mutate(charttime = as.POSIXct(charttime)) %>%
  arrange(subject_id, itemid, charttime) %>%
  mutate(measure_date = as.Date(charttime)) %>%
  group_by(subject_id, itemid, measure_date) %>%
  slice(1) %>%
  ungroup() %>%
  select(-measure_date)

stroke_chartevents_0423 <- stroke_chartevents_0417 %>%
  mutate(charttime = as.POSIXct(charttime)) %>%
  arrange(subject_id, itemid, charttime) %>%
  mutate(measure_date = as.Date(charttime)) %>%
  group_by(subject_id, itemid, measure_date) %>%
  slice(1) %>%
  ungroup() %>%
  select(-measure_date)

# =========================================================
# 7) 先筛 SOFA + APACHEII 变量，再筛“>=70%变量至少3次测量”患者
# =========================================================
labevent_0423 <- stroke_labevent_0423 %>%
  filter(itemid %in% labevent_itemids_0423)

charttime_0423 <- stroke_chartevents_0423 %>%
  filter(itemid %in% charttime_itemids_0423)

all_itemids <- unique(c(labevent_itemids_0423, charttime_itemids_0423))
n_items <- length(all_itemids)

lab_sub <- labevent_0423 %>%
  transmute(subject_id, itemid, charttime = as.POSIXct(charttime))

chart_sub <- charttime_0423 %>%
  transmute(subject_id, itemid, charttime = as.POSIXct(charttime))

meas_all <- bind_rows(lab_sub, chart_sub)

cnt_pid_item <- meas_all %>%
  group_by(subject_id, itemid) %>%
  summarise(n_meas = n(), .groups = "drop")

patient_ge3_ratio <- cnt_pid_item %>%
  filter(itemid %in% all_itemids) %>%
  group_by(subject_id) %>%
  summarise(
    n_item_ge3 = n_distinct(itemid[n_meas >= 3]),
    ratio_ge3 = n_item_ge3 / n_items,
    .groups = "drop"
  )

patient_0423 <- stroke_patients_0417 %>%
  semi_join(
    patient_ge3_ratio %>% filter(ratio_ge3 >= 0.7) %>% select(subject_id),
    by = "subject_id"
  )

# =========================================================
# 8) 最终目标数据
# =========================================================
labevent_0423_filter <- labevent_0423 %>%
  semi_join(patient_0423 %>% select(subject_id), by = "subject_id")

chartevent_0423_filter <- charttime_0423 %>%
  semi_join(patient_0423 %>% select(subject_id), by = "subject_id")

cat("patient_0423 人数:", nrow(patient_0423), "\n")
cat("labevent_0423_filter 行数:", nrow(labevent_0423_filter), "\n")
cat("chartevent_0423_filter 行数:", nrow(chartevent_0423_filter), "\n")

# 可选导出
# write.csv(labevent_0423_filter, "F:/文章_大论文/0417/代码/新建文件夹/labevent_0423_filter.csv", row.names = FALSE, fileEncoding = "UTF-8")
# write.csv(chartevent_0423_filter, "F:/文章_大论文/0417/代码/新建文件夹/chartevent_0423_filter.csv", row.names = FALSE, fileEncoding = "UTF-8")
library(readxl)
library(dplyr)
library(stringr)

write.csv(stroke_labevent_0417, "F:/文章_大论文/0417/代码/新建文件夹/stroke_labevent_0417_sim.csv", row.names = FALSE, fileEncoding = "UTF-8")
write.csv(stroke_chartevents_0417, "F:/文章_大论文/0417/代码/新建文件夹/stroke_chartevents_0417_sim.csv", row.names = FALSE, fileEncoding = "UTF-8")
stroke_admission_0417 <- read_excel("F:/文章_大论文/0417/stroke_admission_0417.xlsx")

stroke_chartevents_0417 <- read.csv("F:/文章_大论文/0417/代码/新建文件夹/stroke_chartevents_0417_sim.csv")

stroke_cohort_0417 <- read_excel("F:/文章_大论文/0417/stroke_cohort_0417.xlsx")
stroke_diagnoses_0416 <- read_excel("F:/文章_大论文/0417/stroke_diagnoses_0416.xlsx")
stroke_diagnoses_icd_0417 <- read_excel("F:/文章_大论文/0417/stroke_diagnoses_icd_0417.xlsx")

stroke_labevent_0417 <- read.csv("F:/文章_大论文/0417/stroke_labevent_0417.csv")

stroke_patients_0417 <- read_excel("F:/文章_大论文/0417/stroke_patients_0417.xlsx")
stroke_icustays_0417 <- read_excel("F:/文章_大论文/0417/stroke_icustays_0417.xlsx")
stroke_inputevents_0417 <- read_excel("F:/文章_大论文/0417/stroke_inputevents_0417.xlsx")
stroke_outputevents_0417 <- read_excel("F:/文章_大论文/0417/stroke_outputevents_0417.xlsx")
stroke_procedureevents_0417 <- read_excel("F:/文章_大论文/0417/stroke_procedureevents_0417.xlsx")






labevent_itemids_0423 <- c(
  # SOFA
  50821,  # PaO2
  51265,  # Platelet
  50885,  # Total Bilirubin
  50912,  # Creatinine
  # APACHE II
  50820,  # pH
  50882,  # HCO3
  50983,  # Sodium
  50971,  # Potassium
  51221,  # Hematocrit
  51300,  # WBC Count
  51301   # White Blood Cells
)

# charttime_0423: ICU 监测指标（来自 chartevents）
charttime_itemids_0423 <- c(
  # SOFA
  223835, # O2 Flow（可用于 FiO2 折算）
  3420,   # FiO2
  3422,   # FiO2 [Meas]
  220052, # MAP (arterial)
  220181, # MAP (NIBP)
  225312, # ART BP mean
  224322, # BP mean
  220739, # GCS eye
  223900, # GCS verbal
  223901, # GCS motor
  # APACHE II 常用生命体征
  220045, # Heart Rate
  220210, # Respiratory Rate
  223761, # Temperature Fahrenheit
  223762  # Temperature Celsius
)


############变量统计############
stroke_admission_0417



#######################
# 用 Weibull 分布为“已死亡患者”模拟生存时间（单位：天），并重建 dod
set.seed(20260420)

# 目标均值取 12 天
target_mean_days <- 12
weibull_shape <- 1.6
weibull_scale <- target_mean_days / gamma(1 + 1 / weibull_shape)

n_pat <- nrow(stroke_patients_0417)
d_time_sim <- rweibull(n_pat, shape = weibull_shape, scale = weibull_scale)

# 仅对原始 dod 非 NA（已死亡）患者生成 d_time
stroke_patients_0417 <- stroke_patients_0417 %>%
  mutate(
    died_flag = !is.na(dod),
    d_time = if_else(died_flag, round(pmax(d_time_sim, 1), 1), NA_real_)
  )

# 以 anchor_year 的 1 月 1 日作为基准时间，避免 intime 解析异常导致全 NA
stroke_patients_0417 <- stroke_patients_0417 %>%
  mutate(
    baseline_date = as.Date(paste0(anchor_year, "-01-01"))
  ) %>%
  mutate(
    dod = if_else(
      died_flag & !is.na(baseline_date),
      baseline_date + as.integer(round(d_time)),
      as.Date(NA)
    )
  ) %>%
  select(-baseline_date, -died_flag)

print(summary(stroke_patients_0417$d_time))
########################




##################
# 先识别 ICU 患者并模拟 ICU 住院时长，再校正总住院时长
set.seed(20260421)

# 目标分布
hosp_mean_target <- 13.91
hosp_sd_target <- 12.44
icu_mean_target <- 4.49
icu_sd_target <- 5.10

# ICU 住院患者（按 hadm_id 识别）
icu_hadm <- stroke_icustays_0417 %>%
  filter(!is.na(hadm_id)) %>%
  distinct(subject_id, hadm_id)

# 模拟 ICU 住院时长（天），截断避免负值
icu_hadm <- icu_hadm %>%
  mutate(
    icu_los_days_sim = round(pmax(rnorm(n(), mean = icu_mean_target, sd = icu_sd_target), 0.1), 2)
  )

# 回填 admission，标记 ICU 患者
stroke_admission_0417 <- stroke_admission_0417 %>%
  left_join(icu_hadm, by = c("subject_id", "hadm_id")) %>%
  mutate(icu_flag = !is.na(icu_los_days_sim))

# 全体 admission 先生成 hosp_los 初始值
hosp_raw <- pmax(
  rnorm(nrow(stroke_admission_0417), mean = hosp_mean_target, sd = hosp_sd_target),
  0.1
)

# ICU 患者总住院时长至少不小于 ICU 时长
hosp_raw <- ifelse(
  stroke_admission_0417$icu_flag,
  pmax(hosp_raw, stroke_admission_0417$icu_los_days_sim),
  hosp_raw
)

# 迭代校正到目标均值/标准差，并维持业务约束
hosp_adj <- hosp_raw
for (i in 1:3) {
  cur_mean <- mean(hosp_adj, na.rm = TRUE)
  cur_sd <- sd(hosp_adj, na.rm = TRUE)
  if (!is.na(cur_sd) && cur_sd > 0) {
    hosp_adj <- (hosp_adj - cur_mean) / cur_sd * hosp_sd_target + hosp_mean_target
  }
  hosp_adj <- pmax(hosp_adj, 0.1)
  hosp_adj <- ifelse(
    stroke_admission_0417$icu_flag,
    pmax(hosp_adj, stroke_admission_0417$icu_los_days_sim),
    hosp_adj
  )
}

stroke_admission_0417 <- stroke_admission_0417 %>%
  mutate(hosp_los_days = round(hosp_adj, 2))

# 可选：同步到 icustays（同一住院号共享同一个模拟 ICU 时长）
stroke_icustays_0417 <- stroke_icustays_0417 %>%
  left_join(
    icu_hadm %>% select(hadm_id, icu_los_days_sim),
    by = "hadm_id"
  )

cat("hosp_los_days mean±sd:",
    round(mean(stroke_admission_0417$hosp_los_days, na.rm = TRUE), 2), "±",
    round(sd(stroke_admission_0417$hosp_los_days, na.rm = TRUE), 2), "\n")
cat("icu_los_days_sim mean±sd:",
    round(mean(icu_hadm$icu_los_days_sim, na.rm = TRUE), 2), "±",
    round(sd(icu_hadm$icu_los_days_sim, na.rm = TRUE), 2), "\n")
###################






















############
# 二次保障：死亡患者住院时长不应超过其 d_time
death_time_df <- stroke_patients_0417 %>%
  select(subject_id, dod, d_time) %>%
  mutate(is_dead = !is.na(dod) & !is.na(d_time))

# 调整 admission 的 hosp_los_days
stroke_admission_0417 <- stroke_admission_0417 %>%
  left_join(death_time_df, by = "subject_id") %>%
  mutate(
    hosp_los_days = if_else(
      is_dead,
      pmin(hosp_los_days, d_time),
      hosp_los_days
    )
  ) %>%
  select(-dod, -d_time, -is_dead)

# 调整 icustays 的 icu_los_days_sim
stroke_icustays_0417 <- stroke_icustays_0417 %>%
  left_join(death_time_df, by = "subject_id") %>%
  mutate(
    icu_los_days_sim = if_else(
      is_dead & !is.na(icu_los_days_sim),
      pmin(icu_los_days_sim, d_time),
      icu_los_days_sim
    )
  ) %>%
  select(-dod, -d_time, -is_dead)

cat("After death constraint - hosp_los_days mean±sd:",
    round(mean(stroke_admission_0417$hosp_los_days, na.rm = TRUE), 2), "±",
    round(sd(stroke_admission_0417$hosp_los_days, na.rm = TRUE), 2), "\n")
cat("After death constraint - icu_los_days_sim mean±sd:",
    round(mean(stroke_icustays_0417$icu_los_days_sim, na.rm = TRUE), 2), "±",
    round(sd(stroke_icustays_0417$icu_los_days_sim, na.rm = TRUE), 2), "\n")
#############





################
# 根据 hosp_los_days 重建 dischtime（不修改 admittime）
stroke_admission_0417 <- stroke_admission_0417 %>%
  mutate(
    admittime = as.POSIXct(admittime),
    dischtime = if_else(
      !is.na(admittime) & !is.na(hosp_los_days),
      admittime + hosp_los_days * 24 * 3600,
      as.POSIXct(NA)
    )
  )

cat("dischtime reconstructed by admittime + hosp_los_days\n")








#################
# 基于 admission 时间重建 labevent 的 charttime
# 规则：单条记录对齐 admittime；多条记录首条对齐 admittime、末条对齐 dischtime，中间等间隔分布
lab_time_ref <- stroke_admission_0417 %>%
  select(subject_id, hadm_id, admittime, dischtime) %>%
  mutate(
    admittime = as.POSIXct(admittime),
    dischtime = as.POSIXct(dischtime)
  )

stroke_labevent_0417 <- stroke_labevent_0417 %>%
  mutate(charttime_raw = as.POSIXct(charttime)) %>%
  left_join(lab_time_ref, by = c("subject_id", "hadm_id")) %>%
  group_by(subject_id, hadm_id) %>%
  arrange(charttime_raw, .by_group = TRUE) %>%
  mutate(
    n_rec = n(),
    rec_idx = row_number(),
    charttime = case_when(
      is.na(admittime) ~ as.POSIXct(NA),
      n_rec == 1 ~ admittime,
      is.na(dischtime) | dischtime <= admittime ~ admittime,
      TRUE ~ admittime + (rec_idx - 1) *
        (as.numeric(difftime(dischtime, admittime, units = "secs")) / (n_rec - 1))
    )
  ) %>%
  ungroup() %>%
  select(-charttime_raw, -admittime, -dischtime, -n_rec, -rec_idx)
############
# 基于 admission 时间重建 chartevents 的 charttime（同 labevent 规则）
chart_time_ref <- stroke_admission_0417 %>%
  select(subject_id, hadm_id, admittime, dischtime) %>%
  mutate(
    admittime = as.POSIXct(admittime),
    dischtime = as.POSIXct(dischtime)
  )

stroke_chartevents_0417 <- stroke_chartevents_0417 %>%
  mutate(charttime_raw = as.POSIXct(charttime)) %>%
  left_join(chart_time_ref, by = c("subject_id", "hadm_id")) %>%
  group_by(subject_id, hadm_id) %>%
  arrange(charttime_raw, .by_group = TRUE) %>%
  mutate(
    n_rec = n(),
    rec_idx = row_number(),
    charttime = case_when(
      is.na(admittime) ~ as.POSIXct(NA),
      n_rec == 1 ~ admittime,
      is.na(dischtime) | dischtime <= admittime ~ admittime,
      TRUE ~ admittime + (rec_idx - 1) *
        (as.numeric(difftime(dischtime, admittime, units = "secs")) / (n_rec - 1))
    )
  ) %>%
  ungroup() %>%
  select(-charttime_raw, -admittime, -dischtime, -n_rec, -rec_idx)
###########


#write.csv(stroke_labevent_0417, "F:/文章_大论文/0417/代码/新建文件夹/stroke_labevent_0417_sim.csv", row.names = FALSE, fileEncoding = "UTF-8")
#write.csv(stroke_chartevents_0417, "F:/文章_大论文/0417/代码/新建文件夹/stroke_chartevents_0417_sim.csv", row.names = FALSE, fileEncoding = "UTF-8")
##################
# 日内去重：同一患者同一天同一 itemid 仅保留第一条 labevent
stroke_labevent_0423 <- stroke_labevent_0417 %>%
  mutate(charttime = as.POSIXct(charttime)) %>%
  arrange(subject_id, itemid, charttime) %>%
  mutate(measure_date = as.Date(charttime)) %>%
  group_by(subject_id, itemid, measure_date) %>%
  slice(1) %>%
  ungroup() %>%
  select(-measure_date)


# 日内去重：同一患者同一天同一 itemid 仅保留第一条 chartevent
stroke_chartevents_0423 <- stroke_chartevents_0417 %>%
  mutate(charttime = as.POSIXct(charttime)) %>%
  arrange(subject_id, itemid, charttime) %>%
  mutate(measure_date = as.Date(charttime)) %>%
  group_by(subject_id, itemid, measure_date) %>%
  slice(1) %>%
  ungroup() %>%
  select(-measure_date)




write.csv(stroke_labevent_0423, "F:/文章_大论文/0417/代码/新建文件夹/stroke_labevent_0423_sim.csv", row.names = FALSE, fileEncoding = "UTF-8")
write.csv(stroke_chartevents_0423, "F:/文章_大论文/0417/代码/新建文件夹/stroke_chartevents_0423_sim.csv", row.names = FALSE, fileEncoding = "UTF-8")

############


all_itemids <- unique(c(labevent_itemids_0423, charttime_itemids_0423))
n_items <- length(all_itemids)

patient_ge3_ratio <- cnt_pid_item %>%
  filter(itemid %in% all_itemids) %>%
  group_by(subject_id) %>%
  summarise(
    n_item_ge3 = n_distinct(itemid[n_meas >= 3]),
    ratio_ge3 = n_item_ge3 / n_items,
    .groups = "drop"
  )

n_patient_ge3_70 <- patient_ge3_ratio %>%
  filter(ratio_ge3 >= 0.7) %>%
  nrow()

cat(">=70%变量都>=3次测量的患者数:", n_patient_ge3_70, "\n")



###############



# 提取 >=70%变量都>=3次测量 的患者ID
id_0423 <- patient_ge3_ratio %>%
  filter(ratio_ge3 >= 0.7) %>%
  distinct(subject_id)
# 从患者表中提取这 4593 人
patient_0423 <- stroke_patients_0417 %>%
  semi_join(id_0423, by = "subject_id")
cat("patient_0423 人数:", nrow(patient_0423), "\n")




# 按 patient_0423 的 subject_id 筛选事件表
labevent_0423_filter <- stroke_labevent_0423 %>%
  semi_join(patient_0423 %>% select(subject_id), by = "subject_id")
chartevent_0423_filter <- stroke_chartevents_0423 %>%
  semi_join(patient_0423 %>% select(subject_id), by = "subject_id")
cat("labevent_0423_filter 行数:", nrow(labevent_0423_filter), "\n")
cat("chartevent_0423_filter 行数:", nrow(chartevent_0423_filter), "\n")



#write.csv(labevent_0423_filter, "F:/文章_大论文/0417/代码/新建文件夹/labevent_0423_filter.csv", row.names = FALSE, fileEncoding = "UTF-8")
#write.csv(chartevent_0423_filter, "F:/文章_大论文/0417/代码/新建文件夹/chartevent_0423_filter.csv", row.names = FALSE, fileEncoding = "UTF-8")

##################################
stroke_patients_0417 <- read_excel("F:/文章_大论文/0417/stroke_patients_0417.xlsx")
stroke_admission_0417 <- read_excel("F:/文章_大论文/0417/stroke_admission_0417.xlsx")

labevent_0423_filter <- read.csv("F:/文章_大论文/0417/代码/新建文件夹/labevent_0423_filter.csv")
chartevent_0423_filter <- read.csv("F:/文章_大论文/0417/代码/新建文件夹/chartevent_0423_filter.csv")





















