library(dplyr)
library(tidyr)
library(readxl)
library(purrr)

input_dir <- "F:/文章_大论文/0319大改/测试集/模拟数据_10V1C/生成Y"
extract_dir <- "F:/文章_大论文/0521/提取文件"

sim500_30_10V_lowINTER_1c_L1_df <- read_xlsx(
  file.path(input_dir, "sim500_30_10V_lowINTER_1c_L1.xlsx"),
  sheet = 1
)
stroke_admission_0520<- read.csv("F:/文章_大论文/0521/提取文件/stroke_admission_0520.csv")
stroke_aki_0520<- read.csv("F:/文章_大论文/0521/提取文件/stroke_aki_0520.csv")
stroke_apsiii_0520<- read.csv("F:/文章_大论文/0521/提取文件/stroke_apsiii_0520.csv")
stroke_bg_0520<- read.csv("F:/文章_大论文/0521/提取文件/stroke_bg_0520.csv")
stroke_charlson_0520<- read.csv("F:/文章_大论文/0521/提取文件/stroke_charlson_0520.csv")

stroke_cohort_0520<- read.csv("F:/文章_大论文/0521/提取文件/stroke_cohort_0520.csv")
stroke_delirium_0520<- read.csv("F:/文章_大论文/0521/提取文件/stroke_delirium_0520.csv")
stroke_diagnoses_0520<- read.csv("F:/文章_大论文/0521/提取文件/stroke_diagnoses_0520.csv")
stroke_gcs_0520<- read.csv("F:/文章_大论文/0521/提取文件/stroke_gcs_0520.csv")
stroke_icustays_0520<- read.csv("F:/文章_大论文/0521/提取文件/stroke_icustays_0520.csv")

stroke_oasis_0520<- read.csv("F:/文章_大论文/0521/提取文件/stroke_oasis_0520.csv")
stroke_outcome_0520<- read.csv("F:/文章_大论文/0521/提取文件/stroke_outcome_0520.csv")
stroke_outputevents_0520<- read.csv("F:/文章_大论文/0521/提取文件/stroke_outputevents_0520.csv")
stroke_sapsii_0520<- read.csv("F:/文章_大论文/0521/提取文件/stroke_sapsii_0520.csv")
stroke_sofa_0520<- read.csv("F:/文章_大论文/0521/提取文件/stroke_sofa_0520.csv")
stroke_stroketype_0520<- read.csv("F:/文章_大论文/0521/提取文件/stroke_stroketype_0520.csv")

stroke_labdaliy_missingrate_0527<-read.csv("F:/文章_大论文/0521/提取文件/stroke_labdaliy_missingrate_0527.csv")
stroke_chartdaliy_missingrate_0527<-read.csv("F:/文章_大论文/0521/提取文件/stroke_chartdaliy_missingrate_0527.csv")


############纵向数据处理#########



stroke_labdaliy_long_0528 <- stroke_labdaliy_missingrate_0527 %>%
  # 同一天同一 itemid 若有多条，先合并
  group_by(subject_id, hadm_id, chart_date, itemid) %>%
  summarise(value = max(value, na.rm = TRUE), .groups = "drop") %>%
  # 按 hadm_id 内日期排序，给每个 chart_date 一个 times
  mutate(chart_date_parsed = as.Date(chart_date, format = "%d/%m/%Y")) %>%
  group_by(hadm_id) %>%
  arrange(chart_date_parsed, .by_group = TRUE) %>%
  mutate(times = dense_rank(chart_date_parsed)) %>%
  ungroup() %>%
  select(-chart_date_parsed) %>%
  # 同一天的所有 itemid 合并到同一行
  pivot_wider(
    id_cols = c(subject_id, hadm_id, chart_date, times),
    names_from = itemid,
    values_from = value,
    names_prefix = "item_"
  )


stroke_chartdaliy_long_0528 <- stroke_chartdaliy_missingrate_0527 %>%
  # 同一天同一 itemid 若有多条，先合并
  group_by(subject_id, hadm_id, chart_date, itemid) %>%
  summarise(value = max(value, na.rm = TRUE), .groups = "drop") %>%
  # 按 hadm_id 内日期排序，给每个 chart_date 一个 times
  mutate(chart_date_parsed = as.Date(chart_date, format = "%d/%m/%Y")) %>%
  group_by(hadm_id) %>%
  arrange(chart_date_parsed, .by_group = TRUE) %>%
  mutate(times = dense_rank(chart_date_parsed)) %>%
  ungroup() %>%
  select(-chart_date_parsed) %>%
  # 同一天的所有 itemid 合并到同一行
  pivot_wider(
    id_cols = c(subject_id, hadm_id, chart_date, times),
    names_from = itemid,
    values_from = value,
    names_prefix = "item_"
  )



# Delirium assessment only
stroke_deliriumdaliy_0528 <- stroke_delirium_0520 %>%
  mutate(itemid = as.integer(itemid)) %>%
  filter(itemid == 228332) %>%
  mutate(
    chart_date = format(
      as.POSIXct(charttime, format = "%d/%m/%Y %H:%M:%S"),
      format = "%d/%m/%Y"
    ),
    value_num = case_when(
      value == "Positive" ~ 1,
      value == "Negative" ~ 0,
      TRUE ~ NA_real_
    )
  ) %>%
  group_by(subject_id, hadm_id, chart_date, itemid) %>%
  summarise(
    value = case_when(
      any(value_num == 1, na.rm = TRUE) ~ 1,
      any(value_num == 0, na.rm = TRUE) ~ 0,
      TRUE ~ NA_real_
    ),
    .groups = "drop"
  ) %>%
  filter(!is.na(value)) %>%
  mutate(chart_date_parsed = as.Date(chart_date, format = "%d/%m/%Y")) %>%
  group_by(hadm_id) %>%
  arrange(chart_date_parsed, .by_group = TRUE) %>%
  mutate(times = dense_rank(chart_date_parsed)) %>%
  ungroup() %>%
  select(-chart_date_parsed) %>%
  pivot_wider(
    id_cols = c(subject_id, hadm_id, chart_date, times),
    names_from = itemid,
    values_from = value,
    names_prefix = "item_"
  )



# 定义所需的所有尿量相关 itemid（MIMIC-IV 官方衍生表 urin_output 使用的标准集）
urine_itemids <- c(
  226557, 226558, 226559, 226560, 226561, 226563,
  226564, 226565, 226567, 226584, 227488, 227489
)

# 冲洗液输入 itemid（需转为负值）
irrigant_in_itemid <- 227488

stroke_outputdaliy_0528 <- stroke_outputevents_0520 %>%
  mutate(
    itemid = as.integer(itemid),
    value = as.numeric(value)
  ) %>%
  filter(itemid %in% urine_itemids) %>%
  mutate(
    # 关键步骤：将冲洗液入量（227488）的 value 转为负值，其余保持不变
    value_adj = if_else(itemid == irrigant_in_itemid & !is.na(value), -value, value),
    # 处理 charttime：假设原始格式为 "%d/%m/%Y %H:%M:%S"
    chart_date = as.Date(charttime, format = "%d/%m/%Y")
  ) %>%
  filter(!is.na(chart_date)) %>%   # 剔除日期缺失的记录
  group_by(subject_id, hadm_id, chart_date) %>%
  summarise(
    # 净尿量 = 所有正值尿量 + 负的冲洗液入量
    total_urine_output = sum(value_adj, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # 可选：过滤掉生理上不可能的负值（若出现负值，说明冲洗液记录过多）
  filter(total_urine_output >= 0 | abs(total_urine_output) < 1e-6) %>%
  group_by(hadm_id) %>%
  arrange(chart_date, .by_group = TRUE) %>%
  mutate(times = dense_rank(chart_date)) %>%
  ungroup()


stroke_akidaliy_0528 <- stroke_aki_0520 %>%
  mutate(
    chart_date = format(
      as.POSIXct(charttime, format = "%d/%m/%Y %H:%M:%S"),
      format = "%d/%m/%Y"
    ),
    creat = as.numeric(creat),
    aki_stage = as.numeric(aki_stage)
  ) %>%
  group_by(subject_id, hadm_id, chart_date) %>%
  summarise(
    creat = if (any(!is.na(creat))) max(creat, na.rm = TRUE) else NA_real_,
    aki_stage = if (any(!is.na(aki_stage))) max(aki_stage, na.rm = TRUE) else NA_real_,
    .groups = "drop"
  ) %>%
  filter(!is.na(creat) | !is.na(aki_stage)) %>%
  mutate(chart_date_parsed = as.Date(chart_date, format = "%d/%m/%Y")) %>%
  group_by(hadm_id) %>%
  arrange(chart_date_parsed, .by_group = TRUE) %>%
  mutate(times = dense_rank(chart_date_parsed)) %>%
  ungroup() %>%
  select(-chart_date_parsed)



stroke_gcsdaliy_0528 <- stroke_gcs_0520 %>%
  mutate(
    chart_date = format(
      as.POSIXct(charttime, format = "%d/%m/%Y %H:%M:%S"),
      format = "%d/%m/%Y"
    ),
    gcs = as.numeric(gcs)
  ) %>%
  group_by(subject_id, hadm_id, chart_date) %>%
  summarise(
    gcs = if (any(!is.na(gcs))) min(gcs, na.rm = TRUE) else NA_real_,
    .groups = "drop"
  ) %>%
  filter(!is.na(gcs)) %>%
  mutate(chart_date_parsed = as.Date(chart_date, format = "%d/%m/%Y")) %>%
  group_by(hadm_id) %>%
  arrange(chart_date_parsed, .by_group = TRUE) %>%
  mutate(times = dense_rank(chart_date_parsed)) %>%
  ungroup() %>%
  select(-chart_date_parsed)




stroke_bgdaliy_0528 <- stroke_bg_0520 %>%
  mutate(
    chart_date = format(
      as.POSIXct(charttime, format = "%d/%m/%Y %H:%M:%S"),
      format = "%d/%m/%Y"
    ),
    across(
      c(
        ph, pco2, pao2fio2ratio, lactate, glucose, sodium,
        po2, fio2, bicarbonate, hemoglobin, temperature
      ),
      as.numeric
    )
  ) %>%
  group_by(subject_id, hadm_id, chart_date) %>%
  summarise(
    ph              = if (any(!is.na(ph))) min(ph, na.rm = TRUE) else NA_real_,
    pco2            = if (any(!is.na(pco2))) mean(pco2, na.rm = TRUE) else NA_real_,
    pao2fio2ratio   = if (any(!is.na(pao2fio2ratio))) min(pao2fio2ratio, na.rm = TRUE) else NA_real_,
    lactate         = if (any(!is.na(lactate))) max(lactate, na.rm = TRUE) else NA_real_,
    glucose         = if (any(!is.na(glucose))) max(glucose, na.rm = TRUE) else NA_real_,
    sodium          = if (any(!is.na(sodium))) mean(sodium, na.rm = TRUE) else NA_real_,
    po2             = if (any(!is.na(po2))) min(po2, na.rm = TRUE) else NA_real_,
    fio2            = if (any(!is.na(fio2))) max(fio2, na.rm = TRUE) else NA_real_,
    bicarbonate     = if (any(!is.na(bicarbonate))) min(bicarbonate, na.rm = TRUE) else NA_real_,
    hemoglobin      = if (any(!is.na(hemoglobin))) min(hemoglobin, na.rm = TRUE) else NA_real_,
    temperature     = if (any(!is.na(temperature))) max(temperature, na.rm = TRUE) else NA_real_,
    .groups = "drop"
  ) %>%
  filter(
    if_any(
      c(
        ph, pco2, pao2fio2ratio, lactate, glucose, sodium,
        po2, fio2, bicarbonate, hemoglobin, temperature
      ),
      ~ !is.na(.)
    )
  ) %>%
  mutate(chart_date_parsed = as.Date(chart_date, format = "%d/%m/%Y")) %>%
  group_by(hadm_id) %>%
  arrange(chart_date_parsed, .by_group = TRUE) %>%
  mutate(times = dense_rank(chart_date_parsed)) %>%
  ungroup() %>%
  select(-chart_date_parsed)



bg_vars <- c(
  "ph", "pco2", "pao2fio2ratio", "lactate", "glucose", "sodium",
  "po2", "fio2", "bicarbonate", "hemoglobin", "temperature"
)
# 1. 整体缺失率（按行数）
stroke_bgdaliy_missingrate_0528 <- stroke_bgdaliy_0528 %>%
  summarise(across(all_of(bg_vars), ~ mean(is.na(.)) * 100)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "missing_pct") %>%
  mutate(
    n_total = nrow(stroke_bgdaliy_0528),
    n_missing = round(n_total * missing_pct / 100),
    n_non_missing = n_total - n_missing
  ) %>%
  arrange(desc(missing_pct))


sofa_vars <- c(
  "sofa_24hours",
  "cns_24hours",
  "renal_24hours",
  "cardiovascular_24hours",
  "respiration_24hours"
)

stroke_sofadaliy_0528 <- stroke_sofa_0520 %>%
  mutate(
    chart_date = format(
      as.POSIXct(starttime, format = "%d/%m/%Y %H:%M:%S"),
      format = "%d/%m/%Y"
    ),
    across(all_of(sofa_vars), as.numeric)
  ) %>%
  group_by(subject_id, hadm_id, chart_date) %>%
  summarise(
    across(
      all_of(sofa_vars),
      ~ if (any(!is.na(.x))) max(.x, na.rm = TRUE) else NA_real_
    ),
    .groups = "drop"
  ) %>%
  filter(if_any(all_of(sofa_vars), ~ !is.na(.))) %>%
  mutate(chart_date_parsed = as.Date(chart_date, format = "%d/%m/%Y")) %>%
  group_by(hadm_id) %>%
  arrange(chart_date_parsed, .by_group = TRUE) %>%
  mutate(times = dense_rank(chart_date_parsed)) %>%
  ungroup() %>%
  select(-chart_date_parsed)



######




print(stroke_sofa_0520,20)
#######################




daily_to_long <- function(df, source, id_cols = c("subject_id", "hadm_id", "chart_date")) {
  df %>%
    mutate(chart_date = as.Date(chart_date, format = "%d/%m/%Y")) %>%
    select(-any_of("times")) %>%
    pivot_longer(
      cols = -all_of(id_cols),
      names_to = "variable",
      values_to = "value",
      values_drop_na = TRUE
    ) %>%
    mutate(source = source)
}

long_all <- bind_rows(
  daily_to_long(stroke_labdaliy_long_0528,      "lab"),
  daily_to_long(stroke_chartdaliy_long_0528,    "chart"),
  daily_to_long(stroke_deliriumdaliy_0528,      "delirium"),
  daily_to_long(stroke_outputdaliy_0528,        "output"),
  daily_to_long(stroke_akidaliy_0528,           "aki"),
  daily_to_long(stroke_gcsdaliy_0528,           "gcs"),
  daily_to_long(stroke_bgdaliy_0528,            "bg"),
  daily_to_long(stroke_sofadaliy_0528,          "sofa")
)

stroke_longitudinal_merged_0528 <- long_all %>%
  distinct(subject_id, hadm_id, chart_date, variable, .keep_all = TRUE) %>%
  select(subject_id, hadm_id, chart_date, variable, value) %>%
  pivot_wider(
    id_cols = c(subject_id, hadm_id, chart_date),
    names_from = variable,
    values_from = value
  ) %>%
  group_by(hadm_id) %>%
  arrange(chart_date, .by_group = TRUE) %>%
  mutate(times = dense_rank(chart_date)) %>%
  ungroup()














# 各纵向 daily 宽表
daily_tables <- list(
  lab      = stroke_labdaliy_long_0528,
  chart    = stroke_chartdaliy_long_0528,
  delirium = stroke_deliriumdaliy_0528,
  output   = stroke_outputdaliy_0528,
  aki      = stroke_akidaliy_0528,
  gcs      = stroke_gcsdaliy_0528,
  bg       = stroke_bgdaliy_0528,
  sofa     = stroke_sofadaliy_0528
)
# 1. 各表 hadm_id / subject_id 数量
hadm_subject_coverage_0528 <- imap_dfr(daily_tables, function(df, table_name) {
  tibble(
    table = table_name,
    n_hadm_id    = n_distinct(df$hadm_id),
    n_subject_id = n_distinct(df$subject_id),
    n_patient_day = nrow(df)
  )
})
# 2. 以所有表的并集为基准，计算覆盖率
all_hadm    <- daily_tables %>% map("hadm_id")    %>% reduce(union)
all_subject <- daily_tables %>% map("subject_id") %>% reduce(union)
hadm_subject_coverage_0528 <- hadm_subject_coverage_0528 %>%
  mutate(
    n_hadm_all    = length(all_hadm),
    n_subject_all = length(all_subject),
    hadm_coverage_pct    = round(n_hadm_id    / n_hadm_all    * 100, 2),
    subject_coverage_pct = round(n_subject_id / n_subject_all * 100, 2)
  )
print(hadm_subject_coverage_0528)






id_cols <- c("subject_id", "hadm_id", "chart_date", "times")
var_cols <- setdiff(names(stroke_longitudinal_merged_0528), id_cols)
n_hadm_total <- n_distinct(stroke_longitudinal_merged_0528$hadm_id)
# 1) 以 hadm_id 为分母：该变量在该住院「所有随访日均为 NA」的 hadm 占比
hadm_level_missing <- stroke_longitudinal_merged_0528 %>%
  group_by(hadm_id) %>%
  summarise(
    across(all_of(var_cols), ~ all(is.na(.x)), .names = "{.col}"),
    n_days = n(),
    .groups = "drop"
  )
stroke_longitudinal_missingrate_by_hadm_0528 <- hadm_level_missing %>%
  summarise(
    n_hadm_total = n(),
    across(all_of(var_cols), ~ sum(.x), .names = "n_hadm_all_missing_{.col}")
  ) %>%
  pivot_longer(
    starts_with("n_hadm_all_missing_"),
    names_to = "variable",
    names_prefix = "n_hadm_all_missing_",
    values_to = "n_hadm_all_missing"
  ) %>%
  mutate(
    n_hadm_with_data = n_hadm_total - n_hadm_all_missing,
    missing_pct_by_hadm = round(n_hadm_all_missing / n_hadm_total * 100, 2),
    coverage_pct_by_hadm = round(100 - missing_pct_by_hadm, 2)
  ) %>%
  arrange(desc(missing_pct_by_hadm))



write.csv(stroke_longitudinal_merged_0528,"F:/文章_大论文/0521/处理后文件/一次处理/stroke_longitudinal_merged_0528.csv")
write.csv(stroke_longitudinal_missingrate_by_hadm_0528,"F:/文章_大论文/0521/处理后文件/一次处理/stroke_longitudinal_missingrate_by_hadm_0528.csv")






#################筛选患者################

# 1) 卒中队列 hadm_id（上游已完成 ICD 筛选，此处仅取唯一住院）
hadm_stroke <- stroke_diagnoses_0520 %>%
  distinct(hadm_id, subject_id)
# 2) 年龄 >= 18（住院级）
admission_eligible <- stroke_admission_0520 %>%
  semi_join(hadm_stroke, by = c("hadm_id", "subject_id")) %>%
  filter(!is.na(age_at_admit), age_at_admit >= 18)
# 3) 每个 hadm_id 取首次 ICU 入住；剔除 ICU 住院时间 <= 24h
icu_index <- stroke_icustays_0520 %>%
  semi_join(hadm_stroke, by = c("hadm_id", "subject_id")) %>%
  mutate(
    intime_parsed  = as.POSIXct(intime,  format = "%d/%m/%Y %H:%M:%S"),
    outtime_parsed = as.POSIXct(outtime, format = "%d/%m/%Y %H:%M:%S"),
    icu_los_hours  = as.numeric(difftime(outtime_parsed, intime_parsed, units = "hours"))
  ) %>%
  group_by(hadm_id) %>%
  arrange(intime_parsed, stay_id, .by_group = TRUE) %>%
  slice(1) %>%
  ungroup() %>%
  filter(!is.na(icu_los_hours), icu_los_hours > 24)
# 4) 合并为 hadm_id 级分析队列（每个 hadm_id 一行）
stroke_patient_filtered_0528 <- admission_eligible %>%
  inner_join(
    icu_index %>%
      select(
        subject_id, hadm_id, stay_id,
        first_careunit, last_careunit,
        intime, outtime,
        icu_los_days, icu_los_hours, icu_expire_flag
      ),
    by = c("subject_id", "hadm_id")
  ) %>%
  distinct(hadm_id, .keep_all = TRUE)






######根据你现有的筛选流程，下面这段代码会生成汇总表和分步剔除统计：

library(dplyr)
library(tidyr)
# ---------- 筛选流程（与 stroke_patient_filtered_0528 一致）----------
hadm_stroke <- stroke_diagnoses_0520 %>%
  distinct(hadm_id, subject_id)
admission_eligible <- stroke_admission_0520 %>%
  semi_join(hadm_stroke, by = c("hadm_id", "subject_id")) %>%
  filter(!is.na(age_at_admit), age_at_admit >= 18)
icu_first <- stroke_icustays_0520 %>%
  semi_join(hadm_stroke, by = c("hadm_id", "subject_id")) %>%
  mutate(
    intime_parsed  = as.POSIXct(intime,  format = "%d/%m/%Y %H:%M:%S"),
    outtime_parsed = as.POSIXct(outtime, format = "%d/%m/%Y %H:%M:%S"),
    icu_los_hours  = as.numeric(difftime(outtime_parsed, intime_parsed, units = "hours"))
  ) %>%
  group_by(hadm_id) %>%
  arrange(intime_parsed, stay_id, .by_group = TRUE) %>%
  slice(1) %>%
  ungroup()
icu_index <- icu_first %>%
  filter(!is.na(icu_los_hours), icu_los_hours > 24)
stroke_patient_filtered_0528 <- admission_eligible %>%
  inner_join(
    icu_index %>% select(subject_id, hadm_id),
    by = c("subject_id", "hadm_id")
  ) %>%
  distinct(hadm_id, .keep_all = TRUE)
# ---------- 辅助函数：统计 hadm / subject 数量 ----------
count_hadm_subj <- function(df) {
  c(
    hadm_id   = n_distinct(df$hadm_id),
    subject_id = n_distinct(df$subject_id)
  )
}
# 各步骤队列
step_list <- list(
  "0_筛选前(卒中ICD队列)"     = hadm_stroke,
  "1_年龄>=18后"              = admission_eligible,
  "2_有首次ICU记录后"         = icu_first,
  "3_ICU住院>24h后"           = icu_index,
  "4_最终纳入"                = stroke_patient_filtered_0528
)
step_counts <- imap_dfr(step_list, function(df, step_name) {
  n <- count_hadm_subj(df)
  tibble(
    步骤 = step_name,
    hadm_id = n[["hadm_id"]],
    subject_id = n[["subject_id"]]
  )
})
# ---------- 表1：筛选前 vs 筛选后 vs 剔除 ----------
filter_summary_table <- tibble(
  统计口径 = c("hadm_id（住院，分析单位）", "subject_id（患者）"),
  筛选前 = c(
    step_counts$hadm_id[step_counts$步骤 == "0_筛选前(卒中ICD队列)"],
    step_counts$subject_id[step_counts$步骤 == "0_筛选前(卒中ICD队列)"]
  ),
  筛选后 = c(
    step_counts$hadm_id[step_counts$步骤 == "4_最终纳入"],
    step_counts$subject_id[step_counts$步骤 == "4_最终纳入"]
  )
) %>%
  mutate(剔除 = 筛选前 - 筛选后)
print(filter_summary_table)
# ---------- 表2：分步剔除（每步剔除多少）----------
step_exclusion_table <- step_counts %>%
  arrange(步骤) %>%
  mutate(
    本步剔除_hadm_id = hadm_id - lead(hadm_id),
    本步剔除_subject_id = subject_id - lead(subject_id)
  ) %>%
  filter(!is.na(本步剔除_hadm_id)) %>%
  transmute(
    剔除步骤 = case_when(
      步骤 == "0_筛选前(卒中ICD队列)" ~ "年龄<18或缺失",
      步骤 == "1_年龄>=18后"         ~ "无ICU记录",
      步骤 == "2_有首次ICU记录后"   ~ "首次ICU住院<=24h",
      TRUE ~ 步骤
    ),
    剔除_hadm_id = abs(本步剔除_hadm_id),
    剔除_subject_id = abs(本步剔除_subject_id)
  )

write.csv(step_exclusion_table,"F:/文章_大论文/0521/处理后文件/一次处理/step_exclusion_table.csv")
write.csv(step_counts,"F:/文章_大论文/0521/处理后文件/一次处理/step_counts.csv")
write.csv(stroke_patient_filtered_0528,"F:/文章_大论文/0521/处理后文件/一次处理/stroke_patient_filtered_0528.csv")











