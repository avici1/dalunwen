library(dplyr)
library(tidyr)
library(readxl)
library(purrr)


stroke_longitudinal_merged_0528<-read.csv("F:/文章_大论文/0521/处理后文件/一次处理/stroke_longitudinal_merged_0528.csv")
stroke_longitudinal_missingrate_by_hadm_0528<-read.csv("F:/文章_大论文/0521/处理后文件/一次处理/stroke_longitudinal_missingrate_by_hadm_0528.csv")
stroke_patient_filtered_0528<-read.csv("F:/文章_大论文/0521/处理后文件/一次处理/stroke_patient_filtered_0528.csv")


stroke_longitudinal_filter_0530 <- stroke_longitudinal_merged_0528 %>%
  filter(hadm_id %in% stroke_patient_filtered_0528$hadm_id)

##基线数据###
stroke_admission_0520<- read.csv("F:/文章_大论文/0521/提取文件/stroke_admission_0520.csv")
stroke_apsiii_0520<- read.csv("F:/文章_大论文/0521/提取文件/stroke_apsiii_0520.csv")
stroke_charlson_0520<- read.csv("F:/文章_大论文/0521/提取文件/stroke_charlson_0520.csv")
stroke_icustays_0520<- read.csv("F:/文章_大论文/0521/提取文件/stroke_icustays_0520.csv")
stroke_oasis_0520<- read.csv("F:/文章_大论文/0521/提取文件/stroke_oasis_0520.csv")
stroke_sapsii_0520<- read.csv("F:/文章_大论文/0521/提取文件/stroke_sapsii_0520.csv")
stroke_stroketype_0520<- read.csv("F:/文章_大论文/0521/提取文件/stroke_stroketype_0520.csv")




##############
# hadm_id 级别缺失率：该变量在该住院所有随访日均为 NA 的 hadm 占比
id_cols <- c("subject_id", "hadm_id", "chart_date", "times")
var_cols <- setdiff(names(stroke_longitudinal_filter_0528), id_cols)

table1 <- stroke_longitudinal_filter_0528 %>%
  group_by(hadm_id) %>%
  summarise(
    across(all_of(var_cols), ~ all(is.na(.x)), .names = "{.col}"),
    .groups = "drop"
  ) %>%
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
############################




##########筛选基线数据------##############
eligible_hadm <- stroke_patient_filtered_0528 %>%
  distinct(hadm_id, subject_id)

stroke_admission_filter_0530 <- stroke_admission_0520 %>%
  semi_join(eligible_hadm, by = c("subject_id", "hadm_id"))

stroke_apsiii_filter_0530 <- stroke_apsiii_0520 %>%
  semi_join(eligible_hadm, by = c("subject_id", "hadm_id"))

stroke_apsiii_filter2_0530 <- stroke_apsiii_filter_0530 %>%
  select(subject_id, hadm_id, stay_id, apsiii, apsiii_prob)

stroke_charlson_filter_0530 <- stroke_charlson_0520 %>%
  semi_join(eligible_hadm, by = c("subject_id", "hadm_id"))

stroke_charlson_filter2_0530 <- stroke_charlson_filter_0530 %>%
  select(subject_id, hadm_id, charlson_comorbidity_index)

stroke_icustays_filter_0530 <- stroke_icustays_0520 %>%
  semi_join(eligible_hadm, by = c("subject_id", "hadm_id"))

stroke_icustays_filter2_0530 <- stroke_icustays_filter_0530 %>%
  mutate(intime_parsed = as.POSIXct(intime, format = "%d/%m/%Y %H:%M:%S")) %>%
  group_by(hadm_id) %>%
  arrange(intime_parsed, stay_id, .by_group = TRUE) %>%
  slice(1) %>%
  ungroup() %>%
  select(subject_id, hadm_id, stay_id, intime, outtime, icu_los_days)
stroke_oasis_filter_0530 <- stroke_oasis_0520 %>%
  semi_join(eligible_hadm, by = c("subject_id", "hadm_id"))

stroke_oasis_filter2_0530 <- stroke_oasis_filter_0530 %>%
  select(subject_id, hadm_id, stay_id, oasis, age, preiculos, mechvent, electivesurgery)

stroke_sapsii_filter_0530 <- stroke_sapsii_0520 %>%
  semi_join(eligible_hadm, by = c("subject_id", "hadm_id"))

stroke_sapsii_filter2_0530 <- stroke_sapsii_filter_0530 %>%
  semi_join(
    stroke_admission_filter_0530 %>% distinct(hadm_id, subject_id),
    by = c("subject_id", "hadm_id")
  ) %>%
  inner_join(
    stroke_icustays_filter_0530 %>%
      mutate(intime_parsed = as.POSIXct(intime, format = "%d/%m/%Y %H:%M:%S")) %>%
      group_by(hadm_id) %>%
      arrange(intime_parsed, stay_id, .by_group = TRUE) %>%
      slice(1) %>%
      ungroup() %>%
      select(subject_id, hadm_id, stay_id),
    by = c("subject_id", "hadm_id", "stay_id")
  ) %>%
  select(subject_id, hadm_id, stay_id, sapsii)
stroke_stroketype_filter_0530 <- stroke_stroketype_0520 %>%
  semi_join(eligible_hadm, by = c("subject_id", "hadm_id"))

out_dir <- "F:/文章_大论文/0521/处理后文件/二次处理"
write.csv(stroke_admission_filter_0530,  file.path(out_dir, "stroke_admission_filter_0530.csv"),  row.names = FALSE)
write.csv(stroke_apsiii_filter_0530,     file.path(out_dir, "stroke_apsiii_filter_0530.csv"),     row.names = FALSE)
write.csv(stroke_apsiii_filter2_0530,    file.path(out_dir, "stroke_apsiii_filter2_0530.csv"),    row.names = FALSE)
write.csv(stroke_charlson_filter_0530,   file.path(out_dir, "stroke_charlson_filter_0530.csv"),   row.names = FALSE)
write.csv(stroke_charlson_filter2_0530,  file.path(out_dir, "stroke_charlson_filter2_0530.csv"),  row.names = FALSE)
write.csv(stroke_icustays_filter_0530,   file.path(out_dir, "stroke_icustays_filter_0530.csv"),   row.names = FALSE)
write.csv(stroke_icustays_filter2_0530,  file.path(out_dir, "stroke_icustays_filter2_0530.csv"),  row.names = FALSE)
write.csv(stroke_oasis_filter_0530,      file.path(out_dir, "stroke_oasis_filter_0530.csv"),      row.names = FALSE)
write.csv(stroke_oasis_filter2_0530,     file.path(out_dir, "stroke_oasis_filter2_0530.csv"),     row.names = FALSE)
write.csv(stroke_sapsii_filter_0530,     file.path(out_dir, "stroke_sapsii_filter_0530.csv"),     row.names = FALSE)
write.csv(stroke_sapsii_filter2_0530,    file.path(out_dir, "stroke_sapsii_filter2_0530.csv"),    row.names = FALSE)
write.csv(stroke_stroketype_filter_0530, file.path(out_dir, "stroke_stroketype_filter_0530.csv"), row.names = FALSE)
write.csv(stroke_longitudinal_filter_0528, file.path(out_dir, "stroke_longitudinal_filter_0528.csv"), row.names = FALSE)






#####################基线缺失率#####################
# 按行计算各测量指标缺失率（排除 ID / 键变量）
calc_missingrate_0530 <- function(df, id_cols) {
  var_cols <- setdiff(names(df), id_cols)
  n_total <- nrow(df)

  df %>%
    summarise(across(all_of(var_cols), ~ mean(is.na(.x)) * 100)) %>%
    pivot_longer(everything(), names_to = "variable", values_to = "missing_pct") %>%
    mutate(
      n_total = n_total,
      n_missing = round(n_total * missing_pct / 100),
      n_non_missing = n_total - n_missing,
      missing_pct = round(missing_pct, 2),
      coverage_pct = round(100 - missing_pct, 2)
    ) %>%
    arrange(desc(missing_pct))
}

admission_missingrate_0530 <- calc_missingrate_0530(
  stroke_admission_filter_0530,
  id_cols = c("subject_id", "hadm_id")
)

apsiii_missingrate_0530 <- calc_missingrate_0530(
  stroke_apsiii_filter_0530,
  id_cols = c("subject_id", "hadm_id", "stay_id")
)

charlson_missingrate_0530 <- calc_missingrate_0530(
  stroke_charlson_filter_0530,
  id_cols = c("subject_id", "hadm_id")
)

icustays_missingrate_0530 <- calc_missingrate_0530(
  stroke_icustays_filter_0530,
  id_cols = c("subject_id", "hadm_id", "stay_id")
)

oasis_missingrate_0530 <- calc_missingrate_0530(
  stroke_oasis_filter_0530,
  id_cols = c("subject_id", "hadm_id", "stay_id")
)

sapsii_missingrate_0530 <- calc_missingrate_0530(
  stroke_sapsii_filter_0530,
  id_cols = c("subject_id", "hadm_id", "stay_id")
)

stroketype_missingrate_0530 <- calc_missingrate_0530(
  stroke_stroketype_filter_0530,
  id_cols = c("subject_id", "hadm_id")
)
missingrate <- bind_rows(
  admission_missingrate_0530  %>% mutate(table = "admission",  .before = 1),
  apsiii_missingrate_0530     %>% mutate(table = "apsiii",     .before = 1),
  charlson_missingrate_0530   %>% mutate(table = "charlson",   .before = 1),
  icustays_missingrate_0530   %>% mutate(table = "icustays",   .before = 1),
  oasis_missingrate_0530      %>% mutate(table = "oasis",      .before = 1),
  sapsii_missingrate_0530     %>% mutate(table = "sapsii",     .before = 1),
  stroketype_missingrate_0530 %>% mutate(table = "stroketype", .before = 1)
) %>%
  arrange(table, desc(missing_pct))

#####################缺失率筛选（>40%剔除）#####################
missingrate_longitude <- table1 %>%
  filter(missing_pct_by_hadm <= 40)

missingrate_baseline <- missingrate %>%
  filter(missing_pct <= 40)


######################基线变量筛选#########################################

stroke_apsiii_filter2_0530 <- stroke_apsiii_filter_0530 %>%
  select(subject_id, hadm_id, stay_id, apsiii, apsiii_prob)
stroke_charlson_filter2_0530 <- stroke_charlson_filter_0530 %>%
  select(subject_id, hadm_id, charlson_comorbidity_index)
stroke_icustays_filter2_0530 <- stroke_icustays_filter_0530 %>%
  mutate(intime_parsed = as.POSIXct(intime, format = "%d/%m/%Y %H:%M:%S")) %>%
  group_by(hadm_id) %>%
  arrange(intime_parsed, stay_id, .by_group = TRUE) %>%
  slice(1) %>%
  ungroup() %>%
  select(subject_id, hadm_id, stay_id, intime, outtime, icu_los_days)
stroke_oasis_filter2_0530 <- stroke_oasis_filter_0530 %>%
  select(subject_id, hadm_id, stay_id, oasis, age, preiculos, mechvent, electivesurgery)

stroke_sapsii_filter2_0530 <- stroke_sapsii_filter_0530 %>%
  semi_join(
    stroke_admission_filter_0530 %>% distinct(hadm_id, subject_id),
    by = c("subject_id", "hadm_id")
  ) %>%
  inner_join(
    stroke_icustays_filter_0530 %>%
      mutate(intime_parsed = as.POSIXct(intime, format = "%d/%m/%Y %H:%M:%S")) %>%
      group_by(hadm_id) %>%
      arrange(intime_parsed, stay_id, .by_group = TRUE) %>%
      slice(1) %>%
      ungroup() %>%
      select(subject_id, hadm_id, stay_id),
    by = c("subject_id", "hadm_id", "stay_id")
  ) %>%
  select(subject_id, hadm_id, stay_id, sapsii)



stroke_baselinedata_0531 <- stroke_icustays_filter2_0530 %>%
  left_join(stroke_charlson_filter2_0530, by = c("subject_id", "hadm_id")) %>%
  left_join(stroke_apsiii_filter2_0530, by = c("subject_id", "hadm_id", "stay_id")) %>%
  left_join(stroke_oasis_filter2_0530, by = c("subject_id", "hadm_id", "stay_id")) %>%
  left_join(stroke_sapsii_filter2_0530, by = c("subject_id", "hadm_id", "stay_id"))



stroke_outcome_filter_0530 <- stroke_outcome_0520 %>%
  semi_join(
    stroke_baselinedata_0531 %>% distinct(hadm_id),
    by = "hadm_id"
  )
# 2. 按 hadm_id 合并 baseline 与 outcome
stroke_baselinedata_filter1_0531 <- stroke_baselinedata_0531 %>%
  inner_join(stroke_outcome_filter_0530, by = "hadm_id")


write.csv(
  stroke_baselinedata_filter1_0531,
  file.path("F:/文章_大论文/0521/处理后文件/stroke_baselinedata_0531.csv"),
  row.names = FALSE
)
write.csv(
  stroke_longitudinal_filter_0530,
  file.path("F:/文章_大论文/0521/处理后文件/stroke_longitudinal_filter_0530.csv"),
  row.names = FALSE
)









stroke_outcome_filter_0530 <- stroke_outcome_0520 %>%
  inner_join(
    stroke_baselinedata_0531 %>% select(subject_id, hadm_id, stay_id),
    by = c("subject_id", "hadm_id", "stay_id")
  )
stroke_baselinedata_filter1_0531 <- stroke_baselinedata_0531 %>%
  inner_join(stroke_outcome_filter_0530, by = c("subject_id", "hadm_id", "stay_id"))

write.csv2(stroke_baselinedata_filter1_0531,"F:/文章_大论文/0521/处理后文件/stroke_baselinedata_filter1_0531.csv")












