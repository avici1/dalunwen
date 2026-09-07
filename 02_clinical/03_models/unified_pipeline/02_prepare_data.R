suppressPackageStartupMessages({
  library(data.table)
  library(dplyr)
})
script_dir <- Sys.getenv("CH5_SCRIPT_DIR", unset = getwd())
source(file.path(script_dir, "00_config.R"), encoding = "UTF-8")
log_progress("PREPARE_START", "read two locked source files")

if (!file.exists(baseline_path) || !file.exists(longitudinal_path)) {
  stop("Staged input files are missing. Run 01_stage_inputs.R first.")
}

base_raw <- data.table::fread(baseline_path, na.strings = c("", "NA"), encoding = "UTF-8")
long_raw <- data.table::fread(longitudinal_path, na.strings = c("", "NA"), encoding = "UTF-8")

required_base <- c("subject_id", "hadm_id", "intime", "deathtime", "death_28d", "group", "fold", fixed11)
required_long <- c("subject_id", "hadm_id", "times", "group", "fold", long20)
if (length(setdiff(required_base, names(base_raw)))) stop("Missing baseline columns: ", paste(setdiff(required_base, names(base_raw)), collapse = ", "))
if (length(setdiff(required_long, names(long_raw)))) stop("Missing longitudinal columns: ", paste(setdiff(required_long, names(long_raw)), collapse = ", "))

base <- as.data.frame(base_raw) %>%
  mutate(
    subject_id = as.integer(subject_id), hadm_id = as.integer(hadm_id),
    group = as.integer(group), fold = as.integer(fold), death_28d = as.integer(death_28d),
    intime_dt = as.POSIXct(intime, format = "%d/%m/%Y %H:%M:%S", tz = "UTC"),
    deathtime_dt = as.POSIXct(deathtime, format = "%d/%m/%Y %H:%M:%S", tz = "UTC"),
    event_time_days = as.numeric(difftime(deathtime_dt, intime_dt, units = "days")),
    exclusion_reason = case_when(
      death_28d != 1L ~ NA_character_,
      !is.finite(event_time_days) ~ "missing_exact_death_time",
      event_time_days <= 0 ~ "nonpositive_death_time",
      event_time_days > horizon_day ~ "death_time_beyond_28_days",
      TRUE ~ NA_character_
    ),
    invalid_event_time = !is.na(exclusion_reason),
    time_days = ifelse(death_28d == 1L, event_time_days, horizon_day),
    time_u = time_days / time_scale_days,
    status28 = as.integer(death_28d == 1L)
  ) %>%
  filter(group %in% c(1L, 2L)) %>%
  distinct(hadm_id, .keep_all = TRUE)

invalid_event <- base %>% filter(invalid_event_time)
base_valid <- base %>% filter(!invalid_event_time, is.finite(time_u), time_u > 0, time_u <= 1)

long <- as.data.frame(long_raw) %>%
  mutate(
    subject_id = as.integer(subject_id), hadm_id = as.integer(hadm_id),
    group = as.integer(group), fold = as.integer(fold),
    times = as.numeric(times), time_u = times / time_scale_days
  ) %>%
  filter(hadm_id %in% base_valid$hadm_id) %>%
  mutate(across(all_of(long20), as.numeric)) %>%
  arrange(hadm_id, times)

if (anyDuplicated(long[c("hadm_id", "times")])) stop("Duplicated hadm_id-times rows found")

day1 <- long %>%
  filter(times == 1) %>%
  select(hadm_id, all_of(long20))

static_data <- base_valid %>%
  select(subject_id, hadm_id, group, fold, time_days, time_u, status28, all_of(fixed11)) %>%
  inner_join(day1, by = "hadm_id") %>%
  select(subject_id, hadm_id, group, fold, time_days, time_u, status28, all_of(static31))

dynamic_candidates <- base_valid %>%
  filter(time_days > landmark_day) %>%
  select(subject_id, hadm_id, group, fold, time_days, time_u, status28, all_of(fixed11)) %>%
  inner_join(day1 %>% select(hadm_id, all_of(day1_17)), by = "hadm_id") %>%
  mutate(lm_time_u = time_u, lm_status = status28) %>%
  select(subject_id, hadm_id, group, fold, lm_time_u, lm_status, all_of(dynamic_fixed28))

dynamic_long0 <- long %>%
  filter(hadm_id %in% dynamic_candidates$hadm_id, times <= landmark_day) %>%
  select(subject_id, hadm_id, day = times, time_u, all_of(traj3))

trajectory_counts <- dynamic_long0 %>%
  summarise(across(all_of(traj3), ~ sum(is.finite(.x))), .by = c(subject_id, hadm_id))
complete_traj_ids <- trajectory_counts %>%
  filter(if_all(all_of(traj3), ~ .x >= 2L)) %>%
  pull(hadm_id)

dynamic_base <- dynamic_candidates %>% filter(hadm_id %in% complete_traj_ids)
dynamic_long <- dynamic_long0 %>% filter(hadm_id %in% complete_traj_ids)

for (nm in intersect(factor_vars, names(static_data))) static_data[[nm]] <- factor(static_data[[nm]])
for (nm in intersect(factor_vars, names(dynamic_base))) dynamic_base[[nm]] <- factor(dynamic_base[[nm]])

exclusion_log <- bind_rows(
  invalid_event %>% transmute(subject_id, hadm_id, stage = "survival_outcome", reason = exclusion_reason),
  base_valid %>% filter(!hadm_id %in% day1$hadm_id) %>%
    transmute(subject_id, hadm_id, stage = "static_day1", reason = "no_day1_longitudinal_record"),
  base_valid %>% filter(time_days <= landmark_day) %>%
    transmute(subject_id, hadm_id, stage = "landmark", reason = "death_or_end_before_or_at_day5"),
  base_valid %>% filter(time_days > landmark_day, !hadm_id %in% day1$hadm_id) %>%
    transmute(subject_id, hadm_id, stage = "landmark", reason = "no_day1_longitudinal_record"),
  dynamic_candidates %>% filter(!hadm_id %in% complete_traj_ids) %>%
    transmute(subject_id, hadm_id, stage = "landmark", reason = "fewer_than_two_observations_in_any_trajectory")
) %>% arrange(stage, reason, hadm_id)

audit <- bind_rows(
  data.frame(item = "raw_baseline_admissions", n = nrow(base), events = sum(base$death_28d == 1L), subjects = n_distinct(base$subject_id)),
  data.frame(item = "excluded_invalid_event_time", n = nrow(invalid_event), events = nrow(invalid_event), subjects = n_distinct(invalid_event$subject_id)),
  data.frame(item = "valid_survival_admissions", n = nrow(base_valid), events = sum(base_valid$status28), subjects = n_distinct(base_valid$subject_id)),
  data.frame(item = "raw_longitudinal_records", n = nrow(long_raw), events = NA_integer_, subjects = n_distinct(long_raw$subject_id)),
  data.frame(item = "raw_longitudinal_admissions", n = n_distinct(long_raw$hadm_id), events = NA_integer_, subjects = n_distinct(long_raw$subject_id)),
  data.frame(item = "static_train", n = sum(static_data$group == 1L), events = sum(static_data$group == 1L & static_data$status28 == 1L), subjects = n_distinct(static_data$subject_id[static_data$group == 1L])),
  data.frame(item = "static_validation", n = sum(static_data$group == 2L), events = sum(static_data$group == 2L & static_data$status28 == 1L), subjects = n_distinct(static_data$subject_id[static_data$group == 2L])),
  data.frame(item = "landmark_train", n = sum(dynamic_base$group == 1L), events = sum(dynamic_base$group == 1L & dynamic_base$lm_status == 1L), subjects = n_distinct(dynamic_base$subject_id[dynamic_base$group == 1L])),
  data.frame(item = "landmark_validation", n = sum(dynamic_base$group == 2L), events = sum(dynamic_base$group == 2L & dynamic_base$lm_status == 1L), subjects = n_distinct(dynamic_base$subject_id[dynamic_base$group == 2L]))
)

split_leakage <- base %>% distinct(subject_id, group) %>% count(subject_id) %>% filter(n > 1L)
fold_leakage <- base %>% filter(group == 1L) %>% distinct(subject_id, fold) %>% count(subject_id) %>% filter(n > 1L)
qc <- data.frame(
  check = c("static_predictor_count", "dynamic_fixed_count", "trajectory_count", "subjects_spanning_train_validation", "training_subjects_spanning_folds"),
  value = c(length(static31), length(dynamic_fixed28), length(traj3), nrow(split_leakage), nrow(fold_leakage)),
  expected = c(31, 28, 3, 0, 0),
  pass = c(length(static31) == 31L, length(dynamic_fixed28) == 28L, length(traj3) == 3L, nrow(split_leakage) == 0L, nrow(fold_leakage) == 0L)
)
if (!all(qc$pass)) stop("Locked-design QC failed")

saveRDS(static_data, file.path(cache_dir, "static_data.rds"))
saveRDS(dynamic_base, file.path(cache_dir, "dynamic_base.rds"))
saveRDS(dynamic_long, file.path(cache_dir, "dynamic_long.rds"))
saveRDS(base, file.path(cache_dir, "baseline_all.rds"))
saveRDS(long_raw, file.path(cache_dir, "longitudinal_all.rds"))
write_utf8(audit, file.path(artifact_dir, "00_cohort_audit.csv"))
write_utf8(exclusion_log, file.path(artifact_dir, "00_exclusion_log.csv"))
write_utf8(qc, file.path(artifact_dir, "00_locked_design_qc.csv"))

log_progress(
  "PREPARE_OK",
  sprintf("invalid_event=%d; static=%d; landmark=%d", nrow(invalid_event), nrow(static_data), nrow(dynamic_base))
)
cat("PREPARE_OK\n")
