suppressPackageStartupMessages({
  library(dplyr)
  library(survival)
})
script_dir <- Sys.getenv("CH5_SCRIPT_DIR", unset = "work/chapter5_0831")
source(file.path(script_dir, "00_config.R"), encoding = "UTF-8")

base_raw <- read.csv(baseline_path, na.strings = c("", "NA"), fileEncoding = "UTF-8")
long_raw <- read.csv(longitudinal_path, na.strings = c("", "NA"), fileEncoding = "UTF-8")

base <- base_raw %>%
  mutate(
    hadm_id = as.integer(hadm_id),
    group = as.integer(group),
    death_28d = as.integer(death_28d),
    intime_dt = as.POSIXct(intime, format = "%d/%m/%Y %H:%M:%S"),
    deathtime_dt = as.POSIXct(deathtime, format = "%d/%m/%Y %H:%M:%S"),
    raw_event_time = as.numeric(difftime(deathtime_dt, intime_dt, units = "days")),
    invalid_event_time = death_28d == 1L & (!is.finite(raw_event_time) | raw_event_time <= 0 | raw_event_time > horizon),
    time28 = ifelse(death_28d == 1L, raw_event_time, horizon),
    status28 = as.integer(death_28d == 1L)
  ) %>%
  filter(group %in% c(1L, 2L)) %>%
  distinct(hadm_id, .keep_all = TRUE)

excluded <- base %>% filter(invalid_event_time)
base_valid <- base %>% filter(!invalid_event_time, is.finite(time28), time28 > 0)

for (nm in intersect(c(fixed11, clinical8), names(base_valid))) {
  if (nm %in% factor_vars) {
    base_valid[[nm]] <- factor(base_valid[[nm]])
  } else {
    base_valid[[nm]] <- as.numeric(base_valid[[nm]])
  }
}

long <- long_raw %>%
  mutate(hadm_id = as.integer(hadm_id), times = as.numeric(times)) %>%
  filter(hadm_id %in% base_valid$hadm_id) %>%
  mutate(across(all_of(long20), as.numeric)) %>%
  arrange(hadm_id, times)

day1 <- long %>%
  filter(times == min(times, na.rm = TRUE), .by = hadm_id) %>%
  slice(1L, .by = hadm_id) %>%
  select(hadm_id, all_of(long20))

static_data <- base_valid %>%
  select(hadm_id, group, time28, status28, all_of(fixed11)) %>%
  inner_join(day1, by = "hadm_id") %>%
  select(hadm_id, group, time28, status28, all_of(static31))

dynamic_base <- base_valid %>%
  filter(time28 > landmark) %>%
  select(hadm_id, group, time28, status28, all_of(clinical8)) %>%
  inner_join(day1 %>% select(hadm_id, all_of(day1_17)), by = "hadm_id") %>%
  mutate(
    lm_time = time28,
    lm_status = status28
  ) %>%
  select(hadm_id, group, lm_time, lm_status, all_of(dynamic_fixed25))

dynamic_long <- long %>%
  filter(hadm_id %in% dynamic_base$hadm_id, times <= landmark) %>%
  select(hadm_id, time = times, all_of(traj3)) %>%
  filter(if_all(all_of(traj3), is.finite))

complete_traj_ids <- dynamic_long %>%
  summarise(across(all_of(traj3), ~ sum(is.finite(.x))), .by = hadm_id) %>%
  filter(if_all(all_of(traj3), ~ .x >= 2L)) %>%
  pull(hadm_id)
dynamic_base <- dynamic_base %>% filter(hadm_id %in% complete_traj_ids)
dynamic_long <- dynamic_long %>% filter(hadm_id %in% complete_traj_ids)

audit <- bind_rows(
  data.frame(item = "raw_baseline_admissions", value = nrow(base)),
  data.frame(item = "death_28d", value = sum(base$death_28d == 1L, na.rm = TRUE)),
  data.frame(item = "excluded_invalid_event_time", value = nrow(excluded)),
  data.frame(item = "static_train_n", value = sum(static_data$group == 1L)),
  data.frame(item = "static_train_events", value = sum(static_data$group == 1L & static_data$status28 == 1L)),
  data.frame(item = "static_valid_n", value = sum(static_data$group == 2L)),
  data.frame(item = "static_valid_events", value = sum(static_data$group == 2L & static_data$status28 == 1L)),
  data.frame(item = "landmark5_train_n", value = sum(dynamic_base$group == 1L)),
  data.frame(item = "landmark5_train_events", value = sum(dynamic_base$group == 1L & dynamic_base$lm_status == 1L)),
  data.frame(item = "landmark5_valid_n", value = sum(dynamic_base$group == 2L)),
  data.frame(item = "landmark5_valid_events", value = sum(dynamic_base$group == 2L & dynamic_base$lm_status == 1L))
)

saveRDS(static_data, file.path(out_dir, "models", "static_data.rds"))
saveRDS(dynamic_base, file.path(out_dir, "models", "dynamic_base.rds"))
saveRDS(dynamic_long, file.path(out_dir, "models", "dynamic_long.rds"))
saveRDS(excluded, file.path(out_dir, "models", "excluded_invalid_event_time.rds"))
write_utf8(audit, file.path(out_dir, "tables", "00_cohort_audit.csv"))
write_utf8(excluded %>% select(hadm_id, group, death_28d, raw_event_time),
           file.path(out_dir, "tables", "00_excluded_invalid_event_time.csv"))
cat("PREPARE_OK\n")
