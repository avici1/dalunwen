suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(grid)
  library(data.table)
})
script_dir <- Sys.getenv("CH5_SCRIPT_DIR", unset = getwd())
source(file.path(script_dir, "00_config.R"), encoding = "UTF-8")
log_progress("DESCRIPTIVE_START", "tables 5-1a, 5-1b, 5-2 and extended flowchart")

base <- readRDS(file.path(cache_dir, "baseline_all.rds"))
static_data <- readRDS(file.path(cache_dir, "static_data.rds"))
dynamic_base <- readRDS(file.path(cache_dir, "dynamic_base.rds"))
long_raw <- as.data.frame(readRDS(file.path(cache_dir, "longitudinal_all.rds")))
audit <- read.csv(file.path(artifact_dir, "00_cohort_audit.csv"), check.names = FALSE)
exclusions <- read.csv(file.path(artifact_dir, "00_exclusion_log.csv"), check.names = FALSE)

analysis_base <- base %>% filter(!invalid_event_time)
analysis_base <- analysis_base %>% mutate(
  hospital_los_days = as.numeric(difftime(
    as.POSIXct(dischtime, format = "%d/%m/%Y %H:%M:%S", tz = "UTC"),
    as.POSIXct(admittime, format = "%d/%m/%Y %H:%M:%S", tz = "UTC"), units = "days"
  ))
)

continuous_vars <- c(
  age = "\u5e74\u9f84\uff08\u5c81\uff09",
  bmi = "\u4f53\u91cd\u6307\u6570\uff08BMI\uff09",
  charlson_comorbidity_index = "Charlson\u5408\u5e76\u75c7\u6307\u6570",
  preiculos = "ICU\u524d\u4f4f\u9662\u65f6\u957f\uff08h\uff09",
  icu_los_days = "ICU\u4f4f\u9662\u5929\u6570",
  hospital_los_days = "\u603b\u4f4f\u9662\u5929\u6570",
  apsiii = "APSIII", oasis = "OASIS", sapsii = "SAPSII"
)

table_5_1a <- bind_rows(lapply(names(continuous_vars), function(nm) {
  x <- as.numeric(analysis_base[[nm]])
  data.frame(
    variable = unname(continuous_vars[[nm]]),
    n = sum(is.finite(x)), mean = mean(x, na.rm = TRUE), sd = sd(x, na.rm = TRUE),
    mean_sd = format_mean_sd(x), median = median(x, na.rm = TRUE),
    q1 = quantile(x, 0.25, na.rm = TRUE), q3 = quantile(x, 0.75, na.rm = TRUE)
  )
}))
write_utf8(table_5_1a, file.path(artifact_dir, "\u88685-1a_\u7814\u7a76\u961f\u5217\u57fa\u7ebf\u8fde\u7eed\u53d8\u91cf\u7279\u5f81.csv"))

categorical_rows <- list()
add_cat <- function(section, variable, values, labels = NULL) {
  values <- as.character(values)
  tab <- table(values, useNA = "ifany")
  lev <- names(tab)
  lab <- if (is.null(labels)) lev else unname(labels[lev])
  lab[is.na(lab)] <- lev[is.na(lab)]
  data.frame(section = section, variable = variable, level = lab,
             n = as.integer(tab), percent = 100 * as.integer(tab) / length(values),
             n_percent = vapply(as.integer(tab), format_n_pct, character(1), total = length(values)))
}

categorical_rows[[1]] <- add_cat("\u4eba\u53e3\u5b66", "\u6027\u522b", analysis_base$gender,
                                  c(M = "\u7537", F = "\u5973"))
categorical_rows[[2]] <- add_cat("\u4e34\u5e8a\u7279\u5f81", "\u673a\u68b0\u901a\u6c14", analysis_base$mechvent,
                                  c(`0` = "\u5426", `1` = "\u662f"))
categorical_rows[[3]] <- add_cat("\u4e34\u5e8a\u7279\u5f81", "\u62e9\u671f\u624b\u672f", analysis_base$electivesurgery,
                                  c(`0` = "\u5426", `1` = "\u662f"))
categorical_rows[[4]] <- add_cat("\u5352\u4e2d\u5206\u578b", "\u5352\u4e2d\u5206\u578b", analysis_base$stroke_type)
categorical_rows[[5]] <- add_cat("\u7ed3\u5c40", "28\u65e5\u5168\u56e0\u6b7b\u4ea1", analysis_base$status28,
                                  c(`0` = "\u5426", `1` = "\u662f"))
table_5_1b <- bind_rows(categorical_rows)
write_utf8(table_5_1b, file.path(artifact_dir, "\u88685-1b_\u7814\u7a76\u961f\u5217\u57fa\u7ebf\u5206\u7c7b\u53d8\u91cf\u7279\u5f81.csv"))

long_model <- long_raw %>%
  filter(hadm_id %in% static_data$hadm_id) %>%
  mutate(across(all_of(long20), as.numeric))
worst_min <- c("gcs", "ph", "po2", "pao2fio2ratio", "hemoglobin", "total_urine_output")

long_summary <- bind_rows(lapply(long20, function(nm) {
  d <- long_model %>% select(hadm_id, times, value = all_of(nm))
  by_id <- d %>% summarise(
    day1 = value[times == 1][1],
    admission_mean = mean(value, na.rm = TRUE),
    worst = if (nm %in% worst_min) min(value, na.rm = TRUE) else max(value, na.rm = TRUE),
    .by = hadm_id
  )
  data.frame(
    variable_code = nm, variable = unname(cn_labels[[nm]]),
    admissions = nrow(by_id),
    day1_mean_sd = format_mean_sd(by_id$day1),
    admission_mean_sd = format_mean_sd(by_id$admission_mean),
    worst_mean_sd = format_mean_sd(by_id$worst)
  )
}))
write_utf8(long_summary, file.path(artifact_dir, "\u88685-2_\u7814\u7a76\u961f\u5217\u7eb5\u5411\u6570\u636e\u57fa\u672c\u7279\u5f81.csv"))

# Extended Figure 5-1. The upper screening counts are preserved from the supplied article.
get_n <- function(item, field = "n") audit[audit$item == item, field][[1]]
excl_n <- function(stage, reason) sum(exclusions$stage == stage & exclusions$reason == reason)

boxes <- data.frame(
  id = c("mimic", "firsticu", "final6299", "align", "filter", "impute", "outcome", "eligible", "static", "dynamic"),
  xmin = c(4.1,4.1,4.1,0.4,3.7,7.0,3.2,3.2,0.6,5.6),
  xmax = c(9.5,9.5,9.5,3.0,6.3,9.6,7.0,7.0,4.4,9.4),
  ymin = c(27.0,22.5,18.0,14.5,14.5,14.5,11.3,8.3,3.6,3.6),
  ymax = c(29.0,24.5,20.0,16.5,16.5,16.5,13.3,10.3,6.8,6.8),
  label = c(
    "MIMIC-IV \u8111\u5352\u4e2d\u76f8\u5173\u4f4f\u9662\n12,880\u6b21\u4f4f\u9662 / 11,691\u540d\u60a3\u8005",
    "\u5b58\u5728\u9996\u6b21ICU\u5165\u4f4f\u8bb0\u5f55\n7,212\u6b21\u4f4f\u9662 / 6,933\u540d\u60a3\u8005",
    "\u56fe5-1\u539f\u59cb\u7b5b\u9009\u961f\u5217\n6,299\u6b21\u4f4f\u9662 / 6,092\u540d\u60a3\u8005",
    "\u57fa\u7ebf/\u7eb5\u5411\u8868\n\u6309hadm_id\u5bf9\u9f50",
    "\u7f3a\u5931\u7387>40%\n\u5220\u9664\u53d8\u91cf",
    "missForest/KNN\n\u7eb5\u5411\u7f3a\u5931\u503c\u63d2\u8865",
    sprintf("28\u65e5\u751f\u5b58\u7ed3\u5c40\u65f6\u95f4\u5ba1\u8ba1\n\u6392\u9664337\u4f8b\uff1a\u7f3a\u65f6\u523b334\uff0c\u975e\u6b631\uff0c>28\u65e52"),
    sprintf("\u53ef\u7528\u4e8e\u7edf\u4e00\u751f\u5b58\u5206\u6790\n%d\u6b21\u4f4f\u9662 / %d\u540d\u60a3\u8005", get_n("valid_survival_admissions"), audit$subjects[audit$item == "valid_survival_admissions"]),
    sprintf("\u9759\u6001\u4efb\u52a1\uff08COX/RSF\uff09\n\u6392\u9664\u65e0\u7b2c1\u65e5\u7eb5\u5411\u8bb0\u5f55 %d\u4f8b\nN=%d\uff08\u8bad\u7ec3%d / \u9a8c\u8bc1%d\uff09", excl_n("static_day1", "no_day1_longitudinal_record"), nrow(static_data), sum(static_data$group == 1), sum(static_data$group == 2)),
    sprintf("\u52a8\u6001landmark\u4efb\u52a1\uff08JM/RSFLC\uff09\n\u6392\u9664\u7b2c5\u65e5\u524d/\u5f53\u65e5\u7ed3\u5c40 %d\u4f8b\n\u65e0\u7b2c1\u65e5\u8bb0\u5f55 %d\u4f8b\uff1b\u8f68\u8ff9<2\u6b21 %d\u4f8b\nN=%d\uff08\u8bad\u7ec3%d / \u9a8c\u8bc1%d\uff09",
            excl_n("landmark", "death_or_end_before_or_at_day5"),
            excl_n("landmark", "no_day1_longitudinal_record"),
            excl_n("landmark", "fewer_than_two_observations_in_any_trajectory"),
            nrow(dynamic_base), sum(dynamic_base$group == 1), sum(dynamic_base$group == 2))
  )
)

exclusion_boxes <- data.frame(
  xmin = c(0.2, 0.2), xmax = c(3.7, 3.7), ymin = c(23.1, 18.3), ymax = c(26.1, 21.0),
  label = c(
    "\u6392\u9664\uff1a\n\u5e74\u9f84<18\u5c81/\u7f3a\u5931\uff1a0\n\u65e0ICU\u8bb0\u5f55\uff1a5,668\u6b21\u4f4f\u9662\n\uff084,758\u540d\u60a3\u8005\uff09",
    "\u6392\u9664\uff1a\u9996\u6b21ICU\u4f4f\u9662\u65f6\u95f4\u226424 h\n913\u6b21\u4f4f\u9662 / 841\u540d\u60a3\u8005"
  )
)

p <- ggplot() +
  geom_rect(data = boxes, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            fill = "white", colour = "#1F1F1F", linewidth = 0.65) +
  geom_text(data = boxes, aes(x = (xmin + xmax) / 2, y = (ymin + ymax) / 2, label = label),
            family = "Microsoft YaHei", size = 3.35, lineheight = 1.2) +
  geom_rect(data = exclusion_boxes, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            fill = "#F0F0F0", colour = "#555555", linewidth = 0.5) +
  geom_text(data = exclusion_boxes, aes(x = xmin + 0.16, y = (ymin + ymax) / 2, label = label),
            hjust = 0, family = "Microsoft YaHei", size = 3.0, lineheight = 1.15) +
  annotate("segment", x = 6.8, xend = 6.8, y = 27.0, yend = 24.6, arrow = arrow(length = unit(0.16, "cm"))) +
  annotate("segment", x = 6.8, xend = 6.8, y = 22.5, yend = 20.1, arrow = arrow(length = unit(0.16, "cm"))) +
  annotate("segment", x = 6.8, xend = 6.8, y = 18.0, yend = 16.7, arrow = arrow(length = unit(0.16, "cm"))) +
  annotate("segment", x = 3.0, xend = 3.65, y = 15.5, yend = 15.5, arrow = arrow(length = unit(0.14, "cm"))) +
  annotate("segment", x = 6.3, xend = 6.95, y = 15.5, yend = 15.5, arrow = arrow(length = unit(0.14, "cm"))) +
  annotate("segment", x = 6.8, xend = 5.1, y = 14.5, yend = 13.4, arrow = arrow(length = unit(0.16, "cm"))) +
  annotate("segment", x = 5.1, xend = 5.1, y = 11.3, yend = 10.4, arrow = arrow(length = unit(0.16, "cm"))) +
  annotate("segment", x = 5.1, xend = 2.5, y = 8.3, yend = 6.9, arrow = arrow(length = unit(0.16, "cm"))) +
  annotate("segment", x = 5.1, xend = 7.5, y = 8.3, yend = 6.9, arrow = arrow(length = unit(0.16, "cm"))) +
  annotate("segment", x = 6.8, xend = 3.75, y = 25.3, yend = 25.3, arrow = arrow(length = unit(0.14, "cm"))) +
  annotate("segment", x = 6.8, xend = 3.75, y = 20.0, yend = 20.0, arrow = arrow(length = unit(0.14, "cm"))) +
  annotate("text", x = 5.0, y = 30.0, label = "\u56fe5-1  \u7814\u7a76\u961f\u5217\u7b5b\u9009\u4e0e\u5206\u6790\u6570\u636e\u6784\u5efa\u6d41\u7a0b",
           family = "Microsoft YaHei", fontface = "bold", size = 5.0) +
  coord_cartesian(xlim = c(0, 10), ylim = c(3.0, 30.6), clip = "off") +
  theme_void() + theme(plot.margin = margin(10, 10, 10, 10))

ggsave(file.path(artifact_dir, "\u56fe5-1_\u7814\u7a76\u961f\u5217\u7b5b\u9009\u4e0e\u5206\u6790\u6570\u636e\u6784\u5efa\u6d41\u7a0b.png"),
       p, width = 9.0, height = 13.5, dpi = 320, bg = "white")

log_progress("DESCRIPTIVE_OK", sprintf("baseline_n=%d; static_n=%d; landmark_n=%d", nrow(analysis_base), nrow(static_data), nrow(dynamic_base)))
cat("DESCRIPTIVE_OK\n")
