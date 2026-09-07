## 生存时间描述统计：0319PRO_inputmergedata.xlsx
## 与 run_4models_global_strict.R 保持一致的处理逻辑
suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
})

work_dir <- "F:/文章_大论文/MIMIC数据库_代码/TREA代码/0319全局建模"
in_path <- file.path(work_dir, "0319PRO_inputmergedata.xlsx")
if (!file.exists(in_path)) stop("文件不存在: ", in_path)

to_num <- function(x) {
  if (is.numeric(x)) return(as.numeric(x))
  if (is.logical(x)) return(as.numeric(x))
  xc <- trimws(as.character(x))
  xc[xc == ""] <- NA_character_
  suppressWarnings(as.numeric(xc))
}

df <- readxl::read_xlsx(in_path) %>%
  mutate(
    subject_id = as.character(subject_id),
    event = ifelse(!is.na(deathtime) & trimws(as.character(deathtime)) != "", 1, 0),
    obs_time = to_num(los_hosp_days),
    obs_time = ifelse(!is.finite(obs_time) | is.na(obs_time) | obs_time <= 0, to_num(Obstimes) + 1, obs_time),
    obs_time = ifelse(!is.finite(obs_time) | is.na(obs_time) | obs_time <= 0, 1, obs_time),
    t = to_num(Obstimes)
  ) %>%
  group_by(subject_id) %>%
  mutate(t = t - min(t, na.rm = TRUE)) %>%
  ungroup()

## 每个 subject 一条 survival 记录（与建模一致）
subj_outcome <- df %>%
  group_by(subject_id) %>%
  summarise(
    event_subj = as.numeric(max(event, na.rm = TRUE)),
    obs_from_los = suppressWarnings(median(obs_time[is.finite(obs_time) & obs_time > 0], na.rm = TRUE)),
    max_t = suppressWarnings(max(t, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  mutate(
    obs_from_los = ifelse(is.finite(obs_from_los) & !is.na(obs_from_los) & obs_from_los > 0, obs_from_los, NA_real_),
    max_t = ifelse(is.finite(max_t) & !is.na(max_t), max_t, 0),
    obs_time_subj = ifelse(is.na(obs_from_los), max_t + 1, obs_from_los),
    obs_time_subj = ifelse(obs_time_subj <= 0 | !is.finite(obs_time_subj), 1, obs_time_subj),
    event_subj = ifelse(event_subj > 0, 1, 0)
  )

t <- subj_outcome$obs_time_subj
n <- length(t)
m <- mean(t)
s <- sd(t)
min_t <- min(t)
max_t <- max(t)

cat("\n========== 0319PRO_inputmergedata 生存时间描述统计 ==========\n")
cat("数据来源: los_hosp_days（住院天数），与 run_4models_global_strict 一致\n")
cat("样本量: ", n, "\n", sep = "")
cat("均数 ± 标准差: ", round(m, 4), " ± ", round(s, 4), " 天\n", sep = "")
cat("最小值: ", round(min_t, 4), " 天\n", sep = "")
cat("最大值: ", round(max_t, 4), " 天\n", sep = "")
cat("中位数: ", round(median(t), 4), " 天\n", sep = "")
cat("==============================================================\n\n")
