suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(patchwork)
})
script_dir <- Sys.getenv("CH5_SCRIPT_DIR", unset = "work/chapter5_0831")
source(file.path(script_dir, "00_config.R"), encoding = "UTF-8")

rsflc <- read.csv(file.path(out_dir, "tables", "rsflc_survival_metrics.csv"), check.names = FALSE)
jm <- read.csv(file.path(out_dir, "tables", "JM_dynamic_metrics.csv"), check.names = FALSE) %>%
  filter(split == "validation")
jm_train <- read.csv(file.path(out_dir, "tables", "JM_dynamic_metrics.csv"), check.names = FALSE) %>%
  filter(split == "train")

table_5_4B <- bind_rows(
  data.frame(model = "JM", setting = "3 trajectories + 25 fixed covariates; 3 chains, 1500 iterations, burn-in 500",
             train_C_index = jm_train$C_index, valid_C_index = jm$C_index,
             AUC_28 = jm$AUC_28, Brier_28 = jm$Brier_28, IBS_5_28 = jm$IBS_5_28),
  data.frame(model = "RSFLC", setting = rsflc$setting,
             train_C_index = rsflc$train_C_index, valid_C_index = rsflc$valid_C_index,
             AUC_28 = rsflc$AUC_28, Brier_28 = rsflc$Brier_28, IBS_5_28 = rsflc$IBS_5_28)
)
write_utf8(table_5_4B, file.path(out_dir, "tables", "table_5_4B_dynamic_metrics.csv"))

static_curve <- read.csv(file.path(out_dir, "tables", "static_time_metrics.csv"))
jm_curve <- read.csv(file.path(out_dir, "tables", "JM_time_metrics.csv"))
rsflc_curve <- read.csv(file.path(out_dir, "tables", "rsflc_time_metrics.csv")) %>% mutate(model = "RSFLC")
dynamic_curve <- bind_rows(jm_curve, rsflc_curve)
write_utf8(dynamic_curve, file.path(out_dir, "tables", "dynamic_time_metrics.csv"))

p_static <- static_curve %>% pivot_longer(c(AUC, Brier), names_to = "metric", values_to = "value") %>%
  ggplot(aes(time, value, colour = model)) + geom_line(linewidth = 0.8) +
  facet_wrap(~metric, scales = "free_y") + theme_bw() +
  labs(x = "Day after admission", y = NULL, title = "Static task")
p_dynamic <- dynamic_curve %>% pivot_longer(c(AUC, Brier), names_to = "metric", values_to = "value") %>%
  ggplot(aes(time, value, colour = model)) + geom_line(linewidth = 0.8) +
  facet_wrap(~metric, scales = "free_y") + theme_bw() +
  labs(x = "Day after admission", y = NULL, title = "Landmark task (day 5)")
ggsave(file.path(out_dir, "figures", "ALL_01_time_dependent_metrics.png"),
       p_static / p_dynamic, width = 9, height = 8.5, dpi = 300)

all_perf <- bind_rows(
  read.csv(file.path(out_dir, "tables", "table_5_4A_static_metrics.csv")) %>%
    transmute(task = "Static", model, C_index = valid_C_index, AUC = AUC_28,
              Brier = Brier_28, IBS = IBS_0_28),
  table_5_4B %>% transmute(task = "Landmark day 5", model, C_index = valid_C_index,
                           AUC = AUC_28, Brier = Brier_28, IBS = IBS_5_28)
)
write_utf8(all_perf, file.path(out_dir, "tables", "all_model_validation_performance.csv"))

perf_long <- all_perf %>% pivot_longer(c(C_index, AUC, Brier, IBS), names_to = "metric", values_to = "value")
p_perf <- ggplot(perf_long, aes(model, value, fill = model)) + geom_col(width = 0.65) +
  facet_grid(metric ~ task, scales = "free_y") + theme_bw() +
  theme(legend.position = "none", axis.text.x = element_text(angle = 25, hjust = 1)) +
  labs(x = NULL, y = NULL, title = "Internal validation performance by task")
ggsave(file.path(out_dir, "figures", "ALL_02_validation_performance.png"), p_perf,
       width = 8.5, height = 8, dpi = 300)
cat("COMBINE_OK\n")
