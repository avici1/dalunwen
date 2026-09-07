library(tidyverse)
library(ggplot2)
suppressPackageStartupMessages({
  library(readxl)
  library(writexl)
})

plot_dir <- "F:/文章_大论文/MIMIC数据库_代码/TREA代码/0331作图保存"
cmp_dir <- "F:/文章_大论文/MIMIC数据库_代码/TREA代码/0323实例比较"

cv_file_tbl <- tribble(
  ~model, ~basename,
  "RSF", "0323_RSF_5fold_results.xlsx",
  "RSF_LC", "0323_RSFLC_5fold_results.xlsx",
  "COX", "0323_COX_5fold_results.xlsx",
  "JM", "0323_JM_5fold_results.xlsx"
)

resolve_cv_path <- function(bn) {
  p1 <- file.path(plot_dir, bn)
  p2 <- file.path(cmp_dir, bn)
  if (file.exists(p1)) {
    return(p1)
  }
  if (file.exists(p2)) {
    return(p2)
  }
  stop("未找到文件：", bn, "（已查：", plot_dir, " 与 ", cmp_dir, "）")
}

read_cv_detail <- function(path, model_lab) {
  d <- readxl::read_xlsx(path, sheet = "cv_detail") %>%
    mutate(fold = suppressWarnings(as.integer(fold))) %>%
    filter(is.finite(fold), fold >= 1L, fold <= 5L)
  miss <- setdiff(
    c("train_CINDEX", "test_CINDEX", "train_AUC", "test_AUC", "train_BS", "test_BS"),
    names(d)
  )
  if (length(miss)) {
    stop(path, " 缺少列：", paste(miss, collapse = ", "))
  }
  d %>% mutate(model = model_lab, .before = 1)
}

cv_paths <- cv_file_tbl %>% mutate(path = vapply(basename, resolve_cv_path, character(1)))
cv_all <- pmap_dfr(
  list(cv_paths$path, cv_paths$model),
  function(p, m) read_cv_detail(p, m)
)

metric_cols <- tribble(
  ~metric, ~train_col, ~test_col,
  "C-index", "train_CINDEX", "test_CINDEX",
  "AUC", "train_AUC", "test_AUC",
  "BS", "train_BS", "test_BS"
)

summ_one_metric <- function(metric_name, trc, tec) {
  cv_all %>%
    group_by(model) %>%
    summarise(
      metric = metric_name,
      train = mean(.data[[trc]], na.rm = TRUE),
      test = mean(.data[[tec]], na.rm = TRUE),
      train_sd = stats::sd(.data[[trc]], na.rm = TRUE),
      test_sd = stats::sd(.data[[tec]], na.rm = TRUE),
      diff = mean(.data[[trc]] - .data[[tec]], na.rm = TRUE),
      diff_sd = stats::sd(.data[[trc]] - .data[[tec]], na.rm = TRUE),
      .groups = "drop"
    )
}

dat <- metric_cols %>%
  pmap_dfr(function(metric, train_col, test_col) {
    summ_one_metric(metric, train_col, test_col)
  }) %>%
  mutate(
    model = factor(model, levels = c("RSF", "RSF_LC", "COX", "JM")),
    metric = factor(metric, levels = c("C-index", "AUC", "BS"))
  ) %>%
  arrange(model, metric)

## Excel：均数 ± SD，均保留三位小数（与均数同列）
fmt_mean_sd <- function(mu, sigma) {
  mu <- round(as.numeric(mu), 3)
  sigma <- round(as.numeric(sigma), 3)
  ifelse(
    is.na(sigma),
    sprintf("%.3f", mu),
    sprintf("%.3f ± %.3f", mu, sigma)
  )
}

dat_xlsx_out <- dat %>%
  mutate(
    模型 = as.character(model),
    指标 = as.character(metric),
    训练集 = fmt_mean_sd(train, train_sd),
    验证集 = fmt_mean_sd(test, test_sd),
    diff = fmt_mean_sd(diff, diff_sd)
  ) %>%
  group_by(model) %>%
  mutate(模型 = if_else(row_number() == 1L, as.character(model), NA_character_)) %>%
  ungroup() %>%
  select(模型, 指标, 训练集, 验证集, diff)

writexl::write_xlsx(
  list(Sheet1 = dat_xlsx_out),
  path = file.path(plot_dir, "dat.xlsx")
)
message("已写入：", file.path(plot_dir, "dat.xlsx"))

dat_long <- dat %>%
  pivot_longer(c(train, test), names_to = "split", values_to = "value") %>%
  mutate(
    split = recode(split, train = "训练集 n=627", test = "测试集 n=158"),
    model = fct_relevel(factor(model), "RSF", "RSF_LC", "COX", "JM"),
    metric = fct_relevel(factor(metric), "C-index", "AUC", "BS")
  )




ggplot(dat_long, aes(x = model, y = value, fill = split)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.75) +
  facet_wrap(~ metric, scales = "free_y", nrow = 1) +
  scale_fill_brewer(palette = "Set2") +
  labs(x = NULL, y = NULL, fill = NULL,
       title = "各模型在训练集与测试集上的表现") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top",
        axis.text.x = element_text(angle = 25, hjust = 1))




ggplot(dat_long, aes(x = split, y = value, group = model, color = model)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2.5) +
  facet_wrap(~ metric, scales = "free_y", nrow = 1) +
  labs(x = NULL, y = NULL, color = "模型",
       title = "训练集到测试集的变化（线越陡落差越大）") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top")





library(ggplot2)

dat$model <- factor(dat$model, levels = c("RSF", "RSF_LC", "COX", "JM"))
dat$metric <- factor(dat$metric, levels = c("C-index", "AUC", "BS"))

ggplot(dat, aes(y = forcats::fct_rev(model))) +
  geom_segment(
    aes(x = train, xend = test, yend = after_stat(y)),
    linewidth = 1,
    color = "grey60"
  ) +
  geom_point(aes(x = train, color = "训练集"), size = 3) +
  geom_point(aes(x = test,  color = "测试集"), size = 3) +
  facet_wrap(~ metric, nrow = 1, scales = "free_x") +
  scale_color_manual(values = c("训练集" = "#E41A1C", "测试集" = "#377EB8")) +
  labs(
    x = NULL,
    y = NULL,
    color = NULL,
    title = "训练集 vs 测试集（线段两端为取值）"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top")





# 误差棒图：五折间 SD（train_sd / test_sd 来自各折 train/test 指标）
dat_err <- bind_rows(
  transmute(dat, model, metric, split = "训练集 n=627", value = train, se = train_sd),
  transmute(dat, model, metric, split = "测试集 n=158", value = test, se = test_sd)
) %>%
  mutate(
    model = fct_relevel(factor(model), "RSF", "RSF_LC", "COX", "JM"),
    metric = fct_relevel(factor(metric), "C-index", "AUC", "BS"),
    lower_raw = value - se,
    upper_raw = value + se,
    lower = pmax(0, lower_raw),
    upper = case_when(
      metric %in% c("C-index", "AUC") ~ pmin(1, upper_raw),
      TRUE ~ upper_raw
    )
  )

pd_err <- position_dodge(width = 0.78)

p_errbar <- ggplot(dat_err, aes(x = model, y = value, fill = split)) +
  geom_col(position = pd_err, width = 0.68, alpha = 0.9) +
  geom_errorbar(
    aes(ymin = lower, ymax = upper),
    position = pd_err,
    width = 0.18,
    linewidth = 0.45,
    color = "gray25"
  ) +
  facet_wrap(~ metric, scales = "free_y", nrow = 1) +
  scale_fill_manual(
    values = c("训练集 n=627" = "#E41A1C", "测试集 n=158" = "#377EB8")
  ) +
  labs(
    x = NULL,
    y = NULL,
    fill = NULL,
    title = "各模型训练/测试表现 误差棒图",
    caption = "误差棒为均值±1×SD（五折各折指标的标准差）。"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "top",
    axis.text.x = element_text(angle = 25, hjust = 1),
    plot.caption = element_text(size = 9, color = "gray35")
  )

ggsave(
  filename = file.path(plot_dir, "误差棒图.png"),
  plot = p_errbar,
  width = 10,
  height = 4,
  dpi = 300,
  bg = "white"
)
