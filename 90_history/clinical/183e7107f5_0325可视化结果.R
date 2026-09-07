library(tidyverse)
library(ggplot2)


dat <- tggplot2dat <- tribble(
  ~model, ~metric, ~train, ~test, ~diff,
  "RSF",     "C-index", 0.948, 0.663,  0.285,
  "RSF",     "AUC",     0.998, 0.674,  0.324,
  "RSF",     "BS",      0.017, 0.052, -0.035,
  "RSF_LC",  "C-index", 0.889, 0.894, -0.005,
  "RSF_LC",  "AUC",     0.876, 0.882, -0.006,
  "RSF_LC",  "BS",      0.069, 0.069,  0.000,
  "COX",     "C-index", 0.713, 0.595,  0.118,
  "COX",     "AUC",     0.691, 0.564,  0.127,
  "COX",     "BS",      0.061, 0.067, -0.006,
  "JM",      "C-index", 0.543, 0.580, -0.037,
  "JM",      "AUC",     0.622, 0.621,  0.001,
  "JM",      "BS",      0.158, 0.153,  0.005
)

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


# 热力图：每个指标单独一张小图并各自配色（避免 C-index / AUC / BS 量纲混在同一色条上）
# 若未安装： install.packages("patchwork")
library(patchwork)

dat_heat <- dat %>%
  pivot_longer(c(train, test), names_to = "split", values_to = "value") %>%
  mutate(
    split = recode(split, train = "训练集 n=627", test = "测试集 n=158"),
    model = fct_relevel(factor(model), "RSF", "RSF_LC", "COX", "JM"),
    metric = fct_relevel(factor(metric), "C-index", "AUC", "BS")
  )

heat_one_metric <- function(d) {
  title_metric <- as.character(unique(d$metric))
  ggplot(d, aes(x = split, y = model, fill = value)) +
    geom_tile(color = "white", linewidth = 0.6) +
    geom_text(aes(label = sprintf("%.3f", value)), size = 3.4, color = "gray10") +
    scale_fill_viridis_c(option = "C") +
    labs(title = title_metric, x = NULL, y = NULL, fill = "取值") +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      legend.position = "right",
      panel.grid = element_blank()
    )
}

plots_heat <- dat_heat %>%
  group_split(metric) %>%
  map(heat_one_metric)

wrap_plots(plots_heat, nrow = 1) +
  plot_annotation(
    title = "各模型表现热力图（按指标分图，色阶独立）"
  )






# 误差棒图：当前脚本仅有估计值，无真实 SE/CI；下面 se 为展示用占位，正式使用请改为 bootstrap 等得到的 SE 或 95% CI 半宽
dat_err <- dat %>%
  pivot_longer(c(train, test), names_to = "split", values_to = "value") %>%
  mutate(
    split = recode(split, train = "训练集 n=627", test = "测试集 n=158"),
    model = fct_relevel(factor(model), "RSF", "RSF_LC", "COX", "JM"),
    metric = fct_relevel(factor(metric), "C-index", "AUC", "BS"),
    # 演示：训练集略小、测试集略大的 SE；可按需改为与 n 相关的公式或逐格填真实值
    se = if_else(str_detect(split, "^训练集"), 0.028, 0.042),
    lower_raw = value - se,
    upper_raw = value + se,
    lower = pmax(0, lower_raw),
    upper = case_when(
      metric %in% c("C-index", "AUC") ~ pmin(1, upper_raw),
      TRUE ~ upper_raw
    )
  )

pd_err <- position_dodge(width = 0.78)

ggplot(dat_err, aes(x = model, y = value, fill = split)) +
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
    title = "各模型训练/测试表现（含误差棒，展示用 SE）",
    caption = "误差棒为均值±1×SE（演示）；C-index/AUC 误差限已截断至[0,1]。"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "top",
    axis.text.x = element_text(angle = 25, hjust = 1),
    plot.caption = element_text(size = 9, color = "gray35")
  )

