library(dplyr)
library(tidyr)
library(tibble)
library(purrr)

# ============================================================
# 0. 读入数据
# ============================================================
stroke_baselinedata_filter1_0531 <- read.csv2(
  "F:/文章_大论文/0521/处理后文件/stroke_baselinedata_filter1_0531.csv"
)

stroke_longitudinal_filter_0530 <- read.csv(
  "F:/文章_大论文/0521/处理后文件/stroke_longitudinal_filter_0530.csv"
)

# ============================================================
# 1. 公共定义：ID 列、中文标签、工具函数
# ============================================================
id_cols_long <- c("X", "subject_id", "hadm_id", "chart_date", "times")

item_label_cn_map <- tribble(
  ~variable,        ~标签,
  "item_223761",    "体温（华氏度）",
  "item_220210",    "呼吸频率",
  "item_220277",    "血氧饱和度",
  "item_220045",    "心率",
  "item_228332",    "谵妄评估（CAM）",
  "item_50893",     "总钙",
  "item_50970",     "磷酸盐",
  "item_50960",     "镁",
  "item_51265",     "血小板计数",
  "item_51222",     "血红蛋白",
  "item_51221",     "红细胞压积",
  "item_51301",     "白细胞计数",
  "item_50931",     "血糖",
  "item_50868",     "阴离子间隙",
  "item_50882",     "碳酸氢根",
  "item_50902",     "氯离子",
  "item_50912",     "肌酐",
  "item_51006",     "尿素氮",
  "item_50971",     "钾离子",
  "item_50983",     "钠离子",
  "item_50885",     "总胆红素",
  "item_51237",     "INR",
  "item_51274",     "PT（凝血酶原时间）",
  "item_51275",     "PTT（部分凝血活酶时间）",
  "item_220179",    "无创收缩压",
  "item_220180",    "无创舒张压",
  "item_220181",    "无创平均动脉压"
)

clinical_label_map <- tribble(
  ~variable,                 ~标签,
  "aki_stage",               "AKI 分期",
  "creat",                   "血清肌酐",
  "gcs",                     "格拉斯哥昏迷评分",
  "ph",                      "血气 pH",
  "pco2",                    "二氧化碳分压",
  "lactate",                 "血乳酸",
  "po2",                     "氧分压",
  "pao2fio2ratio",           "PaO2/FiO2",
  "glucose",                 "血糖（血气）",
  "sodium",                  "血钠（血气）",
  "bicarbonate",             "碳酸氢根（血气）",
  "hemoglobin",              "血红蛋白（血气）",
  "temperature",             "体温（血气）",
  "fio2",                    "吸入氧浓度",
  "sofa_24hours",            "SOFA 总分",
  "cns_24hours",             "SOFA 神经系统",
  "renal_24hours",           "SOFA 肾脏",
  "cardiovascular_24hours",  "SOFA 心血管",
  "respiration_24hours",     "SOFA 呼吸",
  "total_urine_output",      "日尿量"
)

var_label_cn_map <- bind_rows(clinical_label_map, item_label_cn_map) %>%
  distinct(variable, .keep_all = TRUE)

categorical_coding_map <- tribble(
  ~variable,      ~分组及赋值,
  "aki_stage",    "0-无 AKI；1-Stage I；2-Stage II；3-Stage III",
  "item_228332",  "0-Negative；1-Positive"
)

var_unit_map <- c(
  total_urine_output = "mL"
)

format_var_range <- function(x, unit = "") {
  x <- x[is.finite(x)]
  if (length(x) == 0) {
    return(NA_character_)
  }
  vmin <- min(x)
  vmax <- max(x)
  is_integer_like <- isTRUE(all(abs(x - round(x)) < 1e-8))
  fmt_val <- function(v) {
    if (is_integer_like) {
      as.character(as.integer(round(v)))
    } else {
      format(round(v, 2), nsmall = 2, trim = TRUE)
    }
  }
  suffix <- if (nzchar(unit)) paste0(" ", unit) else ""
  paste0("范围: ", fmt_val(vmin), "-", fmt_val(vmax), suffix)
}

# ============================================================
# 2. P1-P99 行级清洗 -> stroke_longitudinal_filterP1P99_0624
# ============================================================
var_cols_long <- setdiff(names(stroke_longitudinal_filter_0530), id_cols_long)
categorical_vars_long <- c("aki_stage", "item_228332")
continuous_vars_long <- setdiff(var_cols_long, categorical_vars_long)

p1p99_bounds <- map_dfr(continuous_vars_long, function(v) {
  x <- stroke_longitudinal_filter_0530[[v]]
  x <- x[is.finite(x)]
  if (length(x) == 0) {
    return(tibble(variable = v, p1 = NA_real_, p99 = NA_real_))
  }
  q <- quantile(x, probs = c(0.01, 0.99), names = FALSE, type = 7)
  tibble(variable = v, p1 = q[[1]], p99 = q[[2]])
})

row_unreliable <- rep(FALSE, nrow(stroke_longitudinal_filter_0530))
for (i in seq_len(nrow(p1p99_bounds))) {
  v <- p1p99_bounds$variable[[i]]
  p1 <- p1p99_bounds$p1[[i]]
  p99 <- p1p99_bounds$p99[[i]]
  if (is.na(p1)) next
  x <- stroke_longitudinal_filter_0530[[v]]
  row_unreliable <- row_unreliable |
    (!is.na(x) & is.finite(x) & (x < p1 | x > p99))
}

stroke_longitudinal_filterP1P99_0624 <- stroke_longitudinal_filter_0530 %>%
  mutate(.row_unreliable = row_unreliable) %>%
  group_by(hadm_id) %>%
  arrange(times, chart_date, .by_group = TRUE) %>%
  filter(!.row_unreliable) %>%
  mutate(times = row_number()) %>%
  ungroup() %>%
  select(-.row_unreliable)

cat(
  "P1-P99 行清洗: 原始", nrow(stroke_longitudinal_filter_0530),
  "行, 删除", sum(row_unreliable),
  "行, 保留", nrow(stroke_longitudinal_filterP1P99_0624), "行\n"
)

# ============================================================
# 3. 缺失率（47 变量，>40% 筛选前）
# ============================================================
var_cols_long_all47 <- setdiff(names(stroke_longitudinal_filterP1P99_0624), id_cols_long)
n_total_long <- nrow(stroke_longitudinal_filterP1P99_0624)
n_subject_long <- n_distinct(stroke_longitudinal_filterP1P99_0624$subject_id)

# 3.1 全表行级缺失率
longitudinal_missingrate <- stroke_longitudinal_filterP1P99_0624 %>%
  summarise(across(all_of(var_cols_long_all47), ~ mean(is.na(.x)) * 100)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "missing_pct") %>%
  left_join(var_label_cn_map, by = "variable") %>%
  mutate(
    n_total = n_total_long,
    n_missing = round(n_total * missing_pct / 100),
    n_non_missing = n_total - n_missing,
    missing_pct = round(missing_pct, 2),
    coverage_pct = round(100 - missing_pct, 2)
  ) %>%
  arrange(desc(missing_pct)) %>%
  select(variable, 标签, n_total, n_missing, n_non_missing, missing_pct, coverage_pct)

longitudinal_missingrate

# 3.2 按患者平均缺失率
subject_var_missing <- stroke_longitudinal_filterP1P99_0624 %>%
  group_by(subject_id) %>%
  summarise(
    across(all_of(var_cols_long_all47), ~ mean(is.na(.x)) * 100),
    .groups = "drop"
  )

longitudinal_missingrate_bysubject <- subject_var_missing %>%
  summarise(across(all_of(var_cols_long_all47), ~ mean(.x))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "missing_pct") %>%
  left_join(var_label_cn_map, by = "variable") %>%
  mutate(
    n_subject = n_subject_long,
    missing_pct = round(missing_pct, 2),
    coverage_pct = round(100 - missing_pct, 2)
  ) %>%
  arrange(desc(missing_pct)) %>%
  select(variable, 标签, n_subject, missing_pct, coverage_pct)

longitudinal_missingrate_bysubject

# ============================================================
# 4. 变量筛选：患者平均缺失率 > 40% 的变量删除（47 -> 27）
# ============================================================
var_cols_long_keep <- longitudinal_missingrate_bysubject %>%
  filter(missing_pct <= 40) %>%
  pull(variable)

var_cols_long_drop <- setdiff(var_cols_long_all47, var_cols_long_keep)

stroke_longitudinal_filterP1P99_0624 <- stroke_longitudinal_filterP1P99_0624 %>%
  select(all_of(c(id_cols_long, var_cols_long_keep)))

var_cols_long <- var_cols_long_keep

cat(
  "变量缺失率筛选: 保留", length(var_cols_long_keep),
  "个, 删除", length(var_cols_long_drop), "个\n"
)

# ============================================================
# 5. 变量字典 longi__var（27 变量，缺失率 <= 40%）
# ============================================================
continuous_vars <- setdiff(var_cols_long, categorical_coding_map$variable)

var_range_map <- map_dfr(continuous_vars, function(v) {
  unit <- if (v %in% names(var_unit_map)) unname(var_unit_map[v]) else ""
  tibble(
    variable = v,
    分组及赋值 = format_var_range(
      stroke_longitudinal_filterP1P99_0624[[v]],
      unit = unit
    )
  )
})

item_label_df <- tibble(variable = var_cols_long) %>%
  filter(grepl("^item_", variable)) %>%
  left_join(item_label_cn_map, by = "variable") %>%
  select(variable, 标签)

longi__var <- tibble(变量 = var_cols_long) %>%
  left_join(clinical_label_map, by = c("变量" = "variable")) %>%
  left_join(item_label_df %>% rename(标签_item = 标签), by = c("变量" = "variable")) %>%
  left_join(categorical_coding_map, by = c("变量" = "variable")) %>%
  left_join(var_range_map %>% rename(分组及赋值_range = 分组及赋值), by = c("变量" = "variable")) %>%
  mutate(
    标签 = coalesce(标签, 标签_item),
    分组及赋值 = coalesce(分组及赋值, 分组及赋值_range)
  ) %>%
  select(变量, 标签, 分组及赋值)

longi__var

# ============================================================
# 6. 导出纵向结果
# ============================================================
out_dir <- "F:/文章_大论文/0521/放进文章的图,表,图像"

write.csv(
  longitudinal_missingrate,
  file.path(out_dir, "longitudinal_missingrate.csv"),
  row.names = FALSE
)
write.csv(
  longitudinal_missingrate_bysubject,
  file.path(out_dir, "longitudinal_missingrate_bysubject.csv"),
  row.names = FALSE
)
write.csv(
  longi__var,
  file.path(out_dir, "longi__var.csv"),
  row.names = FALSE
)

# ============================================================
# 7. 基线变量字典 baseline_var
# ============================================================
baseline_var_def <- tribble(
  ~variable,                      ~标签,                    ~类型,
  "charlson_comorbidity_index",   "Charlson 合并症指数",    "评分",
  "apsiii",                       "APACHE III 评分",        "评分",
  "apsiii_prob",                  "APACHE III 预测死亡率",  "评分",
  "oasis",                        "OASIS 评分",             "评分",
  "sapsii",                       "SAPS II 评分",           "评分",
  "age",                          "年龄",                   "连续数值",
  "preiculos",                    "ICU 前住院天数",         "连续数值",
  "icu_los_days.x",               "ICU 住院天数",           "连续数值",
  "icu_los_days.y",               "ICU 住院天数（重复列）", "连续数值",
  "hosp_los_days",                "住院总天数",             "连续数值",
  "mechvent",                     "机械通气",               "二分类",
  "electivesurgery",              "择期手术",               "二分类",
  "icu_expire_flag",              "ICU 内死亡",             "二分类",
  "hospital_expire_flag",         "院内死亡",               "二分类"
)

baseline_binary_coding <- tribble(
  ~variable,              ~分组及赋值,
  "mechvent",             "0-否；1-是",
  "electivesurgery",      "0-否；1-是",
  "icu_expire_flag",      "0-否；1-是",
  "hospital_expire_flag", "0-否；1-是"
)

baseline_unit_map <- c(
  age = "years",
  preiculos = "days",
  "icu_los_days.x" = "days",
  "icu_los_days.y" = "days",
  hosp_los_days = "days"
)

baseline_range_vars <- baseline_var_def %>%
  filter(类型 %in% c("评分", "连续数值")) %>%
  pull(variable)

baseline_range_map <- map_dfr(baseline_range_vars, function(v) {
  unit <- if (v %in% names(baseline_unit_map)) unname(baseline_unit_map[v]) else ""
  tibble(
    variable = v,
    分组及赋值 = format_var_range(
      stroke_baselinedata_filter1_0531[[v]],
      unit = unit
    )
  )
})

baseline_var <- baseline_var_def %>%
  left_join(baseline_binary_coding, by = "variable") %>%
  left_join(
    baseline_range_map %>% rename(分组及赋值_range = 分组及赋值),
    by = "variable"
  ) %>%
  mutate(分组及赋值 = coalesce(分组及赋值, 分组及赋值_range)) %>%
  select(变量 = variable, 标签, 类型, 分组及赋值)

write.csv(
  baseline_var,
  file.path(out_dir, "baseline_var.csv"),
  row.names = FALSE
)


#########################作图################################

# 纵向变量患者平均缺失率（47 变量）
library(ggplot2)

pal_low <- colorRampPalette(c("#FFFFFF", "#FEE6CE", "#FDBE85", "#FD8D3C", "#E6550D"))
plot_df <- longitudinal_missingrate_bysubject %>%
  arrange(missing_pct) %>%
  mutate(
    标签 = factor(标签, levels = 标签),
    keep_flag = if_else(missing_pct > 40, "剔除 (>40%)", "保留 (≤40%)"),
    bar_color = if_else(
      missing_pct > 40,
      "#D62728",
      pal_low(401)[pmin(round(missing_pct * 10), 400) + 1]
    ),
    label_pct = sprintf("%.1f%%", missing_pct)
  )

n_keep <- sum(plot_df$missing_pct <= 40)
x_max <- max(plot_df$missing_pct, na.rm = TRUE)

p_missing_bysubject <- ggplot(plot_df, aes(x = missing_pct, y = 标签)) +
  geom_col(
    aes(fill = I(bar_color)),
    width = 0.75,
    color = "black",
    linewidth = 0.35
  ) +
  geom_hline(
    yintercept = n_keep + 0.5,
    linewidth = 0.7,
    color = "black"
  ) +
  geom_vline(
    xintercept = 40,
    linetype = "dashed",
    linewidth = 0.5,
    color = "grey35"
  ) +
  geom_text(
    aes(label = label_pct),
    hjust = -0.15,
    size = 3,
    color = "black"
  ) +
  annotate(
    "text",
    x = x_max * 0.98,
    y = n_keep * 0.55,
    label = "保留变量 (≤40%)",
    hjust = 1,
    vjust = 4,
    size = 3.2,
    fontface = "bold",
    color = "grey25"
  ) +
  annotate(
    "text",
    x = x_max * 0.98,
    y = n_keep + (nrow(plot_df) - n_keep) * 0.45,
    label = "剔除变量 (>40%)",
    hjust = 1,
    vjust = 4,
    size = 3.2,
    fontface = "bold",
    color = "#A50F15"
  ) +
  scale_x_continuous(
    limits = c(0, x_max * 1.18),
    breaks = seq(0, 100, 10),
    expand = expansion(mult = c(0, 0))
  ) +
  scale_y_discrete(expand = expansion(mult = c(0.02, 0.02))) +
  labs(
    x = "缺失率 (%)",
    y = NULL,
    title = "纵向变量患者平均缺失率"
  ) +
  theme_classic(base_size = 11) +
  theme(
    axis.line = element_line(color = "black", linewidth = 0.5),
    axis.ticks = element_line(color = "black", linewidth = 0.4),
    axis.text.y = element_text(color = "black"),
    axis.text.x = element_text(color = "black"),
    axis.title.x = element_text(face = "bold", margin = margin(t = 8)),
    plot.title = element_text(face = "bold", hjust = 0.5, margin = margin(b = 10)),
    panel.grid = element_blank()
  )


png(
  file.path(out_dir, "longitudinal_missingrate_bysubject_barplot.png"),
  width = 1000,
  height = 1500,
  res = 300
)
print(p_missing_bysubject)
dev.off()
