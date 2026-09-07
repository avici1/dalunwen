library(dplyr)
library(tidyr)
library(tibble)
library(purrr)

stroke_longitudinal_filter_0530 <- read.csv(
  "F:/文章_大论文/0521/处理后文件/stroke_longitudinal_filter_0530.csv"
)

id_cols_long <- c("X", "subject_id", "hadm_id", "chart_date", "times")
var_cols_long <- setdiff(names(stroke_longitudinal_filter_0530), id_cols_long)

# 分类变量不参与 P1-P99 判定
categorical_vars_long <- c("aki_stage", "item_228332")
continuous_vars_long <- setdiff(var_cols_long, categorical_vars_long)

# 全表连续变量 P1 / P99 阈值
p1p99_bounds <- map_dfr(continuous_vars_long, function(v) {
  x <- stroke_longitudinal_filter_0530[[v]]
  x <- x[is.finite(x)]
  if (length(x) == 0) {
    return(tibble(variable = v, p1 = NA_real_, p99 = NA_real_))
  }
  q <- quantile(x, probs = c(0.01, 0.99), names = FALSE, type = 7)
  tibble(variable = v, p1 = q[[1]], p99 = q[[2]])
})

# 任一连读变量非缺失且超出 [P1, P99] -> 该行不可靠
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

# 按 hadm_id 删行并重编号 times，再拼回全表
stroke_longitudinal_filterP1P99_0624 <- stroke_longitudinal_filter_0530 %>%
  mutate(.row_unreliable = row_unreliable) %>%
  group_by(hadm_id) %>%
  arrange(times, chart_date, .by_group = TRUE) %>%
  filter(!.row_unreliable) %>%
  mutate(times = row_number()) %>%
  ungroup() %>%
  select(-.row_unreliable)

write.csv(
  stroke_longitudinal_filterP1P99_0624,
  "F:/文章_大论文/0521/处理后文件/stroke_longitudinal_filterP1P99_0624.csv",
  row.names = FALSE
)

cat(
  "P1-P99 行清洗: 原始", nrow(stroke_longitudinal_filter_0530),
  "行, 删除", sum(row_unreliable),
  "行, 保留", nrow(stroke_longitudinal_filterP1P99_0624), "行\n"
)