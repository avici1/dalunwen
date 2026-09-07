# =============================================================================
# 纵向数据 KNN 填补（recipes::step_impute_knn）
# 数据：stroke_longitudinal_filterP1P99_0824.csv
#
# 分组：按 hadm_id 随机分为 70% / 30% 两组，组内各自找邻居填补
# 结果（仅保存在当前 R 会话，并写出合并后的 csv）：
#   stroke_longitudinal_0824_70 / _30          填补前分组
#   stroke_longitudinal_knn_0824_70 / _30      填补后分组
#   stroke_longitudinal_knn_0824               合并表（group：1=70%，2=30%）
# =============================================================================

library(dplyr)
library(recipes)

input_path <- "F:/文章_大论文/0722/实例研究代码/stroke_longitudinal_filterP1P99_0824.csv"
split_seed <- 20240824
prop_70 <- 0.7
neighbors_k <- 5L

stroke_longitudinal_0824 <- read.csv(
  input_path,
  stringsAsFactors = FALSE,
  check.names = FALSE,
  fileEncoding = "UTF-8"
)

# ---------------------------------------------------------------------------
# 1. 列角色
# ---------------------------------------------------------------------------
id_cols <- c("X", "subject_id", "hadm_id", "chart_date", "times")

score_cat_vars <- c(
  "sofa_24hours",
  "cns_24hours",
  "renal_24hours",
  "cardiovascular_24hours",
  "respiration_24hours",
  "aki_stage",
  "gcs",
  "item_228332"
)

flag_cat_vars <- c(
  "gender",
  "sex_male",
  "ischemic_stroke",
  "intracerebral_hemorrhage",
  "subarachnoid_hemorrhage",
  "tia",
  "n_stroke_types",
  "icd_version_primary",
  "seq_num_primary"
)

text_cols <- c(
  "height_source",
  "weight_source",
  "bmi_source",
  "stroke_subtype_primary",
  "stroke_type",
  "icd_code_primary"
)

name_cat_vars <- names(stroke_longitudinal_0824)[
  grepl("sofa|aki_stage|_24hours$|^gcs$", names(stroke_longitudinal_0824), ignore.case = TRUE)
]

cat_vars <- unique(c(score_cat_vars, flag_cat_vars, name_cat_vars))
cat_vars <- intersect(cat_vars, names(stroke_longitudinal_0824))
cat_vars <- setdiff(cat_vars, c(id_cols, text_cols))

num_vars <- setdiff(
  names(stroke_longitudinal_0824),
  c(id_cols, text_cols, cat_vars)
)
num_vars <- num_vars[vapply(stroke_longitudinal_0824[num_vars], is.numeric, logical(1))]

# ---------------------------------------------------------------------------
# 2. 小数位数与分类取值范围（用全表观测，两组共用，保证尺度一致）
# ---------------------------------------------------------------------------
infer_decimals <- function(x, max_d = 4L) {
  x <- x[is.finite(x)]
  if (length(x) == 0) {
    return(0L)
  }
  for (d in 0:max_d) {
    if (mean(abs(x - round(x, d)) < 1e-8) >= 0.99) {
      return(as.integer(d))
    }
  }
  max_d
}

decimal_map <- vapply(num_vars, function(v) {
  infer_decimals(stroke_longitudinal_0824[[v]])
}, integer(1))

cat_range <- lapply(cat_vars, function(v) {
  x <- suppressWarnings(as.numeric(stroke_longitudinal_0824[[v]]))
  x <- x[is.finite(x)]
  if (!length(x)) {
    return(c(NA_real_, NA_real_))
  }
  c(min(x), max(x))
})
names(cat_range) <- cat_vars

orig_char_cat <- cat_vars[vapply(stroke_longitudinal_0824[cat_vars], is.character, logical(1))]
orig_num_cat <- setdiff(cat_vars, orig_char_cat)

restore_cat_numeric <- function(x, rng) {
  z <- suppressWarnings(as.numeric(as.character(x)))
  z <- round(z)
  if (is.finite(rng[[1]]) && is.finite(rng[[2]])) {
    z <- pmin(pmax(z, rng[[1]]), rng[[2]])
  }
  z
}

# ---------------------------------------------------------------------------
# 3. 按 hadm_id 分为 70% / 30%
# ---------------------------------------------------------------------------
set.seed(split_seed)
hadm_all <- unique(stroke_longitudinal_0824$hadm_id)
n_70 <- floor(length(hadm_all) * prop_70)
hadm_70 <- sample(hadm_all, size = n_70)
hadm_30 <- setdiff(hadm_all, hadm_70)

stroke_longitudinal_0824_70 <- stroke_longitudinal_0824 %>%
  filter(hadm_id %in% hadm_70)
stroke_longitudinal_0824_30 <- stroke_longitudinal_0824 %>%
  filter(hadm_id %in% hadm_30)

cat(
  "分组: 70% hadm=", length(hadm_70), " 行=", nrow(stroke_longitudinal_0824_70),
  " ; 30% hadm=", length(hadm_30), " 行=", nrow(stroke_longitudinal_0824_30), "\n",
  sep = ""
)
cat("分类变量:", paste(cat_vars, collapse = ", "), "\n")
cat("连续变量小数位:\n")
print(decimal_map)

# ---------------------------------------------------------------------------
# 4. 单组 KNN 填补
# ---------------------------------------------------------------------------
knn_impute_one <- function(dat_raw, group_label) {
  dat_model <- dat_raw
  for (v in cat_vars) {
    dat_model[[v]] <- factor(dat_model[[v]])
  }

  n_obs <- vapply(dat_model, function(x) sum(!is.na(x)), integer(1))
  n_miss <- vapply(dat_model, function(x) sum(is.na(x)), integer(1))

  model_vars <- setdiff(names(dat_model), c(id_cols, text_cols))
  predictor_vars <- model_vars[n_obs[model_vars] > neighbors_k]
  impute_vars <- predictor_vars[n_miss[predictor_vars] > 0]
  too_sparse <- setdiff(model_vars, predictor_vars)

  cat("---- ", group_label, " 将填补列数: ", length(impute_vars), " ----\n", sep = "")
  if (length(too_sparse)) {
    cat("  观测过少、不进入 KNN: ", paste(too_sparse, collapse = ", "), "\n", sep = "")
  }

  rec <- recipe(~., data = dat_model, strings_as_factors = FALSE) %>%
    update_role(all_of(id_cols), new_role = "id") %>%
    update_role(all_of(intersect(text_cols, names(dat_model))), new_role = "id")

  if (length(too_sparse)) {
    rec <- rec %>% update_role(all_of(too_sparse), new_role = "id")
  }

  rec <- rec %>%
    step_impute_knn(
      all_of(impute_vars),
      neighbors = neighbors_k,
      impute_with = imp_vars(all_of(predictor_vars))
    )

  rec_prep <- prep(rec, training = dat_model)
  dat_knn <- bake(rec_prep, new_data = dat_model) %>%
    select(all_of(names(dat_raw)))

  for (v in orig_num_cat) {
    dat_knn[[v]] <- restore_cat_numeric(dat_knn[[v]], cat_range[[v]])
  }
  for (v in orig_char_cat) {
    dat_knn[[v]] <- as.character(dat_knn[[v]])
  }
  for (v in num_vars) {
    d <- decimal_map[[v]]
    x <- suppressWarnings(as.numeric(dat_knn[[v]]))
    dat_knn[[v]] <- round(x, d)
  }
  for (v in intersect(text_cols, names(dat_raw))) {
    dat_knn[[v]] <- dat_raw[[v]]
  }
  for (v in intersect(id_cols, names(dat_raw))) {
    dat_knn[[v]] <- dat_raw[[v]]
  }

  dat_knn
}

stroke_longitudinal_knn_0824_70 <- knn_impute_one(stroke_longitudinal_0824_70, "70%")
stroke_longitudinal_knn_0824_30 <- knn_impute_one(stroke_longitudinal_0824_30, "30%")

cat(
  "填补完成。\n",
  "  stroke_longitudinal_knn_0824_70 : ",
  nrow(stroke_longitudinal_knn_0824_70), " x ", ncol(stroke_longitudinal_knn_0824_70), "\n",
  "  stroke_longitudinal_knn_0824_30 : ",
  nrow(stroke_longitudinal_knn_0824_30), " x ", ncol(stroke_longitudinal_knn_0824_30), "\n",
  sep = ""
)

stroke_longitudinal_knn_0824 <- bind_rows(
  stroke_longitudinal_knn_0824_70 %>% mutate(group = 1),
  stroke_longitudinal_knn_0824_30 %>% mutate(group = 2)
)
write.csv(stroke_longitudinal_knn_0824, "F:/文章_大论文/0722/实例研究代码/stroke_longitudinal_knn_0824.csv", row.names = FALSE, fileEncoding = "UTF-8")
cat("已保存: stroke_longitudinal_knn_0824.csv ; group=1 为 70%，group=2 为 30%\n")



##########baseline填补############
# =============================================================================
# 基线数据 KNN 填补（recipes::step_impute_knn，包与纵向脚本相同）
# 数据：stroke_baselinedata_0824.csv
# 沿用已有 group：1=70%，2=30%，组内各自找邻居填补
# 不填补：ID / 时间 / 来源文本 / 结局（deathtime 对存活者为结构性缺失）
# 结果：
#   stroke_baseline_0824_70 / _30          填补前分组
#   stroke_baseline_knn_0824_70 / _30      填补后分组
#   stroke_baseline_knn_0824               合并表
# =============================================================================

baseline_input_path <- "F:/文章_大论文/0722/实例研究代码/stroke_baselinedata_0824.csv"

stroke_baseline_0824 <- read.csv(
  baseline_input_path,
  stringsAsFactors = FALSE,
  check.names = FALSE,
  na.strings = c("", "NA"),
  fileEncoding = "UTF-8"
)

# ---------------------------------------------------------------------------
# 1. 缺失核查（全列）
# ---------------------------------------------------------------------------
n_row_bl <- nrow(stroke_baseline_0824)
miss_n_bl <- vapply(stroke_baseline_0824, function(x) sum(is.na(x)), integer(1))
miss_tbl_bl <- data.frame(
  variable = names(miss_n_bl),
  n_miss = as.integer(miss_n_bl),
  pct_miss = round(100 * miss_n_bl / n_row_bl, 2),
  stringsAsFactors = FALSE
)
miss_tbl_bl <- miss_tbl_bl[order(-miss_tbl_bl$n_miss, miss_tbl_bl$variable), ]
rownames(miss_tbl_bl) <- NULL

cat("基线表: ", n_row_bl, " x ", ncol(stroke_baseline_0824), "\n", sep = "")
cat("有缺失的变量:\n")
print(miss_tbl_bl[miss_tbl_bl$n_miss > 0, ], row.names = FALSE)
cat("其余变量均无缺失。\n")

# ---------------------------------------------------------------------------
# 2. 列角色（覆盖纵向段的全局对象，供 knn_impute_one 复用）
# ---------------------------------------------------------------------------
id_cols <- c(
  "subject_id", "hadm_id", "stay_id", "group",
  "intime", "outtime", "admittime", "dischtime", "deathtime",
  "icu_los_days",
  "death_28d", "death_90d", "death_1y", "death_3y",
  "hospital_expire_flag", "icu_expire_flag"
)

text_cols <- c(
  "height_source",
  "weight_source",
  "bmi_source",
  "stroke_subtype_primary",
  "stroke_type",
  "icd_code_primary"
)

flag_cat_vars <- c(
  "gender",
  "sex_male",
  "mechvent",
  "electivesurgery",
  "ischemic_stroke",
  "intracerebral_hemorrhage",
  "subarachnoid_hemorrhage",
  "tia",
  "n_stroke_types",
  "icd_version_primary",
  "seq_num_primary"
)

cat_vars <- intersect(flag_cat_vars, names(stroke_baseline_0824))
cat_vars <- setdiff(cat_vars, c(id_cols, text_cols))

num_vars <- setdiff(
  names(stroke_baseline_0824),
  c(id_cols, text_cols, cat_vars)
)
num_vars <- num_vars[vapply(stroke_baseline_0824[num_vars], is.numeric, logical(1))]

decimal_map <- vapply(num_vars, function(v) {
  infer_decimals(stroke_baseline_0824[[v]])
}, integer(1))

cat_range <- lapply(cat_vars, function(v) {
  x <- suppressWarnings(as.numeric(stroke_baseline_0824[[v]]))
  x <- x[is.finite(x)]
  if (!length(x)) {
    return(c(NA_real_, NA_real_))
  }
  c(min(x), max(x))
})
names(cat_range) <- cat_vars

orig_char_cat <- cat_vars[vapply(stroke_baseline_0824[cat_vars], is.character, logical(1))]
orig_num_cat <- setdiff(cat_vars, orig_char_cat)

# ---------------------------------------------------------------------------
# 3. 按已有 group 分为 70% / 30%
# ---------------------------------------------------------------------------
stroke_baseline_0824_70 <- stroke_baseline_0824 %>%
  filter(group == 1)
stroke_baseline_0824_30 <- stroke_baseline_0824 %>%
  filter(group == 2)

cat(
  "分组: 70% group=1 行=", nrow(stroke_baseline_0824_70),
  " ; 30% group=2 行=", nrow(stroke_baseline_0824_30), "\n",
  sep = ""
)
cat("分类变量:", paste(cat_vars, collapse = ", "), "\n")
cat("连续变量小数位:\n")
print(decimal_map)

# ---------------------------------------------------------------------------
# 4. 组内 KNN 填补（height_cm / weight_kg / bmi）
# ---------------------------------------------------------------------------
stroke_baseline_knn_0824_70 <- knn_impute_one(stroke_baseline_0824_70, "baseline 70%")
stroke_baseline_knn_0824_30 <- knn_impute_one(stroke_baseline_0824_30, "baseline 30%")

# 仅对原本缺失的 BMI，用填补后的身高体重回算，已观测 BMI 保持不变
recalc_bmi_if_missing <- function(dat, dat_raw) {
  if (!all(c("height_cm", "weight_kg", "bmi") %in% names(dat))) {
    return(dat)
  }
  h <- suppressWarnings(as.numeric(dat$height_cm))
  w <- suppressWarnings(as.numeric(dat$weight_kg))
  d <- if ("bmi" %in% names(decimal_map)) decimal_map[["bmi"]] else 2L
  ok <- is.na(dat_raw$bmi) & is.finite(h) & is.finite(w) & h > 0
  dat$bmi[ok] <- round(w[ok] / (h[ok] / 100)^2, d)
  dat
}

stroke_baseline_knn_0824_70 <- recalc_bmi_if_missing(
  stroke_baseline_knn_0824_70, stroke_baseline_0824_70
)
stroke_baseline_knn_0824_30 <- recalc_bmi_if_missing(
  stroke_baseline_knn_0824_30, stroke_baseline_0824_30
)

cat(
  "填补完成。\n",
  "  stroke_baseline_knn_0824_70 : ",
  nrow(stroke_baseline_knn_0824_70), " x ", ncol(stroke_baseline_knn_0824_70), "\n",
  "  stroke_baseline_knn_0824_30 : ",
  nrow(stroke_baseline_knn_0824_30), " x ", ncol(stroke_baseline_knn_0824_30), "\n",
  sep = ""
)

stroke_baseline_knn_0824 <- bind_rows(
  stroke_baseline_knn_0824_70,
  stroke_baseline_knn_0824_30
)

impute_check_vars <- c("height_cm", "weight_kg", "bmi")
impute_check_vars <- intersect(impute_check_vars, names(stroke_baseline_knn_0824))
miss_after <- vapply(
  stroke_baseline_knn_0824[impute_check_vars],
  function(x) sum(is.na(x)),
  integer(1)
)
cat("填补后 BMI 相关数值列缺失:\n")
print(miss_after)

write.csv(
  stroke_baseline_knn_0824,
  "F:/文章_大论文/0722/实例研究代码/stroke_baseline_knn_0824.csv",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)
cat("已保存: stroke_baseline_knn_0824.csv ; group=1 为 70%，group=2 为 30%\n")


