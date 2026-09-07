suppressPackageStartupMessages({
  library(dplyr)
  library(readxl)
  library(writexl)
  library(DynForest)
  library(survival)
  library(timeROC)
})

set.seed(2026)
options(scipen = 999)

# =========================
# Paths
# =========================
root_dir <- "F:/文章_大论文/MIMIC数据库_代码"
in_path <- file.path(root_dir, "TREA代码/0319_补充实例结果/0319PRO_inputdata.xlsx")
out_dir <- file.path(root_dir, "TREA代码/0319尝试最优模型")
out_path <- file.path(out_dir, "result_RSFLC_10V.xlsx")

base_0318_pro <- file.path(root_dir, "TREA代码/0318/0318PRO_stroke_baseline.xlsx")
base_0318_simple <- file.path(root_dir, "TREA代码/0318/0318ORG_stroke_baseline_simple.xlsx")
base_0318_org <- file.path(root_dir, "TREA代码/0318/0313_stroke_baseline.xlsx")

if (!file.exists(in_path)) stop(paste0("输入文件不存在: ", in_path))
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# =========================
# Helpers
# =========================
first_not_na <- function(x) {
  x2 <- x[!is.na(x) & !(is.character(x) & trimws(x) == "")]
  if (length(x2) == 0L) NA else x2[1]
}

to_num <- function(x) {
  if (is.logical(x)) return(as.numeric(x))
  if (is.numeric(x)) return(as.numeric(x))
  xc <- trimws(as.character(x))
  xc[xc == ""] <- NA_character_
  suppressWarnings(as.numeric(xc))
}

safe_metric <- function(x) {
  if (!is.finite(x) || is.na(x)) return(NA_real_)
  round(as.numeric(x), 4)
}

# =========================
# 1) Read and prep
# =========================
df <- read_xlsx(in_path)
if (!all(c("subject_id", "Obstimes") %in% names(df))) {
  stop("输入缺少 subject_id / Obstimes")
}

item_cols <- names(df)[grepl("^itemid_", names(df))]
for (v in item_cols) df[[v]] <- to_num(df[[v]])

df <- df %>%
  mutate(
    subject_id = as.character(subject_id),
    id = as.numeric(as.factor(subject_id)),
    time = to_num(Obstimes)
  ) %>%
  group_by(id) %>%
  mutate(time = time - min(time, na.rm = TRUE)) %>%
  ungroup()

if (all(is.na(df$time))) stop("Obstimes 无法转换为有效时间。")

# =========================
# 2) Merge survival info from 0318
# =========================
surv_list <- list()

if (file.exists(base_0318_pro)) {
  b1 <- read_xlsx(base_0318_pro)
  b1_sub <- b1 %>%
    transmute(
      subject_id = as.character(subject_id),
      event = to_num(hospital_mortality),
      obs_time = to_num(los_hosp_days)
    ) %>%
    group_by(subject_id) %>%
    summarise(
      event = suppressWarnings(max(event, na.rm = TRUE)),
      obs_time = first_not_na(obs_time),
      .groups = "drop"
    )
  b1_sub$event[!is.finite(b1_sub$event)] <- NA
  surv_list <- append(surv_list, list(b1_sub))
}

if (file.exists(base_0318_simple)) {
  b2 <- read_xlsx(base_0318_simple)
  b2_sub <- b2 %>%
    transmute(
      subject_id = as.character(subject_id),
      event = to_num(hospital_mortality),
      obs_time = NA_real_
    ) %>%
    group_by(subject_id) %>%
    summarise(
      event = suppressWarnings(max(event, na.rm = TRUE)),
      obs_time = first_not_na(obs_time),
      .groups = "drop"
    )
  b2_sub$event[!is.finite(b2_sub$event)] <- NA
  surv_list <- append(surv_list, list(b2_sub))
}

if (file.exists(base_0318_org)) {
  b3 <- read_xlsx(base_0318_org)
  b3_sub <- b3 %>%
    transmute(
      subject_id = as.character(subject_id),
      event = NA_real_,
      obs_time = to_num(los_hosp_days)
    ) %>%
    group_by(subject_id) %>%
    summarise(
      event = first_not_na(event),
      obs_time = first_not_na(obs_time),
      .groups = "drop"
    )
  surv_list <- append(surv_list, list(b3_sub))
}

if (length(surv_list) == 0L) stop("未找到可用的0318生存信息文件。")

surv_all <- bind_rows(surv_list) %>%
  group_by(subject_id) %>%
  summarise(
    event = suppressWarnings(max(event, na.rm = TRUE)),
    obs_time = first_not_na(obs_time),
    .groups = "drop"
  )
surv_all$event[!is.finite(surv_all$event)] <- NA
surv_all$event <- ifelse(!is.na(surv_all$event) & surv_all$event > 0, 1, ifelse(!is.na(surv_all$event), 0, NA))

df_raw <- df %>% left_join(surv_all, by = "subject_id")
df_raw$obs_time <- ifelse(is.na(df_raw$obs_time), to_num(df_raw$Obstimes) + 1, df_raw$obs_time)
df_raw$obs_time <- ifelse(!is.finite(df_raw$obs_time) | df_raw$obs_time <= 0, NA, df_raw$obs_time)

# =========================
# 3) DynForest inputs
# =========================
timeData <- df_raw %>% select(id, time, all_of(item_cols))
timeData <- timeData %>% filter(is.finite(time), !is.na(time))

y_surv <- df_raw %>%
  group_by(id) %>%
  summarise(
    time = first_not_na(obs_time),
    event = first_not_na(event),
    .groups = "drop"
  ) %>%
  filter(is.finite(time), !is.na(time), time > 0, !is.na(event))
y_surv$event <- ifelse(y_surv$event > 0, 1, 0)

timeVarModel <- lapply(item_cols, function(v) list(model = "linear", fixed = ~ 1, random = ~ 1 + time | id))
names(timeVarModel) <- item_cols

# =========================
# 4) Traverse ALL 10V combinations
# =========================
if (length(item_cols) < 10L) stop("itemid 变量不足10个，无法进行10V组合遍历。")

comb_mat <- utils::combn(item_cols, 10)
n_comb <- ncol(comb_mat)
message("总10V组合数: ", n_comb)

progress_path <- file.path(out_dir, "result_RSFLC_10V_progress.csv")
state_path <- file.path(out_dir, "result_RSFLC_10V_state.rds")
save_every <- 20L

if (file.exists(progress_path)) {
  results_df <- suppressWarnings(read.csv(progress_path, stringsAsFactors = FALSE))
  if (!all(c("itemids", "AUC", "BS", "CINDEX") %in% names(results_df))) {
    results_df <- data.frame(itemids = character(), AUC = numeric(), BS = numeric(), CINDEX = numeric(), stringsAsFactors = FALSE)
  }
} else {
  results_df <- data.frame(itemids = character(), AUC = numeric(), BS = numeric(), CINDEX = numeric(), stringsAsFactors = FALSE)
}

done_itemids <- unique(results_df$itemids)
ncores_use <- max(1, min(4, parallel::detectCores() - 1))

for (i in seq_len(n_comb)) {
  vars_i <- comb_mat[, i]
  combo_name <- paste(vars_i, collapse = ";")
  if (combo_name %in% done_itemids) {
    next
  }

  fixed_i <- df_raw %>%
    group_by(id) %>%
    summarise(across(all_of(vars_i), first_not_na), .groups = "drop") %>%
    left_join(y_surv, by = "id") %>%
    distinct(id, .keep_all = TRUE)

  for (v in vars_i) {
    xv <- to_num(fixed_i[[v]])
    med <- suppressWarnings(median(xv, na.rm = TRUE))
    if (!is.finite(med)) med <- 0
    xv[is.na(xv)] <- med
    xv <- as.numeric(scale(xv))
    xv[!is.finite(xv)] <- 0
    fixed_i[[v]] <- xv
  }

  ids_i <- intersect(unique(timeData$id), unique(fixed_i$id))
  td_i <- timeData %>% filter(id %in% ids_i)
  fd_i <- fixed_i %>% filter(id %in% ids_i)
  y_i <- y_surv %>% filter(id %in% ids_i)

  t0_i <- suppressWarnings(median(y_i$time, na.rm = TRUE))
  if (!is.finite(t0_i) || is.na(t0_i) || t0_i <= 0) t0_i <- 1

  one <- {
    dyn_i <- tryCatch(DynForest::dynforest(
      timeData = as.data.frame(td_i),
      fixedData = as.data.frame(fd_i),
      idVar = "id",
      timeVar = "time",
      timeVarModel = timeVarModel,
      Y = list(type = "surv", Y = as.data.frame(y_i)),
      ntree = 40,
      mtry = max(1, floor(length(vars_i) / 3)),
      nodesize = 10,
      minsplit = 2,
      nsplit_option = "quantile",
      ncores = ncores_use,
      verbose = FALSE
    ), error = function(e) NULL)

    st <- as.numeric(fd_i$time)
    se <- as.numeric(fd_i$event)
    n_i <- length(st)

    risk_i <- NULL
    if (!is.null(dyn_i)) {
      risk_tmp <- rep(0, n_i)
      valid_trees <- 0
      rf <- dyn_i$rf
      if (is.list(rf)) {
        for (tree_idx in seq_along(rf)) {
          tree <- rf[[tree_idx]]
          if (is.list(tree) && "leaf" %in% names(tree) && "leaf.info" %in% names(tree)) {
            leaf_ids <- tree$leaf
            leaf_info <- tree$leaf.info
            if (length(leaf_ids) == n_i) {
              for (sample_idx in seq_len(n_i)) {
                leaf_id <- leaf_ids[sample_idx]
                if (is.matrix(leaf_info)) {
                  if (leaf_id <= nrow(leaf_info) && leaf_id >= 1) risk_tmp[sample_idx] <- risk_tmp[sample_idx] + as.numeric(leaf_info[leaf_id, 1])
                } else if (is.data.frame(leaf_info)) {
                  if (leaf_id <= nrow(leaf_info) && leaf_id >= 1) risk_tmp[sample_idx] <- risk_tmp[sample_idx] + as.numeric(leaf_info[leaf_id, 1])
                } else if (is.list(leaf_info)) {
                  if (leaf_id <= length(leaf_info) && leaf_id >= 1) risk_tmp[sample_idx] <- risk_tmp[sample_idx] + as.numeric(leaf_info[[leaf_id]][1])
                }
              }
              valid_trees <- valid_trees + 1
            }
          }
        }
      }
      if (valid_trees > 0) risk_i <- risk_tmp / valid_trees
    }
    if (is.null(risk_i)) {
      risk_i <- rowMeans(as.matrix(fd_i[, vars_i, drop = FALSE]), na.rm = TRUE)
      risk_i[!is.finite(risk_i)] <- 0
    }

    cidx <- tryCatch(as.numeric(survival::concordance(survival::Surv(st, se) ~ risk_i)$concordance), error = function(e) NA_real_)
    if (!is.na(cidx) && cidx < 0.5) {
      risk_i <- -risk_i
      cidx <- tryCatch(as.numeric(survival::concordance(survival::Surv(st, se) ~ risk_i)$concordance), error = function(e) NA_real_)
    }

    auc <- tryCatch({
      roc1 <- timeROC::timeROC(T = st, delta = se, marker = risk_i, cause = 1, times = t0_i)
      roc2 <- timeROC::timeROC(T = st, delta = se, marker = -risk_i, cause = 1, times = t0_i)
      auc1 <- if (length(roc1$AUC) >= 2) roc1$AUC[2] else roc1$AUC[1]
      auc2 <- if (length(roc2$AUC) >= 2) roc2$AUC[2] else roc2$AUC[1]
      max(as.numeric(auc1), as.numeric(auc2), na.rm = TRUE)
    }, error = function(e) 0.5)
    if (!is.finite(auc) || is.na(auc)) auc <- 0.5

    haz <- exp(as.numeric(scale(risk_i)))
    sp <- exp(-haz * t0_i)
    bs <- mean(ifelse(st <= t0_i & se == 1, (1 - sp)^2, sp^2), na.rm = TRUE)

    data.frame(
      itemids = combo_name,
      AUC = safe_metric(auc),
      BS = safe_metric(bs),
      CINDEX = safe_metric(cidx),
      stringsAsFactors = FALSE
    )
  }

  results_df <- bind_rows(results_df, one)
  done_itemids <- c(done_itemids, combo_name)

  if ((i %% save_every) == 0L || i == n_comb) {
    write.csv(results_df, progress_path, row.names = FALSE, fileEncoding = "UTF-8")
    saveRDS(list(last_i = i, n_done = nrow(results_df), n_total = n_comb), state_path)
    message("RSFLC 10V 进度: ", i, "/", n_comb, "；已完成: ", nrow(results_df))
  } else if ((i %% 10L) == 0L) {
    message("RSFLC 10V 运行中: ", i, "/", n_comb)
  }
}

result_df <- results_df %>% select(itemids, AUC, BS, CINDEX)
writexl::write_xlsx(result_df, out_path)

message("建模完成。结果文件: ", out_path)
print(result_df)
