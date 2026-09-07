
data_0506<-read.csv("F:/文章_大论文/0417/data_0506.csv")
glimpse(data_0506)
######数据准备#######




dat_raw <- data_0506
event_col_cv <- NULL
for (cand in c("event", "death", "hospitalmortality", "hospital_mortality", "survival")) {
  if (cand %in% names(dat_raw)) {
    event_col_cv <- cand
    break
  }
}
if (is.null(event_col_cv)) {
  stop("未找到事件列（event/death/hospitalmortality/hospital_mortality/survival）")
}
id_col <- NULL
for (cand in c("subject_id", "subjectid", "id")) {
  if (cand %in% names(dat_raw)) {
    id_col <- cand
    break
  }
}
if (is.null(id_col)) {
  stop("未找到个体 ID 列（subject_id/subjectid/id）")
}
# 按个体划分：每个患者一个 fold，同一患者所有行同一折
ids_all <- unique(dat_raw[[id_col]])
patient_event <- dat_raw %>%
  dplyr::group_by(.data[[id_col]]) %>%
  dplyr::summarise(event = as.numeric(dplyr::first(.data[[event_col_cv]])), .groups = "drop")
patient_event$event <- ifelse(
  is.na(patient_event$event) | patient_event$event > 0,
  1L,
  0L
)
K <- 5L
id_e1 <- patient_event[[id_col]][patient_event$event == 1L]
id_e0 <- patient_event[[id_col]][patient_event$event == 0L]
fold_vec <- rep(NA_integer_, length(ids_all))
if (length(id_e1) > 0L) {
  fold_vec[match(id_e1, ids_all)] <- sample(rep(1L:K, length.out = length(id_e1)))
}
if (length(id_e0) > 0L) {
  fold_vec[match(id_e0, ids_all)] <- sample(rep(1L:K, length.out = length(id_e0)))
}
fold_df <- data.frame(id = ids_all, fold = fold_vec, stringsAsFactors = FALSE)
fold_df <- fold_df[!is.na(fold_df$fold), , drop = FALSE]
message("已按个体（患者）分层划分为 5 折，共 ", nrow(fold_df), " 个个体")
# 合并 fold 到数据表，仅保留有有效 fold 的个体
fold_df_join <- fold_df
names(fold_df_join)[names(fold_df_join) == "id"] <- id_col
dat <- dat_raw %>%
  dplyr::inner_join(fold_df_join, by = id_col) %>%
  as.data.frame(stringsAsFactors = FALSE)


#############




###程序#####

########



#########交叉验证#######



#############



####程序打包####
# 第一个参数传入与脚本中 dat 同结构的数据框（须含 subject_id, time, age, hosp_time, survival 及 item_*）
# 示例：DYNforest_LC(dat, c("item_50912", "item_50971", "item_51221"))
DYNforest_LC <- function(dat, itemid) {
  if (!is.data.frame(dat)) {
    stop("dat 须为 data.frame")
  }
  if (!is.character(itemid) || !length(itemid)) {
    stop("itemid 须为非空字符向量，例如 c(\"item_50912\",\"item_50971\")")
  }
  itemid <- unique(itemid)

  tryCatch({
    if (!requireNamespace("timeROC", quietly = TRUE)) {
      install.packages("timeROC")
      library(timeROC)
    }

    ## —— 写死：小样本规模、随机种子 ——
    set.seed(1234)
    n_pick <- 100L
    subj_pool <- unique(dat$subject_id)
    n_take <- min(n_pick, length(subj_pool))
    sid_sample <- sample(subj_pool, size = n_take)
    data_small <- dat %>%
      dplyr::filter(subject_id %in% sid_sample) %>%
      as.data.frame(stringsAsFactors = FALSE)

    req_static <- c("subject_id", "time", "age", "hosp_time", "survival")
    miss_sm <- setdiff(req_static, names(data_small))
    if (length(miss_sm) > 0L) {
      stop("缺少列: ", paste(miss_sm, collapse = ", "))
    }

    miss_item_sm <- setdiff(itemid, names(data_small))
    if (length(miss_item_sm) > 0L) {
      stop("itemid 中有列不在数据中: ", paste(miss_item_sm, collapse = ", "))
    }

    timeVarModel_small <- stats::setNames(
      lapply(itemid, function(nm) {
        list(
          fixed = stats::as.formula(paste(nm, "~ time")),
          random = ~ time
        )
      }),
      itemid
    )
    item_cols_small <- names(timeVarModel_small)

    fixedData_train_small <- data_small %>%
      dplyr::group_by(subject_id) %>%
      dplyr::summarise(
        id = as.numeric(as.character(dplyr::first(subject_id))),
        age = dplyr::first(age),
        .groups = "drop"
      )

    Y_df_small <- data_small %>%
      dplyr::group_by(subject_id) %>%
      dplyr::summarise(
        id = as.numeric(as.character(dplyr::first(subject_id))),
        time = dplyr::first(hosp_time),
        event = as.numeric(dplyr::first(survival)),
        .groups = "drop"
      )
    Y_df_small$event <- ifelse(is.na(Y_df_small$event) | Y_df_small$event > 0, 1L, 0L)

    Y_small <- list(
      type = "surv",
      Y = data.frame(
        id = as.numeric(Y_df_small$id),
        time = as.numeric(Y_df_small$time),
        event = as.numeric(Y_df_small$event),
        stringsAsFactors = FALSE
      )
    )

    timeData_train_small <- data_small %>%
      dplyr::transmute(
        id = as.numeric(as.character(subject_id)),
        time = .data[["time"]],
        dplyr::across(dplyr::all_of(item_cols_small))
      )

    fixed_data_small <- fixedData_train_small %>%
      dplyr::left_join(Y_df_small %>% dplyr::select(id, time, event), by = "id")

    ## —— 写死：DynForest 超参数 ——
    dyn_model_small <- DynForest::dynforest(
      timeData      = as.data.frame(timeData_train_small),
      fixedData     = as.data.frame(fixedData_train_small),
      idVar         = "id",
      timeVar       = "time",
      timeVarModel  = timeVarModel_small,
      Y             = Y_small,
      ntree         = 50L,
      mtry          = 3L,
      nodesize      = 2L,
      minsplit      = 2L,
      nsplit_option = "quantile",
      ncores        = 4L,
      seed          = 1234L,
      verbose       = TRUE
    )

    ## —— 写死：t0 = 每人一行生存时间的中位数（hosp_time） ——
    t0 <- stats::median(fixed_data_small$time, na.rm = TRUE)

    surv_time_sm <- fixed_data_small$time
    surv_event_sm <- fixed_data_small$event
    n_sm <- length(surv_time_sm)

    risk_score_sm <- numeric(n_sm)
    valid_trees_sm <- 0L
    rf_sm <- dyn_model_small$rf

    if (is.list(rf_sm)) {
      for (tree_idx in seq_along(rf_sm)) {
        tree <- rf_sm[[tree_idx]]
        if (is.list(tree) && "leaf" %in% names(tree) && "leaf.info" %in% names(tree)) {
          leaf_ids <- tree$leaf
          leaf_info <- tree$leaf.info
          if (length(leaf_ids) == n_sm) {
            for (sample_idx in seq_len(n_sm)) {
              leaf_id <- leaf_ids[sample_idx]
              if (is.matrix(leaf_info)) {
                risk_score_sm[sample_idx] <- risk_score_sm[sample_idx] + leaf_info[leaf_id, 1]
              } else if (is.data.frame(leaf_info)) {
                risk_score_sm[sample_idx] <- risk_score_sm[sample_idx] + leaf_info[leaf_id, 1]
              } else if (is.list(leaf_info)) {
                risk_score_sm[sample_idx] <- risk_score_sm[sample_idx] + leaf_info[[leaf_id]][1]
              }
            }
            valid_trees_sm <- valid_trees_sm + 1L
          }
        }
      }
    }

    if (valid_trees_sm > 0L) {
      risk_score_sm <- risk_score_sm / valid_trees_sm
    } else {
      risk_score_sm <- as.numeric(fixed_data_small$age)
      risk_score_sm[is.na(risk_score_sm)] <- 0
    }

    cindex_sm <- survival::concordance(survival::Surv(surv_time_sm, surv_event_sm) ~ risk_score_sm)$concordance
    if (cindex_sm < 0.5) {
      risk_score_sm <- -risk_score_sm
      cindex_sm <- survival::concordance(survival::Surv(surv_time_sm, surv_event_sm) ~ risk_score_sm)$concordance
    }

    roc_sm <- timeROC::timeROC(
      T = surv_time_sm,
      delta = surv_event_sm,
      marker = risk_score_sm,
      cause = 1,
      times = t0
    )
    roc_sm_rev <- timeROC::timeROC(
      T = surv_time_sm,
      delta = surv_event_sm,
      marker = -risk_score_sm,
      cause = 1,
      times = t0
    )

    auc_sm <- if (length(roc_sm$AUC) >= 2L) {
      roc_sm$AUC[2]
    } else if (length(roc_sm$AUC) == 1L) {
      roc_sm$AUC[1]
    } else {
      0.5
    }
    auc_rev_sm <- if (length(roc_sm_rev$AUC) >= 2L) {
      roc_sm_rev$AUC[2]
    } else if (length(roc_sm_rev$AUC) == 1L) {
      roc_sm_rev$AUC[1]
    } else {
      0.5
    }
    auc_sm <- max(auc_sm, auc_rev_sm)

    hazard_sm <- exp(risk_score_sm)
    cum_hazard_sm <- hazard_sm * t0
    surv_prob_sm <- exp(-cum_hazard_sm)
    brier_sm <- numeric(n_sm)
    for (i in seq_len(n_sm)) {
      if (surv_time_sm[i] <= t0 && surv_event_sm[i] == 1L) {
        brier_sm[i] <- (1 - surv_prob_sm[i])^2
      } else if (surv_time_sm[i] > t0) {
        brier_sm[i] <- surv_prob_sm[i]^2
      } else {
        brier_sm[i] <- (1 - surv_prob_sm[i])^2
      }
    }
    bs_sm <- mean(brier_sm)

    out <- data.frame(
      AUC = round(auc_sm, 4),
      BS = round(bs_sm, 4),
      Cindex = round(cindex_sm, 4)
    )
    rownames(out) <- paste0("t0=", round(t0, 4))
    return(out)
  }, error = function(e) {
    message("DYNforest_LC failed: ", e$message)
    out <- data.frame(AUC = NA_real_, BS = NA_real_, Cindex = NA_real_)
    rownames(out) <- "error"
    return(out)
  })
}


test1 <- DYNforest_LC(data_0506, c("item_50912", "item_50971", "item_51221"))

###########
# 仅 8 次：按排序后 item_* 的名次切 3 个一组 ——
# 第1–7 次：v1–v3, v4–v6, …, v19–v21；第 8 次：v20–v22（与第 7 次在 v20、v21 上重叠）
# 需要至少 22 个 item_* 列，列名按 sort 后顺序作为 v1…v22
###########
item_all <- sort(grep("^item_", names(data_0506), value = TRUE))
if (length(item_all) < 22L) {
  stop("需要至少 22 个以 item_ 开头的列，当前: ", length(item_all))
}
idx_blocks <- list(
  c(1, 2, 3), c(4, 5, 6), c(7, 8, 9), c(10, 11, 12),
  c(13, 14, 15), c(16, 17, 18), c(19, 20, 21), c(20, 21, 22)
)
trios_list <- lapply(idx_blocks, function(ii) item_all[ii])

res_list <- vector("list", length(trios_list))
for (k in seq_along(trios_list)) {
  tr <- trios_list[[k]]
  message(k, "/", length(trios_list), "  →  ", paste(tr, collapse = ", "))
  r <- DYNforest_LC(data_0506, tr)
  rn <- rownames(r)
  rownames(r) <- NULL
  r$items <- paste(tr, collapse = " | ")
  r$item_1 <- tr[1]
  r$item_2 <- tr[2]
  r$item_3 <- tr[3]
  r$t0_row <- rn
  res_list[[k]] <- r
}
test1_all_item_trios <- do.call(rbind, res_list)
rownames(test1_all_item_trios) <- NULL
message("完成：共 ", nrow(test1_all_item_trios), " 行（8 组固定分块，每组 3 个轨迹 + AUC/BS/Cindex）")

###############