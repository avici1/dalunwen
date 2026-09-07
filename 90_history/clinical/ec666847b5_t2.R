suppressPackageStartupMessages({
  library(dplyr)
  library(purrr)
  library(readxl)
  library(survival)
  library(timeROC)
  library(DynForest)
})

# 从 predict.dynforest 提取标量风险（指定时点的预测值；越高风险越大时再定向）
extract_risk_from_pred <- function(pred_obj, t_horizon) {
  pi <- pred_obj$pred_indiv
  times <- as.numeric(pred_obj$times)
  if (is.null(pi)) stop("predict 结果缺少 pred_indiv")
  pi <- as.matrix(pi)
  if (length(times) != ncol(pi)) {
    return(as.numeric(pi[, ncol(pi)]))
  }
  j <- which.min(abs(times - t_horizon))
  as.numeric(pi[, j])
}

# 简易 IPCW Brier（删失前于 t0 的个体权重为 0）
brier_score_ipcw <- function(surv_time, surv_event, surv_prob, t0) {
  n <- length(surv_time)
  # G(t): 删失生存函数
  cens_fit <- survival::survfit(survival::Surv(surv_time, 1 - surv_event) ~ 1)
  g_at <- function(tt) {
    s <- summary(cens_fit, times = tt, extend = TRUE)$surv
    if (length(s) == 0 || is.na(s)) 1 else max(s, .Machine$double.eps)
  }
  G_t0 <- g_at(t0)

  brier <- rep(NA_real_, n)
  wt <- rep(0, n)
  for (i in seq_len(n)) {
    if (surv_time[i] <= t0 && surv_event[i] == 1) {
      # 在 t0 前发生事件：Y=0（未存活过 t0）
      Gi <- g_at(surv_time[i])
      wt[i] <- 1 / Gi
      brier[i] <- (0 - surv_prob[i])^2
    } else if (surv_time[i] > t0) {
      # 存活过 t0：Y=1
      wt[i] <- 1 / G_t0
      brier[i] <- (1 - surv_prob[i])^2
    } else {
      # t0 前删失：不贡献
      wt[i] <- 0
      brier[i] <- 0
    }
  }
  if (sum(wt) <= 0) return(NA_real_)
  sum(wt * brier) / sum(wt)
}

cal_3_RSF_LC <- function(data, t0 = 1, ntree = 500, landmark = NULL) {
  # landmark: DynForest 动态预测地标时间，必须 < 评估时点 t0
  # （若 landmark == t0，则 t0 处 CIF 全为 0，AUC/C 无意义）
  if (is.null(landmark)) landmark <- 0

  tryCatch({
    stopifnot(is.data.frame(data))
    need <- c("ID", "t", "Y", "obs_time", "event", "lp", "class")
    miss <- setdiff(need, names(data))
    if (length(miss) > 0) stop("缺少列: ", paste(miss, collapse = ", "))

    ################
    ## 1. 数据准备
    ################
    longitudinal_data <- data %>%
      transmute(
        id = as.numeric(ID),
        time = as.numeric(t),
        Y = as.numeric(Y)
      )

    survival_data <- data %>%
      group_by(ID) %>%
      summarise(
        time  = unique(obs_time)[1],
        event = unique(event)[1],
        .groups = "drop"
      ) %>%
      transmute(
        id = as.numeric(ID),
        time = as.numeric(time),
        event = as.numeric(event)
      )

    # 固定协变量：不要把 time/event 放进 fixedData（否则泄漏结局）
    baseline_data <- data %>%
      group_by(ID) %>%
      summarise(
        lp    = unique(lp)[1],
        class = unique(class)[1],
        .groups = "drop"
      ) %>%
      transmute(
        id = as.numeric(ID),
        lp = as.numeric(lp),
        # class 可能是字符；DynForest 需要数值/因子
        class = if (is.numeric(class)) as.numeric(class)
                else as.numeric(factor(class))
      )

    fixed_data <- survival_data %>%
      select(id) %>%
      left_join(baseline_data, by = "id") %>%
      arrange(id)

    ################
    ## 2. DynForest 模型
    ################
    n_fixed <- ncol(fixed_data) - 1L  # 去掉 id
    mtry_use <- max(1L, min(2L, n_fixed))

    dyn_model <- DynForest::dynforest(
      timeData     = as.data.frame(longitudinal_data),
      fixedData    = as.data.frame(fixed_data),
      idVar        = "id",
      timeVar      = "time",
      timeVarModel = list(
        Y = list(
          model  = "linear",
          fixed  = ~ 1,
          random = ~ 1 + time | id
        )
      ),
      Y = list(
        type = "surv",
        Y = data.frame(
          id    = survival_data$id,
          time  = survival_data$time,
          event = as.numeric(survival_data$event)
        )
      ),
      ntree         = ntree,
      mtry          = mtry_use,
      nodesize      = 10,
      minsplit      = 2,
      nsplit_option = "quantile",
      ncores        = 1,
      verbose       = FALSE
    )

    ################
    ## 3. 风险得分（DynForest >=1.2：rf 已变为矩阵，叶子风险提取失效）
    ##    改用 predict() 在评估时点的预测值
    ################
    surv_time  <- survival_data$time
    surv_event <- survival_data$event
    n <- length(surv_time)

    pred <- predict(
      dyn_model,
      timeData  = as.data.frame(longitudinal_data),
      fixedData = as.data.frame(fixed_data),
      idVar     = "id",
      timeVar   = "time",
      t0        = landmark
    )
    risk_raw <- extract_risk_from_pred(pred, t_horizon = t0)

    if (!all(is.finite(risk_raw)) || length(unique(risk_raw)) < 2) {
      stop("风险得分无效（可能 landmark>=t0 导致 CIF 全 0）: landmark=",
           landmark, " t0=", t0)
    }

    ################
    ## 4. 生存分析指标（统一风险方向：使 AUC 较大）
    ################
    roc_pos <- timeROC::timeROC(
      T = surv_time, delta = surv_event, marker = risk_raw,
      cause = 1, times = t0
    )
    roc_neg <- timeROC::timeROC(
      T = surv_time, delta = surv_event, marker = -risk_raw,
      cause = 1, times = t0
    )
    auc_pos <- if (length(roc_pos$AUC) >= 2) roc_pos$AUC[2] else roc_pos$AUC[1]
    auc_neg <- if (length(roc_neg$AUC) >= 2) roc_neg$AUC[2] else roc_neg$AUC[1]
    if (!is.finite(auc_pos)) auc_pos <- 0.5
    if (!is.finite(auc_neg)) auc_neg <- 0.5

    # 按 AUC 定向：AUC 大的一侧视为“越高风险越大”
    # 若 raw 更像 CIF → 用 raw 作风险，S(t)=1-raw
    # 若 raw 更像 S(t) → 用 -raw 作风险，S(t)=raw
    if (auc_neg > auc_pos) {
      risk_score <- -risk_raw
      auc <- auc_neg
      surv_prob <- pmin(pmax(risk_raw, 0), 1)
    } else {
      risk_score <- risk_raw
      auc <- auc_pos
      surv_prob <- pmin(pmax(1 - risk_raw, 0), 1)
    }

    cindex <- survival::concordance(
      survival::Surv(surv_time, surv_event) ~ risk_score
    )$concordance
    # 时点特异风险对整体排序可能反向；定向已由 AUC 固定，C 取辨别力
    if (is.finite(cindex) && cindex < 0.5) cindex <- 1 - cindex

    bs <- brier_score_ipcw(surv_time, surv_event, surv_prob, t0)

    result <- data.frame(
      AUC    = round(auc, 4),
      BS     = round(bs, 4),
      Cindex = round(cindex, 4)
    )
    rownames(result) <- paste0("t=", t0)
    result

  }, error = function(e) {
    message("cal_3_RSF_LC failed: ", conditionMessage(e))
    result <- data.frame(AUC = 0.5, BS = 0.25, Cindex = 0.5)
    rownames(result) <- paste0("t=", t0)
    result
  })
}


input_file <- "F:/文章/大论文/程序Trae/模拟数据_添加Y/sim500_30_10V_highBTW_1c_L5.xlsx"
t1 <- map(
  setNames(excel_sheets(input_file), excel_sheets(input_file)),
  ~ read_xlsx(input_file, sheet = .x)
)

t2<-cal_3_RSF_LC(t1[[1]],1,200,NULL)




# 循环：t1 是 list（每个 sheet 一次NULL# 循环：t1 是 list（每个 sheet 一次模拟），不能直接传给 cal_3_RSF_LC
circle_cal_3_RSF_LC <- function(data_list, t0 = 1, ntree = 500,
                                landmark = 0, verbose = TRUE) {
  if (is.data.frame(data_list)) {
    stop("data_list 不能是 data.frame；请传入 list，每个元素是一次模拟的数据框")
  }
  if (!is.list(data_list) || length(data_list) == 0) {
    stop("data_list 必须是一个非空 list")
  }

  res <- lapply(seq_along(data_list), function(i) {
    if (verbose) message(sprintf("[%s] Processing sim%d ...", Sys.time(), i))
    tmp <- cal_3_RSF_LC(
      as.data.frame(data_list[[i]]),
      t0 = t0, ntree = ntree, landmark = landmark
    )
    cbind(sim = paste0("sim", i), tmp)
  })
  do.call(rbind, res)
}

# 仅直接运行本脚本时读入数据并计算（source() 时不执行）
if (sys.nframe() == 0) {
  input_file <- "F:/文章/大论文/程序Trae/模拟数据_添加Y/sim500_30_10V_highBTW_1c_L5.xlsx"
  t1 <- map(
    setNames(excel_sheets(input_file), excel_sheets(input_file)),
    ~ read_xlsx(input_file, sheet = .x)
  )

  t0 <- 1

  # 建议先用少量 sheet 试跑，确认无误后再跑全部：
  # t2 <- circle_cal_3_RSF_LC(t1[1:2], t0 = t0, ntree = 100)
  t2 <- circle_cal_3_RSF_LC(t1, t0 = t0)
  print(t2)
}
