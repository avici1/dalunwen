library(readxl)
library(survival)
library(timeROC)
library(randomForestSRC)
library(dplyr)



############数据录入##################







# 定位 模拟数据_测试/10V_3C（本脚本在 模拟数据_测试/新代码/）
get_script_dir <- function() {
  args <- commandArgs(FALSE)
  f <- grep("^--file=", args, value = TRUE)
  if (length(f)) {
    return(dirname(normalizePath(sub("^--file=", "", f[1]))))
  }
  if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
    p <- rstudioapi::getActiveDocumentContext()$path
    if (nzchar(p)) {
      return(dirname(normalizePath(p)))
    }
  }
  getwd()
}

data_dir <- normalizePath(file.path(get_script_dir(), "..", "10V_3C"), mustWork = TRUE)

xlsx_paths <- sort(list.files(
  data_dir,
  pattern = "\\.[xX][lL][sS][xX]$",
  full.names = TRUE
))
stopifnot(length(xlsx_paths) > 0)

# 每个 xlsx 对应环境中的一个数据框，对象名 = 文件名（含扩展名，与 basename 一致）
for (i in seq_along(xlsx_paths)) {
  nm <- basename(xlsx_paths[i])
  assign(nm, read_xlsx(xlsx_paths[i]), envir = .GlobalEnv)
}


#########程序#####################

# data_t / data_v：合并多份模拟表时，若各表 ID 均从 1 起编号，须先 mutate(ID = paste(来源, ID)) 等，否则
# !duplicated(ID) 只会保留第一份表里的受试者，L2–L4 会被整表丢弃，样本量远小于预期、指标易接近随机。
modeling <- function(data_t, data_v, t0 = 1) {
  prep <- function(data) {
    surv <- data[!duplicated(data$ID), ]
    vcols <- grep("^V[0-9]+$", names(surv), value = TRUE)
    vcols <- vcols[order(as.integer(sub("^V", "", vcols)))]
    rsf <- surv[, c(vcols, "obs_time", "event"), drop = FALSE]
    list(
      rsf_data = rsf,
      surv_time = surv$obs_time,
      surv_status = surv$event
    )
  }

  tr <- prep(data_t)
  va <- prep(data_v)

  p <- length(grep("^V[0-9]+$", names(tr$rsf_data), value = TRUE))
  rfsrc_fit <- rfsrc(
    Surv(obs_time, event) ~ .,
    data = tr$rsf_data,
    ntree      = 1000,
    mtry       = max(1L, floor(p / 3)),
    nodesize   = 10,
    importance = TRUE,
    proximity  = FALSE,
    seed       = 123
  )

  pred <- predict(rfsrc_fit, newdata = va$rsf_data)
  time_points <- pred$time.interest
  t0_idx <- which.min(abs(time_points - t0))
  survival_probs <- pred$survival[, t0_idx]

  risk_marker <- 1 - survival_probs

  roc_obj <- timeROC(
    T = va$surv_time,
    delta = va$surv_status,
    marker = risk_marker,
    cause = 1,
    times = t0
  )
  AUC <- round(roc_obj$AUC[2], 4)

  n <- length(va$surv_time)
  idx <- combn(n, 2)
  i <- idx[1, ]
  j <- idx[2, ]
  comparable_ij <- (va$surv_status[i] == 1 & va$surv_time[i] < va$surv_time[j])
  comparable_ji <- (va$surv_status[j] == 1 & va$surv_time[j] < va$surv_time[i])
  comparable <- comparable_ij | comparable_ji
  concordant <- (comparable_ij & (risk_marker[i] > risk_marker[j])) |
    (comparable_ji & (risk_marker[j] > risk_marker[i]))
  tied <- (comparable & (risk_marker[i] == risk_marker[j]))
  n_pairs <- sum(comparable)
  n_concordant <- sum(concordant)
  n_tied <- sum(tied)
  Cindex <- ifelse(n_pairs > 0, (n_concordant + 0.5 * n_tied) / n_pairs, NA_real_)

  Y_obs <- as.numeric(va$surv_time > t0 | (va$surv_time <= t0 & va$surv_status == 0))
  censoring_model <- survfit(Surv(obs_time, 1 - event) ~ 1, data = va$rsf_data)
  get_weights <- function(time, event, censoring_model, t0) {
    cens_probs <- summary(censoring_model, times = pmin(time, t0))$surv
    weights <- ifelse(time <= t0 & event == 1, 1 / cens_probs,
      ifelse(time > t0, 1 / summary(censoring_model, times = t0)$surv, 0))
    weights
  }
  weights <- get_weights(va$surv_time, va$surv_status, censoring_model, t0)
  BS <- mean(weights * (survival_probs - Y_obs)^2, na.rm = TRUE)

  data.frame(Cindex = Cindex, AUC = AUC, BS = BS)
}

##################################
# 应用示例：L1–L4 训练，L5 验证（须先运行上文 read 循环；对象名 = 文件名，区分大小写，此处为 3c）
# 合并训练集时为每个 list 的 ID 加前缀，避免不同 xlsx 里重复的 1…500 被 dedupe 掉只剩 L1
data_t <- bind_rows(
  `1_sim500_30_10V_lowINTER_3c_L1.xlsx` %>% mutate(ID = paste("L1", ID, sep = "_")),
  `1_sim500_30_10V_lowINTER_3c_L2.xlsx` %>% mutate(ID = paste("L2", ID, sep = "_")),
  `1_sim500_30_10V_lowINTER_3c_L3.xlsx` %>% mutate(ID = paste("L3", ID, sep = "_")),
  `1_sim500_30_10V_lowINTER_3c_L4.xlsx` %>% mutate(ID = paste("L4", ID, sep = "_"))
)
data_v <- `1_sim500_30_10V_lowINTER_3c_L5.xlsx`
ex_lowINTER <- modeling(data_t, data_v, t0 = 1)




