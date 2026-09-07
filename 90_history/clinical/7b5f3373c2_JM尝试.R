library(survival)
library(nlme)
library(JMbayes2)
library(dplyr)
library(timeROC)
library(readxl)

######################程序###########################
cal_3_JM <- function(data, t0 = 1) {
  tryCatch({
    stopifnot(is.data.frame(data))
    need <- c("ID", "t", "obs_time", "event", "V1", "V2", "V3", "V4", "V5", "V6")
    miss <- setdiff(need, names(data))
    if (length(miss) > 0) {
      stop("data 缺少必要列: ", paste(miss, collapse = ", "))
    }
    
    Dt <- 3  # 与老师代码 tvBrier(..., Tstart = 1, Dt = 3) 一致
    
    #----------------#
    # 1. 统一数据清理
    #----------------#
    data_clean <- data[data$t <= data$obs_time, , drop = FALSE]
    if (nrow(data_clean) == 0) stop("清理后无观测 (t <= obs_time)")
    
    #----------------#
    # 2. 纵向子模型（写死：V1–V6 ~ t，随机截距 | ID）
    #----------------#
    fm1 <- nlme::lme(V1 ~ t, data = data_clean, random = ~ 1 | ID)
    fm2 <- nlme::lme(V2 ~ t, data = data_clean, random = ~ 1 | ID)
    fm3 <- nlme::lme(V3 ~ t, data = data_clean, random = ~ 1 | ID)
    fm4 <- nlme::lme(V4 ~ t, data = data_clean, random = ~ 1 | ID)
    fm5 <- nlme::lme(V5 ~ t, data = data_clean, random = ~ 1 | ID)
    fm6 <- nlme::lme(V6 ~ t, data = data_clean, random = ~ 1 | ID)
    
    #----------------#
    # 3. 生存子模型（写死：~ 1）
    #----------------#
    survData <- data_clean[!duplicated(data_clean$ID), , drop = FALSE]
    coxFit <- survival::coxph(
      Surv(obs_time, event) ~ 1,
      data = survData,
      x = TRUE
    )
    
    #----------------#
    # 4. 联合模型 jm()
    #----------------#
    jmFit <- JMbayes2::jm(
      Surv_object = coxFit,
      Mixed_objects = list(fm1, fm2, fm3, fm4, fm5, fm6),
      time_var = "t"
    )
    
    #----------------#
    # 5. C-index（老师方法：event 过程预测风险，取每人最后时刻 risk）
    #----------------#
    pred <- predict(
      jmFit,
      newdata = data_clean,
      process = "event"
    )
    
    pred_patient <- data.frame(
      ID   = pred$id,
      time = pred$times,
      risk = pred$pred,
      stringsAsFactors = FALSE
    )
    
    risk_last <- pred_patient %>%
      dplyr::group_by(ID) %>%
      dplyr::summarise(risk = dplyr::last(risk), .groups = "drop")
    
    cdata <- merge(survData, risk_last, by = "ID")
    
    # predictor 是风险（越大风险越高），需 reverse = TRUE
    cindex_obj <- survival::concordance(
      Surv(obs_time, event) ~ risk,
      reverse = TRUE,
      data = cdata
    )
    Cindex <- as.numeric(cindex_obj$concordance)
    
    #----------------#
    # 6. AUC（JMbayes2::tvAUC；Tstart = t0, Dt = 3）
    #----------------#
    auc_obj <- JMbayes2::tvAUC(
      object  = jmFit,
      newdata = data_clean,
      Tstart  = t0,
      Dt      = Dt,
      cores   = 1L
    )
    AUC <- round(as.numeric(auc_obj$auc), 4)
    
    #----------------#
    # 7. BS（老师方法：tvBrier；Tstart = t0, Dt = 3）
    #----------------#
    bs_obj <- JMbayes2::tvBrier(
      object  = jmFit,
      newdata = data_clean,
      Tstart  = t0,
      Dt      = Dt,
      cores   = 1L
    )
    BS <- as.numeric(bs_obj$Brier)
    
    #----------------#
    # 8. 返回结果（列顺序与 cal_3 一致）
    #----------------#
    data.frame(
      AUC    = AUC,
      BS     = BS,
      Cindex = Cindex
    )
    
  }, error = function(e) {
    message("Model fitting failed: ", conditionMessage(e))
    data.frame(
      AUC    = NA_real_,
      BS     = NA_real_,
      Cindex = NA_real_
    )
  })
}

###############数据录入######################
# 每个情景只取 L1.xlsx 的第 1 个 sheet，存为同名 data.frame
# 共 24 个情景（5_Jointmodel_C1.R 中缺 sim1000_70_10V_highBTW，此处补全）

input_dir <- "F:/文章/大论文/程序Trae/模拟数据_添加Y"

data_names <- c(
  # low (8)
  "sim500_30_10V_lowINTER_1c_L1",
  "sim500_30_10V_lowBTW_1c_L1",
  "sim500_70_10V_lowINTER_1c_L1",
  "sim500_70_10V_lowBTW_1c_L1",
  "sim1000_30_10V_lowINTER_1c_L1",
  "sim1000_30_10V_lowBTW_1c_L1",
  "sim1000_70_10V_lowINTER_1c_L1",
  "sim1000_70_10V_lowBTW_1c_L1",
  # mid (8)
  "sim500_30_10V_midINTER_1c_L1",
  "sim500_30_10V_midBTW_1c_L1",
  "sim500_70_10V_midINTER_1c_L1",
  "sim500_70_10V_midBTW_1c_L1",
  "sim1000_30_10V_midINTER_1c_L1",
  "sim1000_30_10V_midBTW_1c_L1",
  "sim1000_70_10V_midINTER_1c_L1",
  "sim1000_70_10V_midBTW_1c_L1",
  # high (8)
  "sim500_30_10V_highINTER_1c_L1",
  "sim500_30_10V_highBTW_1c_L1",
  "sim500_70_10V_highINTER_1c_L1",
  "sim500_70_10V_highBTW_1c_L1",
  "sim1000_30_10V_highINTER_1c_L1",
  "sim1000_30_10V_highBTW_1c_L1",
  "sim1000_70_10V_highINTER_1c_L1",
  "sim1000_70_10V_highBTW_1c_L1"
)

stopifnot(length(data_names) == 24L)

for (nm in data_names) {
  fpath <- file.path(input_dir, paste0(nm, ".xlsx"))
  if (!file.exists(fpath)) {
    stop("文件不存在: ", fpath)
  }
  sheet1 <- excel_sheets(fpath)[1]
  df <- as.data.frame(read_xlsx(fpath, sheet = sheet1))
  assign(nm, df, envir = .GlobalEnv)
  message("已读入: ", nm, " | sheet = ", sheet1, " | nrow = ", nrow(df))
}

###############对 24 个数据框调用 cal_3_JM，并 rbind######################

t0 <- 1

res_list <- lapply(seq_along(data_names), function(i) {
  nm <- data_names[i]
  message(sprintf("【%s】(%d/%d) cal_3_JM: %s", Sys.time(), i, length(data_names), nm))
  cbind(
    dataset = nm,
    cal_3_JM(get(nm, envir = .GlobalEnv), t0 = t0)
  )
})

results_jm <- do.call(rbind, res_list)
rownames(results_jm) <- NULL

print(results_jm)
