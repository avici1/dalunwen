library(survival)
library(nlme)
library(JMbayes2)
library(dplyr)
library(timeROC)

#============================================================
# cal_3_JM
# 功能对齐 t1.R 中的 cal_3：
#   输入：模拟数据框 data + 地标时间 t0
#   内部：按老师 JMBayes-20260723.R 的 JMbayes2 逻辑拟合写死模型
#   输出：AUC / BS / Cindex
#
# 数据需含列：ID, t, obs_time, event, V1, V2, V3, V4, V5, V6
# t0 对应 tvAUC / tvBrier 的 Tstart；预测窗长度 Dt 写死为 3（与老师代码一致）
#============================================================
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
    #    老师原稿未写 AUC；与同包 tvBrier 对齐使用官方时依 AUC
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


#============================================================
# 单次调用示例（改 input_file 为你的模拟数据路径后直接跑）
#============================================================
 library(readxl)
 library(purrr)
input_file <- "F:/文章/大论文/程序Trae/模拟数据_添加Y/sim500_30_10V_highBTW_1c_L5.xlsx"
t1 <- map(
  setNames(excel_sheets(input_file), excel_sheets(input_file)),
  ~ read_xlsx(input_file, sheet = .x)
)

 # 对某一个 sheet（一次模拟）调用
 res <- cal_3_JM(t1[[1]], t0 = 1) 
 print(res)
