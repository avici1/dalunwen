library(dplyr)
library(survival)
library(readxl)
library(writexl)
library(purrr)
set.seed(123)
options(scipen = 999)
options(pillar.width = Inf)

##########读取数据#################
# 纵向：变量子集搜索（_search_var_subset_4V_*）15 组中最佳为 V1+V2+V3+V4
# （单变量 jm 不支持仅随机截距；其余子集 AUC<=0.5018）
# 评估窗口写死：t0=0.5, Dt=1
t0 <- 0.5
Dt <- 1

input_dir <- "F:/文章/大论文/程序Trae/模拟数据_4V/生成Y"

###### 低相关 + 中相关 + 高相关（每个类型仅读 L1 第1个 sheet）#####
sim500_30_4V_lowINTER_1c_L1 <- map(setNames(excel_sheets(file.path(input_dir, "sim500_30_4V_lowINTER_1c_L1.xlsx"))[1], excel_sheets(file.path(input_dir, "sim500_30_4V_lowINTER_1c_L1.xlsx"))[1]), ~ read_xlsx(file.path(input_dir, "sim500_30_4V_lowINTER_1c_L1.xlsx"), sheet = .x))
sim500_70_4V_lowINTER_1c_L1 <- map(setNames(excel_sheets(file.path(input_dir, "sim500_70_4V_lowINTER_1c_L1.xlsx"))[1], excel_sheets(file.path(input_dir, "sim500_70_4V_lowINTER_1c_L1.xlsx"))[1]), ~ read_xlsx(file.path(input_dir, "sim500_70_4V_lowINTER_1c_L1.xlsx"), sheet = .x))
sim1000_30_4V_lowINTER_1c_L1 <- map(setNames(excel_sheets(file.path(input_dir, "sim1000_30_4V_lowINTER_1c_L1.xlsx"))[1], excel_sheets(file.path(input_dir, "sim1000_30_4V_lowINTER_1c_L1.xlsx"))[1]), ~ read_xlsx(file.path(input_dir, "sim1000_30_4V_lowINTER_1c_L1.xlsx"), sheet = .x))
sim1000_70_4V_lowINTER_1c_L1 <- map(setNames(excel_sheets(file.path(input_dir, "sim1000_70_4V_lowINTER_1c_L1.xlsx"))[1], excel_sheets(file.path(input_dir, "sim1000_70_4V_lowINTER_1c_L1.xlsx"))[1]), ~ read_xlsx(file.path(input_dir, "sim1000_70_4V_lowINTER_1c_L1.xlsx"), sheet = .x))
sim500_30_4V_midINTER_1c_L1 <- map(setNames(excel_sheets(file.path(input_dir, "sim500_30_4V_midINTER_1c_L1.xlsx"))[1], excel_sheets(file.path(input_dir, "sim500_30_4V_midINTER_1c_L1.xlsx"))[1]), ~ read_xlsx(file.path(input_dir, "sim500_30_4V_midINTER_1c_L1.xlsx"), sheet = .x))
sim500_70_4V_midINTER_1c_L1 <- map(setNames(excel_sheets(file.path(input_dir, "sim500_70_4V_midINTER_1c_L1.xlsx"))[1], excel_sheets(file.path(input_dir, "sim500_70_4V_midINTER_1c_L1.xlsx"))[1]), ~ read_xlsx(file.path(input_dir, "sim500_70_4V_midINTER_1c_L1.xlsx"), sheet = .x))
sim1000_30_4V_midINTER_1c_L1 <- map(setNames(excel_sheets(file.path(input_dir, "sim1000_30_4V_midINTER_1c_L1.xlsx"))[1], excel_sheets(file.path(input_dir, "sim1000_30_4V_midINTER_1c_L1.xlsx"))[1]), ~ read_xlsx(file.path(input_dir, "sim1000_30_4V_midINTER_1c_L1.xlsx"), sheet = .x))
sim1000_70_4V_midINTER_1c_L1 <- map(setNames(excel_sheets(file.path(input_dir, "sim1000_70_4V_midINTER_1c_L1.xlsx"))[1], excel_sheets(file.path(input_dir, "sim1000_70_4V_midINTER_1c_L1.xlsx"))[1]), ~ read_xlsx(file.path(input_dir, "sim1000_70_4V_midINTER_1c_L1.xlsx"), sheet = .x))
sim500_30_4V_highINTER_1c_L1 <- map(setNames(excel_sheets(file.path(input_dir, "sim500_30_4V_highINTER_1c_L1.xlsx"))[1], excel_sheets(file.path(input_dir, "sim500_30_4V_highINTER_1c_L1.xlsx"))[1]), ~ read_xlsx(file.path(input_dir, "sim500_30_4V_highINTER_1c_L1.xlsx"), sheet = .x))
sim500_70_4V_highINTER_1c_L1 <- map(setNames(excel_sheets(file.path(input_dir, "sim500_70_4V_highINTER_1c_L1.xlsx"))[1], excel_sheets(file.path(input_dir, "sim500_70_4V_highINTER_1c_L1.xlsx"))[1]), ~ read_xlsx(file.path(input_dir, "sim500_70_4V_highINTER_1c_L1.xlsx"), sheet = .x))
sim1000_30_4V_highINTER_1c_L1 <- map(setNames(excel_sheets(file.path(input_dir, "sim1000_30_4V_highINTER_1c_L1.xlsx"))[1], excel_sheets(file.path(input_dir, "sim1000_30_4V_highINTER_1c_L1.xlsx"))[1]), ~ read_xlsx(file.path(input_dir, "sim1000_30_4V_highINTER_1c_L1.xlsx"), sheet = .x))
sim1000_70_4V_highINTER_1c_L1 <- map(setNames(excel_sheets(file.path(input_dir, "sim1000_70_4V_highINTER_1c_L1.xlsx"))[1], excel_sheets(file.path(input_dir, "sim1000_70_4V_highINTER_1c_L1.xlsx"))[1]), ~ read_xlsx(file.path(input_dir, "sim1000_70_4V_highINTER_1c_L1.xlsx"), sheet = .x))



##########计算程序#############
# 4V 数据仅有 V1–V4（10V 为 V1–V6）
cal_3_JM_4V <- function(data, t0 = 0.5, Dt = 1) {
  t_start <- Sys.time()
  out <- tryCatch({
    stopifnot(is.data.frame(data))
    need <- c("ID", "t", "obs_time", "event", "V1", "V2", "V3", "V4")
    miss <- setdiff(need, names(data))
    if (length(miss) > 0) {
      stop("data 缺少必要列: ", paste(miss, collapse = ", "))
    }
    
    #----------------#
    # 1. 统一数据清理
    #----------------#
    data_clean <- data[data$t <= data$obs_time, , drop = FALSE]
    if (nrow(data_clean) == 0) stop("清理后无观测 (t <= obs_time)")
    
    #----------------#
    # 2. 纵向子模型（写死最佳变量集：V1–V4 ~ t，随机截距 | ID）
    #----------------#
    fm1 <- nlme::lme(V1 ~ t, data = data_clean, random = ~ 1 | ID)
    fm2 <- nlme::lme(V2 ~ t, data = data_clean, random = ~ 1 | ID)
    fm3 <- nlme::lme(V3 ~ t, data = data_clean, random = ~ 1 | ID)
    fm4 <- nlme::lme(V4 ~ t, data = data_clean, random = ~ 1 | ID)
    
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
      Mixed_objects = list(fm1, fm2, fm3, fm4),
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
    # 6–7. AUC / BS（写死：Tstart = t0, Dt；默认 0.5 / 1）
    #----------------#
    auc_obj <- JMbayes2::tvAUC(
      object  = jmFit,
      newdata = data_clean,
      Tstart  = t0,
      Dt      = Dt,
      cores   = 1L
    )
    AUC <- round(as.numeric(auc_obj$auc), 4)
    
    bs_obj <- JMbayes2::tvBrier(
      object  = jmFit,
      newdata = data_clean,
      Tstart  = t0,
      Dt      = Dt,
      cores   = 1L
    )
    BS <- as.numeric(bs_obj$Brier)
    
    #----------------#
    # 8. 返回结果
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
  elapsed <- as.numeric(difftime(Sys.time(), t_start, units = "secs"))
  message(sprintf("cal_3_JM_4V 运行时间: %.2f 秒", elapsed))
  out$Time <- elapsed
  out
}


########低相关性###########
t0 <- 0.5
Dt <- 1

result_sim500_30_4V_lowINTER_1c_L1 <- cal_3_JM_4V(sim500_30_4V_lowINTER_1c_L1[[1]], t0 = t0, Dt = Dt)
result_sim500_30_4V_lowINTER_1c_L1

result_sim500_70_4V_lowINTER_1c_L1 <- cal_3_JM_4V(sim500_70_4V_lowINTER_1c_L1[[1]], t0 = t0, Dt = Dt)
result_sim1000_30_4V_lowINTER_1c_L1 <- cal_3_JM_4V(sim1000_30_4V_lowINTER_1c_L1[[1]], t0 = t0, Dt = Dt)
result_sim1000_70_4V_lowINTER_1c_L1 <- cal_3_JM_4V(sim1000_70_4V_lowINTER_1c_L1[[1]], t0 = t0, Dt = Dt)


########中相关性###########
t0 <- 0.5
Dt <- 1

result_sim500_30_4V_midINTER_1c_L1 <- cal_3_JM_4V(sim500_30_4V_midINTER_1c_L1[[1]], t0 = t0, Dt = Dt)
result_sim500_70_4V_midINTER_1c_L1 <- cal_3_JM_4V(sim500_70_4V_midINTER_1c_L1[[1]], t0 = t0, Dt = Dt)
result_sim1000_30_4V_midINTER_1c_L1 <- cal_3_JM_4V(sim1000_30_4V_midINTER_1c_L1[[1]], t0 = t0, Dt = Dt)
result_sim1000_70_4V_midINTER_1c_L1 <- cal_3_JM_4V(sim1000_70_4V_midINTER_1c_L1[[1]], t0 = t0, Dt = Dt)


########高相关性###########
t0 <- 0.5
Dt <- 1

result_sim500_30_4V_highINTER_1c_L1 <- cal_3_JM_4V(sim500_30_4V_highINTER_1c_L1[[1]], t0 = t0, Dt = Dt)
result_sim500_70_4V_highINTER_1c_L1 <- cal_3_JM_4V(sim500_70_4V_highINTER_1c_L1[[1]], t0 = t0, Dt = Dt)
result_sim1000_30_4V_highINTER_1c_L1 <- cal_3_JM_4V(sim1000_30_4V_highINTER_1c_L1[[1]], t0 = t0, Dt = Dt)
result_sim1000_70_4V_highINTER_1c_L1 <- cal_3_JM_4V(sim1000_70_4V_highINTER_1c_L1[[1]], t0 = t0, Dt = Dt)


#########保存数据############
result_names <- c(
  "result_sim500_30_4V_lowINTER_1c_L1",
  "result_sim500_70_4V_lowINTER_1c_L1",
  "result_sim1000_30_4V_lowINTER_1c_L1",
  "result_sim1000_70_4V_lowINTER_1c_L1",
  "result_sim500_30_4V_midINTER_1c_L1",
  "result_sim500_70_4V_midINTER_1c_L1",
  "result_sim1000_30_4V_midINTER_1c_L1",
  "result_sim1000_70_4V_midINTER_1c_L1",
  "result_sim500_30_4V_highINTER_1c_L1",
  "result_sim500_70_4V_highINTER_1c_L1",
  "result_sim1000_30_4V_highINTER_1c_L1",
  "result_sim1000_70_4V_highINTER_1c_L1"
)

result_all_4V <- do.call(rbind, lapply(result_names, function(nm) {
  df <- get(nm)
  df$dataset <- sub("^result_", "", nm)
  df[, c("dataset", "AUC", "BS", "Cindex", "Time")]
}))

out_xlsx <- "F:/文章_大论文/0722/模拟研究代码/4Vresult.xlsx"
write_xlsx(result_all_4V, path = out_xlsx)
message("已保存: ", out_xlsx, " | nrow = ", nrow(result_all_4V))


























# ================================
# Debug 脚本：诊断 AUC 过低问题
# 使用数据集：sim500_30_4V_lowINTER_1c_L1[[1]]
# ================================

library(dplyr)
library(survival)
library(nlme)
library(JMbayes2)
set.seed(123)

# ---------- 1. 提取数据 ----------
data_raw <- sim500_30_4V_lowINTER_1c_L1[[1]]  # 请确认此对象存在
cat("\n========== 1. 数据基本信息 ==========\n")
cat("数据行数 (纵向观测):", nrow(data_raw), "\n")
cat("唯一 ID 数:", length(unique(data_raw$ID)), "\n")
cat("列名:", paste(names(data_raw), collapse = ", "), "\n")

# ---------- 2. 生存数据 (每个ID只保留一条) ----------
survData <- data_raw[!duplicated(data_raw$ID), c("ID", "obs_time", "event")]
cat("\n========== 2. 生存时间分布 ==========\n")
cat("事件总数:", sum(survData$event), "\n")
cat("删失总数:", sum(1 - survData$event), "\n")
cat("删失率 (%):", round(mean(1 - survData$event) * 100, 2), "\n")
cat("生存时间分位数 (全部):\n")
print(quantile(survData$obs_time, probs = c(0, 0.25, 0.5, 0.75, 1)))
cat("事件时间分位数 (仅事件):\n")
print(quantile(survData$obs_time[survData$event == 1], probs = c(0, 0.25, 0.5, 0.75, 1)))
cat("删失时间分位数 (仅删失):\n")
print(quantile(survData$obs_time[survData$event == 0], probs = c(0, 0.25, 0.5, 0.75, 1)))

# ---------- 3. 评估老师窗口 [1, 4] 的风险集信息 ----------
Tstart <- 1
Dt <- 3
window_end <- Tstart + Dt
risk_set <- survData[survData$obs_time > Tstart, ]  # 在Tstart时仍处于风险
cat("\n========== 3. 窗口 [", Tstart, ", ", window_end, "] 信息 ==========\n", sep = "")
cat("在 Tstart=", Tstart, "时仍处于风险 (obs_time > Tstart) 的个体数: ", nrow(risk_set), "\n", sep = "")
if (nrow(risk_set) > 0) {
  event_in_window <- sum(risk_set$obs_time <= window_end & risk_set$event == 1)
  cat("在窗口内发生事件的个体数: ", event_in_window, "\n")
  cat("在窗口内删失 (未事件) 的个体数: ", sum(risk_set$obs_time <= window_end & risk_set$event == 0), "\n")
  cat("在窗口结束后仍继续随访的个体数: ", sum(risk_set$obs_time > window_end), "\n")
  cat("窗口内事件比例 (占风险集): ", round(event_in_window / nrow(risk_set) * 100, 2), "%\n")
} else {
  cat("风险集为空！无法计算窗口内指标。\n")
}

# ---------- 4. 拟合联合模型 (与原代码一致) ----------
cat("\n========== 4. 拟合联合模型 ==========\n")
data_clean <- data_raw[data_raw$t <= data_raw$obs_time, ]
if (nrow(data_clean) == 0) stop("清理后无数据")
fm1 <- lme(V1 ~ t, data = data_clean, random = ~ 1 | ID)
fm2 <- lme(V2 ~ t, data = data_clean, random = ~ 1 | ID)
fm3 <- lme(V3 ~ t, data = data_clean, random = ~ 1 | ID)
fm4 <- lme(V4 ~ t, data = data_clean, random = ~ 1 | ID)
coxFit <- coxph(Surv(obs_time, event) ~ 1, data = survData, x = TRUE)
jmFit <- jm(coxFit, list(fm1, fm2, fm3, fm4), time_var = "t")
cat("联合模型拟合完成。\n")

# ---------- 5. 查看关联参数估计 ----------
cat("\n========== 5. 联合模型摘要 (关联参数部分) ==========\n")
summ <- summary(jmFit)
# 提取固定效应中关联参数 (通常为 "Value" 或 "Slope" 等)
assoc_names <- grep("Value|Slope|Assoct", rownames(summ$coefficients), value = TRUE, ignore.case = TRUE)
if (length(assoc_names) > 0) {
  cat("关联参数估计 (含标准误和 p 值):\n")
  print(summ$coefficients[assoc_names, , drop = FALSE])
} else {
  cat("未找到明显的关联参数名称，显示完整固定效应表:\n")
  print(summ$coefficients)
}

# ---------- 6. 使用正确参数重算 tvAUC 和 tvBrier ----------
cat("\n========== 6. 使用正确参数调用 tvAUC/tvBrier ==========\n")
# 使用显式指定 idVar 和 timeVar
tryCatch({
  auc_obj <- tvAUC(jmFit, newdata = data_clean, Tstart = Tstart, Dt = Dt,
                   idVar = "ID", timeVar = "t", cores = 1L)
  cat("tvAUC 成功!\n")
  print(auc_obj)
}, error = function(e) {
  cat("tvAUC 失败，错误信息:\n", conditionMessage(e), "\n")
})

tryCatch({
  bs_obj <- tvBrier(jmFit, newdata = data_clean, Tstart = Tstart, Dt = Dt,
                    idVar = "ID", timeVar = "t", cores = 1L)
  cat("tvBrier 成功!\n")
  print(bs_obj)
}, error = function(e) {
  cat("tvBrier 失败，错误信息:\n", conditionMessage(e), "\n")
})

# ---------- 7. 使用 JMbayes2 官方 cindex 重算 C-index ----------
cat("\n========== 7. 使用 JMbayes2::cindex 计算 C-index ==========\n")
tryCatch({
  cind <- cindex(jmFit, newdata = data_clean, idVar = "ID", timeVar = "t")
  cat("cindex 结果:\n")
  print(cind)
}, error = function(e) {
  cat("cindex 失败，错误信息:\n", conditionMessage(e), "\n")
})

# ---------- 8. 手动计算动态预测风险并评估 C-index (与原始方法一致) ----------
cat("\n========== 8. 手动 C-index (与原始代码一致) ==========\n")
pred <- predict(jmFit, newdata = data_clean, process = "event")
pred_df <- data.frame(ID = pred$id, time = pred$times, risk = pred$pred, stringsAsFactors = FALSE)
risk_last <- pred_df %>% group_by(ID) %>% summarise(risk = last(risk), .groups = "drop")
cdata <- merge(survData, risk_last, by = "ID")
conc_obj <- concordance(Surv(obs_time, event) ~ risk, reverse = TRUE, data = cdata)
cat("手动 C-index =", round(as.numeric(conc_obj$concordance), 4), "\n")
cat("手动 C-index 涉及样本数:", conc_obj$n, "\n")

cat("\n========== Debug 结束 ==========\n")