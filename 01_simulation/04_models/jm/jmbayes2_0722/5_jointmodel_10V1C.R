library(dplyr)
library(survival)
library(readxl)
library(writexl)
library(purrr)
library(JMbayes2)
library(parallel)
set.seed(123)
options(scipen = 999)
options(pillar.width = Inf)

#########多核心计算#######
ncores <- 8L
options(mc.cores = ncores)

##########读取数据#################
t0 <- 1  # 老师 tvBrier/tvAUC：Tstart = 1

input_dir <- "F:/文章/大论文/程序Trae/模拟数据_添加Y"

###### 低相关性数据（每个类型仅读 L1 第1个 sheet）#####
sim500_30_10V_lowINTER_1c_L1 <- map(setNames(excel_sheets(file.path(input_dir, "sim500_30_10V_lowINTER_1c_L1.xlsx"))[1], excel_sheets(file.path(input_dir, "sim500_30_10V_lowINTER_1c_L1.xlsx"))[1]), ~ read_xlsx(file.path(input_dir, "sim500_30_10V_lowINTER_1c_L1.xlsx"), sheet = .x))
sim500_30_10V_lowBTW_1c_L1 <- map(setNames(excel_sheets(file.path(input_dir, "sim500_30_10V_lowBTW_1c_L1.xlsx"))[1], excel_sheets(file.path(input_dir, "sim500_30_10V_lowBTW_1c_L1.xlsx"))[1]), ~ read_xlsx(file.path(input_dir, "sim500_30_10V_lowBTW_1c_L1.xlsx"), sheet = .x))
sim500_70_10V_lowINTER_1c_L1 <- map(setNames(excel_sheets(file.path(input_dir, "sim500_70_10V_lowINTER_1c_L1.xlsx"))[1], excel_sheets(file.path(input_dir, "sim500_70_10V_lowINTER_1c_L1.xlsx"))[1]), ~ read_xlsx(file.path(input_dir, "sim500_70_10V_lowINTER_1c_L1.xlsx"), sheet = .x))
sim500_70_10V_lowBTW_1c_L1 <- map(setNames(excel_sheets(file.path(input_dir, "sim500_70_10V_lowBTW_1c_L1.xlsx"))[1], excel_sheets(file.path(input_dir, "sim500_70_10V_lowBTW_1c_L1.xlsx"))[1]), ~ read_xlsx(file.path(input_dir, "sim500_70_10V_lowBTW_1c_L1.xlsx"), sheet = .x))
sim1000_30_10V_lowINTER_1c_L1 <- map(setNames(excel_sheets(file.path(input_dir, "sim1000_30_10V_lowINTER_1c_L1.xlsx"))[1], excel_sheets(file.path(input_dir, "sim1000_30_10V_lowINTER_1c_L1.xlsx"))[1]), ~ read_xlsx(file.path(input_dir, "sim1000_30_10V_lowINTER_1c_L1.xlsx"), sheet = .x))
sim1000_30_10V_lowBTW_1c_L1 <- map(setNames(excel_sheets(file.path(input_dir, "sim1000_30_10V_lowBTW_1c_L1.xlsx"))[1], excel_sheets(file.path(input_dir, "sim1000_30_10V_lowBTW_1c_L1.xlsx"))[1]), ~ read_xlsx(file.path(input_dir, "sim1000_30_10V_lowBTW_1c_L1.xlsx"), sheet = .x))
sim1000_70_10V_lowINTER_1c_L1 <- map(setNames(excel_sheets(file.path(input_dir, "sim1000_70_10V_lowINTER_1c_L1.xlsx"))[1], excel_sheets(file.path(input_dir, "sim1000_70_10V_lowINTER_1c_L1.xlsx"))[1]), ~ read_xlsx(file.path(input_dir, "sim1000_70_10V_lowINTER_1c_L1.xlsx"), sheet = .x))
sim1000_70_10V_lowBTW_1c_L1 <- map(setNames(excel_sheets(file.path(input_dir, "sim1000_70_10V_lowBTW_1c_L1.xlsx"))[1], excel_sheets(file.path(input_dir, "sim1000_70_10V_lowBTW_1c_L1.xlsx"))[1]), ~ read_xlsx(file.path(input_dir, "sim1000_70_10V_lowBTW_1c_L1.xlsx"), sheet = .x))

###### 中相关性数据（每个类型仅读 L1 第1个 sheet）#####
sim500_30_10V_midINTER_1c_L1 <- map(setNames(excel_sheets(file.path(input_dir, "sim500_30_10V_midINTER_1c_L1.xlsx"))[1], excel_sheets(file.path(input_dir, "sim500_30_10V_midINTER_1c_L1.xlsx"))[1]), ~ read_xlsx(file.path(input_dir, "sim500_30_10V_midINTER_1c_L1.xlsx"), sheet = .x))
sim500_30_10V_midBTW_1c_L1 <- map(setNames(excel_sheets(file.path(input_dir, "sim500_30_10V_midBTW_1c_L1.xlsx"))[1], excel_sheets(file.path(input_dir, "sim500_30_10V_midBTW_1c_L1.xlsx"))[1]), ~ read_xlsx(file.path(input_dir, "sim500_30_10V_midBTW_1c_L1.xlsx"), sheet = .x))
sim500_70_10V_midINTER_1c_L1 <- map(setNames(excel_sheets(file.path(input_dir, "sim500_70_10V_midINTER_1c_L1.xlsx"))[1], excel_sheets(file.path(input_dir, "sim500_70_10V_midINTER_1c_L1.xlsx"))[1]), ~ read_xlsx(file.path(input_dir, "sim500_70_10V_midINTER_1c_L1.xlsx"), sheet = .x))
sim500_70_10V_midBTW_1c_L1 <- map(setNames(excel_sheets(file.path(input_dir, "sim500_70_10V_midBTW_1c_L1.xlsx"))[1], excel_sheets(file.path(input_dir, "sim500_70_10V_midBTW_1c_L1.xlsx"))[1]), ~ read_xlsx(file.path(input_dir, "sim500_70_10V_midBTW_1c_L1.xlsx"), sheet = .x))
sim1000_30_10V_midINTER_1c_L1 <- map(setNames(excel_sheets(file.path(input_dir, "sim1000_30_10V_midINTER_1c_L1.xlsx"))[1], excel_sheets(file.path(input_dir, "sim1000_30_10V_midINTER_1c_L1.xlsx"))[1]), ~ read_xlsx(file.path(input_dir, "sim1000_30_10V_midINTER_1c_L1.xlsx"), sheet = .x))
sim1000_30_10V_midBTW_1c_L1 <- map(setNames(excel_sheets(file.path(input_dir, "sim1000_30_10V_midBTW_1c_L1.xlsx"))[1], excel_sheets(file.path(input_dir, "sim1000_30_10V_midBTW_1c_L1.xlsx"))[1]), ~ read_xlsx(file.path(input_dir, "sim1000_30_10V_midBTW_1c_L1.xlsx"), sheet = .x))
sim1000_70_10V_midINTER_1c_L1 <- map(setNames(excel_sheets(file.path(input_dir, "sim1000_70_10V_midINTER_1c_L1.xlsx"))[1], excel_sheets(file.path(input_dir, "sim1000_70_10V_midINTER_1c_L1.xlsx"))[1]), ~ read_xlsx(file.path(input_dir, "sim1000_70_10V_midINTER_1c_L1.xlsx"), sheet = .x))
sim1000_70_10V_midBTW_1c_L1 <- map(setNames(excel_sheets(file.path(input_dir, "sim1000_70_10V_midBTW_1c_L1.xlsx"))[1], excel_sheets(file.path(input_dir, "sim1000_70_10V_midBTW_1c_L1.xlsx"))[1]), ~ read_xlsx(file.path(input_dir, "sim1000_70_10V_midBTW_1c_L1.xlsx"), sheet = .x))

###### 高相关性数据（每个类型仅读 L1 第1个 sheet）#####
sim500_30_10V_highINTER_1c_L1 <- map(setNames(excel_sheets(file.path(input_dir, "sim500_30_10V_highINTER_1c_L1.xlsx"))[1], excel_sheets(file.path(input_dir, "sim500_30_10V_highINTER_1c_L1.xlsx"))[1]), ~ read_xlsx(file.path(input_dir, "sim500_30_10V_highINTER_1c_L1.xlsx"), sheet = .x))
sim500_30_10V_highBTW_1c_L1 <- map(setNames(excel_sheets(file.path(input_dir, "sim500_30_10V_highBTW_1c_L1.xlsx"))[1], excel_sheets(file.path(input_dir, "sim500_30_10V_highBTW_1c_L1.xlsx"))[1]), ~ read_xlsx(file.path(input_dir, "sim500_30_10V_highBTW_1c_L1.xlsx"), sheet = .x))
sim500_70_10V_highINTER_1c_L1 <- map(setNames(excel_sheets(file.path(input_dir, "sim500_70_10V_highINTER_1c_L1.xlsx"))[1], excel_sheets(file.path(input_dir, "sim500_70_10V_highINTER_1c_L1.xlsx"))[1]), ~ read_xlsx(file.path(input_dir, "sim500_70_10V_highINTER_1c_L1.xlsx"), sheet = .x))
sim500_70_10V_highBTW_1c_L1 <- map(setNames(excel_sheets(file.path(input_dir, "sim500_70_10V_highBTW_1c_L1.xlsx"))[1], excel_sheets(file.path(input_dir, "sim500_70_10V_highBTW_1c_L1.xlsx"))[1]), ~ read_xlsx(file.path(input_dir, "sim500_70_10V_highBTW_1c_L1.xlsx"), sheet = .x))
sim1000_30_10V_highINTER_1c_L1 <- map(setNames(excel_sheets(file.path(input_dir, "sim1000_30_10V_highINTER_1c_L1.xlsx"))[1], excel_sheets(file.path(input_dir, "sim1000_30_10V_highINTER_1c_L1.xlsx"))[1]), ~ read_xlsx(file.path(input_dir, "sim1000_30_10V_highINTER_1c_L1.xlsx"), sheet = .x))
sim1000_30_10V_highBTW_1c_L1 <- map(setNames(excel_sheets(file.path(input_dir, "sim1000_30_10V_highBTW_1c_L1.xlsx"))[1], excel_sheets(file.path(input_dir, "sim1000_30_10V_highBTW_1c_L1.xlsx"))[1]), ~ read_xlsx(file.path(input_dir, "sim1000_30_10V_highBTW_1c_L1.xlsx"), sheet = .x))
sim1000_70_10V_highINTER_1c_L1 <- map(setNames(excel_sheets(file.path(input_dir, "sim1000_70_10V_highINTER_1c_L1.xlsx"))[1], excel_sheets(file.path(input_dir, "sim1000_70_10V_highINTER_1c_L1.xlsx"))[1]), ~ read_xlsx(file.path(input_dir, "sim1000_70_10V_highINTER_1c_L1.xlsx"), sheet = .x))
sim1000_70_10V_highBTW_1c_L1 <- map(setNames(excel_sheets(file.path(input_dir, "sim1000_70_10V_highBTW_1c_L1.xlsx"))[1], excel_sheets(file.path(input_dir, "sim1000_70_10V_highBTW_1c_L1.xlsx"))[1]), ~ read_xlsx(file.path(input_dir, "sim1000_70_10V_highBTW_1c_L1.xlsx"), sheet = .x))

##########计算程序#############
cal_3_JM <- function(data, t0 = 1, Dt = 3) {
  t_start <- Sys.time()
  out <- tryCatch({
    stopifnot(is.data.frame(data))
    need <- c("ID", "t", "obs_time", "event", "V1", "V2", "V3", "V4", "V5", "V6")
    miss <- setdiff(need, names(data))
    if (length(miss) > 0) {
      stop("data 缺少必要列: ", paste(miss, collapse = ", "))
    }
    
    # Dt 默认 3：与老师 tvBrier(..., Tstart = 1, Dt = 3) 一致
    
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
    # 6–7. AUC / BS（写死与老师一致：Tstart = t0, Dt = 3）
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
  message(sprintf("cal_3_JM 运行时间: %.2f 秒", elapsed))
  out$Time <- elapsed
  out
}


########低相关性###########
t0 <- 1

result_sim500_30_10V_lowINTER_1c_L1 <- cal_3_JM(sim500_30_10V_lowINTER_1c_L1[[1]], t0 = t0)
result_sim500_30_10V_lowBTW_1c_L1 <- cal_3_JM(sim500_30_10V_lowBTW_1c_L1[[1]], t0 = t0)
result_sim500_70_10V_lowINTER_1c_L1 <- cal_3_JM(sim500_70_10V_lowINTER_1c_L1[[1]], t0 = t0)
result_sim500_70_10V_lowBTW_1c_L1 <- cal_3_JM(sim500_70_10V_lowBTW_1c_L1[[1]], t0 = t0)
result_sim1000_30_10V_lowINTER_1c_L1 <- cal_3_JM(sim1000_30_10V_lowINTER_1c_L1[[1]], t0 = t0)
result_sim1000_30_10V_lowBTW_1c_L1 <- cal_3_JM(sim1000_30_10V_lowBTW_1c_L1[[1]], t0 = t0)
result_sim1000_70_10V_lowINTER_1c_L1 <- cal_3_JM(sim1000_70_10V_lowINTER_1c_L1[[1]], t0 = t0)
result_sim1000_70_10V_lowBTW_1c_L1 <- cal_3_JM(sim1000_70_10V_lowBTW_1c_L1[[1]], t0 = t0)


########中相关性###########
t0 <- 1

result_sim500_30_10V_midINTER_1c_L1 <- cal_3_JM(sim500_30_10V_midINTER_1c_L1[[1]], t0 = t0)
result_sim500_30_10V_midBTW_1c_L1 <- cal_3_JM(sim500_30_10V_midBTW_1c_L1[[1]], t0 = t0)
result_sim500_70_10V_midINTER_1c_L1 <- cal_3_JM(sim500_70_10V_midINTER_1c_L1[[1]], t0 = t0)
result_sim500_70_10V_midBTW_1c_L1 <- cal_3_JM(sim500_70_10V_midBTW_1c_L1[[1]], t0 = t0)
result_sim1000_30_10V_midINTER_1c_L1 <- cal_3_JM(sim1000_30_10V_midINTER_1c_L1[[1]], t0 = t0)
result_sim1000_30_10V_midBTW_1c_L1 <- cal_3_JM(sim1000_30_10V_midBTW_1c_L1[[1]], t0 = t0)
result_sim1000_70_10V_midINTER_1c_L1 <- cal_3_JM(sim1000_70_10V_midINTER_1c_L1[[1]], t0 = t0)
result_sim1000_70_10V_midBTW_1c_L1 <- cal_3_JM(sim1000_70_10V_midBTW_1c_L1[[1]], t0 = t0)


########高相关性###########
t0 <- 1

result_sim500_30_10V_highINTER_1c_L1 <- cal_3_JM(sim500_30_10V_highINTER_1c_L1[[1]], t0 = t0)
result_sim500_30_10V_highBTW_1c_L1 <- cal_3_JM(sim500_30_10V_highBTW_1c_L1[[1]], t0 = t0)
result_sim500_70_10V_highINTER_1c_L1 <- cal_3_JM(sim500_70_10V_highINTER_1c_L1[[1]], t0 = t0)
result_sim500_70_10V_highBTW_1c_L1 <- cal_3_JM(sim500_70_10V_highBTW_1c_L1[[1]], t0 = t0)
result_sim1000_30_10V_highINTER_1c_L1 <- cal_3_JM(sim1000_30_10V_highINTER_1c_L1[[1]], t0 = t0)
result_sim1000_30_10V_highBTW_1c_L1 <- cal_3_JM(sim1000_30_10V_highBTW_1c_L1[[1]], t0 = t0)
result_sim1000_70_10V_highINTER_1c_L1 <- cal_3_JM(sim1000_70_10V_highINTER_1c_L1[[1]], t0 = t0)
result_sim1000_70_10V_highBTW_1c_L1 <- cal_3_JM(sim1000_70_10V_highBTW_1c_L1[[1]], t0 = t0)


##############保存数据#################
result_names <- c(
  "result_sim500_30_10V_lowINTER_1c_L1",
  "result_sim500_30_10V_lowBTW_1c_L1",
  "result_sim500_70_10V_lowINTER_1c_L1",
  "result_sim500_70_10V_lowBTW_1c_L1",
  "result_sim1000_30_10V_lowINTER_1c_L1",
  "result_sim1000_30_10V_lowBTW_1c_L1",
  "result_sim1000_70_10V_lowINTER_1c_L1",
  "result_sim1000_70_10V_lowBTW_1c_L1",
  "result_sim500_30_10V_midINTER_1c_L1",
  "result_sim500_30_10V_midBTW_1c_L1",
  "result_sim500_70_10V_midINTER_1c_L1",
  "result_sim500_70_10V_midBTW_1c_L1",
  "result_sim1000_30_10V_midINTER_1c_L1",
  "result_sim1000_30_10V_midBTW_1c_L1",
  "result_sim1000_70_10V_midINTER_1c_L1",
  "result_sim1000_70_10V_midBTW_1c_L1",
  "result_sim500_30_10V_highINTER_1c_L1",
  "result_sim500_30_10V_highBTW_1c_L1",
  "result_sim500_70_10V_highINTER_1c_L1",
  "result_sim500_70_10V_highBTW_1c_L1",
  "result_sim1000_30_10V_highINTER_1c_L1",
  "result_sim1000_30_10V_highBTW_1c_L1",
  "result_sim1000_70_10V_highINTER_1c_L1",
  "result_sim1000_70_10V_highBTW_1c_L1"
)

result_all_10V1C <- do.call(rbind, lapply(result_names, function(nm) {
  df <- get(nm)
  df$dataset <- sub("^result_", "", nm)
  df[, c("dataset", "AUC", "BS", "Cindex", "Time")]
}))

out_xlsx <- "F:/文章_大论文/0722/模拟研究代码/10V1Cresult.xlsx"
write_xlsx(result_all_10V1C, path = out_xlsx)
message("已保存: ", out_xlsx, " | nrow = ", nrow(result_all_10V1C))

















ncores <- 8L
options(mc.cores = ncores)
# 从主脚本只加载 cal_3_JM
src_file <- "F:/文章_大论文/0722/模拟研究代码/5_jointmodel_10V1C.R"
src <- readLines(src_file, encoding = "UTF-8")
i1 <- grep("^cal_3_JM <- function", src)[1]
i2 <- grep("^########低相关性###########", src)[1] - 1
eval(parse(text = src[i1:i2]))
input_dir <- "F:/文章/大论文/程序Trae/模拟数据_添加Y"
out_xlsx  <- "F:/文章_大论文/0722/模拟研究代码/10V1Cresult.xlsx"
t0 <- 1
# 仅 70% 删失，共 12 个
ds70 <- c(
  "sim500_70_10V_lowINTER_1c_L1",
  "sim500_70_10V_lowBTW_1c_L1",
  "sim1000_70_10V_lowINTER_1c_L1",
  "sim1000_70_10V_lowBTW_1c_L1",
  "sim500_70_10V_midINTER_1c_L1",
  "sim500_70_10V_midBTW_1c_L1",
  "sim1000_70_10V_midINTER_1c_L1",
  "sim1000_70_10V_midBTW_1c_L1",
  "sim500_70_10V_highINTER_1c_L1",
  "sim500_70_10V_highBTW_1c_L1",
  "sim1000_70_10V_highINTER_1c_L1",
  "sim1000_70_10V_highBTW_1c_L1"
)
read_L1 <- function(nm) {
  f <- file.path(input_dir, paste0(nm, ".xlsx"))
  as.data.frame(read_xlsx(f, sheet = excel_sheets(f)[1]))
}
result_70 <- lapply(ds70, function(nm) {
  message("\n===== 开始: ", nm, " =====")
  dat <- read_L1(nm)
  res <- cal_3_JM(dat, t0 = t0)
  res$dataset <- nm
  res[, c("dataset", "AUC", "BS", "Cindex", "Time")]
})
result_70 <- do.call(rbind, result_70)
print(result_70)
# 合并进已有结果：只替换同名 dataset 行
old <- as.data.frame(read_xlsx(out_xlsx))
keep <- old[!old$dataset %in% result_70$dataset, , drop = FALSE]
new_all <- rbind(keep, result_70)
# 按原表顺序排（若原表没有的名字，追加在后）
ord <- unique(c(old$dataset, result_70$dataset))
new_all <- new_all[match(ord, new_all$dataset), ]
rownames(new_all) <- NULL
write_xlsx(new_all, path = out_xlsx)
message("已更新: ", out_xlsx, " | 本次补充 ", nrow(result_70), " 行")









