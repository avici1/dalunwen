#%%
library(survival)      # Surv()
library(rms)           # cph()
library(timeROC)       # timeROC()
library(dplyr)         # 基本数据处理
library(readxl)        # read_excel()（如已改用 gdata 可再删）
library(tidyverse)
library(nlme)
library(JMbayes2)
library(parallel)


#%%
# 读取数据。
input_file <- "E:/研究生/硕士大论文原始记录/2026周浩然/TEST/sim500_30_10V_highBTW_3c_L1.xlsx"
t1 <- map(
  setNames(excel_sheets(input_file), excel_sheets(input_file)),
  ~ read_xlsx(input_file, sheet = .x)
)

data<-t1$Sheet2

data_clean <- data[data$t <= data$obs_time, ]



#%%
#########################################baseline model

baseline_data <- subset(data_clean, time == 1)

cox_baseline <- coxph(
  Surv(obs_time, event) ~ 
    V1 + V2 + V3 + V4 + V5 + V6,
  data = baseline_data,
  x = TRUE
)


summary(cox_baseline)$concordance


## 采用风险函数，然后用concordance，一定要把reverse=TRUE转过来，此时计算的C-index与前面直接算一样结果
baseline_data$risk <- predict(cox_baseline, type = "lp")
Cindex <- concordance(
  Surv(obs_time,event) ~ risk,
  reverse=TRUE,
  data=baseline_data
)

Cindex 
##################################################


#%%
######Joint model
start <- Sys.time()

## Longitudinal model
fm1 <- lme( V1 ~ t , data=data_clean, random =~ 1 | ID)
fm2 <- lme( V2 ~ t , data=data_clean, random =~ 1 | ID)
fm3 <- lme( V3 ~ t , data=data_clean, random =~ 1 | ID)
fm4 <- lme( V4 ~ t , data=data_clean, random =~ 1 | ID)
fm5 <- lme( V5 ~ t , data=data_clean, random =~ 1 | ID)
fm6 <- lme( V6 ~ t , data=data_clean, random =~ 1 | ID)
  
## Survival model
survData <- data_clean[!duplicated(data_clean$ID), ]

coxFit <- coxph(
  Surv(obs_time, event) ~ 1,
  data = survData,
  x = TRUE
)

## Joint model
jmFit <- jm(
  coxFit,
  list(fm1, fm2, fm3,fm4, fm5, fm6),
  time_var = "t"
)

end <- Sys.time()
cat("CPU cores:", detectCores(), "\n")
cat("Start:", format(start, "%Y-%m-%d %H:%M:%S"), "\n")
cat("End:",  format(end, "%Y-%m-%d %H:%M:%S"), "\n")
cat("Elapsed:", round(as.numeric(difftime(end, start, units = "mins")), 2),
    "minutes\n")


#%%
 start <- Sys.time()
####预测风险
pred <- predict(
  jmFit,
  newdata = data_clean,
  process = "event",
)

####个体风险
pred_patient <- data.frame(
  ID = pred$id,
  time = pred$times,
  risk = pred$pred
)

####提取最后一个时间的预测作为C-index计算
risk_last <- pred_patient %>%
  group_by(ID) %>%
  summarise(
    risk = last(risk)
  )

cdata <- merge(
  survData,
  risk_last,
  by="ID"
)

#######predictor 是风险，不是生存概率。风险越大，
Cindex <- concordance(
  Surv(obs_time,event) ~ risk,
  reverse=TRUE,
  data=cdata
)

Cindex$concordance



end <- Sys.time()
cat("CPU cores:", detectCores(), "\n")
cat("Start:", format(start, "%Y-%m-%d %H:%M:%S"), "\n")
cat("End:",  format(end, "%Y-%m-%d %H:%M:%S"), "\n")
cat("Elapsed:", round(as.numeric(difftime(end, start, units = "mins")), 2),
    "minutes\n")





#%%   
# Landmark time
Tstart <- 1

# Prediction horizon
Dt <- 3

bs <- tvBrier(
    object = jmFit,
    newdata = data_clean,
    Tstart = Tstart,
    Dt = Dt
)

bs



