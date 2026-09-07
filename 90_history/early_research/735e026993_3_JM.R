library(dplyr)
library(lubridate)
library(tibble)
library(JM)
library(lme4)
library(descr)
library("lattice")
library(ggplot2)
options(scipen=999)#禁用科学计数法

aids<-aids
aids.id<-aids.id


######
long_data <- data.frame(
  id = rep(1:5, each = 4),  # 5个受试者，每人4次测量
  time = rep(c(0, 1, 2, 3), 5),  # 测量时间点0,1,2,3
  biomarker = c(
    5.1, 5.5, 5.8, 6.2,   # 受试者1
    4.9, 5.3, 5.6, NA,    # 受试者2(最后一次缺失)
    6.0, 6.2, 6.5, 6.8,   # 受试者3
    5.5, 5.7, NA, NA,      # 受试者4(后两次缺失)
    4.8, 5.0, 5.2, 5.4     # 受试者5
  ),
  treatment = rep(c("A", "B", "A", "B", "A"), each = 4),  # 治疗分组
  age = rep(c(45, 60, 55, 65, 50), each = 4)  # 基线年龄
)
######

long_GLU<-read.csv("F:/文章/大论文/程序/数据/long_GLU.csv")
long_TC<-read.csv("F:/文章/大论文/程序/数据/long_TC.csv")
long_TG<-read.csv("F:/文章/大论文/程序/数据/long_TG.csv")
long_LDLC<-read.csv("F:/文章/大论文/程序/数据/long_LDLC.csv")
long_HDLC<-read.csv("F:/文章/大论文/程序/数据/long_HDLC.csv")



#####################################################


long_GLU1 <- long_GLU %>%
  mutate(GATHER_DATE = as.Date(GATHER_DATE)) %>%  # 确保是 Date 类型
  group_by(B_WT4_ID) %>%
  mutate(first_date = min(GATHER_DATE, na.rm = TRUE),
         days = as.integer(GATHER_DATE - first_date)) %>%
  ungroup()

long_GLU2 <- long_GLU1 %>%   #删除观测少的/极端值
  group_by(B_WT4_ID) %>%
  filter(n() >= 3) %>%
  filter(days <=90)%>%
  filter(RESULTS <=50)%>%
  ungroup()

long_GLU2 <- long_GLU2 %>%
  select(-RESULTS, -GOOUT_DIAGNOSE_NAME, -first_date)


long_GLU2 <- long_GLU2 %>%                   #所有记录结果
  group_by(B_WT4_ID) %>%
  mutate(TIME = max(days, na.rm = TRUE)) %>%
  ungroup()

long_GLU2_1 <- long_GLU2 %>%
  select(B_WT4_ID, days, SURV, GENDER)




long_GLU3 <- long_GLU2 %>%                   #全体人数
  group_by(B_WT4_ID) %>%
  mutate(TIME = max(days, na.rm = TRUE)) %>%
  summarise(
    first_date = min(GATHER_DATE),
    last_date = max(GATHER_DATE),
    n_obs = n(),
    avg_result = mean(RESULT, na.rm = TRUE),
    avg_days = mean(days, na.rm = TRUE),
    TIME = mean(mean(TIME, na.rm = TRUE)),
    SURV = mean(mean(SURV, na.rm = TRUE)),
    GENDER = mean(mean(GENDER, na.rm = TRUE))
  )


long_GLU3 <- long_GLU2 %>%                   
  group_by(B_WT4_ID) %>%
  summarise(
    TIME = max(days, na.rm = TRUE),  # 生存时间=最后随访时间
    SURV = first(SURV),              # 直接取第一条记录的SURV值（确保是0/1）
    GENDER = first(GENDER)           # 直接取第一条记录的性别
  ) %>%
  ungroup()


table(long_GLU3$SURV)    # 必须为0/1
table(long_GLU3$GENDER) 






ggplot(data = long_GLU3, aes(x = avg_result)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  labs(title = "RESULTS 频数直方图", x = "RESULTS", y = "频数") +
  theme_minimal()

ggplot(data = long_GLU2, aes(x = avg_days)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  labs(title = "RESULTS 频数直方图", x = "RESULTS", y = "频数") +
  theme_minimal()






xyplot(RESULT ~ days | GENDER, group = B_WT4_ID, data = long_GLU1,
       xlab = "Days", ylab = expression(RESULT), col = 1, type = "l")

plot(survfit(Surv(days, SURV) ~ GENDER, data = long_GLU1), conf.int = FALSE,
     mark.time = TRUE, col = c("black", "red"), lty = 1:2,
     ylab = "Survival", xlab = "Months")











fitLME <- lme(RESULT ~ days + days:GENDER,
              random=~ 1|B_WT4_ID, data=long_GLU2)
fitSURV <- coxph(Surv(TIME, SURV) ~ GENDER, data=long_GLU3, x=TRUE)
fit.JM <- jointModel(
  fitLME,
  fitSURV,
  timeVar = "days",
  method = "piecewise-PH-GH",
  control = list(
    iter.qN = 1000,          # 增加迭代次数
    tol1 = 1e-3,             # 放宽收敛阈值
    tol2 = 1e-3
  )
)
#############JM建模###############

intersect_IDs <- intersect(unique(long_GLU2$B_WT4_ID), unique(long_GLU3$B_WT4_ID))
length(intersect_IDs) 
long_GLU2_sub <- subset(long_GLU2, B_WT4_ID %in% intersect_IDs)
long_GLU3_sub <- subset(long_GLU3, B_WT4_ID %in% intersect_IDs)

# 纵向模型
fitLME <- lme(RESULT ~ days + days:GENDER, random = ~ days | B_WT4_ID, data = long_GLU2_sub)

# 生存模型，确保 x=TRUE 且使用 cluster() 保证 ID 一致性（虽然不是强制，但更安全）
fitSURV <- coxph(Surv(TIME, SURV) ~ GENDER + cluster(B_WT4_ID), data = long_GLU3_sub, x = TRUE)

fitSURV <- coxph(Surv(TIME, SURV) ~ GENDER, 
                 data = long_GLU2 %>% group_by(B_WT4_ID) %>% slice(1),
                 x = TRUE)

surv_data <- long_GLU2 %>%
  group_by(B_WT4_ID) %>%
  slice(1) %>%  # 取每个患者的第一行
  ungroup()

fitSURV <- coxph(Surv(TIME, SURV) ~ GENDER, 
                 data = surv_data, 
                 x = TRUE)
# 联合模型
fit.JM <- jointModel(fitLME, fitSURV, timeVar = "days", method = "piecewise-PH-GH")


fit.JM <- jointModel(
  fitLME,
  fitSURV,
  timeVar = "days",
  method = "piecewise-PH-GH",
  control = list(iter.qN = 500)  # 增加迭代次数（可选）
)




summary(fit.JM)

str(long_GLU2$days)

head(long_GLU2)
head(long_GLU3)




  
  