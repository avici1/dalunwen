

library(survival)
library(dplyr)

# 选择建模变量
data_cox <- long_TNI %>%
  select(RESULTS, AGE, GENDER, NATIONALITY, MARRAGE, PROFESSION_CODE, EDUCATION, ACCTUAL_DAYS, SURV)

# 把分类变量转为因子
data_cox$EDUCATION <- as.factor(data_cox$EDUCATION)
data_cox$GENDER <- as.factor(data_cox$GENDER)
data_cox$NATIONALITY <- as.factor(data_cox$NATIONALITY)
data_cox$MARRAGE <- as.factor(data_cox$MARRAGE)
data_cox$PROFESSION_CODE <- as.factor(data_cox$PROFESSION_CODE)

surv_obj <- Surv(time = data_cox$ACCTUAL_DAYS, event = data_cox$SURV)

cox_model <- coxph(surv_obj ~ 
                     RESULTS + AGE + GENDER + NATIONALITY + MARRAGE + 
                     PROFESSION_CODE + EDUCATION, data = data_cox)

# 查看模型结果
summary(cox_model)