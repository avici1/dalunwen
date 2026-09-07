
install.packages("randomForest")
library(randomForest)
model <- randomForest(Species ~ ., data = iris, ntree = 500)


head(long_TNI)



install.packages("randomForestSRC")
library(randomForestSRC)

# 模型1：仅使用基线协变量
rsf_model1 <- rfsrc(
  Surv(surv_time, event) ~ AGE + GENDER + RESULTS + DISEASE_CODE1,
  data = long_TNI,
  ntree = 1000,
  importance = TRUE
)

# 输出变量重要性
plot(rsf_model1)  # 生存曲线
plot.variable(rsf_model1) 



###############GROK#####################
library(randomForest)  # 用于随机森林建模
install.packages("caret")
library(caret)        # 用于数据划分和评估

data<-long_TNI





##############GPT###########
library(randomForest)
library(dplyr)

# 读取你的数据，这里假设你已经有 long_TNI 数据框

# 简单数据预处理：
# 选择建模用的变量
data_rf <- long_TNI %>%
  select(RESULTS, AGE, GENDER, NATIONALITY, MARRAGE, PROFESSION_CODE, EDUCATION, ACCTUAL_DAYS, SURV)

# 如果 EDUCATION 变量是字符型，转为因子
data_rf$EDUCATION <- as.factor(data_rf$EDUCATION)

# 把分类变量也转为因子
data_rf$GENDER <- as.factor(data_rf$GENDER)
data_rf$NATIONALITY <- as.factor(data_rf$NATIONALITY)
data_rf$MARRAGE <- as.factor(data_rf$MARRAGE)
data_rf$PROFESSION_CODE <- as.factor(data_rf$PROFESSION_CODE)
data_rf$SURV <- as.factor(data_rf$SURV)  # 随机森林要求因变量为因子做分类任务

# 查看缺失值（如果有需要做缺失值处理）
summary(data_rf)

# 建立随机森林模型
set.seed(123)  # 保证结果可重复
rf_model <- randomForest(SURV ~ ., data = data_rf, ntree = 500, mtry = 3, importance = TRUE)

# 查看模型结果
print(rf_model)

# 变量重要性
importance(rf_model)
varImpPlot(rf_model)





