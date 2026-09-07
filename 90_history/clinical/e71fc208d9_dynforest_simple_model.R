# 确保安装了必要的包
if (!requireNamespace("DynForest", quietly = TRUE)) {
  install.packages("DynForest", dependencies = TRUE)
}
if (!requireNamespace("survival", quietly = TRUE)) {
  install.packages("survival")
}
if (!requireNamespace("dplyr", quietly = TRUE)) {
  install.packages("dplyr")
}

# 加载包
library(DynForest)
library(survival)
library(dplyr)

# 设置随机种子以确保结果可重复
set.seed(123)

# 生成示例数据
n_subjects <- 100  # 患者数量
n_measurements <- 3  # 每个患者的测量次数

# 生成纵向数据
longitudinal_data <- expand.grid(
  id = 1:n_subjects,
  time = 0:(n_measurements - 1)
) %>%
  mutate(
    # 生成随时间变化的标记物Y
    Y = rnorm(n(), mean = 5 + 0.5 * time + 0.1 * id, sd = 1)
  )

# 生成生存数据和基线协变量
fixed_data <- data.frame(
  id = 1:n_subjects,
  # 生成生存时间（指数分布）
  time = rexp(n_subjects, rate = 0.1),
  # 生成事件指示器（1=事件发生，0=删失）
  event = rbinom(n_subjects, size = 1, prob = 0.7),
  # 添加基线协变量
  age = rnorm(n_subjects, mean = 65, sd = 10),
  gender = factor(rbinom(n_subjects, size = 1, prob = 0.5), labels = c("F", "M"))
)

# 确保生存时间为正
fixed_data$time <- abs(fixed_data$time) + 0.1

# 转换为DynForest需要的格式
fixed_data <- fixed_data %>%
  mutate(
    id = as.numeric(id),
    event = as.numeric(event),
    gender_num = as.numeric(gender)
  )

# 打印数据结构（可选，用于调试）
cat("纵向数据结构:")
str(longitudinal_data)
cat("\n\n固定数据结构:")
str(fixed_data)

# 构建DynForest模型 - 这是核心代码
dyn_model <- DynForest::dynforest(
  timeData = as.data.frame(longitudinal_data),
  fixedData = as.data.frame(fixed_data),
  idVar = "id",
  timeVar = "time",
  timeVarModel = list(
    Y = list(
      model = "linear",
      fixed = ~ 1 + time,
      random = ~ 1 + time | id
    )
  ),
  Y = list(
    type = "surv",
    Y = data.frame(
      id = fixed_data$id,
      time = fixed_data$time,
      event = fixed_data$event
    )
  ),
  ntree = 100,  # 使用较少的树以加快计算
  mtry = 2,
  nodesize = 5,
  minsplit = 5,
  nsplit_option = "quantile",
  ncores = 1,
  verbose = TRUE  # 显示详细输出以便调试
)

# 打印模型摘要
cat("\n\n模型构建成功！")
cat("\n模型摘要:")
print(dyn_model)
