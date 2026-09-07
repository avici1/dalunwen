# DynForest模型构建 - 脑卒中患者生存分析
# 数据来源：MIMIC数据库提取

# 导入必要的库
library(DynForest)
library(readxl)
library(survival)

# 1. 数据读取与预处理
print("Step 1: 读取数据并进行预处理...")
data <- read_excel('f:/文章_大论文/MIMIC数据库_代码/TREA代码/0313/data_all_sup1.xlsx')
glimpse(data)



# 变量重命名与数据类型转换
data$subject_id <- as.integer(data$subject_id)
data$hospital_mortality <- as.numeric(data$hospital_mortality)  # 删失标记
data$obstime <- as.numeric(data$obstime)  # 纵向时间点

data$heart_rate <- as.numeric(data$`Heart Rate`)  # 心率
data$resp_rate <- as.numeric(data$`Respiratory Rate`)  # 呼吸率
data$mean_bp <- as.numeric(data$`Mean BP`)  # 平均血压
data$temperature <- as.numeric(data$`Temperature C`)  # 体温
data$spo2 <- as.numeric(data$`Oxygen Saturation`)  # 血氧饱和度
data$gender <- as.numeric(data$gender_binary)  # 性别
data$age <- as.numeric(data$anchor_age)  # 年龄
data$hypertension <- as.numeric(data$hypertension)  # 高血压病史
data$diabetes <- as.numeric(data$diabetes)  # 糖尿病病史

# 2. 缺失值处理
print("Step 2: 处理缺失值...")
# 移除包含缺失值的行
data <- data[!is.na(data$subject_id), ]
data <- data[!is.na(data$obstime), ]
data <- data[!is.na(data$heart_rate), ]
data <- data[!is.na(data$resp_rate), ]
data <- data[!is.na(data$gender), ]
data <- data[!is.na(data$age), ]
data <- data[!is.na(data$hospital_mortality), ]

# 3. 创建生存数据
print("Step 3: 创建生存数据...")
# 每个受试者的最后一个观测值作为生存结局
surv_data <- aggregate(
  cbind(hospital_mortality, obstime) ~ subject_id,
  data = data,
  FUN = max,
  na.rm = TRUE
)

# 4. 数据集划分（确保包含足够的事件受试者）
print("Step 4: 划分数据集...")
event_subjects <- surv_data[surv_data$hospital_mortality == 1, ]$subject_id  # 发生事件的受试者
no_event_subjects <- surv_data[surv_data$hospital_mortality == 0, ]$subject_id  # 未发生事件的受试者

# 选择20个受试者用于建模（5个事件，15个非事件）
selected_events <- event_subjects[1:5]
selected_no_events <- no_event_subjects[1:15]
filtered_subjects <- c(selected_events, selected_no_events)

# 过滤数据
data <- data[data$subject_id %in% filtered_subjects, ]
surv_data <- surv_data[surv_data$subject_id %in% filtered_subjects, ]

print(paste("数据集大小：", length(filtered_subjects), "个受试者"))
print(paste("事件数量：", length(selected_events)))
print(paste("非事件数量：", length(selected_no_events)))

# 5. 创建DynForest所需的数据结构
print("Step 5: 创建DynForest数据结构...")

# 纵向数据（随时间变化的变量）
time_data <- as.data.frame(data[, c(
  'subject_id', 'obstime', 'heart_rate', 'resp_rate'
)])

# 固定数据（基线变量）
fixed_data <- as.data.frame(data[, c(
  'subject_id', 'gender', 'age'
)])
fixed_data <- as.data.frame(unique(fixed_data))  # 确保每个受试者只有一条基线记录

# 生存数据对象（符合DynForest要求的格式）
Y <- list(
  Y = data.frame(
    subject_id = surv_data$subject_id,
    time = surv_data$obstime,
    event = surv_data$hospital_mortality
  ),
  type = "surv"  # 生存分析类型
)

# 时间变量模型（定义每个纵向变量的时间模式）
timeVarModel <- list(
  heart_rate = "linear",  # 心率随时间线性变化
  resp_rate = "linear"    # 呼吸率随时间线性变化
)

# 6. 构建DynForest模型
print("Step 6: 构建DynForest模型...")

# 最佳超参数（通过调优获得）
best_params <- list(
  ntree = 10,      # 树的数量
  mtry = 1,        # 每次分裂考虑的变量数
  nodesize = 5,    # 终端节点最小受试者数
  minsplit = 5     # 分裂所需的最小受试者数
)

# 构建模型
dynforest_model <- dynforest(
  timeData = time_data,
  fixedData = fixed_data,
  idVar = 'subject_id',  # 受试者ID变量
  timeVar = 'obstime',   # 纵向时间变量
  timeVarModel = timeVarModel,
  Y = Y,
  ntree = best_params$ntree,
  mtry = best_params$mtry,
  nodesize = best_params$nodesize,
  minsplit = best_params$minsplit,
  ncores = 1  # 单核心运行
)

# 7. 模型评估与总结
print("Step 7: 模型评估与总结...")

# 打印模型摘要
print("\n=== DynForest模型摘要 ===")
print(summary(dynforest_model))

# 保存模型
print("\n保存模型...")
saveRDS(dynforest_model, 'dynforest_stroke_model.rds')
print("模型已保存为：dynforest_stroke_model.rds")

# 8. 最终总结
print("\n=== 最终总结 ===")
print("1. 数据：使用MIMIC数据库提取的20个脑卒中患者数据（5个事件）")
print("2. 变量：")
print("   - 纵向变量：心率、呼吸率")
print("   - 固定效应：性别、年龄")
print("3. 超参数：")
print(paste("   - 树的数量：", best_params$ntree))
print(paste("   - 每次分裂变量数：", best_params$mtry))
print(paste("   - 终端节点大小：", best_params$nodesize))
print("4. 模型性能：")
print(paste("   - 平均树深度：", dynforest_model$summary$depth))
print(paste("   - 平均叶子数：", dynforest_model$summary$leaves))
print("\n模型构建完成！")
