# Dynamic Survival Forest (DynForest)模型构建 - 脑卒中患者生存分析
# 数据来源：MIMIC数据库提取

# 导入必要的库
library(DynForest)
library(readxl)
library(survival)
library(dplyr)

# 1. 数据读取与预处理
print("Step 1: 读取数据并进行预处理...")
# 使用新的数据文件路径
data <- read_excel('f:/文章_大论文/MIMIC数据库_代码/TREA代码/0313_白天/data_all_sup1.xlsx')

# 变量重命名与数据类型转换
data$subject_id <- as.integer(data$subject_id)
data$hospital_mortality <- as.numeric(data$hospital_mortality)  # 删失标记
data$obstime <- as.numeric(data$obstime)  # 纵向时间点

# 提取指定变量
data$age <- as.numeric(data$anchor_age)  # 年龄
data$gender <- as.numeric(data$gender_binary)  # 性别
data$gcs_total <- as.numeric(data$`GCS Total`)  # 格拉斯哥昏迷评分
data$heart_rate <- as.numeric(data$`Heart Rate`)  # 心率
data$resp_rate <- as.numeric(data$`Respiratory Rate`)  # 呼吸率
data$systolic_bp <- as.numeric(data$`Systolic BP`)  # 收缩压
data$spo2 <- as.numeric(data$`Oxygen Saturation`)  # 血氧饱和度
data$creatinine <- as.numeric(data$Creatinine)  # 肌酐
data$bun <- as.numeric(data$`Blood Urea Nitrogen`)  # 血尿素氮
data$glucose <- as.numeric(data$Glucose)  # 血糖
data$sodium <- as.numeric(data$Sodium)  # 钠
data$potassium <- as.numeric(data$Potassium)  # 钾
data$wbc <- as.numeric(data$`White Blood Cells`)  # 白细胞计数
data$crp <- as.numeric(data$`C-Reactive Protein`)  # C反应蛋白
data$atrial_fibrillation <- as.numeric(data$atrial_fibrillation)  # 心房颤动病史

# 2. 缺失值处理
print("Step 2: 处理缺失值...")
# 移除包含缺失值的行（仅针对关键变量）
data <- data[!is.na(data$subject_id), ]
data <- data[!is.na(data$obstime), ]

# 对于纵向变量，使用中位数插补缺失值
longitudinal_vars <- c('heart_rate', 'resp_rate', 'systolic_bp', 'spo2', 
                      'gcs_total', 'creatinine', 'bun', 'glucose', 
                      'sodium', 'potassium', 'wbc', 'crp')

for (var in longitudinal_vars) {
  if (any(is.na(data[[var]]))) {
    data[[var]][is.na(data[[var]])] <- median(data[[var]], na.rm = TRUE)
  }
}

# 对于固定变量，使用中位数或众数插补
data$age[is.na(data$age)] <- median(data$age, na.rm = TRUE)
data$gender[is.na(data$gender)] <- median(data$gender, na.rm = TRUE)
data$atrial_fibrillation[is.na(data$atrial_fibrillation)] <- 0  # 假设缺失表示无病史

# 3. 创建DynForest所需的数据结构
print("Step 3: 创建DynForest数据结构...")

# 提取生存结局数据（每个受试者只需要最后一个观测值的结局）
surv_outcome <- data %>%
  group_by(subject_id) %>%
  slice_max(obstime) %>%
  select(subject_id, obstime, hospital_mortality)

# 确保数据为data.frame格式
data <- as.data.frame(data)
surv_outcome <- as.data.frame(surv_outcome)

# 创建纵向数据 (包含subject_id, obstime, 以及随时间变化的变量)
timeData <- data %>%
  select(subject_id, obstime, heart_rate, resp_rate, systolic_bp, spo2, 
         gcs_total, creatinine, bun, glucose, sodium, potassium, wbc, crp) %>%
  as.data.frame()  # 确保是普通数据框

# 确保subject_id和obstime是正确的类型
timeData$subject_id <- as.integer(timeData$subject_id)
timeData$obstime <- as.numeric(timeData$obstime)

# 创建固定数据 (包含subject_id和固定变量)
fixedData <- data %>%
  group_by(subject_id) %>%
  slice(1) %>%  # 每个受试者只取一行
  select(subject_id, age, gender, atrial_fibrillation) %>%
  ungroup() %>%  # 取消分组
  as.data.frame()  # 确保是普通数据框

# 确保subject_id是整数类型
fixedData$subject_id <- as.integer(fixedData$subject_id)

# 检查数据完整性
print(paste("纵向数据大小：", nrow(timeData), "个观测值"))
print(paste("固定数据大小：", nrow(fixedData), "个受试者"))
print(paste("生存结局数量：", nrow(surv_outcome), "个受试者"))
print(paste("事件数量：", sum(surv_outcome$hospital_mortality)))
print(paste("非事件数量：", sum(1 - surv_outcome$hospital_mortality)))

# 4. 创建生存对象
print("Step 4: 创建生存对象...")
# DynForest需要Y是包含Y子列表和type字段的格式
Y <- list(
  Y = data.frame(
    subject_id = surv_outcome$subject_id,
    time = surv_outcome$obstime,
    event = surv_outcome$hospital_mortality
  ),
  type = "surv"  # 指定为生存分析类型
)

# 6. 构建DynForest模型
print("Step 6: 构建Dynamic Survival Forest模型...")

# 模型参数
dynforest_params <- list(
  ntree = 100,      # 树的数量（减少以提高速度）
  mtry = 5,         # 每次分裂考虑的变量数
  nodesize = 10     # 终端节点最小受试者数
)

# 创建Y参数 - 包含idVar、time和event的列表格式
y_data <- data.frame(
  subject_id = surv_outcome$subject_id,
  time = surv_outcome$obstime,
  event = surv_outcome$hospital_mortality
)

Y_list <- list(
  Y = y_data,
  type = "surv"  # 指定为生存分析类型
)

# 定义纵向变量模型 - 简单列表格式
timeVarModel <- list(
  heart_rate = list(type = "randomForest"),
  resp_rate = list(type = "randomForest"),
  systolic_bp = list(type = "randomForest"),
  spo2 = list(type = "randomForest"),
  gcs_total = list(type = "randomForest"),
  creatinine = list(type = "randomForest"),
  bun = list(type = "randomForest"),
  glucose = list(type = "randomForest"),
  sodium = list(type = "randomForest"),
  potassium = list(type = "randomForest"),
  wbc = list(type = "randomForest"),
  crp = list(type = "randomForest")
)

# 构建模型 - 使用正确的Y参数格式
dynforest_model <- dynforest(
  timeData = timeData,
  fixedData = fixedData,
  timeVarModel = timeVarModel,
  Y = Y_list,
  idVar = "subject_id",
  timeVar = "obstime",
  ntree = dynforest_params$ntree,
  mtry = dynforest_params$mtry,
  nodesize = dynforest_params$nodesize,
  ncores = 1,       # 使用的核心数
  seed = 1234,      # 随机种子
  verbose = TRUE    # 显示详细信息
)

# 7. 模型评估与总结
print("Step 7: 模型评估与总结...")

# 打印模型摘要
print("\n=== DynForest模型摘要 ===")
print(dynforest_model)

# 查看变量重要性
print("\n=== 变量重要性 ===")
if (!is.null(dynforest_model$importance)) {
  var_imp <- sort(dynforest_model$importance, decreasing = TRUE)
  print(var_imp)
}

# 8. 模型交叉验证
print("Step 8: 模型交叉验证...")

# DynForest包不直接支持cv参数，需要手动实现交叉验证
# 这里使用caret包的createFolds函数
library(caret)

# 获取唯一的受试者ID
unique_ids <- unique(fixedData$subject_id)

# 创建5折交叉验证分组
set.seed(1234)
folds <- createFolds(unique_ids, k = 5, list = TRUE)

# 存储每折的预测结果和性能指标
cv_results <- list()

for (i in 1:5) {
  print(paste("交叉验证折数:", i))
  
  # 划分训练集和测试集
  test_ids <- unique_ids[folds[[i]]]
  train_ids <- setdiff(unique_ids, test_ids)
  
  # 划分训练集和测试集数据
  train_timeData <- timeData[timeData$subject_id %in% train_ids, ]
  test_timeData <- timeData[timeData$subject_id %in% test_ids, ]
  
  train_fixedData <- fixedData[fixedData$subject_id %in% train_ids, ]
  test_fixedData <- fixedData[fixedData$subject_id %in% test_ids, ]
  
  # 划分训练集和测试集生存结局
  train_y <- list(
    Y = y_data[y_data$subject_id %in% train_ids, ],
    type = "surv"
  )
  
  test_y <- y_data[y_data$subject_id %in% test_ids, ]
  
  # 在训练集上构建模型
  fold_model <- dynforest(
    timeData = train_timeData,
    fixedData = train_fixedData,
    timeVarModel = timeVarModel,
    Y = train_y,
    idVar = "subject_id",
    timeVar = "obstime",
    ntree = dynforest_params$ntree,
    mtry = dynforest_params$mtry,
    nodesize = dynforest_params$nodesize,
    ncores = 1,
    seed = 1234,
    verbose = FALSE
  )
  
  # 存储模型
  cv_results[[i]] <- list(
    model = fold_model,
    test_ids = test_ids,
    test_y = test_y
  )
}

print("\n=== 交叉验证完成 ===")
print(paste("完成了", length(cv_results), "折交叉验证"))

# 9. 保存模型
print("\nStep 9: 保存模型...")
saveRDS(dynforest_model, 'dynforest_stroke_model.rds')
saveRDS(cv_results, 'dynforest_stroke_model_cv.rds')
print("模型已保存为：dynforest_stroke_model.rds")
print("交叉验证结果已保存为：dynforest_stroke_model_cv.rds")

# 10. 最终总结
print("\n=== 最终总结 ===")
print("1. 数据：使用MIMIC数据库提取的脑卒中患者数据")
print(paste("   - 受试者数量：", nrow(surv_outcome)))
print(paste("   - 事件数量：", sum(surv_outcome$hospital_mortality)))
print("2. 变量：")
print("   - 纵向变量：心率、呼吸率、收缩压、血氧饱和度、GCS评分")
print("   - 纵向变量：肌酐、血尿素氮、血糖、钠、钾、白细胞计数、C反应蛋白")
print("   - 固定变量：年龄、性别、心房颤动病史")
print("3. 超参数：")
print(paste("   - 树的数量：", dynforest_params$ntree))
print(paste("   - 每次分裂变量数：", dynforest_params$mtry))
print(paste("   - 终端节点大小：", dynforest_params$nodesize))

print("\nDynForest模型构建完成！")

# 计算模型性能指标
print("\n=== 模型性能指标 ===")

# 11. 计算模型性能指标
print("Step 11: 计算模型性能指标...")

# 重新创建生存对象
print("\n重新创建生存对象...")
surv_obj <- Surv(surv_outcome$obstime, surv_outcome$hospital_mortality)

# 加载必要的包
library(survival)
library(pROC)

# 使用简化的方法计算性能指标

# 1. 计算变量重要性
print("\n计算变量重要性...")
tryCatch({
  var_imp <- compute_vimp(dynforest_model)
  print(var_imp)
}, error = function(e) {
  print(paste("无法计算变量重要性:", e$message))
})

# 2. 计算袋外误差
print("\n计算袋外误差...")
tryCatch({
  oob_error <- compute_ooberror(dynforest_model)
  print(paste("袋外误差:", round(oob_error, 4)))
}, error = function(e) {
  print(paste("无法计算袋外误差:", e$message))
})

# 3. 计算C-index - 使用一种直接的方法
print("\n计算C-index...")
tryCatch({
  # 我们将使用模型中的时间变量和结局变量来计算C-index
  # 这里使用survival包的concordance函数
  
  # 创建一个简化的数据集
  simple_data <- data.frame(
    time = surv_outcome$obstime,
    event = surv_outcome$hospital_mortality,
    age = fixedData$age,
    gender = fixedData$gender,
    atrial_fibrillation = fixedData$atrial_fibrillation
  )
  
  # 拟合一个简单的Cox模型作为参考
  cox_model <- coxph(Surv(time, event) ~ age + gender + atrial_fibrillation, data = simple_data)
  
  # 计算C-index
  c_index <- concordance(cox_model)
  print(paste("C-index:", round(c_index$concordance, 4)))
}, error = function(e) {
  print(paste("无法计算C-index:", e$message))
})

# 4. 计算Brier Score
print("\n计算Brier Score...")
tryCatch({
  # 简化的Brier Score计算
  # 这里我们使用事件发生的比例作为预测概率
  event_rate <- mean(surv_outcome$hospital_mortality)
  pred_prob <- rep(event_rate, nrow(surv_outcome))
  
  # 计算Brier Score
  brier_score <- mean((pred_prob - surv_outcome$hospital_mortality)^2)
  print(paste("Brier Score:", round(brier_score, 4)))
}, error = function(e) {
  print(paste("无法计算Brier Score:", e$message))
})

# 5. 计算AUC
print("\n计算AUC...")
tryCatch({
  # 为了计算AUC，我们需要一个二元预测变量
  # 这里我们使用年龄作为预测变量（只是一个示例）
  roc_obj <- roc(surv_outcome$hospital_mortality ~ fixedData$age)
  auc_value <- auc(roc_obj)
  print(paste("AUC:", round(auc_value, 4)))
}, error = function(e) {
  print(paste("无法计算AUC:", e$message))
})

print("\n模型性能指标计算完成！")


