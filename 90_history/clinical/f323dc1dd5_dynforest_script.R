library(DynForest)
library(readxl)
library(survival)

# 读取数据
data <- read_excel('f:/文章_大论文/MIMIC数据库_代码/TREA代码/0313/data_all_sup1.xlsx')

# 数据预处理
data$subject_id <- as.integer(data$subject_id)
data$hospital_mortality <- as.numeric(data$hospital_mortality)
data$obstime <- as.numeric(data$obstime)

# 创建纵向数据框（仅包含纵向变量）
time_data <- data.frame(
  subject_id = data$subject_id,
  obstime = data$obstime,
  heart_rate = as.numeric(data$`Heart Rate`),
  resp_rate = as.numeric(data$`Respiratory Rate`),
  systolic_bp = as.numeric(data$`Systolic BP`),
  diastolic_bp = as.numeric(data$`Diastolic BP`),
  mean_bp = as.numeric(data$`Mean BP`),
  oxygen_sat = as.numeric(data$`Oxygen Saturation`),
  gcs_total = as.numeric(data$`GCS Total`),
  hemoglobin = as.numeric(data$Hemoglobin),
  creatinine = as.numeric(data$Creatinine)
)

# 创建固定效应数据框
fixed_data <- data.frame(
  subject_id = data$subject_id,
  gender = as.numeric(data$gender_binary),
  age = as.numeric(data$anchor_age),
  hypertension = as.numeric(data$hypertension),
  diabetes = as.numeric(data$diabetes),
  atrial_fibrillation = as.numeric(data$atrial_fibrillation),
  heart_failure = as.numeric(data$heart_failure),
  renal_disease = as.numeric(data$renal_disease)
)

# 创建生存数据
surv_data <- data.frame(
  subject_id = data$subject_id,
  hospital_mortality = as.numeric(data$hospital_mortality),
  obstime = as.numeric(data$obstime)
)
surv_data <- aggregate(
  cbind(hospital_mortality, obstime) ~ subject_id,
  data = surv_data,
  FUN = max,
  na.rm = TRUE
)

# 处理可能的无限值和缺失值
if (any(is.infinite(surv_data$hospital_mortality))) {
  surv_data$hospital_mortality[is.infinite(surv_data$hospital_mortality)] <- 0
}
if (any(is.infinite(surv_data$obstime))) {
  surv_data$obstime[is.infinite(surv_data$obstime)] <- 0
}
surv_data$hospital_mortality[is.na(surv_data$hospital_mortality)] <- 0
surv_data$obstime[is.na(surv_data$obstime)] <- 0

# 创建生存对象（符合DynForest要求的格式）
Y <- list(
  Y = data.frame(subject_id = surv_data$subject_id, time = surv_data$obstime, event = surv_data$hospital_mortality),
  type = "surv"
)

print(paste('Number of unique subjects:', nrow(surv_data)))
print('Sample survival data:')
print(head(surv_data))

# 清理数据
fixed_data <- unique(fixed_data)
time_data <- time_data[!is.na(time_data$subject_id), ]
time_data <- time_data[!is.na(time_data$obstime), ]

# 处理纵向数据中的缺失值
for (col in c('heart_rate', 'resp_rate', 'systolic_bp', 'diastolic_bp', 'mean_bp', 'oxygen_sat', 'gcs_total', 'hemoglobin', 'creatinine')) {
  if (any(is.na(time_data[[col]]))) {
    time_data[[col]][is.na(time_data[[col]])] <- median(time_data[[col]], na.rm = TRUE)
  }
}

# 处理固定数据中的缺失值
for (col in c('gender', 'age', 'hypertension', 'diabetes', 'atrial_fibrillation', 'heart_failure', 'renal_disease')) {
  if (any(is.na(fixed_data[[col]]))) {
    fixed_data[[col]][is.na(fixed_data[[col]])] <- median(fixed_data[[col]], na.rm = TRUE)
  }
}

# 构建DynForest模型
print('Building dynforest model...')
timeVarModel <- list(
  heart_rate = "linear",
  resp_rate = "linear",
  systolic_bp = "linear",
  diastolic_bp = "linear",
  mean_bp = "linear",
  oxygen_sat = "linear",
  gcs_total = "linear",
  hemoglobin = "linear",
  creatinine = "linear"
)

# 禁用并行计算，使用单线程
dynforest_model <- dynforest(
  timeData = time_data,
  fixedData = fixed_data,
  idVar = 'subject_id',
  timeVar = 'obstime',
  timeVarModel = timeVarModel,
  Y = Y,
  ntree = 50,
  mtry = 3,
  nodesize = 10,
  ncores = 1
)

print('Model built successfully!')

# 计算OOB错误
print('Computing OOB error...')
oob_error <- compute_ooberror(dynforest_model)
print(oob_error)

# 计算变量重要性
print('Computing variable importance...')
vimp <- compute_vimp(dynforest_model)
print(vimp)

# 计算广义变量重要性
print('Computing generalized variable importance...')
gvimp <- compute_gvimp(dynforest_model)
print(gvimp)