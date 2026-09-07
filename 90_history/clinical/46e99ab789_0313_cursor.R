# DynForest 建模 - data_all_sup1.xlsx
# 15 变量: anchor_age, gender_binary, GCS Total, Heart Rate, Respiratory Rate,
#          Systolic BP, Oxygen Saturation, Creatinine, Blood Urea Nitrogen,
#          Glucose, Sodium, Potassium, White Blood Cells, C-Reactive Protein, atrial_fibrillation
library(DynForest)
library(readxl)
library(dplyr)
library(survival)

# 1. 读取数据
data <- read_excel("f:/文章_大论文/MIMIC数据库_代码/TREA代码/0313_白天/data_all_sup1.xlsx")

# 2. 变量提取与标准化命名（便于 timeVarModel 公式）
data$subject_id <- as.integer(data$subject_id)
data$hospital_mortality <- as.numeric(data$hospital_mortality)
data$obstime <- as.numeric(data$obstime)
data$anchor_age <- as.numeric(data$anchor_age)
data$gender_binary <- as.numeric(data$gender_binary)
data$atrial_fibrillation <- as.numeric(data$atrial_fibrillation)

data$gcs_total      <- as.numeric(data$`GCS Total`)
data$heart_rate     <- as.numeric(data$`Heart Rate`)
data$resp_rate      <- as.numeric(data$`Respiratory Rate`)
data$systolic_bp    <- as.numeric(data$`Systolic BP`)
data$spo2           <- as.numeric(data$`Oxygen Saturation`)
data$creatinine     <- as.numeric(data$Creatinine)
data$bun            <- as.numeric(data$`Blood Urea Nitrogen`)
data$glucose        <- as.numeric(data$Glucose)
data$sodium         <- as.numeric(data$Sodium)
data$potassium      <- as.numeric(data$Potassium)
data$wbc            <- as.numeric(data$`White Blood Cells`)
data$crp            <- as.numeric(data$`C-Reactive Protein`)

# 3. 缺失值处理
data <- data[!is.na(data$subject_id) & !is.na(data$obstime), ]
longitudinal_vars <- c("heart_rate", "resp_rate", "systolic_bp", "spo2", "gcs_total",
                      "creatinine", "bun", "glucose", "sodium", "potassium", "wbc", "crp")
for (v in longitudinal_vars) {
  data[[v]][is.na(data[[v]])] <- median(data[[v]], na.rm = TRUE)
}
data$anchor_age[is.na(data$anchor_age)] <- median(data$anchor_age, na.rm = TRUE)
data$gender_binary[is.na(data$gender_binary)] <- 0
data$atrial_fibrillation[is.na(data$atrial_fibrillation)] <- 0

# 4. 生存结局（每例一行）
surv_outcome <- data %>%
  group_by(subject_id) %>%
  slice_max(obstime, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(subject_id, obstime, hospital_mortality)

# 5. timeData: id, time, 纵向变量
timeData <- data %>%
  select(subject_id, obstime, all_of(longitudinal_vars)) %>%
  as.data.frame()
timeData$subject_id <- as.integer(timeData$subject_id)
timeData$obstime    <- as.numeric(timeData$obstime)

# 6. fixedData: id, 固定变量
fixedData <- data %>%
  group_by(subject_id) %>%
  slice(1) %>%
  select(subject_id, anchor_age, gender_binary, atrial_fibrillation) %>%
  ungroup() %>%
  as.data.frame()
fixedData$subject_id <- as.integer(fixedData$subject_id)

# 7. Y: 生存结局
y_data <- data.frame(
  subject_id = surv_outcome$subject_id,
  time       = surv_outcome$obstime,
  event      = surv_outcome$hospital_mortality
)
Y_list <- list(Y = y_data, type = "surv")

# 8. timeVarModel（与 0313.R 一致，使用 type = "randomForest"）
timeVarModel <- list(
  heart_rate  = list(type = "randomForest"),
  resp_rate   = list(type = "randomForest"),
  systolic_bp = list(type = "randomForest"),
  spo2        = list(type = "randomForest"),
  gcs_total   = list(type = "randomForest"),
  creatinine  = list(type = "randomForest"),
  bun         = list(type = "randomForest"),
  glucose     = list(type = "randomForest"),
  sodium      = list(type = "randomForest"),
  potassium   = list(type = "randomForest"),
  wbc         = list(type = "randomForest"),
  crp         = list(type = "randomForest")
)

# 9. common_id 对齐
common_id <- Reduce(intersect, list(
  unique(timeData$subject_id),
  unique(fixedData$subject_id),
  unique(y_data$subject_id)
))
timeData   <- timeData[timeData$subject_id %in% common_id, ]
fixedData  <- fixedData[fixedData$subject_id %in% common_id, ]
Y_list$Y   <- Y_list$Y[Y_list$Y$subject_id %in% common_id, ]

cat("建模样本:", length(common_id), "例, 纵向观测", nrow(timeData), "行\n")

# 10. DynForest 建模（与 0313.R 第220-232行调用格式一致）
dynforest_params <- list(ntree = 100, mtry = 5, nodesize = 10)
dyn_model <- dynforest(
  timeData     = timeData,
  fixedData    = fixedData,
  timeVarModel = timeVarModel,
  Y            = Y_list,
  idVar        = "subject_id",
  timeVar      = "obstime",
  ntree        = dynforest_params$ntree,
  mtry         = dynforest_params$mtry,
  nodesize     = dynforest_params$nodesize,
  ncores       = 1,
  seed         = 1234,
  verbose      = TRUE
)
summary(dyn_model)
