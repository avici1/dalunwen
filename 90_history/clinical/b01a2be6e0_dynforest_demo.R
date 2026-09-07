# DynForest 单次建模示例（使用包自带 pbc2 数据）
library(DynForest)

# 使用 pbc2 示例数据
data(pbc2)
pbc2$serBilir  <- log(pbc2$serBilir)
pbc2$SGOT      <- log(pbc2$SGOT)
pbc2$albumin   <- log(pbc2$albumin)
pbc2$alkaline  <- log(pbc2$alkaline)

# 取 100 例用于建模
set.seed(1234)
id_sample <- sample(unique(pbc2$id), 100)
pbc2_train <- pbc2[pbc2$id %in% id_sample, ]

# 纵向数据：id, time, 纵向变量
timeData <- pbc2_train[, c("id", "time", "serBilir", "SGOT", "albumin", "alkaline")]

# 基线数据：仅 id + 基线预测变量（不含 time、event）
fixedData <- unique(pbc2_train[, c("id", "age", "drug", "sex")])
fixedData$drug <- as.factor(fixedData$drug)
fixedData$sex <- as.factor(fixedData$sex)

# 结局：id, time, event
Y <- list(
  type = "surv",
  Y = unique(pbc2_train[, c("id", "years", "event")])
)

# timeVarModel：每个纵向变量一个公式
timeVarModel <- list(
  serBilir  = list(fixed = serBilir ~ time,  random = ~ time),
  SGOT      = list(fixed = SGOT ~ time + I(time^2), random = ~ time + I(time^2)),
  albumin   = list(fixed = albumin ~ time,   random = ~ time),
  alkaline  = list(fixed = alkaline ~ time,  random = ~ time)
)

# 建模（cause=2 表示死亡事件）
dyn_model <- DynForest::dynforest(
  timeData     = timeData,
  fixedData    = fixedData,
  idVar        = "id",
  timeVar      = "time",
  timeVarModel = timeVarModel,
  Y            = Y,
  ntree        = 50,
  mtry         = 3,
  nodesize     = 5,
  minsplit     = 5,
  cause        = 2,
  ncores       = 1,
  seed         = 1234
)

summary(dyn_model)
