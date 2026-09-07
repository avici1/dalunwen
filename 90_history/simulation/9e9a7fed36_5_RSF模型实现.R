library(randomForestSRC)
library(readxl)


############建模尝试#################
sim1000_70_10V_highINTER_3c <- read_xlsx("F:/文章/大论文/程序/模拟数据/数据_withY/sim1000_70_10V_highINTER_3c_1.xlsx")

dat<- sim1000_70_10V_highINTER_3c 



fit_rsf <- rfsrc(
  Surv(obs_time, event) ~ ., 
  data = dat[, c(paste0("V", 1:10, c("_0.55","_0.80","_0.30","_0.90",
                                     "_0.25","_0.70","_0.40","_0.95",
                                     "_0.15","_0.60")), 
                 "obs_time","event")],
  ntree = 500,
  importance = TRUE,
  na.action = "na.impute"
)


print(fit_rsf)
plot(fit_rsf)
plot.survival(fit_rsf, subset = 1:10)
vimp_res <- fit_rsf$importance
print(vimp_res)




#############KIMI####
dat <- as.data.frame(sim1000_70_10V_lowINTER_1c_L1[[1]])


rfsrc_fit <- rfsrc(
  Surv(obs_time, event) ~ .,
  data = dat[, c(grep("^V[0-9]", names(dat), value = TRUE),
                 "obs_time", "event")],
  ntree      = 1000,
  mtry       = floor(length(grep("^V[0-9]", names(dat), value = TRUE)) / 3),
  nodesize   = 10,
  importance = TRUE,
  proximity  = FALSE,
  seed       = 123
)




############DS##############
# 假设您的数据存储在 sim1000_70_10V_highINTER_3c[[1]] 中
data <- sim1000_70_10V_highINTER_3c[[1]]

# 查看数据结构
str(data)
head(data)

# 基本随机生存森林模型

# 方法1：使用所有变量（排除非特征变量）
# 选择特征变量（V1-V10）
feature_vars <- grep("^V[0-9]+", names(data), value = TRUE)
formula_all <- as.formula(paste("Surv(obs_time, event) ~", paste(feature_vars, collapse = " + ")))

# 拟合基本模型
rfsrc_fit_basic <- rfsrc(
  formula = formula_all,
  data = data,
  ntree = 500,           # 树的数量
  mtry = NULL,           # 默认mtry
  nodesize = 15,         # 终端节点最小样本数
  splitrule = "logrank", # 分割规则
  importance = TRUE,     # 计算变量重要性
  seed = 123             # 设置随机种子
)


# 模型性能评估

# 1. 绘制误差曲线
plot(rfsrc_fit_basic)

# 2. 变量重要性图
vimp_plot <- plot.variable(rfsrc_fit_basic)
print(vimp_plot)

# 3. 获取变量重要性分数
vimp_scores <- rfsrc_fit_basic$importance
print(sort(vimp_scores, decreasing = TRUE))

# 4. 预测生存曲线
# 对新数据进行预测（这里用训练数据演示）
predicted <- predict(rfsrc_fit_basic, newdata = data)

# 绘制前10个样本的生存曲线
plot.survival(rfsrc_fit_basic, subset = 1:10)






rfsrc_fit_tuned <- rfsrc(
  formula = formula_all,
  data = data,
  ntree = 1000,          # 增加树的数量
  mtry = floor(sqrt(length(feature_vars))), # 手动设置mtry
  nodesize = 5,          # 更小的节点大小
  nsplit = 10,           # 随机分割点数量
  splitrule = "logrank", # 分割规则
  importance = "permute", # 使用置换重要性
  bootstrap = "by.root", # 自助采样方法
  seed = 123
)

cal_brier <- function(data, time_point = 1) {
  # 加载必要的包
  if (!require(randomForestSRC)) {
    install.packages("randomForestSRC")
    library(randomForestSRC)
  }
  
  # 选择特征变量（假设特征变量以"V"开头）
  feature_vars <- grep("^V[0-9]+", names(data), value = TRUE)
  
  # 构建公式
  formula_all <- as.formula(paste("Surv(obs_time, event) ~", paste(feature_vars, collapse = " + ")))
  
  # 使用指定参数拟合随机生存森林模型
  rfsrc_fit_tuned <- rfsrc(
    formula = formula_all,
    data = data,
    ntree = 1000,
    mtry = floor(sqrt(length(feature_vars))),
    nodesize = 5,
    nsplit = 10,
    splitrule = "logrank",
    importance = "permute",
    bootstrap = "by.root",
    seed = 123
  )
  
  # 计算Brier分数
  brier_score <- get.brier.survival(rfsrc_fit_tuned, cens.model = "km")
  time_points <- brier_score$brier.score[, 1]
  brier_values <- brier_score$brier.score[, 2]
  
  # 找到最接近指定时间点的Brier分数
  closest_index <- which.min(abs(time_points - time_point))
  closest_time <- time_points[closest_index]
  brier_at_t <- brier_values[closest_index]
  
  # 如果精确匹配，直接返回；否则使用线性插值
  if (closest_time == time_point) {
    result <- round(brier_at_t, 4)
    cat("在时间 t =", time_point, "时的Brier分数:", result, "\n")
  } else {
    # 线性插值
    lower_indices <- which(time_points <= time_point)
    upper_indices <- which(time_points >= time_point)
    
    if (length(lower_indices) > 0 && length(upper_indices) > 0) {
      lower_index <- max(lower_indices)
      upper_index <- min(upper_indices)
      
      if (lower_index != upper_index) {
        time_lower <- time_points[lower_index]
        time_upper <- time_points[upper_index]
        brier_lower <- brier_values[lower_index]
        brier_upper <- brier_values[upper_index]
        
        # 线性插值公式
        brier_interpolated <- brier_lower + (brier_upper - brier_lower) * 
          (time_point - time_lower) / (time_upper - time_lower)
        
        result <- round(brier_interpolated, 4)
        cat("通过线性插值，在时间 t =", time_point, "时的Brier分数:", result, "\n")
      } else {
        result <- round(brier_at_t, 4)
        cat("在最近时间点 t =", closest_time, "时的Brier分数:", result, "\n")
      }
    } else {
      result <- round(brier_at_t, 4)
      cat("在最近时间点 t =", closest_time, "时的Brier分数:", result, "\n")
    }
  }
  
  # 返回Brier分数数值
  return(result)
}
brier_score <- cal_brier(data = sim1000_70_10V_highINTER_3c[[1]])
print(brier_score)
















cal_brier_enhanced <- function(data, time_point = 1, method = "closest") {
  # 加载必要的包
  if (!require(randomForestSRC)) {
    install.packages("randomForestSRC")
    library(randomForestSRC)
  }
  
  # 选择特征变量
  feature_vars <- grep("^V[0-9]+", names(data), value = TRUE)
  
  # 构建公式
  formula_all <- as.formula(paste("Surv(obs_time, event) ~", paste(feature_vars, collapse = " + ")))
  
  # 拟合模型
  rfsrc_fit_tuned <- rfsrc(
    formula = formula_all,
    data = data,
    ntree = 1000,
    mtry = floor(sqrt(length(feature_vars))),
    nodesize = 5,
    nsplit = 10,
    splitrule = "logrank",
    importance = "permute",
    bootstrap = "by.root",
    seed = 123
  )
  
  # 计算Brier分数
  brier_score <- get.brier.survival(rfsrc_fit_tuned, cens.model = "km")
  time_points <- brier_score$brier.score[, 1]
  brier_values <- brier_score$brier.score[, 2]
  
  # 根据方法选择计算方式
  if (method == "closest") {
    # 使用最接近的时间点
    closest_index <- which.min(abs(time_points - time_point))
    closest_time <- time_points[closest_index]
    result <- round(brier_values[closest_index], 4)
    cat("使用最接近时间点 t =", closest_time, "的Brier分数:", result, "\n")
  } else if (method == "interpolate") {
    # 使用线性插值
    lower_indices <- which(time_points <= time_point)
    upper_indices <- which(time_points >= time_point)
    
    if (length(lower_indices) > 0 && length(upper_indices) > 0) {
      lower_index <- max(lower_indices)
      upper_index <- min(upper_indices)
      
      if (lower_index != upper_index) {
        time_lower <- time_points[lower_index]
        time_upper <- time_points[upper_index]
        brier_lower <- brier_values[lower_index]
        brier_upper <- brier_values[upper_index]
        
        # 线性插值
        brier_interpolated <- brier_lower + (brier_upper - brier_lower) * 
          (time_point - time_lower) / (time_upper - time_lower)
        
        result <- round(brier_interpolated, 4)
        cat("使用线性插值，在时间 t =", time_point, "时的Brier分数:", result, "\n")
      } else {
        result <- round(brier_values[lower_index], 4)
        cat("精确匹配时间点 t =", time_points[lower_index], "的Brier分数:", result, "\n")
      }
    } else {
      closest_index <- which.min(abs(time_points - time_point))
      closest_time <- time_points[closest_index]
      result <- round(brier_values[closest_index], 4)
      cat("无法插值，使用最接近时间点 t =", closest_time, "的Brier分数:", result, "\n")
    }
  }
  
  # 返回Brier分数
  return(result)
}

# 使用示例
brier_closest <- cal_brier_enhanced(data = sim1000_70_10V_highINTER_3c[[1]], method = "closest")
brier_interp <- cal_brier_enhanced(data = sim1000_70_10V_highINTER_3c[[1]], method = "interpolate")


library(survival)
brier_res <- pec::pec(
  object = list("RSF" = rfsrc_fit_tuned),
  formula = Surv(obs_time, event) ~ 1,
  data = data,
  times = 1,          # 指定时间点
  cens.model = "cox", # 删失模型，用 Cox 拟合
  exact = TRUE,
  splitMethod = "none"
)









