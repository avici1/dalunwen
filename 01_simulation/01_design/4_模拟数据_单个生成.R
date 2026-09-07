library(MASS)
library(survival)
library(mvtnorm)
library(truncnorm)  # 用于偏态模拟
library(ggplot2)

options(scipen = 999)

#########################10+40连续变量生成#########################
#########################设置变量
# 构造前10个自定义变量
param_list_custom <- list(
  list(varname = "glu", mean_intercept = 5.5, mean_slope = 0.10, noise_s_sd = 0.02, noise_i_sd = 0.02),   # 血糖 ↑
  list(varname = "hb",  mean_intercept = 14,  mean_slope = -0.02, noise_s_sd = 0.02, noise_i_sd = 0.02),  # 血红蛋白 ↓
  list(varname = "wbc", mean_intercept = 7,   mean_slope = -0.05, noise_s_sd = 0.02, noise_i_sd = 0.02),  # 白细胞 ↓
  list(varname = "rbc", mean_intercept = 4.8, mean_slope = -0.01, noise_s_sd = 0.02, noise_i_sd = 0.02),  # 红细胞 ↓
  list(varname = "plt", mean_intercept = 250, mean_slope = -1.5,  noise_s_sd = 0.02, noise_i_sd = 0.02),  # 血小板 ↓
  list(varname = "sbp", mean_intercept = 120, mean_slope = 0.20,  noise_s_sd = 0.02, noise_i_sd = 0.02),  # 收缩压 ↑
  list(varname = "dbp", mean_intercept = 80,  mean_slope = 0.10,  noise_s_sd = 0.02, noise_i_sd = 0.02),  # 舒张压 ↑
  list(varname = "tc",  mean_intercept = 5.0, mean_slope = 0.03,  noise_s_sd = 0.02, noise_i_sd = 0.02),  # 总胆固醇 ↑
  list(varname = "hdl", mean_intercept = 1.3, mean_slope = 0.00,  noise_s_sd = 0.02, noise_i_sd = 0.02),  # 高密度脂蛋白 →
  list(varname = "bmi", mean_intercept = 24,  mean_slope = 0.05,  noise_s_sd = 0.02, noise_i_sd = 0.02)   # BMI ↑
)

# 构造后40个噪声变量（mean随机，slope小，噪声大）
set.seed(123)
param_list_noise <- lapply(1:40, function(i) {
  list(
    varname = paste0("X", i),
    mean_intercept = runif(1, 0, 10),        # 截距随机
    mean_slope = runif(1, -0.05, 0.05),      # 斜率小（弱趋势）
    noise_s_sd = 0.2,                        # 时间噪声大
    noise_i_sd = 0.2                         # 截距噪声大
  )
})

# 合并
param_list <- c(param_list_custom, param_list_noise)



##################循环生成50个连续变量



sim_multi_continuous <- function(n = 100, n_time = 5, param_list, seed = 123) {
  if (!is.null(seed)) set.seed(seed)
  
  # 个体和时间变量
  ID <- rep(1:n, each = n_time)
  time <- rep(1:n_time, times = n)
  
  df <- data.frame(ID = ID, time = time)
  
  # 遍历参数列表，逐个生成变量
  for (par in param_list) {
    intercepts <- rnorm(n, mean = par$mean_intercept, sd = 1)  # 个体间差异
    slopes <- rnorm(n, mean = par$mean_slope, sd = 0.01)       # 个体间斜率差异
    
    intercept_long <- rep(intercepts, each = n_time)
    slope_long <- rep(slopes, each = n_time)
    
    noise_s <- rnorm(n * n_time, mean = 0, sd = par$noise_s_sd)
    noise_i <- rnorm(n * n_time, mean = 0, sd = par$noise_i_sd)
    
    values <- intercept_long + (slope_long + noise_s) * time + noise_i ############################
    
    df[[par$varname]] <- values
  }
  
  return(df)
}

raw_continuous <- sim_multi_continuous(
  n = 200, 
  n_time = 5, 
  param_list = param_list
)
library(openxlsx)

write.xlsx(raw_continuous, "F:/文章/大论文/程序/模拟数据/raw_continuous.xlsx")

###############################################

#########################5+5分类变量生成###############
###################随时间改变的分类变量
param_list_cat <- list(
  list(varname = "infection_status",
       values = c("1", "2"),
       probs = c(0.15, 0.85)),  # 1=Positive, 2=Negative 感染检测结果
  
  list(varname = "treatment_response",
       values = c("1", "2", "3"),
       probs = c(0.3, 0.4, 0.3)),  # 1=Progressed, 2=Stable, 3=Improved 治疗反应
  
  list(varname = "blood_culture",
       values = c("1", "2"),
       probs = c(0.1, 0.9)),  # 1=Positive, 2=Negative 血培养结果
  
  list(varname = "pain_level",
       values = c("1", "2", "3", "4"),
       probs = c(0.1, 0.3, 0.4, 0.2)),  # 1=None, 2=Mild, 3=Moderate, 4=Severe 疼痛等级
  
  list(varname = "hospitalization_status",
       values = c("1", "2", "3"),
       probs = c(0.6, 0.3, 0.1))  # 1=Outpatient, 2=Inpatient, 3=ICU 就诊状态
)

sim_multi_categorical <- function(n = 100, 
                                  n_time = 5, 
                                  param_list, 
                                  seed = 123) {
  if (!is.null(seed)) set.seed(seed)
  
  # 个体和时间变量
  ID <- rep(1:n, each = n_time)
  time <- rep(1:n_time, times = n)
  
  df <- data.frame(ID = ID, time = time)
  
  # 循环生成分类变量
  for (par in param_list) {
    # 基线值（每个个体一个）
    base_values <- sample(par$values, size = n, replace = TRUE, prob = par$probs)
    # 扩展为长数据（每个个体在所有时间点保持不变）
    values_long <- rep(base_values, each = n_time)
    # 添加到数据框
    df[[par$varname]] <- values_long
  }
  
  return(df)
}

raw_categorical <- sim_multi_categorical(n = 200, n_time = 5, param_list = param_list_cat)


########################不随时间改变的分类变量
######模拟基线固定分类变量
sim_baseline_categorical <- function(n = 100, param_list, seed = 123) {
  if (!is.null(seed)) set.seed(seed)
  
  # 生成ID
  ID <- 1:n
  
  # 遍历 param_list 生成每个分类变量
  df_list <- lapply(param_list, function(par) {
    values <- par$values
    probs <- par$probs
    varname <- par$varname
    data.frame(tmp = sample(values, n, replace = TRUE, prob = probs)) |>
      setNames(varname)
  })
  
  # 合并所有变量到一个 baseline 数据框
  df_baseline <- cbind(data.frame(ID = ID), do.call(cbind, df_list))
  
  return(df_baseline)
}

param_list_baseline <- list(
  list(varname = "sex",
       values = c("1", "2"),
       probs = c(0.5, 0.5)),  # 1=Male, 2=Female
  
  list(varname = "smoking",
       values = c("1", "2"),
       probs = c(0.3, 0.7)),  # 1=Smoker, 2=Non-smoker
  
  list(varname = "alcohol",
       values = c("1", "2"),
       probs = c(0.25, 0.75)),  # 1=Drinker, 2=Non-drinker
  
  list(varname = "education",
       values = c("1", "2", "3"),
       probs = c(0.2, 0.5, 0.3)),  # 1=Primary, 2=Secondary, 3=Higher
  
  list(varname = "marital_status",
       values = c("1", "2"),
       probs = c(0.7, 0.3))  # 1=Married, 2=Single
)

raw_cate_unchange <- sim_baseline_categorical(n = 200, param_list = param_list_baseline)

write.xlsx(raw_categorical,"F:/文章/大论文/程序/模拟数据/raw_categorical.xlsx")
write.xlsx(raw_cate_unchange,"F:/文章/大论文/程序/模拟数据/raw_cate_unchange.xlsx")

###############
#########################偏态数据生成###############



param_list_skewed20 <- list(
  # Gamma 
  list(varname = "X_gamma1", dist = "gamma", shape = 2, scale = 2),
  list(varname = "X_gamma2", dist = "gamma", shape = 2, scale = 2),
  list(varname = "X_gamma3", dist = "gamma", shape = 2, scale = 2),
  list(varname = "X_gamma4", dist = "gamma", shape = 2, scale = 2),
  list(varname = "X_gamma5", dist = "gamma", shape = 2, scale = 2),
  
  # ---- 5个 Beta 
  list(varname = "X_beta1", dist = "beta", shape1 = 2, shape2 = 5),
  list(varname = "X_beta2", dist = "beta", shape1 = 2, shape2 = 5),
  list(varname = "X_beta3", dist = "beta", shape1 = 2, shape2 = 5),
  list(varname = "X_beta4", dist = "beta", shape1 = 2, shape2 = 5),
  list(varname = "X_beta5", dist = "beta", shape1 = 2, shape2 = 5),
  
  # ---- 5个 Lognormal 
  list(varname = "X_lognorm1", dist = "lognormal", meanlog = 0, sdlog = 1),
  list(varname = "X_lognorm2", dist = "lognormal", meanlog = 0, sdlog = 1),
  list(varname = "X_lognorm3", dist = "lognormal", meanlog = 0, sdlog = 1),
  list(varname = "X_lognorm4", dist = "lognormal", meanlog = 0, sdlog = 1),
  list(varname = "X_lognorm5", dist = "lognormal", meanlog = 0, sdlog = 1),
  
  # ---- 5个 Chi-squared 
  list(varname = "X_chisq1", dist = "chisq", df = 3),
  list(varname = "X_chisq2", dist = "chisq", df = 3),
  list(varname = "X_chisq3", dist = "chisq", df = 3),
  list(varname = "X_chisq4", dist = "chisq", df = 3),
  list(varname = "X_chisq5", dist = "chisq", df = 3)
)



sim_skewed <- function(n = 200, n_time = 5, param_list, seed = 123) {
  set.seed(seed)
  
  # 基础 ID & time
  ID <- rep(1:n, each = n_time)
  time <- rep(1:n_time, times = n)
  df_all <- data.frame(ID = ID, time = time)
  
  # 遍历 param_list 生成每个偏态变量
  for (par in param_list) {
    varname <- par$varname
    dist <- par$dist
    
    # 每个时间点都生成独立值
    if (dist == "lognormal") {
      values <- rlnorm(n * n_time, meanlog = par$meanlog, sdlog = par$sdlog)
    } else if (dist == "gamma") {
      values <- rgamma(n * n_time, shape = par$shape, scale = par$scale)
    } else if (dist == "beta") {
      values <- rbeta(n * n_time, shape1 = par$shape1, shape2 = par$shape2)
    } else if (dist == "chisq") {
      values <- rchisq(n * n_time, df = par$df)
    } else {
      stop(paste("未知分布:", dist))
    }
    
    df_all[[varname]] <- values
  }
  
  return(df_all)
}


raw_skewed20 <- sim_skewed(n = 200, n_time = 5, param_list = param_list_skewed20)

write.xlsx(raw_skewed20,"F:/文章/大论文/程序/模拟数据/raw_skewed20.xlsx")


###################简化变量名##########
###缩减偏态数据
shorten_names <- function(names_vec) {
  sapply(names_vec, function(x) {
    if (grepl("^X_", x)) {
      # 拆分掉 "X_"
      core <- sub("^X_", "", x)
      # 拆分字母和数字
      letters <- gsub("[0-9]", "", core)
      numbers <- gsub("[^0-9]", "", core)
      # 取前缀的首字母
      short <- substr(letters, 1, 1)
      paste0("X_", short, numbers)
    } else {
      x
    }
  })
}
names(raw_skewed20) <- shorten_names(names(raw_skewed20))


write.xlsx(raw_continuous, "F:/文章/大论文/程序/模拟数据/raw_continuous.xlsx")
write.xlsx(raw_categorical,"F:/文章/大论文/程序/模拟数据/raw_categorical.xlsx")
write.xlsx(raw_cate_unchange,"F:/文章/大论文/程序/模拟数据/raw_cate_unchange.xlsx")
write.xlsx(raw_skewed20,"F:/文章/大论文/程序/模拟数据/raw_skewed20.xlsx")




##########################生存结局模拟###############
#死亡结局
simulate_stroke_survival <- function(df, 
                                     covariates = c("X1","X2"), 
                                     beta = c(0.5, -0.3), 
                                     h0 = 0.01, 
                                     censor_max = 5, 
                                     seed = 123) {
  # 参数:
  # df: 数据框，每个患者一行，包含协变量
  # covariates: 用来生成生存结局的预测变量
  # beta: 协变量的真实效应 (log-HR)
  # h0: 基线风险 (baseline hazard)，影响整体发病率
  # censor_max: 随机删失的最大随访时间
  # seed: 随机种子，保证可重复
  
  set.seed(seed)
  
  # 线性预测
  lp <- as.matrix(df[, covariates]) %*% beta
  
  # 模拟事件时间 (指数分布，含协变量修正)
  U <- runif(nrow(df))
  event_time <- -log(U) / (h0 * exp(lp))
  
  # 模拟随机删失时间
  censor_time <- runif(nrow(df), min = 0, max = censor_max)
  
  # 观察时间与状态
  time_obs <- pmin(event_time, censor_time)
  status <- as.numeric(event_time <= censor_time)
  
  df$time <- time_obs
  df$status <- status
  
  return(df)
}

# ==== 示例运行 ====
# 构造一个假数据框
set.seed(42)
n <- 200
df_baseline <- data.frame(
  ID = 1:n,
  X1 = rnorm(n, mean = 0, sd = 1),   # 协变量1
  X2 = rbinom(n, 1, 0.4)              # 协变量2 (二元)
)

df_surv <- simulate_stroke_survival(df_baseline,
                                    covariates = c("X1","X2"),
                                    beta = c(0.8, -0.5),
                                    h0 = 0.02,
                                    censor_max = 5)

head(df_surv)




#生存时间



#生存质量















###########尝试########################





library(dplyr)
library(survival)
library(nlme)
library(joineRML) # 用于联合模型

# 设置随机种子以确保可重复性
set.seed(123)

# 从您的纵向数据中提取必要信息
n_patients <- length(unique(raw_continuous$ID))
n_time <- max(raw_continuous$time)

# 步骤1: 为每个患者计算纵向变量的斜率和截距
# 这里我们选择几个关键变量作为示例
calculate_slopes <- function(data, var_name) {
  slopes <- data %>%
    group_by(ID) %>%
    do({
      model <- lm(as.formula(paste(var_name, "~ time")), data = .)
      data.frame(slope = coef(model)["time"], intercept = coef(model)["(Intercept)"])
    }) %>%
    ungroup()
  colnames(slopes)[2:3] <- paste0(var_name, "_", c("slope", "intercept"))
  return(slopes)
}

# 计算关键变量的斜率和截距
glu_slopes <- calculate_slopes(raw_continuous, "glu")
hb_slopes <- calculate_slopes(raw_continuous, "hb")
sbp_slopes <- calculate_slopes(raw_continuous, "sbp")

# 合并所有斜率数据
slope_data <- glu_slopes %>%
  left_join(hb_slopes, by = "ID") %>%
  left_join(sbp_slopes, by = "ID")

# 步骤2: 创建基线协变量数据
# 这里我们模拟一些额外的基线变量
baseline_data <- data.frame(
  ID = 1:n_patients,
  age = rnorm(n_patients, mean = 65, sd = 10),
  gender = rbinom(n_patients, 1, 0.5),
  treatment = rbinom(n_patients, 1, 0.5),
  baseline_comorbidity = rnorm(n_patients, mean = 2, sd = 1)
)

# 步骤3: 合并所有基线数据
all_baseline_data <- baseline_data %>%
  left_join(slope_data, by = "ID")

# 步骤4: 定义联合模型参数
# 这些是关键参数，需要仔细设置
joint_model_params <- list(
  # 纵向过程参数
  longitudinal = list(
    glu = list(
      fixed = c(5.5, 0.10),  # 平均截距和斜率
      random = c(0.5, 0.01), # 随机效应的方差
      sigma = 0.02           # 残差标准差
    ),
    hb = list(
      fixed = c(14, -0.02),
      random = c(0.5, 0.01),
      sigma = 0.02
    )
  ),
  
  # 生存过程参数
  survival = list(
    baseline_hazard = list(
      type = "weibull",      # 基准风险函数类型
      shape = 1.5,           # 形状参数
      scale = 0.01           # 尺度参数
    ),
    
    # 固定效应系数
    fixed_effects = list(
      age = 0.03,            # 年龄效应
      gender = 0.2,          # 性别效应
      treatment = -0.5,      # 治疗效应
      baseline_comorbidity = 0.1  # 基线合并症效应
    ),
    
    # 关联参数 - 这是联合模型的核心
    association = list(
      glu_current_value = 0.3,    # 当前值关联强度
      glu_slope = 0.2,            # 斜率关联强度
      hb_current_value = -0.2,    # 血红蛋白当前值关联
      hb_slope = -0.15            # 血红蛋白斜率关联
    )
  )
)

# 步骤5: 模拟生存时间
# 使用联合模型的概念模拟生存时间
simulate_survival <- function(baseline_data, params) {
  n <- nrow(baseline_data)
  
  # 计算线性预测项
  baseline_data$lp <- with(baseline_data,
                           params$survival$fixed_effects$age * age +
                             params$survival$fixed_effects$gender * gender +
                             params$survival$fixed_effects$treatment * treatment +
                             params$survival$fixed_effects$baseline_comorbidity * baseline_comorbidity +
                             
                             # 关联部分 - 联合模型的核心
                             params$survival$association$glu_current_value * glu_intercept +
                             params$survival$association$glu_slope * glu_slope +
                             params$survival$association$hb_current_value * hb_intercept +
                             params$survival$association$hb_slope * hb_slope
  )
  
  # 基于威布尔分布模拟生存时间
  shape <- params$survival$baseline_hazard$shape
  scale <- params$survival$baseline_hazard$scale
  
  # 风险函数: h(t) = (shape/scale) * (t/scale)^(shape-1) * exp(lp)
  # 使用逆变换法模拟生存时间
  u <- runif(n)
  baseline_data$true_time <- (-log(u) / (scale * exp(baseline_data$lp)))^(1/shape)
  
  # 模拟随机截尾
  study_cutoff <- runif(n, 1, 5) # 研究随访时间1-5年
  baseline_data$status <- ifelse(baseline_data$true_time <= study_cutoff, 1, 0)
  baseline_data$obs_time <- pmin(baseline_data$true_time, study_cutoff)
  
  return(baseline_data)
}

# 模拟生存数据
survival_data <- simulate_survival(all_baseline_data, joint_model_params)

# 步骤6: 创建最终的分析数据集
final_data <- survival_data %>%
  select(ID, obs_time, status, age, gender, treatment, baseline_comorbidity,
         glu_slope, glu_intercept, hb_slope, hb_intercept)

# 查看模拟结果
head(final_data)

# 步骤7: 验证模拟效果
# 拟合Cox模型检查关联是否如预期
cox_model <- coxph(Surv(obs_time, status) ~ age + gender + treatment + baseline_comorbidity +
                     glu_slope + glu_intercept + hb_slope + hb_intercept,
                   data = final_data)
summary(cox_model)

# 步骤8: 合并纵向数据和生存数据
# 创建一个包含所有信息的最终数据集
longitudinal_with_survival <- raw_continuous %>%
  left_join(final_data %>% select(ID, obs_time, status), by = "ID")

# 查看最终数据集
head(longitudinal_with_survival)
























