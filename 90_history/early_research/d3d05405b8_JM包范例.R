library("JM")
library("lattice")

aids<- aids

xyplot(sqrt(CD4) ~ obstime | drug, group = patient, data = aids,
         xlab = "Months", ylab = expression(sqrt("CD4")), col = 1, type = "l")

plot(survfit(Surv(Time, death) ~ drug, data = aids.id), conf.int = FALSE,
       mark.time = TRUE, col = c("black", "red"), lty = 1:2,
       ylab = "Survival", xlab = "Months")


td.Cox <- coxph(Surv(start, stop, event) ~ drug + sqrt(CD4), data = aids)
summary(td.Cox)








n <- nrow(aids)
aids$CD3 <- rnorm(n, mean = 5, sd = 1)




fitLME <- lme(sqrt(CD4) ~ obstime + obstime:drug, random = ~ obstime | patient, data = aids)
#构造协变量 - 时间 的轨迹 子模型
fitSURV <- coxph(Surv(Time, death) ~ drug, data = aids.id, x = TRUE)
#构造生存子模型
fit.JM <- jointModel(fitLME, fitSURV, timeVar = "obstime",method = "piecewise-PH-GH")
#构建JM模型




fitLME1 <- lme(cbind(sqrt(CD4),CD3) ~ obstime + obstime:drug, random = ~ obstime | patient, data = aids)
#构造协变量 - 时间 的轨迹 子模型
fitSURV1 <- coxph(Surv(Time, death) ~ drug, data = aids.id, x = TRUE)
#构造生存子模型
fit.JM1 <- jointModel(fitLME, fitSURV, timeVar = "obstime",method = "piecewise-PH-GH")


fitLME2 <- lme(cbind(sqrt(CD4), CD3) ~ obstime + obstime:drug + obstime:CD3,
               random = ~obstime | patient, data = aids)


joint_model <- jointModel(list(fitLME1, fitLME),fitSURV, timeVar = "obstime",method = "piecewise-PH-GH")




summary(fitLME1)
summary(fitLME)
summary(fitLME2)




summary(fit.JM)
AIC(fit.JM)
BIC(fit.JM)


set.seed(123)
ND <- aids[aids$patient %in% c("7", "15", "117", "303"), ]
predSurv <- survfitJM(fit.JM, newdata = ND, idVar = "patient",
                           last.time = "Time")
predSurv










###########################MCMC在R的实现###########################
install.packages("rstan")
library(rstan)



set.seed(123)
data <- aids  # 假设这是你的数据框

# 生存数据：时间和事件（死亡）
time <- data$Time
event <- data$death

# 纵向数据：CD4计数
y <- sqrt(data$CD4)  # 纵向数据

# 协变量：药物类型（drug）
drug <- as.factor(data$drug)

# 设置样本大小和时间变量
n <- nrow(data)
obstime <- data$obstime  # 纵向数据的时间



model_string <- "
data {
  int<lower=0> n;            // 样本大小
  vector[n] time;            // 生存时间
  int<lower=0, upper=1> event; // 事件指示变量（0=未发生，1=发生）
  vector[n] y;               // 纵向数据（如CD4计数）
  vector[n] obstime;         // 纵向数据时间
  int<lower=0, upper=1> drug; // 药物类型（0=其他药物，1=ddI）
}

parameters {
  real beta0;                // 生存模型的截距
  real beta1;                // 生存模型药物ddI的效应
  real beta2;                // 生存模型CD4变化的效应
  real alpha;                // 纵向模型的截距
  real beta_longitudinal;    // 纵向模型药物效应
  real tau;                  // 纵向数据的精度（tau = 1/σ²）
  real<lower=0> lambda;      // 生存模型的比例风险参数
}

model {
  // 生存部分：Cox比例风险模型
  for (i in 1:n) {
    event[i] ~ bernoulli_logit(beta0 + beta1 * drug[i] + beta2 * y[i]);
    time[i] ~ exponential(lambda * exp(beta0 + beta1 * drug[i] + beta2 * y[i]));
  }

  // 纵向部分：线性混合效应模型
  for (i in 1:n) {
    y[i] ~ normal(alpha + beta_longitudinal * drug[i] + obstime[i], tau);
  }

  // 先验分布
  beta0 ~ normal(0, 10);
  beta1 ~ normal(0, 10);
  beta2 ~ normal(0, 10);
  alpha ~ normal(0, 10);
  beta_longitudinal ~ normal(0, 10);
  tau ~ gamma(0.1, 0.1);
  lambda ~ gamma(0.1, 0.1); // 生存模型的比例风险参数先验
}
"


stan_data <- list(
  n = n,
  time = time,
  event = event,
  y = y,
  obstime = obstime,
  drug = as.integer(drug)  # 转换为数值形式，0和1
)

# 编译Stan模型
stan_model <- stan_model(model_code = model_string)

# 使用MCMC进行采样
fit <- sampling(stan_model, data = stan_data, iter = 2000, chains = 4, warmup = 1000)

# 查看模型的估计结果
print(fit)
#################################################################################


###########################最大似然法填补 范例###########################

install.packages("lavaan")
library(lavaan)

set.seed(123)
n <- 100
data <- data.frame(
  x1 = rnorm(n),
  x2 = rnorm(n),
  x3 = rnorm(n)
)

# 让数据中部分值缺失
data$x2[sample(1:n, size = 20)] <- NA  # 随机设置20个缺失值
data$x3[sample(1:n, size = 30)] <- NA  # 随机设置30个缺失值

# 查看数据
head(data)

# 创建一个线性模型（假设x1对x2和x3有影响）
model <- '
  # 回归模型
  x2 ~ 2*x1
  x3 ~ 2*x1
'

# 使用lavaan包拟合模型，指定估计方法为最大似然法（默认方法）
fit <- sem(model, data = data, missing = "ML")

# 查看模型结果
summary(fit)

fit_hi<-fit@h1
#####################################################################


#############################模拟数据########################################

set.seed(123)

# 模拟患者数
n <- 100

# 患者年龄和治疗组 (0 = 控制组, 1 = 治疗组)
age <- rnorm(n, 60, 10)           # 平均60岁，标准差10
treatment <- sample(0:1, n, replace = TRUE)  # 随机分配治疗组

# 生物标志物随时间的变化
biomarker_intercept <- 50      # 基线水平
biomarker_slope <- -0.2       # 随时间减少的趋势
u <- rnorm(n, 0, 5)           # 随机效应

# 每个患者的生物标志物变化
time <- rep(1:10, each = n)    # 10个时间点
biomarker <- biomarker_intercept + biomarker_slope * time + rep(u, each = 10) + rnorm(n * 10, 0, 2)

# 生存时间的模拟，使用Cox模型
lambda_0 <- 0.05  # 基础风险
beta_age <- 0.03  # 年龄对生存的影响
beta_treatment <- -0.5  # 治疗对生存的影响

# 计算每个患者的生存时间
log_hazard <- log(lambda_0) + beta_age * age + beta_treatment * treatment
survival_time <- rexp(n, exp(log_hazard))

# 模拟数据
data <- data.frame(patient_id = rep(1:n, each = 10),
                   time = time,
                   biomarker = biomarker,
                   age = rep(age, each = 10),
                   treatment = rep(treatment, each = 10),
                   survival_time = rep(survival_time, each = 10))













#模拟生存风险
log_hazard <- log(lambda_0) + beta_age * age + beta_treatment * treatment +
  beta_biomaker1*biomaker1 + beta_biomaker2*biomaker2
  
#增加交互作用
log_hazard <- log(lambda_0) + beta_age * age + beta_treatment * treatment + 
  beta_biomaker1*biomaker1 + beta_biomaker2*biomaker2 +
  beta_interaction * (biomarker1 * biomarker2)



############AFT
log_hazard <- log(lambda_0) + beta_age * age + beta_treatment * treatment
# 生成对数正态分布的误差项（log-normal error term）
epsilon <- rnorm(n, mean = 0, sd = sigma)
# 模拟生存时间（AFT模型）
survival_time <- exp(log_hazard + epsilon)
############

#cox分布
survival_time <- rexp(n, rate = exp(log_hazard))


shape <- 2   # Weibull的形状参数
scale <- exp(log_hazard)  # Weibull的尺度参数

# 生成weibuill分布的生存时间
survival_time_weibull <- rweibull(n, shape = shape, scale = scale)


#######################################################################################

atest<-as.data.frame(rexp(10, 1.5))



