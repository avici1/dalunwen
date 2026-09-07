options(scipen = 999)
options(pillar.width = Inf)

library(randomForestSRC)



# ============================================================
# R语言 randomForestSRC 包 - 主功能示例 (带详细中文注释)
# ============================================================
# 主要涵盖：
# 1. 生存分析(Random Survival Forest)
# 2. 特征重要性与VIMP分析
# 3. 缺失值插补
# 4. 与Cox回归比较
# 5. 竞争风险模型
# 6. 回归与分类任务
# 7. 不平衡样本、非监督分析、双变量/多变量分析
# ============================================================

#------------------------------------------------------------
## 1. 生存分析 (Random Survival Forest)
#------------------------------------------------------------

# veteran 数据：肺癌两种治疗方案的随机试验
data(veteran, package = "randomForestSRC")

# 建立随机生存森林模型
v.obj <- rfsrc(Surv(time, status) ~ ., data = veteran, block.size = 1)

# 绘制第3棵树的结构
plot(get.tree(v.obj, 66))

# 打印训练结果摘要
print(v.obj)

# 绘制整体模型误差随树数变化的图
plot(v.obj)

# 绘制前10个个体的生存曲线（直接提取方式）
matplot(v.obj$time.interest, 100 * t(v.obj$survival.oob[1:10, ]),
        xlab = "Time", ylab = "Survival (%)", type = "l", lty = 1)

# 绘制前10个个体的生存曲线（使用封装函数）
plot.survival(v.obj, subset = 1:10)

# 计算Brier分数（使用KM与RSF两种删失模型）
bs.km  <- get.brier.survival(v.obj, cens.model = "km")$brier.score
bs.rsf <- get.brier.survival(v.obj, cens.model = "rfsrc")$brier.score

# 绘制Brier Score曲线比较模型性能
plot(bs.km, type = "s", col = 2)
lines(bs.rsf, type ="s", col = 4)
legend("topright", legend = c("KM删失", "RSF删失"), fill = c(2,4))

# 计算并绘制CRPS（连续秩概率得分）曲线
trapz <- randomForestSRC:::trapz
time <- v.obj$time.interest
crps.km <- sapply(1:length(time), function(j) {
  trapz(time[1:j], bs.km[1:j, 2] / diff(range(time[1:j])))
})
crps.rsf <- sapply(1:length(time), function(j) {
  trapz(time[1:j], bs.rsf[1:j, 2] / diff(range(time[1:j])))
})
plot(time, crps.km, ylab = "CRPS", type = "s", col = 2)
lines(time, crps.rsf, type ="s", col = 4)
legend("bottomright", legend=c("KM删失", "RSF删失"), fill=c(2,4))

# 自动调参：节点最小样本数 (nodesize)
tune.nodesize(Surv(time,status) ~ ., veteran)


#------------------------------------------------------------
## 2. 另一生存数据集：PBC肝硬化数据
#------------------------------------------------------------

data(pbc, package = "randomForestSRC")
pbc.obj <- rfsrc(Surv(days, status) ~ ., pbc)
print(pbc.obj)

# 大规模树生长示例，演示save.memory选项（减少内存占用）
print(rfsrc(Surv(days, status) ~ ., pbc, splitrule = "random",
            ntree = 25000, nodesize = 1, save.memory = TRUE))


#------------------------------------------------------------
## 3. 绘制任意模型的树结构
#------------------------------------------------------------
data(veteran, package = "randomForestSRC")
vd <- veteran
vd$celltype=factor(vd$celltype)
vd$diagtime=factor(vd$diagtime)
vd.obj <- rfsrc(Surv(time,status)~., vd, ntree = 100, nodesize = 5)
plot(get.tree(vd.obj, 3))


#------------------------------------------------------------
## 4. 分类任务示例（iris数据）
#------------------------------------------------------------

iris.obj <- rfsrc(Species ~., data = iris)
plot(get.tree(iris.obj, 25, class.type = "bayes"))


#------------------------------------------------------------
## 5. 特征重要性（VIMP）分析
#------------------------------------------------------------

# 直接从模型提取
print(rfsrc(Species~.,iris,importance=TRUE)$importance)

# 使用不同的性能度量（如brier误差）
print(rfsrc(Species~.,iris,importance=TRUE,perf.type="brier")$importance)

# 使用vimp函数单独计算
iris.obj <- rfsrc(Species ~., data = iris)
print(vimp(iris.obj)$importance)

# 使用保留样本计算holdout重要性
print(holdout.vimp(Species~.,iris)$importance)


#------------------------------------------------------------
## 6. 子抽样计算VIMP置信区间
#------------------------------------------------------------

o <- rfsrc(Ozone ~ ., data = airquality)
so <- subsample(o)
plot(so)


#------------------------------------------------------------
## 7. 缺失值插补（Imputation）
#------------------------------------------------------------

data(pbc, package = "randomForestSRC")
pbc.obj2 <- rfsrc(Surv(days, status) ~ ., pbc, na.action = "na.impute")
pbc.obj3 <- rfsrc(Surv(days, status) ~ ., pbc, na.action = "na.impute", nimpute = 3)
pbc.imp  <- impute(Surv(days, status) ~ ., pbc, splitrule = "random")


#------------------------------------------------------------
## 8. 与Cox比例风险模型性能比较
#------------------------------------------------------------
# 使用 pec包计算Brier Score与C-index性能对比
if (library("survival", logical.return = TRUE)
    & library("pec", logical.return = TRUE)
    & library("prodlim", logical.return = TRUE)) {
  
  data(pbc, package = "randomForestSRC")
  pbc.na <- na.omit(pbc)
  surv.f <- as.formula(Surv(days, status) ~ .)
  pec.f  <- as.formula(Hist(days,status) ~ 1)
  
  # 拟合Cox和RSF模型
  cox.obj   <- coxph(surv.f, data = pbc.na, x = TRUE)
  rfsrc.obj <- rfsrc(surv.f, pbc.na, ntree = 150)
  
  # 交叉验证估计Brier Score
  set.seed(17743)
  prederror.pbc <- pec(list(cox.obj,rfsrc.obj), data = pbc.na, formula = pec.f,
                       splitMethod = "bootcv", B = 50)
  plot(prederror.pbc)
  
  # 计算C-index比较
  rfsrc.obj <- rfsrc(surv.f, pbc.na)
  cat("\n\tOOB error rates\n\n")
  cat("\tRSF            : ", rfsrc.obj$err.rate[rfsrc.obj$ntree], "\n")
}


#------------------------------------------------------------
## 9. 竞争风险模型 (Competing Risks)
#------------------------------------------------------------

data(wihs, package = "randomForestSRC")
wihs.obj <- rfsrc(Surv(time, status) ~ ., wihs, nsplit = 3, ntree = 100)
plot.competing.risk(wihs.obj)


#------------------------------------------------------------
## 10. 回归任务示例
#------------------------------------------------------------

# 空气质量数据回归
airq.obj <- rfsrc(Ozone ~ ., data = airquality, na.action = "na.impute")
plot.variable(airq.obj, partial = TRUE, smooth.lines = TRUE)

# 汽车数据集
mtcars.obj <- rfsrc(mpg ~ ., data = mtcars)


#------------------------------------------------------------
## 11. 不平衡样本分类 (Balanced Random Forest)
#------------------------------------------------------------

data(breast, package = "randomForestSRC")
breast <- na.omit(breast)
ob <- imbalanced(status ~ ., data = breast, method = "brf")
print(get.imbalanced.performance(ob))


#------------------------------------------------------------
## 12. 非监督学习 (Unsupervised RF)
#------------------------------------------------------------

mtcars.unspv <- rfsrc(Unsupervised() ~., data = mtcars)
mtcars.sid   <- sidClustering(mtcars, k = 1:10)
print(split(mtcars, mtcars.sid$cl[, 3]))


#------------------------------------------------------------
## 13. 双变量回归 (Mahalanobis Splitting)
#------------------------------------------------------------

if (library("mlbench", logical.return = TRUE)) {
  data(BostonHousing)
  bh.mreg <- rfsrc(Multivar(lstat, nox) ~ ., BostonHousing,
                   importance = TRUE, splitrule = "mahal")
}


#------------------------------------------------------------
## 14. 多变量混合模型 (Nutrigenomic study)
#------------------------------------------------------------

data(nutrigenomic, package = "randomForestSRC")
ydta <- data.frame(diet = nutrigenomic$diet,
                   genotype = nutrigenomic$genotype,
                   nutrigenomic$lipids)
mv.obj <- rfsrc(get.mv.formula(colnames(ydta)),
                data.frame(ydta, nutrigenomic$genes),
                importance = TRUE, nsplit = 10)
print(mv.obj, outcome.target = "diet")


#------------------------------------------------------------
## 15. 自定义分裂规则 (Custom Splitting)
#------------------------------------------------------------

mtcars.obj <- rfsrc(mpg ~ ., data = mtcars, splitrule = "custom")
iris.obj   <- rfsrc(Species ~., data = iris, splitrule = "custom1")
