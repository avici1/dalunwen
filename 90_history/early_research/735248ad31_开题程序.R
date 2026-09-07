install.packages("rpart")
install.packages("rpart.plot")
install.packages("readxl")
install.packages("utf8")
library(rpart.plot)
library(rpart)
library(readxl)

data(iris)
cart_model <- rpart(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, 
                    data = iris, method = "class")
summary(cart_model)

rpart.plot(cart_model)


test<-read_xlsx("F:/文章/大论文/程序/开题/开题.xlsx")
# 打印模型摘要

cart_model1 <- rpart(loan ~ age + job + house + credit, 
                    data = test, method = "class")
summary(cart_model1)

# 可视化决策树
rpart.plot(cart_model1)



test1<-read_xlsx("F:/文章/大论文/程序/开题/test1.xlsx")

cart_model2 <- rpart(loan ~ age + job + house + credit, 
                     data = test1, method = "class")
summary(cart_model1)

# 可视化决策树
rpart.plot(cart_model2)





###################模拟数据#####################
set.seed(123)  # 为了保证结果的可复现性

# 生成模拟数据
n <-90  # 模拟数据的行数

# 生成age列：18到65之间的随机整数
age <- sample(18:65, n, replace = TRUE)

# 生成job列：随机选择"yes"或"no"
job <- sample(c("yes", "no"), n, replace = TRUE)

# 生成house列：随机选择"yes"或"no"
house <- sample(c("yes", "no"), n, replace = TRUE)

# 生成credit列：1到3之间的随机整数
credit <- sample(1:3, n, replace = TRUE)

# 生成loan列：根据其他变量生成目标变量loan的值，随机赋值"yes"或"no"
# 假设loan与age、job、house、credit之间有某些关系（这里假设与job和credit有关）
loan <- ifelse(job == "yes" & credit == 1, "no", 
               ifelse(job == "no" & credit == 3, "yes", 
                      sample(c("yes", "no"), n, replace = TRUE)))
test3 <- data.frame(age = age, job = job, house = house, credit = credit, loan = factor(loan))
test3$loan <- ifelse(test3$loan == "yes", 1, 0)

cart_model3 <- rpart(loan ~ age + job + house + credit, 
                     data = test3, method = "class")
rpart.plot(cart_model3)

pruned_cart_model <- prune(cart_model3, cp = 0.02)
rpart.plot(pruned_cart_model)

