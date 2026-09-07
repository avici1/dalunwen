#################设置相关系数矩阵##########################
# 设置参数
n_var <- 10   # 变量数
n_param <- 2  # 每个变量的参数数（截距+斜率）
dim_mat <- n_var * n_param

# 初始化矩阵
mat <- matrix(0, nrow = dim_mat, ncol = dim_mat)

# 给对角线赋值 1
diag(mat) <- 1

# 给每个变量内的截距-斜率相关系数赋一个 <0.05 的数
set.seed(123)  # 保证可复现
for (i in 1:n_var) {
  row_idx <- (i - 1) * n_param + 1
  col_idx <- row_idx + 1
  val <- runif(1, 0, 0.05)  # 随机取 (0,0.05)
  mat[row_idx, col_idx] <- val
  mat[col_idx, row_idx] <- val
}
mat <- round(mat, 3)
mat


#####


###############################
create_grouped_cov_matrix <- function(group1_vars, group2_vars, cor_ranges, seed = 666) {
  set.seed(seed)
  n_vars <- 10
  
  # 创建相关矩阵（不是协方差矩阵）
  cor_matrix <- diag(20)
  
  # 将所有变量索引转换为随机效应索引
  group1_effects <- sort(c((group1_vars * 2 - 1), (group1_vars * 2)))
  group2_effects <- sort(c((group2_vars * 2 - 1), (group2_vars * 2)))
  
  # 为所有变量对设置相关性
  for(i in 1:20) {
    for(j in 1:20) {
      if(i != j) {
        # 检查是否属于同一组
        both_in_group1 <- (i %in% group1_effects) && (j %in% group1_effects)
        both_in_group2 <- (i %in% group2_effects) && (j %in% group2_effects)
        
        if(both_in_group1 || both_in_group2) {
          # 组内相关性：随机选择正负号，然后在指定范围内采样
          sign <- sample(c(-1, 1), 1)
          if(sign == -1) {
            cor_matrix[i, j] <- runif(1, cor_ranges[1], cor_ranges[2])
          } else {
            cor_matrix[i, j] <- runif(1, cor_ranges[3], cor_ranges[4])
          }
        } else {
          # 组间低相关性
          cor_matrix[i, j] <- runif(1, -0.05, 0.05)
        }
      }
    }
  }
  
  # 确保矩阵对称
  cor_matrix <- (cor_matrix + t(cor_matrix)) / 2
  
  # 检查并确保正定性
  eigen_values <- eigen(cor_matrix)$values
  if(any(eigen_values <= 0)) {
    # 使用更稳健的方法确保正定性
    library(Matrix)
    cor_matrix <- as.matrix(nearPD(cor_matrix, corr = TRUE, keepDiag = TRUE)$mat)
  }
  
  # 将相关矩阵转换为协方差矩阵（对角线保持为1）
  # 因为我们希望随机效应的方差为1
  cov_matrix <- cor_matrix
  
  return(cov_matrix)
}

# 创建三种共线性情况的协方差矩阵
group1_vars <- 1:3  # 变量1-3
group2_vars <- 4:6  # 变量4-6

###############输出矩阵####################
#seed=123

# 低共线性: [-0.1, 0.1]
cov_matrix_low1 <- create_grouped_cov_matrix(
  group1_vars, group2_vars, c(-0.1, -0.05, 0.05, 0.1)
)

# 中等共线性: [-0.5, -0.3] 和 [0.3, 0.5]
cov_matrix_medium1 <- create_grouped_cov_matrix(
  group1_vars, group2_vars, c(-0.5, -0.3, 0.3, 0.5)
)

# 高共线性: [-0.8, -0.6] 和 [0.6, 0.8]
cov_matrix_high1 <- create_grouped_cov_matrix(
  group1_vars, group2_vars, c(-0.8, -0.6, 0.6, 0.8)
)


#seed=666

# 低共线性: [-0.1, 0.1]
cov_matrix_low2 <- create_grouped_cov_matrix(
  group1_vars, group2_vars, c(-0.1, -0.05, 0.05, 0.1)
)

# 中等共线性: [-0.5, -0.3] 和 [0.3, 0.5]
cov_matrix_medium2 <- create_grouped_cov_matrix(
  group1_vars, group2_vars, c(-0.5, -0.3, 0.3, 0.5)
)

# 高共线性: [-0.8, -0.6] 和 [0.6, 0.8]
cov_matrix_high2 <- create_grouped_cov_matrix(
  group1_vars, group2_vars, c(-0.8, -0.6, 0.6, 0.8)
)


#seed=1015

# 低共线性: [-0.1, 0.1]
cov_matrix_low3 <- create_grouped_cov_matrix(
  group1_vars, group2_vars, c(-0.1, -0.05, 0.05, 0.1)
)

# 中等共线性: [-0.5, -0.3] 和 [0.3, 0.5]
cov_matrix_medium3 <- create_grouped_cov_matrix(
  group1_vars, group2_vars, c(-0.5, -0.3, 0.3, 0.5)
)

# 高共线性: [-0.8, -0.6] 和 [0.6, 0.8]
cov_matrix_high3 <- create_grouped_cov_matrix(
  group1_vars, group2_vars, c(-0.8, -0.6, 0.6, 0.8)
)


write.xlsx(cov_matrix_low1 ,"F:/文章/大论文/程序/模拟数据/相关系数矩阵/cov_matrix_low1.xlsx")
write.xlsx(cov_matrix_medium1 ,"F:/文章/大论文/程序/模拟数据/相关系数矩阵/cov_matrix_medium1.xlsx")
write.xlsx(cov_matrix_high1 ,"F:/文章/大论文/程序/模拟数据/相关系数矩阵/cov_matrix_high1.xlsx")
write.xlsx(cov_matrix_low2 ,"F:/文章/大论文/程序/模拟数据/相关系数矩阵/cov_matrix_low2.xlsx")
write.xlsx(cov_matrix_medium2 ,"F:/文章/大论文/程序/模拟数据/相关系数矩阵/cov_matrix_medium2.xlsx")
write.xlsx(cov_matrix_high2 ,"F:/文章/大论文/程序/模拟数据/相关系数矩阵/cov_matrix_high2.xlsx")
write.xlsx(cov_matrix_low3 ,"F:/文章/大论文/程序/模拟数据/相关系数矩阵/cov_matrix_low3.xlsx")
write.xlsx(cov_matrix_medium3 ,"F:/文章/大论文/程序/模拟数据/相关系数矩阵/cov_matrix_medium3.xlsx")
write.xlsx(cov_matrix_high3 ,"F:/文章/大论文/程序/模拟数据/相关系数矩阵/cov_matrix_high3.xlsx")


path <- "F:/文章/大论文/程序/模拟数据/相关系数矩阵/"
cov_matrix_low1    <- read_excel(paste0(path, "cov_matrix_low1.xlsx"))
cov_matrix_low2    <- read_excel(paste0(path, "cov_matrix_low2.xlsx"))
cov_matrix_low3    <- read_excel(paste0(path, "cov_matrix_low3.xlsx"))

# 中相关
cov_matrix_medium1 <- read_excel(paste0(path, "cov_matrix_medium1.xlsx"))
cov_matrix_medium2 <- read_excel(paste0(path, "cov_matrix_medium2.xlsx"))
cov_matrix_medium3 <- read_excel(paste0(path, "cov_matrix_medium3.xlsx"))

# 高相关
cov_matrix_high1   <- read_excel(paste0(path, "cov_matrix_high1.xlsx"))
cov_matrix_high2   <- read_excel(paste0(path, "cov_matrix_high2.xlsx"))
cov_matrix_high3   <- read_excel(paste0(path, "cov_matrix_high3.xlsx"))


head(cov_matrix_low3)






#########################转化为协变量矩阵############################

cor_to_cov <- function(cor, std) {
  # 创建标准差对角矩阵
  D <- diag(std)
  
  # 计算协方差矩阵: Σ = D * R * D
  cov_matrix <- D %*% cor %*% D
  
  # 确保矩阵是对称的（处理可能的浮点精度问题）
  cov_matrix <- (cov_matrix + t(cov_matrix)) / 2
  
  # 添加行名和列名（如果原始矩阵有的话）
  if (!is.null(rownames(cor))) {
    rownames(cov_matrix) <- rownames(cor)
    colnames(cov_matrix) <- colnames(cor)
  }
  
  return(cov_matrix)
}





















