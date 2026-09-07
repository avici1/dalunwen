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
create_grouped_cov_matrix <- function(group1_vars, group2_vars, 
                                      cor_range, 
                                      cor_range_inter = c(0, 0.5), 
                                      seed = 666) {
  set.seed(seed)
  
  n_vars <- 20   # 10个变量 × int/slp
  cor_matrix <- diag(n_vars)
  
  # 变量索引转换为截距/斜率索引
  group1_effects <- sort(c((group1_vars * 2 - 1), (group1_vars * 2)))
  group2_effects <- sort(c((group2_vars * 2 - 1), (group2_vars * 2)))
  
  # 遍历填充
  for (i in 1:n_vars) {
    for (j in 1:n_vars) {
      if (i != j) {
        
        # 情况1: int-slp 同一变量对 (1-2, 3-4, ..., 19-20)
        if (ceiling(i/2) == ceiling(j/2)) {
          cor_matrix[i, j] <- runif(1, cor_range_inter[1], cor_range_inter[2])
          
          # 情况2: 组内高相关（V1-3组, V4-6组）
        } else {
          both_in_group1 <- (i %in% group1_effects) && (j %in% group1_effects)
          both_in_group2 <- (i %in% group2_effects) && (j %in% group2_effects)
          
          if (both_in_group1 || both_in_group2) {
            cor_matrix[i, j] <- runif(1, cor_range[1], cor_range[2])
          } else {
            # 情况3: 组间低相关
            cor_matrix[i, j] <- runif(1, 0, 0.1)
          }
        }
      }
    }
  }
  
  # 对称化
  cor_matrix <- (cor_matrix + t(cor_matrix)) / 2
  
  # 确保正定
  eigen_values <- eigen(cor_matrix)$values
  if (any(eigen_values <= 0)) {
    library(Matrix)
    cor_matrix <- as.matrix(nearPD(cor_matrix, corr = TRUE, keepDiag = TRUE)$mat)
  }
  
  return(cor_matrix)
}

# 示例：高共线性 (0.7~0.9)，int-slp相关性 (0~0.5)
group1_vars <- 1:3
group2_vars <- 4:6

#高影响
cov_matrix_high1 <- create_grouped_cov_matrix(
  group1_vars, group2_vars, 
  cor_range = c(0.7, 0.9), 
  cor_range_inter = c(0, 0.5),
  seed=123
)
cov_matrix_high2 <- create_grouped_cov_matrix(
  group1_vars, group2_vars, 
  cor_range = c(0.7, 0.9), 
  cor_range_inter = c(0, 0.5),
  seed=666
)
cov_matrix_high3 <- create_grouped_cov_matrix(
  group1_vars, group2_vars, 
  cor_range = c(0.7, 0.9), 
  cor_range_inter = c(0, 0.5),
  seed=1015
)
#中影响
cov_matrix_medium1 <- create_grouped_cov_matrix(
  group1_vars, group2_vars, 
  cor_range = c(0.3, 0.7), 
  cor_range_inter = c(0, 0.5),
  seed=123
)
cov_matrix_medium2 <- create_grouped_cov_matrix(
  group1_vars, group2_vars, 
  cor_range = c(0.3, 0.7), 
  cor_range_inter = c(0, 0.5),
  seed=666
)
cov_matrix_medium3 <- create_grouped_cov_matrix(
  group1_vars, group2_vars, 
  cor_range = c(0.3, 0.7), 
  cor_range_inter = c(0, 0.5),
  seed=1015
)

#低影响
cov_matrix_low1 <- create_grouped_cov_matrix(
  group1_vars, group2_vars, 
  cor_range = c(0.05, 0.3), 
  cor_range_inter = c(0, 0.5),
  seed=123
)
cov_matrix_low2 <- create_grouped_cov_matrix(
  group1_vars, group2_vars, 
  cor_range = c(0.05, 0.3), 
  cor_range_inter = c(0, 0.5),
  seed=666
)
cov_matrix_low3 <- create_grouped_cov_matrix(
  group1_vars, group2_vars, 
  cor_range = c(0.05, 0.3), 
  cor_range_inter = c(0, 0.5),
  seed=1015
)

library(openxlsx)
write.xlsx(cov_matrix_low1 ,"F:/文章/大论文/程序/模拟数据/相关系数矩阵/cov_matrix_low1.xlsx")
write.xlsx(cov_matrix_medium1 ,"F:/文章/大论文/程序/模拟数据/相关系数矩阵/cov_matrix_medium1.xlsx")
write.xlsx(cov_matrix_high1 ,"F:/文章/大论文/程序/模拟数据/相关系数矩阵/cov_matrix_high1.xlsx")
write.xlsx(cov_matrix_low2 ,"F:/文章/大论文/程序/模拟数据/相关系数矩阵/cov_matrix_low2.xlsx")
write.xlsx(cov_matrix_medium2 ,"F:/文章/大论文/程序/模拟数据/相关系数矩阵/cov_matrix_medium2.xlsx")
write.xlsx(cov_matrix_high2 ,"F:/文章/大论文/程序/模拟数据/相关系数矩阵/cov_matrix_high2.xlsx")
write.xlsx(cov_matrix_low3 ,"F:/文章/大论文/程序/模拟数据/相关系数矩阵/cov_matrix_low3.xlsx")
write.xlsx(cov_matrix_medium3 ,"F:/文章/大论文/程序/模拟数据/相关系数矩阵/cov_matrix_medium3.xlsx")
write.xlsx(cov_matrix_high3 ,"F:/文章/大论文/程序/模拟数据/相关系数矩阵/cov_matrix_high3.xlsx")

library(readxl)

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






#########################阵############################

create_grouped_cov_matrix <- function(group1_vars, group2_vars, 
                                      cor_range, 
                                      cor_range_inter = c(0, 0.5), 
                                      seed = 666) {
  set.seed(seed)
  
  n_vars <- 20   # 10个变量 × int/slp
  cor_matrix <- diag(n_vars)
  
  # 变量索引转换为截距/斜率索引
  group1_effects <- sort(c((group1_vars * 2 - 1), (group1_vars * 2)))
  group2_effects <- sort(c((group2_vars * 2 - 1), (group2_vars * 2)))
  
  # 遍历填充
  for (i in 1:n_vars) {
    for (j in 1:n_vars) {
      if (i != j) {
        
        # 情况1: int-slp 同一变量对 (1-2, 3-4, ..., 19-20)
        if (ceiling(i/2) == ceiling(j/2)) {
          cor_matrix[i, j] <- runif(1, cor_range_inter[1], cor_range_inter[2])
          
          # 情况2: 组内高相关（V1-3组, V4-6组）
        } else {
          both_in_group1 <- (i %in% group1_effects) && (j %in% group1_effects)
          both_in_group2 <- (i %in% group2_effects) && (j %in% group2_effects)
          
          if (both_in_group1 || both_in_group2) {
            cor_matrix[i, j] <- runif(1, cor_range[1], cor_range[2])
          } else {
            # 情况3: 组间低相关
            cor_matrix[i, j] <- runif(1, 0, 0.1)
          }
        }
      }
    }
  }
  
  # 对称化
  cor_matrix <- (cor_matrix + t(cor_matrix)) / 2
  
  # 确保正定
  eigen_values <- eigen(cor_matrix)$values
  if (any(eigen_values <= 0)) {
    library(Matrix)
    cor_matrix <- as.matrix(nearPD(cor_matrix, corr = TRUE, keepDiag = TRUE)$mat)
  }
  
  return(cor_matrix)
}

# 示例：高共线性 (0.7~0.9)，int-slp相关性 (0~0.5)
group1_vars <- 1:3
group2_vars <- 4:6

#高影响
cov_matrix_high1 <- create_grouped_cov_matrix(
  group1_vars, group2_vars, 
  cor_range = c(0.7, 0.9), 
  cor_range_inter = c(0, 0.5),
  seed=123
)
cov_matrix_high2 <- create_grouped_cov_matrix(
  group1_vars, group2_vars, 
  cor_range = c(0.7, 0.9), 
  cor_range_inter = c(0, 0.5),
  seed=666
)
cov_matrix_high3 <- create_grouped_cov_matrix(
  group1_vars, group2_vars, 
  cor_range = c(0.7, 0.9), 
  cor_range_inter = c(0, 0.5),
  seed=1015
)
#中影响
cov_matrix_medium1 <- create_grouped_cov_matrix(
  group1_vars, group2_vars, 
  cor_range = c(0.3, 0.7), 
  cor_range_inter = c(0, 0.5),
  seed=123
)
cov_matrix_medium2 <- create_grouped_cov_matrix(
  group1_vars, group2_vars, 
  cor_range = c(0.3, 0.7), 
  cor_range_inter = c(0, 0.5),
  seed=666
)
cov_matrix_medium3 <- create_grouped_cov_matrix(
  group1_vars, group2_vars, 
  cor_range = c(0.3, 0.7), 
  cor_range_inter = c(0, 0.5),
  seed=1015
)

#低影响
cov_matrix_low1 <- create_grouped_cov_matrix(
  group1_vars, group2_vars, 
  cor_range = c(0.05, 0.3), 
  cor_range_inter = c(0, 0.5),
  seed=123
)
cov_matrix_low2 <- create_grouped_cov_matrix(
  group1_vars, group2_vars, 
  cor_range = c(0.05, 0.3), 
  cor_range_inter = c(0, 0.5),
  seed=666
)
cov_matrix_low3 <- create_grouped_cov_matrix(
  group1_vars, group2_vars, 
  cor_range = c(0.05, 0.3), 
  cor_range_inter = c(0, 0.5),
  seed=1015
)

















