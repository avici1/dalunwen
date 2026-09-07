library(readxl)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(MASS)
library(writexl)
library(readxl)

options(scipen = 999)




##############生成程序#################

sim_single_class1 <- function(n = 200,
                              n_time = 5,
                              param_list,
                              seed = 123) {
  
  ## 0. 环境准备 ----------------------------------------------------------
  if (!is.null(seed)) set.seed(seed)
  
  ## 1. 基本结构 ----------------------------------------------------------
  p         <- length(param_list)
  var_names <- sapply(param_list, `[[`, "varname")
  
  ID    <- rep(1:n, each = n_time)
  class <- rep("c1", n * n_time)
  time  <- rep(1:n_time, times = n)
  
  ## 2. 固定效应参数 ------------------------------------------------------
  beta0 <- sapply(param_list, `[[`, "mean_intercept")
  beta1 <- sapply(param_list, `[[`, "mean_slope")
  noise <- sapply(param_list, `[[`, "noise_sd")
  
  ## 3. 不规则时间（0–1） ------------------------------------------------
  t_mat <- matrix(NA, nrow = n, ncol = n_time)
  
  t_mat[, 1] <- 0
  if (n_time >= 2) t_mat[, 2] <- runif(n, 0, 0.25)
  if (n_time >= 3) t_mat[, 3] <- runif(n, 0.25, 0.5)
  if (n_time >= 4) t_mat[, 4] <- runif(n, 0.5, 0.75)
  if (n_time >= 5) t_mat[, 5] <- runif(n, 0.75, 1)
  
  t_mat <- t(apply(t_mat, 1, sort))
  t_vec <- as.vector(t(t_mat))
  
  ## 4. 生成观测值 --------------------------------------------------------
  Y_mat <- matrix(NA, nrow = n * n_time, ncol = p)
  colnames(Y_mat) <- var_names
  
  for (v in seq_len(p)) {
    mu_mat <- beta0[v] + beta1[v] * t_mat
    mu_vec <- as.vector(t(mu_mat))
    
    Y_mat[, v] <- mu_vec + rnorm(n * n_time, 0, noise[v])
  }
  
  ## 5. 返回 --------------------------------------------------------------
  data.frame(
    Y_mat,
    ID    = ID,
    class = class,
    time  = time,
    t     = t_vec
  )
}

put_in_list <- function(n_datasets = 200,
                        n = 500,
                        n_time = 5,
                        param_list,
                        base_seed = 123,
                        param_set = "L1",
                        corr_level = "low") {
  
  result_list <- list()
  
  for (i in 1:n_datasets) {
    
    dataset <- sim_single_class1(
      n         = n,
      n_time   = n_time,
      param_list = param_list,
      seed      = base_seed + i
    )
    
    list_name <- paste0(
      "sim_data_", n, "_", corr_level, "_", param_set, "_", i
    )
    
    result_list[[list_name]] <- dataset
  }
  
  return(result_list)
}





###########################数据参数list########################
#温和变化
param_list_4v_1 <- list(
  list(varname = "V1", mean_intercept = 0.55, mean_slope =  0.30, noise_sd = 0.03),
  list(varname = "V2", mean_intercept = 0.70, mean_slope = -0.25, noise_sd = 0.10),
  list(varname = "V3", mean_intercept = 0.35, mean_slope =  0.45, noise_sd = 0.04),
  list(varname = "V4", mean_intercept = 0.80, mean_slope = -0.15, noise_sd = 0.11)
)
#（斜率对比更强）
param_list_4v_2 <- list(
  list(varname = "V1", mean_intercept = 0.60, mean_slope =  0.45, noise_sd = 0.028),
  list(varname = "V2", mean_intercept = 0.65, mean_slope = -0.35, noise_sd = 0.12),
  list(varname = "V3", mean_intercept = 0.30, mean_slope =  0.60, noise_sd = 0.035),
  list(varname = "V4", mean_intercept = 0.85, mean_slope = -0.20, noise_sd = 0.10)
)

#（高截距 + 弱斜率）
param_list_4v_3 <- list(
  list(varname = "V1", mean_intercept = 0.75, mean_slope =  0.20, noise_sd = 0.04),
  list(varname = "V2", mean_intercept = 0.80, mean_slope = -0.15, noise_sd = 0.09),
  list(varname = "V3", mean_intercept = 0.70, mean_slope =  0.25, noise_sd = 0.045),
  list(varname = "V4", mean_intercept = 0.85, mean_slope = -0.10, noise_sd = 0.085)
)

#
param_list_4v_4 <- list(
  list(varname = "V1", mean_intercept = 0.35, mean_slope =  0.55, noise_sd = 0.025),
  list(varname = "V2", mean_intercept = 0.40, mean_slope = -0.45, noise_sd = 0.11),
  list(varname = "V3", mean_intercept = 0.30, mean_slope =  0.65, noise_sd = 0.03),
  list(varname = "V4", mean_intercept = 0.45, mean_slope = -0.50, noise_sd = 0.12)
)


param_list_4v_5 <- list(
  list(varname = "V1", mean_intercept = 0.50, mean_slope =  0.35, noise_sd = 0.02),
  list(varname = "V2", mean_intercept = 0.75, mean_slope = -0.30, noise_sd = 0.14),
  list(varname = "V3", mean_intercept = 0.45, mean_slope =  0.40, noise_sd = 0.06),
  list(varname = "V4", mean_intercept = 0.65, mean_slope = -0.20, noise_sd = 0.10)
)




#############################

############## n = 500 ################

## low
sim_data_500_low_L1 <- put_in_list(200, 500, 5, param_list_4v_1, 201, "L1", "low")
sim_data_500_low_L2 <- put_in_list(200, 500, 5, param_list_4v_2, 202, "L2", "low")
sim_data_500_low_L3 <- put_in_list(200, 500, 5, param_list_4v_3, 203, "L3", "low")
sim_data_500_low_L4 <- put_in_list(200, 500, 5, param_list_4v_4, 204, "L4", "low")
sim_data_500_low_L5 <- put_in_list(200, 500, 5, param_list_4v_5, 205, "L5", "low")

## medium
sim_data_500_medium_L1 <- put_in_list(200, 500, 5, param_list_4v_1, 206, "L1", "medium")
sim_data_500_medium_L2 <- put_in_list(200, 500, 5, param_list_4v_2, 207, "L2", "medium")
sim_data_500_medium_L3 <- put_in_list(200, 500, 5, param_list_4v_3, 208, "L3", "medium")
sim_data_500_medium_L4 <- put_in_list(200, 500, 5, param_list_4v_4, 209, "L4", "medium")
sim_data_500_medium_L5 <- put_in_list(200, 500, 5, param_list_4v_5, 210, "L5", "medium")

## high
sim_data_500_high_L1 <- put_in_list(200, 500, 5, param_list_4v_1, 211, "L1", "high")
sim_data_500_high_L2 <- put_in_list(200, 500, 5, param_list_4v_2, 212, "L2", "high")
sim_data_500_high_L3 <- put_in_list(200, 500, 5, param_list_4v_3, 213, "L3", "high")
sim_data_500_high_L4 <- put_in_list(200, 500, 5, param_list_4v_4, 214, "L4", "high")
sim_data_500_high_L5 <- put_in_list(200, 500, 5, param_list_4v_5, 215, "L5", "high")




############## n = 1000 ################

## low
sim_data_1000_low_L1 <- put_in_list(200, 1000, 5, param_list_4v_1, 301, "L1", "low")
sim_data_1000_low_L2 <- put_in_list(200, 1000, 5, param_list_4v_2, 302, "L2", "low")
sim_data_1000_low_L3 <- put_in_list(200, 1000, 5, param_list_4v_3, 303, "L3", "low")
sim_data_1000_low_L4 <- put_in_list(200, 1000, 5, param_list_4v_4, 304, "L4", "low")
sim_data_1000_low_L5 <- put_in_list(200, 1000, 5, param_list_4v_5, 305, "L5", "low")

## medium
sim_data_1000_medium_L1 <- put_in_list(200, 1000, 5, param_list_4v_1, 306, "L1", "medium")
sim_data_1000_medium_L2 <- put_in_list(200, 1000, 5, param_list_4v_2, 307, "L2", "medium")
sim_data_1000_medium_L3 <- put_in_list(200, 1000, 5, param_list_4v_3, 308, "L3", "medium")
sim_data_1000_medium_L4 <- put_in_list(200, 1000, 5, param_list_4v_4, 309, "L4", "medium")
sim_data_1000_medium_L5 <- put_in_list(200, 1000, 5, param_list_4v_5, 310, "L5", "medium")

## high
sim_data_1000_high_L1 <- put_in_list(200, 1000, 5, param_list_4v_1, 311, "L1", "high")
sim_data_1000_high_L2 <- put_in_list(200, 1000, 5, param_list_4v_2, 312, "L2", "high")
sim_data_1000_high_L3 <- put_in_list(200, 1000, 5, param_list_4v_3, 313, "L3", "high")
sim_data_1000_high_L4 <- put_in_list(200, 1000, 5, param_list_4v_4, 314, "L4", "high")
sim_data_1000_high_L5 <- put_in_list(200, 1000, 5, param_list_4v_5, 315, "L5", "high")





#######################保存数据#####################

out_dir <- "F:/文章/大论文/程序Trae/模拟数据_4V/协变量"

## 低相关
write_xlsx(sim_data_500_low_L1,  path = file.path(out_dir, "sim_data_500_low_c1_L1.xlsx"))
write_xlsx(sim_data_500_low_L2,  path = file.path(out_dir, "sim_data_500_low_c1_L2.xlsx"))
write_xlsx(sim_data_500_low_L3,  path = file.path(out_dir, "sim_data_500_low_c1_L3.xlsx"))
write_xlsx(sim_data_500_low_L4,  path = file.path(out_dir, "sim_data_500_low_c1_L4.xlsx"))
write_xlsx(sim_data_500_low_L5,  path = file.path(out_dir, "sim_data_500_low_c1_L5.xlsx"))
write_xlsx(sim_data_1000_low_L1,  path = file.path(out_dir, "sim_data_1000_low_c1_L1.xlsx"))
write_xlsx(sim_data_1000_low_L2,  path = file.path(out_dir, "sim_data_1000_low_c1_L2.xlsx"))
write_xlsx(sim_data_1000_low_L3,  path = file.path(out_dir, "sim_data_1000_low_c1_L3.xlsx"))
write_xlsx(sim_data_1000_low_L4,  path = file.path(out_dir, "sim_data_1000_low_c1_L4.xlsx"))
write_xlsx(sim_data_1000_low_L5,  path = file.path(out_dir, "sim_data_1000_low_c1_L5.xlsx"))

## 中相关
write_xlsx(sim_data_500_medium_L1, path = file.path(out_dir, "sim_data_500_medium_c1_L1.xlsx"))
write_xlsx(sim_data_500_medium_L2, path = file.path(out_dir, "sim_data_500_medium_c1_L2.xlsx"))
write_xlsx(sim_data_500_medium_L3, path = file.path(out_dir, "sim_data_500_medium_c1_L3.xlsx"))
write_xlsx(sim_data_500_medium_L4, path = file.path(out_dir, "sim_data_500_medium_c1_L4.xlsx"))
write_xlsx(sim_data_500_medium_L5, path = file.path(out_dir, "sim_data_500_medium_c1_L5.xlsx"))
write_xlsx(sim_data_1000_medium_L1, path = file.path(out_dir, "sim_data_1000_medium_c1_L1.xlsx"))
write_xlsx(sim_data_1000_medium_L2, path = file.path(out_dir, "sim_data_1000_medium_c1_L2.xlsx"))
write_xlsx(sim_data_1000_medium_L3, path = file.path(out_dir, "sim_data_1000_medium_c1_L3.xlsx"))
write_xlsx(sim_data_1000_medium_L4, path = file.path(out_dir, "sim_data_1000_medium_c1_L4.xlsx"))
write_xlsx(sim_data_1000_medium_L5, path = file.path(out_dir, "sim_data_1000_medium_c1_L5.xlsx"))

## 高相关
write_xlsx(sim_data_500_high_L1, path = file.path(out_dir, "sim_data_500_high_c1_L1.xlsx"))
write_xlsx(sim_data_500_high_L2, path = file.path(out_dir, "sim_data_500_high_c1_L2.xlsx"))
write_xlsx(sim_data_500_high_L3, path = file.path(out_dir, "sim_data_500_high_c1_L3.xlsx"))
write_xlsx(sim_data_500_high_L4, path = file.path(out_dir, "sim_data_500_high_c1_L4.xlsx"))
write_xlsx(sim_data_500_high_L5, path = file.path(out_dir, "sim_data_500_high_c1_L5.xlsx"))
write_xlsx(sim_data_1000_high_L1, path = file.path(out_dir, "sim_data_1000_high_c1_L1.xlsx"))
write_xlsx(sim_data_1000_high_L2, path = file.path(out_dir, "sim_data_1000_high_c1_L2.xlsx"))
write_xlsx(sim_data_1000_high_L3, path = file.path(out_dir, "sim_data_1000_high_c1_L3.xlsx")) 
write_xlsx(sim_data_1000_high_L4, path = file.path(out_dir, "sim_data_1000_high_c1_L4.xlsx"))
write_xlsx(sim_data_1000_high_L5, path = file.path(out_dir, "sim_data_1000_high_c1_L5.xlsx"))






############数据导入##############
library(readxl)
library(purrr) 

out_dir <- "F:/文章/大论文/程序Trae/模拟数据_4V/协变量"

## 低相关
sim_data_500_low_c1_L1 <- map(setNames(excel_sheets(file.path(out_dir, "sim_data_500_low_c1_L1.xlsx")),
                                       excel_sheets(file.path(out_dir, "sim_data_500_low_c1_L1.xlsx"))),
                              ~ read_xlsx(file.path(out_dir, "sim_data_500_low_c1_L1.xlsx"), sheet = .x))

sim_data_500_low_c1_L2 <- map(setNames(excel_sheets(file.path(out_dir, "sim_data_500_low_c1_L2.xlsx")),
                                       excel_sheets(file.path(out_dir, "sim_data_500_low_c1_L2.xlsx"))),
                              ~ read_xlsx(file.path(out_dir, "sim_data_500_low_c1_L2.xlsx"), sheet = .x))

sim_data_500_low_c1_L3 <- map(setNames(excel_sheets(file.path(out_dir, "sim_data_500_low_c1_L3.xlsx")),
                                       excel_sheets(file.path(out_dir, "sim_data_500_low_c1_L3.xlsx"))),
                              ~ read_xlsx(file.path(out_dir, "sim_data_500_low_c1_L3.xlsx"), sheet = .x))

sim_data_500_low_c1_L4 <- map(setNames(excel_sheets(file.path(out_dir, "sim_data_500_low_c1_L4.xlsx")),
                                       excel_sheets(file.path(out_dir, "sim_data_500_low_c1_L4.xlsx"))),
                              ~ read_xlsx(file.path(out_dir, "sim_data_500_low_c1_L4.xlsx"), sheet = .x))

sim_data_500_low_c1_L5 <- map(setNames(excel_sheets(file.path(out_dir, "sim_data_500_low_c1_L5.xlsx")),
                                       excel_sheets(file.path(out_dir, "sim_data_500_low_c1_L5.xlsx"))),
                              ~ read_xlsx(file.path(out_dir, "sim_data_500_low_c1_L5.xlsx"), sheet = .x))

sim_data_1000_low_c1_L1 <- map(setNames(excel_sheets(file.path(out_dir, "sim_data_1000_low_c1_L1.xlsx")),
                                        excel_sheets(file.path(out_dir, "sim_data_1000_low_c1_L1.xlsx"))),
                               ~ read_xlsx(file.path(out_dir, "sim_data_1000_low_c1_L1.xlsx"), sheet = .x))

sim_data_1000_low_c1_L2 <- map(setNames(excel_sheets(file.path(out_dir, "sim_data_1000_low_c1_L2.xlsx")),
                                        excel_sheets(file.path(out_dir, "sim_data_1000_low_c1_L2.xlsx"))),
                               ~ read_xlsx(file.path(out_dir, "sim_data_1000_low_c1_L2.xlsx"), sheet = .x))

sim_data_1000_low_c1_L3 <- map(setNames(excel_sheets(file.path(out_dir, "sim_data_1000_low_c1_L3.xlsx")),
                                        excel_sheets(file.path(out_dir, "sim_data_1000_low_c1_L3.xlsx"))),
                               ~ read_xlsx(file.path(out_dir, "sim_data_1000_low_c1_L3.xlsx"), sheet = .x))

sim_data_1000_low_c1_L4 <- map(setNames(excel_sheets(file.path(out_dir, "sim_data_1000_low_c1_L4.xlsx")),
                                        excel_sheets(file.path(out_dir, "sim_data_1000_low_c1_L4.xlsx"))),
                               ~ read_xlsx(file.path(out_dir, "sim_data_1000_low_c1_L4.xlsx"), sheet = .x))

sim_data_1000_low_c1_L5 <- map(setNames(excel_sheets(file.path(out_dir, "sim_data_1000_low_c1_L5.xlsx")),
                                        excel_sheets(file.path(out_dir, "sim_data_1000_low_c1_L5.xlsx"))),
                               ~ read_xlsx(file.path(out_dir, "sim_data_1000_low_c1_L5.xlsx"), sheet = .x))

## 中相关
sim_data_500_medium_c1_L1 <- map(setNames(excel_sheets(file.path(out_dir, "sim_data_500_medium_c1_L1.xlsx")),
                                          excel_sheets(file.path(out_dir, "sim_data_500_medium_c1_L1.xlsx"))),
                                 ~ read_xlsx(file.path(out_dir, "sim_data_500_medium_c1_L1.xlsx"), sheet = .x))

sim_data_500_medium_c1_L2 <- map(setNames(excel_sheets(file.path(out_dir, "sim_data_500_medium_c1_L2.xlsx")),
                                          excel_sheets(file.path(out_dir, "sim_data_500_medium_c1_L2.xlsx"))),
                                 ~ read_xlsx(file.path(out_dir, "sim_data_500_medium_c1_L2.xlsx"), sheet = .x))

sim_data_500_medium_c1_L3 <- map(setNames(excel_sheets(file.path(out_dir, "sim_data_500_medium_c1_L3.xlsx")),
                                          excel_sheets(file.path(out_dir, "sim_data_500_medium_c1_L3.xlsx"))),
                                 ~ read_xlsx(file.path(out_dir, "sim_data_500_medium_c1_L3.xlsx"), sheet = .x))

sim_data_500_medium_c1_L4 <- map(setNames(excel_sheets(file.path(out_dir, "sim_data_500_medium_c1_L4.xlsx")),
                                          excel_sheets(file.path(out_dir, "sim_data_500_medium_c1_L4.xlsx"))),
                                 ~ read_xlsx(file.path(out_dir, "sim_data_500_medium_c1_L4.xlsx"), sheet = .x))

sim_data_500_medium_c1_L5 <- map(setNames(excel_sheets(file.path(out_dir, "sim_data_500_medium_c1_L5.xlsx")),
                                          excel_sheets(file.path(out_dir, "sim_data_500_medium_c1_L5.xlsx"))),
                                 ~ read_xlsx(file.path(out_dir, "sim_data_500_medium_c1_L5.xlsx"), sheet = .x))

sim_data_1000_medium_c1_L1 <- map(setNames(excel_sheets(file.path(out_dir, "sim_data_1000_medium_c1_L1.xlsx")),
                                           excel_sheets(file.path(out_dir, "sim_data_1000_medium_c1_L1.xlsx"))),
                                  ~ read_xlsx(file.path(out_dir, "sim_data_1000_medium_c1_L1.xlsx"), sheet = .x))

sim_data_1000_medium_c1_L2 <- map(setNames(excel_sheets(file.path(out_dir, "sim_data_1000_medium_c1_L2.xlsx")),
                                           excel_sheets(file.path(out_dir, "sim_data_1000_medium_c1_L2.xlsx"))),
                                  ~ read_xlsx(file.path(out_dir, "sim_data_1000_medium_c1_L2.xlsx"), sheet = .x))

sim_data_1000_medium_c1_L3 <- map(setNames(excel_sheets(file.path(out_dir, "sim_data_1000_medium_c1_L3.xlsx")),
                                           excel_sheets(file.path(out_dir, "sim_data_1000_medium_c1_L3.xlsx"))),
                                  ~ read_xlsx(file.path(out_dir, "sim_data_1000_medium_c1_L3.xlsx"), sheet = .x))

sim_data_1000_medium_c1_L4 <- map(setNames(excel_sheets(file.path(out_dir, "sim_data_1000_medium_c1_L4.xlsx")),
                                           excel_sheets(file.path(out_dir, "sim_data_1000_medium_c1_L4.xlsx"))),
                                  ~ read_xlsx(file.path(out_dir, "sim_data_1000_medium_c1_L4.xlsx"), sheet = .x))

sim_data_1000_medium_c1_L5 <- map(setNames(excel_sheets(file.path(out_dir, "sim_data_1000_medium_c1_L5.xlsx")),
                                           excel_sheets(file.path(out_dir, "sim_data_1000_medium_c1_L5.xlsx"))),
                                  ~ read_xlsx(file.path(out_dir, "sim_data_1000_medium_c1_L5.xlsx"), sheet = .x))

## 高相关
sim_data_500_high_c1_L1 <- map(setNames(excel_sheets(file.path(out_dir, "sim_data_500_high_c1_L1.xlsx")),
                                        excel_sheets(file.path(out_dir, "sim_data_500_high_c1_L1.xlsx"))),
                               ~ read_xlsx(file.path(out_dir, "sim_data_500_high_c1_L1.xlsx"), sheet = .x))

sim_data_500_high_c1_L2 <- map(setNames(excel_sheets(file.path(out_dir, "sim_data_500_high_c1_L2.xlsx")),
                                        excel_sheets(file.path(out_dir, "sim_data_500_high_c1_L2.xlsx"))),
                               ~ read_xlsx(file.path(out_dir, "sim_data_500_high_c1_L2.xlsx"), sheet = .x))

sim_data_500_high_c1_L3 <- map(setNames(excel_sheets(file.path(out_dir, "sim_data_500_high_c1_L3.xlsx")),
                                        excel_sheets(file.path(out_dir, "sim_data_500_high_c1_L3.xlsx"))),
                               ~ read_xlsx(file.path(out_dir, "sim_data_500_high_c1_L3.xlsx"), sheet = .x))

sim_data_500_high_c1_L4 <- map(setNames(excel_sheets(file.path(out_dir, "sim_data_500_high_c1_L4.xlsx")),
                                        excel_sheets(file.path(out_dir, "sim_data_500_high_c1_L4.xlsx"))),
                               ~ read_xlsx(file.path(out_dir, "sim_data_500_high_c1_L4.xlsx"), sheet = .x))

sim_data_500_high_c1_L5 <- map(setNames(excel_sheets(file.path(out_dir, "sim_data_500_high_c1_L5.xlsx")),
                                        excel_sheets(file.path(out_dir, "sim_data_500_high_c1_L5.xlsx"))),
                               ~ read_xlsx(file.path(out_dir, "sim_data_500_high_c1_L5.xlsx"), sheet = .x))

sim_data_1000_high_c1_L1 <- map(setNames(excel_sheets(file.path(out_dir, "sim_data_1000_high_c1_L1.xlsx")),
                                         excel_sheets(file.path(out_dir, "sim_data_1000_high_c1_L1.xlsx"))),
                                ~ read_xlsx(file.path(out_dir, "sim_data_1000_high_c1_L1.xlsx"), sheet = .x))

sim_data_1000_high_c1_L2 <- map(setNames(excel_sheets(file.path(out_dir, "sim_data_1000_high_c1_L2.xlsx")),
                                         excel_sheets(file.path(out_dir, "sim_data_1000_high_c1_L2.xlsx"))),
                                ~ read_xlsx(file.path(out_dir, "sim_data_1000_high_c1_L2.xlsx"), sheet = .x))

sim_data_1000_high_c1_L3 <- map(setNames(excel_sheets(file.path(out_dir, "sim_data_1000_high_c1_L3.xlsx")),
                                         excel_sheets(file.path(out_dir, "sim_data_1000_high_c1_L3.xlsx"))),
                                ~ read_xlsx(file.path(out_dir, "sim_data_1000_high_c1_L3.xlsx"), sheet = .x))

sim_data_1000_high_c1_L4 <- map(setNames(excel_sheets(file.path(out_dir, "sim_data_1000_high_c1_L4.xlsx")),
                                         excel_sheets(file.path(out_dir, "sim_data_1000_high_c1_L4.xlsx"))),
                                ~ read_xlsx(file.path(out_dir, "sim_data_1000_high_c1_L4.xlsx"), sheet = .x))

sim_data_1000_high_c1_L5 <- map(setNames(excel_sheets(file.path(out_dir, "sim_data_1000_high_c1_L5.xlsx")),
                                         excel_sheets(file.path(out_dir, "sim_data_1000_high_c1_L5.xlsx"))),
                                ~ read_xlsx(file.path(out_dir, "sim_data_1000_high_c1_L5.xlsx"), sheet = .x))



