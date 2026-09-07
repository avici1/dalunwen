install.packages("dplyr")

library(dplyr)
library(lubridate)
library(tibble)
library("JM")
library(descr)
library(haven)
options(scipen=999)#禁用科学计数法

freq_a <- function(data, a) {
  var_name <- deparse(substitute(a))
  freq_a <- as.data.frame(freq(data[[var_name]], 
                               report.nas = FALSE, headings = FALSE, plain.ascii = TRUE))
  freq_a <- freq_a %>% 
    rownames_to_column(var = var_name) %>%
    arrange(desc(Frequency))
  return(freq_a)
}                                                                       #分类names
lab_names<- function(data){
  lab_name<-as.data.frame(freq(data$ITEM_NAME))
  lab_name<- lab_name %>% rownames_to_column(var = "lab_name")
  return(lab_name) 
}                                                                         #分类names
lab_count <- function(data, item, item_label) {
  data %>%
    filter(item == item_label) %>%  # 筛选指定的 item
    mutate(Percent = Frequency / sum(Frequency, na.rm = TRUE)) %>%  # 计算 Percent
    add_row(
      lab_name = "total",
      Frequency = sum(.$Frequency, na.rm = TRUE),
      Percent = sum(.$Percent, na.rm = TRUE),  # 总和应为 1
      .after = nrow(.)
    )
}                                                     #计算total
lab_select <- function(data, select) {
  require(dplyr)
  
  # 将字符串转换为表达式
  expr <- parse(text = select)
  
  # 在数据环境中评估表达式
  data %>%
    mutate(item = case_when(eval(expr), TRUE ~ NA_character_))
}
lab_filter <- function(data, select_pattern, reference_column) {
  data %>%
    filter(grepl(select_pattern, {{reference_column}}, ignore.case = TRUE))
}                                                      #筛选参考
lab_filter0 <- function(data, select_pattern, reference_column) {
  data %>%
    filter(!grepl(select_pattern, {{reference_column}}, ignore.case = TRUE))
}                                                      #筛选参考
lab_std <- function(data, var) {
  var_name <- deparse(substitute(var))  # 捕获变量名（非标准评估支持）
  # 转换为数值型，非数字会变成NA
  suppressWarnings({
    data[[var_name]] <- as.numeric(as.character(data[[var_name]]))
  })
  # 删除NA行（即原非数字的观测）
  data <- data[!is.na(data[[var_name]]), ]
  return(data)
}
lab_grand1_freq <-function(data){
  lab_name<-lab_names(data)
  lab_name_ferq<-lab_select(lab_name, select)
  return( lab_name_ferq = lab_name_ferq)
}
lab_grand2_count <- function(data, item_col, item_value) {
  count_item <- lab_count(lab_name_freq, item_col, item_value) 
  count_item <- count_item[, !(names(count_item) %in% c("Frequency", "Percent"))]
  
  data %>%
    left_join(count_item, by = c("ITEM_NAME" = "lab_name")) %>%
    filter(.data[[item_col]] != "") %>%  # 过滤掉 `item_col` 为空的行
    filter(RESULTS != "") %>%
    filter(UINT != "") %>%
    filter(REFERENCE_RESULT != "") %>%
    filter(.data[[item_col]] == item_value)  # 这里用 .data[[item_col]] 来动态引用列
}
lab_grand3_ref_unit<-function(data){
  count_ref <- data %>% { as.data.frame(freq(.$REFERENCE_RESULT)) } %>% rownames_to_column(var = "ref_name")
  count_UNIT <- data %>% { as.data.frame(freq(.$UINT)) } %>% rownames_to_column(var = "unit_name")
  count_name <- data %>% { as.data.frame(freq(.$ITEM_NAME)) } %>% rownames_to_column(var = "item_name")
  return(list(count_ref = count_ref, count_UNIT = count_UNIT, count_name= count_name))
}

lab2013_2023<-read.csv("F:/安贞医院数据/数据管理/lab2013_2023.csv")
baseline2013_2023<-read.csv("F:/文章/大论文/程序/数据/baseline2013_2023_1.csv")


lab_DYNAMIC<- lab_TC_1 %>% filter(ITEM_NAME == "总胆固醇")            #动态数据框查看记录内容
lab_DYNAMIC<- lab_TG_1 %>% filter(UINT == "z")                        #动态数据框查看记录内容
lab_DYNAMIC<- lab_LDLC_1 %>% filter(REFERENCE_RESULT == "-")          #动态数据框查看记录内容


#############提取 胆固醇-TC#################
select<-c(
  'grepl("总胆固醇|TC", lab_name, ignore.case = T) ~ "TC"'
)
lab_name_freq<- lab_grand1_freq(lab2013_2023)
lab_TC_1<- lab_grand2_count(lab2013_2023, "item", "TC")
lab_TC_refunit<-lab_grand3_ref_unit(lab_TC_1)
list2env(lab_TC_refunit, envir = .GlobalEnv)

#step2
select_ref <- "mmol/L"
lab_TC_2 <- lab_filter0(lab_TC_1, select_ref, REFERENCE_RESULT)

#step3
select_unit <- ""
lab_TC_3 <- lab_filter(lab_TC_1, select_unit, UINT)

lab_TC<- freq_a(lab_TC_2, RESULTS)   #查看results
lab_TC<- lab_std(lab_TC_2, RESULTS)  #删除非数字results



#############提取 甘油三脂-TG#################
select<-c(
  'grepl("甘油", lab_name, ignore.case = T) ~ "TG"'
)
lab_name_freq<- lab_grand1_freq(lab2013_2023)
lab_TG_1<- lab_grand2_count(lab2013_2023, "item", "TG")
lab_TG_refunit<-lab_grand3_ref_unit(lab_TG_1)
list2env(lab_TG_refunit, envir = .GlobalEnv)

#step2
select_ref <- "mmol/L"
lab_TG_2 <- lab_filter0(lab_TG_1, select_ref, REFERENCE_RESULT)

#step3
select_unit <- "10^9/L|10~9/L|*10^12/L|*10~9/L|*10^9/L"
lab_TG_3 <- lab_filter(lab_TG_1, select_unit, UINT)

lab_TG<- freq_a(lab_TG_2, RESULTS)   #查看results
lab_TG<- lab_std(lab_TG_2, RESULTS)  #删除非数字results





#############提取 低密度脂蛋白 LDL_C#################
select<-c(
  'grepl("低密度脂蛋白|LDL", lab_name, ignore.case = T) & !grepl("小而密", lab_name, ignore.case = T) ~ "LDLC"'
)
lab_name_freq<- lab_grand1_freq(lab2013_2023)
lab_LDLC_1<- lab_grand2_count(lab2013_2023, "item", "LDLC")
lab_LDLC_refunit<-lab_grand3_ref_unit(lab_LDLC_1)
list2env(lab_LDLC_refunit, envir = .GlobalEnv)

#step2
select_ref <- "mmol/L"
lab_LDLC_2 <- lab_filter0(lab_LDLC_1, select_ref, REFERENCE_RESULT)

#step3
select_unit <- ""
lab_LDLC_3 <- lab_filter(lab_LDLC_1, select_unit, UINT)

lab_LDLC<- freq_a(lab_LDLC_2, RESULTS)   #查看results
lab_LDLC<- lab_std(lab_LDLC_2, RESULTS)  #删除非数字results



#############提取 高密度脂蛋白 HDL_C#################

select<-c(
  'grepl("高密度脂蛋白|HDL", lab_name, ignore.case = T)  ~ "HDLC"'
)
lab_name_freq<- lab_grand1_freq(lab2013_2023)
lab_HDLC_1<- lab_grand2_count(lab2013_2023, "item", "HDLC")
lab_HDLC_refunit<-lab_grand3_ref_unit(lab_HDLC_1)
list2env(lab_HDLC_refunit, envir = .GlobalEnv)

#step2
select_ref <- "mmol/L"
lab_HDLC_2 <- lab_filter0(lab_HDLC_1, select_ref, REFERENCE_RESULT)

#step3
select_unit <- ""
lab_HDLC_3 <- lab_filter(lab_HDLC_1, select_unit, UINT)

lab_HDLC<- freq_a(lab_HDLC_2, RESULTS)   #查看results
lab_HDLC<- lab_std(lab_HDLC_2, RESULTS)  #删除非数字results









#######保存########
write.csv(lab_HDLC,"F:/文章/大论文/程序/数据/lab_HDLC.csv")
write.csv(lab_LDLC,"F:/文章/大论文/程序/数据/lab_LDLC.csv")
write.csv(lab_TC,"F:/文章/大论文/程序/数据/lab_TC.csv")
write.csv(lab_TG,"F:/文章/大论文/程序/数据/lab_TG.csv")




#################数据基础处理##########################
baseline2013_2023$ICD10<- substr(baseline2013_2023$DISEASE_CODE1,1,3)

lab_GLU<-read.csv("F:/安贞医院数据/数据管理/lab_GLU.csv")
lab_GLU<- lab_GLU %>% filter(RESULTS != "")
lab_GLU$RESULT <- suppressWarnings(as.numeric(lab_GLU$RESULTS)) 
lab_GLU<- lab_GLU %>%
  filter(RESULT != "NA") #删除NA
lab_GLU$RESULT[lab_GLU$UINT %in% c("mg/dL", "mg/dl")] <- lab_GLU$RESULT[lab_GLU$UINT %in% c("mg/dL", "mg/dl")] / 18     #将mg/dl与mmol/l进行单位转换






##################合并基线，组成纵向数据######################################
long_pack <- function(data) {
  data <- data %>%
    left_join(baseline2013_2023, by = "B_WT4_ID") %>%
    .[, !(names(.) %in% c("X.1", "X.x", "ITEM_CODE", "REFERENCE_RESULT", "UINT", "X.y", "OUT_DATE", "DATE_INHOSPITAL"))]
  data$SURV <- ifelse(is.na(data$TIME_DEAD), 0, 1)
  data$GATHER_DATE <- as.POSIXct(data$GATHER_DATE, format = "%Y-%m-%d")
  data <- data %>%
    arrange(B_WT4_ID, GATHER_DATE) %>%
    group_by(B_WT4_ID) %>%
    mutate(times = row_number()) %>%  # 添加 times 变量
    ungroup()
  
  return(data)
}

long_TG<-long_pack(lab_TG)
long_TC<-long_pack(lab_TC)
long_LDLC<-long_pack(lab_LDLC)
long_HDLC<-long_pack(lab_HDLC)
long_GLU<-long_pack(lab_GLU)

write.csv(long_GLU,"F:/文章/大论文/程序/数据/long_GLU.csv")
write.csv(long_TC,"F:/文章/大论文/程序/数据/long_TC.csv")
write.csv(long_TG,"F:/文章/大论文/程序/数据/long_TG.csv")
write.csv(long_LDLC,"F:/文章/大论文/程序/数据/long_LDLC.csv")
write.csv(long_HDLC,"F:/文章/大论文/程序/数据/long_HDLC.csv")



###########描述性分析################################

GLU_RESULT<-freq_a(GLU, RESULT)      #





GLU_ID<- freq_a(GLU, B_WT4_ID)       #人数48000人
GLU_ID_1 <- GLU %>%
  group_by(B_WT4_ID) %>%          # 按 ID 分组
  summarise(across(everything(),  # 对所有其他列操作
                   ~ first(.x)))   # 取每列的第一个值
#\\
write.csv(GLU_ID_1,"F:/文章/大论文/程序/数据/GLU_ID.csv")


GLU_DEAD<- freq_a(GLU_ID_1, TIME_DEAD)    #4%死亡
GLU_ICD<- freq_a(GLU, ICD10)         #多数I20 I21 I25


##################################################









lab_names<- function(data){
  lab_name<-as.data.frame(freq(data$ORG_NAME))
  lab_name<- lab_name %>% rownames_to_column(var = "lab_name")
  return(lab_name) 
} 




aL<-read_sas("F:/安贞医院数据/2020_2023/temp20230301.sas7bdat")
a<- lab_nams(aL)

aL<-

  lab_name<-as.data.frame(freq(aL$ORG_NAME))
lab_name<- lab_name %>% rownames_to_column(var = "lab_name")

write.csv(lab_name, "F:/安贞医院数据/2020_2023/a.csv")
