library(descr)
library(dplyr)
library(haven)
library(lubridate)
library(tibble)
library(plyr)
library(stringr)
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
}#合并为纵向数据

freq_a <- function(data, a) {
  var_name <- deparse(substitute(a))
  freq_a <- as.data.frame(freq(data[[var_name]], 
                               report.nas = FALSE, headings = FALSE, plain.ascii = TRUE))
  freq_a <- freq_a %>% 
    rownames_to_column(var = var_name) %>%
    arrange(desc(Frequency))
  return(freq_a)
} #对data中 a变量进行计数
freq_rep <- function(data, var_rep, var_name) {
  var1 <- deparse(substitute(var_rep))  # 捕获变量名
  var2 <- deparse(substitute(var_name))  # 捕获变量名
  
  result <- data %>%
    group_by(!!sym(var1)) %>%
    summarise(freq = n()) %>%
    rename(
      ID = 1,          # 第一列重命名为ID
      !!var2 := 2   # 第二列使用输入的变量名
    ) %>%
    ungroup()
  
  return(as.data.frame(result))  # 确保返回标准数据框
}  #计数data中 var1变量的重复次数，记录其名字为var2

cbind_fill <- function(...) {
  dfs <- list(...)
  max_rows <- max(sapply(dfs, nrow))
  dfs_filled <- lapply(dfs, function(df) {
    if (nrow(df) < max_rows) {
      missing_rows <- max_rows - nrow(df)
      df[(nrow(df) + 1):max_rows, ] <- NA
    }
    return(df)
  })
  do.call(cbind, dfs_filled)
} #强行cbind(data1, data2, ..)

































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
  # 解析 select（将字符串转换为可执行的表达式）
  select_expr <- parse_exprs(select)
  data %>%
    mutate(item = case_when(!!!select_expr, TRUE ~ NA_character_))
}                                                              #筛选条件 #配合 select的值 使用

select<-c(
  'grepl("肌钙蛋白T",lab_name, ignore.case = T)  ~  "TNT"',
  'grepl("肌钙蛋白I|肌钙蛋白Ⅰ|ctni|tniu", lab_name, ignore.case = T) ~ "TNI"',
  'grepl("白细胞|血片共数白细胞",lab_name)  ~ "WBC"',
  'grepl("CK|肌酸激酶|快速心肌酶",lab_name,ignore.case = T)  ~  "CK_MB"',
  'grepl("糖",lab_name, ignore.case = T) &! grepl("餐后|小时|分钟|h|脑|肝|肾|胰|酶|肌酐|血脂|心肌|血红蛋白|电解质|血气|BUN|胸|腹水|蛋白|实验|黄疸|尿葡萄糖|尿|抗原|核|聚糖|菌|渗透",lab_name, ignore.case = T) ~ "GLU"',
  'grepl("BNP|脑钠肽|钠尿肽|钠酸肽",lab_name,ignore.case = T)  ~ "BNP"'
)

lab_grand1_freq <-function(data){
  lab_name<-lab_names(data)
  lab_name_ferq<-lab_select(lab_name, select)
  return( lab_name_ferq = lab_name_ferq)
}
lab_grand2_count <- function(data, item_col, item_value) {
  count_item <- lab_count(data, item_col, item_value) %>%
    select(-c(Frequency, Percent))
  
  data %>%
    left_join(count_item, by = c("ITEM_NAME" = "lab_name")) %>%
    filter(.data[[item_col]] != "") %>%  # 过滤掉 `item_col` 为空的行
    filter(RESULTS != "") %>%
    filter(UINT != "") %>%
    filter(REFERENCE_RESULT != "") %>%
    filter(.data[[item_col]] == item_value)  # 这里用 .data[[item_col]] 来动态引用列
}
lab_TNI<- lab_grand2_count(lab2013_2023,"TNI")
count_item<- lab_count(lab_name_freq, item, "TNI") %>%
  select( -c(Frequency, Percent)) 

lab_TNI<-lab2013_2023 %>%                             
  left_join(count_item, by = c("ITEM_NAME" = "lab_name")) %>%
  filter( item != "") %>%   #将item、结果、参考值、单位上没有值的都删除
  filter( RESULTS != "") %>%
  filter( UINT != "") %>%
  filter( REFERENCE_RESULT != "") %>%
  filter( item == "TNI") 

lab_TNI1<-lab_TNI %>%                             
  filter( item == "TNI+") 
lab_TNI2<-lab_TNI %>%                             
  filter( item == "TNI-")
count_ref <- lab_TNI %>% { as.data.frame(freq(.$REFERENCE_RESULT)) } %>% rownames_to_column(var = "ref_name")
count_UNIT <- lab_TNI %>% { as.data.frame(freq(.$UINT)) } %>% rownames_to_column(var = "unit_name")
lab_grand3_ref_unit<-function(data){
  count_ref <- data %>% { as.data.frame(freq(.$REFERENCE_RESULT)) } %>% rownames_to_column(var = "ref_name")
  count_UNIT <- data %>% { as.data.frame(freq(.$UINT)) } %>% rownames_to_column(var = "unit_name")
}





lab_test_code<-function(data){
  count_item1<- lab_count(data, item, "TNI+")
  count_item2<- lab_count(data, item, "TNI-")
  #标记记录
  count_item <- rbind(count_item1,count_item2)%>%  
    select( -c(Frequency, Percent)) 
  lab_TNI<-lab2013_2023 %>%                             
    left_join(count_item, by = c("ITEM_NAME" = "lab_name")) %>%
    filter( item != "") %>%
    filter( RESULTS != "") %>%
    filter( UINT != "") %>%
    filter( REFERENCE_RESULT != "") #将item、结果、参考值、单位上没有值的都删除
  lab_TNI1<-lab_TNI %>%                             
    filter( item == "TNI+") 
  lab_TNI2<-lab_TNI %>%                             
    filter( item == "TNI-")
  count_ref <- lab_TNI %>% { as.data.frame(freq(.$REFERENCE_RESULT)) } %>% rownames_to_column(var = "ref_name")
  count_UNIT <- lab_TNI %>% { as.data.frame(freq(.$UINT)) } %>% rownames_to_column(var = "unit_name")
  
  return(list(count_item = count_item, lab_TNI = lab_TNI, count_ref=count_ref, count_UNIT=count_UNIT))
  
}
TNI<-lab_test_code(lab_name_all)
list2env(TNI, envir = .GlobalEnv)






lab_TNI_1 <- lab_TNI %>%
  filter(grepl("0.00~0.04|0-0.03|(0.000-0.034)|0.010~0.023|(0.000-0.040)|小于0.04|0-0.08|0-0.02|0-0.028|0-0.04|0-0.034|0.01-0.023|0.00-0.04|0--0.04|0-0.06", 
               REFERENCE_RESULT, ignore.case = TRUE))












