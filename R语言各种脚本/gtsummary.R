#安装包
remotes::install_github("ddsjoberg/gtsummary",force = TRUE)
#加载包
library(gtsummary)
library(tidycmprsk)
#读入数据，这里我以之前的一个自己的数据为示例
mydata<-as_tibble(read.delim("UBSC.txt",header = T))
#查看数据类型
str(mydata)



#该包也是结合了tidyverse的功能，这里利用管道函数以及select函数选择需要统计的变量
data<-mydata %>% select(c(2:5,7:8,11,13,16,21:23))
#对变量进行重命名
colnames(data)<-c("Survival_status","Survival_time","Gender","Marital status",
                  "Age","Race","Disease stage","Grade",
                  "Clinical stage","Radiotherapy","Surgery","Chemotherapy")
#查看数据
head(data)


#变量因子转换
data$Gender<-factor(data$Gender,order=T,levels = c("Female","Male"))
data$`Marital status`<-factor(data$`Marital status`,order=T,levels = c("Married","Others"))
data$Race<-factor(data$Race,order=T,levels = c("White","Black","Others"))
data$`Disease stage`<-factor(data$`Disease stage`,order=T,levels = c("Localized","Regional","Distant"))
data$Grade<-factor(data$Grade,order=T,levels = c("Low","High","Unknown"))
data$`Clinical stage`<-factor(data$`Clinical stage`,order=T,levels = c("Non_muscle_invasive","Muscle_invasive",
                                                                       "Non_organ_confined","Lymph node invasion","Distant_metastasis"))
data$Radiotherapy<-factor(data$Radiotherapy,order=T,levels = c("None/Unknown","Yes"))
data$Surgery<-factor(data$Surgery,order=T,levels = c("None/Unknown","Yes"))
data$Chemotherapy<-factor(data$Chemotherapy,order=T,levels = c("No/Unknown","Yes"))




table1 <- tbl_summary(data) 
print(table1)

table2 <- 
  tbl_summary(
    data,
    by =Survival_status, # 按生存状态分组
    missing = "no" # 不要展示缺失值
  ) %>%
  add_overall() %>% # 添加总体人群信息
  add_p() %>% #组件差异统计
  modify_header(label = "**Variable**") %>% # 变量名标签改为Variables
  modify_caption("Table 1. Baseline characteristics")%>%#添加表头
  bold_labels() #标签加粗
print(table2)#打印



table2 %>%
  as_gt() %>%
  gt::gtsave(filename = "table2.html") # use extensions .html .tex .ltx .rtf
#这个是一种方法，利用as_gt函数保存为html格式，即网页。运行后在当前目录下会得到一个网页文件

tf <- tempfile(fileext = ".docx")#设置临时路径
table2 %>%
  as_flex_table() %>%
  flextable::save_as_docx(table2,path = tf)
#这个时候我们运行一下tf就会知道到时候文件保存在哪个路径了
tf

#加载包
library(survival)
# 拟合Cox比例风险模型
t1 <-coxph(Surv(Survival_time,Survival_status==1) ~., data) %>%
  tbl_regression(exponentiate = TRUE)
print(t1)
