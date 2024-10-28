devtools::install_github("cardiomoon/moonBook")
devtools::install_github("cardiomoon/autoReg")
devtools::install_github("cardiomoon/rrtable")

library(autoReg)
library(survival)
library(dplyr)
library(officer)
library(moonBook)
library(rrtable)
data(cancer,package="survival")#加载示例数据

fit=glm(status~rx+sex+age+obstruct+nodes,data=colon,family="binomial")
fit
autoReg(fit)
autoReg(fit,uni=TRUE,final=FALSE)#设置参数uni=TRUE即可显示单因素结果，final=FALSE不会显示最终模型的结果

##自动实现单因素和多因素回归分析
#直接导出成word格式呢
ft1<-autoReg(fit,uni=TRUE,final=FALSE)%>% myft()#这里我们用dplyr的窗口函数把表格显示出来
ft1
table2docx(ft1)#导出word格式
#Exported table as Report.docx
table2pptx(ft1)#导出PPT格式
#Exported table as Report.pptx

getwd()



#自动实现单因素和多因素回归分析以及逐步向后回归

ft2<-autoReg(fit,uni=TRUE,final=TRUE)%>% myft()
ft2
table2docx(ft2)#导出word格式

fit=coxph(Surv(time,status)~rx+age+sex+obstruct+perfor,data=colon)
fit

ft3<-autoReg(fit,uni=TRUE,threshold=0.05,final=FALSE) %>% myft()
#在上面我们选择单因素里面P值小于0.05的进入多因素Cox回归分析，不选择逐步向后回归
ft3
table2docx(ft3)



?acs
str(acs)

gaze(acs)
gaze(~age+sex,data=acs)#只统计部分变量，如年龄和性别
table1<-gaze(sex~.,data=acs,digits=1,method=1,show.p=TRUE) %>% myft()
table1


table2<-gaze(sex+Dx~.,data=acs,show.p=TRUE) %>% myft()
table2
table2docx(table2)

fit=glm(status~rx+sex+age+obstruct+nodes,data=colon,family="binomial")
modelPlot(fit)

modelPlot(fit,uni=TRUE,multi=TRUE)#同时显示单因素和多因素OR值


fit=coxph(Surv(time,status)~age+sex+obstruct+perfor,data=colon)
modelPlot(fit)#只显示多因素HR值
modelPlot(fit,uni=TRUE,threshold=1)#同时显示单因素和多因素HR值
