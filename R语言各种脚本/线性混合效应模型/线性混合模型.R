library(lmerTest)
library(tidyverse)
source('D:/Users/huxiaoyi/Desktop/mixedDesign.v0.6.3.R')
DF=tibble()
DF
for(ii in 1:30){
  df=mixedDesign(W=4,n=30,SD=30,
                 M=matrix(c(230,280,250,280), nrow =1),long = T)
  df['direction']=ifelse(df$W_a %in% c('a1','a2'),'left','right')
  df['distance']=ifelse(df$W_a %in% c('a1','a3'),'unit1','unit2')
  DF=rbind(DF,df)
}

item=c()
for (ii in 1:120){
  item=c(item,sample(1:30,size = 30,replace = F))
}
DF=DF %>% arrange(W_a,id) %>%
  mutate(item=item)
DF=DF[c('id','item','direction','distance','DV')]
DF[c('id','item','direction','distance')]=lapply(DF[c('id','item','direction','distance')],factor)
DF
str(DF)

###全模型与零模型###
Modelmax <- lmer(data = DF,
                  formula = DV~direction*distance+(1+direction*distance|id)+(1+direction*distance|item),
                 control = lmerControl(optimizer='bobyqa'))
Modelzero <- lmer(data = DF,
                  formula = DV~direction*distance+(1|id)+(1|item),
                  control = lmerControl(optimizer='bobyqa'))
summary(Modelzero)
anova(Modelzero)
library(emmeans)
emmeans::emmeans(Modelzero,pairwise~direction)
emm_options(pbkrtest.limit = 3000)
emmeans::emmeans(Modelzero,pairwise~direction)
emmeans::joint_tests(Modelzero,by='direction')
emmeans::emmeans(Modelzero,pairwise~distance|direction)



contrasts(DF$direction)
contrasts(DF$distance)
M=model.matrix(~direction*distance,DF)
M
DF[c('right','unit2','right_unit2')]=M[,c(2:4)]
View(DF)
contrasts(DF$direction)=contr.sum(2)
contrasts(DF$distance)=contr.sum(2)
contrasts(DF$direction)
contrasts(DF$distance)
M=model.matrix(~direction*distance,DF)

view(M)
DF[c('right','unit2','right_unit2')]=M[,c(2:4)]


###用主成分分析优化模型###
contrasts(DF$direction)=c(-0.5,0.5) #定义direction的对比方式
contrasts(DF$distance)=c(-0.5,0.5) #定义distance的对比方式
M=model.matrix(~direction*distance,DF) #手动生成虚拟变量
DF[c('right_left','unit2_1','interaction')]=M[,c(2:4)]
View(DF)
Modelmax <- lmer(data = DF,
                 formula = DV~direction*distance+(1+right_left+unit2_1+interaction|id)+(1+right_left+unit2_1+interaction|item),
                 control = lmerControl(optimizer='bobyqa'))

Modelzero <- lmer(data = DF,
                 formula = DV~direction*distance+(1+right_left+unit2_1+interaction||id)+(1+right_left+unit2_1+interaction||item),
                 control = lmerControl(optimizer='bobyqa'))
summary(rePCA(Modelzero))
VarCorr(Modelzero)
Modelopt<- lmer(data = DF,
                  formula = DV~direction*distance+(1+right_left+unit2_1||id)+(-1+unit2_1||item),
                  control = lmerControl(optimizer='bobyqa'))
anova(Modelmax,Modelopt)

###定义事先对比###
DF=DF[,1:5]
DF['condition']=paste0(DF$distance,"_",DF$direction)
view(DF)
DF$condition=factor(DF$condition,
                    levels = c('unit2_left','unit1_left','unit1_right','unit2_right'))
levels(DF$condition)
H=rbind(c(0.5,-0.5,-0.5,0.5),
        c(0,-1,1,0),
        c(-1,0,0,1))
rownames(H)=paste0('H',1:3)
colnames(H)=levels(DF$condition)
MASS::ginv(H)
C=MASS::ginv(H)
rownames(C)=colnames(H)
colnames(C)=rownames(H)
contrasts(DF$condition)=C
M=model.matrix(~condition,DF)
DF[paste0('H',1:3)]=M[,2:4]

Modelmax <- lmer(data = DF,
                 formula = DV~H1+H2+H3+(1+H1+H2+H3|id)+(1+H1+H2+H3|item),
                 control = lmerControl(optimizer='bobyqa'))
Modelzcp<- lmer(data = DF,
                 formula = DV~H1+H2+H3+(1+H1+H2+H3||id)+(1+H1+H2+H3||item),
                 control = lmerControl(optimizer='bobyqa'))

summary(rePCA(Modelzcp))
VarCorr(Modelzcp)
Modelopt<- lmer(data = DF,
                formula = DV~H1+H2+H3+(1+H1+H2+H3||id)+(-1+H1||item),
                control = lmerControl(optimizer='bobyqa'))
anova(Modelmax,Modelopt)

H
t(H)
cor(t(H))
summary(Modelopt)

if(!require(devtools)) install.packages("devtools")
devtools::install_github("usplos/YawMMF")
library(YawMMF)
head(DemoData2)
DemoData2$CondA=factor(DemoData2$CondA,levels = c('A1','A2'))
DemoData2$CondB=factor(DemoData2$CondB,levels = c('B1','B2'))
contrasts(DemoData2$CondA)=c(-0.5,0.5)
contrasts(DemoData2$CondB)=c(-0.5,0.5)
Model=glmer(data = DemoData2,DV~CondA*CondB+(1|item),
            family = "binomial")
summary(Model)
##anova(Model)
car::Anova(Model,type=3)
summary(Model)$coef %>% round(3)
emmeans(Model,pairwise~CondA|CondB)

DemoData2 %>% group_by(subj,CondA,CondB)%>%
  summarise(Reg=mean(DV))%>%
  group_by(CondA,CondB)%>%
  summarise(MeanReg=mean(Reg))

