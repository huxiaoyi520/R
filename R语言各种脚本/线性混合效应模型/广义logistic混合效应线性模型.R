library(readxl)
library(lme4)
library(car)
library(lmerTest)


d$T <- as.factor(d$T)
d$T 
colnames(d)
d$sub <- c(1:150)
d$sub

(fit1=glmer(YN~Group*T+(1|d$sub),data=d,family=binomial("logit")))    #定义
fit1
library(emmeans)
emmeans(fit1, pairwise~T)
summary(fit1)
anova(fit1)
