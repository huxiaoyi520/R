Data_after_the_propensity_score_matches
library(xlsx)
write.xlsx(Data_after_the_propensity_score_matches,"Data_after_the_propensity_score_matches.xlsx")

Data_after_the_propensity_score_matches$group1 <- factor(Data_after_the_propensity_score_matches$group,
                                                         levels = c("delirium", "control"))
library(ggplot2)
library(ggpubr)
library(ggbreak)
p2 = ggplot(Data_after_the_propensity_score_matches, aes(x=group1, y=chao, fill=group1)) +
  geom_boxplot()+
  stat_compare_means(aes(label = sprintf("p = %.3f", ..p..)), method = "wilcox.test",label.y = 280,label.x = 1.5)+   ###wilcox.test   
  scale_y_continuous(limits = c(0,310),expand = c(0,0),breaks = seq(0, 310, by = 50))+  ####expand = c(0,0)  # 确保Y轴与X轴对齐
  scale_y_break(breaks = c(10,80),space = 0.1,scales = 15,expand = c(0,0))+  ##scales截断值上面的图和下面的图比值，space是截断值的占据空间大小
  theme_bw()+theme(panel.grid=element_blank(),
                   legend.position = "bottom",   # 将图例放到下方bottom right"none", "left", "right", "bottom", "top", "inside"
                   legend.direction = "horizontal",  ####vertical  horizontal  
                   legend.text = element_text(face = "bold"),
                   panel.border =element_rect(fill=NA,color="black",linewidth=0.75,linetype="solid"),
                   axis.text.x=element_text(vjust=1,size=12,face = "bold",color="black"),
                   axis.text.y=element_text(vjust=0.5,size=12,face = "bold",color="black"),
                   axis.title.x = element_blank(),  ###element_text(face = "bold"),  # X轴标题加粗
                   axis.title.y = element_text(face = "bold"),  # Y轴标题加粗
                   plot.title = element_text(face = "bold"),  # 图表标题加粗（如果有）
                   plot.subtitle = element_text(face = "bold"),  # 副标题加粗（如果有）
                   plot.caption = element_text(face = "bold"),
                   axis.ticks.y.right = element_blank(),  # 移除右边Y轴刻度
                   axis.text.y.right = element_blank()    # 移除右边Y轴标签
  )+
  #xlab("")+
  ylab("Chao index")

p2