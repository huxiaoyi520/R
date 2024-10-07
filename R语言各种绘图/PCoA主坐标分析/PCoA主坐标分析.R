library(readxl)
otu.distance <- read_excel("Beta-unweighted.unifrac.xlsx")

otu.distance <- as.data.frame(otu.distance)

row.names(otu.distance) <- otu.distance$.ID
otu.distance <- otu.distance[,-1]

pcoa <- cmdscale (otu.distance,eig=TRUE)

pc12 <- pcoa$points[,1:2]
pc <- round(pcoa$eig/sum(pcoa$eig)*100,digits=2)#解释度
pc12 <- as.data.frame(pc12)
pc12$samples <- row.names(pc12)
head(pc12)
group <- read_excel("group.xlsx")
df <- merge(pc12,group,by="samples")
head(df)

color=c("#00BFC4","#F8766D","#FFC24B")#颜色变量
p6 <-ggplot(data=df,aes(x=V1,y=V2,
                        color=group,shape=group))+#指定数据、X轴、Y轴，颜色
  theme_bw()+#主题设置
  geom_point(size=1.8)+#绘制点图并设定大小
  geom_vline(xintercept = 0,lty="dashed")+
  geom_hline(yintercept = 0,lty="dashed")+  #图中虚线
  #geom_text(aes(label=samples, y=V2+0.03,x=V1+0.03,  vjust=0),size=3.5)+#添加数据点的标签
  # guides(color=guide_legend(title=NULL))+#去除图例标题
  labs(x=paste0("PC1 ",pc[1],"%"),
       y=paste0("PC2 ",pc[2],"%"))+#将x、y轴标题改为贡献度
  stat_ellipse(data=df,
               geom = "polygon",level=0.9,
               linetype = 2,size=0.5,
               aes(fill=group),
               alpha=0.2,
               show.legend = T)+
  scale_color_manual(values = color) +#点的颜色设置
  scale_fill_manual(values = c("#00BFC4","#F8766D","#FFC24B"))+
  theme(axis.title.x=element_text(size=12),#修改X轴标题文本
        axis.title.y=element_text(size=12),#修改y轴标题文本
        axis.text.y=element_text(size=12),#修改x轴刻度标签文本
        axis.text.x=element_text(size=12))+#修改y轴刻度标签文本
  theme(panel.grid=element_blank(),
        legend.position = "none",   # 将图例放到下方
        #legend.direction = "horizontal",
        #legend.text = element_text(face = "bold"),
        panel.border =element_rect(fill=NA,color="black",linewidth=0.75,linetype="solid"),
        axis.text.x=element_text(vjust=1,size=12,face = "bold",color="black"),
        axis.text.y=element_text(vjust=0.5,size=12,face = "bold",color="black"),
        axis.title.x = element_text(face = "bold"),##element_text(face = "bold"),  # X轴标题加粗
        axis.title.y = element_text(face = "bold"),  # Y轴标题加粗
        plot.title = element_text(face = "bold"),  # 图表标题加粗（如果有）
        plot.subtitle = element_text(face = "bold"),  # 副标题加粗（如果有）
        plot.caption = element_text(face = "bold"),
        axis.ticks.y.right = element_blank(),  # 移除右边Y轴刻度
        axis.text.y.right = element_blank()    # 移除右边Y轴标签
  )

p6

library(ggpubr)
mycomparisons <- list(c("delirium","control"))

max(df$V1)
p7 <- ggboxplot(df,x="group",y="V1",fill="group",add="none",size = 0.5,add.params = list(size=0.25))+
  stat_compare_means(label = "p.format",method = "wilcox.test",map_signif_level = T,comparisons = mycomparisons,textsize = 20,bracket.size=0.8,label.y =0.35 )+
  theme_light()+
  theme(panel.grid=element_blank(),panel.border = element_blank(),axis.ticks = element_blank(),legend.position = "none",axis.text=element_blank())+labs(x="",y="")+
  coord_flip()
p7
p8 <- ggboxplot(df,x="group",y="V2",fill="group",add="none",size = 0.5,add.params = list(size=0.25))+
  stat_compare_means(label = "p.format",method = "wilcox.test",map_signif_level = T,comparisons = mycomparisons,textsize = 20,bracket.size=0.8)+
  theme_light()+
  theme(panel.grid=element_blank(),panel.border = element_blank(),axis.ticks = element_blank(),legend.position = "none",axis.text=element_blank())+labs(x="",y="")
p8
library(gridExtra)
grid.arrange(grobs=list(p7,p8,p6,p8),ncol=2,nrow=2,widths=c(4,1),heights=c(1,4))

library(aplot) # Decorate a 'ggplot' with Associated Information
p9 <- p6%>%insert_top(p7,height = 0.2)%>%insert_right(p8,width = 0.2)


ggsave("p9.pdf", p9, width = 90, height = 90, units = "mm")
