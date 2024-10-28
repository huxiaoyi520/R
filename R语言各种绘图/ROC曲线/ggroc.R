library(pROC)
library(ggplot2)
data(aSAH)
View(aSAH)
rocobj <- roc(aSAH$outcome, aSAH$s100b)
rocobj2 <- roc(aSAH$outcome, aSAH$wfns)
g <- ggroc(rocobj)
ggroc(rocobj, alpha = 0.5, colour = "red", linetype = 2, size = 2)
g + theme_minimal() + ggtitle("My ROC curve") + 
  geom_segment(aes(x = 1, xend = 0, y = 0, yend = 1), color="grey", linetype="dashed")

######### And change axis labels to FPR/FPR重要！！！！！！##############
gl <- ggroc(rocobj, legacy.axes = TRUE,size = 1,colour="#f8766d")
gl
gl + xlab("1 - Specificity") + ylab("Sensitivity") + 
  geom_segment(aes(x = 0, xend = 1, y = 0, yend = 1), color="darkgrey", linetype="dashed")+
  scale_y_continuous(expand = c(0, 0))+scale_x_continuous(expand = c(0, 0))+
  theme_bw()+theme(panel.grid=element_blank(),
                   panel.border = element_rect(fill=NA,color="black", linewidth=0.75, linetype="solid"),
                   axis.text.x=element_blank(),
                   axis.ticks.x = element_blank(),
                   axis.text.y=element_text(vjust=1,size=10,face = "bold"),
                   axis.title.x =element_text(vjust=1,size=10,face = "bold"),
                   axis.title.y = element_text(vjust=1,size=10,face = "bold"),
                   legend.title = element_text(vjust=1,size=10,face = "bold"),
                   legend.text = element_text(vjust=1,size=10,face = "bold"))
# Multiple curves:
g2 <- ggroc(list(s100b=rocobj, wfns=rocobj2, ndka=roc(aSAH$outcome, aSAH$ndka)))
g2

# This is equivalent to using roc.formula:
roc.list <- roc(outcome ~ s100b + ndka + wfns, data = aSAH)
g.list <- ggroc(roc.list)
g.list

# You can change the aesthetics as you normally would with ggplot2:
g.list + scale_colour_brewer(palette="RdGy")
g.list + scale_colour_manual(values = c("red", "blue", "black"))

# with additional aesthetics:
g3 <- ggroc(roc.list, linetype=2)
g3
g4 <- ggroc(roc.list, aes="linetype", color="red")
g4
# changing multiple aesthetics:changeing x-axis called 1-specificity
g5 <- ggroc(roc.list, aes=c("linetype", "color"),legacy.axes = TRUE)
g5

# OR faceting
g.list + facet_grid(.~name) + theme(legend.position="none")
# To have all the curves of the same color, use aes="group":
g.group <- ggroc(roc.list, aes="group")
g.group
g.group + facet_grid(.~name)


#修改原点
g5+scale_y_continuous(expand = c(0, 0))+scale_x_continuous(expand = c(0, 0))

g5+
  scale_y_continuous(expand = c(0, 0))+
  scale_x_continuous(expand = c(0, 0))+
  theme_grey()  + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + theme(panel.background = element_rect(fill = "gray100")) + theme(axis.line = element_line(size = 0.7,
    linetype = "solid"))



windowsFonts(A=windowsFont("Arial"))
