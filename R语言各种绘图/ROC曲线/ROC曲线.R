library(pROC)
rocobj1 <- roc(Data_after_the_propensity_score_matches$group, 
               Data_after_the_propensity_score_matches$dysbiosis,ci=TRUE)
rocobj1

rocobj2 <- roc(Data_after_the_propensity_score_matches$group, 
               Data_after_the_propensity_score_matches$shannon,ci=TRUE)
rocobj2
rocobj3 <- roc(Data_after_the_propensity_score_matches$group, 
               Data_after_the_propensity_score_matches$chao,ci=TRUE)
rocobj3

rocobj4 <- roc(whole_data$delirium, as.numeric(whole_data$Oral_frailty),ci=TRUE)
rocobj4


rocobj3  #chao
rocobj2  #shannon
rocobj1  #dysbiosis
library(pROC)
g1 <- ggroc(list(dysbiosis = rocobj1, shannon = rocobj2, chao = rocobj3),legacy.axes = T)  ##,legacy.axes = T这个可以让xy轴从0开始
g1_1 <- g1 + 
  annotate(geom = "segment", x = 0, y = 0, xend =1, yend = 1,color="gray",linetype="dashed")+
  #geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray") +  # 添加对角线
  labs(x = "1 - Specificity", y = "Sensitivity") +  # 设置坐标轴标题
  scale_y_continuous(expand = c(0,0))+
  scale_x_continuous(expand = c(0,0))+
  #scale_color_manual(values = c("dysbiosis" = "red", "shannon" = "green", "chao" = "blue")) +  # 自定义线条颜色
  theme_bw() +  # 使用简洁主题
  theme(
    panel.grid = element_blank(),
    legend.position = "none",  # 调整图例位置
    legend.title = element_blank(),  # 去掉图例标题
    legend.text = element_text(size = 12, face = "bold",colour = "black"),  # 设置图例字体
    axis.title.x = element_text(size = 12, face = "bold",colour = "black"),  # X轴标题
    axis.title.y = element_text(size = 12, face = "bold",colour = "black"),  # Y轴标题
    axis.text = element_text(size = 12, face = "bold",colour = "black")  # 坐标轴刻度
  ) +
  annotate("text", x = 0.3, y = 0.2, 
           label = paste("Dysbiosis AUC: ", round(auc(rocobj1), 3),
                         "(95% CI ", round(ci.auc(rocobj1)[1], 3), "-", round(ci.auc(rocobj1)[3], 3), ")"),
           color = "red", size = 2, hjust = 0) +  # 添加Dysbiosis的AUC值标注
  annotate("text", x = 0.3, y = 0.15, 
           label = paste("Shannon AUC: ", round(auc(rocobj2), 3),
                         "(95% CI ", round(ci.auc(rocobj2)[1], 3), "-", round(ci.auc(rocobj2)[3], 3), ")"),
           color = "green", size = 2, hjust = 0) +  # 添加Shannon的AUC值标注
  annotate("text", x = 0.3, y = 0.1, 
           label = paste("Chao AUC: ", round(auc(rocobj3), 3),
                         "(95% CI ", round(ci.auc(rocobj3)[1], 3), "-", round(ci.auc(rocobj3)[3], 3), ")"),
           color = "blue", size = 2, hjust = 0)  # 添加Chao的AUC值标注
ggsave("p11.pdf", g1_1, width = 90, height = 90, units = "mm")




g2 <- ggroc(rocobj4, alpha = 0.5, colour = "red", size = 0.75,legacy.axes = T)
g2_1 <- g2+
  annotate(geom = "segment", x = 0, y = 0, xend =1, yend = 1,color="gray",linetype="dashed")+
  #geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray") +  # 添加对角线
  labs(x = "1 - Specificity", y = "Sensitivity") +  # 设置坐标轴标题
  scale_y_continuous(expand = c(0,0))+
  scale_x_continuous(expand = c(0,0))+
  #scale_color_manual(values = c("dysbiosis" = "red", "shannon" = "green", "chao" = "blue")) +  # 自定义线条颜色
  theme_bw() +  # 使用简洁主题
  theme(
    panel.grid = element_blank(),
    legend.position = "none",  # 调整图例位置
    legend.title = element_blank(),  # 去掉图例标题
    legend.text = element_text(size = 12, face = "bold",colour = "black"),  # 设置图例字体
    axis.title.x = element_text(size = 12, face = "bold",colour = "black"),  # X轴标题
    axis.title.y = element_text(size = 12, face = "bold",colour = "black"),  # Y轴标题
    axis.text = element_text(size = 12, face = "bold",colour = "black")  # 坐标轴刻度
  ) +
  annotate("text", x = 0.3, y = 0.2, 
           label = paste("Oral frailty AUC: ", round(auc(rocobj4), 3),
                         "(95% CI ", round(ci.auc(rocobj4)[1], 3), "-", round(ci.auc(rocobj4)[3], 3), ")"),
           color = "red", size = 2, hjust = 0)   # 添加Dysbiosis的AUC值标注
ggsave("p12.pdf", g2_1, width = 90, height = 90, units = "mm")

