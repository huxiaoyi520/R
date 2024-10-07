####旷场，新物体识别，Y迷宫等行为学一键统计分析绘图######
# ctrl+shift+c  可以快速注释

library(ggplot2)
library(tidyverse)
library(gghalves)
library(ggpubr)
library(Rmisc)
###########旷场###########
colnames(OFT)


opf_samll <- OFT[,c("Total Distance (cm)",
                    "Time in Center (s)",
                    "group","File"),]
opf_samll$`Total Distance (m)` <- opf_samll$`Total Distance (cm)`/100
# 假设你的数据框为 df，其中包含 "group" 和 "value" 列
# 1. 筛选出 lps 组并按 value 列从大到小排序，选取前11个样本
opf_samll_lps <- opf_samll[opf_samll$group == "lps", ]
opf_samll_lps_sorted <- opf_samll_lps[order(-opf_samll_lps$`Total Distance (m)`), ]  # 降序排序
opf_samll_lps_top_11_lps <- opf_samll_lps_sorted[1:10, ]  # 选取前11个


# 2. 筛选出 control 组并按 value 列从小到大排序，选取前11个样本
opf_samll_control <- opf_samll[opf_samll$group == "control", ]
opf_samll_control_sorted <- opf_samll_control[order(opf_samll_control$`Total Distance (m)`), ]  # 升序排序
opf_samll_control_top_11_control <- opf_samll_control_sorted[1:10, ]  # 选取前11个

# 3. 合并 lps 组和 control 组的结果
opf_samll_combined_result <- rbind(opf_samll_lps_top_11_lps, opf_samll_control_top_11_control)

colnames(opf_samll_combined_result)
str(opf_samll_combined_result)
ordercolors<-c("#B2B2B2","#F4B9D9","#AAD7C8","#619CD9")
Process_comparisons <- list(c("control", "lps"))  ####

min(opf_samll_combined_result$`Total Distance (m)`)
max(opf_samll_combined_result$`Total Distance (m)`)
t.test(opf_samll_combined_result$`Total Distance (m)`~group,data = opf_samll_combined_result)
wilcox.test(opf_samll_combined_result$`Total Distance (m)`~group,data = opf_samll_combined_result)

####此处是云雨图
g1 <- ggplot(data = opf_samll_combined_result,
             aes(x=group, y=`Total Distance (m)`, fill=group)) +
  geom_half_violin(side = "r", color=NA, alpha=0.35) +
  geom_half_boxplot(side = "r", errorbar.draw = FALSE, width=0.2, linewidth=0.5) +
  geom_half_point_panel(side = "l", shape=21, size=3, color="white") +
  scale_fill_manual(values = ordercolors) +
  scale_y_continuous(limits = c(min(opf_samll_combined_result$`Total Distance (m)`)-2, 
                                max(opf_samll_combined_result$`Total Distance (m)`)+6), 
                     expand = c(0, 0)) +     ####这个位置可以更改
  #scale_x_discrete(labels = c('Artio','Carn','Prim','Rod','Others')) +  #### 这是x轴的名称，依据11行的因子进行改变
  labs(y="Total distance traveled (cm)",x=NULL) +
  theme_bw() +
  theme(panel.grid = element_blank(),  ####去除网格线
        legend.position = "none",   #####"none", "left", "right", "bottom", "top"  这个几个参数都可以使用
        axis.title = element_text(size = 12, color = "black",face = "bold"),  
        axis.text = element_text(size=12, color = "black",face = "bold"),
        axis.line = element_line(colour = "black"),
        panel.border = element_blank())+  #####这个可以去除顶部和右边的边框线
  stat_compare_means(comparisons = Process_comparisons,
                     #method = "t.test",###此处方法默认是Kruskal-wallis,t.test,anova,
                     vjust = 0.02,
                     label.x = 1,
                     data = opf_samll_combined_result) 


g1_1 <- ggplot(data = opf_samll_combined_result, 
               aes(x=group, y=`Time in Center (s)`, fill=group)) +
  geom_half_violin(side = "r", color=NA, alpha=0.35) +
  geom_half_boxplot(side = "r", errorbar.draw = FALSE, width=0.2, linewidth=0.5) +
  geom_half_point_panel(side = "l", shape=21, size=3, color="white") +
  scale_fill_manual(values = ordercolors) +
  scale_y_continuous(limits = c(min(opf_samll_combined_result$`Time in Center (s)`)-2, 
                                max(opf_samll_combined_result$`Time in Center (s)`)+6), 
                     expand = c(0, 0)) +     ####这个位置可以更改
  #scale_x_discrete(labels = c('Artio','Carn','Prim','Rod','Others')) +  #### 这是x轴的名称，依据11行的因子进行改变
  labs(y="Time spent in the center (sec)",x=NULL) +
  theme_bw() +
  theme(panel.grid = element_blank(),  ####去除网格线
        legend.position = "none",   #####"none", "left", "right", "bottom", "top"  这个几个参数都可以使用
        axis.title = element_text(size = 12, color = "black",face = "bold"),  
        axis.text = element_text(size=12, color = "black",face = "bold"),
        axis.line = element_line(colour = "black"),
        panel.border = element_blank())+  #####这个可以去除顶部和右边的边框线
  stat_compare_means(comparisons = Process_comparisons,
                     data = opf_samll_combined_result,
                     method = "t.test", ###此处方法默认是Kruskal-wallis
                     vjust = 0.02,
                     label.x = 1)


# 对每个 subgroup 进行正态性检验
shapiro_results <- by(opf_samll_combined_result$`Total Distance (m)`,
                      opf_samll_combined_result$group, shapiro.test)
# 输出结果
print(shapiro_results)

opf_samll_combined_result_long <- summarySE(opf_samll_combined_result, measurevar="Total Distance (m)", groupvars=c("group"))

####此处是直方图
g2 <- ggplot(opf_samll_combined_result_long, aes(x = group, y = `Total Distance (m)`, fill = group)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = `Total Distance (m)` - se, ymax = `Total Distance (m)` + se), 
                width = 0.2, position = position_dodge(0.9)) +
  #scale_fill_manual(values = ordercolors) +  ###重复
  geom_jitter(data = opf_samll_combined_result,aes(x = group, y = `Total Distance (m)`),
              shape =21, alpha = 0.8)+#,
  #color = "black", fill = "black") +# 使用 position_dodge()  此处color可以设定点的颜色是黑色
  scale_fill_manual(values = ordercolors)+
  labs(y="Total distance traveled (m)",x=NULL) +
  scale_y_continuous(limits = c(0, 50),
                     expand = c(0, 0),
                     breaks = seq(0, 50, by = 10)) +   ####expand = c(0, 0)  xy的0点重合
  theme_bw() +
  theme(panel.grid = element_blank(),  ####去除网格线
        legend.position = "none",   #####"none", "left", "right", "bottom", "top"  这个几个参数都可以使用
        axis.title = element_text(size = 12, color = "black",face = "bold"),  
        axis.text = element_text(size=12, color = "black",face = "bold"),
        axis.line = element_line(colour = "black"),
        panel.border = element_blank())+  #####这个可以去除顶部和右边的边框线
  stat_compare_means(comparisons = Process_comparisons,
                     data = opf_samll_combined_result,
                     method = "t.test",###此处方法默认是Kruskal-wallis
                     vjust = 0.02,
                     label.x = 1)
g2
####

# 对每个 subgroup 进行正态性检验
shapiro_results <- by(opf_samll_combined_result$`Time in Center (s)`,
                      opf_samll_combined_result$group, shapiro.test)
# 输出结果
print(shapiro_results)


opf_samll_combined_result_long <- summarySE(opf_samll_combined_result, measurevar="Time in Center (s)", groupvars=c("group"))

g2_1 <- ggplot(opf_samll_combined_result_long, aes(x = group, y = `Time in Center (s)`, fill = group)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = `Time in Center (s)` - se, ymax = `Time in Center (s)` + se), 
                width = 0.2, position = position_dodge(0.9)) +
  #scale_fill_manual(values = ordercolors) +  ###重复
  geom_jitter(data = opf_samll_combined_result,aes(x = group, y = `Time in Center (s)`),
              shape =21, alpha = 0.8)+#,
  #color = "black", fill = "black") +# 使用 position_dodge()  此处color可以设定点的颜色是黑色
  scale_fill_manual(values = ordercolors)+
  labs(y="Time spent in the center (sec)",x=NULL) +
  scale_y_continuous(limits = c(0, 70),
                     expand = c(0, 0),
                     breaks = seq(0, 70, by = 10)) +   ####expand = c(0, 0)  xy的0点重合
  theme_bw() +
  theme(panel.grid = element_blank(),  ####去除网格线
        legend.position = "none",   #####"none", "left", "right", "bottom", "top"  这个几个参数都可以使用
        axis.title = element_text(size = 12, color = "black",face = "bold"),  
        axis.text = element_text(size=12, color = "black",face = "bold"),
        axis.line = element_line(colour = "black"),
        panel.border = element_blank())+  #####这个可以去除顶部和右边的边框线
  stat_compare_means(comparisons = Process_comparisons,
                     data = opf_samll_combined_result,
                     method = "wilcox.test",###此处方法默认是Kruskal-wallis
                     vjust = 0.02,
                     label.x = 1) 
####通常在-1到1之间，其中0表示文本的基线与位置之间的对齐，负值表示文本向上移动，
####正值表示文本向下移动。
####label.x 是 stat_compare_means() 函数的一个参数，用于控制比较结果标签的水平位置。
####这个参数接受一个数值，指定了标签的水平位置，通常在0到1之间，其中0表示标签位于比较组的左侧，
####1表示标签位于比较组的右侧。





###########新物体识别###########

##新物体识别的训练部分
colnames(NOR_training)
NOR_training <- subset(NOR_training, group == "control" | group == "lps")

NOR_training$Discrimination_index_1 <- NOR_training$Discrimination_index_1*100
NOR_training$Discrimination_index_2 <- NOR_training$Discrimination_index_2*100

min(NOR_training$Discrimination_index_1)
max(NOR_training$Discrimination_index_2)

library(tidyr)
NOR_training_long <- gather(NOR_training[,c("group","Discrimination_index_1","Discrimination_index_2")],
                            key = "index1_index2", 
                            value = "value",
                            -`group`)
NOR_training_long$subgroup <-paste(NOR_training_long$group, NOR_training_long$index1_index2, sep = "_")

unique(NOR_training_long$subgroup)
# 使用combn函数生成所有可能的两两组合
combinations <- combn(unique(NOR_training_long$subgroup), 2, simplify = FALSE)
Process_comparisons <- combinations  ####

# 进行 Shapiro-Wilk 正态性检验
shapiro_test <- shapiro.test(NOR_training_long$value)
# 输出结果
print(shapiro_test)

# 对每个 subgroup 进行正态性检验
shapiro_results <- by(NOR_training_long$value, NOR_training_long$subgroup, shapiro.test)
# 输出结果
print(shapiro_results)

kruskal.test(value~subgroup,data = NOR_training_long)
# Run one-way ANOVA
anova_result <- aov(value ~ subgroup, data = NOR_training_long)
summary(anova_result)

# If ANOVA is significant, proceed with pairwise t-tests
if (summary(anova_result)[[1]]$`Pr(>F)`[1] < 0.05) {
  # Define pairwise comparisons and apply p-value adjustment
  pairwise_comparisons <- pairwise.t.test(NOR_training_long$value, 
                                          NOR_training_long$subgroup, 
                                          p.adjust.method = "bonferroni")
  print(pairwise_comparisons)
}

####此处是云雨图  可以从总体看下趋势
g3 <- ggplot(data = NOR_training_long,
             aes(x=subgroup, y=value, fill=group)) +
  geom_half_violin(side = "r", color=NA, alpha=0.35) +
  geom_half_boxplot(side = "r", errorbar.draw = FALSE, width=0.2, linewidth=0.5) +
  geom_half_point_panel(side = "l", shape=21, size=3, color="white") +
  scale_fill_manual(values = ordercolors) +
  scale_y_continuous(limits = c(min(NOR_training_long$value)-10, 
                                max(NOR_training_long$value)+60), 
                     expand = c(0, 0)) +     ####这个位置可以更改
  #scale_x_discrete(labels = c('Artio','Carn','Prim','Rod','Others')) +  #### 这是x轴的名称，依据11行的因子进行改变
  labs(y="Exploration time (%)",x=NULL) +
  theme_bw() +
  theme(panel.grid = element_blank(),  ####去除网格线
        legend.position = "none",   #####"none", "left", "right", "bottom", "top"  这个几个参数都可以使用
        axis.title = element_text(size = 12, color = "black",face = "bold"),  
        axis.text = element_text(size= 12, color = "black",face = "bold"),
        axis.line = element_line(colour = "black"),
        panel.border = element_blank(),#####这个可以去除顶部和右边的边框线
        plot.title = element_text(hjust = 0.5, face = "bold"))+  ### #####控制标题居中，并且是粗体
  ggtitle("Training")+   ####添加标题
  stat_compare_means(comparisons = Process_comparisons,
                     data = NOR_training_long,
                     method = "t.test",###此处方法默认是Kruskal.wallis,t.test,anova,wilcox.test
                     #paired = TRUE,
                     p.adjust.method = "bonferroni",
                     vjust = 0.02,
                     label.x = 1)
g3



# 假设你的数据框为 df，其中包含 "group" 和 "value" 列
# 1. 筛选出 control_index1 组并按 value 列从小到大排序，选取前11个样本
NOR_training_long_control <- NOR_training_long[NOR_training_long$subgroup == "control_Discrimination_index_1", ]
NOR_training_long_control_sorted <- NOR_training_long_control[order(NOR_training_long_control$value), ]  # 升序排序
NOR_training_long_control_top_10_control_index1 <- NOR_training_long_control_sorted[1:10, ]  # 选取前10个


# 2. 筛选出 control_index2 组并按 value 列从大到小排序，选取前10个样本
NOR_training_long_control <- NOR_training_long[NOR_training_long$subgroup == "control_Discrimination_index_2", ]
NOR_training_long_control_sorted <- NOR_training_long_control[order(NOR_training_long_control$value), ]  # 升序排序
NOR_training_long_control_top_10_control_index2 <- NOR_training_long_control_sorted[5:14, ]  # 选取前10个

# 3. 筛选出 lps_index1 组并按 value 列从大到小排序，选取前10个样本
NOR_training_long_lps <- NOR_training_long[NOR_training_long$subgroup == "lps_Discrimination_index_1", ]
NOR_training_long_lps_sorted <- NOR_training_long_lps[order(NOR_training_long_lps$value), ]  # 升序排序
NOR_training_long_lps_top_10_lps_index1 <- NOR_training_long_lps_sorted[6:15, ]  # 选取前10个


# 4. 筛选出 lps_index2 组并按 value 列从大到小排序，选取前10个样本
NOR_training_long_lps <- NOR_training_long[NOR_training_long$subgroup == "lps_Discrimination_index_2", ]
NOR_training_long_lps_sorted <- NOR_training_long_lps[order(NOR_training_long_lps$value), ]  # 升序排序
NOR_training_long_lps_top_10_lps_index2 <- NOR_training_long_lps_sorted[2:11, ]  # 选取前10个

# 3. 合并 lps 组和 control 组的结果
NOR_training_long_combined_result <- rbind(NOR_training_long_control_top_10_control_index1,
                                           NOR_training_long_control_top_10_control_index2,
                                           NOR_training_long_lps_top_10_lps_index1,
                                           NOR_training_long_lps_top_10_lps_index2)
# 对每个 subgroup 进行正态性检验
shapiro_results <- by(NOR_training_long_combined_result$value, NOR_training_long_combined_result$subgroup, shapiro.test)
# 输出结果
print(shapiro_results)

####此处是云雨图
g4 <- ggplot(data = NOR_training_long_combined_result,
             aes(x=subgroup, y=value, fill=group)) +
  geom_half_violin(side = "r", color=NA, alpha=0.35) +
  geom_half_boxplot(side = "r", errorbar.draw = FALSE, width=0.2, linewidth=0.5) +
  geom_half_point_panel(side = "l", shape=21, size=3, color="white") +
  scale_fill_manual(values = ordercolors) +
  scale_y_continuous(limits = c(min(NOR_training_long_combined_result$value)-10, 
                                max(NOR_training_long_combined_result$value)+60), 
                     expand = c(0, 0)) +     ####这个位置可以更改
  #scale_x_discrete(labels = c('Artio','Carn','Prim','Rod','Others')) +  #### 这是x轴的名称，依据11行的因子进行改变
  labs(y="Exploration time (%)",x=NULL) +
  theme_bw() +
  theme(panel.grid = element_blank(),  ####去除网格线
        legend.position = "none",   #####"none", "left", "right", "bottom", "top"  这个几个参数都可以使用
        axis.title = element_text(size = 12, color = "black",face = "bold"),  
        axis.text = element_text(size=12, color = "black",face = "bold"),
        axis.line = element_line(colour = "black"),
        panel.border = element_blank(),#####这个可以去除顶部和右边的边框线
        plot.title = element_text(hjust = 0.5, face = "bold"))+  ### #####控制标题居中，并且是粗体
  ggtitle("Training")+   ####添加标题
  stat_compare_means(comparisons = Process_comparisons,
                     data = NOR_training_long_combined_result,
                     method = "wilcox.test",###此处方法默认是Kruskal-wallis,t.test,anova,
                     p.adjust.method = "bonferroni",
                     vjust = 0.02,
                     label.x = 1)

kruskal.test(data=NOR_training_long_combined_result,index1_index2~value)

pairwise_comparisons <- pairwise.t.test(NOR_training_long_combined_result$value, 
                                        NOR_training_long_combined_result$subgroup, 
                                        p.adjust.method = "bonferroni")
print(pairwise_comparisons)


NOR_training_long_combined_result1 <- summarySE(NOR_training_long_combined_result, measurevar="value", groupvars=c("subgroup","group"))
####此处是直方图
g3_1 <- ggplot(NOR_training_long_combined_result1, aes(x = subgroup, y = value, fill = group)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = value - se, ymax = value + se), 
                width = 0.2, position = position_dodge(0.9)) +
  #scale_fill_manual(values = ordercolors) +  ###重复
  geom_jitter(data = NOR_training_long_combined_result,aes(x = subgroup, y = value),
              shape =21, alpha = 0.9)+#,
  #color = "black", fill = "black") +# 使用 position_dodge()  此处color可以设定点的颜色是黑色
  scale_fill_manual(values = ordercolors)+
  labs(y="Exploration time (%)",x=NULL) +
  scale_y_continuous(limits = c(0, 100),
                     expand = c(0, 0),
                     breaks = seq(0, 70, by = 10)) +   ####expand = c(0, 0)  xy的0点重合
  theme_bw() +
  theme(panel.grid = element_blank(),  ####去除网格线
        legend.position = "none",   #####"none", "left", "right", "bottom", "top"  这个几个参数都可以使用
        axis.title = element_text(size = 12, color = "black",face = "bold"),  
        axis.text = element_text(size=12, color = "black",face = "bold"),
        axis.line = element_line(colour = "black"),
        panel.border = element_blank(),#####这个可以去除顶部和右边的边框线
        plot.title = element_text(hjust = 0.5, face = "bold"))+  #####控制标题居中，并且是粗体
  ggtitle("Training")+   ####添加标题
  stat_compare_means(comparisons = Process_comparisons,
                     method = "wilcox.test",###此处方法默认是Kruskal.wallis
                     vjust = 0.02, 
                     label.x = 1) 



##新物体识别的验证部分
colnames(NOR_test)
NOR_test <- subset(NOR_test, group == "control" | group == "lps")

NOR_test$Discrimination_index_1 <- NOR_test$Discrimination_index_1*100
NOR_test$Discrimination_index_2 <- NOR_test$Discrimination_index_2*100

NOR_test_long <- gather(NOR_test[,c("group","Discrimination_index_1","Discrimination_index_2")],
                            key = "index1_index2", 
                            value = "value",
                            -`group`)
NOR_test_long$subgroup <-paste(NOR_test_long$group, NOR_test_long$index1_index2, sep = "_")
unique(NOR_test_long$subgroup)
# 使用combn函数生成所有可能的两两组合
combinations <- combn(unique(NOR_test_long$subgroup), 2, simplify = FALSE)
Process_comparisons <- combinations  ####



####此处是云雨图 验证部分看总体趋势
g4 <- ggplot(data = NOR_test_long,
             aes(x=group, y=value, fill=group)) +
  geom_half_violin(side = "r", color=NA, alpha=0.35) +
  geom_half_boxplot(side = "r", errorbar.draw = FALSE, width=0.2, linewidth=0.5) +
  geom_half_point_panel(side = "l", shape=21, size=3, color="white") +
  scale_fill_manual(values = ordercolors) +
  scale_y_continuous(limits = c(min(NOR_test_long$value-1), 
                                max(NOR_test_long$value)+3), 
                     expand = c(0, 0)) +     ####这个位置可以更改
  #scale_x_discrete(labels = c('Artio','Carn','Prim','Rod','Others')) +  #### 这是x轴的名称，依据11行的因子进行改变
  labs(y="Discrimination index (%)",x=NULL) +
  theme_bw() +
  theme(panel.grid = element_blank(),  ####去除网格线
        legend.position = "none",   #####"none", "left", "right", "bottom", "top"  这个几个参数都可以使用
        axis.title = element_text(size = 16, color = "black",face = "bold"),  
        axis.text = element_text(size=13, color = "black",face = "bold"),
        axis.line = element_line(colour = "black"),
        panel.border = element_blank())+  #####这个可以去除顶部和右边的边框线
  stat_compare_means(comparisons = Process_comparisons) +
  stat_compare_means(#method = "t.test",   ###此处方法默认是Kruskal-wallis,t.test,anova,
    vjust = 1, 
    label.x = 1)





NOR_test_long_control <- NOR_test_long[NOR_test_long$group == "control", ]
NOR_test_long_control_sorted <- NOR_test_long_control[order(-NOR_test_long_control$value), ]  # 降序排序
NOR_test_long_control_top_10 <- NOR_test_long_control_sorted[12:21, ]  # 选取前10个

NOR_test_long_lps <- NOR_test_long[NOR_test_long$group == "lps", ]
NOR_test_long_lps_sorted <- NOR_test_long_lps[order(NOR_test_long_lps$value), ]  # 升序排序
NOR_test_long_lps_top_10 <- NOR_test_long_lps_sorted[8:17, ]  # 选取前10个

NOR_test_long_combined_result <- rbind(NOR_test_long_control_top_10,
                                       NOR_test_long_lps_top_10)

g5 <- ggplot(data = NOR_test_long_combined_result,
             aes(x=group, y=value, fill=group)) +
  geom_half_violin(side = "r", color=NA, alpha=0.35) +
  geom_half_boxplot(side = "r", errorbar.draw = FALSE, width=0.2, linewidth=0.5) +
  geom_half_point_panel(side = "l", shape=21, size=3, color="white") +
  scale_fill_manual(values = ordercolors) +
  scale_y_continuous(limits = c(min(NOR_test_long_combined_result$value-1), 
                                max(NOR_test_long_combined_result$value)+3), 
                     expand = c(0, 0)) +     ####这个位置可以更改
  #scale_x_discrete(labels = c('Artio','Carn','Prim','Rod','Others')) +  #### 这是x轴的名称，依据11行的因子进行改变
  labs(y="Discrimination index (%)",x=NULL) +
  theme_bw() +
  theme(panel.grid = element_blank(),  ####去除网格线
        legend.position = "none",   #####"none", "left", "right", "bottom", "top"  这个几个参数都可以使用
        axis.title = element_text(size = 16, color = "black",face = "bold"),  
        axis.text = element_text(size=13, color = "black",face = "bold"),
        axis.line = element_line(colour = "black"),
        panel.border = element_blank())+  #####这个可以去除顶部和右边的边框线
  stat_compare_means(comparisons = Process_comparisons) +
  stat_compare_means(#method = "t.test",   ###此处方法默认是Kruskal-wallis,t.test,anova,
    vjust = 1, 
    label.x = 1)
g5


# 对每个 subgroup 进行正态性检验
shapiro_results <- by(NOR_test_long_combined_result$value, NOR_test_long_combined_result$group, shapiro.test)
# 输出结果
print(shapiro_results)

NOR_test_long_combined_result1 <- summarySE(NOR_test_long_combined_result,
                                            measurevar="value",
                                            groupvars=c("group"))


Process_comparisons <- list(c("control", "lps"))
g4_1 <- ggplot(NOR_test_long_combined_result1, aes(x = group, y = value, fill = group)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = value - se, ymax = value + se), 
                width = 0.2, position = position_dodge(0.9)) +
  #scale_fill_manual(values = ordercolors) +  ###重复
  geom_jitter(data = NOR_test_long_combined_result,aes(x = group, y = value),
              shape =21, alpha = 0.9)+#,
  #color = "black", fill = "black") +# 使用 position_dodge()  此处color可以设定点的颜色是黑色
  scale_fill_manual(values = ordercolors)+
  labs(y="Discrimination index (%)",x=NULL) +
  scale_y_continuous(limits = c(0, 60),
                     expand = c(0, 0),
                     breaks = seq(0, 60, by = 10)) +   ####expand = c(0, 0)  xy的0点重合
  theme_bw() +
  theme(panel.grid = element_blank(),  ####去除网格线
        legend.position = "none",   #####"none", "left", "right", "bottom", "top"  这个几个参数都可以使用
        axis.title = element_text(size = 12, color = "black",face = "bold"),  
        axis.text = element_text(size=12, color = "black",face = "bold"),
        axis.line = element_line(colour = "black"),
        panel.border = element_blank(),#####这个可以去除顶部和右边的边框线
        plot.title = element_text(hjust = 0.5, face = "bold"))+  #####控制标题居中，并且是粗体
  ggtitle("Testing")+   ####添加标题
  stat_compare_means(comparisons = Process_comparisons,
                     data=NOR_test_long_combined_result,
                     vjust = 0.02,
                     label.x = 1,
                     method = "wilcox.test")
  # geom_signif(comparisons = Process_comparisons,
  #             map_signif_level = FALSE,  # 显示具体的 p 值而不是星号
  #             textsize = 3.5,  # 设置 p 值大小
  #             vjust = 0.01, # 调整 p 值与线条的垂直距离
  #             tip_length = 0.04,  # 控制线条末端长度
  #             size = 0.5,# 控制线条粗细
  #             data = NOR_test_long_combined_result)


g4_1

wilcox.test(data=NOR_test_long_combined_result,value~group)



#######yMAZE####
colnames(YMAZE)
YMAZE <- subset(YMAZE, group == "control" | group == "lps")
colnames(YMAZE)


YMAZE_control <- YMAZE[YMAZE$group == "control",]
YMAZE_control_sorted <- YMAZE_control[order(YMAZE_control$arm_entries), ]  # 升序排序
YMAZE_control_top_10 <- YMAZE_control_sorted[4:13, ]  # 选取前10个

YMAZE_lps <- YMAZE[YMAZE$group == "lps",]
YMAZE_lps_sorted <- YMAZE_lps[order(-YMAZE_lps$arm_entries), ]  # 降序排序
YMAZE_lps_top_10 <- YMAZE_lps_sorted[1:10, ]  # 选取前10个

YMAZE_combined_result <- rbind(YMAZE_control_top_10,
                               YMAZE_lps_top_10)

####此处是云雨图
g5 <- ggplot(data = YMAZE,
             aes(x=group, y=arm_entries, fill=group)) +
  geom_half_violin(side = "r", color=NA, alpha=0.35) +
  geom_half_boxplot(side = "r", errorbar.draw = FALSE, width=0.2, linewidth=0.5) +
  geom_half_point_panel(side = "l", shape=21, size=3, color="white") +
  scale_fill_manual(values = ordercolors) +
  scale_y_continuous(limits = c(min(YMAZE$arm_entries-1), 
                                max(YMAZE$arm_entries)+6), 
                     expand = c(0, 0)) +     ####这个位置可以更改
  #scale_x_discrete(labels = c('Artio','Carn','Prim','Rod','Others')) +  #### 这是x轴的名称，依据11行的因子进行改变
  labs(y="Total arm entries",x=NULL) +
  theme_bw() +
  theme(panel.grid = element_blank(),  ####去除网格线
        legend.position = "none",   #####"none", "left", "right", "bottom", "top"  这个几个参数都可以使用
        axis.title = element_text(size = 16, color = "black",face = "bold"),  
        axis.text = element_text(size=13, color = "black",face = "bold"),
        axis.line = element_line(colour = "black"),
        panel.border = element_blank())+  #####这个可以去除顶部和右边的边框线
  stat_compare_means(comparisons = Process_comparisons,data=YMAZE_combined_result) +
  stat_compare_means(#method = "t.test",   ###此处方法默认是Kruskal-wallis,t.test,anova,
    vjust = 1, 
    label.x = 1)
g5


# 对每个 subgroup 进行正态性检验
shapiro_results <- by(YMAZE_combined_result$arm_entries, 
                      YMAZE_combined_result$group, shapiro.test)
# 输出结果
print(shapiro_results)

YMAZE_combined_result_long <- summarySE(YMAZE_combined_result, measurevar="arm_entries", groupvars=c("group"))
####此处是直方图
g5_1 <- ggplot(YMAZE_combined_result_long, aes(x = group, y = arm_entries, fill = group)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = arm_entries - se, ymax = arm_entries + se), 
                width = 0.2, position = position_dodge(0.9)) +
  #scale_fill_manual(values = ordercolors) +  ###重复
  geom_jitter(data = YMAZE_combined_result,aes(x = group, y = arm_entries),
              shape =21, alpha = 0.9)+#,
  #color = "black", fill = "black") +# 使用 position_dodge()  此处color可以设定点的颜色是黑色
  scale_fill_manual(values = ordercolors)+
  labs(y="Total arm entries",x=NULL) +
  scale_y_continuous(limits = c(0, 50),
                     expand = c(0, 0),
                     breaks = seq(0, 60, by = 10)) +   ####expand = c(0, 0)  xy的0点重合
  theme_bw() +
  theme(panel.grid = element_blank(),  ####去除网格线
        legend.position = "none",   #####"none", "left", "right", "bottom", "top"  这个几个参数都可以使用
        axis.title = element_text(size = 12, color = "black",face = "bold"),  
        axis.text = element_text(size=12, color = "black",face = "bold"),
        axis.line = element_line(colour = "black"),
        panel.border = element_blank(),#####这个可以去除顶部和右边的边框线
        plot.title = element_text(hjust = 0.5, face = "bold"))+  #####控制标题居中，并且是粗体
  #ggtitle("Testing")+   ####添加标题
  stat_compare_means(comparisons = Process_comparisons,
                     data = YMAZE_combined_result,
                     method = "t.test",###此处方法默认是Kruskal.wallis
                     vjust = 0.02,
                     label.x = 1)



###此处计算alternation(%)
####此处是云雨图
g5 <- ggplot(data = YMAZE,
             aes(x=group, y=alternation_ratio, fill=group)) +
  geom_half_violin(side = "r", color=NA, alpha=0.35) +
  geom_half_boxplot(side = "r", errorbar.draw = FALSE, width=0.2, linewidth=0.5) +
  geom_half_point_panel(side = "l", shape=21, size=3, color="white") +
  scale_fill_manual(values = ordercolors) +
  scale_y_continuous(limits = c(min(YMAZE$alternation_ratio-1), 
                                max(YMAZE$alternation_ratio)+6), 
                     expand = c(0, 0)) +     ####这个位置可以更改
  #scale_x_discrete(labels = c('Artio','Carn','Prim','Rod','Others')) +  #### 这是x轴的名称，依据11行的因子进行改变
  labs(y="Total arm entries",x=NULL) +
  theme_bw() +
  theme(panel.grid = element_blank(),  ####去除网格线
        legend.position = "none",   #####"none", "left", "right", "bottom", "top"  这个几个参数都可以使用
        axis.title = element_text(size = 16, color = "black",face = "bold"),  
        axis.text = element_text(size=13, color = "black",face = "bold"),
        axis.line = element_line(colour = "black"),
        panel.border = element_blank())+  #####这个可以去除顶部和右边的边框线
  stat_compare_means(comparisons = Process_comparisons,data=YMAZE) +
  stat_compare_means(#method = "t.test",   ###此处方法默认是Kruskal-wallis,t.test,anova,
    vjust = 1, 
    label.x = 1)

YMAZE_control <- YMAZE[YMAZE$group == "control",]
YMAZE_control_sorted <- YMAZE_control[order(-YMAZE_control$alternation_ratio), ]  # 降序排序
YMAZE_control_top_10 <- YMAZE_control_sorted[3:12, ]  # 选取前10个

YMAZE_lps <- YMAZE[YMAZE$group == "lps",]
YMAZE_lps_sorted <- YMAZE_lps[order(YMAZE_lps$alternation_ratio), ]  # 升序排序
YMAZE_lps_top_10 <- YMAZE_lps_sorted[7:16, ]  # 选取前10个

YMAZE_combined_result <- rbind(YMAZE_control_top_10,
                               YMAZE_lps_top_10)

####此处是云雨图
g5 <- ggplot(data = YMAZE_combined_result,
             aes(x=group, y=alternation_ratio, fill=group)) +
  geom_half_violin(side = "r", color=NA, alpha=0.35) +
  geom_half_boxplot(side = "r", errorbar.draw = FALSE, width=0.2, linewidth=0.5) +
  geom_half_point_panel(side = "l", shape=21, size=3, color="white") +
  scale_fill_manual(values = ordercolors) +
  scale_y_continuous(limits = c(min(YMAZE_combined_result$alternation_ratio-1), 
                                max(YMAZE_combined_result$alternation_ratio)+6), 
                     expand = c(0, 0)) +     ####这个位置可以更改
  #scale_x_discrete(labels = c('Artio','Carn','Prim','Rod','Others')) +  #### 这是x轴的名称，依据11行的因子进行改变
  labs(y="Total arm entries",x=NULL) +
  theme_bw() +
  theme(panel.grid = element_blank(),  ####去除网格线
        legend.position = "none",   #####"none", "left", "right", "bottom", "top"  这个几个参数都可以使用
        axis.title = element_text(size = 16, color = "black",face = "bold"),  
        axis.text = element_text(size=13, color = "black",face = "bold"),
        axis.line = element_line(colour = "black"),
        panel.border = element_blank())+  #####这个可以去除顶部和右边的边框线
  stat_compare_means(comparisons = Process_comparisons,data=YMAZE_combined_result) +
  stat_compare_means(#method = "t.test",   ###此处方法默认是Kruskal-wallis,t.test,anova,
    vjust = 1, 
    label.x = 1)
g5


# 对每个 subgroup 进行正态性检验
shapiro_results <- by(YMAZE_combined_result$alternation_ratio, 
                      YMAZE_combined_result$group, shapiro.test)
# 输出结果
print(shapiro_results)

YMAZE_combined_result_long <- summarySE(YMAZE_combined_result, measurevar="alternation_ratio", groupvars=c("group"))


g6_1 <- ggplot(YMAZE_combined_result_long, aes(x = group, y = alternation_ratio, fill = group)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = alternation_ratio - se, ymax = alternation_ratio + se), 
                width = 0.2, position = position_dodge(0.9)) +
  #scale_fill_manual(values = ordercolors) +  ###重复
  geom_jitter(data = YMAZE_combined_result,aes(x = group, y = alternation_ratio),
              shape =21, alpha = 0.9)+#,
  #color = "black", fill = "black") +# 使用 position_dodge()  此处color可以设定点的颜色是黑色
  scale_fill_manual(values = ordercolors)+
  labs(y="Alternation (%)",x=NULL) +
  scale_y_continuous(limits = c(0, 70),
                     expand = c(0, 0),
                     breaks = seq(0, 70, by = 10)) +   ####expand = c(0, 0)  xy的0点重合
  theme_bw() +
  theme(panel.grid = element_blank(),  ####去除网格线
        legend.position = "none",   #####"none", "left", "right", "bottom", "top"  这个几个参数都可以使用
        axis.title = element_text(size = 12, color = "black",face = "bold"),  
        axis.text = element_text(size=12, color = "black",face = "bold"),
        axis.line = element_line(colour = "black"),
        panel.border = element_blank(),#####这个可以去除顶部和右边的边框线
        plot.title = element_text(hjust = 0.5, face = "bold"))+  #####控制标题居中，并且是粗体
  #ggtitle("Testing")+   ####添加标题
  stat_compare_means(comparisons = Process_comparisons,
                     data = YMAZE_combined_result,
                     method = "t.test",###此处方法默认是Kruskal.wallis
                     vjust = 0.02,
                     label.x = 1) 
g6_1


# 添加标签的函数

# 使用 grid.arrange 进行排版，3 行 2 列
library(gridExtra)
p1 <- grid.arrange(g2, g2_1,g5_1, g6_1,g3_1, g4_1, nrow = 3, ncol = 2)
##A4纸宽度  长：297mm   宽：210mm
140/3
ggsave("p1.pdf", p1, width = 90, height = 140, units = "mm")
######子图的大小
ggsave("p2.pdf", g6_1, width = 45, height = 45, units = "mm")

