library(BiocManager)
library(DESeq2)
library(readxl)
if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

BiocManager::install("GenomeInfoDbData")

BiocManager::install("DESeq2")
OTU <- read_excel("Unrarefied operational taxonomic unit table used for this study.xlsx")


OTU1 <-OTU[,c(1:43)]


OTU1 <- as.data.frame(OTU1)
rownames(OTU1) <- OTU1[,1]


OTU1 <- OTU1[,-1]

OTU1 <- as.matrix(OTU1)

group <- read_excel("group.xlsx")

dds <- DESeqDataSetFromMatrix(countData = OTU1, colData = group, design = ~group)#构建 DESeqDataSet 对象  
dds <- DESeq(dds) #差异分析
suppressMessages(dds)

res <- results(dds, contrast=c('group', 'delirium', 'control'))#提取分析结果
res = res[order(res$pvalue),]
res #查看结果
summary(res)  #简要统计结果
table(res$padj<0.05) #查看fdr校正后的P<0.05的个数


diff_OTU_deseq2 <-subset(res, padj < 0.05 & abs(log2FoldChange) > 1)
# 或diff_OTU_deseq2 <-subset(res,padj < 0.05 & (log2FoldChange > 1 | log2FoldChange < -1))
dim(diff_OTU_deseq2)
head(diff_OTU_deseq2)

resdata <-  merge(as.data.frame(res),as.data.frame(counts(dds,normalize=TRUE)),by="row.names",sort=FALSE)

OTU <- as.data.frame(OTU)
rownames(OTU) <- OTU$.ID



rownames(resdata) <- resdata$resdata
OTU$Row.names <- row.names(OTU)
OTU2 <- OTU[,c("Row.names","genus")]

a<- merge(as.data.frame(resdata),as.data.frame(OTU2),by="Row.names",sort=FALSE)

resdata$genus <- a$genus
resdata1 <- resdata
resdata1 <- resdata1[!duplicated(resdata1$genus),]
resdata1 <- na.omit(resdata1) 
rownames(resdata1) <- resdata1$genus
colnames(resdata1)
resdata1$genus
resdata2 <- resdata1[,c(2:50)]
resdata2

for (i in 1:nrow(resdata2)) {
  if (abs(resdata2[i,'log2FoldChange']) >= 0.5) resdata2[i,'select_change'] <- 'y' else resdata2[i,'select_change'] <- 'n'
  if (resdata2[i,'padj'] %in% NA | abs(resdata2[i,'padj']) >= 0.05) resdata2[i,'select_pvalue'] <- 'n' else resdata2[i,'select_pvalue'] <- 'y'
  resdata2[i,'select'] <- paste(resdata2[i,'select_change'], resdata2[i,'select_pvalue'], sep = '')
}

library(ggplot2)
library(ggrepel)
resdata2$select <- factor(resdata2$select, levels = c('nn', 'ny', 'yn', 'yy'), labels = c('p ≥ 0.05, FC < 2', 'p < 0.05, FC < 2', 'p ≥ 0.05, FC ≥ 2', 'p < 0.05, FC ≥ 2'))

volcano_plot_pvalue <- ggplot(resdata2, aes(log2FoldChange, -log(padj, 10)))+
  geom_point(aes(color = select), alpha = 0.6) +
  scale_color_manual(values = c('gray30', 'green4', 'red2', 'blue2')) +
  scale_y_continuous(limits = c(0,4),expand = c(0,0),breaks = seq(0, 4, by = 1))+
  
  theme(panel.grid = element_blank(), 
        legend.title = element_blank(), ####element_rect(fill = 'transparent')
        legend.key = element_blank(), ####element_rect(fill = 'transparent')
        panel.background = element_blank())+    #### element_rect(color = 'black', fill = 'transparent')
  geom_hline(yintercept = -log(0.05, 10), color = 'gray', size = 0.5, linetype = "dashed") +
  geom_vline(xintercept = c(-2, 2), color = 'gray', size = 0.5, linetype = "dashed") +  # 添加两条竖线
  labs(x = expression(log[2](Fold ~ Change)), y = expression(-log[10](pvalue)))+ ###
  geom_text_repel(
    data = subset(resdata2, padj < 0.05 & abs(log2FoldChange) > 2),  # 选择需要标注的点
    aes(label = genus),  # 假设 'genus' 列是要标注的点的名称
    size = 3,  # 控制文本大小
    box.padding = 0.3,  # 文本框与点的距离
    point.padding = 0.3,
    max.overlaps = 10  # 避免过多标签重叠
  )+
  theme(panel.grid=element_blank(),
        legend.position = "bottom",   # 将图例放到下方
        legend.direction = "horizontal",
        legend.text = element_text(face = "bold"),
        panel.border =element_rect(fill=NA,color="black",linewidth=0.75,linetype="solid"),
        axis.text.x=element_text(vjust=1,size=12,face = "bold",color="black"),
        axis.text.y=element_text(vjust=0.5,size=12,face = "bold",color="black"),
        axis.title.x = element_text(face = "bold"),##element_text(face = "bold"),  # X轴标题加粗
        axis.title.y = element_text(face = "bold"),  # Y轴标题加粗
        ##axis.line = element_line(linewidth = 0.75, color = "black"),
        plot.title = element_text(face = "bold"),  # 图表标题加粗（如果有）
        plot.subtitle = element_text(face = "bold"),  # 副标题加粗（如果有）
        plot.caption = element_text(face = "bold"),
        axis.ticks.y.right = element_blank(),  # 移除右边Y轴刻度
        axis.text.y.right = element_blank()    # 移除右边Y轴标签
  )
volcano_plot_pvalue


# with(resdata2, plot(log2FoldChange,-log10(pvalue), pch=20, main="Volcano plot", xlim=c(-7,6)))
# 
# # Add colored points: red if padj<0.05, orange of log2FC>1, green if both)
# with(subset(resdata2, padj<.05 ), points(log2FoldChange, -log10(pvalue), pch=20, col="red"))
# with(subset(resdata2, abs(log2FoldChange)>1), points(log2FoldChange, -log10(pvalue), pch=20, col="orange"))
# with(subset(resdata2, padj<.05 & abs(log2FoldChange)>1), points(log2FoldChange, -log10(pvalue), pch=20, col="green"))
# 
# library(calibrate)
# with(subset(resdata2, padj<.05 & abs(log2FoldChange)>2), textxy(log2FoldChange, -log10(pvalue), labs=genus, cex=.8))
