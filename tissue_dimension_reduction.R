#Tissue gene machine learning example:

library(dslabs)
data("tissue_gene_expression")
dim(tissue_gene_expression$x)
library(ggplot2)
library(tidyverse)
image(tissue_gene_expression$x[,c(1,2)], col = tissue_gene_expression$y)

#Plotting the two principal components and analysing for a cluster:
a <- prcomp(tissue_gene_expression$x)
a$x
data.frame(a$x[,c(1,2)], Tissue = tissue_gene_expression$y)%>%
  ggplot(aes(PC1,PC2, fill = Tissue))+geom_point(cex=3, pch=21)+
  coord_fixed(ratio = 1)+theme_bw()

c <- rowMeans(tissue_gene_expression$x)
data.frame(PC1 = a$x[,c(1)],avg = c, Tissue = tissue_gene_expression$y)%>%
  ggplot(aes(exp(avg),PC1, fill = Tissue))+geom_point(cex=3, pch=21)+
  coord_fixed(ratio = 1)+theme_bw()
data.frame(PC1 = a$x[,c(1)],avg = c, Tissue = tissue_gene_expression$y)%>%
  cor(PC1, c)
cor(PC1, c)
#Removing center:
x <- with(tissue_gene_expression, sweep(x, 1, rowMeans(x)))
pc <- prcomp(x)
data.frame(pc_1 = pc$x[,1], pc_2 = pc$x[,2], 
           tissue = tissue_gene_expression$y) %>%
  ggplot(aes(pc_1, pc_2, color = tissue)) +
  geom_point()

pc$x%>%
  data.frame(., tissue = tissue_gene_expression$y)%>%
  ggplot(aes(x = tissue, y = PC7))+geom_boxplot()


for(i in 1:10){
  boxplot(pc$x[,i] ~ tissue_gene_expression$y, main = paste("PC", i))
}  
pc$x%>%
  data.frame(., tissue = tissue_gene_expression$y)%>%
  summary(.)

importance_df <- data.frame(summary(pc)$importance)
importance_df <- importance_df[2,] %>% 
  gather(key = pc, value = importance)
importance_df <- importance_df %>% mutate(pc_index = as.integer(str_remove(importance_df$pc, "PC")))
importance_df$pc <- factor(importance_df$pc, levels = importance_df$pc[order(importance_df$pc_index)])
importance_df <- importance_df %>% mutate(cum_sum = cumsum(importance))

importance_df %>% 
  filter(pc_index < 20) %>% 
  arrange(pc_index, cum_sum) %>% 
  ggplot(aes(x = pc, y = cum_sum, fill=pc)) +
  geom_col() +
  scale_y_continuous(breaks = seq(0,1,0.1)) +
  theme_grey()


plot(summary(pc)$importance[3,])
