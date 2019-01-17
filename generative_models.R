#Generative models

##Comprehension Check:
library(tidyverse)
library(caret)
library(dslabs)
set.seed(1993)
data("tissue_gene_expression")
ind <- which(tissue_gene_expression$y %in% c("cerebellum", "hippocampus"))
y <- droplevels(tissue_gene_expression$y[ind])
x <- tissue_gene_expression$x[ind, ]
x <- x[, sample(ncol(x), 10)]
train <- train(x,y, "lda", preProcessing = "scale")
train$finalModel$means%>%
  as.data.frame(.)%>%
  mutate(tissue = rownames(.))%>%
  gather(key = "gene", value = "mean", SAPCD1:C21orf62)%>%
  ggplot(aes(x = gene, y = mean, fill = tissue))+geom_bar(stat = "identity", position = "dodge")

set.seed(1993)
data("tissue_gene_expression")
ind <- which(tissue_gene_expression$y %in% c("cerebellum", "hippocampus"))
y <- droplevels(tissue_gene_expression$y[ind])
x <- tissue_gene_expression$x[ind, ]
x <- x[, sample(ncol(x), 10)]
train <- train(x,y, "qda")
train
train$finalModel$means

set.seed(1993)
data("tissue_gene_expression")
y <- tissue_gene_expression$y
x <- tissue_gene_expression$x
x <- x[, sample(ncol(x), 10)]
train <- train(x,y, "lda", preProcessing = "scale")
confusionMatrix(train)


