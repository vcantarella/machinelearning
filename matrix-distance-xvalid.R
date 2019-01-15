#Q6 MAtrix exercise
library(dslabs)
if(!exists("mnist")) mnist <- read_mnist()
dim(mnist$train$images)
pre <- apply(mnist$train$images, 2, function(x){x> 50 & x  < 205} )
colMeans(pre)
dim(pre)
mean(colMeans(pre))

y <- rowMeans(mnist$train$images>50 & mnist$train$images<205)
qplot(as.factor(mnist$train$labels), y, geom = "boxplot")
mnist$train$labels

#Distance comprehension Check:
library(dslabs)
data("tissue_gene_expression")
dim(tissue_gene_expression$x)
table(tissue_gene_expression$y)
d <- dist(tissue_gene_expression$x)
as.matrix(d)

dist(tissue_gene_expression$x[c(1,39),])
dist(tissue_gene_expression$x[39:40,])
dist(tissue_gene_expression$x[73:74,])
ind <- c(1, 2, 39, 40, 73, 74)
as.matrix(d)[ind,ind]
image(as.matrix(d))

#Cross-validation Comprehension Check:
library(tidyverse)
library(caret)
set.seed(1996)
n <- 1000
p <- 10000
x <- matrix(rnorm(n*p), n, p)
colnames(x) <- paste("x", 1:ncol(x), sep = "_")
y <- rbinom(n, 1, 0.5) %>% factor()
#Train
x_subset <- x[ ,sample(p, 100)]
fit <- train(x_subset, y, method = "glm")
fit$results


library(devtools)
devtools::install_bioc("genefilter")
library(genefilter)
tt <- colttests(x, y)
tt$p.value
pvals <- tt$p.value
ind <- pvals < 0.01
sum(ind)
x_subset <- x[ ,ind]
fit <- train(x_subset, y, method = "glm")
fit$results
fit <- train(x_subset, y, method = "knn", tuneGrid = data.frame(k = seq(101, 301, 25)))
ggplot(fit)


fit <- train(tissue_gene_expression$x, tissue_gene_expression$y, method = "knn", tuneGrid = data.frame(k = seq(1, 101)))
ggplot(fit)



#Bootstrapping Comprehension Check:

library(tidyverse)
library(caret)
library(dslabs)
data("mnist_27")
set.seed(1995)
indexes <- createResample(mnist_27$train$y, 10)
sum(indexes[[1]]==7)#Calculate how many times each index appears
sum(indexes[[2]]==3)
sum(indexes[[1]]==4)

indexes[1]
k = seq(1,10)#Calculate how many times the index 3 appears in every resample
b <- map_dbl(k, function(x){
  sum(indexes[[x]]==3)
})
sum(b)

#Estimating with bootstrapping:
set.seed(1)
y <- rnorm(100, 0, 1) #Creating a sample
set.seed(1)
quantile <- replicate(10000, {
   S <- sample(y, 100, replace = T)
   quantile <- quantile(S, 0.75)
})

sd(quantile)


set.seed(1)
indexes <- createResample(y, 10)
q_75_star <- sapply(indexes, function(ind){
  y_star <- y[ind]
  quantile(y_star, 0.75)
})
mean(q_75_star)
sd(q_75_star)
set.seed(1)
y <- rnorm(100, 0, 1)
indexes <- createResample(y, 10000)
k = seq(1,10000)
b <- map_dbl(k, function(x){
  S <- y[indexes[[x]]]
  quantile(S, 0.75)
})
mean(b)
sd(b)
