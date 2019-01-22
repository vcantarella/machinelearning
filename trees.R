library(rpart)
set.seed(1225)
n <- 1000
sigma <- 0.25
x <- rnorm(n, 0, 1)
y <- 0.75 * x + rnorm(n, 0, sigma)
dat <- data.frame(x = x, y = y)
fit <- rpart(y ~ ., data = dat)
plot(fit, margin = 0.1)
text(fit, cex = 0.75)
library(tidyverse)
dat %>% 
  mutate(y_hat = predict(fit)) %>% 
  ggplot() +
  geom_point(aes(x, y)) +geom_step(aes(x, y_hat), col=2)



library(randomForest)
fit <- randomForest(y ~ x, data = dat, nodesize = 50, maxnodes = 25)
dat %>% 
  mutate(y_hat = predict(fit)) %>% 
  ggplot() +
  geom_point(aes(x, y)) +
  geom_step(aes(x, y_hat), col = 2)
plot(fit)  

#Caret Package
library(caret)
library(Rborist)
train(y ~ ., data = dat, method = "Rborist", tuneGrid = (data.frame(predFixed = 1, minNode = seq(25,100,25))))


dat %>% 
  mutate(y_hat = predict(fit)) %>% 
  ggplot() +
  geom_point(aes(x, y)) +
  geom_step(aes(x, y_hat), col = 2)

library(dslabs)
data("tissue_gene_expression")
set.seed(1991)
fit_rpart <- rpart(tissue_gene_expression$y ~ tissue_gene_expression$x, cp = 0.01, control = rpart.control(minsplit = 0))
treino <- train(tissue_gene_expression$x, tissue_gene_expression$y, method = "rpart",tuneGrid = data.frame(cp = seq(0,0.1,0.01)),
                control = rpart.control(minsplit = 0))

ggplot(treino)
confusionMatrix(treino)
plot(treino)

plot(fit, margin = 0.1)
text(fit, cex = 0.75)

set.seed(1991)
rtrain <- train(tissue_gene_expression$x, tissue_gene_expression$y, method = "rf",tuneGrid = data.frame(mtry = seq(50,200,25)),
                nodesize = 1)
rtrain
varImp(rtrain)

tree_terms <- as.character(unique(rtrain$finalModel$frame$var[!(rtrain$finalModel$frame$var == "<leaf>")]))
tree_terms
