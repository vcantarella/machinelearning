##Exeercise to train Linear Regression
library(tidyverse)
library(caret)
###Create a Dataset:
set.seed(1)
n <- 100
Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
dat <- MASS::mvrnorm(n = 100, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))#This draws a dataset from a multivariate normal distribution
###Create training and test sets
rmse <- replicate(n = 100,{
  test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = F)
  train_set <- dat[-test_index,]#Create datasets
  test_set <- dat[test_index,]
  lm_model <- lm(y ~ x, data = train_set)#Model
  y_hat = predict(lm_model, test_set)#Predict
  RMSE = sqrt(mean((y_hat-test_set$y)^2))#Calculare RMSE
  RMSE
})

mean(rmse)
sd(rmse)

