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
set.seed(1)
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

##Next exercise with lagerdatasets:
set.seed(1)
n_rmse <- function(n) {

  Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
  dat <- MASS::mvrnorm(n, c(69, 69), Sigma) %>%
    data.frame() %>% setNames(c("x", "y"))#This draws a dataset from a multivariate normal distribution
  ###Create training and test sets

  rmse <- replicate(100,{
    test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = F)
    train_set <- dat[-test_index,]#Create datasets
    test_set <- dat[test_index,]
    lm_model <- lm(y ~ x, data = train_set)#Model
    y_hat = predict(lm_model, test_set)#Predict
    RMSE = sqrt(mean((y_hat-test_set$y)^2))#Calculare RMSE
    RMSE
  })
  list(mean(rmse), sd(rmse))
}
n <- c(100, 500, 1000, 5000, 10000)
map(n, n_rmse) 

###Same as 01 but with larger correlation:

set.seed(1)
n <- 100
Sigma <- 9*matrix(c(1.0, 0.95, 0.95, 1.0), 2, 2)
dat <- MASS::mvrnorm(n = 100, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))
set.seed(1)
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

##Q6 Linear model with more variables:

set.seed(1)
n <- 1000
Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.25, 0.75, 0.25, 1.0), 3, 3)
dat <- MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) %>%
  data.frame() %>% setNames(c("y", "x_1", "x_2"))
set.seed(1)
  test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = F)
  train_set <- dat[-test_index,]#Create datasets
  test_set <- dat[test_index,]
  lm_model <- lm(y ~ x_1, data = train_set)#Model
  y_hat = predict(lm_model, test_set)#Predict
  RMSE_x_1 = sqrt(mean((y_hat-test_set$y)^2))#Calculare RMSE
  lm_x2 <- lm(y ~ x_2, data = train_set)
  y_hat = predict(lm_x2, test_set)
  RMSE_x_2 = sqrt(mean((y_hat-test_set$y)^2))
  lm_x_all <- lm(y ~ x_1+ x_2, data = train_set)
  y_hat = predict(lm_x_all, test_set)
  RMSE_x_all = sqrt(mean((y_hat-test_set$y)^2))
  data.frame(RMSE_x_1,RMSE_x_2, RMSE_x_all)


##Q8 With highly correlated predictors:
  
  set.seed(1)
  n <- 1000
  Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.95, 0.75, 0.95, 1.0), 3, 3)
  dat <- MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) %>%
    data.frame() %>% setNames(c("y", "x_1", "x_2"))
  set.seed(1)
  test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = F)
  train_set <- dat[-test_index,]#Create datasets
  test_set <- dat[test_index,]
  lm_model <- lm(y ~ x_1, data = train_set)#Model
  y_hat = predict(lm_model, test_set)#Predict
  RMSE_x_1 = sqrt(mean((y_hat-test_set$y)^2))#Calculare RMSE
  lm_x2 <- lm(y ~ x_2, data = train_set)
  y_hat = predict(lm_x2, test_set)
  RMSE_x_2 = sqrt(mean((y_hat-test_set$y)^2))
  lm_x_all <- lm(y ~ x_1+ x_2, data = train_set)
  y_hat = predict(lm_x_all, test_set)
  RMSE_x_all = sqrt(mean((y_hat-test_set$y)^2))
  data.frame(RMSE_x_1,RMSE_x_2, RMSE_x_all)
  
