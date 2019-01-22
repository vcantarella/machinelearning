#Mnist Case Study:

#We are going to test the following models in this next exercise:

models <- c("glm", "lda",  "naive_bayes",  "svmLinear", 
            "gamboost",  "gamLoess", "qda", 
            "knn", "kknn", "loclda", "gam",
            "rf", "ranger",  "wsrf", "Rborist", 
            "avNNet", "mlp", "monmlp",
            "adaboost", "gbm",
            "svmRadial", "svmRadialCost", "svmRadialSigma")
library(caret)
library(dslabs)
set.seed(1)
data("mnist_27")
#Fitting for every model
fits <- lapply(models, function(model){ 
  print(model)
  train(y ~ ., method = model, data = mnist_27$train)
}) 

names(fits) <- models
#Testing predictions
test <- sapply(fits, function(model){
  predict(model, newdata = mnist_27$test)
})
length(mnist_27$test$y)
length(models)
#calculating predictions 
predi <- apply(test, 2, function(x){
  x == mnist_27$test$y
})
#Calculating general prediction accuracy
mean_models <- apply(predi, 2, function(x) {mean(x)})
mean(mean_models)


#Creating ensemble model
ens <- apply(test, 1, function(x){
  two = sum(x == "2")
  seven = sum(x == "7")
  ifelse(two > 7, "2", "7")
})

mean(ens == mnist_27$test$y)
sum(mean_models > mean(ens == mnist_27$test$y))
mean_models > mean(ens == mnist_27$test$y)

#Checking the training data for the best machine learning algorithms:

accu <- sapply(fits, function(x){
  min(x$results["Accuracy"])
})
accu
mean(sapply(accu, function(x){mean(x)}))
ind <- accu > 0.8 #Selecting models with accuracy higher than 0.8
sum(ind)

#Creating new ensemble model with higher accuracy
ens <- apply(test[,ind], 1, function(x){
  two = sum(x == "2")
  seven = sum(x == "7")
  ifelse(two > 7, "2", "7")
})
#New ensemble model accuracy
mean(ens == mnist_27$test$y) #Uhuuuu!
