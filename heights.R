#Using the caret package
library(caret)
library(tidyverse)
#Loading Dataset:
library(dslabs)
data("heights")
set.seed(2)
y <- heights$sex
x <- heights$height
#Dividing the data into training and testing datasets:
index <- createDataPartition(y, times = 1, p = 0.5, list = F)
test_set <- heights[index,]
train_set <- heights[-index,]
#Creating a random predictor for gender:
y_hat <- sample(c("Male", "Female"), length(index), replace = T)%>%
  factor(levels = levels(test_set$sex))
#Overall accuracy of random predictor:
mean(y_hat == test_set$sex)

heights%>%group_by(sex)%>%
  summarise(mean(height), sd(height))
#New predictor of height computed for heights within 2 sds from mean of males:
y_hat <- ifelse(x>62,"Male", "Female")%>%
  factor(levels = levels(test_set$sex))
mean(y_hat == test_set$sex)

#Mapping the best cutoff value and assigning it to the model:

cutoff <- seq(61, 70)
accuracy <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train_set$height > x, "Male", "Female")%>%
    factor(levels = levels(test_set$sex))
  mean(y_hat == train_set$sex)
})
map <- cbind(cutoff, accuracy)
map
cutoff[which.max(accuracy)]
#New predictor based on the previous analysis:
y_hat <- ifelse(test_set$height>64,"Male", "Female")%>%
  factor(levels = levels(test_set$sex))
mean(y_hat == test_set$sex)

#ConfusionMatrix:

confusionMatrix(data = y_hat, reference = test_set$sex)


#Building new model with F1 score instead of accuracy:
cutoff <- seq(61, 70)
F_1 <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train_set$height > x, "Male", "Female")%>%
    factor(levels = levels(test_set$sex))
  F_meas(data = y_hat, reference = factor(train_set$sex))
})
map <- cbind(cutoff, F_1)
map
cutoff[which.max(F_1)]

y_hat <- ifelse(test_set$height>66,"Male", "Female")%>%
  factor(levels = levels(test_set$sex))
mean(y_hat == test_set$sex)
confusionMatrix(data = y_hat, reference = test_set$sex)
