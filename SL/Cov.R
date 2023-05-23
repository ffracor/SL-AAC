# import libraries
library(tidyverse)
library(caret)
library(leaps)
library("MASS")
library("glmnet")
library ( ISLR2 )
library ( boot )
library(tree)
library ( randomForest )
library ( gbm )
library(recipes)
library("tseries")
library(tseries)
library(xtable)
library(groupdata2)
library(tseries)

# clear all environment variable 
rm(list = ls()) 


# PREPROCESSING

# Read Dataset from csv file
x = read.csv("Sleep_Efficiency.csv")

# creating dummy variable for gender and smoking status
x$Smoking.status <- as.numeric(as.factor(x$Smoking.status))
x$Smoking.status <- ifelse(x$Smoking.status==2,1,0)
x$Gender <- ifelse(x$Gender=="Male",1,0)

# removing useless column
x <- x[,-1]
x <- x[,-3]
x <- x[,-3]
x <- x[,-7]

# omitting NA and creating x and y variables
x <- na.omit(x)
y <- x$Sleep.efficiency

# saving number of original columns
dim = ncol(x)

n = 2000

set.seed(42)
train <- sample(dim(x)[1],floor(dim(x)[1]*0.75),replace = FALSE);

rand_model <- randomForest(x$Sleep.efficiency ~ . ,data = x, subset = train,
                           mtry = floor(sqrt(ncol(x)-1)), importance = TRUE, replace = TRUE, ntree = 200)

plot(rand_model, main="Random forest model")
rand_fit <- predict(rand_model, newdata = x[-train,])
plot(rand_fit, x$Sleep.efficiency[-train])
abline(0,1)
MSE_rand = mean((x$Sleep.efficiency[-train] - rand_fit)^2)
MSE_rand

get_predictors_IC <- function(data, index)
{
  temp_train <- sample(nrow(data), floor(nrow(data)), replace = TRUE)
  temp_rand_model <- randomForest(Sleep.efficiency ~ . ,data = data, subset = temp_train,
                                  mtry = floor(sqrt(ncol(x)-1)), importance = TRUE, replace = TRUE, ntree = 200)
  
  temp_fit <- predict(temp_rand_model, newdata = x[-train,])
  
  return(temp_fit)
}
res_predizioni <- boot(x[train,], get_predictors_IC, R=n)

IC_up_predictions <- numeric(nrow(x[-train,]))
IC_down_predictions <- numeric(nrow(x[-train,]))
medie_pre <- numeric(length(IC_up_predictions))
y_test <- x[-train,4]
isDentro <- numeric(ncol(res_predizioni[["t"]]))

for (k in 1:ncol(res_predizioni[["t"]])){
  IC_up_predictions[k] <- quantile(res_predizioni[["t"]][,k], 0.995)
  IC_down_predictions[k] <- quantile(res_predizioni[["t"]][,k], 0.005)
  medie_pre[k] = mean(res_predizioni[["t"]][,k])
  isDentro[k] <- ifelse(y_test[k] >= IC_down_predictions[k] && y_test[k] <= IC_up_predictions[k], 1, 0)
}

#media del valore IC up IC down
tabella_pred <- data.frame(medie_pre,  IC_down_predictions, y_test, IC_up_predictions,isDentro)

  
  
  
  
  
  
  