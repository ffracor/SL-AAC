library(tidyverse)
library(caret)
library(leaps)
library("MASS")
library("glmnet")

library ( ISLR2 )
library ( boot )

rm(list = ls()) # clear all environment variable

# PREPROCESSING
x = read.csv("/Users/anselminicolas/Desktop/Uni/Quarto anno/Statistical Learning/Progetto/Sleep_Efficiency.csv")
x$Smoking.status <- as.numeric(as.factor(x$Smoking.status))
x$Gender <- ifelse(x$Gender=="Male",1,0)
x <- x[,-1]
x <- x[,-3]
x <- x[,-3]

sum(is.na(x))
x <- na.omit(x)
y <- x$Sleep.efficiency


# LINEAR REGRESSION
set.seed(42)

#train <- sample(dim(x)[1],floor(dim(x)[1]*0.75),replace = FALSE);
train <- sample(nrow(x), floor(nrow(x) * 0.75), replace = FALSE)


mdl <- lm(Sleep.efficiency ~ . , data = x[train,])
summary(mdl)

fitt_value <- predict(mdl,newx = x[-train,])

test_MSE = mean((y[-train] - fitt_value)^2)
test_MSE


# STEPWISE
step.model <- stepAIC(mdl, direction = "both", 
                      trace = FALSE)
summary(step.model)
fitt_value <- predict(step.model,newx = x[-train,])
step_test_MSE = mean((y[-train] - fitt_value)^2)
step_test_MSE

# LASSO REGRESSION
l = 10^seq(-2,3, length=50);

set.seed(42);

X <- model.matrix(Sleep.efficiency ~ . , data = x)
Y <- y

cv_lasso <- cv.glmnet(X[train,],Y[train], alpha = 1, nfolds = 10, lambda = NULL);
plot(cv_lasso)
lasso_opt_lambda = cv_lasso$lambda.min;
lasso_opt_lambda

glm_model_lasso = glmnet(X[train,],Y[train],alpha = 1, lambda = lasso_opt_lambda)
summary(glm_model_lasso)
glm_model_lasso$beta

lasso_fit <- predict(glm_model_lasso,s = lasso_opt_lambda, newx = X[-train,])
lasso_test_MSE = mean((y[-train] - lasso_fit)^2)
lasso_test_MSE

# RIDGE REGRESSION
cv_ridge <- cv.glmnet(X[train,],Y[train], alpha = 0, nfolds = 10, lambda = NULL);
plot(cv_ridge)
ridge_opt_lambda = cv_ridge$lambda.min;
ridge_opt_lambda

glm_model_ridge = glmnet(X[train,],Y[train],alpha = 0, lambda = ridge_opt_lambda)
summary(glm_model_ridge)
glm_model_ridge$beta

ridge_fit <- predict(glm_model_ridge,s = ridge_opt_lambda ,newx = X[-train,])
ridge_test_MSE = mean((y[-train] - ridge_fit)^2)
ridge_test_MSE

# 

x = read.csv("/Users/anselminicolas/Desktop/Uni/Quarto anno/Statistical Learning/Progetto/Sleep_Efficiency.csv")
x$Smoking.status <- as.numeric(as.factor(x$Smoking.status))
x$Gender <- ifelse(x$Gender=="Male",1,0)
x <- x[,-1]
x <- x[,-3]
x <- x[,-3]
x$REM.sleep.percentage <- x$Sleep.duration * x$REM.sleep.percentage/100
x$Deep.sleep.percentage <- x$Deep.sleep.percentage/100 * x$Sleep.duration
x$Light.sleep.percentage <- x$Light.sleep.percentage/100 * x$Sleep.duration



