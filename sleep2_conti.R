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

# modifing rem and deep sleep percentage to their actual time value
x$REM.sleep.percentage <- x$Sleep.duration * x$REM.sleep.percentage/100
x$Deep.sleep.percentage <- x$Deep.sleep.percentage/100 * x$Sleep.duration
colnames(x)[5] <- "REM.sleep.duration"
colnames(x)[6] <- "Deep.sleep.duration"

# omitting NA and creating x and y variables
x <- na.omit(x)
y <- x$Sleep.efficiency

# saving number of original columns
dim = ncol(x)

# adding interaction variables and squares
for (i in 1:dim) {
  for (j in i:dim) {
    if(!(length(unique(x[,i]))==2 & (i==j)) & (i!=4) & (j!=4)) {
      x[, ncol(x)+1] <- x[,i]*x[,j]
      colnames(x)[ncol(x)] <- paste(colnames(x)[i],colnames(x)[j], sep = "")
    }
  }
}

set.seed(42)

train <- sample(dim(x)[1],floor(dim(x)[1]*0.75),replace = FALSE);

# STEPWISE

#inizializzo il vettore per il conteggio
col_name <- c("(Intercept)",colnames(x))
contatore_vect_SW <- setNames(numeric(length(col_name)), col_name)
dim = length(col_name)


# Define bootstrap function
get_alpha_SW <- function(data,index){
  
  temp_train <- sample(nrow(data), floor(nrow(data) * 0.75), replace = TRUE)
  
  mdl <- lm(Sleep.efficiency ~ . , data = x[temp_train,])
  
  step.model <- stepAIC(mdl, direction = "both", 
                        trace = FALSE)
  
  temp_fitt_value <- predict(step.model, data[-temp_train,])
  step_test_MSE = mean((data$Sleep.efficiency[-temp_train] - temp_fitt_value)^2)
  
  #parte per conteggio delle comparse
  coeff_stimati <- names(step.model$coefficients)
  i=0
  for (i in 1:dim){
    if(names(contatore_vect_SW[i]) %in% coeff_stimati){
      contatore_vect_SW[i] <<- contatore_vect_SW[i] + 1
    }
  }
  
  return (step_test_MSE)
}

# Use boot() function to perform bootstrap simulations
res_sw <- boot(x,get_alpha_SW,R=2000, parallel = "multicore")
MSE_SW <- res_sw[["t"]]

###############################################################################


#LASSO REGRESSION
X <- model.matrix(Sleep.efficiency ~ . , data = x)
Y <- y

#inizializzazione per conteggio lasso 
contatore_vect_LASSO <- setNames(numeric(length(col_name)), col_name)

#Bootstrap su lasso
get_alpha_L <- function(data,index){
  
  temp_train <- sample(nrow(data), floor(nrow(data) * 0.75), replace = TRUE)
  
  temp_cv_lasso <- cv.glmnet(data[temp_train,], Y[temp_train], alpha = 1, nfolds = 10, lambda = NULL);
  lasso_opt_lambda = temp_cv_lasso$lambda.min;
  
  glm_model_lasso = glmnet(data[temp_train,], Y[temp_train],alpha = 1, lambda = lasso_opt_lambda)
  
  
  temp_fitt_value <- predict(glm_model_lasso, s = lasso_opt_lambda, newx = data[-temp_train,])
  lasso_test_MSE = mean((Y[-temp_train] - temp_fitt_value)^2)
  
  coeff_stimati <- names(glm_model_lasso$beta[, 1][glm_model_lasso$beta[, 1] != 0])
  i=0
  for (i in 1:dim){
    if(names(contatore_vect_LASSO[i]) %in% coeff_stimati){
      contatore_vect_LASSO[i] <<- contatore_vect_LASSO[i] + 1
    }
  }
  
  return (c(lasso_opt_lambda, lasso_test_MSE))
}


# Use boot() function to perform bootstrap simulations
res_lasso <- boot(X,get_alpha_L,R=2000, parallel = "multicore")
MSE_lasso <- res_lasso[["t"]][,2]
lamda_lasso <- res_lasso[["t"]][,1]
opt_lambda_lasso <- mean(lamda_lasso)
mean_lasso <- mean(MSE_lasso)
mean_lasso


glm_model_lasso = glmnet(X, Y,alpha = 1, lambda = opt_lambda_lasso)
glm_model_lasso$beta
contatore_vect_LASSO
lasso_table <- table(glm_model_lasso$beta)
hist(lamda_lasso)
ic = c(quantile(lamda_lasso, .025), quantile(lamda_lasso, .975))
glm_model_lasso = glmnet(X, Y,alpha = 1, lambda = quantile(lamda_lasso, .975))
glm_model_lasso$beta


#######################################################################################

# RIDGE REGRESSION
get_alpha_ridge <- function(data,index){
  
  temp_train <- sample(nrow(data), floor(nrow(data) * 0.75), replace = FALSE)
  
  temp_cv_ridge <- cv.glmnet(data[temp_train,], Y[temp_train], alpha = 0, nfolds = 10, lambda = NULL);
  ridge_opt_lambda = temp_cv_ridge$lambda.min;
  
  glm_model_ridge = glmnet(data[temp_train,], Y[temp_train],alpha = 0, lambda = ridge_opt_lambda)
  
  temp_fitt_value <- predict(glm_model_ridge, s = ridge_opt_lambda, newx = data[-temp_train,])
  ridge_test_MSE = mean((Y[-temp_train] - temp_fitt_value)^2)
  
  return (c(ridge_opt_lambda, ridge_test_MSE))
}

# Use boot() function to perform bootstrap simulations
res_ridge <- boot(X,get_alpha_ridge,R=2000, parallel = "multicore")
MSE_ridge <- res_lasso[["t"]][,2]
lamda_ridge <- res_lasso[["t"]][,1]

##################################################################################

#BAGGING
bagg_model <- randomForest(Sleep.efficiency ~ . ,data = x, subset = train, mtry = ncol(x)-1, importance = TRUE, replace = TRUE, ntree = 2000)

plot(bagg_model)
bagg_fit <- predict(bagg_model, newdata = x[-train,])
plot(bagg_fit, x$Sleep.efficiency[-train])
abline(0,1)
MSE_bagg = mean((x$Sleep.efficiency[-train] - bagg_fit)^2)

#RANDOM FOREST
rand_model <- randomForest(x$Sleep.efficiency ~ . ,data = x, subset = train,
                           mtry = floor(sqrt(ncol(x)-1)), importance = TRUE, replace = TRUE, ntree = 2000)

plot(rand_model)
rand_fit <- predict(rand_model, newdata = x[-train,])
plot(rand_fit, x$Sleep.efficiency[-train])
abline(0,1)
MSE_rand = mean((x$Sleep.efficiency[-train] - rand_fit)^2)

#boosting

#Bootstrap su boosting
get_alpha_boost <- function(data,index){
  
  temp_train <- sample(nrow(data), floor(nrow(data) * 0.75), replace = TRUE)
  temp_boost_model <- gbm(Sleep.efficiency ~ . , data = x[temp_train, ], distribution = "gaussian" ,
                          n.trees = 1500, interaction.depth = 4, shrinkage = 0.01 , verbose = F)
  temp_fitt_value <- predict( temp_boost_model, newdata = data[-temp_train,], n.trees = 1500)
  temp_test_MSE = mean((data$Sleep.efficiency[-temp_train] - temp_fitt_value)^2)
  return (temp_test_MSE)
}

res_boost <- boot(x,get_alpha_boost,R=2000, parallel = "multicore") #attenzione 'x' minuscola in questo caso
MSE_boost <- res_boost[["t"]]

