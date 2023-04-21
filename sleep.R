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

install.packages('tseries')
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



# LINEAR REGRESSION
set.seed(421)

train <- sample(dim(x)[1],floor(dim(x)[1]*0.75),replace = FALSE);

mdl <- lm(Sleep.efficiency ~ . , data = x[train,])
summary(mdl)
mdl$coefficients

fitt_value <- predict.lm(mdl, x[-train,])

test_MSE = mean((y[-train] - fitt_value)^2)
test_MSE

jarque.bera.test((y[-train] - fitt_value))
hist((y[-train] - fitt_value))

# Define bootstrap function
get_alpha <- function(data,index){
  
  temp_train <- sample(nrow(data), floor(nrow(data) * 0.75), replace = FALSE)
  temp_mdl <- lm(Sleep.efficiency ~ . , data = data, subset = temp_train)
  temp_fitt_value <- predict(temp_mdl, data[-temp_train,])
  temp_test_MSE = mean((data$Sleep.efficiency[-temp_train] - temp_fitt_value)^2)
  return (temp_test_MSE)
}

# Use boot() function to perform bootstrap simulations
res <- boot(x,get_alpha,R=10)

summary(res)
pippo <- res[["t"]]
hist(pippo)
mean(pippo)


# STEPWISE
step.model <- stepAIC(mdl, direction = "both", 
                      trace = FALSE)
summary(step.model)
fitt_value <- predict(step.model,  x[-train,])
step_test_MSE = mean((y[-train] - fitt_value)^2)
step_test_MSE
step.model$coefficients


#inizializzo il vettore per il conteggio
col_name <- c("(Intercept)",colnames(x))
contatore_vect_SW <- setNames(numeric(length(col_name)), col_name)
dim = length(col_name)


# Define bootstrap function
get_alpha_SW <- function(data,index){
  
  temp_train <- sample(nrow(data), floor(nrow(data) * 0.75), replace = FALSE)
  
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
res <- boot(x,get_alpha_SW,R=5)
summary(res)
pippo <- res[["t"]]
hist(pippo)
mean(pippo)






#LASSO REGRESSION
l = 10^seq(-2,3, length=50);

set.seed(42);

X <- model.matrix(Sleep.efficiency ~ . , data = x)
Y <- y

cv_lasso <- cv.glmnet(X[train, ],Y[train], alpha = 1, nfolds = 10, lambda = NULL);
plot(cv_lasso)
lasso_opt_lambda = cv_lasso$lambda.min;
lasso_opt_lambda

glm_model_lasso <- glmnet(X[train, ],Y[train],alpha = 1, lambda = lasso_opt_lambda)
summary(glm_model_lasso)
glm_model_lasso$beta

lasso_fit <- predict(glm_model_lasso, newx = X[-train, ], s = lasso_opt_lambda)
lasso_test_MSE = mean((Y[-train] - lasso_fit)^2)
lasso_test_MSE
glm_model_lasso$beta

#inizializzazione per conteggio lasso 
contatore_vect_LASSO <- setNames(numeric(length(col_name)), col_name)


#Bootstrap su lasso
get_alpha_L <- function(data,index){
  
  temp_train <- sample(nrow(data), floor(nrow(data) * 0.75), replace = FALSE)
  
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
  
  return (lasso_test_MSE)
  
  #return (c(lasso_opt_lambda, lasso_test_MSE))
}

# Use boot() function to perform bootstrap simulations
res <- boot(X,get_alpha_L,R=10)
summary(res)

pippo <- res[["t"]]
#hist(pippo[,2])
hist(pippo)
#mean(pippo[,2])
mean(pippo)


# RIDGE REGRESSION
cv_ridge <- cv.glmnet(X[train,],Y[train], alpha = 0, nfolds = 10, lambda = NULL);
plot(cv_ridge)
ridge_opt_lambda = cv_ridge$lambda.min;
ridge_opt_lambda

glm_model_ridge = glmnet(X[train,],Y[train],alpha = 0, lambda = ridge_opt_lambda)
summary(glm_model_ridge)
glm_model_ridge$beta

ridge_fit <- predict(glm_model_ridge, s = ridge_opt_lambda , newx = X[-train,])
ridge_test_MSE = mean((y[-train] - ridge_fit)^2)
ridge_test_MSE

get_alpha_L <- function(data,index){
  
  temp_train <- sample(nrow(data), floor(nrow(data) * 0.75), replace = FALSE)
  
  temp_cv_ridge <- cv.glmnet(data[temp_train,], Y[temp_train], alpha = 0, nfolds = 10, lambda = NULL);
  ridge_opt_lambda = temp_cv_ridge$lambda.min;
  
  glm_model_ridge = glmnet(data[temp_train,], Y[temp_train],alpha = 0, lambda = ridge_opt_lambda)
  
  
  temp_fitt_value <- predict(glm_model_ridge, s = ridge_opt_lambda, newx = data[-temp_train,])
  ridge_test_MSE = mean((Y[-temp_train] - temp_fitt_value)^2)
  
  return (ridge_test_MSE)
  
  #return (c(lasso_opt_lambda, lasso_test_MSE))
}

# Use boot() function to perform bootstrap simulations
res <- boot(X,get_alpha_L,R=1000)
summary(res)

pippo <- res[["t"]]
#hist(pippo[,2])
hist(pippo)
#mean(pippo[,2])
mean(pippo)





#PARTE SUGLI ALBERI

##Metodi ad albero

#ALBERO SEMPLICE
tree_model <- tree( x$Sleep.efficiency ~ . -x$Sleep.efficiency , data= x, subset = train)
#tree_model <- tree( x$Sleep.efficiency ~ . -x$Sleep.efficiency , data= x, subset = train)
summary(tree_model)
plot(tree_model)
text(tree_model, pretty = 0)

tree_fit <- predict(tree_model, newdata = x[-train,])
tree_test_MSE = mean((x$Sleep.efficiency[-train] - tree_fit)^2)
tree_test_MSE

#Boostrap su albero semplice
get_alpha_tree <- function(data,index){
  
  temp_train <- sample(nrow(data), floor(nrow(data) * 0.75), replace = FALSE)
  temp_tree_model <- tree( data$Sleep.efficiency ~ . -data$Sleep.efficiency , data= data, subset = temp_train)
  temp_fitt_value <- predict(temp_tree_model, newdata = data[-temp_train,])
  temp_test_MSE = mean((data$Sleep.efficiency[-temp_train] - temp_fitt_value)^2)
  return (temp_test_MSE)
}

res <- boot(x,get_alpha_tree,R=100) #attenzione 'x' minuscola in questo caso
pippo <- res[["t"]]
#hist(pippo[,2])
hist(pippo)
#mean(pippo[,2])
mean(pippo)



#BAGGING
bagg_model <- randomForest(Sleep.efficiency ~ . ,data = x, subset = train, mtry = ncol(x)-1, importance = TRUE, replace = TRUE, ntree = 300)
summary(bagg_model)

plot(bagg_model)
bagg_fit <- predict(bagg_model, newdata = x[-train,])
plot(bagg_fit, x$Sleep.efficiency[-train])
abline(0,1)

bagg_test_MSE = mean((x$Sleep.efficiency[-train] - bagg_fit)^2)
bagg_test_MSE

#Bootstrap su bagging
get_alpha_bagg <- function(data,index){
  
  temp_train <- sample(nrow(data), floor(nrow(data) * 0.75), replace = FALSE)
  temp_bagg_model <- randomForest(Sleep.efficiency ~ . ,data = data, subset = temp_train, mtry = ncol(data)-1, importance = TRUE, replace = TRUE, ntree = 100)
  temp_fitt_value <- predict( temp_bagg_model, newdata = data[-temp_train,])
  temp_test_MSE = mean((data$Sleep.efficiency[-temp_train] - temp_fitt_value)^2)
  return (temp_test_MSE)
}

res <- boot(x,get_alpha_bagg,R=100) #attenzione 'x' minuscola in questo caso
pippo <- res[["t"]]

hist(pippo)
mean(pippo)



#RANDOM FOREST
rand_model <- randomForest(x$Sleep.efficiency ~ . ,data = x, subset = train,
                           mtry = floor(sqrt(ncol(x)-1)), importance = TRUE, replace = TRUE, ntree = 300)
summary(rand_model)

plot(rand_model)
rand_fit <- predict(rand_model, newdata = x[-train,])
plot(rand_fit, x$Sleep.efficiency[-train])
abline(0,1)

rand_test_MSE = mean((x$Sleep.efficiency[-train] - rand_fit)^2)
rand_test_MSE

#boosting
boost_model <- gbm(Sleep.efficiency ~ . , data = x[train, ],distribution = "gaussian" ,
                   n.trees = 300, interaction.depth = 4, shrinkage = 0.01 , verbose = F)

boost_fit <- predict(boost_model, newdata = x[-train,], n.trees = 300)
boost_test_MSE <- mean((boost_fit - x$Sleep.efficiency[-train])^2)
boost_test_MSE

#Bootstrap su bagging
get_alpha_boost <- function(data,index){
  
  temp_train <- sample(nrow(data), floor(nrow(data) * 0.75), replace = FALSE)
  temp_boost_model <- gbm(Sleep.efficiency ~ . , data = x[temp_train, ], distribution = "gaussian" ,
                          n.trees = 300, interaction.depth = 4, shrinkage = 0.01 , verbose = F)
  temp_fitt_value <- predict( temp_boost_model, newdata = data[-temp_train,], n.trees = 300)
  temp_test_MSE = mean((data$Sleep.efficiency[-temp_train] - temp_fitt_value)^2)
  return (temp_test_MSE)
}

res <- boot(x,get_alpha_boost,R=100) #attenzione 'x' minuscola in questo caso
pippo <- res[["t"]]

hist(pippo)
mean(pippo)
