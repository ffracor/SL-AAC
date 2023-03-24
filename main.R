install.packages("tidyverse")
install.packages("caret")
install.packages("leaps")
install.packages("MASS")


library(tidyverse)
library(caret)
library(leaps)
library("MASS")
library("glmnet")

dataset = read.csv("dataset.csv")

mydata <- subset(dataset, position == "Forward")
mydata <- mydata[,-6];
mydata <- mydata[,colSums(is.na(mydata))==0]
labels = colnames(mydata);




# Fit the full model 
full.model <- lm(X2021.guaranteed.comp. ~. -X2022.guar..comp. -name -club_abb -club -birth_city -birth_country -foot, data = mydata)
# Stepwise regression model
step.model <- stepAIC(full.model, direction = "both", 
                      trace = FALSE)
summary(step.model)
l = 10^seq(-2,3, length=50);

set.seed(1);

x <- model.matrix(X2021.guaranteed.comp. ~ .-X2022.guar..comp. -name -club_abb -club -birth_city -birth_country -foot, data = mydata)[ , -1]
y <- mydata$X2021.guaranteed.comp.

cv_lasso <- cv.glmnet(x,y, alpha = 1, nfolds = 10, lambda = NULL);
plot(cv_lasso)
opt_lambda = cv_lasso$lambda.min;

glm_model_lasso = glmnet(x,y,alpha = 1, lambda = opt_lambda)
summary(glm_model_lasso)
glm_model_lasso$beta

fitt_value <- predict(model = glm_model_lasso, s = opt_lambda, newx = x)

glm_model_ridge = glmnet(x,y,alpha = 0, lambda = opt_lambda, newx = x)
summary(glm_model_ridge)
glm_model_ridge$beta
glm_model_ridge$

mdl_ridge = predict(model=glm_model_ridge, s = opt_lambda, newx = x)




