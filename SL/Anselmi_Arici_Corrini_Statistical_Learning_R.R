# import libraries
install.packages("corrplot")
install.packages("gridExtra")
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
library(corrplot)
library(gridExtra)
library(ggplot2)


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

#analisi preliminare delle variabili
var_dati <- setNames(numeric(length(colnames(x))), colnames(x))
for(i in 1:length(var_dati)){
  var_dati[i] <- var(x[,i])
  hist(x[,i], main=names(var_dati)[i], sub="")
}

corr_matrix <- cor(x)
corrplot(corr_matrix, type="upper", order="hclust")

#istorammi

par(mfrow = c(3,2))
p1<-hist(x$Age, main="" , xlab = "Age", ylab= "Frequency")
p2<-barplot(table(x$Gender), main="" , xlab = "Gender", ylab= "Frequency")
p3<-barplot(table(x$Sleep.duration), main="" , xlab = "Sleep Duration", ylab= "Frequency")
p4<-hist(x$Sleep.efficiency, main="" , xlab = "Sleep Efficiency", ylab= "Frequency")
p5<-hist(x$REM.sleep.percentage, main="" , xlab = "REM Sleep Percentage", ylab= "Frequency")
p6<-hist(x$Deep.sleep.percentage, main="" , xlab = "Deep Sleep Percentage", ylab= "Frequency")
p7<-barplot(table(x$Awakenings), main="" , xlab = "Awakenings", ylab= "Frequency")
p8 <- barplot(table(x$Caffeine.consumption), main="" , xlab = "Caffeine Consumption", ylab= "Frequency")
p9<-barplot(table(x$Alcohol.consumption), main="" , xlab = "Alcohol Consumption", ylab= "Frequency")
p10<-barplot(table(x$Smoking.status), main="" , xlab = "Smoking Status", ylab= "Frequency")
p11<-barplot(table(x$Exercise.frequency), main="" , xlab = "Exercise Frequency", ylab= "Frequency")


# saving number of original columns
dim = ncol(x)

n = 100

set.seed(42)

train <- sample(dim(x)[1],floor(dim(x)[1]*0.75),replace = FALSE);

calcoloR_2 <- function(y, y_hat){
  D_tot = sum((y - mean(y))^2)
  D_res = sum((y - y_hat)^2)
  return( 1- D_res/D_tot)
}

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
  
  #parte per conteggio delle comparse
  coeff_stimati <- names(step.model$coefficients)
  i=0
  for (i in 1:dim){
    if(names(contatore_vect_SW[i]) %in% coeff_stimati){
      contatore_vect_SW[i] <<- contatore_vect_SW[i] + 1
    }
  }
}

# Use boot() function to perform bootstrap simulations
res_sw <- boot(x,get_alpha_SW,R = n, parallel = "multicore")

stepwise_significativi <- names(contatore_vect_SW[contatore_vect_SW > n/2])
stepwise_significativi <- stepwise_significativi[-1]

stepwise_final_model <- lm(reformulate(stepwise_significativi, "Sleep.efficiency"), data=x)
summary(stepwise_final_model)

# Define bootstrap function
get_alpha <- function(data,index){
  
  temp_train <- sample(nrow(data), floor(nrow(data) * 0.75), replace = TRUE)
  temp_mdl <- lm(reformulate(stepwise_significativi, "Sleep.efficiency"), data=x, subset = temp_train)
  temp_fitt_value <- predict(temp_mdl, data[-temp_train,])
  temp_test_MSE = mean((data$Sleep.efficiency[-temp_train] - temp_fitt_value)^2)
  return (c(temp_test_MSE, temp_mdl$coefficients))
}

# Use boot() function to perform bootstrap simulations
res <- boot(x,get_alpha,R=n)
Coefficients_stepwise_final <- res[["t"]][,2]
MSE_stepwise_final <- res[["t"]][,1]
mean(MSE_stepwise_final)
hist(MSE_stepwise_final, main="Stepwise MSE", sub="")


#creazione vettore per intervalli di confidenza
IC_up_SW <- setNames(numeric(length(stepwise_significativi)+2), c("MSE", "Intercept", stepwise_significativi))
IC_down_SW <- setNames(numeric(length(stepwise_significativi)+2), c("MSE", "Intercept", stepwise_significativi))
isSignificativo <- setNames(numeric(length(stepwise_significativi)+2), c("MSE", "Intercept", stepwise_significativi))

for (k in 1:ncol(res[["t"]])){
  IC_up_SW[k] <- quantile(res[["t"]][,k], 0.975)
  IC_down_SW[k] <- quantile(res[["t"]][,k], 0.025)
  isSignificativo[k] <- ifelse(IC_up_SW[k]*IC_down_SW[k]<=0,0,1)
}

media_beta <- numeric(length(stepwise_significativi)+1)

for (k in 2:ncol(res[["t"]])){
  media_beta[k-1] <- mean(res[["t"]][,k])
}
tabella_stepwise <- data.frame(media_beta, IC_down_SW[-1], IC_up_SW[-1], isSignificativo[-1], contatore_vect_SW[contatore_vect_SW > n/2]) 
<<results=tex>>
xtable(tabella_stepwise, digits=c(0,-3,-3,-3,1,4))


###############################################################################


#LASSO REGRESSION
X <- model.matrix(Sleep.efficiency ~ . , data = x)
Y <- y

#inizializzazione per conteggio lasso 
contatore_vect_LASSO <- setNames(numeric(length(col_name)), col_name)

cv_lasso <- cv.glmnet(X, Y, alpha = 1, nfolds = 10, lambda = NULL)
lasso_opt_lambda_s = cv_lasso$lambda.min
glm_model_lasso_s = glmnet(X[train,], Y[train],alpha = 1, lambda = lasso_opt_lambda_s)
plot(cv_lasso)


#Bootstrap su lasso
get_alpha_L <- function(data,index){
  
  temp_train <- sample(nrow(data), floor(nrow(data) * 0.75), replace = TRUE)
  
  glm_model_lasso = glmnet(data[temp_train,], Y[temp_train],alpha = 1, lambda = lasso_opt_lambda_s)
  
  temp_fitt_value <- predict(glm_model_lasso, s = lasso_opt_lambda_s, newx = data[-temp_train,])
  lasso_test_MSE = mean((Y[-temp_train] - temp_fitt_value)^2)
  
  coeff_stimati <- names(glm_model_lasso$beta[, 1][glm_model_lasso$beta[, 1] != 0])
  i=0
  for (i in 1:dim){
    if(names(contatore_vect_LASSO[i]) %in% coeff_stimati){
      contatore_vect_LASSO[i] <<- contatore_vect_LASSO[i] + 1
    }
  }
  
  return (lasso_test_MSE)
}

# Use boot() function to perform bootstrap simulations
res_lasso <- boot(X,get_alpha_L,R=n, parallel = "multicore")
MSE_lasso <- res_lasso[["t"]]
hist(MSE_lasso, main= "Lasso MSE")
mean(MSE_lasso)

lasso_significativi <- names(contatore_vect_LASSO[contatore_vect_LASSO > n/2])
lasso_significativi
lassoIsSignificativo <- ifelse((contatore_vect_LASSO > n/2), "Yes", "No")
tabella_lasso <- data.frame(glm_model_lasso_s$beta[,1], contatore_vect_LASSO[-4], lassoIsSignificativo[-4])
<<results=tex>>
xtable(tabella_lasso, digits=c(0, -5,4,0))

#######################################################################################

# RIDGE REGRESSION

cv_ridge <- cv.glmnet(X, Y, alpha = 0, nfolds = 10, lambda = NULL);
ridge_opt_lambda = cv_ridge$lambda.min;


get_alpha_ridge <- function(data,index){
  
  temp_train <- sample(nrow(data), floor(nrow(data) * 0.75), replace = TRUE)
  
  glm_model_ridge = glmnet(data[temp_train,], Y[temp_train],alpha = 0, lambda = ridge_opt_lambda)
  
  temp_fitt_value <- predict(glm_model_ridge, s = ridge_opt_lambda, newx = data[-temp_train,])
  ridge_test_MSE = mean((Y[-temp_train] - temp_fitt_value)^2)
  
  return  (ridge_test_MSE)
}

# Use boot() function to perform bootstrap simulations
res_ridge <- boot(X,get_alpha_ridge,R=n, parallel = "multicore")
MSE_ridge <- res_lasso[["t"]][,1]
hist(MSE_ridge, main="Ridge MSE")
mean(MSE_ridge)
plot(cv_ridge)

##################################################################################

#BAGGING
bagg_model <- randomForest(Sleep.efficiency ~ . ,data = x, subset = train, mtry = ncol(x)-1, importance = TRUE, replace = TRUE, ntree = 200)
plot(bagg_model, main="Bagging model")
bagg_fit <- predict(bagg_model, newdata = x[-train,])
plot(bagg_fit, x$Sleep.efficiency[-train])
abline(0,1)
MSE_bagg = mean((x$Sleep.efficiency[-train] - bagg_fit)^2)
MSE_bagg

#RANDOM FOREST

train <- sample(nrow(x), floor(nrow(x) * 0.75), replace = FALSE)

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
  temp_rand_model <- randomForest(Sleep.efficiency ~ . ,data = data[temp_train,],
                                  mtry = (ncol(x)-1), importance = TRUE, replace = TRUE, ntree = 200)
  
  temp_fit <- predict(temp_rand_model, newdata = x[-train])
  
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
xtable(tabella_pred, digits=c(0, -5, -5,-5))


#coverage LR


train <- sample(nrow(x), floor(nrow(x) * 0.75), replace = FALSE)

get_alpha <- function(data,index){
  
  temp_train <- sample(index,nrow(data), replace = TRUE)
  temp_mdl <- lm(reformulate(stepwise_significativi, "Sleep.efficiency"), data=data, subset = temp_train)
  temp_fitt_value <- predict(temp_mdl, x[-train,])
  return (temp_fitt_value)
}
res_predizioni <- boot(x[train,], get_alpha, R=n)

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



##############################################################################
#POISSON REGRESSION

xeq <- balance(
  data = x,
  size = 40,
  cat_col = 'Awakenings',
  id_col = NULL,
  id_method = "n_ids",
  mark_new_rows = FALSE,
  new_rows_col_name = ".new_row"
)


test_fix <- sample(nrow(x), floor(nrow(x) * 0.25), replace = TRUE)
x <- x[-test_fix,]

#inizializzo il vettore per il conteggio
col_name <- c("(Intercept)",colnames(x))
contatore_vect_POISS <- setNames(numeric(length(col_name)), col_name)
dim = length(col_name)

# Define bootstrap function
get_alpha_POISS <- function(data,index){
  
  temp_train <- sample(nrow(data), floor(nrow(data) * 0.75), replace = TRUE)
  
  pois_mdl <- glm(Awakenings ~ . , data = x[temp_train,] , family = poisson)
  
  step_pois <- stepAIC(pois_mdl, direction = "both", 
                        trace = FALSE)
  
  #parte per conteggio delle comparse
  coeff_stimati <- names(step_pois$coefficients)
  i=0
  for (i in 1:dim){
    if(names(contatore_vect_POISS[i]) %in% coeff_stimati){
      contatore_vect_POISS[i] <<- contatore_vect_POISS[i] + 1
    }
  }
}

# Use boot() function to perform bootstrap simulations
res_sw <- boot(x,get_alpha_POISS,R = n, parallel = "multicore")

stepwise_significativi_poiss <- names(contatore_vect_POISS[contatore_vect_POISS > n/2])
stepwise_significativi_poiss <- stepwise_significativi_poiss[-1]

stepwise_final_model_poiss <- glm(reformulate(stepwise_significativi_poiss, "Awakenings"), data=x, family = 'poisson')
summary(stepwise_final_model_poiss)

pred <- predict(stepwise_final_model_poiss,type="response")
plot(x$Awakenings,pred)

#sm.poisson
library(sm)
y <- as.factor(x$Awakenings)
poissmdl <- sm.poisson.bootstrap(x = x, y = y, h = 0.5)


#contrasts
pois <- glm(Awakenings ~ . , data = xeq , family = poisson)
summary(pois)
pred <- predict(pois, type = "response"); #predict of lambda

plot(xeq$Awakenings,pred)


#Classification (logistic regression)

log_reg <- glm(Awakenings ~ . ,data = x , family = binomial)

summary(log_reg) #dal summary si vede che i p-value sono alti e gli z sono vicini allo 0

fit <- predict( log_reg , type = "response") #all'interno di fit ho delle probabilità e se guardo tanto sono vicine a 0,5 ---> non buono
#Quindi sono molto sensibile a variazioni di dati

log <- polr(as.factor(Awakenings) ~ . ,data = x, method = "logistic")
pred <- predict(log ,type="probs")

v <- numeric(388)
for (i in 1:388) {
  v[i] <- which.max(pred[i,])-1
}
plot(x$Awakenings,v)


########################################################################################################################################
#TREE

set.seed(42)
train <- sample(dim(x)[1],floor(dim(x)[1]*0.75),replace = FALSE)
treeMdl <- tree(data = x, subset = train, formula = as.factor(Awakenings) ~ ., split = "gini")
plot(treeMdl)
text(treeMdl,pretty = 0)

cvTree <- cv.tree(treeMdl, FUN = prune.misclass)
summary(treeMdl)

plot(cvTree$size, cvTree$dev)
best <- min(cvTree$size[cvTree$dev == min(cvTree$dev)])
alfa <- min(cvTree$k[cvTree$dev == min(cvTree$dev)])

prTree <- prune.misclass(treeMdl, best = best)
plot(prTree)
text(prTree,pretty = 0)
summary(prTree)

#########################################################################################################################################

# KNN - non parametric method
library(class)
set.seed(42)
train <- sample(dim(x)[1],floor(dim(x)[1]*0.75),replace = FALSE)
data_train <- x[train,]
data_test <- x[-train,]
label_train <- unique(as.factor(x$Awakenings))

knn_fit <- knn(data_train,data_test,label_train, k=3)

table(knn_fit,Smarket$Direction[!train])

# Poisson Regression - GLM (Distribution = poisson)
pois_fit <- glm(bikers ~ mnth + hr + workingday + 
                  temp + weathersit, data = Bikeshare , family = poisson)

#contrasts(Bikeshare$hr)
summary(pois_fit)
pred <- predict(pois_fit, type = "response"); #predict of lambda, per questo posso avere anche numeri che non sono interi

#lambda è il parametro della poisson

plot(Bikeshare$bikers,pred)