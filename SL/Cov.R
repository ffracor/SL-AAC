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

n = 150

set.seed(42)
train <- sample(dim(x)[1],floor(dim(x)[1]*0.75),replace = FALSE);

rand_model <- randomForest(x$Sleep.efficiency ~ . ,data = x, subset = train,
                           mtry = 5, importance = TRUE, replace = TRUE, ntree = n, keep.forest = TRUE)


#funzione calcolo R^2
calcoloR_2 <- function(y, y_hat){
  D_tot = sum((y - mean(y))^2)
  D_res = sum((y - y_hat)^2)
  return( 1- D_res/D_tot)
}


plot(rand_model, main="Random forest model")
rand_fit <- predict(rand_model, newdata = x[-train,])
plot(rand_fit, x$Sleep.efficiency[-train])
abline(0,1)
MSE_rand_RF = mean((x$Sleep.efficiency[-train] - rand_fit)^2)
r2_rand_RF <- calcoloR_2(x$Sleep.efficiency[-train], rand_fit)

get_alpha_tree <- function(data,index){
  
  temp_train <- sample(train, length(train), replace = TRUE)
  
  temp_features <- sample(10, 5, replace = FALSE)
  temp_features[temp_features==4] <- 11
  
  temp_tree_model <- tree(data$Sleep.efficiency ~ . , data= data[,temp_features], subset= temp_train, split='gini')
  
  temp_fitt_value <- predict(temp_tree_model, newdata = data[-train, temp_features])
  
  #temp_test_MSE = mean((data$Sleep.efficiency[-train] - temp_fitt_value)^2)
  temp_r2 <- calcoloR_2(data$Sleep.efficiency[-train], temp_fitt_value)
  
  return (temp_fitt_value)
}

#res <- boot(x,get_alpha_tree,R=1000) #attenzione 'x' minuscola in questo caso

####Prova sostituzione boot con for per bagging

# valore_predizioni <- matrix(nrow= n, ncol=97)
# 
# for (i in 1:n){
#   temp_train <- sample(train, length(train), replace = TRUE)
#   temp_features <- sample(10, 3, replace = FALSE)
#   temp_features[temp_features==4] <- 11
#   temp_tree_model <- tree(x$Sleep.efficiency ~ . , data= x[,temp_features], subset= temp_train)
#   
#   
#   tree_cv <- cv.tree(temp_tree_model, FUN = prune.tree)
#   # for regression FUN = prune.tree
#   
#   best = min(tree_cv$size[tree_cv$dev == min(tree_cv$dev)])
#   #k = min(tree_cv$k[tree_cv$dev == min(tree_cv$dev)]) #alpha in the book
#   
#   prune <- prune.tree(temp_tree_model, best = best)
#   
#   if(class(prune) == "tree"){
#     temp_fitt_value <- predict(prune, newdata = x[-train, temp_features])
#     valore_predizioni[i,] <- temp_fitt_value
#   }
#   else {
#     valore_predizioni[i,] <- mean(y[temp_train])
#   }
#   
#   #temp_fitt_value <- predict(prune, newdata = x[-train, temp_features])
#   #temp_fitt_value <- predict(temp_tree_model, newdata = x[-train, temp_features])
#   #valore_predizioni[i,] <- temp_fitt_value
# }
# 
# IC_up_predictions_3 <- numeric(nrow(x[-train,]))
# IC_down_predictions_3 <- numeric(nrow(x[-train,]))
# medie_pre_3 <- numeric(length(IC_up_predictions_3))
# y_test_3 <- x[-train,4]
# isDentro_3 <- numeric(ncol(valore_predizioni))
# 
# for (k in 1:ncol(valore_predizioni)){
#   IC_up_predictions_3[k] <- quantile(valore_predizioni[,k], 0.975)
#   IC_down_predictions_3[k] <- quantile(valore_predizioni[,k], 0.025)
#   medie_pre_3[k] = mean(valore_predizioni[,k])
#   isDentro_3[k] <- ifelse(y_test_3[k] >= IC_down_predictions_3[k] && y_test_3[k] <= IC_up_predictions_3[k], 1, 0)
#   
#   
#   }
# 
# MSE_rand_naive = mean((x$Sleep.efficiency[-train] - medie_pre_3)^2)
# 
# #media del valore IC up IC down
# tabella_pred_3 <- data.frame(medie_pre_3,  IC_down_predictions_3, y_test_3, IC_up_predictions_3,isDentro_3)
# sum(isDentro_3)
# 






########
res_predizioni_3 <- boot(x,get_alpha_tree,R=n) #attenzione 'x' minuscola in questo caso


#hist(res[["t"]][,1])
#r2_RF <- mean(res_predizioni_3[["t"]][,2])
hist(res_predizioni_3[["t"]][,2])

IC_up_predictions_3 <- numeric(nrow(x[-train,]))
IC_down_predictions_3 <- numeric(nrow(x[-train,]))
medie_pre_3 <- numeric(length(IC_up_predictions_3))
y_test_3 <- x[-train,4]
isDentro_3 <- numeric(ncol(res_predizioni_3[["t"]]))

for (k in 1:ncol(res_predizioni_3[["t"]])){
  IC_up_predictions_3[k] <- quantile(res_predizioni_3[["t"]][,k], 0.975)
  IC_down_predictions_3[k] <- quantile(res_predizioni_3[["t"]][,k], 0.025)
  medie_pre_3[k] = mean(res_predizioni_3[["t"]][,k])
  isDentro_3[k] <- ifelse(y_test_3[k] >= IC_down_predictions_3[k] && y_test_3[k] <= IC_up_predictions_3[k], 1, 0)
}

MSE_rand = mean((x$Sleep.efficiency[-train] - medie_pre_3)^2)
sqrt(MSE_rand)

r2_naive_RF <- calcoloR_2(x$Sleep.efficiency[-train], medie_pre_3)

#media del valore IC up IC down
tabella_pred_3 <- data.frame(medie_pre_3,  IC_down_predictions_3, y_test_3, IC_up_predictions_3,isDentro_3)
sum(isDentro_3)

#plot dei grafici (escono brutti, non inserire)
# Creazione del grafico a linea con i primi 30 elementi dei tre vettori
plot(IC_down_predictions_3[1:30], type="l", col="red", ylim=c(min(IC_down_predictions_3, y_test_3, IC_up_predictions_3), max(IC_down_predictions_3, y_test_3, IC_up_predictions_3)), xlab="Numero di osservazioni", ylab="Valore", main="Grafico a linea")

# Aggiunta delle linee per i primi 30 elementi dei vettori y_test_3 e IC_up_predictions_3
lines(y_test_3[1:30], col="blue")
lines(IC_up_predictions_3[1:30], col="green")

# Aggiunta della legenda
legend("topright", legend=c("IC_down_predictions_3", "y_test_3", "IC_up_predictions_3"), col=c("red", "blue", "green"), lty=c(1,1,1))


# non utilizzatre bootstrap. ma inserire degli alberi normali di tipo 2000 e abbiamo una stima puntuale per ogni punto
# così abbiamo una distribuzione per ogni punto e facciamo il quantile su questa distribuzione.

# altrimenti un random forest in cui abbiamo una sola stima della media non abbiamo una distribuzione




### parte con ipotesi di normalità


IC_up_predictions <- numeric(nrow(x[-train,]))
IC_down_predictions <- numeric(nrow(x[-train,]))
medie_pre <- numeric(length(IC_up_predictions))
y_test <- x[-train,4]
isDentro <- numeric(ncol(res_predizioni[["t"]]))

for (k in 1:ncol(res_predizioni[["t"]])){
  IC_up_predictions[k] <- quantile(res_predizioni[["t"]][,k], 0.975)
  IC_down_predictions[k] <- quantile(res_predizioni[["t"]][,k], 0.025)
  medie_pre[k] = mean(res_predizioni[["t"]][,k])
  isDentro[k] <- ifelse(y_test[k] >= IC_down_predictions[k] && y_test[k] <= IC_up_predictions[k], 1, 0)
}

#pnormGC(0.975, region = 'below', mean = 0, std = 1, graph = TRUE)
z = qnorm(0.975, mean=0, sd=1)

IC_up_predictions_2 <- numeric(nrow(x[-train,]))
IC_down_predictions_2 <- numeric(nrow(x[-train,]))
isDentro_2 <- numeric(length(rand_fit))

for (k in 1:length(rand_fit)){
  IC_up_predictions_2[k] <- rand_fit[k] + z*sqrt(MSE_rand)
  IC_down_predictions_2[k] <- rand_fit[k] - z*sqrt(MSE_rand)
  isDentro_2[k] <- ifelse(y_test[k] >= IC_down_predictions_2[k] && y_test[k] <= IC_up_predictions_2[k], 1, 0)
}

#media del valore IC up IC down
tabella_pred <- data.frame(medie_pre,  IC_down_predictions, y_test, IC_up_predictions,isDentro)

#media del valore IC up IC down
tabella_pred_2 <- data.frame(rand_fit,  IC_down_predictions_2, y_test, IC_up_predictions_2,isDentro_2)

  
  
  
  
  
  