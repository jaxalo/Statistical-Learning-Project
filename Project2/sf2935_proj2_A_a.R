
rm(list = ls())
library(dplyr)
library(e1071)
library(tidyverse)
library(ggplot2)
library(MASS)
library(FNN)
library(class)
library(DMwR)
library(gridExtra)

load_auto_ds <- function()
{
  auto <- ISLR::Auto;
  return (auto);
}

compute_median <- function(data)
{
  d1 <- as.numeric(unlist(data))
  median <-  median(d1,na.rm = TRUE);
  return (median);
}


scale_only_numeric <- function(df)
{
  for (colName in names(df)) {
    
    # Check if the column contains numeric data.
    if(class(df[,colName]) == 'integer' | class(df[,colName]) == 'numeric') {
      
      # Scale this column (scale() function applies z-scaling).
      df[,colName] <- scale(df[,colName])
    }
  }
  return(df);
}

transform_auto_ds <- function(attribute_name = 'mpg')
{
  attribute_name = 'mpg'
  auto <- load_auto_ds();
  median_mpg <- compute_median(auto[attribute_name]);
  #add column
  auto <- mutate(auto, HL_mpg = mpg > median_mpg);
  auto['mpg'] <- NULL;
  
  #remove variable for the training
  remove <- c('name','year','origin');
  for(e in remove)
  {
    auto[e] <- NULL;
  }
  
  auto <- scale_only_numeric(auto);
  auto$HL_mpg=as.factor(as.numeric(auto$HL_mpg))
 
  
  return (auto);
}


set_split<- function(x)
{
  auto<-transform_auto_ds()

  smp_size <- floor(0.75 * nrow(auto))

  set.seed(123)
  train_ind <- sample(seq_len(nrow(auto)), size = smp_size)

  train <-auto[train_ind, ]
  test <- auto[-train_ind, ]
  
  model_svm = svm(factor(HL_mpg) ~ ., data = train, kernel = "polynomial", cost = 10,gamma=8,degree=5,scale=FALSE)
  
  predictions_train <- predict(model_svm, newdata=test);
  sucess_rate_train = mean(predictions_train==test$HL_mpg) 
  
  
  
}
auto<-transform_auto_ds()
smp_size <- floor(0.75 * nrow(auto))

set.seed(123)
train_ind <- sample(seq_len(nrow(auto)), size = smp_size)

train <-auto[train_ind, ]
test <- auto[-train_ind, ]
auto<-transform_auto_ds()

svm_tune <- tune(svm, HL_mpg ~ ., data = train,
                 ranges = list(epsilon = seq(0,1,0.01), cost = 2^(2:9)),kernel="linear"
)
print(svm_tune)

best_mod <- svm_tune$best.model
best_mod_pred <- predict(best_mod, test) 
error_best_mod <- mean(best_mod_pred==test$HL_mpg)
print(error_best_mod)

svm_tune_poly <- tune(svm, HL_mpg ~ ., data = train,
                 ranges = list(gamma = seq(0,1,0.01), cost = 2^(2:9)),kernel="polynomial")
print(svm_tune_poly)
best_mod_poly <- svm_tune_poly$best.model
best_mod_pred_poly <- predict(best_mod_poly, test) 
error_best_mod_poly <- mean(best_mod_pred_poly==test$HL_mpg)
print(error_best_mod_poly)

svm_tune_radial <- tune(svm, HL_mpg ~ ., data = train,
                      ranges = list(gamma = seq(0,1,0.01), cost = 2^(2:9)),kernel="radial")
print(svm_tune_radial)
best_mod_radial <- svm_tune_radial$best.model
best_mod_pred_radial <- predict(best_mod_radial, test) 
error_best_mod_radial <- mean(best_mod_pred_radial==test$HL_mpg)
print(error_best_mod_radial)

svm_tune_sigmoid <- tune(svm, HL_mpg ~ ., data = train,
                        ranges = list(gamma = seq(0,1,0.01), cost = 2^(2:9)),kernel="sigmoid")
print(svm_tune_sigmoid )
best_mod_sigmoid  <- svm_tune_sigmoid $best.model
best_mod_pred_sigmoid  <- predict(best_mod_sigmoid , test) 
error_best_mod_sigmoid  <- mean(best_mod_pred_sigmoid ==test$HL_mpg)

print(error_best_mod_sigmoid )
print(error_best_mod_radial)
print(error_best_mod_poly)
print(error_best_mod)

error_test<-function(ratio,s) {
    set.seed(s)
    auto<-transform_auto_ds()
  
    smp_size <- floor(ratio * nrow(auto))
  
    
    train_ind <- sample(seq_len(nrow(auto)), size = smp_size)
  
    train <-auto[train_ind, ]
    test <- auto[-train_ind, ]
  
    model_svm = svm(factor(HL_mpg) ~ ., data = train, kernel = "linear", cost = 4,epsilon=0,scale=FALSE)
  
    predictions_train <- predict(model_svm, newdata=test);
    sucess_rate_train = mean(predictions_train==test$HL_mpg)   
  
  
    return (sucess_rate_train)
}

error_train<-function(ratio,s) {
  set.seed(s)
  auto<-transform_auto_ds()
  
  smp_size <- floor(ratio * nrow(auto))
  
  
  train_ind <- sample(seq_len(nrow(auto)), size = smp_size)
  
  train <-auto[train_ind, ]
  test <- auto[-train_ind, ]
  
  model_svm = svm(factor(HL_mpg) ~ ., data = train, kernel = "linear", cost = 4,epsilon=0,scale=FALSE)
  
  predictions_train <- predict(model_svm, newdata=train);
  sucess_rate_train = mean(predictions_train==train$HL_mpg)   
  
  
  return (sucess_rate_train)
}
ks=c()
er_tr=c()
er_te=c()
for (k in seq(0.05,0.95,0.01)){
    errors_te=c()
    errors_tr=c()
    for (s in seq(1,10,1)){
        errors_te=c(errors_te,error_test(k,s))
    }
    final_error_te=1-mean(errors_te) 
    
    errors_tr=c()
    for (s in seq(1,10,1)){
      errors_tr=c(errors_tr,error_train(k,s))
    }
    final_error_tr=1-mean(errors_tr)  
    
    ks=c(ks,k)
    er_te=c(er_te,final_error_te)
    er_tr=c(er_tr,final_error_tr)
}

data=data.frame(ks)
data$er_te=er_te
data$er_tr=er_tr
min(error_te)
ggplot(data, aes(ks)) + 
  geom_line(aes(y = er_te, colour = "test error")) + 
  geom_line(aes(y = er_tr, colour = "train error"))+xlab("train/test ratio")+ylab("error")+ggtitle("Training/test errors for different ratio splits")
