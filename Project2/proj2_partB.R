rm(list = ls())
library(dplyr)
library(caret)
library(tidyverse)
library(tidyr)
library(ggplot2)
library(MASS)
library(FNN)
library(class)
library(DMwR)
library(party)

#Beautify tree
library(rattle)
library(rpart.plot)
library(RColorBrewer)


# Decision Tree Model
library(rpart)

#Scoring
library(ROCR)



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

accuracy <- function(x)
{
  return (sum(diag(x)/(sum(rowSums(x)))) * 100);
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

##################
##Classification##
##################


transform_auto_ds <- function(attribute_name = 'mpg')
{
  auto <- load_auto_ds();
  median_mpg <- compute_median(auto[attribute_name]);
  #add column
  auto <- mutate(auto, HL_mpg = mpg > median_mpg);
  auto['mpg'] <- NULL;
  
  #remove variable for the training
  #remove <- c('name','cylinders','displacement','acceleration','year','origin');
  #for(e in remove)
  #{
  #  auto[e] <- NULL;
  #}
  #--
  
  auto <- scale_only_numeric(auto);
  return (auto);
}


build_perf_unprunned_tree_class <-function(nbSeed = 1, training, test)
{

  model1 <- rpart(HL_mpg ~., data = training, method = "class");
  
  predicted_test.classes <- model1 %>% predict(test, type = "class");
  
  test_error_rate = (1 - mean(predicted_test.classes == test$HL_mpg)) * 100 ;
  
  predicted_train.classes <- model1 %>% predict(training, type = "class");
  
  training_error_rate =  (1 - mean(predicted_train.classes == test$HL_mpg)) * 100;
  
  return (c(training_error_rate, test_error_rate));
}

build_perf_prunned_tree_class <-function(nbSeed = 1, training, test)
{
  train = caret::train;
  
  model2 <- train(
    HL_mpg ~., data = training, method = "rpart",
    trControl = trainControl("cv", number = 10),
    tuneLength = 30
  );
  
  predicted_test.classes <- model2 %>% predict(test);
  
  test_error_rate = (1 - (mean(predicted_test.classes == test$HL_mpg)))*100;
  
  plot(model2);
  
  predicted_train.classes <- model2 %>% predict(training);
  
  training_error_rate = (1 - (mean(predicted_train.classes == training$HL_mpg)))*100;
  
  return (c(training_error_rate, test_error_rate));
}

compute_all_functions_classification<-function(ratio = 0.6, nbSeed = 122)
{
  #--- partition
  set.seed(nbSeed);
  #shuffle the data
  fraction <- ratio;
  
  set.seed(nbSeed);
  
  mydata = transform_auto_ds();
  
  # Make dependent variable as a factor (categorical)
  mydata$HL_mpg = as.factor(mydata$HL_mpg);
  
  # Split data into training (70%) and validation (30%)
  dt = sort(sample(nrow(mydata), nrow(mydata)*fraction));
  train<-mydata[dt,];
  val<-mydata[-dt,]; # Check number of rows in training data set
  
  unpruned_tree = build_perf_unprunned_tree_class(nbSeed, train, val);
  prunned_tree = build_perf_prunned_tree_class(nbSeed,train,val);
  
  
  return(c(unpruned_tree[1],unpruned_tree[2],prunned_tree[1],prunned_tree[2]));
}

#computes errors for different ratios
compute_error_diff_ratios_classification<-function()
{
  k_values <- c(seq(from=0.1, to=0.9, by=0.05))
  #different rations to try
  num_k <- length(k_values)
  error_df <- tibble(k=rep(0, num_k),
                     unprunned_error_tr=rep(0, num_k),
                     unprunned_error_tst=rep(0, num_k),
                     prunned_error_tr=rep(0, num_k),
                     prunned_error_tst=rep(0, num_k))
  #setup df which we deposit errors in 
  
  #compute errors for each k and put in error_df
  for(i in 1:num_k){
    # fix k for this loop iteration
    k <- k_values[i]
    
    
    # computes the train/test errors for knn
    errors<-compute_all_functions_classification(k)
    
    # store values in the data frame
    error_df[i, 'k'] <- k;
    error_df[i, 'unprunned_error_tr'] <- errors[1];
    error_df[i, 'unprunned_error_tst'] <- errors[2];
    error_df[i, 'prunned_error_tr'] <- errors[3];
    error_df[i, 'prunned_error_tst'] <- errors[4];
    
  }
  
  
  #plot test errors against k for all functions
  error_df %>% 
    gather(key='type', value='error', unprunned_error_tr,unprunned_error_tst,prunned_error_tr,prunned_error_tst) %>% 
    ggplot() +
    geom_point(aes(x=k, y=error, color=type, shape=type)) +
    geom_line(aes(x=k, y=error, color=type, linetype=type));
}

##############
##Regression##
##############

transform_auto_ds_reg <- function()
{
  auto <- load_auto_ds();
  auto <- scale_only_numeric(auto);
  return (auto);
}

build_perf_unprunned_tree_reg <-function(nbSeed = 1, training, test)
{
  train = caret::train;
  
  #tune length is the one controling the cp value if we choose a high tuneLength it will test several cp and choose the optimal one
 
  model <- train( mpg ~., data = training, method = "rpart", trControl = trainControl("cv", number = 10), tuneLength = 0);
  
  predictions_test <- model %>% predict(test);
  
  test_error_rate = RMSE(predictions_test, test$mpg);
  
  predictions_train <- model %>% predict(training);
  
  training_error_rate = RMSE(predictions_train, training$mpg);
  
  return (c(training_error_rate, test_error_rate));
}

build_perf_prunned_tree_reg <-function(nbSeed = 1, training, test)
{
  
  train = caret::train;
  
  #tune length is the one controling the cp value if we choose a high tuneLength it will test several cp and choose the optimal one
  
  model <- train( mpg ~., data = training, method = "rpart", trControl = trainControl("cv", number = 10), tuneLength = 10);
  
  predictions_test <- model %>% predict(test);
  
  test_error_rate = RMSE(predictions_test, test$mpg);
  
  predictions_train <- model %>% predict(training);
  
  training_error_rate = RMSE(predictions_train, training$mpg);
  
  return (c(training_error_rate, test_error_rate));
  
}

compute_all_functions_regression<-function(ratio = 0.6, nbSeed = 123)
{
  data_s = transform_auto_ds_reg();
  #--- partition
  set.seed(nbSeed);
  #shuffle the data
  fraction <- ratio;
  
  set.seed(nbSeed);
  
  mydata = data_s;
  
  
  # Split data into training (70%) and validation (30%)
  dt = sort(sample(nrow(mydata), nrow(mydata)*fraction));
  train<-mydata[dt,];
  val<-mydata[-dt,]; # Check number of rows in training data seta
  
  unpruned_tree = build_perf_unprunned_tree_reg(nbSeed, train, val);
  prunned_tree = build_perf_prunned_tree_reg(nbSeed,train,val);
  
  return(c(unpruned_tree[1],unpruned_tree[2],prunned_tree[1],prunned_tree[2]));
}

#computes errors for different ratios
compute_error_diff_ratios_regression<-function()
{
  k_values <- c(seq(from=0.1, to=0.9, by=0.05))
  #different rations to try
  num_k <- length(k_values)
  error_df <- tibble(k=rep(0, num_k),
                     unprunned_error_tr=rep(0, num_k),
                     unprunned_error_tst=rep(0, num_k),
                     prunned_error_tr=rep(0, num_k),
                     prunned_error_tst=rep(0, num_k))
  #setup df which we deposit errors in 
  
  #compute errors for each k and put in error_df
  for(i in 1:num_k){
    # fix k for this loop iteration
    k <- k_values[i]
    
    
    # computes the train/test errors for knn
    errors<-compute_all_functions_regression(k)
    
    # store values in the data frame
    error_df[i, 'k'] <- k;
    error_df[i, 'unprunned_error_tr'] <- errors[1];
    error_df[i, 'unprunned_error_tst'] <- errors[2];
    error_df[i, 'prunned_error_tr'] <- errors[3];
    error_df[i, 'prunned_error_tst'] <- errors[4];
    
  }
  
  
  #plot test errors against k for all functions
  error_df %>% 
    gather(key='type', value='error', unprunned_error_tr,unprunned_error_tst,prunned_error_tr,prunned_error_tst) %>% 
    ggplot() +
    geom_point(aes(x=k, y=error, color=type, shape=type)) +
    geom_line(aes(x=k, y=error, color=type, linetype=type));
}

#####
#copy
##
rrr <-function()
{
nbSeed = 3; ratio = 0.6;
data_s = transform_auto_ds_reg();
#--- partition
set.seed(nbSeed);
#shuffle the data
fraction <- ratio;

set.seed(nbSeed);

mydata = data_s;


# Split data into training (70%) and validation (30%)
dt = sort(sample(nrow(mydata), nrow(mydata)*fraction));
training<-mydata[dt,];
val<-mydata[-dt,];
}