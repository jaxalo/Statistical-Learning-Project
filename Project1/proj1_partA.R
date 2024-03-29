#install.packages('ISLR')
install.packages("caret")
install.packages("gridExtra")
rm(list = ls())
library(dplyr)
library(caret)
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

accuracy <- function(x)
{
  return (sum(diag(x)/(sum(rowSums(x))))*100);
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
  #--
  
  auto <- scale_only_numeric(auto);
  return (auto);
}

auto<-transform_auto_ds()

data=data.frame(Q=seq(N), Freq=runif(N,0,1), Success=sample(seq(0,1), 
                                                            size=N, replace=TRUE))
ggplot(data, aes(x=Freq, y=Success))+geom_point(size=2, alpha=0.4)+
  stat_smooth(method="loess", colour="blue", size=1.5)+
  xlab("Frequency")+
  ylab("Probability of Detection")+
  theme_bw()

par(mfrow=c(2,2))
cdplot(as.factor(HL_mpg)~(acceleration),data=auto,xlab="acceleration",ylab="HL_mpg")
cdplot(as.factor(HL_mpg)~weight,data=auto,xlab="weight",ylab="HL_mpg")
cdplot(as.factor(HL_mpg)~displacement,data=auto,xlab="displacement",ylab="HL_mpg")
cdplot(as.factor(HL_mpg)~horsepower,data=auto,xlab="horsepower",ylab="HL_mpg")





grid.arrange(x,y,ncol=2) 


study_new_auto <- function()
{
  new_auto <- transform_auto_ds();
  attributes_auto <- colnames(new_auto);
  #Remove obvious unecessary attributes
  #Remove objective attributes
  remove <- c('name','HL_mpg','displacement','accelaration','year','origin');
  attributes_auto <- attributes_auto [!attributes_auto %in% remove];
  
  #test all attributes and visualize the plots
  attach(mtcars)
  par(mfrow=c(4,3))
  y_axis = as.vector(new_auto['HL_mpg']);
  
  for(i in 1:7)
  {
    x_axis = as.vector(new_auto[attributes_auto[i]]);
    data_frame_disp = data.frame(x_axis,y_axis);
    plot(data_frame_disp, main = paste("Scatterplot of",attributes_auto[i],"vs HL_mpg"));
  }
}

study_new_auto()

partition_ds <- function(data_s, fraction = 0.6)
{
  set.seed(123)
  #shuffle the data
  ind <- sample(2, nrow(data_s),replace=TRUE, prob = c(fraction,1.0 - fraction));
  training <- data_s[ind==1,];
  testing <- data_s[ind==2,];
  learning_set <- list("train" = training, "test" = testing);
  return (learning_set);
}

compute_LDA <- function(data_s)
{
  data_s = transform_auto_ds();
  #--- partition
  set.seed(123)
  #shuffle the data
  fraction <- 0.8;
  ind <- sample(2, nrow(data_s),replace=TRUE, prob = c(fraction,1.0 - fraction));
  training <- data_s[ind==1,];
  testing <- data_s[ind==2,];
  #--- end partition
  
  model.lda <- lda(formula = HL_mpg ~ .,  data = training);
  predictions_test <- predict(model.lda, newdata=testing);
  sucess_rate_test = mean(predictions_test$class==testing$HL_mpg) * 100;
  print(sucess_rate_test);
  
  predictions_train <- predict(model.lda, newdata=training);
  sucess_rate_train = mean(predictions_train$class==training$HL_mpg) * 100;
  print(sucess_rate_train);
  
  
}

compute_QDA <- function(data_s)
{
  data_s = transform_auto_ds();
  #--- partition
  set.seed(123)
  #shuffle the data
  fraction <- 0.8;
  ind <- sample(2, nrow(data_s),replace=TRUE, prob = c(fraction,1.0 - fraction));
  training <- data_s[ind==1,];
  testing <- data_s[ind==2,];
  #--- end partition
  
  model.qda <- qda(formula = HL_mpg ~ .,  data = training);
  predictions <- predict(model.qda, newdata=testing);
  sucess_rate = mean(predictions$class==testing$HL_mpg) * 100;
  print(sucess_rate);
  plot(predictions$posterior[,2], predictions$class, col=testing$HL_mpg+10)
}

compute_logisitic_regression <- function()
{
  data_s = transform_auto_ds();
  #--- partition
  set.seed(123)
  #shuffle the data
  fraction <- 0.8;
  ind <- sample(2, nrow(data_s),replace=TRUE, prob = c(fraction,1.0 - fraction));
  training <- data_s[ind==1,];
  testing <- data_s[ind==2,];
  #--- end partition
  
  model <- glm(HL_mpg ~.,family=binomial(link='logit'),data=training);
  
  
  fitted.results <- predict(model,newdata=testing,type='response')
  fitted.results <- ifelse(fitted.results > 0.5,1,0)
  
  misClasificError <- mean(fitted.results != testing$HL_mpg)
  print(paste('Accuracy',1-misClasificError))
}


compute_KNN2 <- function(K)
{
  library(class)
  
  data_s = transform_auto_ds();
  fraction = 0.8;
  set.seed(3033);
  intrain <- sample(2, nrow(data_s),replace=TRUE, prob = c(fraction,1.0 - fraction));
  training <- data_s[intrain==1,];
  testing <- data_s[intrain==2,];
  
  # get the range of x1 and x2
  rx1 <- range(training[1])
  rx2 <- range(training[2])
  # get lattice points in predictor space
  px1 <- seq(from = rx1[1], to = rx1[2], by = 0.1 )
  px2 <- seq(from = rx2[1], to = rx2[2], by = 0.1 )
  xnew <- expand.grid(x1 = px1, x2 = px2)
  
  # get the contour map
  knn15 <- knn(train = training[,2:3], test = xnew, cl = training[,1], k = K, prob = TRUE)
  prob <- attr(knn15, "prob")
  prob <- ifelse(knn15=="1", prob, 1-prob)
  prob15 <- matrix(prob, nrow = length(px1), ncol = length(px2))
  
  # Figure 2.2
  par(mar = rep(2,4))
  contour(px1, px2, prob15, levels=0.5, labels="", xlab="", ylab="", main=
            paste(K," -nearest neighbour"), axes=FALSE)
  points(training[,2:3], col=ifelse(training[,1]==1, "coral", "cornflowerblue"))
  points(xnew, pch=".", cex=1.2, col=ifelse(prob15>0.5, "coral", "cornflowerblue"))
  box()
}

select <- dplyr::select 

compute_all_functions<-function(ratio)
{
  data_s = transform_auto_ds();
  #--- partition
  set.seed(123)
  #shuffle the data
  fraction <- ratio;
  ind <- sample(2, nrow(data_s),replace=TRUE, prob = c(fraction,1.0 - fraction));
  training <- data_s[ind==1,];
  testing <- data_s[ind==2,]
  
  model.lda <- lda(formula = HL_mpg ~ .,  data = training);
  model.qda <- qda(formula = HL_mpg ~ .,  data = training);
  model.lr <- glm(HL_mpg ~.,family=binomial(link='logit'),data=training);
  
  predictions_test <- predict(model.lda, newdata=testing);
  lda_error_tst = 1-mean(predictions_test$class==testing$HL_mpg);
  predictions_train <- predict(model.lda, newdata=training);
  lda_error_tr = 1-mean(predictions_train$class==training$HL_mpg);
  
  predictions_test <- predict(model.qda, newdata=testing);
  qda_error_tst = 1-mean(predictions_test$class==testing$HL_mpg);
  predictions_train <- predict(model.qda, newdata=training);
  qda_error_tr = 1-mean(predictions_train$class==training$HL_mpg);
  
  fitted.results <- predict(model.lr,newdata=testing,type='response');
  fitted.results <- ifelse(fitted.results > 0.5,1,0);
  lr_error_tst <- mean(fitted.results != testing$HL_mpg);
  
  fitted.results <- predict(model.lr,newdata=training,type='response');
  fitted.results <- ifelse(fitted.results > 0.5,1,0);
  lr_error_tr <- mean(fitted.results != training$HL_mpg);
  
  training <- data_s[ind==1,-which(names(data_s)=="HL_mpg")];
  testing <- data_s[ind==2,-which(names(data_s)=="HL_mpg")]
  training_category <- data_s[ind==1,"HL_mpg"]
  testing_category<-data_s[ind==2,"HL_mpg"]
  
  model.knn_test <- knn(train = training, test = testing, cl = training_category, k = 8)
  model.knn_train <- knn(train = training, test = training, cl = training_category, k = 8)
  
  tab<-table(model.knn_test,testing_category)
  knn_error_tst=1-(accuracy(tab)/100)
  tab<-table(model.knn_train,training_category)
  knn_error_tr=1-(accuracy(tab)/100)
  
  return(c(lda_error_tr,lda_error_tst,qda_error_tr,qda_error_tst,lr_error_tr,lr_error_tst,knn_error_tr,knn_error_tst))
}

compute_error_diff_ratios<-function()
{
  k_values <- c(seq(from=0.1, to=0.9, by=0.05))
  num_k <- length(k_values)
  error_df <- tibble(k=rep(0, num_k),
                     lda_error_tr=rep(0, num_k),
                     lda_error_tst=rep(0, num_k),
                     qda_error_tr=rep(0, num_k),
                     qda_error_tst=rep(0, num_k),
                     lr_error_tr=rep(0, num_k),
                     lr_error_tst=rep(0, num_k),
                     knn_error_tr=rep(0, num_k),
                     knn_error_tst=rep(0, num_k))
  
  for(i in 1:num_k){
    
    # fix k for this loop iteration
    k <- k_values[i]
    
    # get_knn_error_rates() is from the knn_functions.R script
    # it computes the train/test errors for knn
    errors<-compute_all_functions(k)
    
    # store values in the data frame
    error_df[i, 'k'] <- k
    error_df[i, 'lda_error_tr'] <- errors[1]
    error_df[i, 'lda_error_tst'] <- errors[2]
    error_df[i, 'qda_error_tr'] <- errors[3]
    error_df[i, 'qda_error_tst'] <- errors[4]
    error_df[i, 'lr_error_tr'] <- errors[5]
    error_df[i, 'lr_error_tst'] <- errors[6]
    error_df[i, 'knn_error_tr'] <- errors[7]
    error_df[i, 'knn_error_tst'] <- errors[8]
  }
  error_df %>% 
    gather(key='type', value='error', lda_error_tst,qda_error_tst,lr_error_tst,knn_error_tst) %>% 
    ggplot() +
    geom_point(aes(x=k, y=error, color=type, shape=type)) +
    geom_line(aes(x=k, y=error, color=type, linetype=type))
}
compute_error_diff_ratios()


compute_knn<-function(K)
{
  data_s = transform_auto_ds();
  #--- partition
  set.seed(123)
  #shuffle the data
  fraction <- 0.66;
  ind <- sample(2, nrow(data_s),replace=TRUE, prob = c(fraction,1.0 - fraction));
  training <- data_s[ind==1,-which(names(data_s)=="HL_mpg")];
  testing <- data_s[ind==2,-which(names(data_s)=="HL_mpg")]
  training_category <- data_s[ind==1,"HL_mpg"]
  testing_category<-data_s[ind==2,"HL_mpg"]
  
  
  model.knn <- knn(train = training, test = testing, cl = training_category, k = K)
  tab_test<-table(model.knn,testing_category)
  test_error=1-(accuracy(tab_test)/100)
  
  model.knn <- knn(train = training, test = training, cl = training_category, k = K)
  tab_training<-table(model.knn,training_category)
  training_error=1-(accuracy(tab_training)/100)
  return(c(training_error,test_error))
}

compute_knn_multiple_k<-function()
{
  
  
  k_values <- c(seq(from=1, to=200, by=1))
  num_k <- length(k_values)
  error_df <- tibble(k=rep(0, num_k),
                     tr=rep(0, num_k),
                     tst=rep(0, num_k))
  
  for(i in 1:num_k){
    
    # fix k for this loop iteration
    k <- k_values[i]
    
    # get_knn_error_rates() is from the knn_functions.R script
    # it computes the train/test errors for knn
    errors<-compute_knn(k)
    
    # store values in the data frame
    error_df[i, 'k'] <- k
    error_df[i, 'training_error'] <- errors[1]
    error_df[i, 'test_error'] <- errors[2]
  }
  error_df %>% 
    gather(key='type', value='error', training_error, test_error) %>% 
    ggplot() +
    geom_point(aes(x=k, y=error, color=type, shape=type)) +
    geom_line(aes(x=k, y=error, color=type, linetype=type))
}
compute_knn_multiple_k()


