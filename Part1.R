#install.packages('ISLR')
rm(list = ls())
library(dplyr)
library(caret)
library(tidyverse)
library(ggplot2)
library(MASS)
library(FNN)
library(class)
library(DMwR)

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

transform_auto_ds <- function(attribute_name = 'mpg')
{
  auto <- load_auto_ds();
  median_mpg <- compute_median(auto[attribute_name]);
  #add column
  auto <- mutate(auto, HL_mpg = mpg > median_mpg);
  auto['mpg'] <- NULL;
  
  #remove variable for the training
  remove <- c('name','cylinders','displacement','acceleration','year','origin');
  for(e in remove)
  {
    auto[e] <- NULL;
  }
  #--
  
  auto <- scale_only_numeric(auto);
  return (auto);
}

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
  predictions <- predict(model.lda, newdata=testing);
  sucess_rate = mean(predictions$class==testing$HL_mpg) * 100;
  print(sucess_rate);
  plot(predictions$x[,1], predictions$class, col=testing$HL_mpg+10);
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

  
test_diff_methods <- function()
{
  auto <- transform_auto_ds();
  compute_LDA(auto);
}