#install.packages('ISLR')
rm(list = ls())
library(dplyr)
library(caret)
library(tidyverse)
library(MASS)


set.seed(123)

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

compute_LDA <- function(train, test)
{
  HL_mpg = train['HL_mpg'];
  model <- lda(formula = HL_mpg ~ .,  data = train, prior = c(1,1)/2, method = "class");
  plot(model);
}

test_diff_methods <- function()
{
  auto <- transform_auto_ds();
  learning_set <- partition_ds(auto);
  compute_LDA(learning_set['train'], learning_set['test']);
}