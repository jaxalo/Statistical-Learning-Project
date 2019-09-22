#install.packages('ISLR')
rm(list = ls())
library(dplyr)
library(caret)
library(tidyverse)

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

transform_auto_ds <- function(attribute_name = 'mpg')
{
  auto <- load_auto_ds();
  median_mpg <- compute_median(auto[attribute_name]);
  #add column
  auto <- mutate(auto, HL_mpg = mpg > median_mpg);
  auto['mpg'] <- NULL
  return (auto);
}

study_new_auto <- function()
{
  new_auto <- transform_auto_ds();
  attributes_auto <- colnames(new_auto);
  #Remove obvious unecessary attributes
  #Remove objective attributes
  remove <- c('name','HL_mpg');
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


