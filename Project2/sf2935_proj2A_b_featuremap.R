
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



set.seed(123)

kern="linear"
r=0
s1=1
s2=1

N=1000
  
rho <- 0
mu1 <- 0; 
mu2 <- 0; 

# creating data frame
mu <- c(mu1,mu2) 
sigma1 <- matrix(c(s1^2, s1*s2*rho, s1*s2*rho, s2^2),2) 
sigma2 <- matrix(c(0.1*s1^2, s1*s2*rho, s1*s2*rho, 0.1*s2^2),2) 
set.seed(123)
data1 <-as.data.frame(mvrnorm(N/2, mu = mu, Sigma = sigma1))
data2<-as.data.frame(mvrnorm(N/2, mu = mu, Sigma = sigma2 ))
data=rbind(data1,data2)
colnames(data) <- c("x1","x2")
data$y=c(replicate(500,1),replicate(500,-1))
data$y=as.factor(data$y)

# shifting points
data[seq(1,500,1),1]=data[seq(1,500,1),1]+r
qplot(x1,x2,data=data,color=y)

#
newdata=data.frame()
newdata=as.data.frame(data$x1)
newdata$x3=(data$x1^2+data$x2^2)

newdata$y=data$y
colnames(newdata) <- c("x1","x2","y")
qplot(x1,x2,data=newdata,color=y)


train=rbind(data[seq(1,400,1),],data[seq(501,900,1),])
test=rbind(data[seq(401,500,1),],data[seq(901,1000,1),])
model_svm = svm(y~ ., data = data, kernel = "radial",cost=0.0000001)
predictions_test <- predict(model_svm,test);
sucess_rate_train = mean(predictions_test==test$y) 
print(sucess_rate_train)
plot(model_svm,data)

train=rbind(newdata[seq(1,400,1),],newdata[seq(501,900,1),])
test=rbind(newdata[seq(401,500,1),],newdata[seq(901,1000,1),])
svm_map=svm(y~ ., data = newdata, kernel = "polynomial",cost=1)
pred_map<-predict(svm_map,test)
error_map<-mean(pred_map==test$y)
plot(svm_map,train)




