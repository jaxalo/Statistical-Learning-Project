
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

N=1000

rho <- 0
mu1 <- 0; 
mu2 <- 0; 
s1<-1;
s2<-1;
r<-1.5
# creating data frame
mu <- c(mu1,mu2) 
sigma <- matrix(c(s1^2, s1*s2*rho, s1*s2*rho, s2^2),2) 
set.seed(123)
data <-as.data.frame(mvrnorm(N, mu = mu, Sigma = sigma ))
colnames(data) <- c("x1","x2")
data$y=c(replicate(500,1),replicate(500,-1))
data$y=as.factor(data$y)
test<-svm(y ~ .,data,kernel="radial",cost=10000)
plot(test,data)
# shifting points

data[seq(1,500,1),1]=data[seq(1,500,1),1]+r
qplot(x1,x2,data=data,color=y)

train=rbind(data[seq(1,400,1),],data[seq(501,900,1),])
test=rbind(data[seq(401,500,1),],data[seq(901,1000,1),])

svm_tune_poly <- tune(svm, y ~ ., data = train, ranges = list(gamma = seq(0,1,0.1), cost = 2^(2:6)),kernel="polynomial")
print(svm_tune_poly)
best_mod_poly <- svm_tune_poly$best.model
best_mod_pred_poly <- predict(best_mod_poly, test) 
error_best_mod_poly <- mean(best_mod_pred_poly==test$y)
print(error_best_mod_poly)

svm_tune1<- tune(svm, y~ ., data = train,
                 ranges = list(epsilon = seq(0,1,0.05), cost = 2^(2:7)),kernel="linear"
)
print(svm_tune1)

best_mod <- svm_tune1$best.model
best_mod_pred <- predict(best_mod, test) 

error_best_mod <- mean(best_mod_pred==test$y)
print(error_best_mod)





error<-function(kern,r,s1,s2){
  N=1000
  
  rho <- 0
  mu1 <- 0; 
  mu2 <- 0; 
  
  # creating data frame
  mu <- c(mu1,mu2) 
  sigma <- matrix(c(s1^2, s1*s2*rho, s1*s2*rho, s2^2),2) 
  set.seed(123)
  data <-as.data.frame(mvrnorm(N, mu = mu, Sigma = sigma ))
  colnames(data) <- c("x1","x2")
  data$y=c(replicate(500,1),replicate(500,-1))
  data$y=as.factor(data$y)
  
  # shifting points
  
  data[seq(1,500,1),1]=data[seq(1,500,1),1]+r
  qplot(x1,x2,data=data,color=y)

  train=rbind(data[seq(1,400,1),],data[seq(501,900,1),])
  test=rbind(data[seq(401,500,1),],data[seq(901,1000,1),])
  model_svm = svm(y~ ., data = train, kernel = kern)
  predictions_train <- predict(model_svm, test);
  sucess_rate_train = mean(predictions_train==test$y) 
}




ratio_plot<-function(){
  linear_error=c()
  radial_error=c()
  polynomial_error=c()
  sigmoid_error=c()
  rs=c()
  for (r in seq(0,10,0.1)){
    rs=c(rs,r)
    linear_error=c(linear_error,1-error("linear",r,1,1))
    radial_error=c(radial_error,1-error("radial",r,1,1))
    polynomial_error=c(polynomial_error,1-error("polynomial",r,1,1))
    sigmoid_error=c(sigmoid_error,1-error("sigmoid",r,1,1))
  }
  lin_data=data.frame(rs,linear_error)
  lin_data$data="linear"
  colnames(lin_data)=c("r","error","kernel")
  
  rad_data=data.frame(rs,radial_error)
  rad_data$data="radial"
  colnames(rad_data)=c("r","error","kernel")
  
  pol_data=data.frame(rs,polynomial_error)
  pol_data$data="polynomial"
  colnames(pol_data)=c("r","error","kernel")
  
  sig_data=data.frame(rs,sigmoid_error)
  sig_data$data="sigmoid"
  colnames(sig_data)=c("r","error","kernel")
  
  ratio_data=rbind(lin_data,pol_data,sig_data,rad_data)
  
  ggplot(ratio_data, aes(x = r, y = error)) + 
    geom_line(aes(color = kernel, linetype = kernel))
}
ratio_plot()
variance_plot<-function(){
  linear_error=c()
  radial_error=c()
  polynomial_error=c()
  sigmoid_error=c()
  vs=c()
  r=10
  for (v in seq(1,40,0.1)){
    vs=c(vs,v)
    linear_error=c(linear_error,1-error("linear",r,v,v))
    radial_error=c(radial_error,1-error("radial",r,v,v))
    polynomial_error=c(polynomial_error,1-error("polynomial",r,v,v))
    sigmoid_error=c(sigmoid_error,1-error("sigmoid",r,v,v))
  }
  lin_data=data.frame(vs,linear_error)
  lin_data$data="linear"
  colnames(lin_data)=c("v","error","kernel")
  
  rad_data=data.frame(vs,radial_error)
  rad_data$data="radial"
  colnames(rad_data)=c("v","error","kernel")
  
  pol_data=data.frame(vs,polynomial_error)
  pol_data$data="polynomial"
  colnames(pol_data)=c("v","error","kernel")
  
  sig_data=data.frame(vs,sigmoid_error)
  sig_data$data="sigmoid"
  colnames(sig_data)=c("v","error","kernel")
  
  ratio_data=rbind(lin_data,pol_data,sig_data,rad_data)
  
  ggplot(ratio_data, aes(x = v, y = error)) + 
    geom_line(aes(color = kernel, linetype = kernel))
}
variance_plot()

errork<-function(kern,r,s1,s2,k){
  N=1000
  rho <- 0
  mu1 <- 0; 
  mu2 <- 0; 

  # creating data frame
  mu <- c(mu1,mu2) 
  sigma <- matrix(c(s1^2, s1*s2*rho, s1*s2*rho, s2^2),2) 
  set.seed(123)
  data <-as.data.frame(mvrnorm(N, mu = mu, Sigma = sigma ))
  colnames(data) <- c("x1","x2")
  data$y=c(replicate(1000*k,1),replicate(1000-1000*k,-1))
  data$y=as.factor(data$y)
  
  # shifting points
  
  data[seq(1,1000*k,1),1]=data[seq(1,1000*k,1),1]+r
  qplot(x1,x2,data=data,color=y)
  
  train=rbind(data[seq(1,800*k,1),],data[seq(1000*k+1,1000-200+200*k,1),])
  test=rbind(data[seq(800*k+1,1000*k,1),],data[seq(1000-200+200*k+1,1000,1),])
  model_svm = svm(y~ ., data = train, kernel = kern)
  predictions_train <- predict(model_svm, test);
  sucess_rate_train = mean(predictions_train==test$y) 
}

ratio_plot()

errork("linear",1,1,1,as.numeric(0.58))
for (k in seq(0.5,1,0.08)){
  
  errork("linear",1,1,1,as.numeric(k))
  print(k)
}
(errork("linear",1,1,1,0.58))
k=0.5

repeat {
  print(k)
  (errork("linear",1,1,1,k))
  k = k+0.01
  k=signif(k,digits=3)
  if (k>=1){
    break
  }
}

k_plot<-function(){
  linear_error=c()
  radial_error=c()
  polynomial_error=c()
  sigmoid_error=c()
  ks=c()
  r=1.5
  for (k in seq(0.5,0.99,0.08)){
    k=signif(k,digits=3)
    ks=c(ks,k)
    #print(errork("linear",r,1,1,k))
    linear_error=c(linear_error,1-errork("linear",r,1,1,k))
    radial_error=c(radial_error,1-errork("radial",r,1,1,k))
    polynomial_error=c(polynomial_error,1-errork("polynomial",r,1,1,k))
    sigmoid_error=c(sigmoid_error,1-errork("sigmoid",r,1,1,k))
  }
  lin_data=data.frame(ks,linear_error)
  lin_data$data="linear"
  colnames(lin_data)=c("k","error","kernel")
  
  rad_data=data.frame(ks,radial_error)
  rad_data$data="radial"
  colnames(rad_data)=c("k","error","kernel")
  
  pol_data=data.frame(ks,polynomial_error)
  pol_data$data="polynomial"
  colnames(pol_data)=c("k","error","kernel")
  
  sig_data=data.frame(ks,sigmoid_error)
  sig_data$data="sigmoid"
  colnames(sig_data)=c("k","error","kernel")
  
  ratio_data=rbind(lin_data,pol_data,sig_data,rad_data)
  
  ggplot(ratio_data, aes(x = k, y = error)) + 
    geom_line(aes(color = kernel, linetype = kernel))
}
k_plot()

