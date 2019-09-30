
library(MASS)


N <- 300 # Number of random samples
M <- 134
set.seed(123)
# Target parameters for univariate normal distributions
rho <- 0
mu1 <- 1; s1 <- 1
mu2 <- 1; s2 <- 1

# Parameters for bivariate normal distribution
mu <- c(mu1,mu2) # Mean
sigma <- matrix(c(s1^2, s1*s2*rho, s1*s2*rho, s2^2),2) # Covariance matrix

smpl_1 <- mvrnorm(M, mu = mu, Sigma = sigma )

rho <- 0
mu1 <- 2; s1 <- 0.1
mu2 <- 2; s2 <- 0.1

# Parameters for bivariate normal distribution
mu <- c(mu1,mu2) # Mean
sigma <- matrix(c(s1^2, s1*s2*rho, s1*s2*rho, s2^2),2) # Covariance matrix

smpl_2 <- mvrnorm(N-M, mu = mu, Sigma = sigma )

data <- tibble( x1=rep(0,N),
                x2=rep(0,N),
                y=rep(0,N))
                  

data[,c(1,2)]<-rbind(smpl_1,smpl_2)
data[,3]=rbind(matrix(1,M,1),matrix(0,N-M,1))


set.seed(123)
fraction <- 0.8
ind <- sample(2, nrow(data),replace=TRUE, prob = c(fraction,1.0 - fraction));
training <- data[ind==1,];
testing <- data[ind==2,]



model.lda <- lda(formula = y ~ .,  data = training);
model.qda <- qda(formula = y ~ .,  data = training);
model.lr <- glm(y ~.,family=binomial(link='logit'),data=training);

predictions_test <- predict(model.lda, newdata=testing);
lda_error_tst = 1-mean(predictions_test$class==testing$y);
predictions_train <- predict(model.lda, newdata=training);
lda_error_tr = 1-mean(predictions_train$class==training$y);

predictions_test <- predict(model.qda, newdata=testing);
qda_error_tst = 1-mean(predictions_test$class==testing$y);
predictions_train <- predict(model.qda, newdata=training);
qda_error_tr = 1-mean(predictions_train$class==training$y);

fitted.results <- predict(model.lr,newdata=testing,type='response');
fitted.results <- ifelse(fitted.results > 0.5,1,-1);
lr_error_tst <- mean(fitted.results != testing$y);

fitted.results <- predict(model.lr,newdata=training,type='response');
fitted.results <- ifelse(fitted.results > 0.5,1,-1);
lr_error_tr <- mean(fitted.results != training$y);


(c(lda_error_tr,lda_error_tst,qda_error_tr,qda_error_tst,lr_error_tr,lr_error_tst))
