##############################################################################################################################################################################################
## Ch. 9 Lab

library(ISLR)
library(e1071)
library(ROCR)
library(glmnet)
library(ggplot2)
## 9.6.1 Support Vector Classifier
set.seed(1)
x <- matrix(rnorm(20*2), ncol=2)
y <- c(rep(-1, 10), rep(1, 10))
x[y==1,] <- x[y==1,] + 1

plot(x, col=(3-y))
dat <- data.frame(x=x, y=as.factor(y))
svm.fit <- svm(y~., data=dat, kernel="linear", cost=10, scale=FALSE)
plot(svm.fit, dat)
# Only one observation is misclassified, the "x" near the bottom of the plot.
str(svm.fit)
svm.fit$index
summary(svm.fit)
svm.fit <- svm(y~., data=dat, kernel="linear", cost=0.1, scale=FALSE)
plot(svm.fit, dat)
svm.fit$index

# To perform cross-validation use the tune() function
set.seed(1)
tune.out <- tune(svm, y~., data=dat, kernel="linear", range=list(cost=c(0.001, 0.01, 0.1, 1, 5, 10, 100)))
summary(tune.out)
#The lowest error occurs when the cost is 0.1.
bestmod <- tune.out$best.model
summary(bestmod)
plot(bestmod, dat)

xtest <- matrix(rnorm(20*2), ncol=2)
ytest <- sample(c(-1, 1), 20, rep=TRUE)
xtest[ytest==1,] <- xtest[ytest==1,] + 1
testdat <- data.frame(x=xtest, y=as.factor(ytest))

ypred <- predict(bestmod, testdat)
table(predict=ypred, truth=testdat$y)

svm.fit <- svm(y~., data=dat, kernel="linear", cost=0.01, scale=FALSE)
ypred <- predict(svm.fit, testdat)
table(predict=ypred, truth=testdat$y)

## If linearly separable:
x[y==1,] <- x[y==1,] + 0.5
plot(x, col=(y+5)/2, pch=19)
dat=data.frame(x, y=as.factor(y))
svm.fit <- svm(y~., data=dat, kernel="linear", cost=1e5, scale=FALSE)
summary(svm.fit)
plot(svm.fit, dat)

svm.fit <- svm(y~., data=dat, kernel="linear", cost=1)
summary(svm.fit)
plot(svm.fit, dat)

## 9.6.2 Support Vector Machines
set.seed(1)
x <- matrix(rnorm(200*2), ncol=2)
x[1:100,] <- x[1:100,] + 2
x[101:150,] <- x[101:150,] - 2
y <- c(rep(1, 150), rep(2, 50))
dat <- data.frame(x=x, y=as.factor(y))
plot(x, col=y)

# Use a radial kernel to differentiate outcomes
train <- sample(200, 100)
svm.fit <- svm(y~., data=dat[train,], kernel="radial", gamma=1, cost=1)
plot(svm.fit, dat[train,])
summary(svm.fit)

svm.fit <- svm(y~., data=dat[train,], kernel="radial", gamma=1, cost=1e5)
plot(svm.fit, dat[train,])
summary(svm.fit)

set.seed(1)
tune.out <- tune(svm, y~., data=dat[train,], kernal="radial", ranges=list(cost=c(0.1, 1, 10, 100, 1000), gamma=c(0.5, 1, 2, 3, 4)))
summary(tune.out)
min(summary(tune.out)$performances[3])

## 9.6.3 ROC Curves
library(ROCR)

#Creates a prediction object and measures the performance using the true positive rate and false positive rate, then plots the performance.
rocplot <- function(pred, truth,...){
  predob <- prediction(pred, truth)
  perf <- performance(predob, "tpr", "fpr")
  plot(perf,...)
}

svm.fit.opt <- svm(y~., data=dat[train,], kernel="radial", gamma=2, cost=1, decision.values=TRUE)
fitted <- attributes(predict(svm.fit.opt, dat[train,], decision.values=TRUE))$decision.values

par(mfrow=c(1, 2))
rocplot(fitted, dat[train, "y"], main="Training Data")

svm.fit.flex <- svm(y~., data=dat[train,], kernel="radial", gamma=50, cost=1, decision.values=TRUE)
fitted <- attributes(predict(svm.fit.flex, dat[train,], decision.values=TRUE))$decision.values
rocplot(fitted, dat[train, "y"], add=TRUE, col="red")

fitted <- attributes(predict(svm.fit.opt, dat[-train,], decision.values=TRUE))$decision.values
rocplot(fitted, dat[-train, "y"], main="Test Data")
fitted <- attributes(predict(svm.fit.flex, dat[-train,], decision.values=TRUE))$decision.values
rocplot(fitted, dat[-train, "y"], add=TRUE, col="red")
# The higher gamma value may have worked better on the training data, but this appears to most likely be due to overfitting.  The fit to the testing data shows that a gamma=2 performs much more accurately, hugging the top left corner.

## 9.6.4 SVM with multiple classes
set.seed(1)
x <- rbind(x, matrix(rnorm(50*2), ncol=2))
y <- c(y, rep(0, 50))
x[y==0, 2] <- x[y==0, 2] + 2
dat <- data.frame(x=x, y=as.factor(y))
par(mfrow=c(1,1))
plot(x, col=(y+1))

svm.fit <- svm(y~., data=dat, kernel="radial", cost=10, gamma=1)
plot(svm.fit, dat)

## 9.6.5 Application to Gene Expression Data
library(ISLR)
names(Khan)
dim(Khan$xtrain)
dim(Khan$xtest)
length(Khan$ytrain)
length(Khan$ytest)
table(Khan$ytrain)
table(Khan$ytest)

#Use a linear kernel because p >> n
dat <- data.frame(x=Khan$xtrain, y=as.factor(Khan$ytrain))
out <- svm(y~., data=dat, kernel="linear", cost=10)
summary(out)
table(out$fitted, dat$y)
# No training errors?!  When p >> n, there are so many variables that it is easy to construct a hyperplane to completely separate the observations into their classes.  This is training data, however.
dat.te <- data.frame(x=Khan$xtest, y=as.factor(Khan$ytest))
pred.te <- predict(out, newdata=dat.te)
tbl <- table(pred.te, dat.te$y)
tbl
# The test data is very nearly perfectly predicted. 90% true positive rate.
sum(diag(tbl)) / sum(tbl)


######################################################################################################################################################################################
## Ch. 9 Exercises

## Q1
# (A) 
x <- seq(-10, 10, length=30)
y <- seq(-10, 10, length=30)
f <- function(x, y){
  z <- 1 + 3*x - y
}
z <- outer(x, y, f)
persp(x, y, z, theta=0, phi=30, expand=0.5, col="gray")
# Above this plane lie points which satisfy the equation >0 and below this plane satisfy points for which the equation <0.

# (B)
g <- function(x, y, z){
  z <- -2 + x + 2*y
}
w <- outer(x, y, f)
persp(x, y, w, theta=0, phi=30, expand=0.5, col="blue")

## Q2
# (A)
library(plotrix)

xcent <- -1
ycent <- 2
rad <- 2
x <- seq(xcent - rad, xcent+rad, length=100)
y <- -sqrt(4-(1+x)^2)+2
y2 <- sqrt(4-(1+x)^2)+2
par(mfrow=c(1, 2), mar=c(4, 4, 4, 4))
plot(x, y, xlim=c(-3, 1), ylim=c(0, 4), type="l", asp=1)
lines(x, y2)
draw.circle(-1, 2, 10, col="blue")
draw.circle(-1, 2, 2, col="red")

# (C)
#The point (0,0) is classified as a blue point because the function evaluates to 5 > 4.
# (-1, 1) is a red point.  (2, 2) is a blue point.  (3, 8) is a blue point.  

# (D)
# Since x1 and x2 are not linear in this equation, the decision boundary cannot be linear in x1 and x2 (squared).  However, the coefficients of x1 and x2 are linear because there are no functions applied to the coefficients once the formula is expanded.  

## Q3
# (A)
x1 <- c(3, 2, 4, 1, 2, 4, 4)
x2 <- c(4, 2, 4, 4, 1, 3, 1)
y <- c("Red", "Red", "Red", "Red", "Blue", "Blue", "Blue")
par(mfrow=c(1, 1))
plot(x1, x2, col=y, asp=1, pch=19)
# (B)    
abline(0, (3.5 - 1) / (4 - 1))
# x2 = 0 + 2.5/3 x1     ------>       0 + 2.5*x1 + 3*x2
# (C) Classify Red if 0 + 2.5*x1 + 3*x2 > 0
#     Classify Blue is 0 + 2.5*x1 + 3*x2 < 0
# (D)
abline(0.25, (3.5 - 1) / (4 - 1), lty="dashed")
abline(-0.25, (3.5 - 1) / (4 - 1), lty="dashed")
# (E)
sv <- data.frame(x1=x1[c(2, 3, 6)], x2=x2[c(2, 3, 6)])
points(sv, pch=5)
# (F)
# Moving the point furthest from the margin (and thus the maximal margin hyperplane) does not affect the margin whatsoever.  The margin is bounded (determined) by the three points closest to it, so unless the furthest point were to be moved into the margin, the margin is unaffected.
# (G)
abline(3, .5, lty="dotted")
# (H)
points(2, 3, pch=11, col="blue")

## Q4
set.seed(1)
x <- matrix(rnorm(100*2), ncol=2)
x[1:25,] <- x[1:25,] - 2
x[26:50,] <- x[26:50,] + 2
y <- c(rep(1, 50), rep(2, 50))
dat <- data.frame(x=x, y=as.factor(y))
plot(x, col=y+1)

train <- sample(100, 50)
svm.rad <- svm(y~., data=dat[train,], kernel="radial", gamma=1, cost=1, scale=FALSE, decision.values=TRUE)
svm.lin <- svm(y~., data=dat[train,], kernel="linear", cost=1, scale=FALSE)
plot(svm.rad, dat)
plot(svm.lin, dat)

pred <- predict(svm.rad, dat[train,], decision.values=TRUE)
fitted <- attributes(pred)$decision.values
par(mfrow=c(1, 2))
rocplot(fitted, dat[train, "y"], main="Training Data")
tbl <- table(pred, y[train])
err.rate <- 1-sum(diag(tbl))/sum(tbl)
err.rate

pred <- predict(svm.lin, dat[train,], decision.values=TRUE)
fitted <- attributes(pred)$decision.values
rocplot(fitted, dat[train,"y"], add=TRUE, col="red")
tbl <- table(pred, y[train])
err.rate <- 1-sum(diag(tbl))/sum(tbl)
err.rate

pred <- predict(svm.rad, dat[-train,], decision.values=TRUE)
fitted <- attributes(pred)$decision.values
rocplot(fitted, dat[-train, "y"], main="Test Data")
tbl <- table(pred, y[-train])
err.rate <- 1-sum(diag(tbl))/sum(tbl)
err.rate

pred <- predict(svm.lin, dat[-train,], decision.values=TRUE)
fitted <- attributes(pred)$decision.values
rocplot(fitted, dat[-train, "y"], add=TRUE, col="red")
tbl <- table(pred, y[-train])
err.rate <- 1-sum(diag(tbl))/sum(tbl)
err.rate
# In both cases the nonlinear boundary performed better than the linear boundary by a significant amount. The error rates for the training data were 4% and 46% for radial and linear, respectively.  The error rates for test data were 12% and 54% respectively.  

# Let's try this for a polynomial case *NOTE - (THE MAX NUMBER OF ITERATIONS FOR SVM WAS REACHED AND DID NOT CONVERGE TO AN ANSWER)
set.seed(1)
x <- matrix(seq(0, 199) / 5 - 10, ncol=2)
x[seq(1, 100, 2),2] <- x[seq(1, 100, 2),1]^3 - 200
x[seq(2, 100, 2),2] <- x[seq(2, 100, 2),1]^3 + 200
y <- rep(1:2, 50)
dat <- data.frame(x=x, y=as.factor(y))
plot(x, col=y+1)

train <- sample(100, 50)
svm.poly <- tune(svm, y~., data=dat[train,], kernel="polynomial", range=list(cost=c(0.001, 0.01, 0.1, 1, 5, 10, 100)), scale=FALSE, decision.values=TRUE)
svm.lin <- svm(y~., data=dat[train,], kernel="linear", cost=1, scale=FALSE)
plot(svm.poly, dat)
plot(svm.lin, dat)

pred <- predict(svm.poly, dat[train,], decision.values=TRUE)
fitted <- attributes(pred)$decision.values
par(mfrow=c(1, 2))
rocplot(fitted, dat[train, "y"], main="Training Data")
tbl <- table(pred, y[train])
err.rate <- 1-sum(diag(tbl))/sum(tbl)
err.rate

pred <- predict(svm.lin, dat[train,], decision.values=TRUE)
fitted <- attributes(pred)$decision.values
rocplot(fitted, dat[train,"y"], add=TRUE, col="red")
tbl <- table(pred, y[train])
err.rate <- 1-sum(diag(tbl))/sum(tbl)
err.rate

pred <- predict(svm.poly, dat[-train,], decision.values=TRUE)
fitted <- attributes(pred)$decision.values
rocplot(fitted, dat[-train, "y"], main="Test Data")
tbl <- table(pred, y[-train])
err.rate <- 1-sum(diag(tbl))/sum(tbl)
err.rate

pred <- predict(svm.lin, dat[-train,], decision.values=TRUE)
fitted <- attributes(pred)$decision.values
rocplot(fitted, dat[-train, "y"], add=TRUE, col="red")
tbl <- table(pred, y[-train])
err.rate <- 1-sum(diag(tbl))/sum(tbl)
err.rate

## Q5
# (A)
library(glmnet)
x1 <- runif(500) - 0.5
x2 <- runif(500) - 0.5
y <- 1*(x1^2 - x2^2 > 0)
dat <- data.frame(x1=x1, x2=x2, y=as.factor(y))
# (B)
plot(x1, x2, col=y+2, main="Actual Data")
# (C)
log.fit <- glm(y~., data=dat, family=binomial)
summary(log.fit)
# (D)
log.probs <- predict(log.fit, dat, type="response")
log.preds <- rep(NA, length(log.probs))
log.preds  <- ifelse(log.probs > 0.5, 2, 1)
plot(x1, x2, col=log.preds + 2, main="Prediction")
# (E)
log.fit <- glm(y~. + I(x1^2) + I(x2^2), data=dat, family=binomial)
summary(log.fit)
# (F)
log.probs <- predict(log.fit, dat, type="response")
log.preds <- ifelse(log.probs > 0.5, 1, 0)
plot(x1, x2, col=log.preds + 2, main="Nonlinear Model Training")
# (G)
svm.fit <- svm(y~., data=dat, scale=FALSE, kernel="linear", cost=10)
plot(svm.fit, dat)
summary(svm.fit)

pred <- predict(svm.fit, dat, decision.values=TRUE)
fitted <- attributes(pred)$decision.values
par(mfrow=c(1, 2))
rocplot(fitted, dat$y, main="Training Data")
tbl <- table(pred, y)
tbl
plot(x1, x2, col=pred, main="Support Vector Classifier")

# (H)
svm.fit <- svm(y~., data=dat, scale=FALSE, kernel="polynomial", gamma=1, cost=100)
plot(svm.fit, dat)
summary(svm.fit)

pred <- predict(svm.fit, dat, decision.values=TRUE)
fitted <- attributes(pred)$decision.values
par(mfrow=c(1, 2))
rocplot(fitted, dat$y, main="Training Data")
tbl <- table(pred, y)
tbl
plot(x1, x2, col=pred, main="Support Vector Machine")

#Now try radial
par(mfrow=c(2, 5))
costs <- 10^seq(-6, 4)
gamma <- 10^seq(-6, 4)
for(j in 1:10){
  for(i in 1:10){
    svm.fit <- svm(y~., data=dat, scale=FALSE, kernel="radial", gamma=gamma[j], cost=costs[i])
    pred <- predict(svm.fit, dat, decision.values=TRUE)
    fitted <- attributes(pred)$decision.values
    plot(x1, x2, col=pred, main="Support Vector Machine")
  }
}

# For various values of gamma and cost the support vector machine using a radial kernel properly predicts class labels for msot of the observations, as can be found from the visuals.  The support vector classifier creates a linear boundary which performs quite poorly, but a nonlinear boundary performs well, like logistic regression with higher order terms.  

## Q6
# (A)
library(ggplot2)
par(mfrow=c(1,1))
set.seed(1)
x1 <- runif(200)
x2 <- runif(200)
y <- 1*(x1-x2 > 0)
x1[y==1] <- x1[y==1] + 0.005
dat <- data.frame(x1=x1, x2=x2, y=as.factor(y))
plot(x1, x2, col=y+2, main="Actual Data")

svm.tune <- tune(svm, y~., data=dat, kernel="linear", ranges=list(cost=c(0.01, 0.1, 1, 10, 100, 1000)))
summary(svm.tune)

costs <- summary(svm.tune)$performances[1]
gammas <- summary(svm.tune)$performances[2]
errors <- summary(svm.tune)$performances[3]
cv <- data.frame(cost=costs, gamma=gammas, error=errors)
plt <- ggplot() 
plt + geom_point(data=cv, aes(log(cost), gamma, size=error))
low.err <- min(summary(svm.tune)$performances[3])
summary(svm.tune)$performances[3] == low.err

# How many training errors are misclassified for each value of cost?
err <- rep(NA, dim(costs)[1])
for(i in 1:dim(costs)[1]){
  svm.fit <- svm(y~., data=dat, kernel="linear", cost=costs[i,1])
  svm.pred <- predict(svm.fit, dat, decision.values=TRUE)
  tbl <- table(svm.pred, y)  
  err[i] <- 1-sum(diag(tbl))/sum(tbl)
}
err
misclass <- err * length(x1)
misclass
# The error rates on the full training data set follow the general trend set forth by the cross-validation, increasing and decreasing at similar values of the Cost.  Higher cost tends to result in lower error rates on the training data.

# (C)
set.seed(15)
x1p <- runif(200)
x2p <- runif(200)
test <- 1*(x1p-x2p > 0)
test.dat <- data.frame(x1p=x1p, x2p=x2p, test=as.factor(test))
plot(x1p, x2p, col=test+2)

err.test <- rep(NA, dim(costs)[1])
for(i in 1:dim(costs)[1]){
  svm.fit <- svm(y~., data=dat, kernel="linear", cost=costs[i,1])
  svm.pred <- predict(svm.fit, data=test.dat, decision.values=TRUE)
  tbl <- table(svm.pred, test)  
  err.test[i] <- 1-sum(diag(tbl))/sum(tbl)
}
err
err.test
misclass.test <- err.test * length(x1p)
misclass
misclass.test
plot(svm.fit, test.dat)
# The error rates on the test data are much higher than on the training data.  In fact, they are of approximately equal error rates, misclassifying nearly half of the results.  

#(D) Though the error rates are very high for the test data, the number of misclassifications is lower for the lower costs compared to higher costs.  

## Q7
names(Auto)
attach(Auto)
# (A)
mpg.bin1 <- ifelse(mpg > median(mpg), 1, 0)
Auto$mpg.bin <- as.factor(mpg.bin1)
# (B)
set.seed(20)
svm.tune <- tune(svm, mpg.bin ~ ., data=Auto, kernel="linear", ranges=list(cost=c(0.01, 0.1, 1, 5, 10, 100)))
summary(svm.tune)
cost <- summary(svm.tune)$performances[1]
cv.err <- summary(svm.tune)$performances[2]
perf <- data.frame(cost, cv.err)
plot(log(perf$cost), log(perf$error))
minlin <- min(summary(svm.tune)$performances[3])

# The cross-validation finds that a cost of 1 results in the lowest error rate, about 1%, with higher costs around 2-3% error rates.

# (C)
set.seed(20)
svm.tune <- tune(svm, mpg.bin ~ ., data=Auto, kernel="radial", ranges=list(cost=c(0.01, 0.1, 1, 5, 10, 100), gamma=c(.5, 1, 2, 3, 4)))
summary(svm.tune)
cost <- summary(svm.tune)$performances[1]
gamma <- summary(svm.tune)$performances[2]
cv.err <- summary(svm.tune)$performances[3]
perf <- data.frame(cost=cost, gamma=gamma, error=cv.err)
plt <- ggplot()
plt + geom_point(data=perf, aes(log(perf$cost), perf$gamma, size=error))
# This plot shows us that larger values of cost and lower values of gamma lead to the lowest error rates. The lowest error rate is when cost=5 and gamma=0.5 (tied for cost=10, gamma=0.5)
minrad <- min(summary(svm.tune)$performances[3])

# Now for polynomial
set.seed(20)
svm.tune <- tune(svm, mpg.bin ~ ., data=Auto, kernel="polynomial", ranges=list(cost=c(0.01, 0.1, 1, 5, 10, 100), gamma=c(.5, 1, 2, 3, 4)))
summary(svm.tune)
cost <- summary(svm.tune)$performances[1]
gamma <- summary(svm.tune)$performances[2]
cv.err <- summary(svm.tune)$performances[3]
perf <- data.frame(cost=cost, gamma=gamma, error=cv.err)
plt <- ggplot()
plt + geom_point(data=perf, aes(log(perf$cost), perf$gamma, size=error))
minpoly <- min(summary(svm.tune)$performances[3])
# Here it becomes apparent that larger values of cost and alrger gammas result in a lower training error.  There are many options of cost and gamma to choose from, so we will choose as low a value of each as we can.  Cost = 1, gamma=0.5 is tied for the lowest error rate at 3.6%.
# (D)
# The support vector classifier had the lowest training error rate, and this is apparent in the svm plots where the linear classifier works moderately well and is visually splitting the data into two parts, whereas the radial and polynomial do not perform as well.
minlin
minrad
minpoly

svm.lin <- svm(mpg.bin ~ ., data=Auto, kernel="linear", cost=1)
svm.rad <- svm(mpg.bin ~ ., data=Auto, kernel="radial", cost=5, gamma=0.5)
svm.poly <- svm(mpg.bin ~ ., data=Auto, kernel="polynomial", cost=1, gamma=0.5)

plot(svm.lin, Auto, mpg~horsepower)
plot(svm.rad, Auto, mpg~horsepower)
plot(svm.poly, Auto, mpg~horsepower)

plot(svm.lin, Auto, mpg~acceleration)
plot(svm.rad, Auto, mpg~acceleration)
plot(svm.poly, Auto, mpg~acceleration)

plot(svm.lin, Auto, mpg~weight)
plot(svm.rad, Auto, mpg~weight)
plot(svm.poly, Auto, mpg~weight)


## Q8
library(ISLR)
# (A)
names(OJ)
samp.size <- 800
set.size <- dim(OJ)[1]
set.seed(1)
train <- sample(set.size, samp.size)
OJ.train <- OJ[train,]
OJ.test <- OJ[-train,]
OJ.Purchase <- OJ$Purchase[-train]
# (B)
# Support Vector Classifier means we will use a linear kernel.
svm.fit <- svm(Purchase~., data=OJ.train, kernel="linear", cost=0.01)
summary(svm.fit)
# Of the 800 observations in the training set, 440 of these are used as support vectors.  
# (C)
train.pred <- predict(svm.fit, data=OJ.train)
train.tbl <- table(train.pred, OJ$Purchase[train])
train.err <- sum(train.pred != OJ$Purchase[train])/samp.size
train.err
test.pred <- predict(svm.fit, newdata=OJ.test)
test.tbl <- table(test.pred, OJ.Purchase)
test.err <- sum(test.pred != OJ.Purchase)/(set.size - samp.size)
test.err
# The training error rate is 16.6% and the test error rate is 18.1%.
# (D)
set.seed(1)
svm.tune <- tune(svm, Purchase~., data=OJ.train, kernel="linear", ranges=list(cost=c(0.01, 0.1, 1, 5, 10)))
summary(svm.tune)
# The optimal value for the cost hyperparameter is 0.01 in this case, with an error of 17.5%. 
# (E)
# As we have previously seen, the error rates for a cost of 0.01 is 16.6% in training and 18.1% in testing.  For varieties sake we will try the second lowest cost as a parameter and find the training and testing error rates.  
svm.fit <- svm(Purchase~., data=OJ.train, kernel="linear", cost=1)
lin.train.pred <- predict(svm.fit, newdata=OJ.train)
lin.train.err <- sum(train.pred != OJ$Purchase[train]) / samp.size
lin.train.err
lin.test.pred <- predict(svm.fit, newdata=OJ.test)
lin.test.err <- sum(test.pred != OJ.Purchase) / (set.size - samp.size)
lin.test.err
# We now end up with a better training error rate but a worse test error rate, as expected from the cross validation.
# (F)
set.seed(1)
rad.svm.tune <- tune(svm, Purchase~., data=OJ.train, kernel="radial", ranges=list(cost=c(0.01, 0.1, 1, 5, 10)))
summary(rad.svm.tune)
bestmod <- summary(rad.svm.tune)$best.model
rad.train.pred <- predict(bestmod, newdata=OJ.train)
rad.train.err  <- sum(rad.train.pred != OJ$Purchase[train]) / samp.size
rad.train.err
rad.test.pred <- predict(bestmod, newdata=OJ.test)
rad.test.err <- sum(rad.test.pred != OJ.Purchase) / (set.size - samp.size)
rad.test.err
# The radial kernel has a training error of 13.75% and a test error of 18.1%.

# (G)
set.seed(1)
poly.svm.tune <- tune(svm, Purchase~., data=OJ.train, kernel="polynomial", degree=2, ranges=list(cost=c(0.01, 0.1, 1, 5, 10)))
summary(poly.svm.tune)
bestmod <- summary(poly.svm.tune)$best.model
poly.train.pred <- predict(bestmod, newdata=OJ.train)
poly.train.err  <- sum(poly.train.pred != OJ$Purchase[train]) / samp.size
poly.train.err
poly.test.pred <- predict(bestmod, newdata=OJ.test)
poly.test.err <- sum(poly.test.pred != OJ.Purchase) / (set.size - samp.size)
poly.test.err
# The polynomial kernel resulted in a training error of 14.88% and a test error of 18.1%.

# (H) 
lin.train.err
rad.train.err
poly.train.err

lin.test.err
rad.test.err
poly.test.err
# Overall the radial kernel resulted in the lowest training error by about 1%.
# However, the radial and polynomial kernels tied with lowest test error at 18.1%.  


