######################################################################################################################################################################################
### Ch. 5 Exercises
library(ISLR)
library(boot)

## Lab 5.3.1 Validation Set
?Auto
Attach(Auto)
set.seed(1)
train = sample(392, 196)

## Fit a linear regression to mpg v. horsepower and find the MSE of the test set.
lm.fit <- lm(mpg ~ horsepower, data=Auto, subset=train)
mean((mpg - predict(lm.fit, Auto))[-train]^2)
## Now find the MSE using polynomial regressions of higher order
lm.fit2 <- lm(mpg ~ poly(horsepower, 2), data=Auto, subset=train)
mean((mpg - predict(lm.fit2, Auto))[-train]^2)

lm.fit3 <- lm(mpg ~ poly(horsepower, 3), data=Auto, subset=train)
mean((mpg - predict(lm.fit3, Auto))[-train]^2)


## Lab 5.3.2 LOOCV

glm.fit <- glm(mpg ~ horsepower, data=Auto)
coef(glm.fit)

lm.fit <- lm(mpg ~ horsepower, data=Auto)
coef(lm.fit)

glm.fit <- glm(mpg ~ horsepower, data=Auto)
cv.err <- cv.glm(Auto, glm.fit)
cv.err$delta
# delta corresponds to the CV statistics, AKA the average of the MSE for each validation set

cv.error <- rep(0, 5)
for (i in 1:5){
  glm.fit <- glm(mpg ~ poly(horsepower, i), data=Auto)
  cv.error[i] <- cv.glm(Auto, glm.fit)$delta[1]
}
round(cv.error, 2)


## k-fold Cross Validation

set.seed(17)
cv.error.10 <- rep(0, 10)
for (i in 1:10){
  glm.fit <- glm(mpg ~ poly(horsepower, i), data=Auto)
  cv.error.10[i] <- cv.glm(Auto, glm.fit, K=10)$delta[1]
}
round(cv.error.10, 2)
# the delta values are the same for LOOCV, but can differ for k-fold CV.  The first is standard, the second bias-corrected


## Bootstrap
names(Portfolio)
alpha.fn <- function(data, index){
  X <- data$X[index]
  Y <- data$Y[index]
  return ((var(Y) - cov(X, Y)) / (var(X) + var(Y) - 2*cov(X, Y)))
}

# estimate alpha (see text example on Bootstrap) from the 100 X, Y observations in portfolio
alpha.fn(Portfolio, 1:100)

set.seed(1)
# A single boostrap sampling (with replacement) all 100 observations
alpha.fn(Portfolio, sample(100, 100, replace=TRUE))

# Boostrap analysis:
set.seed(1)
alpha.est <- rep(0, 100)
for (i in 1:100){
  alpha.est[i] <- alpha.fn(Portfolio, sample(100, 100, replace=TRUE))
}
mean(alpha.est)
sd(alpha.est)

# The boot() function automates this process
boot(Portfolio, alpha.fn, R=1000)

# Now try using the bootstrap to evaluate the coefficients in a linear model.
boot.fn <- function(data, index){
  return(coef(lm(mpg ~ horsepower, data=data, subset=index)))
}
boot.fn(Auto, 1:392)

set.seed(1)
boot.fn(Auto, sample(392, 392, replace=TRUE))
boot.fn(Auto, sample(392, 392, replace=TRUE))

boot(Auto, boot.fn, R=1000)

# Quadratic model bootstrap
boot.fn <- function(data, index){
  coef(lm(mpg ~ horsepower + I(horsepower^2), data=Auto, subset=index))
}
set.seed(1)
boot(Auto, boot.fn, R=1000)



######################################################################################################################################################################################
###  Ch. 5 Exercises

## Q1
# Minimize Var(aX + (1-a)Y)
# Property: Var(aX + bY) = a^2 Var(X) + b^2 Var(Y) + 2ab Cov(X, Y)
# >>> a^2 Var(X) + (1-a)^2 Var(Y) + 2a(1-a)Cov(X,Y)
# group terms by powers of a to find:
# a^2 (Var(X) + Var(Y) - 2Cov(X, Y)) + -2a(Var(Y) - Cov(X, Y)) + Var(Y)
# Take the derivative with respect to a and set equal to 0
# 2a(Var(X) + Var(Y) - 2Cov(X, Y)) = 2(Var(Y) - Cov(X, Y))
# Solve for a

## Q2
# (A) n-1/n   because there are n possible data points to choose from for the 1st bootstrap observation, and n-1 observations are NOT the jth observation from the original sample.
# (B) n-1/n 
# (C) Since the probabilities are independent, we can multiply them for each additional observation.  (n-1)/n * (n-1)/n * ..... = [(n-1)/n]^k where k is the number of observations in the bootsrrap sample.  If k is equal to n, the number in the original sample, then this expression trivially simplifies to [1-1/n]^n
# (D) When n=5, the probability the jth observation IS in the sample is:
prob <- function(n){
  return((1-1/n)^n)
}
prob(5)
1-prob(5)

# (E) When n=100
prob(100)
1-prob(100)

# (F) When n=10000
prob(10000)
1-prob(10000)

# (G)
max <- 100000
prbs <- rep(0, max)
for (i in 1:max){
  prbs[i] <- 1-prob(i)
}
plot(prbs)
# It is obvious that for a set of only 1 observation, the datapoint MUST be in the bootstrap sample.  From here on the plot quickly asymptotes to 1-e.

# (H) 
store <- rep(NA, 10000)
for(i in 1:10000){
  store[i] <- sum(sample(1:100, rep=TRUE) == 4)  > 0
}
mean(store)

# The code says to create a sample of 100 observations with replacement.  If any of them are the 4th observation, they will be TRUE and sum counts TRUE as 1 and FALSE as 0.  The average is .6418, which is near 1-e.

## Q3
# (A)  In k-fold cross validation the training set is broken up into k equal sets (as close to equal as possible).  The model is trained on k-1 sets and validated on the last set.  This process is repeated until each set has acted as the validation set, so that the MSE can be calculated for each of the k validation sets and averaged.  Thus, an average of many models is taken for the estimates from cross-validation.  

# (B)  (i) The advantages of k-fold cross validation is that it has much lower variance compared to the validation set approach, but a disadvantage is that it will take more time to complete since there are k models to create.
#       (ii)  The advantages of k-fold cross validation is that it has lower variance than LOOCV because in LOOCV the outputs are highly correlated, since the model is trained on the same data bar one data point. The k-fold method is less correlated resulting in lower test variance.  A disadvantage is that k-fold CV will take longer.  


## Q4
#  To estimate the standard deviation of the prediction we may use a bootstrap.  We could start by simply finding the standard deviation of the sample.  We may be dealing with non-parametric models or simply want a more representative standard error so this would naturally lead to a bootstrap prediction.  The sample will be repeatedly resampled (with replacement) and a model applied, and the required statistics will be measured on these bootstrap models.  Then, all of these resampled statistics will be averaged to generate an estimate of the prediction's standard deviation.

## Q5
names(Default)
set.seed(1)
# (A)
glm.fit <- glm(default ~ income + balance, data=Default, family=binomial)
summary(glm.fit)

# (B)
# Create a validation set that is half the size of the number of observations/
val.set.size <- length(default) / 2
train <- sample(2 * val.set.size, val.set.size, replace=FALSE)
val.set <- Default[-train, ]

# Fit a logistic regression model with income and balance.  Use the posterior probabilities to generate a prediction for defaulting: "yes" or "no".
glm.fit2 <- glm(default ~ income + balance, data=Default, subset=train, family=binomial)
glm.probs <- predict(glm.fit2, Default[-train,], type="response")
glm.pred = rep("No", 5000)
glm.pred[glm.probs > 0.5] = "Yes"
tbl <- table(glm.pred, val.set$default)
tbl
val.set.error <- (tbl[2] + tbl[3] )/ sum(tbl)
val.set.error
# The validation set error is 2.86%

# (C)
set.seed(10)
val.set.size <- length(default) / 2
train <- sample(2 * val.set.size, val.set.size, replace=FALSE)
val.set <- Default[-train, ]
glm.fit2 <- glm(default ~ income + balance, data=Default, subset=train, family=binomial)
glm.probs <- predict(glm.fit2, Default[-train,], type="response")
glm.pred = rep("No", 5000)
glm.pred[glm.probs > 0.5] = "Yes"
tbl <- table(glm.pred, val.set$default)
tbl
val.set.error <- (tbl[2] + tbl[3] )/ sum(tbl)
val.set.error
# The validation set error is 2.36%

set.seed(100)
val.set.size <- length(default) / 2
train <- sample(2 * val.set.size, val.set.size, replace=FALSE)
val.set <- Default[-train, ]
glm.fit2 <- glm(default ~ income + balance, data=Default, subset=train, family=binomial)
glm.probs <- predict(glm.fit2, Default[-train,], type="response")
glm.pred = rep("No", 5000)
glm.pred[glm.probs > 0.5] = "Yes"
tbl <- table(glm.pred, val.set$default)
tbl
val.set.error <- (tbl[2] + tbl[3] )/ sum(tbl)
val.set.error
# The validation set error is 2.58%

set.seed(1000)
val.set.size <- length(default) / 2
train <- sample(2 * val.set.size, val.set.size, replace=FALSE)
val.set <- Default[-train, ]
glm.fit2 <- glm(default ~ income + balance, data=Default, subset=train, family=binomial)
glm.probs <- predict(glm.fit2, Default[-train,], type="response")
glm.pred = rep("No", 5000)
glm.pred[glm.probs > 0.5] = "Yes"
tbl <- table(glm.pred, val.set$default)
tbl
val.set.error <- (tbl[2] + tbl[3] )/ sum(tbl)
val.set.error
# The validation set error is 2.5%.

# All of the validation set errors were very similar, around 2.5% misclassification rate.

# (D)
head(Default)
Default$isStud <- 0
Default$isStud[Default$student == "Yes"] <- 1

set.seed(1)
val.set.size <- length(default) / 2
train <- sample(2 * val.set.size, val.set.size, replace=FALSE)
val.set <- Default[-train, ]
glm.fit2 <- glm(default ~ income + balance + isStud, data=Default, subset=train, family=binomial)
glm.probs <- predict(glm.fit2, Default[-train,], type="response")
glm.pred = rep("No", 5000)
glm.pred[glm.probs > 0.5] = "Yes"
tbl <- table(glm.pred, val.set$default)
tbl
val.set.error <- (tbl[2] + tbl[3] )/ sum(tbl)
val.set.error
# The validation set error is 2.88%.  
# Including student as a variable in the model made a minimal difference in the validation set error.  This leads us to conclude that student is most likely not an important variable in predicting whether someone will default.


# Q6
# (A)
set.seed(1)
glm.fit <- glm(default ~ income + balance, data=Default, family=binomial)
summary(glm.fit)

# (B) 

boot.fn <- function(data, index){
  glm.fit <- glm(default ~ income + balance, data=data, subset=index, family=binomial)
  return(glm.fit$coeff)
}

# (C) 
boot(Default, boot.fn, R=1000)

# (D)
summary(glm.fit)
# The estimates for the standard errors of the intercept, income, and balance coefficients found using the bootstrap is very close to those found via the glm model.  they are all slightly smaller than the glm estimates.


## Q7
# (A)
set.seed(1)
summary(Weekly)
glm.fit <- glm(Direction ~ Lag1 + Lag2, data=Weekly, family=binomial)
summary(glm.fit)

# (B) 
glm.fit <- glm(Direction ~ Lag1 + Lag2, data=Weekly, subset=-1, family=binomial)
summary(glm.fit)

# (C)
glm.probs <- predict(glm.fit, Weekly[1,], type="response")
glm.pred <- rep("Down", 1)
glm.pred[glm.probs > 0.5] <- "Up"
tbl <- table(glm.pred, Direction[1])
tbl
mean(glm.pred == Direction[1])
#  No, this was not classified correctly.  

# (D) 
LOOCV.err <- rep(NA, nrow(Weekly))
for(i in 1:nrow(Weekly)){
  glm.fit <- glm(Direction ~ Lag1 + Lag2, data=Weekly, subset=-i, family=binomial)
  glm.probs <- predict(glm.fit, Weekly[i, ], type="response")
  glm.pred <- rep("Down", 1)
  glm.pred[glm.probs > 0.5] <- "Up"
  tbl <- table(glm.pred, Direction[i])
  LOOCV.err[i] <- mean(glm.pred == Direction[i])
}
mean(LOOCV.err)

# (E)
mean(LOOCV.err)
#  The test error rate estimated using the LOOCV method was 55.0%.  The error rate of the model without using LOOCV was 55.6%, so the LOOCV estimate is off by 0.6%, relatively close.  

## Q8
# (A) 
set.seed(1)
y <- rnorm(100)
x <- rnorm(100)
y <- x-2*x^2 + rnorm(100)
# In this data set n=100 and p=2, the order of the polynomial.
# Y = X - 2X^2 + E

# (B)
plot(x, y)
# The quadratic pattern from the formula can easily be observed, with the random error from the formula apparent in the imperfect concave down parabola.  

# (C) 
library(boot)

set.seed(1)
xy <- data.frame(x, y)

cv.results <- rep(0, 4)
for(i in 1:4){
  glm.fit <- glm(y ~ poly(x, i), data=xy)
  cv.err <- cv.glm(xy, glm.fit)
  cv.results[i] <- cv.err$delta[1]
}
cv.results
# There is a large decrease in error going from a linear model to quadratic, but no higher orders do noticeably different from quadratic.

# (D)

set.seed(100)

cv.results <- rep(0, 4)
for(i in 1:4){
  glm.fit <- glm(y ~ poly(x, i), data=xy)
  print(summary(glm.fit))
  cv.err <- cv.glm(xy, glm.fit)
  cv.results[i] <- cv.err$delta[1]
}
cv.results
# The results are the exact same as the first random seed.  The LOOCV is entirely deterministic for regression by the formula, since leaving out one measurement will always result in the same model regardless of random seed since ALL other measurements are used.

# (E) 
# The quadratic model has the lowest error, which is expected because we have the prior knowledge that the true model was quadratic.

# (F)
# Each of the models shows that the first- and second-degree terms are statistically significant, but no higher-degree terms.  This is what we would expect, and the cross-validation reinforces this idea with the large decrease in CV error at degree 2, but no dip afterwards.

## Q9
# (A)
library(MASS)
summary(Boston)
mean(Boston$medv)

# (B)
sd(Boston$medv) / sqrt(nrow(Boston))
# The sample median value of an owner-occupied home is on average $409 away from the mean of $22,532

# (C) 
set.seed(1)
boot.fn <- function(data, index){
  return(mean(data[index]))
}

est <- boot(Boston$medv, boot.fn, R=1000)


# The mean is estimated to be 22.533 with a standard error of 0.412, very close to the sample standard error of $409.

# (D)
m.x <- est$t0
s.e <- sd(est$t)
n <- nrow(Boston)
t.crit <- qt(0.025, n - 1, lower.tail=FALSE)
m.x - t.crit * s.e
m.x + t.crit * s.e

t.test(Boston$medv)
# The confidence interval from the bootstrapped estimates matches to 1 decimal place.  

# (E)  
# An estimate for the median value of medv is 21.2
median(Boston$medv)

# (F) 
set.seed(1)
boot.fn <- function(data, index){
  return(median(data[index]))
}

est <- boot(Boston$medv, boot.fn, R=1000)
est

# The estimate for the standard error of the median value of medv is 0.387.  This is somewhat smaller than the standard error of the mean value, and only accessible via bootstrap.

# (G)
quantile(Boston$medv, 0.1)

# (H)
set.seed(1)
boot.fn <- function(data, index){
  return(quantile(data[index], 0.1))
}

est <- boot(Boston$medv, boot.fn, R=1000)
est
# The estimated standard error of the tenth percentile is 0.505, larger than the standard error of the median or the mean.  This seems plausible because the 10th percentile is towards the lower extreme of the data, which may contain outliers (or the reverse!)  which would affect the 10th percentile more than the mean or median