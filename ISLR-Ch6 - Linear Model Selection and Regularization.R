######################################################################################################################################################################################
### ISLR-CH6 Lab

## 6.5.1 Best Subset Selection
library(ISLR)
library(leaps)
library(glmnet)
library(pls)
library(ggplot2)
library(MASS)

options(digits=3)

names(Hitters)
summary(Hitters)
fix(Hitters)
which(is.na(Hitters))
## Upon inspection, there are 59 entries in Salary that are NA.  All other columns are complete.  We can remove the rows (players) with missing salaries.
dim(Hitters)
Hitters = na.omit(Hitters)
dim(Hitters)

regfit.full <- regsubsets(Salary ~., data=Hitters)
summary(regfit.full)

regfit.full <- regsubsets(Salary ~., data=Hitters, nvmax=19)
reg.summary <- summary(regfit.full)
names(reg.summary)
which.min(reg.summary$rss)
which.max(reg.summary$rsq)
which.min(reg.summary$bic)
which.min(reg.summary$cp)
nmodels <- length(reg.summary$rss)

best <- data.frame("index"=seq(1:nmodels), "rsq"=reg.summary$rsq, "rss"=reg.summary$rss, "cp"=reg.summary$cp, "bic"=reg.summary$bic)
best$nrss <- (best$rss - min(best$rss))/(max(best$rss) - min(best$rss))
best$ncp <- (best$cp - min(best$cp))/(max(best$cp) - min(best$cp))
best$nbic <- (best$bic - min(best$bic))/(max(best$bic) - min(best$bic))
head(best)

plt <- ggplot() +
  geom_point(data=best, aes(x=index, y=nrss, color="RSS")) +
  geom_point(data=best, aes(x=index, y=ncp, color="CP")) +
  geom_point(data=best, aes(x=index, y=nbic, color="BIC")) +
  labs(title="Error Measurements", x="Index", y="Normalized Scale", color="Evaluation Method") +
  scale_color_manual(values=c("RSS"="Red", "CP"="Purple", "BIC"="Blue"))
plt

plot(regfit.full, scale="r2")
plot(regfit.full, scale="adjr2")
plot(regfit.full, scale="Cp")
plot(regfit.full, scale="bic")
coef(regfit.full, 6)


## 6.5.2 Forward and Backward Stepwise Selection
regfit.fwd <- regsubsets(Salary ~., data=Hitters, nvmax=10, method="forward")
summary(regfit.fwd)
plot(regfit.fwd, scale="r2")
plot(regfit.fwd, scale="adjr2")
plot(regfit.fwd, scale="Cp")
plot(regfit.fwd, scale="bic")

regfit.bwd <- regsubsets(Salary ~., data=Hitters, nvmax=10, method="backward")
summary(regfit.bwd)

## 6.5.3 Validation and Cross-Validation
set.seed(1)
train <- sample(c(TRUE, FALSE), nrow(Hitters), replace=TRUE)
test <- (!train)

regfit.best <- regsubsets(Salary~., data=Hitters[train, ], nvmax=19)
summ <- summary(regfit.best)

test.mat <- model.matrix(Salary~., data=Hitters[test, ])
val.errors <- rep(NA, 19)
for(i in 1:19){
  coefi <- coef(regfit.best, id=i)
  pred <- test.mat[,names(coefi)]%*%coefi
  val.errors[i] <- mean((Hitters$Salary[test] - pred)^2)
}
which.min(val.errors)
plot(val.errors)

######### Predict function for best model using validation.  
# Object is the regsubsets model
# newdata is the dataframe
# id is the model of size "id"
predict.regsubsets <- function(object, newdata, id,...){
  form <- as.formula(object$call [[2]])
  mat <- model.matrix(form, newdata)
  coefi <- coef(object, id=id)
  xvars <- names(coefi)
  mat[,xvars]%*%coefi
}
#########

## Apply the best size model to the entire dataset
regfit.best <- regsubsets(Salary~., data=Hitters, nvmax=19)
coef(regfit.best, 10)


## Using Cross-validation
# For k-fold cross validation, assign each data point to one of the k-folds and create a cv error matrix to store data in later.
k <- 10
set.seed(1)
folds <- sample(1:k, nrow(Hitters), replace=TRUE)
cv.errors <- matrix(NA, k, 19, dimnames=list(NULL, paste(1:19)))

## Each of the k-folds will act as a test set.  Train the model on the rest of the data (training set) and make predictions on the test set.  Compute MSE for each test set with i variables in it.
for(j in 1:k){
  best.fit <- regsubsets(Salary~., data=Hitters[folds!=j,], nvmax=19)
  for(i in 1:19){
    pred <- predict(best.fit, Hitters[folds==j,], id=i)
    cv.errors[j, i] <- mean((Hitters$Salary[folds==j] - pred)^2)
  }
}
# Find the average MSE the models of each # of variables.  Then, select the smallest one.
mean.cv.errors <- apply(cv.errors, 2, mean)
mean.cv.errors
which.min(mean.cv.errors)
plot(mean.cv.errors, type="b")

# The 11-variable model is best.  Perform the fit on the entire dataset for 11-variables.
best.fit <- regsubsets(Salary~., data=Hitters, nvmax=19)
coef(best.fit, 11)


### 6.6 Ridge Regression and the LASSO

## Ridge Regression
Hitters = na.omit(Hitters)
x <- model.matrix(Salary~., Hitters)[,-1]
y <- Hitters$Salary

grid <- 10^seq(10, -2, length=100)
ridge.model <- glmnet(x, y, alpha=0, lambda=grid)
dim(coef(ridge.model))

ridge.model$lambda[50]
coef(ridge.model)[,50]

ridge.model$lambda[60]
coef(ridge.model)[,60]
sqrt(sum(coef(ridge.model)[-1, 60]^2))

#Predicts the coefficients for a lambda value of 50
predict(ridge.model, s=50, type="coefficients")[1:20,]

set.seed(1)
train <- sample(1:nrow(x), nrow(x)/2)
test <- (-train)
y.test <- y[test]

# Model with lambda = 4
ridge.model <- glmnet(x[train,], y[train], alpha=0, lambda=grid, thresh=1e-12)
ridge.pred <- predict(ridge.model, s=4, newx=x[test,])
mean((ridge.pred - y.test)^2)

# Model with just an intercept (AKA no other)
mean((mean(y[train])-y.test)^2)

# Model with lambda = 10^10
ridge.pred <- predict(ridge.model, s=1e10, newx=x[test,])
mean((ridge.pred - y.test)^2)

# Model with lambda = 0 (AKA Linear Regression)
ridge.pred <- predict(ridge.model, s=0, newx=x[test,])
mean((ridge.pred - y.test)^2)

lm(y~x, subset=train)
## predict(ridge.model, s=0, exact=TRUE, type="coefficients")[1:20,]

# Instead of choosing a random lambda (such as 4), use cross-validation to tune the lambda parameter.  Find the lowest cv and use that lambda.  
set.seed(1)
cv.out <- cv.glmnet(x[train,], y[train], alpha=0)
plot(cv.out)
bestlam <- cv.out$lambda.min
bestlam
ridge.pred <- predict(ridge.model, s=bestlam, newx=x[test,])
mean((ridge.pred - y.test)^2)

out <- glmnet(x, y, alpha=0)
predict(out, s=bestlam, type="coefficients")[1:20,]


## LASSO

lasso.model <- glmnet(x[train,], y[train], alpha=1, lambda=grid)
plot(lasso.model)

set.seed(1)
cv.out  <- cv.glmnet(x[train,], y[train], alpha=1)
plot(cv.out)
bestlam <- cv.out$lambda.min
bestlam
lasso.pred <- predict(lasso.model, s=bestlam, newx=x[test,])
mean((lasso.pred - y.test)^2)

outLasso <- glmnet(x, y, alpha=1, lambda=grid)
lasso.coef <- predict(outLasso, s=bestlam, type="coefficients")[1:20,]
lasso.coef
lasso.coef[lasso.coef != 0]


### 6.7 - PCR and PLS Regression

## Principal Component Regression

Hitters = na.omit(Hitters)
set.seed(2)
pcr.fit <- pcr(Salary~., data=Hitters, scale=TRUE, validation="CV")
summary(pcr.fit)
validationplot(pcr.fit, val.type="MSEP")

set.seed(1)
pcr.fit <- pcr(Salary~., data=Hitters, subset=train, scale=TRUE, validation="CV")
validationplot(pcr.fit, val.type="MSEP")
pcr.pred <- predict(pcr.fit, x[test,], ncomp=7)
mean((pcr.pred - y.test)^2)

pcr.fit <- pcr(y ~ x, scale=TRUE, ncomp=7)
summary(pcr.fit)


## Partial Least Squares

set.seed(1)
pls.fit <- plsr(Salary~., data=Hitters, subset=train, scale=TRUE, validation="CV")
summary(pls.fit)
validationplot(pls.fit, val.type="MSEP")

pls.pred <- predict(pls.fit, x[test,], ncomp=2)
mean((pls.pred - y.test)^2)

# The number of components identified in CV is 2, so fit the full dataset using M=2 components
pls.fit <- plsr(Salary~., data=Hitters, scale=TRUE, validation="CV", ncomp=2)
summary(pls.fit)


###########################################################################################
###########################################################################################
## Ch. 6 Exercises

## Q1
# (A) The best subset selection will have the best training RSS because the process involves fitting the training data to find the lowest RSS/highest R^2, whereas forward and backward selection are greedy algorithms which may miss on the "best possible" model.

# (B) We cannot know which model will have the smallest test RSS without knowing the test data.  It may seem like best subset selection will result in the smallest test RSS but recall that training RSS can be very low but test RSS may be higher due to overfitting.

# (C)
#     (i) TRUE, the (k+1) variable model will use the k previous predictors and find the next predictor that minimizes the training RSS.  
#     (ii) TRUE, the (k+1) variable model will use the k previous predictors deemed "best" and then find the next predictor that minimizes the training RSS.
#     (iii) FALSE, the backward and forward selection may find different variables they deem significant/not significant
#     (iv) FALSE, the backward and forward selection may find different variables they deem significant/not significant
#     (v) FALSE, The best subset selection can find a model with k+1 predictors, some of which are different than the k previous predictors due to minimizing the RSS and finding the BEST combination.  

## Q2
# (A) (iii) The LASSO is less flexible than least squares because it limits many of the coefficients in least squares, sometimes reducing them to 0.  As there are less coefficients and less variables that affect the model, the flexibility decreases but the variance will also decrease, hoping to balance out an increasing bias.

# (B) (iii) Ridge regression is less flexible than least squares for reasons similar to LASSO.  It is attempting to minimize the impact of extraneous variables on the model, leading to less fitting to the noise and a decrease in variance, counterbalancing the increase in bias.

# (C) (ii) Nonlinear methods are known to be more flexible than least squares, but with this can come overfitting to the training data and an increase in variance, while the model will fit the data very closely, decreasing bias.  

## Q3
# (A) (iv) As s -> infinity, we are including more and more impact of each variable on the model.  This has the ability to fit to the noise, and as more variables are included the TRAINING RSS will decrease.  
# (B) (ii) If s is 0 then the coefficients are all 0 and the model consists of only the intercept.  Including some of the variables should improve the model's performance on test data until the noise variables are reincluded, and as s-> infinity the model approaches the least squares.
# (C) (iii) As s-> infinity the model becomes more flexible and thus the variance will increase.
# (D) (iv) As s -> infininty the model will more closely fit to the data, as discussed in part (A), which will decrease the bias.  
# (E) (v)  The irreducible error will not change regardless of the model.

## Q4
# (A) (iii) As lambda -> infinity, we are restricting the coefficients more and more.  This is the reverse of Q3, Part (A).  The training RSS will increase.
# (B) (ii) If lambda is 0 then the coefficients are that of the least squares model.  As lambda grows we may start to get a better and better prediction of the test set (lower test RSS) until the signal variables are accounted for in the model.  As lambda keeps growing we will end up underfitting the model and the test RSS will increase again.
# (C) (iv) As lambda -> infinity the model becomes less flexible and thus the variance will decrease.
# (D) (iii) As lambda -> infinity the model will more less closely fit to the training data, as discussed in part (A), which will increase the bias.  
# (E) (v)  The irreducible error will not change regardless of the model.

## Q5

# (A) (y1 - b1 * x11 - b2 * x12)^2 + (y2 - b1 * x21 - b2 * x22 )^2 + lambda * (b1^2 + b2^2)
# (B) Since x11 = -x21 and x12 = -x22, we can differentiate wrt b1 and set the result equal to 0.  Solve for b1.  Do similarly for b2.  They are symmetric in b1 and b2, so we can conclude that b1=b2 satisfies the minimization.  
# (C) (y1 - b1 * x11 - b2 * x12)^2 + (y2 - b1 * x21 - b2 * x22 )^2 + lambda * abs(b1) + abs(b2)
# (D) In the lasso optimization as seen in Figure 6.7 the solutions to the constraint lie along the edge of the diamond.  Because the entire edge is a solution, there are infinitely many combinations of b1 and b2 that would satisfy the optimization problem.

## Q6
# (A)
y1 <- 10
lambda <- 8
x <- c(seq(-15, 15, by=.001))


y <- (y1 - x)^2 + lambda*x^2
plt <- ggplot() + geom_point(data=data.frame(x, y), aes(x, y)) + labs(title="Plot of Minimization v. b1", x="b1", y="minimzation")
y1 / (1+lambda)
min(y)
x[which.min(y)]
# The plot confirms that the minimization function is optimized when b1 is given by formula (6.14)
plt + geom_vline(xintercept=x[which.min(y)])

# (B)
y <- (y1 - x)^2 + lambda*abs(x)
plt <- ggplot() + geom_point(data=data.frame(x, y), aes(x, y)) + labs(title="Plot of Minimization v. b1", x="b1", y="minimzation")
y1-lambda/2

# In this case y1 > lambda/2, so the minimum is given by y1-lambda/2 (red line.)
plt + geom_vline(xintercept= c(lambda/2, -lambda/2)) + geom_vline(xintercept = y1-lambda/2, color="red")

# If y1 < lambda/2:
y1 <- -20
lambda <- 25
y <- (y1 - x)^2 + lambda*abs(x)
plt <- ggplot() + geom_point(data=data.frame(x, y), aes(x, y)) + labs(title="Plot of Minimization v. b1", x="b1", y="minimzation")
plt + geom_vline(xintercept= c(lambda/2, -lambda/2)) + geom_vline(xintercept = y1+lambda/2, color="red")

# IF abs(y1) < lambda/2
y1 <- 5
lambda <- 25
y <- (y1 - x)^2 + lambda*abs(x)
plt <- ggplot() + geom_point(data=data.frame(x, y), aes(x, y)) + labs(title="Plot of Minimization v. b1", x="b1", y="minimzation")
plt + geom_vline(xintercept= c(lambda/2, -lambda/2)) + geom_vline(xintercept = 0, color="red")


# Q7
# (A) - (B)  See paper
# (C) The lasso is the mode as we can see the constraint term in p(beta).  We have shifted the mode from 0 to our new term.
# (D) See paper
# (E)  The normal distribution has

## Q8
# (A)
set.seed(1)
X <- rnorm(100)
eps <- rnorm(100)
# (B) 
b <- c(3, 2, -3, 0.3)
Y <- b[1] + b[2]*X + b[3]*X^2 + b[4]*X^3 + eps
# (C)
XY <- data.frame(X, Y)
ggplot() + geom_point(data=XY, aes(X, Y))
best.fit <- regsubsets(Y~poly(X, 10, raw=TRUE), data=XY, nvmax=10)
best.sum <- summary(best.fit)
names(best.sum)
plot(best.sum$adjr2)
plot(best.sum$rss)
plot(best.sum$cp)
plot(best.sum$bic)
which.max(best.sum$adjr2)
which.min(best.sum$cp)
which.min(best.sum$bic)
# From these plots it is obvious that the R^2 (and thus RSS), CP, and BIC all drastically change when we use a degree 3 polynomial.  After 3 the statistics all flatten out.  This aligns with what we know from model creation.
summary(best.fit)
names(best.fit)
coef(best.fit, 3)
# (D) USing forward selection:
fwd.fit <- regsubsets(Y~poly(X, 10, raw=TRUE), data=XY, nvmax=10, method="forward")
fwd.sum <- summary(fwd.fit)
fwd.sum
names(fwd.sum)
plot(fwd.sum$adjr2)
plot(fwd.sum$rss)
plot(fwd.sum$cp)
plot(fwd.sum$bic)
which.max(fwd.sum$adjr2)
which.min(fwd.sum$cp)
which.min(fwd.sum$bic)
names(fwd.fit)
coef(fwd.fit, 3)
# Using Backward Selection
bwd.fit <- regsubsets(Y~poly(X, 10, raw=TRUE), data=XY, nvmax=10, method="backward")
bwd.sum <- summary(bwd.fit)
bwd.sum
names(bwd.sum)
plot(bwd.sum$adjr2)
plot(bwd.sum$rss)
plot(bwd.sum$cp)
plot(bwd.sum$bic)
which.max(bwd.sum$adjr2)
which.min(bwd.sum$cp)
which.min(bwd.sum$bic)
names(bwd.fit)
coef(bwd.fit, 3)

# Each of the models chooses the 3rd degree polynomial as the one that optimizes the appropriate measures.  

## (E) LASSO model

XX <- model.matrix(Y~poly(X, 10, raw=TRUE), XY)[,-1]
grid <- 10^seq(10, -2, length=100)
lasso.model <- glmnet(XX, Y, alpha=1, lambda=grid)
plot(lasso.model)

set.seed(1)
cv.out  <- cv.glmnet(XX, Y, alpha=1)
plot(cv.out)
bestlam <- cv.out$lambda.min
bestlam
lasso.pred <- predict(lasso.model, s=bestlam, newx <- XX)
mean((lasso.pred - Y)^2)

outLasso <- glmnet(XX, Y, alpha=1, lambda=grid)
lasso.coef <- predict(outLasso, s=bestlam, type="coefficients")
lasso.coef
lasso.coef[lasso.coef != 0]
# According to the LASSO model the important coefficients are 1, 2, 5, and 7.  It is also apparent that the first two coefficients are the largest with the 5th and 7th order coefficients are much smaller.  

y.pred <- 0
for(i in seq(1:11)){
  y.pred <- y.pred + lasso.coef[i] * X^(i-1)
}

plt <- ggplot() + geom_point(data=XY, aes(X, Y))
plt + geom_point(data=data.frame(X, y.pred), aes(X, y.pred), color="red")
# Ultimately the fit looks to be pretty near the actual data.  

# (F)
b0 <- 3
b7 <- 7
new.Y <- b0 + b7 * X^7 + eps
## Best Subset Selection
new.XY <- data.frame(X, new.Y)
ggplot() + geom_point(data=new.XY, aes(X, new.Y))
best.fit <- regsubsets(new.Y~poly(X, 10, raw=TRUE), data=new.XY, nvmax=10)
best.sum <- summary(best.fit)
names(best.sum)
plot(best.sum$adjr2)
plot(best.sum$cp)
plot(best.sum$bic)
which.max(best.sum$adjr2)
which.min(best.sum$cp)
which.min(best.sum$bic)
# The measures do not agree on the optimal degree of the polynomial.  
summary(best.fit)
names(best.fit)
coef(best.fit, id = 1)
coef(best.fit, id = 2)
coef(best.fit, id = 4)

## LASSO

new.XX <- model.matrix(new.Y~poly(X, 10, raw=TRUE), new.XY)[,-1]
grid <- 10^seq(10, -2, length=100)
lasso.model <- glmnet(new.XX, new.Y, alpha=1, lambda=grid)
plot(lasso.model)


cv.out  <- cv.glmnet(new.XX, new.Y, alpha=1)
plot(cv.out)
bestlam <- cv.out$lambda.min
bestlam
outLasso <- glmnet(new.XX, new.Y, alpha=1, lambda=grid)
lasso.coef <- predict(outLasso, s=bestlam, type="coefficients")
lasso.coef

# The LASSO does much better, identifying a 7th order polynomial and roughly accurate with the coefficients.  

## Q9
fix(College)
dim(College)[1]

# (A)
set.seed(1)
train <- sample(c(TRUE, TRUE, FALSE), nrow(College), replace=TRUE)
test <- (!train)

# (B) Least Squares
lm.fit <- lm(Apps ~., data=College, subset=train)
summary(lm.fit)
lm.pred <- predict(lm.fit, College[test,])
lm.MSE <- mean((lm.pred - College$Apps[test])^2)
lm.MSE
lm.rsq <- summary(lm.fit)$r.squared
lm.rsq

# (C) Ridge Regression

x <- model.matrix(College$Apps~., College)[,-1]
y <- College$Apps
y.test <- College$Apps[test]
grid <- 10^seq(10, -2, length=100)

ridge.model <- glmnet(x[train,], y[train], alpha=0, lambda=grid, thresh=1e-12)
cv.ridge <- cv.glmnet(x[train,], y[train], alpha=0, lambda=grid, thresh=1e-12)
plot(cv.ridge)
bestlam <- cv.ridge$lambda.min
bestlam
ridge.pred <- predict(ridge.model, s=bestlam, newx=x[test,])
ridge.MSE <- mean((ridge.pred - y.test)^2)
ridge.MSE
#Find Rsquared from tss (total sum of squares) and error of sum of squares (ess)
tss <- sum((y.test-mean(y.test))^2)
ess <- sum((ridge.pred - y.test)^2)
ridge.rsq <- 1-ess/tss

# (D) LASSO
lasso.model <- glmnet(x[train,], y[train], alpha=1)
cv.lasso <- cv.glmnet(x[train,], y[train], alpha=1)
plot(cv.lasso)
bestlam <- cv.lasso$lambda.min
bestlam
lasso.pred <- predict(lasso.model, s=bestlam, newx=x[test,])
lasso.MSE <- mean((lasso.pred - y.test)^2)
lasso.MSE
#Find Rsquared from tss (total sum of squares) and error of sum of squares (ess)
tss <- sum((y.test - mean(y.test))^2)
ess <- sum((lasso.pred - y.test)^2)
lasso.rsq <- 1-ess/tss

# (E) PCR Model
pcr.fit <- pcr(College$Apps~., data=College, subset=train, scale=TRUE, validation="CV")
validationplot(pcr.fit, val.type="MSEP")
# The minimum is reached when M=16, but remains relatively flat after M=5 components.  However, the scale skews this so a small reduction on the graph is a large absolute reduction.  We will use M=16.  
summary(pcr.fit)
pcr.pred <- predict(pcr.fit, x[test,], ncomp=16)
pcr.MSE <- mean((pcr.pred - y.test)^2)
pcr.MSE
pcr.rsq <- R2(pcr.fit, ncomp=16)
pcr.rsq <- 0.916

# (F) PLS Model
pls.fit <- plsr(College$Apps ~ ., data=College, subset=train, scale=TRUE, validation="CV")
validationplot(pls.fit, val.type="MSEP")
summary(pls.fit)
# The graph starts to asymptote around M=7 components.  
pls.pred <- predict(pls.fit, x[test,], ncomp=7)
pls.MSE <- mean((pls.pred - y.test)^2)
pls.MSE
pls.rsq <- R2(pls.fit, ncomp=7)
pls.rsq <- 0.909

lbls <- c("lm", "ridge", "lasso", "pcr", "pls")
MSE <- round(c(lm.MSE, ridge.MSE, lasso.MSE, pcr.MSE, pls.MSE), 0)
rsq <- round(c(lm.rsq, ridge.rsq, lasso.rsq, pcr.rsq, pls.rsq), 3)
errors <- data.frame(lbls, MSE, rsq)
errors
# The MSE for all of the models do not vary too much, ranging from about 1.62 million to 1.74 million.  The model that performed best to predict the number of applications to each College was actually the linear model.  The highest rsquared is also shared by the linear model by about 1%, with the lowest again from ridge regression.

barplot(rsq, names.arg=lbls, ylim=c(0, 1))

## Q10

# (A) 
# Create a coefficient vector with some of them zeroes, then convert this to the diagonal of an empty matrix.  Create a dataset 1000 X 20 and an error vector.  Perform matrix multiplication and sum each row to find the output y.
set.seed(1)
n=1000
p=20
beta <- sample(3, p, replace=TRUE) - 1
ex <- matrix(data=0, nrow=p, ncol=p)
diag(ex) <- beta
dataset <- matrix(rnorm(p*n), nrow=n)
eps <- rnorm(n)
y <- rowSums(dataset %*% ex) + eps
dataset <- cbind(dataset, y)

# (B)
set.seed(1)
train <- sample(1000, 100, replace=FALSE)
test <- (-train)

# (C)
df <- as.data.frame(dataset)
train.mat <- model.matrix(y~., data=df[train,])
best.fit <- regsubsets(y ~., data=df[train,], nvmax=p)
best.MSE <- rep(NA, p)
for(i in 1:p){
  coefi <- coef(best.fit, id=i)
  best.pred <- train.mat[,names(coefi)]%*%coefi
  best.MSE[i] <- mean((y[train] - best.pred)^2)
}
index = seq(1:p)

ggplot() +
  geom_point(data=data.frame(index, best.MSE), aes(index, best.MSE)) +
  labs(title="Training MSE of Best Model for each # of components", x="Size of model", y="Training MSE")

which.min(best.MSE)

# (D)
test.mat <- model.matrix(y~., data=df[test,])
best.MSE <- rep(NA, p)
for(i in 1:p){
  coefi <- coef(best.fit, id=i)
  best.pred <- test.mat[,names(coefi)]%*%coefi
  best.MSE[i] <- mean((y[test] - best.pred)^2)
}
index = seq(1:p)

ggplot() +
  geom_point(data=data.frame(index, best.MSE), aes(index, best.MSE)) +
  labs(title="Test MSE of Best Model for each # of components", x="Size of model", y="Test MSE")

which.min(best.MSE)

# (E)
# The training set MSE was minimized for a model of size = 20, the maximum possible size.  The test set MSE Was minimized for a model of size = 15, which is notably NOT the maximum size.  This is because the training MSE will always decrease since more features (Variables) will ALWAYS lead to a closer fit to the data being modelled.  However, when testing it on new data overfitting may have occurred in the model.  

# (F)
beta
coef(best.fit, id=15)
# The coefficients of the test set with the lowest MSE are, for the most part, quite close to the original coefficients used to create the model.  They are mostly within 10% of the actual values.  In fact, the best selection discovered all of the 0 coefficients.  

# (G)
x_cols <- colnames(df, do.NULL=FALSE, prefix="x.")
RMSE.coef <- rep(NA, p)
for(i in 1:p){
  coefi <- coef(best.fit, id=i)
  RMSE.coef[i] <- sqrt(sum(coefi[names(coefi) %in% x_cols] - beta[x_cols %in% names(coefi)])^2)
}

ggplot() +
  geom_point(data=data.frame(RMSE.coef, index), aes(index, RMSE.coef))

which.min(RMSE.coef)
# The minimum for the RMSE of the coefficients occurs at r=10, which is not the same as the lowest test MSE.  Thus, though the coefficients may fit better it does not predict the outcome as well as the r=15 model.


## Q11
# (A) Best subset selection, lasso, ridge regression, and PCR will be used on the Boston dataset to predict the crime rate per capita based on the recorded variables.
library(MASS)
names(Boston)
attach(Boston)
n <- dim(Boston)[1]
p <- length(names(Boston)) - 1

## Use 10-fold cross-validation on the Best Subset Selection
k <- 10
set.seed(1)
folds <- sample(1:k, nrow(Boston), replace=TRUE)
best.cv.errors <- matrix(NA, k, p, dimnames=list(NULL, paste(1:p)))
for(j in 1:k){
  best.fit <- regsubsets(crim~., data=Boston[folds!=j,], nvmax=p)
  for(i in 1:p){
    pred <- predict(best.fit, Boston[folds==j,], id=i)
    best.cv.errors[j, i] <- mean((crim[folds==j] - pred)^2)
  }
}
## Find the average MSE of the models of each # of variables and select the smallest one.
mean.best.cv.errors <- apply(best.cv.errors, 2, mean)
best.p <- which.min(mean.best.cv.errors)
best.p.error <- mean.best.cv.errors[best.p]
plot(mean.best.cv.errors, type="b", pch=19)

## Ridge Regression
# Use the same folds to perform a cross-validation
grid <- 10^seq(5, -2, length=100)
Bos.mat <- model.matrix(crim~., Boston)[,-1]
ridge.fit <- cv.glmnet(Bos.mat, crim, alpha=0, lambda=grid, foldid=folds)
bestlam <- ridge.fit$lambda.min
plot(ridge.fit)
abline(v = bestlam)
ridge.error <- ridge.fit$cvm[ridge.fit$lambda == bestlam]

## LASSO
lasso.fit <- cv.glmnet(Bos.mat, crim, alpha=1, lambda=grid, foldid=folds)
bestlam <- lasso.fit$lambda.min
plot(lasso.fit)
abline(v=bestlam)
lasso.error <- lasso.fit$cvm[lasso.fit$lambda == bestlam]

## PCR
pcr.fit <- pcr(crim~., data=Boston, scale=TRUE, validation="CV")
validationplot(pcr.fit, val.type="MSEP")
summary(pcr.fit)
MSEP(pcr.fit)$val[1,1,]
min.cv.index <- which.min(MSEP(pcr.fit)$val[1,1,])
pcr.error <- MSEP(pcr.fit)$val[1,1,min.cv.index]

lbls <- c("Best", "Ridge", "Lasso", "PCR")
mod.errs <- c(best.p.error, ridge.error, lasso.error, pcr.error)
error.df <- data.frame(lbls, mod.errs)
ggplot(error.df, aes(x=lbls, y=mod.errs)) +
  geom_bar(stat="identity")
which.min(mod.errs)

# (B) Of all the cross-validated models, the Best Subset Selection resulted in the lowest MSE and thus should be chosen to train a model on ALL the training data.  (See graphs)

# (C) 
coef(best.fit, id=best.p)
# All variables except for age ended up being used in the model.  This is because best subset selection checks what the best subset is of ALL sizes, and found that age of a neighborhood did not predict the crime rate well in the cross-validated models.
