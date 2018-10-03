##############################################################################################################################################################################################
## CH. 8 Lab

library(ISLR)
library(tree)
library(randomForest)
library(gbm)
library(ggplot2)
library(class)

## Lab 8.3.1 Classification Trees
names(Carseats)
attach(Carseats)

#For a classification tree the output must be categorical, so Sales is coded as a binary variable
High <- ifelse(Sales <= 8, "No", "Yes")
Carseats <- data.frame(Carseats, High)

#The tree created from the entire training set
tree.carseats <- tree(High~.-Sales, data=Carseats)
summary(tree.carseats)
plot(tree.carseats)
text(tree.carseats, pretty=0)

#The tree created from a training set, tested on a validation set.
set.seed(2)
train=sample(1:nrow(Carseats), 200)
Carseats.test <- Carseats[-train,]
High.test <- Carseats$High[-train]
tree.carseats <- tree(High~.-Sales, data=Carseats, subset=train)
tree.pred <- predict(tree.carseats, Carseats.test, type="class")
tbl <- table(tree.pred, High.test)
error.rate <- 1 - sum(diag(tbl))/sum(tbl)
error.rate

#Will pruning the tree lead to improved results?  It is possible that using all the variables is overfitting the data, and pruning can help reduce the overfitting.  A less complex tree with fewer splits can lead to lower variance.  Use cross-validation to determine the subtree with the lowest test RSS.
set.seed(3)
cv.carseats <- cv.tree(tree.carseats, FUN=prune.misclass)
names(cv.carseats)
cv.carseats
#$dev is the cross validation error rate.
par(mfrow=c(1, 2))
plot(cv.carseats$size, cv.carseats$dev, type="b")
plot(cv.carseats$k, cv.carseats$dev, type="b")
prune.carseats <- prune.misclass(tree.carseats, best=9)
# Alternatively, use
# prune.carseats <- prune.misclass(tree.carseats, k=1.75)
plot(prune.carseats)
text(prune.carseats, pretty=0)
tree.pred <- predict(prune.carseats, Carseats.test, type="class")
tbl <- table(tree.pred, High.test)
tbl
error.rate <- 1-sum(diag(tbl))/sum(tbl)
error.rate

# Lab 8.3.2 Regression Trees
library(MASS)
set.seed(1)
train <- sample(1:nrow(Boston), nrow(Boston)/2)
tree.Boston <- tree(medv~., data=Boston, subset=train)
summary(tree.Boston)
par(mfrow=c(1, 1))
plot(tree.Boston)
text(tree.Boston, pretty=0)

# Let's see if pruning will improve the performance by performing a Cross-validation to determine the alpha parameter
cv.Boston <- cv.tree(tree.Boston)
cv.Boston
plot(cv.Boston$size, cv.Boston$dev, type="b")
#We could prune the tree to a size of 7 for slight improvement
prune.Boston <- prune.tree(tree.Boston, best=7)
plot(prune.Boston)
text(prune.Boston, pretty=0)

#Make our predictions on the test set
yhat <- predict(tree.Boston, newdata=Boston[-train,])
Boston.test <- Boston[-train,"medv"]
plot(yhat, Boston.test)
abline(0, 1)
MSE <- mean((yhat - Boston.test)^2)
MSE
# Since the MSE is 25.05, the RMSE is ~5.  Thus, the prediction is within about $5000 of the true median home value.

## Lab 8.3.3 Bagging and Random Forests
set.seed(1)
names(Boston)
# How many predictors are there besides the predicted variable
length(names(Boston))-1
bag.Boston <- randomForest(medv~., data=Boston, subset=train, mtry=13, importance=TRUE)
bag.Boston
yhat.bag <- predict(bag.Boston, newdata=Boston[-train,])
plot(yhat.bag, Boston.test)
abline(0, 1)
bagMSE <- mean((yhat.bag - Boston.test)^2)
bagMSE

#Change the number of trees grown
bag.Boston <- randomForest(medv~., data=Boston, subset=train, mtry=13, ntree=25)
yhat.bag <- predict(bag.Boston, newdata=Boston[-train,])
plot(yhat.bag, Boston.test)
abline(0, 1)
bagMSE2 <- mean((yhat.bag - Boston.test)^2)
bagMSE2

#Change the number of predictors
set.seed(1)
rf.Boston <- randomForest(medv~., data=Boston, subset=train, mtry=6, importance=TRUE)
yhat.rf <- predict(rf.Boston, newdata=Boston[-train,])
plot(yhat.rf, Boston.test)
abline(0,1)
rfMSE <- mean((yhat.rf - Boston.test)^2)
rfMSE

importance(rf.Boston)
varImpPlot(rf.Boston)

# Lab 8.3.4 Boosting
library(gbm)

set.seed(1)
boost.Boston <- gbm(medv~., data=Boston[train,], distribution="gaussian", n.trees=5000, interaction.depth=4)
summary(boost.Boston)

par(mfrow=c(1, 2))
plot(boost.Boston, i="rm")
plot(boost.Boston, i="lstat")
yhat.boost <- predict(boost.Boston, newdata=Boston[-train,], n.trees=5000)
bstMSE <- mean((yhat.boost - Boston.test)^2)
bstMSE

#Change the shrinkage parameter lambda
boost.Boston <- gbm(medv~., data=Boston[train,], distribution="gaussian", n.trees=5000, interaction.depth=4, shrinkage=0.2, verbose=FALSE)
yhat.boost <- predict(boost.Boston, newdata=Boston[-train,], n.trees=5000)
bstMSE2 <- mean((yhat.boost - Boston.test)^2)
bstMSE2


######################################################################################################################################################################################
# CH. 8 Exercises

## Q1

set.seed(1)
data <- data.frame("x"=(rnorm(100) + 2.5), "y"=2*rnorm(100) + 5)

plt <- ggplot() + geom_segment(aes(x=0, y=0, xend=5, yend=0)) + geom_segment(aes(x=0, y=0, xend=0, yend=10)) + geom_segment(aes(x=0, y=10, xend=5, yend=10)) + geom_segment(aes(x=5, y=0, xend=5, yend=10)) + geom_segment(aes(x=0, y=5, xend=5, yend=5)) + geom_segment(aes(x=1, y=0, xend=1, yend=5)) + geom_segment(aes(x=1, y=2, xend=5, yend=2)) + geom_segment(aes(x=3, y=5, xend=3, yend=10)) + geom_segment(aes(x=3, y=7.5, xend=5, yend=7.5)) + geom_text(aes(label="R1", x=.5, y=2.5)) + geom_text(aes(label="R2", x=3, y=1)) + geom_text(aes(label="R3", x=3, y=3.5)) + geom_text(aes(label="R4", x=1.5, y=7.5)) + geom_text(aes(label="R5", x=4, y=6.25)) + geom_text(aes(label="R6", x=4, y=8.75)) + geom_text(aes(label="t1", x=-.1, y=5)) + geom_text(aes(label="t2", x=1, y=5.2)) + geom_text(aes(label="t3", x=.9, y=2)) + geom_text(aes(label="t4", x=3, y=10.2)) + geom_text(aes(label="t5", x=2.9, y=7.5))

plt + geom_point(data=data, aes(x, y, col="red"))

## Q2
## Boosting using depth-one trees leads to an additive model because the prediction is simply a sum of each of the previous models fit to the residuals, which is already additive.  

## Q3 Classification Error, Gini Index, and Cross-Entropy

pm <- seq(0, 1, .01)
E <- 1- pmax(pm, 1-pm)
G <- pm*(1-pm) * 2
D <- -(pm*log(pm) + (1-pm)*log(1-pm))
data <- data.frame(pm, E, G, D)
plt <- ggplot() + ggtitle("Classification Error, Gini Index, and Cross-Entropy") + geom_point(data=data, aes(pm, E, col="Error")) + geom_point(data=data, aes(pm, G, col="Gini")) + geom_point(data=data, aes(pm, D, col="Cross-Entropy"))
plt

## Q4
# (A) See sketch
# (B)
plt <- ggplot() + labs(title="Predictor Space", xlab="X1", ylab="X2") + geom_segment(aes(x=-1, y=0, xend=2, yend=0)) + geom_segment(aes(x=-1, y=0, xend=-1, yend=4)) + geom_segment(aes(x=-1, y=4, xend=2, yend=4)) + geom_segment(aes(x=, y=, xend=, yend=)) + geom_segment(aes(x=2, y=0, xend=2, yend=4)) + geom_segment(aes(x=-1, y=1, xend=2, yend=1)) + geom_segment(aes(x=1, y=0, xend=1, yend=1)) + geom_segment(aes(x=-1, y=2, xend=2, yend=2)) + geom_segment(aes(x=0, y=1, xend=0, yend=2)) + geom_text(aes(label="X2<1", x=-1.1, y=1)) + geom_text(aes(label="X1<1", x=1, y=-.1)) + geom_text(aes(label="X2<2", x=-1.1, y=2)) + geom_text(aes(label="X1<0", x=0, y=2.1)) + geom_text(aes(label="-1.80", x=0, y=.5)) + geom_text(aes(label="0.63", x=1.5, y=.5)) + geom_text(aes(label="2.49", x=.5, y=3)) + geom_text(aes(label="-1.06", x=-.5, y=1.5)) + geom_text(aes(label="0.21", x=1, y=1.5))
plt

## Q5
p <- c(0.1, 0.15, 0.2, 0.2, 0.55, 0.6, 0.6, 0.65, 0.7, 0.75)
# Assume that if P(Class is Red | X) > 0.5, we choose red.  Otherwise, choose green.
# Majority Vote
sum(p > 0.5)/10
# The majority votes that the class is red

# Average Probability
as.integer((mean(p)))
# The average probability decides the class is green.

## Q6
# To fit a regression tree with p predictors and n data points, the first step is to create a training set (or use cross-validation).
# Choose any of the p predictors.  The training data is split using this predictor iteratively.  All training data < a specified value, and all training data > a specified value.  The model is trained on this split, and an RSS is found.  This is repeated for all possible splits of that predictor on the training data.  The model with the lowest MSE is chosen.
# Repeat the previous step for ALL predictors, individually.  Of ALL the predictors, the model with the lowest MSE is chosen.  
# Then, using the remaining p-1 predictors, repeat previous steps until the RSS marginally changes, reaches a threshold, the terminal node contains fewer than some minimum # of observations, or some other stopping criteria is met.  


## Q7
# Examine test MSE for various values of mtry and ntree

library(reshape2)
numVars <- dim(Boston)[2] - 1
increment <- 25
ntrees <- seq(1, 501, increment)
rf.MSE <- matrix(NA, length(ntrees), numVars + 1)
rf.MSE[,1] <- ntrees
rf.MSE <- data.frame(rf.MSE)
cols <- c("mtry", seq(1:numVars))
names(rf.MSE) <- c("ntrees",seq(1:numVars))

for(i in ntrees){
  for(j in 1:numVars){
    rf.Boston <- randomForest(medv~., data=Boston, subset=train, mtry=j, ntree=i, importance=TRUE)
    yhat.rf <- predict(rf.Boston, newdata=Boston[-train,])
    rf.MSE[(i-1)/increment + 1, j + 1] <- mean((yhat.rf - Boston.test)^2)
  }
}

rf.MSE
plt <- ggplot() + labs(title="Random Forest MSE v. hyperparameters", xlab="Number of Trees", ylab="MSE")
rf.MSE.long <- melt(rf.MSE, id.vars=names(rf.MSE[1]))
names(rf.MSE.long) <- c("ntrees", "mtry", "MSE")
plt + geom_line(data=rf.MSE.long, aes(ntrees, MSE, color=mtry))

## From our graph it is apparent that most of the Random Forests flatten out beyond about 25 trees, with the optimal number of variables in the 5-8 range.  

## Q8
# (A)
names(Carseats)
nrows <- dim(Carseats)[1]
set.seed(1)
train <- sample(1:nrows, nrows/2)
# (B)
tree.carseats <- tree(Sales~.-High, data=Carseats, subset=train)
plot(tree.carseats)
text(tree.carseats, pretty=0)
## The predominant factor in greedily predicting the carseat sales is whether or not the carseat is on a good shelving location v. a bad/medium location.  Additionally, the pricing of the carseat determines sales foremost, with various other factors having lesser effects.
pred.carseats <- predict(tree.carseats, newdata=Carseats[-train,])
carseats.MSE <- mean((pred.carseats - Carseats$Sales[-train])^2)
carseats.MSE
## The MSE for this regression tree is 4.15.
# (C)
cv.carseats <- cv.tree(tree.carseats, FUN=prune.tree)
plot(cv.carseats$size, cv.carseats$dev)
min(cv.carseats$dev)
best <- cv.carseats$size[which.min(cv.carseats$dev)]
## The optimal tree size is one with 12 terminal nodes (leaves)
prune.carseats <- prune.tree(tree.carseats, best=best)
plot(prune.carseats)
text(prune.carseats, pretty=0)
pred.prune <- predict(prune.carseats, newdata=Carseats[-train,])
prune.MSE <- mean((pred.prune - Carseats$Sales[-train])^2)
prune.MSE
prune.MSE < carseats.MSE
## Pruning the tree did reduce the test MSE from 4.15 to 3.89.
# (D)
# The bagging approach is a randomForest choosing amongst ALL of the variables at each node rather than a subset.
bag.carseats <- randomForest(Sales~.-High, data=Carseats, subset=train, mtry=10, importance=TRUE)
bag.carseats.pred <- predict(bag.carseats, newdata=Carseats[-train,])
bag.MSE <- mean((bag.carseats.pred - Carseats$Sales[-train])^2)
bag.MSE
# The bagged  MSE is 2.70, noticeably lower than the pruned (or unpruned) tree.  
importance(bag.carseats)
varImpPlot(bag.carseats)
# Similarly, the importance plot shows that the price and shelving location are the most important predictors of # of carseat sales.  
# (E)
rf.carseats <- randomForest(Sales~.-High, data=Carseats, subset=train, mtry=as.integer(sqrt(10)))
rf.carseats
rf.carseats.pred <- predict(rf.carseats, newdata=Carseats[-train,])
rf.MSE <- mean((rf.carseats.pred - Carseats$Sales[-train])^2)
rf.MSE
# The Random Forest MSE is not as low as the bagged MSE.
importance(rf.carseats)
varImpPlot(rf.carseats)
# It shows similar importance to the bagged model, with price and shelving location the most important predictors of price.  

rf.MSE.all <- rep(NA, 10)
for(i in 1:10){
  rf.carseats <- randomForest(Sales~.-High, data=Carseats, subset=train, mtry=i)
  rf.carseats.pred <- predict(rf.carseats, newdata=Carseats[-train,])
  rf.MSE.all[i] <- mean((rf.carseats.pred - Carseats$Sales[-train])^2)
}
plot(rf.MSE.all)
min(rf.MSE.all)
which.min(rf.MSE.all)
# A random Forest choosing amongst 7 predictors at each split performs marginally better than a bagged model at predicting the test MSE of carseat sales.  The test MSE flattens out when over half of the predictors are used at each split, overall decreasing with more variables used.  


## Q9
# (A)
set.seed(1)
names(OJ)
nrows <- dim(OJ)[1]
train <- sample(1:nrows, 800)
OJ.train <- OJ[train,]
OJ.test <- OJ[-train,]
Purchase.test <- OJ$Purchase[-train]
# (B)
OJ.tree <- tree(Purchase~., data=OJ.train)
OJ.tree
summary(OJ.tree)
plot(OJ.tree)
text(OJ.tree, pretty=0)
# The tree only uses 4 variables in construction: LoyalCH, SpecialCH, PriceDiff, ListPriceDiff.  This suggests that the most important factors in which brand a customer purchases is which one they are loyal to, if there is a special on CH, and if CH costs less than MM (or is listed as such).
# There are 8 terminal nodes, split evenly between MM and CH.  The misclassification error rate is 16.5%.  
# (C)
OJ.tree
# Node 10: This node is for the case when a customer has a brand loyalty less than 0.51 but greater than 0.26 to CH, and there is a price difference less than 0.195.  In this case, 83 observations fall into that category, the deviance is 248.8, the customer is predicted to purchase MM, and 41% would purchase CH while 59% would purchase MM.  
# (D)
plot(OJ.tree)
text(OJ.tree, pretty=0)
#See part (B).
# (E)
OJ.pred <- predict(OJ.tree, newdata=OJ.test, type="class")
tbl <- table(OJ.pred, Purchase.test)
tbl
error.rate <- 1 - sum(diag(tbl)) / sum(tbl)
error.rate
# The test error rate is 22.6%, slightly higher than the training error rate.  This is to be expected.  
# (F)
OJ.cv <- cv.tree(OJ.tree, FUN=prune.misclass)
OJ.cv
plot(OJ.cv$size, OJ.cv$dev, type="b")
# The smallest misclassification rate is when the number of terminal nodes is 2, 5, or 8.  Since 2 terminal nodes is a simpler model, the 2 model should be used instead.  
# (G)
# See (F)
# (H)
# See (F)
# (I)
min <- min(OJ.cv$dev)
sub.best <- which(OJ.cv$dev == min)
best <- min(OJ.cv$size[sub.best])
OJ.prune <- prune.misclass(OJ.tree, best=6)
plot(OJ.prune)
text(OJ.prune, pretty=0)
summary(OJ.prune)
# The misclassification rate is 16.5%.  The model with 8 terminal nodes is marginally more accurate.  
OJ.prune.pred <- predict(OJ.prune, newdata=OJ[-train,], type="class")
tbl.prune <- table(OJ.prune.pred, Purchase.test)
tbl.prune
prune.error.rate <- 1 - sum(diag(tbl.prune)) / sum(tbl.prune)
prune.error.rate
#The test error rate for the pruned table is 22.5%, which is exactly the same as the 8 terminal node model.  The simply model with 2 terminal nodes should be used. 

## Q10
# (A)
set.seed(1)
names(Hitters)
Hitters <- na.omit(Hitters)
Hitters$logSalary <- log(Hitters$Salary)
# (B)
train <- 1:200
logSalary.test <- Hitters$logSalary[-train]
# (C)
library(gbm)

shrinks <- seq(-4, -.2, by=0.1)
boost.MSE <- data.frame(matrix(NA, length(shrinks), 3))
names(boost.MSE) <- c("Shrinkage", "trainMSE", "testMSE")
boost.MSE$Shrinkage <- 10^shrinks

for(i in 1:length(shrinks)){
  boost.Hitters <- gbm(logSalary~.-Salary, data=Hitters[train,], distribution="gaussian", n.trees=1000, interaction.depth=4, shrinkage=boost.MSE$Shrinkage[i], verbose=FALSE)
  pred <- predict(boost.Hitters, newdata=Hitters[train,], n.trees=1000)
  boost.MSE$trainMSE[i] <- mean((pred - Hitters$logSalary[train])^2)
  pred <- predict(boost.Hitters, newdata=Hitters[-train,], n.trees=1000)
  boost.MSE$testMSE[i] <- mean((pred - Hitters$logSalary[-train])^2)
}

par(mfrow=c(1, 2))
plot(boost.MSE$Shrinkage, boost.MSE$trainMSE, xlab="Shrinkage factor", ylab="Training MSE", main="Shrinkage Factor v. Training MSE")
min(boost.MSE$trainMSE)
which.min(boost.MSE$trainMSE)

plot(boost.MSE$Shrinkage, boost.MSE$testMSE, xlab="Shrinkage factor", ylab="Testing MSE", main="Shrinkage Factor v. Testing MSE")
min(boost.MSE$testMSE)
which.min(boost.MSE$testMSE)
boost.MSE$Shrinkage[which.min(boost.MSE$testMSE)]

# (D)     
# See part (C)
# The shrinkage factor causes the training MSE to monotonically decrease, but the test MSE decreases rapidly then begins increasing again.  

# (E)

# A linear regression
library(glmnet)
lm.fit <- lm(logSalary~.-Salary, data=Hitters[train,])
pred <- predict(lm.fit, Hitters[-train,])
lm.MSE <- mean((pred - Hitters$logSalary[-train])^2)
lm.MSE

# A LASSO model
set.seed(1)
x <- model.matrix(logSalary~.-Salary, data=Hitters[train,])
y <- Hitters$logSalary[train]
x.test <- model.matrix(logSalary~.-Salary, data=Hitters[-train,])
lasso.fit <- glmnet(x, y, alpha=1)
pred <- predict(lasso.fit, s=0.01, newx=x.test)
lasso.MSE <- mean((pred - Hitters$logSalary[-train])^2)
lasso.MSE

# The test MSE for the boost method is as low as 0.26 for a lambda of 0.016, while the linear and LASSO models are 0.49 and 0.47, respectively, much larger.  

# (F)

boost.Hitters <- gbm(logSalary~.-Salary, data=Hitters[train,], distribution="gaussian", n.trees=1000, interaction.depth=4, shrinkage=0.016, verbose=FALSE)
summary(boost.Hitters)
# The most important variables are CAtBat and a far second, CWalks/CRBI.

# (G) 
library(randomForest)
numVars <- dim(Hitters) - 2
bag.Hitters <- randomForest(logSalary~.-Salary, data=Hitters, subset=train, mtry=sqrt(numVars))
pred <- predict(bag.Hitters, newdata=Hitters[-train,])
rf.MSE <- mean((pred - Hitters$logSalary[-train])^2)
rf.MSE
# The random forest performs even better than the boosted model with an MSE of 0.21.

## Q11
# (A)
names(Caravan)
# (A)
Caravan$Purchase <- ifelse(Caravan$Purchase == "Yes", 1, 0)

train <- 1:1000
Caravan.train <- Caravan[train,]
Caravan.test <- Caravan[-train,]
Purchase.test <- Caravan$Purchase[-train]
# (B)
n.trees <- 1000
lambda <- 0.01
boost.caravan <- gbm(Purchase~., data=Caravan.train, n.trees=n.trees, shrinkage=lambda, distribution="bernoulli")
boost.caravan
summary(boost.caravan)
# The most important predictors are PPERSAUT, MKOOPKLA, and MOPLHOOG as the top 3 (respectively) and a number of less important predictors following.
# (C)
predProb <- predict(boost.caravan, newdata=Caravan.test, n.trees=n.trees, type="response")
pred <- ifelse(predProb > 0.2, 1, 0)
# If someone has over a 20% chance of making a purchase, predict that they will do so

tbl <- table(pred, Purchase.test)
tbl
true.pos.rate <- tbl[4] / (tbl[2] + tbl[4])
true.pos.rate
# Given that someone is predicted to make a purchase, there is a 20% chance that they actually will.

## Using KNN
library(class)
Caravan.scraps <- Caravan[,-86]
Caravan.train <- Caravan.scraps[train,]
Caravan.test <- Caravan.scraps[-train,]

k <- 12
true.rates <- rep(NA, k)

knn.caravan <- knn(Caravan.train, Caravan.test, Caravan$Purchase[train], k=3)
summary(knn.caravan)
tbl.knn <- table(knn.caravan, Purchase.test)
tbl.knn
true.pos.rate <- tbl.knn[4] / (tbl.knn[2] + tbl.knn[4])
true.pos.rate

for(i in 1:k){
  knn.caravan <- knn(Caravan.train, Caravan.test, Caravan$Purchase[train], k=i)
  summary(knn.caravan)
  tbl.knn <- table(knn.caravan, Purchase.test)
  true.pos.rate <- tbl.knn[4] / (tbl.knn[2] + tbl.knn[4])
  true.rates[i] <- true.pos.rate
  print(i)
}
true.rates
plot(true.rates)
max(true.rates[!is.na(true.rates)])
which.max(true.rates)
# The true positive rate for the KNN classifier depends on the chosen k.  For k=12 the true positive rate was 33%, notably better than boosting.  

## Q12
## We will predict a cars mpg using various predictors and qualities of the car.
library(MASS)
library(randomForest)
library(reshape2)
library(gbm)
names(mtcars)
numObs <- dim(mtcars)[1]
numVars <- dim(mtcars)[2]
set.seed(1)
train <- sample(1:numObs, numObs/2)
cars.train <- mtcars[train,]
cars.test <- mtcars[-train,]
mpg.test <- mtcars$mpg[-train]

# The Bagged model
ntrees <- seq(1, 2001, by=50)
bag.MSE <- data.frame(ntrees,rep(NA, length(ntrees))) 
names(bag.MSE) <- c("ntrees","MSE")
for(i in 1:length(ntrees)){
  bag.cars <- randomForest(mpg~., data=cars.train, mtry=numVars-1, importance=TRUE, ntrees=ntrees[i])
  bag.pred <- predict(bag.cars, newdata=cars.test)
  bag.MSE[i, 2] <- mean((bag.pred - mpg.test)^2)
}
bag.MSE
plot(bag.MSE$ntrees, bag.MSE$MSE)
bag.fin.MSE <- min(bag.MSE$MSE)
best <- bag.MSE$ntrees[which.min(bag.MSE$MSE)]
best
# The training MSE takes on a minimum when the number of trees used is 401.  This will be used on the entire dataset, so that the model will be trained on the whole dataset.  
bag.cars <- randomForest(mpg~., data=mtcars, mtry=numVars-1, importance=TRUE, ntrees=best)
bag.pred <- predict(bag.cars, newdata=mtcars)
#bag.fin.MSE <- mean((bag.pred - mtcars$mpg)^2)
#bag.fin.MSE
importance(bag.cars)
varImpPlot(bag.cars)


# Random Forest model
rf.MSE <- matrix(NA, length(ntrees), numVars)
rf.MSE <- data.frame(rf.MSE)
rf.MSE[,1] <- ntrees
names(rf.MSE) <- c("ntrees", 1:(numVars-1))
for(i in 1:(numVars-1)){
  for(j in 1:length(ntrees)){
    rf.cars <- randomForest(mpg~., data=cars.train, mtry=i, importance=TRUE, ntrees=ntrees[j])
    rf.pred <- predict(rf.cars, newdata=cars.test)
    rf.MSE[j, i + 1] <- mean((rf.pred - mpg.test)^2)
  }
}
rf.MSE
rf.MSE.long <- melt(rf.MSE, id.vars="ntrees")
names(rf.MSE.long) <- c("ntrees", "mtry", "MSE")
plt <- ggplot() + geom_line(data=rf.MSE.long, aes(x=ntrees, y=MSE,col=mtry))
plt
rf.fin.MSE <- min(rf.MSE.long$MSE)
best.ntrees <- rf.MSE.long$ntrees[which.min(rf.MSE.long$MSE)]
best.mtry <- as.numeric(rf.MSE.long$mtry[which.min(rf.MSE.long$MSE)])
best.ntrees
best.mtry
# The minimum MSE occurred for a the number of trees = 1101 and mtry=6.  These will be used to retrain the entire data set. 

rf.cars <- randomForest(mpg~., data=mtcars, mtry=best.mtry, ntrees=best.ntrees, importance=TRUE)
rf.pred <- predict(rf.cars, newdata=mtcars)
#rf.fin.MSE <- mean((rf.pred - mtcars$mpg)^2)
#rf.fin.MSE


## Fit a boosting model to the mtcars mpg data...Hmm, it seems that the dataset is too small for gbm to properly apply trees.  We increase the bag.fraction and train on the entire dataset.

lambda <- seq(-4, -2, 0.01)
boost.MSE <- matrix(NA, length(lambda), 2)
names(boost.MSE) <- c("lambda","MSE")
boost.MSE[,1] <- 10^lambda

# 
# for(i in 1:length(lambda)){
#   boost.cars <-gbm(mpg~., data=cars.train, distribution="gaussian", n.trees = 5000, interaction.depth=1, shrinkage=boost.MSE[i, 1], verbose=FALSE, bag.fraction=1)
#   boost.pred <- predict(boost.cars, newdata=cars.train, n.trees=5000)
#   boost.MSE[i, 2] <- mean((boost.pred - mpg.test)^2)
# }

boost.cars <- gbm(mpg~., data=mtcars, distribution="gaussian", n.trees=5000, interaction.depth=1, shrinkage=0.001, verbose=FALSE, bag.fraction=1)
boost.pred <- predict(boost.cars, newdata=mtcars, n.trees=5000)
boost.fin.MSE <- mean((boost.pred - mtcars$mpg)^2)
boost.fin.MSE


## finally, let's apply linear regression to predict mpg.  
lm.cars <- lm(mpg~., data=cars.train)
lm.pred <- predict(lm.cars, newdata=cars.test)
lm.MSE <- mean((lm.pred - mpg.test)^2)
lm.MSE

MSEname <- c("Bagged", "Random Forest", "Boost", "LinReg")
MSE <- c(bag.fin.MSE, rf.fin.MSE, boost.fin.MSE, lm.MSE)
data.frame(MSEname, MSE)

## As we can see, the linear regression model performed worst in MSE. This likely because of many nonlinear variables meaning that a linear model is not a good fit.  The random forest model performed better than the bagged model, to be expected since there are more hyperparameters to tune and the bagged model is a subset of random forest models.  The Boost model performed "best", though there was no test set so the MSE must be taken skeptically.  Overall, the random forest model will be chosen as the best model, and the final model trained on this data.

best.cars <- randomForest(mpg~., data=mtcars, mtry=best.mtry, ntrees=best.ntrees, importance=TRUE)
