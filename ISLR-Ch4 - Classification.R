######################################################################################################################################################################################
## Chapter 4 Labs

library(ISLR)
library(corrplot)
library(MASS)
library(class)

names(Default)
attach(Default)
plot(balance, income)

## Check how many defaults there are in the dataset
no.default <- Default[Default[,"default"] == "No",]
dim(no.default)
yes.default <- Default[Default[,"default"] == "Yes",]
dim(yes.default)

## Visualize the two groups separately: defaulters and non-defaulters
plot(yes.default[,"balance"], yes.default[,"income"], col="red")
points(no.default[,"balance"], no.default[,"income"], col="blue")

## Recode the categorical variables to 0's and 1's in order to perform a preliminary linear regression
Default$DefNum <- as.integer(Default$default == "No")
Default$DefNum <- as.integer(Default$default == "Yes")
lm.fit <- lm(DefNum ~ balance)
summary(lm.fit)
plot(balance, DefNum)
abline(lm.fit, col="red")
detach(Default)

###C. 4 Lab
names(Smarket)
attach(Smarket)
dim(Smarket)
summary(Smarket)
correlations <- cor(Smarket[,-9])
corrplot(correlations, type="lower")
lm.fit <- lm(Volume ~ Year, data=Smarket)

plot(Year, Volume)
abline(lm.fit)
hist(Volume)


## Logistic Regression

# Creates a generalized linear model which can include logistic regression (family=binomial)
glm.fit <- glm(Direction ~ .-Year-Direction-Today, data=Smarket, family=binomial)
summary(glm.fit)
coef(glm.fit)
summary(glm.fit)$coef
# None of the p-values are significant
p <- summary(glm.fit)$coef[,4]
# Makes predictions using the glm, outputs in the form of probabilities
glm.probs <- predict(glm.fit, type="response", data=Smarket)
?predict
head(glm.probs)
max(glm.probs)
min(glm.probs)
contrasts(Direction)
# Create the prediction of whether the market will go up or down
glm.pred <- rep("Down", 1250)
glm.pred[glm.probs > .50] = "Up"
head(glm.pred)
# Creates a confusion matrix of the predictions v. the reality of Up/Down
table(glm.pred, Direction)
true.pos <- 507 / (141 + 507)
true.neg <- 145 / (457 + 145)
false.pos <- 457 / (457 + 145)
false.neg <- 141 / (141 + 507)

rates <- c(true.pos, true.neg, false.pos, false.neg)
rates
mean(glm.pred == Direction)
correct.pct <- 507/(507 + 145 + 457 + 141) + 145/(507 + 145 + 457 + 141)
correct.pct

## Now we will create a logistic model by using a training and a testing set.
## To do this, we will predict the stock market for 2005 by using data from 2001-2004
train = (Year<2005)
Smarket.2005 <- Smarket[!train,]
dim(Smarket.2005)
Direction.2005 <- Direction[!train]


## Create the model based on training data and apply it to testing data
glm.fit <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, data=Smarket, family=binomial, subset=train)
glm.probs <- predict(glm.fit, Smarket.2005, type="response")
glm.pred <- rep("Down", 252)
glm.pred[glm.probs > 0.50] = "Up"
table(glm.pred, Direction.2005)
(77 + 44)/ (77 + 44 + 34 + 97)
mean(glm.pred == Direction.2005)


## This still results in a very poor success rate, worse than random guessing.  Let's return to the original variables and use only the most significant Lag variables to attempt to fit a model.
glm.fit <- glm(Direction ~ Lag1 + Lag2, data=Smarket, family=binomial, subset=train)
glm.probs <- predict(glm.fit, Smarket.2005, type="response")
glm.pred <- rep("Down", 252)
glm.pred[glm.probs > 0.50] = "Up"
table(glm.pred, Direction.2005)
mean(glm.pred == Direction.2005)
106 / (106+76)

##This shows that the model is correct 56% of the time, but it when it predicts the markey is increasing it is correct 58% of the time

## We can also predict ONLY on particular conditions, such as when Lag1 and Lag2 equal certain values.
predict(glm.fit, newdata=data.frame(Lag1=c(1.2, 1.5), Lag2=c(1.1, -0.8)), type="response")


### 4.6.3 Linear Discriminant Analysis
## LDA is in the MASS library and has the same arguments as lm (or glm)
lda.fit <- lda(Direction ~ Lag1 + Lag2, data=Smarket, subset=train)
lda.fit
plot(lda.fit)

# The prior probabilities tell us that: 49.2% of training observations correspond to days when the market went down and 50.8% when the market went up.
# The Group means: when the market goes up, there is a slight negative return on the market the previous two days.  When the market goes down, there is a slight positive return on the market the previous two days.
# The coefficients of linear discriminants: when -.642*Lag1 + -.513*Lag2 is large, then the LDA classifier will predict a market increase.  If it is small, predict a decrease.

lda.pred <- predict(lda.fit, Smarket.2005)
names(lda.pred)
# lda.pred$class contains the predictions for whether the market will move up or down
lda.class <- lda.pred$class
# These predictions are very similar to the logistic regression output
table(lda.class, Direction.2005)
mean(lda.class == Direction.2005)
# A 50% threshold on the posterior probabilities recreates the predictions contained in lda.pred$class.  These outputs are the total # of days predicted to go down/up.
sum(lda.pred$posterior[,1] >= 0.5) # predicts down
sum(lda.pred$posterior[,1] < 0.5)  # predicts up
lda.pred$posterior[1:20, 1]
lda.class[1:20]

sum(lda.pred$posterior[,1] > 0.9)

### 4.6.4 Quadractice Discriminant Analysis
## qda is found in the MASS library

qda.fit <- qda(Direction ~ Lag1 + Lag2, data=Smarket, subset=train)
qda.fit

qda.class <- predict(qda.fit, Smarket.2005)$class
table(qda.class, Direction.2005)
mean(qda.class == Direction.2005)


### KNN
## knn() is part of the class library and works very different from the other models.

train.X <- cbind(Lag1, Lag2)[train,]
test.X <- cbind(Lag1, Lag2)[!train,]
train.Direction <- Direction[train]

set.seed(1)
knn.pred <- knn(train.X, test.X, train.Direction, k=1)
tbl <- table(knn.pred, Direction.2005)
correct.pct <- sum(diag(tbl)) / sum(tbl) 
correct.pct

knn.pred <- knn(train.X, test.X, train.Direction, k=3)
tbl <- table(knn.pred, Direction.2005)
tbl
correct.pct <- sum(diag(tbl)) / sum(tbl) 
correct.pct

correct.vec <- c(rep(0, 10))
k.vec <- c(seq(1:10))
knn.results <- data.frame(k.vec, correct.vec)
colnames(knn.results) <- c("k=", "Correct %")

for (x in seq(1:10)){
  knn.pred <- knn(train.X, test.X, train.Direction, k=x)
  tbl <- table(knn.pred, Direction.2005)
  correct.pct <- sum(diag(tbl)) / sum(tbl) 
  knn.results[x, 2] <- round(correct.pct, 3)
}

knn.results
max(knn.results[,2])
which.max(knn.results[,2])

detach(Smarket)

### 4.6.6 Application to Caravan Insurance
## We will be using the K-Nearest Neighbors classification on the Caravan insurance database to classify customers as either Purchasing or Not Purchasing insurance, based on 85 variables for 5822 individuals.
fix(Caravan)
?Caravan
dim(Caravan)
attach(Caravan)
summary(Purchase)
348/sum(summary(Purchase))

# Standardize the data in order to ensure proper classification based on relative distance
# leave out the Purchase variable
standardized.X <- scale(Caravan[, -86])
var(Caravan[,1])
var(Caravan[,2])
var(standardized.X[,1])
var(standardized.X[,2])

# Split the observations in to the training and test sets.  The test set will be the first 1000 observations.
test <- 1:1000
train.X <- standardized.X[-test, ]
test.X <- standardized.X[test, ]
train.Y <- Purchase[-test]
test.Y <- Purchase[test]
set.seed(1)
knn.pred <- knn(train.X, test.X, train.Y, k=1)

mean(test.Y != knn.pred)
mean(test.Y != "No")
## We find an error rate of 11.8%.  Since only 6% of customers actually purchased insurance, this error rate could be reduced by predicting "NO" for ALL individuals, reducing the error rate to 6%.  This is not a good model.

## However, we are more interested in the correct prediction rate of "Yes".  
tbl <- table(knn.pred, test.Y)
tbl
9/(68+9)
tbl[4]/(tbl[2] + tbl[4])
## This is found to be 11.7%, which is much better than 6% from random guessing.
## Let's see if larger k's will result in any better results.

knn.results <- data.frame(k = seq(1:10), Correct.Yes = rep(0, 10))
knn.results

for (x in seq(1:10)){
  set.seed(1)
  knn.pred <- knn(train.X, test.X, train.Y, k=x)
  tbl <- table(knn.pred, test.Y)
  print("k=")
  print(x)
  print(tbl)
  knn.results[x, 2] <- tbl[4]/(tbl[2] + tbl[4])
  }

knn.results
## Based on the output, it's apparent that larger k's seem to be resulting in better predictions.  The percentage correct of those who said yes increases as high as 30%, over 5X better than random guessing.  Let's take a closer look at the confusion tables for these k's.

## We can see that while our percentage correct of Yes's increases, the magnitude of Yes's decreases and is nearly halved.  In fact, why don't we use k = 9 or higher?  Of all the people predicted to say Yes, it correctly identified them all!  However, it only identified 1 person as buying insurance. Not a useful model.

## Let's try a logistic model.
glm.fit <- glm(Purchase ~ ., data=Caravan, family=binomial, subset = -test)
glm.probs <- predict(glm.fit, Caravan[test, ], type="response")
glm.pred <- rep("No", 1000)
glm.pred[glm.probs > 0.5] = "Yes"
table(glm.pred, test.Y)
## Ok, so at a threshold of 0.5 this model doesn't do too well.  Let's try a different threshold.
glm.pred <- rep("No", 1000)
glm.pred[glm.probs > 0.25] = "Yes"
tbl <- table(glm.pred, test.Y)
tbl[4]/(tbl[2] + tbl[4])
## Changing the threshold to 0.25 had improved results significantly, even better than KNN.  We are correctly predicting 33% of predicted Yes's.
detach(Caravan)

######################################################################################################################################################################################
###ISLR  CH. 4 Exercises

## Q1
# Proof is trivial

## Q2
# See proof on paper.  Use properties of logs and the fact that the "l" terms will be the same for all values of k

## Q3 
# See proof on paper.  The 1/(sigma)^2 cannot be factored out of each of the x^2 terms and thus they do not end up cancelling out.

## Q4
# (A) Since the observations are uniformly distributed, 10%.
# (B) Since they are independent, 10% of 10% is 1%.
# (C) Following our pattern, 10% of 10% of....is .1^100
# (D) As the number of features p grows linearly, the number of training observations shrinks exponentially
# (E) .1 = x^p

## Q5
# (A) If Bayes decision buoundary is linear we would expect QDA to perform better on the training set and LDA to perform better on the test set.  This is because QDA has more more parameters to tune so it is bound to fit the training set better, but may result in overfitting.
# (B) If Bayes decision boundary is non-linear, we would expect QDA to perform better on the training set AND the test set.  LDA is not a good fit for nonlinear data and will almost certainly perform poorer on the test set because of it.
# (C) As sample size n increases, QDA is expected to improve relative to LDA because of the lowering of variance in the variance/bias tradeoff, whereas if p predictors increases LDA is expected to improve over QDA.
# (D) False, The /test/ data may be overfitted by using QDA and result in a higher test error rate.

## Q6 
# (A)

getProb <- function(x1, x2) {
  b0 <- -6
  b1 <- 0.05
  b2 <- 1
  px <- (exp(b0 + b1*x1 + b2*x2)) / (1 + exp(b0 + b1*x1 + b2*x2)) 
  px
}

getProb(40, 3.5)
# The student has approximately a 37.7% chance to get an A in the class.   


# (B)

getHours <- function(x2, px){
  b0 <- -6
  b1 <- 0.05
  b2 <- 1
  x1 <- (log(px/(1-px)) - b0 - b2*x2) / b1
  x1
}
 getHours(3.5, 0.50)
# The student needs to study approximately 50 hours.  
 
## Q7
#  Given: E(X | "Yes") = mu1 = 10
#         E(X | "No")  = mu2  = 0
#         sd^2 = 36
#         P("Yes") = 0.80  so  P("No") = 0.20
# Using the Bayes' Theorem formula (4.10) and plugging in our values, we can solve and simplify for our probability:   75%.  There is a 75% chance that a company will issue a dividend this year if its percent profit was 4 last year.  
 (.8)*exp(-(4-10)^2/(2*36))/((.8*exp(-(4-10)^2/(2*36))) + .2*exp(-(4-0)^2/(2*36)))



## Q8
#  Given that the training and test sets are equal sizes, we may still want to use logistic regression.  This will depend on what the training data error rate is for the KNN classification.  Theoretically, the error rate could be as low as 0%, which means that in order to have an average error rate of 18% means there was a 36% error rate in the KNN test set, higher than logistic regression.  If the training error rate for knn is greater than 6%, we would ultimately use KNN as it would have a test error rate lower than 30%.
 
## Q9
# (A) The odds are defined as:   odds = P(A) / P(~A)  = P(A) / 1-P(A)
#  Thus, solving this for P(A):
#  P(A) = odds / (1 + odds)
0.37 / (1 + 0.37)
#  So the fraction of people who will in fact default is 27 out of 100.

# (B) If someone has a 16% chance of defaulting, P(A) = 0.16
#  Thus, the odds they will default are: 0.19
0.16 / 0.84


## Q10
fix(Weekly)
names(Weekly)
attach(Weekly)
unique(Year)

#(A)
correlations <- cor(Weekly[,-9])
corrplot(correlations, type = "lower")
plot(Year, Volume, main="Volume v. Year")
hist(Lag1)
abline(v = mean(Lag1))
hist(Lag2)
abline(v = mean(Lag2))
hist(Lag3)
abline(v = mean(Lag3))
hist(Lag4)
abline(v = mean(Lag4))
hist(Lag5)
abline(v = mean(Lag5))

# Based on out correlogram there is only 1 strong relationship between the variables Volume and Year, implying that more shares are traded every year.  All of the Lag variables are similarly distributed, being roughly symmetric and unimodal with a mean near 0.  

# (B)
glm.fit <- glm(Direction ~ . -Year-Today, data=Weekly, family=binomial)
summary(glm.fit)
# Only one of the variables, Lag2, is found to be statistically significant, suggesting that there is a slight relationship between the market 2 days ago and Today's direction.

# (C)
# First use the model to make probability predictions
glm.probs <- predict(glm.fit, data=Weekly, type="response")
# Now use these probabilities to predict "Up" or "Down" with a 50% threshold
length(glm.probs)
glm.pred <- rep("Down", 1089)
glm.pred[glm.probs > 0.5] = "Up"

tbl <- table(glm.pred, Direction)
tbl
total.correct <- sum(diag(tbl)) / sum(tbl)
total.correct
# The total percent of correct predictions is 56.1%.  Of the times the market was predicted to go up, it actually went up 56.4% of the time.  Of the times the market was predicted to go down, it actually went down 52.9% of the time.  Overall, both appear to be slightly better than random guessing.
tbl[4] / (tbl[4] + tbl[2])
tbl[1] / (tbl[1] + tbl[3])

# (D) using Logistic Regression
train <- (Year < 2009)
Weekly.train <- Weekly[train, ]
Weekly.test <- Weekly[!train, ]
Direction.test <- Direction[!train]
dim(Weekly.train)
dim(Weekly.test)

glm.fit <- glm(Direction ~ Lag2, data=Weekly, subset=train, family=binomial)
summary(glm.fit)
# Now we find that Lag2 is not statistically significant when fitted to the training data alone.  
glm.probs <- predict(glm.fit, Weekly.test, type="response")
glm.pred <- rep("Down", length(glm.probs))
glm.pred[glm.probs > 0.5] = "Up"
length(glm.probs)
tbl <- table(glm.pred, Direction.test)
tbl
correct.pct <- sum(diag(tbl)) / sum(tbl)
correct.pct
# Using only Lag2 as a predictor and splitting the data into training and test sets, we find a correct prediction rate of 62.5% on the test set.  Of those days predicted to go "Up", the model was correct 62.2% of the time.  Of the days the market was predicted to go "Down", the model was correct 64.3% of the time.
tbl[4] / (tbl[4] + tbl[2])
tbl[1] / (tbl[1] + tbl[3])

# (E)  Using LDA
lda.fit <- lda(Direction ~ Lag2, data=Weekly, subset=train)
plot(lda.fit)

lda.pred <- predict(lda.fit, Weekly.test)
lda.class <- lda.pred$class
mean(lda.class == Direction.test)

tbl <- table(lda.class, Direction.test)
tbl
correct.pct <- sum(diag(tbl)) / sum(tbl)
correct.pct
tbl[4] / (tbl[4] + tbl[2])
tbl[1] / (tbl[1] + tbl[3])
# We find an overall correct percentage of 62.5%, a correct "UP" percentage of 62.2%, and a correct "Down" percentage of 64.2%.  These are the same as logistic regression.

# (F)  using qda
qda.fit <- qda(Direction ~ Lag2, data=Weekly, subset=train)

qda.pred <- predict(qda.fit, Weekly.test)
qda.class <- qda.pred$class
mean(qda.class == Direction.test)

tbl <- table(qda.class, Direction.test)
tbl
correct.pct <- sum(diag(tbl)) / sum(tbl)
correct.pct
tbl[4] / (tbl[4] + tbl[2])
tbl[1] / (tbl[1] + tbl[3])
# QDA finds an overall correct percentage of 58.6%, but achieves this by predicting every day the market will go "Up".  As such, it is entirely useless.  

# (G)   using KNN k=1
# ***NOTE*** MUST TURN train.X and test.X into dataframes in order to read it into knn()
train.X <- data.frame(Lag2[train])
test.X <- data.frame(Lag2[!train])
train.Direction <- Direction[train]
length(train.X)
length(test.X)

set.seed(1)
knn.pred <- knn(train.X, test.X, train.Direction, k=1)

tbl <- table(knn.pred, Direction.test)
correct.pct <- sum(diag(tbl)) / sum(tbl) 
correct.pct

knn.pred <- knn(train.X, test.X, train.Direction, k=3)
tbl <- table(knn.pred, Direction.test)
tbl
correct.pct <- sum(diag(tbl)) / sum(tbl) 
correct.pct

# (H)  Of these methods, the Logistic Regression and LDA perform better than QDA and knn (k=1), and equal to each other.  

# (I)  
# We will start with trying different values of k for the knn() classification.  
maxK = 100
knn.results <- data.frame(k = seq(1:maxK), Correct.Yes = rep(0, maxK))

for (x in seq(1:maxK)){
  set.seed(1)
  knn.pred <- knn(train.X, test.X, train.Direction, k=x)
  tbl <- table(knn.pred, Direction.test)
  #print("k=")
  #print(x)
  #print(tbl)
  knn.results[x, 2] <- sum(diag(tbl)) / sum(tbl)
}

knn.results
max(knn.results[,2])
which.max(knn.results[,2])
min(knn.results[,2])
which.min(knn.results[,2])
# So if we test from using k = 1 to 100 NN, we find that at k=4 we achieve the best correct.pct: 61.5%.  Let's take a look at the confusion matrix.

set.seed(1)
knn.pred <- knn(train.X, test.X, train.Direction, k=4)
tbl <- table(knn.pred, Direction.test)
tbl
correct.pct <- sum(diag(tbl)) / sum(tbl)
correct.pct
tbl[4] / (tbl[4] + tbl[2])
tbl[1] / (tbl[1] + tbl[3])
plot(knn.results[,2], xlab="k=", ylab="Overall % Correct", main="Overall % Correct v. K")
# For whatever reason, when k=4 the knn model fits the test data very well, perhaps modelling some underlying structure well.
max(knn.results[1:10, 2])
which.max(knn.results[1:10, 2])
# Ok, let's try Logistic Regression with all variables

glm.fit <- glm(Direction ~ .-Year-Today, data=Weekly, subset=train, family=binomial)
summary(glm.fit)
glm.probs <- predict(glm.fit, Weekly.test, type="response")
glm.pred <- rep("Down", length(glm.probs))
glm.pred[glm.probs > 0.5] = "Up"
tbl <- table(glm.pred, Direction.test)
tbl
correct.pct <- sum(diag(tbl)) / sum(tbl)
correct.pct
tbl[4] / (tbl[4] + tbl[2])
tbl[1] / (tbl[1] + tbl[3])
# Using all Lag variables and Volume as predictors don't achieve any better test data predictions, at only 46.1% overall.  

# What if we include interaction terms, such as with Lag 1 and Lag 2
glm.fit <- glm(Direction ~ .-Year-Today+ Lag1:Lag2, data=Weekly, subset=train, family=binomial)
summary(glm.fit)
glm.probs <- predict(glm.fit, Weekly.test, type="response")
glm.pred <- rep("Down", length(glm.probs))
glm.pred[glm.probs > 0.5] = "Up"
tbl <- table(glm.pred, Direction.test)
tbl
correct.pct <- sum(diag(tbl)) / sum(tbl)
correct.pct
tbl[4] / (tbl[4] + tbl[2])
tbl[1] / (tbl[1] + tbl[3])

# This performs pretty poorly! Oy!

# Ok, time to try LDA with all variables

lda.fit <- lda(Direction ~ .-Year-Today, data=Weekly, subset=train)
plot(lda.fit)

lda.pred <- predict(lda.fit, Weekly.test)
lda.class <- lda.pred$class
mean(lda.class == Direction.test)

tbl <- table(lda.class, Direction.test)
tbl
correct.pct <- sum(diag(tbl)) / sum(tbl)
correct.pct
tbl[4] / (tbl[4] + tbl[2])
tbl[1] / (tbl[1] + tbl[3])
# Ugh, this performs worse than with only Lag2.

# Ok, time to switch to trying all the variables in a QDA model.

qda.fit <- qda(Direction ~ .-Year-Today, data=Weekly, subset=train)

qda.pred <- predict(qda.fit, Weekly.test)
qda.class <- qda.pred$class
mean(qda.class == Direction.test)

tbl <- table(qda.class, Direction.test)
tbl
correct.pct <- sum(diag(tbl)) / sum(tbl)
correct.pct
tbl[4] / (tbl[4] + tbl[2])
tbl[1] / (tbl[1] + tbl[3])

# This doesn't perform noticeably better than lda or qda with all variables.

## For a simplified model the Logistic and LDA models performed best with only a Lag2 predictor, but applying KNN with k=41 results in the best test fit at 60% overall correctness.
detach(Weekly)

## Q11
# (A) Auto
attach(Auto)
# Creates a variable that contains 1 if mpg > median(mpg) and 0 otherwise.
mpg01 <- rep(0, length(mpg))
mpg01[mpg > median(mpg)] <- 1
Auto.df <- data.frame(Auto, mpg01)

# (B)
names(Auto.df)
pairs(Auto.df)
plot(mpg01, acceleration) 
plot(mpg01, weight)
plot(mpg01, horsepower)
plot(mpg01, displacement)
boxplot(Auto.df$acceleration[mpg01 == 1], Auto.df$acceleration[mpg01 == 0], names=c("mpg > med", "mpg < med"), ylab="acceleration", main="Acceleration v. mpg")
boxplot(Auto.df$weight[mpg01 == 1], Auto.df$weight[mpg01 == 0], names=c("mpg > med", "mpg < med"), ylab="weight", main="Weight v. mpg")
boxplot(Auto.df$horsepower[mpg01 == 1], Auto.df$horsepower[mpg01 == 0], names=c("mpg > med", "mpg < med"), ylab="horsepower", main="Horsepower v. mpg")
boxplot(Auto.df$displacement[mpg01 == 1], Auto.df$displacement[mpg01 == 0], names=c("mpg > med", "mpg < med"), ylab="displacement", main="displacement v. mpg")
boxplot(Auto.df$cylinders[mpg01 == 1], Auto.df$cylinders[mpg01 == 0], names=c("mpg > med", "mpg < med"), ylab="cylinders", main="cylinders v. mpg")
boxplot(Auto.df$year[mpg01 == 1], Auto.df$year[mpg01 == 0], names=c("mpg > med", "mpg < med"), ylab="year", main="year v. mpg")
boxplot(Auto.df$displacement[mpg01 == 1], Auto.df$displacement[mpg01 == 0], names=c("mpg > med", "mpg < med"), ylab="displacement", main="displacement v. mpg")


# There is graphically a relationship between mpg and acceleration, weight, horsepower, and displacement.  Cars with MPG's higher than the median tend to have higher accelerations, lower weight, lower horsepower, and lower displacement.  Inversely, cars with mpg's lower than the median tend to have lower accelerations, higher weights, higher horsepower, and higher displacement.  

# (C)  Split data into training and test set, using a random 300 observations as the training set..

set.seed(1)
train <- sample(x=seq(392), size=300, replace=FALSE)
train.Auto <- Auto.df[train, ]
test.Auto <- Auto.df[-train, ]
test.mpg01 <- mpg01[-train]

# (D) USE LDA
lda.fit <- lda(mpg01 ~ cylinders + displacement + horsepower + weight + acceleration, data=Auto.df, subset=train)

lda.pred <- predict(lda.fit, test.Auto)
lda.class <- lda.pred$class

tbl <- table(lda.class, test.mpg01)
tbl
correct.pct <- sum(diag(tbl)) / sum(tbl)
correct.pct
# This LDA model correctly classifies 93.4% of the test data.  It erroneously classifies    6.6% of test observations.

# (E)  USE QDA
qda.fit <- qda(mpg01 ~ cylinders + displacement + horsepower + weight + acceleration, data=Auto.df, subset=train)

qda.pred <- predict(qda.fit, test.Auto)
qda.class <- qda.pred$class

tbl <- table(qda.class, test.mpg01)
tbl
correct.pct <- sum(diag(tbl)) / sum(tbl)
correct.pct
# QDA correctly classifies 91.3% of the test set.  It erroneously classifies 8.7% of test observations.

# (F)
glm.fit <- glm(mpg01 ~ cylinders + displacement + horsepower + weight + acceleration, data=Auto.df, subset=train, family="binomial")
glm.probs <- predict(glm.fit, test.Auto, type="response")
glm.pred <- rep(0, dim(test.Auto)[1])
glm.pred[glm.probs > 0.5] = 1

tbl <- table(glm.pred, test.mpg01)
tbl
correct.pct <- sum(diag(tbl)) / sum(tbl)
correct.pct
# The logistic regression model correctly classifies 90.2% of the test set.  It erroneously classifies 9.8% of test observations.

# (G)  USE KNN
train.Auto <- cbind(cylinders, displacement, horsepower, weight, acceleration)[train, ]
test.Auto <- cbind(cylinders, displacement, horsepower, weight, acceleration)[-train, ]
train.mpg01 <- mpg01[train]

maxK <- 100
knn.results <- data.frame(k = seq(1:maxK), Correct.Yes = rep(0, maxK))
for (x in seq(1:maxK)){
  set.seed(1)
  knn.pred <- knn(train.Auto, test.Auto, train.mpg01, k=x)
  tbl <- table(knn.pred, test.mpg01)
  #print("k=")
  #print(x)
  #print(tbl)
  knn.results[x, 2] <- sum(diag(tbl)) / sum(tbl)
}

knn.results
max(knn.results[,2])
which.max(knn.results[,2])
min(knn.results[,2])
which.min(knn.results[,2])

# From out dataframe we can see that for k=30 we get the best results, with an error rate of 7.7%.  Let's take a closer look.
knn.pred <- knn(train.Auto, test.Auto, train.mpg01, k=30)
tbl <- table(knn.pred, test.mpg01)
tbl


# Overall, the LDA performed best with the lowest error rate.


## Q12
# (A)

Power <- function(){
  print(2^3)
  print(paste0("2^3 = ", 2^3))
}
Power()

# (B)
Power2 <- function(x, a){
  print(paste0(x, " to the ", a, " is equal to ", x^a))
}
Power2(3, 8)

# (C)
Power2(10, 3)
Power2(8, 17)
Power2(131, 3)

# (D)
Power3 <- function(x, a){
  return(x^a)
}
Power3(3, 8)

# (E)
plot(Power3(c(1:10), 2), xlab="x", ylab="x^2", main="Graph of x^2")
plot(Power3(c(1:10), 2), xlab="x", ylab="x^2", main="Graph of x^2", log="x")
plot(Power3(c(1:10), 2), xlab="x", ylab="x^2", main="Graph of x^2", log="y")
plot(Power3(c(1:10), 2), xlab="x", ylab="x^2", main="Graph of x^2", log="xy")

# (F)
PlotPower <- function(x, a){
  plot(Power3(c(x), a))
}

PlotPower(1:10, 3)

# (G)
names(Boston)
attach(Boston)
# I will explore the Boston dataset by fitting classification models in order to predict whether a given suburn has a crime rate above or below the median.  We will use 3 models: Logistic Regression, LDA, and KNN

# Create a new binary variable crim01 that takes the value 1 if the suburb is greater than the median and 0 otherwise.
crim01 <- rep(0, length(crim))
crim01[crim > median(crim)] = 1

Boston.df <- data.frame(Boston, crim01)

# Create a training and a test dataset.
train <- sample(length(crim01), size = 0.7*length(crim01), replace=FALSE)
Boston.train <- Boston[train, ]
Boston.test <- Boston[-train, ]
crim.test <- crim01[-train]

# Start with a logistic model.  From previous study we have determined what we consider to be important predictors in the model: rad, tax, nox, lstat, black, medv, dis
correlations <- cor(Boston)
corrplot(correlations)

glm.fit <- glm(crim01 ~ rad + tax + nox + lstat + black + medv, data=Boston.df, subset=train, family=binomial)
summary(glm.fit)

glm.probs <- predict(glm.fit, Boston.test, type="response")
glm.pred <- rep(0, length(crim.test))
glm.pred[glm.probs > 0.5] = 1
glm.pred
tbl <- table(glm.pred, crim.test)
tbl
correct.pct <- sum(diag(tbl)) / sum(tbl)
correct.pct
error.rate <- 1-correct.pct
error.rate
tbl[4] / (tbl[3] + tbl[4])

# Ok!  Our predictors only have an error rate of 8.5%!  When a neighborhood is a high-crime neighborhood we correctly identify it 88.6% of the time.

# Let's try LDA next.
lda.fit <- lda(crim01 ~ rad + tax + nox + lstat + black + medv, data=Boston.df, subset=train)
lda.pred <- predict(lda.fit, Boston.test)
lda.class <- lda.pred$class
tbl <- table(lda.class, crim.test)
tbl
correct.pct <- sum(diag(tbl)) / sum(tbl)
correct.pct
error.rate <- 1-correct.pct
error.rate
tbl[4] / (tbl[3] + tbl[4])

# LDA has not performed quite as well as logistic regression.  It has a 10.5% error rate and only correctly identifies high-crime neighborhoods 81% of the time.

# Onwards to KNN
train.Boston <- cbind(rad, tax, nox, lstat, black, medv)[train, ]
test.Boston <- cbind(rad, tax, nox, lstat, black, medv)[-train, ]
train.crim01 <- crim01[train]

maxK <- 100
knn.results <- data.frame(k = seq(1:maxK), Correct.Yes = rep(0, maxK))
for (x in seq(1:maxK)){
  set.seed(1)
  knn.pred <- knn(train.Boston, test.Boston, train.crim01, k=x)
  tbl <- table(knn.pred, crim.test)
  #print("k=")
  #print(x)
  #print(tbl)
  knn.results[x, 2] <- sum(diag(tbl)) / sum(tbl)
}

knn.results
max(knn.results[,2])
which.max(knn.results[,2])
1 - max(knn.results[,2])
knn.pred <- knn(train.Boston, test.Boston, train.crim01, k=1)
tbl <- table(knn.pred, crim.test)
tbl[4] / (tbl[3] + tbl[4])

plot(knn.results[,2])

# The KNN model has found its best fit when k=1 with an error rate of 9.2%.  It correctly identifies 91.1% of high-crime neighborhoods.  

# Overall the Logistic regression model had the lowest overall error rate, but the KNN (k=1) model had the highest correct classification rate of high-crime neighborhoods.  Dependent on the circumstances, either model may be used. If someone only wants to make sure they do not move to a high-crime area, they may want to use KNN as it is marginally better than logistic regression.  