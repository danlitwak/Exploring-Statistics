################################################################################################
## Chapter 7: Beyond Linearity:  Polynomial Regression, Splines, Generalized Additive Models

## Lab 7.8
library(ISLR)
library(splines)
library(gam)
library(ggplot2)
library(glmnet)
library(boot)
library(leaps)

attach(Wage)

fit <- lm(wage~poly(age, 4), data=Wage)
coef(summary(fit))

fit2 <- lm(wage~poly(age, 4, raw=TRUE), data=Wage)
coef(summary(fit2))

fit2a <- lm(wage~age + I(age^2) + I(age^3) + I(age^4), data=Wage)
coef(fit2a)

fit2b <- lm(wage~cbind(age, age^2, age^3, age^4), data=Wage)
coef(summary(fit2b))

agelims <- range(age)
age.grid <- seq(from=agelims[1], to=agelims[2])
preds <- predict(fit, newdata=list(age=age.grid), se=TRUE)
se.bands <- cbind(preds$fit + 2*preds$se.fit, preds$fit - 2*preds$se.fit)

par(mfrow=c(1, 2), mar=c(4.5, 4.5, 1, 1), oma=c(0, 0, 4, 0))
plot(age, wage, xlim=agelims, cex=0.5, col="darkgrey")
title("Degree-4 Polynomial", outer=TRUE)
lines(age.grid, preds$fit, lwd=2, col="blue")
matlines(age.grid, se.bands, lwd=1, col="blue", lty=3)

preds2 <- predict(fit2, newdata=list(age=age.grid), se=TRUE)
max(abs(preds$fit - preds2$fit))

fit.1 <- lm(wage ~ age, data=Wage)
fit.2 <- lm(wage ~ poly(age, 2), data=Wage)
fit.3 <- lm(wage ~ poly(age, 3), data=Wage)
fit.4 <- lm(wage ~ poly(age, 4), data=Wage)
fit.5 <- lm(wage ~ poly(age, 5), data=Wage)

anova(fit.1, fit.2, fit.3, fit.4, fit.5)
# Thus, since we have low p-values between 1st and 2nd degree, and 2nd and 3rd degree, it appears a 3rd degree polynomial would fit best.  However, a 4th degree would fit just about as well.

coef(summary(fit.5))

fit.1 <- lm(wage ~ education + age, data=Wage)
fit.2 <- lm(wage ~ education + poly(age, 2), data=Wage)
fit.3 <- lm(wage ~ education + poly(age, 3), data=Wage)
anova(fit.1, fit.2, fit.3)

fit <- glm(I(wage > 250)~poly(age, 4), data=Wage, family=binomial)
preds <- predict(fit, newdata=list(age=age.grid), se=TRUE)
pfit <- exp(preds$fit) / (1 + exp(preds$fit))
se.bands.logit <- cbind(preds$fit + 2*preds$se.fit, preds$fit - 2*preds$se.fit)
se.bands <- exp(se.bands.logit) / (1 + exp(se.bands.logit))
preds <- predict(fit, newdata=list(age=age.grid), type="response", se=TRUE)

plot(age, I(wage>250), xlim=agelims, type="n", ylim=c(0, 0.2))
points(jitter(age), I((wage>250)/5), cex=0.5, pch="|", col="darkgrey")
lines(age.grid, pfit, lwd=2, col="blue")
matlines(age.grid, se.bands, lwd=1, col="blue", lty=3)

table(cut(age, 4))
fit <- lm(wage ~ cut(age, 4), data=Wage)
coef(summary(fit))

## 7.8.2 Splines

fit <- lm(wage ~ bs(age, knots=c(25, 40, 60)), data=Wage)
pred <- predict(fit, newdata=list(age=age.grid), se=TRUE)
plot(age, wage, col="gray")
lines(age.grid, pred$fit, lwd=2)
lines(age.grid, pred$fit + 2*pred$se, lty="dashed")
lines(age.grid, pred$fit - 2*pred$se, lty="dashed")
dim(bs(age, knots=c(25, 40, 60)))
dim(bs(age, df=6))
attr(bs(age, df=6), "knots")

fit2 <- lm(wage ~ ns(age, df=4), data=Wage)
pred2 <- predict(fit2, newdata=list(age=age.grid), se=TRUE)
lines(age.grid, pred2$fit, col="red", lwd=2)

plot(age, wage, xlim=agelims, cex=0.5, col="darkgrey")
title("Smoothing Splines")
fit <- smooth.spline(age, wage, df=16)
fit2 <- smooth.spline(age, wage, cv=TRUE)
fit2$df
lines(fit, col="red", lwd=2)
lines(fit2, col="blue", lwd=2)
legend("topright", legend=c("16 DF", "6.8DF"), col=c("red", "blue"), lty=1, lwd=2, cex=0.8)

plot(age, wage, xlim=agelims, cex=0.5, col="darkgrey")
title("Local Regression")
fit <- loess(wage~age, span=0.2, data=Wage)
fit2 <- loess(wage~age, span=0.5, data=Wage)
lines(age.grid, predict(fit, data.frame(age=age.grid)), col="red", lwd=2)
lines(age.grid, predict(fit2, data.frame(age=age.grid)), col="blue", lwd=2)
legend("topright", legend=c("Span=0.2", "Span=0.5"), col=c("red", "blue"), lty=1, lwd=2, cex=0.8)


## 7.8.3 GAMs
gam1 <- lm(wage ~ ns(year, df=4) + ns(age, df=5) + education, data=Wage)

library(gam)
gam.m3 <- gam(wage~s(year, df=4) + s(age, df=5) + education, data=Wage)
par(mfrow=c(1, 3))
plot(gam.m3, se=TRUE, col="blue")
plot.Gam(gam1, se=TRUE, col="red")

gam.m1 <- gam(wage~s(age, 5) + education, data=Wage)
gam.m2 <- gam(wage~year + s(age, 5) + education, data=Wage)

anova(gam.m1, gam.m2, gam.m3, test="F")
summary(gam.m3)

preds <- predict(gam.m2, newdata=Wage)
gam.lo <- gam(wage~s(year, df=4) + lo(age, span=0.7) + education, data=Wage)
plot.Gam(gam.lo, se=TRUE, col="green")
gam.lo.i <- gam(wage~lo(year, age, span=0.5) + education, data=Wage)

library(akima)
plot(gam.lo.i)

gam.lr <- gam(I(wage>250) ~ year + s(age, df=5) + education, family=binomial, data=Wage)
par(mfrow=c(1, 3))
plot(gam.lr, se=TRUE, col="green")
table(education, I(wage>250))

gam.lr.s <- gam(I(wage>250) ~ year + s(age, df=5) + education, family=binomial, data=Wage, subset=(education!="1. < HS Grad"))
plot(gam.lr.s, se=TRUE, col="green")

########################################################################################################################################################################################
## ISLR Ch. 7 Exercises

## Q1
# (A) a1 = beta0
#     b1 = beta1
#     c1 = beta2
#     d1 = beta3
# (B) a2 = beta0 - e^2
#     b2 = beta1 + 3*e^2
#     c2 = beta2 - 3*e^2
#     d2 = beta3 + 1
# (C) Plug in e into f1 and f2, equations are equal
# (D) Plug in e into f1' and f2', equations are equal
# (E) Plug in e into f1'' and f2'', equations are equal

## Q2 
# As a result of a large lambda, the derivative term will have to be very small in the smoothing part of the function.  
# (A) Any horizontal line would minimize the function
x <- seq(0, 100)
y <- 0 * x + 5 + rnorm(101)
plot(x, y, xlims = c(0, 100), ylims = c(0, 10))
abline(h=5, col="red", lwd=2)
# (B) Minimizing the 1st derivative would result in a quadratic
# (C) Minimizing the 2nd derivative would result in a cubic
# (D) Minimizing the 3rd derivative would result in a quartic
# (E) Minimizing with lambda = 0 would have no penalty term and thus be a linear regression

## Q3
x <- seq(-2, 2, by=0.1)
y <- 1 + 1*(x) + -2*((x-1)^2 * I(x>=1))
ggplot() + geom_line(data=data.frame(x, y), aes(x, y))

## Q4
x <- seq(-2, 2, by=.1)
y <- 1 + 1*(I(x>0 & x<=2) - (x-1) * I(x>=1 & x<=2))
ggplot() + geom_line(data=data.frame(x, y), aes(x, y))

## Q5
# (A) g2 will have a smaller training RSS As it is fitting a higher order polynomial to the training data, which will always result in a closer training fit.
# (B) It is unknown which will have a smaller test RSS as the true form of the data is unknown.  g2 is NOT guaranteed to have a smaller test RSS because the test data is not known
# (C) The function g1 and g2 are the same if lambda = 0 and as such will result in the same training and test RSS.

## Q6
# (A) Perform 10-fold cross validation on the polynomial fits
k <- 10
poly.order <- 5
set.seed(1)
folds <- sample(1:k, nrow(Wage), replace=TRUE)
cv.errors <- matrix(NA, poly.order, k)

for(i in 1:poly.order){
  for(j in 1:k){
    poly.fit <- lm(wage~poly(age, i), data=Wage[folds!=j,])
    pred <- predict(poly.fit, Wage[folds==j,])
    cv.errors[i, j] = mean((Wage$wage[folds == j] - pred)^2)
  }
}
cv.errors
mean.cv.errors <- apply(cv.errors, 1, mean)
mean.cv.errors
min(mean.cv.errors)
which.min(mean.cv.errors)
# The cross-validation has determined that the 4th degree fit resulted in the lowest cross-validation.
# Now to check how this performed compared to an ANOVA of each of the full models.
poly.fit1 <- lm(wage~poly(age, 1), data=Wage)
poly.fit2 <- lm(wage~poly(age, 2), data=Wage)
poly.fit3 <- lm(wage~poly(age, 3), data=Wage)
poly.fit4 <- lm(wage~poly(age, 4), data=Wage)
poly.fit5 <- lm(wage~poly(age, 5), data=Wage)
anova(poly.fit1, poly.fit2, poly.fit3, poly.fit4, poly.fit5)
# The ANOVA results show there is an improvement in the 2nd degree fit over the 1st degree fit, as well as a cubic being a better fit than a quadratic. There is not much evidence that a 4th degree polynomial is a better fit than a 3rd degree.
my.formula <- wage ~ poly(age, 2, raw=TRUE)

plt <- ggplot(data.frame(age, wage), aes(age, wage))
plt <- plt + geom_point()
plt <- plt + stat_smooth(method="lm", se=FALSE, fill=NA,
                formula=y ~ poly(x, 3, raw=TRUE),colour="red")
plt <- plt + stat_smooth(method="lm", se=FALSE, fill=NA,
                  formula=y ~ poly(x, 2, raw=TRUE),colour="blue")
plt <- plt + stat_smooth(method="lm", se=FALSE, fill=NA,
                  formula=y ~ poly(x, 1, raw=TRUE),colour="green")
plt

# (B) Fit a step function using cross-validation to determine the optimal number of cuts
k <- 10
max.cut <- 15
set.seed(1)
cv.errors <- rep(NA, max.cut)

for(i in 2:max.cut){
  Wage$tmp <- cut(age, i)
  step.fit <- glm(wage~tmp, data=Wage)
  cv.errors[i] = cv.glm(Wage, step.fit, K=10)$delta[1]
}

cv.errors
plot(cv.errors)
min(cv.errors[!is.na(cv.errors)])
which.min(cv.errors)
step.fit <- glm(wage~cut(age, 8), data=Wage)

# Extract the x and y steps from the model and plot them using ggplot
summary(step.fit)
str(step.fit)
coef(step.fit)
names(coef(step.fit))


y.steps <- as.numeric(coef(step.fit))
for(i in 2:length(y.steps)){
  y.steps[i] = y.steps[i-1] + y.steps[i]
}
x.steps <- (names(coef(step.fit)))
x.steps

splits <- unlist(strsplit(x.steps, split="(", fixed=TRUE))
new.splits <- rep(NA, length(y.steps))
new.splits[1] = 0
for(i in seq(5, length(splits), by=3)){
  new.splits[(i +1 )/3] <- splits[i]
}
fin.splits <- unlist(strsplit(new.splits, split=",", fixed=TRUE))
fin.splits
x.steps[1] <- 0 
for(i in seq(2, length(fin.splits), by=2)){
  x.steps[(i + 2)/2] = fin.splits[i]  
}
x.steps <- as.numeric(x.steps)

ggplot() + geom_step(data=data.frame(x.steps, y.steps), aes(x.steps, y.steps, col="red")) + geom_point(data.frame(age, wage), mapping=aes(age, wage))


## Q7
plot(maritl, wage)
plot(jobclass, wage)
plot(health, wage)
gam.fit1 <- gam(wage~jobclass + health, data=Wage)
gam.fit2 <- gam(wage~maritl + health, data=Wage)
gam.fit3 <- gam(wage~maritl + jobclass, data=Wage)
gam.fit4 <- gam(wage~maritl + jobclass + health, data=Wage)

anova(gam.fit1, gam.fit2, gam.fit4, test="F")
summary(gam.fit4)
## There is evidence to suggest that a model with all 3 variables will better explain the relationship to wage than with any less.  These are all categorical variables, so the inclusion of each one in the model corresponds to a fixed increase/decrease in wage for being in a given category.  Being in very good health generally increases wages, while so does working in the information sector.  Additionally, being married shows a relationship with higher wages as well, perhaps suggesting one works harder when working for another.

## Q8
names(Auto)
attach(Auto)
plot(horsepower, mpg)
plot(acceleration, mpg)
plot(weight, mpg)
plot(cylinders, mpg)
is.numeric(horsepower)
# At first glance, there appears to be nonlinear relationships between mpg and horsepower, acceleration, weight, and perhaps cylinders.  We will perform polynomial fits and cross-validation for each of these quantitative predictors to determine the best polynomial degree.

k <- 10
poly.order <- 10
set.seed(1)
folds <- sample(1:k, nrow(Auto), replace=TRUE)
cv.errors <- matrix(NA, poly.order, k)

for(i in 1:poly.order){
  for(j in 1:k){
    poly.fit <- glm(mpg~poly(horsepower, i), data=Auto[folds!=j,])
    pred <- predict(poly.fit, Auto[folds==j,])
    cv.errors[i, j] = mean((Auto$mpg[folds == j] - pred)^2)
  }
}
cv.errors
mean.cv.errors <- apply(cv.errors, 1, mean)
mean.cv.errors
min(mean.cv.errors)
which.min(mean.cv.errors)
# These findings suggest that horsepower is best predicting mpg via a 7th degree polynomial.

k <- 10
poly.order <- 10
set.seed(1)
folds <- sample(1:k, nrow(Auto), replace=TRUE)
cv.errors <- matrix(NA, poly.order, k)

for(i in 1:poly.order){
  for(j in 1:k){
    poly.fit <- glm(mpg~poly(weight, i), data=Auto[folds!=j,])
    pred <- predict(poly.fit, Auto[folds==j,])
    cv.errors[i, j] = mean((Auto$mpg[folds == j] - pred)^2)
  }
}
cv.errors
mean.cv.errors <- apply(cv.errors, 1, mean)
mean.cv.errors
min(mean.cv.errors)
which.min(mean.cv.errors)
# These findings suggest that mpg is best predicted by a 2nd-degree polynomial of weight

k <- 10
poly.order <- 10
set.seed(1)
folds <- sample(1:k, nrow(Auto), replace=TRUE)
cv.errors <- matrix(NA, poly.order, k)

for(i in 1:poly.order){
  for(j in 1:k){
    poly.fit <- glm(mpg~poly(acceleration, i), data=Auto[folds!=j,])
    pred <- predict(poly.fit, Auto[folds==j,])
    cv.errors[i, j] = mean((Auto$mpg[folds == j] - pred)^2)
  }
}
cv.errors
mean.cv.errors <- apply(cv.errors, 1, mean)
mean.cv.errors
min(mean.cv.errors)
which.min(mean.cv.errors)
# These findings suggest that mpg is best predicted by a 6th degree polynomial of acceleration.

par(mfrow=c(1, 3), mar=c(4.5, 4.5, 1, 1), oma=c(0, 0, 4, 0))

horselims <- range(horsepower)
horse.grid <- seq(from=horselims[1], to=horselims[2])
poly.fit <- glm(mpg~poly(horsepower, 7), data=Auto)
horse.pred <- predict(poly.fit, newdata=list(horsepower=horse.grid), se=TRUE)
se.bands <- (cbind(horse.pred$fit + 2*horse.pred$se.fit, horse.pred$fit - 2*horse.pred$se.fit))
plot(horsepower, mpg)
lines(horse.grid, horse.pred$fit, lwd=1, col="blue", lty=3)

weightlims <- range(weight)
weight.grid <- seq(from=weightlims[1], to=weightlims[2])
poly.fit <- glm(mpg~poly(weight, 2), data=Auto)
weight.pred <- predict(poly.fit, newdata=list(weight=weight.grid), se=TRUE)
se.bands <- (cbind(weight.pred$fit + 2*weight.pred$se.fit, weight.pred$fit - 2*weight.pred$se.fit))
plot(weight, mpg)
lines(weight.grid, weight.pred$fit, lwd=1, col="blue", lty=3)

accellims <- range(acceleration)
accel.grid <- seq(from=accellims[1], to=accellims[2])
poly.fit <- glm(mpg~poly(acceleration, 6), data=Auto)
accel.pred <- predict(poly.fit, newdata=list(acceleration=accel.grid), se=TRUE)
se.bands <- (cbind(accel.pred$fit + 2*accel.pred$se.fit, accel.pred$fit - 2*accel.pred$se.fit))
plot(acceleration, mpg)
lines(accel.grid, accel.pred$fit, lwd=1, col="blue", lty=3)

## Try fitting these with splines
spline.fit <- gam(mpg~ns(horsepower, df=5) + ns(weight, df=5) + ns(acceleration, df=5), data=Auto)
preds <- predict(spline.fit, se=TRUE)
plot(spline.fit, se=TRUE, col="red")

## Try fitting these using local regression
span=0.5
local.fit <- loess(mpg~horsepower, span=span, data=Auto)
plot(horsepower, mpg)
lines(horse.grid, predict(local.fit, data.frame(horsepower=horse.grid)), col="red", lwd=2)

local.fit <- loess(mpg~weight, span=span, data=Auto)
plot(weight, mpg)
lines(weight.grid, predict(local.fit, data.frame(weight=weight.grid)), col="red", lwd=2)

local.fit <- loess(mpg~acceleration, span=span, data=Auto)
plot(acceleration, mpg)
lines(accel.grid, predict(local.fit, data.frame(acceleration=accel.grid)), col="red", lwd=2)

## Q9
# (A)
library(MASS)
attach(Boston)
names(Boston)

par(mfrow=c(1,1))
dis.lims <- range(dis)
dis.grid <- seq(dis.lims[1], dis.lims[2])

poly.fit <- lm(nox~poly(dis, 3), data=Boston)
preds <- predict(poly.fit, newdata=list(dis=dis.grid))
summary(poly.fit)

plot(dis, nox)
lines(dis.grid, preds, col="red")

# (B)
rss.poly <- rep(NA, 10)

par(mfrow=c(2, 5))
for(i in 1:10){
  poly.fit <- lm(nox~poly(dis, i), data=Boston)
  preds <- predict(poly.fit, newdata=list(dis=dis.grid))
  rss.poly[i] <- sum((summary(poly.fit)$residuals)^2)
  plot(dis, nox)
  lines(dis.grid, preds, col="red")
}
par(mfrow=c(1,1))
plot(rss.poly)

# (C)
k <- 10
set.seed(1)
folds <- sample(1:k, nrow(Boston), replace=TRUE)
poly.deg <- 10
cv.errors <- matrix(NA, poly.deg, k)

for(i in 1:poly.deg){
  for(j in 1:k){
    poly.fit <- lm(nox~poly(dis, i), data=Boston[folds!=j,])
    preds <- predict(poly.fit, Boston[folds==j,])
    cv.errors[i, j] = mean((Boston$nox[folds==j] - preds)^2)
  }
}

mean.cv.errors = apply(cv.errors, 1, mean)
mean.cv.errors
plot(mean.cv.errors)
min(mean.cv.errors)
which.min(mean.cv.errors)
# Cross validation shows that the minimum of the cross validation is when the degree of the polynomial is 4, though it is nearly the same as 3.  As such, since a 3rd degree polynomial is a simpler model it should be used.  

# (D) Regression spline
par(mfrow=c(1,1))
spline.fit <- lm(nox~bs(dis, knots=c(4, 8, 10)), data=Boston)
pred <- predict(spline.fit, newdata=list(dis=dis.grid), se=TRUE)
plot(dis, nox)
lines(dis.grid, pred$fit, lwd=2, col="green")

spline.fit <- lm(nox~bs(dis, df=4), data=Boston)
pred <- predict(spline.fit, newdata=list(dis=dis.grid), se=TRUE)
plot(dis, nox)
lines(dis.grid, pred$fit, lwd=2, col="red")
str(spline.fit)
attr(bs(dis, df=4), "knots")
# With 4 degrees of freedom there will only be 1 knot, which is at 3.2.

# (E)
numSplines <- 20
spline.RSS <- rep(NA, numSplines)
par(mfrow=c(2, 5))
for(i in seq(1, numSplines)){
  spline.fit <- lm(nox~bs(dis, df=i), data=Boston)
  pred <- predict(spline.fit, data=Boston)
  spline.RSS[i] <- mean((Boston$nox - pred)^2)
  plot(dis, nox)
  pred <- predict(spline.fit, newdata=list(dis=dis.grid))
  lines(dis.grid, pred, lwd=2, col="green")
}
spline.RSS
par(mfrow=c(1,1))
plot(spline.RSS)
# The RSS for each spline generally decreases as the degrees of freedom increase.  This is because with more degrees of freedom the fit can "wiggle more" to fit all the training points, most likely overfitting. It flattens out around 15 degrees of freedom.

# (F)
k <- 10
set.seed(1)
numSplines <- 40
spline.cv <- rep(NA, numSplines)

for(i in 1:numSplines){
  spline.fit <- glm(nox~bs(dis, df=i), data=Boston)
  spline.cv[i] <- cv.glm(Boston, spline.fit, K=k)$delta[2]
}
spline.cv
min(spline.cv)
which.min(spline.cv)
plot(1:numSplines, spline.cv, type="l")
# Via cross-validation, we find that for 5 degrees of freedom the CV is minimum.  

## Q10
# (A) Perform forward stepwise selection
library(leaps)
predict.regsubsets <- function(object, newdata, id,...){
  form <- as.formula(object$call [[2]])
  mat <- model.matrix(form, newdata)
  coefi <- coef(object, id=id)
  xvars <- names(coefi)
  mat[,xvars]%*%coefi
}

names(College)
attach(College)

numVars <- length(names(College))
set.seed(1)
train <- sample(c(TRUE, FALSE), size=nrow(College), replace=TRUE)
test <- (!train)
cv.errors <- rep(NA, numVars - 1)

fwd.fit <- regsubsets(Outstate~., data=College[train,], nvmax=numVars-1, method="forward")

for(i in seq(1, numVars - 1)){
  pred <- predict(fwd.fit, College[test,], id=i)
  cv.errors[i] <- mean((College$Outstate[test] - pred)^2)
}
cv.errors
plot(cv.errors)
min(cv.errors)
which.min(cv.errors)
coef(fwd.fit, 6)
# The Cross-validation starts flattening out around when the number of variables is ~6.  The significant variables are PrivateYes, Room.Board, Terminal, perc.alumni, Expend, Grad.Rate

# (B) (C)
par(mfrow=c(2, 3))
plot(Private,Outstate, sub="Outstate v. Private")
plot(Room.Board,Outstate, sub="Outstate v. Room.Board")
plot(Terminal, Outstate, sub="Outstate v. Terminal")
plot(perc.alumni, Outstate, sub="Outstate v. perc.alumni")
plot(Expend, Outstate, sub="Outstate v. Expend")
plot(Grad.Rate, Outstate, sub="Outstate v. Grad.Rate")

df <- 5
gam.fit <- gam(Outstate~ Private + s(Room.Board, df=5) + s(Terminal, df=5) + s(perc.alumni, df=5) + s(Expend, df=5) + s(Grad.Rate, df=5), data=College[train,])
pred <- predict(gam.fit, newdata=College[test,])
cv.gam <- mean((College$Outstate[test] - pred)^2)
cv.gam
cv.errors[6]
plot.Gam(gam.fit, se=TRUE, col="blue")
# Applying splines with 5 degrees of freedom to the quantitative variables in out model improve the cross-validation noticeably in the GAM.

# (D)
summary(gam.fit)
# The variables Expend, terminal, and potentially Grad.Rate appear to have nonlinear relationships in their scatterplots, and this is confirmed in the splines for which these graphs are the least linear, though terminal is somewhat linear. 

# Q11
# (A)
par(mfrow=c(1,2))
set.seed(1)
x1 <- rnorm(100)
x2 <- rnorm(100)
eps <- rnorm(100)
b0 <- 7
b1 <- 2
b2 <- 5
y <- b1*x1 + b2*x2 + b0 + eps
plot(x1, y)
plot(x2, y)
# (B)
beta1 <- 8
# (C)
a <- y-beta1*x1
beta2 <- lm(a~x2)$coef[2]
# (D)
a <- y-beta2*x2
beta1 <- lm(a~x2)$coef[2]
# (E)
b1s <- rep(NA, 1000)
b2s <- rep(NA, 1000)
b0s <- rep(NA, 1000)
beta1 <- 1000
for(i in 1:1000){
  b1s[i] <- beta1
  a <- y-beta1*x1
  fit <- lm(a~x2)
  beta2 <- fit$coef[2]
  b0s[i] <- fit$coef[1]
  
  a <- y-beta2*x2
  beta1 <- lm(a~x1)$coef[2]
  b2s[i] <- beta2
  
}
head(b1s)
head(b2s)
head(b0s)
par(mfrow=c(1, 3))
plot(b1s)
plot(b2s)
plot(b0s)

# (F)
mlr.fit <- lm(y ~ x1 + x2)
mlr.fit$coef
plot(x1, y)
abline(mlr.fit$coef[1], mlr.fit$coef[2])
plot(b1s)
abline(mlr.fit$coef[2], 0, col="red")
plot(x2, y)
abline(mlr.fit$coef[1], mlr.fit$coef[3])
plot(b2s)
abline(mlr.fit$coef[3], 0, col="red")

# (G)
# On this data set it took less than 5 iterations for the coefficients to converge to their Multiple Linear Regression values.  

## Q12
set.seed(1)
p <- 100
n <- 1000
toy <- as.matrix(data.frame(replicate(p, sample(rnorm(p), n, rep=TRUE))))
coeff <- sample(-10:10, p, replace=TRUE)
eps <- rnorm(n)
y <- toy%*%coeff + eps

beta <- sample(-10:10, 100, replace=TRUE)

for(i in 1:5){
  for(j in 1:p){
      a <- y - toy %*% beta + beta[j] * toy[,j]
      beta[j] <- lm(a~toy[,j])$coef[2]
  }
}

# The maximum deviation of any single given coefficient is 0.096 within 10 iterations.  
max(abs(coeff - beta))
plot(abs(coeff - beta))

beta <- sample(-10:10, 100, replace=TRUE)
par(mfrow=c(1, 4))
for(k in 2:5){
  for(i in 1:k){
    for(j in 1:p){
      a <- y - toy %*% beta + beta[j] * toy[,j]
      beta[j] <- lm(a~toy[,j])$coef[2]
    }
  }
  
    
  max(abs(coeff - beta))
  plot(abs(coeff - beta))
}

## It only takes about 2 iterations for the values to converge.  