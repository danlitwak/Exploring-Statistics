######################################################################################################################################################################################
### Lab: Linear Regression

library(MASS)
library(ISLR)
fix(Boston)
which(is.na(Boston))
names(Boston)
attach(Boston)
?Boston

## Linear model with lstat predicting medv
lm.fit <- lm(medv ~ lstat)
lm.fit
summary(lm.fit)
names(lm.fit)
coef(lm.fit)

## Constructs a __% confidence interval for the coefficients of the regression
confint(lm.fit, level = .99)

## Constructs a confidence and prediction interval for the predictions of medv using
## values of lstat (5, 10, 15)
predict(lm.fit, data.frame(lstat=(c(5, 10, 15))),
  interval = "confidence")

predict(lm.fit, data.frame(lstat=(c(5, 10, 15))),
        interval = "prediction")

plot(lstat, medv, pch="+", col="red")
abline(lm.fit,lwd=3, col="blue")
plot(1:40, 1:40, pch=1:20)
par(mfrow=c(2,2))
plot(lm.fit)
plot(lstat, residuals(lm.fit))
plot(lstat, rstudent(lm.fit))
plot(predict(lm.fit), residuals(lm.fit))
plot(predict(lm.fit), rstudent(lm.fit))

## Because the residual plots do not appear to be randomly scattered,
## there is evidence of nonlinearity in the data

## leverage statistics
hatvalues(lm.fit)
plot(hatvalues(lm.fit))
## which.max identifies the index of the largest element of a vector
which.max(hatvalues(lm.fit))



### Multiple Linear Regression

## Perform a regression using all variables
lm.fit <- lm(medv ~ ., data=Boston)
summary(lm.fit)
summary(lm.fit)$r.sq
## Residual Squared Error (RSE)
summary(lm.fit)$sigma


## Since age has such a high p-value, remove it from the regression
lm.fit1 <- lm(medv ~ .-age-indus, data=Boston)
summary(lm.fit1)
## Or we could use the update function lm.fit1 <- update(lm.fit, ~.-age)


### Interaction Terms

## we could independently add an interaction between lstat and black...  lstat:black
## or we could include both terms and the interaction... 
## lstat*black = lstat + black + lstat:black

summary(lm(medv ~ lstat*age, data = Boston))


### Non-linear transformations

## Wrap a predictor in I() to include that term in the model
lm.fit2 <- lm(medv ~ lstat + I(lstat^2))
summary(lm.fit2)
par(mfrow=c(1,1))
plot(lstat, medv)
lines(lstat, predict(lm.fit2), col="red")

## Use anova to check if there is an improvement in the two term model compared to 
## a single variable
lm.fit <- lm(medv ~ lstat)
anova(lm.fit, lm.fit2)

par(mfrow=c(2,2))
plot(lm.fit2)

## Higher order polynomials
lm.fitEX <- lm(medv ~ poly(lstat, 6))
summary(lm.fitEX)





#### Qualitative Predictors

library(corrplot)
attach(Carseats)
which(is.na(Carseats))

names(Carseats)
?Carseats
fix(Carseats)
par(mfrow=c(1,1))
correlations <- cor(Carseats[,c(1:6, 8:9)], use="everything")
corrplot(correlations, type="lower")
pairs(Carseats)

lm.fit <- lm(Sales ~ . + Income:Advertising + Price:Age, data=Carseats)
summary(lm.fit)
contrasts(ShelveLoc)
?contrasts


LoadLibraries=function() {
  library(ISLR)
  library(MASS)
  print("The libraries have been loaded")
}

LoadLibraries()

######################################################################################################################################################################################

### EXERCISES

##Q1
# The H0's for table 3.4 assume that there is no relationship between each given media's      advertising allocation and the sales made.  We can conclude from the p-values  that       without any advertising there is a non-zero amount of sales (the Intercept), and that     both TV and radio have a statistically significant impact on sales. However, newspaper     shows no impact on sales.

##Q2
# In the KNN classifier, a ppoint is chosen and the k nearest neighbors are checked  for      that data point.  The majority attribute of those k nearest neighbors is what the new     data point is classified as.  In KNN regression, the k-nearest neighbors are checked      for that data point.  The predicted outcome for the new point is based  on the average     of the k nearest neighbors for the given variable.

##Q3
# (a) The correct answer is (iii).  For fixed GPA and IQ, females tend to earn $35K more        on average than males.  However, due to the interaction term of beta_5 = -10, this        means that for females a higher GPA may lead to lower salaries for them.
# (b) Predict the salary of a female with IQ of 110 and GPA of 4.0
sal=function(GPA, IQ, Gend){
  50 + 20*x1 + .07*x2 + 35*x3 + .01*x1*x2 + -10*x1*x3
}
sal(4.0, 110, 1)
# (c) False, we do not examine the size of the interaction to determine if it exists
#     but the magnitude of the p-value.

##Q4
# (a) We would expect the training RSS to be lower for the cubic regression, even if            the true relationship is linear.  This is because the training data is almost             certainly not perfectly linear and a cubic could be reduced to a linear model             through the higher coefficients being 0.  Higher orders will overfit with smaller         RSS.
# (b) When actually tested, we would expect the linear model to have a lower RSS now.           Since the underlying form should be linear, the cubic function will overfit and           result in more error than the linear by itself.
# (c) The cubic should fit better for reasons as explained in part (a).
# (d) In this case we would not be able to tell.  If the true relationship was nearly linear then the         linear model may fit better, but if the true relationship was far from linear then the cubic may        fit better (and thus have lower RSS).

##Q5
# Do algebra

##Q6
#   The least squares line will always pass through (x-bar, y-bar) in the case of Simple      Linear Regression.  This is because regardless of what beta_0 and beta_1 are, equation     2 states that the means are a solution, thus a point on the line.  This can be shown      via calculus and minimizing the RSS.

##Q7
#   See paper

##Q8
# (a)
names(Auto)
summary(Auto)
lm.fit <- lm(mpg ~ horsepower, data = Auto)
summary(lm.fit)
# Based on the summary, there is a definite relationship between mpg and horsepower as      the p-value is very low.  For every gain of 1HP, the mpg is expected to decrease by       about 0.16mpg.  The R^2 is 0.6059, indicating that 60% of the variation in mpg can be     explained by HP.
predict(lm.fit, data.frame(horsepower=(c(98))),
        interval = "confidence")
predict(lm.fit, data.frame(horsepower=(c(98))),
        interval = "confidence", level=.99)
predict(lm.fit, data.frame(horsepower=(c(98))),
        interval = "prediction")

# (b) 
plot(Auto$horsepower, Auto$mpg)
abline(lm.fit, col="red", lwd=3)
par(mfrow=c(2,2))
plot(lm.fit)

# (c) There is a pattern in the residual plot and not random scatter, so a linear model was       not the best model to fit the data.  We could try to include higher order terms to         create a better model.

lm.fit <- lm(mpg~ horsepower + I(horsepower^2), data = Auto)
summary(lm.fit)
plot(lm.fit)
plot(Auto$horsepower, Auto$mpg)


##Q9
# (a) scatterplot matrix
names(Auto)

# (b)
pairs(Auto)
correlations <- cor(Auto[,-9], use="everything")
correlations > 0.9
which(correlations > .9)

# (c) 
lm.fit <- lm(mpg ~ .-name, data = Auto)
summary(lm.fit)
#   There is a relationship between mpg and (cylinders, horsepower, weight, year, origin)      as determined by the significant p-values.  According to the year variable coefficient,     every year the mpg is expected to increase by 0.75 mpg.

# (d) 
par(mfrow=c(2,2))
plot(lm.fit)
#   The residual plot shows that the multiple linear model is a decent fit, but for larger     fitted values there tend to be larger residuals.  There is also one point of unusually     high leverage that should be examined.

# (e)  
lm.fit <- lm(mpg ~ cylinders*horsepower + cylinders*weight + cylinders*year + cylinders*origin + horsepower*weight + horsepower*year + horsepower*origin + weight*year + weight*origin + year*origin, data=Auto)
summary(lm.fit)

lm.fit <- lm(mpg ~ cylinders*weight + cylinders*origin + horsepower*year + horsepower*origin + weight*origin, data=Auto)
summary(lm.fit)
plot(lm.fit)

lm.fit.log <- lm(mpg ~ cylinders*log(weight) + cylinders*origin + log(horsepower)*year + log(horsepower)*origin + log(weight)*origin, data=Auto)
summary(lm.fit.log)
plot(lm.fit.log)

lm.fit.sqrt <- lm(mpg ~ cylinders*sqrt(weight) + cylinders*origin + sqrt(horsepower)*year + sqrt(horsepower)*origin + sqrt(weight)*origin, data=Auto)
summary(lm.fit.sqrt)
plot(lm.fit.sqrt)

lm.fit.sq <- lm(mpg ~ cylinders*I(weight^2) + cylinders*origin + I(horsepower^2)*year + I(horsepower^2)*origin + I(weight^2)*origin, data=Auto)
summary(lm.fit.sqrt)
plot(lm.fit.sqrt)

summary(lm.fit)
summary(lm.fit.log)
summary(lm.fit.sqrt)
summary(lm.fit.sq)

# Based on the RSE, all of these models perform roughly the same, with the SQRT being the best-fitting and the squared being the worst fitting.

corrplot(correlations > .9)

lm.fit.new <- lm(-log(mpg) ~ cylinders*displacement + displacement*weight + horsepower + origin + year, data=Auto)
summary(lm.fit.new)
par(mfrow=c(2,2))
plot(lm.fit.new)

# Based on the scatterplot and correlation matrices, as well as variables defined as         significant when we ran all variables in the model, a new model was built with interactio   ns between cylinders:displacement and weight:displacement.  This new model has an RSE of   0.113, much lower than our other models, and an R^2 of 0.89, account for 89% of variation   in mpg.
# The residual plot shows random scatter and relatively constant variance, though the normal Q-Q plot veers from Normality near the edges.  The studentized residuals do not go above 2.0, and there is a high leverage point, but it does not show an unusually large residual.



###Q10
# (a)
?Carseats
names(Carseats)
lm.fit <- lm(Sales ~ Price + Urban + US, data=Carseats)
summary(lm.fit)
# (b)   The F-statistic results in a low p-value, meaning there is most likely a relationship between Sales and at least one of our variables.  In particular, significant variables are found to be Price and US, with Urban not being found to be significant.  The coefficient of Price details that for every $1 the company increases the price of the carseat, they expect to make 54 less sales at each location.  The coefficient of US means that if a store is in the US it is expected to make 1200 more sales than a store not in the US.

# (c)  
sales=function(price, US){
  13.04 - 0.054*price + 1.200*US
}
sales(100, 0)

# (d) The Urban variable is not found to be significant, with such a large p-value that it should be removed from the model.

# (e) 
lm.fit <- lm(Sales ~ Price + US, data=Carseats)
summary(lm.fit)
plot(lm.fit)
# (f) Both models fit the data very similarly, so when it comes down to choosing a model we would choose the simpler model with fewer variables.  It is easier to interpret and provides extremely similar results.

# (g)  
?predict
?confint
lm.fit$coeff
confint(lm.fit)

# (h) There is one high leverage point, but no apparent outliers in the residual plot.  The Normal Q-Q plot looks linear.


###Q11
# (a)
set.seed(1)
x <- rnorm(100)
y <- 2*x*rnorm(100)
hist(x)
hist(y)

# (a)
lm.fit <- lm(y ~ x + 0)
summary(lm.fit)
# The coefficient of x is -0.45 with a p-value <0.05, so it is statistically significant.  The t-statistics is -2.866.  We would not expect any correlation between the two variables because x is multipled by a random factor, but we would expect the "bow-tie" shape as seen in the scatterplot as larger x-values will result in larger y-values, on average.

# (b)
lm.fit <- lm(x ~ y + 0)
summary(lm.fit)
# We get the same result, with the same t-statistica and p-value but a different coefficient of -0.17.  Since the correlation exists independent of which variable is the predictor, we should have expected the same t-statistic and thus p-value.  The coefficient relationship 

# (d)  See paper
tstat=function(n, x, y){
  sqrt(n-1)*sum(x*y) / sqrt(sum(x^2)*sum(y^2)-sum(x*y)^2)
}

tstat(100, x, y)
tstat(100, y, x)

# (e) The formula is symmetric in x and y, meaning they could be interchanged in every instance and the formula would not be affected, nor would the final outcome.  

# (f) 
lm.fit <- lm(y ~ x, data=Carseats)
summary(lm.fit)

lm.fit <- lm(x ~ y, data=Carseats)
summary(lm.fit)

###Q12

# (a) The coefficient estimates will be the same when the sum of the squared x's is equal to the sum of the squared y's.  Effectively, the function must be symmetric about the y=x line.

# (b) Different coefficient estimates
x <- rnorm(100)
y <- 4*x*rnorm(100)
plot(x, y)
hist(x)
hist(y)
sum(x)
sum(y)

lm.fit <- lm(y ~ x + 0)
summary(lm.fit)

lm.fit <- lm(x ~ y + 0)
summary(lm.fit)

# (c)  Same coefficient estimates
x <- rnorm(100)
y <- x
sum(x)
sum(y)

lm.fit <- lm(y ~ x + 0)
summary(lm.fit)

lm.fit <- lm(x ~ y + 0)
summary(lm.fit)


###Q13

# (a)
set.seed(1)
x <- rnorm(100)

# (b)
eps <- rnorm(100, 0, 0.25)

par(mfrow=c(2,2))
hist(x)
hist(eps)

# (c)
y <- -1 + .5*x + eps
Data <- data.frame(x, eps, y)
hist(y)
# The length of vector y is 100, because it has used vectorized functions to calculate each entry.

lm.fit <- lm(y ~ x + eps)
summary(lm.fit)

lm.fit <- lm(y ~ x)
summary(lm.fit)

# (d) 
par(mfrow=c(1,1))
plot(x, y)
# The model including x and eps is a near-perfect fit to the data, as it accounts for all error.
# There is a clear linear relationship between x and y in the positive direction, though not perfect.  On average it is obvious the measurements are around the y=.5x line with some error involved.  The R^2 of 0.77 agrees with the visual fit of this model.

# (e) See above.  beta_0 and beta_1 are both very close to their true values in both cases.

# (f)
abline(lm.fit)
abline(-1, .5, col="red")
legend(1, y=-1.5, legend=c("LSRL","Pop"),col=c("black","red"), lty=1:2)

# (g)
lm.fit.sq <- lm(y ~ x + I(x^2))
summary(lm.fit.sq)
plot(x, y)
lines(Data$x, predict(lm.fit.sq), col="blue")
#  The R^2 and RSE are marginally better for the model with x^2.  This does not provide evidence this model is significantly better because we may just be overfitting to the data.

# (h)
set.seed(1)
x <- rnorm(100)
eps <- rnorm(100, 0, 0.1)
y <- -1 + .5*x + eps
Data <- data.frame(x, eps, y)

lm.fit.quiet <- lm(y ~ x + eps)
summary(lm.fit.quiet)

lm.fit.quiet <- lm(y ~ x)
summary(lm.fit.quiet)
plot(x, y)

abline(lm.fit.quiet)
abline(-1, .5, col="red")
legend(1, y=-1.5, legend=c("LSRL","Pop"),col=c("black","red"), lty=1:2)

# We see the results we would expect: a lower RSE and higher R^2 coupled with less spread in the data and a better fitting line.

# (i)

set.seed(1)
x <- rnorm(100)
eps <- rnorm(100, 0, 1)
y <- -1 + .5*x + eps
Data <- data.frame(x, eps, y)

lm.fit.noisy <- lm(y ~ x + eps)
summary(lm.fit.noisy)

lm.fit.noisy <- lm(y ~ x)
summary(lm.fit.noisy)
plot(x, y)

abline(lm.fit.noisy)
abline(-1, .5, col="red")
legend(1, y=-1.5, legend=c("LSRL","Pop"),col=c("black","red"), lty=1:2)
# Again we see what is expected  a higher RSE and lower R^2 with data much more scattered.  It is more difficult to tell if there is a pattern in the data.

# (j)
confint(lm.fit)
confint(lm.fit.quiet)
confint(lm.fit.noisy)

# We can tell that the noisier the data, the wider the confidence interval, while the quieter the data, the narrower the interval.

###Q14

# (a) 
set.seed(1)
x1 <- runif(100)
x2 <- 0.5*x1 + rnorm(100)/10
y <- 2 + 2*x1 + 0.3*x2 + rnorm(100)
hist(x1)
hist(x2)
hist(y)
# y = 2 + 2x1 + 0.3x2 + eps

# (b)
cor(x1, x2)
plot(x1, x2)

# (c)
lm.fit <- lm(y ~ x1 + x2)
summary(lm.fit)

# We find a small F-statistic so there is a relationship between y and at least one of the variables.  The RSE is 1.056 and R^2 is 0.21, suggesting not a great fit to the training data.  We find the y-intercept to be 2.13 in the model compared to 2 in the defined relationship.  We find beta_1 to be 1.44 compared to 2 in the defined relationship.  We find beta_2 to be 1 compared to .3 in the defined relationship.  In fact, beta_2 is not even found to be significant.  This does not present a good fit.

# (d)
lm.fit <- lm(y ~ x1)
summary(lm.fit)
# This model fits approximately the same, having a very close y-intercept and beta_1 coefficient.  The null is rejected.

# (e)
lm.fit <- lm(y ~ x2)
summary(lm.fit)
# This model fits slightly worse since there were two sources of error related to x2, having a very close y-intercept and a beta_2 coefficient that is far from expected.  The null is rejected.

# (f) No, they do not contradict each other because x1 and x2 are collinear predictors so the effect on y is difficult to separate out.

# (g)
x1 <- c(x1, 0.1)
x2 <- c(x2, 0.8)
y <- c(y, 6)

lm.fit <- lm(y ~ x1 + x2)
summary(lm.fit)
par(mfrow=c(2,2))
plot(lm.fit)

lm.fit <- lm(y ~ x1)
summary(lm.fit)
par(mfrow=c(2,2))
plot(lm.fit)

lm.fit <- lm(y ~ x2)
summary(lm.fit)
par(mfrow=c(2,2))
plot(lm.fit)

# The added data point is an outlier when comparing x1 and x2 on a plot and on the first and third models it is very easy to see it is a high-leverage point.  However, on the residual graphs it is not easy to pick out any particular outliers.  


###Q15

# (a) 
par(mfrow=c(1,1))
names(Boston)
?Boston
pairs(Boston)
correlations <- cor(Boston)
corrplot(correlations)



linReg <- function(vars) {
  lm.fit <- lm(crim ~ vars, data = Boston)
  summary(lm.fit)
  return(lm.fit$coefficients[2])
}

coeffVec <- c(0)  
coeffVec <- cbind(coeffVec, linReg(zn))
coeffVec <- cbind(coeffVec, linReg(indus))
coeffVec <- cbind(coeffVec, linReg(chas))
coeffVec <- cbind(coeffVec, linReg(nox))
coeffVec <- cbind(coeffVec, linReg(rm))
coeffVec <- cbind(coeffVec, linReg(age))
coeffVec <- cbind(coeffVec, linReg(dis))
coeffVec <- cbind(coeffVec, linReg(rad))
coeffVec <- cbind(coeffVec, linReg(tax))
coeffVec <- cbind(coeffVec, linReg(ptratio))
coeffVec <- cbind(coeffVec, linReg(black))
coeffVec <- cbind(coeffVec, linReg(lstat))
coeffVec <- cbind(coeffVec, linReg(medv))

# The following variables were found to be statistically significantly correlated with crime per capita:  zn, indus, nox, rm, age, dis, rad, tax, ptratio, black, lstat, medv.  In fact, the only variable not correlated with crim is chas.  See the scatterplot matrix for relationships.

# (b)
lm.fit <- lm(crim ~ ., data=Boston)
summary(lm.fit)

# This fit suggests the following variables are significant: zn, nox, dis, rad, black, lstat, and medv.  

# (c) Many fewer variables are considered significant in the multiple regression, and none of the significant ones are in conflict with the univariate models.

coeffs <- data.frame(lm.fit$coefficients)
colnames(coeffs) <- c("MR Coeff")
coeffs
coeffs["SLR Coeff"] <- as.vector(coeffVec)
coeffs

plot(coeffs[-1,"SLR Coeff"], coeffs[-1, "MR Coeff"], xlab="Simple LinReg Coeff", ylab="Multiple LinReg Coeff")

# nox stands out as having drastically different coefficients in each of the models


# (d)
lm.fit <- lm(crim ~ poly(zn, degree=1))
summary(lm.fit)
lm.fit <- lm(crim ~ poly(zn, degree=3))
summary(lm.fit)
plot(zn, crim)
# There is a small improvement for a third degree polynomial, but it does not seem like enough of an improvement to warrant a more complex model.

lm.fit <- lm(crim ~ poly(indus, degree=1))
summary(lm.fit)
lm.fit <- lm(crim ~ poly(indus, degree=3))
summary(lm.fit)
plot(indus, crim)
# There is an improvement in R^2 from 0.16 to 0.26 and a reduction of the RSE.  There may be evidence of a nonlinear association.

lm.fit <- lm(crim ~ poly(nox, degree=1))
summary(lm.fit)
lm.fit <- lm(crim ~ poly(nox, degree=3))
summary(lm.fit)
plot(nox, crim)
# There is an improvement in R^2 from 0.17 to 0.30 and a reduction of the RSE.  There may be evidence of a nonlinear association, and this can roughly be seen in the scatterplot.

lm.fit <- lm(crim ~ poly(rm, degree=1))
summary(lm.fit)
lm.fit <- lm(crim ~ poly(rm, degree=3))
summary(lm.fit)
plot(rm, crim)
# There is minimal improvement in the R^2 or RSE and no apparent association in the scatterplot.

lm.fit <- lm(crim ~ poly(age, degree=1))
summary(lm.fit)
lm.fit <- lm(crim ~ poly(age, degree=3))
summary(lm.fit)
plot(age, crim)
# There is minimal improvement in the R^2 and RSE and no clear association in the scatterplot.

lm.fit <- lm(crim ~ poly(dis, degree=1))
summary(lm.fit)
lm.fit <- lm(crim ~ poly(dis, degree=3))
summary(lm.fit)
plot(dis, crim)
# There is an improvement in the R^2 from 0.14 to 0.28 and a reduction in the RSE, suggesting a possible nonlinear association.

lm.fit <- lm(crim ~ poly(rad, degree=1))
summary(lm.fit)
lm.fit <- lm(crim ~ poly(rad, degree=3))
summary(lm.fit)
plot(rad, crim)
# There is minimal improvement in R^2 or RSE, and no apparent nonlinear association in the scatterplot.

lm.fit <- lm(crim ~ poly(tax, degree=1))
summary(lm.fit)
lm.fit <- lm(crim ~ poly(tax, degree=3))
summary(lm.fit)
plot(tax, crim)
# There is minimal improvement in R^2 or RSE and no apparent nonlinear association in the scatterplot

lm.fit <- lm(crim ~ poly(ptratio, degree=1))
summary(lm.fit)
lm.fit <- lm(crim ~ poly(ptratio, degree=3))
summary(lm.fit)
plot(ptratio, crim)
# There is minimal improvement in R^2 and RSE and no apparent nonlinear association in the scatterplot.

lm.fit <- lm(crim ~ poly(black, degree=1))
summary(lm.fit)
lm.fit <- lm(crim ~ poly(black, degree=3))
summary(lm.fit)
plot(black, crim)
# There is minimal improvement in R^2 and RSE and no apparent nonlinear association in the scatterplot.

lm.fit <- lm(crim ~ poly(lstat, degree=1))
summary(lm.fit)
lm.fit <- lm(crim ~ poly(lstat, degree=3))
summary(lm.fit)
plot(lstat, crim)
# There is minimal improvement in R^2 and RSE and no apparent nonlinear association in the scatterplot.

lm.fit <- lm(crim ~ poly(medv, degree=1))
summary(lm.fit)
lm.fit <- lm(crim ~ poly(medv, degree=3))
summary(lm.fit)
plot(medv, crim)
# There is significant improvement in R^2 from 0.15 to 0.42 and a noticeable reduction in RSE strongly suggestive of a nonlinear association in the scatterplot.
