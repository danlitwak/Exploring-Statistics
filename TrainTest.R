house.df = read.csv("train.csv")
house.df
attach(house.df)

## Finds the quantitative variables in house.df
house.num.vars <- sapply(house.df, is.numeric)
house.num.df <- house.df[house.num.vars]
regress.vars.df <- house.num.df[-38]

## First test case of SalePrice and relation to Lot Area
houseLM1 <- lm(SalePrice ~ LotArea)
houseLM1
coefficients(houseLM1)
Predicted.Sale.Price <- fitted(houseLM1)
summary(houseLM1)
plot(SalePrice ~ LotArea)
abline(houseLM1)

## Multiple regression relating Sale Price to all numerical variables
houseLM2 <- lm(SalePrice ~ ., data=regress.vars.df)
houseLM2
coefficients(houseLM2)
## Predicted.Sale.Price2 <- fitted(houseLM2)
Predicted.Sale.Price2 <- predict(houseLM2, newdata=regress.vars.df)
summary(houseLM2)


## Fits a second degree polynomial to fit all variables
houseLM3 <- lm(SalePrice ~ . +.^2, data=regress.vars.df)
houseLM3
summary(houseLM3)
##Predicted.Sale.Price3 <- fitted(houseLM3)
Predicted.Sale.Price3 <- predict(houseLM3, newdata=regress.vars.df)
plot(SalePrice ~ ., data=regress.vars.df)

## Extracts the p-values for each variable from the summary and keeps only those that are significant at the .05 level
pvals <- summary(houseLM2)$coefficients[,4]
sig.vars <- pvals[pvals < .05]
sig.vars.names <- names(sig.vars)
sig.vars.names <- c(sig.vars.names, "SalePrice")

## Fits a multiple regression model ONLY to those variables 
## previously determined to be significantly correlated with Sale Price
sig.house.df <- house.df[sig.vars.names]
houseLM4 <- lm(SalePrice ~ ., data=sig.house.df)
houseLM4
summary(houseLM4)
##Predicted.Sale.Price4 <- fitted(houseLM4)
Predicted.Sale.Price4 <- predict(houseLM4, newdata=sig.house.df)
plot(SalePrice ~ ., data=sig.house.df)

## Fits a second degree polynomial ONLY to those variables
## previously determiend to be significantly correlated with Sale Price
houseLM5 <- lm(SalePrice ~ . + .^2, data=sig.house.df)
houseLM5
summary(houseLM5)
Predicted.Sale.Price5 <- fitted(houseLM5)
Predicted.Sale.Price5 <- predict(houseLM5, newdata=sig.house.df)


summary(houseLM1)$r.squared
summary(houseLM2)$r.squared
summary(houseLM3)$r.squared
summary(houseLM4)$r.squared
summary(houseLM5)$r.squared


comparison.df <- data.frame(Id, SalePrice, Predicted.Sale.Price, Predicted.Sale.Price2,
                            Predicted.Sale.Price3, Predicted.Sale.Price4, Predicted.Sale.Price5)

## Sets the NA predicted Sales Prices as the average of the predicted values of the model
for(i in 3:ncol(comparison.df)){
  comparison.df[is.na(comparison.df[,i]), i] <- mean(comparison.df[,i], na.rm = TRUE)
}
  
comparison.df
comparison.df.copy <- comparison.df

## Adds the residuals of each predictive model to the copied dataframe
comparison.df.copy$Mod5Res <- (SalePrice - Predicted.Sale.Price5)

plot(comparison.df.copy$SalePrice, comparison.df.copy$Mod5Res, ylab="Residuals", xlab="Sale Price", main="Model 5")
abline(0, 0)

## uses test data to predict sales price
test.df = read.csv("test.csv")
test.df[1:3, ]

## picks out quantitative variables
test.num.vars <- sapply(test.df, is.numeric)
test.num.df <- test.df[test.num.vars]

## Applies the houseLM5 model to the test data and handles NA columns
test.predictions <- predict(houseLM5, newdata=test.num.df)

rm(try)
try <- data.frame(seq(1461, 1461 + length(test.predictions) - 1))
try$SalePrice <- test.predictions
names(try) <- c("Id", "SalePrice")

try[is.na(try[, 2]), 2] <- mean(try[, 2], na.rm=TRUE)


write.csv(try, file = "TestPredictions.csv", row.names=FALSE)

