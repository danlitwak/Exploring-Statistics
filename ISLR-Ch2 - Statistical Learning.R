######################################################################################################################################################################################


##  Question 8

tab <- matrix(c(0,3,0,2,0,0,0,1,3,0,1,2,-1,0,1,1,1,1), ncol=3, byrow=TRUE)
tab
dist <- round(sqrt(tab[,1]^2+tab[,2]^2+tab[,3]^2),2)
NN <- data.frame(dist, Y = c("Red", "Red", "Red", "Green", "Green", "Red"))
NN
NN[order(NN$dist),]

college <- read.csv("College.csv")
names(college)
attach(college)
rownames(college) = college[,1]
college <- college[,-1]
summary(college)
pairs(college[,1:10])
college$Private <- as.factor(college$Private)
plot(college$Private, college$Outstate, col="green", xlab="Private", ylab="Out of State", main="Out of State students by Private School")

Elite <- rep("No", nrow(college))
Elite[college$Top10perc > 50] ="Yes"
Elite <- as.factor(Elite)
college <- data.frame(college, Elite)
summary(college$Elite)
college
plot(Elite, Outstate, xlab="Elite", ylab="Outstate", main="Out of State Students by Elite University")
names(college)
par(mfrow=c(2,2))
hist(Terminal)
hist(PhD)
hist(Grad.Rate)
hist(Books)


## Question 9

Auto <- read.csv("Auto.csv", header=TRUE, na.strings="?")
Auto <- na.omit(Auto)
attach(Auto)
names(Auto)
range(Auto[,"mpg"])
range(Auto[,"cylinders"])
range(Auto[,"displacement"])
range(Auto[,"horsepower"])
range(Auto[,"weight"])
range(Auto[,"acceleration"])
range(Auto[,"year"])

sapply(Auto[, 1:7], range)
sapply(Auto[, 1:7], mean)
sapply(Auto[, 1:7], sd)

Auto <- Auto[-(10:85), ]

sapply(Auto[, 1:7], range)
sapply(Auto[, 1:7], mean)
sapply(Auto[, 1:7], sd)

library(corrplot)
par(mfrow=c(1,1))
correlations <- cor(Auto[ , 1:7])
corrplot(correlations, type="lower")


## Question 10

library(MASS)
Boston
?Boston
names(Boston)
pairs(Boston)
correlations <- cor(Boston[,])
corrplot(correlations, type="lower")
range(Boston[,"crim"])
range(Boston[,"tax"])
range(Boston[,"ptratio"])
Boston$chas <- as.factor(Boston$chas)
summary(Boston$chas)
summary(Boston$ptratio)
names(Boston)

min(Boston$medv)
which(Boston$medv == 5)
minRow <- which(Boston$medv == 5)[1]
Boston[minRow,]
summary(Boston)
summary(Boston$rm>7)
summary(Boston$rm>8)

eight.or.more <- which(Boston$rm > 8)
Boston[eight.or.more,]
boxplot(Boston[eight.or.more, "crim"], Boston$crim, ylab="Crime per Capita",
        names=c("8+","<8"))
boxplot(Boston[eight.or.more, "zn"], Boston$zn, ylab="Prop. of Residential Land Zoned for > 25000 sq. ft",
        names=c("8+","<8"))
boxplot(Boston[eight.or.more, "indus"], Boston$indus, ylab="Industrial",
        names=c("8+","<8"))
boxplot(Boston[eight.or.more, "nox"], Boston$nox, ylab="Nitrogen Oxide ppm",
        names=c("8+","<8"))
boxplot(Boston[eight.or.more, "age"], Boston$age, ylab="Age of Neighborhood",
        names=c("8+","<8"))
boxplot(Boston[eight.or.more, "rad"], Boston$rad, ylab="Accessability to Highways",
        names=c("8+","<8"))
boxplot(Boston[eight.or.more, "tax"], Boston$tax, ylab="Tax Rate",
        names=c("8+","<8"))
boxplot(Boston[eight.or.more, "ptratio"], Boston$ptratio, ylab="Pupil-to-Teacher Ratio",
        names=c("8+","<8"))
boxplot(Boston[eight.or.more, "black"], Boston$black, ylab="Proportion of Black Residents",
        names=c("8+","<8"))
boxplot(Boston[eight.or.more, "lstat"], Boston$lstat, ylab="Lower Status %",
        names=c("8+","<8"))
boxplot(Boston[eight.or.more, "medv"], Boston$medv, ylab="Median Home Value",
        names=c("8+","<8"))
