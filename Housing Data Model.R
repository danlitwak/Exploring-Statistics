library(Hmisc)
require(dplyr)
require(data.table)
require(car)
require(caret)
require(Metrics)
require(randomForest)
require(xgboost)

##  We will be modeling housing data based on a number of variables.  This data must first be organized and cleaned.
##  We will train three different models.  A linear model, a random forest model, and xgboost
##  The best model will be chosen to predict other house prices.  




train <- read.csv("train.csv", stringsAsFactors = FALSE)
test <- read.csv("test.csv", stringsAsFactors = FALSE)

## All variable names, numerical variable names, categorical variable names
names(train)
quant <- names(train[sapply(train, is.numeric)])
cat <- names(train[! sapply(train, is.numeric)])

## Clean the data.  
## Check the levels of each variable and turn them into binary variables when possible.
## When there are more than 2 distinct levels, variables are grouped by the average SalePrice of
## homes sold with that attribute, as an ordinal variable.
table(train$Street)
train$paved[train$Street == "Pave"] <- 1
train$paved[train$Street != "Pave"] <- 0


table(train$LotShape)
train$regshape[train$LotShape == "Reg"] <- 1
train$regshape[train$LotShape != "Reg"] <- 0


table(train$LandContour)
train$flat[train$LandContour == "Lvl"] <- 1
train$flat[train$LandContour != "Lvl"] <- 0

table(train$Utilities)
train$pubutil[train$Utilities == "AllPub"] <- 1
train$pubutil[train$Utilities != "AllPub"] <- 0


table(train$LotConfig)
train$culdesac_fr3[train$LandSlope %in% c("CulDSac", "FR3")] <- 1
train$culdesac_fr3[!train$LandSlope %in% c("CulDSac", "FR3")] <- 0


table(train$LandSlope)
train$gentle_slope[train$LandSlope == "Gtl"] <- 1
train$gentle_slope[train$LandSlope != "Gtl"] <- 0

table(train$Neighborhood)
nbhdprice <- summarize(group_by(train, Neighborhood),
                       mean(SalePrice, na.rm=T))

nbhdprice_lo <- filter(nbhdprice, nbhdprice$`mean(SalePrice, na.rm = T)` < 140000)
nbhdprice_med <- filter(nbhdprice, nbhdprice$`mean(SalePrice, na.rm = T)` < 200000 &
                          nbhdprice$`mean(SalePrice, na.rm = T)` >= 140000 )
nbhdprice_hi <- filter(nbhdprice, nbhdprice$`mean(SalePrice, na.rm = T)` >= 200000)
 
train$nbhd_price_level[train$Neighborhood %in% nbhdprice_lo$Neighborhood] <- 1
train$nbhd_price_level[train$Neighborhood %in% nbhdprice_med$Neighborhood] <- 2
train$nbhd_price_level[train$Neighborhood %in% nbhdprice_hi$Neighborhood] <- 3

# table(train$Condition1)
# summarize(group_by(train, Condition1),
#           mean(SalePrice, na.rm=TRUE))

train$pos_features_1[train$Condition1 %in% c("PosA", "PosN")] <- 1
train$pos_features_1[!train$Condition1 %in% c("PosA", "PosN")] <- 0


train$pos_features_2[train$Condition2 %in% c("PosA", "PosN")] <- 1
train$pos_features_2[!train$Condition2 %in% c("PosA", "PosN")] <- 0


table(train$BldgType)
train$twnhs_end_or_1fam[train$BldgType %in% c("1Fam", "TwnhsE")] <- 1
train$twnhs_end_or_1fam[!train$BldgType %in% c("1Fam", "TwnhsE")] <- 0


table(train$HouseStyle)
housestyle_price <- summarize(group_by(train, HouseStyle),
                              mean(SalePrice, na.rm=T))

housestyle_lo <- filter(housestyle_price, housestyle_price$`mean(SalePrice, na.rm = T)` < 140000)
housestyle_med <- filter(housestyle_price, housestyle_price$`mean(SalePrice, na.rm = T)` < 200000 &
                           housestyle_price$`mean(SalePrice, na.rm = T)` >= 140000 )
housestyle_hi <- filter(housestyle_price, housestyle_price$`mean(SalePrice, na.rm = T)` >= 200000)

train$house_style_level[train$HouseStyle %in% housestyle_lo$HouseStyle] <- 1
train$house_style_level[train$HouseStyle %in% housestyle_med$HouseStyle] <- 2
train$house_style_level[train$HouseStyle %in% housestyle_hi$HouseStyle] <- 3

table(train$RoofStyle)
train$roof_hip_shed[train$RoofStyle %in% c("Hip", "Shed")] <- 1
train$roof_hip_shed[!train$RoofStyle %in% c("Hip", "Shed")] <- 0

table(train$RoofMatl)
roofmat1_price <- summarize(group_by(train, RoofMatl),
                            mean(SalePrice, na.rm=TRUE))
train$roof_matl_hi[train$RoofMatl %in% c("Membran", "WdShake", "WdShngl")] <- 1
train$roof_matl_hi[!train$RoofMatl %in% c("Membran", "WdShake", "WdShngl")] <- 0


table(train$Exterior1st)
ext_price <- summarize(group_by(train, Exterior1st),
                       mean(SalePrice, na.rm=TRUE))
matl_lo_1 <- filter(ext_price, ext_price$`mean(SalePrice, na.rm = TRUE)` < 140000)
matl_med_1 <- filter(ext_price, ext_price$`mean(SalePrice, na.rm = TRUE)` >= 140000 
                     & ext_price$`mean(SalePrice, na.rm = TRUE)` < 200000)
matl_hi_1 <- filter(ext_price, ext_price$`mean(SalePrice, na.rm = TRUE)` > 200000)

train$exterior_1[train$Exterior1st %in% matl_lo_1$Exterior1st] <- 1
train$exterior_1[train$Exterior1st %in% matl_med_1$Exterior1st] <- 2
train$exterior_1[train$Exterior1st %in% matl_hi_1$Exterior1st] <- 3


table(train$Exterior2nd)
ext_price_2 <- summarize(group_by(train, Exterior2nd),
                       mean(SalePrice, na.rm=TRUE))
matl_lo_2 <- filter(ext_price_2, ext_price_2$`mean(SalePrice, na.rm = TRUE)` < 140000)
matl_med_2 <- filter(ext_price_2, ext_price_2$`mean(SalePrice, na.rm = TRUE)` >= 140000 
                     & ext_price_2$`mean(SalePrice, na.rm = TRUE)` < 200000)
matl_hi_2 <- filter(ext_price_2, ext_price_2$`mean(SalePrice, na.rm = TRUE)` > 200000)

train$exterior_2[train$Exterior2nd %in% matl_lo_2$Exterior2nd] <- 1
train$exterior_2[train$Exterior2nd %in% matl_med_2$Exterior2nd] <- 2
train$exterior_2[train$Exterior2nd %in% matl_hi_2$Exterior2nd] <- 3


table(train$MasVnrType)
price <- summarize(group_by(train, MasVnrType),
                   mean(SalePrice, na.rm=TRUE))
train$exterior_mason_1[train$MasVnrType %in% c("Stone", "BrkFace") | is.na(train$MasVnrType)] <- 1
train$exterior_mason_1[!train$MasVnrType %in% c("Stone", "BrkFace") | is.na(train$MasVnrType)] <- 0


table(train$ExterQual)
price <- summarize(group_by(train, ExterQual),
                   mean(SalePrice, na.rm=TRUE))
train$exterior_cond[train$ExterQual == "Ex"] <- 4
train$exterior_cond[train$ExterQual == "Gd"] <- 3
train$exterior_cond[train$ExterQual == "TA"] <- 2
train$exterior_cond[train$ExterQual == "Fa"] <- 1


table(train$ExterQual)
price <- summarize(group_by(train, ExterCond),
                   mean(SalePrice, na.rm=TRUE))
train$exterior_cond2[train$ExterQual == "Ex"] <- 5
train$exterior_cond2[train$ExterQual == "Gd"] <- 4
train$exterior_cond2[train$ExterQual == "TA"] <- 3
train$exterior_cond2[train$ExterQual == "Fa"] <- 2
train$exterior_cond2[train$ExterQual == "Po"] <- 1


table(train$Foundation)
price <- summarize(group_by(train, Foundation),
                   mean(SalePrice, na.rm=TRUE))
train$found_concrete[train$Foundation == "PConc"] <- 1
train$found_concrete[!train$Foundation == "PConc"] <- 0


table(train$BsmtQual)
price <- summarize(group_by(train, BsmtQual),
                   mean(SalePrice, na.rm=TRUE))
train$bsmt_cond1[train$BsmtQual == "Ex"] <- 5
train$bsmt_cond1[train$BsmtQual == "Gd"] <- 4
train$bsmt_cond1[train$BsmtQual == "TA"] <- 3
train$bsmt_cond1[train$BsmtQual == "Fa"] <- 2
train$bsmt_cond1[is.na(train$BsmtQual)] <- 1


price <- summarize(group_by(train, BsmtCond),
                   mean(SalePrice, na.rm=TRUE))
train$bsmt_cond2[train$BsmtCond == "Ex"] <- 5
train$bsmt_cond2[train$BsmtCond == "Gd"] <- 4
train$bsmt_cond2[train$BsmtCond == "TA"] <- 3
train$bsmt_cond2[train$BsmtCond == "Fa"] <- 2
train$bsmt_cond2[train$BsmtCond == "Po"] <- 2
train$bsmt_cond2[is.na(train$BsmtCond)] <- 1

price <- summarize(group_by(train, BsmtExposure),
                   mean(SalePrice, na.rm=TRUE))
train$bsmt_exp[train$BsmtExposure == "Gd"] <- 5
train$bsmt_exp[train$BsmtExposure == "Av"] <- 4
train$bsmt_exp[train$BsmtExposure == "Mn"] <- 3
train$bsmt_exp[train$BsmtExposure == "No"] <- 2
train$bsmt_exp[is.na(train$BsmtExposure)] <- 1


price <- summarize(group_by(train, BsmtFinType1),
                   mean(SalePrice, na.rm=TRUE))
train$bsmt_fin1[train$BsmtFinType1 == "GLQ"] <- 5
train$bsmt_fin1[train$BsmtFinType1 == "Unf"] <- 4
train$bsmt_fin1[train$BsmtFinType1 == "ALQ"] <- 3
train$bsmt_fin1[train$BsmtFinType1 %in% c("BLQ", "Rec", "LwQ")] <- 2
train$bsmt_fin1[is.na(train$BsmtFinType1)] <- 1


price <- summarize(group_by(train, BsmtFinType2),
                   mean(SalePrice, na.rm=T))
train$bsmt_fin2[train$BsmtFinType2 == "ALQ"] <- 6
train$bsmt_fin2[train$BsmtFinType2 == "Unf"] <- 5
train$bsmt_fin2[train$BsmtFinType2 == "GLQ"] <- 4
train$bsmt_fin2[train$BsmtFinType2 %in% c("Rec", "LwQ")] <- 3
train$bsmt_fin2[train$BsmtFinType2 == "BLQ"] <- 2
train$bsmt_fin2[is.na(train$BsmtFinType2)] <- 1


price <- summarize(group_by(train, Heating),
                   mean(SalePrice, na.rm=T))
train$gasheat[train$Heating %in% c("GasA", "GasW")] <- 1
train$gasheat[!train$Heating %in% c("GasA", "GasW")] <- 0


price <- summarize(group_by(train, HeatingQC),
                   mean(SalePrice, na.rm=T))
train$heatqual[train$HeatingQC == "Ex"] <- 5
train$heatqual[train$HeatingQC == "Gd"] <- 4
train$heatqual[train$HeatingQC == "TA"] <- 3
train$heatqual[train$HeatingQC == "Fa"] <- 2
train$heatqual[train$HeatingQC == "Po"] <- 1


price <- summarize(group_by(train, CentralAir),
                   mean(SalePrice, na.rm=T))
train$air[train$CentralAir == "Y"] <- 1
train$air[train$CentralAir == "N"] <- 0


price <- summarize(group_by(train, Electrical),
                   mean(SalePrice, na.rm=T))
train$standard_electric[train$Electrical == "SBrkr" | is.na(train$Electrical)] <- 1
train$standard_electric[!train$Electrical == "SBrkr" & !is.na(train$Electrical)] <- 0


price <- summarize(group_by(train, KitchenQual),
                   mean(SalePrice, na.rm=T))
train$kitchen[train$KitchenQual == "Ex"] <- 4
train$kitchen[train$KitchenQual == "Gd"] <- 3
train$kitchen[train$KitchenQual == "TA"] <- 2
train$kitchen[train$KitchenQual == "Fa"] <- 1


price <- summarize(group_by(train, FireplaceQu),
                   mean(SalePrice, na.rm=T))
train$fire[train$FireplaceQu == "Ex"] <- 5
train$fire[train$FireplaceQu == "Gd"] <- 4
train$fire[train$FireplaceQu == "TA"] <- 3
train$fire[train$FireplaceQu == "Fa"] <- 2
train$fire[train$FireplaceQu == "Po" | is.na(train$FireplaceQu)] <- 1


price <- summarize(group_by(train, GarageType),
                   mean(SalePrice, na.rm=T))
train$gar_attach[train$GarageType %in% c("Attchd", "BuiltIn")] <- 1
train$gar_attach[!train$GarageType %in% c("Attchd", "BuiltIn")] <- 0


price <- summarize(group_by(train, GarageFinish),
                   mean(SalePrice, na.rm=T))
train$gar_finish[train$GarageFinish %in% c("Fin", "RFn")] <- 1
train$gar_finish[!train$GarageFinish %in% c("Fin", "RFn")] <- 0


price <- summarize(group_by(train, GarageQual),
                   mean(SalePrice, na.rm=T))
train$garqual[train$GarageQual == "Ex"] <- 5
train$garqual[train$GarageQual == "Gd"] <- 4
train$garqual[train$GarageQual == "TA"] <- 3
train$garqual[train$GarageQual == "Fa"] <- 2
train$garqual[train$GarageQual == "Po" | is.na(train$GarageQual)] <- 1


price <- summarize(group_by(train, GarageCond),
                   mean(SalePrice, na.rm=T))
train$garqual2[train$GarageCond == "Ex"] <- 5
train$garqual2[train$GarageCond == "Gd"] <- 4
train$garqual2[train$GarageCond == "TA"] <- 3
train$garqual2[train$GarageCond == "Fa"] <- 2
train$garqual2[train$GarageCond == "Po" | is.na(train$GarageCond)] <- 1


price <- summarize(group_by(train, PavedDrive),
                   mean(SalePrice, na.rm=T))
train$paved_drive[train$PavedDrive == "Y"] <- 1
train$paved_drive[!train$PavedDrive != "Y"] <- 0
train$paved_drive[is.na(train$paved_drive)] <- 0


price <- summarize(group_by(train, Functional),
                   mean(SalePrice, na.rm=T))
train$housefunction[train$Functional %in% c("Typ", "Mod")] <- 1
train$housefunction[!train$Functional %in% c("Typ", "Mod")] <- 0


price <- summarize(group_by(train, PoolQC),
                   mean(SalePrice, na.rm=T))
train$pool_good[train$PoolQC %in% c("Ex")] <- 1
train$pool_good[!train$PoolQC %in% c("Ex")] <- 0


price <- summarize(group_by(train, Fence),
                   mean(SalePrice, na.rm=T))
train$priv_fence[train$Fence %in% c("GdPrv")] <- 1
train$priv_fence[!train$Fence %in% c("GdPrv")] <- 0


price <- summarize(group_by(train, MiscFeature),
                   mean(SalePrice, na.rm=T))
#This doesn't seem worth using at the moment. May adjust later.


price <- summarize(group_by(train, SaleType),
                   mean(SalePrice, na.rm=T))
# price[order(price$`mean(SalePrice, na.rm = T)`),]
train$sale_cat[train$SaleType %in% c("New", "Con")] <- 5
train$sale_cat[train$SaleType %in% c("CWD", "ConLI")] <- 4
train$sale_cat[train$SaleType %in% c("WD")] <- 3
train$sale_cat[train$SaleType %in% c("COD", "ConLw", "ConLD")] <- 2
train$sale_cat[train$SaleType %in% c("Oth")] <- 1


price <- summarize(group_by(train, SaleCondition),
                   mean(SalePrice, na.rm=T))
# price[order(price$`mean(SalePrice, na.rm = T)`),]
train$sale_cond[train$SaleCondition %in% c("Partial")] <- 4
train$sale_cond[train$SaleCondition %in% c("Normal", "Alloca")] <- 3
train$sale_cond[train$SaleCondition %in% c("Family","Abnorml")] <- 2
train$sale_cond[train$SaleCondition %in% c("AdjLand")] <- 1


price <- summarize(group_by(train, MSZoning),
                   mean(SalePrice, na.rm=T))
# price[order(price$`mean(SalePrice, na.rm = T)`),]
train$zone[train$MSZoning %in% c("FV")] <- 4
train$zone[train$MSZoning %in% c("RL")] <- 3
train$zone[train$MSZoning %in% c("RH","RM")] <- 2
train$zone[train$MSZoning %in% c("C (all)")] <- 1


price <- summarize(group_by(train, Alley),
                   mean(SalePrice, na.rm=T))
# price[order(price$`mean(SalePrice, na.rm = T)`),]

table(train$Alley)
train$alleypave[train$Alley %in% c("Pave")] <- 1
train$alleypave[!train$Alley %in% c("Pave")] <- 0


## Many variables in the dataframe have been made numeric and their categorical attributes are not needed anymore.  
## These will be removed

train$Street <- NULL
train$LotShape <- NULL
train$LandContour <- NULL
train$Utilities <- NULL
train$LotConfig <- NULL
train$LandSlope <- NULL
train$Neighborhood <- NULL
train$Condition1 <- NULL
train$Condition2 <- NULL
train$BldgType <- NULL
train$HouseStyle <- NULL
train$RoofStyle <- NULL
train$RoofMatl <- NULL

train$Exterior1st <- NULL
train$Exterior2nd <- NULL
train$MasVnrType <- NULL
train$ExterQual <- NULL
train$ExterCond <- NULL

train$Foundation <- NULL
train$BsmtQual <- NULL
train$BsmtCond <- NULL
train$BsmtExposure <- NULL
train$BsmtFinType1 <- NULL
train$BsmtFinType2 <- NULL

train$Heating <- NULL
train$HeatingQC <- NULL
train$CentralAir <- NULL
train$Electrical <- NULL
train$KitchenQual <- NULL
train$FireplaceQu <- NULL

train$GarageType <- NULL
train$GarageFinish <- NULL
train$GarageQual <- NULL
train$GarageCond <- NULL
train$PavedDrive <- NULL

train$Functional <- NULL
train$PoolQC <- NULL
train$Fence <- NULL
train$MiscFeature <- NULL
train$SaleType <- NULL
train$SaleCondition <- NULL
train$MSZoning <- NULL
train$Alley <- NULL

## Are there any variables that interact with each other.  We can examine these by checking the correlation
## between variables.
require(corrplot)

## Break up the corrplot into several in order to more easily see associations
correlations <- cor(train[ , c(5, 6, 7, 8, 16:25)], use="everything")
# corrplot.mixed(correlations, sig.level=0.01, insig="blank")
corrplot(correlations, method="circle", type="lower", sig.level=0.01, insig="blank")

correlations <- cor(train[ , c(5, 6, 7, 8, 26:35)], use="everything")
corrplot(correlations, method="circle", type="lower", sig.level=0.01, insig="blank")

correlations <- cor(train[ , c(5, 6, 7, 8, 66:75)], use="everything")
corrplot(correlations, method="circle", type="lower", sig.level=0.01, insig="blank")

##  Some obvious things pop out from these correlations, as well as some interesting relationships.
##          Quality has increased as time passes.  Living Areas have increased as time passes,
##          most likely related to more full bathrooms.  Older houses are generally in worse condition.
##          Garages tend to hold more cars in higher quality homes, which tend to be newer.  
##          Year built is negatively correlated with enclosed porches, suggesting that enclosed porches are less
##          common now.  
##          Newer kitchens tend to be higher quality.  Newer homes tend to have garage attached and a nicer finish.


##  This builds a matrix of scatterplots of the chosen variables.  We know which variables are correlated.
##  The scatterplot matrix makes it simpler to tell how large the effect size is.  Is it obvious?
pairs(~YearBuilt+OverallQual+TotalBsmtSF+GrLivArea, data=train, main="Simple Scatterplot Matrix")


## Scatterplot creates boxplots with marginal boxplots to easily view the distribution of the axis data.  



scatterplot(SalePrice ~ YearBuilt, data=train, xlab="Year Built", ylab="Sale Price", grid=FALSE, smooth=FALSE, regLine=FALSE)
# scatterplot(SalePrice ~ YearBuilt, data=train, xlab="Year Built", ylab="Sale Price", grid=FALSE)
scatterplot(SalePrice ~ YrSold, data=train, xlab="Year Sold", ylab="Sale Price", grid=FALSE)
scatterplot(SalePrice ~ X1stFlrSF, data=train, xlab="X1stFlrSF", ylab="Sale Price", grid=FALSE)

##  There are still some NA's in the data that must be cleaned.  Which columns have NA?
na_cols <- colnames(train[colSums(is.na(train)) > 0])
na_cols

train$GarageYrBlt[is.na(train$GarageYrBlt)] <- 0
train$MasVnrArea[is.na(train$MasVnrArea)] <- 0
train$LotFrontage[is.na(train$LotFrontage)] <- 0

na_cols <- colnames(train[colSums(is.na(train)) > 0])
na_cols

##  Now there shouldn't be any pesky NA's to have to deal with in the data.  
##  Based on what was found on the scatterplot matrix, we also want to include the interactions found

train$year_qual <- train$YearBuilt*train$OverallQual  #overall condition
train$year_r_qual <- train$YearRemodAdd*train$OverallQual #quality x remodel
train$qual_bsmt <- train$OverallQual*train$TotalBsmtSF #quality x basement size

train$livarea_qual <- train$OverallQual*train$GrLivArea #quality x living area
train$qual_bath <- train$OverallQual*train$FullBath #quality x baths
train$qual_ext <- train$OverallQual*train$exterior_cond #quality x exterior


##  Ok, the data is prepped and cleaned.  Now we are ready to get into modeling.

outcome <- train$SalePrice

partition <- createDataPartition(y=outcome,
                                 p=0.5,
                                 list=FALSE)
training <- train[partition, ]
testing <- train[-partition, ]


##  We will try three models.  A linear model, a random forest model, and xgboost

lm_model_0 <- lm(SalePrice ~ ., data=training)
summary(lm_model_0)

##  The first thing to notice is that many variables are not statistically significant, so we should be able to 
##  drop them from the model.  The next thing is that R^2 is 0.887, relatively high.


lm_model_1 <- lm(SalePrice ~ MSSubClass + LotArea + OverallCond + MasVnrArea + BsmtFinSF1 + BsmtFinSF2 + BsmtUnfSF + X1stFlrSF +
     X2ndFlrSF + BsmtFullBath + FullBath + BedroomAbvGr + GarageYrBlt + GarageCars + GarageArea + WoodDeckSF + 
     EnclosedPorch + ScreenPorch + PoolArea + YrSold + paved + pos_features_1 + roof_matl_hi + exterior_mason_1 + 
     exterior_cond + bsmt_cond2 + bsmt_exp + fire + housefunction + pool_good + sale_cond + zone + qual_bsmt + 
     qual_bath + qual_ext, data = training)
summary(lm_model_1)


lm_model_2 <- lm(SalePrice ~ MSSubClass + LotArea + OverallCond +
                   MasVnrArea + BsmtFinSF1 + BsmtFinSF2 + BsmtUnfSF +
                   X1stFlrSF + X2ndFlrSF + FullBath + BedroomAbvGr + 
                   GarageCars + PoolArea + exterior_mason_1 + 
                   exterior_cond + bsmt_exp + housefunction + 
                   pool_good + sale_cond + zone + qual_bsmt + 
                   qual_bath + qual_ext, data = training)
summary(lm_model_2)


lm_model_3 <- lm(SalePrice ~ MSSubClass + LotArea + OverallCond +
                   MasVnrArea + BsmtFinSF1 + BsmtFinSF2 + BsmtUnfSF +
                   X1stFlrSF + X2ndFlrSF + FullBath + BedroomAbvGr + 
                   GarageCars + PoolArea + exterior_cond + bsmt_exp + 
                   pool_good + sale_cond + zone + qual_bsmt + 
                   qual_bath + qual_ext, data = training)
summary(lm_model_3)

##  Let's check how close the model is to the testing data.  We will use Root Mean Square Error (RMSE)
##  We will take the log of the RMSE 

prediction <- predict(lm_model_3, testing, type="response")
model_output <- cbind(testing, prediction)

model_output$log_prediction <- log(model_output$prediction)
model_output$log_SalePrice <- log(model_output$SalePrice)


lm_rmse <- rmse(model_output$log_SalePrice, model_output$log_prediction)
lm_rmse

##  So, we have the RMSE of the linear model.  Let's see if any other model improves on this.
##  Now let's try a Random Forest
##  Random Forest uses an ensemble of decision trees to create a model.  This means it will
##  construct a decision tree by choosing from a random subset of variables at each node.  
##  After constructing one decision tree, it will create many more and in the end summarize the results by
##  taking an "average model" of all models constructed.

##  Make the model using the training set
rf_model_1 <- randomForest(SalePrice ~ ., data=training)

##  Apply the model to the test set and its error with RMSE
prediction <- predict(rf_model_1, testing)
model_output <- cbind(testing, prediction)

model_output$log_prediction <- log(model_output$prediction)
model_output$log_SalePrice <- log(model_output$SalePrice)

rf_rmse <- rmse(model_output$log_SalePrice, model_output$log_prediction)
rf_rmse

##  The random forest is looking like the better model.  Let's try one last model: xgboost
##  xgboost: extreme gradient boosting - xgboost is also a tree ensemble, like a random forest.
##  The difference arises in how we train the models.  xgboost is focused on optimizing the training loss
##  (aka how well a model predicts the data) and the regularization (aka not overfitting the data, simple model)

training$log_SalePrice <- log(training$SalePrice)
testing$log_SalePrice <- log(testing$SalePrice)

##  Create matrixes from data frames
trainData <- as.matrix(training, rownames.force=NA)
testData <- as.matrix(testing, rownames.force=NA)

##  Turn matrixes into sparse matrixes
train2 <- as(trainData, "sparseMatrix")
test2 <- as(testData, "sparseMatrix")

colnames(train2)
##  Cross Validate the model

vars <- c(2:37, 39:86) #choose the columns we want to use in the prediction matrix

trainD <- xgb.DMatrix(data = train2[ , vars], label=train2[ , "SalePrice"]) #Convert to xgb.Dmatrix format

cv.sparse <- xgb.cv(data=trainD,
                    nrounds=600,
                    min_child_weight = 0,
                    max_depth = 10, 
                    eta = 0.02, 
                    subsample = .7, 
                    colsample_bytree = 0.7, 
                    booster ="gbtree", 
                    eval_metric = "rmse", 
                    verbose = TRUE, 
                    print_every_n = 50, 
                    nfold = 4,
                    nthread = 2, 
                    objective = "reg:linear")

## Train the model

## Choose the parameters for the model
param <- list(colsample_bytree = 0.7,
              subsample=0.7,
              booster = "gbtree",
              max_depth = 10, 
              eta = 0.02, 
              eval_metric = "rmse",
              objective = "reg:linear")

## Train the model using previous parameters
bstSparse <- xgb.train(params = param, 
                       data = trainD, 
                       nrounds = 600, 
                       watchlist = list(train = trainD),
                       verbose = TRUE,
                       print_every_n = 50,
                       nthread = 2)

##  Predict and test the RMSE
testD <- xgb.DMatrix(data = test2[ , vars])
## Column naems must match the inputs EXACTLY
prediction <- predict(bstSparse, testD) #Make the prediction based on the half of training data set

##  Put testing prediction and test dataset all together
test3 <- as.data.frame(as.matrix(test2))
prediction <- as.data.frame(as.matrix(prediction))
colnames(prediction) <- "prediction"
model_output <- cbind(test3, prediction)

model_output$log_prediction <- log(model_output$prediction)
model_output$log_SalePrice <- log(model_output$SalePrice)

## Check the RMSE
xgb_rmse <- rmse(model_output$log_SalePrice, model_output$log_prediction)


##  The RMSE for xgboost is even better than that of the linear model AND Random Forest
lm_rmse
rf_rmse
xgb_rmse

##  Now that we've trained and validated the data, we will use the full training sample (train + validation)
##  to retrain the model
rm(bstSparse)

##  Create matrixes from data frames
retrainData <- as.matrix(train, rownames.force=NA)


##  Turn matrixes into sparse matrixes
retrain <- as(retrainData, "sparseMatrix")

param <- list(colsample_bytree = 0.7,
              subsample=0.7,
              booster = "gbtree",
              max_depth = 10, 
              eta = 0.02, 
              eval_metric = "rmse",
              objective = "reg:linear")

retrainD <- xgb.DMatrix(data = retrain[ , vars], label=retrain[ , "SalePrice"]) #Convert to xgb.Dmatrix format

## ReTrain the model

## Choose the parameters for the model

## Train the model using previous parameters
bstSparse <- xgb.train(params = param, 
                       data = retrainD, 
                       nrounds = 600, 
                       watchlist = list(train = trainD),
                       verbose = TRUE,
                       print_every_n = 50,
                       nthread = 2)

##  Ok, the final model has been built.  Now we will apply it to the test dataset.  First, we must 
##  clean up the test dataset so that it is formatted just like the training dataset.

test$paved[test$Street == "Pave"] <- 1
test$paved[test$Street != "Pave"] <- 0


table(test$LotShape)
test$regshape[test$LotShape == "Reg"] <- 1
test$regshape[test$LotShape != "Reg"] <- 0


table(test$LandContour)
test$flat[test$LandContour == "Lvl"] <- 1
test$flat[test$LandContour != "Lvl"] <- 0

table(test$Utilities)
test$pubutil[test$Utilities == "AllPub"] <- 1
test$pubutil[test$Utilities != "AllPub"] <- 0


table(test$LotConfig)
test$culdesac_fr3[test$LandSlope %in% c("CulDSac", "FR3")] <- 1
test$culdesac_fr3[!test$LandSlope %in% c("CulDSac", "FR3")] <- 0


table(test$LandSlope)
test$gentle_slope[test$LandSlope == "Gtl"] <- 1
test$gentle_slope[test$LandSlope != "Gtl"] <- 0

table(test$Neighborhood)
nbhdprice <- summarize(group_by(test, Neighborhood),
                       mean(SalePrice, na.rm=T))

nbhdprice_lo <- filter(nbhdprice, nbhdprice$`mean(SalePrice, na.rm = T)` < 140000)
nbhdprice_med <- filter(nbhdprice, nbhdprice$`mean(SalePrice, na.rm = T)` < 200000 &
                          nbhdprice$`mean(SalePrice, na.rm = T)` >= 140000 )
nbhdprice_hi <- filter(nbhdprice, nbhdprice$`mean(SalePrice, na.rm = T)` >= 200000)

test$nbhd_price_level[test$Neighborhood %in% nbhdprice_lo$Neighborhood] <- 1
test$nbhd_price_level[test$Neighborhood %in% nbhdprice_med$Neighborhood] <- 2
test$nbhd_price_level[test$Neighborhood %in% nbhdprice_hi$Neighborhood] <- 3

# table(test$Condition1)
# summarize(group_by(test, Condition1),
#           mean(SalePrice, na.rm=TRUE))

test$pos_features_1[test$Condition1 %in% c("PosA", "PosN")] <- 1
test$pos_features_1[!test$Condition1 %in% c("PosA", "PosN")] <- 0


test$pos_features_2[test$Condition2 %in% c("PosA", "PosN")] <- 1
test$pos_features_2[!test$Condition2 %in% c("PosA", "PosN")] <- 0


table(test$BldgType)
test$twnhs_end_or_1fam[test$BldgType %in% c("1Fam", "TwnhsE")] <- 1
test$twnhs_end_or_1fam[!test$BldgType %in% c("1Fam", "TwnhsE")] <- 0


table(test$HouseStyle)
housestyle_price <- summarize(group_by(test, HouseStyle),
                              mean(SalePrice, na.rm=T))

housestyle_lo <- filter(housestyle_price, housestyle_price$`mean(SalePrice, na.rm = T)` < 140000)
housestyle_med <- filter(housestyle_price, housestyle_price$`mean(SalePrice, na.rm = T)` < 200000 &
                           housestyle_price$`mean(SalePrice, na.rm = T)` >= 140000 )
housestyle_hi <- filter(housestyle_price, housestyle_price$`mean(SalePrice, na.rm = T)` >= 200000)

test$house_style_level[test$HouseStyle %in% housestyle_lo$HouseStyle] <- 1
test$house_style_level[test$HouseStyle %in% housestyle_med$HouseStyle] <- 2
test$house_style_level[test$HouseStyle %in% housestyle_hi$HouseStyle] <- 3

table(test$RoofStyle)
test$roof_hip_shed[test$RoofStyle %in% c("Hip", "Shed")] <- 1
test$roof_hip_shed[!test$RoofStyle %in% c("Hip", "Shed")] <- 0

table(test$RoofMatl)
roofmat1_price <- summarize(group_by(test, RoofMatl),
                            mean(SalePrice, na.rm=TRUE))
test$roof_matl_hi[test$RoofMatl %in% c("Membran", "WdShake", "WdShngl")] <- 1
test$roof_matl_hi[!test$RoofMatl %in% c("Membran", "WdShake", "WdShngl")] <- 0


table(test$Exterior1st)
ext_price <- summarize(group_by(test, Exterior1st),
                       mean(SalePrice, na.rm=TRUE))
matl_lo_1 <- filter(ext_price, ext_price$`mean(SalePrice, na.rm = TRUE)` < 140000)
matl_med_1 <- filter(ext_price, ext_price$`mean(SalePrice, na.rm = TRUE)` >= 140000 
                     & ext_price$`mean(SalePrice, na.rm = TRUE)` < 200000)
matl_hi_1 <- filter(ext_price, ext_price$`mean(SalePrice, na.rm = TRUE)` > 200000)

test$exterior_1[test$Exterior1st %in% matl_lo_1$Exterior1st] <- 1
test$exterior_1[test$Exterior1st %in% matl_med_1$Exterior1st] <- 2
test$exterior_1[test$Exterior1st %in% matl_hi_1$Exterior1st] <- 3


table(test$Exterior2nd)
ext_price_2 <- summarize(group_by(test, Exterior2nd),
                         mean(SalePrice, na.rm=TRUE))
matl_lo_2 <- filter(ext_price_2, ext_price_2$`mean(SalePrice, na.rm = TRUE)` < 140000)
matl_med_2 <- filter(ext_price_2, ext_price_2$`mean(SalePrice, na.rm = TRUE)` >= 140000 
                     & ext_price_2$`mean(SalePrice, na.rm = TRUE)` < 200000)
matl_hi_2 <- filter(ext_price_2, ext_price_2$`mean(SalePrice, na.rm = TRUE)` > 200000)

test$exterior_2[test$Exterior2nd %in% matl_lo_2$Exterior2nd] <- 1
test$exterior_2[test$Exterior2nd %in% matl_med_2$Exterior2nd] <- 2
test$exterior_2[test$Exterior2nd %in% matl_hi_2$Exterior2nd] <- 3


table(test$MasVnrType)
price <- summarize(group_by(test, MasVnrType),
                   mean(SalePrice, na.rm=TRUE))
test$exterior_mason_1[test$MasVnrType %in% c("Stone", "BrkFace") | is.na(test$MasVnrType)] <- 1
test$exterior_mason_1[!test$MasVnrType %in% c("Stone", "BrkFace") | is.na(test$MasVnrType)] <- 0


table(test$ExterQual)
price <- summarize(group_by(test, ExterQual),
                   mean(SalePrice, na.rm=TRUE))
test$exterior_cond[test$ExterQual == "Ex"] <- 4
test$exterior_cond[test$ExterQual == "Gd"] <- 3
test$exterior_cond[test$ExterQual == "TA"] <- 2
test$exterior_cond[test$ExterQual == "Fa"] <- 1


table(test$ExterQual)
price <- summarize(group_by(test, ExterCond),
                   mean(SalePrice, na.rm=TRUE))
test$exterior_cond2[test$ExterQual == "Ex"] <- 5
test$exterior_cond2[test$ExterQual == "Gd"] <- 4
test$exterior_cond2[test$ExterQual == "TA"] <- 3
test$exterior_cond2[test$ExterQual == "Fa"] <- 2
test$exterior_cond2[test$ExterQual == "Po"] <- 1


table(test$Foundation)
price <- summarize(group_by(test, Foundation),
                   mean(SalePrice, na.rm=TRUE))
test$found_concrete[test$Foundation == "PConc"] <- 1
test$found_concrete[!test$Foundation == "PConc"] <- 0


table(test$BsmtQual)
price <- summarize(group_by(test, BsmtQual),
                   mean(SalePrice, na.rm=TRUE))
test$bsmt_cond1[test$BsmtQual == "Ex"] <- 5
test$bsmt_cond1[test$BsmtQual == "Gd"] <- 4
test$bsmt_cond1[test$BsmtQual == "TA"] <- 3
test$bsmt_cond1[test$BsmtQual == "Fa"] <- 2
test$bsmt_cond1[is.na(test$BsmtQual)] <- 1


price <- summarize(group_by(test, BsmtCond),
                   mean(SalePrice, na.rm=TRUE))
test$bsmt_cond2[test$BsmtCond == "Ex"] <- 5
test$bsmt_cond2[test$BsmtCond == "Gd"] <- 4
test$bsmt_cond2[test$BsmtCond == "TA"] <- 3
test$bsmt_cond2[test$BsmtCond == "Fa"] <- 2
test$bsmt_cond2[test$BsmtCond == "Po"] <- 2
test$bsmt_cond2[is.na(test$BsmtCond)] <- 1

price <- summarize(group_by(test, BsmtExposure),
                   mean(SalePrice, na.rm=TRUE))
test$bsmt_exp[test$BsmtExposure == "Gd"] <- 5
test$bsmt_exp[test$BsmtExposure == "Av"] <- 4
test$bsmt_exp[test$BsmtExposure == "Mn"] <- 3
test$bsmt_exp[test$BsmtExposure == "No"] <- 2
test$bsmt_exp[is.na(test$BsmtExposure)] <- 1


price <- summarize(group_by(test, BsmtFinType1),
                   mean(SalePrice, na.rm=TRUE))
test$bsmt_fin1[test$BsmtFinType1 == "GLQ"] <- 5
test$bsmt_fin1[test$BsmtFinType1 == "Unf"] <- 4
test$bsmt_fin1[test$BsmtFinType1 == "ALQ"] <- 3
test$bsmt_fin1[test$BsmtFinType1 %in% c("BLQ", "Rec", "LwQ")] <- 2
test$bsmt_fin1[is.na(test$BsmtFinType1)] <- 1


price <- summarize(group_by(test, BsmtFinType2),
                   mean(SalePrice, na.rm=T))
test$bsmt_fin2[test$BsmtFinType2 == "ALQ"] <- 6
test$bsmt_fin2[test$BsmtFinType2 == "Unf"] <- 5
test$bsmt_fin2[test$BsmtFinType2 == "GLQ"] <- 4
test$bsmt_fin2[test$BsmtFinType2 %in% c("Rec", "LwQ")] <- 3
test$bsmt_fin2[test$BsmtFinType2 == "BLQ"] <- 2
test$bsmt_fin2[is.na(test$BsmtFinType2)] <- 1


price <- summarize(group_by(test, Heating),
                   mean(SalePrice, na.rm=T))
test$gasheat[test$Heating %in% c("GasA", "GasW")] <- 1
test$gasheat[!test$Heating %in% c("GasA", "GasW")] <- 0


price <- summarize(group_by(test, HeatingQC),
                   mean(SalePrice, na.rm=T))
test$heatqual[test$HeatingQC == "Ex"] <- 5
test$heatqual[test$HeatingQC == "Gd"] <- 4
test$heatqual[test$HeatingQC == "TA"] <- 3
test$heatqual[test$HeatingQC == "Fa"] <- 2
test$heatqual[test$HeatingQC == "Po"] <- 1


price <- summarize(group_by(test, CentralAir),
                   mean(SalePrice, na.rm=T))
test$air[test$CentralAir == "Y"] <- 1
test$air[test$CentralAir == "N"] <- 0


price <- summarize(group_by(test, Electrical),
                   mean(SalePrice, na.rm=T))
test$standard_electric[test$Electrical == "SBrkr" | is.na(test$Electrical)] <- 1
test$standard_electric[!test$Electrical == "SBrkr" & !is.na(test$Electrical)] <- 0


price <- summarize(group_by(test, KitchenQual),
                   mean(SalePrice, na.rm=T))
test$kitchen[test$KitchenQual == "Ex"] <- 4
test$kitchen[test$KitchenQual == "Gd"] <- 3
test$kitchen[test$KitchenQual == "TA"] <- 2
test$kitchen[test$KitchenQual == "Fa"] <- 1


price <- summarize(group_by(test, FireplaceQu),
                   mean(SalePrice, na.rm=T))
test$fire[test$FireplaceQu == "Ex"] <- 5
test$fire[test$FireplaceQu == "Gd"] <- 4
test$fire[test$FireplaceQu == "TA"] <- 3
test$fire[test$FireplaceQu == "Fa"] <- 2
test$fire[test$FireplaceQu == "Po" | is.na(test$FireplaceQu)] <- 1


price <- summarize(group_by(test, GarageType),
                   mean(SalePrice, na.rm=T))
test$gar_attach[test$GarageType %in% c("Attchd", "BuiltIn")] <- 1
test$gar_attach[!test$GarageType %in% c("Attchd", "BuiltIn")] <- 0


price <- summarize(group_by(test, GarageFinish),
                   mean(SalePrice, na.rm=T))
test$gar_finish[test$GarageFinish %in% c("Fin", "RFn")] <- 1
test$gar_finish[!test$GarageFinish %in% c("Fin", "RFn")] <- 0


price <- summarize(group_by(test, GarageQual),
                   mean(SalePrice, na.rm=T))
test$garqual[test$GarageQual == "Ex"] <- 5
test$garqual[test$GarageQual == "Gd"] <- 4
test$garqual[test$GarageQual == "TA"] <- 3
test$garqual[test$GarageQual == "Fa"] <- 2
test$garqual[test$GarageQual == "Po" | is.na(test$GarageQual)] <- 1


price <- summarize(group_by(test, GarageCond),
                   mean(SalePrice, na.rm=T))
test$garqual2[test$GarageCond == "Ex"] <- 5
test$garqual2[test$GarageCond == "Gd"] <- 4
test$garqual2[test$GarageCond == "TA"] <- 3
test$garqual2[test$GarageCond == "Fa"] <- 2
test$garqual2[test$GarageCond == "Po" | is.na(test$GarageCond)] <- 1


price <- summarize(group_by(test, PavedDrive),
                   mean(SalePrice, na.rm=T))
test$paved_drive[test$PavedDrive == "Y"] <- 1
test$paved_drive[!test$PavedDrive != "Y"] <- 0
test$paved_drive[is.na(test$paved_drive)] <- 0


price <- summarize(group_by(test, Functional),
                   mean(SalePrice, na.rm=T))
test$housefunction[test$Functional %in% c("Typ", "Mod")] <- 1
test$housefunction[!test$Functional %in% c("Typ", "Mod")] <- 0


price <- summarize(group_by(test, PoolQC),
                   mean(SalePrice, na.rm=T))
test$pool_good[test$PoolQC %in% c("Ex")] <- 1
test$pool_good[!test$PoolQC %in% c("Ex")] <- 0


price <- summarize(group_by(test, Fence),
                   mean(SalePrice, na.rm=T))
test$priv_fence[test$Fence %in% c("GdPrv")] <- 1
test$priv_fence[!test$Fence %in% c("GdPrv")] <- 0


price <- summarize(group_by(test, MiscFeature),
                   mean(SalePrice, na.rm=T))
#This doesn't seem worth using at the moment. May adjust later.


price <- summarize(group_by(test, SaleType),
                   mean(SalePrice, na.rm=T))
# price[order(price$`mean(SalePrice, na.rm = T)`),]
test$sale_cat[test$SaleType %in% c("New", "Con")] <- 5
test$sale_cat[test$SaleType %in% c("CWD", "ConLI")] <- 4
test$sale_cat[test$SaleType %in% c("WD")] <- 3
test$sale_cat[test$SaleType %in% c("COD", "ConLw", "ConLD")] <- 2
test$sale_cat[test$SaleType %in% c("Oth")] <- 1


price <- summarize(group_by(test, SaleCondition),
                   mean(SalePrice, na.rm=T))
# price[order(price$`mean(SalePrice, na.rm = T)`),]
test$sale_cond[test$SaleCondition %in% c("Partial")] <- 4
test$sale_cond[test$SaleCondition %in% c("Normal", "Alloca")] <- 3
test$sale_cond[test$SaleCondition %in% c("Family","Abnorml")] <- 2
test$sale_cond[test$SaleCondition %in% c("AdjLand")] <- 1


price <- summarize(group_by(test, MSZoning),
                   mean(SalePrice, na.rm=T))
# price[order(price$`mean(SalePrice, na.rm = T)`),]
test$zone[test$MSZoning %in% c("FV")] <- 4
test$zone[test$MSZoning %in% c("RL")] <- 3
test$zone[test$MSZoning %in% c("RH","RM")] <- 2
test$zone[test$MSZoning %in% c("C (all)")] <- 1


price <- summarize(group_by(test, Alley),
                   mean(SalePrice, na.rm=T))
# price[order(price$`mean(SalePrice, na.rm = T)`),]
table(test$Alley)
test$alleypave[test$Alley %in% c("Pave")] <- 1
test$alleypave[!test$Alley %in% c("Pave")] <- 0


## Many variables in the dataframe have been made numeric and their categorical attributes are not needed anymore.  
## These will be removed

test$Street <- NULL
test$LotShape <- NULL
test$LandContour <- NULL
test$Utilities <- NULL
test$LotConfig <- NULL
test$LandSlope <- NULL
test$Neighborhood <- NULL
test$Condition1 <- NULL
test$Condition2 <- NULL
test$BldgType <- NULL
test$HouseStyle <- NULL
test$RoofStyle <- NULL
test$RoofMatl <- NULL

test$Exterior1st <- NULL
test$Exterior2nd <- NULL
test$MasVnrType <- NULL
test$ExterQual <- NULL
test$ExterCond <- NULL

test$Foundation <- NULL
test$BsmtQual <- NULL
test$BsmtCond <- NULL
test$BsmtExposure <- NULL
test$BsmtFinType1 <- NULL
test$BsmtFinType2 <- NULL

test$Heating <- NULL
test$HeatingQC <- NULL
test$CentralAir <- NULL
test$Electrical <- NULL
test$KitchenQual <- NULL
test$FireplaceQu <- NULL

test$GarageType <- NULL
test$GarageFinish <- NULL
test$GarageQual <- NULL
test$GarageCond <- NULL
test$PavedDrive <- NULL

test$Functional <- NULL
test$PoolQC <- NULL
test$Fence <- NULL
test$MiscFeature <- NULL
test$SaleType <- NULL
test$SaleCondition <- NULL
test$MSZoning <- NULL
test$Alley <- NULL

na_cols <- colnames(test[colSums(is.na(test)) > 0])
na_cols

test$GarageYrBlt[is.na(test$GarageYrBlt)] <- 0
test$MasVnrArea[is.na(test$MasVnrArea)] <- 0
test$LotFrontage[is.na(test$LotFrontage)] <- 0
test$BsmtFinSF1[is.na(test$BsmtFinSF1)] <- 0
test$BsmtFinSF2[is.na(test$BsmtFinSF2)] <- 0
test$BsmtUnfSF[is.na(test$BsmtUnfSF)] <- 0
test$TotalBsmtSF[is.na(test$TotalBsmtSF)] <- 0
test$BsmtFullBath[is.na(test$BsmtFullBath)] <- 0
test$BsmtHalfBath[is.na(test$BsmtHalfBath)] <- 0
test$GarageCars[is.na(test$GarageCars)] <- 0
test$GarageArea[is.na(test$GarageArea)] <- 0
test$pubutil[is.na(test$pubutil)] <- 0
test$exterior_1[is.na(test$exterior_1)] <- 0
test$exterior_2[is.na(test$exterior_2)] <- 0
test$kitchen[is.na(test$kitchen)] <- 0
test$sale_cat[is.na(test$sale_cat)] <- 0
test$zone[is.na(test$zone)] <- 0

na_cols <- colnames(test[colSums(is.na(test)) > 0])
na_cols

##  Now there shouldn't be any pesky NA's to have to deal with in the data.  
##  Based on what was found on the scatterplot matrix, we also want to include the interactions found

test$year_qual <- test$YearBuilt*test$OverallQual  #overall condition
test$year_r_qual <- test$YearRemodAdd*test$OverallQual #quality x remodel
test$qual_bsmt <- test$OverallQual*test$TotalBsmtSF #quality x basement size

test$livarea_qual <- test$OverallQual*test$GrLivArea #quality x living area
test$qual_bath <- test$OverallQual*test$FullBath #quality x baths
test$qual_ext <- test$OverallQual*test$exterior_cond #quality x exterior

##  All that is left is to format the data for input into xgboost, then make the predictions.

predict1 <- as.data.frame(test)  #format as a dataframe to combine with later

##  Transform the dataframe into a matrix, then a sparse matrix for better computation time
predData <- as.matrix(predict1, rownames.force=NA)
predicting <- as(predData, "sparseMatrix")

vars <- c("MSSubClass","LotFrontage","LotArea","OverallQual","OverallCond","YearBuilt",
          "YearRemodAdd","MasVnrArea","BsmtFinSF1","BsmtFinSF2","BsmtUnfSF","TotalBsmtSF"   ,   
          "X1stFlrSF","X2ndFlrSF","LowQualFinSF","GrLivArea","BsmtFullBath","BsmtHalfBath"  ,   
          "FullBath","HalfBath","BedroomAbvGr","KitchenAbvGr","TotRmsAbvGrd","Fireplaces"     ,  
          "GarageYrBlt","GarageCars","GarageArea","WoodDeckSF","OpenPorchSF","EnclosedPorch"    ,
          "X3SsnPorch","ScreenPorch","PoolArea","MiscVal","MoSold","YrSold",
          "paved","regshape","flat","pubutil","culdesac_fr3", "gentle_slope",
          "nbhd_price_level" , "pos_features_1","pos_features_2","twnhs_end_or_1fam","house_style_level", "roof_hip_shed"    ,
          "roof_matl_hi","exterior_1","exterior_2","exterior_mason_1","exterior_cond","exterior_cond2"   ,
          "found_concrete","bsmt_cond1","bsmt_cond2","bsmt_exp","bsmt_fin1","bsmt_fin2"    ,   
          "gasheat","heatqual","air","standard_electric", "kitchen","fire",
          "gar_attach","gar_finish","garqual","garqual2","paved_drive","housefunction",
          "pool_good","priv_fence","sale_cat","sale_cond","zone","alleypave",
          "year_qual","year_r_qual","qual_bsmt","livarea_qual","qual_bath", "qual_ext")


##  Make some predictions
prediction_final <- predict(bstSparse, predicting[,vars])
length(prediction_final)


## Make prediction back into a dataframe for easier data output
prediction_final <- as.data.frame(as.matrix(prediction_final)) 
colnames(prediction_final) <- "prediction"
model_output <- cbind(predict1, prediction_final)

sub2 <- data.frame(Id = model_output$Id, SalePrice = model_output$prediction)
length(model_output$prediction)

write.csv(sub2, file = "Housing Predictions.csv", row.names = FALSE)
head(sub2$SalePrice)
