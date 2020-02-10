data <- read.table("https://msudataanalytics.github.io/SSC442/Labs/data/ames.csv",
                   header = TRUE,
                   sep = ",")

removed <- c("OverallQual", "OverallCond")
Ames <- data[, ! names(data) %in% removed, drop = F]
(l <- sapply(Ames, function(x) is.factor(x)))
m <- Ames[, l]
ifelse(n <- sapply(m, function(x) length(levels(x))) == 1, "DROP", "NODROP")

fit_all = lm(SalePrice ~ MSSubClass + MSZoning + LotFrontage + LotArea + Street + LotShape + LandContour + LotConfig +
            LandSlope + Neighborhood + Condition1 + Condition2 + BldgType + HouseStyle +
            YearBuilt + YearRemodAdd + RoofStyle + RoofMatl + Exterior1st + Exterior2nd + 
            MasVnrType + MasVnrArea + ExterQual + ExterCond + Foundation + BsmtQual + BsmtCond + 
            BsmtExposure + BsmtFinType1 + BsmtFinSF1 + BsmtFinType2 + BsmtFinSF2 + BsmtUnfSF +
          TotalBsmtSF +Heating +HeatingQC + CentralAir +Electrical + X1stFlrSF + X2ndFlrSF +
            LowQualFinSF + GrLivArea + BsmtFullBath + BsmtHalfBath + FullBath + HalfBath + BedroomAbvGr +
            KitchenAbvGr + KitchenQual + TotRmsAbvGrd + Functional + Fireplaces + FireplaceQu + GarageType +
            GarageYrBlt + GarageFinish + GarageCars + GarageArea + GarageQual + GarageCond + PavedDrive +
            WoodDeckSF + OpenPorchSF + EnclosedPorch + X3SsnPorch + ScreenPorch + PoolArea + MiscVal +
            MoSold + YrSold + SaleType + SaleCondition, data=Ames)
# can't use alley, poolQC, Fence, MiscFeature variables in equation - these ones throw the error
summary(fit_all)

fit_start = lm(SalePrice ~1, data=Ames) #intercept only
summary(fit_start)

step(fit_start, direction = "forward", scope=formula(fit_all))
#this will produce an ordered list of the next variable with the lowest RSS

fit_2 = lm(SalePrice ~ x1, data=Ames) #where x1 is the variable with the lowest RSS
step(fit_2, direction = "forward", scope=formula(fit_all)) #finds the next variable with the lowest RSS
fit_3= lm(SalePrice ~ x1 + x2, data=Ames) 
step(fit_3, direction = "forward", scope=formula(fit_all)) #finds the next variable with the lowest RSS
fit_4= lm(SalePrice ~ x1 + x2 + x3, data=Ames) 
step(fit_4, direction = "forward", scope=formula(fit_all)) #finds the next variable with the lowest RSS
fit_5= lm(SalePrice ~ x1 + x2 + x3 + x4, data=Ames) 
step(fit_5, direction = "forward", scope=formula(fit_all)) #finds the next variable with the lowest RSS
fit_6= lm(SalePrice ~ x1 + x2 + x3 + x4 + x5, data=Ames) 
step(fit_6, direction = "forward", scope=formula(fit_all)) #finds the next variable with the lowest RSS
fit_7= lm(SalePrice ~ x1 + x2 + x3 + x4 + x5 + x6, data=Ames) 
step(fit_7, direction = "forward", scope=formula(fit_all)) #finds the next variable with the lowest RSS
fit_8= lm(SalePrice ~ x1 + x2 + x3 + x4 + x5 + x6 + x7, data=Ames) 
step(fit_8, direction = "forward", scope=formula(fit_all)) #finds the next variable with the lowest RSS
fit_9= lm(SalePrice ~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8, data=Ames) 
step(fit_9, direction = "forward", scope=formula(fit_all)) #finds the next variable with the lowest RSS
fit_10= lm(SalePrice ~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9, data=Ames) 
step(fit_10, direction = "forward", scope=formula(fit_all)) #finds the next variable with the lowest RSS
fit_11= lm(SalePrice ~  x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10, data=Ames) 
step(fit_11, direction = "forward", scope=formula(fit_all)) #finds the next variable with the lowest RSS
fit_12= lm(SalePrice ~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + x11, data=Ames) 
step(fit_12, direction = "forward", scope=formula(fit_all)) #finds the next variable with the lowest RSS
fit_13= lm(SalePrice ~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + x11 + x12, data=Ames) 
step(fit_13, direction = "forward", scope=formula(fit_all)) #finds the next variable with the lowest RSS
fit_14= lm(SalePrice ~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13, data=Ames) 
step(fit_14, direction = "forward", scope=formula(fit_all)) #finds the next variable with the lowest RSS
fit_15= lm(SalePrice ~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13 + x14, data=Ames) 
step(fit_15, direction = "forward", scope=formula(fit_all)) #finds the next variable with the lowest RSS

final_fit = lm(SalePrice ~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13 + x14 + x15, data = Ames)


fits <- list(fit_start, fit_2, fit_3, fit_4, fit_5, fit_6, fit_7, fit_8, fit_9, fit_10, fit_11, fit_12, fit_13, fit_14, fit_15, final_fit)
RMSE <- list()
complexity <- list(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16)

for(i in fits){
  rss <- c(crossprod(i$residuals))
  mse <- rss / length(i$residuals)
  rmse <- sqrt(mse)
  RMSE <- rmse
}

plot(complexity,RMSE,main = "Complexity progression", xlab = "Complexity",ylab = "RMSE")







