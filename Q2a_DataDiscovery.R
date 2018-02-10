# Load necessary packages
load.lib = c("olsrr","ggplot2","caret","Amelia")

install.lib = load.lib[!load.lib %in% installed.packages()]
for(lib in install.lib){
  install.packages(lib,dependences=TRUE)
} 

sapply(load.lib,require,character=TRUE)

LogColsFunc = function(x,y){
  for(i in 1:length(x)){
    y[,x[i]] =  ifelse(y[,x[i]] == 0,0 ,log(y[,x[i]]))
  }
  return(y) 
}

# Load data
data.Train = read.csv("train.csv")
data.Test = read.csv("test.csv")

# Determine Categorical Variables
CatColumns = data.Train[,sapply(data.Train,is.factor)]
# See which Categorical Variables are largely null
CatNullCols = as.data.frame(sapply(CatColumns, function(x) sum(is.na(x))))
colnames(CatNullCols) = c("Nulls")
CatNullCols$NullPerc = round(CatNullCols$Nulls/length(data.Train),2)
CatNullCols = CatNullCols[order(-CatNullCols$Nulls),]
CatNullCols = subset(CatNullCols, CatNullCols$Nulls > 0)
CatNullCols$Varialbe = rep("Categorical",times = nrow(CatNullCols))

# Determine Continuous Variables
ConColumns = data.Train[,sapply(data.Train,is.numeric)]
# See which Continuous Variables are largely null
ConNullCols = as.data.frame(sapply(ConColumns, function(x) sum(is.na(x))))
colnames(ConNullCols) = c("Nulls")
ConNullCols$NullPerc = round(ConNullCols$Nulls/length(data.Train),2)
ConNullCols = ConNullCols[order(-ConNullCols$Nulls),]
ConNullCols = subset(ConNullCols, ConNullCols$Nulls > 0)
ConNullCols$Varialbe = rep("Continuous",times = nrow(ConNullCols))

NAframe = rbind(CatNullCols, ConNullCols)
NAframe = NAframe[order(-NAframe$Nulls),]

# Remove columns with > 10% NA's
NAframe = subset(NAframe, NAframe$NullPerc > 10)

ColsToRemove = rownames(NAframe)

# Add columns with 2 or less levels
LowLevels = as.data.frame(sapply(data.Train, function(x) length(levels(x))))
names(LowLevels) = "Levels"
LowLevels = subset(LowLevels,LowLevels$Levels==2)

ColsToRemove2 = rownames(LowLevels)

ColsToRemove = c(ColsToRemove,ColsToRemove2)

for (cols in ColsToRemove) {
  data.Train[,cols] = NULL
  data.Test[,cols] = NULL
}

# Apply Log Transform on Columns that are trully continuous

ColsToLogTrain = c('BsmtFinSF1','BsmtFinSF2','BsmtUnfSF','EnclosedPorch','GarageArea','GrLivArea','LotArea','LowQualFinSF',
              'MasVnrArea','MiscVal','OpenPorchSF','PoolArea','SalePrice','ScreenPorch','TotalBsmtSF','WoodDeckSF',
              'X1stFlrSF','X2ndFlrSF','X3SsnPorch')
ColsToLogTest = c('BsmtFinSF1','BsmtFinSF2','BsmtUnfSF','EnclosedPorch','GarageArea','GrLivArea','LotArea','LowQualFinSF',
                   'MasVnrArea','MiscVal','OpenPorchSF','PoolArea','ScreenPorch','TotalBsmtSF','WoodDeckSF',
                   'X1stFlrSF','X2ndFlrSF','X3SsnPorch')

data.Train = LogColsFunc(ColsToLogTrain,data.Train)

data.Test = LogColsFunc(ColsToLogTest,data.Test)

ggplot(data.Train, aes(y=SalePrice, x = GrLivArea, colour=Neighborhood))+geom_point() +
  labs(title = "Sales Price vs Living Area", y="Sales Price ($)", x="Gross Living Area") +
  theme(axis.title=element_text(size=14,face="bold"), title=element_text(size=14,face="bold"), 
        axis.text = element_text(size=10), legend.position = "bottom")

FullModel = lm(data=data.Train, SalePrice ~ .) 
par(mfrow=c(2,2)); plot(FullModel); par(mfrow=c(1,1)); # Generate base R Residual plot on model
olsrr::ols_rsd_hist(FullModel)
olsrr::ols_cooksd_chart(FullModel)

# Delte points deamed outliers that effect model
BadPoints=c(216,338,215,1325,524,633,335,215,463,582,689,463)
data.Train = data.Train[ ! data.Train$Id %in% BadPoints, ]

# Re-plot to show better model
FullModel = lm(data=data.Train, SalePrice ~ .) 
par(mfrow=c(2,2)); plot(FullModel); par(mfrow=c(1,1)); # Generate base R Residual plot on model
olsrr::ols_rsd_hist(FullModel)
olsrr::ols_cooksd_chart(FullModel)

# ameliated <- amelia(data.Train,m=1, p2s=1, ords = c("MSZoning", "LotShape", "LotConfig", "Neighborhood", "Condition1", 
#                                                     "BldgType", "HouseStyle", "RoofStyle", "Exterior1st", "Exterior2nd", 
#                                                     "MasVnrType", "ExterQual", "ExterCond", "Foundation", "BsmtQual", 
#                                                     "BsmtExposure", "BsmtFinType1", "HeatingQC", 
#                                                     "KitchenQual", "GarageType", "GarageFinish", "PavedDrive", "SaleType", 
#                                                     "SaleCondition"))
# 
# write.amelia(obj=ameliated, file.stem="data.Train1") #names it something else, wierd.
# data.Train2 <- read.csv("data.Train31.csv")
# data.Train2$X = NULL #Remove column that duplicates Id
# 
# ameliated2 <- amelia(data.Test,m=1, p2s=1, ords = c("MSZoning", "LotShape", "LotConfig", "Neighborhood", "Condition1", 
#                                                     "BldgType", "HouseStyle", "RoofStyle", "Exterior1st", "Exterior2nd", 
#                                                     "MasVnrType", "ExterQual", "ExterCond", "Foundation", "BsmtQual", 
#                                                     "BsmtExposure", "BsmtFinType1", "HeatingQC", "CentralAir", "Electrical", 
#                                                     "KitchenQual", "GarageType", "GarageFinish", "PavedDrive", "SaleType", 
#                                                     "SaleCondition"))
# 
# write.amelia(obj=ameliated2, file.stem="data.Test3") #names it something else, wierd.
# data.Test2 <- read.csv("data.Test31.csv")
# data.Test2$X = NULL #Remove column that duplicates Id

# Fit full model on all remaining variables and data points
VSsteps = lm(SalePrice ~ . , data = data.Train)

par(mfrow=c(2,2)); plot(VSsteps)
par(mfrow=c(1,1)); ols_rsd_hist(VSsteps)

# Formula forward variable sel
k <- ols_stepaic_forward(VSsteps, details = T)
plot(k) # Plot generates the AIC step chart, it stops when AIC is no longer dropping (I think)
stepForward = as.formula( SalePrice ~ OverallQual + GrLivArea + Neighborhood + BsmtFinSF1 + GarageCars + OverallCond + HouseStyle + YearBuilt + LotArea + RoofMatl + SaleCondition + KitchenQual + Functional + MSZoning + KitchenAbvGr + Condition1 + GarageCond + Exterior1st + Foundation + BsmtFullBath + Fireplaces + LandSlope + TotalBsmtSF + BsmtQual + BsmtExposure + GarageQual + PoolArea + YearRemodAdd + LotConfig + LowQualFinSF + HeatingQC + WoodDeckSF + ScreenPorch + SaleType + BsmtUnfSF + BsmtFinType1 + ExterCond + BldgType + X2ndFlrSF + GarageArea + EnclosedPorch + TotRmsAbvGrd + PavedDrive + HalfBath + FullBath  , env = new.env())

# Formula backward variable sel
ols_stepaic_backward(VSsteps, details = T)
stepBack = as.formula(SalePrice ~ MSSubClass + MSZoning + LotArea + LotConfig + Neighborhood + Condition1 + OverallQual + OverallCond + YearBuilt + YearRemodAdd + Exterior1st + MasVnrType + MasVnrArea + ExterCond + Foundation + BsmtQual + BsmtExposure + BsmtFinType1 + BsmtFinSF1 + BsmtUnfSF + TotalBsmtSF + HeatingQC + CentralAir + X1stFlrSF + GrLivArea + BsmtFullBath + FullBath + HalfBath + KitchenAbvGr + KitchenQual + Fireplaces + GarageType + GarageCars + EnclosedPorch + ScreenPorch + PoolArea + SaleCondition, env = new.env())

ForwardFit = lm(stepForward, data = data.Train, na.action = na.exclude)
summary(ForwardFit) # Adj R2 = .9474

##### Cross Validation #####
modelForwardSelection = caret::train(stepForward, data = data.Train, method = "lm",
                                   trControl = trainControl(method = "cv", number = 10,verboseIter = TRUE),
                                   na.action = na.omit
)

modelForwardSelection$finalModel
summary(modelForwardSelection) # Adjusted R2 = .9474
sum(residuals(modelForwardSelection$finalModel)^2, na.rm=T) # CV Press = 8.8126

##### Kaggle Exports #####
data.Test$SalePrice = NA
data.Test$SalePrice = predict.lm(object = ForwardFit, newdata = data.Test)
data.Test$SalePrice = exp(data.Test$SalePrice)
forwardKaggle = data.frame(Id=data.Test2$Id,SalePrice=data.Test2$SalePrice);
forwardKaggle$SalePrice = na.aggregate(forwardKaggle$SalePrice) #Replace NA with mean of others
write.csv(forwardKaggle,"ForwardK.csv")

data.Test2$SalePriceBack = NA
data.Test2$SalePriceBack = predict.lm(object = BackwardFit, newdata = data.Test2)
data.Test2$SalePriceBack = exp(data.Test2$SalePriceBack)
backKaggle = data.frame(Id=data.Test2$Id,SalePrice=data.Test2$SalePriceBack);
backKaggle$SalePrice = na.aggregate(backKaggle$SalePrice) #Replace NA with mean of others
write.csv(backKaggle,"BackwordK.csv")

data.Test2$SalePriceBoth = NA
data.Test2$SalePriceBoth = predict.lm(object = BothFit, newdata = data.Test2)
data.Test2$SalePriceBoth = exp(data.Test2$SalePriceBoth)
bothKaggle = data.frame(Id=data.Test2$Id,SalePrice=data.Test2$SalePriceBoth);
bothKaggle$SalePrice = na.aggregate(bothKaggle$SalePrice) #Replace NA with mean of others
write.csv(bothKaggle,"BothK.csv")


##################### OLD STUFF HERE ##########################



# ##### Custom Model!!!!! #####
# 
# CustomModel = as.formula(SalePrice ~ OverallQual + GrLivArea + Neighborhood + BsmtFinSF1 + LotArea + YearRemodAdd + GarageCars + OverallCond + YearBuilt + SaleCondition + X1stFlrSF + KitchenQual + BsmtFinType1 + Exterior1st + BsmtExposure + MSZoning + Condition1 + BsmtQual + Fireplaces + BsmtFullBath + ScreenPorch + CentralAir + GarageType + ExterCond + PoolArea + Foundation + TotalBsmtSF + HeatingQC + LotConfig + MasVnrArea + BsmtUnfSF + FullBath + HalfBath + KitchenAbvGr + Neighborhood:GrLivArea + OverallQual:GrLivArea, env = new.env() )
# 
# Custom = lm(CustomModel, data = data.Train2, na.action = na.exclude)
# summary(Custom) # Adj R2 = .9402
# 
# modelCustom = data.Train(CustomModel, data = data.Train2, method = "lm",
#                     trControl = data.TrainControl(method = "cv", number = 10,verboseIter = TRUE),
#                     na.action = na.omit
#                     )
# 
# sum(residuals(modelCustom$finalModel)^2, na.rm=T) # CV Press = 12.47733
# 
# data.Test2$SalePriceCust = NA
# data.Test2$SalePriceCust = predict.lm(object = Custom, newdata = data.Test2)
# data.Test2$SalePriceCust = exp(data.Test2$SalePriceCust)
# customKaggle = data.frame(Id=data.Test2$Id,SalePrice=data.Test2$SalePriceCust);
# customKaggle$SalePrice = na.aggregate(customKaggle$SalePrice) #Replace NA with mean of others
# write.csv(customKaggle,"CustomK.csv")

# data.Train$SalePrice =  ifelse(data.Train$SalePrice ==0,0 ,log(data.Train$SalePrice))
# data.Train$GrLivArea = ifelse(data.Train$GrLivArea ==0,0 ,log(data.Train$GrLivArea/100))
# data.Train$LotArea = ifelse(data.Train$LotArea ==0,0 ,log(data.Train$LotArea))
# data.Train$BsmtFinSF1 = ifelse(data.Train$BsmtFinSF1 ==0,0 ,log(data.Train$BsmtFinSF1))
# data.Train$BsmtFinSF2 = ifelse(data.Train$BsmtFinSF2 ==0,0 ,log(data.Train$BsmtFinSF2))
# data.Train$BsmtUnfSF = ifelse(data.Train$BsmtUnfSF ==0,0 ,log(data.Train$BsmtUnfSF))
# data.Train$TotalBsmtSF = ifelse(data.Train$TotalBsmtSF ==0,0 ,log(data.Train$TotalBsmtSF))
# data.Train$X1stFlrSF = ifelse(data.Train$X1stFlrSF ==0,0 ,log(data.Train$X1stFlrSF))
# data.Train$X2ndFlrSF = ifelse(data.Train$X2ndFlrSF ==0,0 ,log(data.Train$X2ndFlrSF))
# data.Train$GarageArea = ifelse(data.Train$GarageArea ==0,0 ,log(data.Train$GarageArea))
# data.Train$EnclosedPorch = ifelse(data.Train$EnclosedPorch ==0,0 ,log(data.Train$EnclosedPorch))
# data.Train$X3SsnPorch = ifelse(data.Train$X3SsnPorch ==0,0 ,log(data.Train$X3SsnPorch))
# data.Train$ScreenPorch = ifelse(data.Train$ScreenPorch ==0,0 ,log(data.Train$ScreenPorch))

##### Apply Same Transofrmations to data.Test Data #####

# data.Test$SalePrice =  ifelse(data.Test$SalePrice ==0,0 ,log(data.Test$SalePrice))
# data.Test$GrLivArea = ifelse(data.Test$GrLivArea ==0,0 ,log(data.Test$GrLivArea/100))
# data.Test$LotArea = ifelse(data.Test$LotArea ==0,0 ,log(data.Test$LotArea))
# data.Test$BsmtFinSF1 = ifelse(data.Test$BsmtFinSF1 ==0,0 ,log(data.Test$BsmtFinSF1))
# data.Test$BsmtFinSF2 = ifelse(data.Test$BsmtFinSF2 ==0,0 ,log(data.Test$BsmtFinSF2))
# data.Test$BsmtUnfSF = ifelse(data.Test$BsmtUnfSF ==0,0 ,log(data.Test$BsmtUnfSF))
# data.Test$TotalBsmtSF = ifelse(data.Test$TotalBsmtSF ==0,0 ,log(data.Test$TotalBsmtSF))
# data.Test$X1stFlrSF = ifelse(data.Test$X1stFlrSF ==0,0 ,log(data.Test$X1stFlrSF))
# data.Test$X2ndFlrSF = ifelse(data.Test$X2ndFlrSF ==0,0 ,log(data.Test$X2ndFlrSF))
# data.Test$GarageArea = ifelse(data.Test$GarageArea ==0,0 ,log(data.Test$GarageArea))
# data.Test$EnclosedPorch = ifelse(data.Test$EnclosedPorch ==0,0 ,log(data.Test$EnclosedPorch))
# data.Test$X3SsnPorch = ifelse(data.Test$X3SsnPorch ==0,0 ,log(data.Test$X3SsnPorch))
# data.Test$ScreenPorch = ifelse(data.Test$ScreenPorch ==0,0 ,log(data.Test$ScreenPorch))
