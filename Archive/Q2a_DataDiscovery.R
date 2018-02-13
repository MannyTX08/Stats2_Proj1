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

# Determine colunms with factors
CatColumns = data.Train[,sapply(data.Train,is.factor)]
# See which Categorical Variables are largely null
CatNullCols = as.data.frame(sapply(CatColumns, function(x) sum(is.na(x))))
colnames(CatNullCols) = c("Nulls")
CatNullCols$NullPerc = round(CatNullCols$Nulls/nrow(data.Train),3)
CatNullCols = CatNullCols[order(-CatNullCols$Nulls),]
CatNullCols = subset(CatNullCols, CatNullCols$Nulls > 0)

# Determine remaining columns
ConColumns = data.Train[,sapply(data.Train,is.numeric)]
# See which Continuous Variables are largely null
ConNullCols = as.data.frame(sapply(ConColumns, function(x) sum(is.na(x))))
colnames(ConNullCols) = c("Nulls")
ConNullCols$NullPerc = round(ConNullCols$Nulls/nrow(data.Train),3)
ConNullCols = ConNullCols[order(-ConNullCols$Nulls),]
ConNullCols = subset(ConNullCols, ConNullCols$Nulls > 0)

NAframe = rbind(CatNullCols, ConNullCols)
NAframe = NAframe[order(-NAframe$Nulls),]
NAframe

# Remove columns with > 10% NA's
NAframe = subset(NAframe, NAframe$NullPerc > .10)

ColsToRemove = rownames(NAframe)

# Add columns with 2 or less levels
LowLevels = as.data.frame(sapply(data.Train, function(x) length(levels(x))))
names(LowLevels) = "Levels"
LowLevels = subset(LowLevels,LowLevels$Levels==2)

ColsToRemove2 = rownames(LowLevels)
ColsToRemove2 = ColsToRemove2[1:3] # Keeping CentralAir
ColsToRemove = c(ColsToRemove,ColsToRemove2)

for (cols in ColsToRemove) {
  data.Train[,cols] = NULL
  data.Test[,cols] = NULL
}

# Plot model prior to log transformations
FullModel = lm(data=data.Train, SalePrice ~ .) 
par(mfrow=c(2,2)); plot(FullModel); par(mfrow=c(1,1)); # Generate base R Residual plot on model

# Apply Log Transform on Columns that are trully continuous

ColsToLogTrain = c('BsmtFinSF1','BsmtFinSF2','BsmtUnfSF','EnclosedPorch','GarageArea','GrLivArea','LotArea','LowQualFinSF',
              'MasVnrArea','MiscVal','OpenPorchSF','PoolArea','SalePrice','ScreenPorch','TotalBsmtSF','WoodDeckSF',
              'X1stFlrSF','X2ndFlrSF','X3SsnPorch')
ColsToLogTest = c('BsmtFinSF1','BsmtFinSF2','BsmtUnfSF','EnclosedPorch','GarageArea','GrLivArea','LotArea','LowQualFinSF',
                   'MasVnrArea','MiscVal','OpenPorchSF','PoolArea','ScreenPorch','TotalBsmtSF','WoodDeckSF',
                   'X1stFlrSF','X2ndFlrSF','X3SsnPorch')

data.Train = LogColsFunc(ColsToLogTrain,data.Train)

data.Test = LogColsFunc(ColsToLogTest,data.Test)

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

###########################################
# Need to fix past here

ameliated <- amelia(data.Train,m=1, p2s=1, ords = c("MSZoning", "LotShape", "LotConfig", "Neighborhood", "Condition1","BldgType", "HouseStyle",
                                               "RoofStyle", "Exterior1st", "Exterior2nd","MasVnrType", "ExterQual", "ExterCond", 
                                               "Foundation", "BsmtQual", "BsmtExposure", "BsmtFinType1", "HeatingQC", "CentralAir", "Electrical",
                                               "KitchenQual", "GarageType", "GarageFinish", "PavedDrive", "SaleType", "LandContour", "LandSlope", "Condition2",
                                               "RoofMatl","SaleCondition","BsmtCond","BsmtFinType2","Heating","Functional","GarageQual","GarageCond"))
 
write.amelia(obj=ameliated, file.stem="amelia.Train") # Package adds 1 at the end for imputation number.
data.Train2 <- read.csv("amelia.Train1.csv")
data.Train2$X = NULL #Remove column that duplicates Id
 
ameliated2 <- amelia(data.Test,m=1, p2s=1, ords = c("MSZoning", "LotShape", "LotConfig", "Neighborhood", "Condition1","BldgType", "HouseStyle",
                                                    "RoofStyle", "Exterior1st", "Exterior2nd","MasVnrType", "ExterQual", "ExterCond", 
                                                    "Foundation", "BsmtQual", "BsmtExposure", "BsmtFinType1", "HeatingQC", "CentralAir", "Electrical",
                                                    "KitchenQual", "GarageType", "GarageFinish", "PavedDrive", "SaleType", "LandContour", "LandSlope", "Condition2",
                                                    "RoofMatl","SaleCondition","BsmtCond","BsmtFinType2","Heating","Functional","GarageQual","GarageCond"))
 
write.amelia(obj=ameliated2, file.stem="amelia.Test") # Package adds 1 at the end for imputation number.
data.Test2 <- read.csv("amelia.Test1.csv")
data.Test2$X = NULL #Remove column that duplicates Id

# Fit full model on all remaining variables and data points
VSsteps = lm(SalePrice ~ . , data = data.Train)

par(mfrow=c(2,2)); plot(VSsteps)
par(mfrow=c(1,1)); ols_rsd_hist(VSsteps)

# Formula forward variable sel
k <- ols_stepaic_forward(VSsteps, detail=TRUE)
plot(k) # Plot generates the AIC step chart, it stops when AIC is no longer dropping

# Build Formula for train using resulting predictor variables
ForwardFormula ="SalePrice~"
ForwardFormula = paste0(ForwardFormula,paste(k$predictors,collapse = "+"))

stepForward = as.formula(ForwardFormula, env = new.env())

# Formula backward variable sel
#ols_stepaic_backward(VSsteps, details = T)
#stepBack = as.formula(SalePrice ~ MSSubClass + MSZoning + LotArea + LotConfig + Neighborhood + Condition1 + OverallQual + OverallCond + YearBuilt + YearRemodAdd + Exterior1st + MasVnrType + MasVnrArea + ExterCond + Foundation + BsmtQual + BsmtExposure + BsmtFinType1 + BsmtFinSF1 + BsmtUnfSF + TotalBsmtSF + HeatingQC + CentralAir + X1stFlrSF + GrLivArea + BsmtFullBath + FullBath + HalfBath + KitchenAbvGr + KitchenQual + Fireplaces + GarageType + GarageCars + EnclosedPorch + ScreenPorch + PoolArea + SaleCondition, env = new.env())

ForwardFit = lm(stepForward, data = data.Train, na.action = na.exclude)
summary(ForwardFit) # Adj R2 = .9483

##### Cross Validation #####
modelForwardSelection = caret::train(stepForward, data = data.Train, method = "lm",
                                   trControl = trainControl(method = "cv", number = 10,verboseIter = TRUE),
                                   na.action = na.omit
)

# modelForwardSelection$finalModel
summary(modelForwardSelection) # Adjusted R2 = .9483
sum(residuals(modelForwardSelection$finalModel)^2, na.rm=T) # CV Press = 8.5914

##### Kaggle Exports #####
data.Test$SalePrice = NA
data.Test$SalePrice = predict(object = ForwardFit, newdata = data.Test) 
data.Test$SalePrice = exp(data.Test$SalePrice)
forwardKaggle = data.frame(Id=data.Test2$Id,SalePrice=data.Test2$SalePrice);
forwardKaggle$SalePrice = na.aggregate(forwardKaggle$SalePrice) #Replace NA with mean of others
write.csv(forwardKaggle,"ForwardK.csv")


data.Test2$SalePrice = NA
data.Test2$SalePrice = predict(object = ForwardFit, newdata = data.Test2) 
data.Test$SalePrice = exp(data.Test$SalePrice)
forwardKaggle = data.frame(Id=data.Test2$Id,SalePrice=data.Test2$SalePrice);
forwardKaggle$SalePrice = na.aggregate(forwardKaggle$SalePrice) #Replace NA with mean of others
write.csv(forwardKaggle,"ForwardK.csv")