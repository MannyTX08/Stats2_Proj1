# Load necessary packages
load.lib = c("olsrr","ggplot2","caret","Amelia")

install.lib = load.lib[!load.lib %in% installed.packages()]
for(lib in install.lib){
  install.packages(lib,dependences=TRUE)
} 

sapply(load.lib,require,character=TRUE)

# Function for log transformation on muliple columns
# if value is 0 leave 0
LogColsFunc = function(x,y){
  for(i in 1:length(x)){
    y[,x[i]] =  ifelse(y[,x[i]] == 0,0 ,log(y[,x[i]]))
  }
  return(y) 
}

# Load data
train = read.csv("train.csv")
test = read.csv("test.csv")

# Review full model with all variables (more than likely way overfitted :0)  
FullDataSetModel = lm(SalePrice~.,data = train)
par(mfrow=c(2,2)); plot(FullDataSetModel)
par(mfrow=c(1,1)); 

# Remove columns that hurt variable selection process
ColsToRemove = c("Alley","PoolQC","Fence","FireplaceQu","OpenPorchSF","WoodDeckSF","Street",
                 "LandContour","LandSlope","Condition2","RoofMatl","BsmtCond","BsmtFinType2",
                 "Heating","Functional","GarageQual","GarageCond","MiscFeature","Utilities")

for (cols in ColsToRemove) {
  train[,cols] = NULL
  test[,cols] = NULL
}

# Apply log Transformation on Key Columns
ColsToLog.train = c('BsmtFinSF1','BsmtFinSF2','BsmtUnfSF','EnclosedPorch','GarageArea','GrLivArea',
                    'LotArea','LowQualFinSF','MasVnrArea','MiscVal','PoolArea','SalePrice',
                    'ScreenPorch','TotalBsmtSF','X1stFlrSF','X2ndFlrSF','X3SsnPorch')
ColsToLog.test = c('BsmtFinSF1','BsmtFinSF2','BsmtUnfSF','EnclosedPorch','GarageArea','GrLivArea',
                    'LotArea','LowQualFinSF','MasVnrArea','MiscVal','PoolArea',
                    'ScreenPorch','TotalBsmtSF','X1stFlrSF','X2ndFlrSF','X3SsnPorch')

train = LogColsFunc(ColsToLog.train,train)
test = LogColsFunc(ColsToLog.test,test)

# Delte points deamed outliers that effect model
outliers = c(1299, 524, 1183, 692, 589, 1325, 463, 633, 31, 1433)
train = train[!(train$Id %in% outliers), ]

ordinalcols = c("MSZoning", "LotShape", "LotConfig", "Neighborhood", "Condition1","BldgType", "HouseStyle", 
                "RoofStyle", "Exterior1st", "Exterior2nd","MasVnrType", "ExterQual", "ExterCond", "Foundation", 
                "BsmtQual","BsmtExposure", "BsmtFinType1", "HeatingQC", "CentralAir", "Electrical", "KitchenQual", 
                "GarageType", "GarageFinish", "PavedDrive", "SaleType","SaleCondition")

# Imputation for missing values on train and test
ameliated <- amelia(train,m=1, p2s=1, ords = ordinalcols)

write.amelia(obj=ameliated, file.stem="train2") #names it something else, wierd.
train2 <- read.csv("train21.csv")
train2$X = NULL #Remove column that duplicates Id

ameliated2 <- amelia(test,m=1, p2s=1, ords = ordinalcols)

write.amelia(obj=ameliated2, file.stem="test2") #names it something else, wierd.
test2 <- read.csv("test21.csv")
test2$X = NULL #Remove column that duplicates Id

# Fit full model on all remaining variables and data points
VSsteps = lm(SalePrice ~ . , data = train2)

par(mfrow=c(2,2)); plot(VSsteps)
par(mfrow=c(1,1)); ols_rsd_hist(VSsteps)

# Formula forward variable sel
k = ols_stepaic_forward(VSsteps, details = T)
ForwardFormula ="SalePrice~"
ForwardFormula = paste0(ForwardFormula,paste(k$predictors,collapse = "+")) # Capture model predictor variables
stepForward = as.formula(ForwardFormula, env = new.env()) # Pass model predictors into new object

ForwardFit = lm(stepForward, data = train2, na.action = na.exclude)
summary(ForwardFit) # Adj R2 = .9402

##### Cross Validation #####
modelForwardSelection = train(stepForward, data = train2, method = "lm",
                              trControl = trainControl(method = "cv", number = 10,verboseIter = TRUE),
                              na.action = na.omit
)

sum(residuals(modelForwardSelection$finalModel)^2, na.rm=T) # CV Press = 12.29009

##### Kaggle Exports #####
test2$SalePrice = NA
test2$SalePrice = predict.lm(object = ForwardFit, newdata = test2)
test2$SalePrice = exp(test2$SalePrice)
forwardKaggle = data.frame(Id=test2$Id,SalePrice=test2$SalePrice);
write.csv(forwardKaggle,"ForwardK.csv", row.names = FALSE)

# test2$SalePriceBack = NA
# test2$SalePriceBack = predict.lm(object = BackwardFit, newdata = test2)
# test2$SalePriceBack = exp(test2$SalePriceBack)
# backKaggle = data.frame(Id=test2$Id,SalePrice=test2$SalePriceBack);
# backKaggle$SalePrice = na.aggregate(backKaggle$SalePrice) #Replace NA with mean of others
# write.csv(backKaggle,"BackwordK.csv")
# 
# test2$SalePriceBoth = NA
# test2$SalePriceBoth = predict.lm(object = BothFit, newdata = test2)
# test2$SalePriceBoth = exp(test2$SalePriceBoth)
# bothKaggle = data.frame(Id=test2$Id,SalePrice=test2$SalePriceBoth);
# bothKaggle$SalePrice = na.aggregate(bothKaggle$SalePrice) #Replace NA with mean of others
# write.csv(bothKaggle,"BothK.csv")