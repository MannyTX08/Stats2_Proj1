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

# Remove columns that hurt variable selection process
ColsToRemove = c("Alley","PoolQC","Fence","FireplaceQu","OpenPorchSF","WoodDeckSF","Street",
                 "LandContour","LandSlope","Condition2","RoofMatl","BsmtCond","BsmtFinType2",
                 "Heating","Functional","GarageQual","GarageCond","MiscFeature","Utilities")

for (cols in ColsToRemove) {
  train[,cols] = NULL
  test[,cols] = NULL
}

# Review full model with remaining variables (prior to log transformations)  
FullDataSetModel = lm(SalePrice~.,data = train)
par(mfrow=c(2,2)); plot(FullDataSetModel)
par(mfrow=c(1,1)); 

# Apply log Transformation on Key Columns
ColsToLog.train = c('BsmtFinSF1','BsmtFinSF2','BsmtUnfSF','EnclosedPorch','GarageArea','GrLivArea',
                    'LotArea','LowQualFinSF','MasVnrArea','MiscVal','PoolArea','SalePrice',
                    'ScreenPorch','TotalBsmtSF','X1stFlrSF','X2ndFlrSF','X3SsnPorch')
ColsToLog.test = c('BsmtFinSF1','BsmtFinSF2','BsmtUnfSF','EnclosedPorch','GarageArea','GrLivArea',
                    'LotArea','LowQualFinSF','MasVnrArea','MiscVal','PoolArea',
                    'ScreenPorch','TotalBsmtSF','X1stFlrSF','X2ndFlrSF','X3SsnPorch')

train = LogColsFunc(ColsToLog.train,train)
test = LogColsFunc(ColsToLog.test,test)


# Review full model with remaining variables (after log transformations) 
FullDataSetModel = lm(SalePrice~.,data = train)
par(mfrow=c(2,2)); plot(FullDataSetModel)
par(mfrow=c(1,1)); 

# Delte points deamed outliers that effect model
outliers = c(1299, 524, 1183, 692, 589, 1325, 463, 633, 31, 1433)
train = train[!(train$Id %in% outliers), ]

# Review full model with remaining variables (after log transformations) 
FullDataSetModel = lm(SalePrice~.,data = train)
par(mfrow=c(2,2)); plot(FullDataSetModel)
par(mfrow=c(1,1)); 

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

####################
# Formula Forward Stepwise Variable Selection via AIC
k = ols_stepaic_forward(VSsteps, details = T) # AIC = -2568.171
ForwardFormula ="SalePrice~"
ForwardFormula = paste0(ForwardFormula,paste(k$predictors,collapse = "+")) # Capture model predictor variables
stepForward = as.formula(ForwardFormula, env = new.env()) # Pass model predictors into new object

ForwardFit = lm(stepForward, data = train2, na.action = na.exclude)
summary(ForwardFit) # Adj R2 = .9407

##### Cross Validation #####
modelForwardSelection = train(stepForward, data = train2, method = "lm",
                              trControl = trainControl(method = "cv", number = 10,verboseIter = TRUE),
                              na.action = na.omit
)

sum(residuals(modelForwardSelection$finalModel)^2, na.rm=T) # CV Press = 12.12315

##### Kaggle Export #####
##### Score = .12140 #####
test2$SalePrice = NA
test2$SalePrice = predict.lm(object = ForwardFit, newdata = test2)
test2$SalePrice = exp(test2$SalePrice)
forwardKaggle = data.frame(Id=test2$Id,SalePrice=test2$SalePrice);
write.csv(forwardKaggle,"ForwardK.csv", row.names = FALSE)

####################
# Both  Stepwise Variable Selection via AIC
k2 = ols_stepaic_both(VSsteps, details = T) # AIC = -2568.171
BothFormula ="SalePrice~"
BothFormula = paste0(BothFormula,paste(k2$predictors,collapse = "+")) # Capture model predictor variables
stepBoth = as.formula(BothFormula, env = new.env()) # Pass model predictors into new object

BothFit = lm(stepBoth, data = train2, na.action = na.exclude)
summary(BothFit) # Adj R2 = .9407

##### Cross Validation #####
modelBothSelection = train(stepBoth, data = train2, method = "lm",
                              trControl = trainControl(method = "cv", number = 10,verboseIter = TRUE),
                              na.action = na.omit
)

sum(residuals(modelBothSelection$finalModel)^2, na.rm=T) # CV Press = 12.12315

##### Kaggle Export #####
##### Score = .12446 #####
test2$SalePriceBoth = NA
test2$SalePriceBoth = predict.lm(object = BothFit, newdata = test2)
test2$SalePriceBoth = exp(test2$SalePriceBoth)
bothKaggle = data.frame(Id=test2$Id,SalePrice=test2$SalePriceBoth);
write.csv(bothKaggle,"BothK.csv", row.names = FALSE)

############ LASSO ATTEMPT ###########

# Convert all factor string columns into integers for LASSO
train3 = train2
test3 = test2

ColsToConvert = c("MSZoning","LotShape","LotConfig","Neighborhood","Condition1","BldgType","HouseStyle","RoofStyle",
                  "Exterior1st","Exterior2nd","MasVnrType","ExterQual","ExterCond","Foundation","BsmtQual",
                  "BsmtExposure","BsmtFinType1","HeatingQC","CentralAir","Electrical","KitchenQual","GarageType",
                  "GarageFinish","PavedDrive","SaleType","SaleCondition")

for (cols in ColsToConvert) {
  train3[,cols] = as.factor(train3[,cols])
  levels(train3[,cols]) = 1:length(levels(train3[,cols]))
  train3[,cols] = as.numeric(train3[,cols])
  test3[,cols] = as.factor(test3[,cols])
  levels(test3[,cols]) = 1:length(levels(test3[,cols]))
  test3[,cols] = as.numeric(test3[,cols])
}

x = model.matrix(SalePrice~.,data=train3)
x=x[,-1] # Remove the intercept

glmnet1 = cv.glmnet(x=x,y=train3$SalePrice, type.measure='mse', nfolds = 10, alpha=1) # alpha = 1 for LASSO
co = coef(glmnet1, s = "lambda.1se")
inds = which(co!=0)
variables = row.names(co)[inds]
variables = variables[!(variables %in% '(Intercept)')]
variables # Get variables selected from LASSO in cv.glmnet
plot(glmnet1)
glmnet1$lambda.min # 0.001466091
glmnet1$lambda.1se # 0.008586923

LASSOformula =as.formula(SalePrice ~ MSZoning + LotArea + OverallQual + OverallCond + YearBuilt + YearRemodAdd + 
                           ExterQual + Foundation + BsmtQual + BsmtExposure + BsmtFinSF1+ TotalBsmtSF + HeatingQC + 
                           CentralAir + X1stFlrSF + GrLivArea + BsmtFullBath + KitchenAbvGr + KitchenQual + Fireplaces + 
                           GarageCars + GarageArea + PavedDrive + ScreenPorch + SaleCondition, env = new.env())

LASSOfit = lm(LASSOformula, data = train3, na.action = na.exclude)
summary(LASSOfit) # Adj R2 = .9175

##### Cross Validation #####
modelLASSO = train(LASSOformula, data = train3, method = "lm",
                           trControl = trainControl(method = "cv", number = 10,verboseIter = TRUE),
                           na.action = na.omit
)

sum(residuals(modelLASSO$finalModel)^2, na.rm=T) # CV Press = 18.15048

##### Kaggle Export #####
##### Score = .12629 #####
test3$SalePriceLASSO = NA
test3$SalePriceLASSO = predict.lm(object = LASSOfit, newdata = test3)
test3$SalePriceLASSO = exp(test3$SalePriceLASSO)
LASSOKaggle = data.frame(Id=test3$Id,SalePrice=test3$SalePriceLASSO);
write.csv(LASSOKaggle,"LASSOK.csv", row.names = FALSE)
