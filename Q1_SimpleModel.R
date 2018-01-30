# https://rpubs.com/Ludovicbenistant/HousePrice
# https://github.com/susanli2016/Data-Analysis-with-R/blob/master/Predict-House-Price.Rmd 

# Load necessary packages
load.lib <- c("olsrr","ggplot2","GGally","caret")

install.lib <- load.lib[!load.lib %in% installed.packages()]
for(lib in install.lib){
  install.packages(lib,dependences=TRUE)
} 
sapply(load.lib,require,character=TRUE)

# Set WD, should be the location of your cloned repository
data.Train = read.csv("train.csv")

# Determine which columns contain most nulls (null count divided by total row count)
nullRatio <- round(sapply(data.Train, function(x) sum(is.na(x))/nrow(data.Train)),2)
nullFrame <- data.frame(Column=names(data.Train),NullRatio=nullRatio)
nullFrame <- subset(nullFrame,nullFrame$NullRatio>0)
nullFrame <- nullFrame[order(-nullFrame$NullRatio),]
nullFrame

# Now that we know which columns have multiple Null values, we can omit them from our list

# Review simple model of interest
simpleModel = lm(data = data.Train, SalePrice ~ GrLivArea + OverallCond + OverallQual)
par(mfrow=c(2,2)); plot(simpleModel); par(mfrow=c(1,1)); # Generate base R Residual plot on model (need transform on Y)
bestSub <- olsrr::ols_best_subset(simpleModel) # Find best model given available explanatory variables
bestSub; plot(bestSub) # Display results and plot

##### Cross Validation #####
model1 = caret::train(SalePrice ~ GrLivArea + OverallQual, data = data.Train, method = "lm",
                      trControl = trainControl(method = "cv", number = 10,verboseIter = TRUE),
                      na.action = na.omit)
sum(residuals(model1$finalModel)^2, na.rm=T) # CV Press = 2.63e+12

# Create transformed response variable log(SalePrice) to account for Residuals vs Fitted trend
data.Train$SalePrice = log(data.Train$SalePrice)

simpleModel = lm(data = data.Train, SalePrice ~ GrLivArea + OverallCond + OverallQual)
par(mfrow=c(2,2)); plot(simpleModel); par(mfrow=c(1,1)); # Observe better randomized Residuals vs Fitted
bestSub <- olsrr::ols_best_subset(simpleModel) # Find best model given available explanatory variables
bestSub; plot(bestSub) # Display results and plot

##### Cross Validation #####
model2 = caret::train(SalePrice ~ GrLivArea + OverallCond + OverallQual, data = data.Train, method = "lm",
                      trControl = trainControl(method = "cv", number = 10,verboseIter = TRUE),
                      na.action = na.omit)
sum(residuals(model2$finalModel)^2, na.rm=T) # CV Press = 60.02

# Create transformed explanatory variable log(GrLivArea) to account for Residuals vs Fitted trend
data.Train$GrLivArea = log(data.Train$GrLivArea)

simpleModel = lm(data = data.Train, SalePrice ~ GrLivArea + OverallCond + OverallQual)
par(mfrow=c(2,2)); plot(simpleModel); par(mfrow=c(1,1)); # Observe better randomized Residuals vs Fitted
bestSub <- olsrr::ols_best_subset(simpleModel) # Find best model given available explanatory variables
bestSub; plot(bestSub) # Display results and plot

##### Cross Validation #####
model3 = caret::train(SalePrice ~ GrLivArea + OverallCond + OverallQual, data = data.Train, method = "lm",
                      trControl = trainControl(method = "cv", number = 10,verboseIter = TRUE),
                      na.action = na.omit)
sum(residuals(model3$finalModel)^2, na.rm=T) # CV Press = 60.02

# Resulting model is log(SalePrice) ~ log(GrLivArea) + OverallQual, OverallCond 
olsrr::ols_dsrvsp_plot(simpleModel)    # Residual plot 
olsrr::ols_cooksd_barplot(simpleModel) # Cook's D Bar Plot

# Remove worst points in data set for model
worstOutliers <- c(1299,524,31,496,968)
data.Train = data.Train[!(data.Train$Id %in% worstOutliers), ]

##### Cross Validation #####
model4 = caret::train(SalePrice ~ GrLivArea + OverallCond + OverallQual, data = data.Train, method = "lm",
                              trControl = trainControl(method = "cv", number = 10,verboseIter = TRUE),
                              na.action = na.omit)
sum(residuals(model4$finalModel)^2, na.rm=T) # CV Press = 52.42

corCols <- data.Train[,c("SalePrice","GrLivArea","OverallCond","OverallQual")]
GGally::ggpairs(corCols)

olsrr::ols_dsrvsp_plot(simpleModel)    # Residual plot 
olsrr::ols_rsd_hist(simpleModel)       # Hitogram of residuals with normal curve
olsrr::ols_rsd_qqplot(simpleModel)     # Normal QQ Plot
olsrr::ols_cooksd_barplot(simpleModel) # Cooks D Plot (Id 1299 has highest Cook's, 524 second highest)
olsrr::ols_rsdlev_plot(simpleModel)    # Leverage Plot

VIF <- olsrr::ols_vif_tol(simpleModel)  # Determine if VIF is appropriate
VIF # VIF for model is low

ggplot(data.Train,aes(y=SalePrice,x=GrLivArea)) + geom_point(color="blue") + 
  labs(title = "log(Sales Price) vs log(Living Area)", y="log of Sales Price ($)", x="log of Gross Living Area") +
  theme(axis.title=element_text(size=14,face="bold"), title=element_text(size=14,face="bold"), 
        axis.text = element_text(size=10), legend.position = "bottom")

############################ OLD STUFF AFTER HERE ###################################

# # Remove columns containing large amounts of empty values
# colsToNull <- c("Alley","PoolQC","Fence","FireplaceQu","OpenPorchSF","WoodDeckSF","Street","LandContour","LandSlope","Condition2",
#                 "RoofMat1","BsmtCond","BsmtFinType2","Heating","Functional","GarageQual","GarageCond","MiscFeature","Utilities")
# for (cols in colsToNull) {
#   data.Train[,cols] = NULL
# }
# ncol(data.Train)
# 
# ##### Figure 1 #####
# # Scatter Plot by Neighborhood (raw data) 
# ggplot(data.Train, aes(y=SalePrice, x = GrLivArea, colour=Neighborhood))+geom_point() +
#   labs(title = "Sales Price vs Living Area", y="Sales Price ($)", x="Living Area (sq ft)") +
#   theme(axis.title=element_text(size=14,face="bold"), title=element_text(size=14,face="bold"), 
#         axis.text = element_text(size=10), legend.position = "bottom")
# 
# ##### Figure 2 #####
# # Scatter Plot by Neighborhood (raw data), use Log on X and Log on Y
# ggplot(data.Train, aes(y=log_SalePrice, x = log_GrLivArea, colour=Neighborhood))+geom_point() +
#   labs(title = "log(Sales Price) vs log(Living Area)", y="log of Sales Price ($)", x="log of Living Area (100 sq ft)") +
#   theme(axis.title=element_text(size=14,face="bold"), title=element_text(size=14,face="bold"), 
#         axis.text = element_text(size=10), legend.position = "bottom")
# 
# # Create Linear Regressions on log transformed data (all data)
# initial_model = lm(log_SalePrice~log_GrLivArea, data=data.Train) # Pearson's R = 0.6483826, Adjust R^2 = .4188
# 
# # Review Cooks Distance for model, determine if some have high leverage
# CooksInitial = cooks.distance(initial_model)
# # Create smaller data frame to just find highest Cooks Distance points
# Cooks.df <- data.frame(Id=data.Train$Id,Cooks=CooksInitial)
# Cooks.df <- Cooks.df[order(-Cooks.df$Cooks),]
# head(Cooks.df,10) # Top 10 leverage points
# 
# plot(CooksInitial, ylab = "Cooks Distance")


# ggplot(data.Train,aes(y=log_SalePrice,x=log_GrLivArea,colour=Neighborhood))+geom_point()+geom_smooth(method = "lm") +
#   labs(title = "log(Sales Price) vs log(Living Area)", y="log of Sales Price ($)", x="log of Living Area (100 sq ft)") +
#   theme(axis.title=element_text(size=14,face="bold"), title=element_text(size=14,face="bold"), 
#         axis.text = element_text(size=10), legend.position = "bottom")
# 
# # Center to account for high VIF
# model_cent = lm(log_SalePrice ~ I(log_GrLivArea-mean(log_GrLivArea)) + Neighborhood +  I(log_GrLivArea-mean(log_GrLivArea)):Neighborhood, data = data.Train) #R2 = .5216
# 
# summary(model_cent)
# ##### Table 4 #####
# olsrr::ols_vif_tol(model_int)
# olsrr::ols_vif_tol(model_cent)
# 
# # Log Log model centered (no grouping on Neighborhood)
# # add NAmes Regression Line
# # Y = 11.74 + .82(Cent1) + -.03(Edwards) + .11(NAmes) - .15(Edwards*Cent1) - .35(NAmes*Cent1)
# #abline(a = 11.74212 + .11, b = .81965-.34662, col="green")
# #abline(a = 11.74212 - .03, b = .81965-.14631, col="red")
# #abline(a = 11.74, b = .81965, col = "blue")

