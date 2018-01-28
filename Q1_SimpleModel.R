# https://rpubs.com/Ludovicbenistant/HousePrice
# https://github.com/susanli2016/Data-Analysis-with-R/blob/master/Predict-House-Price.Rmd 

# Load necessary packages
load.lib <- c("olsrr","ggplot2")

install.lib <- load.lib[!load.lib %in% installed.packages()]
for(lib in install.lib) install.packages(lib,dependences=TRUE)
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

# We have been tasked with providing a simple model to help realty agents and prospective
# buyers determine a potential sales price of a home given certain variables.
# This is intended to be easy to measure and interpret, therefore we will use what we believe
# to be the most important variables concerning a home in general: Gross Living Area,
# Overall Condition, and Overall Quality.

# Review simple model of interest
simpleModel = lm(data = data.Train, SalePrice ~ GrLivArea + OverallCond + OverallQual)
par(mfrow=c(2,2)); plot(simpleModel); par(mfrow=c(1,1)); # Generate base R Residual plot on model (need transform on Y)
olsrr::ols_dsrvsp_plot(simpleModel) # Residual plot
olsrr::ols_rsd_hist(simpleModel)    # Hitogram of residuals with normal curve
olsrr::ols_rsd_qqplot(simpleModel)  # Normal QQ Plot
olsrr::ols_cooksd_barplot(simpleModel) # Cooks D Plot
olsrr::ols_rsdlev_plot(simpleModel) # Leverage Plot 

# Review combinations of GrLivArea, OverallCond, and OverallQual to find best on key measures
bestSub <- olsrr::ols_best_subset(simpleModel)
bestSub
plot(bestSub)
# Resulting model is SalesPrice ~ GrLivArea + OverallQual, OverallCond did not aid the model
# based on R2 remaining the same, Adj. R2 going down and C(p)/AIC/SBIC/SBC increasing
simpleModel = lm(data = data.Train, SalePrice ~ GrLivArea + OverallQual)
par(mfrow=c(2,2)); plot(simpleModel); par(mfrow=c(1,1)); # Generate base R Residual plot on model (need transform on Y)
olsrr::ols_dsrvsp_plot(simpleModel) # Residual plot
olsrr::ols_rsd_hist(simpleModel)    # Hitogram of residuals with normal curve
olsrr::ols_rsd_qqplot(simpleModel)  # Normal QQ Plot
olsrr::ols_cooksd_barplot(simpleModel) # Cooks D Plot
olsrr::ols_rsdlev_plot(simpleModel) # Leverage Plot 


# Create transformed response variable log(SalePrice) and explanatory variable log(GrLivArea)
data.Train$SalePrice = log(data.Train$SalePrice)
data.Train$GrLivArea = log(data.Train$GrLivArea)

logModel = lm(data = data.Train, SalePrice ~ GrLivArea + OverallQual)

par(mfrow=c(2,2)); plot(logModel); par(mfrow=c(1,1));
olsrr::ols_dsrvsp_plot(logModel)    # Residual plot (Id = 524 and Id = 1299 may warrant removal)
olsrr::ols_rsd_hist(logModel)       # Hitogram of residuals with normal curve
olsrr::ols_rsd_qqplot(logModel)     # Normal QQ Plot
olsrr::ols_cooksd_barplot(logModel) # Cooks D Plot (Id 1299 has highest Cook's, 524 second highest)
olsrr::ols_rsdlev_plot(logModel)    # Leverage Plot

# Remove two worst points in data set for model
data.Train2 = data.Train[data.Train$Id!=1299 & data.Train$Id!=524, ]

logModel2 = lm(data = data.Train2, SalePrice ~ GrLivArea + OverallQual)

par(mfrow=c(2,2)); plot(logModel2); par(mfrow=c(1,1));
olsrr::ols_dsrvsp_plot(logModel2)    # Residual plot 
olsrr::ols_rsd_hist(logModel2)       # Hitogram of residuals with normal curve
olsrr::ols_rsd_qqplot(logModel2)     # Normal QQ Plot
olsrr::ols_cooksd_barplot(logModel2) # Cooks D Plot (Id 1299 has highest Cook's, 524 second highest)
olsrr::ols_rsdlev_plot(logModel2)    # Leverage Plot

VIF <- olsrr::ols_vif_tol(logModel2)  # Determine if VIF is appropriate
VIF # VIF for model is low

# Generate results of logModel2
summary(logModel2) #R2 = .7642, Adj R2 = .7639 
confint(logModel2)

# Interpretation here is 
# log(SalePrice) = 7.66 + .46*log(GrLivArea) + .17*OverallQual
# 2^.46 = 1.37, so for every doubling of GrLivArea there is a 1.37 increase in median sale price accounting for OverallQual
# 95% CI [1.336196, 1.408642]
# Need to see how to interpret OverallQual here...
# CI needed








############################ OLD STUFF AFTER HERE ###################################

# # Remove columns containing large amounts of empty values
# colsToNull <- c("Alley","PoolQC","Fence","FireplaceQu","OpenPorchSF","WoodDeckSF","Street","LandContour","LandSlope","Condition2",
#                 "RoofMat1","BsmtCond","BsmtFinType2","Heating","Functional","GarageQual","GarageCond","MiscFeature","Utilities")
# for (cols in colsToNull) {
#   data.Train[,cols] = NULL
# }
# ncol(data.Train)
# 
# train$Alley = NULL; test$Alley = NULL
# train$PoolQC = NULL; test$PoolQC = NULL
# train$Fence = NULL; test$Fence = NULL
# train$FireplaceQu = NULL; test$FireplaceQu = NULL
# train$OpenPorchSF = NULL ; test$OpenPorchSF = NULL 
# train$WoodDeckSF = NULL; test$WoodDeckSF = NULL
# train$Street = NULL; test$Street = NULL
# train$LandContour = NULL; test$LandContour = NULL
# train$LandSlope = NULL; test$LandSlope = NULL
# train$Condition2 = NULL; test$Condition2 = NULL
# train$RoofMatl = NULL; test$RoofMatl = NULL
# train$BsmtCond = NULL; test$BsmtCond = NULL
# train$BsmtFinType2 = NULL; test$BsmtFinType2 = NULL
# train$Heating = NULL; test$Heating = NULL
# train$Functional = NULL; test$Functional = NULL
# train$GarageQual = NULL; test$GarageQual = NULL
# train$GarageCond = NULL; test$GarageCond = NULL
# train$MiscFeature  = NULL; test$MiscFeature  = NULL
# train$Utilities = NULL; test$Utilities = NULL
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
# 
# # Pearson's R = 0.7156116, Adjust R^2 = .5056
# model_int = lm(log_SalePrice ~ log_GrLivArea + 
#                  Neighborhood + log_GrLivArea:Neighborhood, data = data.Train) 
# 
# # Assumptions for Residuals not met, visible in residual plots and histogram
# ##### Figure 3 #####
# par(mfrow=c(2,2)); plot(initial_model)
# par(mfrow=c(1,1)); ols_rsd_hist(initial_model)
# 
# # Remove 2 data points with highest cooks distance (Id = 1299 or 524)
# # These two points are also the highest cooks distance in the MLR containing interaction.
# data.Train = data.Train[data.Train$Id!=1299 & data.Train$Id!=524,  ]
# 
# # Fit Initial Model without high Cook's rows
# ##### Table 1 #####
# initial_model = lm(log_SalePrice~log_GrLivArea, data=data.Train)
# summary(initial_model)
# 
# # Generate MLR with interaction
# ##### Table 2 #####
# model_int = lm(log_SalePrice ~ log_GrLivArea + Neighborhood + log_GrLivArea:Neighborhood, data = data.Train) #R2 = .5216, high VIF
# summary(model_int)
# 
# ##### Figure 4 #####
# par(mfrow=c(2,2)); plot(model_int)
# par(mfrow=c(1,1)); ols_rsd_hist(model_int)
# 
# # Plot with interaction and 95% CI
# ##### Figure 5 #####
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
# # 
# # # Group Edwards and BrkSide into 1 category for Neighborhood
# # grouped = data.Train
# # grouped$Neighborhood2 = ifelse(grouped$Neighborhood == "NAmes", "NAmes","BrkSide/Edwards")
# # model2group = lm(log_SalePrice ~ log_GrLivArea + Neighborhood2 + log_GrLivArea:Neighborhood2, data = grouped)
# # summary(model2group) #Pearson's R = 0.7242 and R2 = .5206
# # vif(model2group)
# # ##### Table 5 #####
# # confint(model_int)
# # # Compare full to reduced model, see if the extra terms are statistically significant
# # ##### Table 3 #####
# # Anova(model_int, model2group)
