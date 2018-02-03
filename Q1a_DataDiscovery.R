# This code is used to determine best model for Stats 2 Project 1 Question 1
# Manuel Rosales

# https://rpubs.com/Ludovicbenistant/HousePrice
# https://github.com/susanli2016/Data-Analysis-with-R/blob/master/Predict-House-Price.Rmd 

# Load necessary packages
load.lib = c("olsrr","ggplot2","caret")

install.lib = load.lib[!load.lib %in% installed.packages()]
for(lib in install.lib){
  install.packages(lib,dependences=TRUE)
} 

sapply(load.lib,require,character=TRUE)

# Function to generate summary dataframes for different models
# x = vector of names to append in a lm() statement
# y = initialized data frame to append results for each model
GenerateFitStats = function(x,y){
  for(i in 1:length(x)){
    my.formula = paste("SalePrice ~ GrLivArea +",x[i], sep=" ")
    my.model = lm(data = model.df, my.formula)
    R2 = summary(lm(data = model.df, my.formula))$r.squared
    AdjR2 = summary(lm(data = model.df, my.formula))$adj.r.squared
    AIC = ols_aic(my.model); SBC = ols_sbc(my.model);
    modelstats.df = data.frame(Model=my.formula, R2=R2, AdjR2=AdjR2, AIC=AIC, SBC=SBC) 
    y = rbind(y,modelstats.df)
  }
  return(y) 
}

# Set WD, should be the location of your cloned repository
data.Train = read.csv("train.csv")

# Determine Categorical and Continuous Variables
CatColumns = data.Train[,sapply(data.Train,is.factor)]
CatColNames = names(CatColumns)

# Set up data set of Id, GrLivArea, SalePrice to create model statistics for each categorical variable
model.df = data.Train[,c("Id","GrLivArea","SalePrice")]
model.df = cbind(model.df,CatColumns)

dfSummary = data.frame(Model=as.character(), R2=as.numeric(), AdjR2=numeric(), AIC=numeric(), SBC=numeric()) 
rawSummary = GenerateFitStats(CatColNames,dfSummary) # Call Function For Fit Stats

# Take log of Response to determine fit statistics
model.df$SalePrice = log(model.df$SalePrice)
logySummary = GenerateFitStats(CatColNames,dfSummary) # Call Function For Fit Stats

# Take log of explanatory continuous varible to determine fit statistics
model.df$GrLivArea = log(model.df$GrLivArea)
logylogxSummary = GenerateFitStats(CatColNames,dfSummary) # Call Function For Fit Stats

# Review Best Raw Data Model Residuals
BestRawModel = lm(data = data.Train, SalePrice ~ GrLivArea + Neighborhood)
par(mfrow=c(2,2)); plot(BestRawModel); par(mfrow=c(1,1)); # Generate base R Residual plot on model (need transform)

# Review Best log(Y) Model Residuals
BestlogYModel = lm(data = data.Train, log(SalePrice) ~ GrLivArea + Neighborhood)
par(mfrow=c(2,2)); plot(BestlogYModel); par(mfrow=c(1,1)); # Generate base R Residual plot on model

# Review Best log(Y) and log(X) Model Residuals
BestlogYlogXModel = lm(data = data.Train, log(SalePrice) ~ log(GrLivArea) + Neighborhood)
par(mfrow=c(2,2)); plot(BestlogYlogXModel); par(mfrow=c(1,1)); # Generate base R Residual plot on model

# Generate pretty Residual Plots
olsrr::ols_dsrvsp_plot(BestRawModel)
olsrr::ols_dsrvsp_plot(BestlogYModel)   
olsrr::ols_dsrvsp_plot(BestlogYlogXModel)   