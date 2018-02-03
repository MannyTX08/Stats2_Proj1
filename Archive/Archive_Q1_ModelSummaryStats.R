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

# CV Press Statistic on 10 cross validations
# Comparing top 3 models in log(SalePrice) ~ log(GrLivArea) list
# Neighborhood still is the winner, rank same as on R2 and Adj R2 basis
cvmodel1 = caret::train(log(SalePrice) ~ log(GrLivArea) + Neighborhood, data = data.Train, method = "lm",
                       trControl = trainControl(method = "cv", number = 10,verboseIter = TRUE),
                       na.action = na.omit)
sum(residuals(cvmodel1$finalModel)^2, na.rm=T) # CV Press = 53.69073

cvmodel2 = caret::train(log(SalePrice) ~ log(GrLivArea) + BsmtQual, data = data.Train, method = "lm",
                       trControl = trainControl(method = "cv", number = 10,verboseIter = TRUE),
                       na.action = na.omit)
sum(residuals(cvmodel2$finalModel)^2, na.rm=T) # CV Press = 66.63905

cvmodel3 = caret::train(log(SalePrice) ~ log(GrLivArea) + ExterQual, data = data.Train, method = "lm",
                       trControl = trainControl(method = "cv", number = 10,verboseIter = TRUE),
                       na.action = na.omit)
sum(residuals(cvmodel3$finalModel)^2, na.rm=T) # CV Press = 72.21623

VIF = olsrr::ols_vif_tol(BestlogYlogXModel)  # Determine if VIF is appropriate
print(VIF, n=25) # VIF for model indicates we require means centering (Think this just happens on SalePrice and GrLivArea)

# Resulting model paramters and 95% CI
summary(BestlogYlogXModel)$ceofficients
confint(BestlogYlogXModel)

ParameterCoef=coef(summary(BestlogYlogXModel)) # Capture Coefficients

ResultsFrame=as.data.frame(ParameterCoef) # Place into data frame
ResultsFrame$Parameter=rownames(ResultsFrame) # Capture names for parameters
ResultsFrame=ResultsFrame[,c("Parameter","Estimate" , "Std. Error", "t value" , "Pr(>|t|)")] # Re-order columns
row.names(ResultsFrame)=NULL # Remove row names

ConfIntFrame=as.data.frame(confint(BestlogYlogXModel)) # Capture lower and upper CI (95%)
ResultsFrame = cbind(ResultsFrame,ConfIntFrame) # Combine information into 1 data frame
row.names(ResultsFrame)=NULL # Remove row names

ResultsFrame$Significant = ifelse(ResultsFrame[,5]>=.05,"N","Y") # Add Y/N for Statistical Significance at alpha=.05
ResultsFrame

# Scatter plots
# Raw Data
ScatterPlot = ggplot(data.Train,aes(y=SalePrice,x=GrLivArea)) + geom_point(color="blue") +
  labs(title = "SalePrice vs GrLivArea", y="Sales Price ($)", x="Gross Living Area") +
  theme(axis.title=element_text(size=14,face="bold"), title=element_text(size=14,face="bold"), 
        axis.text = element_text(size=10),legend.position="none") 
# log(SalePrice)~GrLivArea
ScatterPlot_logY = ggplot(data.Train,aes(y=log(SalePrice),x=GrLivArea)) + geom_point(color="blue") +
  labs(title = "log(SalePrice) vs GrLivArea", y="log of Sales Price ($)", x="Gross Living Area") +
  theme(axis.title=element_text(size=14,face="bold"), title=element_text(size=14,face="bold"), 
        axis.text = element_text(size=10),legend.position="none") 
# log(SalePrice)~log(GrLivArea)
ScatterPlot_logYlogX = ggplot(data.Train,aes(y=log(SalePrice),x=log(GrLivArea))) + geom_point(color="blue") +
  labs(title = "log(SalePrice) vs log(GrLivArea)", y="log of Sales Price ($)", x="log of Gross Living Area") +
  theme(axis.title=element_text(size=14,face="bold"), title=element_text(size=14,face="bold"), 
        axis.text = element_text(size=10),legend.position="none") 

# log/log + Neighborhood
ScatterPlot = ggplot(data.Train,aes(y=log(SalePrice),x=log(GrLivArea),color=Neighborhood)) + geom_point() +
   labs(title = "log(SalePrice) vs log(Living Area)", y="log of Sales Price ($)", x="log of Gross Living Area") +
   theme(axis.title=element_text(size=14,face="bold"), title=element_text(size=14,face="bold"), 
         axis.text = element_text(size=10), legend.position = "right") 

ScatterByFactor = ggplot(data.Train,aes(y=log(SalePrice),x=log(GrLivArea),color=Neighborhood)) + geom_point() +  
  labs(title = "log(Sales Price) vs log(Living Area)", y="log of Sales Price ($)", x="log of Gross Living Area") +
  theme(axis.title=element_text(size=14,face="bold"), title=element_text(size=14,face="bold"), 
        axis.text = element_text(size=10), legend.position="none") + facet_wrap( ~ Neighborhood, ncol=5)
  
ScatterPlot
ScatterPlot_logY
ScatterPlot_logYlogX

ScatterByFactor

# Create a data frame of all the intercepts and slopes for each Neighborhood
Intercept=rep(BestlogYlogXModel$coefficients[1],25)
Slope=rep(BestlogYlogXModel$coefficients[2],25)
Betas=c(0,BestlogYlogXModel$coefficients[3:26])
Intercept=Intercept+Betas
linesframe = data.frame(linesIntercept = Intercept, linesSlope=rep(BestlogYlogXModel$coefficients[2],25),
                        Neighborhood=sort(unique(data.Train$Neighborhood)))

# Including individual regression lines for each Neighborhood facet
ScatterByFactor + geom_abline(data = linesframe, aes(intercept = linesIntercept, slope = linesSlope))