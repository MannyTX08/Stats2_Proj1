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

# Set WD, should be the location of your cloned repository
data.Train = read.csv("train.csv")

# CV Press Statistic on 10 cross validations
# Comparing top 3 models in log(SalePrice) ~ log(GrLivArea) options

# Neighborhood still is the winner, rank same as on R2 and Adj R2 basis
cvmodel1 = caret::train(log(SalePrice) ~ log(GrLivArea) + Neighborhood, data = data.Train, method = "lm",
                        trControl = trainControl(method = "cv", number = 10,verboseIter = TRUE),
                        na.action = na.omit)
sum(residuals(cvmodel1$finalModel)^2, na.rm=T) # CV Press = 53.69073

# BsmtQual
cvmodel2 = caret::train(log(SalePrice) ~ log(GrLivArea) + BsmtQual, data = data.Train, method = "lm",
                        trControl = trainControl(method = "cv", number = 10,verboseIter = TRUE),
                        na.action = na.omit)
sum(residuals(cvmodel2$finalModel)^2, na.rm=T) # CV Press = 66.63905

# ExterQual
cvmodel3 = caret::train(log(SalePrice) ~ log(GrLivArea) + ExterQual, data = data.Train, method = "lm",
                        trControl = trainControl(method = "cv", number = 10,verboseIter = TRUE),
                        na.action = na.omit)
sum(residuals(cvmodel3$finalModel)^2, na.rm=T) # CV Press = 72.21623

# Resulting model paramters, 95% CI, Significance, and VIFs
summary(BestlogYlogXModel)$ceofficients
confint(BestlogYlogXModel)

BestlogYlogXModel = lm(log(SalePrice) ~ log(GrLivArea) + Neighborhood, data = data.Train)
ParameterCoef=coef(summary(BestlogYlogXModel)) # Capture Coefficients

ResultsFrame=as.data.frame(ParameterCoef) # Place into data frame
ResultsFrame$Parameter=rownames(ResultsFrame) # Capture names for parameters
ResultsFrame=ResultsFrame[,c("Parameter","Estimate" , "Std. Error", "t value" , "Pr(>|t|)")] # Re-order columns
row.names(ResultsFrame)=NULL # Remove row names

ConfIntFrame=as.data.frame(confint(BestlogYlogXModel)) # Capture lower and upper CI (95%)
ResultsFrame = cbind(ResultsFrame,ConfIntFrame) # Combine information into 1 data frame
row.names(ResultsFrame)=NULL # Remove row names

ResultsFrame$Significant = ifelse(ResultsFrame[,5]>=.05,"N","Y") # Add Y/N for Statistical Significance at alpha=.05

VIF = olsrr::ols_vif_tol(BestlogYlogXModel)  # Determine if VIF is appropriate
print(VIF, n=25) # VIF for model indicates we require means centering (Think this just happens on SalePrice and GrLivArea)
VIFcol = as.data.frame(VIF[,3]) # Capture the VIFs
VIFcol.row1 = data.frame(VIF=NA) # Add empty row in VIF for intercept
VIFcol = rbind(VIFcol.row1,VIFcol)

ResultsFrame$VIF = VIFcol
ResultsFrame
write.csv(ResultsFrame, file='Q1_BestModelSummary.csv', row.names=FALSE)

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