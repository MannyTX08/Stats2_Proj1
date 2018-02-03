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

BestlogYlogXModel = lm(log(SalePrice) ~ log(GrLivArea) + Neighborhood, data = data.Train)

# Create a data frame of all the intercepts and slopes for each Neighborhood
Intercept=rep(BestlogYlogXModel$coefficients[1],25)
Slope=rep(BestlogYlogXModel$coefficients[2],25)
Betas=c(0,BestlogYlogXModel$coefficients[3:26])
Intercept=Intercept+Betas
linesframe = data.frame(linesIntercept = Intercept, linesSlope=rep(BestlogYlogXModel$coefficients[2],25),
                        Neighborhood=sort(unique(data.Train$Neighborhood)))

# Including individual regression lines for each Neighborhood facet
ScatterByFactor + geom_abline(data = linesframe, aes(intercept = linesIntercept, slope = linesSlope))
