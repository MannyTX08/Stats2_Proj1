# Load necessary packages
load.lib <- c("caret","olsrr","ggplot2","car")

install.lib <- load.lib[!load.lib %in% installed.packages()]
for(lib in install.lib) install.packages(lib,dependences=TRUE)
sapply(load.lib,require,character=TRUE)

# Set WD, this could be your downloaded repository
wd = "/Users/manny/Library/Mobile Documents/com~apple~CloudDocs/DataScience@SMU/MSDS_6372/Project_01"
setwd(wd)

data.Train = read.csv("train.csv")
# Descriptive information of data set (1460 rows, 81 cols, 25 neighborhoods )
nrow(data.Train) ; ncol(data.Train); length(unique(data.Train$Neighborhood))
# data.Train = data.Train[data.Train$Neighborhood == "NAmes" | data.Train$Neighborhood == "Edwards" | data.Train$Neighborhood == "BrkSide",]

# Create transformed data set and remove outliers
data.Train$log_GrLivArea = log(data.Train$GrLivArea)
data.Train$log_SalePrice = log(data.Train$SalePrice)

##### Figure 1 #####
# Scatter Plot by Neighborhood (raw data) 
ggplot(data.Train, aes(y=SalePrice, x = GrLivArea, colour=Neighborhood))+geom_point() +
  labs(title = "Sales Price vs Living Area", y="Sales Price ($)", x="Living Area (sq ft)") +
  theme(axis.title=element_text(size=14,face="bold"), title=element_text(size=14,face="bold"), 
        axis.text = element_text(size=10), legend.position = "bottom")

##### Figure 2 #####
# Scatter Plot by Neighborhood (raw data), use Log on X and Log on Y
ggplot(data.Train, aes(y=log_SalePrice, x = log_GrLivArea, colour=Neighborhood))+geom_point() +
  labs(title = "log(Sales Price) vs log(Living Area)", y="log of Sales Price ($)", x="log of Living Area (100 sq ft)") +
  theme(axis.title=element_text(size=14,face="bold"), title=element_text(size=14,face="bold"), 
        axis.text = element_text(size=10), legend.position = "bottom")

# Create Linear Regressions on log transformed data (all data)
initial_model = lm(log_SalePrice~log_GrLivArea, data=data.Train) # Pearson's R = 0.6483826, Adjust R^2 = .4188

# Review Cooks Distance for model, determine if some have high leverage
CooksInitial = cooks.distance(initial_model)
# Create smaller data frame to just find highest Cooks Distance points
Cooks.df <- data.frame(Id=data.Train$Id,Cooks=CooksInitial)
Cooks.df <- Cooks.df[order(-Cooks.df$Cooks),]
head(Cooks.df,10) # Top 10 leverage points

plot(CooksInitial, ylab = "Cooks Distance")

# Pearson's R = 0.7156116, Adjust R^2 = .5056
model_int = lm(log_SalePrice ~ log_GrLivArea + 
                 Neighborhood + log_GrLivArea:Neighborhood, data = data.Train) 

# Assumptions for Residuals not met, visible in residual plots and histogram
##### Figure 3 #####
par(mfrow=c(2,2)); plot(initial_model)
par(mfrow=c(1,1)); ols_rsd_hist(initial_model)

# Remove 2 data points with highest cooks distance (Id = 1299 or 524)
# These two points are also the highest cooks distance in the MLR containing interaction.
data.Train = data.Train[data.Train$Id!=1299 & data.Train$Id!=524,  ]

# Fit Initial Model without high Cook's rows
##### Table 1 #####
initial_model = lm(log_SalePrice~log_GrLivArea, data=data.Train)
summary(initial_model)

# Generate MLR with interaction
##### Table 2 #####
model_int = lm(log_SalePrice ~ log_GrLivArea + Neighborhood + log_GrLivArea:Neighborhood, data = data.Train) #R2 = .5216, high VIF
summary(model_int)

##### Figure 4 #####
par(mfrow=c(2,2)); plot(model_int)
par(mfrow=c(1,1)); ols_rsd_hist(model_int)

# Plot with interaction and 95% CI
##### Figure 5 #####
ggplot(data.Train,aes(y=log_SalePrice,x=log_GrLivArea,colour=Neighborhood))+geom_point()+geom_smooth(method = "lm") +
  labs(title = "log(Sales Price) vs log(Living Area)", y="log of Sales Price ($)", x="log of Living Area (100 sq ft)") +
  theme(axis.title=element_text(size=14,face="bold"), title=element_text(size=14,face="bold"), 
        axis.text = element_text(size=10), legend.position = "bottom")

# Center to account for high VIF
model_cent = lm(log_SalePrice ~ I(log_GrLivArea-mean(log_GrLivArea)) + Neighborhood +  I(log_GrLivArea-mean(log_GrLivArea)):Neighborhood, data = data.Train) #R2 = .5216

summary(model_cent)
##### Table 4 #####
# car package 
vif(model_int)
vif(model_cent)

# Log Log model centered (no grouping on Neighborhood)
# add NAmes Regression Line
# Y = 11.74 + .82(Cent1) + -.03(Edwards) + .11(NAmes) - .15(Edwards*Cent1) - .35(NAmes*Cent1)
#abline(a = 11.74212 + .11, b = .81965-.34662, col="green")
#abline(a = 11.74212 - .03, b = .81965-.14631, col="red")
#abline(a = 11.74, b = .81965, col = "blue")
# 
# # Group Edwards and BrkSide into 1 category for Neighborhood
# grouped = data.Train
# grouped$Neighborhood2 = ifelse(grouped$Neighborhood == "NAmes", "NAmes","BrkSide/Edwards")
# model2group = lm(log_SalePrice ~ log_GrLivArea + Neighborhood2 + log_GrLivArea:Neighborhood2, data = grouped)
# summary(model2group) #Pearson's R = 0.7242 and R2 = .5206
# vif(model2group)
# ##### Table 5 #####
# confint(model_int)
# # Compare full to reduced model, see if the extra terms are statistically significant
# ##### Table 3 #####
# Anova(model_int, model2group)
