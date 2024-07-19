#PArtial Least Squares Regression analysis of hyperspectral library to [O3] as a continuous predictor
#PLSR

#load relevant libraries
library(hyperSpec)
library(stats)
library(pls)
library(caret)
library(ggplot2)
library(ggpubr)
#set working directory as R project location
setwd("~/Library/CloudStorage/OneDrive-Nexus365/01 OXFORD PHD/Field Work 2023/R_Data_Analysis")
#load  "merged", the hyperspectral library for Wytham and Bifor, oak only
load(file = "~/Library/CloudStorage/OneDrive-Nexus365/01 OXFORD PHD/Field Work 2023/R_Data_Analysis/QWYTHAM_BIFOR_merged.RData")
head(merged)
merged<-subset(merged,merged$ozone!="NA")
#fit the  partial least squares model
#make this example reproducible
set.seed(1)

#fit PLSR model
model <- plsr(ozone~spc, data=merged, scale=TRUE, validation="CV")#left running for a while, started 13:00 ish, finished 16:30, mac may have gone to sleep in between 
summary(model)
#visualise cross validation plots
validationplot(model, ncomp=1:60)#Root mean square deviation, in units of predictor, more useful
validationplot(model, val.type="MSEP")#mean squared prediction error, not in same units as predictor
validationplot(model, val.type="R2", ncomp=1:60)
#optimal number of comps looks to be around 35



#now split the dataset into test and train and fit for 60 components, 80:20 split
sample <- sample(c(TRUE, FALSE), nrow(merged), replace=TRUE, prob=c(0.8,0.2))
train<-merged[sample,]
test<-merged$spc[!sample,]
y_test<-merged$ozone[!sample]

#make the model based on train set
model_2<-plsr(ozone~spc, data=train, scale=TRUE, validation="CV",ncomp=60)#runs fast
summary(model_2)
validationplot(model_2)#Root mean square deviation, in units of predictor, minima at 35 comps
validationplot(model_2, val.type="R2")
#make predictions on test set
pcr_pred <- predict(model_2, test, ncomp=35)
plot(y_test, pcr_pred, main="Test Dataset", xlab="Observed", ylab="PLS Predicted")
abline(0,1,col="red")
#evaluate the plsr model's performance for test data
pls_eval<-data.frame(obs=y_test, pred=pcr_pred[,1,1])
defaultSummary(pls_eval)
#output:
# RMSE  Rsquared       MeanAbsoluteError 
# 4.5789956 0.6565933 3.1594922 
