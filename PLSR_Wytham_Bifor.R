#PArtial Least Squares Regression analysis of hyperspectral library to [O3] as a continuous predictor
#PLSR

#load relevant libraries
library(hyperSpec)
library(stats)
library(pls)
library(caret)
library(ggplot2)
library(cowplot)
library(ggpubr)
library(cowplot)
library(gridGraphics)
#set working directory as R project location
setwd("~/Library/CloudStorage/OneDrive-Nexus365/01 OXFORD PHD/Field Work 2023/R_Data_Analysis")
#load  "merged", the hyperspectral library for Wytham and Bifor, oak only
load(file = "~/Library/CloudStorage/OneDrive-Nexus365/01 OXFORD PHD/Field Work 2023/R_Data_Analysis/QWYTHAM_BIFOR_merged.RData")
head(merged)
merged<-subset(merged,merged$ozone!="NA")

#average per site/date
merged$site_date<-paste(merged$site, merged$date)
site_date_means<-aggregate(merged, merged$site_date, mean)
head(site_date_means)

#split the dataset by site/date
merged$site_date<-as.factor(merged$site_date)
stdts<-levels((merged$site_date))
lst<-list()
var_names<-list()
v<-new("hyperSpec")
train_merged<-new("hyperSpec")
test_merged<-new("hyperSpec")
#make a list of subsetted hyperspectral objects
for (n in 1:length(stdts)){
  var_names<-append(var_names, paste(stdts[n],"train", sep="_"))
  var_names<-append(var_names, paste(stdts[n],"test", sep="_"))
  v<-subset(merged, merged$site_date==stdts[n])
  sample_set<- sample(c(TRUE, FALSE), nrow(v), replace=TRUE, prob=c(0.7,0.3))
  train<-v[sample_set,]
  test<-v[!sample_set,]
  train_merged<-collapse(train_merged,train)
  test_merged<-collapse(test_merged,test)
}

#now average the test_merged and train_merged by site/date
train_means<-aggregate(train_merged, train_merged$site_date, mean)
test_means<-aggregate(test_merged, test_merged$site_date, mean)

#fit the  partial least squares model
#make this example reproducible
set.seed(1)

#fit PLSR model based on the train data set
model <- plsr(ozone~spc, data=train_means, scale=TRUE, validation="CV")
summary(model)
#visualise cross validation plots
validationplot(model, main="", xlab="Number of components")#Root mean square deviation, in units of predictor, more useful
RMSEP_plt<-recordPlot()
validationplot(model, val.type="MSEP")#mean squared prediction error, not in same units as predictor
#optimal number of comps looks to be 2
scoreplot(model)
loadingplot(model,comps=1:2, legendpos = "topright", labels="numbers", xlab="Wavelength (nm)", ylab="Loading value")
loading_plt<-recordPlot()
#make predictions on test set
test<-test_means$spc
y_test<-test_means$ozone
pcr_pred <- predict(model, test, ncomp=2)
plot(y_test, pcr_pred, main="Test Dataset", xlab="Observed", ylab="PLS Predicted")
abline(0,1,col="red")
#make this plot a bit better?
pred_df<-data.frame(x=y_test, y=pcr_pred)
colnames(pred_df)<-c("Observed","PLS Predicted")

ggscatter(pred_df, x="Observed", y="PLS Predicted", add="reg.line", color="red")+
  stat_cor(label.x = 30, label.y = 50) +
  stat_regline_equation(label.x = 30, label.y = 52)
formula<-y~x
test_plt<-ggplot(pred_df, aes(Observed,`PLS Predicted` ))+
  geom_point(size = 3, col ='red')+
  stat_smooth(formula= 'y~x',method='lm', linewidth=1, col='black')+
  stat_regline_equation(
    aes(label =  paste(..eq.label.., ..adj.rr.label.., sep = "~~~~")),
    formula = formula, label.x=30,label.y=50
  ) +
  theme_cowplot(14)+ labs(x="Observed ozone concentration (ppb)",y="Predicted ozone concentration (ppb)")
test_plt
#evaluate the plsr model's performance for test data
pls_eval<-data.frame(obs=y_test, pred=pcr_pred[,1,1])
defaultSummary(pls_eval)

#multi plot
RH<-plot_grid(RMSEP_plt, loading_plt, labels="AUTO", ncol=1, rel_heights = c(1,1))
RH
plot_grid(RMSEP_plt, labels="AUTO", ncol=1, rel_heights = c(1,1))
setwd("~/Library/CloudStorage/OneDrive-Nexus365/01 OXFORD PHD/Field Work 2023/R_Data_Analysis/Wytham_Bifor_Ozone")
tiff("F4_PLSR.tiff", units="in", width=8, height=6, res=300)
plot_grid(RH, test_plt, labels=c('','C'), ncol=2)
dev.off()
