#PArtial Least Squares Regression analysis of hyperspectral library to [O3] as a continuous predictor
#PLSR

#load relevant libraries
library(hyperSpec)
library(stats)
library(pls)
#set working directory as R project location
setwd("~/Library/CloudStorage/OneDrive-Nexus365/01 OXFORD PHD/Field Work 2023/R_Data_Analysis")
#load  "merged", the hyperspectral library for Wytham and Bifor, oak only
load(file = "~/Library/CloudStorage/OneDrive-Nexus365/01 OXFORD PHD/Field Work 2023/R_Data_Analysis/QWYTHAM_BIFOR_merged.RData")
head(merged)
#fit the  partial least squares model
#make this example reproducible
set.seed(1)

#fit PCR model
model <- plsr(ozone~spc, data=merged, scale=TRUE, validation="CV")

merged$spc[,2151]


