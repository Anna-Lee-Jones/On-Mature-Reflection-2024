#SVC Analysis of all BIFoR Spectra
#based on code from "SVC Leaf Clip Original Analysis" used in 2022

#Load relevant packages
library(hyperSpec)
library(missMDA)
library(factoextra)
library(FactoMineR)
library(ggrepel)
library(ggplot2)
library(ggpubr)
library(EMSC)
library(pls)
library(viridis)
library(prospectr)
library(MASS)
library(readxl)
library(mdatools)
library(vegan)
library(stringr)

#load in csv file of ozone exposure here, formatted as Month (eg June 1 or August 2), Array, PPB
#Ready to pair up with mean spectra based on the Month Half and Array 

#import csv of all leaf clip hyperspectral data from jupyter notebook
LEAFCLIP_Hyperspectral<- read.csv("~/OneDrive - Nexus365/01 OXFORD PHD/Field Work 2023/SVC_2023/all_bifor/bifor_Hyperspectral.csv", header=TRUE)
#create hyperspectral object 
LEAFCLIP_ALL<-new("hyperSpec", wavelength=LEAFCLIP_Hyperspectral$X, spc=t(LEAFCLIP_Hyperspectral [, -1]), data=data.frame(sample=colnames(LEAFCLIP_Hyperspectral[,-1])), labels=list(.wavelength="Wavelength",spc="I/a.u."))

value<-1
length<-length(LEAFCLIP_ALL)
#set up hyperspec variables for each date and array
#would add in the half monthly ozone dosage as another variable
LEAFCLIP_ALL@data$month<-""
LEAFCLIP_ALL@data$array<-""
LEAFCLIP_ALL@data$ozone<-""
#assign month
for (value in 1:length){
  LEAFCLIP_ALL[value]$sample<-toupper(LEAFCLIP_ALL[value]$sample)
  if (grepl("X160623_*",LEAFCLIP_ALL[value]$sample)){
    LEAFCLIP_ALL[value]$month<-"June_16"
  }else if (grepl("X220623_*",LEAFCLIP_ALL[value]$sample)) {
    LEAFCLIP_ALL[value]$month<-"June_22"
  }else if (grepl("X050723_*",LEAFCLIP_ALL[value]$sample)) {
    LEAFCLIP_ALL[value]$month<-"July_5"
  }else if (grepl("X250723_*",LEAFCLIP_ALL[value]$sample)) {
    LEAFCLIP_ALL[value]$month<-"July_25"
  }
}
#assign array
for (value in 1:length){
  LEAFCLIP_ALL[value]$sample<-toupper(LEAFCLIP_ALL[value]$sample)
  if (grepl("*A5_*",LEAFCLIP_ALL[value]$sample)){
    LEAFCLIP_ALL[value]$array<-"Array_5"
  }else if (grepl("*A3_*",LEAFCLIP_ALL[value]$sample)) {
    LEAFCLIP_ALL[value]$array<-"Array_3"
  }else if (grepl("*A6_*",LEAFCLIP_ALL[value]$sample)) {
    LEAFCLIP_ALL[value]$array<-"Array_6"
  }else if (grepl("*A1_*",LEAFCLIP_ALL[value]$sample)) {
    LEAFCLIP_ALL[value]$array<-"Array_1"
  }
}
LEAFCLIP_ALL$month_array<-str_c(LEAFCLIP_ALL$month,"_", LEAFCLIP_ALL$array)

#save hyperspec data files 
save(LEAFCLIP_ALL, file="~/OneDrive - Nexus365/01 OXFORD PHD/Field Work 2023/SVC_2023/all_bifor/LEAFCLIP_ALL.RData")
write.txt.wide(LEAFCLIP_ALL,file="~/OneDrive - Nexus365/01 OXFORD PHD/Field Work 2023/SVC_2023/all_bifor/LEAFCLIP_ALL.txt" )

#plotting all spectra just to see
plot(LEAFCLIP_ALL, "spcmeansd")
plot(LEAFCLIP_ALL, "spcprctl5")

#Need to remove outing saturated or empty spectra 
#(eg saturated spectra when leafclip slips off leaf and opens, resulting in saturation warning; 
#or empty spectra when the leafclip remains closed but the leaf comes out)
#removing bad spectra
high.lim<-apply(LEAFCLIP_ALL> 0.8,1, any) #any point with intensity of 1 is excluded
low.lim<-apply(LEAFCLIP_ALL, 1, max)<0.1#the maximum intensity should be at least 0.1 
LEAFCLIP_minmax<-LEAFCLIP_ALL[! high.lim & ! low.lim]
plot(LEAFCLIP_minmax, "spcprctl5")
write.txt.wide(LEAFCLIP_minmax,file="~/OneDrive - Nexus365/01 OXFORD PHD/Field Work 2023/SVC_2023/all_bifor/LEAFCLIP_minmax.txt" )

#subtracting the overall composition
fifth_spc<-LEAFCLIP_minmax-quantile(LEAFCLIP_minmax, 0.05)
plot(fifth_spc,"spcprctile")
#average spectra per month and array
means<-aggregate(LEAFCLIP_minmax, LEAFCLIP_minmax$month_array, mean)
write.txt.wide(means,file="~/OneDrive - Nexus365/01 OXFORD PHD/Field Work 2023/SVC_2023/all_bifor/LEAFCLIP_MEANS.txt" )
write.csv(means,file="~/OneDrive - Nexus365/01 OXFORD PHD/Field Work 2023/SVC_2023/all_bifor/LEAFCLIP_MEANS.csv")
qplotspc(means)+aes(colour=month_array)

#hyperspec object of monthly average per array
A3_means<-subset(means, array=="Array_3")
qplotspc(A3_means)+aes(colour=month)

A5_means<-subset(means, array=="Array_5")
qplotspc(A5_means)+aes(colour=month)

A6_means<-subset(means, array=="Array_6")
qplotspc(A6_means)+aes(colour=month)

A1_means<-subset(means, array=="Array_1")
qplotspc(A1_means)+aes(colour=month)





