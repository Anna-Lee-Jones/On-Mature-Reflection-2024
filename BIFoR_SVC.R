#SVC Analysis of all BIFoR Spectra
#based on code from "SVC Leaf Clip Original Analysis" used in 2022

#Load relevant packages
library(hyperSpec)
library(ggplot2)
library(viridis)
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
LEAFCLIP_ALL@data$site<-"BIFOR"
LEAFCLIP_ALL@data$date<-""
LEAFCLIP_ALL@data$array<-""
LEAFCLIP_ALL@data$ozone<-""
LEAFCLIP_ALL@data$tree<-""
LEAFCLIP_ALL@data$CO2<-""
#assign date
for (value in 1:length){
  LEAFCLIP_ALL[value]$sample<-toupper(LEAFCLIP_ALL[value]$sample)
  if (grepl("X160623_*",LEAFCLIP_ALL[value]$sample)){
    LEAFCLIP_ALL[value]$date<-"2023-06-16"
  }else if (grepl("X220623_*",LEAFCLIP_ALL[value]$sample)) {
    LEAFCLIP_ALL[value]$date<-"2023-06-22"
  }else if (grepl("X050723_*",LEAFCLIP_ALL[value]$sample)) {
    LEAFCLIP_ALL[value]$date<-"2023-07-05"
  }else if (grepl("X250723_*",LEAFCLIP_ALL[value]$sample)) {
    LEAFCLIP_ALL[value]$date<-"2023-07-25"
  }else if (grepl("X100823_*",LEAFCLIP_ALL[value]$sample)) {
    LEAFCLIP_ALL[value]$date<-"2023-08-10"
  }else if (grepl("X240823_*",LEAFCLIP_ALL[value]$sample)) {
    LEAFCLIP_ALL[value]$date<-"2023-08-24"
  }else if (grepl("X130923_*",LEAFCLIP_ALL[value]$sample)) {
    LEAFCLIP_ALL[value]$date<-"2023-09-13"
  }else if (grepl("X061023_*",LEAFCLIP_ALL[value]$sample)) {
    LEAFCLIP_ALL[value]$date<-"2023-10-06"
  }
}
#convert strings to date objects
LEAFCLIP_ALL$date<-as.Date(LEAFCLIP_ALL$date)

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

#assign CO2 treatment
for (value in 1:length){
  LEAFCLIP_ALL[value]$sample<-toupper(LEAFCLIP_ALL[value]$sample)
  if (grepl("*A5_*",LEAFCLIP_ALL[value]$sample)){
    LEAFCLIP_ALL[value]$CO2<-"Ambient"
  }else if (grepl("*A3_*",LEAFCLIP_ALL[value]$sample)) {
    LEAFCLIP_ALL[value]$CO2<-"Ambient"
  }else if (grepl("*A6_*",LEAFCLIP_ALL[value]$sample)) {
    LEAFCLIP_ALL[value]$CO2<-"Elevated"
  }else if (grepl("*A1_*",LEAFCLIP_ALL[value]$sample)) {
    LEAFCLIP_ALL[value]$CO2<-"Elevated"
  }
}
#assign tree
for (value in 1:length){
  LEAFCLIP_ALL[value]$sample<-toupper(LEAFCLIP_ALL[value]$sample)
  if (grepl("*_T8467.*",LEAFCLIP_ALL[value]$sample)){
    LEAFCLIP_ALL[value]$tree<-"T8467"
  }else if (grepl("*_T8468.*",LEAFCLIP_ALL[value]$sample)) {
    LEAFCLIP_ALL[value]$tree<-"T8468"
  }else if (grepl("*_T8351.*",LEAFCLIP_ALL[value]$sample)) {
    LEAFCLIP_ALL[value]$tree<-"T8351"
  }else if (grepl("*_T9301.*",LEAFCLIP_ALL[value]$sample)) {
    LEAFCLIP_ALL[value]$tree<-"T9301"
  }else if (grepl("*_T6382.*",LEAFCLIP_ALL[value]$sample)) {
    LEAFCLIP_ALL[value]$tree<-"T6382"
  }else if (grepl("*_T6384.*",LEAFCLIP_ALL[value]$sample)) {
    LEAFCLIP_ALL[value]$tree<-"T6384"
  }else if (grepl("*_T3572.*",LEAFCLIP_ALL[value]$sample)) {
    LEAFCLIP_ALL[value]$tree<-"T3572"
  }else if (grepl("*_T5846.*",LEAFCLIP_ALL[value]$sample)) {
    LEAFCLIP_ALL[value]$tree<-"T5846"
  }else if (grepl("*_T6406.*",LEAFCLIP_ALL[value]$sample)) {
    LEAFCLIP_ALL[value]$tree<-"T6406."
  }
}


#assign avg ozone conc in the two weeks prior to reflectance measurement
#assign date
for (value in 1:length){
  LEAFCLIP_ALL[value]$sample<-toupper(LEAFCLIP_ALL[value]$sample)
  if (grepl("2023-06-16",LEAFCLIP_ALL[value]$date)){
    LEAFCLIP_ALL[value]$ozone<-NA
  }else if (grepl("2023-06-22",LEAFCLIP_ALL[value]$date)) {
    LEAFCLIP_ALL[value]$ozone<-NA
  }else if (grepl("2023-07-05",LEAFCLIP_ALL[value]$date)) {
    LEAFCLIP_ALL[value]$ozone<-27.70
  }else if (grepl("2023-07-25",LEAFCLIP_ALL[value]$date)) {
    LEAFCLIP_ALL[value]$ozone<-26.71
  }else if (grepl("2023-08-10",LEAFCLIP_ALL[value]$date)) {
    LEAFCLIP_ALL[value]$ozone<-24.25
  }else if (grepl("2023-08-24",LEAFCLIP_ALL[value]$date)) {
    LEAFCLIP_ALL[value]$ozone<-27.70
  }else if (grepl("2023-09-13",LEAFCLIP_ALL[value]$date)) {
    LEAFCLIP_ALL[value]$ozone<-27.75
  }else if (grepl("2023-10-06",LEAFCLIP_ALL[value]$date)) {
    LEAFCLIP_ALL[value]$ozone<-27.23
  }
}
#assign a variable that combines array and month
LEAFCLIP_ALL$date_array<-str_c(LEAFCLIP_ALL$date,"_", LEAFCLIP_ALL$array)

#save hyperspec data files 
save(LEAFCLIP_ALL, file="~/OneDrive - Nexus365/01 OXFORD PHD/Field Work 2023/SVC_2023/all_bifor/BIFOR_LEAFCLIP_ALL.RData")

#plotting all spectra just to see
plot(LEAFCLIP_ALL, "spcmeansd")

#Need to remove outing saturated or empty spectra 
#(eg saturated spectra when leafclip slips off leaf and opens, resulting in saturation warning; 
#or empty spectra when the leafclip remains closed but the leaf comes out)
#removing bad spectra
high.lim<-apply(LEAFCLIP_ALL> 0.8,1, any) #any point with intensity of 1 is excluded
low.lim<-apply(LEAFCLIP_ALL, 1, max)<0.1#the maximum intensity should be at least 0.1 
LEAFCLIP_minmax<-LEAFCLIP_ALL[! high.lim & ! low.lim]
plot(LEAFCLIP_minmax, "spcmeansd")
save(LEAFCLIP_minmax, file="~/Library/CloudStorage/OneDrive-Nexus365/01 OXFORD PHD/Field Work 2023/R_Data_Analysis/BIFOR_LEAFCLIP_MINMAX.RData")

#skip to here
load(file = "~/Library/CloudStorage/OneDrive-Nexus365/01 OXFORD PHD/Field Work 2023/R_Data_Analysis/BIFOR_LEAFCLIP_MINMAX.RData")


#means per array
CO2_means<-aggregate(LEAFCLIP_minmax, LEAFCLIP_minmax$CO2, mean)
qplotspc(CO2_means)+aes(colour=CO2)+labs(x="Wavelength (nm)",y="% Reflectance",colour=expression(CO[2]*" Treatment") )
#at first glance leaves from elevated CO2 arrays (6+1) don't have a clear
#pattern in the triple bump, but decreased green peak reflectance, shallower
#absorption troughs at the water absoroption peaks, and increased reflectance
# between 1500-1800nm. In SWIR ~2250nm peak, somewhat increased in elevated CO2
#average spectra per month and array
means<-aggregate(LEAFCLIP_minmax, LEAFCLIP_minmax$date_array, mean)
save(means, file="~/OneDrive - Nexus365/01 OXFORD PHD/Field Work 2023/SVC_2023/all_bifor/BIFOR_LEAFCLIP_MEANS.RData")
qplotspc(means)+aes(colour=date_array)


#hyperspec object of monthly average per array
A3_means<-subset(means, array=="Array_3")
qplotspc(A3_means)+aes(colour=date)

A5_means<-subset(means, array=="Array_5")
qplotspc(A5_means)+aes(colour=date)

A6_means<-subset(means, array=="Array_6")
qplotspc(A6_means)+aes(colour=date)

A1_means<-subset(means, array=="Array_1")
qplotspc(A1_means)+aes(colour=date)

#highest reflectance in July, then decreases in August, and then increases until October




