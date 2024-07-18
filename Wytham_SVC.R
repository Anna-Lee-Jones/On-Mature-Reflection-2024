#SVC Analysis of all Wytham Spectra
#based on code from "SVC Leaf Clip Original Analysis" used in 2022

#Load relevant packages
library(hyperSpec)
library(ggplot2)
library(viridis)
library(stringr)

#load in csv file of ozone exposure here, formatted as Month (eg June 1 or August 2), PPB
#Ready to pair up with mean spectra based on the Month Half 

#import csv of all leaf clip hyperspectral data from jupyter notebook
LEAFCLIP_Hyperspectral<- read.csv("~/OneDrive - Nexus365/01 OXFORD PHD/Field Work 2023/SVC_2023/all_wytham/Wytham_Hyperspectral.csv", header=TRUE)
#create hyperspectral object 
LEAFCLIP_ALL<-new("hyperSpec", wavelength=LEAFCLIP_Hyperspectral$X, spc=t(LEAFCLIP_Hyperspectral [, -1]), data=data.frame(sample=colnames(LEAFCLIP_Hyperspectral[,-1])), labels=list(.wavelength="Wavelength",spc="I/a.u."))

value<-1
length<-length(LEAFCLIP_ALL)
#set up hyperspec variables for each date and array
#would add in the half monthly ozone dosage as another variable
LEAFCLIP_ALL$sample
LEAFCLIP_ALL@data$site<-"WYTHAM"
LEAFCLIP_ALL@data$date<-""
LEAFCLIP_ALL@data$ozone<-""
LEAFCLIP_ALL@data$tree<-""
LEAFCLIP_ALL@data$species<-""
#assign date
for (value in 1:length){
  LEAFCLIP_ALL[value]$sample<-toupper(LEAFCLIP_ALL[value]$sample)
  if (grepl("X010823_*",LEAFCLIP_ALL[value]$sample)){
    LEAFCLIP_ALL[value]$date<-"2023-08-01"
  }else if (grepl("X040923_*",LEAFCLIP_ALL[value]$sample)) {
    LEAFCLIP_ALL[value]$date<-"2023-09-04"
  }else if (grepl("X090823_*",LEAFCLIP_ALL[value]$sample)) {
    LEAFCLIP_ALL[value]$date<-"2023-08-09"
  }else if (grepl("X120723_*",LEAFCLIP_ALL[value]$sample)) {
    LEAFCLIP_ALL[value]$date<-"2023-07-12"
  }else if (grepl("X220923_*",LEAFCLIP_ALL[value]$sample)) {
    LEAFCLIP_ALL[value]$date<-"2023-09-22"
  }else if (grepl("X270623_*",LEAFCLIP_ALL[value]$sample)) {
    LEAFCLIP_ALL[value]$date<-"2023-06-27"
  }
}
#convert strings to date objects
LEAFCLIP_ALL$date<-as.Date(LEAFCLIP_ALL$date)

#assign tree
for (value in 1:length){
  LEAFCLIP_ALL[value]$sample<-toupper(LEAFCLIP_ALL[value]$sample)
  if (grepl("A",LEAFCLIP_ALL[value]$sample)){
    LEAFCLIP_ALL[value]$tree<-"A"
  }else if (grepl("B",LEAFCLIP_ALL[value]$sample)) {
    LEAFCLIP_ALL[value]$tree<-"B"
  }else if (grepl("C",LEAFCLIP_ALL[value]$sample)) {
    LEAFCLIP_ALL[value]$tree<-"C"
  }else if (grepl("D",LEAFCLIP_ALL[value]$sample)) {
    LEAFCLIP_ALL[value]$tree<-"D"
  }else if (grepl("E",LEAFCLIP_ALL[value]$sample)) {
    LEAFCLIP_ALL[value]$tree<-"E"
  }else if (grepl("F",LEAFCLIP_ALL[value]$sample)) {
    LEAFCLIP_ALL[value]$tree<-"F"
  }else if (grepl("G",LEAFCLIP_ALL[value]$sample)) {
    LEAFCLIP_ALL[value]$tree<-"G"
  }else if (grepl("H",LEAFCLIP_ALL[value]$sample)) {
    LEAFCLIP_ALL[value]$tree<-"H"
  }else if (grepl("I",LEAFCLIP_ALL[value]$sample)) {
    LEAFCLIP_ALL[value]$tree<-"I"
  }else if (grepl("J",LEAFCLIP_ALL[value]$sample)) {
    LEAFCLIP_ALL[value]$tree<-"J"
  }else if (grepl("K",LEAFCLIP_ALL[value]$sample)) {
    LEAFCLIP_ALL[value]$tree<-"K"
  }else if (grepl("L",LEAFCLIP_ALL[value]$sample)) {
    LEAFCLIP_ALL[value]$tree<-"L"
  }else if (grepl("M",LEAFCLIP_ALL[value]$sample)) {
    LEAFCLIP_ALL[value]$tree<-"M"
  }
}
for (value in 1:length){
  LEAFCLIP_ALL[value]$sample<-toupper(LEAFCLIP_ALL[value]$sample)
  if (grepl("A",LEAFCLIP_ALL[value]$sample)){
    LEAFCLIP_ALL[value]$species<-"oak"
  }else if (grepl("B",LEAFCLIP_ALL[value]$sample)) {
    LEAFCLIP_ALL[value]$species<-"sycamore"
  }else if (grepl("C",LEAFCLIP_ALL[value]$sample)) {
    LEAFCLIP_ALL[value]$species<-"sycamore"
  }else if (grepl("D",LEAFCLIP_ALL[value]$sample)) {
    LEAFCLIP_ALL[value]$species<-"oak"
  }else if (grepl("E",LEAFCLIP_ALL[value]$sample)) {
    LEAFCLIP_ALL[value]$species<-"sycamore"
  }else if (grepl("F",LEAFCLIP_ALL[value]$sample)) {
    LEAFCLIP_ALL[value]$species<-"sycamore"
  }else if (grepl("G",LEAFCLIP_ALL[value]$sample)) {
    LEAFCLIP_ALL[value]$species<-"sycamore"
  }else if (grepl("H",LEAFCLIP_ALL[value]$sample)) {
    LEAFCLIP_ALL[value]$species<-"sycamore"
  }else if (grepl("I",LEAFCLIP_ALL[value]$sample)) {
    LEAFCLIP_ALL[value]$species<-"sycamore"
  }else if (grepl("J",LEAFCLIP_ALL[value]$sample)) {
    LEAFCLIP_ALL[value]$species<-"oak"
  }else if (grepl("K",LEAFCLIP_ALL[value]$sample)) {
    LEAFCLIP_ALL[value]$species<-"beech"
  }else if (grepl("L",LEAFCLIP_ALL[value]$sample)) {
    LEAFCLIP_ALL[value]$species<-"sycamore"
  }else if (grepl("M",LEAFCLIP_ALL[value]$sample)) {
    LEAFCLIP_ALL[value]$species<-"oak"
  }
}
LEAFCLIP_ALL$date
#assign mean ozone concentration for the preceeding 2 weeks
for (value in 1:length){
  LEAFCLIP_ALL[value]$date<-toupper(LEAFCLIP_ALL[value]$date)
  if (grepl("2023-08-01",LEAFCLIP_ALL[value]$date)){
    LEAFCLIP_ALL[value]$ozone<-29.48
  }else if (grepl("2023-09-04",LEAFCLIP_ALL[value]$date)) {
    LEAFCLIP_ALL[value]$ozone<-34.98
  }else if (grepl("2023-08-09",LEAFCLIP_ALL[value]$date)) {
    LEAFCLIP_ALL[value]$ozone<-34.98
  }else if (grepl("2023-07-12",LEAFCLIP_ALL[value]$date)) {
    LEAFCLIP_ALL[value]$ozone<-36.10
  }else if (grepl("2023-09-22",LEAFCLIP_ALL[value]$date)) {
    LEAFCLIP_ALL[value]$ozone<-41.00
  }else if (grepl("2023-06-27",LEAFCLIP_ALL[value]$date)) {
    LEAFCLIP_ALL[value]$ozone<-53.54
  }
}

LEAFCLIP_ALL$ozone<-as.double(LEAFCLIP_ALL$ozone)
#assign a variable that combines tree and month
LEAFCLIP_ALL$date_species<-str_c(LEAFCLIP_ALL$date,"_", LEAFCLIP_ALL$species)

#save hyperspec data files 
save(LEAFCLIP_ALL, file="~/Library/CloudStorage/OneDrive-Nexus365/01 OXFORD PHD/Field Work 2023/R_Data_Analysis/WYTHAM_LEAFCLIP_minmax.RData")

#skip to here
load(file="~/Library/CloudStorage/OneDrive-Nexus365/01 OXFORD PHD/Field Work 2023/R_Data_Analysis/WYTHAM_LEAFCLIP_minmax.RData")
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
save(LEAFCLIP_minmax,file="~/OneDrive - Nexus365/01 OXFORD PHD/Field Work 2023/SVC_2023/all_wytham/LEAFCLIP_minmax.RData" )

#means per species#means per speciesozone
species_means<-aggregate(LEAFCLIP_minmax, LEAFCLIP_minmax$species, mean)
species_means$species<-as.factor(species_means$species)
qplotspc(species_means)+aes(colour=species)
#means per date
means<-aggregate(LEAFCLIP_minmax, LEAFCLIP_minmax$date, mean)
save(means,file="~/OneDrive - Nexus365/01 OXFORD PHD/Field Work 2023/SVC_2023/all_wytham/LEAFCLIP_DATE_MEANS.RData")
qplotspc(means)+aes(colour=ozone)


#output saved as R.data files, will import with whatever var name they had in session
