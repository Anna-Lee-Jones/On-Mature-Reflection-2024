#Calculate VIs from merged QWytham and BiFOR hyperspectral library
#load packages
library(hyperSpec)
library(ggplot2)
library(stats)
#set working directory
setwd("~/Library/CloudStorage/OneDrive-Nexus365/01 OXFORD PHD/Field Work 2023/R_Data_Analysis")
#load merged hyperspectral library (QWytham and BiFOR)
load(file = "~/Library/CloudStorage/OneDrive-Nexus365/01 OXFORD PHD/Field Work 2023/R_Data_Analysis/QWYTHAM_BIFOR_merged.RData")

#calculate VIs per spectra as variable
merged$OZDI<-as.numeric(((merged[,,2204]$spc)-(merged[,,2248]$spc))/((merged[,,2203]$spc)+(merged[,,2253]$spc)))
merged$NDVI<-as.numeric(((merged[,,800]$spc)-(merged[,,670]$spc))/((merged[,,800]$spc)+(merged[,,670]$spc)))
merged$MCARI<-as.numeric((((merged[,,700]$spc)-(merged[,,670]$spc))-0.2*((merged[,,700]$spc)-(merged[,,550]$spc)))*((merged[,,700]$spc)/(merged[,,670]$spc)))
merged$PRI<-as.numeric(((merged[,,570]$spc)-(merged[,,530]$spc))/((merged[,,570]$spc)+(merged[,,530]$spc)))
merged$PSRI<-as.numeric(((merged[,,680]$spc)-(merged[,,500]$spc))/((merged[,,680]$spc)+(merged[,,500]$spc)))
merged$site_date<-paste(merged$site, merged$date)
merged$TESTH<-as.numeric(((merged[,,515]$spc)-(merged[,,647]$spc))/((merged[,,515]$spc)+(merged[,,647]$spc)))
merged$TESTL<-as.numeric(((merged[,,480]$spc)-(merged[,,405]$spc))/((merged[,,480]$spc)+(merged[,,405]$spc)))


VIdf<-data.frame(merged$site, merged$date, merged$site_date ,merged$ozone,merged$OZDI,merged$NDVI,merged$MCARI, merged$PRI, merged$PSRI,merged$TESTH,merged$TESTL)#needed for ggplots
colnames(VIdf)<-c("site", "date", "site_date","ozone","OZDI","NDVI","MCARI","PRI","PSRI", "TESTH","TESTL")


VIdf<-as.data.frame.list(VIdf)
means_VI<-aggregate(VIdf, list(VIdf$site_date, VIdf$site),mean)
means_VI<-means_VI[-5]
means_VI<-means_VI[-3]
colnames(means_VI)<-c("site_date", "site", "date",  "ozone", "OZDI" ,"NDVI", "MCARI", "PRI", "PSRI", "TESTH","TESTL" )

ggplot(means_VI, aes(x=ozone,y=TESTH,color=site))+geom_point()+scale_color_manual(values=c("red","blue"))+ylab("High Pearson NDSI")
ggplot(means_VI, aes(x=ozone,y=TESTL,color=site))+geom_point()+scale_color_manual(values=c("red","blue"))+ylab("Low Pearson NDSI")

ggplot(means_VI, aes(x=ozone,y=MCARI,color=site))+geom_point()+scale_color_manual(values=c("red","blue"))
ggplot(means_VI, aes(x=site,y=MCARI,color=site))+geom_boxplot()+scale_color_manual(values=c("red","blue"))

ggplot(means_VI, aes(x=ozone,y=OZDI,color=site))+geom_point()+scale_color_manual(values=c("red","blue"))
ggplot(means_VI, aes(x=site,y=OZDI,color=site))+geom_boxplot()+scale_color_manual(values=c("red","blue"))

ggplot(means_VI, aes(x=ozone,y=NDVI,color=site))+geom_point()+scale_color_manual(values=c("red","blue"))
# ggplot(means_VI, aes(x=site,y=NDVI,color=site))+geom_boxplot()+scale_color_manual(values=c("red","blue"))

ggplot(means_VI, aes(x=ozone,y=PRI,color=site))+geom_point()+scale_color_manual(values=c("red","blue"))
ggplot(means_VI, aes(x=site,y=PRI,color=site))+geom_boxplot()+scale_color_manual(values=c("red","blue"))

ggplot(means_VI, aes(x=ozone,y=PSRI,color=site))+geom_point()+scale_color_manual(values=c("red","blue"))
ggplot(means_VI, aes(x=site,y=PSRI,color=site))+geom_boxplot()+scale_color_manual(values=c("red","blue"))
#MCARI and NDVI SHOWS QUITE GOOD DIFFRENTIATION BETWEEN SITES and correlation to ozone

#correlative test of MCARI to ozone concentration 