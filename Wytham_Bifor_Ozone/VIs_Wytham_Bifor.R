#Calculate VIs from merged QWytham and BiFOR hyperspectral library
#load packages
library(hyperSpec)
library(ggplot2)
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

VIdf<-data.frame(merged$site ,merged$ozone,merged$OZDI,merged$NDVI,merged$MCARI, merged$PRI, merged$PSRI)#needed for ggplots
colnames(VIdf)<-c("site","ozone","OZDI","NDVI","MCARI","PRI","PSRI")
as.factor(VIdf$site)

ggplot(VIdf, aes(x=ozone,y=MCARI,color=site))+geom_point()+scale_color_manual(values=c("red","blue"))
ggplot(VIdf, aes(x=site,y=MCARI,color=site))+geom_boxplot()+scale_color_manual(values=c("red","blue"))
ggplot(VIdf, aes(x=site,y=OZDI,color=site))+geom_boxplot()+scale_color_manual(values=c("red","blue"))
ggplot(VIdf, aes(x=site,y=NDVI,color=site))+geom_boxplot()+scale_color_manual(values=c("red","blue"))
ggplot(VIdf, aes(x=site,y=PRI,color=site))+geom_boxplot()+scale_color_manual(values=c("red","blue"))
ggplot(VIdf, aes(x=site,y=PSRI,color=site))+geom_boxplot()+scale_color_manual(values=c("red","blue"))
#MCARI SHOWS QUITE GOOD DIFFRENTIATION BETWEEN SITES
