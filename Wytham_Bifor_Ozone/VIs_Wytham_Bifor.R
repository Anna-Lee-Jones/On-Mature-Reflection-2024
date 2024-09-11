#Calculate VIs from merged QWytham and BiFOR hyperspectral library
#load packages
library(hyperSpec)
library(ggplot2)
library(stats)
library(ggpubr)
library(grid)
library(BayesFactor)
#set working directory
setwd("~/Library/CloudStorage/OneDrive-Nexus365/01 OXFORD PHD/Field Work 2023/R_Data_Analysis")
#load merged hyperspectral library (QWytham and BiFOR)
load(file = "~/Library/CloudStorage/OneDrive-Nexus365/01 OXFORD PHD/Field Work 2023/R_Data_Analysis/QWYTHAM_BIFOR_merged.RData")
#colours
colours<-c("BiFOR"="#f35e5a", "Wytham"="#17b3b7")
colours
#calculate VIs per spectra as variable
merged$OZDI<-as.numeric(((merged[,,2204]$spc)-(merged[,,2248]$spc))/((merged[,,2203]$spc)+(merged[,,2253]$spc)))
merged$NDVI<-as.numeric(((merged[,,800]$spc)-(merged[,,670]$spc))/((merged[,,800]$spc)+(merged[,,670]$spc)))
merged$MCARI<-as.numeric((((merged[,,700]$spc)-(merged[,,670]$spc))-0.2*((merged[,,700]$spc)-(merged[,,550]$spc)))*((merged[,,700]$spc)/(merged[,,670]$spc)))
merged$PRI<-as.numeric(((merged[,,570]$spc)-(merged[,,530]$spc))/((merged[,,570]$spc)+(merged[,,530]$spc)))
merged$PSRI<-as.numeric(((merged[,,680]$spc)-(merged[,,500]$spc))/((merged[,,680]$spc)+(merged[,,500]$spc)))
merged$site_date<-paste(merged$site, merged$date)
merged$ndsi<-as.numeric(((merged[,,515]$spc)-(merged[,,647]$spc))/((merged[,,515]$spc)+(merged[,,647]$spc)))


VIdf<-data.frame(merged$site, merged$date, merged$site_date ,merged$ozone,merged$OZDI,merged$NDVI,merged$MCARI, merged$PRI, merged$PSRI,merged$ndsi)#needed for ggplots
colnames(VIdf)<-c("site", "date", "site_date","ozone","OZDI","NDVI","MCARI","PRI","PSRI", "NDSI_max")


VIdf<-as.data.frame.list(VIdf)
means_VI<-aggregate(VIdf, list(VIdf$site_date, VIdf$site),mean)
means_VI<-means_VI[-5]
means_VI<-means_VI[-3]
colnames(means_VI)<-c("site_date", "site", "date",  "ozone", "OZDI" ,"NDVI", "MCARI", "PRI", "PSRI", "NDSI_max" )

a<-ggplot(means_VI, aes(x=ozone,y=NDVI,color=site))+geom_point()+scale_color_manual(values=c("#f35e5a","#17b3b7"))+xlab("")
b<-ggplot(means_VI, aes(x=ozone,y=MCARI,color=site))+geom_point()+scale_color_manual(values=c("#f35e5a","#17b3b7"))+xlab("")
c<-ggplot(means_VI, aes(x=ozone,y=PRI,color=site))+geom_point()+scale_color_manual(values=c("#f35e5a","#17b3b7"))+xlab("")
d<-ggplot(means_VI, aes(x=ozone,y=PSRI,color=site))+geom_point()+scale_color_manual(values=c("#f35e5a","#17b3b7"))+xlab("")
e<-ggplot(means_VI, aes(x=ozone,y=NDSI_max,color=site))+geom_point()+scale_color_manual(values=c("#f35e5a","#17b3b7"))+ylab("Max Pearson NDSI")+xlab("")
f<-ggplot(means_VI, aes(x=ozone,y=OZDI,color=site))+geom_point()+scale_color_manual(values=c("#f35e5a","#17b3b7"))+xlab("")

fig<-ggarrange(a,b,c,d,e,f, ncol=3,nrow=2, common.legend=TRUE, labels = "auto")
annotate_figure(fig, bottom = textGrob("Ozone Concentration (ppb)", gp = gpar(cex = 1)))


#MCARI and NDVI SHOWS QUITE GOOD DIFFRENTIATION BETWEEN SITES and correlation to ozone

#correlative test of VIs to ozone concentration 
#ozone conc and VIs roughly normal distribution, but potentially the correlations aren't linear
ggqqplot(means_VI$NDSI_max, ylab = "NDSI")

cor.test(means_VI$ozone,means_VI$NDVI, method=c("pearson"))
# t = 1.9243, df = 10, p-value = 0.08321
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#   -0.07704769  0.84241738
# sample estimates:
#   cor 
# 0.5198401 
cor.test(means_VI$ozone,means_VI$MCARI, method=c("pearson"))
# t = 4.632, df = 10, p-value = 0.0009333
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#   0.4790428 0.9496677
# sample estimates:
#   cor 
# 0.8258883 
cor.test(means_VI$ozone,means_VI$PRI, method=c("pearson"))
# t = 0.086416, df = 10, p-value = 0.9328
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#   -0.5552900  0.5919386
# sample estimates:
#   cor 
# 0.02731701
cor.test(means_VI$ozone,means_VI$PSRI, method=c("pearson"))
# t = -2.1516, df = 10, p-value = 0.05691
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#   -0.8590875  0.0167902
# sample estimates:
#   cor 
# -0.5625319
cor.test(means_VI$ozone,means_VI$NDSI_max, method=c("pearson"))
# t = 4.883, df = 10, p-value = 0.0006391
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#   0.5121794 0.9538042
# sample estimates:
#   cor 
# 0.8393593
cor.test(means_VI$ozone,means_VI$OZDI, method=c("pearson"))
# t = -0.99677, df = 10, p-value = 0.3424
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#   -0.7458470  0.3302539
# sample estimates:
#   cor 
# -0.3006264

