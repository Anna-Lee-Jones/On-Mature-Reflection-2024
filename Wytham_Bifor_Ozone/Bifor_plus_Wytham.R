#Combining BIFOR and WYTHAM hyperspectral libraries

#Load relevant packages
library(hyperSpec)
library(ggplot2)
library(viridis)
library(stringr)
library(stats)
library(FactoMineR)
library(factoextra)
library(vegan)
library(ggpubr)
setwd("~/Library/CloudStorage/OneDrive-Nexus365/01 OXFORD PHD/Field Work 2023/R_Data_Analysis")
# Load objects in projectimage.RData into my workspace
load(file = "~/Library/CloudStorage/OneDrive-Nexus365/01 OXFORD PHD/Field Work 2023/R_Data_Analysis/BIFOR_LEAFCLIP_MINMAX.RData")
BIFOR<-LEAFCLIP_minmax
load(file = "~/Library/CloudStorage/OneDrive-Nexus365/01 OXFORD PHD/Field Work 2023/R_Data_Analysis/WYTHAM_LEAFCLIP_minmax.RData")
WYTHAM<-LEAFCLIP_minmax
#should I filter to just oak here?
#filter wytham data to only quercus measurements
QWYTHAM<-subset(WYTHAM,species=="oak" )
#merge Qwytham and Bifor into one hyperspec object
merged<-collapse(BIFOR,QWYTHAM)
#get rid of AsIs class for $spc matrix
unAsIs <- function(X) {
  if("AsIs" %in% class(X)) {
    class(X) <- class(X)[-match("AsIs", class(X))]
  }
  X
}
merged$spc<-unAsIs(merged$spc)
merged$ozone<-as.numeric(merged$ozone)

#save the merged hyperspec object
save(merged, file="~/Library/CloudStorage/OneDrive-Nexus365/01 OXFORD PHD/Field Work 2023/R_Data_Analysis/QWYTHAM_BIFOR_merged.RData")

#skip to here
load(file = "~/Library/CloudStorage/OneDrive-Nexus365/01 OXFORD PHD/Field Work 2023/R_Data_Analysis/QWYTHAM_BIFOR_merged.RData")


site_means<-aggregate(merged, merged$site, mean)

qplotspc(site_means)+aes(colour=site)+labs(x="Wavelength (nm)",y="% Reflectance",colour="Site")


#calculate mean spectra per site and standard deviation
bifor_mean_SD<-as.t.df(apply(BIFOR,2, mean_pm_sd))
head(bifor_mean_SD)
Qwytham_mean_SD<-as.t.df(apply(QWYTHAM,2, mean_pm_sd))
head(Qwytham_mean_SD)

#plot the mean spectra per site with ribbon of standard deviation. Manual scale used because two datasets.
colours<-c("BiFOR"="#9c0315", "Wytham"="#200485")
colours
ggplot()+geom_ribbon (data=bifor_mean_SD, aes (x=.wavelength,ymin = mean.minus.sd, ymax = mean.plus.sd),fill = "#f2bbc170")+
  geom_line (data=bifor_mean_SD, aes (x=.wavelength,y = mean, color="BiFOR"))+
  geom_ribbon (data=Qwytham_mean_SD, aes (x=.wavelength,ymin = mean.minus.sd, ymax = mean.plus.sd),fill = "#b09fed70")+
  geom_line (data=Qwytham_mean_SD, aes (x=.wavelength,y = mean,color="Wytham"))+
  scale_color_manual(values=colours)+labs(x="Wavelength (nm)",y="% Reflectance",colour="site" )


