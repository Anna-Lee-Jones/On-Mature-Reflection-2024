#Combining BIFOR and WYTHAM hyperspectral libraries

#Load relevant packages
library(hyperSpec)
library(ggplot2)
library(viridis)
library(stringr)
library(stats)
library(FactoMineR)
library(factoextra)

# Load objects in projectimage.RData into my workspace
load(file = "~/Library/CloudStorage/OneDrive-Nexus365/01 OXFORD PHD/Field Work 2023/R_Data_Analysis/BIFOR_LEAFCLIP_MINMAX.RData")
BIFOR<-LEAFCLIP_minmax
load(file = "~/Library/CloudStorage/OneDrive-Nexus365/01 OXFORD PHD/Field Work 2023/R_Data_Analysis/WYTHAM_LEAFCLIP_minmax.RData")
WYTHAM<-LEAFCLIP_minmax
#should I filter to just oak here?
#filter wytham data to only quercus measurements
QWYTHAM<-subset(WYTHAM,species=="oak" )

merged<-collapse(BIFOR,QWYTHAM)
merged$ozone<-as.numeric(merged$ozone)

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

#PCA PLOTS
#PCA
PCA<-prcomp(merged)
summary(PCA)
pca1<-PCA$rotation[,1]
pca2<-PCA$rotation[,2]
pca3<-PCA$rotation[,3]
pca4<-PCA$rotation[,4]

plot(350:2500,pca4, type='l', lwd=2, col="blue", xlab="Wavelength (nm)", ylab="PC Loading", )
lines(350:2500, pca2, type='l', lwd=2, col="red")
lines(350:2500, pca3, type='l', lwd=2, col="green")    
lines(350:2500, pca1, type='l', lwd=2, col="black")  
legend(1980,0.067,legend = c("Component 1","Component 2","Component 3","Component 4"), col = c("black","red","green", "blue"), lwd =2, cex=0.8)




labsdf<-merged
labsdf$SITE<-c(merged$site)
labsdf$DATE<-c(merged$date)
labsdf$OZ<-c(merged$ozone)
labsdf$SPP<-c(merged$species)
df<-as.matrix(merged)

pca<-PCA(df, scale.unit = TRUE, ncp=30, graph=FALSE)
PLOT<-fviz_pca_ind(pca, geom.ind = "point", axes=c(1,2),
                   col.ind = labsdf$SITE,# color by sites
                   palette = 'all',
                   addEllipses = FALSE, ellipse.type = "convex",
                   legend.title = "Site")+ggtitle("")
PLOT

fviz_pca_ind(pca, geom.ind = "point",
             col.ind = labsdf$DATE,# color by date
             palette = 'all',
             addEllipses = FALSE, ellipse.type = "convex",
             legend.title = "Date")+ggtitle("")

fviz_pca_ind(pca, geom.ind = "point",
             col.ind = labsdf$OZ,# color by OZONE
             palette = 'all',
             addEllipses = FALSE, ellipse.type = "convex",
             legend.title = "Ozone")+ggtitle("")


#calculate OZDI per spectra as variable
merged$OZDI<-as.numeric(((merged[,,2203]$spc)-(merged[,,2253]$spc))/((merged[,,2203]$spc)+(merged[,,2253]$spc)))
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
