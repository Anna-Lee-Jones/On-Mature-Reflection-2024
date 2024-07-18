#Analysis of ambient vs elevated CO2 treatment's reflectance at BIFOR
#Load relevant packages
library(hyperSpec)
library(ggplot2)
library(viridis)
library(stringr)
library(stats)
library(FactoMineR)
library(factoextra)

#Load bifor spectral library
load(file = "~/Library/CloudStorage/OneDrive-Nexus365/01 OXFORD PHD/Field Work 2023/R_Data_Analysis/BIFOR_LEAFCLIP_MINMAX.RData")
#means per CO2 treatment
BIFOR<-LEAFCLIP_minmax
CO2_means<-aggregate(LEAFCLIP_minmax, LEAFCLIP_minmax$CO2, mean)
qplotspc(CO2_means)+aes(colour=CO2)+labs(x="Wavelength (nm)",y="% Reflectance",colour=expression(CO[2]*" Treatment") )
#at first glance leaves from elevated CO2 arrays (6+1) don't have a clear
#pattern in the triple bump, but decreased green peak reflectance, shallower
#absorption troughs at the water absoroption peaks, and increased reflectance
# between 1500-1800nm. In SWIR ~2250nm peak, somewhat increased in elevated CO2
#average spectra per month and array

elevated<-subset(LEAFCLIP_minmax, CO2=="Elevated")
elevated_mean_SD<-as.t.df(apply(elevated,2, mean_pm_sd))
head(elevated_mean_SD)

ambient<-subset(LEAFCLIP_minmax, CO2=="Ambient")
ambient_mean_SD<-as.t.df(apply(ambient,2, mean_pm_sd))
head(ambient_mean_SD)


colours<-c("Elevated"="#9c0315", "Ambient"="#200485")
colours
ggplot()+geom_ribbon (data=elevated_mean_SD, aes (x=.wavelength,ymin = mean.minus.sd, ymax = mean.plus.sd),fill = "#f2bbc170")+
  geom_line (data=elevated_mean_SD, aes (x=.wavelength,y = mean, color="Elevated"))+
  geom_ribbon (data=ambient_mean_SD, aes (x=.wavelength,ymin = mean.minus.sd, ymax = mean.plus.sd),fill = "#b09fed70")+
  geom_line (data=ambient_mean_SD, aes (x=.wavelength,y = mean,color="Ambient"))+
  scale_color_manual(values=colours)+labs(x="Wavelength (nm)",y="% Reflectance",colour=expression(CO[2]*" Treatment") )

#now to do BHattacharyya distance test to measure spectral separation
#also could do PCA weighting plots and clustering

#PCA
PCA<-prcomp(BIFOR)
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




labsdf<-BIFOR
labsdf$CO2<-c(BIFOR$CO2)
labsdf$DATE<-c(BIFOR$date)
labsdf$OZ<-c(BIFOR$ozone)
df<-BIFOR$spc
df<-as.data.frame(df)

pca<-PCA(df, scale.unit = TRUE, ncp=3, graph=FALSE)
PLOT<-fviz_pca_ind(pca, geom.ind = "point", axes=c(2,3),
                col.ind = labsdf$CO2,# color by groups
                palette = 'all',
                addEllipses = FALSE, ellipse.type = "convex",
                legend.title = "CO2 Treatment")+ggtitle("")
PLOT

fviz_pca_ind(pca, geom.ind = "point",
             col.ind = labsdf$date,# color by groups
             palette = 'all',
             addEllipses = FALSE, ellipse.type = "convex",
             legend.title = "CO2 Treatment")+ggtitle("")


#Bhattacharyya spectral separability is designed for multivariate spectra, I get an singular error bc matrix of spc can't be inverted
#PLSDA IS MORE APPROPRIATE


