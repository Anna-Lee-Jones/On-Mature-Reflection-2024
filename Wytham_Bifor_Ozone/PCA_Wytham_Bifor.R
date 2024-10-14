# Principle component analysis for BIFOR and QWYTHAM merged 
#Load relevant packages
library(hyperSpec)
library(stats)
library(FactoMineR)
library(factoextra)
library(ggpubr)
library(lubridate)
#setwd
setwd("~/Library/CloudStorage/OneDrive-Nexus365/01 OXFORD PHD/Field Work 2023/R_Data_Analysis")
#load merged hyperspectral library
load(file = "~/Library/CloudStorage/OneDrive-Nexus365/01 OXFORD PHD/Field Work 2023/R_Data_Analysis/QWYTHAM_BIFOR_merged.RData")
colours<-c("BiFOR"="#f35e5a", "Wytham"="#17b3b7")

#PCA by prcomp
PCA<-prcomp(merged)
PCA_four<-prcomp(merged,rank.=4)#just the first four PCs
summary(PCA_four)#automatic calculation of proportion of variance explained
#manual calculation of variance explained:
PCA_var<-PCA$sdev^2#calculate variance explained by each PC from standard dev ^2
PCA_VE<-PCA_var/sum(PCA_var) #VE= proportion of Variance explained, Variance explained per PC /total variance explained by all PCs
PCA_VE[1:4] #the first four PC explain 0.988 of the variance in the data. 
four_VE<-sum(PCA_VE[1:4]) #variance explained by first four PCA
pca1<-PCA$rotation[,1]
pca2<-PCA$rotation[,2]
pca3<-PCA$rotation[,3]
pca4<-PCA$rotation[,4]

plot(350:2500,pca4, type='l', lwd=2, col="#17b3b7", xlab="Wavelength (nm)", ylab="PC Loading", )
lines(350:2500, pca2, type='l', lwd=2, col="#f35e5a")
lines(350:2500, pca3, type='l', lwd=2, col="#f1c40f")    
lines(350:2500, pca1, type='l', lwd=2, col="black")  
legend(1455,0.105,legend = c("Component 1","Component 2","Component 3","Component 4"), col = c("black","#f35e5a","#f1c40f", "#17b3b7"), lwd =2, cex=0.8, bty="n")
loadingsplot<-recordPlot()
loadingsplot
#other PCA method
labsdf<-merged
labsdf$SITE<-c(merged$site)
labsdf$DATE<-c(merged$date)
labsdf$OZ<-c(merged$ozone)
labsdf$SPP<-c(merged$species)
df<-as.matrix(merged)
pca<-PCA(df, scale.unit = TRUE, ncp=30, graph=FALSE)
#scree plot
fviz_eig(pca, addlabels=TRUE)
#PC multiplot
PC12<-fviz_pca_ind(pca, geom.ind = "point", axes=c(1,2),
                   col.ind = labsdf$SITE,# color by sites
                   palette = 'all',
                   addEllipses = FALSE, ellipse.type = "convex",
                   legend.title = "Site")+ggtitle("")
PC12
PC23<-fviz_pca_ind(pca, geom.ind = "point", axes=c(2,3),
                   col.ind = labsdf$SITE,# color by sites
                   palette = 'all',
                   addEllipses = FALSE, ellipse.type = "convex",
                   legend.title = "Site")+ggtitle("")
PC23
PC13<-fviz_pca_ind(pca, geom.ind = "point", axes=c(1,3),
                   col.ind = labsdf$SITE,# color by sites
                   palette = 'all',
                   addEllipses = FALSE, ellipse.type = "convex",
                   legend.title = "Site")+ggtitle("")
PC13


ggarrange(PC12,PC23,PC13,loadingsplot, common.legend=TRUE, labels=c("B","C","D","E"))

#other variable PC plots
fviz_pca_ind(pca, geom.ind = "point",
             col.ind = labsdf$DATE,# color by date
             palette = 'all',
             addEllipses = FALSE, ellipse.type = "convex",
             legend.title = "Date")+ggtitle("")
labsdf$yday<-yday(labsdf$DATE)
fviz_pca_ind(pca, geom.ind = "point",
             col.ind = labsdf$yday,# color by date
             palette = 'all',
             addEllipses = FALSE, ellipse.type = "convex",
             legend.title = "Day of Year")+ggtitle("")+scale_color_continuous(type = "viridis")


fviz_pca_ind(pca, geom.ind = "point",axes = c(1,2),
             col.ind = labsdf$OZ,# color by OZONE
             palette = 'all',
             addEllipses = FALSE, ellipse.type = "convex",
             legend.title = "Ozone")+ggtitle("")+scale_color_continuous(type = "viridis")

