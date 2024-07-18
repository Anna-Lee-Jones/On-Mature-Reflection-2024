# Principle component analysis for BIFOR and QWYTHAM merged 
#Load relevant packages
library(hyperSpec)
library(stats)
library(FactoMineR)
library(factoextra)
library(ggpubr)
#setwd
setwd("~/Library/CloudStorage/OneDrive-Nexus365/01 OXFORD PHD/Field Work 2023/R_Data_Analysis")
#load merged hyperspectral library
load(file = "~/Library/CloudStorage/OneDrive-Nexus365/01 OXFORD PHD/Field Work 2023/R_Data_Analysis/QWYTHAM_BIFOR_merged.RData")


#PCA by prcomp
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
legend(1455,0.105,legend = c("Component 1","Component 2","Component 3","Component 4"), col = c("black","red","green", "blue"), lwd =2, cex=0.8)

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
PC23<-fviz_pca_ind(pca, geom.ind = "point", axes=c(2,3),
                   col.ind = labsdf$SITE,# color by sites
                   palette = 'all',
                   addEllipses = FALSE, ellipse.type = "convex",
                   legend.title = "Site")+ggtitle("")
PC34<-fviz_pca_ind(pca, geom.ind = "point", axes=c(3,4),
                   col.ind = labsdf$SITE,# color by sites
                   palette = 'all',
                   addEllipses = FALSE, ellipse.type = "convex",
                   legend.title = "Site")+ggtitle("")
PC45<-fviz_pca_ind(pca, geom.ind = "point", axes=c(4,5),
                   col.ind = labsdf$SITE,# color by sites
                   palette = 'all',
                   addEllipses = FALSE, ellipse.type = "convex",
                   legend.title = "Site")+ggtitle("")
ggarrange(PC12,PC23,PC34,PC45, common.legend=TRUE, labels='auto')

#other variable PC plots
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
