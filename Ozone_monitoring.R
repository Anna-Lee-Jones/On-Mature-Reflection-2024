#Load relevant packages
library(ggplot2)
library(viridis)
library(stringr)
library(readxl)

## ANALYSIS AND VISUALISATION OF OZONE MONITORING DATA ##
Ozone_Results <- read_excel("~/Library/CloudStorage/OneDrive-Nexus365/01 OXFORD PHD/Field Work 2023/Ozone diffusion Tubes/Ozone Results.xlsx", 
                            sheet = "Import")
Ozone_Results$`Date Finished`<-as.Date(Ozone_Results$`Date Finished`)

#time series plot of BIFOR vs Wytham ozone 
ggplot(Ozone_Results, aes(x=`Date Finished`, y=`Ozone Concentration (ppb)`, col=Site))+
  geom_line()+labs(x="Date", y=expression("Mean "*O[3]*" Concentration (ppb)"))
#should do some stats to compare the ozone of two sites, TIme interval is variable
#so Mann Whitney U Test probably most appropriate rather than specifically a time series test
wilcox.test(`Ozone Concentration (ppb)`~ Site, data=Ozone_Results)
#P=0.002165, can conclude the ozone concentration profiles of the two sites are significantly different. 
summary(Ozone_Results)
tapply(Ozone_Results$`Ozone Concentration (ppb)`, Ozone_Results$Site, summary)
