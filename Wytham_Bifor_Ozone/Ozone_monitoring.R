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
  geom_line()+labs(x="Date", y=expression("Mean "*O[3]*" (ppb)"))+
  geom_errorbar(aes(ymax=`Uncertainty Max`, ymin=`Uncertainty Min`))+
  theme_light()

ggplot(Ozone_Results, aes(x=`Date Finished`, y=`Ozone Concentration (ppb)`, col=Site))+
  geom_point()+labs(x="Date", y=expression("Mean "*O[3]*" Concentration (ppb)"))+
  theme_light()
#should do some stats to compare the ozone of two sites, TIme interval is variable
#so Mann Whitney U Test probably most appropriate rather than specifically a time series test
wilcox.test(`Ozone Concentration (ppb)`~ Site, data=Ozone_Results)
#P=0.002165, can conclude the ozone concentration profiles of the two sites are significantly different. 
summary(Ozone_Results)
tapply(Ozone_Results$`Ozone Concentration (ppb)`, Ozone_Results$Site, summary)

#test for time ozone correlation
library(lubridate)
Ozone_Results$yday<-yday(Ozone_Results$`Date Finished`)
# Shapiro-Wilk normality test for day of measurement, normal
shapiro.test(Ozone_Results$yday) # => p = 0.68
# Shapiro-Wilk normality test for oz, not normal
shapiro.test(Ozone_Results$`Ozone Concentration (ppb)`) # => p = 0.01 

res<-cor.test(Ozone_Results$yday, Ozone_Results$`Ozone Concentration (ppb)`, method = c("kendall"))
res

# Kendall's rank correlation tau
# 
# data:  Ozone_Results$yday and Ozone_Results$`Ozone Concentration (ppb)`
# T = 31, p-value = 0.8406
# alternative hypothesis: true tau is not equal to 0
# sample estimates:
#         tau 
# -0.06060606 