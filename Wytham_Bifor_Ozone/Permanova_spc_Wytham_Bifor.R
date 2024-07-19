#PERMANOVA analysis of ozone, site and date for merged hyperspectral library
#Load relevant packages
library(hyperSpec)
library(vegan)
#set working directory to project location
setwd("~/Library/CloudStorage/OneDrive-Nexus365/01 OXFORD PHD/Field Work 2023/R_Data_Analysis")
#load the merged bifor and wytham hyperspectral library
load(file = "~/Library/CloudStorage/OneDrive-Nexus365/01 OXFORD PHD/Field Work 2023/R_Data_Analysis/QWYTHAM_BIFOR_merged.RData")

#PERMANOVA of whole spectral library to factors eg ozone
#remove any missing values in ozone
merged_noNA<-subset(merged, ozone!="NA")
is.na(merged_noNA$ozone)

#permanova of spc to ozone
adonis2(merged_noNA$spc~ozone,data=merged_noNA)#takes a little while to run
#OUTPUT
#Permutation test for adonis under reduced model
# Terms added sequentially (first to last)
# Permutation: free
# Number of permutations: 999
# adonis2(formula = merged_noNA$spc ~ ozone, data = merged_noNA)
#             Df  SumOfSqs    R2     F    Pr(>F)    
# ozone       9   0.9574 0.06526 14.21  0.001 ***
# Residual 1832  13.7140 0.93474                 
# Total    1841  14.6714 1.00000                 
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#permanova of spc to ozone with sample used as random effects variable
adonis2(merged_noNA$spc~tree*ozone,data=merged_noNA)#takes a little while to run

#permanova of spc to ozone and site
adonis2(merged_noNA$spc~ozone*site,data=merged_noNA)#takes a little while to run
#OUTPUT
# adonis2(formula = merged_noNA$spc ~ ozone * site, data = merged_noNA)
# Df SumOfSqs      R2       F Pr(>F)    
# ozone         1   0.2953 0.02013 38.1612  0.001 ***
# site          1   0.0682 0.00465  8.8181  0.001 ***
# ozone:site    1   0.0838 0.00571 10.8321  0.002 ** 
# Residual   1838  14.2240 0.96951                   
# Total      1841  14.6714 1.00000                   
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


#permanova of spc to site 
adonis2(merged_noNA$spc~site,data=merged_noNA)#takes a little while to run
#Output
# adonis2(formula = merged_noNA$spc ~ site, data = merged_noNA)
# Df SumOfSqs      R2      F Pr(>F)    
# site        1   0.3255 0.02218 41.742  0.001 ***
# Residual 1840  14.3460 0.97782                  
# Total    1841  14.6714 1.00000                  
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#permanova of spc to date 
adonis2(merged_noNA$spc~date,data=merged_noNA)#takes a little while to run
#Output
# adonis2(formula = merged_noNA$spc ~ date, data = merged_noNA)
# Df SumOfSqs      R2      F Pr(>F)    
# date        1   0.1943 0.01324 24.689  0.001 ***
#   Residual 1840  14.4772 0.98676                  
# Total    1841  14.6714 1.00000                  
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#permanova of spc to ozone, site, and date 
adonis2(merged_noNA$spc~ozone*site*date,data=merged_noNA)#takes a little while to run
#Output
# adonis2(formula = merged_noNA$spc ~ ozone * site * date, data = merged_noNA)
# Df SumOfSqs      R2       F Pr(>F)    
# ozone              1   0.2953 0.02013 39.2952  0.001 ***
#   site               1   0.0682 0.00465  9.0801  0.001 ***
#   date               1   0.3643 0.02483 48.4671  0.001 ***
#   ozone:site         1   0.0236 0.00161  3.1359  0.056 .  
# ozone:date         1   0.0603 0.00411  8.0187  0.003 ** 
#   site:date          1   0.0056 0.00038  0.7409  0.444    
# ozone:site:date    1   0.0707 0.00482  9.4082  0.001 ***
#   Residual        1834  13.7835 0.93948                   
# Total           1841  14.6714 1.00000                   
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1