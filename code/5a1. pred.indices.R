## ---- pred.simp

# Script starts with 'wide' predator count data per 

rm(list=ls())
library(gdata)
library(vegan)
library(readxl)
library(labdsv)
library(tidyverse)
setwd("~/OneDrive - James Cook University/Ben PhD/Data & analysis/KimbePreds")

load(file='data/predvegan.RData')
predvegan

# Generate new df of indices

TID <- rownames(predvegan) # First extract row names from predvegan
indices <- as.data.frame(TID) # And initiate into a df


# Set up rest of the required columns
indices <- indices %>% 
  mutate(Reeftype = case_when(grepl("BRAD", `TID`) ~ "Pinnacle",      # Assign reef type status
                            grepl("JOEL", `TID`) ~ "Pinnacle",
                            grepl("KBOM", `TID`) ~ "Pinnacle",
                            grepl("INGL", `TID`) ~ "Pinnacle",
                            grepl("LADI", `TID`) ~ "Nearshore",
                            grepl("MADA", `TID`) ~ "Nearshore",
                            grepl("SUSA", `TID`) ~ "Nearshore",
                            grepl("DON", `TID`) ~ "Nearshore",
                            grepl("EMA", `TID`) ~ "Offshore",
                            grepl("HOG", `TID`) ~ "Offshore",
                            grepl("OTT", `TID`) ~ "Offshore",
                            grepl("KIS", `TID`) ~ "Offshore")) %>% 
  mutate(Reeftype = factor(Reeftype, levels= c("Pinnacle", "Offshore", "Nearshore"))) %>% # Organise by reef type
  mutate(SiteCode = case_when(grepl("BRAD", `TID`) ~ "BRAD",      # Assign site name
                              grepl("JOEL", `TID`) ~ "JOEL",
                              grepl("KBOM", `TID`) ~ "KBOM",
                              grepl("INGL", `TID`) ~ "INGL",
                              grepl("LADI", `TID`) ~ "LADI",
                              grepl("MADA", `TID`) ~ "MADA",
                              grepl("SUSA", `TID`) ~ "SUSA",
                              grepl("DON", `TID`) ~ "DON",
                              grepl("EMA", `TID`) ~ "EMA",
                              grepl("HOG", `TID`) ~ "HOG",
                              grepl("OTT", `TID`) ~ "OTT",
                              grepl("KIS", `TID`) ~ "KIIS")) %>%
  mutate(SiteCode = factor(SiteCode, levels= c("BRAD", "JOEL", "INGL", "KBOM",   # Pinnacles first
                                               "EMA",  "HOG", "KIS", "OTT",      # then offshore
                                               "DON", "LADI", "MADA", "SUSA"))) %>% 
  mutate(Site = case_when(grepl("BRAD", `TID`) ~ "Bradford Shoals",      # Assign site name
                          grepl("JOEL", `TID`) ~ "Joels",
                          grepl("KBOM", `TID`) ~ "Kimbe Bommie",
                          grepl("INGL", `TID`) ~ "Inglis Shoals",
                          grepl("LADI", `TID`) ~ "Lady Di",
                          grepl("MADA", `TID`) ~ "Madaro",
                          grepl("SUSA", `TID`) ~ "Susans",
                          grepl("DON", `TID`) ~ "Donnas",
                          grepl("EMA", `TID`) ~ "Ema",
                          grepl("HOG", `TID`) ~ "Hogu",
                          grepl("OTT", `TID`) ~ "Otto",
                          grepl("KIS", `TID`) ~ "Kimbe Island")) %>% 
  mutate(SurvCode = case_when(grepl("1018", `TID`) ~ "2018",      # Assign surv code
                              grepl("0319", `TID`) ~ "2019"))



# Compute indices:
indices$Richness <- rowSums(predvegan>0)
indices$Shannon <- diversity(predvegan) # shannon is default
indices$Rarefied <- c(rarefy(predvegan, MARGIN =1, sample = 15))
indices$Evenness <- apply(predvegan>0,1,sum)
indices$Evenness <-diversity(predvegan, index="simpson")/log(indices$Evenness)

indices$Simpson <-diversity(predvegan, index="simpson")
indices$Simpson2 <-diversity(predvegan, "simpson")
indices$PielouJ <- indices$Shannon/log(indices$Richness)
indices$Abundance <- apply(predvegan, 1, sum)

save(indices, file = "data/indices.RData")


# Some eda using boxplots:

ggplot(indices, aes(x=Reeftype, y=Richness))+
  geom_boxplot()

ggplot(indices, aes(x=Reeftype, y=Shannon))+
  geom_boxplot()

ggplot(indices, aes(x=Reeftype, y=Simpson))+
  geom_boxplot()

ggplot(indices, aes(x=Reeftype, y=Evenness))+
  geom_boxplot()

ggplot(indices, aes(x=Reeftype, y=Rarefied))+
  geom_boxplot()
 
ggplot(indices, aes(x=Reeftype, y=Abundance))+
  geom_boxplot()


# Normality checks
hist(indices$Shannon)
hist(indices$Richness)
hist(indices$Abundance)
hist(indices$Evenness)
hist(indices$Simpson)
hist(indices$Rarefied)
hist(indices$PielouJ)



### Junk work on diversity



## ---- pred.nmds_pca

dev.off()
rm(list=ls())

library(gdata)
library(vegan)
library(readxl)
library(labdsv)
library(tidyverse)

setwd("~/OneDrive - James Cook University/Ben PhD/Data & analysis/KimbePreds")

load(file='data/predvegan.RData')
load(file='data/indices.RData')

head(predvegan)
predvegan[is.na(predvegan)] = 0 # Remove NAs in whole df and set to zero


# Hellinger pre-transformation of the species data 
pred.stand <- decostand(predvegan, "hellinger")


pred.stand <- cbind(pred.stand, indices[1:5])
head(pred.stand)
pred.stand <- pred.stand %>%
  mutate(Reeftype = factor(Reeftype, levels= c("Pinnacle", "Offshore", "Nearshore")))# Organise by reef type

pred.stand <- pred.stand %>%
  arrange(Reeftype)


PCA.out<-princomp(pred.stand[,1:53])
PCA.out

names(PCA.out)
summary(PCA.out)
PCA.out$loadings
plot(PCA.out$scores)

plot(PCA.out$scores[1:40, ], col='red', pch=15, cex=.8)
points(PCA.out$scores[41:80, ], col='blue', pch=16, cex=0.8)
points(PCA.out$scores[81:120, ], col='black', pch=17, cex=0.8)

glimpse(pred.stand)

which(is.na(pred.stand[,1:53]))
which(is.na(predvegan))


MDS.out<-metaMDS(pred.stand, k=3, trymax=500, trace=F, autotransform = F)

plot(MDS.out, display="sites", type="text")
plot(MDS.out$points, type="n", xlab="NMDS1", ylab="NMDS2")+
  points(MDS.out$points[1:6,], type='p', cex=2.3, pch=16, col = "grey")+
  points(MDS.out$points[7:12,], type='p', cex=2.3, pch=16, col="black")+
  title(main="NMDS Bray Curtis Ordination Plot")



