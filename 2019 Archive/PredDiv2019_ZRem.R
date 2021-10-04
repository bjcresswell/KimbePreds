## NOTE ON SCRIPT
# This code uses the PredVeganAll.csv file to generate an NMDS on Bray dissimilarities
# It also manipulates the data in order to run ANOSIM


# Housekeeping
rm(list=ls())
dev.off()


# Set working directory
setwd("~/OneDrive - James Cook University/Ben PhD/Data & analysis/KimbePreds")
getwd()


# Install relevant packages (gdata, vegan etc)
library(gdata)
library(vegan)
library(tidyverse)
library(ape)
#library(BiodiversityR)


# Load file
pd <- read.csv("data/PredVeganAll.csv")
pdrestore <- pd
# Set the row names as per column 1 and then remove unnecessary columns:
pd <- pd %>% 
  column_to_rownames(var='TRANSECT') %>% 
  select(!c(SITE, REEFTYPE))

# To remove transects with all zero entries
pdz <- pd[rowSums(pd==0, na.rm=TRUE)<ncol(pd), ]
pd
# Create df with details of survey parameters (need this for ANOSIM later)
pdenv <- data.frame("Site" = 1, "ReefType" = 1:51)
pdenv$ReefType <- "Nearshore"
pdenv$ReefType[14:31] <- "Offshore"
pdenv$ReefType[32:51] <- "Pinnacle"
pdenv$Site <- c(rep("DONN",4), rep("SUSA",4), rep("MADA", 3), rep("LADI", 2),
                rep("OTTO",5), rep("HOGU",4), rep("EMAS",5), rep("KIIS",4),
                rep("BRAD",5), rep("KIBO",5), rep("INGL",5), rep("JOEL",5))
pdenv$Site <- as.factor(pdenv$Site)
pdenv$ReefType <- as.factor(pdenv$ReefType)
pdenv
summary(pdenv)


# Data exploration via hierarchical cluster analysis
#Run vegdist
pdzveg <- vegdist(pdz)

tail(pdveg)
dist.zeroes(pd,vegdist)

pdveg <- (pdveg, na.)
plot(pdveg)

pdveg <- hclust(pdveg)
plot(pdveg)
plot(pdveg, hang = -1, cex=0.5)

pd <- pdrestore

pdveg <- as.phylo(pdveg)
plot(pdveg, type="cladogram", hang = 0.1, cex=0.5)

graphics.off()

#pdveg <- dist(pd, method = "canberra") # Alternative method - euclidean dists only




# ANOSIM Analysis

# Attach the two files
attach(pdenv)

# Run ANOSIM
pd.ano <- anosim(pdveg, Site)
pd.ano
summary(pd.ano)
plot(pd.ano)




# NMDS
par(mfrow=c(1,1))

NMDS<-metaMDS(pd, k=5)
plot(NMDS$points, type='n', xlab= "NMDS Axis 1", ylab= "NMDS Axis 2")
points(NMDS$points[1:13, ], type='p', cex=0.9, pch=0) # Near Shore Square
points(NMDS$points[14:31, ], type='p', cex=0.9, pch=1) # Off Shore Circle
points(NMDS$points[32:51, ], type='p', cex=0.9, pch=2) # Sea Mount Triangle

# Add 95% CI Ellipses
treat=c(rep("NS",13), rep("OS",18), rep("SM", 20))
ordiellipse(NMDS, groups = treat, kind="se", conf=0.95,
            lwd=1.5, draw = "polygon", border="black", col = "grey", alpha=30, label = T)

stressplot(NMDS)


# With a "total" standardisation
pdtot <- decostand(pd[,-1], method="total")

NMDS<-metaMDS(pdtot, k=3)
plot(NMDS$points, type='n', xlab= "NMDS Axis 1", ylab= "NMDS Axis 2")
points(NMDS$points[1:13, ], type='p', cex=0.9, pch=0) # Near Shore
points(NMDS$points[14:31, ], type='p', cex=0.9, pch=1) # Off Shore
points(NMDS$points[32:51, ], type='p', cex=0.9, pch=16) # Sea Mount

# Add 95% CI Ellipses
treat=c(rep("NS",13), rep("OS",18), rep("SM", 20))
ordiellipse(NMDS, groups = treat, kind="se", conf=0.95,
            lwd=1.5, draw = "polygon", border="black", col = "grey", alpha=30, label = T)





# Junk (for now)


pdmds <- metaMDSdist(veggie, distance = "bray", autotransform = TRUE, 
            noshare = TRUE, trace = 1, zerodist = "ignore", 
            distfun = vegdist)
plot(pdmds, display="sites", type="text")
plot (pdmds)
View(pdmds)

MDS.out<-metaMDS(pdmds, dist="jaccard")
plot(MDS.out, display="sites", type="text")
plot(MDS.out$points, type="n", xlab="NMDS1", ylab="NMDS2")+
  points(MDS.out$points[1:6,], type='p', cex=2.3, pch=16, col = "grey")+
  points(MDS.out$points[7:12,], type='p', cex=2.3, pch=16, col="black")+
  title(main="NMDS Bray Curtis Ordination Plot")


theme(plot.title = element_text(color="black", size=15, face="bold"))

## Principle componoent analysis ##

PCA.out<-princomp(env[,2:4]) #Coral.cover, Coral.richness, Water.Temp
names(PCA.out)
summary(PCA.out)
PCA.out$loadings


mod.1<-lm(Fish.Rich ~ Coral.cover, data=env)
mod.2<-lm(Fish.Rich ~ Coral.cover + Coral.richness, data=env)
mod.3<-lm(Fish.Rich ~ Coral.cover + Coral.richness + Water.Temp, data=env)
AIC(mod.1,mod.2,mod.3)

