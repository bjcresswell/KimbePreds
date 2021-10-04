## NOTE ON SCRIPT
# This code uses the PredVeganAll.csv file to generate hierarchical cluster analyses
# It also manipulates the data in order to run ANOSIM

# Housekeeping
rm(list=ls())
dev.off()

# Set working directory
setwd("~/OneDrive - James Cook University/Ben PhD/Data & analysis/KimbePreds/2019 Archive")

# Install relevant packages (gdata, vegan etc)
require(gdata)
require(vegan)
require(ggplot2)
require(dplyr)
require(ape)
require(BiodiversityR)
require(graphics)
require(ggdendro)
require(MASS)

# Load file
pd <- read.csv("PredVeganAll.csv", header = T)
pd 

# Set the row names as per column 1 and then remove unnecessary columns:


rownames(pd) = pd[,1]
pd <- pd[,4:43]
# To restore to original if reqd:
pdfull <- pd

# To remove transects with all zero entries
pd <- pd[rowSums(pd==0, na.rm=TRUE)<ncol(pd), ]


# Data exploration via  cluster analysis

#Run vegdist
pdveg <- vegdist(pd,method="bray")
pdveg
pdveg1 <- vegdist(pdfull,method="bray")
pdveg1 <- dist.zeroes(pdfull,pdveg1)
pdveg1

par(mfrow=c(1,2))
plot(pdveg)
plot(pdveg1) #NaNs removed

# So now got 2 distance matrices - one with zeros removed at source and one with NaNs removed from dist mat.

# Convert into dendrograms
pdclust <- hclust(pdveg)
pdclust1 <- hclust(pdveg1)

plot(pdclust)

# Plot on side (as phylogram) using ape package:
#tiff(file = "./Figures/PredClust.tiff", width = 13, height = 13, units = 'in', res = 72)
par(mfrow=c(1,2))
plot(as.phylo(pdclust), cex = 0.9, label.offset = 0.01)
title("Cluster dendrogram - Zeros Removed", adj = 0.3, line = 0.5)
plot(as.phylo(pdclust1), cex = 0.9, label.offset = 0.01)
title("Cluster dendrogram -  NaNs Substituted", adj = 0.2, line = 0.5)
#dev.off()

# As a fan plot
plot(as.phylo(pdclust), cex = 0.6, label.offset = 0.01, type = "fan")
plot(as.phylo(pdclust1), cex = 0.6, label.offset = 0.01, type = "fan")

# To colour in the clusters (at tip ends) - NOTE: This is rubbish:
colors = c("red", "blue")
colors = c("red", "blue", "green", "black")
clus2 = cutree(pdclust, 2)
clus4 = cutree(pdclust, 4)
plot(as.phylo(pdclust), cex = 0.6, label.offset = 0.01, tip.color = colors[clus4])

graphics.off()


# ANOSIM Analysis

# Create df with details of survey parameters (need this for ANOSIM later)
pdenv <- data.frame("Site" = 1, "ReefType" = 1:51)
pdenv$ReefType <- "Nearshore"
pdenv$ReefType[14:31] <- "Offshore"
pdenv$ReefType[32:51] <- "Sea Mount"
pdenv$Site <- c(rep("DONN",4), rep("SUSA",4), rep("MADA", 3), rep("LADI", 2),
                rep("OTTO",5), rep("HOGU",4), rep("EMAS",5), rep("KIIS",4),
                rep("BRAD",5), rep("KIBO",5), rep("INGL",5), rep("JOEL",5))
pdenv$Site <- as.factor(pdenv$Site)
pdenv$ReefType <- as.factor(pdenv$ReefType)
pdenv
summary(pdenv)

# Attach the two files
attach(pdenv)

# Run ANOSIM
pd.ano <- anosim(pdveg, ReefType)
pd.ano
summary(pd.ano)
plot(pd.ano)

# SIMPER (how to interpret??)
pdsimp <- simper(pd, ReefType, permutations = 0, trace = FALSE,  parallel = getOption("mc.cores"))
pdsimp
summary(pdsimp)
lapply(pdsimp, FUN=function(x){x$overall})


# NMDS - need more data to make this an effective visualisation
par(mfrow=c(2,2))

NMDS<-metaMDS(pd,k=2,try = 30, autotransform = F)
plot(NMDS$points, type='n', xlab= "NMDS Axis 1", ylab= "NMDS Axis 2")
points(NMDS$points[1:13, ], type='p', cex=0.9, pch=0) # Near Shore Square
points(NMDS$points[14:31, ], type='p', cex=0.9, pch=1) # Off Shore Circle
points(NMDS$points[32:51, ], type='p', cex=0.9, pch=2) # Sea Mount Triangle

# Add 95% CI Ellipses
treat=c(rep("NS",13), rep("OS",18), rep("SM", 20))
ordiellipse(NMDS, groups = treat, kind="se", conf=0.95,
            lwd=1.5, draw = "polygon", border="black", col = "grey", alpha=30, label = T)

stressplot(NMDS)


# With a "total" standardisation - seems to work better (also in 3D)
pdtot <- decostand(pd[,-1], method="total")
par(mfrow=c(1,1))
tiff(file = "./Figures/nMDS.tiff", width = 13, height = 13, units = 'in', res = 72)
NMDS<-metaMDS(pdtot, k=3, autotransform = T)
plot(NMDS$points, type='n', xlab= "NMDS Axis 1", ylab= "NMDS Axis 2")
points(NMDS$points[1:13, ], type='p', cex=1.5, pch=21) # Near Shore
points(NMDS$points[14:31, ], type='p', cex=1.5, pch=6) # Off Shore
points(NMDS$points[32:51, ], type='p', cex=1.5, pch=17) # Sea Mount

# Add 95% CI Ellipses
treat=c(rep("NS",13), rep("OS",18), rep("SM", 20))
ordiellipse(NMDS, groups = treat, kind="se", conf=0.95,
            lwd=1.5, draw = "polygon", border="black", col = "grey", alpha=30, label = T, cex=2.2)
dev.off()





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

