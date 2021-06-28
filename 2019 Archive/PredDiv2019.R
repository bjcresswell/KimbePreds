## NOTE ON SCRIPT
# This code uses the PredVeganAll.csv file to analyse differences in diversity between NS, OS and SM sites

# Housekeeping
rm(list=ls())
dev.off()

# Set working directory
setwd("~/Science & Education/Ben PhD/Data/Pred Div Analysis")

# Install relevant packages (gdata, vegan etc)
require(gdata)
require(vegan)
require(ggplot2)
require(dplyr)

# Load file
pdmain <- read.csv("PredVeganAll.csv", header = T)
pd <- pdmain

# Set the row names as per column 1 and then remove unnecessary columns:
rownames(pd) = pd[,1]
pd <- pd[,-1]
pd <- pd[,-1]
pd <- pd[,-1]

# Push a data-frame into a vector according to indices (Richness, Shannon-Weiner, Simpson etc) #
pred.rich<-specnumber(pd)
pred.shannon<-diversity(pd)

# Explore the data (for full dataset - see below for residuals etc)

# So what are the means and SEMs for each group?
sumFunc <- function(x) c(mean=mean(x), n=length(x), median=median(x), se=sd(x)/sqrt(length(x)))
sumFunc(pred.rich[1:20])
sumFunc(pred.rich[21:40])
sumFunc(pred.rich[41:60])
sumFunc(pred.shannon[1:20])
sumFunc(pred.shannon[21:40])
sumFunc(pred.shannon[41:60])

# Run next line to save these plots to file...
#tiff(file = "./Figures/PredDivBox.tiff")
par(mfrow=c(1,2))
boxplot(pred.rich[1:20], pred.rich[21:40], pred.rich[41:60],
        names=c("NS", "OS", "SM"), 
        ylab="Richness (No. of Species)",
        main = "Species Richness",
        col = "grey")
boxplot(pred.shannon[1:20], pred.shannon[21:40], pred.shannon[41:60],
        names=c("NS", "OS", "SM"), 
        ylab="Shannon Index",
        main = "H' Diversity",
        col = "grey")
dev.off()

# Test for normality:

# Visual check
# Run next line to save these plots to file...
# tiff(file = "./Figures/PredDivHists.tiff")
par(mfrow=c(2,3))
# Spp richness
hist(pred.rich[1:20], prob="T", main = "Nearshore", xlab = "Spp richness")
curve(dnorm(x, mean(pred.rich[1:20]),sd(pred.rich[1:20])), add=TRUE, col='red', lwd=3) # Near shore
hist(pred.rich[21:40], prob="T", main = "Offshore", xlab = "Spp richness") # Off shore
curve(dnorm(x, mean(pred.rich[21:40]),sd(pred.rich[21:40])), add=TRUE, col='red', lwd=3) # Near shore
hist(pred.rich[41:60], prob="T", main = "Seamount", xlab = "Spp richness") # Seamount
curve(dnorm(x, mean(pred.rich[41:60]),sd(pred.rich[41:60])), add=TRUE, col='red', lwd=3) # Near shore
# Shannon
hist(pred.shannon[1:20], prob="T", main = "Nearshore", xlab = "H' Diversity")
curve(dnorm(x, mean(pred.shannon[1:20]),sd(pred.shannon[1:20])), add=TRUE, col='red', lwd=3) # Near shore
hist(pred.shannon[21:40], prob="T", main = "Offshore", xlab = "H' Diversity") # Off shore
curve(dnorm(x, mean(pred.shannon[21:40]),sd(pred.shannon[21:40])), add=TRUE, col='red', lwd=3) # Near shore
hist(pred.shannon[41:60], prob="T", main = "Seamount", xlab = "H' Diversity") # Seamount
curve(dnorm(x, mean(pred.shannon[41:60]),sd(pred.shannon[41:60])), add=TRUE, col='red', lwd=3) # Near shore


# Histograms look skewed...??? What about QQ Plots:
# Run next line to save these plots to file...
# tiff(file = "./Figures/PredDivQQs.tiff")
par(mfrow=c(2,3))
qqnorm(pred.rich[1:20], main = "Spp Rich Nearshore")
qqline(pred.rich[1:20], col="red", lwd=3)
qqnorm(pred.rich[21:40], main = "Spp Rich Offshore")
qqline(pred.rich[21:40], col="red", lwd=3)
qqnorm(pred.rich[41:60], main = "Spp Rich Seamount")
qqline(pred.rich[41:60], col="red", lwd=3)
qqnorm(pred.shannon[1:20], main = "H' Nearshore")
qqline(pred.shannon[1:20], col="red", lwd=3)
qqnorm(pred.shannon[21:40], main = "H' Offshore")
qqline(pred.shannon[21:40], col="red", lwd=3)
qqnorm(pred.shannon[41:60], main = "H' Seamount")
qqline(pred.shannon[41:60], col="red", lwd=3)
dev.off()

# Shapiro Wilk Test
# Species richness
shapiro.test(pred.rich) # the whole data set - NO
shapiro.test(pred.rich[1:20]) # Near shore - NO 
shapiro.test(pred.rich[21:40]) # Off shore - NO
shapiro.test(pred.rich[41:60]) # Seamount - NO
# Shannon Weiner
shapiro.test(pred.shannon) # NO
shapiro.test(pred.shannon[1:20]) # NO
shapiro.test(pred.shannon[21:40]) # NO
shapiro.test(pred.shannon[41:60]) # YES

# So neither species richness nor H' look normally distributed - maybe need log transformation?.

pred.rich <- log(pred.rich)
pred.shannon <- sqrt(pred.shannon)

# No transformation method works!!!! (ln, log10, sqrt, abs, exp, sin, asin, ^1/9 etc)

# Before moving on need to regenerate the original vectors (if they are messed up from the transformation):
pred.rich<-specnumber(pd)  
pred.shannon<-diversity(pd)

# Next step: Check distribution of the residuals...

# First need to create a data frame with the diversity indices/vectors in it:
pd1 <- data.frame(pred.rich, pred.shannon)
pd1$ReefType <- NA #blank column
pd1$ReefType <- "Near Shore"
pd1$ReefType[21:40] <- "Off Shore"
pd1$ReefType[41:60] <- "Sea Mount"
pd1$ReefType <- as.factor(pd1$ReefType)
summary(pd1)
plm <- lm(pred.shannon ~ ReefType, data=pd1)
par(mfrow=c(2,2))
plot(plm,  main = "Linear Model Checks H' Diversity",)

# Residuals also aren't great but do not depart too drastically from normal. 
# One option could be bootstrapping (see below)

# Does ANOVA support the data visualisation? -- YES!
summary(aov(pred.shannon ~ ReefType, data=pd1))
pd1aov <- aov(pred.shannon ~ ReefType, data=pd1)
TukeyHSD(pd1aov)

# Non-parametric Kruskal Wallis also supports this. Diff between SM and OS/NS only.
kruskal.test(pred.shannon ~ ReefType, data = pd1)
pairwise.wilcox.test(pd1$pred.shannon, pd1$ReefType,
                     p.adjust.method = "BH")

# Significant differences between SM and both OS/NS but NOT between OS/NS

# Maybe bootstrapping is a good way to go to "normalise" the data??
# First I need to turn each indices vector into a bootstrapped vector of i "observations" - 
# i each from Near Shore, Off Shore and Sea Mount
# NB: Dropping Simpsons from here on in...

# Species Richness
a<-numeric(200)
for(i in 1:200) a[i]<-mean(sample(pred.rich[1:20],replace=T))
b<-numeric(200)
for(i in 1:200) b[i]<-mean(sample(pred.rich[21:40],replace=T))
c<-numeric(200)
for(i in 1:200) c[i]<-mean(sample(pred.rich[41:60],replace=T))

# Shannon Wiener
d<-numeric(200)
for(i in 1:200) d[i]<-mean(sample(pred.shannon[1:20],replace=T))
e<-numeric(200)
for(i in 1:200) e[i]<-mean(sample(pred.shannon[21:40],replace=T))
f<-numeric(200)
for(i in 1:200) f[i]<-mean(sample(pred.shannon[41:60],replace=T))




# I should check again for normality...
par(mfrow=c(2,3))
hist(a, prob="T", main="Nearshore", xlab = "Spp richness")
curve(dnorm(x, mean(a),sd(a)), add=TRUE, col='red', lwd=3)
hist(b, prob="T", main="Offshore", xlab = "Spp richness")
curve(dnorm(x, mean(b),sd(b)), add=TRUE, col='red', lwd=3)
hist(c, prob="T", main="Seamount", xlab = "Spp richness")
curve(dnorm(x, mean(c),sd(c)), add=TRUE, col='red', lwd=3)
hist(d, prob="T", main="", xlab = "H' Diversity")
curve(dnorm(x, mean(d),sd(d)), add=TRUE, col='red', lwd=3)
hist(e, prob="T", main="", xlab = "H' Diversity")
curve(dnorm(x, mean(e),sd(e)), add=TRUE, col='red', lwd=3)
hist(f, prob="T",  main="", xlab = "H' Diversity")
curve(dnorm(x, mean(f),sd(f)), add=TRUE, col='red', lwd=3)

# Normality test...
shapiro.test(a)
shapiro.test(b)
shapiro.test(c)
shapiro.test(d)
shapiro.test(e)
shapiro.test(f)

# So now the plots look more "normal" and the Shapiro test is coming back normal 

# Other normality checks:
qqnorm(a, main="Nearshore Spp Rich")
qqline(a)
qqnorm(b, main="Offshore Spp Rich")
qqline(b)
qqnorm(c, main="Seamount Spp Rich")
qqline(c)
qqnorm(d, main="Nearshore H'")
qqline(d)
qqnorm(e, main="Offshore H'")
qqline(e)
qqnorm(f, main="Seamount H'")
qqline(f)

# "Looks" normal now 


# Can run the box plots again...

par(mfrow=c(1,2))

boxplot(a,b,c,
        names=c("Near Shore", "Off Shore", "Seamount"), 
        ylab="Richness (No. of Species)",
        main = "Species Richness by Reef Type",
        col = "grey")

boxplot(d,e,f,
        names=c("Near Shore", "Off Shore", "Seamount"), 
        ylab="Shannon Index",
        main = "Shannon-Weiner Boxplot by Reef Type",
        col = "grey")

# Or in ggplot for better output (but need to make a df so see below)

# So let's make a new df with the bootstrapped values

# Vectors first..
bootpr <- c(a,b,c)
bootpshan <- c(d,e,f)

# Then df
pdboot <- data.frame(bootpr, bootpshan)
pdboot$ReefType <- NA #blank column
pdboot$ReefType <- "Near Shore"
pdboot$ReefType[201:400] <- "Off Shore"
pdboot$ReefType[401:600] <- "Sea Mount"
pdboot$ReefType <- as.factor(pdboot$ReefType)
summary(pdboot)
par(mfrow=c(2,2))
pdblm <- lm(bootpshan ~ ReefType, data = pdboot)
plot(pdblm)

# Now can run ANOVA
summary(aov(bootpr ~ ReefType, data= pdboot))
summary(aov(bootpshan ~ ReefType, data= pdboot))
bootaov <- aov(bootpshan ~ ReefType, data= pdboot)
TukeyHSD(bootaov)
bootaov

# Following bootstrapping we are now seeing significant differences between all three reef types

# NOTE: Again, can't do T test as there are 3 levels 
#t.test(bootpr ~ ReefType, data=pdboot)
#t.test(bootpshan ~ ReefType, data=pdboot)


## GGPLOT GRAPHICS FROM RAW DF (pd1) ##

# Create three colour palette: Black, blue, green:
cbPalette <- c("#000000", "#56B4E9", "#009E73")
# OR greyscale palette:
gsPalette <- c("grey", "grey48", "grey38")

# To use for fills, add: scale_fill_manual(values=cbPalette) etc...


# Bar Plot

pb <- ggplot(pd1, aes(x=ReefType, y=pred.shannon, fill=ReefType)) +
  stat_summary(geom = "bar", fun.y = mean, width=0.7) +
  stat_summary(geom = "errorbar", fun.data = mean_se, width=0.15) +
  scale_fill_manual(values=gsPalette) + 
  theme_classic() + 
  ggtitle("Predator Diversity by Reef Type") +
  xlab("Reef Type") + ylab("Shannon Diversity") +
  theme(plot.title = element_text(family="Arial", size=16, hjust = 0.5)) +
  theme(axis.title.x = element_text(family="Arial", size=12)) +
  theme(legend.position='none')
pb


## GGPLOT GRAPHICS FROM BOOTSTRAPPED DF (pdboot) ##

# Bar plot
bpb <- ggplot(pdboot, aes(x=ReefType, y=bootpshan, fill=ReefType)) +
  stat_summary(geom = "bar", fun.y = mean, width=0.7) +
  stat_summary(geom = "errorbar", fun.data = mean_se, width=0.15) +
  scale_fill_manual(values=gsPalette) + 
  theme_classic() + 
  ggtitle("Predator Diversity by Reef Type") +
  xlab("Reef Type") + ylab("Shannon Diversity") +
  theme(plot.title = element_text(family="Arial", size=16, hjust = 0.5)) +
  theme(axis.title.x = element_text(family="Arial", size=12)) +
  theme(legend.position='none')
bpb



## Analysis in vegan for community comparisons ##
## Cannot do in this script - zero lines left in cause errors in metaMDS function!!!

# NMDS
graphics.off()
par(mfrow=c(2,2))

NMDS<-metaMDS(pd)
plot(NMDS$points, type='n', xlab= "NMDS Axis 1", ylab= "NMDS Axis 2")
points(NMDS$points[1:20, ], type='p', cex=0.9, pch=0) # Near Shore
points(NMDS$points[21:40, ], type='p', cex=0.9, pch=1) # Off Shore
points(NMDS$points[41:60, ], type='p', cex=0.9, pch=16) # Sea Mount

# Add 95% CI Ellipses
treat=c(rep("NS",20), rep("OS",20), rep("SM", 20))
ordiellipse(NMDS, groups = treat, kind="se", conf=0.95,
            lwd=1.5, draw = "polygon", border="black", col = "grey", alpha=30, label = T)


pdtot <- decostand(pd[,-1], method="total")

NMDS<-metaMDS(pdtot)
plot(NMDS)

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




