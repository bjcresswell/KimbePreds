# Housekeeping
rm(list=ls())
dev.off()

# Set working directory
setwd("~/Science & Education/Ben PhD/Data/Pred Div Analysis")

# Install relevant packages (gdata, vegan etc)
require(gdata)
require(vegan)
require(ggplot2)
library(dplyr)

# Load file
pd <- read.csv("PredVegan.csv", header = T)

# Set the row names as per column 1 and then remove unnecessary columns:
rownames(pd) = pd[,1]
pd <- pd[,-1]
pd <- pd[,-1]
pd <- pd[,-1]

# Remove transects with all zero entries
pd <- pd[rowSums(pd==0, na.rm=TRUE)<ncol(pd), ]

# Push a data-frame into a vector according to indices (Richness, Shannon-Weiner, Simpson etc) #
pred.rich<-specnumber(pd)
pred.shannon<-diversity(pd)
pred.simpson<-diversity(pd, index="simpson")

# If you want all plots in one picture/file/jpg do this first #
par(mfrow=c(1,3))

boxplot(pred.rich[1:14], pred.rich[15:38],
        names=c("Nearshore", "Seamount"), 
        ylab="Richness (No. of Species)",
        main = "Species Richness by Reef Type",
        col = "grey")

boxplot(pred.shannon[1:14], pred.shannon[15:38],
        names=c("Nearshore", "Seamount"), 
        ylab="Shannon Index",
        main = "Shannon-Weiner Boxplot by Reef Type",
        col = "grey")

boxplot(pred.simpson[1:14], pred.simpson[15:38],
        names=c("Nearshore", "Seamount"), 
        ylab="Simpson Index",
        main = "Simpson Index Boxplot by Reef Zone",
        col = "grey")


## ANOVA to test what appears to be the trends from the above plot..

# First things first: need to test for normality...

graphics.off()
par(mfrow=c(2,2))

# Species richness
shapiro.test(pred.rich[1:14])
shapiro.test(pred.rich[15:38])
hist(pred.rich[1:14])
hist(pred.rich[15:38])
qqnorm(pred.rich[1:14])
qqline(pred.rich[1:14], col="red", lwd=3)
qqnorm(pred.rich[15:38])
qqline(pred.rich[15:38], col="red", lwd=3)

# So species richness not normally distributed - need log transformation?

# Next try with Shannon-Wiener index
shapiro.test(pred.shannon[1:14])
shapiro.test(pred.shannon[15:38])
hist(pred.shannon[1:14])
hist(pred.shannon[15:38])
qqnorm(pred.shannon[1:14])
qqline(pred.shannon[1:14], col="red", lwd=3)
qqnorm(pred.shannon[15:38])
qqline(pred.shannon[15:38], col="red", lwd=3)

# Shannon index also not normally distributed (surprised?)

# Finally, Simpson index:
# Next try with Shannon-Wiener index
shapiro.test(pred.simpson[1:14])
shapiro.test(pred.simpson[15:38])
hist(pred.simpson[1:14])
hist(pred.simpson[15:38])
qqnorm(pred.simpson[1:14])
qqline(pred.simpson[1:14], col="red", lwd=3)
qqnorm(pred.simpson[15:38])
qqline(pred.simpson[15:38], col="red", lwd=3)

# Again no surprises...

# So next try transforming the data...

pred.rich <- (pred.rich)^1/3
pred.shannon <- abs(pred.shannon)

# No transformation method works (ln, log10, sqrt, abs, exp, sin, asin, ^1/9 etc)

# Need to regenerate the original vectors (now they are messed up from the transformation):
pred.rich<-specnumber(pd)
pred.shannon<-diversity(pd)
pred.simpson<-diversity(pd, index="simpson")

# Now go for non-parametric test
# First need to create a data frame with the diversity indices/vectors in it:

pd1 <- data.frame(pred.rich, pred.shannon, pred.simpson)
pd1$ReefType <- NA #blank column
pd1$ReefType <- "Near Shore"
pd1$ReefType[15:38] <- "Sea Mount"
pd1

# Wilcox rank sum test
wilcox.test(pred.rich ~ ReefType, data=pd1)
wilcox.test(pred.shannon ~ ReefType, data=pd1)
wilcox.test(pred.simpson ~ ReefType, data=pd1)

# Seems to show statistically significant differences (except shannon?)


# Maybe bootstrapping is a good way to go??
# First I need to turn each indices vector into a bootstrapped vector of 10000 "observations" - 
# 5000 each from Near Shore and Sea Mount:

# Species Richness
a<-numeric(5000)
for(i in 1:5000) a[i]<-mean(sample(pred.rich[1:14],replace=T))
b<-numeric(5000)
for(i in 1:5000) b[i]<-mean(sample(pred.rich[15:38],replace=T))

# Shannon Wiener
c<-numeric(5000)
for(i in 1:5000) c[i]<-mean(sample(pred.shannon[1:14],replace=T))
d<-numeric(5000)
for(i in 1:5000) d[i]<-mean(sample(pred.shannon[15:38],replace=T))

# Simpsons
e<-numeric(5000)
for(i in 1:5000) e[i]<-mean(sample(pred.simpson[1:14],replace=T))
f<-numeric(5000)
for(i in 1:5000) f[i]<-mean(sample(pred.simpson[15:38],replace=T))


# I should check again for normality...
graphics.off()
par(mfrow=c(3,2))
hist(a,main="", prob="T")
curve(dnorm(x, mean(a),sd(a)), add=TRUE, col='red', lwd=5)
hist(b,main="", prob="T")
curve(dnorm(x, mean(b),sd(b)), add=TRUE, col='red', lwd=5)
hist(c,main="", prob="T")
curve(dnorm(x, mean(c),sd(c)), add=TRUE, col='red', lwd=5)
hist(d,main="", prob="T")
curve(dnorm(x, mean(d),sd(d)), add=TRUE, col='red', lwd=5)
hist(e,main="", prob="T")
curve(dnorm(x, mean(e),sd(e)), add=TRUE, col='red', lwd=5)
hist(f,main="", prob="T")
curve(dnorm(x, mean(f),sd(f)), add=TRUE, col='red', lwd=5)

# Normality test...
shapiro.test(a)
shapiro.test(b)
shapiro.test(c)
shapiro.test(d)
shapiro.test(e)
shapiro.test(f)

# Hmmmm....

# Other normality checks:
graphics.off()
qqnorm(a)
qqline(a)
qqnorm(b)
qqline(b)
qqnorm(c)
qqline(c)
qqnorm(d)
qqline(d)
qqnorm(e)
qqline(e)
qqnorm(f)
qqline(f)

# "Looks" normal but the test says otherwise.. Could go for Wilcox test again but need to make new df

# Also should maybe compare variances?
var.test(b,a)

# Can run the box plots again...

boxplot(a,b,
        names=c("Nearshore", "Seamount"), 
        ylab="Richness (No. of Species)",
        main = "Species Richness by Reef Type",
        col = "grey")

boxplot(c,d,
        names=c("Nearshore", "Seamount"), 
        ylab="Shannon Index",
        main = "Shannon-Weiner Boxplot by Reef Type",
        col = "grey")

boxplot(e,f,
        names=c("Nearshore", "Seamount"), 
        ylab="Simpson Index",
        main = "Simpson Index Boxplot by Reef Zone",
        col = "grey")

# Or in ggplot for better output

p <- ggplot(pdboot, aes(x=ReefType, y=bootpshan, fill=ReefType)) + 
  geom_violin()
p <- p + stat_summary(fun.y=mean, geom="point", shape=23, size=2)
p <- p + scale_fill_brewer(palette="Paired")+ theme_classic()
p <- p + labs(title="Predator Diversity (Shannon Index) by Reef Type",x="Reef Type", y = "Shannon Diversity")


# So let's make a new df with the bootstrapped values

# Vectors first..
bootpr <- c(a,b)
bootpshan <- c(c,d)
bootpsimp <- c(e,f)

# Then df
pdboot <- data.frame(bootpr, bootpshan, bootpsimp)
pdboot$ReefType <- NA #blank column
pdboot$ReefType <- "Near Shore"
pdboot$ReefType[5001:10000] <- "Sea Mount"
summary(pdboot)

# Now can do ANOVA (finally)
summary(aov(bootpr ~ ReefType, data= pdboot))
summary(aov(bootpshan ~ ReefType, data= pdboot))
summary(aov(bootpsimp ~ ReefType, data= pdboot))


# OR T test
t.test(bootpr ~ ReefType, data=pdboot)
t.test(bootpshan ~ ReefType, data=pdboot)
t.test(bootpsimp ~ ReefType, data=pdboot)





## NMDS for Community Ordination ##

graphics.off()

treat=c(rep("NS",20),rep("SM",23))
treat

veg <- vegdist(pd)
veg

NMDS<-metaMDS(pd, dist="jaccard")
plot(NMDS)

plot(NMDS$points, type='n', xlab= "NMDS Axis 1", ylab= "NMDS Axis 2")
points(NMDS$points[1:14, ], type='p', cex=0.9, pch=0) # SEY15 - open square
points(NMDS$points[15:38, ], type='p', cex=0.9, pch=1) # SEY16 - open circle







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

