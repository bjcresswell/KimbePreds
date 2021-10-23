# Predator abundance assessment

# Housekeeping
rm(list=ls())
dev.off()

# Set working directory
setwd("~/Science & Education/Ben PhD/Data/Pred Div Analysis/PredAbun")

# Install relevant packages (gdata, vegan etc)
require(gdata)
require(vegan)
require(ggplot2)
require(plyr)
require(dplyr)

# Load and manipulate file
pa <- read.csv("PredAbun.csv", header=F)
head(pa)
tail(pa)
pa <- pa[-c(4:43),]
pa <- t(pa)
pa <- as.data.frame(pa)
colnames(pa) <- as.character(unlist(pa[1,]))
pa = pa[-1, ]
rownames(pa) = pa[,1]
pa <- pa[,-1]
pa$TOTAL <- as.factor(pa$TOTAL)
write.csv(pa, "Temp/pa_as factor.csv")
pa <- read.csv("Temp/pa_as factor.csv", header= T)
pa$TOTAL <- as.numeric(pa$TOTAL)
rownames(pa) = pa[,1]
pa <- pa[,-1]
pa
pa$TOTAL[43] <- 35 # Basically removes outlier of 905 S qenie observed and replaces with observation of 26 (equiv to next largest obs) 
pa
summary(pa)


# So what are the means and SEMs for each group?
sumFunc <- function(x) c(mean=mean(x), n=length(x), median=median(x), se=sd(x)/sqrt(length(x)))
sumFunc(pa$TOTAL[pa$REEFTYPE=="NEARSHORE"])
sumFunc(pa$TOTAL[pa$REEFTYPE=="OFFSHORE"])
sumFunc(pa$TOTAL[pa$REEFTYPE=="SEAMOUNT"])

# Boxplot for data exploration
par(mfrow=c(1,1))
boxplot(TOTAL ~ REEFTYPE, data = pa,
        names=c("Nearshore", "Offshore", "Seamount"), 
        ylab="Abundance (total fish counted/transect",
        main = "Species Abundance",
        col = "grey")

# Obviously not normally distributed!!

# However ANOVA on raw data shows sig diff btween SM and OS/NS
aov(TOTAL ~ REEFTYPE, data = pa)
paaov <- aov(TOTAL ~ REEFTYPE, data = pa)
summary(paaov)
TukeyHSD(paaov)

# Non-parametric Kruskal Wallis shows diff between all
kruskal.test(TOTAL ~ REEFTYPE, data = pa)
pairwise.wilcox.test(pa$TOTAL, pa$REEFTYPE,
                     p.adjust.method = "BH")

# Transformation?
# Save original values for later
pa$RECOVER <- pa$TOTAL
pa$TOTAL <- sqrt(pa$TOTAL)
pa$TOTAL <- pa$RECOVER

# Normality checks

# Visual
par(mfrow=c(2,3))
hist(pa$TOTAL[pa$REEFTYPE=="NEARSHORE"], prob="T", main = "Nearshore", xlab = "Abundance")
curve(dnorm(x, mean(pa$TOTAL[pa$REEFTYPE=="NEARSHORE"]),sd(pa$TOTAL[pa$REEFTYPE=="NEARSHORE"])), 
      add=TRUE, col='red', lwd=3) # Nearshore
hist(pa$TOTAL[pa$REEFTYPE=="OFFSHORE"], prob="T", main = "Offshore", xlab = "Abundance")
curve(dnorm(x, mean(pa$TOTAL[pa$REEFTYPE=="OFFSHORE"]),sd(pa$TOTAL[pa$REEFTYPE=="OFFSHORE"])), 
      add=TRUE, col='red', lwd=3) # Offshore
hist(pa$TOTAL[pa$REEFTYPE=="SEAMOUNT"], prob="T", main = "Seamount", xlab = "Abundance")
curve(dnorm(x, mean(pa$TOTAL[pa$REEFTYPE=="SEAMOUNT"]),sd(pa$TOTAL[pa$REEFTYPE=="SEAMOUNT"])), 
      add=TRUE, col='red', lwd=3) # Offshore
qqnorm(pa$TOTAL[pa$REEFTYPE=="NEARSHORE"], main = "Nearshore")
qqline(pa$TOTAL[pa$REEFTYPE=="NEARSHORE"], col="red", lwd=3)
qqnorm(pa$TOTAL[pa$REEFTYPE=="OFFSHORE"], main = "Offshore")
qqline(pa$TOTAL[pa$REEFTYPE=="OFFSHORE"], col="red", lwd=3)
qqnorm(pa$TOTAL[pa$REEFTYPE=="SEAMOUNT"], main = "Seamount")
qqline(pa$TOTAL[pa$REEFTYPE=="SEAMOUNT"], col="red", lwd=3)

# Shapiro-Wilks
shapiro.test(pa$TOTAL[pa$REEFTYPE=="NEARSHORE"])
shapiro.test(pa$TOTAL[pa$REEFTYPE=="OFFSHORE"])
shapiro.test(pa$TOTAL[pa$REEFTYPE=="SEAMOUNT"])

# So non-normal distributionsn (same as for H' Diversity) Can go with bootstrapping again (see below)

# For now, we'll produce a bar plot on the raw data

# Create three colour palette: Black, blue, green:
cbPalette <- c("#000000", "#56B4E9", "#009E73")
# OR greyscale palette:
gsPalette <- c("grey", "grey48", "grey38")

par(mfrow=c(1,1))
pb <- ggplot(pa, aes(x=REEFTYPE, y=TOTAL, fill=REEFTYPE)) +
  stat_summary(geom = "bar", fun.y = mean, width=0.7) +
  stat_summary(geom = "errorbar", fun.data = mean_se, width=0.15) +
  scale_fill_manual(values=gsPalette) + 
  theme_classic() + 
  ggtitle("Predator abundance by reef type") +
  xlab("Reef Type") + ylab("Fish abundance") +
  theme(plot.title = element_text(family="Arial", size=16, hjust = 0.5)) +
  theme(axis.title.x = element_text(family="Arial", size=12)) +
  theme(legend.position='none')
pb

# ANOVA
aov(TOTAL ~ REEFTYPE, data = pa)
paaov <- aov(TOTAL ~ REEFTYPE, data = pa)
summary(paaov)
TukeyHSD(paaov)



## TIME TO BOOTSTRAP!!!!

#Bootstrapping
a<-numeric(200)
for(i in 1:200) a[i]<-mean(sample(pa$TOTAL[pa$REEFTYPE=="NEARSHORE"],replace=T))
b<-numeric(200)
for(i in 1:200) b[i]<-mean(sample(pa$TOTAL[pa$REEFTYPE=="OFFSHORE"],replace=T))
c<-numeric(200)
for(i in 1:200) c[i]<-mean(sample(pa$TOTAL[pa$REEFTYPE=="SEAMOUNT"],replace=T))

# Recheck again for normality...
par(mfrow=c(2,3))
hist(a, prob="T", main="Nearshore", xlab = "Abundance")
curve(dnorm(x, mean(a),sd(a)), add=TRUE, col='red', lwd=3)
hist(b, prob="T", main="Offshore", xlab = "Abundance")
curve(dnorm(x, mean(b),sd(b)), add=TRUE, col='red', lwd=3)
hist(c, prob="T", main="Seamount", xlab = "Abundance")
curve(dnorm(x, mean(c),sd(c)), add=TRUE, col='red', lwd=3)
qqnorm(a, main="Nearshore Abundance")
qqline(a)
qqnorm(b, main="Offshore Abundance")
qqline(b)
qqnorm(c, main="Seamount Abundance")
qqline(c)

# Normality test...
shapiro.test(a)
shapiro.test(b)
shapiro.test(c)

# Revisualise the distributions
par(mfrow=c(1,1))
boxplot(a,b,c,
        names=c("Near Shore", "Off Shore", "Seamount"), 
        ylab="Abundance (No. of individuals/transect)",
        main = "Fish abundance by reef type",
        col = "grey")

# Reconstruct df with bootstrapped values
TOTAL <- c(a,b,c)
paboot <- data.frame(TOTAL)
paboot$REEFTYPE <- NA #blank column
paboot$REEFTYPE <- "NEARSHORE"
paboot$REEFTYPE[201:400] <- "OFFSHORE"
paboot$REEFTYPE[401:600] <- "SEAMOUNT"
paboot$REEFTYPE <- as.factor(paboot$REEFTYPE)
paboot
summary(paboot)
par(mfrow=c(2,2))
pablm <- lm(TOTAL ~ REEFTYPE, data = paboot)
plot(pablm)

sumFunc(paboot$TOTAL[paboot$REEFTYPE=="NEARSHORE"])
sumFunc(paboot$TOTAL[paboot$REEFTYPE=="OFFSHORE"])
sumFunc(paboot$TOTAL[paboot$REEFTYPE=="SEAMOUNT"])


# Run ANOVA again
summary(aov(bootpa ~ ReefType, data= paboot))
bootaov <- aov(bootpa ~ ReefType, data= paboot)
TukeyHSD(bootaov)
bootaov

# Plot bootstrapped values
bpab <- ggplot(paboot, aes(x=ReefType, y=bootpa, fill=ReefType)) +
  stat_summary(geom = "bar", fun.y = mean, width=0.7) +
  stat_summary(geom = "errorbar", fun.data = mean_se, width=0.15) +
  scale_fill_manual(values=gsPalette) + 
  theme_classic() + 
  ggtitle("Predator Diversity by Reef Type") +
  xlab("Reef Type") + ylab("Shannon Diversity") +
  theme(plot.title = element_text(family="Arial", size=16, hjust = 0.5)) +
  theme(axis.title.x = element_text(family="Arial", size=12)) +
  theme(legend.position='none')
bpab







