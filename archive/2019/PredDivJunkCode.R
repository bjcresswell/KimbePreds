## PRED DIV JUNK CODE

# Push a data-frame into a vector according to indices (Richness, Shannon-Weiner, Simpson etc) #
pred.rich<-specnumber(pd)
pred.shannon<-diversity(pd)

# Explore the data (for full dataset - see below for res)
par(mfrow=c(1,2))
boxplot(pred.rich[1:13], pred.rich[14:31], pred.rich[32:51],
        names=c("NS", "OS", "SM"), 
        ylab="Richness (No. of Species)",
        main = "Species Richness",
        col = "grey")
boxplot(pred.shannon[1:13], pred.shannon[14:31], pred.shannon[32:51],
        names=c("NS", "OS", "SM"), 
        ylab="Shannon Index",
        main = "H' Diversity",
        col = "grey")

# Test for normality:

# Visual check
# Spp richness
par(mfrow=c(2,3))
hist(pred.rich[1:13], prob="T", main = "Nearshore", xlab = "Spp richness")
curve(dnorm(x, mean(pred.rich[1:13]),sd(pred.rich[1:13])), add=TRUE, col='red', lwd=3) # Near shore
hist(pred.rich[14:31], prob="T", main = "Offshore", xlab = "Spp richness") # Off shore
curve(dnorm(x, mean(pred.rich[14:31]),sd(pred.rich[14:31])), add=TRUE, col='red', lwd=3) # Near shore
hist(pred.rich[32:51], prob="T", main = "Seamount", xlab = "Spp richness") # Seamount
curve(dnorm(x, mean(pred.rich[32:51]),sd(pred.rich[32:51])), add=TRUE, col='red', lwd=3) # Near shore
# Shannon
hist(pred.shannon[1:13], prob="T", main = "Nearshore", xlab = "H' Diversity")
curve(dnorm(x, mean(pred.shannon[1:13]),sd(pred.shannon[1:13])), add=TRUE, col='red', lwd=3) # Near shore
hist(pred.shannon[14:31], prob="T", main = "Offshore", xlab = "H' Diversity") # Off shore
curve(dnorm(x, mean(pred.shannon[14:31]),sd(pred.shannon[14:31])), add=TRUE, col='red', lwd=3) # Near shore
hist(pred.shannon[32:51], prob="T", main = "Seamount", xlab = "H' Diversity") # Seamount
curve(dnorm(x, mean(pred.shannon[32:51]),sd(pred.shannon[32:51])), add=TRUE, col='red', lwd=3) # Near shore

# Histograms look skewed...??? What about QQ Plots:

qqnorm(pred.rich[1:13], main = "Spp Rich Nearshore")
qqline(pred.rich[1:13], col="red", lwd=3)
qqnorm(pred.rich[14:31], main = "Spp Rich Offshore")
qqline(pred.rich[14:31], col="red", lwd=3)
qqnorm(pred.rich[32:51], main = "Spp Rich Seamount")
qqline(pred.rich[32:51], col="red", lwd=3)
qqnorm(pred.shannon[1:13], main = "H' Nearshore")
qqline(pred.shannon[1:13], col="red", lwd=3)
qqnorm(pred.shannon[14:31], main = "H' Offshore")
qqline(pred.shannon[14:31], col="red", lwd=3)
qqnorm(pred.shannon[32:51], main = "H' Seamount")
qqline(pred.shannon[32:51], col="red", lwd=3)

# Shapiro Wilks Test
# Species richness
shapiro.test(pred.rich) # the whole data set
shapiro.test(pred.rich[1:13]) # Near shore
shapiro.test(pred.rich[14:31]) # Off shore
shapiro.test(pred.rich[32:51]) # Seamount
shapiro.test(pred.shannon[1:13])
shapiro.test(pred.shannon[14:31])
shapiro.test(pred.shannon[32:51])

# Shannon index also not normally distributed (surprised?)

# So next try transforming the data...

pred.rich <- log(pred.rich)
pred.shannon <- sqrt(pred.shannon)

# No transformation method works (ln, log10, sqrt, abs, exp, sin, asin, ^1/9 etc)

# Need to regenerate the original vectors (now they are messed up from the transformation):
pred.rich<-specnumber(pd)
pred.shannon<-diversity(pd)

# Next step: Check distribution of the residuals...

# First need to create a data frame with the diversity indices/vectors in it:
pd1 <- data.frame(pred.rich, pred.shannon)
pd1$ReefType <- NA #blank column
pd1$ReefType <- "Near Shore"
pd1$ReefType[14:31] <- "Off Shore"
pd1$ReefType[32:51] <- "Sea Mount"
pd1$ReefType <- as.factor(pd1$ReefType)
summary(pd1)
plm <- lm(pred.shannon ~ ReefType, data=pd1)
par(mfrow=c(2,2))
plot(plm)


# OLD CODE REMOVED (used for two factor interatction when df only had SM and NS)
# Wilcox rank sum test (will only work with two levels eg NS/OS or SM/NS )
# wilcox.test(pred.rich ~ ReefType, data=pd1)
# wilcox.test(pred.shannon ~ ReefType, data=pd1)

# What does ANOVA reveal?
summary(aov(pred.shannon ~ ReefType, data=pd1))
pd1aov <- aov(pred.shannon ~ ReefType, data=pd1)
TukeyHSD(pd1aov)


# Maybe bootstrapping is a good way to go??
# First I need to turn each indices vector into a bootstrapped vector of 6000 "observations" - 
# 2000 each from Near Shore, Off Shore and Sea Mount
# NB: Dropping Simpsons from here on in...

# Species Richness
a<-numeric(200)
for(i in 1:200) a[i]<-mean(sample(pred.rich[1:13],replace=T))
b<-numeric(200)
for(i in 1:200) b[i]<-mean(sample(pred.rich[14:31],replace=T))
c<-numeric(200)
for(i in 1:200) c[i]<-mean(sample(pred.rich[32:51],replace=T))

# Shannon Wiener
d<-numeric(200)
for(i in 1:200) d[i]<-mean(sample(pred.shannon[1:13],replace=T))
e<-numeric(200)
for(i in 1:200) e[i]<-mean(sample(pred.shannon[14:31],replace=T))
f<-numeric(200)
for(i in 1:200) f[i]<-mean(sample(pred.shannon[31:51],replace=T))




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

# Hmmmm.... so the plots look more "normal" but the Shapiro test is still saying not normally distributed.

# Other normality checks:
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


# NOTE: Can't do T test as there are 3 levels 
# t.test(bootpr ~ ReefType, data=pdboot)
# t.test(bootpshan ~ ReefType, data=pdboot)


## GGPLOT GRAPHICS FROM RAW DF (pd1) ##


# Create three colour palette: Black, blue, green:
cbPalette <- c("#000000", "#56B4E9", "#009E73")
# OR greyscale palette:
gsPalette <- c("grey", "grey48", "grey38")

# To use for fills, add: scale_fill_manual(values=cbPalette) etc...

# Violin Plot
pv <- ggplot(pd1, aes(x=ReefType, y=pred.shannon, fill=ReefType)) + 
  geom_violin()
pv <- pv + stat_summary(fun.y=mean, geom="point", shape=23, size=2)
pv <- pv + scale_fill_brewer(palette="Paired")+ theme_classic()
pv <- pv + labs(title="Predator Diversity by Reef Type",x="Reef Type", y = "Shannon Diversity")
pv

# Bar plot
pb <- ggplot(pd1, aes(x=ReefType, y=pred.shannon, fill=ReefType)) +
  stat_summary(geom = "bar", fun.y = mean, width=0.7) +
  stat_summary(geom = "errorbar", fun.data = mean_se, width=0.15) +
  scale_fill_manual(values=cbPalette) + 
  theme_classic() + 
  ggtitle("Predator Diversity by Reef Type") +
  xlab("Reef Type") + ylab("Shannon Diversity") +
  theme(plot.title = element_text(family="Arial", size=16, hjust = 0.5)) +
  theme(axis.title.x = element_text(family="Arial", size=12)) +
  theme(legend.position='none')
pb


# A more detailed attempt:

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

# Violin Plot
bpv <- ggplot(pdboot, aes(x=ReefType, y=bootpshan, fill=ReefType)) + 
  geom_violin()
bpv <- bpv + stat_summary(fun.y=mean, geom="point", shape=23, size=2)
bpv <- bpv + scale_fill_brewer(palette="Paired")+ theme_classic()
bpv <- bpv + labs(title="Predator Diversity by Reef Type",x="Reef Type", y = "Shannon Diversity")
bpv

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


