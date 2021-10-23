# Libraries:
library(dplyr)
library(vegan)
library(ggplot2)
library(devtools)
library(ggpubr)
library(tidyverse)
library(ggplot2)
library(car)

fish<-Chapter.1.Fish.Data
save(Chapter_1_Fish_Data, file=('Gdivwork/fish.RData'))
load(file=('Gdivwork/fish.RData'))
fish <- Chapter_1_Fish_Data

  # Remove transect
fish <- fish[ -c(1) ]
  fish.order <- c("Bradford", "Joelles", "Inglis", "Kimbe Bomie","Ema","Ottos","Hogu","Kimbe Island", "Donnas", "Susans", "Lady Di","Madaro")
# mutate the data frame
fish.ordered <- fish %>%
  mutate(SITE=factor(SITE, levels = fish.order)) %>%
  arrange(SITE)
View(fish.ordered)
fish<-fish.ordered

# Rename Seamount to pinnacle
Reef=c(rep("Pinnacle",20), rep("Offshore",20), rep("Nearshore", 20))
fish$REEFTYPE<-Reef
view(fish)

# abundance matrix
fish.matrix <-fish[,3:232] 

# store computed indices in a new data frame called 'indices'
indices <- fish[,c("SITE","REEFTYPE")]
indices$Richness <- rowSums(fish.matrix>0)
indices$Shannon <- diversity(fish.matrix) # shannon is default
indices$Rarefied <- c(rarefy(fish.matrix, MARGIN =1, sample = 15))
indices$Evenness <- apply(fish.matrix>0,1,sum)
indices$Evenness <-diversity(fish.matrix, index="simpson")/log(indices$Evenness)

# Add to data frame
fish<- fish %>% mutate(SHANNON=(diversity(.[,3:230], index="shannon")))
fish<-fish %>% mutate(RICHNESS=apply(.[,3:230]>0, 1, sum))
fish<-fish %>% mutate(ABUNDANCE=apply(.[,3:230],1, sum))
fish<- fish %>% mutate(EVENNESS=(diversity(.[,3:230], index="simpson")))


diversity(data[-1], index="simpson")/log(S)

diversity(data[-1], index="shannon")

Reef=c(rep("Pinnacle",20), rep("Offshore",20), rep("Nearshore", 20))
Reef

# Data exploration and normality checks

# Normality checks
hist(fish$SHANNON)
hist(fish$RICHNESS)
hist(fish$ABUNDANCE)
hist(fish$EVENNESS)

hist(fish$SHANNON[fish$REEFTYPE=="NEARSHORE"])
hist(fish$SHANNON[fish$REEFTYPE=="OFFSHORE"])
hist(fish$SHANNON[fish$REEFTYPE=="PINNACLE"])

plotNormalHistogram(fish$RICHNESS)
plotNormalHistogram(fish$SHANNON)
plotNormalHistogram(fish$ABUNDANCE)
plotNormalHistogram(fish$EVENNESS)

qqnorm(fish$RICHNESS,
       ylab="Fish Richness")
qqline(fish$RICHNESS,
       col="red")

qqnorm(fish$SHANNON,
       ylab="Fish Diversity")
qqline(fish$SHANNON,
       col="red")

qqnorm(fish$ABUNDANCE,
       ylab="Fish Abundance")
qqline(fish$ABUNDANCE,
       col="red")

qqnorm(fish$EVENNESS,
       ylab="Fish Evenness")
qqline(fish$EVENNESS,
       col="red")

shapiro.test(fish$RICHNESS) #NEARLY NORMAL
shapiro.test(fish$ABUNDANCE) #NOT NORMAL#
shapiro.test(fish$SHANNON)#NORMAL
shapiro.test(fish$EVENNESS)  #NOT NORMAL


par(mfrow = c(2, 3), oma = c(0, 0, 2, 0))

# Reset 4x4 graphics
dev.off()
graphics.off

# -- - - - - - - - - - - - - - - - - - - -
# SHANNON H - normal GLM
# -- - - - - - - - - - - - - - - - - - - -
library(MASS)
div.mod <- glm(SHANNON ~ REEFTYPE, data=fish, 
               family=gaussian(link="identity"))


# Explore patterns in simulated residuals:
library(DHARMa)
com.sim<-simulateResiduals(div.mod)
com.sim
plotSimulatedResiduals(com.sim)

# Check model for lack of fit
# Person x2
com.resid <- sum(resid(div.mod, type="pearson")^2)
1-pchisq(com.resid, div.mod$df.resid)
#[1] 0 # evidence of lack of fit (result is <0.05)
# Devience via G2
1-pchisq(div.mod$deviance, div.mod$df.resid)
#[1] 0 # further evidence of lack of fit
# Standardised residuals
plot(div.mod)

# Fitted and observed values
tempdata <- predict(div.mod, type='response', se=TRUE)
tempdata <- cbind(fish, Mean=tempdata[[1]], Lower=tempdata[[1]]-tempdata[[2]],
                  Upper=tempdata[[1]]+tempdata[[2]])
library(ggplot2)
ggplot(tempdata, aes(y=RICHNESS, x=REEFTYPE)) + geom_boxplot() +
  geom_pointrange(aes(y=Mean, ymin=Lower, ymax=Upper), color='red')


# Kolmogorov-Smirnov uniformity test
testUniformity(com.sim)

# Estimate the dispersion parameter to evaluate over dispersion in the fitted model
com.resid/div.mod$df.resid
 
# test the stats and test whether the model is better than the null - analysis if deviance

anova(div.mod, test = "Chisq")
anova(div.mod, test = "LRT")
anova(div.mod)

Anova(div.mod)
Anova(div.mod, type=)
Anova(div.mod, test.statistic = "LR")
Anova(div.mod, test.statistic = "Wald")
summary(div.mod)

# these should produce the same test stat?
anova(div.mod, test = "Chisq")
Anova(div.mod, test.statistic = "LR")



# post hoc

library(multcompView)

library(emmeans)

marginal = emmeans(div.mod, 
                   ~ REEFTYPE)

pairs(marginal,
      adjust="tukey")



# -- - - - - - - - - - - - - - - - - - - -
# RICHNESS
# -- - - - - - - - - - - - - - - - - - - -
# Explore normality for richness, diversity and abundance - choose GLM with approprate error
# distribution

boxplot(RICHNESS~REEFTYPE, data=fish)
plotNormalHistogram(fish$RICHNESS)
library(vcd)
fit <- goodfit(fish$RICHNESS, type='poisson',)
summary(fit)
rootogram(fit)
fit <- goodfit(fish$RICHNESS, type='nbinom')
summary(fit)
rootogram(fit)
Ord_plot(fish$RICHNESS, tol=0.2)
distplot(fish$RICHNESS, type='poisson')

# Fit a poisson GLM

fish.rich.glm <- glm(RICHNESS~REEFTYPE, family=poisson, data=fish)

# Explore patterns in simulated residuals:
library(DHARMa)
com.sim<-simulateResiduals(fish.rich.glm)
com.sim
plotSimulatedResiduals(com.sim)

# Check model for lack of fit
# Person x2
com.resid <- sum(resid(fish.rich.glm, type="pearson")^2)
1-pchisq(com.resid, fish.rich.glm$df.resid)
#[1] 0 # evidence of lack of fit (result is <0.05)
# Devience via G2
1-pchisq(fish.rich.glm$deviance, fish.rich.glm$df.resid)
#[1] 0 # further evidence of lack of fit
# Standardised residuals
plot(fish.rich.glm)

# Fitted and observed values
tempdata <- predict(fish.rich.glm, type='response', se=TRUE)
tempdata <- cbind(fish, Mean=tempdata[[1]], Lower=tempdata[[1]]-tempdata[[2]],
                  Upper=tempdata[[1]]+tempdata[[2]])
library(ggplot2)
ggplot(tempdata, aes(y=RICHNESS, x=REEFTYPE)) + geom_boxplot() +
  geom_pointrange(aes(y=Mean, ymin=Lower, ymax=Upper), color='red')


# Kolmogorov-Smirnov uniformity test
testUniformity(com.sim)

# Estimate the dispersion parameter to evaluate over dispersion in the fitted model
com.resid/fish.rich.glm$df.resid
# Evidence of over dispersion
library(AER)
dispersiontest(fish.rich.glm)
# if α<0, the model is underdispersed and if α>0, the model is
# overdispersed. α is estimated via auxillary OLS regression and 
# tested against a null hypothesis of α=0.  

boxplot(fish$RICHNESS, horizontal=TRUE)
rug(jitter(fish$RICHNESS), side=1)

# not too bad for over dispersion
#but lets try nbinomial

library(MASS)
rich.glmNB <- glm.nb(RICHNESS~REEFTYPE, data=fish)
plot(rich.glmNB)

summary(rich.glmNB)

# explore the model further
fit <- goodfit(fish$RICHNESS, type='nbinom')
summary(fit)

rootogram(fit)
distplot(fish$RICHNESS, type='nbinom')

plot(rich.glmNB)

library(DHARMa)
dat.nb.sim <- simulateResiduals(rich.glmNB)
dat.nb.sim

plotSimulatedResiduals(dat.nb.sim)

#The negative binomial family should handle the majority of overdispersion. 
# However, zero-inflation can still be an issue SO explore overdispersion, goodness of fit and zero inflation

testUniformity(dat.nb.sim)
dat.nb.sim <- simulateResiduals(rich.glmNB, refit=T)
testOverdispersion(dat.nb.sim)
testZeroInflation(dat.nb.sim)

comp.resid <- sum(resid(rich.glmNB, type="pearson")^2)
1-pchisq(comp.resid, rich.glmNB$df.resid)

fit <- goodfit(fish$RICHNESS, type='nbinom')
summary(fit)

# CHECK HOW THE NB MODEL PERFOMRS AGAINST poisson
AIC(rich.glmNB, fish.rich.glm)
# better!

#now lets test the null

#If there was any evidence that the assumptions had been violated or the model 
#was not an appropriate fit, then we would need to reconsider the model and start 
#the process again. In this case, there is no evidence that the test will be unreliable 
#so we can proceed to explore the test statistics. 
anova(rich.glmNB,test="Chisq")
Anova(rich.glmNB,test.statistic="Wald")
Anova(rich.glmNB, test.statistic = "LR")

# POST HOC

library(multcompView)

library(emmeans)

marginal = emmeans(rich.glmNB, 
                   ~ REEFTYPE)

pairs(marginal,
      adjust="tukey")

cld(marginal,
    alpha=0.05, 
    Letters=letters,  ### Use lower-case letters for .group
    adjust="tukey") 

# -- - - - - - - - - - - - - - - - - - - -
# ABUNDANCE - 
# -- - - - - - - - - - - - - - - - - - - -

# Fit a poisson GLM

ab.glm <- glm(ABUNDANCE~REEFTYPE, family=poisson, data=fish)

# Explore patterns in simulated residuals:
library(DHARMa)
com.sim<-simulateResiduals(ab.glm)
com.sim
plotSimulatedResiduals(com.sim)

# Check model for lack of fit
# Person x2
com.resid <- sum(resid(ab.glm, type="pearson")^2)
1-pchisq(com.resid, ab.glm$df.resid)
#[1] 0 # evidence of lack of fit (result is <0.05)
# Devience via G2
1-pchisq(ab.glm$deviance, ab.glm$df.resid)
#[1] 0 # further evidence of lack of fit
# Standardised residuals
plot(ab.glm)

# Fitted and observed values
tempdata <- predict(ab.glm, type='response', se=TRUE)
tempdata <- cbind(fish, Mean=tempdata[[1]], Lower=tempdata[[1]]-tempdata[[2]],
                  Upper=tempdata[[1]]+tempdata[[2]])
library(ggplot2)
ggplot(tempdata, aes(y=RICHNESS, x=REEFTYPE)) + geom_boxplot() +
  geom_pointrange(aes(y=Mean, ymin=Lower, ymax=Upper), color='red')


# Kolmogorov-Smirnov uniformity test
testUniformity(com.sim)

# Estimate the dispersion parameter to evaluate over dispersion in the fitted model
com.resid/fish.rich.glm$df.resid
# Evidence of over dispersion
library(AER)
dispersiontest(ab.glm)
# if α<0, the model is underdispersed and if α>0, the model is
# overdispersed. α is estimated via auxillary OLS regression and 
# tested against a null hypothesis of α=0.  

boxplot(fish$ABUNDANCE, horizontal=TRUE)
rug(jitter(fish$ABUNDANCE), side=1)

# overdispersed

# Quassie-poisson
ab.qp.glm <- glm(ABUNDANCE~REEFTYPE, data=fish, family="quasipoisson")


# Check model for lack of fit
# Person x2
com.resid <- sum(resid(ab.qp.glm, type="pearson")^2)
1-pchisq(com.resid, ab.qp.glm$df.resid)
#[1] 0 # evidence of lack of fit (result is <0.05)
# Devience via G2
1-pchisq(ab.qp.glm$deviance, ab.qp.glm$df.resid)
#[1] 0 # further evidence of lack of fit
# Standardised residuals
plot(ab.qp.glm)

# Fitted and observed values
tempdata <- predict(ab.qp.glm, type='response', se=TRUE)
tempdata <- cbind(fish, Mean=tempdata[[1]], Lower=tempdata[[1]]-tempdata[[2]],
                  Upper=tempdata[[1]]+tempdata[[2]])
library(ggplot2)
ggplot(tempdata, aes(y=RICHNESS, x=REEFTYPE)) + geom_boxplot() +
  geom_pointrange(aes(y=Mean, ymin=Lower, ymax=Upper), color='red')


# Estimate the dispersion parameter to evaluate over dispersion in the fitted model
com.resid/ab.qp.glm$df.resid
# Evidence of over dispersion

#but lets try nbinomial

library(MASS)
ab.glmNB <- glm.nb(ABUNDANCE~REEFTYPE, data=fish)
plot(ab.glmNB)

summary(ab.glmNB)

# explore the model further
fit <- goodfit(fish$ABUNDANCE, type='nbinom')
summary(fit)

rootogram(fit)
distplot(fish$ABUNDANCE, type='nbinom')

plot(ab.glmNB)

library(DHARMa)
dat.nb.sim <- simulateResiduals(ab.glmNB)
dat.nb.sim

plotSimulatedResiduals(dat.nb.sim)

# CHECK HOW THE NB MODEL PERFOMRS AGAINST poisson
AIC(ab.glmNB, ab.glm, ab.qp.glm)
# better!

#now lets test the null

#If there was any evidence that the assumptions had been violated or the model 
#was not an appropriate fit, then we would need to reconsider the model and start 
#the process again. In this case, there is no evidence that the test will be unreliable 
#so we can proceed to explore the test statistics. 
anova(ab.glmNB,test="Chisq")
Anova(ab.glmNB,test.statistic="Wald")
Anova(ab.glmNB,test.statistic="LR")
# POST HOC

library(multcompView)

library(emmeans)

marginal = emmeans(ab.glmNB, 
                   ~ REEFTYPE)

pairs(marginal,
      adjust="tukey")

cld(marginal,
    alpha=0.05, 
    Letters=letters,  ### Use lower-case letters for .group
    adjust="tukey") 



#``{r Post Hoc Testing}

# Note, Tukey post hoc testing assumes normal distribution...?along with Tukey HSD and friends ---- assume a conditional normal distribution of the dependent variable. If someone is using a negative binomial, logistic, Poisson, etc. distribution for their regression, obviously they are not treating their data as conditionally normal. And a zero-inflated model assumes that the data are that further from normality.

# Use emmeans package

library(multcompView)

library(emmeans)

marginal = emmeans(model.zi, 
                   ~ Garden)

pairs(marginal,
      adjust="tukey")

cld(marginal,
    alpha=0.05, 
    Letters=letters,  ### Use lower-case letters for .group
    adjust="tukey") 


https://www.graphpad.com/support/faqid/1081/
  
  https://rcompanion.org/handbook/J_01.html

https://cran.r-project.org/web/packages/emmeans/vignettes/models.html

https://newonlinecourses.science.psu.edu/stat504/node/157/
  
# -- - - - - - - - - - - - - - - - - - - -
# EVENNESS - 
# -- - - - - - - - - - - - - - - - - - - -
  
  # Fit a poisson GLM
ev.norm<-  glm(EVENNESS~REEFTYPE, family=gaussian, data=fish)
 ev.glm <- glm(EVENNESS~REEFTYPE, family=poisson, data=fish)

# Explore patterns in simulated residuals:
library(DHARMa)
com.sim<-simulateResiduals(ev.glm)
com.sim
plotSimulatedResiduals(com.sim)

# Check model for lack of fit
# Person x2
com.resid <- sum(resid(ev.norm, type="pearson")^2)
1-pchisq(com.resid, ev.norm$df.resid)
#[1] 1 #  no evidence of lack of fit (result is > 0.05)
# Devience via G2
1-pchisq(ev.norm$deviance, ev.norm$df.resid)
#[1] 1 # no further evidence of lack of fit
# Standardised residuals
plot(ev.norm)

# Fitted and observed values
tempdata <- predict(ev.norm, type='response', se=TRUE)
tempdata <- cbind(fish, Mean=tempdata[[1]], Lower=tempdata[[1]]-tempdata[[2]],
                  Upper=tempdata[[1]]+tempdata[[2]])
library(ggplot2)
ggplot(tempdata, aes(y=EVENNESS, x=REEFTYPE)) + geom_boxplot() +
  geom_pointrange(aes(y=Mean, ymin=Lower, ymax=Upper), color='red')


# Kolmogorov-Smirnov uniformity test
testUniformity(com.sim)

# Estimate the dispersion parameter to evaluate over dispersion in the fitted model
com.resid/ev.norm$df.resid

# Evidence of over dispersion
library(AER)
dispersiontest(ev.glm) # only to test poisson models
# if α<0, the model is underdispersed and if α>0, the model is
# overdispersed. α is estimated via auxillary OLS regression and 
# tested against a null hypothesis of α=0.

# slightly underdispersed

boxplot(fish$EVENNESS, horizontal=TRUE)
rug(jitter(fish$EVENNESS), side=1)


#but lets try nbinomial

library(MASS)
ev.glmNB <- glm.nb(EVENNESS~REEFTYPE, data=fish)
plot(ev.glmNB)

summary(ev.glmNB)

# explore the model further
fit <- goodfit(fish$EVENNESS, type='nbinom')
summary(fit)

rootogram(fit)
distplot(fish$ABUNDANCE, type='nbinom')

plot(ab.glmNB)

library(DHARMa)
dat.nb.sim <- simulateResiduals(ev.glmNB)
dat.nb.sim

plotSimulatedResiduals(dat.nb.sim)

# CHECK HOW THE NB MODEL PERFOMRS AGAINST poisson
AIC(ev.glmNB, ev.glm, ev.norm)
# negative AIC is good

#now lets test the null

#If there was any evidence that the assumptions had been violated or the model 
#was not an appropriate fit, then we would need to reconsider the model and start 
#the process again. In this case, there is no evidence that the test will be unreliable 
#so we can proceed to explore the test statistics.
anova(ev.norm)
anova(ev.glm, test="Chisq")

anova(ev.glmNB,test="Chisq")
Anova(ev.glmNB,test.statistic="Wald")
Anova(ev.glmNB,test.statistic="LR")




#```{r Means}
#Means per reeftype

mean(BBP.dat$HEXA_COMPLEX[BBP.dat$REEFTYPE=="NEARSHORE"])
mean(BBP.dat$HEXA_COMPLEX[BBP.dat$REEFTYPE=="OFFSHORE"])
mean(BBP.dat$HEXA_COMPLEX[BBP.dat$REEFTYPE=="PINNACLE"])


# adding the reeftype factor BACK into the data summary so we can fill colour by REEFTYPE
fish.matrix$REEFTYPE<-c("PINNACLE","PINNACLE","PINNACLE","PINNACLE", "OFFSHORE","OFFSHORE","OFFSHORE","OFFSHORE",
                     "NEARSHORE","NEARSHORE","NEARSHORE","NEARSHORE")
fish.matrix$SITE<-c(rep("Bradford",5), rep("Joelles",5), rep("Inglis",5), 
                 rep("Kimbe Bomie",5), rep("Ema",5),rep("Ottos",5), 
                 rep("Hogu",5),rep("Kimbe Island",5),rep("Donnas",5),
                 rep("Susans",5),rep("Lady Di",5),rep("Madaro",5))


new.data



+++++++++++++++++++++++++
  # Function to calculate the mean and the standard deviation
  # for each group
  #+++++++++++++++++++++++++
  # data : a data frame
  # varname : the name of a column containing the variable
  #to be summariezed
  # groupnames : vector of column names to be used as
  # grouping variables
  data_summary <- function(data, varname, groupnames){
    require(plyr)
    summary_func <- function(x, col){
      c(mean = mean(x[[col]], na.rm=TRUE),
        se = sd(x[[col]], na.rm=TRUE)/sqrt(length(data())))
    }
    data_sum<-ddply(data, groupnames, .fun=summary_func,
                    varname)
    data_sum <- rename(data_sum, c("mean" = varname))
    return(data_sum)
  }

# Fish Shannon
h.sum<- data_summary(fish, varname="SHANNON", groupnames="REEFTYPE")
h.sum
h.sum<- data_summary(fish, varname="SHANNON", groupnames="SITE")
h.sum

# Fish Abundance
ab.sum<- data_summary(fish, varname="ABUNDANCE", groupnames="SITE")
ab.sum
ab.sum<- data_summary(fish, varname="ABUNDANCE", groupnames="REEFTYPE")
ab.sum

# Fish Richness
rich.sum<- data_summary(fish, varname="RICHNESS", groupnames="SITE")
rich.sum
rich.sum<- data_summary(fish, varname="RICHNESS", groupnames="REEFTYPE")
rich.sum

# Fish Evennes
eve.sum<- data_summary(fish, varname="EVENNESS", groupnames="SITE")
eve.sum
eve.sum<- data_summary(fish, varname="EVENNESS", groupnames="REEFTYPE")
eve.sum


###### All Bar Plots with SE
# Fish diversity, richness, evenness and abundance plots

library(vegan)
library(mobsim)
library(psych)
library(tidyverse)
library(plyr)


+++++++++++++++++++++++++
  # Function to calculate the mean and the standard deviation
  # for each group
  #+++++++++++++++++++++++++
  # data : a data frame
  # varname : the name of a column containing the variable
  #to be summariezed
  # groupnames : vector of column names to be used as
  # grouping variables
  
  data_summary <- function(data, varname, groupnames){
    require(plyr)
    summary_func <- function(x, col){
      c(mean = mean(x[[col]], na.rm=TRUE),
        se = sd(x[[col]], na.rm=TRUE)/sqrt(length(data())))
    }
    data_sum<-ddply(data, groupnames, .fun=summary_func,
                    varname)
    data_sum <- rename(data_sum, c("mean" = varname))
    return(data_sum)
  }


#3. H' Shannon

fish$REEFTYPE<-Reef
view(fish)

div.fish<- data_summary(fish, varname="SHANNON", groupnames="REEFTYPE")
div.fish


Reef.2=c(rep("Pinnacle",4), rep("Offshore",4), rep("Nearshore", 4))

site.div<- data_summary(fish, varname="SHANNON", groupnames="SITE")
site.div
site.div$REEFTYPE<-Reef.2



ggplot(div.fish, aes(x=REEFTYPE, y=SHANNON, fill=REEFTYPE)) + 
  geom_bar(stat="identity", width=.5, color="black", 
           position=position_dodge()) +
  scale_fill_brewer(palette = "Greys")+ 
  geom_errorbar(aes(ymin=SHANNON-se, ymax=SHANNON+se), width=.1,
                position=position_dodge(.9)) +
  labs(title="Mean Diversity of Fishes", x="Habitat Type", y = "Shannon Diversity H'")+
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))



ggplot(site.div, aes(x=SITE, y=SHANNON, fill=REEFTYPE )) + 
  geom_bar(stat="identity", width=.5, color="black", 
           position=position_dodge()) +
  scale_fill_brewer(palette = "Greys") +
  geom_errorbar(aes(ymin=SHANNON-se, ymax=SHANNON+se), width=.1,
                position=position_dodge(.9)) +
  labs(title="Mean Diversity Fishes", x="Site", y = "Shannon Diversity H'")+
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))



aov.H <- aov(SHANNON ~  REEFTYPE, data = fish)
summary(aov.H)
TukeyHSD(aov.H)


aov.H.site <- aov(SHANNON ~  SITE, data = fish)
summary(aov.H.site)
TukeyHSD(aov.H.site)

# Richness

fish<-fish %>% mutate(RICHNESS=apply(.[,3:230]>0, 1, sum))
fish

rich.fish<- data_summary(fish, varname="RICHNESS", groupnames="REEFTYPE")
rich.fish

siterich<- data_summary(fish, varname="RICHNESS", groupnames="SITE")
siterich$REEFTYPE<-Reef.2
siterich

ggplot(rich.fish, aes(x=REEFTYPE, y=RICHNESS, fill=REEFTYPE)) + 
  geom_bar(stat="identity", width=.5, color="black", 
           position=position_dodge()) +
  scale_fill_brewer(palette = "Greys")+ 
  geom_errorbar(aes(ymin=RICHNESS-se, ymax=RICHNESS+se), width=.1,
                position=position_dodge(.9)) +
  labs(title="Mean Richness of Fishes", x="Habitat Type", y = "Mean total Richness")+
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))



ggplot(siterich, aes(x=SITE, y=RICHNESS, fill=REEFTYPE )) + 
  geom_bar(stat="identity", width=.5, color="black", 
           position=position_dodge()) +
  scale_fill_brewer(palette = "Greys") +
  geom_errorbar(aes(ymin=RICHNESS-se, ymax=RICHNESS+se), width=.1,
                position=position_dodge(.9)) +
  labs(title="Mean Richness of Fishes", x="Site", y = "Mean total Richness")+
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))

aov.rich <- aov(RICHNESS ~  REEFTYPE, data = fish)
summary(aov.rich)
TukeyHSD(aov.rich)


aov.rich.site <- aov(RICHNESS ~  SITE, data = fish)
summary(aov.rich.site)
TukeyHSD(aov.rich.site)

# Abundance
fish<-fish %>% mutate(ABUNDANCE=apply(.[,3:230],1, sum))
fish  

ab.fish<- data_summary(fish, varname="ABUNDANCE", groupnames="REEFTYPE")
ab.fish

site.ab<- data_summary(fish, varname="ABUNDANCE", groupnames="SITE")
site.ab
site.ab$REEFTYPE<-Reef.2
site.ab

ggplot(ab.fish, aes(x=REEFTYPE, y=ABUNDANCE, fill=REEFTYPE)) + 
  geom_bar(stat="identity", width=.5, color="black", 
           position=position_dodge()) +
  scale_fill_brewer(palette = "Greys")+ 
  geom_errorbar(aes(ymin=ABUNDANCE-se, ymax=ABUNDANCE+se), width=.1,
                position=position_dodge(.9)) +
  labs(title="Mean Abundance of Fishes", x="Habitat Type", y = "Mean total Abundance")+
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))



ggplot(site.ab, aes(x=SITE, y=ABUNDANCE, fill=REEFTYPE )) + 
  geom_bar(stat="identity", width=.5, color="black", 
           position=position_dodge()) +
  scale_fill_brewer(palette = "Greys") +
  geom_errorbar(aes(ymin=ABUNDANCE-se, ymax=ABUNDANCE+se), width=.1,
                position=position_dodge(.9)) +
  labs(title="Mean Abundance of Fishes", x="Site", y = "Mean total Abundance")+
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))

aov.ab <- aov(ABUNDANCE ~  REEFTYPE, data = fish)
summary(aov.ab)
TukeyHSD(aov.ab)


aov.ab.site <- aov(ABUNDANCE ~  SITE, data = fish)
summary(aov.ab.site)
TukeyHSD(aov.ab.site)


# Evenness

fish<-fish %>% mutate(EVENNESS=apply(.[,3:230]>0, 1, sum))
fish

ev.fish<- data_summary(fish, varname="EVENNESS", groupnames="REEFTYPE")
ev.fish

site.ev<- data_summary(fish, varname="EVENNESS", groupnames="SITE")
site.ev$REEFTYPE<-Reef.2
site.ev

ggplot(ev.fish, aes(x=REEFTYPE, y=EVENNESS, fill=REEFTYPE)) + 
  geom_bar(stat="identity", width=.5, color="black", 
           position=position_dodge()) +
  scale_fill_brewer(palette = "Greys")+ 
  geom_errorbar(aes(ymin=EVENNESS-se, ymax=EVENNESS+se), width=.1,
                position=position_dodge(.9)) +
  labs(title="Mean Evenness of Fishes", x="Habitat Type", y = "Evenness (J)")+
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))



ggplot(site.ev, aes(x=SITE, y=EVENNESS, fill=REEFTYPE )) + 
  geom_bar(stat="identity", width=.5, color="black", 
           position=position_dodge()) +
  scale_fill_brewer(palette = "Greys") +
  geom_errorbar(aes(ymin=EVENNESS-se, ymax=EVENNESS+se), width=.1,
                position=position_dodge(.9)) +
  labs(title="Mean Evenness of Fishes", x="Site", y = "Evenness (J)")+
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))

aov.ev <- aov(EVENNESS ~  REEFTYPE, data = fish)
summary(aov.ev)
TukeyHSD(aov.ev)


aov.ev.site <- aov(EVENNESS ~  SITE, data = fish)
summary(aov.ev.site)
TukeyHSD(aov.ev.site)

###### All Dot Plots with SE
# Fish diversity, richness, evenness and abundance plots

# Fish Diversity
# using h.sum

ggplot(h.sum, aes(x=REEFTYPE, y=SHANNON, fill=REEFTYPE)) + 
  geom_point(stat="identity", size=2, color="black")+
  geom_errorbar(aes(ymin=SHANNON-se, ymax=SHANNON+se), size= .5,width=.1,
                position=position_dodge(.9)) +
  xlab("Reef Morphology")+
  ylab(" Fish diversity \n (H'/transect)")+
  expand_limits( y = 0)+
  theme(
    axis.title.x = element_text( size=14, face="bold"),
    axis.title.y = element_text(size=14, face="bold"),
    axis.text.x = element_text(size=12),
    axis.text.y = element_text(size=12),
    panel.background = element_blank(),
    axis.line = element_line(color = "black", 
                             size = 1, linetype = "solid"),
    legend.position = "none")

ggplot(ab.sum, aes(x=REEFTYPE, y=ABUNDANCE, fill=REEFTYPE)) + 
  geom_point(stat="identity", size=2, color="black")+
  geom_errorbar(aes(ymin=ABUNDANCE-se, ymax=ABUNDANCE+se), size= .5,width=.1,
                position=position_dodge(.9)) +
  xlab("Reef Morphology")+
  ylab(" Fish abundance \n (n/150m")+
  expand_limits( y = 0)+
  theme(
    axis.title.x = element_text( size=14, face="bold"),
    axis.title.y = element_text(size=14, face="bold"),
    axis.text.x = element_text(size=12),
    axis.text.y = element_text(size=12),
    panel.background = element_blank(),
    axis.line = element_line(color = "black", 
                             size = 1, linetype = "solid"),
    legend.position = "none")


ggplot(rich.sum, aes(x=REEFTYPE, y=RICHNESS, fill=REEFTYPE)) + 
  geom_point(stat="identity", size=2, color="black")+
  geom_errorbar(aes(ymin=RICHNESS-se, ymax=RICHNESS+se), size= .5,width=.1,
                position=position_dodge(.9)) +
  xlab("Reef Morphology")+
  ylab(" Fish richness \n (n/transect)")+
  expand_limits( y = 0)+
  theme(
    axis.title.x = element_text( size=14, face="bold"),
    axis.title.y = element_text(size=14, face="bold"),
    axis.text.x = element_text(size=12),
    axis.text.y = element_text(size=12),
    panel.background = element_blank(),
    axis.line = element_line(color = "black", 
                             size = 1, linetype = "solid"),
    legend.position = "none")


ggplot(eve.sum, aes(x=REEFTYPE, y=EVENNESS, fill=REEFTYPE)) + 
  geom_point(stat="identity", size=2, color="black")+
  geom_errorbar(aes(ymin=EVENNESS-se, ymax=EVENNESS+se), size= .5,width=.1,
                position=position_dodge(.9)) +
  xlab("Reef Morphology")+
  ylab(" Fish species evenness\n (J'/transect)")+
  expand_limits( y = 0)+
  theme(
    axis.title.x = element_text( size=14, face="bold"),
    axis.title.y = element_text(size=14, face="bold"),
    axis.text.x = element_text(size=12),
    axis.text.y = element_text(size=12),
    panel.background = element_blank(),
    axis.line = element_line(color = "black", 
                             size = 1, linetype = "solid"),
    legend.position = "none")


# Data exploration and normality checks

# Normality checks
hist(fish$SHANNON)
hist(fish$RICHNESS)
hist(fish$ABUNDANCE)

hist(fish$SHANNON[fish$REEFTYPE=="NEARSHORE"])
hist(fish$SHANNON[fish$REEFTYPE=="OFFSHORE"])
hist(fish$SHANNON[fish$REEFTYPE=="SEAMOUNT"])

# Basic exploration
plot(fish$SHANNON[fish$REEFTYPE=="NEARSHORE"])
plot(fish$SHANNON[fish$REEFTYPE=="OFFSHORE"])
plot(fish$SHANNON[fish$REEFTYPE=="SEAMOUNT"])



# adding the reeftype factor BACK into the data summary so we can fill colour by REEFTYPE
new.data$REEFTYPE<-c("PINNACLE","PINNACLE","PINNACLE","PINNACLE", "OFFSHORE","OFFSHORE","OFFSHORE","OFFSHORE",
                     "NEARSHORE","NEARSHORE","NEARSHORE","NEARSHORE")
new.data$SITE<-c(rep("Bradford",5), rep("Joelles",5), rep("Inglis",5), rep("Kimbe Bomie",5), rep("Ema",5),rep("Ottos",5), rep("Hogu",5),rep("Kimbe Island",5),rep("Donnas",5),rep("Susans",5),rep("Lady Di",5),rep("Madaro",5))

new.data


