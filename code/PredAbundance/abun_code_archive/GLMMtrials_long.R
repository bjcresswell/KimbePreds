## ---- pred.abun.modelfit

# Script to model differences in abundance
# Still hacking away 24.6.2021  BJC


#rm(list=ls())
#graphics.off()

library(gt)
library(lme4)
library(car)       #for regression diagnostics
library(multcomp)  #for lm work
library(broom)     #for tidy output
library(ggfortify) #for model diagnostics
library(DHARMa)    #for residual diagnostics
library(performance)
library(glmmTMB)   #for wider selection of error families
library(sjPlot)    #for outputs
library(effects)   #for partial effects plots
library(ggeffects) #for partial effects plots
library(emmeans)   #for estimating marginal means
library(modelr)    #for auxillary modelling functions
library(tidyverse) #for data wrangling

# Load data
load('data/predsum.RData')
head(predsum)

# Quick check of data
ggplot(predsum, aes(x=No_Preds)) + 
  geom_histogram(binwidth=50, color="black", fill="grey")


ggplot(predsum, aes(x=PredPC)) + 
  geom_histogram(binwidth=0.01, color="black", fill="grey")

# Clearly non-normal distribution so we'll most likely need to utilise a distribution other than Gaussian in the final 

######## PART A ########
## GLMM analysis on predator numbers - there are two angles on this:

# 1. Using the absolute abundances of predators. These data are very skewed: lots of 0 observations and some very high numbers (driven by schools of e.g. Sphyraenids/Carangids etc)
# Can attempt to adjust for this, for example by transforming data or scaling back the largest observations (these might be considered outliers anyway??), OR:

# 2. Using the predator percentages (proportion of whole fish assemblage). This would provide values between 0-100% (or 0-1 in the case of proportions)
# Downside of this approach is that we have to add a tiny amount to the % calculation in the pred_diw code that produces the PredPC column as most model error families that can handle such skewed data can only handle 
# values between 0 and 1.

# Am not sure which approach to settle on - see the below script which shows that predator numbers come out significanly higher, regardless of which of the two approaches above are used.
# Either way, it needs to be a mixed effects model to handle the random factors required (Site and SurvCode). Packages will either be lme4 or glmmTMB

#### 1a. GLMM on No_Preds (absolute numbers + 1)

# Gaussian
pred.glmm <- glmmTMB(No_Preds~Reeftype+(1|SurvCode)+(1|SiteCode), data=predsum)  
simulateResiduals(pred.glmm, plot=T) # Residuals bad
qqnorm(resid(pred.glmm))
binned_residuals(pred.glmm) # 45% within error bounds
check_normality(pred.glmm, effects = "random") %>% 
  plot()
summary(pred.glmm)
emmeans(pred.glmm, pairwise~Reeftype, transform="response")

# Poisson
pred.glmmp <- glmer(No_Preds~Reeftype+(1|SurvCode)+(1|SiteCode), data=predsum, family=poisson()) 
simulateResiduals(pred.glmmp, plot=T) # 
qqnorm(resid(pred.glmmp))
binned_residuals(pred.glmmp) #73% within error bounds
check_normality(pred.glmmp, effects = "random") %>% 
  plot()
summary(pred.glmmp)
emmeans(pred.glmmp, pairwise~Reeftype, transform="response")

# Neg binom 1 (have to use glmmTMB)
pred.glmmnb1 <- glmmTMB(No_Preds~Reeftype+(1|SurvCode)+(1|SiteCode), data=predsum, family=nbinom1(link = "log"))  
simulateResiduals(pred.glmmnb1, plot=T) #
qqnorm(resid(pred.glmmnb1))
binned_residuals(pred.glmmnb1) #18% inside error bounds
check_normality(pred.glmmnb1, effects = "random") %>% 
  plot()
summary(pred.glmmnb1)
emmeans(pred.glmmnb1, pairwise~Reeftype, transform="response")

# Neg binom 2 (have to use glmmTMB)
pred.glmmnb2 <- glmmTMB(No_Preds~Reeftype+(1|SurvCode)+(1|SiteCode), data=predsum, family=nbinom2(link = "log"))  
simulateResiduals(pred.glmmnb2, plot=T) 
qqnorm(resid(pred.glmmnb2))
binned_residuals(pred.glmmnb2) #73% inside bounds
check_normality(pred.glmmnb2, effects = "random") %>% 
  plot()
summary(pred.glmmnb2)
emmeans(pred.glmmnb2, pairwise~Reeftype, transform="response")

# ZI
pred.glmmzi <- glmmTMB(No_Preds~Reeftype+(1|SurvCode)+(1|SiteCode), data=predsum, family=nbinom2(), ziformula=~1)
simulateResiduals(pred.glmmzi, plot=T) 
qqnorm(resid(pred.glmmzi))
binned_residuals(pred.glmmzi) #73% inside bounds
check_normality(pred.glmmzi, effects = "random") %>% 
  plot()
summary(pred.glmmzi)
emmeans(pred.glmmzi, pairwise~Reeftype, transform="response")

# Gamma - using glmmTMB
pred.glmmg1 <- glmmTMB(No_Preds~Reeftype+(1|SurvCode)+(1|SiteCode), data=predsum, family=Gamma())  
simulateResiduals(pred.glmmg1, plot=T) # DHARMa not working on this model
qqnorm(resid(pred.glmmg1))
binned_residuals(pred.glmmg1) #73% inside bounds
check_normality(pred.glmmg1, effects = "random") %>% 
  plot()
summary(pred.glmmg1)
emmeans(pred.glmmg1, pairwise~Reeftype, transform="response")

# ZI Gamma
pred.glmmzig <- glmmTMB(No_Preds~Reeftype+(1|SurvCode)+(1|SiteCode), data=predsum, family=ziGamma())
simulateResiduals(pred.glmmzig, plot=T) # Doesn't work
qqnorm(resid(pred.glmmzig))
binned_residuals(pred.glmmzig) #73% within bounds
check_normality(pred.glmmzig, effects = "random") %>% 
  plot()

# Tweedie (takes ages)
pred.glmmxxx <- glmmTMB(No_Preds~Reeftype+(1|SurvCode)+(1|SiteCode), data=predsum, family=tweedie(link = "log"))  
simulateResiduals(pred.glmmxxx, plot=T) #  Residuals looking better
qqnorm(resid(pred.glmmxxx))
binned_residuals(pred.glmmxxx)
check_normality(pred.glmmxxx, effects = "random") %>% 
  plot()
AIC(pred.glmmxxx)
summary(pred.glmmxxx)
emmeans(pred.glmmxxx, pairwise~Reeftype, transform="response")

## Discarded (for No_Preds):

# Gaussian: residuals (45% within error bounds)
# Binomial y values must be 0 <= y <= 1
# Compois: wouldn't run/stalled
# Neg Binom 1: residuals (18% within error bounds)
# Beta:  y values must be 0 < y < 1
# Tweedie: residuals (73% within error bounds)

## AIC Scoring - No_Preds
AIC(pred.glmm, pred.glmmp, pred.glmmnb1, pred.glmmnb2, pred.glmmzi, pred.glmmzig, pred.glmmg1, pred.glmmxxx) 

#### 1b. GLMM on No_PredsZ (absolute numbers)

# Gaussian
predz.glmm <- glmmTMB(No_PredsZ~Reeftype+(1|SurvCode)+(1|SiteCode), data=predsum)  
simulateResiduals(predz.glmm, plot=T) 
qqnorm(resid(predz.glmm))
binned_residuals(predz.glmm) #73% wihin error bounds
check_normality(predz.glmm, effects = "random") %>% 
  plot()


# Poisson
predz.glmmp <- glmer(No_PredsZ~Reeftype++(1|SurvCode)+(1|SiteCode), data=predsum, family=poisson()) 
simulateResiduals(predz.glmmp, plot=T) 
qqnorm(resid(predz.glmmp))
binned_residuals(predz.glmmp) #91% within error bounds
check_normality(predz.glmmp, effects = "random") %>% 
  plot()

# Binomial 
#predz.glmmbn <- glmer(No_PredsZ~Reeftype++(1|SurvCode)+(1|SiteCode), data=predsum, family=binomial())  # Doesn't work. y values must be 0 <= y <= 1

# Neg binom 1 (have to use glmmTMB)
predz.glmmnb1 <- glmmTMB(No_PredsZ~Reeftype+(1|SurvCode)+(1|SiteCode), data=predsum, family=nbinom1())  
simulateResiduals(predz.glmmnb1, plot=T)
qqnorm(resid(predz.glmmnb1))
binned_residuals(predz.glmmnb1) #18% inside error bounds
check_normality(predz.glmmnb1, effects = "random") %>% 
  plot()

predz.glmmnb2 <- glmmTMB(No_PredsZ~Reeftype+(1|SurvCode)+(1|SiteCode), data=predsum, family=nbinom2())  
simulateResiduals(predz.glmmnb2, plot=T) #  Residuals looking better
qqnorm(resid(predz.glmmnb2))
binned_residuals(predz.glmmnb2) # 91% inside error bounds
check_normality(predz.glmmnb2, effects = "random") %>% 
  plot()

# ZI
predz.glmmzi <- glmmTMB(No_PredsZ~Reeftype++(1|SurvCode)+(1|SiteCode), data=predsum, family=nbinom2(), ziformula=~1)
simulateResiduals(predz.glmmzi, plot=T) 
qqnorm(resid(predz.glmmzi))
binned_residuals(predz.glmmzi) # 91% inside error bounds
check_normality(predz.glmmzi, effects = "random") %>% 
  plot()

# ZI
predz.glmmzi <- glmmTMB(No_PredsZ~Reeftype++(SurvCode|SiteCode), data=predsum, family=nbinom2(), ziformula=~1)
simulateResiduals(predz.glmmzi, plot=T) 
qqnorm(resid(predz.glmmzi))
binned_residuals(predz.glmmzi) # 91% inside error bounds
check_normality(predz.glmmzi, effects = "random") %>% 
  plot()



## Discarded (for No_PredsZ):

# Gaussian: residuals (73% within error bounds)
# Binomial y values must be 0 <= y <= 1
# Neg Binom 1: residuals (18% within error bounds)
# Beta:  y values must be 0 < y < 1
# All gamma - can't use on non-positive values
# Tweedie: residuals (73% within error bounds)

  
## AIC Scoring - unadjusted data
AIC(predz.glmm, predz.glmmp, predz.glmmnb1, predz.glmmnb2, predz.glmmzi) 

## AIC Scoring - all data
AIC(predz.glmm, predz.glmmp, predz.glmmnb1, predz.glmmnb2, predz.glmmzi, pred.glmm, pred.glmmp, pred.glmmnb1, pred.glmmnb2, pred.glmmzi, pred.glmmzig, pred.glmmg1) 

# predz.glmmnb2 best AIC score

# So now try with SurvPeriod excluded as random factor
predz.glmmnb3 <- glmmTMB(No_PredsZ~Reeftype+(1|SiteCode), data=predsum, family=nbinom2())  
AIC(predz.glmmnb2, predz.glmmnb3)


### Model interogation  ###
summary(predz.glmmnb2)

# emmeans and contrasts:
emmeans(predz.glmmnb2, pairwise~Reeftype, transform="response") %>% # Pairwise comparisons back in the response scale.
  confint() 

emmeans(predz.glmmnb3, pairwise~Reeftype, transform="response") %>% # Pairwise comparisons back in the response scale.
  confint() 



# On average Pinnacles have around 26 more predator fishes per transect (150m^-2) than  offshore emergent reefs but this could range 0-53
# On average Pinnacles have around 28 more predator fishes per transect (150m^-2) than  offshore emergent reefs but this could range 0-55
# No significant difference between offshore and near shore





