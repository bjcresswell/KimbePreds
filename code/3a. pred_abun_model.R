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
glimpse(predsum)

# Check data


######## PART A ########
## GLMM analysis on predator numbers - there are two angles on this:

# 1. Using the absolute abundances of predators. These data are very skewed: lots of 0 observations and some very high numbers (driven by schools of e.g. Sphyraenids/Carangids etc)
# Can attempt to adjust for this, for example by transforming data or scaling back the largest observations (these might be considered outliers anyway??), OR:

# 2. Using the predator percentages (proportion of whole fish assemblage). This would provide values between 0-100% (or 0-1 in the case of proportions)
# Downside of this approach is that we have to add a tiny amount to the % calculation in the pred_diw code that produces the PredPC column as most model error families that can handle such skewed data can only handle 
# values between 0 and 1.

# Both approaches require a mixed effects model to handle the random factor involved (Site). 
# Models built in either be lme4 or glmmTMB.

#### GLMM on Abundance (Preds or PredsZ will vary depending on error distribution)

# Poisson
# Note: not much difference in AIC between adjusted and raw (3515 and 3537 - and this is huge compared to other models), nor between using glmer and glmmTMB)
# Note 2: could try ZI on Poisson distribution but AIC is so bad that not worth it - will do on nb instead
pred.glmmp <- glmmTMB(No_Preds~Reeftype+(1|SiteCode), data=predsum, family=poisson()) 
simulateResiduals(pred.glmmp, plot=T) # 
plot_grid(plot_model(pred.glmmp,  type='diag'))
performance::check_model(pred.glmmp) # Residuals look bad, however...
binned_residuals(pred.glmmp) #100% within error bounds
check_normality(pred.glmmp, effects = "random") %>% 
  plot()
summary(pred.glmmp)
emmeans(pred.glmmp, pairwise~Reeftype, transform="response")

# Neg binom 2 (Neg binom 1 poor fit)
pred.glmmnb2 <- glmmTMB(No_Preds~Reeftype+(1|SiteCode), data=predsum, family=nbinom2(link = "log"))  
simulateResiduals(pred.glmmnb2, plot=T) 
plot_grid(plot_model(pred.glmmnb2,  type='diag'))
performance::check_model(pred.glmmnb2) # Residuals don't look great, however...
binned_residuals(pred.glmmnb2) #80% inside bounds
check_normality(pred.glmmnb2, effects = "random") %>% 
  plot()
summary(pred.glmmnb2)
emmeans(pred.glmmnb2, pairwise~Reeftype, transform="response")

# ZI on nb
pred.glmmzi <- glmmTMB(No_Preds~Reeftype+(1|SiteCode), data=predsum, family=nbinom2(), ziformula=~1)
simulateResiduals(pred.glmmzi, plot=T) 
plot_grid(plot_model(pred.glmmzi,  type='diag'))
performance::check_model(pred.glmmzi) # Residuals don't look great, however...
binned_residuals(pred.glmmzi) #82% inside bounds
check_normality(pred.glmmzi, effects = "random") %>% 
  plot()
summary(pred.glmmzi)
emmeans(pred.glmmzi, pairwise~Reeftype, transform="response")

# Gamma - takes a while to run
pred.glmmg <- glmmTMB(No_Preds~Reeftype+(1|SiteCode), data=predsum, family=Gamma())  
simulateResiduals(pred.glmmg, plot=T) # DHARMa not working on this model
plot_grid(plot_model(pred.glmmg,  type='diag'))
performance::check_model(pred.glmmg) # Residuals don't look great, however...
binned_residuals(pred.glmmg) #80% inside bounds
check_normality(pred.glmmg, effects = "random") %>% 
  plot()
summary(pred.glmmg)
emmeans(pred.glmmg, pairwise~Reeftype, transform="response")

# ZI Gamma - also takes a while
pred.glmmzig <- glmmTMB(No_Preds~Reeftype+(1|SiteCode), data=predsum, family=ziGamma())
simulateResiduals(pred.glmmzig, plot=T) # Doesn't work
plot_grid(plot_model(pred.glmmzig,  type='diag'))
performance::check_model(pred.glmmzig) # Residuals don't look great, however...
binned_residuals(pred.glmmzig) #73% within bounds
check_normality(pred.glmmzig, effects = "random") %>% 
  plot()

# Tweedie (takes ages so have saved for future use)
#pred.glmmt <- glmmTMB(No_Preds~Reeftype+(1|SiteCode), data=predsum, family=tweedie(link = "log"))  
#simulateResiduals(pred.glmmt, plot=T) #  Residuals looking better
#plot_grid(plot_model(pred.glmmt,  type='diag'))
#performance::check_model(pred.glmmt) # Residuals don't look great, however...
binned_residuals(pred.glmmt) #82% inside error bounds
check_normality(pred.glmmt, effects = "random") %>% 
  plot()
#save(pred.glmmt, file = "./data/pred.glmmt.rda")
load("./data/pred.glmmt.rda")

## Discarded (for No_Preds):

# Gaussian: residuals (45% within error bounds)
# Binomial y values must be 0 <= y <= 1
# Compois: wouldn't run/stalled
# Neg Binom 1: residuals (18% within error bounds, for both raw and adjusted data
# Beta:  y values must be 0 < y < 1
# Tweedie: residuals (73% within error bounds)

## AIC Scoring - No_Preds
AIC(pred.glmmp, pred.glmmnb2, pred.glmmzi, pred.glmmzig, pred.glmmg, pred.glmmt) 

#### 1b. GLMM on No_PredsZ (absolute numbers)

# Neg binom form 2 (formula 1 poor fit)
predz.glmmnb <- glmmTMB(No_PredsZ~Reeftype+(1|SiteCode), data=predsum, family=nbinom2())  
simulateResiduals(predz.glmmnb, plot=T) # 
plot_grid(plot_model(predz.glmmnb,  type='diag'))
performance::check_model(predz.glmmnb) # Residuals don't look great, however...
binned_residuals(predz.glmmnb) # 100% inside error bounds
check_normality(predz.glmmnb, effects = "random") %>% 
  plot()

# ZI
predz.glmmzi <- glmmTMB(No_PredsZ~Reeftype+(1|SiteCode), data=predsum, family=nbinom2(), ziformula=~1)
simulateResiduals(predz.glmmzi, plot=T) 
plot_grid(plot_model(predz.glmmzi,  type='diag'))
performance::check_model(predz.glmmzi) # Residuals don't look great, however...
binned_residuals(predz.glmmzi) # 100% inside error bounds
check_normality(predz.glmmzi, effects = "random") %>% 
  plot()

# Tweedie (takes ages)
#predz.glmmt <- glmmTMB(No_PredsZ~Reeftype+(1|SiteCode), data=predsum, family=tweedie(link = "log"))  
#simulateResiduals(pred.glmmt, plot=T) #  Residuals looking better
plot_grid(plot_model(predz.glmmt,  type='diag'))
performance::check_model(predz.glmmt) # Residuals don't look great, however...
binned_residuals(predz.glmmt) #100% inside error bounds
check_normality(predz.glmmt, effects = "random") %>% 
  plot()
#save(predz.glmmt, file = "./data/predz.glmmt.rda")
load("./data/predz.glmmt.rda")

## Discarded (for No_PredsZ):

# Gaussian: residuals (73% within error bounds)
# Poisson: no diff to No_Preds
# Binomial y values must be 0 <= y <= 1
# Neg Binom 1: residuals (18% within error bounds)
# Beta:  y values must be 0 < y < 1
# All gamma - can't use on non-positive values


## AIC Scoring - unadjusted data
AIC(predz.glmmnb, predz.glmmzi, predz.glmmt) 

## AIC Scoring - all data
AIC(predz.glmmnb, predz.glmmzi, predz.glmmt, pred.glmmp, pred.glmmnb2, pred.glmmzi, pred.glmmzig, pred.glmmg, pred.glmmt) 

# Refit nb with REML
predz.glmmnb2 <- update(predz.glmmnb, ~ . + offset(log(No_Fish)))
simulateResiduals(pred.glmmnb2, plot=T) #  Residuals looking better
binned_residuals(predz.glmmnb2)
AIC(predz.glmmnb, predz.glmmnb2)

### Model interogation  ###
summary(predz.glmmnb)
summary(predz.glmmzi)
emmeans(predz.glmmnb, pairwise~Reeftype, transform="response") %>% 
  confint()

emmeans(predz.glmmnb2, pairwise~Reeftype, transform="response") %>% 
  confint()

emmeans(predz.glmmzi, pairwise~Reeftype, transform="response") %>% 
  confint()

emmeans(predz.glmmnb2, pairwise~Reeftype) %>% 
  confint()

# On average Pinnacles have around 26 more predator fishes per transect (150m^-2) than  offshore emergent reefs but this could range 0-53
# On average Pinnacles have around 28 more predator fishes per transect (150m^-2) than  offshore emergent reefs but this could range 0-55
# No significant difference between offshore and near shore

