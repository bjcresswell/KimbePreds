## ---- pred.abun.modelfit

# Script to model differences in abundance
# Last edit 26 July 2021  BJC

rm(list=ls())
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
setwd("/Users/bjcresswell/OneDrive - James Cook University/Ben PhD/Data & analysis/KimbePreds/code/PredAbundance")
load('../../data/predsum.RData')
head(predsum)
glimpse(predsum)


##### GLMM analysis on predator numbers #####

# Considered a few options for this: 

# 1. Using the absolute abundances of predators. Data are very skewed: lots of 0 observations and some very high numbers (driven by schools of e.g. Sphyraenids/Carangids etc)
# Can attempt to adjust for this, for example by transforming data or scaling back the largest observations (these might be considered outliers anyway??), OR:

# 2. Using the predator percentages (proportion of whole fish assemblage). This would provide values between 0-100% (or 0-1 in the case of proportions)
# Downside of this approach is that we have to add a tiny amount to the % calculation in the pred_diw code that produces the PredPC column as most model error families that can handle such skewed data can only handle 
# values between 0 and 1.  Also converting to proportion data loses some detail?

# 3. Using absolute abundances of predators + 1: means you can use more error families (Gamma etc) but is a bit janky

# 4. Using absolute abundances of predators with an offset to handle overall abundances of fish

# Went with #4: a mixed effects model (random factor - Site) with offset
# Models built in either be lme4 or glmmTMB.


#### 1. Basic GLMM on absolute numbers (will update model later with offset)

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
#save(predz.glmmt, file = "./data/predz.glmmt.rda")
load("../../data/predz.glmmt.rda")
#simulateResiduals(pred.glmmt, plot=T) #  Crashes R
plot_model(predz.glmmt,  type='diag')
performance::check_model(predz.glmmt) # Residuals don't look great, however...
binned_residuals(predz.glmmt) #100% inside error bounds
check_normality(predz.glmmt, effects = "random") %>% 
  plot()

## Discarded :

# Gaussian: residuals (73% within error bounds)
# Poisson: no diff to No_Preds
# Binomial y values must be 0 <= y <= 1
# Neg Binom 1: residuals (18% within error bounds)
# Beta:  y values must be 0 < y < 1
# All gamma - can't use on non-positive values

## AIC scoring
AIC(predz.glmmnb, predz.glmmzi, predz.glmmt) 

# Refit models with offset
predz.glmmnboff <- update(predz.glmmnb, ~ . + offset(log(No_Fish)))
predz.glmmzioff <- update(predz.glmmzi, ~ . + offset(log(No_Fish)))
predz.glmmtoff <- update(predz.glmmt, ~ . + offset(log(No_Fish)))

simulateResiduals(predz.glmmnboff, plot=T) #  
simulateResiduals(predz.glmmzioff, plot=T) #  
simulateResiduals(predz.glmmtoff, plot=T) #  


binned_residuals(predz.glmmnboff) # 91% inside error bounds
binned_residuals(predz.glmmzioff) # 91% inside error bounds
binned_residuals(predz.glmmtoff) # 91% inside error bounds


AIC(predz.glmmnb, predz.glmmnboff, predz.glmmzi, predz.glmmzioff, predz.glmmt, predz.glmmtoff)

# Net binom with offset best fit

### Model interogation  ###

# Check main model
summary(predz.glmmnboff)

# Check ZI model
summary(predz.glmmzi)

## emmeans:

# Comparing total numbers
emmeans(predz.glmmnboff, pairwise~Reeftype, transform="response") %>% 
  confint() %>% 
  as.data.frame() %>% 
  tab_df()
# Comparing ratios
emmeans(predz.glmmnboff, pairwise~Reeftype, type="response") %>% 
  confint()


# On average Pinnacles have around 11 more predator fishes per transect (150m^-2) than  offshore emergent reefs but this could range 0-22
# On average Pinnacles have around 12 more predator fishes per transect (150m^-2) than  near shore reefs but this could range 1-23
# No significant difference between offshore and near shore

