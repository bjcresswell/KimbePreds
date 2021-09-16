

# BGLMM Model work - Reeftype ~ Predator Numbers

# Packages and housework
rm(list=ls())
library(rstanarm)   #for fitting models in STAN
library(brms)       #for fitting models in STAN
library(coda)       #for diagnostics
library(bayesplot)  #for diagnostics
library(rstan)      #for interfacing with STAN
library(emmeans)    #for marginal means etc
library(broom)      #for tidying outputs
library(broom.mixed)
library(DHARMa)     #for residual diagnostics
library(tidybayes)  #for more tidying outputs
library(ggeffects)  #for partial plots
library(tidyverse)  #for data wrangling etc

# Load and check data
load('data/predsum.RData')
head(predsum)

getwd()
vignette("brms_families")

ggplot(predsum, aes(y=No_Preds,x=Reeftype)) + geom_boxplot()
ggplot(predsum, aes(y=No_Preds,x=as.numeric(SurvCode), linetype=Reeftype)) + geom_line()
ggplot(predsum, aes(y=No_Preds,x=as.numeric(SurvCode), linetype=Reeftype)) + geom_blank(aes(x=SurvCode)) + geom_line()

# For the Intercept
pred.form <- bf(No_Preds~(1|SurvCode)+ # SurvCode as random or "varying" effect
                     Reeftype,  family=student)

# Simpler model:

pred.form <- bf(No_Preds~Reeftype,  family=hurdle_lognormal) # No varying effect


# Going to use default priors
pred.brms <- brm(pred.form, data=predsum,
                    refresh=0,
                    chains=3, iter=10000, warmup=6000, thin=5)

# Model diagnostics
mcmc_plot(pred.brms,  type='trace')
mcmc_plot(pred.brms,  type='acf_bar')
mcmc_plot(pred.brms,  type='rhat_hist')
mcmc_plot(pred.brms,  type='neff_hist')

# Residual diagnostics
pred.post <- posterior_predict(pred.brms,  nsamples=250,  summary=FALSE)
pred.resids <- createDHARMa(simulatedResponse = t(pred.post),
                               observedResponse = predsum$No_Preds,
                               fittedPredictedResponse = apply(pred.post, 2, median),
                               integerResponse = T)
plot(pred.resids)

ggpredict(pred.brms) %>% plot
tidyMCMC(pred.brms$fit,conf.int=TRUE, conf.method='HPDinterval',
         rhat=TRUE, ess=TRUE, estimate.method='median')
