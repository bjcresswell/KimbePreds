
# Last edit 26 July 2021

#### GLMM on adjusted abundance - number of predators per transect +1 (No_Preds)
# Removed this from script as I went with the offset approach instead

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
#pred.glmmg <- glmmTMB(No_Preds~Reeftype+(1|SiteCode), data=predsum, family=Gamma())  
#save(pred.glmmg, file = "../../data/pred.glmmg.rda")
load("../../data/pred.glmmg.rda")

#simulateResiduals(pred.glmmg, plot=T) # DHARMa not working on this model
plot_grid(plot_model(pred.glmmg,  type='diag'))
performance::check_model(pred.glmmg) # Residuals don't look great, however...
binned_residuals(pred.glmmg) #80% inside bounds
check_normality(pred.glmmg, effects = "random") %>% 
  plot()
summary(pred.glmmg)
emmeans(pred.glmmg, pairwise~Reeftype, transform="response")

# ZI Gamma - also takes a while
#pred.glmmzig <- glmmTMB(No_Preds~Reeftype+(1|SiteCode), data=predsum, family=ziGamma())
#save(pred.glmmzig, file = "../../data/pred.glmmzig.rda")
load("../../data/pred.glmmzig.rda")

#simulateResiduals(pred.glmmzig, plot=T) # Doesn't work
plot_grid(plot_model(pred.glmmzig,  type='diag'))
performance::check_model(pred.glmmzig) # Residuals don't look great, however...
binned_residuals(pred.glmmzig) #73% within bounds
check_normality(pred.glmmzig, effects = "random") %>% 
  plot()

# Tweedie (takes ages so have saved for future use)
#pred.glmmt <- glmmTMB(No_Preds~Reeftype+(1|SiteCode), data=predsum, family=tweedie(link = "log")) 
#save(pred.glmmt, file = "../../data/pred.glmmt.rda")
load("../../data/pred.glmmt.rda")

simulateResiduals(pred.glmmt, plot=T) #  Residuals looking better
#plot_grid(plot_model(pred.glmmt,  type='diag'))
#performance::check_model(pred.glmmt) # Residuals don't look great, however...
binned_residuals(pred.glmmt) #82% inside error bounds
check_normality(pred.glmmt, effects = "random") %>% 
  plot()

## Discarded (for No_Preds):

# Gaussian: residuals (45% within error bounds)
# Binomial y values must be 0 <= y <= 1
# Compois: wouldn't run/stalled
# Neg Binom 1: residuals (18% within error bounds, for both raw and adjusted data
# Beta:  y values must be 0 < y < 1
# Tweedie: residuals (73% within error bounds)

## AIC Scoring - No_Preds
AIC(pred.glmmp, pred.glmmnb2, pred.glmmzi, pred.glmmzig, pred.glmmg, pred.glmmt) 




## 2. GLMM on PredPC (proportion of assemblage that are predators - some of these are on the unadjusted data and some on the +0.000001 depending on what the model family requires)

# Gaussian
pc.glmm <- glmmTMB(PredPCZ~Reeftype+(1|SurvCode)+(1|SiteCode), data=predsum, family='gaussian')
simulateResiduals(pc.glmm, plot=T) # -> Bad residuals for Gaussian - cannot use this model formula but will send forward for AIC scoring anyhow as I'm interested
qqnorm(resid(pc.glmm))
binned_residuals(pc.glmm) #82%
check_normality(pc.glmm, effects = "random") %>% 
  plot()
AIC(pc.glmm)
summary(pc.glmm)
emmeans(pc.glmm, pairwise~Reeftype, transform="response")

# Poisson
pc.glmmp <- glmmTMB(PredPC~Reeftype+(1|SurvCode)+(1|SiteCode), data=predsum, family='poisson')
simulateResiduals(pc.glmmp, plot=T) 
qqnorm(resid(pc.glmmp))
binned_residuals(pc.glmmp) #82%
check_normality(pc.glmmp, effects = "random") %>% 
  plot()
AIC(pc.glmmp)
summary(pc.glmmp)
emmeans(pc.glmmp, pairwise~Reeftype, transform="response")


# Gamma - won't work with non-positive integers
pc.glmmg <- glmmTMB(PredPC~Reeftype+(1|SurvCode)+(1|SiteCode), data=predsum, family='Gamma')
simulateResiduals(pc.glmmg, plot=T) 
qqnorm(resid(pc.glmmg))
binned_residuals(pc.glmmg)
check_normality(pc.glmmg, effects = "random") %>% 
  plot()
AIC(pc.glmmg)
summary(pc.glmmg)
emmeans(pc.glmmg, pairwise~Reeftype, transform="response")



# Neg binomial 1
pc.glmmnb1 <- glmmTMB(PredPCZ~Reeftype+(1|SurvCode)+(1|SiteCode), data=predsum, family=nbinom1(link = "log"))
simulateResiduals(pc.glmmnb1, plot=T) # 
binned_residuals(pc.glmmnb1) #64%
check_normality(pc.glmmnb1, effects = "random") %>% 
  plot()
AIC(pc.glmmnb1)
summary(pc.glmmnb1)
emmeans(pc.glmmnb1, pairwise~Reeftype, transform="response")


# Neg binomial 2
pc.glmmnb2 <- glmmTMB(PredPCZ~Reeftype+(1|SiteCode), data=predsum, family=nbinom2(link = "log"))
simulateResiduals(pc.glmmnb1, plot=T) # -> 
qqnorm(resid(pc.glmmnb1))
binned_residuals(pc.glmmnb1) #64%
check_normality(pc.glmmnb1, effects = "random") %>% 
  plot()
AIC(pc.glmmnb1)
summary(pc.glmmnb1)
emmeans(pc.glmmnb1, pairwise~Reeftype, transform="response")

# Proportional data so really need to try beta family or something similar (won't work on Preds_PCZ as y values must be 0 < y < 1)
pc.glmmbeta <- glmmTMB(PredPC~Reeftype+(1|SurvCode)+(1|SiteCode), data=predsum, family=beta_family(link = "logit"))
simulateResiduals(pc.glmmbeta, plot=T) # Good residuals
qqnorm(resid(pc.glmmbeta))
binned_residuals(pc.glmmbeta) #73%
check_normality(pc.glmmbeta, effects = "random") %>% 
  plot()
AIC(pc.glmmbeta)
summary(pc.glmmbeta)
emmeans(pc.glmmbeta, pairwise~Reeftype, transform="response")


# Try beta with ZI (you would think you'd need to use the raw, unadjusted data - PredPCZ but actually get better residuals and AIC from the adjusted)
pc.glmmbetazi <- glmmTMB(PredPC~Reeftype+(1|SurvCode)+(1|SiteCode), data=predsum, family=beta_family(link = "logit"),  ziformula=~1)
simulateResiduals(pc.glmmbetazi, plot=T) # Good residuals
qqnorm(resid(pc.glmmbetazi))
binned_residuals(pc.glmmbetazi) #73%
check_normality(pc.glmmbetazi, effects = "random") %>% 
  plot()
AIC(pc.glmmbetazi)
summary(pc.glmmbetazi)
emmeans(pc.glmmbetazi, pairwise~Reeftype, transform="response")


# Betabinomial (need to use glmmTMB)
#pc.glmmbbn <- glmmTMB(PredPCZ~Reeftype+(1|SiteCode), data=predsum, family=betabinomial(link = "logit"))
#simulateResiduals(pc.glmmbbn, plot=T) #  Problem with convergence - exclude from AIC scoring

# Tweedie (need to use glmmTMB) 
pc.glmmt <- glmmTMB(PredPCZ~Reeftype+(1|SiteCode), data=predsum, family=tweedie(link = "log"))
simulateResiduals(pc.glmt, plot=T) #  Won't work most of the time, just crashes computer! When it does run: problem with convergence - exclude from AIC scoring


# Other



## AIC Scoring
AIC(pc.glmm, pc.glmmg, pc.glmmbeta, pc.glmmt) 

# Beta best

######

# From the AIC scores (that would display) Beta is the best model fit


pcglmm.TAB<-emmeans(pc.glmmbeta, pairwise~Reeftype, transform="response") %>% 
  confint() 
pcglmm.TAB












# Options for model validation
#performance::check_model(pred.glmmg) # Doesn't work with this data structure
#plot_grid(plot_model(pred.glmmg,  type='diag')) # Same as above
#testZeroInflation(resid) # We actually adjusted the data so they don't contain any zeros so of course it comes out with less than expected

# Hypothesis testing and model investigation
# The following lifted from ML tutorial. Not sure how useful
plot(allEffects(pred.glmmg))
ggemmeans(pred.glmmg, ~Reeftype) %>% plot()
plot_model(pred.glmmg,  type='eff')

# To interrogate the random effect
summary(pred.glmmg)
summary(pred.glmmg2)


pred.glmmg.TAB<-emmeans(pred.glmmg2, pairwise~Reeftype, transform="response") %>% # Pairwise comparisons back in the response scale.
  confint() 
predglmg.TAB








