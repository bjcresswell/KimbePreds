



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








