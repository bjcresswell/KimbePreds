

## 1. Fitting standard GLM on No_Preds (absolute numbers)


# Std GLM = Gaussian
pred.glm <- glm(No_Preds~Reeftype, data=predsum, family='gaussian')
simulateResiduals(pred.glm, plot=T) # -> Bad residuals for Gaussian - cannot use this model formula but will send forward for AIC scoring anyhow as I'm interested

# Std GLM = Poisson
pred.glmp <- glm(No_Preds~Reeftype, data=predsum, family='poisson')
simulateResiduals(pred.glmp, plot=T) # -> Bad residuals for Poisson - cannot use this model formula but will send forward for AIC scoring anyhow

# Std GLM - Gamma
pred.glmg <- glm(No_Preds~Reeftype, data=predsum, family='Gamma') # As this model ends up with the best AIC scores, need to keep the Pred+1 code in diw
simulateResiduals(pred.glmg, plot=T) # -> Better residuals for Gamma - send forward for AIC scoring

# Std GLM - Neg binom (need to use glmmTMB)
pred.glmnb <- glmmTMB(No_Preds~Reeftype, data=predsum, family=nbinom2())
simulateResiduals(pred.glmnb, plot=T) # Residuals better for NB - send forward for AIC scoring

# Models that won't work on the No_Preds data
# Binomial: y values must be 0 <= y <= 1
# Beta: y values must be 0 < y < 1
# Tweedie: not sure why but wouldn't converge

# Note: the only issue with the residuals for both Gamma and NB models are over dispersion and I think both these distributions can handle overdisperson errors?

# Final 2 models I want to check is ZI on NB and Gamma distribution (need to do it on No_PredsZ to keep the zero values in)
pred.glmnbz <- glmmTMB(No_PredsZ~Reeftype, data=predsum, family=nbinom1(), ziformula=~1)
simulateResiduals(pred.glmnbz, plot=T) # Residuals better for NB - send forward for AIC scoring

pred.glmgz <- glmmTMB(No_PredsZ~Reeftype, data=predsum, family=ziGamma(link="log"), ziformula=~1)
simulateResiduals(pred.glmgz, plot=T) # Residuals better for NB - send forward for AIC scoring


## AIC Scoring
AIC(pred.glm, pred.glmp, pred.glmg, pred.glmnb, pred.glmnbz, pred.glmgz) # Even with the ZI models on the raw data Gamma best score

# So: emmeans and contrasts on Gamma model:
predglmg.TAB<-emmeans(pred.glmg, pairwise~Reeftype, transform="response") %>% # Pairwise comparisons back in the response scale.
  confint() 
predglmg.TAB

# So on average Pinnacles have around 60 more predator fishes per transect (150m^-2) than either near shore or offshore emergent reefs
# and this could range: 23-96 more (for offshore) or 25-98 more (for near shore).
# No significant difference between offshore and near shore

# Interestingly the neg binom model estimates tighter confints:
predglmnb.TAB<-emmeans(pred.glmnb, pairwise~Reeftype, transform="response") %>% 
  confint() 
predglmnb.TAB


## 2. Fitting standard GLM on PredPC (proportion of assemblage that are predators)

# Std GLM = Gaussian
pc.glm <- glm(PredPC~Reeftype, data=predsum, family='gaussian')
simulateResiduals(pc.glm, plot=T) # -> Bad residuals for Gaussian - cannot use this model formula but will send forward for AIC scoring anyhow as I'm interested

# Std GLM = Poisson
#pc.glmp <- glm(PredPC~Reeftype, data=predsum, family='poisson')
#simulateResiduals(pc.glmp, plot=T) # -> Poisson not appropriate for non-integer data

# Std GLM = binomial
#pc.glmb <- glm(PredPC~Reeftype, data=predsum, family='binomial')
#simulateResiduals(pc.glmb, plot=T) # -> won't work properly on non-integer data

# Std GLM - Gamma
pc.glmg <- glm(PredPC~Reeftype, data=predsum, family='Gamma')
simulateResiduals(pc.glmg, plot=T) # Bad residuals for both Gamma but will send forward for AIC scoring anyhow as I'm interested

# Std GLM - Neg binomial (need to use glmmTMB)
#pc.glmnb <- glmmTMB(PredPC~Reeftype, data=predsum, family=nbinom1(link = "log"))
#simulateResiduals(pc.glmnb, plot=T) #  Problem with convergence - exclude from AIC scoring

# Proportional data so really need to try beta family or something similar
pc.glmbeta <- glmmTMB(PredPC~Reeftype, data=predsum, family=beta_family(link = "logit"))
simulateResiduals(pc.glmbeta, plot=T) # Good residuals

# Try beta with ZI (you would think you'd need to use the raw, unadjusted data - PredPCZ but actually get better residuals and AIC from the adjusted)
pc.glmbetaz <- glmmTMB(PredPCZ~Reeftype, data=predsum, family=beta_family(link = "logit"),  ziformula=~1)
simulateResiduals(pc.glmbetaz, plot=T) # Good residuals


# Std GLM - Betabinomial (need to use glmmTMB)
#pc.glmbbn <- glmmTMB(PredPC~Reeftype, data=predsum, family=betabinomial(link = "logit"))
#simulateResiduals(pc.glmbbn, plot=T) #  Problem with convergence - exclude from AIC scoring

# Std GLM - Tweedie (need to use glmmTMB) 
#pc.glmt <- glmmTMB(PredPC~Reeftype, data=predsum, family=tweedie(link = "log"))
#simulateResiduals(pc.glmt, plot=T) #  Won't work most of the time, just crashes computer! When it does run: problem with convergence - exclude from AIC scoring


# Others....

# Quasi
pc.glmq <- glm(PredPC~Reeftype, data=predsum, family=quasi)
# simulateResiduals(pc.glmqbn, plot=T) # Doesn't work on quasi family
autoplot(pc.glmq, which=1:6, ncol=2, label.size=3)

# Quasibinomial
pc.glmqbn <- glm(PredPC~Reeftype, data=predsum, family=quasibinomial)
#simulateResiduals(pc.glmqbn, plot=T) # Doesn't work on quasi family
autoplot(pc.glmqbn, which=1:6, ncol=2, label.size=3)


## AIC Scoring
AIC(pc.glm, pc.glmg, pc.glmbeta, pc.glmq, pc.glmqbn, pc.glmbetaz) 

# Can't get an AIC score for the quasi and quasi binomial. Hmmmm....  Maybe for the same reasons I can't run simulateResiduals?
# Plain Beta best

######

# From the AIC scores (that would display) Beta is the best model fit


pcglm.TAB<-emmeans(pc.glmbeta, pairwise~Reeftype, transform="response") %>% 
  confint() 
pcglm.TAB
