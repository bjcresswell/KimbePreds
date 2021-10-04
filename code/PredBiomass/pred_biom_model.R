## ---- pred.biomass.modelfit

# Script to model differences in biomass between reef types
# Created 7.7.2021 BJC
# Last edited 7.7.2021 BJC

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
library(MuMIn)     #for pseudo R2
library(tidyverse) #for data wrangling

# Load data


head(merpredsum_kg)
glimpse(merpredsum_kg)

# As with abundance, need to use a mixed effects model to account for the random factor of site
# Very skewed response data so expecting Gaussian won't work. Not count (integer) data so Poisson probably won't work either

# Gaussian
pred.glmm <- glmmTMB(Transect_biomass_kg~Reeftype+(1|SiteCode), data=merpredsum_kg) 
simulateResiduals(pred.glmm, plot=T) # 
plot_grid(plot_model(pred.glmm,  type='diag'))
performance::check_model(pred.glmm) # Residuals look terrible
binned_residuals(pred.glmm) #20% within error bounds
check_normality(pred.glmm, effects = "random") %>% 
  plot()
summary(pred.glmm)
emmeans(pred.glmm, pairwise~Reeftype, transform="response")

# Poisson
pred.glmmp <- glmmTMB(Transect_biomass_kg~Reeftype+(1|SiteCode), data=merpredsum_kg, family=genpois()) 
simulateResiduals(pred.glmmp, plot=T) # 
plot_grid(plot_model(pred.glmmp,  type='diag'))
performance::check_model(pred.glmmp) # Residuals look bad, however...
binned_residuals(pred.glmmp) #100% within error bounds
check_normality(pred.glmmp, effects = "random") %>% 
  plot()
summary(pred.glmmp)
emmeans(pred.glmmp, pairwise~Reeftype, transform="response")

# Neg binom 2 (Neg binom 1 poor fit)
pred.glmmnb2 <- glmmTMB(Transect_biomass_kg~Reeftype+(1|SiteCode), data=merpredsum_kg, family=nbinom2(link = "log"))  
simulateResiduals(pred.glmmnb2, plot=T) 
plot_grid(plot_model(pred.glmmnb2,  type='diag'))
performance::check_model(pred.glmmnb2) # Residuals look ok
binned_residuals(pred.glmmnb2) #91% inside error bounds
check_normality(pred.glmmnb2, effects = "random") %>% 
  plot()
summary(pred.glmmnb2)
emmeans(pred.glmmnb2, pairwise~Reeftype, transform="response")

# ZI on nb
pred.glmmzi <- glmmTMB(Transect_biomass_kg~Reeftype+(1|SiteCode), data=merpredsum_kg, family=nbinom2(), ziformula=~1)
simulateResiduals(pred.glmmzi, plot=T) 
plot_grid(plot_model(pred.glmmzi,  type='diag'))
performance::check_model(pred.glmmzi) # Residuals look ok
binned_residuals(pred.glmmzi) #100% inside bounds
check_normality(pred.glmmzi, effects = "random") %>% 
  plot()
summary(pred.glmmzi)
emmeans(pred.glmmzi, pairwise~Reeftype, transform="response")

# Tweedie
pred.glmmt <- glmmTMB(Transect_biomass_kg~Reeftype+(1|SiteCode), data=merpredsum_kg, family=tweedie(link = "log"))  
simulateResiduals(pred.glmmt, plot=T) #  Residuals looking better
plot_grid(plot_model(pred.glmmt, type='diag')) # Doesn't work?
performance::check_model(pred.glmmt) # Residuals don't look great, however...
binned_residuals(pred.glmmt) #100% inside error bounds
check_normality(pred.glmmt, effects = "random") %>% 
  plot()
summary(pred.glmmt)
emmeans(pred.glmmt, pairwise~Reeftype, transform="response") %>% 
  confint()


# Discarded:
# A straight up log gaussian won't work
# Gamma or ziGamma - non-positive values not allowed for the 'Gamma' family. Also doesn't work on the T_B_1 data
# Truncated neg binom (with and without ZI)
# ziTweedie
# Beta - inappropriate (0:1)
# Genpois on +1
# Compois on +1

# AIC Scores
AIC(pred.glmm, pred.glmmp, pred.glmmnb2, pred.glmmzi, pred.glmmt)

## Offsetting against total fish biomass per transect (as per abundance)
pred.glmmoff <- update(pred.glmm, ~ . + offset(log(Transect_Fish_Biomass_kg)))
pred.glmmpoff <- update(pred.glmmp, ~ . + offset(log(Transect_Fish_Biomass_kg)))
pred.glmmnboff <- update(pred.glmmnb2, ~ . + offset(log(Transect_Fish_Biomass_kg)))
pred.glmmzioff <- update(pred.glmmzi, ~ . + offset(log(Transect_Fish_Biomass_kg)))
pred.glmmtoff <- update(pred.glmmt, ~ . + offset(log(Transect_Fish_Biomass_kg)))

# Model validation
simulateResiduals(pred.glmmoff, plot=T)
simulateResiduals(pred.glmmpoff, plot=T)
simulateResiduals(pred.glmmnboff, plot=T)
simulateResiduals(pred.glmmzioff, plot=T) 
simulateResiduals(pred.glmmtoff, plot=T) 


# AIC Scores - all
AIC(pred.glmm, pred.glmmp, pred.glmmnb2, pred.glmmzi, pred.glmmt, 
    pred.glmmoff, pred.glmmpoff, pred.glmmnboff, pred.glmmzioff, pred.glmmtoff)


# emmeans
emmeans(pred.glmmoff, pairwise~Reeftype, transform="response") %>% 
  confint()
emmeans(pred.glmmtoff, pairwise~Reeftype, transform="response") %>% 
  confint()
emmeans(pred.glmmnboff, pairwise~Reeftype, transform="response") %>% 
  confint()
emmeans(pred.glmmzioff, pairwise~Reeftype, transform="response") %>% 
  confint()



# Neg Binom offset is best model-> contrasts in response scale:
emmeans(pred.glmmnboff, pairwise~Reeftype, transform="response") %>% 
  confint()


# Tab df output
table <- emmeans(pred.glmmnboff, pairwise~Reeftype, transform="response") %>% 
  confint() %>% 
  as.data.frame() %>% 
  tab_df()
table

simulateResiduals(pred.glmmnboff, plot=T) # Weird looking residuals - why not 3 level factor? Something to do with the offset
#plot_grid(plot_model(pred.glmmtoff, type='diag')) # Doesn't work?
performance::check_model(pred.glmmnboff) # 
binned_residuals(pred.glmmtoff) #100% inside error bounds
check_normality(pred.glmmtoff, effects = "random") %>% 
  plot()
summary(pred.glmmnboff)


# Check pseudo R2 in MuMin 
r.squaredGLMM(pred.glmmnb2) # low R2 on offset model - not sure why?

# To get emmeans contrasts as ratios:
ratios <- emmeans(pred.glmmnboff, pairwise ~ Reeftype, type='response') %>%
  confint() 
ratios

# As absolute kg differences (just changing 'type' to 'transform')
results <- emmeans(pred.glmmnboff, pairwise ~ Reeftype, transform='response') %>%
  confint() 
results


# emmeans figure
plotresults <- ggplot(results$emmeans, aes(y=response,  x=Reeftype)) +
  geom_pointrange(aes(ymin=lower.CL,  ymax=upper.CL))+
  xlab('Reef type')+
  ylab('Mean (kg biomass per transect)')+
  theme_scientific()

# Contrasts figure
plotcontrasts <- ggplot(results$contrasts, aes(y=estimate,  x=contrast)) +
  geom_pointrange(aes(ymin=lower.CL,  ymax=upper.CL))+
  geom_hline(yintercept = 0, linetype='dashed')+
  xlab('Contrast')+
  ylab('Contrast estimate (Δ kg biomass per transect)')+
  coord_flip()+
  theme_scientific()

# Plot together
fig <- grid.arrange(plotresults, plotcontrasts, nrow=1)
plot(fig)
#ggsave(fig, file= '')


