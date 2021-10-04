


pred.div.metrics <- pred.div.metrics %>% 
  mutate(SimpsonAdj = (Simpson+0.00000001))


# Histogram
shhist <- ggplot(pred.div.metrics, aes(x=Simpson)) + 
  geom_histogram(binwidth=0.1, color="black", fill="grey") # Possibly bimodal??
shhist


# Gaussian
psimp.glmmnorm <- glmmTMB(Simpson ~ Reeftype+(1|Site), data = pred.div.metrics)
simulateResiduals(psimp.glmmnorm, plot=T) # Looks good
testZeroInflation(psimp.glmmnorm)
binned_residuals(psimp.glmmnorm) # 91% inside error bounds
plot_grid(plot_model(psimp.glmmnorm,  type='diag')[-2])
performance::check_model(psimp.glmmnorm)


# Inverse Gaussian
psimp.glmmig <- glmmTMB(Simpson ~ Reeftype+(1|Site), data = pred.div.metrics, family = inverse.gaussian())
simulateResiduals(psimp.glmmnorm, plot=T) # Looks good
binned_residuals(psimp.glmmnorm) # 100% inside error bounds
plot_grid(plot_model(psimp.glmmnorm,  type='diag')[-2])
performance::check_model(psimp.glmmnorm)



# Poisson
psimp.glmmp <- glmmTMB(Simpson ~ Reeftype+(1|Site), data = pred.div.metrics, family='poisson')
simulateResiduals(psimp.glmmp, plot=T) # Bad
testZeroInflation(psimp.glmmp)

binned_residuals(psimp.glmmp) # 91% inside error bounds
plot_grid(plot_model(psimp.glmmp,  type='diag')[-2])
performance::check_model(psimp.glmmnorm)

# Binom
psimp.glmmbn <- glmmTMB(Simpson ~ Reeftype+(1|Site), data = pred.div.metrics, family=binomial(link = "logit"))
simulateResiduals(psimp.glmmbn, plot=T) # Bad
binned_residuals(psimp.glmmbn) # 82% inside error bounds
plot_grid(plot_model(psimp.glmmnorm,  type='diag')[-2])
performance::check_model(psimp.glmmnorm)
rm(psimp.glmmbn)


# Neg binom 1
psimp.glmmnb1 <- glmmTMB(Simpson ~ Reeftype+(1|Site), data = pred.div.metrics, family='nbinom1')
simulateResiduals(psimp.glmmnb1, plot=T) # Bad
#testZeroInflation(psimp.glmmnb1) # No ZI
binned_residuals(psimp.glmmnb1) # 82% inside error bounds
plot_grid(plot_model(psimp.glmmnorm,  type='diag')[-2])
performance::check_model(psimp.glmmnorm)

# Neg binom 2
psimp.glmmnb2 <- glmmTMB(Simpson ~ Reeftype+(1|Site), data = pred.div.metrics, family='nbinom2')
simulateResiduals(psimp.glmmnb2, plot=T) # Bad
#testZeroInflation(psimp.glmmnb2) # No ZI
binned_residuals(psimp.glmmnb2) # 82% inside error bounds
plot_grid(plot_model(psimp.glmmnb2,  type='diag')[-2])
performance::check_model(psimp.glmmnb2) # Terrible residuals

# Gamma - only on SimpsonAdj
psimp.glmmgam <- glmmTMB(SimpsonAdj ~ Reeftype+(1|Site), data = pred.div.metrics, family=Gamma)
simulateResiduals(psimp.glmmgam, plot=T) # Terrible residuals
binned_residuals(psimp.glmmgam) # 100% inside error bounds
plot_grid(plot_model(psimp.glmmgam,  type='diag')[-2])
performance::check_model(psimp.glmmgam) # Terrible residuals

# Tweedie
psimp.glmmtw <- glmmTMB(Simpson ~ Reeftype+(1|Site), data = pred.div.metrics, family=tweedie(link = "log"))
simulateResiduals(psimp.glmmtw, plot=T) # Looks good
testZeroInflation(psimp.glmmtw) # No ZI
binned_residuals(psimp.glmmtw) # 100% inside error bounds
plot_grid(plot_model(psimp.glmmtw,  type='diag')[-2])
performance::check_model(psimp.glmmtw)

# Quasi
psimp.glmmq <- glmmTMB(Simpson ~ Reeftype+(1|Site), data = pred.div.metrics, family=quasibinomial(link = "logit"))
simulateResiduals(psimp.glmmq, plot=T) # Looks good
testZeroInflation(psimp.glmmq) # No ZI
binned_residuals(psimp.glmmq) # 100% inside error bounds
plot_grid(plot_model(psimp.glmmq,  type='diag')[-2])
performance::check_model(psimp.glmmq)


# Won't work:
# Gamma - has to be positive values
# ziGamma - same
# Beta - has to be  0 < y < 1
# Binom - y values must be 0 <= y <= 1
# Inverse gaussian - positive values only and wouldn't work on SimpsonAdj
# All quasi families = don't work in glmmTMB
# All truncated families




AIC(psimp.glmmnorm, psimp.glmmp, psimp.glmmnb1, psimp.glmmnb2, psimp.glmmtw, psimp.glmmgam)

