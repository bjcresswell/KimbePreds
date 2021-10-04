# mvabund modeling approach
# Last edit: 8 Sep 2021

library(mvabund)
library(plyr)
library(reshape2)
library(performance)
library(tidyverse)
library(emmeans)

setwd("/Users/bjcresswell/OneDrive - James Cook University/Ben PhD/Data & analysis/KimbePreds/code/PredDiversity")
rm(list=ls())
graphics.off()
load(file='../../data/preddiv.RData')


# 1. Data wrangling

# Assign row names (actually not necessary as mvabund gets rid anyways)
preddiv <- preddiv %>% column_to_rownames("TID") 

predabund <- mvabund(preddiv[8:70]) # Drops row names and all other explanatory variables
predenv <- preddiv[1:7]


# 2. EDA

# Can interrogate the mean-variance relationship
par(predabund)
meanvar <- meanvar.plot(predabund) # Strong MV relationship - expected from this type of count data - prob suggests neg binom

# Basic overview of main species differences across reef type
#specabun <- plot(predabund~preddiv$Reeftype, transformation="no")
specabuntran <- plot(predabund~preddiv$Reeftype)
#grid.arrange(specabun, specabuntran)


# 3. Initial model fitting

# Poisson model
mvmodpois <- manyglm(predabund ~ predenv$Reeftype, test="LR", family="poisson")
# Neg binom model
mvmodnb <- manyglm(predabund ~ predenv$Reeftype, test="LR", family="negative_binomial")


# 4. Model validation

# Check residuals
plot(mvmodpois) # Looks like there may be a curved pattern
plot(mvmodnb) # Negbinom good residuals


# Check AIC scores
AIC(mvmodpois, mvmodnb) # Negbinom better so will move forward with that


# 5. Model investigation and hypothesis testing

# 5a.  Model summary and comparisons- takes a while to run each
#summary_mvmodnb <- summary(mvmodnb)
#summary.manyglm(mvmodnb) # -> This is the same as just running the summary function above
# summary_mvmodnb # The summary output is of limited use as it doesn't have the same resampling step incorporated as achieved by anova and test statistic is wald value

# 5b. So conduct anova:

# mvabund cannot handle random factors
# So need permute package to conduct resampling based on "Site"
# (Below code taken from http://edild.github.io/mvabund/ ) 

library(permute)
control <- how(within = Within(type = 'none'),
               plots = Plots(strata = predenv$Site, type = 'free'),
               nperm = 999)
permutations <- shuffleSet(nrow(predenv), control = control)

# Then run 
#modelaovbasic <- anova(mvmodnb,  bootID = permutations)
modelaovpairwise <- anova(mvmodnb, bootID = permutations, pairwise.comp = predenv$Reeftype, p.uni="adjusted")

# Quick check
#modelaovbasic
modelaovpairwise

# Details
modelaovpairwise$table # Can report as: "Significant effect of reef type on predator fish communities (LRT=507, P=0.02)
modelaovpairwise$pairwise.comp.table # LRT pairwise comparisons - report results in table
modelaovpairwise$uni.p # These 2 exported to excel and filtered there as I can't work out how to coerce to df inside R
modelaovpairwise$uni.test # These 2 exported to excel and filtered there as I can't work out how to coerce to df inside R


# Note: can't use emmeans as it can't handle a manyglm object

# Save results for use later

#save(modelaovpairwise, file = "../../data/mvabundaov.rda")
load(file = "../../data/mvabundaov.rda")





### JUNK ###

#mod_nb_null <- manyglm(predabund ~ Site, data = predenv, 
#                       family = 'negative.binomial')
#mod_treat_aov <- anova(mvmodnb, mod_nb_null , 
#                       bootID = permutations,  
#                       test = 'LR') 
#mod_treat_aov


#mod_pt <- NULL
#for (i in levels(predenv$Reeftype)) {
#  take_abu <- predabund[predenv$Reeftype == i, ]
#  take_env <- predenv[predenv$Reeftype == i, ]
#  # model
#  mod_pt[[i]]$mod <- manyglm(take_abu ~ Site, data = take_env)
#  mod_pt[[i]]$aov <- anova(mod_pt[[i]]$mod, nBoot = 100, 
#                           p.uni = 'adjusted', test = 'LR', show.time = "none")
#  mod_pt[[i]]$sum <- summary(mod_pt[[i]]$mod, nBoot = 100, 
#                             p.uni = 'adjusted', test = 'LR')
#}


#get_pvals <- function(x){
#  comm <- c(community = x$aov$table[2, 4])
#  spec <- x$aov$uni.p[2, ]
#  c(comm, spec)
#}
#ldply(mod_pt, get_pvals)

#devs <- ldply(mod_pt, function(x) x$aov$uni.test[2, ])
#plotdf <- melt(devs, id.vars = '.id')

#ggplot(plotdf, aes(x = .id, y = value, fill = variable)) +
#  geom_col() + 
#  ylim(0, 100) +
#  theme_bw() +
#  labs(x = 'Site', y = 'Deviance (=Effect)')


#get_coef <- function(x){
#  x$sum$coefficients[-1, 1]
#}
#coefs <- ldply(mod_pt, get_coef)
#coefs <- melt(coefs, id.vars = '.id')

#ggplot(coefs, aes(x = as.numeric(as.character(.id)), y = value, col = variable)) +
#  geom_bar() +
#  theme_bw() +
#  labs(x = 'Time', y = 'Deviance (=Effect)') +
#  scale_color_discrete('Treatment', labels = c(0.1, 0.9, 6, 44)) +
#  theme(legend.position = 'bottom', 
#        legend.key = element_blank())
