# Multivariate analysis using mvabund 
# Allows GLM on multivariate data

# Created 13 Aug 2021 BJC
# Last edit: 16 Sep 2021 BJC


# Housekeeping
rm(list=ls())
#graphics.off()


# Packages
library(mvabund)
library(reshape2)
library(stringr)
library(writexl)
library(performance)
library(tidyverse)

# Setwd
setwd("/Users/bjcresswell/OneDrive - James Cook University/Ben PhD/Data & analysis/KimbePreds/code/PredDiversity")

# Start with predator diversity df - preddiv
load(file='../../data/preddiv.RData')
# This is a classic species count table with:
# Sites/transects as rows (120 transects)
# Species counts as columns (63 separate taxa in this dataset)
# Plus a bunch of columns with other information (most importantly - reef type)

head(preddiv)


# 1. Data wrangling

# First need to turn our df into an mvabund object:
predabund <- mvabund(preddiv[11:73]) # Drops row names and all other explanatory variables

# Creating mvabund object puts . in all spaces so need to get them back (for later on)
colnames(predabund) <- colnames(predabund) %>% 
  str_replace_all(c("\\." = " "))


# and another object with the predictor variables in (very simple in this case but could be more complicated)
predenv <- preddiv[1:7]



# 2. EDA

# Can interrogate the mean-variance relationship - expected to correlate in abundance data (think Poisson, nb)
par(predabund) # Have to set up viewing pane for some reason.. (also necessary for Rmd output)
meanvar <- meanvar.plot(predabund) 
# Strong MV relationship - expected from this type of count data and would point to Poisson or neg binom distribution as you'd expect

# Basic overview of main species abundance differences across reef type:

specabuntran <- plot(predabund~predenv$Reeftype) # By default applies log transformation, gives you top 10-12. Without transformation you get:
#specabun <- plot(predabund~preddiv$Reeftype, transformation="no")



# 3. Model fitting
# Note:  mvabund cannot handle random factors (yet) so a resampling approach used instead (see below)

# Poisson model
mvmodpois <- manyglm(predabund ~ predenv$Reeftype, test="LR", family="poisson")

# Neg binom model
mvmodnb <- manyglm(predabund ~ predenv$Reeftype, test="LR", family="negative_binomial")



# 4. Model validation 
# In mvabund the only way to check this is plotting residuals vs fitted values
plot.manyglm(mvmodpois, which = 1:4) # Looks like there may be a curved pattern
plot(mvmodnb, which = 1:4) # Negbinom good residuals

# Check AIC scores
AIC(mvmodpois, mvmodnb)

# Neg binom going to be a better fit but...

# Big question: how to take into account random effect of site?

# Option A (not really an option): mixed model variation:

# Any attempt to fit GLMM just crashes computer!!!!
#mvmodnbmm <- manyglm(predabund ~ predenv$Reeftype + 1|predenv$Site, test="LR", family="negative_binomial")

# Option B: include site as an interaction term in model formula

mvmodint <- manyglm(predabund ~ predenv$Reeftype + predenv$Site, test="LR", family="negative_binomial")
AIC(mvmodint, mvmodnb) 

# Model with interaction scores worse (but prob could have predicted this as it's a less parsimonious model)
# Will see what the output is in the anova below


# Option C: some permutational approach
# Below code sourced from http://edild.github.io/mvabund/ 

# Need permute package
library(permute)

# Conduct resampling based on "Site"
control <- how(within = Within(type = 'none'),
               plots = Plots(strata = predenv$Site, type = 'free'),
               nperm = 999)
permutations <- shuffleSet(nrow(predenv), control = control)


# Pairwise comparisons (uni and multivariate) extracted using anova.manyglm function:

# Using the bootID argument:
#modelaovpairwise <- anova.manyglm(mvmodnb, bootID = permutations, pairwise.comp = predenv$Reeftype, p.uni="adjusted")
# Very slow to run -save results for use later
#save(modelaovpairwise, file = "../../data/mvabundaov.rda") # Last run and save 31/10/21
load(file = "../../data/mvabundaov.rda")
# Results
modelaovpairwise

# Deviance is main output from this - how much "variation" unexplained (compared to Null model)

# What about using montecarlo resampling in the anova method?
#modelaovpairwise2 <- anova.manyglm(mvmodnb, resamp="montecarlo", pairwise.comp = predenv$Reeftype, p.uni="adjusted")
# Again, very slow to run -save results for use later
#save(modelaovpairwise2, file = "../../data/mvabundaov2.rda") # Last run and save 4/4/22
load(file = "../../data/mvabundaov2.rda")
# Results
modelaovpairwise2


# What about the model with the interaction term?
#modelaovpairwise3 <- anova.manyglm(mvmodint, resamp="montecarlo", pairwise.comp = predenv$Reeftype, p.uni="adjusted")
# Again, very slow to run -save results for use later
#save(modelaovpairwise3, file = "../../data/mvabundaov3.rda") # Last run and save 4/4/22
load(file = "../../data/mvabundaov3.rda")
# Results
modelaovpairwise3


# And without the pairwise comparison
#modelaovpairwise3a <- anova.manyglm(mvmodint, resamp="montecarlo", nBoot=49, p.uni="adjusted") # Small # of perms to speed up


# Explore results from these anovas

#Summary table (multi and uni variate)
modelaovpairwise

modelaovpairwise$table # Can report as: "Significant effect of reef type on predator fish communities (LRT=507 (* this metric is "deviance"), Permutation P = 0.02)



#LRT = Deviance of model / Devaiance Null model (or other)
  
# Pairwise comparisons
modelaovpairwise$pairwise.comp.table


#Summary
modelaovpairwise2$table 
modsum2 # Can report as: "Significant effect of reef type on predator fish communities (LRT=507, P=0.01)
modelaovpairwise$pairwise.comp.table # No different from model with interaction term




# Pairwise 
modpairs <- modelaovpairwise$pairwise.comp.table %>% 
  as.data.frame()
modpairs
# LRT pairwise comparisons - report results in table

# 2. Details - univariate
uniLRTs <- as.data.frame(t(modelaovpairwise$uni.test)) %>% 
  rownames_to_column("Taxa") %>% 
  dplyr::select("Taxa", `predenv$Reeftype`) %>% 
  as_tibble() %>% 
  mutate(LRT = `predenv$Reeftype`, .keep="unused")

uniPvals <- as.data.frame(t(modelaovpairwise$uni.p)) %>% 
  rownames_to_column("Taxa") %>% 
  dplyr::select("Taxa", `predenv$Reeftype`) %>% 
  as_tibble() %>% 
  mutate(Pvals = `predenv$Reeftype`, .keep="unused")

# Combine to one df
unitests <- uniLRTs %>%
  bind_cols(uniPvals[2]) %>% 
  arrange(Pvals) %>% 
  tibble()
  
unitests

# Note: can't use emmeans as it can't handle a manyglm object

# Save output
write_xlsx(unitests, "../../output/rtables/taxa_LRT_mvabund.xlsx")
save(unitests, file = "../../data/mvabund_unitests.RData")




#### 













