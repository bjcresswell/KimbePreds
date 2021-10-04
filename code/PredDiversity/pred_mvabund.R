# Multivariate analysis using mvabund 
# Allows GLM on multivariate data

# Created 13 Aug 2021 BJC
# Last edit: 16 Sep 2021 BJC


# Housekeeping
#rm(list=ls())
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


# 1. Data wrangling

# First need to turn our df into an mvabund object:
predabund <- mvabund(preddiv[9:71]) # Drops row names and all other explanatory variables

colnames(predabund) <- colnames(predabund) %>% 
  str_replace_all(c("\\." = " "))


# and another object with the predictor variables in (very simple in this case but could be more complicated)
predenv <- preddiv[1:7]



# 2. EDA

# Can interrogate the mean-variance relationship
par(predabund) # Sets up viewing pane - might not need in Rmd
meanvar <- meanvar.plot(predabund) 
# Strong MV relationship - expected from this type of count data and would point to Poisson or neg binom distribution

# Basic overview of main species abundance differences across reef type
#specabun <- plot(predabund~preddiv$Reeftype, transformation="no")
specabuntran <- plot(predabund~predenv$Reeftype) # By default applies log transformation


# 3. Model fitting
# Note:  mvabund cannot handle random factors (yet) so a resampling approach used instead (see below)

# Poisson model
mvmodpois <- manyglm(predabund ~ predenv$Reeftype, test="LR", family="poisson")
# Neg binom model
mvmodnb <- manyglm(predabund ~ predenv$Reeftype, test="LR", family="negative_binomial")


# 4. Model validation 
# In mvabund the only way to check this is plotting residuals vs fitted values
plot(mvmodpois) # Looks like there may be a curved pattern
plot(mvmodnb) # Negbinom good residuals

# Check AIC scores
AIC(mvmodpois, mvmodnb) # Negbinom better so will move forward with that


# 5. Model investigation and hypothesis testing
# So need permute package to conduct resampling based on "Site"
# Below code sourced from http://edild.github.io/mvabund/ 

library(permute)
control <- how(within = Within(type = 'none'),
               plots = Plots(strata = predenv$Site, type = 'free'),
               nperm = 999)
permutations <- shuffleSet(nrow(predenv), control = control)

R.version.string

# Pairwise comparisons (uni and multivariate) extracted using anova.manyglm function:
# modelaovpairwise <- anova.manyglm(mvmodnb, bootID = permutations, pairwise.comp = predenv$Reeftype, p.uni="adjusted")
# Very slow to run -save results for use later


#save(modelaovpairwise, file = "../../data/mvabundaov.rda") # Last save 17/9/21
load(file = "../../data/mvabundaov.rda")

# Results overview
modelaovpairwise

# 1. Details - multivariate
modsum <- modelaovpairwise$table 
modsum # Can report as: "Significant effect of reef type on predator fish communities (LRT=507, P=0.02)
modpairs <- modelaovpairwise$pairwise.comp.table 
modpairs # LRT pairwise comparisons - report results in table

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





