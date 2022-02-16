# Multivariate analysis using vegan - adonis (PERMANOVA) 
# Alternative to GLM on multivariate data

# Created 13 Feb 2022 BJC
# Last edit: 


# Housekeeping
rm(list=ls())
#graphics.off()


# Packages
library(vegan)
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

# Create observation matrix for permanova
predmatrix <- preddiv[11:73] # Drops row names and all other explanatory variables

# Create env object with the predictor variables
predenv <- preddiv[1:7]


# 2. Run permanova
predperm <- adonis(predmatrix ~ Reeftype, data = predenv)
predperm
predperm$coefficients
predperm$aov.tab


# Doesn't seem to work? Get no AOV table
# Try adding a fraction

# 2a. Permanova with fraction added
predmatrix <- predmatrix + 0.01
predperm <- adonis(predmatrix ~ Reeftype, data = predenv)
predperm
predperm$coefficients
predperm$aov.tab

# No real relationship. Weird.


# ANOSIM

# First need distance matrix
pred.dist <- vegdist(predmatrix)
pred.anosim <- with(predenv, anosim(pred.dist, Reeftype))

summary(pred.anosim)
plot(pred.anosim)






