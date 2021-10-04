# Abundance diw script

# Last edit 31 Aug 2021 BJC


rm(list=ls())
# Load data
setwd("~/OneDrive - James Cook University/Ben PhD/Data & analysis/KimbePreds")
library(readxl)
library(tidyverse)


# If skipping above initial data import step (due to RData files already made) - can start here:
load('data/fish.Rdata')
load(file='data/preds.RData')

# Check what transects left in once filtered by predators:
(predsum <- preds %>% group_by(TID) %>%
    summarise(No_Preds =sum(Number))) # Check output in console - in this case 99 rows (transects)

# ..against in fish df
(fishsum <- fish %>% group_by(Reeftype, SiteSurv, SurvCode, Site, SiteCode, Transect, TID) %>%
    summarise(No_Fish =sum(Number))) # For all fish there are 120 rows (transects) - 5T x 12 sites over 2 years
# So 22 transects over the 2 years without any pred observations.

# So need to add these back in with 0 counts for abundance and diversity analysis - janky way:
predsum <- tibble(merge(predsum, fishsum, by="TID", all = T)) %>%  # Merge back together and keep all 120 rows
  replace(is.na(.), 0) %>%                                            # Remove NAs in whole df and set to zero
  mutate(PredPC = (No_Preds/No_Fish)) # Generates predators as proportion of whole fish assemblage - just used for EDA


# Did have the below code tacked on when experimenting with different analyses - settled on using an offset during the glmm.
#mutate(PredPC = (No_Preds/No_Fish)+0.00000001) %>% # Generates predators as proportion of whole fish assemblage - may use instead of transformation later on. Adds small amount to avoid 0s for GLM 
#mutate(No_PredsZ = No_Preds) %>% # Retain column with raw data (inc 0 observations)
#mutate(No_Preds = (No_Preds+1)) # Have added 1 predator to every transect for modeling purposes (see Script 4). Need to check this is reasonable

predsum
#save(predsum, file='data/predsum.RData') # Save as RData file
#rm(list=ls()) # Tidy up







