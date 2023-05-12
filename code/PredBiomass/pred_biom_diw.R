## ---- predBiomdiw

# Script to wrangle biomass data from dataset
# Created 1.7.2021 BJC
# Last edit 31.8.2021 BJC


#rm(list=ls())
graphics.off()
library(readxl)
library(tidyverse)
#library(artyfarty)
getwd()
setwd("~/OneDrive - James Cook University/Ben PhD/Data & analysis/KimbePreds/code/PredBiomass")

# Load data: preds
load(file='../../data/preds.RData')

#preds <- preds %>% 
 # mutate(Length = Length/10)

# Biomass calculations
merpreds<-preds %>% 
  mutate(Individ_pred_biomass_g = (`Biomass Constant A`*(Length)^`Biomass Constant B`)) %>% # Incorporate growth coefficients to calculate biomass per individual fish (Fulton's model: https://www.worldcat.org/title/rate-of-growth-of-sea-fishes/oclc/320852829)
  mutate(Obs_pred_biomass_g = Number*Individ_pred_biomass_g) %>% # Biomass in grams for each observation (if there's more than one individ)
  mutate(Individ_pred_biomass_kg = Individ_pred_biomass_g/1000) %>% # Convert individual measurements to kg
  mutate(Obs_pred_biomass_kg = Obs_pred_biomass_g/1000) # Convert group measurements to kg

# Sum up to get biomass by transect (need to do g and kg separately and then merge back together)
merpredsum <- merpreds %>% 
  group_by(TID, Reeftype, SurvCode, Site, Transect) %>%
  summarise(Transect_pred_biomass_g =sum(Obs_pred_biomass_g)) %>% 
  mutate(Transect_pred_biomass_kg =sum(Transect_pred_biomass_g/1000))

# Now we have the 99 transects issue again...

# If we merge back with the predsum df from the abundance diw script then we can get back to 120 transects
# This is why we needed to leave TID in earlier (can't merge using group_by etc)

# Load predsum
load(file='../../data/predsum.RData')
merpredsum
predsum

# And merge (g)
merpredsum <- merge(predsum[c(1,3,4,5,6,7,8)], merpredsum[c(1,6,7)], by='TID', all = T) %>% 
  replace(is.na(.), 0)



### To create offset data for modeling later need to conduct biomass calculations for whole fish assemblage
# Start with merfish

load(file='../../data/merfish.RData')

# Biomass calculations
merfish<-merfish %>% 
  mutate(Individ_fish_biomass_g = (`Biomass Constant A`*(Length)^`Biomass Constant B`)) %>% # Incorporate growth coefficients to calculate biomass per individual fish
  mutate(Obs_fish_biomass_g = Number*Individ_fish_biomass_g) %>% # Biomass for each observation (if there's more than one individ)
  mutate(Individ_fish_biomass_kg = Individ_fish_biomass_g/1000) %>% # Convert individual measurements to kg
  mutate(Obs_fish_biomass_kg = Obs_fish_biomass_g/1000) # Convert group measurements to kg


# Sum up
merfishsum <- merfish %>% 
  group_by(TID, Reeftype, SurvCode, Site, Transect) %>%
  summarise(Transect_fish_biomass_g =sum(Obs_fish_biomass_g)) %>% 
  mutate(Transect_fish_biomass_kg = Transect_fish_biomass_g/1000)

# Combine
merpredsum <- bind_cols(merpredsum, merfishsum[c(6,7)])


# Save
save(merpredsum, file = "../../data/merpredsum.RData")
