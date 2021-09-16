## ---- predReadData

# Created 1.7.2021 BJC
# Last edit 1.7.2021 BJC

# Based on abundance diw script

# This script loads Master DOV file and subsets just the predator biomass data
# Outputs are:
# 1. fish file - 2018 and 19 merged together 
# 2. preds file - details of all predator observations from all surveys (a subset of the fish file above)
# 3. predsum - a summary file with counts of predators by transect

# Housekeeping + prelims
#rm(list=ls())
#dev.off()
library(readxl)
library(tidyverse)
#getwd()
#setwd("~/OneDrive - James Cook University/Ben PhD/Data & analysis/KimbePreds")

# Load data - can skip to load.data if already done this stage (~ Line 47)
fish18 <- read_xlsx('~/Dropbox/Bemma Shared/Kimbe Fish Data/DOV Fish Data/Master DOV 18-19 FINAL Validated 2021.xlsx', 2)
fish19 <- read_xlsx('~/Dropbox/Bemma Shared/Kimbe Fish Data/DOV Fish Data/Master DOV 18-19 FINAL Validated 2021.xlsx', 3)



fish <- rbind(fish18, fish19)
glimpse(fish)
head(fish)

# Sort out variables
fish[1:10] <- fish[1:10] %>%
  mutate_if(is.character, as.factor)
fish <- fish %>% rename(SiteCode = 'Site Code',
                        SurvCode = 'Survey Year',
                        TID = 'Transect ID',
                        Taxa = 'Concat Name') %>%
  mutate(SiteSurv = factor(paste(SiteCode, SurvCode)))

glimpse(fish)

#save(fish, file='data/fish.RData') # Save as RData file
# To load directly see below


# Filter out just the predators

# a.If you just want one family:
# preds <- fish %>% filter(Family=="Carangidae") 


# Or, b. If you want all the preds present:
# First, view all the families present:
summary(fish$Family)

preds <- fish %>%
  filter(Family=="Carangidae" |
           Family=="Scombridae" |
           Family== "Lethrinidae" |
           Family == "Lutjanidae" |
           Family == "Sphyraenidae" |
           Family == "Serranidae"|
           Family == "Carcharhinidae")
head(preds)

# Now filter out any strange taxa that have snuck in:
preds <- preds %>%
  filter(Genus !="Pseudanthias")#

#if (!dir.exists('data')) dir.create('data') # Create folder for data if one not present
#save(preds, file='data/preds.RData') # Save as RData file


# If skipping above initial data import step (due to RData files already made) - can start here:
#load('data/fish.Rdata')
#load(file='data/preds.RData')

# Check what transects left in once filtered by predators:
(predsum <- preds %>% group_by(TID) %>%
  summarise(No_Preds =sum(Number))) # Check output in console - in this case 97 rows (transects)

# ..against in fish df
(fishsum <- fish %>% group_by(SiteSurv, SurvCode, SiteCode, Transect, TID) %>%
  summarise(No_Fish =sum(Number))) # For all fish there are 120 rows (transects) - 5T x 12 sites over 2 years
# So 23 transects over the 2 years without any pred observations.

# So need to add these back in with 0 counts for abundance and diversity analysis - janky way:
predsum <- tibble(merge(predsum, fishsum, by="TID", all = T)) %>%  # Merge back together and keep all 120 rows
  replace(is.na(.), 0) %>%                                            # Remove NAs in whole df and set to zero
  mutate(Reeftype = case_when(grepl("BRAD", `SiteCode`) ~ "Pinnacle",      # Assign reef type status
                            grepl("JOEL", `SiteCode`) ~ "Pinnacle",
                            grepl("KBOM", `SiteCode`) ~ "Pinnacle",
                            grepl("INGL", `SiteCode`) ~ "Pinnacle",
                            grepl("LADI", `SiteCode`) ~ "Nearshore",
                            grepl("MADA", `SiteCode`) ~ "Nearshore",
                            grepl("SUSA", `SiteCode`) ~ "Nearshore",
                            grepl("DON", `SiteCode`) ~ "Nearshore",
                            grepl("EMA", `SiteCode`) ~ "Offshore",
                            grepl("HOG", `SiteCode`) ~ "Offshore",
                            grepl("OTT", `SiteCode`) ~ "Offshore",
                            grepl("KIS", `SiteCode`) ~ "Offshore")) %>% 
  mutate(Reeftype = factor(Reeftype, levels= c("Pinnacle", "Offshore", "Nearshore"))) %>% # Organise by reef type
  mutate(SiteCode = factor(SiteCode, levels= c("BRAD", "JOEL", "INGL", "KBOM",   # Pinnacles first
                                               "EMA",  "HOG", "KIS", "OTT",      # then offshore
                                               "DON", "LADI", "MADA", "SUSA"))) %>%   # then nearshore
  mutate(PredPCZ = (No_Preds/No_Fish)) %>% # Generates predators as proportion of whole fish assemblage retains 0s
  mutate(PredPC = (No_Preds/No_Fish)+0.00000001) %>% # Generates predators as proportion of whole fish assemblage - may use instead of transformation later on. Adds small amount to avoid 0s for GLM 
  mutate(No_PredsZ = No_Preds) %>% # Retain column with raw data (inc 0 observations)
  mutate(No_Preds = (No_Preds+1)) # Have added 1 predator to every transect for modeling purposes (see Script 4). Need to check this is reasonable
 
predsum
#save(predsum, file='data/predsum.RData') # Save as RData file
#rm(list=ls()) # Tidy up

