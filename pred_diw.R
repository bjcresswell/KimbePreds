## ---- predReadData

# This script loads Master DOV file and subsets just the predator data

# Housekeeping
rm(list=ls())
dev.off()

# Install relevant packages (gdata, vegan etc)
library(gdata)
library(vegan)
library(readxl)
library(labdsv)
library(tidyverse)

# Set working directory
setwd("~/OneDrive - James Cook University/Ben PhD/Data & analysis/KimbePreds")

# Load data
fish18 <- read_xlsx('~/Dropbox/Bemma Shared/Kimbe Fish Data/DOV Fish Data/Master DOV 18-19 Validated Species.xlsx', 2)
fish19 <- read_xlsx('~/Dropbox/Bemma Shared/Kimbe Fish Data/DOV Fish Data/Master DOV 18-19 Validated Species.xlsx', 4)
fish <- rbind(fish18, fish19)
glimpse(fish)
head(fish)

# Sort out variables
fish[1:10] <- fish[1:10] %>%
  mutate_if(is.character, as.factor)
fish <- fish %>% rename(SiteCode = 'Site Code',
                        SurvYr = 'Survey Year',
                        TID = 'Transect ID',
                        Taxa = 'Concat Name') 

# Filter out just the main predator families
# preds <- fish %>% filter(Family=="Carangidae") # If you just want one family
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


# Check what transects left in
View(predsum <- preds %>% group_by(TID) %>%
  summarise(mean(Number)))
# ..against in fish df
View(fishsum <- fish %>% group_by(TID) %>%
  summarise(mean(Number)))
# So 23 transects over the 2 years without any pred observations. Will need to add these back in with 0 counts

if (!dir.exists('data')) dir.create('data') # Create folder for data if one not present
save(preds, file='data/preds.RData') # Save as RData file
rm(list=ls()) # Tidy up









#### JUNK CODE #####





