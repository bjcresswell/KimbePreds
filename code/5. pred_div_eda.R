## ---- pred.div.EDA

# Author: BJC
# Last edit: 1.7.2021


rm(list=ls())
library(gdata)
library(vegan)
library(readxl)
library(labdsv)
library(tidyverse)
setwd("~/OneDrive - James Cook University/Ben PhD/Data & analysis/KimbePreds")
load('data/preds.RData')
load('data/fish.RData')
preds
fish

# Generate spp counts for both preds...
pred.spec <- preds %>% 
  group_by(TID, Taxa) %>% 
  summarise(Count=sum(Number))
head(pred.spec) # Not enough observations (only 97 transects) so need to merge with an equiv tibble for fish

#.. and fish
fish.spec <- fish %>% 
  group_by(TID, Taxa) %>% 
  summarise(Ignore=sum(Number))
head(fish.spec) # Has 120 rows (all transects)

# Make wide vegan matrix for both fish and preds and merge to get all transects
preddiv <- pred.spec %>%
  pivot_wider(names_from = Taxa, values_from=Count)

fishdiv <- fish.spec %>% 
  pivot_wider(names_from = Taxa, values_from=Ignore)

preddiv
fishdiv

predmatrix <- merge(preddiv, fishdiv[,1], by=c("TID"), all = T) # Merge back together and keep all 120 rows


predmatrix[is.na(predmatrix)] = 0 # Remove NAs in whole df and set to zero (this is the case if cell is empty)
rownames(predmatrix) = predmatrix[,1]
predmatrix <- predmatrix[,-1]
save(predmatrix, file='data/predmatrix.RData')

predmatrix
head(predmatrix)

# Make distance matrix
predmatrix <- as.data.frame(predmatrix)
glimpse(predmatrix)

