# Predator diversity diw script #

# Last edit: 14 Sep 2021 BJC - just a check over - everything working

# Script generates one big dataframe preddiv from which it's easy to extract

# 1. Species count matrix (with or without columns for reeftype, transect etc)
# 2. Diversity metrics dataframe (with Spp richness and Shannon-Weiner H scores)

# Housekeeping
getwd()
setwd("~/OneDrive - James Cook University/Ben PhD/Data & analysis/KimbePreds/code/PredDiversity") # Have to specify this so that the script works embedded within the Rmd
#rm(list=ls())


library(MASS)
library(ggdendro)
library(vegan)
library(ggvegan) 
library(tidyverse)


# Import raw predator and fish data 
load('../../data/preds.RData')
load('../../data/fish.RData')

# Examine
head(preds)
glimpse(preds)
str(preds)
summary(preds)

# Summarise spp counts by transect for both preds...
pred.count <- preds %>% 
  group_by(Family, TID, Taxa) %>% # Group including family for use in taxa_nos analysis later
  dplyr::summarise(Count=sum(Number))
head(pred.count) 

#save(pred.count, file='data/pred.count.RData')

#.. and fish
fish.count <- fish %>% 
  group_by(TID, Taxa) %>% 
  summarise(Ignore=sum(Number))
head(fish.count) # Observations of all fish species (numerous entries per transect - hence 2644 rows)

# Make wide vegan matrix for both fish and preds and merge to get all transects
pred.spec <- pred.count[2:4] %>%
  pivot_wider(names_from = Taxa, values_from=Count)

pred.spec # Only has 99 rows - as we know, some transects contained zero predator fishes

# Should be able to fix with complete() function from tidyr package but I can't work it out at the moment:
# eg. pred.spec2 <- pred.spec %>% complete(TID)
#complete(TID, nesting(Family), fill = list(Count = 0))

fish.spec <- fish.count %>% 
  pivot_wider(names_from = Taxa, values_from=Ignore)
fish.spec # Has 120 rows, ie 1 per transect

# Merge back together, keep all 120 rows and add back in necessary information
preddiv <- merge(pred.spec, fish.spec[,1], by=c("TID"), all = T) %>% 
  replace(is.na(.), 0) %>%  # Get rid of NAs
  tibble::column_to_rownames("TID") # Get rid of TID column so the next 3 lines will run

preddiv <- preddiv %>% 
  mutate(SpecNo = specnumber(preddiv)) %>% 
  mutate(Shannon = diversity(preddiv)) %>% 
  mutate(Simpson = diversity(preddiv, index = "simpson")) %>% # Just generating Simpson out of interest - someone bound to ask about it
  rownames_to_column("TID") %>% # Put back TID column
  mutate(Reeftype = factor(case_when(grepl("BRAD", `TID`) ~ "Pinnacle",      # Assign reef type status
                                     grepl("JOEL", `TID`) ~ "Pinnacle",
                                     grepl("KBOM", `TID`) ~ "Pinnacle",
                                     grepl("INGL", `TID`) ~ "Pinnacle",
                                     grepl("LADI", `TID`) ~ "Nearshore",
                                     grepl("MADA", `TID`) ~ "Nearshore",
                                     grepl("SUSA", `TID`) ~ "Nearshore",
                                     grepl("DON", `TID`) ~ "Nearshore",
                                     grepl("EMA", `TID`) ~ "Offshore",
                                     grepl("HOG", `TID`) ~ "Offshore",
                                     grepl("OTT", `TID`) ~ "Offshore",
                                     grepl("KIS", `TID`) ~ "Offshore"))) %>% 
  mutate(Reeftype = factor(Reeftype, levels= c("Pinnacle", "Offshore", "Nearshore"))) %>% # Organise by reef type
  mutate(SiteCode = factor(case_when(grepl("BRAD", `TID`) ~ "BRAD",      # Assign site name
                                     grepl("JOEL", `TID`) ~ "JOEL",
                                     grepl("KBOM", `TID`) ~ "KBOM",
                                     grepl("INGL", `TID`) ~ "INGL",
                                     grepl("LADI", `TID`) ~ "LADI",
                                     grepl("MADA", `TID`) ~ "MADA",
                                     grepl("SUSA", `TID`) ~ "SUSA",
                                     grepl("DON", `TID`) ~ "DON",
                                     grepl("EMA", `TID`) ~ "EMA",
                                     grepl("HOG", `TID`) ~ "HOG",
                                     grepl("OTT", `TID`) ~ "OTT",
                                     grepl("KIS", `TID`) ~ "KIIS"))) %>%
  mutate(SiteCode = factor(SiteCode, levels= c("BRAD", "JOEL", "INGL", "KBOM",   # Pinnacles first
                                               "EMA",  "HOG", "KIS", "OTT",      # then offshore
                                               "DON", "LADI", "MADA", "SUSA"))) %>% 
  mutate(Site = factor(case_when(grepl("BRAD", `TID`) ~ "Bradford Shoals",      # Assign site name
                                 grepl("JOEL", `TID`) ~ "Joels",
                                 grepl("KBOM", `TID`) ~ "Kimbe Bommie",
                                 grepl("INGL", `TID`) ~ "Inglis Shoals",
                                 grepl("LADI", `TID`) ~ "Lady Di",
                                 grepl("MADA", `TID`) ~ "Madaro",
                                 grepl("SUSA", `TID`) ~ "Susans",
                                 grepl("DON", `TID`) ~ "Donnas",
                                 grepl("EMA", `TID`) ~ "Ema",
                                 grepl("HOG", `TID`) ~ "Hogu",
                                 grepl("OTT", `TID`) ~ "Otto",
                                 grepl("KIS", `TID`) ~ "Kimbe Island"))) %>% 
  mutate(SurvCode = factor(case_when(grepl("1018", `TID`) ~ "2018",      # Assign surv code
                                     grepl("0319", `TID`) ~ "2019"))) %>% 
  mutate(Transect = factor(case_when(grepl("T1", `TID`) ~ "T1",      # Assign site name
                                     grepl("T2", `TID`) ~ "T2",
                                     grepl("T3", `TID`) ~ "T3",
                                     grepl("T4", `TID`) ~ "T4",
                                     grepl("T5", `TID`) ~ "T5"))) %>% 
  mutate(Site_Tran = factor(paste(SiteCode, Transect)))
  

head(preddiv)
# Columns 2-64 are the taxa
# Columns 65-67 are the metrics (richness, Shannon, Simpson)
# Columns 68-73 are the survey parameters
# Reorder for sensibility
preddiv <- preddiv[c(1,68:73,65:67,2:64)]  

## Notes on indices selection: 
# Simpson index will be excluded for further analysis here because:
# All the 0 score transects by default get a Simpson score of 1
# So when analysed in a model it will skew the results (unless you manually remove - don't see the point)
# Also Shannon "rewards for rarity" which is what I actually want to delve into

# If you want to see why Simpson not good in this instance can run this plot:
ggplot(preddiv, aes(y=Simpson,  x=Shannon))+
  geom_point()
ggplot(preddiv, aes(x=Reeftype, y=Simpson))+
  geom_boxplot()

# Histograms
# Simpson across whole data set
ggplot(preddiv, aes(x=Simpson)) + 
  geom_histogram(binwidth=0.1, color="black", fill="grey")
# Simpson just for nearshore sites
ggplot(data=subset(preddiv, Reeftype=='Nearshore'), aes(x=Simpson)) + 
  geom_histogram(binwidth=0.1, color="black", fill="grey")

# Compare to Shannon...
ggplot(preddiv, aes(x=Shannon)) + 
  geom_histogram(binwidth=0.1, color="black", fill="grey")
ggplot(data=subset(preddiv, Reeftype=='Nearshore'), aes(x=Shannon)) + 
  geom_histogram(binwidth=0.1, color="black", fill="grey")


# Save for use in eda and modeling
save(preddiv, file='../../data/preddiv.RData')
load(file='../../data/preddiv.RData')
