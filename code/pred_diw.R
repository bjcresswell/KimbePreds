## ---- predReadData

# This script loads Master DOV file and subsets just the predator data
# Outputs are:
# 1. fish file - 2018 and 19 merged together 
# 2. preds file - details of all predator observations from all surveys (a subset of the fish file above)
# 3. predsum - a summary file with counts of predators by transect

# Housekeeping + prelims
#rm(list=ls())
#dev.off()
library(readxl)
library(arsenal) # For comparing dataframes
library(tidyverse)
getwd()
#setwd("../")

# Load data - can skip to load.data if already done this stage (~ Line 47)
# Load both survey periods in: 
fish18 <- read_xlsx('~/Dropbox/Bemma Shared/Kimbe Fish Data/DOV Fish Data/Master DOV 18-19 FINAL Validated 2021.xlsx', 2)
fish19 <- read_xlsx('~/Dropbox/Bemma Shared/Kimbe Fish Data/DOV Fish Data/Master DOV 18-19 FINAL Validated 2021.xlsx', 3)

# And combine
fish <- rbind(fish18, fish19)

# Sort out variables
fish[1:10] <- fish[1:10] %>%
  mutate_if(is.character, as.factor)
fish <- fish %>% 
  rename(SiteCode = 'Site Code', # Tidy up column headings
         SurvCode = 'Survey Year',
         TID = 'Transect ID',
         Taxa = 'Concat Name') %>%
  mutate(SiteSurv = factor(paste(SiteCode, SurvCode))) %>% # Don't really need this (can use group_by instead when required)
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
  #mutate(TID2 = paste(Site, Transect)) %>% 
  mutate(Length = Length/10)

#save(fish, file='data/fish.RData') # Save as RData file
#load('data/fish.Rdata') # Should be 6242 observations

# Now we have a single df with all fish survey transects in from both periods and ready to go
# We  now want to wrangle out only the predators from this data frame 
# Can do by family (known predator taxa)
# Could also wrangle out by trophic level (numerical score available from Mermaid which in turn extracts from fishbase)
# Or max body size, piscivory etc, or combinations of all of these - but we will need information in the df on these parameters
# Can get from fishbase via the Mermad dataset
# So can merge Mermaid data with this df and then start filtering

# Load Mermaid parameter detail csv:
mermaid <- read_csv('../data/fish-species.csv') # Contains all species on mermaid database
# Some initial wrangling on the mermaid df
mermaid <- mermaid %>% 
  mutate_if(is.character, as.factor) %>% # Char -> fact variables
  mutate(Taxa = factor(paste(Genus, Species))) # New column ("Taxa") to allow merge with fish df

# Can also check against Sandin List
sandin <-  read_xlsx('../data/Sandin Fish List.xlsx', 1)
# And some additional wrangling
sandin <- sandin %>% 
  mutate_if(is.character, as.factor)


### The next section examines differences in assignment of taxa as predators by Mermaid vs Sandin
# Need to do this to make sure we don't miss any predators out of the final analysis

# Extract the unique species observations from each df - Kimbe, Mermaid and Sandin

# Kimbe fish data 
fishtaxa <- fish %>% 
  group_by(Taxa) %>% 
  summarise() %>% 
  arrange(Taxa) # (so there are 283 individual fish taxa observed across the whole Kimbe surey)

## Mermaid database (Mermaid is global and comprehensive so need to filter out all the other parameters)
mermaidpreds <- mermaid %>% # New df with just the predators
  group_by(Family, Taxa, `Trophic Group`) %>% # Want to keep all this information in
  filter(str_detect(`Regions`, str_c(c('Indo-Pacific'), collapse="|"))) %>%  # Restrict to Indo-Pacific
  filter(str_detect(`Trophic Group`, str_c(c('pisc'), collapse="|"))) %>% # Filter out just piscivores
  summarise() %>% # One entry per taxon
  arrange(Taxa) 
mermaidpreds # So 487 individual possible piscivorous fish taxa identified in our bioregion according to Mermaid/Fishbase

# Sandin database - after checking this is ignored from the final list (as it adds nothing) but the code left in for transparency
# See note below *
# Don't need to check regions as we know this database is restricted to the USA Pacific so likely will be missing several Indo-Pac spp
#sandinpreds <- sandin %>% 
#  group_by(Taxa, `Trophic Group`) %>% 
#  filter(str_detect(`Trophic Group`, str_c(c('isci'), collapse="|"))) %>% # Filter out just piscivores
#  summarise() %>% 
#  arrange(Taxa) # 131 spp identified

# If merged only produces 95 observations (spp) - lack of Indo cross over most likely
#mersandpreds <- merge(mermaidpreds, sandinpreds, by = "Taxa")
#mersandpreds # So 131-95=36 taxa in Sandin that aren't in Mermaid. If these are just C&E Pacific and therefore not in our datset we can ignore

# Let's investigate these differences and see if we can ignore the Sandin data
#mersanddiff <- summary(comparedf(mermaidpreds, sandinpreds, by = "Taxa"))
#difftable <- mersanddiff$obs.table %>% 
#  filter(version=='y') # A table with entries that are in Sandin but not Mermaid (36) - it's short so we can manually check
# The only taxa included is L. ehrenbergii (excluded from Merpreds as it's listed as an invertivore)
# * The key point to note here is: are there any spp present in the "y" component of the obs.table diff that are also in our dataset
# (and not captured using the Family filtering step below)??  If not then we can pretty much ignore the Sandin df from here on 
# as we will have included all piscivorous taxa from this database now.

# Can also examine differences between our list of taxa and the combined list - have 
#sandfishpredtaxa <- merge(fishtaxa, mersandpreds, by = "Taxa")
#merfishpredtaxa <- merge(fishtaxa, mermaidpreds, by = "Taxa")
#mersandfishpredtaxa <- full_join(sandfishpredtaxa, merfishpredtaxa, by = "Taxa")

#mersandfishdiff <- summary(comparedf(fishpredtaxa, mersandpreds, by = "Taxa"))
#mermaidfishdiff <- summary(comparedf(fishpredtaxa, mermaidpreds, by = "Taxa"))
#(mersandfishdiff$obs.table) # 95 spp in that are not in our dataset - can conduct manual check on this
#(mermaidfishdiff$obs.table) # Lots of spp from our dataset missing - will be captured during the family filtering step below


### Now can filter out our own df for Kimbe Bay predators

## First step - by family:
# First need to merge the fish df with the mermaid df so that we can do biomass and other wrangling/calculations later
merfish <- merge(fish, mermaid, by = "Taxa")

# Tweak some column headings
merfish<-merfish %>% 
  rename("Family" = "Family.x",
         "Genus" = "Genus.x",
         "Species" = "Species.x") %>% 
    dplyr::select(!(c(`Family.y`, `Genus.y`, `Species.y`))) # and get rid of duplicates

save(merfish, file='../data/merfish.RData') # Save as RData file

# Next, check all the families present:
summary(merfish$Family)

# Can also check that taxa are assigned to correct families:
(famcheck <- merfish %>% 
  group_by(Family, Genus) %>% 
  summarise())

# There are 9 families we initially will filter out into a new preds dataframe:
preds <- merfish %>% 
  filter(Family=="Carangidae" | # Trevallies 
           Family=="Cirrhitidae" | # Hawkfishes
           Family=="Scombridae" | # Mackerels/tunas
           Family== "Lethrinidae" | # Emperors
           Family == "Lutjanidae" | # Snappers
           Family == "Haemulidae" | # Grunts/sweetlips
           Family == "Sphyraenidae" | # Barracudas
           Family == "Serranidae"| # Groupers (also contains pseudanthias - will get rid of these below)
           Family == "Carcharhinidae") %>% # Sharks (only one family observed in KB)
  droplevels()
head(preds)

# Now need to filter out any outlier, non-predator taxa that have snuck in:
preds <- preds %>%
  filter(Genus !="Pseudanthias") #

# Checks
summary(preds$`Trophic Level`) # Check spp under 3.4 (Pinjalo lewisi) - check on FishBase and Allen et al - probably feed on small fishes.
summary(preds$`Max Length (cm)`) # Minimum max length 22cm
preds %>% group_by(Taxa) %>% summarise() # 56 taxa total based on the family filter


# What about predators from other families, based on trophic level, size etc
# Run the below code and see if we need to incorporate anything further into our preds df

library(stringr)

# Create filters:

# 1. Large bodied piscivores only 
preds_mermaid1 <- mermaid %>% 
  filter(str_detect(`Regions`, str_c(c('Indo-Pacific'), collapse="|"))) %>%  # Restrict to I-P
  filter(str_detect(`Trophic Group`, str_c(c('pisc'), collapse="|"))) %>%  # Filter out piscivores
  filter(`Max Length (cm)` >=30) # And only those that reach at least 30cm

# 2. Only taxa above certain trophic level
preds_mermaid2 <- mermaid %>% 
  filter(str_detect(`Regions`, str_c(c('Indo-Pacific'), collapse="|"))) %>%  #  Restrict to I-P
  filter(`Trophic Level` >=3.9) # Filter out by high trophic level


# See if any of these taxa are in our fish dataset (currently these are just lists from the Mermaid database)

# Following code pulls out only the predators from our surveys that relate to the lists created in "preds_mermaidX"
preds1 <- merge(fish, preds_mermaid1, by = "Taxa") # Pisc only - 397 observations
preds2 <- merge(fish, preds_mermaid2, by = "Taxa") # >=TP of 3.9 - 477 observations

unique(preds1$Taxa) # 41 taxa meet this criteria in our dataset
unique(preds2$Taxa) # 48 taxa meet this criteria in our dataset

combinedpredlist <- bind_rows(preds1, preds2) %>% 
  group_by(Taxa) %>% 
  summarise() # 55 taxa combined

predscheck1 <- summary(comparedf(preds, preds1, by = "Taxa"))
#View(predscheck1$obs.table)
check1 <- predscheck1$obs.table %>% 
  filter(version == 'y') %>% # Shows we have 6 extra taxa in the preds1 df than in the preds df
  group_by(Taxa) %>% 
  summarise() %>% 
  merge(mermaid, by = "Taxa") %>% 
  dplyr::select(`Taxa`, `Trophic Group`, `Trophic Level`, `Max Length (cm)`)


predscheck2 <- summary(comparedf(preds, preds2, by = "Taxa"))
#View(predscheck2$obs.table)
check2 <- predscheck2$obs.table %>% # Also 6 extra taxa but 2 of these are tiny Labroides spp (presumably a result of them consuming parasites on higher TP members)
  filter(version == 'y') %>% 
  group_by(Taxa) %>% 
  summarise() %>% 
  merge(mermaid, by = "Taxa") %>% 
  dplyr::select(`Taxa`, `Trophic Group`, `Trophic Level`, `Max Length (cm)`)


# So: going to incorporate the large bodied piscivores () plus napoleon wrasse - 7 extra taxa total
# Epibulus insidiator       piscivore          4.01              54
# Myripristis botche        piscivore          4.00              30
# Oxycheilinus digramma     piscivore          3.70              40
# Platax teira              piscivore          3.95              70
# Priacanthus hamrur        piscivore          3.82              45
# Sargocentron spiniferum   piscivore          3.80              51
# Cheilinus undulatus       inverti-mobile     3.99              229


# Create df with just large bodied piscivores (ie check1 with a max-length filter)...
# ..and the napoleon wrasse (from check2 with the same filter)
others <- bind_rows(check1, check2) %>% 
  filter(`Max Length (cm)` >= 30) %>% # Need to include this term again as there was no body size filter on the check2 df
  distinct() # Removes duplicate rows

# Use this list to extract matching entries from the fish data set
otherpreds <- merge(merfish, others[1], by = "Taxa") # only merge with the taxa column of 'others' so that resulting df same width as preds :)

# And combine with the existing preds df to 
preds <- bind_rows(preds, otherpreds)

# Can double check trophic group
preds %>% 
  distinct(`Trophic Group`)
  
  
# Can interrogate deeper to see what taxa comprise the invertivore group:
preds %>% 
  group_by(Taxa) %>% 
  filter(`Trophic Group`== 'invertivore-mobile') %>% 
  summarise()

#if (!dir.exists('data')) dir.create('data') # Create folder for data if one not present
save(preds, file='../data/preds.RData') # Save as RData file
#rm(list=ls())
#load(file='data/preds.RData') # Save as RData file



