# Venn diagram code
# Created 13 Aug 2021
# Last edit 9 Sept 2021

# Setwd
setwd("/Users/bjcresswell/OneDrive - James Cook University/Ben PhD/Data & analysis/KimbePreds/code/PredDiversity")
getwd()


# Packages
library(VennDiagram)
library(plyr)
library(tidyverse)
library(ggvenn)


# Data
# Need to start with the preds data (has the a column for reef type as well as taxa)
#rm(list=ls())
load(file='../../data/preds.RData')

# How many species total?
totalpredspp <- preds %>% 
  distinct(Taxa)
# 63 pred sp in Kimbe Bay

# See how this breaks down
unique(preds$Taxa[preds$Reeftype=='Pinnacle']) # 53 spp total
unique(preds$Taxa[preds$Reeftype=='Nearshore']) # 22 spp total
unique(preds$Taxa[preds$Reeftype=='Offshore']) # 19 spp total

# Prep data - make a tbl for each reef type and spp present there
P <- as_tibble(unique(preds$Taxa[preds$Reeftype=='Pinnacle'])) %>% 
  dplyr::rename(Taxa=value) %>% 
  mutate(`Reef type`="Pinnacle") %>% 
  mutate(Pinnacle="Present")

N <-  as_tibble(unique(preds$Taxa[preds$Reeftype=='Nearshore'])) %>% 
  dplyr::rename(Taxa=value) %>% 
  mutate(`Reef type`="Nearshore") %>% 
  mutate(Nearshore="Present")

O <-  as_tibble(unique(preds$Taxa[preds$Reeftype=='Offshore'])) %>% 
  dplyr::rename(Taxa=value) %>%
  mutate(`Reef type`="Offshore") %>% 
  mutate(Offshore="Present")

P
N
O

# Find out what spp there are in common between reef types
NO <- merge(N,O, by="Taxa") # 10 spp in common between nearshore and offshore
PO <- merge(P,O, by="Taxa") # 13 spp in common between pinnacle and offshore 
NP <- merge(N,P, by="Taxa") # 14 spp in common between nearshore and pinnacle

# Common across all reef types
commontaxa <- merge(NO[1],P[1], by="Taxa") %>% # Just 6 spp in common between all rt
  #rename(Taxa=value) %>% 
  mutate(`Reef type`=factor("All"))
#write_xlsx(commontaxa, "../../output/rtables/commontaxa.xlsx")

# Df with all taxa and presence vs absence
alltaxa <- tibble(join_all(list(totalpredspp, P[-2], N[-2], O[-2]), by="Taxa")) %>%
  replace(is.na(.), "Absent")

#write_xlsx(alltaxa, "../../output/rtables/taxa_presence_absence.xlsx")
#alltaxa <- read_excel("../../output/rtables/taxa_presence_absence.xlsx")

# To find unique spp for each habitat
# First find pairwise common taxa
NandO <- full_join(N, O, "Taxa") # 31 (22+19-10) combined spp between nearshore and offshore (so 63-31=32 spp only on pinnacles)
NandP <- full_join(N, P, "Taxa") # 61 (22+53-14) combined spp between nearshore and pinnacle (so 63-61=2 spp only on offshore)
PandO <- full_join(P, O, "Taxa") # 59 (53+19-13) combined spp between offshore and pinnacle (so 63-59=4 spp only on nearshore)

# Then anti_join to extract unique taxa - may as well make it a df
Ponly <- anti_join(P, NandO, "Taxa") %>% 
  mutate(`Reef type`=factor("Pinnacle")) # 32 spp

Nonly <- anti_join(N, PandO, "Taxa") %>% # 4 spp
  mutate(`Reef type`=factor("Nearshore")) 

Oonly <- anti_join(O, NandP, "Taxa") %>% # 2 spp
  mutate(`Reef type`=factor("Offshore"))

# Don't really need in one df but here it is
noncommontaxa <- bind_rows(Ponly, Nonly, Oonly) %>% 
  replace(is.na(.), "Absent")


  
  
  
# Make venn diagram
  
# First need to extract the vectors from the dfs
Pinnacle <- factor(P$Taxa)
Nearshore <- factor(N$Taxa)
Offshore <- factor(O$Taxa)



spp_list <- list(`Pinnacle (53)` = factor(P$Taxa),
          `Nearshore (22)` = factor(N$Taxa),
          `Offshore (19)` = factor(O$Taxa))


predvenn <- ggvenn(spp_list, show_percentage = FALSE, 
       fill_color = c("#35978F", "#DFC27D", "#436EEE"),
       fill_alpha = 0.9,
       stroke_color = "black",
       stroke_alpha = 1,
       stroke_size = 0.6,
       stroke_linetype = "solid",
       set_name_color = "black",
       text_color = "black",
       set_name_size = 3,
       text_size = 3)

predvenn

#ggsave(predvenn, filename= '../../output/rfigures/predvenn.pdf', width=4,  height=4, dpi = 1000 )

       





