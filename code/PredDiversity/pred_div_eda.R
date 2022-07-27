## ---- pred.div.EDA

# Author: BJC
# Last edit: 5 Jul 2022 BJC -> added chunk to explore P/A of families

rm(list=ls())
library(gridExtra)
library(vegan)
library(readxl)
library(labdsv)
library(tidyverse)
#getwd()
#setwd("~/OneDrive - James Cook University/Ben PhD/Data & analysis/KimbePreds")

# Histograms
# Species richness
(srhist <- ggplot(preddiv, aes(x=SpecNo)) + 
  geom_histogram(binwidth=1, color="black", fill="grey")) # Count data and looks poisson distributed
# Shannon
(shhist <- ggplot(preddiv, aes(x=Shannon)) + 
  geom_histogram(binwidth=0.1, color="black", fill="grey")) # Possibly bimodal??
# Simpson - not used
#sihist <- ggplot(data=subset(preddiv, Reeftype=='Nearshore'), aes(x=Simpson)) + 
#  geom_histogram(binwidth=0.1, color="black", fill="grey") # No idea...

# Boxplots - by reeftype
# Richness
srbox <- ggplot(preddiv, aes(x=Reeftype, y=SpecNo))+
  geom_boxplot()
# Shannon
shbox <- ggplot(preddiv, aes(x=Reeftype, y=Shannon))+
  geom_boxplot()
# Simpson - not using as penalises highly abundant communities. See Ch 23 in Krebs

grid.arrange(srhist, shhist, srbox, shbox, ncol=2)



# Family presence/absence checks

library(magrittr)
preds %$%  
  summary(Family)

# 1.Carangids (P/O/N)
preds %>% 
  filter(Family == "Carangidae") %>% #%$%  
  #summary(Reeftype)
  filter(Reeftype != 'Pinnacle')
# mostly on pinnacles (five total not on pinnacles and these = Carangoides and Elagatis)

# 2. Reef sharks (P)
preds %>% 
  filter(Family == "Carcharhinidae")  #%$% 
  summary(Reeftype)
# 6 only present on pinnacles

# 3. Hawkfishes (P)
preds %>% 
  filter(Family == "Cirrhitidae") %$%  
  summary(Reeftype)
# only on pinnacles

# 4. Sweetlips (P/N)
preds %>% 
  filter(Family == "Haemulidae") %$%  
  summary(Reeftype)
# 1 pinnacle and 1 nearshore

# 5. Emperors (P/O/N)
preds %>% 
  filter(Family == "Lethrinidae") %$%  
  summary(Reeftype)
# All reef types

# 6. Snappers (P/O/N)
preds %>% 
  filter(Family == "Lutjanidae") %$%  
  summary(Reeftype)
# All reef types

# 7. Mackerels/tunas (P/O)
preds %>% 
  filter(Family == "Scombridae") %$%  
  summary(Reeftype)
# Pinnacles + offshore

# 8. Groupers (P/O/N)
preds %>% 
  filter(Family == "Serranidae") %$%  
  summary(Reeftype)
# All reef types

# 9. Barracudas (P)
preds %>% 
  filter(Family == "Sphyraenidae") %$%  
  summary(Reeftype)

# 10. Batfish (P/O)
preds %>% 
  filter(Family == "Ephippidae")# %$%  
  summary(Reeftype)
# Pinnacles + offshore

# 11. Soldierfish (P) (nocturnal)
preds %>% 
  filter(Family == "Holocentridae") #%$%  
  summary(Reeftype)
# BRAD only

# 12. Wrasses (P/O/N)
preds %>% 
  filter(Family == "Labridae") %$%  
  summary(Reeftype)
# All reef types


# 13. Bigeyes (P) (nocturnal)
preds %>% 
  filter(Family == "Priacanthidae") #%$%  
  #summary(Reeftype)
# BRAD only
  
# So in summary:

## All reeftypes: Carangids, Emperors, Snappers, Groupers and Wrasses
## Nearhore only: None
## Offshore only: None
## Pinnacle only: sharks, hawkfishes, barracudas, soldierfishes, and bigeyes (last two nocturnal)




  





