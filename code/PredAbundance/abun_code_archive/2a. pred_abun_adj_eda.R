### Adjusted predator numbers ###
# Adjusts for outliers (predominately large schools of Sphyraena qenie)
# Starts with preds and fish dfs and follows pattern of initial EDA

rm(list=ls())
library(tidyverse)
library(artyfarty)
setwd("~/OneDrive - James Cook University/Ben PhD/Data & analysis/KimbePreds")

# Load necessary dfs
load('data/preds.RData')
load('data/fish.RData')

preds.adj <- preds %>% # New df which will contain adjusted entries for large school sizes
  mutate(Number=replace(Number, Number>67, 67))  # Set max school size to same as max for C sexfasc

# Redo the predsum df using adjusted predator numbers
(predsum.adj <- preds.adj %>% group_by(TID) %>%
    summarise(No_Preds =sum(Number))) # Check output in console - in this case 97 rows (transects)
# ..against in fish df
(fishsum <- fish %>% group_by(SiteSurv, SurvCode, SiteCode, Transect, TID) %>%
    summarise(No_Fish =sum(Number))) # For all fish there are 120 rows (transects) - 5T x 12 sites over 2 years
# So 23 transects over the 2 years without any pred observations.

# So need to add these back in with 0 counts for abundance and diversity analysis - janky way:
predsum.adj <- tibble(merge(predsum.adj, fishsum, by="TID", all = T)) %>%  # Merge back together and keep all 120 rows
  replace(is.na(.), 0) %>%                                                 # Convert NAs in whole df to zero
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
  mutate(PredPC = (No_Preds/No_Fish)*100)
predsum.adj
#save(predsum.adj, file='data/predsum_adjusted.RData') # Save as RData file


### Predator statistics by site/survey
pred.adj.ss <- predsum.adj %>% 
  group_by(Reeftype, SiteCode, SurvCode, SiteSurv) %>% 
  summarise_each(funs(mean,sd,se=sd(.)/sqrt(n())), No_Preds)
pred.adj.ss

# Bar plot
ssbar <- ggplot(pred.adj.ss, aes(x=SiteCode, y=mean, fill=SurvCode))+
  geom_bar(stat="identity", position="dodge", color="black")+
  scale_y_continuous(limits = c(0,250), "Mean mesopredator count (±SE)") +
  scale_x_discrete("Site and survey period")+
  #labs(title="Partial mortality weighted scores")+
  scale_fill_viridis_d()+
  theme_scientific()+
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.2, position=position_dodge(.9))+
  theme(legend.key.size = unit(0.4, "cm"))+
  theme(plot.title = element_text(size = 14, face = "bold"),
        legend.title=element_text(size=9), 
        legend.text=element_text(size=8),
        axis.title.y = element_text(size=10, face="bold"),
        axis.title.x = element_text(size=10, face="bold"),
        axis.text = element_text(size=6))
ssbar

# Box plot
ssbox <- ggplot(predsum.adj, aes(x=SiteCode, y=No_Preds, fill=SurvCode))+
  geom_boxplot()+
  scale_y_continuous(limits = c(0,400), "Mean mesopredator count (±SE)") +
  scale_x_discrete("Site and survey period")+
  #labs(title="Partial mortality weighted scores")+
  scale_fill_viridis_d()+
  theme_scientific()+
  theme(legend.key.size = unit(0.4, "cm"))+
  theme(plot.title = element_text(size = 14, face = "bold"),
        legend.title=element_text(size=9), 
        legend.text=element_text(size=8),
        axis.title.y = element_text(size=10, face="bold"),
        axis.title.x = element_text(size=10, face="bold"),
        axis.text = element_text(size=6))
ssbox


