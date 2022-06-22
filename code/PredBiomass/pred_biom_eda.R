## ---- pred.biomass.EDA

### Predator biomass statistics by reeftype/site/survey
# Last edit 7.7.2021  BJC

getwd()

library(tidyverse)
library(artyfarty)

# Start with merpredsum df
load('data/merpredsum.RData')
head(merpredsum)

### From merpredsum data need to generate some descriptive statistics:

## Firstly on Transect_Biomass ##

# 1. By site only:
(predsitestats <- merpredsum_kg %>% 
    group_by(Reeftype, SiteCode) %>% 
    summarise_each(funs(mean,sd,se=sd(.)/sqrt(n())), Transect_biomass_kg))

# 2. By site and survey period (to check if there are any crazy differences between survey periods)
(predsitesurvstats <- merpredsum_kg %>% 
    group_by(SiteCode, SurvCode) %>% 
    summarise_each(funs(mean,sd,se=sd(.)/sqrt(n())), Transect_biomass_kg))

# 3. By reeftype only: (this is what we are most interested in)
(predreeftypestats <- merpredsum_kg %>% 
    group_by(Reeftype) %>% 
    summarise_each(funs(mean,sd,se=sd(.)/sqrt(n())), Transect_biomass_kg))
# Can check the means in this output table - should be the same as in emmeans in GLM later on

# 4. By reeftype and survey period (more to check )
(predreeftypesurvstats <- merpredsum_kg %>% 
    group_by(Reeftype, SurvCode) %>% 
    summarise_each(funs(mean,sd,se=sd(.)/sqrt(n())), Transect_biomass_kg))


# Graphics

# Summary histograms

# On raw predator biomass
ggplot(merpredsum_kg, aes(Transect_biomass_kg)) + 
  geom_histogram(binwidth=50, color="black", fill="grey")

# Lots of zeros, very right skewed

# On log pred biomass
ggplot(merpredsum_kg, aes(x=log(Transect_biomass_kg))) + 
  geom_histogram(binwidth=0.4, color="black", fill="grey")

# Looks like we'll need a model family on the log scale

## 1. Site stats across both survey periods

# Bar plot of individual sites - gives broad overview of underlying numbers for each site
sbar <- ggplot(predsitestats, aes(x=SiteCode, y=mean))+
  geom_bar(stat="identity", position="dodge", color="black")+
  scale_y_continuous("Mean mesopredator biomass (±SE)") +
  scale_x_discrete("Site and survey period")+
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
plot(sbar)

## 2. Site and survey period: check to see if there are any wild differences between the two survey periods
# Bar plot - for mean values
ssbar <- ggplot(predsitesurvstats, aes(x=SiteCode, y=mean, fill=SurvCode))+
  geom_bar(stat="identity", position="dodge", color="black")+
  scale_y_continuous("Mean predator biomass (±SE)") +
  scale_x_discrete("Site and survey period")+
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
ssbar # 

# Box plot - for median values
ssbox <- ggplot(merpredsum_kg, aes(x=SiteCode, y=Transect_biomass_kg, fill=SurvCode))+
  geom_boxplot()+
  scale_y_continuous("Median predator biomass (±SE)") +
  scale_x_discrete("Site and survey period")+
  scale_fill_viridis_d()+
  theme_scientific()+
  theme(legend.key.size = unit(0.4, "cm"))+
  theme(plot.title = element_text(size = 14, face = "bold"),
        legend.title=element_text(size=9), 
        legend.text=element_text(size=8),
        axis.title.y = element_text(size=10, face="bold"),
        axis.title.x = element_text(size=10, face="bold"),
        axis.text = element_text(size=6))
ssbox # Sizable difference for BRAD between periods - driven by the S qenie schools in 0319 - should be handled in the analysis as a source of variance


## 3. Reef type
# Bar plot - this is now what we are really interested in
rtbar <- ggplot(predreeftypestats, aes(x=Reeftype, y=mean))+
  geom_bar(stat="identity", position="dodge", color="black")+
  scale_y_continuous("Mean predator biomass (±SE)")+
  scale_x_discrete("Reef type and survey period")+
  labs(title="Predator biomass by reef type")+
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
rtbar


# Bar plot - log scale
rtbarlog <- ggplot(predreeftypestats, aes(x=Reeftype, y=log(mean)))+
  geom_bar(stat="identity", position="dodge", color="black")+
  scale_y_continuous("Log mean predator biomass (±SE)")+
  scale_x_discrete("Reef type and survey period")+
  labs(title="Predator log biomass by reef type")+
  scale_fill_viridis_d()+
  theme_scientific()+
  geom_errorbar(aes(ymin=log(mean)-log(se), ymax=log(mean)+log(se)), width=.2, position=position_dodge(.9))+
  theme(legend.key.size = unit(0.4, "cm"))+
  theme(plot.title = element_text(size = 14, face = "bold"),
        legend.title=element_text(size=9), 
        legend.text=element_text(size=8),
        axis.title.y = element_text(size=10, face="bold"),
        axis.title.x = element_text(size=10, face="bold"),
        axis.text = element_text(size=6))
rtbarlog


# Box plot - to check medians - expecting "Pinnacle" median to be skewed
rtbox <- ggplot(predsitestats, aes(x=Reeftype, y=mean))+
  geom_boxplot()+
  scale_y_continuous("Mean predator biomass (±SE)") +
  #geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.2, position=position_dodge(.9))+
  scale_x_discrete("Site and survey period")+
  scale_fill_viridis_d()+
  theme_scientific()+
  theme(legend.key.size = unit(0.4, "cm"))+
  theme(plot.title = element_text(size = 14, face = "bold"),
        legend.title=element_text(size=9), 
        legend.text=element_text(size=8),
        axis.title.y = element_text(size=10, face="bold"),
        axis.title.x = element_text(size=10, face="bold"),
        axis.text = element_text(size=6))
rtbox

