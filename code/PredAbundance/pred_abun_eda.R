## ---- pred.abun.EDA

### Predator abundance statistics by reeftype/site/survey
# Last edit 19.8.2021  BJC

# Housekeeping
rm(list=ls())
setwd("~/OneDrive - James Cook University/Ben PhD/Data & analysis/KimbePreds/code/PredAbundance")
library(tidyverse)
library(artyfarty)

# Start with predsum df
load('../../data/predsum.RData')
head(predsum)


### From predsum data need to generate some descriptive statistics:

## Firstly on No_Preds ##

# 1. By site only:
(predsitestats <- predsum %>% 
    group_by(Reeftype, SiteCode) %>% 
    summarise_each(funs(mean,sd,se=sd(.)/sqrt(n())), No_Preds))

# 2. By site and survey period (to check if there are any crazy differences between survey periods)
(predsitesurvstats <- predsum %>% 
  group_by(SiteCode, SurvCode) %>% 
  summarise_each(funs(mean,sd,se=sd(.)/sqrt(n())), No_Preds))

# 3. By reeftype only: (this is what we are most interested in)
(predreeftypestats <- predsum %>% 
  group_by(Reeftype) %>% 
  summarise_each(funs(mean,sd,se=sd(.)/sqrt(n())), No_Preds))
# Can check the means in this output table - should be the same as in emmeans in GLM later on

# 4. By reeftype and survey period (more to check )
(predreeftypesurvstats <- predsum %>% 
  group_by(Reeftype, SurvCode) %>% 
  summarise_each(funs(mean,sd,se=sd(.)/sqrt(n())), No_Preds))


# Graphics

# Summary histograms

# On raw predator abundance 
ggplot(predsum, aes(x=No_Preds)) + 
  geom_histogram(binwidth=50, color="black", fill="grey")

# Count data with a lot of zeros, so we'll most likely need a Poisson or neg binom model fit for either of these

# On proportion of assemblage (%) that are predators
ggplot(predsum, aes(x=PredPC)) + 
  geom_histogram(binwidth=0.01, color="black", fill="grey") 

# Again a lot of zeros but it's percentage/proportion data so would prob need a beta distribution for this


## 1. Site stats across both survey periods

# Bar plot of individual sites - gives broad overview of underlying numbers for each site
sbar <- ggplot(predsitestats, aes(x=SiteCode, y=mean))+
  geom_bar(stat="identity", position="dodge", color="black")+
  scale_y_continuous(limits = c(0,300), "Mean mesopredator count (±SE)") +
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
  scale_y_continuous(limits = c(0,400), "Mean predator count (±SE)") +
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
ssbar # No crazy differences in mean values (±SE)

# Box plot - for median values
ssbox <- ggplot(predsum, aes(x=SiteCode, y=No_Preds, fill=SurvCode))+
  geom_boxplot()+
  scale_y_continuous(limits = c(0,1000), "Median predator count (±SE)") +
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
  scale_y_continuous(limits = c(0,90), "Mean predator count (±SE)")+
  scale_x_discrete("Reef type and survey period")+
  labs(title="Predator abundance by reef type")+
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

# Box plot - to check medians - expecting "Pinnacle" median to be skewed
rtbox <- ggplot(predsitestats, aes(x=Reeftype, y=mean))+
  geom_boxplot()+
  scale_y_continuous(limits = c(0,200), "Mean predator count (±SE)") +
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


####################


## Now repeating the above on proportion of preds:

# 1. By site only:
(pcsitestats <- predsum %>% 
   group_by(Reeftype, SiteCode) %>% 
   summarise_each(funs(mean,sd,se=sd(.)/sqrt(n())), PredPC))

# 2. By site and survey period (to check if there are any crazy differences between survey periods)
(pcsitesurvstats <- predsum %>% 
    group_by(SiteCode, SurvCode) %>% 
    summarise_each(funs(mean,sd,se=sd(.)/sqrt(n())), PredPC))

# 3. By reeftype only: (this is what we are most interested in)
(pcreeftypestats <- predsum %>% 
    group_by(Reeftype) %>% 
    summarise_each(funs(mean,sd,se=sd(.)/sqrt(n())), PredPC))
# Can check the means in this output table - should be the same as in emmeans in GLM later on

# 4. By reeftype and survey period (more to check )
(pcreeftypesurvstats <- predsum %>% 
    group_by(Reeftype, SurvCode) %>% 
    summarise_each(funs(mean,sd,se=sd(.)/sqrt(n())), PredPC))


# Graphics

## 1. Site stats across both survey periods

# Bar plot of individual sites - gives broad overview of underlying numbers for each site
sbar <- ggplot(pcsitestats, aes(x=SiteCode, y=mean))+
  geom_bar(stat="identity", position="dodge", color="black")+
  scale_y_continuous(limits = c(0,1), "Mean predator proportion (±SE)") +
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
plot(sbar) # MADA looking like it's got a few more than expected (as % of assemblage)

## 2. Site and survey period: check to see if there are any wild differences between the two survey periods
# Bar plot - for mean values
ssbar <- ggplot(pcsitesurvstats, aes(x=SiteCode, y=mean, fill=SurvCode))+
  geom_bar(stat="identity", position="dodge", color="black")+
  scale_y_continuous(limits = c(0,1), "Mean predator proportion (±SE)") +
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
ssbar # BRAD, HOGU, MADA boosted by 0319 

# Box plot - for median values
ssbox <- ggplot(predsum, aes(x=SiteCode, y=PredPC, fill=SurvCode))+
  geom_boxplot()+
  scale_y_continuous(limits = c(0,1), "Median predator proportion (±SE)") +
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
rtbar <- ggplot(pcreeftypestats, aes(x=Reeftype, y=mean))+
  geom_bar(stat="identity", position="dodge", color="black")+
  scale_y_continuous(limits = c(0,0.15), "Mean predator proportion (±SE)")+
  scale_x_discrete("Reef type and survey period")+
  #labs(title="Predator abundance by reef type")+
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

# Box plot - to check medians
rtbox <- ggplot(pcsitestats, aes(x=Reeftype, y=mean))+
  geom_boxplot()+
  scale_y_continuous(limits = c(0,1), "Mean predator proportion (±SE)") +
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





