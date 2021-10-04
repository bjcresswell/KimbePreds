## ---- pred.abun.pc.EDA

rm(list=ls())
library(tidyverse)
library(artyfarty)
setwd("~/OneDrive - James Cook University/Ben PhD/Data & analysis/KimbePreds")

# Start with predsum df
load('data/predsum.RData')
head(predsum)

### Predator percent statistics by site/survey 

# New df with stats
predpc <- predsum %>% 
  group_by(Reeftype, SiteCode, SurvCode, SiteSurv) %>% 
  summarise_each(funs(mean,sd,se=sd(.)/sqrt(n())), PredPC)
predpc

# Bar plot - by site/survey
pcbar <- ggplot(predpc, aes(x=SiteCode, y=mean, fill=SurvCode))+
  geom_bar(stat="identity", position="dodge", color="black")+
  scale_y_continuous(limits = c(0,30), "Mean mesopredator count (±SE)") +
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
pcbar
