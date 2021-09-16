## ---- pred.shan

# Script starts with 'wide' predator count data per 

rm(list=ls())
library(gdata)
library(vegan)
library(readxl)
library(labdsv)
library(tidyverse)
setwd("~/OneDrive - James Cook University/Ben PhD/Data & analysis/KimbePreds")

load(file='data/predvegan.RData')
predvegan


# Shannon diversity
pred.shannon<-as.data.frame(diversity(predvegan)) %>% 
  rename(Shannon = `diversity(predvegan)`) %>% 
  mutate(TID = rownames(predvegan)) # Need to move the rownames into the main df
  
head(pred.shannon)


pred.shannon <- pred.shannon %>% 
  mutate(Reeftype = case_when(grepl("BRAD", `TID`) ~ "Pinnacle",      # Assign reef type status
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
                              grepl("KIS", `TID`) ~ "Offshore")) %>% 
  mutate(Reeftype = factor(Reeftype, levels= c("Pinnacle", "Offshore", "Nearshore"))) %>% # Organise by reef type
  mutate(SiteCode = case_when(grepl("BRAD", `TID`) ~ "BRAD",      # Assign site name
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
                              grepl("KIS", `TID`) ~ "KIIS")) %>%
  mutate(SiteCode = factor(SiteCode, levels= c("BRAD", "JOEL", "INGL", "KBOM",   # Pinnacles first
                                               "EMA",  "HOG", "KIS", "OTT",      # then offshore
                                               "DON", "LADI", "MADA", "SUSA"))) %>% 
  mutate(Site = case_when(grepl("BRAD", `TID`) ~ "Bradford Shoals",      # Assign site name
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
                          grepl("KIS", `TID`) ~ "Kimbe Island")) %>% 
  mutate(SurvCode = case_when(grepl("1018", `TID`) ~ "2018",      # Assign surv code
                              grepl("0319", `TID`) ~ "2019"))

head(pred.shannon)


# Some eda using boxplots:

ggplot(pred.shannon, aes(x=Reeftype, y=Shannon))+
  geom_boxplot()

ggplot(pred.shannon, aes(x=Site, y=Shannon))+
  geom_boxplot()

ggplot(pred.shannon, aes(x=SurvCode, y=Shannon))+
  geom_boxplot() # Interesting - almost no difference between the 2 periods...


# Shannon stats

pshan.lm <- glm(Shannon ~ Reeftype, data = pred.shannon, family=poisson(link='log'))
simulateResiduals(pshan.lm, plot=T) # KS deviance


pshan.lm <- glmmTMB(Shannon ~ Reeftype, data = pred.shannon, family= 'zero_inflated_negbinomial')
simulateResiduals(pshan.lm, plot=T) # KS deviance

pshan.lmp <- glmmTMB(Shannon ~ Reeftype, data = pred.shannon, family='poisson')
simulateResiduals(pshan.lmp, plot=T) # Dispersion deviance

pshan.lmmnb <- glmmTMB(Shannon ~ Reeftype+(1|SurvCode), data = pred.shannon, family='nbinom1')
simulateResiduals(pshan.lmmnb, plot=T) # Good fit, but do we need random effect?

pshan.lmnb <- glmmTMB(Shannon ~ Reeftype, data = pred.shannon, family='nbinom2')
simulateResiduals(pshan.lmnb, plot=T) # Good fit

pshan.glmgamma <- glmmTMB(Shannon ~ Reeftype+SurvCode, data = pred.shannon, family='beta')
simulateResiduals(pshan.lmnb, plot=T) # Good fit






