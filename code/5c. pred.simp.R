## ---- pred.simp

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


# Distance matrix - is this necessary???
preddist <- vegdist(predvegan, method="bray")
preddist <- as.matrix(preddist)

# Simpson diversity on PREDDIST
pred.simpson<-as.data.frame(diversity(preddist, index="simpson")) %>% 
  rename(Simpson =  `diversity(preddist, index = "simpson")`) %>% 
  mutate(TID = rownames(preddist)) # Need to move the rownames into the main df


# Simpson diversity
pred.simpson<-as.data.frame(diversity(predvegan, index="simpson")) %>% 
  rename(Simpson =  `diversity(predvegan, index = "simpson")`) %>% 
  mutate(TID = rownames(predvegan)) # Need to move the rownames into the main df

head(pred.simpson)



pred.simpson <- pred.simpson %>% 
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

head(pred.simpson)


# Some eda using boxplots:

ggplot(pred.simpson, aes(x=Reeftype, y=Simpson))+
  geom_boxplot()

ggplot(pred.simpson, aes(x=Site, y=Simpson))+
  geom_boxplot()

ggplot(pred.simpson, aes(x=SurvCode, y=Simpson))+
  geom_boxplot()




