## ---- pred.div.EDA

rm(list=ls())
library(gdata)
library(vegan)
library(readxl)
library(labdsv)
library(tidyverse)
setwd("~/OneDrive - James Cook University/Ben PhD/Data & analysis/Kimbe Pred DandA")
load('data/preds.RData')

# Some data wrangling
preds.div <- preds %>% group_by(TID, Taxa) %>% summarise(Count=sum(Number))
head(preds.div)
predv <- preds.div %>%  pivot_wider(names_from = Taxa, values_from=Count)
predv[is.na(predv)] = 0 # Remove NAs in whole df and set to zero (this is the case if cell is empty)
predv

pv <- t(predv)                                                         
