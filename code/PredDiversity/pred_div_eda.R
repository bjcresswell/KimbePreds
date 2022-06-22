## ---- pred.div.EDA

# Author: BJC
# Last edit: 1.7.2021

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




