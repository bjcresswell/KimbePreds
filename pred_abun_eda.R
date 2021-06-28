## ---- pred.abun.EDA

rm(list=ls())
library(gdata)
library(vegan)
library(readxl)
library(labdsv)
library(tidyverse)
setwd("~/OneDrive - James Cook University/Ben PhD/Data & analysis/Kimbe Pred DandA")

# Load data
load('data/preds.RData')
head(preds)

# Some data wrangling - need to get all transects in preds.abun df. Here's a rather wriggly workaround...
preds.abun <- preds %>% group_by(TID) %>% summarise(Count=sum(Number)) # DF with pred ounts by TID (97 of these)
fish.abun <- fish %>% group_by(TID) %>% summarise(No.=sum(Number)) # DF with fish counts by TID (the full 120 of these)
preds.abun
fish.abun
preds.abun1 <- merge(preds.abun, fish.abun, by="TID", all = T) # Merge back together and keep all 120 rows
preds.abun1[is.na(preds.abun1)] = 0 # Remove NAs in whole df and set to zero
preds.abun1 <- preds.abun1[1:2] %>% # Remove the "fish" counts
  mutate(ID = TID) %>% # Add a column for ID that will remain once TID has gone (next line)
  extract(TID, into = c("Site", "Transect", "Survey"), "(.*)_([^_]+)_([^_]+)$") %>%  # Use regex to splt out TID column
  mutate(Reeftype = case_when(grepl("BRAD", `Site`) ~ "Pinnacle",
                              grepl("JOEL", `Site`) ~ "Pinnacle",
                              grepl("KBOM", `Site`) ~ "Pinnacle",
                              grepl("INGL", `Site`) ~ "Pinnacle",
                              grepl("LADI", `Site`) ~ "Nearshore",
                              grepl("MADA", `Site`) ~ "Nearshore",
                              grepl("SUSA", `Site`) ~ "Nearshore",
                              grepl("DON", `Site`) ~ "Nearshore",
                              grepl("EMA", `Site`) ~ "Offshore",
                              grepl("HOG", `Site`) ~ "Offshore",
                              grepl("OTT", `Site`) ~ "Offshore",
                              grepl("KIS", `Site`) ~ "Offshore"))

preds.abun1


### Summary Statistics ###
pa <- preds.abun1
pa
sumFunc <- function(x) c(mean=mean(x), n=length(x), median=median(x), se=sd(x)/sqrt(length(x)))
sumFunc(pa$Count[pa$Reeftype=="Nearshore"])
sumFunc(pa$Count[pa$Reeftype=="Offshore"])
sumFunc(pa$Count[pa$Reeftype=="Pinnacle"])

par(mfrow=c(1,1))
boxplot(Count ~ Reeftype, data = pa,
        names=c("Nearshore", "Offshore", "Pinnacle"), 
        ylab="Abundance (total fish counted/transect",
        main = "Species Abundance",
        col = "grey")

### JUNK CODE ###

# Visual
par(mfrow=c(2,3))
hist(pa$Count[pa$Reeftype=="Nearshore"], prob="T", main = "Nearshore", xlab = "Abundance")
curve(dnorm(x, mean(pa$Count[pa$Reeftype=="Nearshore"]),sd(pa$Count[pa$Reeftype=="Nearshore"])), 
      add=TRUE, col='red', lwd=3) # Nearshore
hist(pa$Count[pa$Reeftype=="Offshore"], prob="T", main = "Offshore", xlab = "Abundance")
curve(dnorm(x, mean(pa$Count[pa$Reeftype=="Offshore"]),sd(pa$Count[pa$Reeftype=="Offshore"])), 
      add=TRUE, col='red', lwd=3) # Offshore
hist(pa$Count[pa$Reeftype=="Pinnacle"], prob="T", main = "Seamount", xlab = "Abundance")
curve(dnorm(x, mean(pa$Count[pa$Reeftype=="Pinnacle"]),sd(pa$Count[pa$Reeftype=="Pinnacle"])), 
      add=TRUE, col='red', lwd=3) # Offshore
qqnorm(pa$Count[pa$Reeftype=="Nearshore"], main = "Nearshore")
qqline(pa$Count[pa$Reeftype=="Nearshore"], col="red", lwd=3)
qqnorm(pa$Count[pa$Reeftype=="Offshore"], main = "Offshore")
qqline(pa$Count[pa$Reeftype=="Offshore"], col="red", lwd=3)
qqnorm(pa$Count[pa$Reeftype=="Pinnacle"], main = "Seamount")
qqline(pa$Count[pa$Reeftype=="Pinnacle"], col="red", lwd=3)

# Shapiro-Wilks
shapiro.test(pa$Count[pa$Reeftype=="Nearshore"])
shapiro.test(pa$Count[pa$Reeftype=="Offshore"])
shapiro.test(pa$Count[pa$Reeftype=="Pinnacle"])

# For now, we'll produce a bar plot on the raw data

# Create three colour palette: Black, blue, green:
cbPalette <- c("#000000", "#56B4E9", "#009E73")
# OR greyscale palette:
gsPalette <- c("grey", "grey48", "grey38")

par(mfrow=c(1,1))
pb <- ggplot(pa, aes(x=Reeftype, y=Count, fill=Reeftype)) +
  stat_summary(geom = "bar", fun.y = mean, width=0.7) +
  stat_summary(geom = "errorbar", fun.data = mean_se, width=0.15) +
  scale_fill_manual(values=gsPalette) + 
  theme_classic() + 
  ggtitle("Predator abundance by reef type") +
  xlab("Reef Type") + ylab("Fish abundance") +
  theme(plot.title = element_text(family="Arial", size=16, hjust = 0.5)) +
  theme(axis.title.x = element_text(family="Arial", size=12)) +
  theme(legend.position='none')
pb

