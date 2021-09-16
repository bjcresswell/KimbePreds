## ---- pred.richness

# Script starts with 'wide' predator count data in  predvegan df and produces species richness (pred.rich) count df.

rm(list=ls())
library(gdata)
library(vegan)
library(readxl)
library(labdsv)
library(tidyverse)
setwd("~/OneDrive - James Cook University/Ben PhD/Data & analysis/KimbePreds")

load(file='data/predvegan.RData')
predvegan

# For species richness calculation:
pred.rich<-as.data.frame(specnumber(predmatrix)) %>% 
  mutate(TID = rownames(predmatrix))  %>%  # Need to move the rownames into the main df
  rename(SpecNo = `specnumber(predmatrix)`)

head(pred.rich)


pred.rich <- pred.rich %>% 
  mutate(Reeftype = factor(case_when(grepl("BRAD", `TID`) ~ "Pinnacle",      # Assign reef type status
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
                            grepl("KIS", `TID`) ~ "Offshore"))) %>% 
  mutate(Reeftype = factor(Reeftype, levels= c("Pinnacle", "Offshore", "Nearshore"))) %>% # Organise by reef type
  mutate(SiteCode = factor(case_when(grepl("BRAD", `TID`) ~ "BRAD",      # Assign site name
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
                                grepl("KIS", `TID`) ~ "KIIS"))) %>%
  mutate(SiteCode = factor(SiteCode, levels= c("BRAD", "JOEL", "INGL", "KBOM",   # Pinnacles first
                                                   "EMA",  "HOG", "KIS", "OTT",      # then offshore
                                                   "DON", "LADI", "MADA", "SUSA"))) %>% 
  mutate(Site = factor(case_when(grepl("BRAD", `TID`) ~ "Bradford Shoals",      # Assign site name
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
                                 grepl("KIS", `TID`) ~ "Kimbe Island"))) %>% 
  mutate(SurvCode = factor(case_when(grepl("1018", `TID`) ~ "2018",      # Assign surv code
                              grepl("0319", `TID`) ~ "2019")))

head(pred.rich)


# Some eda using boxplots:

ggplot(pred.rich, aes(x=Reeftype, y=SpecNo))+
  geom_boxplot()
           
ggplot(pred.rich, aes(x=Site, y=SpecNo))+
  geom_boxplot()

ggplot(pred.rich, aes(x=Site, y=SpecNo, fill=SurvCode))+
  geom_boxplot() 


ggplot(pred.rich, aes(x=No_PredsZ)) + 
  geom_histogram(binwidth=50, color="black", fill="grey")


# Pred richness stats


head(pred.rich)


prglm <- glmmTMB(SpecNo ~ Reeftype, data = pred.rich, family='gaussian')
simulateResiduals(prglm, plot=T) # KS deviance

prglmp <- glmmTMB(SpecNo ~ Reeftype, data = pred.rich, family='poisson')
simulateResiduals(prglmp, plot=T) # Dispersion deviance

prglmmnb <- glmmTMB(SpecNo ~ Reeftype+(1|SurvCode), data = pred.rich, family='nbinom2')
simulateResiduals(prglmnb, plot=T) # Good fit, but do we need random effect?

prglmnb <- glmmTMB(SpecNo ~ Reeftype, data = pred.rich, family='nbinom2')
simulateResiduals(prglmnb, plot=T) # Good fit

prglmnbfe <- glmmTMB(SpecNo ~ Reeftype+SurvCode, data = pred.rich, family='nbinom2')
simulateResiduals(prglmnb, plot=T) # Good fit


# AIC Scores

AIC(prglm, prglmp, prglmmnb, prglmnb, prglmnbfe) # pred.glmnb is the best fit



# Model output
model.means <- emmeans(prglmnb, pairwise~Reeftype, transform="response") %>% 
  confint() %>% 
  as.data.frame()

tab_df(model.means)

model.means

plot_model(pred.glmnb,  type='eff')


####  
ggplot(pred.rich, aes(x=Reeftype, y=SpecNo))+
  geom_bar()

  
  scale_shape_manual(values=c(21, 24))+
  scale_fill_viridis_d()+
  geom_line(aes(color=Sitetype))+
  scale_y_continuous(limits = c(0,100), "Mean coral cover (% ±S.E.)") +
  scale_x_discrete("Survey period", expand = c(0.05, 0.05))+
  #labs(title="Bleaching weighted scores - impact vs reference sites")+
  scale_color_viridis_d()+
  theme_scientific()+
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.05, size=.5, alpha=0.9)+
  theme(legend.key.size = unit(0.6, "cm"))+
  theme(plot.title = element_text(size = 14, face = "bold", colour="black"),
        legend.title=element_text(size=12, face="bold", colour="black"), 
        legend.text=element_text(size=12, colour="black"),
        axis.title.y = element_text(size=14, face="bold", colour="black"),
        axis.title.x = element_text(size=14, face="bold", colour="black"),
        axis.text.x = element_text(size=10, colour="black"), 
        axis.text.y = element_text(size=10, colour="black"))+
  labs(fill= "Site type", pch="Site type", colour="Site type")


boxplot(pred.rich[1:14], pred.rich[15:38],
        names=c("Nearshore", "Seamount"), 
        ylab="Richness (No. of Species)",
        main = "Species Richness by Reef Type",
        col = "grey")
  

boxplot(pred.rich,
          names=c("Nearshore", "Seamount"), 
          ylab="Richness (No. of Species)",
          main = "Species Richness by Reef Type",
          col = "grey")
  