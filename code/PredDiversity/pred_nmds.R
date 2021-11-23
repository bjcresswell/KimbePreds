# Predator diversity PCA script 

# Created 13 Aug 2021 BJC
# Last edit 16 Sep 2021

# Got the initial code from https://bayesbaes.github.io/2021/01/28/PCA-tutorial.html
# Also some inspiration from https://ourcodingclub.github.io/tutorials/ordination/#section5

# Housekeeping
rm(list=ls())
graphics.off()

# Packages
library(MASS)
library(ggdendro)
library(vegan)
library(ggvegan)
library(tidyverse)


# Setwd
setwd("/Users/bjcresswell/OneDrive - James Cook University/Ben PhD/Data & analysis/KimbePreds/code/PredDiversity")



# Start with predator diversity df - preddiv
load(file='../../data/preddiv.RData')

head(preddiv) # First column is TID and the spp. observations start at column 11

# Pull out the descriptors into an object called env
env <- preddiv[1:10] %>%
  mutate(Reeftype = factor(case_when(grepl("BRAD", TID) ~ "Pinnacle",
                                     grepl("JOEL", TID) ~ "Pinnacle",
                                     grepl("KBOM", TID) ~ "Pinnacle",
                                     grepl("INGL", TID) ~ "Pinnacle",
                                     grepl("LADI", TID) ~ "Nearshore",
                                     grepl("MADA", TID) ~ "Nearshore",
                                     grepl("SUSA", TID) ~ "Nearshore",
                                     grepl("DON", TID) ~ "Nearshore",
                                     grepl("EMA", TID) ~ "Offshore",
                                     grepl("HOG", TID) ~ "Offshore",
                                     grepl("OTT", TID) ~ "Offshore",
                                     grepl("KIS", TID) ~ "Offshore")))



# Now turn preddiv into matrix with only spp observations
predmatrix <- preddiv[c(1,11:71)] %>% 
  column_to_rownames("TID") %>%
  filter_all(any_vars(. != 0))# %>% # Drop 22 transects with only 0 observations
  +0.00001


# Sqrt trans
#predmatrix <-sqrt(predmatrix)


# Now can create distance matrix
preddist <- vegdist(predmatrix,  method = "bray")

preddist <- decostand(preddist, method = "standardize")

predNMDS <- metaMDS(preddist, k = 2, trymax = 100, trace = F)


NMDS1 <- predNMDS$points[,1] ##also found using scores(predNMDS)
NMDS2 <- predNMDS$points[,2]


pred.plot<-cbind(predmatrix, NMDS1, NMDS2) %>% 
  rownames_to_column(var = "TID")

env1 <- env %>% 
  right_join(pred.plot, by = "TID", keep = FALSE)






p<-ggplot(env1, aes(NMDS1, NMDS2, color=Reeftype))+
  geom_point(position=position_jitter(.1), shape=3)+##separates overlapping points
  #geom_text(aes(label = TID))+
  stat_ellipse(type='t',size =1)+ ##draws 95% confidence interval ellipses
  theme_minimal()
p

stressplot(predNMDS)



# Tutorial works on the varespec object - which is basically an observation matrix
# Load
data(varespec)

# Make distance matrix
dist <- vegdist(varespec,  method = "bray")







# The matrix is just on raw data - multivariate analysis used log transformation on neg binom distribution
# So will use same transforation here
std.matrix <- decostand(predmatrix, method = "log")







sqrt(3)
sqrt(2)
sqrt(1)
sqrt(0.5)
sqrt(0.1)
sqrt(0)






