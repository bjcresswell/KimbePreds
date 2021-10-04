# A third attempt at sorting out an ordination plot for multivariate analysis
# Have been trying to make PCoA work but difficult!!

# Packages
library(ape) # Contains alternative method for conducting PCoA
library(vegan)
library(ggvegan)
remotes::install_github("cmartin/ggConvexHull")
library("ggConvexHull")
library(tidyverse)

# Setwd
setwd("/Users/bjcresswell/OneDrive - James Cook University/Ben PhD/Data & analysis/KimbePreds/code/PredDiversity")

# Load data
#rm(list=ls())
load(file='../../data/preddiv.RData')

# Take the preddiv matrix and remove the env parameters (need to remove empty rows so have to get rid of these)
predmatrix <- preddiv[c(1,9:71)] %>% 
  column_to_rownames("TID") %>% 
  filter_all(any_vars(. != 0)) # Empty rows destroy the ordination (so back to 99)
predmatrix

# Create env df populated with transect and reeftype information:
env <- tibble(predmatrix %>% 
  rownames_to_column("TID") %>% # 
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
                            grepl("KIS", TID) ~ "Offshore")))) %>% 
  select("TID", "Reeftype")

env

# How many transects are we left with per reeftype?
sum(env$Reeftype=="Nearshore") # 24 - so 16 missing
sum(env$Reeftype=="Offshore") # 35 - 5 missing
sum(env$Reeftype=="Pinnacle") # 40 - all present


# Create distance matrix (Bray-Curtis by default in vegdist)
dist <- vegdist(predmatrix,  method = "bray")

# Create PCOA object using weighted classical (metric) multidimentional scaling (WCMD scaling - MDS/PCoA)
PCOA <- wcmdscale(dist, eig=TRUE)
PCOA$points

# Have negative eigenvalues so need to add correction:
PCOA <- wcmdscale(dist, eig=TRUE, add = "cailliez")
PCOA$points


pcoa <- as.data.frame(PCOA$points)


# Pull the site data out of the PCOA object
sitedata <- pcoa[1:2] %>% 
  bind_cols(env)

# Pull out the species scores
spp.scrs <- envfit(pcoa, predmatrix)

# The coordinates are contained in an $object called $arrows:
spp.scrs$vectors$arrows

# Filter out just the results for the significant drivers of differences (as identified in manyglm)
sppdata <- as.data.frame(scores(spp.scrs, display = "vectors")) %>% 
  rownames_to_column("Species") %>% 
  filter(Species %in% c("Sphyraena qenie", "Caranx sexfasciatus", "Macolor macularis", "Caranx melampygus"))

sppdata


# Now can make plot
ggplot(sitedata)+
  geom_point(mapping = aes(x = Dim1, y = Dim2, colour = Reeftype, shape = Reeftype)) +
  #coord_fixed() + ## need aspect ratio of 1!
  geom_vline(xintercept = 0, lty="dashed")+
  geom_hline(yintercept = 0, lty="dashed")+
  #geom_convexhull(aes(x = Dim1, y = Dim2, colour = Reeftype, fill = Reeftype), alpha=0)+
  theme_classic()+
  scale_color_viridis_d()+
  #stat_ellipse(data = sitedata, aes(x = Dim1, y = Dim2, colour = Reeftype, fill=Reeftype, alpha=0.5))+
  stat_ellipse(data = sitedata, aes(x = Dim1, y = Dim2, colour = "black", 
                                    fill=Reeftype), alpha=0.3, geom = "polygon")+
  scale_fill_viridis_d()+
  geom_segment(data = sppdata,
               aes(x = 0, xend = Dim1*3.5, y = 0, yend = Dim2*3.5),
               arrow = arrow(length = unit(0.2, "cm")), colour = "black") +
  geom_text(data = sppdata, aes(x = Dim1*4, y = Dim2*4, label = Species), 
            position = position_dodge2(width=0.1, preserve = "total"),
            size = 3)

+facet_wrap(vars(Reeftype), nrow = 2, ncol = 2)


scale_color_manual(values=c("#35978F","#436EEE","#DFC27D" ))
  
########


### JUNK ###

# Very basic plot
#The below is adapted from here: https://stackoverflow.com/questions/42799838/follow-up-plotting-ordiellipse-function-from-vegan-package-onto-nmds-plot-creat/43566309#43566309

PCOAdata <- data.frame(Axes1 = PCOA$vectors[,1],     Axes2=PCOA$vectors[,2], group=env$Reeftype)
PCOA.mean=aggregate(PCOAdata[,1:2], list(group=PCOAdata$group), mean)

ggplot(data = PCOAdata, aes(Axes1, Axes2)) + 
  geom_point(aes(color = group))
  




