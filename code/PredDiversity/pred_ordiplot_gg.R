# Predator diversity PCA script 

# Created 13 Aug 2021 BJC
# Last edit 16 Sep 2021

# Got the initial code from https://bayesbaes.github.io/2021/01/28/PCA-tutorial.html
# Also some inspiration from https://ourcodingclub.github.io/tutorials/ordination/#section5

# Housekeeping
#rm(list=ls())
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

# Turn into matrix with only spp observations
predmatrix <- preddiv[c(1,9:71)] %>% 
  column_to_rownames("TID") #%>% # Get the transect labels into the rownames - might look like this is back and forth but need for decostand below
  #filter_all(any_vars(. != 0)) # Keep all rows - as per mvabund
predmatrix

# Create env object populated with transect and reeftype information:
env <- predmatrix %>%
  rownames_to_column("TID") %>% 
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
                                     grepl("KIS", TID) ~ "Offshore"))) %>% 
  dplyr::select("TID", "Reeftype")


# The matrix is just on raw data - multivariate analysis used log transformation on neg binom distribution
# So will use same transforation here
std.matrix <- decostand(predmatrix, method = "log")


# Run PCA using rda function in vegan
PCA<- rda(std.matrix)


# Use eigenvectors to check how much variance explained by first two axes
sum((as.vector(PCA$CA$eig)/sum(PCA$CA$eig))[1:2]) # Combined
sum((as.vector(PCA$CA$eig)/sum(PCA$CA$eig))[1]) # Axis 1 - 32% of variance explained
sum((as.vector(PCA$CA$eig)/sum(PCA$CA$eig))[2]) # Axis 2 - 14% of variance explained


# Now extract site scores
sitePCA <- as.data.frame(PCA$CA$u[,1:2]) %>%  # only need first two axes
  bind_cols(env) # bind back to the env df (which has all the site info now)

# And extract species scores
speciesPCA <- as.data.frame(PCA$CA$v[,1:2]) %>%  # Species scores
  rownames_to_column("Species") %>% 
  filter(Species %in% c("Sphyraena qenie", "Caranx sexfasciatus", "Macolor macularis",
                        "Caranx melampygus", "Lutjanus gibbus", "Cephalopholis cyanostigma"))


# Make basic plot
basic_plot <- 
  ggplot(sitePCA, aes(x = PC1, y = PC2)) +
  geom_point(aes(colour = Reeftype)) +
  scale_color_manual(values=c("#DFC27D","#436EEE","#35978F"))+
  xlim(-0.65, 0.3)+
  theme_classic()

basic_plot


# If we want convex hulls to group together by reef type need to create a hull object
pca_hull <- 
  sitePCA %>% 
  group_by(Reeftype) %>% 
  slice(chull(PC1, PC2))

# now add into basic plot
chull_plot <- 
  basic_plot +
  geom_vline(xintercept = 0, lty="dashed", alpha=0.5)+
  geom_hline(yintercept = 0, lty="dashed", alpha=0.5)+
  geom_polygon(data = pca_hull,
               aes(fill = Reeftype,
                   colour = Reeftype),
               alpha = 0.3,
               show.legend = FALSE)+
  scale_fill_manual(values=c("#DFC27D","#436EEE","#35978F"))
  

chull_plot

# And add species loadings in
final_plot <- chull_plot +
  geom_segment(data = speciesPCA, aes(x = 0, y = 0, xend = PC1, yend = PC2), 
               arrow = arrow(length = unit(0.2, 'cm')), colour = "black", alpha=0.6) +
  annotate('text', x = (speciesPCA$PC1*1.1), y = (speciesPCA$PC2*1.1),
           label = speciesPCA$Species,
           size = 3.5) +
  theme(legend.position = c(0.85, 0.2))

final_plot

ggsave(final_plot, filename= '../../output/rfigures/predPCA2.pdf', width=7,  height=6, dpi = 1000 )



