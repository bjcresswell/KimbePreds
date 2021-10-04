# Predator diversity PCA script 

# Created 13 Aug 2021 BJC
# Last edit 16 Aug 2021
# Got the initial prcomp code from https://bayesbaes.github.io/2021/01/28/PCA-tutorial.html


# Setwd
setwd("/Users/bjcresswell/OneDrive - James Cook University/Ben PhD/Data & analysis/KimbePreds/code/PredDiversity")

library(MASS)
library(ggdendro)
library(vegan)
library(ggvegan)
library(tidyverse)


# Start with predator observation matrix created in diw section
rm(list=ls())
load(file='../../data/preddiv.RData')

predmatrix <- preddiv[c(1,9:71)] %>% 
  column_to_rownames("TID") %>% 
  filter_all(any_vars(. != 0)) # Empty rows destroy the ordination (so back to 99)
predmatrix

# Create env df populated with transect and reeftype information:
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

env

# Get rid of 0 entries
predmatrix <- preddiv[8:70] %>% 
  filter_all(any_vars(. != 0)) # Removes empty rows so back to 99


# Going to need row names 
predtable <- predtable %>%
  rownames_to_column("TID") 

# And probably a separate df with just the env parameters
env <- predtable[1]

# 1. PCA on raw data

# Initial PCA
pred.pca <- prcomp(preddiv[8:70], center = TRUE, scale = TRUE)
# pred.pca <- prcomp(preddiv[8:70]) # Not centered or scaled - not a good output

summary(pred.pca)
biplot(pred.pca)
# Convert the pca results to a tibble - raw
pca_points <- as_tibble(pred.pca$x) %>%
  bind_cols(env) # bind back to the env df (which has all the site info now)

# Make basic plot
basic_plot <- 
  ggplot(pca_points, aes(x = PC1, y = PC2)) +
  geom_point(aes(colour = Reeftype)) +
  theme_light()

# If we want convex hulls to group together by reef type need to create a hull object
pca_hull <- 
  pca_points %>% 
  group_by(Reeftype) %>% 
  slice(chull(PC1, PC2))

# now add into basic plot
chull_plot <- 
  basic_plot +
  geom_polygon(data = pca_hull,
               aes(fill = Reeftype,
                   colour = Reeftype),
               alpha = 0.3,
               show.legend = FALSE)

chull_plot

# To add loadings back in:
pca_load <- 
  as_tibble(pred.pca$rotation, rownames = 'rownames')# %>% 
  arrange(-PC2) %>% 
  arrange(-PC1)

pca_load$PC1


chull_plot +
  geom_segment(data = pca_load, 
               aes(x = 0, y = 0, 
                   xend = PC1*50,
                   yend = PC2*50),
               arrow = arrow(length = unit(1/2, 'picas'))) +
  annotate('text', x = (pca_load$PC1*6), y = (pca_load$PC2*5.2),
           label = pca_load$rownames,
           size = 3.5) 


# 2. PCA on Transformed data

# Hellinger
helling.matrix <- decostand(preddiv[8:70], method = "hellinger")

# Initial PCA on Hellinger data
helling.pca <- prcomp(helling.matrix, center = TRUE, scale = TRUE) # Bad


helling_pca_points <- as_tibble(helling.pca$x) %>%
  bind_cols(env) # bind back to rest of preddiv (which has all the site info now)

# Plot transformed data
helling_plot <- 
  ggplot(helling_pca_points, aes(x = PC1, y = PC2)) +
  geom_point(aes(colour = Reeftype)) +
  theme_light()

# Convert the pca results to a tibble - Hellinger
helling_pca_points <- as_tibble(helling.pca$x) %>%
  bind_cols(env) # bind back to the env df (which has all the site info now)

helling_hull <- 
  helling_pca_points %>% 
  group_by(Reeftype) %>% 
  slice(chull(PC1, PC2))


helling_hullplot <-
  helling_plot +
  geom_polygon(data = helling_hull,
               aes(fill = Reeftype,
                   colour = Reeftype),
               alpha = 0.3,
               show.legend = FALSE)


helling_hullplot



rda <-  rda(preddiv[8:70])
biplot(rda)
biplot(rda,
       display = c("sites", 
                   "species"),
       type = c("text",
                "points"),
       scaling = "site")


ordihull(rda,
         group = preddiv$Reeftype,
         col = c(1,2,3))


legend("topright",
       col = c(1,2,3), 
       lty = 1,
       levels(preddiv$Reeftype))



autoplot(rda, layers = 'sites')

rda_fort <- fortify(rda, display = 'sites') %>% 
  bind_cols(env)
  
ggplot(rda_fort, aes(x = PC1, y = PC2, colour = Reeftype, group = Reeftype)) +
  geom_line() + #use geom_path not geom_line
  #geom_point(aes(size = if_else(week == min(week), 1, NA_real_)), show.legend = FALSE) +
  scale_color_viridis_d() +
  scale_size(range = 2) +
  coord_equal()


autoplot(rda)+
  scale_color_viridis_d()+
  theme_classic()+
  coord_equal()

## Tutorial

data("pyrifos")

data("iris")
summary(iris)
iris
iris.pca <- princomp(iris[,-5])
biplot(iris.pca)

my.rda <- rda(iris[,-5])
biplot(my.rda)

biplot(my.rda,
       display = c("sites", 
                   "species"),
       type = c("text",
                "points"))



##### JUNK CODE #####


# To see what species co-occur commonly

library(corrgram)

corrgram(preddiv[1:20],
         lower.panel = panel.pie, upper.panel = NULL,
         label.pos = c(1, 0.4),
         cex.labels = 0.6)



# Attempted NMDS #

# 1.  Directly from preddiv

pred_nmds <- metaMDS(preddivadj, k=2, zerodist='add', distance='cao') # Doesn't work
pred_nmds <- monoMDS(preddiv, k=2, zerodist='add') # Doesn't work

plot(pred_nmds, type = "t")

# 2.  With zeros removed from preddiv
rm(list=ls())
load(file='../../data/preddiv.RData')

preddiv <- preddiv %>% 
  filter_all(any_vars(. != 0))

pred_nmds <- metaMDS(preddiv, k=2, zerodist='add') # Doesn't work with Bray
pred_nmds <- metaMDS(preddiv, k=2, zerodist='add', distance="cao") # High stress (0.31) with Cao
pred_nmds
plot(pred_nmds)


# 3. Manipulate first in vegdist then remove zeros
rm(list=ls())
load(file='../../data/preddiv.RData')

# Create distance matrix first
pred.dist <- as.matrix(vegdist(preddiv[8:70], method='cao', upper=FALSE, na.rm=TRUE)) # Use "Cao" distance calculation
pred.dist <- pred.dist[rowSums(is.na(pred.dist)) == 0, colSums(is.na(pred.dist)) == 0, drop = FALSE]

pred_nmds <- metaMDS(pred.dist, k=2)
stressplot(pred_nmds)
plot(pred_nmds)

# PCoA on data
library(ape)
predPCOA <- pcoa(pred.dist)
biplot.pcoa(predPCOA)








# Remove empty rows
preddivnan <- preddiv[rowSums(preddiv[])>0,]

pred_nmds <- metaMDS(pred.dist, k=3)
stressplot(pred_nmds)

sratmax(pred_nmds)
plot(pred_nmds)


pred_nmds <- metaMDS(preddiv, k=2, method='cao')

summary(pred_nmds)
stress(pred_nmds)





# With standardization:
# Transform
predstand <- decostand(preddiv, method = 'hellinger', na.rm = TRUE)
pred_stand_nmds <- metaMDS(predstand, distance='cao', k=2)
stressplot(pred_stand_nmds)
plot(pred_stand_nmds)




pred.dist = pred.dist[rowSums(is.na(pred.dist)) == 0, colSums(is.na(pred.dist)) == 0, drop = FALSE]
pred.dist <- as.dist(pred.dist)







plot(pred_nmds)

preddiv %>% 
  sum(is.na())



# PCoA tutorial from https://ourcodingclub.github.io/tutorials/ordination/:


# Load the community dataset which we`ll use in the examples today
data(varespec)

# Open the dataset and look if you can find any patterns
View(varespec)
# It is probably very difficult to see any patterns by just looking at the data frame!

# With this command, you`ll perform a NMDS and plot the results
varespec %>%
  metaMDS(trace = F) %>%
  ordiplot(type = "none") %>%
  text("sites")

# Principle Coordinates Analysis (PCoA)

# First step is to calculate a distance matrix. 
# Here we use Bray-Curtis distance metric
dist <- vegdist(varespec,  method = "bray")

# PCoA is not included in vegan. 
# We will use the ape package instead
library(ape)
PCOA <- pcoa(dist)

# plot the eigenvalues and interpret
barplot(PCOA$values$Relative_eig[1:10])
# Can you also calculate the cumulative explained variance of the first 3 axes?

# Some distance measures may result in negative eigenvalues. In that case, add a correction:
PCOA <- pcoa(dist, correction = "cailliez")

# Plot your results
biplot.pcoa(PCOA)

# You see what`s missing? 
# Indeed, there are no species plotted on this biplot. 
# That's because we used a dissimilarity matrix (sites x sites) 
# as input for the PCOA function. 
# Hence, no species scores could be calculated. 
#However, we could work around this problem like this:
biplot.pcoa(PCOA, varespec)








