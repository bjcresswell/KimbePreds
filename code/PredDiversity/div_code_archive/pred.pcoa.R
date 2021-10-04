# First attempt at PCoA 

# Last edit 15 Sep 2021 BJC

# PCoA

# First step is to calculate a distance matrix:
predtable <- preddiv[8:70] %>% 
  filter_all(any_vars(. != 0)) # Removes empty rows so back to 99

# Can try on Hellinger transformed
hellingtable <- helling.matrix %>% 
  filter_all(any_vars(. != 0))


# Here we use Bray-Curtis distance metric
dist1 <- vegdist(predtable,  method = "bray")
dist2 <- vegdist(hellingtable,  method = "bray")





# Tutorial from https://ourcodingclub.github.io/tutorials/ordination/
# PCoA is not included in vegan. 
# We will use the ape package instead
library(ape)
PCOA <- pcoa(dist1)


PCOA$values
PCOA$vectors


# plot the eigenvalues and interpret
barplot(PCOA$values$Relative_eig[1:10])
# Can you also calculate the cumulative explained variance of the first 3 axes?

# Some distance measures may result in negative eigenvalues. In that case, add a correction:
PCOA <- pcoa(dist1, correction = "cailliez")

# Plot your results
biplot.pcoa(PCOA)

PCOAaxes <- PCOA$vectors[,c(1,2)]



# NMDS

NMDS1 <- metaMDS(comm = hellingtable, k = 2, trymax = 100, trace = F)
stressplot(NMDS1)

plot(NMDS1)
