# Chapter 1 - Fish community ordination and tests

# Data - Ch 1 Fish Data

fish<-Chapter.1.Fish.Data
fish

# Packages
library(dplyr)
library(vegan)
library(ggplot2)
library(devtools)
library(ggpubr)
library(tidyverse)
library(ggplot2)

# Remove transect column

fish <- fish[ -c(1) ] 
fish

# Arrange sites in a better order
# create a vector with letters in the desired order
fish.order <- c("Bradford", "Joelles", "Inglis", "Kimbe Bomie","Ema","Ottos","Hogu","Kimbe Island", "Donnas", "Susans", "Lady Di","Madaro")
# mutate the data frame
fish.ordered <- fish %>%
  mutate(SITE=factor(SITE, levels = fish.order)) %>%
  arrange(SITE)
View(fish.ordered)
fish<-fish.ordered


# Species abundances are columns 4:233,this exclues transect site and reeftype use to create a species matrix

fish.matrix <- fish[,3:232]


# Hellinger transform

fish.trans <- decostand(fish.matrix, "hellinger")


#metaMDS wrapper, does transformations and applies dissimilarity matrix
# turn off auto transform to stop the default wisconson double
fish.nmds<-metaMDS(fish.trans,k=2,trymax=500,trace=F, autotransform = F) #k = # of dimensions you want for the solution, trymax is the max number of iterations to try to converge on a solution with the lowest stress
fish.nmds 
stressplot(fish.nmds,main="Sheppard Plot")    
gof=goodness(fish.nmds)
plot(fish.nmds, type="t", main="Goodness of Fit")
points(fish.nmds, display = "sites", cex=gof*2)
ordiplot(fish.nmds,type="text")  


# Using the rep function we can assign rows into groups, whether thats by site name or reeftype

Site=c(rep("BRAD", 5),rep("JOELLES",5), rep("INGLIS",5), rep("KIMBO", 5), rep("EMA",5), rep("OTTOS",5),
       rep("HOGU",5),rep("KIMIS",5),rep("DONNA", 5), rep("SUSA", 5), rep("LADI", 5), rep("MADA",5))
Site

Reef=c(rep("Pinnacle",20), rep("Offshore",20), rep("Nearshore", 20))
Reef

# to use ggplot we need to extract the x and y NMDS scores for both sites and species

data.scores <- as.data.frame(scores(fish.nmds))  #Using the scores function from vegan to extract the site scores and convert to a data.frame
data.scores$Site <- rownames(data.scores)  # create a column of site names, from the rownames of data.scores2
data.scores$Site <- Site  #  add the sites variable created earlier
data.scores$Reef <- rownames(data.scores) # and for reef types
data.scores$Reef <- Reef
head(data.scores)  #look at the data


species.scores <- as.data.frame(scores(fish.nmds, "species"))  #Using the scores function from vegan to extract the species scores and convert to a data.frame
species.scores$species <- rownames(species.scores)  # create a column of species, from the rownames of species.scores
head(species.scores) 



#Add Hulls

Pinnacle <- data.scores[data.scores$Reef == "Pinnacle", ][chull(data.scores[data.scores$Reef == 
                                                                              "Pinnacle", c("NMDS1", "NMDS2")]), ]  # hull values for grp A
Offshore<-data.scores[data.scores$Reef == "Offshore", ] [chull(data.scores[data.scores$Reef == 
                                                                             "Offshore", c("NMDS1", "NMDS2")]), ]
Nearshore <- data.scores[data.scores$Reef == "Nearshore", ][chull(data.scores[data.scores$Reef == 
                                                                                "Nearshore", c("NMDS1", "NMDS2")]), ]  # hull values for grp B

hull.data <- rbind(Pinnacle, Offshore, Nearshore)  #combine Seamounts, Offshore and Nearshore reef types
hull.data


ggplot(data.scores, aes(x=NMDS1, y=NMDS2, col=Reef)) +
  geom_point(data=data.scores,aes(x=NMDS1,y=NMDS2,shape=Reef),size=4) +
  scale_shape_manual(values=c(16, 8, 17)) +
  scale_color_manual(values=c("#DFC27D", "#436EEE", "#35978F"))+
  labs(col="Reef Habitat Type", shape="Reef Habitat Type") +
  ggtitle("Stress 0.2")+
  xlab("NMDS1")+
  ylab("NMDS2")+
  coord_equal()+
  theme_classic()+
  theme(
    plot.title = element_text(hjust = 1, size = 10,face = "bold"), 
    axis.ticks = element_line(size=1), 
    axis.ticks.length = unit(4, "pt"),
    axis.title=element_text(colour="black", size = 14, face = "bold"),
    axis.text.x = element_text(size=12, face="bold", colour = "black"),
    axis.text.y = element_text(size=12, face="bold", colour = "black"),
    legend.title = element_text(color = "black", size = 12, face = "bold"),
    legend.text = element_text(size=12),
    legend.position=("bottom"),
    legend.box.spacing = unit(1, "cm"))


# Use to exapnd axis limits
expand_limits(x=c(0,1.5), y=c(0, 1.5))+

# With Hulls


ggplot(data.scores, aes(x=NMDS1, y=NMDS2, col=Reef)) +
  geom_polygon(data=hull.data,aes(x=NMDS1,y=NMDS2,group=Reef, fill=Reef),alpha=0.30)+
  geom_point(data=data.scores,aes(x=NMDS1,y=NMDS2,shape=Reef),size=4) +
  scale_shape_manual(values=c(16, 8, 17)) +
  scale_color_manual(values=c("#DFC27D", "#436EEE", "#35978F"))+
  labs(col="Reef Habitat Type", shape="Reef Habitat Type") +
  ggtitle("Stress 0.2")+
  xlab("NMDS1")+
  ylab("NMDS2")+
  coord_equal()+
  theme_classic()+
  theme(
    plot.title = element_text(hjust = 1, size = 10,face = "bold"), 
    axis.ticks = element_line(size=1), 
    axis.ticks.length = unit(4, "pt"),
    axis.title=element_text(colour="black", size = 14, face = "bold"),
    axis.text.x = element_text(size=12, face="bold", colour = "black"),
    axis.text.y = element_text(size=12, face="bold", colour = "black"),
    legend.title = element_text(color = "black", size = 12, face = "bold"),
    legend.text = element_text(size=12),
    legend.position=("bottom"),
    legend.box.spacing = unit(1, "cm"))


# With Ellipses
ggplot(data.scores, aes(x=NMDS1, y=NMDS2, col=Reef)) +
  geom_point(data=data.scores,aes(x=NMDS1,y=NMDS2,shape=Reef),size=4) +
  scale_shape_manual(values=c(16, 8, 17)) +
  scale_color_manual(values=c("#DFC27D", "#436EEE", "#35978F"))+
  stat_ellipse (show.legend = FALSE)+
  labs(col="Reef Habitat Type", shape="Reef Habitat Type") +
  ggtitle("Stress 0.2")+
  xlab("NMDS1")+
  ylab("NMDS2")+
  coord_equal()+
  theme_classic()+
  theme(
    plot.title = element_text(hjust = 1, size = 10,face = "bold"), 
    axis.ticks = element_line(size=1), 
    axis.ticks.length = unit(4, "pt"),
    axis.title=element_text(colour="black", size = 14, face = "bold"),
    axis.text.x = element_text(size=12, face="bold", colour = "black"),
    axis.text.y = element_text(size=12, face="bold", colour = "black"),
    legend.title = element_text(color = "black", size = 12, face = "bold"),
    legend.text = element_text(size=12),
    legend.position=("bottom"),
    legend.box.spacing = unit(1, "cm"))

# Adding species vectors

library(vegan)
library(ggplot2)
library(grid)


# The distance matrix
fish.matrix
# The nmds
fish.nmds

scrs.fish <- as.data.frame(scores(fish.nmds, display = "sites"))
scrs.fish <- cbind(scrs.fish, Group = c(rep("Seamount",20), rep("Offshore",20), rep("Nearshore", 20)))


set.seed(123)
vf.fish <- envfit(fish.nmds, fish.matrix, perm = 999)
vf.fish

spp.scrs.fish <- as.data.frame(scores(vf.fish, display = "vectors"))
spp.scrs.fish <- cbind(spp.scrs.fish, Species = rownames(spp.scrs.fish))


# JUST TOP MOST CORRELATED FISH SPECIES lowest p values

#only significant pvalues

#shortcutting ef$vectors
A <- as.list(vf.fish$vectors)
#creating the dataframe
pvals<-as.data.frame(A$pvals)
arrows<-as.data.frame(A$arrows*sqrt(A$r))
C<-cbind(arrows, pvals)
#subset
Cred<-subset(C,pvals<0.007)
Cred <- cbind(Cred, Species = rownames(Cred))
# "Cred "can now be implemented in the geom_segment-argument as discussed above.
Cred


library(ggrepel)

ggplot(scrs.fish) +
  geom_point(data=data.scores,aes(x=NMDS1,y=NMDS2,shape=Reef, col=Reef),size=3) +
  scale_shape_manual(values=c(16, 8, 17)) +
  scale_color_manual(values=c("#DFC27D", "#436EEE", "#35978F")) +
  geom_segment(data = Cred,
               aes(x = 0, xend = NMDS1, y = 0, yend = NMDS2),
               arrow = arrow(length = unit(0.25, "cm")), colour = "grey") +
  geom_text_repel(data = Cred, aes(x = NMDS1, y = NMDS2, label = Species),
                  size = 3) +
  labs(col="Reef Type", shape="Reef Type") +
  theme(legend.position = "none")+
  coord_equal()+
  theme_classic() +
  labs(title = "NMDS Plot Stress = 0.2")


#Just the biplot
ggplot(scrs.fish) +
  geom_segment(data = Cred,
               aes(x = 0, xend = NMDS1, y = 0, yend = NMDS2),
               arrow = arrow(length = unit(.8, "cm"),), colour = "#36648B") +
  geom_text_repel(data = Cred, aes(x = NMDS1, y = NMDS2, label = Species),
                  size = 7) +
  theme_void() + 
  theme(legend.position="none")

# biplot no text
ggplot(scrs) +
  geom_segment(data = Cred,
               aes(x = 0, xend = NMDS1, y = 0, yend = NMDS2),
               arrow = arrow(length = unit(.8, "cm"),), colour = "#36648B") +
  theme_void() + 
  theme(legend.position="none")



# PERMANOVA
adonis_fish <- adonis(fish.trans ~ REEFTYPE, fish)
adonis_fish
# Testing homogeneity of group dispersion
## Calculate multivariate dispersions
fish.dist<-vegdist(fish.trans)
mod.fish <- betadisper(fish.dist, Reef)
mod.fish
## Perform test
anova(mod.fish)

# Pairwise tests
# install_github("pmartinezarbizu/pairwiseAdonis/pairwiseAdonis")

library(pairwiseAdonis)
data(fish)
pairwise.adonis(fish[,3:230],fish$REEFTYPE)


# SIMPER

data(fish.trans)
data(fish)
(sim.fish <- with(fish, simper(fish.trans, REEFTYPE)))
summary(sim.fish)
str(sim.fish)
view(sim.fish)

# Mean abundances across reef types

aggregate(fish[, 3:230], list(fish$REEFTYPE), mean)
mean.abundances<-aggregate(fish[, 3:230], list(fish$REEFTYPE), mean)
mean.abundances$Pseudanthias.tuka
mean.abundances$Acanthurus.thompsoni
mean.abundances$Amblyglyphidodon.aureus
mean.abundances$Caesio.cuning
mean.abundances$Pterocaesio.tile
mean.abundances$Chromis.amboinensis
mean.abundances$Pomacentrus.nigromanus
mean.abundances$Ctenochaetus.tominiensis
mean.abundances$Chrysiptera.parasema



#order the fish vectors by the p value# NOT SURE THIS WORKS 
ordered.vf<-as.data.frame(vf.fish)
top.vf.fish<-spp.scrs.fish %>% arrange(desc(pvals))


# reaaply 15 top fish species to the scores
top.scrs.fish <- as.data.frame(scores(top.vf.fish, display = "vectors"))
top.scrs.fish <- cbind(top.scrs.fish, Species = rownames(top.scrs.fish))


