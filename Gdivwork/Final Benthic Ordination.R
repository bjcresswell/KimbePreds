
#Benthic nMDS Script

# Packages
library(dplyr)
library(vegan)
library(ggplot2)
library(devtools)
library(ggpubr)
library(tidyverse)
library(ggplot2)

# Data used = Broad Scale Benthic CSV. Counts not percentages
benthic<-Broad.Scale.Benthic

# Remove transect column and Tuare

benthic <- benthic[ -c(3) ] 
benthic<-benthic[-c(56:60),]

# Arrange sites in a better order
# create a vector with letters in the desired order
site.order<- c("BRADFORD", "JOELLES", "INGLIS", "KIMBOM","EMA","OTTOS","HOGU","KIMISLAND", "DONNA", "SUSANS", "LADYDI","MADARO")
# mutate the data frame
benthic.ordered <- benthic %>%
  mutate(SITE=factor(SITE, levels = site.order)) %>%
  arrange(SITE)
View(benthic.ordered)
benthic<-benthic.ordered


#Or another way - make sure spelling and caps match
# Sort the order of sites
percent.data$SITE <- factor(percent.data$SITE, levels =c("MADARO","LADYDI","SUSANS","DONNA","TUARE",
                                                         "KIMISLAND","OTTOS","HOGU","EMA",
                                                         "BRADFORD","JOELLES","INGLIS","KIMBOM"))

# Species abundances are columns 4:233,this exclues transect site and reeftype use to create a species matrix

ab.matrix <- benthic[,3:16]


# Hellinger transform

sptrans.2 <- decostand(ab.matrix, "hellinger")


#metaMDS wrapper, does transformations and applies dissimilarity matrix
# turn off auto transform to stop the default wisconson double
benthic.nmds<-metaMDS(sptrans.2,k=2,trymax=500,trace=F, autotransform = F) #k = # of dimensions you want for the solution, trymax is the max number of iterations to try to converge on a solution with the lowest stress
benthic.nmds

stressplot(benthic.nmds,main="Sheppard Plot")    
gof=goodness(benthic.nmds)
plot(benthic.nmds, type="t", main="Goodness of Fit")
points(benthic.nmds, display = "sites", cex=gof*2)
ordiplot(benthic.nmds,type="text")  


# Using the rep function we can assign rows into groups, whether thats by site name or reeftype

Site=c(rep("BRAD", 5),rep("JOELLES",5), rep("INGLIS",5), rep("KIMBO", 5), rep("EMA",5), rep("OTTOS",5),
       rep("HOGU",5),rep("KIMIS",5),rep("DONNA", 5), rep("SUSA", 5), rep("LADI", 5), rep("MADA",5))
Site

Reef=c(rep("Pinnacle",20), rep("Offshore",20), rep("Nearshore", 20))
Reef

# to use ggplot we need to extract the x and y NMDS scores for both sites and species

data.scores <- as.data.frame(scores(benthic.nmds))  #Using the scores function from vegan to extract the site scores and convert to a data.frame
data.scores$Site <- rownames(data.scores)  # create a column of site names, from the rownames of data.scores2
data.scores$Site <- Site  #  add the sites variable created earlier
data.scores$Reef <- rownames(data.scores) # and for reef types
data.scores$Reef <- Reef
head(data.scores)  #look at the data


species.scores <- as.data.frame(scores(benthic.nmds, "species"))  #Using the scores function from vegan to extract the species scores and convert to a data.frame
species.scores$species <- rownames(species.scores)  # create a column of species, from the rownames of species.scores
head(species.scores) 


# Customise shapes for each site

ggplot() + 
  geom_point(data=data.scores,aes(x=NMDS1,y=NMDS2,shape=Site,colour=Site),size=3) + # add the point markers
  scale_shape_manual(values=c(17, 3, 15, 18,12,7,3,19,16,4,8,1,2)) +
  coord_equal() +
  theme_classic()

#Add Hulls

Pinnacle <- data.scores[data.scores$Reef == "Pinnacle", ][chull(data.scores[data.scores$Reef == 
                                                                               "Pinnacle", c("NMDS1", "NMDS2")]), ]  # hull values for grp A
Offshore<-data.scores[data.scores$Reef == "Offshore", ] [chull(data.scores[data.scores$Reef == 
                                                                             "Offshore", c("NMDS1", "NMDS2")]), ]
Nearshore <- data.scores[data.scores$Reef == "Nearshore", ][chull(data.scores[data.scores$Reef == 
                                                                                "Nearshore", c("NMDS1", "NMDS2")]), ]  # hull values for grp B

hull.data <- rbind(Pinnacle, Offshore, Nearshore)  #combine Seamounts, Offshore and Nearshore reef types
hull.data

ggplot() + 
  geom_polygon(data=hull.data,aes(x=NMDS1,y=NMDS2,fill=Reef,group=Reef),alpha=0.30)+
  geom_point(data=data.scores,aes(x=NMDS1,y=NMDS2,shape=Site,colour=Site),size=3) + # add the point markers
  scale_shape_manual(values=c(17, 3, 15, 18,12,7,3,19,16,4,8,1,2)) +
  coord_equal() +
  theme_classic()

ggplot() + 
  geom_polygon(data=hull.data,aes(x=NMDS1,y=NMDS2,fill=Reef,group=Reef),alpha=0.30)+
  geom_point(data=data.scores,aes(x=NMDS1,y=NMDS2,shape=Reef),size=3) + # add the point markers
  scale_shape_manual(values=c(1, 8, 17)) +
  coord_equal() +
  labs(fill="Reef Type", shape="Reef Type")+
  ggtitle("2D Stress 0.2")+
  theme_classic()


ggplot(data.scores, aes(x=NMDS1, y=NMDS2, col=Reef)) +
  geom_point(data=data.scores,aes(x=NMDS1,y=NMDS2,shape=Reef),size=3, color="black") +
  scale_shape_manual(values=c(1, 8, 17)) +
  labs(col="Reef Type", shape="Reef Type") +
  stat_ellipse (show.legend = FALSE) +
  coord_equal()+
  theme_classic() +
  labs(title = "NMDS Plot Stress = 0.2")

ggplot(data.scores, aes(x=NMDS1, y=NMDS2, col=Reef)) +
  geom_point(data=data.scores,aes(x=NMDS1,y=NMDS2,shape=Reef),size=3) +
  scale_shape_manual(values=c(1, 8, 17)) +
  labs(col="Reef Type", shape="Reef Type") +
  stat_ellipse (show.legend = FALSE) +
  coord_equal()+
  theme_classic() +
  labs(title = "NMDS Plot Stress = 0.2")


ggplot(data.scores, aes(x=NMDS1, y=NMDS2, col=Reef)) +
  geom_point(data=data.scores,aes(x=NMDS1,y=NMDS2,shape=Reef),size=3) +
  scale_shape_manual(values=c(16, 8, 17)) +
  labs(col="Reef Type", shape="Reef Type") +
  coord_equal()+
  theme_classic() +
  labs(title = "NMDS Plot Stress = 0.2")

library(RColorBrewer)
display.brewer.all()
ggplot(data.scores, aes(x=NMDS1, y=NMDS2, col=Reef)) +
  geom_point(data=data.scores,aes(x=NMDS1,y=NMDS2,shape=Reef),size=3) +
  scale_shape_manual(values=c(16, 8, 17)) +
  scale_color_brewer(palette="BrBG")+
  labs(col="Reef Type", shape="Reef Type") +
  coord_equal()+
  theme_classic() +
  labs(title = "NMDS Plot Stress = 0.2")

# Hexadecimal color specification 
brewer.pal(n = 8, name = "BrBG")
[1] "#8C510A" "#BF812D" "#DFC27D" "#F6E8C3" "#C7EAE5" "#80CDC1" "#35978F" "#01665E"

  
#colourpicker
mycols<-c("#DEB887", "#528B8B", "#473C8B")

ggplot(data.scores, aes(x=NMDS1, y=NMDS2, col=Reef)) +
  geom_point(data=data.scores,aes(x=NMDS1,y=NMDS2,shape=Reef),size=4) +
  scale_shape_manual(values=c(16, 8, 17)) +
  scale_color_manual(values=c("#DFC27D", "#436EEE", "#35978F"))+
  labs(col="Reef Habitat Type", shape="Reef Habitat Type") +
  ggtitle("Stress 0.2")+
  xlab("NMDS1")+
  ylab("NMDS2")+
  coord_equal()+
  expand_limits(x=c(0,1), y=c(0, 1))+
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
sptrans.2
# The nmds
benthic.nmds

scrs <- as.data.frame(scores(benthic.nmds, display = "sites"))
scrs <- cbind(scrs, Group = c(rep("Seamount",20), rep("Offshore",20), rep("Nearshore", 20)))

set.seed(123)
vf <- envfit(benthic.nmds, sptrans.2, perm = 999)
vf

spp.scrs <- as.data.frame(scores(vf, display = "vectors"))
spp.scrs <- cbind(spp.scrs, Species = rownames(spp.scrs))


  ggplot(scrs) +
    geom_point(data=data.scores,aes(x=NMDS1,y=NMDS2,shape=Reef, col=Reef),size=3) +
    scale_shape_manual(values=c(16, 8, 17)) +
    scale_color_manual(values=c("#DFC27D", "#436EEE", "#35978F")) +
  geom_segment(data = spp.scrs,
               aes(x = 0, xend = NMDS1, y = 0, yend = NMDS2),
               arrow = arrow(length = unit(0.25, "cm")), colour = "grey") +
  geom_text_repel(data = spp.scrs, aes(x = NMDS1, y = NMDS2, label = Species),
            size = 3) +
    labs(col="Reef Type", shape="Reef Type") +
    theme(legend.position = "none")+
    coord_equal()+
    theme_classic() +
    labs(title = "NMDS Plot Stress = 0.2")
    

#Just the biplot
  ggplot(scrs) +
    geom_segment(data = spp.scrs,
                 aes(x = 0, xend = NMDS1, y = 0, yend = NMDS2),
                 arrow = arrow(length = unit(.8, "cm"),), colour = "#36648B") +
    geom_text_repel(data = spp.scrs, aes(x = NMDS1, y = NMDS2, label = Species),
              size = 7) +
    theme_void() + 
    theme(legend.position="none")
  
# Biplot no text
  
  ggplot(scrs) +
    geom_segment(data = spp.scrs,
                 aes(x = 0, xend = NMDS1, y = 0, yend = NMDS2),
                 arrow = arrow(length = unit(.8, "cm"),), colour = "#36648B") +
    theme_void() + 
    theme(legend.position="none")
  
 
  
  # Move text away from arrows
  install.packages("ggrepel") 
  library(ggrepel)
  
  ggplot(scrs) +
    geom_point(data=data.scores,aes(x=NMDS1,y=NMDS2,shape=Reef, col=Reef),size=3) +
    scale_shape_manual(values=c(16, 8, 17)) +
    scale_color_manual(values=c("white", "white", "white")) +
    geom_segment(data = spp.scrs,
                 aes(x = 0, xend = NMDS1, y = 0, yend = NMDS2),
                 arrow = arrow(length = unit(0.25, "cm")), colour = "grey") +
    geom_text_repel(data = spp.scrs, aes(x = NMDS1, y = NMDS2, label = Species),
              size = 4) +
    coord_equal()+
    theme_classic()
   
  
# PERMANOVA
adonis_REEFTYPE <- adonis(sptrans.2 ~ REEFTYPE, benthic.2)
adonis_REEFTYPE
# Testing homogeneity of group dispersion
## Calculate multivariate dispersions
sptrans.dist<-vegdist(sptrans.2)
mod <- betadisper(sptrans.dist, Reef)
mod
## Perform test
anova(mod)

# Pairwise tests
# install_github("pmartinezarbizu/pairwiseAdonis/pairwiseAdonis")

library(pairwiseAdonis)
data(benthic.2)
pairwise.adonis(benthic.2[,3:16],benthic.2$REEFTYPE)


# SIMPER

data(sptrans.2)
data(benthic.2)
(sim <- with(benthic.2, simper(sptrans.2, REEFTYPE)))
summary(sim)
str(sim)
view(sim)
# ANOSIM

anosim_reeftype = anosim(sptrans,benthic$REEFTYPE)
anosim_reeftype # take a look at results
summary(anosim_reeftype)
plot(anosim_reeftype)
