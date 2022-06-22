# Script to analyse abundance by taxa (and/or family) by reef type

# Last edit: 19 Sep 2021 BJC

# This is to accompany the mvabund results which report the taxa which driving significant community composition differences:
# 1. Caranx melampygus
# 2. Macolor macularis
# 3. Sphyraena qenie
# 4. Caranx sexfasciatus
# 5. Lutjanus gibbus
# 6. Cephalopholis cyanostigma


# The back half of this script imports the mvabund p.uni output file and merges with the taxa abundances
# Creates one file with all the individual abundance data in it (plus LRT and p vals)
# Also used to create abundance by family bar graphs - not sure if it should be this or top 10 pred spp? Ask Gem.

# Housekeeping
setwd("/Users/bjcresswell/OneDrive - James Cook University/Ben PhD/Data & analysis/KimbePreds/code/PredDiversity")
rm(list=ls())

library(writexl)
library(readxl)
library(tidyverse)

# Start with preds object from diw
load(file='../../data/preds.RData')
preds 

# Make a count df - for each Taxa/TID combination there will be a count generated
pred.count1 <- preds %>% 
  group_by(TID, Taxa) %>% # Group including family for use in taxa_nos analysis later
  dplyr::summarise(Count=sum(Number))

pred.count1 # 370 rows

# If all taxa observed on all transects then should be 7560 rows -> 63 * 5(trans) * 2(surv) * 12(site)
# Need to wrangle back in by forcing at least one entry per TID/taxa combination back in:
pred.count2 <- pred.count1 %>% 
  group_by(Taxa) %>% 
  complete(TID, fill = list(Count = 0))
pred.count2 # Right number of rows but no families for a lot of the taxa

# Merge back with first df and remove any duplicates
pred.count <- tibble(merge(pred.count2[c(1,2,5)], pred.count1[2:4], by="TID", no.dups = TRUE, all.y = FALSE)) %>% 
  distinct()
pred.count # Now have all the zero observations back in - 7560 rows. Can conduct analysis on this (mean, SE etc)




pred.count <- tibble(merge(pred.count2, preds[c(1,8)], by="Taxa", no.dups = TRUE, all.y = FALSE)) %>% 
  distinct()





# Now wrangle back in some useful columns
pred.count <- pred.count %>% 
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

pred.count


# Checks

# Check we have 120 Transects represented (12*2*5)
pred.count %>% 
  group_by(TID) %>% 
  dplyr::summarise()

# Check we have 13 families
pred.count %>% 
  group_by(Family) %>% 
  summarise_each(funs(sum), Count)

# Check total taxa observed (should be 63)
pred.count %>% 
  group_by(Taxa) %>% 
  dplyr::summarise() 

# Now to figure out total abundance of each taxa across the whole of Kimbe Bay (need this for sorting data out later on)
TAXcount <- pred.count %>% 
  group_by(Taxa) %>% 
  summarise_each(funs(sum), Count) %>% 
  arrange(-Count)
TAXcount # 63 rows, one per taxa

# Total individual fish observations for the whole study - 2560
TAXcount %>% 
  summarise_each(funs(sum), Count) 

# Or can check against preds df:
preds %>% 
  summarise_each(funs(sum), Number) # Also 2560



# Mean/SD etc for each taxa by reeftype

RTtax <- pred.count %>% 
  group_by(Family, Taxa, Reeftype) %>% 
  summarise_each(funs(sum, mean,sd,se=sd(.)/sqrt(n())), Count)
RTtax # 189 rows - 63 taxa * 3 reef types



# This for all preds - can merge  with the  LRT and P values from the multivariate abundance analysis to pull out taxa driving differences
 
# Load file
load(file = "../../data/mvabund_unitests.RData")

# Merge
RTsig <- tibble(merge(RTtax, unitests, by = "Taxa")) %>%
  filter(Pvals <= 0.05)


RTsig

# And reorder based on abundance
RTsig$Taxa <- reorder(RTsig$Taxa, -RTsig$sum)


# Plot

library(cowplot)

theme_set(theme_cowplot())

sigtaxaplot <- 
ggplot(data=RTsig, aes(x=Reeftype, y=mean, fill=Reeftype))+
  geom_linerange(aes(ymin=mean-se, ymax=mean+se), width=.2)+ # The width argument is if you want to use geom_errorbar
  geom_point(aes(shape = Reeftype),stat="identity", colour="black", size = 5)+
  scale_shape_manual(values=c(24, 21, 22))+                       # had to adapt Kimbe pch scheme
  scale_fill_manual(values=c("#35978F","#436EEE","#DFC27D"))+
  theme_classic()+
  theme(axis.line = element_blank(), panel.border = element_rect(size = 1, fill = "transparent")) +
  facet_wrap(~factor(Taxa), scales = 'free', ncol = 3) +
  ylab('Mean predator fish species abundance (±SE)')+
  xlab('')+
  #geom_text(aes( label = mean, y = mean ), vjust = 1.5, size = 3, color = "white" )+
  theme(axis.title.y = element_text(family="Helvetica", size=10, colour = 'black', vjust = 1.8),
        axis.text = element_text(family="Helvetica", size= 9, colour = 'black'))+
  theme(strip.text.x = element_text(family="Helvetica", face = "italic", size = 10, color = "black", hjust = -0.02))+
  theme(strip.background = element_rect(color="white", size=1.5, linetype="solid"))+
  theme(legend.position = 'none')+
  guides(colour = FALSE, fill = FALSE, shape = FALSE)#+  
  #theme_cowplot()
  
  

sigtaxaplot


# Save
# Coral reefs dims: Width max: 174mm Height max; 234mm
ggsave(sigtaxaplot, filename= '../../output/rfigures/sigtaxaplot_box.png', width = 174, height = 100, units = "mm", dpi = 1000 )






library(cowplot)
library(magick)

ggdraw() +
  draw_image("../../data/barracuda", x = 1, y = 1, scale = 1) +
  draw_plot(sigtaxaplot)




 # Save
#ggsave(rt20abunbar, filename= '../../output/rfigures/taxa_topabun20_bar_fig.png', width=12,  height=6, dpi = 1000 )

