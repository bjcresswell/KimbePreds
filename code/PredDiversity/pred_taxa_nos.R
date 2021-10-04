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
#rm(list=ls())

library(writexl)
library(readxl)
library(tidyverse)

# Start with preds object from diw
load(file='../../data/preds.RData')
preds 

# Make a count df - for each Taxa/TID combination there will be a count generated
pred.count1 <- preds %>% 
  group_by(TID, Family, Taxa) %>% # Group including family for use in taxa_nos analysis later
  dplyr::summarise(Count=sum(Number))
pred.count1 # 370 rows

# If all taxa observed on all transects then should be 63 * 5(trans) * 2(surv) * 12(site) = 7560 rows
# Need to wrangle back in by forcing at least one entry per TID/taxa combination back in:
pred.count2 <- pred.count1 %>% 
  group_by(Taxa) %>% 
  complete(TID, fill = list(Count = 0))
pred.count2 # Right number of rows but no families for a lot of the taxa

# Merge back with first df and remove any duplicates
pred.count <- tibble(merge(pred.count2[c(1,2,4)], pred.count1[2:3], by="Taxa", no.dups = TRUE, all.y = FALSE)) %>% 
  distinct()
pred.count # Now have all the zero observations back in - 7560 rows. Can conduct analysis on this (mean, SE etc)

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
TIDsum <- pred.count %>% 
  group_by(TID) %>% 
  summarise()
TIDsum # All good

# Check we have 13 families
FAMsum <- pred.count %>% 
  group_by(Family) %>% 
  summarise_each(funs(sum), Count)
FAMsum # All good

# Check total taxa observed (should be 63)
TAXsum <- pred.count %>% 
  group_by(Taxa) %>% 
  summarise() 
TAXsum # All good

# Now to figure out total abundance of each taxa across the whole of Kimbe Bay (need this for sorting data out later on)
TAXcount <- pred.count %>% 
  group_by(Taxa) %>% 
  summarise_each(funs(sum), Count) %>% 
  arrange(-Count)
TAXcount # 63 rows, one per taxa

# Total individual fish observations for the whole study - 2560
TAXcount %>% summarise_each(funs(sum), Count) 
# Or can check against preds df:
preds %>% summarise_each(funs(sum), Number) # Also 2560

# Total abundance of each predator fish family by transect (would need this to work out )
TIDfam <- pred.count %>% 
  group_by(Reeftype, Family, TID) %>% 
  summarise_each(funs(sum), Count) %>% 
  complete(TID, fill = list(Count = 0))
TIDfam # 13 families * 120 transects = 1560 rows

# Mean ± sd/se abundance of each predator fish family by reef type - used for family abundance bar plot
RTfam <- pred.count %>% 
  group_by(Family, Reeftype) %>% 
  summarise_each(funs(mean,sd,se=sd(.)/sqrt(n())), Count) %>% 
  merge(FAMsum, by = "Family") %>% 
  arrange(-Count)

RTfam # Should be 39 rows (3 reef types x 13 families)
# This df used for plotting below so reorder by abundance
RTfam$Family <- reorder(RTfam$Family, -RTfam$mean)

# Mean ± sd/se abundance of each predator fish taxa by reef type - used for taxa abundance bar plot - don't need
RTtax <- pred.count %>% 
  group_by(Family, Taxa, Reeftype) %>% 
  summarise_each(funs(sum, mean,sd,se=sd(.)/sqrt(n())), Count)
RTtax # 189 rows - 63 taxa * 3 reef types

# Create df with one row for each taxa, with mean±SE - using pivot_wider in dplyr
taxabun <- RTtax %>% 
  pivot_wider(id_cols = c(Family, Taxa), names_from = Reeftype, values_from = c(mean, se))
taxabun

# Merge with total abundance (across whole study site) by taxa
taxabunsum <- tibble(merge(taxabun, TAXcount, by = "Taxa"))
taxabunsum

# Can merge this with the df from the multivariate abundance analysis - with LRT and P values for taxa driving differences
# Load
taxaLRT <- read_excel("../../output/rtables/taxa_LRT_mvabund.xlsx", 1)
load(file = "../../data/mvabund_unitests.RData")
# and combine
taxabunstat <- tibble(merge(taxabunsum, unitests, by = "Taxa"))
taxabunstat

# Merge with trophic group information
# First extract from the preds df
trophic <- preds %>% 
  group_by(Taxa, `Trophic Group`) %>% 
  summarise()

# and combine
taxabunstat <- tibble(merge(taxabunstat, trophic, by = "Taxa")) %>% 
  arrange(-Count)

#write_xlsx(taxabunstat[c(1,2,12,3,6,4,7,5,8:11)], "../../output/rtables/taxa_abun_by_reeftype.xlsx")


# Plots

# 1. By family - not going to run this for final analysis.
#rtfambar <- ggplot(data=RTfam[1:27,], aes(x=Reeftype, y=mean, fill=Reeftype))+
#  geom_bar(stat="identity", colour="black", width=0.8, position=position_dodge(0.8))+
#  scale_fill_manual(values=c("#35978F","#436EEE","#DFC27D"))+
#  theme_classic()+
#  facet_wrap(~Family, scales = 'free', ncol = 3)+
#  geom_errorbar(aes(ymin=mean, ymax=mean+se), width=.2,
#                position=position_dodge(0.8))+
#  ylab('Mean predator family abundance (±SE)')+
#  xlab('')+
#  #geom_text(aes( label = mean, y = mean ), vjust = 1.5, size = 3, color = "white" )+
#  theme(axis.title.y = element_text(family="Arial", size=14, colour = 'black', vjust = 1.8),
#        axis.text = element_text(family="Arial", size= 9, colour = 'black'))+
#  theme(strip.text.x = element_text(family="Arial", size = 14, color = "black", hjust = -0.02))+
#  theme(strip.background = element_rect(color="white", size=1.5, linetype="solid"))+
#  theme(legend.position = 'none')

#rtfambar # Probably not as punchy as presenting top most 12 abundant individual taxa (and can include those 4 most driving differences)...

# 2a. By taxa - top 20 most abundant - not going to use this
# Filter out top 20 most abundance taxa:
#top20 <- tibble(TAXcount[1:20,])
#RT20abun <- tibble(merge(RTtax, top20, by = "Taxa"))
# And reorder based on abundance
#RT20abun$Taxa <- reorder(RT20abun$Taxa, -RT20abun$Count)

# Top 20 most abundant taxa plot
#rt20abunbar <- ggplot(data=RT20abun, aes(x=Reeftype, y=mean, fill=Reeftype))+
#  geom_bar(stat="identity", colour="black", width=0.8, position=position_dodge(0.8))+
#  scale_fill_brewer(palette="Greys", direction=-1)+
#  theme_classic()+
#  facet_wrap(~factor(Taxa), scales = 'free', ncol = 5)+
#  geom_errorbar(aes(ymin=mean, ymax=mean+se), width=.2,
#                position=position_dodge(0.8))+
#  ylab('Mean predator species abundance (±SE)')+
#  xlab('')+
#  #geom_text(aes( label = mean, y = mean ), vjust = 1.5, size = 3, color = "white" )+
#  theme(axis.title.y = element_text(family="Arial", size=14, colour = 'black', vjust = 1.8),
#        axis.text = element_text(family="Arial", size= 9, colour = 'black'))+
#  theme(strip.text.x = element_text(family="Arial", face = "italic", size = 14, color = "black", hjust = -0.02))+
#  theme(strip.background = element_rect(color="white", size=1.5, linetype="solid"))+
#  theme(legend.position = 'none')

#rt20abunbar


# 2b. By taxa - only the significant drivers of community difference
# Filter out significant taxa:
RTsig <- tibble(merge(RTtax, unitests, by = "Taxa")) %>%
  filter(Pvals <= 0.05)


RTsig

# And reorder based on abundance
RTsig$Taxa <- reorder(RTsig$Taxa, -RTsig$sum)


# Top 20 most abundant taxa plot
rtsigbar <- ggplot(data=RTsig, aes(x=Reeftype, y=mean, fill=Reeftype))+
  geom_bar(stat="identity", colour="black", width=0.8, position=position_dodge(0.8))+
  scale_fill_manual(values=c("#35978F","#436EEE","#DFC27D"))+
  theme_classic()+
  facet_wrap(~factor(Taxa), scales = 'free', ncol = 3)+
  geom_errorbar(aes(ymin=mean, ymax=mean+se), width=.2,
                position=position_dodge(0.8))+
  ylab('Mean predator species abundance (±SE)')+
  xlab('')+
  #geom_text(aes( label = mean, y = mean ), vjust = 1.5, size = 3, color = "white" )+
  theme(axis.title.y = element_text(family="Arial", size=14, colour = 'black', vjust = 1.8),
        axis.text = element_text(family="Arial", size= 9, colour = 'black'))+
  theme(strip.text.x = element_text(family="Arial", face = "italic", size = 14, color = "black", hjust = -0.02))+
  theme(strip.background = element_rect(color="white", size=1.5, linetype="solid"))+
  theme(legend.position = 'none')

rtsigbar


 # Save
#ggsave(rtfambar, filename= '../../output/rfigures/family_bar_fig.png', width=11,  height=7, dpi = 1000 )
#ggsave(rt20abunbar, filename= '../../output/rfigures/taxa_topabun20_bar_fig.png', width=12,  height=6, dpi = 1000 )
ggsave(rtsigbar, filename= '../../output/rfigures/taxa_sig_bar_fig.svg', width=12,  height=6, dpi = 1000 )

