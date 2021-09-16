


load('data/fish.Rdata')
glimpse(fish)



preds1 <- fish %>%
  group_by(Site, Transect, SurvCode) %>% 
  filter(Family=="Carangidae" |
           Family=="Scombridae" |
           Family== "Lethrinidae" |
           Family == "Lutjanidae" |
           Family == "Sphyraenidae" |
           Family == "Serranidae"|
           Family == "Carcharhinidae", preserve=TRUE)
head(preds1)

# Now filter out any strange taxa that have snuck in:
preds1 <- preds1 %>%
  group_by(Site, Transect, SurvCode) %>% 
  filter(Genus !="Pseudanthias", preserve = TRUE)#





fish %>% anti_join(preds)

preddiv <- preds %>% 
  group_by("Site", "Transect", "SurvCode") %>% 
  pivot_wider(names_from = Taxa, values_from = Number)

preddiv


summarise(preds$TID)
load(file='data/preds.RData')

fishcol <- fish[6]
fishcol <- fish[c(2,4,5)]

predsID <- preds$TID %>% 
  unique() #%>% 
  
predsID

length()

# Trying compare:

library(compare)
install.packages("compare")


aj <- anti_join(fish, preds)

aj$TID


le

comparison <- compare(fish, preds, allowAll = T)

difference <-
  data.frame(lapply(1:ncol(fish),function(i)setdiff(fish[,i],comparison$tM[,i])))
colnames(difference) <- colnames(fish)
difference

comparison$tC
comparison$tM

head(preds)
(preds)


tally(preds$TID)






preddiv <- tibble(merge(fishcol, preds, by="TID", all = T)) #%>%  # Merge back together and keep all 120 rows


preddiv <- tibble(merge(preds, fishcol, by=c("Site", "Transect", "SurvCode"), all = F)) #%>%  # Merge back together and keep all 120 rows
  #replace(is.na(.), 0) %>%                                            # Remove NAs in whole df and set to zero
  mutate(Reeftype = case_when(grepl("BRAD", `SiteCode`) ~ "Pinnacle",      # Assign reef type status
                              grepl("JOEL", `SiteCode`) ~ "Pinnacle",
                              grepl("KBOM", `SiteCode`) ~ "Pinnacle",
                              grepl("INGL", `SiteCode`) ~ "Pinnacle",
                              grepl("LADI", `SiteCode`) ~ "Nearshore",
                              grepl("MADA", `SiteCode`) ~ "Nearshore",
                              grepl("SUSA", `SiteCode`) ~ "Nearshore",
                              grepl("DON", `SiteCode`) ~ "Nearshore",
                              grepl("EMA", `SiteCode`) ~ "Offshore",
                              grepl("HOG", `SiteCode`) ~ "Offshore",
                              grepl("OTT", `SiteCode`) ~ "Offshore",
                              grepl("KIS", `SiteCode`) ~ "Offshore")) %>% 
  mutate(Reeftype = factor(Reeftype, levels= c("Pinnacle", "Offshore", "Nearshore"))) %>% # Organise by reef type
  mutate(SiteCode = factor(SiteCode, levels= c("BRAD", "JOEL", "INGL", "KBOM",   # Pinnacles first
                                               "EMA",  "HOG", "KIS", "OTT",      # then offshore
                                               "DON", "LADI", "MADA", "SUSA")))
