



set.seed(2)
predNMDS <- metaMDS(predmatrix, k = 2, trymax = 100, trace = FALSE, autotransform = FALSE, distance="bray")


species.scores <- as.data.frame(scores(predNMDS, "species")) %>% 
  rownames_to_column(var = "species")

species.scores <- species.scores %>% 
  filter(species %in% c("Sphyraena qenie", "Caranx sexfasciatus", "Macolor macularis",
                        "Caranx melampygus", "Lutjanus gibbus", "Cephalopholis cyanostigma"))


#             1      2       3       4       5        6
Coord_1 <- c(-2.7,	-4.2,	 -2.1,	  -2.6,	   1.2,	     -6.5)
Coord_2 <- c(-0.2,	0.2,	   0.9,	   0.5,	  -0.2,	   -0.7)


species.labels <- species.scores %>% 
  mutate(Coord_1 = Coord_1,
         Coord_2 = Coord_2)



data.scores <- as.data.frame(scores(predNMDS))  #Using the scores function from vegan to extract the site scores and convert to a data.frame
data.scores$TID <- rownames(data.scores)  # create a column of site names, from the rownames of data.scores
data.scores$grp <- env$Reeftype  #  add the grp variable created earlier




ggplot(data.scores, aes(NMDS1, NMDS2)) + # Each individual TID score in NMDS space
  geom_point(aes(colour = grp, shape = grp, size=Shannon), alpha=0.6, position=position_jitter(.1)) +  # grouping by reef type (grp)
  xlim(-8,2.5)+
  ylim(-2.5, 2.5)+
  scale_color_manual(values=c("#DFC27D", "#436EEE", "#35978F"))+ # The classic Kimbe colour scheme
  scale_fill_manual(values=c("#DFC27D", "#436EEE", "#35978F"))+ # The classic Kimbe colour scheme
  scale_shape_manual(values=c(16, 8, 17))+                       # and classic Kimbe pch scheme
  stat_ellipse(geom = "polygon", aes(colour = grp, fill = grp), alpha=0.1, type='t',size =0.8) +          # 95% conf ellipses
  geom_point(data = species.scores, aes(NMDS1, NMDS2), pch = 15, size = 2,
             position=position_jitter(0.1)) +                    # Pts for Spp scores for top 6 taxa
  geom_text(data = species.labels, aes(Coord_1, Coord_2, label = species), size = 4, fontface = "italic") + # Text for these           
  #geom_segment(data = species.scores, aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2),                # And arrows
  #           arrow = arrow(length = unit(0.2, 'cm')), colour = "black", alpha=0.6)+
  #geom_text(aes(label = TID), size = 2)+  
  geom_hline(yintercept = 0, linetype=2, alpha=0.3) +     # 0 on y axis
  geom_vline(xintercept = 0, linetype=2, alpha=0.3) +     # 0 on x axis
  theme_classic()+                        # White fill
  theme(
    axis.line = element_blank(), 
    panel.border = element_rect(size = 1, fill = "transparent"),
    legend.title = element_blank(),
    legend.text = element_text(face = "plain", size = 10),
    legend.position = c(.1,.13),
    legend.background = element_rect(fill = "transparent"),
    legend.box.background = element_rect(colour = "black"),
    legend.spacing.y = unit(0.1, "mm"),
    legend.key.size = unit(5, "mm")
  )


######

## venn.diagram code


pred.ven <- 
  venn.diagram(
    x = list(Pinnacle, Nearshore, Offshore),
    category.names = c('Pinnacle (53)', 'Nearshore (22)', 'Offshore (19)'),
    filename = '../../output/rfigures/preds_venn_fig2.png',
    output=TRUE,
    height = 3200,
    width = 3200,
    resolution = 300,
    compression = 'lzw',
    units = 'px',
    lwd = 6,
    cex = 5,
    fontface = "plain",
    fontfamily = "sans",
    cat.cex = 4,
    cat.fontface = "plain",
    cat.default.pos = "outer",
    cat.pos = c(340, 20, 135),
    cat.dist = c(0.07, 0.07, 0.1),
    cat.fontfamily = "sans",
    rotation = 1)
