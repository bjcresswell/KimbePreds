# mvabund model testing
# Deprecated to main script
# Aug 2021

mvmodnb1 <- manyglm(predabund ~ predenv$Reeftype, test="LR", family="negative_binomial")
mvmodnb2 <- manyglm(predabund ~ predenv$Reeftype, test="LR", cor.type="shrink", family="negative_binomial")
mvmodnb3 <- manyglm(predabund ~ predenv$Reeftype, test="LR", cor.type="R", family="negative_binomial")

# Can't use cor.type shrink or R if you want LRT output. However, I is robust to violations of independence anyway - get Ref.

modelaovsimple1 <- anova(mvmodnb1)
modelaovsimple2 <- anova(mvmodnb2)
modelaovsimple3 <- anova(mvmodnb3)

modelaovsimple1
modelaovsimple2
modelaovsimple3



modelaovsimple1a <- anova(mvmodnb1, bootID = permutations)
modelaovsimple1b <- anova(mvmodnb1, resamp = "montecarlo")


modelaovsimple1
modelaovsimple1a
modelaovsimple1b


modelaovpairwise <- anova(mvmodnb1, bootID = permutations, pairwise.comp = predenv$Reeftype, p.uni="adjusted")

modelaovpairwise <- modelaovpairwise %>% 
  tab_model() # Doesn't work and can't get into a df, tibble or anything else


modelaovpairwise$pairwise.comp.table
modelaovpairwise$uni.p
modelaovpairwise$uni.test
modelaovpairwise$
modelaovpairwise




