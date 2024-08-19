load("Results/Result3.RData")
Alignment <- ONET_SkillsImportance[c(2,1)]
Alignment$Skill <- tolower(Alignment$Skill)
SkillsBN <- BN[1:4]
SkillsBN$Skill <- rownames(SkillsBN)
rm(list=setdiff(ls(), c("Alignment", "SkillsBN")))
alignment <- merge(Alignment, SkillsBN, by = "Skill")
colnames(alignment)[2] <- "O*NET.Importance"
library(psych)

pairs.panels(alignment[2:6], 
             digits = 2, 
             method = "pearson",
             cex.cor = 3,
             stars = TRUE,
             ci = TRUE,
             density = TRUE,
             pch = 8,
             rug = TRUE,
             scale = FALSE,
             cex = 0.5,
             hist.col = "purple2")
