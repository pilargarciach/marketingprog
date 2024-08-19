load("Results/Result3.RData")
Alignment <- ONET_SkillsImportance[c(2,1)]
Alignment$Skill <- tolower(Alignment$Skill)
SkillsBN <- BN[1:4]
SkillsBN$Skill <- rownames(SkillsBN)
