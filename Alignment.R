load("Results/Result3.RData")
Alignment <- ONET_SkillsImportance[c(2,1)]
Alignment$Skill <- tolower(Alignment$Skill)
SkillsBN <- BN[1:4]
SkillsBN$Skill <- rownames(SkillsBN)
rm(list=setdiff(ls(), c("Alignment", "SkillsBN")))
alignment <- merge(Alignment, SkillsBN, by = "Skill")
colnames(alignment)[2] <- "O*NET.Importance"
hist(alignment$`O*NET.Importance`)

library(ggplot2)
png("F3.png", width = 10, height = 8, units = 'in', res = 300)
ggplot(alignment, mapping = aes(x = reorder(Skill, `O*NET.Importance`), `O*NET.Importance`)) + 
  geom_bar(stat = "identity", color = "black", fill="#ffd800") + 
  theme_minimal() +
  theme(axis.text.x=element_text(size=14, colour = "black"),
        axis.text.y = element_text(size = 14, colour = "black"),
         axis.title.x=element_text(size=16,face="bold"),
         axis.title.y=element_text(size = 16,face = "bold")) +
  coord_flip() +
  xlab("Skill") +
  ylab("O*NET importance")
dev.off()


library(psych)
png("F4.png", width = 10, height = 8, units = 'in', res = 300)
pairs.panels(alignment[2:6], 
             digits = 2, 
             method = "pearson",
             cex.cor = 1,
             stars = TRUE,
             ci = TRUE,
             density = TRUE,
             pch = 8,
             rug = TRUE,
             scale = FALSE,
             cex = 0.5,
             hist.col = "yellow1")
dev.off()
