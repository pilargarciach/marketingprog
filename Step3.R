load("Results/Result2.RData")
Network <- SS[c(8,1,10:15)]
rm(list=setdiff(ls(), c("Network", "SS")))
Network$Competence <- tolower(Network$Competence)

table(Network$Competence)
network <- Network[!duplicated(Network[c(1,2,4,6,7)]),]

library(tidyverse)
Public <- network %>% filter(., grepl("Public",InstitutionType))
Private <- network %>%  filter(., grepl("Private", InstitutionType))

seleccionados <- unique(network$docname)
load("Results/Result1.RData")
todos <- unique(TextosData$Text)
setdiff(todos, seleccionados)



library(igraph)
bn2 <- graph_from_data_frame(network,directed=FALSE)
BN <- data.frame(Degree = igraph::degree(bn2),
                 Closeness = igraph::closeness(bn2),
                 Betweennes = igraph::betweenness(bn2),
                 Eigen = igraph::eigen_centrality(bn2))
BN <- BN[ -c(5:25) ]
BN$Partition <- "Skills"
BN$Partition[29:286] <- "Brochures"
BN$Node <- rownames(BN)
write.csv(BN, file = "BN.csv")

library(psych)
describeBy(BN$Degree, group = BN$Partition, mat = TRUE, digit = 2)
quantile(BN$Degree, probs = seq(.1, .99, by = .01))


library(tidyverse)
Skills <- BN %>% filter(., Partition == "Skills")
Brochures <- BN %>% filter(., Partition == "Brochures")
summary(Skills$Degree)

png("F2.png", width = 8, height = 10, units = 'in', res = 300)
par(mfrow=c(2,1))
hist(Skills$Degree, freq = FALSE, xlab = "Degree", main = "Degree distribution for Skills", col = "#ffd800")
lines(x = density(x = Skills$Degree), col = "black")
hist(Brochures$Degree, freq = FALSE, xlab = "Degree", main = "Degree distribution for Brochures", col = "purple3")
lines(x = density(x = Brochures$Degree), col = "black")
dev.off()



boxplot(BN$Degree ~ BN$Partition, xlab = "Node Partition", ylab = "Degree", col = "blue", notch = T)

library(ggplot2)
library(ggridges)
ggplot(BN, aes(x = Degree, y = Partition, fill = Partition)) +
  geom_density_ridges(alpha=0.6) +
  theme_light() + 
  theme(legend.position = "none")



describeBy(BN$Degree, group = BN$Partition, mat = TRUE, digits = 3)


bipartite_mapping(bn2)
V(bn2)$type <- bipartite_mapping(bn2)$type



BiM <- as_biadjacency_matrix(bn2, types = V(bn2)$type, names = TRUE)
statisticalSummary <- function(x) {
  data.frame(
    Mean = apply(x, 2, mean),
    Median = apply(x, 2, median),
    StandardDeviation = apply(x, 2, sd),
    Min = apply(x, 2, min),
    Max = apply(x, 2, max)
  )
}
pave <- statisticalSummary(BiM)
library(network)
red <- network(BiM, 
               directed = FALSE, 
               hyper = FALSE, 
               loops = FALSE, 
               multiple = FALSE, 
               bipartite = TRUE)
is.network(red)
red
get.vertex.attribute(red, "vertex.names")
#write.csv(BiM, file = "MarketingNetwork.csv")


sna::gden(red)
ProgramAttributes <- TextosData[TextosData$Text %in% seleccionados, ]
#write.csv(Atributos, file = "Atributos.csv")
SkillAttributes <- data.frame(Competence = unique(SS$Competence))
library(readr)
ONET_SkillsImportance <- read_csv("ONET_SkillsImportance.csv")
SkillAttributes <- merge(SkillAttributes, ONET_SkillsImportance, by.x = "Competence", by.y = "Skill", all.x = TRUE)
set.vertex.attribute(red, "OnetImportance", SkillAttributes$Importance)
OnetImportance <- data.frame(OnetImportance = get.vertex.attribute(red, "OnetImportance"))
OnetImportance$OnetImportance[29:286] <- 0 
SchoolType <- data.frame(SchoolType = c(rep(0, 28), ProgramAttributes$InstitutionType))
Region <- data.frame(Region = c(rep(0,28), ProgramAttributes$Region))
AllAtributes <- data.frame(OnetImportance, SchoolType, Region)
write.csv(AllAtributes, file = "AllAtributes.csv")
set.vertex.attribute(red, "SchoolType", SchoolType$SchoolType)
get.vertex.attribute(red, "SchoolType")
set.vertex.attribute(red, "OnetImportance", OnetImportance$OnetImportance)
set.vertex.attribute(red, "Region", Region$Region)
red
get.vertex.attribute(red, "OnetImportance")
save.image("Results/Result3.RData")
