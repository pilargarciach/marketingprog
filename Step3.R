load("Results/Result2.RData")
Network <- SS[c(8,1,10:15)]
rm(list=setdiff(ls(), c("Network", "SS")))
Network$Competence <- tolower(Network$Competence)

table(Network$Competence)
network <- Network[!duplicated(Network[c(1,2,4,6,7)]),]

seleccionados <- unique(network$docname)
load("Results/Result1.RData")
todos <- unique(TextosData$Text)
setdiff(todos, seleccionados)

library(igraph)
bn2 <- graph_from_data_frame(network,directed=FALSE)
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
OnetImportance$OnetImportance[29:286] <- NA 
set.vertex.attribute(red, "OnetImportance", OnetImportance$OnetImportance)
red
save.image("Results/Result3.RData")
