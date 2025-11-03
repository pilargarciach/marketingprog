load("Results/Result2.RData")
Network <- SS[c(8,1,10:15)]
rm(list=setdiff(ls(), c("Network", "SS")))
Network$Competence <- tolower(Network$Competence)

table(Network$Competence)
network <- Network[!duplicated(Network[c(1,2,4,6,7)]),]
network <- network[, c(2, 1, 3, 4, 5, 6, 7, 8)]

library(tidyverse)
Public <- network %>% filter(., grepl("Public",InstitutionType))
Private <- network %>%  filter(., grepl("Private", InstitutionType))

seleccionados <- unique(network$docname)
load("Results/Result1.RData")
todos <- unique(TextosData$Text)
setdiff(todos, seleccionados)

library(igraph)
bn2 <- graph_from_data_frame(network,directed=FALSE)
V(bn2)$type <- bipartite_mapping(bn2)$type
V(bn2)$shape <- ifelse(V(bn2)$type, "circle", "square")
V(bn2)$size <- 3.5
V(bn2)$label <- ""
set.seed(7093)
png("F3.png", width = 15, height = 15, units = 'in', res = 300)
plot(bn2,
     vertex.color = ifelse(V(bn2)$type == FALSE, "#FFCD00", "purple3"),
     edge.width = 0.3, 
     edge.color = "gray",
     layout = layout_components, 
     main = "")
dev.off()

library(readr)
Data_Figure2 <- read_csv("hypotheticaldata.csv")

library(igraph)
Network <- Data_Figure2

bn2 <- graph.data.frame(Network,directed=FALSE)
bipartite.mapping(bn2)
V(bn2)$type <- bipartite_mapping(bn2)$type
V(bn2)$color <- ifelse(V(bn2)$type, "green", "red")
V(bn2)$shape <- ifelse(V(bn2)$type, "square", "circle")
V(bn2)$label.cex <- ifelse(V(bn2)$type, 0.5, 1)
V(bn2)$size <- sqrt(igraph::degree(bn2))
E(bn2)$color <- "lightgrey"

bn2.pr <- bipartite.projection(bn2)
Programs <- bn2.pr$proj2
Skills <- bn2.pr$proj1
class(Skills)
#matw1 <- as.matrix(get.adjacency(Skills))

# Plot the network with node colors based on centrality
set.seed(56)
png("F2a.png", width = 15, height = 7, units = 'in', res = 300)
plot(Skills, vertex.label.color = "white", 
     vertex.label.cex = 1.5, 
     vertex.color = "#729fcf", 
     vertex.size = 25, 
     edge.width = 4, 
     edge.color = "gray", 
     layout = layout_components, main = "")
dev.off()

set.seed(56)
png("F2b.png", width = 15, height = 7, units = 'in', res = 300)
plot(Programs, vertex.label.color = "white", 
     vertex.label.cex = 1.2, 
     vertex.color = "#158466", 
     vertex.size = 25, 
     edge.width = 4, 
     edge.color = "gray", 
     layout = layout_components, 
     main = "")
dev.off()


