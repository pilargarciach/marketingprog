load("Results/Result2.RData")
Network <- SS[c(8,1)]
rm(list=setdiff(ls(), "Network"))
Network$Competence <- tolower(Network$Competence)

table(Network$Competence)
network <- Network[!duplicated(Network[c(1,2)]),]


library(igraph)
bn2 <- graph_from_data_frame(network,directed=FALSE)
bipartite_mapping(bn2)
V(bn2)$type <- bipartite_mapping(bn2)$type

BiM <- as_biadjacency_matrix(bn2, types = V(bn2)$type, names = TRUE)
library(network)
red <- network(BiM, 
               directed = FALSE, 
               hyper = FALSE, 
               loops = FALSE, 
               multiple = FALSE, 
               bipartite = TRUE)
is.network(red)
red


V(bn2)$color <- ifelse(V(bn2)$type, "red", "green")
V(bn2)$shape <- ifelse(V(bn2)$type, "circle", "square")
V(bn2)$label.cex <- ifelse(V(bn2)$type, 0.5, 1)
V(bn2)$size <- sqrt(igraph::degree(bn2))
E(bn2)$color <- "lightgrey"

bn2.pr <- bipartite_projection(bn2)
Terms <- bn2.pr$proj2

centrality_scores <- igraph::eigen_centrality(Terms)
centrality_scores <- centrality_scores$vector
# Create a color palette with different colors
color_palette <- colorRampPalette(c("#C71135", "#E7E705", "#2E851B"))(length(unique(centrality_scores)))

# Assign colors to nodes based on their normalized centrality scores
node_colors <- color_palette[rank(centrality_scores)]

# Plot the network with node colors based on centrality
set.seed(56)
plot(Terms, vertex.label.color = "black", 
     vertex.label = NA,
     vertex.label.cex = 0,
     vertex.color = node_colors, 
     vertex.size = 15, 
     edge.width = 0.5, 
     edge.color = "lightgray", 
     layout = layout_components, main = "")

save.image("Results/Result3.RData")
