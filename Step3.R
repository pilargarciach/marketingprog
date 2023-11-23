load("Results/Result2.RData")
Network <- SS[c(1,7)]
rm(list=setdiff(ls(), "Network"))
Network$pattern <- tolower(Network$pattern)

Network$pattern[Network$pattern=="pensamiento crítico"] <- "S1"
Network$pattern[Network$pattern=="solución de problemas"] <- "S2"
Network$pattern[Network$pattern=="comunica*"] <- "S3"
Network$pattern[Network$pattern=="crea*"] <- "S4"
Network$pattern[Network$pattern=="paciencia"] <- "S5"
Network$pattern[Network$pattern=="lider*"] <- "S6"
Network$pattern[Network$pattern=="resolver"] <- "S7"
Network$pattern[Network$pattern=="comprom*"] <- "S8"
Network$pattern[Network$pattern=="emprende*"] <- "S9"
Network$pattern[Network$pattern=="gesti*"] <- "S10"
Network$pattern[Network$pattern=="reflexionar"] <- "S11"
Network$pattern[Network$pattern=="control*"] <- "S12"
Network$pattern[Network$pattern=="étic*"] <- "S13"
Network$pattern[Network$pattern=="tolera*"] <- "S14"
Network$pattern[Network$pattern=="argument*"] <- "S15"
Network$pattern[Network$pattern=="conflict*"] <- "S16"
Network$pattern[Network$pattern=="negocia*"] <- "S17"
Network$pattern[Network$pattern=="entender"] <- "S18"
Network$pattern[Network$pattern=="trabajo en equipo"] <- "S19"
Network$pattern[Network$pattern=="planifi*"] <- "S20"
Network$pattern[Network$pattern=="genera*"] <- "S21"
Network$pattern[Network$pattern=="empatía"] <- "S22"
Network$pattern[Network$pattern=="compartir"] <- "S23"
Network$pattern[Network$pattern=="anali*"] <- "S24"
Network$pattern[Network$pattern=="reconoc*"] <- "S25"
Network$pattern[Network$pattern=="guiar"] <- "S26"
Network$pattern[Network$pattern=="respetar"] <- "S27"
Network$pattern[Network$pattern=="motiva*"] <- "S28"
Network$pattern[Network$pattern=="coopera*"] <- "S29"
Network$pattern[Network$pattern=="fortalecer"] <- "S30"
Network$pattern[Network$pattern=="empuj*"] <- "S31"
Network$pattern[Network$pattern=="acercar"] <- "S32"
Network$pattern[Network$pattern=="conectar"] <- "S33"
Network$pattern[Network$pattern=="cambiar"] <- "S34"
Network$pattern[Network$pattern=="apreciar"] <- "S35"
Network$pattern[Network$pattern=="fomentar"] <- "S36"
Network$pattern[Network$pattern=="interactuar"] <- "S37"
Network$pattern[Network$pattern=="identificar"] <- "S38"
Network$pattern[Network$pattern=="competir"] <- "S39"
Network$pattern[Network$pattern=="manifestar"] <- "S40"
Network$pattern[Network$pattern=="responsable"] <- "S41"
Network$pattern[Network$pattern=="evalua*"] <- "S42"
Network$pattern[Network$pattern=="innova*"] <- "S43"
Network$pattern[Network$pattern=="deci*"] <- "S44"
Network$pattern[Network$pattern=="flexibilidad"] <- "S45"
Network$pattern[Network$pattern=="persua*"] <- "S46"
Network$pattern[Network$pattern=="convenc*"] <- "S47"
Network$pattern[Network$pattern=="inspir*"] <- "S48"

table(Network$pattern)
network <- Network[!duplicated(Network[c(1,2)]),]


library(igraph)
bn2 <- graph.data.frame(network,directed=FALSE)
bipartite.mapping(bn2)
V(bn2)$type <- bipartite_mapping(bn2)$type
V(bn2)$color <- ifelse(V(bn2)$type, "red", "green")
V(bn2)$shape <- ifelse(V(bn2)$type, "circle", "square")
V(bn2)$label.cex <- ifelse(V(bn2)$type, 0.5, 1)
V(bn2)$size <- sqrt(igraph::degree(bn2))
E(bn2)$color <- "lightgrey"

bn2.pr <- bipartite.projection(bn2)
Terms <- bn2.pr$proj2

centrality_scores <- igraph::eigen_centrality(Terms)
centrality_scores <- centrality_scores$vector
# Create a color palette with different colors
color_palette <- colorRampPalette(c("#C71135", "#E7E705", "#2E851B"))(length(unique(centrality_scores)))

# Assign colors to nodes based on their normalized centrality scores
node_colors <- color_palette[rank(centrality_scores)]

# Plot the network with node colors based on centrality
set.seed(56)
plot(Terms, vertex.label.color = "black", vertex.label.cex = 0.8, vertex.color = node_colors, vertex.size = 15, edge.width = 0.5, edge.color = "lightgray", layout = layout_nicely, main = "")

save.image("Results/Result3.RData")
