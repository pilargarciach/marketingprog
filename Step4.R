load("Results/Result3.RData")
library(igraph)
set.seed(56)
png("F1.png", width = 15, height = 7, units = 'in', res = 300)

plot(Terms, vertex.label = NA, vertex.label.color = "black", vertex.label.cex = 0.8, vertex.color = node_colors, vertex.size = 15, edge.width = 0.5, edge.color = "lightgray", layout = layout_with_drl, main = "")

dev.off()

BNA <- graph.data.frame(Network, directed = FALSE)
Programs <- data.frame(Degree = igraph::degree(BNA),
                   Closeness = igraph::closeness(BNA),
                   Betweennes = igraph::betweenness(BNA),
                   Eigen = igraph::eigen_centrality(BNA))
Programs <- Programs[ -c(5:25) ]
rownames(Programs)
Programs$SS <- rownames(Programs)
Programs <- Programs[!grepl("^text", Programs$SS),]
colnames(Programs)[4] <- "Eigenvector"



Programs <- Programs[1:4]


library(psych)
png("F2.png", width = 15, height = 7, units = 'in', res = 300)
pairs.panels(Programs, 
             method = "spearman", 
             hist.col = "#C71135",
             density = TRUE,  
             ellipses = TRUE,
             pch = 15,
             cex = 1,
             cex.axis = 1.8,
             cex.labels = 2.5,
             lwd = 2,
             rug = TRUE,
             stars = TRUE
)
dev.off()


save.image("Results/Result4.RData")
