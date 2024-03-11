load("Results/Result1.RData")
load("Results/Result2.RData")
textos$docname <- paste0("text", 1:length(textos$doc_id))
rm(list=setdiff(ls(), c("textos","SS")))

SoftSkills <- merge(SS, textos, by.x = "docname", by.y = "docname", all.x = TRUE)

library(dplyr)
Public <- SoftSkills %>% filter(., SchoolType=="Public") %>% select(., c(pattern, docname))
library(igraph)
public <- graph.data.frame(Public, directed = FALSE)

PUBLIC <- data.frame(Degree = igraph::degree(public),
                     Closeness = igraph::closeness(public),
                     Betweennes = igraph::betweenness(public),
                     Eigen = igraph::eigen_centrality(public))
PUBLIC <- PUBLIC[ -c(5:25) ]
rownames(PUBLIC)
PUBLIC$SS <- rownames(PUBLIC)
PUBLIC <- PUBLIC[order(PUBLIC$SS), ]
PUBLIC <- PUBLIC[!grepl('text', PUBLIC$SS), ]
PUBLIC$SchoolType <- "Public"

TopPublic <- head(PUBLIC[order(-PUBLIC$Closeness), ], 10)
selected_columns <- rownames(TopPublic)

IM.b <- as_incidence_matrix(public, names = TRUE, sparse = TRUE, types = bipartite_mapping(public)$type)
IM2 <- t(as.matrix(IM.b))



# Subset the matrix by column names
IM3 <- IM2[, selected_columns, drop = FALSE]

library(bipartite)
png("B1.png", width = 25, height = 7, units = 'in', res = 300)
plotweb(IM3, method = "normal", 
        col.high = "#729fcf", 
        bor.col.high = "#729fcf",
        col.low = "gray50", 
        bor.col.low = "gray50",
        col.interaction = "grey90",
        bor.col.interaction = "grey90",
        low.lablength = 0,
        labsize = 3,
        text.rot = 90,
        ybig = 2)
dev.off()


Private <- SoftSkills %>% filter(., SchoolType=="Private") %>% select(., c(pattern, docname))
private <- graph.data.frame(Private, directed = FALSE)
PRIVATE <- data.frame(Degree = igraph::degree(private),
                      Closeness = igraph::closeness(private),
                      Betweennes = igraph::betweenness(private),
                      Eigen = igraph::eigen_centrality(private))
PRIVATE <- PRIVATE[ -c(5:25) ]
rownames(PRIVATE)
PRIVATE$SS <- rownames(PRIVATE)
PRIVATE <- PRIVATE[order(PRIVATE$SS), ]
PRIVATE <- PRIVATE[!grepl('text', PRIVATE$SS), ]
PRIVATE$SchoolType <- "Private"

TopPrivate <- head(PRIVATE[order(-PRIVATE$Closeness), ], 10)
selected_columns <- rownames(TopPrivate)

IM.b <- as_incidence_matrix(private, names = TRUE, sparse = TRUE, types = bipartite_mapping(private)$type)
IM2 <- t(as.matrix(IM.b))



# Subset the matrix by column names
IM3 <- IM2[, selected_columns, drop = FALSE]

png("B2.png", width = 25, height = 7, units = 'in', res = 300)
plotweb(IM3, method = "normal", 
        col.high = "#158466", 
        bor.col.high = "#158466",
        col.low = "gray50", 
        bor.col.low = "gray50",
        col.interaction = "grey90",
        bor.col.interaction = "grey90",
        low.lablength = 0,
        labsize = 3,
        text.rot = 90,
        ybig = 2)
dev.off()

Asia <- SoftSkills %>% filter(., Region=="AP") %>% select(., c(pattern, docname))
library(igraph)
asia <- graph.data.frame(Asia, directed = FALSE)
ASIA <- data.frame(Degree = igraph::degree(asia),
                   Closeness = igraph::closeness(asia),
                   Betweennes = igraph::betweenness(asia),
                   Eigen = igraph::eigen_centrality(asia))
ASIA <- ASIA[ -c(5:25) ]
rownames(ASIA)
ASIA$SS <- rownames(ASIA)
ASIA <- ASIA[order(ASIA$SS), ]
ASIA <- ASIA[!grepl('text', ASIA$SS), ]
ASIA$Region <- "AP"

TopAsia <- head(ASIA[order(-ASIA$Closeness), ], 10)
selected_columns <- rownames(TopAsia)

IM.b <- as_incidence_matrix(asia, names = TRUE, sparse = TRUE, types = bipartite_mapping(asia)$type)
IM2 <- t(as.matrix(IM.b))
# Subset the matrix by column names
IM3 <- IM2[, selected_columns, drop = FALSE]

library(bipartite)
png("B4.png", width = 25, height = 7, units = 'in', res = 300)
plotweb(IM3, method = "normal", 
        col.high = "#377eb8", 
        bor.col.high = "#377eb8",
        col.low = "gray50", 
        bor.col.low = "gray50",
        col.interaction = "grey90",
        bor.col.interaction = "grey90",
        low.lablength = 0,
        labsize = 3,
        text.rot = 90,
        ybig = 2)
dev.off()

