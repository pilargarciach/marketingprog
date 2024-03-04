load("Results/Result1.RData")
load("Results/Result2.RData")
textos$docname <- paste0("text", 1:length(textos$doc_id))
rm(list=setdiff(ls(), c("textos","SS")))


SoftSkills <- merge(SS, textos, by.x = "docname", by.y = "docname", all.x = TRUE)
variable.names(SoftSkills)

library(dplyr)
Public <- SoftSkills %>% filter(., SchoolType=="Public") %>% select(., c(pattern, docname))
Private <- SoftSkills %>% filter(., SchoolType=="Private") %>% select(., c(pattern, docname))

library(igraph)
public <- graph.data.frame(Public, directed = FALSE)
private <- graph.data.frame(Private, directed = FALSE)


PUBLIC <- data.frame(Degree = igraph::degree(public),
                      Closeness = igraph::closeness(public),
                      Betweennes = igraph::betweenness(public),
                      Eigen = igraph::eigen_centrality(public))
PUBLIC <- PUBLIC[ -c(5:25) ]
rownames(PUBLIC)
PUBLIC$SS <- rownames(PUBLIC)
PUBLIC <- PUBLIC[order(PUBLIC$SS), ]
PUBLIC <- PUBLIC[!grepl('text', PUBLIC$SS), ]
PUBLIC$Region <- "AM"

PRIVATE <- data.frame(Degree = igraph::degree(private),
                      Closeness = igraph::closeness(private),
                      Betweennes = igraph::betweenness(private),
                      Eigen = igraph::eigen_centrality(private))
PRIVATE <- PRIVATE[ -c(5:25) ]
rownames(PRIVATE)
PRIVATE$SS <- rownames(PRIVATE)
PRIVATE <- PRIVATE[order(PRIVATE$SS), ]
PRIVATE <- PRIVATE[!grepl('text', PRIVATE$SS), ]
PRIVATE$Region <- "EU-ME-AF"

rm(list=setdiff(ls(), c("PUBLIC","PRIVATE")))

Programs <- rbind(PUBLIC, PRIVATE)
scaled_programs <- scale(Programs[c(1:4)])
rescaled <- data.frame(apply(scaled_programs, 2, function(x) (x - min(x)) / (max(x) - min(x))))
rescaled$SS <- Programs$SS
rescaled$Region <- Programs$Region

library(tidyverse)
Program <- rescaled %>% 
  pivot_longer(c(`Degree`, `Closeness`, `Betweennes`, `Eigen.vector`), names_to = "Centrality", values_to = "cases")
