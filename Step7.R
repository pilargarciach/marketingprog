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
eu.me.af <- graph.data.frame(EU.ME.AF, directed = FALSE)
am <- graph.data.frame(AM, directed = FALSE)
ap <- graph.data.frame(AP, directed = FALSE)

Region1 <- data.frame(Degree = igraph::degree(am),
                      Closeness = igraph::closeness(am),
                      Betweennes = igraph::betweenness(am),
                      Eigen = igraph::eigen_centrality(am))
Region1 <- Region1[ -c(5:25) ]
rownames(Region1)
Region1$SS <- rownames(Region1)
Region1 <- Region1[order(Region1$SS), ]
Region1 <- Region1[!grepl('text', Region1$SS), ]
Region1$Region <- "AM"

Region2 <- data.frame(Degree = igraph::degree(eu.me.af),
                      Closeness = igraph::closeness(eu.me.af),
                      Betweennes = igraph::betweenness(eu.me.af),
                      Eigen = igraph::eigen_centrality(eu.me.af))
Region2 <- Region2[ -c(5:25) ]
rownames(Region2)
Region2$SS <- rownames(Region2)
Region2 <- Region2[order(Region2$SS), ]
Region2 <- Region2[!grepl('text', Region2$SS), ]
Region2$Region <- "EU-ME-AF"

Region3 <- data.frame(Degree = igraph::degree(ap),
                      Closeness = igraph::closeness(ap),
                      Betweennes = igraph::betweenness(ap),
                      Eigen = igraph::eigen_centrality(ap))
Region3 <- Region3[ -c(5:25) ]
rownames(Region3)
Region3$SS <- rownames(Region3)
Region3 <- Region3[order(Region3$SS), ]
Region3 <- Region3[!grepl('text', Region3$SS), ]
Region3$Region <- "AP"





rm(list=setdiff(ls(), c("Region1","Region2", "Region3")))

Programs <- rbind(Region1, Region2, Region3)
scaled_programs <- scale(Programs[c(1:4)])
rescaled <- data.frame(apply(scaled_programs, 2, function(x) (x - min(x)) / (max(x) - min(x))))
rescaled$SS <- Programs$SS
rescaled$Region <- Programs$Region

library(tidyverse)
Program <- rescaled %>% 
  pivot_longer(c(`Degree`, `Closeness`, `Betweennes`, `Eigen.vector`), names_to = "Centrality", values_to = "cases")
