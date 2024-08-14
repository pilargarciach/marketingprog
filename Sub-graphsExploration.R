load("Results/Result2.RData")
Network <- SS[c(8,1,10:15)]
rm(list=setdiff(ls(), c("Network", "SS")))
Network$Competence <- tolower(Network$Competence)

table(Network$Competence)
network <- Network[!duplicated(Network[c(1,2,4,6,7)]),]

library(tidyverse)
Public <- network %>% filter(., grepl("Public",InstitutionType))
Private <- network %>%  filter(., grepl("Private", InstitutionType))
AM <- network %>% filter(., grepl("AM", Region))
EU <- network %>% filter(., grepl("EU-ME-AF", Region))
AP <- network %>% filter(., grepl("AP", Region))

library(igraph)
public <- graph_from_data_frame(Public, directed=FALSE)
PUBLIC <- data.frame(Degree = igraph::degree(public),
                 Closeness = igraph::closeness(public),
                 Betweennes = igraph::betweenness(public),
                 Eigen = igraph::eigen_centrality(public))
PUBLIC <- PUBLIC[ -c(5:25) ]
PUBLIC$Partition <- "Skills"
PUBLIC$Partition[28:205] <- "Brochures"

library(psych)
describeBy(PUBLIC$Degree, group = PUBLIC$Partition, mat = TRUE, digit = 2)

private <- graph_from_data_frame(network,directed=FALSE)
PRIVATE <- data.frame(Degree = igraph::degree(private),
                 Closeness = igraph::closeness(private),
                 Betweennes = igraph::betweenness(private),
                 Eigen = igraph::eigen_centrality(private))
PRIVATE <- PRIVATE[ -c(5:25) ]
PRIVATE$Partition <- "Skills"
PRIVATE$Partition[29:286] <- "Brochures"

describeBy(PRIVATE$Degree, group = PRIVATE$Partition, mat = TRUE, digit = 2)

americas <- graph_from_data_frame(network,directed=FALSE)
AMERICAS <- data.frame(Degree = igraph::degree(americas),
                 Closeness = igraph::closeness(americas),
                 Betweennes = igraph::betweenness(americas),
                 Eigen = igraph::eigen_centrality(americas))
AMERICAS <- AMERICAS[ -c(5:25) ]
AMERICAS$Partition <- "Skills"
AMERICAS$Partition[29:286] <- "Brochures"
