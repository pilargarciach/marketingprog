# To explore the existence of homophily
# we need to see if skills most connected
# along with program brochures most connected
# in combination, explain a little bit better
# the network as whole. 
# For that purpose, we need to find who most connected 
# brochures were (i.e., identifying them by knowing
# their position in the bi-adjacency matrix BiM).

load("Results/Result3.RData")
rm(list=setdiff(ls(), c("red")))
red
library(ergm)
library(network)
library(coda)
red
