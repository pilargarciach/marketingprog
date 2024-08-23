# To explore the existence of homophily
# we need to see if skills most connected
# along with program brochures most connected
# in combination, explain a little bit better
# the network as whole. 
# For that purpose, we need to find who most connected 
# brochures were (i.e., identifying them by knowing
# their position in the bi-adjacency matrix BiM).

load("Results/Result3.RData")
rm(list=setdiff(ls(), c("BN", "red", "BiM")))
programs <- data.frame(Position = colnames(BiM))
library(tidyverse)
Brochures <- BN %>% filter(., Partition == "Brochures")
hist(Brochures$Degree)

red
library(ergm)
library(network)
library(coda)
red
set.seed(2758)
ModelAAA <- ergm(red ~ edges + b1sociality(c(3, 11, 13)) +
                   b2sociality(c(36, 66, ))
                   b2factor('SchoolType', levels = "Public") +
                   b2factor('Region', levels = "EU-ME-AF") + 
                   nodecov('OnetImportance'), 
                 control = control.ergm(MCMC.samplesize = 100000, 
                                        MCMC.burnin = 10000, 
                                        MCMLE.maxit = 10))
summary(ModelAAA) # AIC = 7466