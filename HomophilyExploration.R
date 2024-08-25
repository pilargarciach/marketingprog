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
library(ergm)
library(network)
library(coda)
red

set.seed(9615)
Model3h <- ergm(red ~ edges + 
                  b1sociality(c(3, 11, 13)) + 
                  b1nodematch("OnetImportance"), 
                control = control.ergm(MCMC.samplesize = 100000, 
                                       MCMC.burnin = 10000, 
                                       MCMLE.maxit = 10))
summary(Model3h)
mcmc.diagnostics(Model3h)
gof(Model3h, GOF =~model)






Model3H <- ergm(red ~ edges + 
                   b1sociality(c(3, 11, 13, 2, 6)) + 
                   b1nodematch("OnetImportance"), 
                 control = control.ergm(MCMC.samplesize = 100000, 
                                        MCMC.burnin = 10000, 
                                        MCMLE.maxit = 10))
summary(Model3H)
mcmc.diagnostics(Model3H)
gof(Model3H, GOF =~model)
