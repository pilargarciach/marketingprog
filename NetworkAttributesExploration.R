load("Results/Result3.RData")
rm(list=setdiff(ls(), c("red")))
library(ergm)
library(network)
library(coda)
red

Atributos <- data.frame(Nodos = get.vertex.attribute(red, 'vertex.names'),
                        ONET = get.vertex.attribute(red, 'OnetImportance'))
