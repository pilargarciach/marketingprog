load("Results/Result3.RData")
library(ergm)
library(network)
library(coda)
red
sna::gden(red)
model0 <- ergm(red ~ edges)
summary(model0)
Simuladas0 <- simulate(model0, nsim = 1000)
Simuladas0[1000]




# Modelo con efecto de homofilia en las competencias
model1 <- ergm(red ~ edges + b1sociality(nodes = c(1:28)))
summary(model1)
# Modelo sin intercepto (no recomendado en general)
model2 <- ergm(red ~ b1sociality(nodes = c(1:28))) 

model3 <- ergm(red ~ edges + b1star(75))
summary(model3) # Modelo degenerado
mcmc.diagnostics(model3)

model3 <- ergm(red ~ edges + b2dsp(8))
