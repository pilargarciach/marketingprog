load("Results/Result3.RData")
library(ergm)
library(network)
library(coda)

set.seed(1107)
model0 <- ergm(red ~ edges, control = control.ergm(
    MCMC.burnin = 10000,
    MCMC.interval = 100
  ))
summary(model0) # AIC = 8729

model1 <- ergm(red ~ edges + b1sociality(nodes = c(1:28)), control = control.ergm(
  MCMC.burnin = 10000,
  MCMC.interval = 100
))
summary(model1) # AIC = 6828

model2 <- ergm(red ~ b1sociality(nodes = c(1:28)), control = control.ergm(
  MCMC.burnin = 10000,
  MCMC.interval = 100
))
summary(model2) # AIC = 6826

model3 <- ergm(red ~ gwb1dsp(fixed=FALSE, cutoff=240), 
               control = control.ergm(
                 MCMC.burnin = 10000,
                 MCMC.interval = 100))
