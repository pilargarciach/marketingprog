load("Results/Result3.RData")
library(network)
library(ergm)
red
sna::gden(red)
# terminos dependientes de díadas
#b1concurrent (Descartado por ser infinito su estimado)
#b1degrange
#b1degree
#b1dsp (Descartado "unable to reach target effective size in iterations alotted")
#b1mindegree
#b1star con b1star(10) me da un conteo exageradamente grande 10^17
#b1starmix
#b1twostar
#gwb1degree
#gwb1dsp
#isolatededges


model1 <- ergm(red ~ b1sociality(nodes = c(1:15)))
summary(model1)
mcmc.diagnostics(model1)
library(tnet)



pdf("veamos.pdf")
mcmc.diagnostics(model3)
dev.off()
coda::m