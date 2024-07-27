load("Results/Result3.RData")
library(network)
library(ergm)
red
sna::gden(red)
sna::centralgraph(red)
sna::components(red)
mixingmatrix(red, 'SchoolType')
mixingmatrix(red, 'Region')
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
search.ergmTerms(keywords = c("bipartite"))

model0 <- ergm(red ~ edges)
summary(model0)
model1 <- ergm(red ~ edges + b1sociality(nodes = c(1:28)))
summary(model1)
model1A <- ergm(red ~ b1sociality(nodes = c(1:28)))
summary(model1A)
a <- summary(model1A)
a <- data.frame(a$coefficients)
a$Skill <- SkillAttributes$Competence
a <- a %>% dplyr::relocate(Skill, .before = everything())
model2 <- ergm(red ~ b1cov("OnetImportance"))
summary(model2)
model2A <- ergm(red ~ b1cov("OnetImportance") + b1sociality(nodes = c(1:28)))
summary(model2A)
model3 <- ergm(red ~ b1sociality(nodes = c(3,11,13)))
summary(model3)
model3A <- ergm(red ~ b1cov("OnetImportance") + b1sociality(nodes = c(3,11,13)))
summary(model3A)
model4A <- ergm(red ~ nodefactor("Region") + nodefactor('SchoolType'))
summary(model4A)
model4B <- ergm(red ~ nodefactor("Region") + nodefactor('SchoolType') + b1sociality(nodes = c(1:28)))
summary(model4B)

control = control.ergm(MCMC.burnin = 10000, MCMC.samplesize = 50000)
model4 <- ergm(red ~ b1dsp(5), control = control)
summary(model4)





mcmc.diagnostics(model1)

control.
library(tnet)



pdf("veamos.pdf")
mcmc.diagnostics(model3)
dev.off()
coda::m