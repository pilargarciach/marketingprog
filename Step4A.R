load("Results/Result3.RData")
library(network)
library(ergm)
library(coda)
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

set.seed(1107)
model0 <- ergm(red ~ edges, control = control.ergm(
  MCMC.burnin = 10000,
  MCMC.interval = 100
))
summary(model0)

model0$coefficients
pave <- gof(model0)
pave2 <- gof(model0, GOF = ~model)
pave
pave2
plot(pave)
Sim.M0 <- simulate(model0, nsim = 1000, 
                   coef = model0$coefficients,
                   control = 
                     control.simulate.ergm(
                       MCMC.burnin = 10000, 
                       MCMC.interval = 100))
print(Sim.M0[[1]])
Sim.M0
resumen <- summary(Sim.M0)



pave3 <- gof(Sim.M0[[1]], GOF = ~model)


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
# Homofilia por tipo de escuela
model5 <- ergm(red ~ nodefactor("SchoolType"))
summary(model5)
# Homofilia por Region
model5A <- ergm(red ~ nodefactor("Region"))
summary(model5A)
# Homofilia de region y tipo de escuela
model5B <- ergm(red ~ nodefactor("Region") + nodefactor("SchoolType"))
summary(model5B)
#sink("ergm_output.txt") # Redirects standard output to a file named ergm_output.txt

model6 <- ergm(red ~ gwb1dsp(decay = 0.1, fixed = TRUE),
               control = control.ergm(MCMC.burnin = 20000, MCMC.samplesize = 20000))


control_params <- control.simulate.ergm(
  MCMC.burnin = 10000,
  MCMC.interval = 100,
  MCMC.samplesize = 1000,
  MCMC.maxit = 50000
)

Starting maximum pseudolikelihood estimation (MPLE):
  Obtaining the responsible dyads.
Evaluating the predictor and response matrix.
Maximizing the pseudolikelihood.
Finished MPLE.
Starting Monte Carlo maximum likelihood estimation (MCMLE):
  Iteration 1 of at most 60:
  Error in ergm.MCMLE(init, s, s.obs, control = control, verbose = verbose,  : 
                        Unconstrained MCMC sampling did not mix at all. Optimization cannot continue.
                      In addition: Warning message:
                        In ergm_MCMC_sample(s, control, theta = mcmc.init, verbose = max(verbose -  :
                                                                                           Unable to reach target effective size in iterations alotted.
sink() # Resets standard output to go to the console again
sink


summary(model6)





mcmc.diagnostics(model1)

control.
library(tnet)



pdf("veamos.pdf")
mcmc.diagnostics(model3)
dev.off()
coda::m