load("Results/Result3.RData")
library(tnet)
data(tnet)
net <- Davis.Southern.women.2mode
net <- as.tnet(BiM, type = "binary two-mode tnet")
reinforcement_tm(net) #0.4947676
Clustering <- clustering_tm(net)
ClusteringLocal <- clustering_local_tm(net)
UnoMenosD2 <- 1 - (sna::gden(red))^2
1 - UnoMenosD2^256

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

# Modelos Endógenos ----

set.seed(1107)
model0 <- ergm(red ~ edges, control = control.ergm(
  MCMC.burnin = 10000,
  MCMC.interval = 100
))
summary(model0) # AIC = 8729

sufficient_statistics <- summary(model0)$coef

# Calcular la matriz de covarianza
cov_matrix <- cov(sufficient_statistics)

# Función para calcular la distancia de Mahalanobis
mahalanobis_distance <- function(x, mean, cov) {
  sqrt(t(x - mean) %*% solve(cov) %*% (x - mean))
}

# Aplicar la función a cada fila de los estadísticos suficientes
distances <- apply(sufficient_statistics, 1, mahalanobis_distance, 
                   mean = colMeans(sufficient_statistics), cov = cov_matrix)


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
summary(model1) # AIC = 6828

model1A <- ergm(red ~ b1sociality(nodes = c(1:28)))
summary(model1A) # AIC = 6826

sufficient_statistics <- summary(model1A)$coef

# Calcular la matriz de covarianza
cov_matrix <- cov(sufficient_statistics)

# Función para calcular la distancia de Mahalanobis
mahalanobis_distance <- function(x, mean, cov) {
  sqrt(t(x - mean) %*% solve(cov) %*% (x - mean))
}

# Aplicar la función a cada fila de los estadísticos suficientes
distances <- apply(sufficient_statistics, 1, mahalanobis_distance, 
                   mean = colMeans(sufficient_statistics), cov = cov_matrix)
if (det(cov_matrix) == 0) {
  # Manejar el caso de matriz singular (por ejemplo, eliminar variables, reducir datos, etc.)
} else {
  # Calcular la distancia de Mahalanobis
  distances <- apply(sufficient_statistics, 1, mahalanobis_distance, 
                     mean = colMeans(sufficient_statistics), cov = cov_matrix)
}

model1a <- ergm(red ~ nodecov('OnetImportance'))
summary(model1a) # AIC = 9024

model1aa <- ergm(red ~ nodecov('OnetImportance') + b1sociality(nodes = c(1:28)))
summary(model1aa) # AIC = 6828

model1B <- ergm(red ~ nodecov('OnetImportance') + b1sociality(nodes = c(1:28)) + nodefactor('Region'))
summary(model1B) # AIC = 6833

model1C <- ergm(red ~ nodecov('OnetImportance') + b1sociality(nodes = c(1:28)) + nodefactor('Region') + nodefactor('SchoolType'))
summary(model1C) # AIC = 6834

model1D <- ergm(red ~ b2sociality(nodes = c(1:258)))
summary(model1D) # AIC = 8550

model1E <- ergm(red ~ b1sociality(nodes = c(1:28)) + b2sociality(nodes = c(1:258)))
summary(model1E) # AIC = 6379

a <- summary(model1A)
a <- data.frame(a$coefficients)
a$Skill <- SkillAttributes$Competence
a <- a %>% dplyr::relocate(Skill, .before = everything())

model1AA <- ergm(red ~ b1sociality(nodes = c(3,11,13)))
summary(model1AA)

model1AAA <- ergm(red ~ edges + b1sociality(nodes = c(3,11,13)))
summary(model1AAA)

model1a1 <- ergm(red ~ edges + nodecov('OnetImportance') + b1sociality(nodes = c(3,11,13)))
summary(model1a1)







model1B <- ergm(red ~ gwb1dsp(fixed=FALSE, cutoff=258), control = control.ergm(
  MCMC.burnin = 10000,
  MCMC.interval = 100))



# Modelos Exógenos ----

model2 <- ergm(red ~ edges + b1cov("OnetImportance"))
summary(model2) # AIC = 8654


model2B <- ergm(red ~ b1cov("OnetImportance"))
summary(model2B) # AIC = 9024


# Modelo Exogeno-Endógeno

model2C <- ergm(red ~ b1cov("OnetImportance") + b1sociality(nodes = c(1:28)))
summary(model2C) # AIC = 6828


model3 <- ergm(red ~ b1sociality(nodes = c(3,11,13)))
summary(model3)


model3A <- ergm(red ~ b1cov("OnetImportance") + b1sociality(nodes = c(3,11,13)))
summary(model3A)

model4A <- ergm(red ~ nodefactor("Region") + nodefactor('SchoolType'))
summary(model4A) # AIC = 8734

model4B <- ergm(red ~ nodefactor("Region") + nodefactor('SchoolType') + b1sociality(nodes = c(1:28)))
summary(model4B) # AIC = 6832

# Homofilia por tipo de escuela
model5 <- ergm(red ~ nodefactor("SchoolType"))
summary(model5) # AIC = 8728

# Homofilia por Region
model5A <- ergm(red ~ nodefactor("Region"))
summary(model5A) # AIC = 8732

# Homofilia de region y tipo de escuela
model5B <- ergm(red ~ nodefactor("Region") + nodefactor("SchoolType"))
summary(model5B) # AIC = 8734
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