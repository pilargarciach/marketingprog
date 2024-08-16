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

igraph::degree()

# Función para calcular estadísticas de una red
network_stats <- function(network) {
  density <- sna::gden(network)
  degree <- mean(igraph::degree(network))
  # ... calcular otras estadísticas
  return(c(density = density, degree = degree))
}

# Aplicar la función a todas las redes
stats_sim <- t(sapply(Simuladas0, network_stats))

# Calcular estadísticas de la red original
stats_orig <- network_stats(red)

# Calcular medias y desviaciones estándar
means_sim <- colMeans(stats_sim)
sds_sim <- apply(stats_sim, 2, sd)

# Realizar prueba t (ejemplo para la densidad)
t.test(stats_sim[, "density"], mu = stats_orig["density"])













# Modelo con efecto de homofilia en las competencias
model1 <- ergm(red ~ edges + b1sociality(nodes = c(1:28)))
summary(model1)
# Modelo sin intercepto (no recomendado en general)
model2 <- ergm(red ~ b1sociality(nodes = c(1:28))) 

model3 <- ergm(red ~ edges + b1star(75))
summary(model3) # Modelo degenerado
mcmc.diagnostics(model3)

model3 <- ergm(red ~ edges + b2dsp(8))
