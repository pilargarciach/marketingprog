load("Results/Result3.RData")
library(ergm)
library(network)
library(coda)
red
sna::gden(red)
model0 <- ergm(red ~ edges)
Model0 <- summary(model0)
Simuladas0 <- simulate(model0, nsim = 1000)
Simuladas0[1000]
class(Simuladas0)

COEF1 <- Model0$coefficients[1]




library(tidyverse)

# Función para extraer información de una red
extract_network_info <- function(net) {
  data.frame(
    nodos = network.size(net),
    edges = network.edgecount(net),
    densidad = network.density(net)
  )
}

# Aplicar la función a cada red en la lista y crear un dataframe
df_redes <- map_df(Simuladas0, extract_network_info, .id = "sim_num")

# Convertir la columna sim_num a numérico (si es necesario)
df_redes$sim_num <- as.numeric(df_redes$sim_num)



# Modelo con efecto de homofilia en las competencias
model1 <- ergm(red ~ edges + b1sociality(nodes = c(1:28)))
summary(model1)
# Modelo sin intercepto (no recomendado en general)
model2 <- ergm(red ~ b1sociality(nodes = c(1:28))) 

model3 <- ergm(red ~ edges + b1star(75))
summary(model3) # Modelo degenerado
mcmc.diagnostics(model3)

model3 <- ergm(red ~ edges + b2dsp(8))
