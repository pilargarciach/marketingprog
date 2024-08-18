load("Results/Result3.RData")
rm(list=setdiff(ls(), c("red", "BiM", "BN")))
SkillsNumber <- data.frame(Skill = rownames(BiM))
library(ergm)
library(network)
library(coda)
set.seed(1107)
ModelC <- ergm(red ~ edges + b1sociality(c(3, 11, 13, 2, 6)))
summary(ModelC) # AIC = 7461
ModelD <- ergm(red ~ edges + b1sociality(nodes = c(1:28)))
summary(ModelD) # AIC = 6828


Simuladas1 <- simulate(ModelC, nsim = 1000, 
                       coef = ModelC$coefficients,
                       control = 
                         control.simulate.ergm(
                           MCMC.burnin = 50000, 
                           MCMC.interval = 100))

simulations <- lapply(1:4, function(i) {
  simulate(ModelC, nsim = 1000, coef = ModelC$coefficients, 
           control = control.simulate.ergm(MCMC.burnin = 15000, MCMC.interval = 1000))
})

library(tidyverse)
extract_coefs_simulations <- function(Simuladas1, model) {
  # Obtener el número de simulaciones
  num_sims <- length(Simuladas1)
  
  # Crear un data frame vacío para almacenar los coeficientes
  coef_df1 <- data.frame(
    sim = 1:num_sims,
    nodos = numeric(num_sims),
    edges = numeric(num_sims),
    densidad = numeric(num_sims)
  )
  
  # Obtener los nombres de los coeficientes del modelo original
  coef_names <- names(coef(model))
  
  # Iterar sobre cada red simulada y extraer los coeficientes
  for (i in 1:num_sims) {
    sim_net <- Simuladas1[[i]]
    sim_model <- ergm(sim_net ~ edges + b1sociality(c(3, 11, 13, 2, 6)))
    sim_coefs <- coef(sim_model)
    
    # Agregar los coeficientes al data frame
    coef_df1[i, coef_names] <- sim_coefs
  }
  
  return(coef_df1)
}

coef_df1 <- extract_coefs_simulations(Simuladas1, ModelC)

COEF1 <- ModelC$coefficients[2]


hist(coef_df1$b1sociality3, main = "Distribución del estadístico en las redes simuladas",
     xlab = "Valor del estadístico")
abline(v = COEF1, col = "red", lwd = 2)


p_value <- mean(coef_df1[2] >= COEF1)
p_value

GOF1 <- gof(ModelC, GOF = ~model)
GOF1

media_sim <- colMeans(coef_df1[c(3,5:9)])
cov_sim <- cov(coef_df1[c(3,5:9)])

# Vector de estadísticos de la red observada
Z <- coef(ModelC)

Z <- as.matrix(Z)
rownames(Z)

media_sim <- as.matrix(media_sim)
rownames(media_sim)
# Verificar la invertibilidad de la matriz de covarianza
if (det(cov_sim) == 0) {
  stop("La matriz de covarianza es singular, no se puede calcular la distancia de Mahalanobis")
}

dim(Z)
dim(media_sim)
dim(cov_sim)

det(cov_sim)


mcmc_output <- mcmc(coef_df1[c(3,5:9)])

# Trazar las cadenas de Markov
plot(mcmc_output)

# Calcular estadísticas de convergencia
# Geweke diagnóstico
geweke.diag(mcmc_output)

# Estadístico de Gelman-Rubin
gelman.diag(mcmc_output)



# Calcular la distancia de Mahalanobis
distancia_mahalanobis <- sqrt((Z - media_sim) %*% solve(cov_sim) %*% t(Z - media_sim))

print(distancia_mahalanobis)





# Convertir las simulaciones a un objeto mcmc
mcmc_output <- mcmc.list(lapply(simulations, mcmc))
class(mcmc_output[1])
mcmc_list_as_dataframes <- lapply(mcmc_output, as.data.frame)

# Combinar las matrices en un único data frame
df <- do.call(rbind, mcmc_list_as_matrices)

# Calcular estadísticas de convergencia
gelman.diag(mcmc_output)








# Modelo con efecto de homofilia en las competencias






# Modelo sin intercepto (no recomendado en general)
model2 <- ergm(red ~ b1sociality(nodes = c(1:28))) 

model3 <- ergm(red ~ edges + b1star(75))
summary(model3) # Modelo degenerado
mcmc.diagnostics(model3)

model3 <- ergm(red ~ edges + b2dsp(8))
