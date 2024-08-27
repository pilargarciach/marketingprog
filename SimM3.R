load("Results/Result3.RData")
rm(list=setdiff(ls(), c("red")))
library(ergm)
library(network)
library(coda)
set.seed(8903)
Model3 <- ergm(red ~ edges + b1sociality(c(3, 11, 13)) + 
                 b1nodematch("OnetImportance"), 
               control = control.ergm(MCMC.samplesize = 100000, 
                                      MCMC.burnin = 10000, 
                                      MCMLE.maxit = 10))
summary(Model3) # AIC = 7656
GOF <- gof(Model3)
plot(GOF)


Simuladas1 <- simulate(Model3, nsim = 1000, 
                       coef = Model3$coefficients,
                       control = 
                         control.simulate.ergm(
                           MCMC.burnin = 100000, 
                           MCMC.interval = 500))


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
    sim_model <- ergm(sim_net ~ edges + b1sociality(c(3, 11, 13)) + 
                        b1nodematch("OnetImportance"))
    sim_coefs <- coef(sim_model)
    
    # Agregar los coeficientes al data frame
    coef_df1[i, coef_names] <- sim_coefs
  }
  
  return(coef_df1)
}

coef_df1 <- extract_coefs_simulations(Simuladas1, Model3)

COEF1 <- Model3$coefficients[2]


hist(coef_df1$b1sociality3, main = "Distribución del estadístico en las redes simuladas",
     xlab = "Valor del estadístico")
abline(v = COEF1, col = "red", lwd = 2)


t.test(coef_df1$edges, mu = Model3$coefficients[1])
t.test(coef_df1$b1sociality3, mu = Model3$coefficients[2])

cov_sim <- cov(coef_df1[c(3,5:8)])
observed_stats <- Model3$coefficients
coef <- coef_df1[c(3,5:8)]

MD <- mahalanobis(x = t(observed_stats), center = colMeans(coef), cov = cov_sim)
MD

hist(observed_stats, main = "Distribución de las distancias de Mahalanobis", 
     xlab = "Distancia de Mahalanobis")
abline(v = MD, col = "red", lwd = 2)

cor(observed_stats, colMeans(coef[1:5]))
Resumen <- summary(Model3)
Resumen$aic
Resumen$bic


mcmc.diagnostics(Model3) # it works only if force MCMC 
