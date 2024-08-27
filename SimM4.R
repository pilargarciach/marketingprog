load("Results/Result3.RData")
rm(list=setdiff(ls(), c("red")))
red
library(ergm)
library(network)
library(coda)
set.seed(9615)
Model4 <- ergm(red ~ edges + b1sociality(c(3, 11, 13, 2, 6)) + 
                  b1nodematch("OnetImportance"), 
                control = control.ergm(MCMC.samplesize = 100000, 
                                       MCMC.burnin = 10000, 
                                       MCMLE.maxit = 10))
summary(Model4) # AIC = 7469

GOF <- gof(Model4)
plot(GOF)
GOF

Simuladas4 <- simulate(Model4, nsim = 1000, 
                       coef = Model4$coefficients,
                       control = 
                         control.simulate.ergm(
                           MCMC.burnin = 100000, 
                           MCMC.interval = 500))


library(tidyverse)
extract_coefs_simulations <- function(Simuladas4, model) {
  # Obtener el número de simulaciones
  num_sims <- length(Simuladas4)
  
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
    sim_net <- Simuladas4[[i]]
    sim_model <- ergm(sim_net ~ edges + b1sociality(c(3, 11, 13, 2, 6)) + 
                        b1nodematch("OnetImportance"))
    sim_coefs <- coef(sim_model)
    
    # Agregar los coeficientes al data frame
    coef_df1[i, coef_names] <- sim_coefs
  }
  
  return(coef_df1)
}

coef_df1 <- extract_coefs_simulations(Simuladas4, Model4)

COEF1 <- Model4$coefficients[2]


hist(coef_df1$b1sociality3, main = "Distribución del estadístico en las redes simuladas",
     xlab = "Valor del estadístico")
abline(v = COEF1, col = "red", lwd = 2)


t.test(coef_df1$edges, mu = Model4$coefficients[1])
t.test(coef_df1$b1sociality3, mu = Model4$coefficients[2])

cov_sim <- cov(coef_df1[c(3,5:10)])
observed_stats <- Model4$coefficients
coef <- coef_df1[c(3,5:10)]

MD <- mahalanobis(x = t(observed_stats), center = colMeans(coef), cov = cov_sim)
MD

hist(observed_stats, main = "Distribución de las distancias de Mahalanobis", 
     xlab = "Distancia de Mahalanobis")
abline(v = MD, col = "red", lwd = 2)

cor(observed_stats, colMeans(coef[1:7]))
Resumen <- summary(Model4)
Resumen$aic
Resumen$bic


mcmc.diagnostics(Model4) # it works only if force MCMC 
