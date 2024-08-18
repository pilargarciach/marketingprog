load("Results/Result3.RData")
rm(list=setdiff(ls(), c("red")))
library(ergm)
library(network)
library(coda)
set.seed(9173)
ModelB <- ergm(red ~ edges + b1sociality(c(3, 11, 13)), 
               control = control.ergm(MCMC.samplesize = 10000,
                                      MCMC.burnin = 5000,
                                      MCMLE.maxit = 10))
summary(ModelB) # AIC = 7664
GOF <- gof(ModelB)
GOF1 <- gof(ModelB, GOF = ~model)
plot(GOF)
plot(GOF1)
GOF1


ModelB <- ergm(formula = red ~ edges + b1sociality(c(3, 11, 13)), 
               control = control.ergm(MCMC.samplesize = 10000, MCMC.burnin = 5000, 
                                      MCMLE.maxit = 10, force.main = TRUE))

mcmc.diagnostics(ModelB)
gof(ModelB, GOF=~model)

Simuladas0 <- simulate(ModelB, nsim = 1000, 
                       coef = ModelB$coefficients,
                       control = 
                         control.simulate.ergm(
                           MCMC.burnin = 100000, 
                           MCMC.interval = 500))


library(tidyverse)
extract_coefs_simulations <- function(Simuladas0, model) {
  # Obtener el número de simulaciones
  num_sims <- length(Simuladas0)
  
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
    sim_net <- Simuladas0[[i]]
    sim_model <- ergm(sim_net ~ edges + b1sociality(c(3, 11, 13)))
    sim_coefs <- coef(sim_model)
    
    # Agregar los coeficientes al data frame
    coef_df1[i, coef_names] <- sim_coefs
  }
  
  return(coef_df1)
}

coef_df1 <- extract_coefs_simulations(Simuladas0, ModelB)

COEF1 <- ModelB$coefficients[2]


hist(coef_df1$b1sociality3, main = "Distribución del estadístico en las redes simuladas",
     xlab = "Valor del estadístico")
abline(v = COEF1, col = "red", lwd = 2)


t.test(coef_df1$edges, mu = ModelB$coefficients[1])
t.test(coef_df1$b1sociality3, mu = ModelB$coefficients[2])

cov_sim <- cov(coef_df1[c(3,5:7)])
observed_stats <- ModelB$coefficients
coef <- coef_df1[c(3,5:7)]

MD <- mahalanobis(x = t(observed_stats), center = colMeans(coef), cov = cov_sim)

hist(observed_stats, main = "Distribución de las distancias de Mahalanobis", 
     xlab = "Distancia de Mahalanobis")
abline(v = MD, col = "red", lwd = 2)

cor(observed_stats, colMeans(coef[1:4]))
Resumen <- summary(ModelB)
Resumen$aic
Resumen$bic


mcmc.diagnostics(ModelB) # it works only if force MCMC 
