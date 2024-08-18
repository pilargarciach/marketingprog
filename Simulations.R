load("Results/Result3.RData")
rm(list=setdiff(ls(), c("red")))
library(ergm)
library(network)
library(coda)
set.seed(1107)
ModelC <- ergm(red ~ edges + b1sociality(c(3, 11, 13, 2, 6)), 
               control = control.ergm(MCMC.samplesize = 10000,
                                      MCMC.burnin = 5000,
                                      MCMLE.maxit = 10))
summary(ModelC) # AIC = 7461
GOF <- gof(ModelC)
GOF1 <- gof(ModelC, GOF = ~model)
plot(GOF)
plot(GOF1)
GOF1


ModelC <- ergm(formula = red ~ edges + b1sociality(c(3, 11, 13, 2, 6)), 
               control = control.ergm(MCMC.samplesize = 10000, MCMC.burnin = 5000, 
                                      MCMLE.maxit = 10, force.main = TRUE))

mcmc.diagnostics(ModelC)
gof(ModelC, GOF=~model)

Simuladas1 <- simulate(ModelC, nsim = 1000, 
                       coef = ModelC$coefficients,
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


t.test(coef_df1$edges, mu = ModelC$coefficients[1])
t.test(coef_df1$b1sociality3, mu = ModelC$coefficients[2])

cov_sim <- cov(coef_df1[c(3,5:9)])
observed_stats <- ModelC$coefficients
coef <- coef_df1[c(3,5:9)]

MD <- mahalanobis(x = t(observed_stats), center = colMeans(coef), cov = cov_sim)

hist(MD, main = "Distribución de las distancias de Mahalanobis", 
     xlab = "Distancia de Mahalanobis")
abline(v = MD, col = "red", lwd = 2)

cor(observed_stats, colMeans(coef[1:6]))




mcmc.diagnostics(ModelC) # it works only if force MCMC 
