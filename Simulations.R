load("Results/Result3.RData")
rm(list=setdiff(ls(), c("red", "BiM", "BN")))
SkillsNumber <- data.frame(Skill = rownames(BiM))
library(ergm)
library(network)
library(coda)
red
sna::gden(red)
set.seed(1107)
ModelA <- ergm(red ~ edges)
summary(ModelA) # AIC = 8729
ModelB <- ergm(red ~ b1sociality(nodes = c(3, 11, 13, 2, 6)))
summary(ModelB) # AIC = 9650 PEOR!
ModelC <- ergm(red ~ edges + b1sociality(c(3, 11, 13, 2, 6)))
summary(ModelC) # AIC = 7461
ModelD <- ergm(red ~ edges + b1sociality(nodes = c(1:28)))
summary(ModelD) # AIC = 6828


Simuladas1 <- simulate(ModelC, nsim = 1000)
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


p_value <- mean(df_redes$edges_ergm >= COEF1)

GOF1 <- gof(ModelC, GOF = ~model)
GOF1











# Modelo con efecto de homofilia en las competencias



# Modelo sin intercepto (no recomendado en general)
model2 <- ergm(red ~ b1sociality(nodes = c(1:28))) 

model3 <- ergm(red ~ edges + b1star(75))
summary(model3) # Modelo degenerado
mcmc.diagnostics(model3)

model3 <- ergm(red ~ edges + b2dsp(8))
