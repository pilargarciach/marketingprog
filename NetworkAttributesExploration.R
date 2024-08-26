load("Results/Result3.RData")
rm(list=setdiff(ls(), c("red", "BN")))
library(ergm)
library(network)
library(coda)
red



Atributos <- data.frame(Nodos = get.vertex.attribute(red, 'vertex.names'),
                        ONET = get.vertex.attribute(red, 'OnetImportance'),
                        nodos = rownames(BN),
                        Grado = BN$Degree)
cor(Atributos$ONET, Atributos$Grado)

library(ggplot2)
ggplot(Atributos, aes(x=ONET[1:28], y=Grado[1:28])) + 
  geom_point()+
  geom_smooth(method=lm)
