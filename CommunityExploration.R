load("Results/Result3.RData")
library(bipartite)
pave <- computeModules(BiM, method="DormannStrauss", deep = TRUE, steps = 30)

png("Bipartite.png", width = 22, height = 10, units = 'in', res = 300)
plotModuleWeb(pave, 
              plotModules = TRUE, 
              rank = TRUE, 
              displayAlabels = TRUE, 
              displayBlabels = FALSE,
              labsize = 4,
              xlabel = "Skills")
dev.off()
