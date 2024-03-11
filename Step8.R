load("Results/Result1.RData")
load("Results/Result2.RData")
textos$docname <- paste0("text", 1:length(textos$doc_id))
rm(list=setdiff(ls(), c("textos","SS")))


SoftSkills <- SS[c(1,7)]
library(igraph)
softskills <- graph.data.frame(SoftSkills, directed = FALSE)

SOFTSKILLS <- data.frame(Degree = igraph::degree(softskills),
                         Closeness = igraph::closeness(softskills),
                         Betweennes = igraph::betweenness(softskills),
                         Eigen = igraph::eigen_centrality(softskills))
SOFTSKILLS <- SOFTSKILLS[ -c(5:25) ]
rownames(SOFTSKILLS)
SOFTSKILLS$SS <- rownames(SOFTSKILLS)
SOFTSKILLS <- SOFTSKILLS[order(SOFTSKILLS$SS), ]
SOFTSKILLS <- SOFTSKILLS[!grepl('text', SOFTSKILLS$SS), ]

scaled_SOFTSKILLS <- scale(SOFTSKILLS[c(1:4)])
rescaled <- data.frame(apply(scaled_SOFTSKILLS, 2, function(x) (x - min(x)) / (max(x) - min(x))))
rescaled$SS <- SOFTSKILLS$SS


library(tidyverse)
Program <- rescaled %>% 
  pivot_longer(c(`Degree`, `Closeness`, `Betweennes`, `Eigen.vector`), names_to = "Centrality", values_to = "cases")


library(ggplot2)
R1 <- ggplot(SOFTSKILLS, aes(x=reorder(SS, Eigen.vector), y=Eigen.vector)) +
  geom_point(size=5, alpha=0.6) +
  theme_bw() +
  theme(axis.text.x = element_text(angle=60, hjust=1),
        panel.grid.major.y = element_line(colour="grey80", linetype="dashed"),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_line(colour="grey80", linetype="dashed")) +
  coord_flip() +
  theme(legend.position="top",
        axis.text.x=element_text(size=30, colour="black"),
        axis.text.y=element_text(size=30, colour="black"),
        axis.title.x=element_text(face="italic", colour="black", size=30),
        axis.title.y=element_text(face="italic", colour="black", size=30),
        legend.text = element_text(size = 30),
        legend.title = element_text(size = 30)) + # Increase legend font size
  xlab("Soft Skills") +
  ylab("Eigenvector Centrality") +
  theme(legend.position=c(0.95,0.02), legend.justification=c(0.95,0.1))

R2 <- ggplot(SOFTSKILLS, aes(x=reorder(SS, Closeness), y=Closeness)) +
  geom_point(size=5, alpha=0.6) +
  theme_bw() +
  theme(axis.text.x = element_text(angle=60, hjust=1),
        panel.grid.major.y = element_line(colour="grey80", linetype="dashed"),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_line(colour="grey80", linetype="dashed")) +
  coord_flip() +
  theme(legend.position="top",
        axis.text.x=element_text(size=30, colour="black"),
        axis.text.y=element_text(size=30, colour="black"),
        axis.title.x=element_text(face="italic", colour="black", size=30),
        axis.title.y=element_text(face="italic", colour="black", size=30),
        legend.text = element_text(size = 30),
        legend.title = element_text(size = 30)) + # Increase legend font size
  xlab("Soft Skills") +
  ylab("Closeness Centrality") +
  theme(legend.position=c(0.95,0.02), legend.justification=c(0.95,0.1))

R3 <- ggplot(SOFTSKILLS, aes(x=reorder(SS, Betweennes), y=Betweennes)) +
  geom_point(size=5, alpha=0.6) +
  theme_bw() +
  theme(axis.text.x = element_text(angle=60, hjust=1),
        panel.grid.major.y = element_line(colour="grey80", linetype="dashed"),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_line(colour="grey80", linetype="dashed")) +
  coord_flip() +
  theme(legend.position="top",
        axis.text.x=element_text(size=30, colour="black"),
        axis.text.y=element_text(size=30, colour="black"),
        axis.title.x=element_text(face="italic", colour="black", size=30),
        axis.title.y=element_text(face="italic", colour="black", size=30),
        legend.text = element_text(size = 30),
        legend.title = element_text(size = 30)) + # Increase legend font size
  xlab("Soft Skills") +
  ylab("Betweenness Centrality") +
  theme(legend.position=c(0.95,0.02), legend.justification=c(0.95,0.1))

R4 <- ggplot(SOFTSKILLS, aes(x=reorder(SS, Degree), y=Degree)) +
  geom_point(size=5, alpha=0.6) +
  theme_bw() +
  theme(axis.text.x = element_text(angle=60, hjust=1),
        panel.grid.major.y = element_line(colour="grey80", linetype="dashed"),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_line(colour="grey80", linetype="dashed")) +
  coord_flip() +
  theme(legend.position="top",
        axis.text.x=element_text(size=30, colour="black"),
        axis.text.y=element_text(size=30, colour="black"),
        axis.title.x=element_text(face="italic", colour="black", size=30),
        axis.title.y=element_text(face="italic", colour="black", size=30),
        legend.text = element_text(size = 30),
        legend.title = element_text(size = 30)) + # Increase legend font size
  xlab("Soft Skills") +
  ylab("Degree Centrality") +
  theme(legend.position=c(0.95,0.02), legend.justification=c(0.95,0.1))



library(ggpubr)
png("R0.png", width = 32, height = 18, units = 'in', res = 300)
ggarrange(R1, R2, R3, R4, ncol = 4, nrow = 1)
dev.off()

