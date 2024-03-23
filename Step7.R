load("Results/Result1.RData")
load("Results/Result2.RData")
textos$docname <- paste0("text", 1:length(textos$doc_id))
rm(list=setdiff(ls(), c("textos","SS")))
SoftSkills <- merge(SS, textos, by.x = "docname", by.y = "docname", all.x = TRUE)

library(dplyr)
Public <- SoftSkills %>% filter(., SchoolType=="Public") %>% select(., c(pattern, docname))
Private <- SoftSkills %>% filter(., SchoolType=="Private") %>% select(., c(pattern, docname))

library(igraph)
public <- graph.data.frame(Public, directed = FALSE)
private <- graph.data.frame(Private, directed = FALSE)


PUBLIC <- data.frame(Degree = igraph::degree(public),
                      Closeness = igraph::closeness(public),
                      Betweennes = igraph::betweenness(public),
                      Eigen = igraph::eigen_centrality(public))
PUBLIC <- PUBLIC[ -c(5:25) ]
rownames(PUBLIC)
PUBLIC$SS <- rownames(PUBLIC)
PUBLIC <- PUBLIC[order(PUBLIC$SS), ]
PUBLIC <- PUBLIC[!grepl('text', PUBLIC$SS), ]
PUBLIC$SchoolType <- "Public"

PRIVATE <- data.frame(Degree = igraph::degree(private),
                      Closeness = igraph::closeness(private),
                      Betweennes = igraph::betweenness(private),
                      Eigen = igraph::eigen_centrality(private))
PRIVATE <- PRIVATE[ -c(5:25) ]
rownames(PRIVATE)
PRIVATE$SS <- rownames(PRIVATE)
PRIVATE <- PRIVATE[order(PRIVATE$SS), ]
PRIVATE <- PRIVATE[!grepl('text', PRIVATE$SS), ]
PRIVATE$SchoolType <- "Private"

rm(list=setdiff(ls(), c("PUBLIC","PRIVATE")))

Programs <- rbind(PUBLIC, PRIVATE)
scaled_programs <- scale(Programs[c(1:4)])
rescaled <- data.frame(apply(scaled_programs, 2, function(x) (x - min(x)) / (max(x) - min(x))))
rescaled$SS <- Programs$SS
rescaled$SchoolType <- Programs$SchoolType

library(tidyverse)
Program <- rescaled %>% 
  pivot_longer(c(`Degree`, `Closeness`, `Betweennes`, `Eigen.vector`), names_to = "Centrality", values_to = "cases")


library(ggplot2)
R1 <- ggplot(Programs, aes(x=reorder(SS, Eigen.vector), y=Eigen.vector)) +
  geom_point(size=5, aes(colour=SchoolType), alpha=0.6) + 
  scale_color_manual(values=c("#158466", "#729fcf")) +
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

R2 <- ggplot(Programs, aes(x=reorder(SS, Closeness), y=Closeness)) +
  geom_point(size=5, aes(colour=SchoolType), alpha=0.6) +
  scale_color_manual(values=c("#158466", "#729fcf")) +
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

R3 <- ggplot(Programs, aes(x=reorder(SS, Betweennes), y=Betweennes)) +
  geom_point(size=5, aes(colour=SchoolType), alpha=0.6) +
  scale_color_manual(values=c("#158466", "#729fcf")) +  # Set colors
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

R4 <- ggplot(Programs, aes(x=reorder(SS, Degree), y=Degree)) +
  geom_point(size=5, aes(colour=SchoolType), alpha=0.6) +
  scale_color_manual(values=c("#158466", "#729fcf")) +
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
png("R2.png", width = 32, height = 18, units = 'in', res = 300)
ggarrange(R1, R2, R3, R4, ncol = 4, nrow = 1)
dev.off()





library(ggridges)

png("R5.png", width = 22, height = 11, units = 'in', res = 300)
ggplot(Programs, aes(x = Closeness, y = SchoolType, fill = SchoolType)) +
  geom_density_ridges(alpha = 0.3) +
  scale_fill_manual(values = c("#158466", "#729fcf")) +
  theme_ridges() + 
  theme(legend.position = "none",
        axis.text.x=element_text(size=30, colour="black"),
        axis.text.y=element_text(size=30, colour="black"),
        axis.title.x=element_text(face="italic", colour="black", size=30),
        axis.title.y=element_text(face="italic", colour="black", size=30)) +
  xlab("Closeness Centrality") + 
  ylab("")
dev.off()