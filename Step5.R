load("Results/Result1.RData")
load("Results/Result2.RData")
textos$docname <- paste0("text", 1:length(textos$doc_id))
rm(list=setdiff(ls(), c("textos","SS")))


SoftSkills <- merge(SS, textos, by.x = "docname", by.y = "docname", all.x = TRUE)

library(dplyr)
ALL <- SoftSkills %>% select(., c(pattern, docname))
AM <- SoftSkills %>% filter(., Region=="AM") %>% select(., c(pattern, docname))
EU.ME.AF <- SoftSkills %>% filter(., Region=="EU-ME-AF") %>% select(., c(pattern, docname))
AP <- SoftSkills %>% filter(., Region=="AP") %>% select(., c(pattern, docname))

library(igraph)
all <- graph.data.frame(ALL, directed = FALSE)
eu.me.af <- graph.data.frame(EU.ME.AF, directed = FALSE)
am <- graph.data.frame(AM, directed = FALSE)
ap <- graph.data.frame(AP, directed = FALSE)

igraph::vcount(all)
igraph::vcount(eu.me.af)
igraph::vcount(am)
igraph::vcount(ap)

igraph::ecount(all)
igraph::ecount(eu.me.af)
igraph::ecount(am)
igraph::ecount(ap)
igraph::edge_density(all)
igraph::edge_density(eu.me.af)
igraph::edge_density(am)
igraph::edge_density(ap)
igraph::diameter(all)
igraph::diameter(eu.me.af)
igraph::diameter(am)
igraph::diameter(ap)

Region1 <- data.frame(Degree = igraph::degree(am),
                      Closeness = igraph::closeness(am),
                      Betweennes = igraph::betweenness(am),
                      Eigen = igraph::eigen_centrality(am))
Region1 <- Region1[ -c(5:25) ]
rownames(Region1)
Region1$SS <- rownames(Region1)
Region1 <- Region1[order(Region1$SS), ]
Region1 <- Region1[!grepl('text', Region1$SS), ]
Region1$Region <- "AM"

Region2 <- data.frame(Degree = igraph::degree(eu.me.af),
                      Closeness = igraph::closeness(eu.me.af),
                      Betweennes = igraph::betweenness(eu.me.af),
                      Eigen = igraph::eigen_centrality(eu.me.af))
Region2 <- Region2[ -c(5:25) ]
rownames(Region2)
Region2$SS <- rownames(Region2)
Region2 <- Region2[order(Region2$SS), ]
Region2 <- Region2[!grepl('text', Region2$SS), ]
Region2$Region <- "EU-ME-AF"

Region3 <- data.frame(Degree = igraph::degree(ap),
                      Closeness = igraph::closeness(ap),
                      Betweennes = igraph::betweenness(ap),
                      Eigen = igraph::eigen_centrality(ap))
Region3 <- Region3[ -c(5:25) ]
rownames(Region3)
Region3$SS <- rownames(Region3)
Region3 <- Region3[order(Region3$SS), ]
Region3 <- Region3[!grepl('text', Region3$SS), ]
Region3$Region <- "AP"





rm(list=setdiff(ls(), c("Region1","Region2", "Region3")))

Programs <- rbind(Region1, Region2, Region3)
scaled_programs <- scale(Programs[c(1:4)])
rescaled <- data.frame(apply(scaled_programs, 2, function(x) (x - min(x)) / (max(x) - min(x))))
rescaled$SS <- Programs$SS
rescaled$Region <- Programs$Region

library(tidyverse)
Program <- rescaled %>% 
  pivot_longer(c(`Degree`, `Closeness`, `Betweennes`, `Eigen.vector`), names_to = "Centrality", values_to = "cases")



library(ggplot2)
R1 <- ggplot(rescaled, aes(x=reorder(SS, Eigen.vector), y=Eigen.vector)) +
  geom_point(size=5, aes(colour=Region), alpha=0.6) + 
  scale_color_manual(values=c("#C71135", "#1a50a6", "#2E851B")) +
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

R2 <- ggplot(rescaled, aes(x=reorder(SS, Closeness), y=Closeness)) +
  geom_point(size=5, aes(colour=Region), alpha=0.6) +
  scale_color_manual(values=c("#C71135", "#1a50a6", "#2E851B")) +
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

R3 <- ggplot(rescaled, aes(x=reorder(SS, Betweennes), y=Betweennes)) +
  geom_point(size=5, aes(colour=Region), alpha=0.6) +
  scale_color_manual(values=c("#C71135", "#1a50a6", "#2E851B")) +  # Set colors
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

R4 <- ggplot(rescaled, aes(x=reorder(SS, Degree), y=Degree)) +
  geom_point(size=5, aes(colour=Region), alpha=0.6) +
  scale_color_manual(values=c("#C71135", "#1a50a6", "#2E851B")) +
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
png("R1.png", width = 32, height = 18, units = 'in', res = 300)
ggarrange(R1, R2, R3, R4, ncol = 4, nrow = 1)
dev.off()



library(ggridges)
ggplot(Programs, aes(x = Closeness, y = Region, fill = Region)) +
  geom_density_ridges(alpha = 0.3) +
  scale_fill_manual(values = c("#C71135", "#1a50a6", "#2E851B")) +
  theme_ridges() + 
  theme(legend.position = "none",
        axis.text.x=element_text(size=30, colour="black"),
        axis.text.y=element_text(size=30, colour="black"),
        axis.title.x=element_text(face="italic", colour="black", size=30),
        axis.title.y=element_text(face="italic", colour="black", size=30)) +
  xlab("Closeness Centrality") + 
  ylab("")