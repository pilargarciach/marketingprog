load("Results/Result1.RData")
load("Results/Result2.RData")
textos$docname <- paste0("text", 1:length(textos$doc_id))
rm(list=setdiff(ls(), c("textos","SS")))


SoftSkills <- merge(SS, textos, by.x = "docname", by.y = "docname", all.x = TRUE)

library(dplyr)
AM <- SoftSkills %>% filter(., Region=="AM") %>% select(., c(pattern, docname))
EU.ME.AF <- SoftSkills %>% filter(., Region=="EU-ME-AF") %>% select(., c(pattern, docname))
AP <- SoftSkills %>% filter(., Region=="AP") %>% select(., c(pattern, docname))

library(igraph)
eu.me.af <- graph.data.frame(EU.ME.AF, directed = FALSE)
am <- graph.data.frame(AM, directed = FALSE)
ap <- graph.data.frame(AP, directed = FALSE)

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
ggplot(rescaled, aes(x=reorder(SS, Eigen.vector), y=Eigen.vector)) +
  geom_point(size=5, aes(colour=Region), alpha=0.6) + 
  theme_bw() +
  theme(axis.text.x = element_text(angle=60, hjust=1),
        panel.grid.major.y = element_line(colour="grey60", linetype="dashed"),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_line(colour="grey60", linetype="dashed")) +
  coord_flip() +
  theme(legend.position="top",
        axis.text.x=element_text(size=15, colour="black"),
        axis.text.y=element_text(size=15, colour="black"),
        axis.title.x=element_text(face="italic", colour="black", size=20),
        axis.title.y=element_text(face="italic", colour="black", size=20)) +
  xlab("Soft Skills") +
  ylab("Eigenvector Centrality") +
  theme(legend.position=c(0.95,0.1), legend.justification=c(0.95,0.1))

ggplot(rescaled, aes(x=reorder(SS, Closeness), y=Closeness)) +
  geom_point(size=5, aes(colour=Program), alpha=0.6) +
  scale_color_manual(values=c("orange", "darkgreen")) +  # Set colors
  theme_bw() +
  theme(axis.text.x = element_text(angle=60, hjust=1),
        panel.grid.major.y = element_line(colour="grey60", linetype="dashed"),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_line(colour="grey60", linetype="dashed")) +
  coord_flip() +
  theme(legend.position="top",
        axis.text.x=element_text(size=15, colour="black"),
        axis.text.y=element_text(size=15, colour="black"),
        axis.title.x=element_text(face="italic", colour="black", size=20),
        axis.title.y=element_text(face="italic", colour="black", size=20)) +
  xlab("Soft Skills") +
  ylab("Closeness Centrality") +
  theme(legend.position=c(0.95,0.1), legend.justification=c(0.95,0.1))

ggplot(rescaled, aes(x=reorder(SS, Betweennes), y=Betweennes)) +
  geom_point(size=5, aes(colour=Program), alpha=0.6) +
  scale_color_manual(values=c("orange", "darkgreen")) +  # Set colors
  theme_bw() +
  theme(axis.text.x = element_text(angle=60, hjust=1),
        panel.grid.major.y = element_line(colour="grey60", linetype="dashed"),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_line(colour="grey60", linetype="dashed")) +
  coord_flip() +
  theme(legend.position="top",
        axis.text.x=element_text(size=15, colour="black"),
        axis.text.y=element_text(size=15, colour="black"),
        axis.title.x=element_text(face="italic", colour="black", size=20),
        axis.title.y=element_text(face="italic", colour="black", size=20)) +
  xlab("Soft Skills") +
  ylab("Betweenness Centrality") +
  theme(legend.position=c(0.95,0.1), legend.justification=c(0.95,0.1))

ggplot(rescaled, aes(x=reorder(SS, Degree), y=Degree)) +
  geom_point(size=5, aes(colour=Program), alpha=0.6) +
  scale_color_manual(values=c("orange", "darkgreen")) +  # Set colors
  theme_bw() +
  theme(axis.text.x = element_text(angle=60, hjust=1),
        panel.grid.major.y = element_line(colour="grey60", linetype="dashed"),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_line(colour="grey60", linetype="dashed")) +
  coord_flip() +
  theme(legend.position="top",
        axis.text.x=element_text(size=15, colour="black"),
        axis.text.y=element_text(size=15, colour="black"),
        axis.title.x=element_text(face="italic", colour="black", size=20),
        axis.title.y=element_text(face="italic", colour="black", size=20)) +
  xlab("Soft Skills") +
  ylab("Degree Centrality") +
  theme(legend.position=c(0.95,0.1), legend.justification=c(0.95,0.1))


library(ggridges)
ggplot(Programs, aes(x = Closeness, y = Program, fill = Program)) +
  geom_density_ridges(alpha = 0.3) +
  scale_fill_manual(values = c("orange", "darkgreen")) +  # Set colors
  theme_ridges() + 
  theme(legend.position = "none",
        axis.text.x=element_text(size=20, colour="black"),
        axis.text.y=element_text(size=20, colour="black"),
        axis.title.x=element_text(face="italic", colour="black", size=20),
        axis.title.y=element_text(face="italic", colour="black", size=20)) +
  xlab("Closeness Centrality") + 
  ylab("Academic Program")

ggplot(Programs, aes(x = Degree, y = Program, fill = Program)) +
  geom_density_ridges(alpha = 0.3) +
  scale_fill_manual(values = c("orange", "darkgreen")) +  # Set colors
  theme_ridges() + 
  theme(legend.position = "none",
        axis.text.x=element_text(size=20, colour="black"),
        axis.text.y=element_text(size=20, colour="black"),
        axis.title.x=element_text(face="italic", colour="black", size=20),
        axis.title.y=element_text(face="italic", colour="black", size=20)) +
  xlab("Degree Centrality") + 
  ylab("Academic Program")

ggplot(Programs, aes(x = Betweennes, y = Program, fill = Program)) +
  geom_density_ridges(alpha = 0.3) +
  scale_fill_manual(values = c("orange", "darkgreen")) +  # Set colors
  theme_ridges() + 
  theme(legend.position = "none",
        axis.text.x=element_text(size=20, colour="black"),
        axis.text.y=element_text(size=20, colour="black"),
        axis.title.x=element_text(face="italic", colour="black", size=20),
        axis.title.y=element_text(face="italic", colour="black", size=20)) +
  xlab("Betweenness Centrality") + 
  ylab("Academic Program")

ggplot(Program, aes(x = cases, y = Centrality, color = Program, point_color = Program, fill = Program)) +
  geom_density_ridges(
    jittered_points = TRUE, scale = .95, rel_min_height = .01,
    point_shape = "|", point_size = 3, size = 0.25,
    position = position_points_jitter(height = 0)
  ) +
  scale_y_discrete(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0), name = "Centrality (rescaled 0-1)") +
  scale_fill_manual(values = c("orange", "darkgreen"), labels = c("bachelor", "postgraduate")) +
  coord_cartesian(clip = "off") + theme_minimal()
