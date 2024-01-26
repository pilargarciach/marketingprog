load("Results/Result3.RData")
BNA <- graph.data.frame(Network, directed = FALSE)
Programs <- data.frame(Degree = igraph::degree(BNA),
                       Closeness = igraph::closeness(BNA),
                       Betweennes = igraph::betweenness(BNA),
                       Eigen = igraph::eigen_centrality(BNA))
Programs <- Programs[ -c(5:25) ]
rownames(Programs)
Programs$SS <- rownames(Programs)
Programs <- Programs[!grepl("^text", Programs$SS),]
colnames(Programs)[4] <- "Eigenvector"
colnames(Programs)

Programs1 <- Programs[order(-Programs$Degree),]
Programs1$Rank <- 1:length(Programs1$Degree)
Programs2 <- Programs[order(-Programs$Eigenvector),]
Programs2$Rank <- 1:length(Programs2$Eigenvector)
Programs3 <- Programs[order(-Programs$Closeness),]
Programs3$Rank <- 1:length(Programs3$Eigenvector)
Programs4 <- Programs[order(-Programs$Betweennes),]
Programs4$Rank <- 1:length(Programs4$Betweennes)

SkillsEigen <- head(Programs2$SS, 10)
SkillsClose <- head(Programs3$SS, 10)
SkillsBetween <- head(Programs4$SS, 10)

SkillsEigen == SkillsClose
SkillsEigen == SkillsBetween
SkillsBetween == SkillsClose

IM <- as_incidence_matrix(BNA, names = TRUE, sparse = TRUE, types = bipartite_mapping(BNA)$type)
IM2 <- as.matrix(IM)
# Let's pick the most important soft skills
# as per their eigenvector centrality


# Subset the matrix by column names
IM3 <- IM2[, SkillsEigen, drop = FALSE]
colnames(IM3)[colnames(IM3) == "S49"] <- "Strategy"
colnames(IM3)[colnames(IM3) == "S10"] <- "Management"
colnames(IM3)[colnames(IM3) == "S43"] <- "Innovation"
colnames(IM3)[colnames(IM3) == "S21"] <- "Generate"
colnames(IM3)[colnames(IM3) == "S6"] <- "Leadership"
colnames(IM3)[colnames(IM3) == "S3"] <- "Communication"
colnames(IM3)[colnames(IM3) == "S44"] <- "Decision-Making"
colnames(IM3)[colnames(IM3) == "S25"] <- "Acknowledge"
colnames(IM3)[colnames(IM3) == "S4"] <- "Create"
colnames(IM3)[colnames(IM3) == "S13"] <- "Ethics"


library(bipartite)
png("F3.png", width = 35, height = 7, units = 'in', res = 300)
plotweb(IM3, method = "normal", 
        col.high = "red", 
        bor.col.high = "red",
        col.low = "darkgreen", 
        bor.col.low = "darkgreen",
        col.interaction = "grey90",
        bor.col.interaction = "grey90",
        low.lablength = 0,
        labsize = 3,
        arrow = "up")
dev.off()



IM3 <- IM2[, SkillsClose, drop = FALSE]
colnames(IM3)[colnames(IM3) == "S49"] <- "Strategy"
colnames(IM3)[colnames(IM3) == "S21"] <- "Generate"
colnames(IM3)[colnames(IM3) == "S10"] <- "Management"
colnames(IM3)[colnames(IM3) == "S3"] <- "Communication"
colnames(IM3)[colnames(IM3) == "S4"] <- "Create"
colnames(IM3)[colnames(IM3) == "S44"] <- "Decision-Making"
colnames(IM3)[colnames(IM3) == "S25"] <- "Acknowledge"
colnames(IM3)[colnames(IM3) == "S43"] <- "Innovation"
colnames(IM3)[colnames(IM3) == "S6"] <- "Leadership"
colnames(IM3)[colnames(IM3) == "S42"] <- "Evaluate"


png("F4.png", width = 35, height = 7, units = 'in', res = 300)
plotweb(IM3, method = "normal", 
        col.high = "yellow", 
        bor.col.high = "yellow",
        col.low = "darkgreen", 
        bor.col.low = "darkgreen",
        col.interaction = "grey90",
        bor.col.interaction = "grey90",
        low.lablength = 0,
        labsize = 3,
        arrow = "up")
dev.off()



IM3 <- IM2[, SkillsBetween, drop = FALSE]

colnames(IM3)[colnames(IM3) == "S49"] <- "Strategy"
colnames(IM3)[colnames(IM3) == "S10"] <- "Management"
colnames(IM3)[colnames(IM3) == "S21"] <- "Generate"
colnames(IM3)[colnames(IM3) == "S43"] <- "Innovation"
colnames(IM3)[colnames(IM3) == "S44"] <- "Decision-Making"
colnames(IM3)[colnames(IM3) == "S25"] <- "Acknowledge"
colnames(IM3)[colnames(IM3) == "S3"] <- "Communication"
colnames(IM3)[colnames(IM3) == "S4"] <- "Create"
colnames(IM3)[colnames(IM3) == "S6"] <- "Leadership"
colnames(IM3)[colnames(IM3) == "S23"] <- "Sharing"

png("F5.png", width = 35, height = 7, units = 'in', res = 300)
plotweb(IM3, method = "normal", 
        col.high = "blue2", 
        bor.col.high = "blue2",
        col.low = "darkgreen", 
        bor.col.low = "darkgreen",
        col.interaction = "grey90",
        bor.col.interaction = "grey90",
        low.lablength = 0,
        labsize = 3,
        arrow = "up")
dev.off()


SKILLS <- list()
SKILLS[[1]] <- Programs1[5:6]

SKILLS[[2]] <- Programs2[5:6]

SKILLS[[3]] <- Programs3[5:6]

SKILLS[[4]] <- Programs4[5:6]

irr::iota(SKILLS, scaledata = "nominal")

library(irr)
data("diagnoses")
kappam.fleiss(diagnoses) 

kappam.fleiss(diagnoses, exact=TRUE)
kappam.fleiss(diagnoses, detail=TRUE)
kappam.fleiss(diagnoses[,1:4])
