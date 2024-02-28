# 1.Initialization ----
# Step 1: Opening the sample of texts
# this local folder is a clone of the GitHub Repo
library(readtext)
textos <- readtext("Programs/")
Programas <- data.frame(table(textos$doc_id))

textos$doc_id <- sort(as.numeric(gsub("[^0-9]", "", textos$doc_id)), decreasing = FALSE)
sort(textos$doc_id, decreasing = FALSE)
textos$University <- c("TEC",
                       "Insper",
                       "Pontificia Universidad Católica de Chile",
                       "Pontificia Universidad Católica de Perú",
                       "Universdad Adolfo Ibañez Chile",
                       "Universidad de Los Andes",
                       "Universidad de Chile",
                       "Universidad ESAN",
                       "Universidad de Buenos Aires",
                       "Universidad ESAN",
                       "Universidad ORT",
                       "TEC", 
                       "Universidad San Ignasio Loyola",
                       "Universidad ICESI", 
                       "Universidad del Pacífico", 
                       "Universidad EAFIT", 
                       "FIA Business School",
                       "FIA Business School",
                       "IESA",
                       "INCAE")


library(quanteda)
Textos <- corpus(textos$text)
docvars(Textos, "Program") <- textos$doc_id
docvars(Textos, "University") <- textos$University
summary(Textos)
TextosData <- data.frame(summary(Textos))
hist(TextosData$Sentences)
Programs <- tokens(Textos, 
                     remove_numbers = TRUE, 
                     remove_punct = TRUE, 
                     remove_url = TRUE, 
                     remove_symbols = TRUE) %>%  
  tokens_remove(stopwords("spanish"))

save.image("Results/Result1.RData")
# Saved_Results ----


library(readr)
Inst <- read_csv("GeolocationInstitutions.csv")
library(treemap)
colnames(Inst)[1] <- "School Type"
colnames(Inst)[2] <- "Geolocation"
colnames(Inst)[3] <- "value"
variable.names(Inst)

library(png)
library(treemap)
png("F00.png", width = 15, height = 7, units = 'in', res = 300)
treemap(Inst,
        index=c("Geolocation", "School Type"),
        vSize="value",
        vColor = "School Type",
        type="index",
        title = "",
        algorithm = "pivotSize",
        mirror.x = TRUE,
        mirror.y = FALSE,
        palette = "Set1")
dev.off()
