# 1.Initialization ----
# Step 1: Opening the sample of texts
# this local folder is a clone of the GitHub Repo
library(readtext)
textos <- readtext("Programs/")
library(readr)
AU <- read_csv("Accredited Universities.csv")
colnames(AU)[4] <- "SchoolType"
library(tools)
textos$institution <- file_path_sans_ext(textos$doc_id)
textos$Institution <- gsub("^P\\w+(\\.)?", "", textos$institution)
textos$Institution <- gsub("\\.docx$", "", textos$Institution)
textos$Institution <- sub("^ ", "", textos$Institution)
textos$institution <- NULL

library(tidyverse)
textos <- left_join(textos, AU, by = "Institution", keep = FALSE)
Institutions <- data.frame(table(textos$Institution))

library(quanteda)
Textos <- corpus(textos$text)
docvars(Textos, "Region") <- textos$Region
docvars(Textos, "Institution") <- textos$Institution
docvars(Textos, "InstitutionType") <- textos$SchoolType
summary(Textos)
TextosData <- as.data.frame(summary(Textos, n = length(textos$text)))
TextosData$InstitutionType[c(14,25,116,117,164,176,261)] <- "Public"
TextosData$Region[c(14,25,116,117,164,176,261)] <- c("AM","EU-ME-AF","AM", "AM", "AM", "AM", "AP" )
table(TextosData$InstitutionType)
table(TextosData$Region)
hist(TextosData$Sentences)
Programs <- tokens(Textos, 
                     remove_numbers = TRUE, 
                     remove_punct = TRUE, 
                     remove_url = TRUE, 
                     remove_symbols = TRUE) %>%  
  tokens_remove(stopwords("english"))

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
png("F00.png", width = 20, height = 7, units = 'in', res = 300)
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
