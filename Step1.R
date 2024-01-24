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

Programs <- tokens(Textos, 
                     remove_numbers = TRUE, 
                     remove_punct = TRUE, 
                     remove_url = TRUE, 
                     remove_symbols = TRUE) %>%  
  tokens_remove(stopwords("spanish"))

save.image("Results/Result1.RData")
