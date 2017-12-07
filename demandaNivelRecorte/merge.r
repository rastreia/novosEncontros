setwd("/home/administrador/Documentos/DemandaNivelRecorte/municipios/Todos") # caminho par ao seu diretório
arquivos <- list.files(path = "/home/administrador/Documentos/DemandaNivelRecorte/municipios/Todos", pattern = "*.csv") # caminho para o diretório e extensao deles

library(dplyr)
library(rio)



cocoFinal <- NULL
for( file in arquivos){
  print(file)
  boloFecal <- read.csv(file, header = TRUE, sep = ",")
  print(nrow(boloFecal))
  nomeMunicipio <- unlist(strsplit(file, ".csv"))
  vetorMunicipio <- rep(nomeMunicipio, nrow(boloFecal))
  boloFecal <- cbind(boloFecal, vetorMunicipio)
  cocoFinal <- rbind(cocoFinal, boloFecal)
}

############################ Tratamento de dados ####################################
copiaCoco <- cocoFinal
cocoFinal <- copiaCoco

cocoFinal <- cocoFinal[is.na(cocoFinal$V0007) == FALSE,]
cocoFinal$V0007[cocoFinal$V0007 == "SIM"] <- "TESTE"

View(cocoFinal[cocoFinal$V0007 == "SIM",]) 

