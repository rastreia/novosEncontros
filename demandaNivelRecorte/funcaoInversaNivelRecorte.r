library(data.table)
library(bit64)
library(descr)
library(dplyr)
library(tidyr)
library(magrittr)
library(lubridate)
library(ggplot2)
library(xtable)
library(lme4)
library(merTools)
library(lmtest)
library(texreg)
library(xlsx)
library(readr)
library(readxl)

###################################### Leitura dos Bancos ############################################
setwd("/home/administrador/Documentos/DemandaNivelRecorte/")
listaSPE <- fread("mergeSres.csv") #Aquivo merge de todas as listas SPE

setwd("/home/administrador/Documentos/DemandaNivelRecorte/municipios/Todos/")
listaMunicipio <- fread("mergeMunicipios.csv")
#names(listaMunicipio) <- c('ranking','nome','apelido','cpf', 'nis','endereco')



################ Tratamento de dados (divisão da listaMunicipio em dois DF) ###########################

#listaMunicipioSemNIS <- (listaMunicipio[is.na(listaMunicipio$V0004)]) 

#listaMunicipioComNIS <- subset(listaMunicipio, !is.na(lista$V0004)) 

#View(subset(listaSPE, !is.na(listaSPE$NIS)))
#View (listaSPE[is.na(listaSPE$NIS)])

###################### Nível de Manutenção do Recortepor - NIS ############################
#Nis com Nis
listaSPE$NIS[is.na(listaSPE$NIS)] <- 999 # Limpando NAs Demoniacos
listaSPE$NIS[is.na(listaMunicipio$V0004)] <- 666 # Limpando NAs Demoniacos
nisPertenceListaMunicipios <- NULL
nisPertenceListaMunicipios <- is.element(as.numeric(listaSPE$NIS), as.numeric(listaMunicipio$V0004))
listaSPE <- cbind(listaSPE, nisPertenceListaMunicipios)

#Nis com CPF
listaSPE$NIS[is.na(listaSPE$NIS)] <- 999 # Limpando NAs Demoniacos
listaSPE$NIS[is.na(listaMunicipio$V0005)] <- 666 # Limpando NAs Demoniacos
nisCPFPertenceListaMunicipios <- NULL
nisCPFPertenceListaMunicipios <- is.element(as.numeric(listaSPE$NIS), as.numeric(listaMunicipio$V0005))
listaSPE <- cbind(listaSPE, nisCPFPertenceListaMunicipios )

#Cpf com Cpf
listaSPE$NIS[is.na(listaSPE$CPF)] <- 999 # Limpando NAs Demoniacos
listaSPE$NIS[is.na(listaMunicipio$V0005)] <- 666 # Limpando NAs Demoniacos
cpfPertenceListaMunicipios <- NULL
cpfPertenceListaMunicipios <- is.element(as.numeric(listaSPE$CPF), as.numeric(listaMunicipio$V0005))
listaSPE <- cbind(listaSPE, cpfPertenceListaMunicipios)

#CPF com NIS
listaSPE$NIS[is.na(listaSPE$CPF)] <- 999 # Limpando NAs Demoniacos
listaSPE$NIS[is.na(listaMunicipio$V0004)] <- 666 # Limpando NAs Demoniacos
cpfNISPertenceListaMunicipios <- NULL
cpfNISPertenceListaMunicipios <- is.element(as.numeric(listaSPE$CPF), as.numeric(listaMunicipio$V0004))
listaSPE <- cbind(listaSPE, cpfNISPertenceListaMunicipios)

## NOME 
nomePertenceListaMunicipio <- NULL
nomePertenceListaMunicipio <- is.element(listaSPE$Nome, listaMunicipio$V0002)
listaSPE <- cbind(listaSPE, nomePertenceListaMunicipio)

## Resultado Final
#&& listaSPE$nomePertenceListaMunicipio[i][] == FALSE

i <- 1
pertence <- NULL
while (i <= length((listaSPE$Nome))){
  print(i)
  if (listaSPE$nisPertenceListaMunicipios[i][] == FALSE && listaSPE$nisCPFPertenceListaMunicipios[i][] == FALSE && listaSPE$cpfPertenceListaMunicipios[i][] == FALSE && listaSPE$cpfNISPertenceListaMunicipios[i][] == FALSE ){
    pertence <- c(pertence, FALSE)
  }
  else{
    pertence <- c(pertence, TRUE)
  }
  
  i <- i + 1
}
listaSPE <- cbind (listaSPE, pertence)

listaSPE <- listaSPE[is.na(listaSPE$Nome) == FALSE, ]


setwd("/home/administrador/Documentos/DemandaNivelRecorte/")

write.csv(listaSPE, file = "funcaoInversaDesconsiderandoNome.csv")
