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


## Nis com Nis
listaMunicipio$V0004[is.na(listaMunicipio$V0004)] <- 666 # Limpando NAs Demoniacos
nisPertenceListaSPE <- NULL

i=1
for (nis in listaMunicipio$V0004){
  print(i)
  i = i + 1
  nisPertenceListaSPE <- c(nisPertenceListaSPE, (is.element(as.numeric(nis), as.numeric(listaSPE$NIS) )))
}
listaMunicipio <- cbind(listaMunicipio, nisPertenceListaSPE)

## Nis com Cpf
listaMunicipio$V0004[is.na(listaMunicipio$V0004)] <- 666 # Limpando NAs Demoniacos
nisCPFPertenceListaSPE <- NULL

i=1
for (nis in listaMunicipio$V0004){
  print(i)
  i = i + 1
  nisCPFPertenceListaSPE<- c(nisCPFPertenceListaSPE, (is.element(as.numeric(nis), as.numeric(listaSPE$CPF) )))
}
listaMunicipio <- cbind(listaMunicipio, nisCPFPertenceListaSPE)

## Cpf com Cpf
listaMunicipio$V0005[is.na(listaMunicipio$V0005)] <- 666 # Limpando NAs Demoniacos
cpfPertenceListaSPE <- NULL

i=1
for (cpf in listaMunicipio$V0005){
  print(i)
  i = i + 1
  cpfPertenceListaSPE<- c(cpfPertenceListaSPE, (is.element(as.numeric(cpf), as.numeric(listaSPE$CPF) )))
}
listaMunicipio <- cbind(listaMunicipio, cpfPertenceListaSPE)

## Cpf com NIS
listaMunicipio$V0005[is.na(listaMunicipio$V0005)] <- 666 # Limpando NAs Demoniacos
cpfNISPertenceListaSPE <- NULL

i=1
for (cpf in listaMunicipio$V0005){
  print(i)
  i = i + 1
  cpfNISPertenceListaSPE<- c(cpfNISPertenceListaSPE, (is.element(as.numeric(cpf), as.numeric(listaSPE$NIS) )))
}
listaMunicipio <- cbind(listaMunicipio, cpfNISPertenceListaSPE)


## NOME 
nomePertenceLista <- NULL
for (nome in listaMunicipio$V0002){
  print(nome)
  nomePertenceLista <- c(nomePertenceLista, (is.element(nome, listaSPE$Nome)))
}
listaMunicipio <- cbind(listaMunicipio, nomePertenceLista)

## Resultado Final

i <- 1
pertence <- NULL
while (i <= length((nomePertenceLista))){
  print(i)
  if (listaMunicipio$nisPertenceListaSPE[i][] == FALSE && listaMunicipio$nisCPFPertenceListaSPE[i][] == FALSE && listaMunicipio$cpfPertenceListaSPE[i][] == FALSE && listaMunicipio$cpfNISPertenceListaSPE[i][] == FALSE && listaMunicipio$nomePertenceLista[i][] == FALSE){
    pertence <- c(pertence, FALSE)
  }
  else{
    pertence <- c(pertence, TRUE)
  }
  
  i <- i + 1
}
listaMunicipio <- cbind (listaMunicipio, pertence)

listaMunicipio <- listaMunicipio[is.na(listaMunicipio$V0002) == FALSE, ]


## E se... fosse eu

i <- 1
suspeito <- NULL
while (i <= length((nomePertenceLista))){
  print(i)
  if (listaMunicipio$nisPertenceListaSPE[i][] == FALSE && listaMunicipio$nisCPFPertenceListaSPE[i][] == FALSE && listaMunicipio$cpfPertenceListaSPE[i][] == FALSE && listaMunicipio$cpfNISPertenceListaSPE[i][] == FALSE && listaMunicipio$nomePertenceLista[i][] == TRUE ){
    suspeito <- c(suspeito, TRUE)
  }
  else{
    suspeito <- c(suspeito, FALSE)
  }
  
  i <- i + 1
}
listaMunicipio <- cbind (listaMunicipio, suspeito)


i <- 1
suspeito2 <- NULL
while (i <= length((nomePertenceLista))){
  print(i)
  if (listaMunicipio$pertence[i][] == TRUE && listaMunicipio$nomePertenceLista[i][] == FALSE ){
    suspeito2 <- c(suspeito2, TRUE)
  }
  else{
    suspeito2 <- c(suspeito2, FALSE)
  }
  
  i <- i + 1
}
listaMunicipio <- cbind (listaMunicipio, suspeito2)


setwd("/home/administrador/Documentos/DemandaNivelRecorte/")

write.csv(listaMunicipio, file = "listaMunicipioComResalvas.csv")




############################################################################################

listaMunicipio$V0004[is.na(listaMunicipio$V0004)] <- 666 # Limpando NAs Demoniacos
nisPertenceListaSPE <- NULL
for (nis in listaMunicipio$V0004){
  print(nis)
  if (is.element(nis, listaSPE$NIS)){
    nisPertenceListaSPE <- c(nisPertenceListaSPE, TRUE)
  }
  else if(is.element(nis, listaSPE$CPF)){
    nisPertenceListaSPE <- c(nisPertenceListaSPE, TRUE)
  }
  else{
    nisPertenceListaSPE <- c(nisPertenceListaSPE, FALSE)
  }
}
listaMunicipio <- cbind(listaMunicipio, nisPertenceListaSPE)


listaMunicipio$V0005[is.na(listaMunicipio$V0005)] <- 666 # Limpando NAs Demoniacos
cpfPertenceListaSPE <- NULL
for (cpf in listaMunicipio$V0005){
  print(cpf)
  if (is.element(cpf, listaSPE$CPF)){
    cpfPertenceListaSPE  <- c(cpfPertenceListaSPE , TRUE)
  }
  else if(is.element(nis, listaSPE$NIS)){
    cpfPertenceListaSPE  <- c(cpfPertenceListaSPE , TRUE)
  }
  else{
    cpfPertenceListaSPE  <- c(cpfPertenceListaSPE , FALSE)
  }
}
listaMunicipio <- cbind(listaMunicipio, cpfPertenceListaSPE)


nomePertenceLista <- NULL
for (nome in listaMunicipio$V0002){
  print(nome)
  nomePertenceLista <- c(nomePertenceLista, (is.element(nome, listaSPE$Nome)))
}
listaMunicipio <- cbind(listaMunicipio, nomePertenceLista)


i <- 1
pertence <- NULL
while (i <= length((nomePertenceLista))){
  print(i)
  if (listaMunicipio$nisPertenceListaSPE[i][] == FALSE && listaMunicipio$cpfPertenceListaSPE[i][] == FALSE && listaMunicipio$nomePertenceLista[i][] == FALSE){
    pertence <- c(pertence, FALSE)
  }
  else{
    pertence <- c(pertence, TRUE)
  }

  i <- i + 1
}

listaMunicipio <- cbind (listaMunicipio, pertence)


setwd("/home/administrador/Documentos/DemandaNivelRecorte/")


################ FAlta gravar

copia <- listaMunicipio

listaMunicipio <- listaMunicipio[is.na(listaMunicipio$V0002) == FALSE, ]

write.csv(listaMunicipio, file = "listaMunicipioComResalvas.csv")

write.csv(listaMunicipio, file = "resultadosListaMunicipio.csv")


