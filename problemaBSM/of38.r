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


setwd("~/Documentos/CADUNICO")
CADUNICO <- fread('CADDOM.csv')
CADPES <- fread('CADPES.csv')

metas <- read.csv2('selecao_publico_cadunico.csv', stringsAsFactors = F, header=F, encoding = 'UTF-8', sep = ",")
names(metas) <- c('cd_ibge','nome_munic','nome_regiao','ano_meta')

CADUNICO <- left_join(CADUNICO, CADPES) # faz o merge
CADUNICO <- left_join(CADUNICO, metas)

ranking_munic = fread("intercepto_aleatorio.csv")
CADUNICO <- left_join(CADUNICO, ranking_munic)


setwd("~/Documentos/")
listaBMS <- fread("of38.csv")
#names(metas) <- c('cd_ibge','MUNICIPIO','nome_regiao','ano_meta')
#listaBMS <- left_join(listaBMS, metas)

listaBMS <- na.omit(listaBMS)

listaBMSComResalvas <- listaBMS
listaBMSComResalvasUsuarioFinal <- listaBMS



setwd("~/Documentos/CADUNICO/SRE_Concatenadas")
listaDiamantina <- fread("MergeSres.csv")

names(listaBMS)[3:3] <- c('num_nis_pessoa_atual')
listaBMS <- left_join(listaBMS, CADUNICO, by = "num_nis_pessoa_atual")

#listaBMS <- subset(listaBMS, listaBMS$MUNICIPIO=="DIAMANTINA")
#listaBMSComResalvas <- subset(listaBMSComResalvas, listaBMSComResalvas$MUNICIPIO=="DIAMANTINA")

###################### Conferencia por NIS ############################
nisPertenceCadunico <- NULL
for (nis in listaBMS$num_nis_pessoa_atual){
  nisPertenceCadunico <- c(nisPertenceCadunico, (is.element(nis, CADUNICO$num_nis_pessoa_atual)))
}
listaBMSComResalvas <- cbind(listaBMSComResalvas, nisPertenceCadunico)


nisPertenceListaSPE <- NULL
for (nis in listaBMS$num_nis_pessoa_atual){
  nisPertenceListaSPE <- c(nisPertenceListaSPE, (is.element(nis, listaDiamantina$NIS)))
}
nisPertenceListaSPE
listaBMSComResalvas <- cbind(listaBMSComResalvas, nisPertenceListaSPE)

###################### Conferencia por CPF ############################

cpfPertenceCadunico <- NULL
for (cpf in listaBMS$CPF){
  cpfPertenceCadunico <- c(cpfPertenceCadunico, (is.element(cpf, CADUNICO$num_cpf_pessoa)))
}
cpfPertenceCadunico
listaBMSComResalvas <- cbind(listaBMSComResalvas, cpfPertenceCadunico)


cpfPertenceListaSPE <- NULL
for (cpf in listaBMS$CPF){
  cpfPertenceListaSPE <- c(cpfPertenceListaSPE, (is.element(cpf, listaDiamantina$CPF)))
}
cpfPertenceListaSPE
listaBMSComResalvas <- cbind(listaBMSComResalvas, cpfPertenceListaSPE)


###################### Conferencia Nome ################################

nomePertenceCadunico <- NULL
for (nome in listaBMS$NOME){
  nomePertenceCadunico <- c(nomePertenceCadunico, (is.element(nome, CADUNICO$nom_pessoa)))
}
nomePertenceCadunico
listaBMSComResalvas <- cbind(listaBMSComResalvas, nomePertenceCadunico)


nomePertenceListaSPE <- NULL
for (nome in listaBMS$NOME){
  nomePertenceListaSPE <- c(nomePertenceListaSPE, (is.element(nome, listaDiamantina$Nome)))
}
nomePertenceListaSPE
listaBMSComResalvas <- cbind(listaBMSComResalvas, nomePertenceListaSPE)

#################### Checa se pertence ao ano Meta ######################

fa <- function(x) iconv(x, to = "ASCII//TRANSLIT")
setwd("~/Documentos/")
munic <- read.csv("listaMunic2.csv", header = TRUE, sep = ",", encoding = "UTF-8")

munic2 <- NULL
for(i in munic){
  munic2 <- c(munic2, (fa(i) ))
}

#Utilizando Nome Municipio do CadUnico
anoMetaELocalPertencemPrograma <- NULL
for (anoMeta in listaBMS$nome_munic){
  anoMetaELocalPertencemPrograma <- c(anoMetaELocalPertencemPrograma, ((is.element(anoMeta, munic2)) || (is.element(anoMeta, munic$MUNIC))))
}
anoMetaELocalPertencemPrograma
listaBMSComResalvas <- cbind(listaBMSComResalvas, anoMetaELocalPertencemPrograma)


##################### Checa se 
### CSFC 4) Selecionar apenas domicílios classificados como rurais na base do CADUNICO.

ruralPertence <- NULL
for (rural in listaBMS$cod_local_domic_fam){
  ruralPertence <- c(ruralPertence, (rural == "2"))
}
ruralPertence
listaBMSComResalvas <- cbind(listaBMSComResalvas, ruralPertence)

### CSFC 5) Famílias com renda mensal per capita de até meio salário mínimo.
#1 - Ate R$77,00 | 2-Entre R$77,01 ate R$154,00 | 3-Entre R$154,01 ate 1/2 S.M. |4-Acima de 1/2 S.M.

rendaCompativel <- NULL
for (rural in listaBMS$fx_rfpc){
  rendaCompativel <- c(rendaCompativel, (rural != 4))
}
rendaCompativel
listaBMSComResalvas <- cbind(listaBMSComResalvas, rendaCompativel)

### CSFC 6) Disponibilidade de água (incluir abastecimento por poço e cisterna)

aguaCanalizada <- NULL
for (dadoAgua in listaBMS$cod_agua_canalizada_fam){
  aguaCanalizada <- c(aguaCanalizada, (dadoAgua == 1))
}
aguaCanalizada
listaBMSComResalvas <- cbind(listaBMSComResalvas, aguaCanalizada)

abastAgua <- NULL
for (dadoAgua in listaBMS$cod_abaste_agua_domic_fam){
  abastAgua <- c(abastAgua, (dadoAgua == 2 || dadoAgua == 3))
}
abastAgua
listaBMSComResalvas <- cbind(listaBMSComResalvas, abastAgua)

### CSFC 7) Chefes de familia

ChefeDeFámilia <- NULL
for (dado in listaBMS$cod_parentesco_rf_pessoa){
  ChefeDeFámilia <- c(ChefeDeFámilia, (dado == 1))
}
ChefeDeFámilia
listaBMSComResalvas <- cbind(listaBMSComResalvas, ChefeDeFámilia)

######################## Compilar o resultado geral ########################
#setwd("~/Documentos/")
#write.csv(listaBMSComResalvas, file = "listaBSMCOMResalvas.csv")


i <- 1
motivo <- NULL
while ( i <= (length(aguaCanalizada))){
  print(i)
  
  if(listaBMSComResalvas$nisPertenceCadunico[i][] == FALSE && listaBMSComResalvas$cpfPertenceCadunico[i][] == FALSE){
    if ( listaBMSComResalvas$nomePertenceListaSPE[i][] == TRUE && listaBMSComResalvas$nomeMaePertenceCadunico[i][] == TRUE){
      if (listaBMSComResalvas$nomePertenceListaSPE [i][] == TRUE){
        motivo <- c(motivo, "Está sendo atendido em nossa lista, porém CPF e NIS não batem")
      }
      else{
        motivo <- c(motivo, "Não possui documentos no CADUNICO. NOME e NOME DA MÃE estão no CADUNICO, mas não podemos afirmar se é caso homônimo ou não.")
      }
    }
    else{
      motivo <- c(motivo, "Não está cadastrado no CADUNICO")  
    }
  }
  
  else if(listaBMSComResalvas$ruralPertence[i][] != TRUE || is.na(listaBMSComResalvas$ruralPertence[i][])){
    motivo <- c(motivo, "Não pertence a área rural")
  }
  
  #else if(is.na(listaBMSComResalvas$aguaCanalizada[i][]) && is.na(listaBMSComResalvas$abastAgua[i][])){
  #  motivo <- c(motivo, "Não há informações sobre água")
  #}  
  
  #else if(listaBMSComResalvas$aguaCanalizada[i][] == FALSE && listaBMSComResalvas$abastAgua[i][] == FALSE){
  #  motivo <- c(motivo, "Não possui disponibilidade de água")
  #}
  
  else if(listaBMSComResalvas$rendaCompativel[i][] == FALSE){
    motivo <- c(motivo, "Renda incompativél")
  }
  
  else if(listaBMSComResalvas$nisPertenceListaSPE[i][] == TRUE || listaBMSComResalvas$cpfPertenceListaSPE[i][] == TRUE){
    motivo <- c(motivo, "Está sendo atendido em nossa lista")
  }
  
  else if(listaBMSComResalvas$anoMetaELocalPertencemPrograma[i][] == FALSE || is.na(listaBMSComResalvas$anoMetaELocalPertencemPrograma[i][])){
    motivo <- c(motivo, "Não pertence a um local atendido no ano de 2017")
  }
  
  else{
    if(is.na(listaBMSComResalvas$ChefeDeFámilia[i][])){
      motivo <- c(motivo, "Não há informação sobre chefe de família")
    }else if(listaBMSComResalvas$ChefeDeFámilia[i][] == FALSE){
      motivo <- c(motivo, "Não é chefe de família")
    }else{
      motivo <- c(motivo, "Outro")      
    }
    
  }
  
  i <- i+1
}
length(motivo)

listaBMSComResalvas <- cbind(listaBMSComResalvas, motivo)
listaBMSComResalvasUsuarioFinal <- cbind(listaBMSComResalvasUsuarioFinal, motivo)

setwd("~/Documentos/")
write.csv(listaBMSComResalvas, file = "of38_listacomResalvas.csv")
write.csv(listaBMSComResalvasUsuarioFinal, file = "of38_listacomResalvasUserFinal.csv")











cpf <- subset(CADUNICO, num_cpf_pessoa == 6449587660) #unico cpf encontrado





setwd("~/Documentos/CADUNICO/SRE_Concatenadas")
listaSPE <- fread("MergeSres.csv")

setwd("~/Documentos/ListasBSMComResalva")
listaBSM <- fread("ListaBSMComResalvasCompleta.csv")

listaBSM_atendidos <- subset(listaBSM, motivo = "Está sendo atendido em nossa lista")

atendidos <- NULL
for(i in listaBSM$NIS ){
  atendidos <- c(atendidos, is.element(i, listaSPE$NIS))
}


teste <- c("6449587660", "4191169661", "88252957668", "9281197693")
is.element(teste, CADUNICO$num_cpf_pessoa)
