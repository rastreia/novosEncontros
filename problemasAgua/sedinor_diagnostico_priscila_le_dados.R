#####################################
# Problemas  - ações com água
# RASTREIA
# Neylson Crepalde
#####################################

#################################
multiplot <- function(..., plotlist = NULL, file, cols = 1, layout = NULL) {
  require(grid)
  
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  if (is.null(layout)) {
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots == 1) {
    print(plots[[1]])
    
  } else {
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    for (i in 1:numPlots) {
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
#################################

library(descr)
library(dplyr)
library(tidyr)
library(magrittr)
library(xtable)
library(ggplot2)
library(reshape2)

setwd('~/RASTREIA/NOVOS ENCONTROS/problemas_agua')

#SEDINOR
###############
sedinor = read.csv2('diagnostico_priscila_sedinor.csv',stringsAsFactors = F)
sedinor = sedinor[1:366, 1:26]
#View(sedinor)
###############

dados = sedinor[,c(5,13:15,19:21,26)]

# Ignorar tecnologias. São todos poços!
# PRONTINHO!
##############################################

### ANÁLISES:
names(dados)[2] <- "situacao"
dados$num_controle_poco = 1:nrow(dados)

tabela_descricao = dados %>% unnest(strsplit(situacao, ', '))
names(tabela_descricao)[10] = "Prob1"
tabela_descricao = tabela_descricao %>% unnest(strsplit(Prob1, ','))
names(tabela_descricao)[11] = "Prob.dividido"
tabela_descricao$Prob.dividido[tabela_descricao$Prob.dividido == "10 "] = "10"

#View(tabela_descricao)

#limpando
names(tabela_descricao)[5] <- "status"
tabela_descricao$status = gsub(" ;",",", tabela_descricao$status)
tabela_descricao$status = gsub(";",",", tabela_descricao$status)
tabela_descricao$status = gsub("1,0","1", tabela_descricao$status)
tabela_descricao$status = gsub("1,20","1,2", tabela_descricao$status)

#Separando
desc_status = tabela_descricao %>% unnest(strsplit(status, ','))
names(desc_status)[12] <- "status.dividido"

#View(desc_status)

#Renomeando as categorias
desc_status$Prob.dividido[desc_status$Prob.dividido=="8"] = "Água imprópria para conusmo humano"
desc_status$Prob.dividido[desc_status$Prob.dividido=="9"] = "Estudo da análise da água"
desc_status$Prob.dividido[desc_status$Prob.dividido=="10"] = "Tubulação e reservatório entregue"
desc_status$Prob.dividido[desc_status$Prob.dividido=="11"] = "Tubulação e reservatório não entregue"

desc_status$status.dividido[desc_status$status.dividido=="1"] = "Perfurado"
desc_status$status.dividido[desc_status$status.dividido=="2"] = "Equipado"
desc_status$status.dividido[desc_status$status.dividido=="3"] = "Energizado"


#Cruzando descricao com status

table(desc_status$Prob.dividido, desc_status$status.dividido)

###############################
#Programando as dummies

casted1 = desc_status[,c(9,11)] %>% unique
casted2 = desc_status[,c(9,12)] %>% unique


banco1 = dcast(casted1, num_controle_poco~Prob.dividido)
#banco1
for (row in 1:nrow(banco1)){
  for (col in 2:5){
    if(is.na(banco1[row,col])){
      banco1[row,col] = 0
    } 
    else{
      banco1[row,col] = 1
    }
  }
}
#banco1
#View(banco1)


banco2 = dcast(casted2, num_controle_poco~status.dividido)
for (row in 1:nrow(banco2)){
  for (col in 2:ncol(banco2)){
    if(is.na(banco2[row,col])){
      banco2[row,col] = 0
    } 
    else{
      banco2[row,col] = 1
    }
  }
}
banco2

casted.junto = left_join(banco1, banco2)

sedinor$num_controle_poco = 1:nrow(sedinor)

sedinor_atual = left_join(sedinor, casted.junto, by="num_controle_poco")
#View(sedinor_atual)

#write.csv(sedinor_atual, "dados_sedinor_atualizado.csv", row.names = F, 
#          fileEncoding = "UTF-8")
