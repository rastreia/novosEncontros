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

### Limpando os dados:
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

#
names(desc_status)[7] = "se.nao.porque"
indexes = which(desc_status$se.nao.porque == "")
desc_status$se.nao.porque[indexes] = "0"
desc_status$se.nao.porque = gsub(";",",", desc_status$se.nao.porque)
desc_status$se.nao.porque = gsub("-","0", desc_status$se.nao.porque)
desc_status$se.nao.porque = gsub("_","0", desc_status$se.nao.porque)


desc_status_prob = desc_status %>% unnest(strsplit(se.nao.porque, ','))
names(desc_status_prob)[13] = "se.nao.porque.dividido"

freq(desc_status_prob$se.nao.porque.dividido, plot=F)

#View(desc_status_prob)

#Renomeando as categorias
desc_status_prob$Prob.dividido[desc_status$Prob.dividido=="8"] = "Água imprópria para consumo humano"
desc_status_prob$Prob.dividido[desc_status$Prob.dividido=="9"] = "Estudo da análise da água"
desc_status_prob$Prob.dividido[desc_status$Prob.dividido=="10"] = "Tubulação e reservatório entregue"
desc_status_prob$Prob.dividido[desc_status$Prob.dividido=="11"] = "Tubulação e reservatório não entregue"

desc_status_prob$status.dividido[desc_status$status.dividido=="1"] = "Perfurado"
desc_status_prob$status.dividido[desc_status$status.dividido=="2"] = "Equipado"
desc_status_prob$status.dividido[desc_status$status.dividido=="3"] = "Energizado"

desc_status_prob$se.nao.porque.dividido[desc_status_prob$se.nao.porque.dividido == "0"] = NA
desc_status_prob$se.nao.porque.dividido[desc_status_prob$se.nao.porque.dividido == "1"] = "Sem análise de água"
desc_status_prob$se.nao.porque.dividido[desc_status_prob$se.nao.porque.dividido == "2"] = "Água imprópria para consumo humano"
desc_status_prob$se.nao.porque.dividido[desc_status_prob$se.nao.porque.dividido == "3"] = "Tubulação não entregue"
desc_status_prob$se.nao.porque.dividido[desc_status_prob$se.nao.porque.dividido == "5"] = "Sem energização"


#Cruzando descricao com status

table(desc_status$Prob.dividido, desc_status$status.dividido)


###############################
#Programando as dummies

casted1 = desc_status_prob[,c(9,11)] %>% unique
casted2 = desc_status_prob[,c(9,12)] %>% unique
casted3 = desc_status_prob[,c(9,13)] %>% unique

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

banco3 = dcast(casted3, num_controle_poco~se.nao.porque.dividido)
for (row in 1:nrow(banco3)){
  for (col in 2:ncol(banco3)){
    if(is.na(banco3[row,col])){
      banco3[row,col] = 0
    } 
    else{
      banco3[row,col] = 1
    }
  }
}
banco3

casted.junto = left_join(banco1, banco2, by = "num_controle_poco")
casted.junto2 = left_join(casted.junto, banco3, by = "num_controle_poco")


sedinor$num_controle_poco = 1:nrow(sedinor)

sedinor_atual = left_join(sedinor, casted.junto2, by="num_controle_poco")
#View(sedinor_atual)

#write.csv(sedinor_atual, "dados_sedinor_atualizado.csv", row.names = F, 
#          fileEncoding = "UTF-8")

print(dim(banco1))
print(dim(banco2))
print(dim(banco3))
