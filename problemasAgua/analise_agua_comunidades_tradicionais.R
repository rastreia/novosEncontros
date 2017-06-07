#Análise de água entre comunidades tradicionais, quilombolas e indígenas
#RASTREIA
################

setwd("~/Documentos/CADUNICO")

library(data.table)
library(bit64)
library(descr)
library(dplyr)
library(tidyr)
library(magrittr)
library(lubridate)
library(ggplot2)
library(xtable)

#####################################
# Lendo os dados
CADUNICO <- fread('CADDOM.csv')
gc()
dic <- fread('dicionariodomicilio.csv')
View(dic)
##########################################

#com_tradicionais[com_tradicionais == 0] = NA
#freq(com_tradicionais)

#Rankeando
#Extraindo apenas domicílios de comunidades tradicionais
indigenas = CADUNICO[CADUNICO$cod_indigena_reside_fam == 1, ]
quilombolas = CADUNICO[CADUNICO$ind_familia_quilombola_fam == 1, ]
tradicionais = CADUNICO[CADUNICO$ind_parc_mds_fam != 0, ]

indigenas = indigenas[is.na(indigenas$cod_indigena_reside_fam) == F,]
quilombolas = quilombolas[is.na(quilombolas$ind_familia_quilombola_fam) == F,]
tradicionais = tradicionais[is.na(tradicionais$ind_parc_mds_fam) == F,]

comunidades_tradicionais = rbind(indigenas, quilombolas, tradicionais) %>% unique
###################

comunidades_tradicionais = arrange(comunidades_tradicionais,
                                   intercepto_aleatorio,fx_rfpc)

dim(comunidades_tradicionais)
#write.csv(comunidades_tradicionais, "comunidades_tradicionais_cadunico_hierarquizado.csv",
#          row.names = F, fileEncoding = "UTF-8")


##############################


####################################################
#Gerando alguns gráficos de avaliação
# Analisando algumas descritivas dos dados...
freq( CADUNICO$cod_agua_canalizada_fam[CADUNICO$cod_indigena_reside_fam == 1] ) 
freq( CADUNICO$cod_agua_canalizada_fam[CADUNICO$ind_familia_quilombola_fam == 1] )
table( CADUNICO$cod_agua_canalizada_fam, CADUNICO$ind_parc_mds_fam )

freq( CADUNICO$cod_abaste_agua_domic_fam[CADUNICO$cod_indigena_reside_fam == 1] ) 
freq( CADUNICO$cod_abaste_agua_domic_fam[CADUNICO$ind_familia_quilombola_fam == 1] )
table( CADUNICO$cod_abaste_agua_domic_fam, CADUNICO$ind_parc_mds_fam )

#Gerando gráficos

#Possui água canalizada?
comunidades_sum = plyr::ddply(comunidades_tradicionais, "cod_agua_canalizada_fam", 
                              summarize, y=length(cod_agua_canalizada_fam))
str(comunidades_sum)

ggplot(comunidades_sum, aes(x=cod_agua_canalizada_fam, y=y))+
  geom_bar(stat="identity", fill='#ffd42a')+
  geom_text(aes(label = y), vjust=-0.25)+theme_bw()+
  labs(x="O Domicílio possui água canalizada?", y= "Frequência")

#Tipo de abastecimento de água
comunidades_sum2 = plyr::ddply(comunidades_tradicionais, "cod_abaste_agua_domic_fam", 
                              summarize, y=length(cod_abaste_agua_domic_fam))
str(comunidades_sum2)

ggplot(comunidades_sum2, aes(x=cod_abaste_agua_domic_fam, y=y))+
  geom_bar(stat="identity", fill='#ffd42a')+
  geom_text(aes(label = y), vjust=-0.25)+theme_bw()+
  labs(x="Tipo de abastecimento de água do domicílio", y= "Frequência")



##########################################################################
#Verificar concentração de comunidades tracionais sem água por município

freq(comunidades_tradicionais$cd_ibge[comunidades_tradicionais$cod_agua_canalizada_fam == 2])

###########################################################################
#Analisando as comunidades com menos acesso à água

freq(comunidades_tradicionais$cod_agua_canalizada_fam, plot=F)
sem_agua = comunidades_tradicionais[comunidades_tradicionais$cod_agua_canalizada_fam == "Não",]

freq(sem_agua$nom_povo_indigena_fam, plot=F)
freq(sem_agua$nom_reserva_indigena_fam, plot=F)
freq(sem_agua$nom_comunidade_quilombola_fam, plot=F) %>% as.data.frame(., strings)

separado_ind_n = comunidades_tradicionais %>% group_by(nom_povo_indigena_fam) %>%
  summarise(n = n())
separado_ind_div = comunidades_tradicionais %>% group_by(nom_povo_indigena_fam) %>%
  count(cod_agua_canalizada_fam) %>% na.omit
names(separado_ind_div)[3] = "count"

separado_ind_per = left_join(separado_ind_div, separado_ind_n) %>% 
  mutate(per = round(count/n, 2))
#Exportando latex
separado_ind_per[,-4] %>% xtable


group_by(cod_agua_canalizada_fam, add=T) %>%
  mutate(per = paste0(round(100*count(cod_agua_canalizada_fam)/count, 2), ""))

  
  count(cod_agua_canalizada_fam) %>% na.omit

  

