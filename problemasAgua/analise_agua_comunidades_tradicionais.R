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

metas <- read.csv2('selecao_publico_cadunico.csv',
                   stringsAsFactors = F, header=F, encoding = 'UTF-8')
names(metas) <- c('cd_ibge','nome_munic','nome_regiao','ano_meta')

CADUNICO <- left_join(CADUNICO, metas) # faz o merge

ranking_munic = fread("intercepto_aleatorio.csv")
CADUNICO <- left_join(CADUNICO, ranking_munic)

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
#Comunidades indígenas

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


# Agora para comunidades quilombolas
separado_qui_n = comunidades_tradicionais %>% group_by(nom_comunidade_quilombola_fam) %>%
  summarise(n = n())
separado_qui_div = comunidades_tradicionais %>% group_by(nom_comunidade_quilombola_fam) %>%
  count(cod_agua_canalizada_fam) %>% na.omit
names(separado_qui_div)[3] = "count"

separado_qui_per = left_join(separado_qui_div, separado_qui_n) %>% 
  mutate(per = round(count/n, 2))
#Exportando latex
separado_qui_per = separado_qui_per[separado_qui_per$cod_agua_canalizada_fam == "Não",] %>%
  arrange(desc(per))
separado_qui_per[,-4] %>% xtable %>% print(., tabular.environment = "longtable",
                                               floating = F)

#TOTAIS dos domicilios sem acesso à água
sum(separado_ind_per$count[separado_ind_per$nom_povo_indigena_fam != "" &
                             separado_ind_per$cod_agua_canalizada_fam == "Não"])

sum(separado_qui_per$count[separado_qui_per$cod_agua_canalizada_fam == "Não"])


