# CADUNICO
# Neylson
# Em andamento...
##############

#### Instalar pacotes manualmente com tar.gz
# install.packages("~/Downloads/nome-do-pacote", repos=NULL)

setwd("~/Documentos/CADUNICO")

library(data.table)
library(bit64)
library(descr)
library(dplyr)
library(tidyr)
library(magrittr)
library(lubridate)
library(ggplot2)
library(lme4)
library(merTools)
library(lmtest)
library(texreg)

#alto_jequi <- fread("Alto Jequitinhonha.csv") %>% as.data.frame(.,stringsAsFactors=F)
#medio_baixo <- fread('Médio e Baixo Jequitinhonha.csv') %>% as.data.frame(.,stringsAsFactors=F)
#mucuri <- fread('Mucuri.csv') %>% as.data.frame(.,stringsAsFactors=F)
#norte <- fread('Norte.csv') %>% as.data.frame(.,stringsAsFactors=F)
#vale_rio_doce <- fread('Vale do Rio Doce.csv') %>% as.data.frame(.,stringsAsFactors=F)

#CADUNICO <- rbind(alto_jequi,medio_baixo,mucuri,norte,vale_rio_doce); rm(alto_jequi,medio_baixo,mucuri,norte,vale_rio_doce)

###############################################################
###############################################################
###############################################################
###############################################################
#Lendo o CADUNICO e separando os municípios selecionados
# Aguardando rolar a instalação de pacotes........
CADUNICO <- fread('CADDOM.csv')

gc()

freq(CADUNICO$fx_rfpc,plot=F)

dic <- fread('dicionariodomicilio.csv')
dicpes <- fread('dicionariopessoa.csv')
View(dic)
View(dicpes)

#linha 1510739 deu problema
#CADPES1 <- fread('pessoa.csv', nrows = 1510737) %>% as.data.frame(.,stringsAsFactors=F)
#CADPES2 <- fread('pessoa.csv', skip =  1510737) %>% as.data.frame(.,stringsAsFactors=F)
#names(CADPES2) <- names(CADPES1)

#CADPES <- rbind(CADPES1,CADPES2); rm(CADPES1, CADPES2)
CADPES <- fread('CADPES.csv')
gc()

#### Já verificamos duplicidade de CPF's #####
#########################
#Juntando as metas de aplicação 2017/2018

metas <- read.csv2('selecao_publico_cadunico.csv',
                   stringsAsFactors = F, header=F, encoding = 'UTF-8')
names(metas) <- c('cd_ibge','nome_munic','nome_regiao','ano_meta')

CADUNICO <- left_join(CADUNICO, metas) # faz o merge
#View(CADUNICO[1:100,])

###############################################
# Extraindo municípios sem energia elétrica:
#freq(CADUNICO$cod_iluminacao_domic_fam, plot=F)
#munic_sem_luz <-  CADUNICO[CADUNICO$cod_iluminacao_domic_fam == 4 |
#                             CADUNICO$cod_iluminacao_domic_fam == 5 |
#                             CADUNICO$cod_iluminacao_domic_fam == 6,]
#munic_sem_luz %<>% arrange(desc(cod_iluminacao_domic_fam))
#write.csv(munic_sem_luz, "municipios_sem_energia.csv",
#          row.names = F, fileEncoding = "UTF-8")




########################################################
# Tentando um modelo logístico para verificar quais variáveis possuem
# maior impacto no fato de uma família estar na faixa mais baixa
# de renda

# Valor pago em aluguel e remédios não deu certo
# variavel escoa sanitario nao deu certo

CADUNICO$pobreza <- ifelse(CADUNICO$fx_rfpc == 1, 1, 0)
CADUNICO$cod_local_domic_fam %<>% as.factor
levels(CADUNICO$cod_local_domic_fam) <- c('Urbanas', 'Rurais')
CADUNICO$cod_agua_canalizada_fam %<>% as.factor
levels(CADUNICO$cod_agua_canalizada_fam) <- c('Sim','Não')
CADUNICO$cod_abaste_agua_domic_fam %<>% as.factor
levels(CADUNICO$cod_abaste_agua_domic_fam) <- c('Rede geral de distribuição',
                                                'Poço ou nascente',
                                                'Cisterna',
                                                'Outra forma')
CADUNICO$cod_banheiro_domic_fam %<>% as.factor
levels(CADUNICO$cod_banheiro_domic_fam) <- c('Sim', 'Não')
CADUNICO$cod_iluminacao_domic_fam %<>% as.factor
levels(CADUNICO$cod_iluminacao_domic_fam) <- c('Elétrica com medidor próprio',
                                               'Elétrica com medidor comunitário',
                                               'Elétrica sem medidor',
                                               'Óleo, querosene ou gás',
                                               'Vela',
                                               'Outra forma')



#reg <- glm(pobreza ~ cod_local_domic_fam +
#             qtd_comodos_dormitorio_fam + cod_agua_canalizada_fam +
#             cod_abaste_agua_domic_fam + cod_banheiro_domic_fam +
#             cod_iluminacao_domic_fam,
#           data = CADUNICO, family = binomial(link='logit'))
#summary(reg)

# Tentando um modelo logístico hierárquico
#reg_multi <- glmer(pobreza ~ cod_local_domic_fam + (1 | nome_munic) +
#                     qtd_comodos_dormitorio_fam + cod_agua_canalizada_fam +
#                     cod_abaste_agua_domic_fam + cod_banheiro_domic_fam +
#                     cod_iluminacao_domic_fam,
#                   data = CADUNICO, family = binomial(link='logit'))
#summary(reg_multi)
#ICC = var(reg_multi@u) / (var(reg_multi@u)+var(residuals(reg_multi)))
#lrtest(reg, reg_multi)
#plotREsim(REsim(reg_multi))

#Gerando a tabela com os resultados das regressoes
#texreg(list(reg, reg_multi), 
#       custom.model.names = c('Logístico', 'Logístico Multinível'),
#       center = F, caption.above = T, 
#       caption = 'Modelos estatísticos')


#reg_multi_effale <- glmer(pobreza ~ cod_local_domic_fam +
#                     qtd_comodos_dormitorio_fam + cod_agua_canalizada_fam +
#                     cod_abaste_agua_domic_fam + (cod_banheiro_domic_fam | nome_munic) +
#                     cod_iluminacao_domic_fam,
#                   data = CADUNICO, family = binomial(link='logit'))
#summary(reg_multi_effale)

# Verificando as probabilidades
beta2prob <- function(x){
  return((exp(x)-1)*100)
}

#beta2prob(coef(reg))
#xtable::xtable(as.data.frame(beta2prob(coef(reg))))
#beta2prob(reg_multi@beta)

################################
# vendo a data do cadastramento

#datas <- CADUNICO[,4] %>% ymd %>% as_date
#datas.df <- table(datas) %>% as.data.frame(.,stringsAsFactors=F)
#limits = c(20140101,20170101)
#limits %<>% ymd %>% as_date
#ggplot(datas.df, aes(x=as_date(datas), y=Freq))+geom_line()+
#  scale_x_date(date_minor_breaks = '1 year', date_breaks = '1 year',
#               date_labels = '%Y', limits = limits)+
#  scale_y_continuous(limits = c(0, 1000))
################################


#### Rankeando os municipios
# Juntando o resultado do intercepto aleatorio do modelo multinivel
#resultados = coef(reg_multi)

#ranking_munic = data.frame(rownames(resultados$nome_munic),resultados$nome_munic[,1])
#names(ranking_munic) <- c('nome_munic', 'intercepto_aleatorio')

#write.csv(ranking_munic, "intercepto_aleatorio.csv", row.names = F,
#          fileEncoding = "UTF-8")

ranking_munic = fread("intercepto_aleatorio.csv")

#View(ranking_munic)
CADUNICO <- left_join(CADUNICO, ranking_munic)
head(CADUNICO)


#####################################
# Fazendo a seleção

#Seleciona família com renda até R$85,00 per capita E que não
#tenham recebido Bolsa Família
rurais = CADUNICO[CADUNICO$cod_local_domic_fam == 'Rurais',]

selecao_acao1 <- CADUNICO[CADUNICO$marc_pbf == 0 & CADUNICO$cod_local_domic_fam == 'Rurais' &
                          CADUNICO$fx_rfpc != 4,]
selecao_acao1 <- arrange(selecao_acao1, desc(fx_rfpc),
                         desc(cod_iluminacao_domic_fam),
                         desc(cod_banheiro_domic_fam),
                         desc(intercepto_aleatorio))
#View(selecao_acao1)
#write.csv2(selecao_acao1, 'selecionados_acao1.csv', 
#            row.names = F, fileEncoding = 'UTF-8')

#############################################
# Variáveis importantes
# MERGE - cod_familiar_fam [,3]
# Faixa de renda = fx_rfpc [, 23]


# Idade (nascimento) tá no PES. Tem que dar merge
# 0 - 17 e 65 < x -> 00 (puxa os dois e vê)

##############################################
##### FEITO
#Filtrando os municípios do banco pessoas
#ibge_munics = CADPES$cd_ibge %>% unique
#CADDOM_ibge = CADUNICO$cd_ibge
#linhas = c()

#for (i in 1:nrow(CADPES)){
#  for (munic in ibge_munics){
#    if (i == munic){
#      linhas[i] = i
#    } else{
#      linhas[i] = NA
#   }
#  }
#}

selec_index = function(num, x=CADDOM_ibge){
  retorna_index = function(a, b){
    if (a == x[b]){
      return(b)
    }
  }
  lista_indexes = sapply(1:length(x), retorna_index, a=num)
  lista_indexes = unlist(lista_indexes)
  return(lista_indexes)
}

#Teste
#selec_index(2, c(1,2,5,4,6,2,7,6,2,19,2))
#sapply(1:5, selec_index, x = x) %>% unlist

# Pra Valer!!!
#library(parallel)
#no_cores = detectCores()
#cl = makeCluster(no_cores)
#clusterExport(cl, c('ibge_munics', 'CADDOM_ibge', 'selec_index'))

#linhas = parSapply(cl, ibge_munics, selec_index) %>% unlist

#stopCluster(cl)


#CADDOM_selecao = CADUNICO[linhas,]

#write.csv2(CADDOM_selecao, 'CADDOM.csv', row.names = F)

#CADPES <-  fread('CADPES.csv') %>% as.data.frame(.,stringsAsFactors=F)

################################################
################################################
################################################
################################################

###############################################
# Ação 2 - Entrega de KIT Alimento e trabalho
#rm(reg)
gc()

selecao_acao2 = CADUNICO[CADUNICO$ano_meta == 2017,]
freq(selecao_acao2$ano_meta, plot=F)

selecao_acao2$ind_parc_mds_fam[selecao_acao2$ind_parc_mds_fam==0] = 999

# hierarquizando por quilombolas, indigenas, tradicionais, renda
selecao_acao2 = arrange(selecao_acao2, ind_familia_quilombola_fam,
                        cod_familia_indigena_fam, ind_parc_mds_fam,
                        fx_rfpc)

library(readr)
#write_csv(selecao_acao2, 'selecionados_kits_sementes_presentes.csv')

### Juntando com o banco de pessoas
selecao_acao2 = left_join(selecao_acao2, CADPES, by="cod_familiar_fam")
dim(selecao_acao2)

##########################################################
#Exportando listas para Almenara, Januária e Montes Claros

#SER Almenara

setwd("~/Documentos/CADUNICO/SRE_Almenara")

almenara = selecao_acao2[selecao_acao2$nome_munic=="ALMENARA",]
almenara %>% filter(cod_parentesco_rf_pessoa == 1) %>%
  mutate(Endereço = paste(nom_tip_logradouro_fam, nom_titulo_logradouro_fam, 
                          nom_logradouro_fam, num_logradouro_fam, 
                          des_complemento_fam, des_complemento_adic_fam,
                          num_cep_logradouro_fam)) %>%
  mutate(n = 1:nrow(.), Nome = nom_pessoa, Apelido = nom_apelido_pessoa,
         NIS = num_nis_pessoa_atual, 
         CPF = num_cpf_pessoa, Endereço = Endereço,
         # Parte da AS
         Dom_acessa_CRAS = NA, Apto_AS = NA, 
         # Parte da Emater
         Renda = NA, DAP = NA, Apto_EMATER = NA, Objetivo = NA, Data = NA) %>%
  select_("n","Nome","Apelido","NIS","CPF","Endereço", "Dom_acessa_CRAS",
          "Apto_AS","Renda","DAP", "Apto_EMATER","Objetivo","Data") %>%
  write_excel_csv(., "lista_almenara_EMATER.csv", na = "")

bandeira = selecao_acao2[selecao_acao2$nome_munic=="BANDEIRA",]
bandeira %>% filter(cod_parentesco_rf_pessoa == 1) %>%
  mutate(Endereço = paste(nom_tip_logradouro_fam, nom_titulo_logradouro_fam, 
                          nom_logradouro_fam, num_logradouro_fam, 
                          des_complemento_fam, des_complemento_adic_fam,
                          num_cep_logradouro_fam)) %>%
  mutate(n = 1:nrow(.), Nome = nom_pessoa, Apelido = nom_apelido_pessoa,
         NIS = num_nis_pessoa_atual, 
         CPF = num_cpf_pessoa, Endereço = Endereço,
         # Parte da AS
         Dom_acessa_CRAS = NA, Apto_AS = NA, 
         # Parte da Emater
         Renda = NA, DAP = NA, Apto_EMATER = NA, Objetivo = NA, Data = NA) %>%
  select_("n","Nome","Apelido","NIS","CPF","Endereço", "Dom_acessa_CRAS",
          "Apto_AS","Renda","DAP", "Apto_EMATER","Objetivo","Data") %>%
  write_excel_csv(., "lista_bandeira_EMATER.csv", na = "")

divisopolis = selecao_acao2[selecao_acao2$nome_munic=="DIVISÓPOLIS",]
divisopolis %>% filter(cod_parentesco_rf_pessoa == 1) %>%
  mutate(Endereço = paste(nom_tip_logradouro_fam, nom_titulo_logradouro_fam, 
                          nom_logradouro_fam, num_logradouro_fam, 
                          des_complemento_fam, des_complemento_adic_fam,
                          num_cep_logradouro_fam)) %>%
  mutate(n = 1:nrow(.), Nome = nom_pessoa, Apelido = nom_apelido_pessoa,
         NIS = num_nis_pessoa_atual, 
         CPF = num_cpf_pessoa, Endereço = Endereço,
         # Parte da AS
         Dom_acessa_CRAS = NA, Apto_AS = NA, 
         # Parte da Emater
         Renda = NA, DAP = NA, Apto_EMATER = NA, Objetivo = NA, Data = NA) %>%
  select_("n","Nome","Apelido","NIS","CPF","Endereço", "Dom_acessa_CRAS",
          "Apto_AS","Renda","DAP", "Apto_EMATER","Objetivo","Data") %>%
  write_excel_csv(., "lista_divisopolis_EMATER.csv", na = "")

felisburgo = selecao_acao2[selecao_acao2$nome_munic=="FELISBURGO",]
felisburgo %>% filter(cod_parentesco_rf_pessoa == 1) %>%
  mutate(Endereço = paste(nom_tip_logradouro_fam, nom_titulo_logradouro_fam, 
                          nom_logradouro_fam, num_logradouro_fam, 
                          des_complemento_fam, des_complemento_adic_fam,
                          num_cep_logradouro_fam)) %>%
  mutate(n = 1:nrow(.), Nome = nom_pessoa, Apelido = nom_apelido_pessoa,
         NIS = num_nis_pessoa_atual, 
         CPF = num_cpf_pessoa, Endereço = Endereço,
         # Parte da AS
         Dom_acessa_CRAS = NA, Apto_AS = NA, 
         # Parte da Emater
         Renda = NA, DAP = NA, Apto_EMATER = NA, Objetivo = NA, Data = NA) %>%
  select_("n","Nome","Apelido","NIS","CPF","Endereço", "Dom_acessa_CRAS",
          "Apto_AS","Renda","DAP", "Apto_EMATER","Objetivo","Data") %>%
  write_excel_csv(., "lista_felisburso_EMATER.csv", na = "")

jacinto = selecao_acao2[selecao_acao2$nome_munic=="JACINTO",]
jacinto %>% filter(cod_parentesco_rf_pessoa == 1) %>%
  mutate(Endereço = paste(nom_tip_logradouro_fam, nom_titulo_logradouro_fam, 
                          nom_logradouro_fam, num_logradouro_fam, 
                          des_complemento_fam, des_complemento_adic_fam,
                          num_cep_logradouro_fam)) %>%
  mutate(n = 1:nrow(.), Nome = nom_pessoa, Apelido = nom_apelido_pessoa,
         NIS = num_nis_pessoa_atual, 
         CPF = num_cpf_pessoa, Endereço = Endereço,
         # Parte da AS
         Dom_acessa_CRAS = NA, Apto_AS = NA, 
         # Parte da Emater
         Renda = NA, DAP = NA, Apto_EMATER = NA, Objetivo = NA, Data = NA) %>%
  select_("n","Nome","Apelido","NIS","CPF","Endereço", "Dom_acessa_CRAS",
          "Apto_AS","Renda","DAP", "Apto_EMATER","Objetivo","Data") %>%
  write_excel_csv(., "lista_jacinto_EMATER.csv", na = "")

jequitinhonha = selecao_acao2[selecao_acao2$nome_munic=="JEQUITINHONHA",]
jequitinhonha %>% filter(cod_parentesco_rf_pessoa == 1) %>%
  mutate(Endereço = paste(nom_tip_logradouro_fam, nom_titulo_logradouro_fam, 
                          nom_logradouro_fam, num_logradouro_fam, 
                          des_complemento_fam, des_complemento_adic_fam,
                          num_cep_logradouro_fam)) %>%
  mutate(n = 1:nrow(.), Nome = nom_pessoa, Apelido = nom_apelido_pessoa,
         NIS = num_nis_pessoa_atual, 
         CPF = num_cpf_pessoa, Endereço = Endereço,
         # Parte da AS
         Dom_acessa_CRAS = NA, Apto_AS = NA, 
         # Parte da Emater
         Renda = NA, DAP = NA, Apto_EMATER = NA, Objetivo = NA, Data = NA) %>%
  select_("n","Nome","Apelido","NIS","CPF","Endereço", "Dom_acessa_CRAS",
          "Apto_AS","Renda","DAP", "Apto_EMATER","Objetivo","Data") %>%
  write_excel_csv(., "lista_jequitinhonha_EMATER.csv", na = "")

joaima = selecao_acao2[selecao_acao2$nome_munic=="JOAÍMA",]
joaima %>% filter(cod_parentesco_rf_pessoa == 1) %>%
  mutate(Endereço = paste(nom_tip_logradouro_fam, nom_titulo_logradouro_fam, 
                          nom_logradouro_fam, num_logradouro_fam, 
                          des_complemento_fam, des_complemento_adic_fam,
                          num_cep_logradouro_fam)) %>%
  mutate(n = 1:nrow(.), Nome = nom_pessoa, Apelido = nom_apelido_pessoa,
         NIS = num_nis_pessoa_atual, 
         CPF = num_cpf_pessoa, Endereço = Endereço,
         # Parte da AS
         Dom_acessa_CRAS = NA, Apto_AS = NA, 
         # Parte da Emater
         Renda = NA, DAP = NA, Apto_EMATER = NA, Objetivo = NA, Data = NA) %>%
  select_("n","Nome","Apelido","NIS","CPF","Endereço", "Dom_acessa_CRAS",
          "Apto_AS","Renda","DAP", "Apto_EMATER","Objetivo","Data") %>%
  write_excel_csv(., "lista_joaima_EMATER.csv", na = "")

jordania = selecao_acao2[selecao_acao2$nome_munic=="JORDÂNIA",]
jordania %>% filter(cod_parentesco_rf_pessoa == 1) %>%
  mutate(Endereço = paste(nom_tip_logradouro_fam, nom_titulo_logradouro_fam, 
                          nom_logradouro_fam, num_logradouro_fam, 
                          des_complemento_fam, des_complemento_adic_fam,
                          num_cep_logradouro_fam)) %>%
  mutate(n = 1:nrow(.), Nome = nom_pessoa, Apelido = nom_apelido_pessoa,
         NIS = num_nis_pessoa_atual, 
         CPF = num_cpf_pessoa, Endereço = Endereço,
         # Parte da AS
         Dom_acessa_CRAS = NA, Apto_AS = NA, 
         # Parte da Emater
         Renda = NA, DAP = NA, Apto_EMATER = NA, Objetivo = NA, Data = NA) %>%
  select_("n","Nome","Apelido","NIS","CPF","Endereço", "Dom_acessa_CRAS",
          "Apto_AS","Renda","DAP", "Apto_EMATER","Objetivo","Data") %>%
  write_excel_csv(., "lista_jordania_EMATER.csv", na = "")

monte_formoso = selecao_acao2[selecao_acao2$nome_munic=="MONTE FORMOSO",]
monte_formoso %>% filter(cod_parentesco_rf_pessoa == 1) %>%
  mutate(Endereço = paste(nom_tip_logradouro_fam, nom_titulo_logradouro_fam, 
                          nom_logradouro_fam, num_logradouro_fam, 
                          des_complemento_fam, des_complemento_adic_fam,
                          num_cep_logradouro_fam)) %>%
  mutate(n = 1:nrow(.), Nome = nom_pessoa, Apelido = nom_apelido_pessoa,
         NIS = num_nis_pessoa_atual, 
         CPF = num_cpf_pessoa, Endereço = Endereço,
         # Parte da AS
         Dom_acessa_CRAS = NA, Apto_AS = NA, 
         # Parte da Emater
         Renda = NA, DAP = NA, Apto_EMATER = NA, Objetivo = NA, Data = NA) %>%
  select_("n","Nome","Apelido","NIS","CPF","Endereço", "Dom_acessa_CRAS",
          "Apto_AS","Renda","DAP", "Apto_EMATER","Objetivo","Data") %>%
  write_excel_csv(., "lista_monte_formoso_EMATER.csv", na = "")

palmopolis = selecao_acao2[selecao_acao2$nome_munic=="PALMÓPOLIS",]
palmopolis %>% filter(cod_parentesco_rf_pessoa == 1) %>%
  mutate(Endereço = paste(nom_tip_logradouro_fam, nom_titulo_logradouro_fam, 
                          nom_logradouro_fam, num_logradouro_fam, 
                          des_complemento_fam, des_complemento_adic_fam,
                          num_cep_logradouro_fam)) %>%
  mutate(n = 1:nrow(.), Nome = nom_pessoa, Apelido = nom_apelido_pessoa,
         NIS = num_nis_pessoa_atual, 
         CPF = num_cpf_pessoa, Endereço = Endereço,
         # Parte da AS
         Dom_acessa_CRAS = NA, Apto_AS = NA, 
         # Parte da Emater
         Renda = NA, DAP = NA, Apto_EMATER = NA, Objetivo = NA, Data = NA) %>%
  select_("n","Nome","Apelido","NIS","CPF","Endereço", "Dom_acessa_CRAS",
          "Apto_AS","Renda","DAP", "Apto_EMATER","Objetivo","Data") %>%
  write_excel_csv(., "lista_palmopolis_EMATER.csv", na = "")

pedra_azul = selecao_acao2[selecao_acao2$nome_munic=="PEDRA AZUL",]
pedra_azul %>% filter(cod_parentesco_rf_pessoa == 1) %>%
  mutate(Endereço = paste(nom_tip_logradouro_fam, nom_titulo_logradouro_fam, 
                          nom_logradouro_fam, num_logradouro_fam, 
                          des_complemento_fam, des_complemento_adic_fam,
                          num_cep_logradouro_fam)) %>%
  mutate(n = 1:nrow(.), Nome = nom_pessoa, Apelido = nom_apelido_pessoa,
         NIS = num_nis_pessoa_atual, 
         CPF = num_cpf_pessoa, Endereço = Endereço,
         # Parte da AS
         Dom_acessa_CRAS = NA, Apto_AS = NA, 
         # Parte da Emater
         Renda = NA, DAP = NA, Apto_EMATER = NA, Objetivo = NA, Data = NA) %>%
  select_("n","Nome","Apelido","NIS","CPF","Endereço", "Dom_acessa_CRAS",
          "Apto_AS","Renda","DAP", "Apto_EMATER","Objetivo","Data") %>%
  write_excel_csv(., "lista_pedra_azul_EMATER.csv", na = "")

rio_do_prado = selecao_acao2[selecao_acao2$nome_munic=="RIO DO PRADO",]
rio_do_prado %>% filter(cod_parentesco_rf_pessoa == 1) %>%
  mutate(Endereço = paste(nom_tip_logradouro_fam, nom_titulo_logradouro_fam, 
                          nom_logradouro_fam, num_logradouro_fam, 
                          des_complemento_fam, des_complemento_adic_fam,
                          num_cep_logradouro_fam)) %>%
  mutate(n = 1:nrow(.), Nome = nom_pessoa, Apelido = nom_apelido_pessoa,
         NIS = num_nis_pessoa_atual, 
         CPF = num_cpf_pessoa, Endereço = Endereço,
         # Parte da AS
         Dom_acessa_CRAS = NA, Apto_AS = NA, 
         # Parte da Emater
         Renda = NA, DAP = NA, Apto_EMATER = NA, Objetivo = NA, Data = NA) %>%
  select_("n","Nome","Apelido","NIS","CPF","Endereço", "Dom_acessa_CRAS",
          "Apto_AS","Renda","DAP", "Apto_EMATER","Objetivo","Data") %>%
  write_excel_csv(., "lista_rio_do_prado_EMATER.csv", na = "")

rubim = selecao_acao2[selecao_acao2$nome_munic=="RUBIM",]
rubim %>% filter(cod_parentesco_rf_pessoa == 1) %>%
  mutate(Endereço = paste(nom_tip_logradouro_fam, nom_titulo_logradouro_fam, 
                          nom_logradouro_fam, num_logradouro_fam, 
                          des_complemento_fam, des_complemento_adic_fam,
                          num_cep_logradouro_fam)) %>%
  mutate(n = 1:nrow(.), Nome = nom_pessoa, Apelido = nom_apelido_pessoa,
         NIS = num_nis_pessoa_atual, 
         CPF = num_cpf_pessoa, Endereço = Endereço,
         # Parte da AS
         Dom_acessa_CRAS = NA, Apto_AS = NA, 
         # Parte da Emater
         Renda = NA, DAP = NA, Apto_EMATER = NA, Objetivo = NA, Data = NA) %>%
  select_("n","Nome","Apelido","NIS","CPF","Endereço", "Dom_acessa_CRAS",
          "Apto_AS","Renda","DAP", "Apto_EMATER","Objetivo","Data") %>%
  write_excel_csv(., "lista_rubim_EMATER.csv", na = "")

divisa = selecao_acao2[selecao_acao2$nome_munic=="SALTO DA DIVISA",]
divisa %>% filter(cod_parentesco_rf_pessoa == 1) %>%
  mutate(Endereço = paste(nom_tip_logradouro_fam, nom_titulo_logradouro_fam, 
                          nom_logradouro_fam, num_logradouro_fam, 
                          des_complemento_fam, des_complemento_adic_fam,
                          num_cep_logradouro_fam)) %>%
  mutate(n = 1:nrow(.), Nome = nom_pessoa, Apelido = nom_apelido_pessoa,
         NIS = num_nis_pessoa_atual, 
         CPF = num_cpf_pessoa, Endereço = Endereço,
         # Parte da AS
         Dom_acessa_CRAS = NA, Apto_AS = NA, 
         # Parte da Emater
         Renda = NA, DAP = NA, Apto_EMATER = NA, Objetivo = NA, Data = NA) %>%
  select_("n","Nome","Apelido","NIS","CPF","Endereço", "Dom_acessa_CRAS",
          "Apto_AS","Renda","DAP", "Apto_EMATER","Objetivo","Data") %>%
  write_excel_csv(., "lista_salto_da_divisa_EMATER.csv", na = "")

salto = selecao_acao2[selecao_acao2$nome_munic=="SANTA MARIA DO SALTO",]
salto %>% filter(cod_parentesco_rf_pessoa == 1) %>%
  mutate(Endereço = paste(nom_tip_logradouro_fam, nom_titulo_logradouro_fam, 
                          nom_logradouro_fam, num_logradouro_fam, 
                          des_complemento_fam, des_complemento_adic_fam,
                          num_cep_logradouro_fam)) %>%
  mutate(n = 1:nrow(.), Nome = nom_pessoa, Apelido = nom_apelido_pessoa,
         NIS = num_nis_pessoa_atual, 
         CPF = num_cpf_pessoa, Endereço = Endereço,
         # Parte da AS
         Dom_acessa_CRAS = NA, Apto_AS = NA, 
         # Parte da Emater
         Renda = NA, DAP = NA, Apto_EMATER = NA, Objetivo = NA, Data = NA) %>%
  select_("n","Nome","Apelido","NIS","CPF","Endereço", "Dom_acessa_CRAS",
          "Apto_AS","Renda","DAP", "Apto_EMATER","Objetivo","Data") %>%
  write_excel_csv(., "lista_samta_maria_do_salto_EMATER.csv", na = "")

stantonio = selecao_acao2[selecao_acao2$nome_munic=="SANTO ANTÔNIO DO JACINTO",]
stantonio %>% filter(cod_parentesco_rf_pessoa == 1) %>%
  mutate(Endereço = paste(nom_tip_logradouro_fam, nom_titulo_logradouro_fam, 
                          nom_logradouro_fam, num_logradouro_fam, 
                          des_complemento_fam, des_complemento_adic_fam,
                          num_cep_logradouro_fam)) %>%
  mutate(n = 1:nrow(.), Nome = nom_pessoa, Apelido = nom_apelido_pessoa,
         NIS = num_nis_pessoa_atual, 
         CPF = num_cpf_pessoa, Endereço = Endereço,
         # Parte da AS
         Dom_acessa_CRAS = NA, Apto_AS = NA, 
         # Parte da Emater
         Renda = NA, DAP = NA, Apto_EMATER = NA, Objetivo = NA, Data = NA) %>%
  select_("n","Nome","Apelido","NIS","CPF","Endereço", "Dom_acessa_CRAS",
          "Apto_AS","Renda","DAP", "Apto_EMATER","Objetivo","Data") %>%
  write_excel_csv(., "lista_santo_antonio_do_jacinto_EMATER.csv", na = "")

mata_verde = selecao_acao2[selecao_acao2$nome_munic=="MATA VERDE",]
mata_verde %>% filter(cod_parentesco_rf_pessoa == 1) %>%
  mutate(Endereço = paste(nom_tip_logradouro_fam, nom_titulo_logradouro_fam, 
                          nom_logradouro_fam, num_logradouro_fam, 
                          des_complemento_fam, des_complemento_adic_fam,
                          num_cep_logradouro_fam)) %>%
  mutate(n = 1:nrow(.), Nome = nom_pessoa, Apelido = nom_apelido_pessoa,
         NIS = num_nis_pessoa_atual, 
         CPF = num_cpf_pessoa, Endereço = Endereço,
         # Parte da AS
         Dom_acessa_CRAS = NA, Apto_AS = NA, 
         # Parte da Emater
         Renda = NA, DAP = NA, Apto_EMATER = NA, Objetivo = NA, Data = NA) %>%
  select_("n","Nome","Apelido","NIS","CPF","Endereço", "Dom_acessa_CRAS",
          "Apto_AS","Renda","DAP", "Apto_EMATER","Objetivo","Data") %>%
  write_excel_csv(., "lista_mata_verde_EMATER.csv", na = "")

aguas_vermelhas = selecao_acao2[selecao_acao2$nome_munic=="ÁGUAS VERMELHAS",]
aguas_vermelhas %>% filter(cod_parentesco_rf_pessoa == 1) %>%
  mutate(Endereço = paste(nom_tip_logradouro_fam, nom_titulo_logradouro_fam, 
                          nom_logradouro_fam, num_logradouro_fam, 
                          des_complemento_fam, des_complemento_adic_fam,
                          num_cep_logradouro_fam)) %>%
  mutate(n = 1:nrow(.), Nome = nom_pessoa, Apelido = nom_apelido_pessoa,
         NIS = num_nis_pessoa_atual, 
         CPF = num_cpf_pessoa, Endereço = Endereço,
         # Parte da AS
         Dom_acessa_CRAS = NA, Apto_AS = NA, 
         # Parte da Emater
         Renda = NA, DAP = NA, Apto_EMATER = NA, Objetivo = NA, Data = NA) %>%
  select_("n","Nome","Apelido","NIS","CPF","Endereço", "Dom_acessa_CRAS",
          "Apto_AS","Renda","DAP", "Apto_EMATER","Objetivo","Data") %>%
  write_excel_csv(., "lista_aguas_vermelhas_EMATER.csv", na = "")

cachoeira = selecao_acao2[selecao_acao2$nome_munic=="CACHOEIRA DE PAJEÚ",]
cachoeira %>% filter(cod_parentesco_rf_pessoa == 1) %>%
  mutate(Endereço = paste(nom_tip_logradouro_fam, nom_titulo_logradouro_fam, 
                          nom_logradouro_fam, num_logradouro_fam, 
                          des_complemento_fam, des_complemento_adic_fam,
                          num_cep_logradouro_fam)) %>%
  mutate(n = 1:nrow(.), Nome = nom_pessoa, Apelido = nom_apelido_pessoa,
         NIS = num_nis_pessoa_atual, 
         CPF = num_cpf_pessoa, Endereço = Endereço,
         # Parte da AS
         Dom_acessa_CRAS = NA, Apto_AS = NA, 
         # Parte da Emater
         Renda = NA, DAP = NA, Apto_EMATER = NA, Objetivo = NA, Data = NA) %>%
  select_("n","Nome","Apelido","NIS","CPF","Endereço", "Dom_acessa_CRAS",
          "Apto_AS","Renda","DAP", "Apto_EMATER","Objetivo","Data") %>%
  write_excel_csv(., "lista_cachoeira_de_pajeu_EMATER.csv", na = "")


curral = selecao_acao2[selecao_acao2$nome_munic=="CURRAL DE DENTRO",]
curral  %>% filter(cod_parentesco_rf_pessoa == 1) %>%
  mutate(Endereço = paste(nom_tip_logradouro_fam, nom_titulo_logradouro_fam, 
                          nom_logradouro_fam, num_logradouro_fam, 
                          des_complemento_fam, des_complemento_adic_fam,
                          num_cep_logradouro_fam)) %>%
  mutate(n = 1:nrow(.), Nome = nom_pessoa, Apelido = nom_apelido_pessoa,
         NIS = num_nis_pessoa_atual, 
         CPF = num_cpf_pessoa, Endereço = Endereço,
         # Parte da AS
         Dom_acessa_CRAS = NA, Apto_AS = NA, 
         # Parte da Emater
         Renda = NA, DAP = NA, Apto_EMATER = NA, Objetivo = NA, Data = NA) %>%
  select_("n","Nome","Apelido","NIS","CPF","Endereço", "Dom_acessa_CRAS",
          "Apto_AS","Renda","DAP", "Apto_EMATER","Objetivo","Data") %>%
  write_excel_csv(., "lista_curral_EMATER.csv", na = "")

divisa_alegre = selecao_acao2[selecao_acao2$nome_munic=="DIVISA ALEGRE",]
divisa_alegre %>% filter(cod_parentesco_rf_pessoa == 1) %>%
  mutate(Endereço = paste(nom_tip_logradouro_fam, nom_titulo_logradouro_fam, 
                          nom_logradouro_fam, num_logradouro_fam, 
                          des_complemento_fam, des_complemento_adic_fam,
                          num_cep_logradouro_fam)) %>%
  mutate(n = 1:nrow(.), Nome = nom_pessoa, Apelido = nom_apelido_pessoa,
         NIS = num_nis_pessoa_atual, 
         CPF = num_cpf_pessoa, Endereço = Endereço,
         # Parte da AS
         Dom_acessa_CRAS = NA, Apto_AS = NA, 
         # Parte da Emater
         Renda = NA, DAP = NA, Apto_EMATER = NA, Objetivo = NA, Data = NA) %>%
  select_("n","Nome","Apelido","NIS","CPF","Endereço", "Dom_acessa_CRAS",
          "Apto_AS","Renda","DAP", "Apto_EMATER","Objetivo","Data") %>%
  write_excel_csv(., "lista_divisa_alegre_EMATER.csv", na = "")


sre_almenara = rbind(almenara, bandeira, divisopolis, felisburgo, jacinto,
                     jequitinhonha, joaima, jordania, monte_formoso, palmopolis,
                     pedra_azul, rio_do_prado, rubim, divisa, salto, stantonio,
                     mata_verde, aguas_vermelhas, cachoeira, curral, divisa_alegre)

dim(sre_almenara)
#write_csv(sre_almenara, "selecionados_sementes_presentes_sre_almenara.csv")


### SRE Januária

setwd()

januaria = selecao_acao2[selecao_acao2$nome_munic=="JANUÁRIA",]
manga = selecao_acao2[selecao_acao2$nome_munic=="MANGA",]
bonito = selecao_acao2[selecao_acao2$nome_munic=="BONITO DE MINAS",]
conego_marinho = selecao_acao2[selecao_acao2$nome_munic=="CÔNEGO MARINHO",]
ibiracatu = selecao_acao2[selecao_acao2$nome_munic=="IBIRACATU",]
icarai = selecao_acao2[selecao_acao2$nome_munic=="ICARAÍ DE MINAS",]
itacarambi = selecao_acao2[selecao_acao2$nome_munic=="ITACARAMBI",]
juvenilia = selecao_acao2[selecao_acao2$nome_munic=="JUVENÍLIA",]
miravania = selecao_acao2[selecao_acao2$nome_munic=="MIRAVÂNIA",]
montalvania = selecao_acao2[selecao_acao2$nome_munic=="MONTALVÂNIA",]
pedras = selecao_acao2[selecao_acao2$nome_munic=="PEDRAS DE MARIA DA CRUZ",]
pintopolis = selecao_acao2[selecao_acao2$nome_munic=="PINTÓPOLIS",]
sao_francisco = selecao_acao2[selecao_acao2$nome_munic=="SÃO FRANCISCO",]
missoes = selecao_acao2[selecao_acao2$nome_munic=="SÃO JOÃO DAS MISSÕES",]
ubai = selecao_acao2[selecao_acao2$nome_munic=="UBAÍ",]
urucuia = selecao_acao2[selecao_acao2$nome_munic=="URUCUIA",]
varzelandia = selecao_acao2[selecao_acao2$nome_munic=="VARZELÂNDIA",]
matias_cardoso = selecao_acao2[selecao_acao2$nome_munic=="MATIAS CARDOSO",]

sre_januaria = rbind(januaria, manga, bonito, conego_marinho, ibiracatu,
                     icarai, itacarambi, juvenilia, miravania, montalvania, 
                     pedras, pintopolis, sao_francisco, missoes, ubai, urucuia,
                     varzelandia, matias_cardoso)

#write_csv(sre_januaria, "selecionados_sementes_presentes_sre_januaria.csv")

### SRE Montes CLaros
#Itacambira e Olhos D'água não tem ninguém!

setwd("~/Documentos/CADUNICO/SRE_Montes_Claros")

montes_claros = selecao_acao2[selecao_acao2$nome_munic=="MONTES CLAROS",]
montes_claros %>% filter(cod_parentesco_rf_pessoa == 1) %>%
  mutate(Endereço = paste(nom_tip_logradouro_fam, nom_titulo_logradouro_fam, 
                          nom_logradouro_fam, num_logradouro_fam, 
                          des_complemento_fam, des_complemento_adic_fam,
                          num_cep_logradouro_fam)) %>%
  mutate(n = 1:nrow(.), Nome = nom_pessoa, Apelido = nom_apelido_pessoa,
         NIS = num_nis_pessoa_atual, 
         CPF = num_cpf_pessoa, Endereço = Endereço,
         # Parte da AS
         Dom_acessa_CRAS = NA, Apto_AS = NA, 
         # Parte da Emater
         Renda = NA, DAP = NA, Apto_EMATER = NA, Objetivo = NA, Data = NA) %>%
  select_("n","Nome","Apelido","NIS","CPF","Endereço", "Dom_acessa_CRAS",
          "Apto_AS","Renda","DAP", "Apto_EMATER","Objetivo","Data") %>%
  write_excel_csv(., "lista_montes_claros_EMATER.csv", na = "")

campo_azul = selecao_acao2[selecao_acao2$nome_munic=="CAMPO AZUL",]
campo_azul %>% filter(cod_parentesco_rf_pessoa == 1) %>%
  mutate(Endereço = paste(nom_tip_logradouro_fam, nom_titulo_logradouro_fam, 
                          nom_logradouro_fam, num_logradouro_fam, 
                          des_complemento_fam, des_complemento_adic_fam,
                          num_cep_logradouro_fam)) %>%
  mutate(n = 1:nrow(.), Nome = nom_pessoa, Apelido = nom_apelido_pessoa,
         NIS = num_nis_pessoa_atual, 
         CPF = num_cpf_pessoa, Endereço = Endereço,
         # Parte da AS
         Dom_acessa_CRAS = NA, Apto_AS = NA, 
         # Parte da Emater
         Renda = NA, DAP = NA, Apto_EMATER = NA, Objetivo = NA, Data = NA) %>%
  select_("n","Nome","Apelido","NIS","CPF","Endereço", "Dom_acessa_CRAS",
          "Apto_AS","Renda","DAP", "Apto_EMATER","Objetivo","Data") %>%
  write_excel_csv(., "lista_campo_azul_EMATER.csv", na = "")

japonvar = selecao_acao2[selecao_acao2$nome_munic=="JAPONVAR",]
japonvar %>% filter(cod_parentesco_rf_pessoa == 1) %>%
  mutate(Endereço = paste(nom_tip_logradouro_fam, nom_titulo_logradouro_fam, 
                          nom_logradouro_fam, num_logradouro_fam, 
                          des_complemento_fam, des_complemento_adic_fam,
                          num_cep_logradouro_fam)) %>%
  mutate(n = 1:nrow(.), Nome = nom_pessoa, Apelido = nom_apelido_pessoa,
         NIS = num_nis_pessoa_atual, 
         CPF = num_cpf_pessoa, Endereço = Endereço,
         # Parte da AS
         Dom_acessa_CRAS = NA, Apto_AS = NA, 
         # Parte da Emater
         Renda = NA, DAP = NA, Apto_EMATER = NA, Objetivo = NA, Data = NA) %>%
  select_("n","Nome","Apelido","NIS","CPF","Endereço", "Dom_acessa_CRAS",
          "Apto_AS","Renda","DAP", "Apto_EMATER","Objetivo","Data") %>%
  write_excel_csv(., "lista_japonvar_EMATER.csv", na = "")

lontra = selecao_acao2[selecao_acao2$nome_munic=="LONTRA",]
lontra %>% filter(cod_parentesco_rf_pessoa == 1) %>%
  mutate(Endereço = paste(nom_tip_logradouro_fam, nom_titulo_logradouro_fam, 
                          nom_logradouro_fam, num_logradouro_fam, 
                          des_complemento_fam, des_complemento_adic_fam,
                          num_cep_logradouro_fam)) %>%
  mutate(n = 1:nrow(.), Nome = nom_pessoa, Apelido = nom_apelido_pessoa,
         NIS = num_nis_pessoa_atual, 
         CPF = num_cpf_pessoa, Endereço = Endereço,
         # Parte da AS
         Dom_acessa_CRAS = NA, Apto_AS = NA, 
         # Parte da Emater
         Renda = NA, DAP = NA, Apto_EMATER = NA, Objetivo = NA, Data = NA) %>%
  select_("n","Nome","Apelido","NIS","CPF","Endereço", "Dom_acessa_CRAS",
          "Apto_AS","Renda","DAP", "Apto_EMATER","Objetivo","Data") %>%
  write_excel_csv(., "lista_lontra_EMATER.csv", na = "")

patis = selecao_acao2[selecao_acao2$nome_munic=="PATIS",]
patis  %>% filter(cod_parentesco_rf_pessoa == 1) %>%
  mutate(Endereço = paste(nom_tip_logradouro_fam, nom_titulo_logradouro_fam, 
                          nom_logradouro_fam, num_logradouro_fam, 
                          des_complemento_fam, des_complemento_adic_fam,
                          num_cep_logradouro_fam)) %>%
  mutate(n = 1:nrow(.), Nome = nom_pessoa, Apelido = nom_apelido_pessoa,
         NIS = num_nis_pessoa_atual, 
         CPF = num_cpf_pessoa, Endereço = Endereço,
         # Parte da AS
         Dom_acessa_CRAS = NA, Apto_AS = NA, 
         # Parte da Emater
         Renda = NA, DAP = NA, Apto_EMATER = NA, Objetivo = NA, Data = NA) %>%
  select_("n","Nome","Apelido","NIS","CPF","Endereço", "Dom_acessa_CRAS",
          "Apto_AS","Renda","DAP", "Apto_EMATER","Objetivo","Data") %>%
  write_excel_csv(., "lista_patis_EMATER.csv", na = "")

sjdponte = selecao_acao2[selecao_acao2$nome_munic=="SÃO JOÃO DA PONTE",]
sjdponte %>% filter(cod_parentesco_rf_pessoa == 1) %>%
  mutate(Endereço = paste(nom_tip_logradouro_fam, nom_titulo_logradouro_fam, 
                          nom_logradouro_fam, num_logradouro_fam, 
                          des_complemento_fam, des_complemento_adic_fam,
                          num_cep_logradouro_fam)) %>%
  mutate(n = 1:nrow(.), Nome = nom_pessoa, Apelido = nom_apelido_pessoa,
         NIS = num_nis_pessoa_atual, 
         CPF = num_cpf_pessoa, Endereço = Endereço,
         # Parte da AS
         Dom_acessa_CRAS = NA, Apto_AS = NA, 
         # Parte da Emater
         Renda = NA, DAP = NA, Apto_EMATER = NA, Objetivo = NA, Data = NA) %>%
  select_("n","Nome","Apelido","NIS","CPF","Endereço", "Dom_acessa_CRAS",
          "Apto_AS","Renda","DAP", "Apto_EMATER","Objetivo","Data") %>%
  write_excel_csv(., "lista_sao_joao_da_ponte_EMATER.csv", na = "")


bocaiuva = selecao_acao2[selecao_acao2$nome_munic=="BOCAIÚVA",]
bocaiuva %>% filter(cod_parentesco_rf_pessoa == 1) %>%
  mutate(Endereço = paste(nom_tip_logradouro_fam, nom_titulo_logradouro_fam, 
                          nom_logradouro_fam, num_logradouro_fam, 
                          des_complemento_fam, des_complemento_adic_fam,
                          num_cep_logradouro_fam)) %>%
  mutate(n = 1:nrow(.), Nome = nom_pessoa, Apelido = nom_apelido_pessoa,
         NIS = num_nis_pessoa_atual, 
         CPF = num_cpf_pessoa, Endereço = Endereço,
         # Parte da AS
         Dom_acessa_CRAS = NA, Apto_AS = NA, 
         # Parte da Emater
         Renda = NA, DAP = NA, Apto_EMATER = NA, Objetivo = NA, Data = NA) %>%
  select_("n","Nome","Apelido","NIS","CPF","Endereço", "Dom_acessa_CRAS",
          "Apto_AS","Renda","DAP", "Apto_EMATER","Objetivo","Data") %>%
  write_excel_csv(., "lista_bocaiuva_EMATER.csv", na = "")

botumirim = selecao_acao2[selecao_acao2$nome_munic=="BOTUMIRIM",]
botumirim %>% filter(cod_parentesco_rf_pessoa == 1) %>%
  mutate(Endereço = paste(nom_tip_logradouro_fam, nom_titulo_logradouro_fam, 
                          nom_logradouro_fam, num_logradouro_fam, 
                          des_complemento_fam, des_complemento_adic_fam,
                          num_cep_logradouro_fam)) %>%
  mutate(n = 1:nrow(.), Nome = nom_pessoa, Apelido = nom_apelido_pessoa,
         NIS = num_nis_pessoa_atual, 
         CPF = num_cpf_pessoa, Endereço = Endereço,
         # Parte da AS
         Dom_acessa_CRAS = NA, Apto_AS = NA, 
         # Parte da Emater
         Renda = NA, DAP = NA, Apto_EMATER = NA, Objetivo = NA, Data = NA) %>%
  select_("n","Nome","Apelido","NIS","CPF","Endereço", "Dom_acessa_CRAS",
          "Apto_AS","Renda","DAP", "Apto_EMATER","Objetivo","Data") %>%
  write_excel_csv(., "lista_botumirim_EMATER.csv", na = "")

brasilia_de_minas = selecao_acao2[selecao_acao2$nome_munic=="BRASÍLIA DE MINAS",]
brasilia_de_minas %>% filter(cod_parentesco_rf_pessoa == 1) %>%
  mutate(Endereço = paste(nom_tip_logradouro_fam, nom_titulo_logradouro_fam, 
                          nom_logradouro_fam, num_logradouro_fam, 
                          des_complemento_fam, des_complemento_adic_fam,
                          num_cep_logradouro_fam)) %>%
  mutate(n = 1:nrow(.), Nome = nom_pessoa, Apelido = nom_apelido_pessoa,
         NIS = num_nis_pessoa_atual, 
         CPF = num_cpf_pessoa, Endereço = Endereço,
         # Parte da AS
         Dom_acessa_CRAS = NA, Apto_AS = NA, 
         # Parte da Emater
         Renda = NA, DAP = NA, Apto_EMATER = NA, Objetivo = NA, Data = NA) %>%
  select_("n","Nome","Apelido","NIS","CPF","Endereço", "Dom_acessa_CRAS",
          "Apto_AS","Renda","DAP", "Apto_EMATER","Objetivo","Data") %>%
  write_excel_csv(., "lista_brasilia_de_minas_EMATER.csv", na = "")

capitao_eneas = selecao_acao2[selecao_acao2$nome_munic=="CAPITÃO ENÉAS",]
capitao_eneas %>% filter(cod_parentesco_rf_pessoa == 1) %>%
  mutate(Endereço = paste(nom_tip_logradouro_fam, nom_titulo_logradouro_fam, 
                          nom_logradouro_fam, num_logradouro_fam, 
                          des_complemento_fam, des_complemento_adic_fam,
                          num_cep_logradouro_fam)) %>%
  mutate(n = 1:nrow(.), Nome = nom_pessoa, Apelido = nom_apelido_pessoa,
         NIS = num_nis_pessoa_atual, 
         CPF = num_cpf_pessoa, Endereço = Endereço,
         # Parte da AS
         Dom_acessa_CRAS = NA, Apto_AS = NA, 
         # Parte da Emater
         Renda = NA, DAP = NA, Apto_EMATER = NA, Objetivo = NA, Data = NA) %>%
  select_("n","Nome","Apelido","NIS","CPF","Endereço", "Dom_acessa_CRAS",
          "Apto_AS","Renda","DAP", "Apto_EMATER","Objetivo","Data") %>%
  write_excel_csv(., "lista_capitao_eneas_EMATER.csv", na = "")

claro_dos_pocoes = selecao_acao2[selecao_acao2$nome_munic=="CLARO DOS POÇÕES",]
claro_dos_pocoes %>% filter(cod_parentesco_rf_pessoa == 1) %>%
  mutate(Endereço = paste(nom_tip_logradouro_fam, nom_titulo_logradouro_fam, 
                          nom_logradouro_fam, num_logradouro_fam, 
                          des_complemento_fam, des_complemento_adic_fam,
                          num_cep_logradouro_fam)) %>%
  mutate(n = 1:nrow(.), Nome = nom_pessoa, Apelido = nom_apelido_pessoa,
         NIS = num_nis_pessoa_atual, 
         CPF = num_cpf_pessoa, Endereço = Endereço,
         # Parte da AS
         Dom_acessa_CRAS = NA, Apto_AS = NA, 
         # Parte da Emater
         Renda = NA, DAP = NA, Apto_EMATER = NA, Objetivo = NA, Data = NA) %>%
  select_("n","Nome","Apelido","NIS","CPF","Endereço", "Dom_acessa_CRAS",
          "Apto_AS","Renda","DAP", "Apto_EMATER","Objetivo","Data") %>%
  write_excel_csv(., "lista_claro_dos_pocoes_EMATER.csv", na = "")

coracao_de_jesus = selecao_acao2[selecao_acao2$nome_munic=="CORAÇÃO DE JESUS",]
coracao_de_jesus %>% filter(cod_parentesco_rf_pessoa == 1) %>%
  mutate(Endereço = paste(nom_tip_logradouro_fam, nom_titulo_logradouro_fam, 
                          nom_logradouro_fam, num_logradouro_fam, 
                          des_complemento_fam, des_complemento_adic_fam,
                          num_cep_logradouro_fam)) %>%
  mutate(n = 1:nrow(.), Nome = nom_pessoa, Apelido = nom_apelido_pessoa,
         NIS = num_nis_pessoa_atual, 
         CPF = num_cpf_pessoa, Endereço = Endereço,
         # Parte da AS
         Dom_acessa_CRAS = NA, Apto_AS = NA, 
         # Parte da Emater
         Renda = NA, DAP = NA, Apto_EMATER = NA, Objetivo = NA, Data = NA) %>%
  select_("n","Nome","Apelido","NIS","CPF","Endereço", "Dom_acessa_CRAS",
          "Apto_AS","Renda","DAP", "Apto_EMATER","Objetivo","Data") %>%
  write_excel_csv(., "lista_coracao_de_jesus_EMATER.csv", na = "")

cristalia = selecao_acao2[selecao_acao2$nome_munic=="CRISTÁLIA",]
cristalia %>% filter(cod_parentesco_rf_pessoa == 1) %>%
  mutate(Endereço = paste(nom_tip_logradouro_fam, nom_titulo_logradouro_fam, 
                          nom_logradouro_fam, num_logradouro_fam, 
                          des_complemento_fam, des_complemento_adic_fam,
                          num_cep_logradouro_fam)) %>%
  mutate(n = 1:nrow(.), Nome = nom_pessoa, Apelido = nom_apelido_pessoa,
         NIS = num_nis_pessoa_atual, 
         CPF = num_cpf_pessoa, Endereço = Endereço,
         # Parte da AS
         Dom_acessa_CRAS = NA, Apto_AS = NA, 
         # Parte da Emater
         Renda = NA, DAP = NA, Apto_EMATER = NA, Objetivo = NA, Data = NA) %>%
  select_("n","Nome","Apelido","NIS","CPF","Endereço", "Dom_acessa_CRAS",
          "Apto_AS","Renda","DAP", "Apto_EMATER","Objetivo","Data") %>%
  write_excel_csv(., "lista_cristalia_EMATER.csv", na = "")

engenheiro_navarro = selecao_acao2[selecao_acao2$nome_munic=="ENGENHEIRO NAVARRO",]
engenheiro_navarro %>% filter(cod_parentesco_rf_pessoa == 1) %>%
  mutate(Endereço = paste(nom_tip_logradouro_fam, nom_titulo_logradouro_fam, 
                          nom_logradouro_fam, num_logradouro_fam, 
                          des_complemento_fam, des_complemento_adic_fam,
                          num_cep_logradouro_fam)) %>%
  mutate(n = 1:nrow(.), Nome = nom_pessoa, Apelido = nom_apelido_pessoa,
         NIS = num_nis_pessoa_atual, 
         CPF = num_cpf_pessoa, Endereço = Endereço,
         # Parte da AS
         Dom_acessa_CRAS = NA, Apto_AS = NA, 
         # Parte da Emater
         Renda = NA, DAP = NA, Apto_EMATER = NA, Objetivo = NA, Data = NA) %>%
  select_("n","Nome","Apelido","NIS","CPF","Endereço", "Dom_acessa_CRAS",
          "Apto_AS","Renda","DAP", "Apto_EMATER","Objetivo","Data") %>%
  write_excel_csv(., "lista_engenheiro_navarro_EMATER.csv", na = "")

francisco_dumont = selecao_acao2[selecao_acao2$nome_munic=="FRANCISCO DUMONT",]
francisco_dumont %>% filter(cod_parentesco_rf_pessoa == 1) %>%
  mutate(Endereço = paste(nom_tip_logradouro_fam, nom_titulo_logradouro_fam, 
                          nom_logradouro_fam, num_logradouro_fam, 
                          des_complemento_fam, des_complemento_adic_fam,
                          num_cep_logradouro_fam)) %>%
  mutate(n = 1:nrow(.), Nome = nom_pessoa, Apelido = nom_apelido_pessoa,
         NIS = num_nis_pessoa_atual, 
         CPF = num_cpf_pessoa, Endereço = Endereço,
         # Parte da AS
         Dom_acessa_CRAS = NA, Apto_AS = NA, 
         # Parte da Emater
         Renda = NA, DAP = NA, Apto_EMATER = NA, Objetivo = NA, Data = NA) %>%
  select_("n","Nome","Apelido","NIS","CPF","Endereço", "Dom_acessa_CRAS",
          "Apto_AS","Renda","DAP", "Apto_EMATER","Objetivo","Data") %>%
  write_excel_csv(., "lista_francisco_dumont_EMATER.csv", na = "")

francisco_sa = selecao_acao2[selecao_acao2$nome_munic=="FRANCISCO SÁ",]
francisco_sa %>% filter(cod_parentesco_rf_pessoa == 1) %>%
  mutate(Endereço = paste(nom_tip_logradouro_fam, nom_titulo_logradouro_fam, 
                          nom_logradouro_fam, num_logradouro_fam, 
                          des_complemento_fam, des_complemento_adic_fam,
                          num_cep_logradouro_fam)) %>%
  mutate(n = 1:nrow(.), Nome = nom_pessoa, Apelido = nom_apelido_pessoa,
         NIS = num_nis_pessoa_atual, 
         CPF = num_cpf_pessoa, Endereço = Endereço,
         # Parte da AS
         Dom_acessa_CRAS = NA, Apto_AS = NA, 
         # Parte da Emater
         Renda = NA, DAP = NA, Apto_EMATER = NA, Objetivo = NA, Data = NA) %>%
  select_("n","Nome","Apelido","NIS","CPF","Endereço", "Dom_acessa_CRAS",
          "Apto_AS","Renda","DAP", "Apto_EMATER","Objetivo","Data") %>%
  write_excel_csv(., "lista_francisco_sa_EMATER.csv", na = "")

glaucilandia = selecao_acao2[selecao_acao2$nome_munic=="GLAUCILÂNDIA",]
glaucilandia %>% filter(cod_parentesco_rf_pessoa == 1) %>%
  mutate(Endereço = paste(nom_tip_logradouro_fam, nom_titulo_logradouro_fam, 
                          nom_logradouro_fam, num_logradouro_fam, 
                          des_complemento_fam, des_complemento_adic_fam,
                          num_cep_logradouro_fam)) %>%
  mutate(n = 1:nrow(.), Nome = nom_pessoa, Apelido = nom_apelido_pessoa,
         NIS = num_nis_pessoa_atual, 
         CPF = num_cpf_pessoa, Endereço = Endereço,
         # Parte da AS
         Dom_acessa_CRAS = NA, Apto_AS = NA, 
         # Parte da Emater
         Renda = NA, DAP = NA, Apto_EMATER = NA, Objetivo = NA, Data = NA) %>%
  select_("n","Nome","Apelido","NIS","CPF","Endereço", "Dom_acessa_CRAS",
          "Apto_AS","Renda","DAP", "Apto_EMATER","Objetivo","Data") %>%
  write_excel_csv(., "lista_glaucilandia_EMATER.csv", na = "")

grao_mogol = selecao_acao2[selecao_acao2$nome_munic=="GRÃO MOGOL",]
grao_mogol %>% filter(cod_parentesco_rf_pessoa == 1) %>%
  mutate(Endereço = paste(nom_tip_logradouro_fam, nom_titulo_logradouro_fam, 
                          nom_logradouro_fam, num_logradouro_fam, 
                          des_complemento_fam, des_complemento_adic_fam,
                          num_cep_logradouro_fam)) %>%
  mutate(n = 1:nrow(.), Nome = nom_pessoa, Apelido = nom_apelido_pessoa,
         NIS = num_nis_pessoa_atual, 
         CPF = num_cpf_pessoa, Endereço = Endereço,
         # Parte da AS
         Dom_acessa_CRAS = NA, Apto_AS = NA, 
         # Parte da Emater
         Renda = NA, DAP = NA, Apto_EMATER = NA, Objetivo = NA, Data = NA) %>%
  select_("n","Nome","Apelido","NIS","CPF","Endereço", "Dom_acessa_CRAS",
          "Apto_AS","Renda","DAP", "Apto_EMATER","Objetivo","Data") %>%
  write_excel_csv(., "lista_grao_mogol_EMATER.csv", na = "")

guaraciama = selecao_acao2[selecao_acao2$nome_munic=="GUARACIAMA",]
guaraciama %>% filter(cod_parentesco_rf_pessoa == 1) %>%
  mutate(Endereço = paste(nom_tip_logradouro_fam, nom_titulo_logradouro_fam, 
                          nom_logradouro_fam, num_logradouro_fam, 
                          des_complemento_fam, des_complemento_adic_fam,
                          num_cep_logradouro_fam)) %>%
  mutate(n = 1:nrow(.), Nome = nom_pessoa, Apelido = nom_apelido_pessoa,
         NIS = num_nis_pessoa_atual, 
         CPF = num_cpf_pessoa, Endereço = Endereço,
         # Parte da AS
         Dom_acessa_CRAS = NA, Apto_AS = NA, 
         # Parte da Emater
         Renda = NA, DAP = NA, Apto_EMATER = NA, Objetivo = NA, Data = NA) %>%
  select_("n","Nome","Apelido","NIS","CPF","Endereço", "Dom_acessa_CRAS",
          "Apto_AS","Renda","DAP", "Apto_EMATER","Objetivo","Data") %>%
  write_excel_csv(., "lista_guaraciama_EMATER.csv", na = "")

itacambira = selecao_acao2[selecao_acao2$nome_munic=="ITABAMBIRA",]
itacambira %>% filter(cod_parentesco_rf_pessoa == 1) %>%
  mutate(Endereço = paste(nom_tip_logradouro_fam, nom_titulo_logradouro_fam, 
                          nom_logradouro_fam, num_logradouro_fam, 
                          des_complemento_fam, des_complemento_adic_fam,
                          num_cep_logradouro_fam)) %>%
  mutate(n = 1:nrow(.), Nome = nom_pessoa, Apelido = nom_apelido_pessoa,
         NIS = num_nis_pessoa_atual, 
         CPF = num_cpf_pessoa, Endereço = Endereço,
         # Parte da AS
         Dom_acessa_CRAS = NA, Apto_AS = NA, 
         # Parte da Emater
         Renda = NA, DAP = NA, Apto_EMATER = NA, Objetivo = NA, Data = NA) %>%
  select_("n","Nome","Apelido","NIS","CPF","Endereço", "Dom_acessa_CRAS",
          "Apto_AS","Renda","DAP", "Apto_EMATER","Objetivo","Data") %>%
  write_excel_csv(., "lista_itacambira_EMATER.csv", na = "")

juramento = selecao_acao2[selecao_acao2$nome_munic=="JURAMENTO",]
juramento %>% filter(cod_parentesco_rf_pessoa == 1) %>%
  mutate(Endereço = paste(nom_tip_logradouro_fam, nom_titulo_logradouro_fam, 
                          nom_logradouro_fam, num_logradouro_fam, 
                          des_complemento_fam, des_complemento_adic_fam,
                          num_cep_logradouro_fam)) %>%
  mutate(n = 1:nrow(.), Nome = nom_pessoa, Apelido = nom_apelido_pessoa,
         NIS = num_nis_pessoa_atual, 
         CPF = num_cpf_pessoa, Endereço = Endereço,
         # Parte da AS
         Dom_acessa_CRAS = NA, Apto_AS = NA, 
         # Parte da Emater
         Renda = NA, DAP = NA, Apto_EMATER = NA, Objetivo = NA, Data = NA) %>%
  select_("n","Nome","Apelido","NIS","CPF","Endereço", "Dom_acessa_CRAS",
          "Apto_AS","Renda","DAP", "Apto_EMATER","Objetivo","Data") %>%
  write_excel_csv(., "lista_juramento_EMATER.csv", na = "")

luislandia = selecao_acao2[selecao_acao2$nome_munic=="LUISLÂNDIA",]
luislandia %>% filter(cod_parentesco_rf_pessoa == 1) %>%
  mutate(Endereço = paste(nom_tip_logradouro_fam, nom_titulo_logradouro_fam, 
                          nom_logradouro_fam, num_logradouro_fam, 
                          des_complemento_fam, des_complemento_adic_fam,
                          num_cep_logradouro_fam)) %>%
  mutate(n = 1:nrow(.), Nome = nom_pessoa, Apelido = nom_apelido_pessoa,
         NIS = num_nis_pessoa_atual, 
         CPF = num_cpf_pessoa, Endereço = Endereço,
         # Parte da AS
         Dom_acessa_CRAS = NA, Apto_AS = NA, 
         # Parte da Emater
         Renda = NA, DAP = NA, Apto_EMATER = NA, Objetivo = NA, Data = NA) %>%
  select_("n","Nome","Apelido","NIS","CPF","Endereço", "Dom_acessa_CRAS",
          "Apto_AS","Renda","DAP", "Apto_EMATER","Objetivo","Data") %>%
  write_excel_csv(., "lista_luislandia_EMATER.csv", na = "")

mirabela = selecao_acao2[selecao_acao2$nome_munic=="MIRABELA",]
mirabela %>% filter(cod_parentesco_rf_pessoa == 1) %>%
  mutate(Endereço = paste(nom_tip_logradouro_fam, nom_titulo_logradouro_fam, 
                          nom_logradouro_fam, num_logradouro_fam, 
                          des_complemento_fam, des_complemento_adic_fam,
                          num_cep_logradouro_fam)) %>%
  mutate(n = 1:nrow(.), Nome = nom_pessoa, Apelido = nom_apelido_pessoa,
         NIS = num_nis_pessoa_atual, 
         CPF = num_cpf_pessoa, Endereço = Endereço,
         # Parte da AS
         Dom_acessa_CRAS = NA, Apto_AS = NA, 
         # Parte da Emater
         Renda = NA, DAP = NA, Apto_EMATER = NA, Objetivo = NA, Data = NA) %>%
  select_("n","Nome","Apelido","NIS","CPF","Endereço", "Dom_acessa_CRAS",
          "Apto_AS","Renda","DAP", "Apto_EMATER","Objetivo","Data") %>%
  write_excel_csv(., "lista_mirabela_EMATER.csv", na = "")

sao_joao_da_lagoa = selecao_acao2[selecao_acao2$nome_munic=="SÃO JOÃO DA LAGOA",]
sao_joao_da_lagoa %>% filter(cod_parentesco_rf_pessoa == 1) %>%
  mutate(Endereço = paste(nom_tip_logradouro_fam, nom_titulo_logradouro_fam, 
                          nom_logradouro_fam, num_logradouro_fam, 
                          des_complemento_fam, des_complemento_adic_fam,
                          num_cep_logradouro_fam)) %>%
  mutate(n = 1:nrow(.), Nome = nom_pessoa, Apelido = nom_apelido_pessoa,
         NIS = num_nis_pessoa_atual, 
         CPF = num_cpf_pessoa, Endereço = Endereço,
         # Parte da AS
         Dom_acessa_CRAS = NA, Apto_AS = NA, 
         # Parte da Emater
         Renda = NA, DAP = NA, Apto_EMATER = NA, Objetivo = NA, Data = NA) %>%
  select_("n","Nome","Apelido","NIS","CPF","Endereço", "Dom_acessa_CRAS",
          "Apto_AS","Renda","DAP", "Apto_EMATER","Objetivo","Data") %>%
  write_excel_csv(., "lista_sao_joao_da_lagoa_EMATER.csv", na = "")

sao_joao_do_pacui = selecao_acao2[selecao_acao2$nome_munic=="SÃO JOÃO DO PACUÍ",]
sao_joao_do_pacui %>% filter(cod_parentesco_rf_pessoa == 1) %>%
  mutate(Endereço = paste(nom_tip_logradouro_fam, nom_titulo_logradouro_fam, 
                          nom_logradouro_fam, num_logradouro_fam, 
                          des_complemento_fam, des_complemento_adic_fam,
                          num_cep_logradouro_fam)) %>%
  mutate(n = 1:nrow(.), Nome = nom_pessoa, Apelido = nom_apelido_pessoa,
         NIS = num_nis_pessoa_atual, 
         CPF = num_cpf_pessoa, Endereço = Endereço,
         # Parte da AS
         Dom_acessa_CRAS = NA, Apto_AS = NA, 
         # Parte da Emater
         Renda = NA, DAP = NA, Apto_EMATER = NA, Objetivo = NA, Data = NA) %>%
  select_("n","Nome","Apelido","NIS","CPF","Endereço", "Dom_acessa_CRAS",
          "Apto_AS","Renda","DAP", "Apto_EMATER","Objetivo","Data") %>%
  write_excel_csv(., "lista_sao_joao_do_pacui_EMATER.csv", na = "")

josenopolis = selecao_acao2[selecao_acao2$nome_munic=="JOSENÓPOLIS",]
josenopolis %>% filter(cod_parentesco_rf_pessoa == 1) %>%
  mutate(Endereço = paste(nom_tip_logradouro_fam, nom_titulo_logradouro_fam, 
                          nom_logradouro_fam, num_logradouro_fam, 
                          des_complemento_fam, des_complemento_adic_fam,
                          num_cep_logradouro_fam)) %>%
  mutate(n = 1:nrow(.), Nome = nom_pessoa, Apelido = nom_apelido_pessoa,
         NIS = num_nis_pessoa_atual, 
         CPF = num_cpf_pessoa, Endereço = Endereço,
         # Parte da AS
         Dom_acessa_CRAS = NA, Apto_AS = NA, 
         # Parte da Emater
         Renda = NA, DAP = NA, Apto_EMATER = NA, Objetivo = NA, Data = NA) %>%
  select_("n","Nome","Apelido","NIS","CPF","Endereço", "Dom_acessa_CRAS",
          "Apto_AS","Renda","DAP", "Apto_EMATER","Objetivo","Data") %>%
  write_excel_csv(., "lista_josenopolis_EMATER.csv", na = "")

padre_carvalho = selecao_acao2[selecao_acao2$nome_munic=="PADRE CARVALHO",]
padre_carvalho %>% filter(cod_parentesco_rf_pessoa == 1) %>%
  mutate(Endereço = paste(nom_tip_logradouro_fam, nom_titulo_logradouro_fam, 
                          nom_logradouro_fam, num_logradouro_fam, 
                          des_complemento_fam, des_complemento_adic_fam,
                          num_cep_logradouro_fam)) %>%
  mutate(n = 1:nrow(.), Nome = nom_pessoa, Apelido = nom_apelido_pessoa,
         NIS = num_nis_pessoa_atual, 
         CPF = num_cpf_pessoa, Endereço = Endereço,
         # Parte da AS
         Dom_acessa_CRAS = NA, Apto_AS = NA, 
         # Parte da Emater
         Renda = NA, DAP = NA, Apto_EMATER = NA, Objetivo = NA, Data = NA) %>%
  select_("n","Nome","Apelido","NIS","CPF","Endereço", "Dom_acessa_CRAS",
          "Apto_AS","Renda","DAP", "Apto_EMATER","Objetivo","Data") %>%
  write_excel_csv(., "lista_padre_carvalho_EMATER.csv", na = "")

sao_joao_do_paraiso = selecao_acao2[selecao_acao2$nome_munic=="SÃO JOÃO DO PARAÍSO",]
sao_joao_do_paraiso %>% filter(cod_parentesco_rf_pessoa == 1) %>%
  mutate(Endereço = paste(nom_tip_logradouro_fam, nom_titulo_logradouro_fam, 
                          nom_logradouro_fam, num_logradouro_fam, 
                          des_complemento_fam, des_complemento_adic_fam,
                          num_cep_logradouro_fam)) %>%
  mutate(n = 1:nrow(.), Nome = nom_pessoa, Apelido = nom_apelido_pessoa,
         NIS = num_nis_pessoa_atual, 
         CPF = num_cpf_pessoa, Endereço = Endereço,
         # Parte da AS
         Dom_acessa_CRAS = NA, Apto_AS = NA, 
         # Parte da Emater
         Renda = NA, DAP = NA, Apto_EMATER = NA, Objetivo = NA, Data = NA) %>%
  select_("n","Nome","Apelido","NIS","CPF","Endereço", "Dom_acessa_CRAS",
          "Apto_AS","Renda","DAP", "Apto_EMATER","Objetivo","Data") %>%
  write_excel_csv(., "lista_sao_joao_do_paraiso_EMATER.csv", na = "")

vargem_grande_rio_pardo = selecao_acao2[selecao_acao2$nome_munic=="VARGEM GRANDE DO RIO PARDO",]
vargem_grande_rio_pardo %>% filter(cod_parentesco_rf_pessoa == 1) %>%
  mutate(Endereço = paste(nom_tip_logradouro_fam, nom_titulo_logradouro_fam, 
                          nom_logradouro_fam, num_logradouro_fam, 
                          des_complemento_fam, des_complemento_adic_fam,
                          num_cep_logradouro_fam)) %>%
  mutate(n = 1:nrow(.), Nome = nom_pessoa, Apelido = nom_apelido_pessoa,
         NIS = num_nis_pessoa_atual, 
         CPF = num_cpf_pessoa, Endereço = Endereço,
         # Parte da AS
         Dom_acessa_CRAS = NA, Apto_AS = NA, 
         # Parte da Emater
         Renda = NA, DAP = NA, Apto_EMATER = NA, Objetivo = NA, Data = NA) %>%
  select_("n","Nome","Apelido","NIS","CPF","Endereço", "Dom_acessa_CRAS",
          "Apto_AS","Renda","DAP", "Apto_EMATER","Objetivo","Data") %>%
  write_excel_csv(., "lista_vargem_grande_do_rio_pardo_EMATER.csv", na = "")


olhos_dagua = selecao_acao2[selecao_acao2$nome_munic=="OLHOS-D'ÁGUA",]
olhos_dagua %>% filter(cod_parentesco_rf_pessoa == 1) %>%
  mutate(Endereço = paste(nom_tip_logradouro_fam, nom_titulo_logradouro_fam, 
                          nom_logradouro_fam, num_logradouro_fam, 
                          des_complemento_fam, des_complemento_adic_fam,
                          num_cep_logradouro_fam)) %>%
  mutate(n = 1:nrow(.), Nome = nom_pessoa, Apelido = nom_apelido_pessoa,
         NIS = num_nis_pessoa_atual, 
         CPF = num_cpf_pessoa, Endereço = Endereço,
         # Parte da AS
         Dom_acessa_CRAS = NA, Apto_AS = NA, 
         # Parte da Emater
         Renda = NA, DAP = NA, Apto_EMATER = NA, Objetivo = NA, Data = NA) %>%
  select_("n","Nome","Apelido","NIS","CPF","Endereço", "Dom_acessa_CRAS",
          "Apto_AS","Renda","DAP", "Apto_EMATER","Objetivo","Data") %>%
  write_excel_csv(., "lista_olhos_dagua_EMATER.csv", na = "")


#Itacambira e Olhos D'água não tem ninguém!

sre_montes_claros = rbind(montes_claros, campo_azul, japonvar, lontra, patis, sjdponte,
                          bocaiuva, botumirim, brasilia_de_minas, capitao_eneas, 
                          claro_dos_pocoes, coracao_de_jesus, cristalia, engenheiro_navarro,
                          francisco_dumont, francisco_sa, glaucilandia, grao_mogol,
                          guaraciama, itacambira, juramento, luislandia, mirabela, 
                          sao_joao_da_lagoa, sao_joao_do_pacui, josenopolis,
                          padre_carvalho, sao_joao_do_paraiso, vargem_grande_rio_pardo,
                          olhos_dagua)

#write_csv(sre_montes_claros, "selecionados_sementes_presentes_sre_montes_claros.csv")



#####################################
#Testes
almenara %>% filter(cod_parentesco_rf_pessoa == 1) %>%
  mutate(Endereço = paste(nom_tip_logradouro_fam, nom_titulo_logradouro_fam, 
                          nom_logradouro_fam, num_logradouro_fam, 
                          des_complemento_fam, des_complemento_adic_fam,
                          num_cep_logradouro_fam)) %>%
  mutate(n = 1:nrow(.), Nome = nom_pessoa, Apelido = nom_apelido_pessoa,
         NIS = num_nis_pessoa_atual, 
         CPF = num_cpf_pessoa, Endereço = Endereço,
         # Parte da AS
         Dom_acessa_CRAS = NA, Apto_AS = NA, 
         # Parte da Emater
         Renda = NA, DAP = NA, Apto_EMATER = NA, Objetivo = NA, Data = NA) %>%
  select_("n","Nome","Apelido","NIS","CPF","Endereço", "Dom_acessa_CRAS",
          "Apto_AS","Renda","DAP", "Apto_EMATER","Objetivo","Data") %>%
  write_excel_csv(., "lista_almenara_teste.csv", na = "")
  
