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
almenara = selecao_acao2[selecao_acao2$nome_munic=="ALMENARA",]
bandeira = selecao_acao2[selecao_acao2$nome_munic=="BANDEIRA",]
divisopolis = selecao_acao2[selecao_acao2$nome_munic=="DIVISÓPOLIS",]
felisburgo = selecao_acao2[selecao_acao2$nome_munic=="FELISBURGO",]
jacinto = selecao_acao2[selecao_acao2$nome_munic=="JACINTO",]
jequitinhonha = selecao_acao2[selecao_acao2$nome_munic=="JEQUITINHONHA",]
joaima = selecao_acao2[selecao_acao2$nome_munic=="JOAÍMA",]
jordania = selecao_acao2[selecao_acao2$nome_munic=="JORDÂNIA",]
monte_formoso = selecao_acao2[selecao_acao2$nome_munic=="MONTE FORMOSO",]
palmopolis = selecao_acao2[selecao_acao2$nome_munic=="PALMÓPOLIS",]
pedra_azul = selecao_acao2[selecao_acao2$nome_munic=="PEDRA AZUL",]
rio_do_prado = selecao_acao2[selecao_acao2$nome_munic=="RIO DO PRADO",]
rubim = selecao_acao2[selecao_acao2$nome_munic=="RUBIM",]
divisa = selecao_acao2[selecao_acao2$nome_munic=="SALTO DA DIVISA",]
salto = selecao_acao2[selecao_acao2$nome_munic=="SANTA MARIA DO SALTO",]
stantonio = selecao_acao2[selecao_acao2$nome_munic=="SANTO ANTÔNIO DO JACINTO",]
mata_verde = selecao_acao2[selecao_acao2$nome_munic=="MATA VERDE",]
aguas_vermelhas = selecao_acao2[selecao_acao2$nome_munic=="ÁGUAS VERMELHAS",]
cachoeira = selecao_acao2[selecao_acao2$nome_munic=="CACHOEIRA DE PAJEÚ",]
curral = selecao_acao2[selecao_acao2$nome_munic=="CURRAL DE DENTRO",]
divisa_alegre = selecao_acao2[selecao_acao2$nome_munic=="DIVISA ALEGRE",]

sre_almenara = rbind(almenara, bandeira, divisopolis, felisburgo, jacinto,
                     jequitinhonha, joaima, jordania, monte_formoso, palmopolis,
                     pedra_azul, rio_do_prado, rubim, divisa, salto, stantonio,
                     mata_verde, aguas_vermelhas, cachoeira, curral, divisa_alegre)

dim(sre_almenara)
#write_csv(sre_almenara, "selecionados_sementes_presentes_sre_almenara.csv")


### SRE Januária
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
montes_claros = selecao_acao2[selecao_acao2$nome_munic=="MONTES CLAROS",]
campo_azul = selecao_acao2[selecao_acao2$nome_munic=="CAMPO AZUL",]
japonvar = selecao_acao2[selecao_acao2$nome_munic=="JAPONVAR",]
lontra = selecao_acao2[selecao_acao2$nome_munic=="LONTRA",]
patis = selecao_acao2[selecao_acao2$nome_munic=="PATIS",]
sjdponte = selecao_acao2[selecao_acao2$nome_munic=="SÃO JOÃO DA PONTE",]
bocaiuva = selecao_acao2[selecao_acao2$nome_munic=="BOCAIÚVA",]
botumirim = selecao_acao2[selecao_acao2$nome_munic=="BOTUMIRIM",]
brasilia_de_minas = selecao_acao2[selecao_acao2$nome_munic=="BRASÍLIA DE MINAS",]
capitao_eneas = selecao_acao2[selecao_acao2$nome_munic=="CAPITÃO ENÉAS",]
claro_dos_pocoes = selecao_acao2[selecao_acao2$nome_munic=="CLARO DOS POÇÕES",]
coracao_de_jesus = selecao_acao2[selecao_acao2$nome_munic=="CORAÇÃO DE JESUS",]
cristalia = selecao_acao2[selecao_acao2$nome_munic=="CRISTÁLIA",]
engenheiro_navarro = selecao_acao2[selecao_acao2$nome_munic=="ENGENHEIRO NAVARRO",]
francisco_dumont = selecao_acao2[selecao_acao2$nome_munic=="FRANCISCO DUMONT",]
francisco_sa = selecao_acao2[selecao_acao2$nome_munic=="FRANCISCO SÁ",]
glaucilandia = selecao_acao2[selecao_acao2$nome_munic=="GLAUCILÂNDIA",]
grao_mogol = selecao_acao2[selecao_acao2$nome_munic=="GRÃO MOGOL",]
guaraciama = selecao_acao2[selecao_acao2$nome_munic=="GUARACIAMA",]
itacambira = selecao_acao2[selecao_acao2$nome_munic=="ITABAMBIRA",]
juramento = selecao_acao2[selecao_acao2$nome_munic=="JURAMENTO",]
luislandia = selecao_acao2[selecao_acao2$nome_munic=="LUISLÂNDIA",]
mirabela = selecao_acao2[selecao_acao2$nome_munic=="MIRABELA",]
sao_joao_da_lagoa = selecao_acao2[selecao_acao2$nome_munic=="SÃO JOÃO DA LAGOA",]
sao_joao_do_pacui = selecao_acao2[selecao_acao2$nome_munic=="SÃO JOÃO DO PACUÍ",]
josenopolis = selecao_acao2[selecao_acao2$nome_munic=="JOSENÓPOLIS",]
padre_carvalho = selecao_acao2[selecao_acao2$nome_munic=="PADRE CARVALHO",]
sao_joao_do_paraiso = selecao_acao2[selecao_acao2$nome_munic=="SÃO JOÃO DO PARAÍSO",]
vargem_grande_rio_pardo = selecao_acao2[selecao_acao2$nome_munic=="VARGEM GRANDE DO RIO PARDO",]
olhos_dagua = selecao_acao2[selecao_acao2$nome_munic=="OLHOS-D'ÁGUA",]

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
                          num_cep_logradouro_fam, sep=", ")) %>%
  mutate(n = 1:nrow(.), Nome = nom_pessoa, NIS = num_nis_pessoa_atual, 
         CPF = num_cpf_pessoa, Endereço = Endereço) %>%
  select_("n","Nome","NIS","CPF","Endereço") %>%
  write_excel_csv(., "lista_almenara_teste.csv", na = "")
  
