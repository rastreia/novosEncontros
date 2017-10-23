### Critérios para seleção das famílias e comunidades - CSFC
### Digite Ctr+f e busque CSFC

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
library(lme4)
library(merTools)
library(lmtest)
library(texreg)
library(xlsx)
library(readr)

### Lendo e construindo o Banco de Dados

### CSFC 1) Selecionar no CadÚnico apenas os municípios que estão na estratégia (229
## municípios pertencentes às regiões Norte, Alto Jequitinhonha, Baixo e Médio
## Jequitinhonha, Mucuri e Vale do Rio Doce do Estado de Minas Gerais).
## CADDOM.csv já apresenta estes filtros

CADUNICO <- fread('CADDOM.csv')
dic <- fread('dicionariodomicilio.csv')
CADPES <- fread('CADPES.csv')

metas <- read.csv2('selecao_publico_cadunico.csv', stringsAsFactors = F, header=F, encoding = 'UTF-8', sep = ",")
names(metas) <- c('cd_ibge','nome_munic','nome_regiao','ano_meta')

CADUNICO <- left_join(CADUNICO, CADPES) # faz o merge
CADUNICO <- left_join(CADUNICO, metas)

ranking_munic = fread("intercepto_aleatorio.csv")
CADUNICO <- left_join(CADUNICO, ranking_munic)

### CSFC 2) Selecionar no CadÚnico municípios com previsão de atendimento pelo projeto
## Sementes Presentes em 2017 (159 municípios).

CADUNICO <- CADUNICO[CADUNICO$ano_meta == 2017,]   # Apenas meta para 2017


### CSFC 4) Selecionar apenas domicílios classificados como rurais na base do CADUNICO.

CADUNICO <- CADUNICO[CADUNICO$cod_local_domic_fam == 2,]

### CSFC 5) Famílias com renda mensal per capita de até meio salário mínimo.

CADUNICO <- CADUNICO[CADUNICO$fx_rfpc != 4,]

### CSFC 6) Disponibilidade de água (incluir abastecimento por poço e cisterna)

bd <- CADUNICO
CADUNICO <- bd

CADUNICO = CADUNICO[is.na(CADUNICO$nome_munic) == FALSE, ] # Apenas limpando quem é missing em nome_munic

CADUNICO <- CADUNICO[CADUNICO$cod_agua_canalizada_fam == 1 || CADUNICO$cod_abaste_agua_domic_fam == 2 || CADUNICO$cod_abaste_agua_domic_fam == 3,]

CADUNICO <- CADUNICO[CADUNICO$cod_parentesco_rf_pessoa == 1,]

CADUNICO = CADUNICO[is.na(CADUNICO$nome_munic) == FALSE, ] # Apenas limpando quem é missing em nome_munic

## CSFC 3) Da lista de 159 municípios selecionou-se apenas os municípios contemplados
## no convênio Irriga Minas (29 municípios).

listaMunicipios1 = c("AIMORÉS", "ITABIRINHA", "MANTENA", "RESPLENDOR", "GOVERNADOR VALADARES", "SANTA RITA DO ITUETO", "MENDES PIMENTEL", "NOVA BELÉM", "CONSELHEIRO PENA", "FREI INOCÊNCIO", "SARDOÁ", "COROACI", "TUMIRITINGA", "SÃO GERALDO DA PIEDADE", "SÃO FÉLIX DE MINAS", "CUPARAQUE", "CENTRAL DE MINAS", "ITANHOMI", "CAPITÃO ANDRADE", "GALILÉIA")
    
listaMunicipios2 = c("CRISÓLITA", "BANDEIRA", "FELISBURGO", "JEQUITINHONHA", "JOAÍMA", "PADRE PARAÍSO", "PEDRA AZUL", "PONTO DOS VOLANTES", "RUBIM" )

setwd("~/Documentos/CADUNICO/teste")

geraDados <- function(municipio, CADUNICO){
  print (municipio)
  caminho <- paste("csv/", municipio, ".csv", collapse = "")
  caminho2 <- paste("xlsx/", municipio, ".xlsx", collapse = "")
  
  subSetMunicipio <- CADUNICO[CADUNICO$nome_munic == municipio, ] %>%
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
    write_excel_csv(., caminho, na = "") %>% write.xlsx(., caminho2, sheetName="Sheet1", showNA = F)

}


for (i in listaMunicipios1){
  print(i)
  geraDados(i, CADUNICO)
}

for (i in listaMunicipios2){
  print(i)
  geraDados(i, CADUNICO)
}

#length(unique(comunidades_vale$nome_munic))

