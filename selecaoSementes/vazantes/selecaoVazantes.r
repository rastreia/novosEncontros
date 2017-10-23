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

setwd("~/Documentos/BETA/CADUNICO")
#Cadunico Utilizado foi extraido via sigpbf/CECAD  ~final de setembro

CADDOMICILIO <- fread('domicilio.csv')
CADPES <- fread('pessoa.csv')
#CADPES <- read.csv('pessoa.csv', sep = ";")

CADUNICO <- left_join(CADDOMICILIO, CADPES)
CADUNICO <- left_join(CADPES, CADDOMICILIO)

ranking_munic = fread("intercepto_aleatorio.csv")
CADUNICO <- left_join(CADUNICO, ranking_munic)

### Selecionar apenas domicílios de Vazantes.

CADUNICO <- subset(CADUNICO, CADUNICO$cd_ibge == "3171006")

### Selecionar apenas domicílios classificados como rurais na base do CADUNICO.

CADUNICO <- CADUNICO[CADUNICO$cod_local_domic_fam == 2,]

### Famílias com renda mensal per capita de até meio salário mínimo.

CADUNICO <- CADUNICO[CADUNICO$fx_rfpc != 4,]

CADUNICO <- CADUNICO[CADUNICO$cod_parentesco_rf_pessoa == 1,]

write.csv(CADUNICO, "extracaoVazantes_somenteMerge.csv")



# > Rankeamento: Pedro Costa | Excel
