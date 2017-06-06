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

#####################################
# Lendo os dados
CADUNICO <- fread('CADDOM.csv')
gc()
dic <- fread('dicionariodomicilio.csv')
View(dic)
##########################################

#com_tradicionais[com_tradicionais == 0] = NA
#freq(com_tradicionais)

freq( CADUNICO$cod_agua_canalizada_fam[CADUNICO$cod_indigena_reside_fam == 1] ) 
freq( CADUNICO$cod_agua_canalizada_fam[CADUNICO$ind_familia_quilombola_fam == 1] )
table( CADUNICO$cod_agua_canalizada_fam, CADUNICO$ind_parc_mds_fam )

freq( CADUNICO$cod_abaste_agua_domic_fam[CADUNICO$cod_indigena_reside_fam == 1] ) 
freq( CADUNICO$cod_abaste_agua_domic_fam[CADUNICO$ind_familia_quilombola_fam == 1] )
table( CADUNICO$cod_abaste_agua_domic_fam, CADUNICO$ind_parc_mds_fam )

##########################################################################
#Verificar concentração de comunidades tracionais sem água por município

#Separando os banco de dados por comunidades tradicionais
comunidades_tradicionais = CADUNICO[CADUNICO$cod_indigena_reside_fam == 1 |
                                      CADUNICO$ind_familia_quilombola_fam == 1 |
                                      CADUNICO$ind_parc_mds_fam != 0, ]

#Verificando a concentração espacial de domicílios de comunidades tradicionais
#sem água
freq(comunidades_tradicionais$cd_ibge[comunidades_tradicionais$cod_agua_canalizada_fam == 2])


