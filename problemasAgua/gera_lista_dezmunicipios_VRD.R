######################################################
### Seleção de domicílios para receber kit de sementes
### 10 municípios no Vale do Rio Doce
### RASTREIA
# SCRIPT: Neylson Crepalde
######################################################

#Municípios:
dezmunicipios = c("ALPERCATA", "DIVINO DAS LARANJEIRAS", "ENGENHEIRO CALDAS",
                  "FERNANDES TOURINHO", "ITUETA", "NACIP RAYDAN", 
                  "SÃO GERALDO DO BAIXIO", "SÃO JOÃO DO MANTENINHA",
                  "SÃO JOSÉ DA SAFIRA", "VIRGOLÂNDIA")

setwd("~/Documentos/CADUNICO")
source("analise_agua_comunidades_tradicionais.R")
# CADUNICO já vai sair apenas com domicílios RURAIS com meta para 2017

freq(CADUNICO$ano_meta, plot=F)
freq(CADUNICO$cod_local_domic_fam, plot=F)

################################################################################
#Verificando se as cidades estão no CADUNICO

sapply(dezmunicipios, function(x) x %in% CADUNICO$nome_munic) # Estão todos!

###############################################################################

#Criando o banco de dados para apenas as dez cidades selecionadas
bd_dez = CADUNICO[CADUNICO$nome_munic == dezmunicipios,]
bd_dez = bd_dez[is.na(bd_dez$nome_munic) == FALSE, ]
freq(bd_dez$nome_munic, plot=F)

###############################################################################

# Excluindo domicílios com renda superior a 1/2 salário mínimo
bd_dez = bd_dez[bd_dez$fx_rfpc != 4,]

