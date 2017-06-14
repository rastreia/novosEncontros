#Separando mulheres rurais sem documento no CADUNICO
# Neylson Crepalde
###############################

freq(CADPES$cod_sexo_pessoa, plot=F)
mulheres = CADPES[CADPES$cod_sexo_pessoa == 2]
rm(CADPES); gc()

freq(mulheres$cod_sexo_pessoa, plot = F) #Apenas mulheres

#Fazendo o join
mulheres <- left_join(mulheres, CADUNICO, by='cod_familiar_fam')

#Separando mulheres que vivem em domicílios rurais
mulheres = mulheres[mulheres$cod_local_domic_fam == 2,]

names(mulheres)
#Variáveis de interesse
# 6  num_nis_pessoa_atual
# 20 cod_certidao_registrada_pessoa
# 21 fx_idade
# 34 num_cpf_pessoa
# 35 num_identidade_pessoa
# 40 num_cart_trab_prev_soc_pessoa
# 44 num_titulo_eleitor_pessoa

freq(mulheres[,20], plot=F)
#Mulheres sem certidão de nascimento:
42070 + 73
# 42143
# Não consegui achar a codificação da faixa de idade!!!!!!!!
##################

#Concentração de municípios
mulheres[mulheres[,20] == 2 | mulheres[,20] == 3,] %>% count(nome_munic) %>%
  arrange(desc(n)) %>% print(.)

#Mulheres sem CPF
summary(mulheres[,34])

#Mulheres sem Certidão de Nascimento
class(mulheres[[35]])

contagem = 0
for (i in 1:nrow(mulheres)){
  if (is.na(mulheres[[35]][i])){
    contagem = contagem+1
  }
}
print(contagem)
# :)

#Mulheres sem Carteira de Trabalho
class(mulheres[,40])
summary(mulheres[,40])

#Mulheres sem título de eleitor
class(mulheres[,44])
summary(mulheres[,44])

