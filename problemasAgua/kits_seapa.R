# Seleção Domicílios kits irrigação
# KITS SEAPA
# Script: Neylson Crepalde
##############################

#Olhar pela variável 33 - cod_abaste_agua
#Separar aqueles que tem até cistena (até 3)


setwd("~/Documentos/CADUNICO")
source("analise_agua_comunidades_tradicionais.R")

#Demandas posteriores ---------------
#######################
# Subset Vale do Rio Doce - KITS SEAPA

# TRABALHAR COM CADUNICO do jeito que sai do script source
# Apenas domicílios rurais em municípios com meta para 2017



comunidades_vale = comunidades_tradicionais[comunidades_tradicionais$nome_regiao == "Vale do Rio Doce",]
freq(comunidades_vale$cod_indigena_reside_fam,plot=F)
freq(comunidades_vale$ind_familia_quilombola_fam,plot=F)

#Separando as comunidades tradicionais que interessam, a saber:
# 203 - família de terreiro - NAO TEM!
# 205 - família de agricultores familiares
# 301 - Família assentada da reforma agraria
# 303 - Família acampada
# 
comunidades_vale$ind_parc_mds_fam[comunidades_vale$ind_parc_mds_fam == 0] = NA
comunidades_vale$ind_parc_mds_fam[comunidades_vale$ind_parc_mds_fam != 203 & 
                                    comunidades_vale$ind_parc_mds_fam != 205 &
                                    comunidades_vale$ind_parc_mds_fam != 301 &
                                    comunidades_vale$ind_parc_mds_fam != 303] = NA

freq(comunidades_vale$ind_parc_mds_fam)

#Total de atendidos
atendidos = sum(comunidades_vale$qtd_pessoas_domic_fam, na.rm = T); print(atendidos)

#Total de atendidos por nome de comunidade indígena
comunidades_vale %>% group_by(nom_povo_indigena_fam) %>%
  summarize(total = sum(qtd_pessoas_domic_fam))

#Total de atendidos por comunidade quilombola
comunidades_vale %>% group_by(nom_comunidade_quilombola_fam) %>%
  summarize(total = sum(qtd_pessoas_domic_fam))

#Total de atendidos por famílias tradicionais
comunidades_vale %>% group_by(ind_parc_mds_fam) %>%
  summarize(total = sum(qtd_pessoas_domic_fam))

#####################################
#Verificação de acesso à água por comunidade
#Índios
separado_ind_n_vale = comunidades_vale %>% group_by(nom_povo_indigena_fam) %>%
  summarise(n = n())
separado_ind_div_vale = comunidades_vale %>% group_by(nom_povo_indigena_fam) %>%
  count(cod_agua_canalizada_fam) %>% na.omit
names(separado_ind_div_vale)[3] = "count"

separado_ind_per_vale = left_join(separado_ind_div_vale, separado_ind_n_vale) %>% 
  mutate(per = round(count/n, 2))
print(separado_ind_per_vale[,-4])

#Prioridade: KRENAK
#Total com água = 43

#Quilombolas
separado_qui_n_vale = comunidades_vale %>% group_by(nom_comunidade_quilombola_fam) %>%
  summarise(n = n())
separado_qui_div_vale = comunidades_vale %>% group_by(nom_comunidade_quilombola_fam) %>%
  count(cod_agua_canalizada_fam) %>% na.omit
names(separado_qui_div_vale)[3] = "count"

separado_qui_per_vale = left_join(separado_qui_div_vale, separado_qui_n_vale) %>% 
  mutate(per = round(count/n, 2)) %>% filter(cod_agua_canalizada_fam == 1) %>%
  arrange(desc(per))
print(separado_qui_per_vale[,-4])


#Prioridade SAO DOMINGOS, SAO FELIX, SESMARIA, QUILOMBO, SANTA BARBARA E BARRA,
#CORREGO MESTRE
#Total (soma de todos): 50
sum(separado_qui_per_vale$count[-3])

# Assentados
separado_ass_n_vale = comunidades_vale %>% group_by(ind_parc_mds_fam) %>%
  summarise(n = n())
separado_ass_div_vale = comunidades_vale %>% group_by(ind_parc_mds_fam) %>%
  count(cod_agua_canalizada_fam) %>% na.omit
names(separado_ass_div_vale)[3] = "count"

separado_ass_per_vale = left_join(separado_ass_div_vale, separado_ass_n_vale) %>% 
  mutate(per = round(count/n, 2)) %>% filter(cod_agua_canalizada_fam == 1) %>%
  arrange(desc(per))
print(separado_ass_per_vale[,-4])

###################
#Total (301 + 205 + 303) = 139
sum(separado_ass_per_vale$count)

################################
#TOTAL de selecionados = 222!!!
43 + 40 + 139
################################


#Separando por município
comunidades_vale %>% group_by(nome_munic) %>% count(nom_povo_indigena_fam) %>% 
  filter(nom_povo_indigena_fam != "")
comunidades_vale %>% group_by(nome_munic) %>% filter(cod_agua_canalizada_fam == 1) %>% 
  count(nom_comunidade_quilombola_fam) %>% filter(nom_comunidade_quilombola_fam != "")

comunidades_vale %>% group_by(nome_munic) %>% filter(cod_agua_canalizada_fam == 1) %>% 
  count(ind_parc_mds_fam) %>% na.omit %>% arrange(desc(n)) %>% View

#Ranking dos municípios em situação crítica
separado_qui_n_vale = comunidades_vale %>% group_by(nome_munic) %>%
  summarise(n = n())
separado_qui_div_vale = comunidades_vale %>% group_by(nome_munic) %>%
  count(cod_agua_canalizada_fam) %>% na.omit
names(separado_qui_div_vale)[3] = "count"

left_join(separado_qui_div_vale, separado_qui_n_vale) %>% 
  mutate(per = round(count/n, 2)) %>% filter(cod_agua_canalizada_fam == 1) %>%
  arrange(desc(count)) %>% View


#Municípios prioritários:
# SENHORA DO PORTO, SABINÓPOLIS, SÃO JOSÉ DA SAFIRA, TUMIRITINGA, RESPLENDOR, 
# JAMPRUCA e AGUA BOA

###################################################################
###################################################################
# Excluindo quem recebe mais de 1/2 salário mínimo mensal per capita
CADUNICO = CADUNICO[CADUNICO$fx_rfpc != 4,]

#Incluindo apenas domicílios que possuem disponibilidade de água
# considerando poço e cisterna
CADUNICO = CADUNICO[CADUNICO$cod_abaste_agua_domic_fam != 4,]

#Selecionando as categorias de comunidades tradicionais selecionadas
#Assentados e agricultores familiares
CADUNICO$ind_parc_mds_fam[CADUNICO$ind_parc_mds_fam == 101] = NA
CADUNICO$ind_parc_mds_fam[CADUNICO$ind_parc_mds_fam == 201] = NA
CADUNICO$ind_parc_mds_fam[CADUNICO$ind_parc_mds_fam == 202] = NA
CADUNICO$ind_parc_mds_fam[CADUNICO$ind_parc_mds_fam == 203] = NA
CADUNICO$ind_parc_mds_fam[CADUNICO$ind_parc_mds_fam == 204] = NA
CADUNICO$ind_parc_mds_fam[CADUNICO$ind_parc_mds_fam == 302] = NA
CADUNICO$ind_parc_mds_fam[CADUNICO$ind_parc_mds_fam == 303] = NA
CADUNICO$ind_parc_mds_fam[CADUNICO$ind_parc_mds_fam == 304] = NA
CADUNICO$ind_parc_mds_fam[CADUNICO$ind_parc_mds_fam == 305] = NA
CADUNICO$ind_parc_mds_fam[CADUNICO$ind_parc_mds_fam == 306] = NA
CADUNICO$ind_parc_mds_fam[CADUNICO$ind_parc_mds_fam == 0] = NA
freq(CADUNICO$ind_parc_mds_fam, plot=F)

# Ordenando o banco
CADUNICO = arrange(CADUNICO, ind_familia_quilombola_fam, cod_familia_indigena_fam,
                   ind_parc_mds_fam, fx_rfpc)
View(CADUNICO)

#Separando apenas chefes de domiciílio no CADPES
CADPES <- fread('CADPES.csv')
CADPES = CADPES[CADPES$cod_parentesco_rf_pessoa == 1,]
gc()

#Juntando com pessoas

CADUNICO = left_join(CADUNICO, CADPES, by="cod_familiar_fam")
gc()

##########################
# Extraindo as listas de municípios
library(readr)
setwd("~/Documentos/CADUNICO/kits_irrigacao")
#aimores = CADUNICO[CADUNICO$nome_munic == "AIMORÉS",]
CADUNICO[is.na(CADUNICO$nome_munic) == F,] %>% filter(nome_munic == "AIMORÉS") %>%
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
  write_excel_csv(., "lista_aimores_EMATER.csv", na = "")

###
#itabirinha = comunidades_vale[comunidades_vale$nome_munic == "ITABIRINHA",]
CADUNICO[is.na(CADUNICO$nome_munic) == F,] %>% filter(nome_munic == "ITABIRINHA") %>%
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
  write_excel_csv(., "lista_itabirinha_EMATER.csv", na = "")
###
#mantena = comunidades_vale[comunidades_vale$nome_munic == "MANTENA",]
CADUNICO[is.na(CADUNICO$nome_munic) == F,] %>% filter(nome_munic == "MANTENA") %>%
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
  write_excel_csv(., "lista_mantena_EMATER.csv", na = "")
###
#resplendor = comunidades_vale[comunidades_vale$nome_munic == "RESPLENDOR",]
CADUNICO[is.na(CADUNICO$nome_munic) == F,] %>% filter(nome_munic == "RESPLENDOR") %>%
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
  write_excel_csv(., "lista_resplendor_EMATER.csv", na = "")
###
#gvaladares = comunidades_vale[comunidades_vale$nome_munic == "GOVERNADOR VALADARES",]
CADUNICO[is.na(CADUNICO$nome_munic) == F,] %>% filter(nome_munic == "GOVERNADOR VALADARES") %>%
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
  write_excel_csv(., "lista_governador_valadares_EMATER.csv", na = "")
###
#itueto = comunidades_vale[comunidades_vale$nome_munic == "SANTA RITA DO ITUETO",]
CADUNICO[is.na(CADUNICO$nome_munic) == F,] %>% filter(nome_munic == "SANTA RITA DO ITUETO") %>%
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
  write_excel_csv(., "lista_santa_rita_do_itueto_EMATER.csv", na = "")
###
#mendes_pimentel = comunidades_vale[comunidades_vale$nome_munic == "MENDES PIMENTEL",]
CADUNICO[is.na(CADUNICO$nome_munic) == F,] %>% filter(nome_munic == "MENDES PIMENTEL") %>%
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
  write_excel_csv(., "lista_mendes_pimentel_EMATER.csv", na = "")
###
#nbelem = comunidades_vale[comunidades_vale$nome_munic == "NOVA BELÉM",] não tem quilombolas e indígenas!!!
CADUNICO[is.na(CADUNICO$nome_munic) == F,] %>% filter(nome_munic == "NOVA BELÉM") %>%
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
  write_excel_csv(., "lista_nova_belem_EMATER.csv", na = "")
###
#cpena = comunidades_vale[comunidades_vale$nome_munic == "CONSELHEIRO PENA",]
CADUNICO[is.na(CADUNICO$nome_munic) == F,] %>% filter(nome_munic == "CONSELHEIRO PENA") %>%
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
  write_excel_csv(., "lista_conselheiro_pena_EMATER.csv", na = "")
###
#frei_inocencio = comunidades_vale[comunidades_vale$nome_munic == "FREI INOCÊNCIO",]
CADUNICO[is.na(CADUNICO$nome_munic) == F,] %>% filter(nome_munic == "FREI INOCÊNCIO") %>%
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
  write_excel_csv(., "lista_frei_inocencio_EMATER.csv", na = "")
###
#sardoa = comunidades_vale[comunidades_vale$nome_munic == "SARDOÁ",] não tem""
CADUNICO[is.na(CADUNICO$nome_munic) == F,] %>% filter(nome_munic == "SARDOÁ") %>%
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
  write_excel_csv(., "lista_sardoa_EMATER.csv", na = "")
###
#coroaci = comunidades_vale[comunidades_vale$nome_munic == "COROACI",]
CADUNICO[is.na(CADUNICO$nome_munic) == F,] %>% filter(nome_munic == "COROACI") %>%
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
  write_excel_csv(., "lista_coroaci_EMATER.csv", na = "")
###
#tumiritinga = comunidades_vale[comunidades_vale$nome_munic == "TUMIRITINGA",]
CADUNICO[is.na(CADUNICO$nome_munic) == F,] %>% filter(nome_munic == "TUMIRITINGA") %>%
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
  write_excel_csv(., "lista_tumiritinga_EMATER.csv", na = "")
###
#geraldo = comunidades_vale[comunidades_vale$nome_munic == "SÃO GERALDO DA PIEDADE",]
CADUNICO[is.na(CADUNICO$nome_munic) == F,] %>% filter(nome_munic == "SÃO GERALDO DA PIEDADE") %>%
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
  write_excel_csv(., "lista_sao_geraldo_da_piedade_EMATER.csv", na = "")
###
#felix = comunidades_vale[comunidades_vale$nome_munic == "SÃO FÉLIX DE MINAS",] não tem!!!
CADUNICO[is.na(CADUNICO$nome_munic) == F,] %>% filter(nome_munic == "SÃO FÉLIX DE MINAS") %>%
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
  write_excel_csv(., "lista_sao_felix_de_minas_EMATER.csv", na = "")
###
#cuparaque = comunidades_vale[comunidades_vale$nome_munic == "CUPARAQUE",] não tem!!
CADUNICO[is.na(CADUNICO$nome_munic) == F,] %>% filter(nome_munic == "CUPARAQUE") %>%
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
  write_excel_csv(., "lista_cuparaque_EMATER.csv", na = "")
###
#central = comunidades_vale[comunidades_vale$nome_munic == "CENTRAL DE MINAS",]
CADUNICO[is.na(CADUNICO$nome_munic) == F,] %>% filter(nome_munic == "CENTRAL DE MINAS") %>%
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
  write_excel_csv(., "lista_central_de_minas_EMATER.csv", na = "")
###
#itanhomi = comunidades_vale[comunidades_vale$nome_munic == "ITANHOMI",]
CADUNICO[is.na(CADUNICO$nome_munic) == F,] %>% filter(nome_munic == "ITANHOMI") %>%
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
  write_excel_csv(., "lista_itanhomi_EMATER.csv", na = "")
###
#cap_andrade = comunidades_vale[comunidades_vale$nome_munic == "CAPITÃO ANDRADE",] não tem!!!
CADUNICO[is.na(CADUNICO$nome_munic) == F,] %>% filter(nome_munic == "CAPITÃO ANDRADE") %>%
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
  write_excel_csv(., "lista_capitao_andrade_EMATER.csv", na = "")
###
#galileia = comunidades_vale[comunidades_vale$nome_munic == "GALILÉIA",] não tem
CADUNICO[is.na(CADUNICO$nome_munic) == F,] %>% filter(nome_munic == "GALILÉIA") %>%
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
  write_excel_csv(., "lista_galileia_EMATER.csv", na = "")
###


