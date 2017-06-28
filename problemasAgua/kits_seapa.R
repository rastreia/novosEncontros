# Analise de problemas com água
# Comunidades Tradicionais
# KITS SEAPA
##############################

#Refazer
#Olhar pela variável 33 - cod_abaste_agua
#Separar aqueles que tem até cistena (até 3)


setwd("~/Documentos/Neylson Crepalde/RASTREIA/CADUNICO")
source("~/Documentos/Neylson Crepalde/RASTREIA/novosEncontros/problemasAgua/analise_agua_comunidades_tradicionais.R")

#Demandas posteriores ---------------
#######################
# Subset Vale do Rio Doce - KITS SEAPA

comunidades_vale = comunidades_tradicionais[comunidades_tradicionais$nome_regiao == "Vale do Rio Doce",]
freq(comunidades_vale$cod_indigena_reside_fam,plot=F)
freq(comunidades_vale$ind_familia_quilombola_fam,plot=F)

#Separando as comunidades tradicionais que interessam, a saber:
# 203 - família de terreiro - NAO TEM!
# 205 - família de agricultores familiares
# 301 - Família assentada da reforma agraria
# 303 - Família acampada # NÃO PODE
# 
comunidades_vale$ind_parc_mds_fam[comunidades_vale$ind_parc_mds_fam == 0] = NA
comunidades_vale$ind_parc_mds_fam[comunidades_vale$ind_parc_mds_fam != 203 & 
                                    comunidades_vale$ind_parc_mds_fam != 205 &
                                    comunidades_vale$ind_parc_mds_fam != 301] = NA

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
43 + 40 + 258
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

