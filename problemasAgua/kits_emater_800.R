# Analise de problemas com água
# Comunidades Tradicionais
# KITS SEAPA
##############################

setwd("~/Documentos/CADUNICO")
source(analise_agua_comunidades_tradicionais.R)

comunidades_exceto_vale = comunidades_tradicionais[comunidades_tradicionais$nome_regiao != "Vale do Rio Doce",]
freq(comunidades_exceto_vale$cod_indigena_reside_fam)
freq(comunidades_exceto_vale$ind_familia_quilombola_fam)

#Separando as comunidades tradicionais que interessam, a saber:
# 203 - família de terreiro
# 205 - família de agricultores familiares
# 301 - Família assentada da reforma agraria
# 303 - Família acampada
# 
comunidades_exceto_vale$ind_parc_mds_fam[comunidades_exceto_vale$ind_parc_mds_fam == 0] = NA
comunidades_exceto_vale$ind_parc_mds_fam[comunidades_exceto_vale$ind_parc_mds_fam != 203 & 
                                    comunidades_exceto_vale$ind_parc_mds_fam != 205 &
                                    comunidades_exceto_vale$ind_parc_mds_fam != 301 &
                                    comunidades_exceto_vale$ind_parc_mds_fam != 303] = NA

freq(comunidades_exceto_vale$ind_parc_mds_fam)

#Total de atendidos
atendidos = sum(comunidades_exceto_vale$qtd_pessoas_domic_fam, na.rm = T); print(atendidos)

#Total de atendidos por nome de comunidade indígena
comunidades_exceto_vale %>% group_by(nom_povo_indigena_fam) %>%
  summarize(total = sum(qtd_pessoas_domic_fam))

#Total de atendidos por comunidade quilombola
comunidades_exceto_vale %>% group_by(nom_comunidade_quilombola_fam) %>%
  summarize(total = sum(qtd_pessoas_domic_fam))

#Total de atendidos por famílias tradicionais
comunidades_exceto_vale %>% group_by(ind_parc_mds_fam) %>%
  summarize(total = sum(qtd_pessoas_domic_fam))

#####################################
#Verificação de acesso à água por comunidade
#Índios
separado_ind_n_naovale = comunidades_exceto_vale %>% group_by(nom_povo_indigena_fam) %>%
  summarise(n = n())
separado_ind_div_naovale = comunidades_exceto_vale %>% group_by(nom_povo_indigena_fam) %>%
  count(cod_agua_canalizada_fam) %>% na.omit
names(separado_ind_div_naovale)[3] = "count"

separado_ind_per_naovale = left_join(separado_ind_div_naovale, separado_ind_n_naovale) %>% 
  mutate(per = round(count/n, 2)) %>% filter(cod_agua_canalizada_fam == "Não") %>%
  arrange(desc(per))
separado_ind_per_naovale = separado_ind_per_naovale[separado_ind_per_naovale$nom_povo_indigena_fam != "",]
print(separado_ind_per_naovale[,-4])

#Prioridade: AMANAYE, ARANA, MAXAKALI, AIKANA, WAYANA
#Total sem água (prioritários) = 318 <-----------
#Total sem água (todos) = 551
sum(separado_ind_per_naovale$count[1:4])

#Quilombolas
separado_qui_n_naovale = comunidades_exceto_vale %>% group_by(nom_comunidade_quilombola_fam) %>%
  summarise(n = n())
separado_qui_div_naovale = comunidades_exceto_vale %>% group_by(nom_comunidade_quilombola_fam) %>%
  count(cod_agua_canalizada_fam) %>% na.omit
names(separado_qui_div_naovale)[3] = "count"

separado_qui_per_naovale = left_join(separado_qui_div_naovale, separado_qui_n_naovale) %>% 
  mutate(per = round(count/n, 2)) %>% filter(cod_agua_canalizada_fam == "Não") %>%
  arrange(desc(per))
separado_qui_per_naovale = separado_qui_per_naovale[separado_qui_per_naovale$nom_comunidade_quilombola_fam !="",]
View(separado_qui_per_naovale[,-4])

# Recorte: acima de 60% [posicao 38]
# Total (Soma dos prioritários): 465
# Total (soma de todos): 1996
sum(separado_qui_per_naovale$count[1:38])

# Assentados
separado_ass_n_naovale = comunidades_exceto_vale %>% group_by(ind_parc_mds_fam) %>%
  summarise(n = n())
separado_ass_div_naovale = comunidades_exceto_vale %>% group_by(ind_parc_mds_fam) %>%
  count(cod_agua_canalizada_fam) %>% na.omit
names(separado_ass_div_naovale)[3] = "count"

separado_ass_per_naovale = left_join(separado_ass_div_naovale, separado_ass_n_naovale) %>% 
  mutate(per = round(count/n, 2)) %>% filter(cod_agua_canalizada_fam == "Não") %>%
  arrange(desc(per))
print(separado_ass_per_naovale[,-4])

#Prioritário: 301 - Família assentada de reforma agrária
#Total prioritários: 290
#Total (301 + 205 + 303) = 2626
sum(separado_ass_per_naovale$count)

################################
#TOTAL de selecionados = 194!!!
290 + 465 + 318
################################


#Separando por município
comunidades_exceto_vale %>% group_by(nome_munic) %>% count(nom_povo_indigena_fam) %>% 
  filter(nom_povo_indigena_fam != "")
comunidades_exceto_vale %>% group_by(nome_munic) %>% count(nom_comunidade_quilombola_fam) %>%
  filter(nom_comunidade_quilombola_fam != "")
comunidades_exceto_vale %>% group_by(nome_munic) %>% count(ind_parc_mds_fam) %>%
  na.omit %>% arrange(desc(n)) %>% View

#Ranking dos municípios em situação crítica
separado_qui_n_naovale = comunidades_exceto_vale %>% group_by(nome_munic) %>%
  summarise(n = n())
separado_qui_div_naovale = comunidades_exceto_vale %>% group_by(nome_munic) %>%
  count(cod_agua_canalizada_fam) %>% na.omit
names(separado_qui_div_naovale)[3] = "count"

left_join(separado_qui_div_naovale, separado_qui_n_naovale) %>% 
  mutate(per = round(count/n, 2)) %>% filter(cod_agua_canalizada_fam == "Não") %>%
  arrange(desc(per)) %>% View


#Municípios prioritários:
# Continua
