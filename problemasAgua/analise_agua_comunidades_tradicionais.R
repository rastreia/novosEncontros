# Analise de problemas com água
# Comunidades Tradicionais
# KITS SEAPA
##############################

setwd("~/Documentos/CADUNICO")
source(analise_agua_comunidades_tradicionais.R)

#Demandas posteriores ---------------
#######################
# Subset Vale do Rio Doce - KITS SEAPA

comunidades_vale = comunidades_tradicionais[comunidades_tradicionais$nome_regiao == "Vale do Rio Doce",]
freq(comunidades_vale$cod_indigena_reside_fam)
freq(comunidades_vale$ind_familia_quilombola_fam)

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
comunidades_vale %>% group_by(nom_povo_indigena_fam) %>% count(cod_agua_canalizada_fam)
comunidades_vale %>% group_by(nom_comunidade_quilombola_fam) %>% count(cod_agua_canalizada_fam)
