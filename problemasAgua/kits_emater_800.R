# Analise de problemas com água
# Comunidades Tradicionais
# KITS EMATER - 800 para outras regiões
########################################

setwd("~/Documentos/Neylson Crepalde/RASTREIA/CADUNICO")
source("~/Documentos/Neylson Crepalde/RASTREIA/novosEncontros/problemasAgua/analise_agua_comunidades_tradicionais.R")

comunidades_exceto_vale = comunidades_tradicionais[comunidades_tradicionais$nome_regiao != "Vale do Rio Doce",]
freq(comunidades_exceto_vale$cod_indigena_reside_fam, plot=F)
freq(comunidades_exceto_vale$ind_familia_quilombola_fam, plot=F)
freq(comunidades_exceto_vale$nome_regiao, plot=F)
#Separando as comunidades tradicionais que interessam, a saber:
# 203 - família de terreiro
# 205 - família de agricultores familiares
# 301 - Família assentada da reforma agraria
# 303 - Família acampada ***  ACAMPAMENTO NÃO PODE
# 
comunidades_exceto_vale$ind_parc_mds_fam[comunidades_exceto_vale$ind_parc_mds_fam == 0] = NA
comunidades_exceto_vale$ind_parc_mds_fam[comunidades_exceto_vale$ind_parc_mds_fam != 203 & 
                                    comunidades_exceto_vale$ind_parc_mds_fam != 205 &
                                    comunidades_exceto_vale$ind_parc_mds_fam != 301] = NA

freq(comunidades_exceto_vale$ind_parc_mds_fam)

#Separando apenas quem tem banheiro

comunidades_exceto_vale = comunidades_exceto_vale[comunidades_exceto_vale$cod_banheiro_domic_fam == 1,]

###########################
#Municípios prioritários:
# Mapeando os municípios que recebem atendimento da EMATER

list.files()
un_emater <- read.csv("unidades_emater.csv", stringsAsFactors = F)
names(un_emater) <- "nome_munic"

# Separando apenas os municípios que são atendidos pela EMATER
dim(comunidades_exceto_vale)
length(levels(as.factor(comunidades_exceto_vale$nome_munic)))

com_emater <- inner_join(un_emater, comunidades_exceto_vale, by="nome_munic")
length(levels(as.factor(com_emater$nome_munic)))
dim(com_emater)
###############################


#Total de atendidos
atendidos = sum(com_emater$qtd_pessoas_domic_fam, na.rm = T); print(atendidos)

#Total de atendidos por nome de comunidade indígena
com_emater %>% group_by(nom_povo_indigena_fam) %>%
  summarize(total = sum(qtd_pessoas_domic_fam))

#Total de atendidos por comunidade quilombola
com_emater %>% group_by(nom_comunidade_quilombola_fam) %>%
  summarize(total = sum(qtd_pessoas_domic_fam))

#Total de atendidos por famílias tradicionais
com_emater %>% group_by(ind_parc_mds_fam) %>%
  summarize(total = sum(qtd_pessoas_domic_fam))

#####################################
#Verificação de acesso à água por comunidade
#Índios
separado_ind_n_naovale = com_emater %>% group_by(nom_povo_indigena_fam) %>%
  summarise(n = n())
separado_ind_div_naovale = com_emater %>% group_by(nom_povo_indigena_fam) %>%
  count(cod_agua_canalizada_fam) %>% na.omit
names(separado_ind_div_naovale)[3] = "count"

separado_ind_per_naovale = left_join(separado_ind_div_naovale, separado_ind_n_naovale) %>% 
  mutate(per = round(count/n, 2)) %>% filter(cod_agua_canalizada_fam == 2) %>%
  arrange(desc(per))
separado_ind_per_naovale = separado_ind_per_naovale[separado_ind_per_naovale$nom_povo_indigena_fam != "",]
print(separado_ind_per_naovale[,-4])

#Prioridade: AMANAYE, ARANA, AIKANA, WAYANA, XAKRIABA
# 2017:61 | # 2018:1
#Total sem água (prioritários) = 62 (excluindo MACAKALI)
#Total sem água (todos) = 69
sum(separado_ind_per_naovale$count)

#Quilombolas
separado_qui_n_naovale = com_emater %>% group_by(nom_comunidade_quilombola_fam) %>%
  summarise(n = n())
separado_qui_div_naovale = com_emater %>% group_by(nom_comunidade_quilombola_fam) %>%
  count(cod_agua_canalizada_fam) %>% na.omit
names(separado_qui_div_naovale)[3] = "count"

separado_qui_per_naovale = left_join(separado_qui_div_naovale, separado_qui_n_naovale) %>% 
  mutate(per = round(count/n, 2)) %>% filter(cod_agua_canalizada_fam == 2) %>%
  arrange(desc(per))
separado_qui_per_naovale = separado_qui_per_naovale[separado_qui_per_naovale$nom_comunidade_quilombola_fam !="",]
View(separado_qui_per_naovale[,-4])

# Recorte: acima de 40% [posicao 43]
# Para apenas 2017 corte: acima de 20% [posicao 52]

# Total (Soma dos prioritários): 392
# Total (soma de todos): 1068
sum(separado_qui_per_naovale$count[1:43])

# Assentados
#separado_ass_n_naovale = com_emater %>% group_by(ind_parc_mds_fam) %>%
#  summarise(n = n())
#separado_ass_div_naovale = com_emater %>% group_by(ind_parc_mds_fam) %>%
#  count(cod_agua_canalizada_fam) %>% na.omit
#names(separado_ass_div_naovale)[3] = "count"

#separado_ass_per_naovale = left_join(separado_ass_div_naovale, separado_ass_n_naovale) %>% 
#  mutate(per = round(count/n, 2)) %>% filter(cod_agua_canalizada_fam == 2) %>%
#  arrange(desc(per))
#print(separado_ass_per_naovale[,-4])

#Prioritário: 301 - Família assentada de reforma agrária
#Total prioritários: 380
# Família de terreiro: 10
# Agricultores Familiares: 3660
#Total (301 + 205 + 303) = 4050
#sum(separado_ass_per_naovale$count)

################################
#TOTAL de selecionados = 1078 (somente assentados)!!!
62 + 392 + 337      #(337 = assentamentos da SEDA) 2017 + 2018
61 + 398 + 337      #(337 = assentamentos da SEDA) 2017 apenas!
################################

#Separando por município
com_emater %>% group_by(nome_munic) %>% count(nom_povo_indigena_fam) %>% 
  filter(nom_povo_indigena_fam != "")
com_emater %>% group_by(nome_munic) %>% count(nom_comunidade_quilombola_fam) %>%
  filter(nom_comunidade_quilombola_fam != "")
com_emater %>% group_by(nome_munic) %>% count(ind_parc_mds_fam) %>%
  na.omit %>% arrange(desc(n)) %>% View

#Ranking dos municípios em situação crítica
separado_qui_n_naovale = com_emater %>% group_by(nome_munic) %>%
  summarise(n = n())
separado_qui_div_naovale = com_emater %>% group_by(nome_munic) %>%
  count(cod_agua_canalizada_fam) %>% na.omit
names(separado_qui_div_naovale)[3] = "count"

left_join(separado_qui_div_naovale, separado_qui_n_naovale) %>% 
  mutate(per = round(count/n, 2)) %>% filter(cod_agua_canalizada_fam == 2) %>%
  arrange(desc(per)) %>% View



