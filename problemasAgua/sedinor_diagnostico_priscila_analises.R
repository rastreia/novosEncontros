#####################################
# Problemas  - ações com água
# RASTREIA
# Neylson Crepalde
#####################################

setwd('~/RASTREIA/NOVOS ENCONTROS/problemas_agua')
source("sedinor_diagnostico_priscila_le_dados.R")
#################################################

# Comparando categorias semelhantes nas duas variáveis
table(as.numeric(sedinor_atual[,28]),as.numeric(sedinor_atual[,35]))
chisq.test(table(as.numeric(sedinor_atual[,28]),as.numeric(sedinor_atual[,35])))

table(as.numeric(sedinor_atual[,31]),as.numeric(sedinor_atual[,38]))
chisq.test(table(as.numeric(sedinor_atual[,31]),as.numeric(sedinor_atual[,38])))


# Renomeando variáveis
names(sedinor_atual)[c(30,31,38)] = c("Tubulação e reservatório entregue",
                                      "Tubulação e reservatório não entregue",
                                      "Tubulação não entregue")

sedinor_atual %>% filter(Equipado == 1) %>% count(`Tubulação não entregue`)


# Poços com tubulação e sem energia
sedinor_atual %>% filter(`Tubulação e reservatório entregue`==1) %>% count(Energizado)
