library(dplyr)

# Renomeando a tabela 
agro = X_AGRICULTURA_Produção_agrícola_SIDRA

# Filtrando por atividade Contas Regionais
agro_cr = agro %>% group_by(CR , Ano , Trimestre) %>% summarise(Valor = sum(Valor)) %>% na.omit()

# Filtro de atividades contas regionais
filtro = unique(agro_cr$CR)

# Preenchendo a tabela com os valores

valores_agro = data.frame(matrix(NA, nrow = nrow(agro_cr), ncol = 9))
names(valores_agro) = c(filtro)

for (i in seq_along(filtro)) {
  valores_agro[,i] = filter(agro_cr , CR == filtro[i])$Valor 
}
valores_agro = as.matrix(valores_agro)
## Preenchendo a tabela com as variações
variacoes_agro = data.frame(matrix(NA, nrow = nrow(agro_cr), ncol = 9))
names(variacoes_agro) = c(filtro)

for (col in 1:ncol(valores_agro)) {
  for (row in 2:nrow(valores_agro)) {
    variacoes_agro[row, col] = valores_agro[row , col] / valores_agro[row-1, col]
  }
}

#retirando valores NaN e Inf
variacoes_agro = sapply(variacoes_agro, function(x) { x[is.nan(x) | is.infinite(x)| is.na(x)] <- 0; x })
variacoes_agro = as.matrix(variacoes_agro)

###########################################################

# A partir daqui, faremos a ACP para criar um indicador para 
# a atividade Agricultura, inclusive o apoio e a pós colheita.

# Tendo em mãos esse indicador, para a atividade da agropecuária, faremos uma ACP novamente para criar 
# o indicador para a atividade agropecuária.

##########################################################

# Renomeando a tabela
pec = X_PECUÁRIA_Animais_abatidos_SIDRA

# Filtrando por atividade Contas Regionais

filtro_2 =  colnames(pec)[c(6,7,9,11,13,15,3)]
pec_cr = pec %>% group_by(across(one_of(filtro_2))) %>%  summarise(Valor = sum(Valor))

# Valores que precisamos

valores_pec = pec_cr  %>% filter(`Referência temporal` == "Total do trimestre" 
                                & Variável == "Animais abatidos" 
                                & `Tipo de rebanho bovino` == "Total"
                                & `Tipo de inspeção` == "Total") %>%
                          pull(Valor) %>% 
                          as.matrix()

# Variações da pecuária
variacoes_pec = matrix(NA, nrow = 1, ncol = 1)

 for (i in 2:length(valores_pec)) {
   variacoes_pec[i] = valores_pec[i] / valores_pec[i-1]
 }
# Deixando no formato matriz
variacoes_pec = as.matrix(variacoes_pec , dim = c(1:length(variacoes_pec) , 1))
# Retirando os valores NA
variacoes_pec[is.na(variacoes_pec)] = 0

### Produção florestal e pesca
# Importando dados da pim já filtrando de acordo com o que precisamos

proflor = X_INDÚSTRIA_PIM %>% filter(`Unidade de Medida` == "Número-índice" 
                                     & `Seções e atividades industriais (CNAE 2.0)` == "3.17 Fabricação de celulose, papel e produtos de papel" 
                                     & Variável == "PIMPF - Número-índice (2012=100)")

# Criando uma coluna de trimestre para somar depois
library(car)
proflor$Trimestre <- Recode(proflor$Mês,
                          "'janeiro' = 1;'fevereiro' = 1;'março' = 1;'abril' = 2;
                           'maio' = 2;'junho' = 2;'julho' = 3;'agosto' = 3;
                           'setembro' = 3;'outubro' = 4;'novembro' = 4;'dezembro' = 4")

# Realocando a coluna para o lado do mês #estéticatambémconta

proflor = proflor %>% relocate(Trimestre, .after = Mês)  %>%
                      relocate(Valor, .after = last_col())

proflor_cr = proflor %>% group_by(Ano , Trimestre) %>% summarise(Valor = mean(Valor))
valores_proflor = proflor

