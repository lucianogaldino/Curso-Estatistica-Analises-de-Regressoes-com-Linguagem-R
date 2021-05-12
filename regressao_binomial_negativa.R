#######################################
###   REGRESSÃO BINOMIAL NEGATIVA   ###
#######################################


library(dplyr) # Manipulação de dados


# BUSCAR DIRETÓRIO (PASTA COM OS ARQUIVOS)
setwd("C:/Users/Luciano/Desktop/regressoes_R")

# Objetivo: Analisar reclamações em uma nova empresa de internet

#ABRIR ARQUIVO
library(readxl)
reclamacoes <- read_xlsx('reclamacoes.xlsx')
View (reclamacoes)


# CRIAÇÃO DOS MODELOS DE REGRESSÕES BINOMIAL NEGATIVA
library(MASS)

modelo_binom_neg1 <- glm.nb(velocidade ~ dia, data = reclamacoes)
summary(modelo_binom_neg1)

# Equação: velocidade = e^(3.767909+0.034464*dia)
reclamacoes$modelo_veloc <- modelo_binom_neg1$fitted.values
View(reclamacoes)

# Validação: AIC=96.094





modelo_binom_neg2 <- glm.nb(instabilidade ~ dia, data = reclamacoes)
summary(modelo_binom_neg2)

# Equação: instabilidade = e^(3.61161-0.09853*dia)
reclamacoes$modelo_insta <- modelo_binom_neg2$fitted.values
View(reclamacoes)

# Validação: AIC=78.245





modelo_binom_neg3 <- glm.nb(conexao ~ dia, data = reclamacoes)
summary(modelo_binom_neg3)

# Equação: conexao = e^(2.860358-0.005103*dia)

reclamacoes$modelo_con <- modelo_binom_neg3$fitted.values
View(reclamacoes)

# Validação: AIC=82.463





modelo_binom_neg4 <- glm.nb(velocidade ~ dia+instabilidade, data = reclamacoes)
summary(modelo_binom_neg4)

# Equação: velocidade = e^(3.7560318+0.0350997*dia+0.0003691*instabilidade)

reclamacoes$modelo_veloc2 <- modelo_binom_neg4$fitted.values
View(reclamacoes)

# Validação: AIC=98.093



