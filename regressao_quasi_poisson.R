###################################
###   REGRESSÃO QUASI POISSON   ###
###################################


library(dplyr) # Manipulação de dados


# BUSCAR DIRETÓRIO (PASTA COM OS ARQUIVOS)
setwd("C:/Users/Luciano/Desktop/regressoes_R")

# Objetivo: Analisar reclamações em uma nova empresa de internet

#ABRIR ARQUIVO
library(readxl)
reclamacoes <- read_xlsx('reclamacoes.xlsx')
View (reclamacoes)


# CRIAÇÃO DOS MODELOS DE QUASI POISSON

modelo_quasi1 <- glm(velocidade ~ dia, data = reclamacoes,
                    family = "quasipoisson")
summary(modelo_quasi1)
# Equação: velocidade = e^(3.767909+0.034464*dia)
reclamacoes$modelo_veloc <- modelo_quasi1$fitted.values





modelo_quasi2 <- glm(instabilidade ~ dia, data = reclamacoes,
                     family = "quasipoisson")
summary(modelo_quasi2)

# Equação: instabilidade = e^(3.61161-0.098534*dia)
reclamacoes$modelo_insta <- modelo_quasi2$fitted.values
View(reclamacoes)





modelo_quasi3 <- glm(conexao ~ dia, data = reclamacoes,
                        family = "quasipoisson")
summary(modelo_quasi3)

# Equação: conexao = e^(2.860358+0.005103*dia)

reclamacoes$modelo_con <- modelo_quasi3$fitted.values
View(reclamacoes)





modelo_quasi4 <- glm(velocidade ~ dia+instabilidade, data = reclamacoes,
                     family = "quasipoisson")
summary(modelo_quasi4)

# Equação: velocidade = e^(3.7560318+0.0350997*dia+0.0003691*instabilidade)

reclamacoes$modelo_veloc2 <- modelo_quasi4$fitted.values
View(reclamacoes)


