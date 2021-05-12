################################
###   REGRESSÃO DE POISSON   ###
################################


library(dplyr) # Manipulação de dados


# BUSCAR DIRETÓRIO (PASTA COM OS ARQUIVOS)
setwd("C:/Users/Luciano/Desktop/regressoes_R")

# Objetivo: Analisar reclamações em uma nova empresa de internet

#ABRIR ARQUIVO
library(readxl)
reclamacoes <- read_xlsx('reclamacoes.xlsx')
View (reclamacoes)


# CRIAÇÃO DOS MODELOS DE POISSON

modelo_poisson1 <- glm(velocidade ~ dia, data = reclamacoes,
                       family = poisson)
summary(modelo_poisson1)

# Equação: velocidade = e^(3.767909+0.034464*dia)
reclamacoes$modelo_veloc <- modelo_poisson1$fitted.values

# Validação: AIC=94.094


# Análise da dispersão (indício quando for maior que 1)
# Ho = Não há superdispersão: p > 0,05
# Ha = Há superdispersão: p <= 0,05
install.packages("AER")
library(AER)
dispersiontest(modelo_poisson1)



modelo_poisson2 <- glm(instabilidade ~ dia, data = reclamacoes,
                       family = poisson)
summary(modelo_poisson2)
reclamacoes$modelo_insta <- modelo_poisson2$fitted.values

# Equação: instabilidade = e^(3.61161-0.09853*dia)

# Validação: AIC=76.245


# Análise da dispersão
dispersiontest(modelo_poisson2)




modelo_poisson3 <- glm(conexao ~ dia, data = reclamacoes,
                       family = poisson)
summary(modelo_poisson3)
reclamacoes$modelo_con <- modelo_poisson3$fitted.values

# Equação: conexao = e^(2.860358-0.005103*dia)

# Validação: AIC=80.463

# Análise da dispersão
dispersiontest(modelo_poisson3)





modelo_poisson4 <- glm(velocidade ~ dia+instabilidade, data = reclamacoes,
                       family = poisson)
summary(modelo_poisson4)

# Equação: velocidade = e^(3.7560316+0.0350997*dia+0.0003691*instabilidade)
reclamacoes$modelo_veloc2 <- modelo_poisson4$fitted.values

# Validação: AIC=96.093

# Análise da dispersão
dispersiontest(modelo_poisson4)




