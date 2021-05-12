############################################################################
###  REGRESSÃO DE COMPONENTES PRINCIPAIS E DE MÍNIMOS QUADRADOS PARCIAIS ###
############################################################################

library(dplyr) 


# BUSCAR DIRETÓRIO (PASTA COM OS ARQUIVOS)
setwd("C:/Users/Luciano/Desktop/regressoes_R")

#ABRIR ARQUIVO

library(readxl)

gasto <- read_xlsx("gasto_almoco.xlsx")


# Verificando valores missing (Ausentes)
# NA = valores ausentes
# NAN = not a number(valor indefinido)
sapply(gasto, function(x) sum(is.na(x)))
sapply(gasto, function(x) sum(is.nan(x)))

# Análise da classificação dos atributos
glimpse(gasto)

# Multicolinearidade (somente entre as variáveis independentes)
library(psych)
pairs.panels(gasto) # Multicolinearidade (r > 0.9).


# CRIAÇÃO DO MODELO DE REGRESSÃO DE COMPONENTES PRINCIPAIS (PCR)
install.packages("pls")
library(pls)

modelo_pcr <- pcr(gasto_acum_reais ~ dia + num_refeicoes,
                  data = gasto, scale = TRUE, validation = "CV")
# scale = TRUE (variáveis padronizadas)
# validation = "CV" (validação cruzada)
summary(modelo_pcr)

# Criação de uma coluna previsao
gasto$previsao_pcr <- predict (modelo_pcr, gasto, ncomp = 2)

# Criação de uma coluna com erro absoluto
gasto$erro_pcr <- abs(gasto$previsao_pcr - gasto$gasto_acum_reais)
sum(gasto$erro_pcr)

# Desvio padrão
sd(gasto$previsao_pcr - gasto$gasto_acum_reais)




# CRIAÇÃO DO MODELO DE REGRESSÃO POR MÍNIMOS QUADRADOS PARCIAIS (PLS)
if(!require(plsdepot)) install.packages("plsdepot")
library(plsdepot)

modelo_pls <- plsreg1(gasto[ , c(1,3)], gasto[ , 4], comps = 2)

# Coeficientes
modelo_pls$reg.coefs
# Equação: gasto_acum_reais = 2.634575 + 4,891452*dia + 25.639543*num_refeicoes

# Criação de uma coluna previsao
gasto$previsao_pls <- modelo_pls$y.pred

# Criação de uma coluna com erro absoluto
gasto$erro_pls <- abs(gasto$previsao_pls - gasto$gasto_acum_reais)
sum(gasto$erro_pls)

# Desvio padrão
sd(gasto$previsao_pls - gasto$gasto_acum_reais)




