#################################################
###   REGRESSÃO RIDGE - LASSO - ELASTIC NET   ###
#################################################

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


if(!require(glmnet)) install.packages("glmnet")
library(glmnet)

# Variáveis independentes
x <- subset(gasto, select= c(dia,num_refeicoes))
# Variável dependente
y <- subset(gasto, select= c(gasto_acum_reais))




## MODELO RIDGE (Alpha = 0)
modelo_ridge <- cv.glmnet(as.matrix(x), as.matrix(y), alpha = 0)

# melhor parâmetro de regularização
melhor_lambda <- modelo_ridge$lambda.min

coef <- predict(modelo_ridge, s = melhor_lambda, type = "coefficients")
coef


# EQUAÇÃO:
gasto$ridge <- 41.65+14.81*gasto$dia+14.54*gasto$num_refeicoes

gasto$erro_rid <- abs(gasto$ridge - gasto$gasto_acum_reais)
sum(gasto$erro_rid)

# Desvio padrão
sd(gasto$ridge - gasto$gasto_acum_reais)




# MODELO LASSO (Alpha = 1)
modelo_lasso = cv.glmnet(as.matrix(x), as.matrix(y), alpha = 1)

# melhor parâmetro de regularização
melhor_lambda = modelo_lasso$lambda.min

coef = predict(modelo_lasso, s = melhor_lambda, type = "coefficients")
coef

# EQUAÇÃO: 
gasto$lasso <- 22.39+9.9*gasto$dia+20.03*gasto$num_refeicoes

gasto$erro_lasso <- abs(gasto$lasso - gasto$gasto_acum_reais)
sum(gasto$erro_lasso)

# Desvio padrão
sd(gasto$lasso - gasto$gasto_acum_reais)




# Modelo de Elastic Net (Alpha entre 0 e 1)
modelo_elastic = cv.glmnet(as.matrix(x), as.matrix(y), alpha = 0.5)

# melhor parâmetro de regularização
melhor_lambda = modelo_elastic$lambda.min

coef = predict(modelo_elastic, s = melhor_lambda, type = "coefficients")
coef

# EQUAÇÕES:
# Alpha = 0.5
gasto$elastic_05 <- 14.79+14.94*gasto$dia+15.23*gasto$num_refeicoes
# Erro absoluto
gasto$erro_elas05 <- abs(gasto$elastic_05 - gasto$gasto_acum_reais)
sum(gasto$erro_elas05)
sd(gasto$elastic_05 - gasto$gasto_acum_reais)


# Alpha = 0.75
modelo_elastic = cv.glmnet(as.matrix(x), as.matrix(y), alpha = 0.75)
melhor_lambda = modelo_elastic$lambda.min

coef = predict(modelo_elastic, s = melhor_lambda, type = "coefficients")
coef

gasto$elastic_075 <- 15.74+14.57*gasto$dia+15.58*gasto$num_refeicoes

# Erro absoluto
gasto$erro_elas075 <- abs(gasto$elastic_075 - gasto$gasto_acum_reais)
sum(gasto$erro_elas075)
# Desvio padrão
sd(gasto$elastic_075 - gasto$gasto_acum_reais)


# Alpha = 0.25
modelo_elastic = cv.glmnet(as.matrix(x), as.matrix(y), alpha = 0.25)
melhor_lambda = modelo_elastic$lambda.min

coef = predict(modelo_elastic, s = melhor_lambda, type = "coefficients")
coef


gasto$elastic_025 <- 15.51+15.08*gasto$dia+15.07*gasto$num_refeicoes

# Erro absoluto
gasto$erro_elas025 <- abs(gasto$elastic_025 - gasto$gasto_acum_reais)
sum(gasto$erro_elas025)

# Desvio padrão
sd(gasto$elastic_025 - gasto$gasto_acum_reais)



