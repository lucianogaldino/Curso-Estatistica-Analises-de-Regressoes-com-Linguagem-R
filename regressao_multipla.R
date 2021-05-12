#####################################
###   REGRESSÃO LINEAR MÚLTIPLA   ###
#####################################

if(!require(dplyr)) install.packages("dplyr")
library(dplyr) # Manipulação de dados


# BUSCAR DIRETÓRIO (PASTA COM OS ARQUIVOS)
setwd("C:/Users/Luciano/Desktop/regressoes_R")

#ABRIR ARQUIVO

enem <- read.csv('enem_2019_tratado.csv', sep = ",")
View (enem)

colegiox <- enem %>% filter (CO_ESCOLA=="35132287")


# Verificando valores missing (Ausentes)
# NA = valores ausentes
# NAN = not a number(valor indefinido)
sapply(colegiox, function(x) sum(is.na(x)))
sapply(colegiox, function(x) sum(is.nan(x)))

# Análise da classificação dos atributos
glimpse(colegiox)
str(colegiox)








## Construção do modelo 1:
modelo1 <- lm(NOTA_REDACAO ~ COMP2 + COMP4 + COMP5, colegiox)


# Análise gráfica
plot(modelo1)

# Gráfico 1: Linearidade.
# Gráfico 2: Normalidade.
# Gráfico 3: Homocedasticidade.
# Gráfico 4: Outliers (discrepantes) e Pontos de alavancagem
#           (pontos que influenciam o modelo). 
#            Pontos entre -3 e 3 indicam não ter outliers.

## Normalidade dos resíduos:
# Ho = distribuição normal : p > 0.05
# Ha = distribuição != normal : p <= 0.05
shapiro.test(modelo1$residuals)


## Outliers nos resíduos (limites entre -3 e 3):
summary(rstandard(modelo1))


## Independência dos resíduos (Durbin-Watson):
# ideal para medidas repetidas (longitudinais: Ex: mesmo aluno)
if(!require(car)) install.packages("car")
library(car)
# Ho = resíduos independentes (não estão correlacionados): p > 0.05
# Ha = resíduos dependentes : p <= 0.05
durbinWatsonTest(modelo1)


## Homocedasticidade (Breusch-Pagan):
if(!require(lmtest)) install.packages("lmtest")
library(lmtest)
# Ho = existe homocedasticidade : p > 0.05
# Ha = não existe homocedasticidade : p <= 0.05
bptest(modelo1)


# Ausência de Multicolinearidade (somente entre as variáveis independentes)

variaveis <- subset(colegiox, select= c(COMP2, COMP4, COMP5))

if(!require(psych)) install.packages("psych")
library(psych)

pairs.panels(variaveis)
# Multicolinearidade:
# Considerada multicolinearidade quando r > 0.9.

vif(modelo1)
# Multicolinearidade ocorre quando VIF > 10

# Análise do modelo
# Intercept
# p_valor para cada coeficiente < 0,05 (estatisticamente significativos).
# Adjusted R-squared (explicação do modelo através dos dados)
# p_valor da estatística F < 0.05 (valida o modelo de regressão)

summary(modelo1)

# Equação: Nota Redação=116.1987+1.2831*COMP2+1.72983*COMP4+1.19933*COMP5


# Criando uma coluna de previsão: 
colegiox$previsao <- modelo1$fitted.values



# Coeficientes padronizados para compará-los
if(!require(QuantPsyc)) install.packages("QuantPsyc")
library(QuantPsyc)

lm.beta(modelo1)








## Construção do modelo 2:
modelo2 <- lm(NOTA_REDACAO ~ COMP2 + COMP4, colegiox)


# Análise gráfica
plot(modelo2)

# Gráfico 1: Linearidade.
# Gráfico 2: Normalidade.
# Gráfico 3: Homocedasticidade.
# Gráfico 4: Outliers (discrepantes) e Pontos de alavancagem
#           (pontos que influenciam o modelo). 
#            Pontos entre -3 e 3 indicam não ter outliers.

## Normalidade dos resíduos:
# Ho = distribuição normal : p > 0.05
# Ha = distribuição != normal : p <= 0.05
shapiro.test(modelo2$residuals)


## Outliers nos resíduos (limites entre -3 e 3):
summary(rstandard(modelo2))


## Independência dos resíduos (Durbin-Watson):
# ideal para medidas repetidas (longitudinais: Ex: mesmo aluno)
if(!require(car)) install.packages("car")
library(car)
# Ho = resíduos independentes (não estão correlacionados): p > 0.05
# Ha = resíduos dependentes : p <= 0.05
durbinWatsonTest(modelo2)


## Homocedasticidade (Breusch-Pagan):
if(!require(lmtest)) install.packages("lmtest")
library(lmtest)
# Ho = existe homocedasticidade : p > 0.05
# Ha = não existe homocedasticidade : p <= 0.05
bptest(modelo2)


# Ausência de Multicolinearidade (somente entre as variáveis independentes)

variaveis <- subset(colegiox, select= c(COMP2, COMP4))

if(!require(psych)) install.packages("psych")
library(psych)

pairs.panels(variaveis)
# Multicolinearidade:
# Considerada multicolinearidade quando r > 0.9.

vif(modelo2)
# Multicolinearidade ocorre quando VIF > 10

# Análise do modelo

# Intercept
# p_valor para cada coeficiente < 0,05 (estatisticamente significativos)
# Adjusted R-squared (explicação do modelo através dos dados)
# p_valor da estatística F < 0.05 (valida o modelo de regressão estatisticamente)
# Não comparar os coeficientes, pois podem ter unidades diferentes.

summary(modelo2)

# Equação: Nota Redação = 214.0062+1.4391*COMP2 +2.1895*COMP4

# Criando uma coluna de previsão: 
colegiox$previsao2 <- modelo2$fitted.values


# Coeficientes padronizados para poder compará-los.
if(!require(QuantPsyc)) install.packages("QuantPsyc")
library(QuantPsyc)

lm.beta(modelo2)








# Comparação dos modelos

# Para qualquer tipo de modelo
# O melhor modelo é com resultado menor (menor variação dos resíduos)

# Critério de Informação de Akaike (AIC)
AIC(modelo1, modelo2) 
# Critério de Informação Bayesiano (BIC)
BIC(modelo1, modelo2) 


# Teste Anova para modelos aninhados
# (modelo 2 derivado do modelo 1 e mesma variável dependente)
# O melhor é com o menor valor do RSS (soma dos resíduos ao quadrado)
# Ho = modelos iguais: p>0,05
# Ha = modelos diferentes: p<=0,05
anova(modelo1, modelo2)


# Gráfico de dispersão 3D
if(!require(scatterplot3d)) install.packages("scatterplot3d")
library(scatterplot3d)

# Modelo 1
grafico_3d <- scatterplot3d(colegiox$NOTA_REDACAO ~ colegiox$COMP2 + colegiox$COMP4 + colegiox$COMP5,
                       pch = 16, angle = 30, color = "blue", box = FALSE)

# Modelo 2
grafico_3d <- scatterplot3d(colegiox$NOTA_REDACAO ~ colegiox$COMP2 + colegiox$COMP4,
                            pch = 20, angle = 40, color = "red", box = FALSE,
                            xlab="COMP2", ylab="COMP4", zlab="Notas")
grafico_3d$plane3d(modelo2, col="black", draw_polygon = TRUE)



# Seleção automática do melhor modelo

if(!require(MASS)) install.packages("MASS")
library(MASS)

modelo_total <- lm(NOTA_REDACAO ~ COMP2+COMP4+COMP5, data = colegiox)
modelo_nulo <- lm(NOTA_REDACAO ~ 1, data = colegiox)

stepAIC(modelo_total, scope = list(upper = modelo_total, lower = modelo_nulo),
                                  direction = "backward")





