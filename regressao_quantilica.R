################################
###   REGRESSÃO QUANTÍLICA   ###
################################

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



# ANÁLISE DA CORRELAÇÃO LINEAR

# ANÁLISE GRÁFICA DA CORRELAÇÃO

plot(gasto$dia, gasto$gasto_acum_reais)


# NORMALIDADE
#Ho = distribuição normal : p > 0.05
#Ha = distribuição != normal : p <= 0.05
shapiro.test(gasto$gasto_acum_reais)

# QQPLOT (GRÁFICO DE DISTRIBUIÇÃO NORMAL)
qqnorm(gasto$gasto_acum_reais)
qqline(gasto$gasto_acum_reais)


# Correlação Linear:

# Ho = não há corrrelação linear: p > 0,05
# Ha = existe correlação linear: p <= 0,05
cor.test(gasto$gasto_acum_reais, gasto$dia, method = "spearman")




# MODELO DE REGRESÃO LINEAR:

# Análise dos resíduo (valor previsto - valor esperado)

modelo_regressao <- lm(gasto_acum_reais ~ dia, gasto)

## Análise gráfica:
plot(modelo_regressao) 

# Gráfico 1: Linearidade.
# Gráfico 2: Normalidade.
# Gráfico 3: Homocedasticidade.
# Gráfico 4: Outliers (discrepantes) e Pontos de alavancagem
#           (pontos que influenciam o modelo). 
#            Pontos entre -3 e 3 indicam não ter outliers.


# Teste de normalidade dos resíduos
# Ho = distribuição normal : p > 0.05
# Ha = distribuição != normal : p <= 0.05
shapiro.test(modelo_regressao$residuals)


# Outliers (Entre -3 e 3)
summary(rstandard(modelo_regressao))


# Homocedasticidade (teste Breuschen-Pagan - somente para resíduos normais)

library(lmtest)

# Ho = existe homocedasticidade : p > 0.05
# Ha = não existe homocedasticidade : p <= 0.05

bptest(modelo_regressao)


###  ANÁLISE PELO MODELO DE REGRESSÃO LINEAR REPROVADO
###  Presença de outliers e heterocedascidade







### REGRESSÃO QUANTÍLICA

install.packages("quantreg")
library("quantreg")

modelo1 <- rq (gasto_acum_reais ~ dia, gasto, tau = 0.25)
summary(modelo1)
# Equação: gasto_acum = -21.71429+30.92857*dia



modelo2 <- rq (gasto_acum_reais ~ dia, gasto, tau = 0.5)
summary(modelo2)
# Equação: gasto_acum = -8.03125+30.78125*dia



modelo3 <- rq (gasto_acum_reais ~ dia, gasto, tau = 0.75)
summary(modelo3)
# Equação: gasto_acum = 13.40741+30.48148*dia



modelo4 <- rq (gasto_acum_reais ~ dia, gasto, tau = 0.85)
summary(modelo4)
# Equação: gasto_acum = -5.4+31.4*dia



modelo5 = rq (gasto_acum_reais ~ dia, gasto, tau = seq (0.25, 1, by = 0.25))
summary(modelo5)


# Comparando com a regressão linear
summary(modelo_regressao)
# Equação: gasto_acum = -17.8542+31.1984.dia

# Ocorreram pequenas diferenças nos coeficientes angulares.




