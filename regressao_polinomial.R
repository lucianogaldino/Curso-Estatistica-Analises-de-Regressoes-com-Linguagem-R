################################
###   REGRESSÃO POLINOMIAL   ###
################################

library(dplyr) # Manipulação de dados

# BUSCAR DIRETÓRIO (PASTA COM OS ARQUIVOS)
setwd("C:/Users/Luciano/Desktop/regressoes_R")


install.packages("readxl")
library(readxl)

vendas <- read_xlsx("comissao.xlsx")


# Verificando valores missing (Ausentes)
# NA = valores ausentes
# NAN = not a number(valor indefinido)
sapply(vendas, function(x) sum(is.na(x)))
sapply(vendas, function(x) sum(is.nan(x)))

# Análise da classificação dos atributos (variáveis)
str(vendas)


# Gráfico para análise inicial
plot(vendas$quantidade, vendas$comissao)


# NORMALIDADE
# QQPLOT (GRÁFICO DE DISTRIBUIÇÃO NORMAL)
qqnorm(vendas$comissao)
qqline(vendas$comissao)

qqnorm(vendas$quantidade)
qqline(vendas$quantidade)

# Shapiro-Wilk
# Ho = distribuição normal : p > 0.05
# Ha = distribuição != normal : p <= 0.05

shapiro.test(vendas$comissao)
shapiro.test(vendas$quantidade)


# CORRELAÇÃO LINEAR:

# Pearson (distribuição normal)
# Spearman (distribuição não normal)
# Kendall (distribuição não normal com quantidade pequena de amostras)

# Ho = não há corrrelação linear: p > 0,05
# Ha = existe correlação linear: p <= 0,05
cor.test(vendas$comissao, vendas$quantidade, method = "spearman")



# REGRESSÃO LINEAR
modelo1 = lm (vendas$comissao ~ vendas$quantidade)
summary(modelo1)


# Representação gráfica
if(!require(ggplot2)) install.packages("ggplot2") #gráfico 
if(!require(ggpubr)) install.packages("ggpubr") #equação da reta no gráfico

library(ggplot2)
library(ggpubr)

ggplot(data = vendas, mapping = aes(x = quantidade, y = comissao)) +
  geom_point() +
  geom_smooth(method = "lm", col = "red") +
  stat_regline_equation(aes(label= paste(..eq.label..,..adj.rr.label..,
                            sep = "*plain(\",\")~~")),label.x = 20, label.y = 100) +
  theme_classic()

# Coluna de previsão
vendas$previsao1 <- modelo1$fitted.values







# REGRESSÃO POLINOMIAL - GRAU 2

# y = b0 + b1.x + b2.x^2

x <- vendas$quantidade

var_indep = cbind (x, x ^ 2)

modelo2 = lm (vendas$comissao ~ var_indep)
summary(modelo2)

# Equação: Comissão = 700 + 25*quantidade + 3*quantidade^2

ggplot (data = vendas) + geom_point (aes (x = x, y = comissao)) +
  geom_line (aes (x = quantidade, y = modelo1 $ fit), col = "blue") +
  geom_line (aes (x = x, y = modelo2 $ fit), col = "red")


vendas$previsao2 <- modelo2$fitted.values






# REGRESSÃO POLINOMIAL - GRAU 3

# y = b0 + b1.x + b2.x^2 + b3.x^3

x <- vendas$quantidade

var_indep_3 = cbind (x, x ^ 2, x ^ 3)

modelo3 = lm (vendas$comissao ~ var_indep_3)
summary(modelo3)

# Equação: Comissão = 700 + 25*quantidade + 3*quantidade^2 - 0*quantidade^3

ggplot (data = vendas) + geom_point (aes (x = x, y = comissao)) +
  geom_line (aes (x = quantidade, y = modelo1 $ fit), col = "blue") +
  geom_line (aes (x = x, y = modelo2 $ fit), col = "red")+
  geom_line (aes (x = x, y = modelo3 $ fit), col = "yellow")


# Coluna previsão
vendas$previsao3 <- modelo3$fitted.values


