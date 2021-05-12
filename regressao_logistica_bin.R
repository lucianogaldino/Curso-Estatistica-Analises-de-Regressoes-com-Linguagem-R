#######################################
###   REGRESSÃO LOGÍSTICA BINÁRIA   ###
#######################################


library(dplyr) # Manipulação de dados


# BUSCAR DIRETÓRIO (PASTA COM OS ARQUIVOS)
setwd("C:/Users/Luciano/Desktop/regressoes_R")

#ABRIR ARQUIVO
doenca_pre <- read.csv('casos_obitos_doencas_preexistentes.csv',
                       sep = ";")
View (doenca_pre)

doenca_pre <- read.csv('casos_obitos_doencas_preexistentes.csv',
                       sep = ";", encoding = 'UTF-8')
View (doenca_pre)

# Objetivo: Analisar se existe uma tendência de óbito entre 
# pessoas do sexo feminino e masculino.

table(doenca_pre$cs_sexo)

# EXCLUIR IGNORADO e INDEFINIDO DA VARIÁVEL cs_sexo
relacao <- doenca_pre %>% filter(cs_sexo!="IGNORADO")
relacao <- relacao %>% filter(cs_sexo!="INDEFINIDO")
View(relacao)

# Quantidade de pessoas do sexo masculino e feminino (variável independente)

library(plotly)
plot_ly(relacao, labels = ~ cs_sexo, type = 'pie')

# Quantidade de óbitos (variável dependente)
table(relacao$obito)

plot_ly(relacao, labels = ~ obito, type = 'pie')



# ANÁLISE DOS REGISTROS (LINHAS)

# Verificando valores missing (Ausentes)
# NA = valores ausentes
# NAN = not a number(valor indefinido)
sapply(relacao, function(x) sum(is.na(x)))
sapply(relacao, function(x) sum(is.nan(x)))


# Análise da classificação dos atributos

str(relacao)

# Alterando as variáveis

relacao$obito[relacao$obito==0] <- "Não"
relacao$obito[relacao$obito==1] <- "Sim"
str(relacao)

# Transformando string para fator

relacao$cs_sexo <- as.factor(relacao$cs_sexo)
relacao$obito <- as.factor(relacao$obito)
str(relacao)

levels(relacao$cs_sexo)  
levels(relacao$obito)






##### CONSTRUÇÃO DO MODELO 1 (uma variável independente):

# Variável dependente binária (dicotômica)
# Categorias mutuamente exclusivas (uma pessoa não pode estar em duas situações)
# Independência das observações (sem medidas repetidas)


modelo1 <- glm(obito ~ cs_sexo, family = binomial,
               data = relacao)



# Análise do modelo
# Estatisticamente significativo: p <= 0,05
# Estatisticamente não é significativo: p > 0,05
# Análise da Ausência de outliers e pontos de alavancagem
# Deve estar entre -3 e 3

summary(modelo1)

# Razão de chance com Intervalo de confiança de 95%

exp(coef(modelo1))


# CONCLUSÃO:
# Estatisticamente, com intervalo de confiança de 95%,
# A chance de uma pessoa do sexo masculino ir a óbito é
# 1,56 vezes maior do que a chance de uma pessoa do sexo feminino.

# Comprovação de que a variável depende obito é o "SIM":
modelo_prova <- glm(cs_sexo ~ obito, family = binomial,
               data = relacao)

summary(modelo_prova)
exp(coef(modelo_prova))







##### MODELO 2 (mais de uma variável independente):

# Diabetes e sexo

# Quantidade de diabéticos e não diabéticos
table(relacao$diabetes)

library(plotly)
plot_ly(relacao, labels = ~diabetes, type = 'pie')

# EXCLUIR IGNORADO DA VARIÁVEL diabetes
relacao2 <- relacao %>% filter(diabetes!="IGNORADO")
table(relacao2$diabetes)
plot_ly(relacao2, labels = ~diabetes, type = 'pie')

# Quantidade de óbitos
plot_ly(relacao, labels = ~ obito, type = 'pie') # antes
plot_ly(relacao2, labels = ~ obito, type = 'pie') # depois

# Quantidade por sexo
plot_ly(relacao, labels = ~ cs_sexo, type = 'pie') # antes
plot_ly(relacao2, labels = ~ cs_sexo, type = 'pie') # depois


# Análise da classificação das variáveis
str(relacao2)

# Transformando string para fator

relacao2$diabetes <- as.factor(relacao2$diabetes)
str(relacao2)

levels(relacao2$obito)  
levels(relacao2$diabetes)  
levels(relacao2$cs_sexo)  

# CONSTRUÇÃO DO MODELO 2 (duas variáveis independentes):

# Variável dependente binária (dicotômica)
# Categorias mutuamente exclusivas (uma pessoa não pode estar em duas situações)
# Independência das observações (sem medidas repetidas)


modelo2 <- glm(obito ~ diabetes + cs_sexo, family = binomial, 
               data = relacao2)


# Análise da Ausência de outliers e pontos de alavancagem
# Deve estar entre -3 e 3
library(MASS)
summary(stdres(modelo2)) # resíduos padronizados


# Ausência de multicolinearidade (VIF < 10)
library(car)
vif(modelo2)


# Análise do modelo
# Estatisticamente significativo: p <= 0,05
# Estatisticamente não é significativo: p > 0,05

summary(modelo2)

# Razão de chance com Intervalo de confiança de 95%
exp(coef(modelo2))

# O resultado da diabetes está inconsistente devido a presença enorme de
# dados ignorados.







###### MODELO 3 (variável independente numérica):

# Objetivo: óbito por idade
relacao3 <- doenca_pre %>% filter(nome_munic =="Santos")
View(relacao3)

str(relacao3)


# ANÁLISE DOS REGISTROS (LINHAS)

# Verificando valores missing (Ausentes)
# NA = valores ausentes
# NAN = not a number(valor indefinido)
sapply(relacao3, function(x) sum(is.na(x)))
sapply(relacao3, function(x) sum(is.nan(x)))

# Excluir valores missing em idade
library(tidyr)
relacao3 <- drop_na(relacao3, idade)


plot(relacao3$idade, relacao3$obito)


# Modelo de Regressão Logística

modelo3 <- glm(obito ~ idade,data=relacao3, family="binomial")

summary(modelo3)

# Modelo comparado aos dados

plot(relacao3$idade,relacao3$obito,col='red',pch=20)
points(relacao3$idade,modelo3$fitted, pch=4)

# Testar o modelo com os próprios candidatos

previsao <- predict(modelo3, newdata=relacao3, type="response")
previsao <- previsao >= 0.3
previsao

# Avaliação do desempenho (matriz de confusão)

matriz_confusao = table(previsao,relacao3$obito)
matriz_confusao
acerto = (matriz_confusao[1] + matriz_confusao[4]) / sum(matriz_confusao)
acerto





# Teste com outro dataframe 

jundiai <- doenca_pre %>% filter(nome_munic =="Jundiaí")
View(jundiai)

# Comparação do modelo
jundiai$previsao <- predict(modelo3,newdata=jundiai,type="response")

# Valores missing
sapply(jundiai, function(x) sum(is.na(x)))
jundiai <- drop_na(jundiai, previsao)


jundiai$previsao[jundiai$previsao >= 0.3] <- 1 # previsão de óbito
jundiai$previsao[jundiai$previsao < 0.3] <- 0 # previsão de não óbito

table(jundiai$previsao)



# Comparando com os dados reais de Jundiaí

# Reposicionando variável obito
jundiai <- jundiai %>% relocate(obito, .before = previsao) 

# Determinando os acertos e erros
jundiai$acertos <- jundiai$obito + jundiai$previsao

jundiai$acertos[jundiai$acertos != 1] <- "ACERTOU"
jundiai$acertos[jundiai$acertos == 1] <- "ERROU"

table(jundiai$acertos)
library(plotly)
plot_ly(jundiai, labels = ~ acertos, type = 'pie')


# EXPORTAR ARQUIVO
write.table(jundiai, file ="jundiai.csv", sep = ";")
