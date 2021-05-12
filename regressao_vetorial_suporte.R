#########################################
###   REGRESSÃO VETORIAL DE SUPORTE   ###
#########################################

library(dplyr) # Manipulação de dados

# Dataframe nativo do R
data(iris)

# Verificando valores missing (Ausentes)
# NA = valores ausentes
# NAN = not a number(valor indefinido)
sapply(iris, function(x) sum(is.na(x)))
sapply(iris, function(x) sum(is.nan(x)))

# Análise da classificação dos atributos (variáveis)
str(iris)

summary(iris)


# Gráfico de separação das espécieis
install.packages("scatterplot3d")
library(scatterplot3d)

colors <- c("red", "yellow", "blue") 
colors <- colors[as.numeric(iris$Species)]
# setosa: vermelho
# versicolor: amarelo
# virginica: azul
scatterplot3d(iris[ ,2:4], pch = 15, color=colors)

# SEPARAÇÃO EM DADOS DE TREINO E DADOS DE TESTE

dados_treino <- sample(1:150, 0.7*150)  
dados_teste <- setdiff(1:150, dados_treino)

iris_treino <- iris[dados_treino, ]
iris_teste <- iris[dados_teste, ]




# MODELO DE REGRESSÃO VETORIAL DE SUPORTE 
install.packages("e1071")
library (e1071)


# Função de Kernel: radial
modelo1 <- svm(Species ~ . , data = iris_treino)
summary(modelo1)
previsao1 <- predict(modelo1,iris_teste)
table(previsao1,iris_teste$Species)



# Função de Kernel: polinomial
modelo2 <- svm(Species ~ . , data = iris_treino, kernel = "polynomial")
summary(modelo2)
previsao2 <- predict(modelo2,iris_teste)
table(previsao2,iris_teste$Species)



# Função de Kernel: linear
modelo3 <- svm(Species ~ . , data = iris_treino, kernel = "linear")
summary(modelo3)
previsao3 <- predict(modelo3,iris_teste)
table(previsao3,iris_teste$Species)



# Função de Kernel: sigmóide
modelo4 <- svm(Species ~ . , data = iris_treino, kernel = "sigmoid")
summary(modelo4)
previsao4 <- predict(modelo4,iris_teste)
table(previsao4,iris_teste$Species)

# Conclusão: O melhor modelo foi o com kernel linear (2 erros)

