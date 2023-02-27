
# REGRESSÃO LINEAR 
#
# dados : conjunto de dados "mtcars" utilizado no exemplo, as informações são 
#         oriundas de testes realizados em veículos nos Estados Unidos na 
#         década de 1970, quando o sistema de medição de eficiência de combustível 
#         em uso nos EUA era baseado em milhas por galão.
#
# Autora: Schênia Taynna

# Carregando o conjunto de dados
data(mtcars)

# Ajustando o modelo de regressão
modelo <- lm(mpg ~ wt , data = mtcars)

# Imprimindo os resultados do modelo
summary(modelo)

# Criando um gráfico de dispersão do mpg versus wt
plot(mtcars$wt, mtcars$mpg, xlab = "Peso", 
     ylab = "Eficiência de combustível", 
     main = "Eficiência de combustível versus peso")
abline(modelo, col = "red")

# Criando um gráfico de resíduos do modelo
plot(modelo, which = 1)


