

plot(1:10, 1:10, type = "n", ann = TRUE, axes = TRUE, main = "Bandeira do BRASIL")
rect(0, 0, 11,11, col = "darkgreen") # Retângulo verde 
polygon(c(5.5, 0.8,5.5,10.2), c(1.5, 5.5,9.5, 5.5), col = "yellow") # Triângulo amarelo
symbols(5.5, 5.5, 1, bg = rgb(0, 0, 1), add = TRUE) # Círculo azul 
rect(3.5, 5.23, 7.46, 5.75, col = "white") # Retângulo branco com a frase 
text(5.5, 5.5, "ORDEM E PROGRESSO", col = "darkgreen") # tamanho da fonte, frase e cor da frase
points(5.92, 5.98, pch = 8, col = "white") # Estrela  acima da faixa 
points(c(4, 4.1, 4.3, 4.6, 5,5.5,4.8,4.8,5,6,5.5,5.5,5.2,5.3,5.7,7.2,7.1,6.9,6.9,6.7,6.5,
+     6.5,6.5,6.2,6.1,5.9), c(4.9, 4, 4.5, 4.8,5.1,3.3,4.5,4.1,3.7,5,4.7,4,4.5,4.2,4.4,5,4.5,
+     4.8,4.2,4.1,3.6,4,4.5,4,3.8,4),pch = 8,col = "white")




# bg = rgb(0, 0, 1) é a cor azul 

# polygon(c(5.5, 1, 6,9.8), c(1, 5.5, 8.8, 5.5)
# o primeiro vetor é os ponto da aresta do eixo x, já o segundo vetor é o ponto da aresta no eixo y

# para melhor visualização dos eixo utilize:
# ann = TRUE, axes = TRUE
# se desejar apagar os eixos substitua TRUE por FALSE

# Se desejar retirar as legendas dos eixos utilize, depois do comando main: xlab="",ylab = ""








