

# GRÁFICO DE VELAS - interativo 

# Você pode criar gráficos candlestick/OHLC com a dyCandlestickfunção. Por exemplo:

library(xts)
data(sample_matrix) # dados
m <- tail(sample_matrix, n = 32)
dygraph(m) %>%
  dyCandlestick()


# Os gráficos de velas usam as quatro primeiras séries de dados para plotar, 
# o restante da série de dados (se houver) é renderizado com plotter de linha:

m <- cbind(m, apply(m[, 1:3], 1, mean))
colnames(m)[5] <- "Mean"
dygraph(m) %>%
  dyCandlestick()

# Você também pode usar compresso argumento de função para compactar os dados do gráfico 
# anualmente, trimestralmente, mensalmente, semanalmente ou diariamente, dependendo 
# do nível de zoom do gráfico atual para evitar o estouro das barras do gráfico:

library(xts)
data(sample_matrix)
dygraph(sample_matrix) %>%
  dyCandlestick(compress = TRUE)



sample_matrix

