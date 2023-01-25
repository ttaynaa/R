
# Pacote necessário 
install.packages('dygraphs')
library(dygraphs)

Dados <- cbind(mdeaths, fdeaths) #base de dados 

# Gráfico
dygraph(Dados, 
        main = "Mortes por Doenças Pulmonares - Reino Unido - 1874-1979",
        ylab = "Número de Morets") %>%
  dySeries("mdeaths", color = "blue", label = "Homens") %>%
  dySeries("fdeaths", color = "green", label = "Mulheres") %>% 
  dyRangeSelector()


