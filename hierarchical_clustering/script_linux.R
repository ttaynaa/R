##################################################################
## script para fazer análise de cluster e visualizar os         ##
## resultados                                                   ##
##                                                              ##
## desenvolvido por: Mercel Santos                             ###
## contato: contato@mercelsantos.com                           ###
## instagram: @mercelsantos                                    ###
##################################################################

#Removendo objetos da memória
rm(list = ls())

####################################################
## Carregando os pacotes                         ###
##                                               ###
## certifique-se de que todos os pacotes         ###
## usados abaixo estão instalados                ###
####################################################
library(factoextra) #[1.0.7] Para visualizar resultados de análise de multivariada 


####################################
## Lendo e organizando os dados ####
####################################

setwd('/home/mercel/mega/work/banners/hierarchical_clustering') #Definindo diretório de trabalho
dados <- read.csv('dados/cond_social.csv') #Lendo os dados
row.names(dados) <- dados$estado #Nomeando Linhas
dados$estado <- NULL #Removendo a coluna de dados "estado"

###########################################
## Processando e visualizando os dados ####
###########################################

dados.n <- scale(dados) #Normalizando os dados
dista <- dist(dados.n)  #Determinando a distância euclidiana entre os casos analisados

fviz_dist(dista,gradient = list(low="blue", # Plotando a matriz de distância
                                  mid="white",
                                  high="red"))
#Salvando o gráfico 
ggsave(filename = "figs/distancia.png", #nome do arquivo
       height = 4.68, #Altura
       width = 7.07,  #Largura
       scale = .9) #Escala

#Determinando a árvore hierárquica usando o método Ward’s minimum variance
arvore.h <- hclust(dista,method = "ward.D2")

p.dendo <- fviz_dend(arvore.h, #árvore hierárquica
                     k=3, #Número de clusters
                     labels_track_height = 11, #Ajuste da margem para conter os rótulos do gráfico 
                     ylab = "Distância", #Rótulo do eixo y
                     main = "Dendrograma",#Título do gráfico
                     horiz = FALSE, #Definindo a orientação do dendrograma.Nesse caso, vertical 
                     ggtheme = theme_classic())#Definindo o tema do gráfico (temas do pacote ggplot2)

#Salvando o gráfico 
ggsave(filename = 'figs/dendrograma.png', #nome do arquivo
       height = 4.68, #Altura
       width = 7.07,  #Largura
       scale = .9) #Escala

#Definindo o número de clusters e inserindo o resultado nos dados originais 
dados$grupos <- cutree(arvore.h,k=3) #k=3 significa que 3 clusters foram escolhidos 

#Visualizando os clusters
p.grupos <- fviz_cluster(list(data=dados,cluster=dados$grupos),#Definindo os dados
             # choose.vars = c("t.envelh","idh"), #Escolhendo as variáveis dos eixos x e y respectivamente
             repel = T,#Evita sobreposição de pontos
             palett="Dark2", #Definindo a paleta de cores do pacote RColorBrewer
             ggtheme = theme_minimal(),#Definindo o tema do pacote ggplot2
             show.clust.cent = FALSE, #Para não mostrar o centro do cluster
             main = "Grupos",#Título do gráfico
             xlab = "PCA 1", #Título do eixo x
             ylab = "PCA 2")+#Título do eixo y
             labs(fill="Grupos")+ #Título da escala de cores (fill)
             guides(col=FALSE,shape=FALSE)#removendo os títulos das cores e dos tipos de pontos
 
#Salvando o gráfico 
ggsave(filename = 'figs/grupos.png', #nome do arquivo
       height = 4.68, #Altura
       width = 7.07,  #Largura
       scale = .9) #Escala



