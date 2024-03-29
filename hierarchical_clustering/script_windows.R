##################################################################
## script para fazer an�lise de cluster e visualizar os         ##
## resultados                                                   ##
##                                                              ##
## desenvolvido por: Mercel Santos                             ###
## contato: contato@mercelsantos.com                           ###
## instagram: @mercelsantos                                    ###
##################################################################

#Removendo objetos da mem�ria
rm(list = ls())

####################################################
## Carregando os pacotes                         ###
##                                               ###
## certifique-se de que todos os pacotes         ###
## usados abaixo est�o instalados                ###
####################################################
library(factoextra) #[1.0.7] Para visualizar resultados de an�lise de multivariada 


####################################
## Lendo e organizando os dados ####
####################################

setwd('C:/Users/merce/OneDrive/Documents/MEGA/work/banners/hierarchical_clustering') #Definindo diret�rio de trabalho
dados <- read.csv('dados/cond_social.csv') #Lendo os dados
row.names(dados) <- dados$estado #Nomeando Linhas
dados$estado <- NULL #Removendo a coluna de dados "estado"

###########################################
## Processando e visualizando os dados ####
###########################################

dados.n <- scale(dados) #Normalizando os dados
dista <- dist(dados.n)  #Determinando a dist�ncia euclidiana entre os casos analisados

fviz_dist(dista,gradient = list(low="blue", # Plotando a matriz de dist�ncia
                                mid="white",
                                high="red"))
#Salvando o gr�fico 
ggsave(filename = "figs/distancia.png", #nome do arquivo
       height = 4.68, #Altura
       width = 7.07,  #Largura
       scale = .9) #Escala

#Determinando a �rvore hier�rquica usando o m�todo Ward???s minimum variance
arvore.h <- hclust(dista,method = "ward.D2")

p.dendo <- fviz_dend(arvore.h, #�rvore hier�rquica
                     k=3, #N�mero de clusters
                     labels_track_height = 11, #Ajuste da margem para conter os r�tulos do gr�fico 
                     ylab = "Dist�ncia", #R�tulo do eixo y
                     main = "Dendrograma",#T�tulo do gr�fico
                     horiz = FALSE, #Definindo a orienta��o do dendrograma.Nesse caso, vertical 
                     ggtheme = theme_classic())#Definindo o tema do gr�fico (temas do pacote ggplot2)

#Salvando o gr�fico 
ggsave(filename = 'figs/dendrograma.png', #nome do arquivo
       height = 4.68, #Altura
       width = 7.07,  #Largura
       scale = .9) #Escala

#Definindo o n�mero de clusters e inserindo o resultado nos dados originais 
dados$grupos <- cutree(arvore.h,k=3) #k=3 significa que 3 clusters foram escolhidos 

#Visualizando os clusters
p.grupos <- fviz_cluster(list(data=dados,cluster=dados$grupos),#Definindo os dados
                         # choose.vars = c("t.envelh","idh"), #Escolhendo as vari�veis dos eixos x e y respectivamente
                         repel = T,#Evita sobreposi��o de pontos
                         palett="Dark2", #Definindo a paleta de cores do pacote RColorBrewer
                         ggtheme = theme_minimal(),#Definindo o tema do pacote ggplot2
                         show.clust.cent = FALSE, #Para n�o mostrar o centro do cluster
                         main = "Grupos",#T�tulo do gr�fico
                         xlab = "PCA 1", #T�tulo do eixo x
                         ylab = "PCA 2")+#T�tulo do eixo y
  labs(fill="Grupos")+ #T�tulo da escala de cores (fill)
  guides(col=FALSE,shape=FALSE)#removendo os t�tulos das cores e dos tipos de pontos

#Salvando o gr�fico 
ggsave(filename = 'figs/grupos.png', #nome do arquivo
       height = 4.68, #Altura
       width = 7.07,  #Largura
       scale = .9) #Escala



