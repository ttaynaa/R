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
library(geobr)      #[1.3]   Para acessar os dados do IBGE
library(dplyr)      #[0.8.5] Para manipula��o de dados
library(ggspatial)  #[1.0.3] Para inserir elementos de mapas [escala e orienta��o]
library(ggvoronoi)  #[0.8.3] Para criar um diagrama de voronoi
library(sf)         #[0.9.2] Para manipula��o de dados espaciais vetoriais 
library(RColorBrewer) #[1.1.2] Para acessar paleta de cores  

#removendo vari�veis
rm(list = ls())

#######################################
#### Lendo e organizando os dados #####
#######################################

#Definindo diret�rio de trabalho
setwd('C:/Users/merce/OneDrive/Documents/MEGA/work/banners/analise_clusters/')

# Lendo arquivo csv
dados <- read.csv('dados/precip_mensal.csv')

#Selecionando coluna de dados 
dados.ok <- dados[,-seq(1,4)]

#usando os c�digos das esta��es de medidas para renomear as linhas 
row.names(dados.ok) <- dados$code

#Acessando dados do limite territorial brasileiro
br <- read_country() 

#######################################
#### Preparando os dados          #####
#######################################

#Normalizando os dados
dados.n <- scale(dados.ok)

#Determinando a dist�ncia euclidiana
dist <- dist(dados.n)

#criando a �rvore hier�rquica 
h.just <- hclust(dist,"ward.D2")

#Selecionando 4 clusters
clusters <- cutree(h.just,4)

#Inserindo os clusters no conjunto de dados
dados$clusters <- as.factor(clusters)

##############################
#### Plotando o resultado  ###
##############################

#Plotando o mapa de pontos
ggplot()+
  geom_point(data=dados,aes(x=Longitude,y=Latitude,
                            fill=clusters,col=clusters),
             shape=21)+ #Adicionando os pontos aos mapas #
  geom_sf(data=br,fill="transparent")+ #Inserindo o limite territorial brasileiro
  annotation_scale(location="bl")+ # Inserindo a escala do mapa
  annotation_north_arrow(location="tr", #Inserindo a orienta��o do mapa
                         style = north_arrow_nautical(), #Estilo da orienta��o
                         height =unit(1.8,"cm"), #Altura da orienta��o 
                         width = unit(1.8,"cm"))+#Largura da orienta��o 
  theme_minimal()+ #Definindo o tema
  labs(x=NULL,y=NULL,fill=NULL,col=NULL)+ #Apagando t�tulos dos eixos e das legendas
  scale_color_manual(values=brewer.pal(5,"Dark2"), #Definindo cores e r�tulos da escala de cores
                     labels=c("Grupo1","Grupo2","Grupo3","Grupo4"))+
  scale_fill_manual(values=brewer.pal(5,"Dark2"),
                    labels=c("Grupo1","Grupo2","Grupo3","Grupo4"))+
  theme(legend.position = 'top')+ #Inserindo legenda no top do mapa
  guides(fill=guide_legend(override.aes = list(size=3))) #Alterando o tamanho dos pontos da legenda

#Salvando o mapa de pontos
ggsave('/home/mercel/Desktop/mapa1.png',scale = .9)

#Convertendo o objeto sf em sp
br.sp <- as_Spatial(br$geom)

ggplot() +
  geom_voronoi(data = dados,        #Criando um diagrama com as esta��es de medidas
               aes(x = Longitude,   #Colorindo os pontos com valores dos clusters
                   y = Latitude, 
                   fill=clusters),
               outline = br.sp) +   
  geom_sf(data=br,fill="transparent")+#Inserindo o limite territorial brasileiro
  annotation_scale(location="bl")+  #Inserindo a escala do mapa
  annotation_north_arrow(location="tr", #Inserindo a orienta��o do mapa
                         style = north_arrow_nautical(),#Estilo da orienta��o
                         height =unit(1.8,"cm"), #Altura da orienta��o
                         width = unit(1.8,"cm"))+ #Largura da orienta��o 
  scale_fill_manual(values = brewer.pal(5,"Dark2"))+ #Definindo cores e r�tulos da escala de cores
  theme_minimal()+#Definindo o tema
  labs(title = "Regi�es Homog�neas",
       x=NULL,y=NULL) #Definindo t�tulos dos eixos e das legendas

#Salvando o mapa das regi�es homog�neas
ggsave('/home/mercel/Desktop/mapa2.png',scale = .9)
