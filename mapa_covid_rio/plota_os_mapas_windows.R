##################################################################
##  script para leitura e plotagem dos dados de covid-19 para o ##
##  munícipio do Rio de Janeiro.Fonte:                          ##
##  https://covidporcep.rio.br e Painel Rio COVID-19            ##
##                                                              ##
##                                                              ##
## desenvolvido por: Mercel Santos                             ###
## contato: contato@mercelsantos.com                           ###
## instagram: @mercelsantos                                    ###
##################################################################

rm(list = ls())     #Remove todos os objetos da memória

####################################################
## Carregando os pacotes                         ###
##                                               ###
## certifique-se de que todos os pacotes         ###
## usados abaixo estão instalados                ###
####################################################

library(ggplot2) #Para plotar dados
library(ggspatial) #Para inserir escala em mapas
library(geobr)     #Para acessar dados do IBGE
library(sf)        #Para manipulação de dados espaciais
library(dplyr)     #Para processamento de dados
library(ggmap)     #Para acessar mapas base (similar ao google)
library(patchwork) #Para arranjar os gráficos facilmente
library(RColorBrewer) # Para acessar paletas de cores


####################################################
##### Acessando e manipulandoo os dados      #######
####################################################

#Definindo diretório de trabalho
setwd('C:/Users/merce/OneDrive/Documents/MEGA/work/banners/mapa_covid_rio')

#Lendo os dados
casos.covid <- read.csv('dados/dist_espacial_covid_ate_19_29_julho_2020.csv')

#convertendo o pontos para classe sf
casos.covid.sf <- casos.covid %>% 
  st_as_sf(coords=c('lon','lat'),
           crs=4674)
#Acessando os limites territoriais do município do Rio de Janeiro
rio.muni <- read_municipality(code_muni = 3304557)

#Definindo área do mapa 
area.rio <- c(left=-43.8,
              bottom=-23.114037,
              right=-43.14,
              top=-22.763898)

#Acessando o mapa base da Stamen Maps
rio.mapa <- get_stamenmap(bbox=area.rio,zoom = 12,
                          maptype = 'terrain')

##################################
##   Plotando os dados      ######      
##################################

#Definindo cor da borda do mapa municipal
bordas.map <- adjustcolor( "darkslateblue", alpha.f = 0.55)


p1 <- ggmap(rio.mapa)+ #Plotando o mapa base
  geom_sf(data=casos.covid.sf,inherit.aes = FALSE, #Plotando os pontos e definindo cores e tamanhos
          alpha=0.1,size=0.5,shape=21,col="#B5263C",fill='#F49A72')+
  labs(x=NULL,y=NULL,
       title = "Distribuição espacial dos casos de Covid-19",
       subtitle = "Atualizado em 29/07/2020")+ #Apagando rótulos dos eixos e definindo títulos
  geom_sf(data=rio.muni,fill='transparent',col=bordas.map, 
          inherit.aes = FALSE) # Adicionando o limite territorial do município

#Definindo as cores das bordas e do interior dos hexabins
bordas.hex <- adjustcolor( "gray40", alpha.f = 0.2)
cores.hex  <- adjustcolor(brewer.pal(9,"YlOrRd"),alpha.f = 0.8)

p2 <- ggmap(rio.mapa)+ #Plotando o mapa base
  coord_cartesian()+ # convertendo a projeção do mapa para cartesiana
  geom_hex(data = casos.covid,aes(x = lon,y = lat), #Plotando os hexágonos e definindo cores e tamanhos
           col=bordas.hex,bins=90)+
  geom_sf(data=rio.muni,fill='transparent',col=bordas.map, 
          inherit.aes = FALSE)+# Adicionando o limite territorial do município
  labs(x=NULL,y=NULL,
       fill='Frequência\nAcumulada')+ #Apagando rótulos dos eixos e definindo título da legenda
  scale_fill_gradientn(colours =cores.hex,
                       trans='sqrt')+ #Definindo a paleta de cores
  theme(legend.position = "bottom", #Definindo posição da legenda
        legend.title = element_text(size = 10), # Definindo tamanho do texto da legenda
        legend.text.align = unit(.6, "npc"),  # Definindo margem da legenda
        legend.key.size =  unit(.09, "npc"),  # Definindo tamanho da legenda
        legend.key.height =unit(.03, "npc"))  # Definindo a altura legenda

#Arranjando os gráficos um abaixo do outro
p3 <- p1/p2

#Adicionando a escala e a orientação do mapa
p3+annotation_scale(height  = unit(.08,'cm'))+
  annotation_north_arrow(style = north_arrow_nautical,
                         location="tl",)

#Salvando o mapa
ggsave(filename = 'figs/mapas_covid_rio.png',
       scale = .9,
       height=7.25,
       width = 6.82)

