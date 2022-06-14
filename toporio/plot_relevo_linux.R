##################################################################
##  script para leitura, processamento e plotagem de dados      ##
##  de topografia (fonte: SRTM) para o estado do Rio de Janeiro ##
##                                                              ##
##                                                              ##
## desenvolvido por: Mercel Santos                             ###
## contato: contato@mercelsantos.com                           ###
## instagram: @mercelsantos                                    ###
## P.S.: as palavras nos cometarios estao sem  acentos  para   ###
## evitar incompatibilidade de editores de texto               ###
##################################################################

####################################################
## Carregando os pacotes                         ###
##                                               ###
## certifique-se de que todos os pacotes         ###
## usados abaixo estao intalados                 ###
####################################################

rm(list = ls())

library('raster')    # Ler arquivos geotiff 
library('rasterVis') # para visualizacao de raster
library('geobr')     # Acessa os dados dos do IBGE
library('fields')    # disponibiliza paleta de cores 
library('geosphere') # facilita os calculos com coordenadas 
library("latticeExtra") # funcoes extras do pacote lattice


####################################################
## Funcao converte distancia em metros          ####
## para graus de longitude                      ####
##                                              ####
## dist = distancia                             ####
##                                              ####
## lat  = latitude de referencia (latitude      ####
## do ponto onde a longitude sera convertida,   ####
## o valor da longitude em metros depende da    ####
## latitude local)                              ####
####################################################

metro_para_long <- function(dist,lat) {
  metros_grau <- distGeo(c(0,lat),c(1,lat))
  graus <- dist/metros_grau
  return(graus)
}

##########################################
## Criando uma nova paleta de cores     ##
##########################################

col <- c('#bcd2a4','#89d2a4','#28a77e','#90b262',
         '#ddb747','#fecf5b','#da9248','#b75554',
         '#ad7562','#b8a29a','#9f9e98')

relevo.col <- colorRampPalette(col)

##########################################
##    Lendo os dados                    ##
##########################################

rj <- read_state(code_state = "RJ",year=2010) # limites para o estado do RJ

dir <- '/home/mercel/mega/work/banners/srtm/TopografiaRioDeJaneiro/'

dados <- raster(paste0(dir,"data/srtm_28_17.tif")) # leitura do arquivo


##########################################
##    manipulando os dados              ##
##########################################

top <- crop(dados,rj) # selecionando a area que abrange RJ

top.rj <- mask(top,rj) # selecionado dados nos limites do RJ

##########################################
##    plotando os dados                 ##
##########################################

# plotando top.rj (topografia)
p  <-  levelplot(top.rj,margin=FALSE,col.regions=relevo.col(101),
                 colorkey=list(space="right", height=0.8,width=1.7),
                 cuts=100,maxpixels = 1e5,
                 main="Mapa Topográfico")

#mapa dos limites do Estado do RJ
limites.rj <- latticeExtra::layer(sp.lines(as(rj$geom,Class = 'Spatial'),
                       col="black",lwd=.5)) 

# indicacao de norte  
ind.norte <- latticeExtra::layer(SpatialPolygonsRescale(layout.north.arrow(type = 1),
                        offset = c(-44.8,-21.25),
                        scale = c(.5,.3))) 


# Determinando a posicao da escala do mapa
compr.escal.metros <- 100000
lat.scale <- -23.3
long.i <- -42.4
compr.escal.graus <- metro_para_long(compr.escal.metros,lat.scale)
long.f <- long.i + compr.escal.graus

#definindo a escala do mapa
escala.map <- latticeExtra::layer(SpatialPolygonsRescale(layout.scale.bar(),
                                     fill=c("transparent","black"),
                                     offset = c(long.i,latitude),
                                     scale = c(compr.escal.graus,1.9))) 

#definindo o texto da escala
texto1 <-  latticeExtra::layer(sp.text(loc = c(long.i,-23.12), "0",cex=0.85))
texto2 <-  latticeExtra::layer(sp.text(loc = c(long.f,-23.12), "100 km",cex=0.85))


##########################################
##    salvando o mapa                   ##
##########################################

png(paste0(dir,'figs/topRJ.png'),width = 700,height = 520,res = 130)
    p + limites.rj + ind.norte +escala.map + texto1 + texto2
graphics.off()
