##################################################################
##  script para processar dados espaciais e gerar uma anima��o ###
##  do �ndice de Vegeta��o Normalizada para Regi�o Nordeste    ###
##                                                              ##
##                                                              ##
## desenvolvido por: Mercel Santos                             ###
## contato: contato@mercelsantos.com                           ###
## instagram: @mercelsantos                                    ###
##################################################################

####################################################
## Carregando os pacotes                         ###
##                                               ###
## certifique-se de que todos os pacotes         ###
## usados abaixo est�o instalados                
rm(list = ls())     #Remove todos os objetos da mem�ria

library(raster) # Para processar dados espaciais do tipo raster
library(geobr)  # Para acessar a base de dados do IBGE
library(dplyr)  # Para processar dados espaciais 
library(tidyr)  # Para processar dados espaciais 
library(lubridate) # Para manipula��o de datas 
library(sf)     # Para processar dados espaciais vetoriais
library(ggplot2) #Para plotar os dados
library(animation) # Para gerar anima��o
library(stringr)   #Para manipula��o de caracteres
library(gridExtra) #Para arranjar os gr�ficos
library(cowplot)  #Para arranjar os gr�ficos
library(ggspatial) #Para inserir a escala e a orienta��oo do mapa


###################################
## Defini��o do diret�rio     #####
###################################

setwd('C:/Users/merce/OneDrive/Documents/MEGA/work/banners/ndvi_animado')


#Criando uma lista com todos arquivos tiff
lista.arq <- list.files(path = "dados/",pattern = "*.TIFF")

#Criando lista que ser� preenchida com dados do tipo raster
lista.rasters <- list()

#Lendo os arqruivos e preenchendo a lista
for (i in 1:length(lista.arq)) {
  lista.rasters[[i]] <- raster(paste0('dados/',lista.arq[i]))
}

#Empilhando os rasters em um �nico objeto(RasterBrick)
ndvi.brick <- brick(lista.rasters)


######################################################
#####    Acessando os dados do IBGE         ##########
######################################################

nordeste <- read_region() %>% #Baixando os limites territoriais das regi�es brasileiras
  dplyr::filter(name_region=="Nordeste") # Selecionando a regi�o Nordeste

estados <- read_state(code_state = 'all') # Baixando os limites territoriais dos Estados brasileiros

ndvi.recorte <- crop(ndvi.brick,nordeste) #Recortando os rasters para extens�o do Nordeste
ndvi.nordeste <- mask(ndvi.recorte,nordeste) # Mascarando os rasters para o limite do Nordeste

ndvi.nordeste[ndvi.nordeste==99999]=NA  # Substituindo os valores n�o definidos por NA

names(ndvi.nordeste) <- names(ndvi.nordeste) %>% #Modificando os nomes originais dos rasters 
  substr(12,21)        


ndvi.df  <-  rasterToPoints(ndvi.nordeste) %>%  #Convertendo os rasters para pontos
  data.frame() %>%  #Convertendo de matrix para data.frame
  tidyr::gather(value = 'NDVI',key='data', contains("2019")) %>% # Simplificando o data.frame (reduzindo o n�mero de colunas)
  dplyr::mutate(data=str_sub(data, -10)) %>%  #recortando os caracteres para extrair as datas
  dplyr::mutate(data=ymd(data)) # Convertendo caracteres em datas

#Calculando a m�dia espacial para cada m�s
ndvi.medio <- ndvi.df %>%  
  dplyr::group_by(data) %>% #Agrupando os dados em fun��o da data
  dplyr::summarise(ndvi.m = mean(NDVI,na.rm=T)) # Calculando a m�dia


datas <- ndvi.df$data %>% unique() #Removendo as datas repetidas


######################################################
###    Plotando os dados e criando anima��o ##########
######################################################

minha.paleta <- c('#C22F1D','#DF7E54','#F8C690',
                  '#FFF9BA','#a7d700','#71c900','#0baa00')

saveGIF(ani.height=522,ani.width=569, #Salvando a anima��o como GIF
        ani.res=120,{
          
          for(i in 1:length(datas)){ #La�o para gerar a anima��o
            
            # Detalhe sobre o sistema de refer�ncia
            # https://epsg.io/4674
            
            ndvi.mes <- ndvi.df %>% 
              dplyr::filter(data==datas[i]) %>% #Selecionando os dados em fun��o da data
              dplyr::select(x,y,NDVI) %>%  # Selecionando colunas
              rasterFromXYZ(crs = '+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs ') %>% #Convertendo os dados para raster
              rasterToPolygons() %>% #Convertendo de raster para pol�gonos
              st_as_sf() #Convertendo o resultado para classe sf
            
            mes.atual <- month(datas[i],label = T,abbr = F) %>% str_to_title() #Extraindo o m�s da data e definindo a primeira letra com mai�scula 
            
            #Plotando o mapa
            ndvi.plot <- ggplot(ndvi.mes) + #Definindo os dados 
              geom_sf(data=estados,fill='transparent')+ #Plotando os Estados Brasileiros
              geom_sf(aes(fill=NDVI),col='transparent')+ #Plotando o NDVI
              geom_sf(data=nordeste,fill='transparent')+ #Plotando os limites para regi�o Nordeste
              coord_sf(xlim = c(-49, -33), ylim = c(-18.34849, -1.043977),
                       expand = F)+ #Limitando o plote para regi�o Nordeste
              scale_x_continuous(breaks = seq(-48,-34,4))+ # definindo n�mero e posi��o dos r�tulos do eixo x
              scale_y_continuous(breaks = seq(-15,-5,5))+  # definindo n�mero e posi��o dos r�tulos do eixo y
              scale_fill_gradientn(colours = minha.paleta, #Definindo elementos da escala de cores (paleta,limites e r�tulos)
                                   limits=c(0,1),breaks=seq(.1,.9,.2))+
              
              theme_minimal()+ #Definindo o tema
              labs(title = mes.atual) #Inserindo o t�tulo
            
            
            #extra�ndo a legenda para juntar ao painel (s� basta pegar uma vez)
            if(i==1) legenda <- get_legend(ndvi.plot)
            
            #A
            ndvi.plot <- ndvi.plot + 
              theme(plot.title  = element_text(hjust = 0.5),
                    legend.position = 'none' ) #Centralizando o t�tulo
            
            cor.preench <- adjustcolor(col = "#D9D9D9",alpha.f = .4) #Tornando a cor transparente 
            
            #Plotando o gr�fico da varia��o sazonal m�dia do NDVI
            sazonal.plot <- ggplot(ndvi.medio,aes(data,ndvi.m))+ #Definindo os dados
              geom_ribbon(aes(ymax=ndvi.m,ymin=.4), #Plotando a sombra 
                          fill=cor.preench)+
              geom_line(col="#D9D9D9")+ #Adicionando a linha
              geom_point(data=ndvi.medio[i,],aes(data,ndvi.m,fill=ndvi.m),
                         pch=21,size=2.5,col="grey30")+ #Adicionando os pontos
              scale_fill_gradientn(colours = minha.paleta, #Definindo as cores dos pontos (paleta,limites e r�tulos)
                                   limits=c(0,1),breaks=seq(.1,.9,.2))+
              scale_y_continuous(limits = c(0.4,0.75))+ #Ajustando os limites do eixo y
              coord_cartesian(expand = FALSE)+ #Eliminando o espa�o entre os dados e eixo y
              labs(y='NDVI',x=NULL)+ #Definindo os t�tulos do r�tulos
              theme_minimal()+ #Definindo o tema
              theme(legend.position = 'none')
            
            
            #Juntando os gr�ficos
            plot.juntos <- grid.arrange(ndvi.plot, sazonal.plot,legenda,ncol=2,nrow=2,
                                        layout_matrix=cbind(c(1,2),c(3,3)),
                                        widths=c(6,1),
                                        heights=c(3,1))
            #imprimindo os dados
            print(plot.juntos)
          }
        })




