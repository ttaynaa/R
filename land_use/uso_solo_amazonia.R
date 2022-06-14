rm(list=ls())

library(raster)   # Para leitura e manipulação de dados raster
library(maptools) # Para leitura de shapefiles
library(fields)   # Para uso de paletas de cores
library(maps)     # Fornece o mapa mundi

############################
##  Lendo o arquivo      ###
############################

diret <- '/home/mercel/mega/work/land_use/'
land.use <- raster(paste0(diret,"LCType.tif"))

##########################################
## Carregando função para gerar escala ###
##########################################
source(paste0(diret,"image_scale.r"))

########################################
##  Cortando a região desejada       ###
########################################

e <- extent(-78, -45, -15, 15)
land.use.sa <- crop(land.use, e)

############################
##  Plotando os dados    ###
############################

graphics.off()

png(paste0(diret,"fig/uso_solo_amazonia.png"),width = 464,height = 366,res = 100)

srat <- 7
nf <- layout(matrix(c(1,2),1,2),heights=c(srat,srat),widths=c(20,3))
zbreaks <- c(-0.5,seq(0.5,16.5,by=1))
mypal <-  c("cornflowerblue","darkgreen","springgreen4","olivedrab4",
            "seagreen2","greenyellow","coral2","burlywood1",
            "darkolivegreen2","tan1","tan3","royalblue3",
            "yellow1","red4","sienna","snow1",
            "snow4")

par(mar=c(2.5,4,.8,.3))
image(land.use.sa,col =mypal,
      xlab="",ylab="Latitude", breaks=zbreaks,
      xaxs="i", yaxs="i",
      xaxt="n", yaxt="n")

y.labels <- c("15.0°S","9.0°S ","3.0°S","3.0°N","9.0°N","15°N")
x.labels <- c("76.0°W","70.0°W","64°W","58.0°W","52.0°W","46.0°W")
axis(2, at=seq(-15, 15, length.out = 6),labels = y.labels, las=2,tck=-0.015,hadj=.8,cex.axis=1.0)
axis(1, at=seq(-76, -46,length.out = 6),labels = x.labels, las=1,tck=-0.015,padj=-0.9,cex.axis=1.0)
mtext("Longitude",1,padj=2,cex.axis=1.0)


#adicionado os limites da bacia amazônica
amazon <- readShapeLines(paste0(diret,'amazon_shape/amazlm_1608.shp'))
coord <- as.matrix(coordinates(amazon)[[1]][[1]])
lines(x=coord[,1],y=coord[,2],col='black',lwd=2.0)
box()
map(database = "world",add=T,interior =FALSE)

#adicionado a escala
par(mar=c(2.5,0,.8,2)) 
my.labs <- c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q")
image.scale(breaks=zbreaks,col=mypal,axis.pos=4,add.axis=FALSE)
axis(4,at=seq(0,16, by=1),labels = my.labs,las=2,tck=-0.25,hadj=0.5,padj=.4,cex.axis=1.)

graphics.off()


