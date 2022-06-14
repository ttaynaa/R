
#devtools::install_github("thomasp85/transformr")
################################################
## script para plotar grafico animado dos    ####
## casos de Covid-19 ocorridos no Brasil,   ####
## Italia e Ira                             ####
##                                          ####
## desenvolvido por: Mercel Santos          ####
## contato: contato@mercelsantos.com        ####
## instagram: @mercelsantos                 ####
################################################


library(ggplot2)
library(gganimate)
library(transformr)
library(ggimage)
library(plyr)
library(dplyr)



dir <- '/home/mercel/mega/work/banners/covidAnimation/'

dados <- read.csv(paste0(dir,"full_data.csv")) # lendo os dados

brasil <- dados %>% 
          filter(location=='Brazil') %>% 
          slice(which(new_cases>0)[1]:length(date)) %>% 
          mutate(dia=1:length(date),brasil=new_cases) %>% 
          select(dia,brasil)


italia <- dados %>% 
  filter(location=='Italy') %>% 
  slice(which(new_cases>0)[1]:length(date)) %>% 
  mutate(dia=1:length(date),italia=new_cases) %>% 
  select(dia,italia)

iran <- dados %>% 
  filter(location=='Iran') %>% 
  slice(which(new_cases>0)[1]:length(date)) %>% 
  mutate(dia=1:length(date),iran=new_cases) %>% 
  select(dia,iran)


juntos <- join(brasil,italia,by="dia") %>% 
              join(iran,by="dia")


paises <- data.frame(
            brasil = 'figs/brasil.png',
            italia = 'figs/italia.png',
            iran   = 'figs/eu-corri.png',
            eua    = 'figs/eua.png'
          )

p <- ggplot(juntos,aes(x=dia,y=italia))+
    geom_path(col="#00AFBB",lwd=1.2)+
    geom_image(aes(col=NULL),image=paste0(dir,paises$italia))+
    geom_path(aes(y=brasil),col="#E7B800",lwd=1.2)+
    geom_image(aes(y=brasil,col=NULL),image=paste0(dir,paises$brasil))+
    geom_path(aes(y=iran),col="#FC4E07",lwd=1.2)+
    geom_image(aes(y=iran,col=NULL),image=paste0(dir,paises$iran))+
    labs(x='Dias após primeiro registro do Covid-19',
         y="Número de Casos",
         color=NULL)+
    transition_reveal(dia)+
    theme_bw()+
    theme(legend.position = "top",
          axis.text.x = element_text(color = "grey10", size = 15),
          axis.text.y = element_text(color = "grey10", size = 15),
          axis.title.x = element_text(color = "grey10", size = 18),
          axis.title.y = element_text(color = "grey10", size = 18))+
  geom_image(aes(x=2,y = 1450),image=paste0(dir,paises$italia))+
  geom_image(aes(x=2,y = 1300),image=paste0(dir,paises$brasil))+
  geom_image(aes(x=2,y = 1150),image=paste0(dir,paises$iran))


annotation <- data.frame(
  x = rep(4.15,3),
  y = c(1450,1300,1150),
  label = c("Itália", "Brasil","Irã")
)

  p <- p+geom_text(data=annotation, aes( x=x, y=y, label=label),                 , 
            color="gray10", 
            size=5 )


animate(p, 100, 10,width=664,height=373)

anim_save(paste0(dir,"covidAnimation.gif"))
