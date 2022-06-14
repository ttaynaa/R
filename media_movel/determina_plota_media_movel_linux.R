##################################################################
##  script para calcular e plotar a média móvel de 15 dias      ##
##  para os dados de temperatura do ar                         ###
##                                                             ###
## desenvolvido por: Mercel Santos                             ###
## contato: contato@mercelsantos.com                           ###
## instagram: @mercelsantos                                    ###
##################################################################

####################################################
## Carregando os pacotes                         ###
##                                               ###
## certifique-se de que todos os pacotes         ###
## usados abaixo estão instalados                ###
####################################################

library(ggplot2)  #Para ploltar dados
library(lubridate) # Para manipulação de datas
library(dplyr)     # Para manipulação de dados
library(zoo)       # Para análise de séries temporais     

#Definindo diretório de trabalho
setwd('/home/mercel/mega/work/banners/media_movel')


##############################################
##       lendo os dados                   ####
##############################################

dados <- read.csv('dados/temperatura.csv') %>% #Leitura do arquivo .csv
  mutate(data=ymd(paste0(ano,'-',mes,'-',dia))) %>%  #Criando a coluna data
  dplyr::select(data,t.ar) %>% # Selecionando colunas que serão utilizadas
  mutate(m.movel.15=rollmean(t.ar,k=15,fill=NA)) #Calculando média móvel de 15 dias

##############################################
##       plotando os dados                ####
##############################################

ggplot(dados,aes(data,t.ar))+ #Definindo dados e valores dos eixos
  geom_line(aes(col="Temperatura",lty="Temperatura"),lwd=.7)+ # Adcionando linha da temperatura
  geom_line(aes(y=m.movel.15,col="Média Móvel 15 dias",
                lty="Média Móvel 15 dias"),lwd=.7)+  # Adicionando linha da média movel 
  scale_color_manual(values = c('#BD4A46','#497DBA'))+ # Definindo cores das linhas
  scale_linetype_manual(values = c('longdash', 'solid'))+ # Definindo tipos de linhas
  labs(x=NULL,y='Temperatura do Ar (°C)',  #Títulos dos eixos
       col=NULL,
       linetype=NULL)+
  theme_minimal()+ #Definindo tema
  theme(legend.position = 'top') # Definindo posição da legenda


##############################################
##       salvando os dados                ####
##############################################

ggsave(filename = 'figs/media_movel.png',
       width = 6.71,
       height = 4.14,
       scale = .85)
