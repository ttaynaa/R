##################################################################
##  script para organizar os dados e plotar uma boxplot com    ###
##  pontos sobrepostos                                         ###
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

#Removendo objetos da memória
rm(list = ls())

#Carregandos os pacotes
library(dplyr)     # Para manipulação de dados
library(lubridate) # Para manipulação de datas
library(ggplot2)   # Para plotar os dados
library(fields)    # Fornece paleta de cores 

setwd('C:/Users/merce/OneDrive/Documents/MEGA/work/banners/box_plot_pontos/')

##############################################
### Lendo e manipulando os dados      ########
##############################################

temp <- read.table('dados/dados.dat') %>% #Leitura dos dados
  select(num_range('V',2:5)) %>%  #Selecionando coluna de dados
  setNames(c('ano','mes','dia','temp')) %>% #Definindo os nomes das colunas
  mutate(data = ymd(paste(ano,mes,dia))) %>%  # Criando a coluna data 
  filter(ano==2000) # Selecionando o ano 2000

ggplot(temp,aes(x=month(data,abbr = T,label = T),y=temp))+ #Definindo os dados e mapeando 
  geom_boxplot(outlier.shape = NA)+ # Plotando o boxplot
  geom_jitter(aes(fill=temp), #Adicionando os pontos e colorindo em função da temperatura
              position=position_jitter(width = .2), # Ajustando o espalhamento horizontal dos dados
              shape=21, #Definindo o tipo de pono
              col='grey30', #Definindo cor de contorno dos pontos
              alpha=.5)+ # Definindo a opacidade dos pontos
  labs(y='Temperatura do Ar [°C]', #Definindo título do eixo y
       x=NULL, #Definindo título do eixo x
       fill='[°C]')+ #Definindo título da legenda 
  scale_fill_gradientn(colours = tim.colors(20))+ # Defindo scala de cores para os pontos
  theme_bw()+ #Definindo tema 
  theme(panel.grid.major.x = element_blank()) #Apagando as grades verticais do gráfico

#############################
## Salvando os dados    #####
#############################

ggsave(filename = 'figuras/boxplot_pontos.png',
       width = 7.02,
       height = 3.55)
