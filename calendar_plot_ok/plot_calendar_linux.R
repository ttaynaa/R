##################################################################
##  script para plotar um gráfico calendário com a série       ###
##  histórica do dólar                                         ###
##                                                             ###
## desenvolvido por: Mercel Santos                             ###
## contato: contato@mercelsantos.com                           ###
## instagram: @mercelsantos                                    ###
##################################################################

rm(list = ls())

#########################################
####  carregando os pacotes         #####
#########################################

library(dplyr) # Para manipulação de dados 
library(lattice) # Para plotar os dados 
library(chron) # Para trabalhar com datas
library(grid)  # Para Manipular layout
library(fields) # Fornece paleta de cores

#########################################
####  Diretório e arquivo           #####
#########################################

dir <- '/home/mercel/mega/work/banners/calendar_plot2/'
arq <- 'dadosDolar2.csv'
completo <- paste0(dir,arq)

source(paste0(dir,"calendarHeat.R"))

#########################################
####  Lendo e processando os dados  #####
#########################################

dados <- read.csv(completo,dec = ',') %>% #Lendo os dados cujo decimal está separado por vírgula
  mutate(data=as.Date(Data,format('%d.%m.%Y'))) %>%  # Convertendo coluna com caracteres para datas
  select(data,Abertura) # Selecionando colunas 

#########################################
####  Plotando e Salvando           #####
#########################################

tiff(filename = paste0(dir,"/figs/figura2.tiff"),width = 1000,height = 1000,res=150)
  calendarHeat(dados$data, dados$Abertura, 
               title=NULL,color=tim.colors(20))
graphics.off()

