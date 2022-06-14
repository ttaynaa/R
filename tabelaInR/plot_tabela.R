################################################
## script que  plota  tabelas usando o       ###
## pacote ggpubr                            ####
##                                          ####
## desenvolvido por: Mercel Santos          ####
## contato: contato@mercelsantos.com        ####
## instagram: @mercelsantos                 ####
## P.S.: as palavras nos cometarios estao   ####
## sem  acentos  para  evitar               ####
## incompatibilidade de editores de texto   ####
################################################

rm(list = ls())

library('ggpubr')
library(dplyr)
library(lubridate)
library(stringr)

dir <- '/home/mercel/mega/work/banners/tabelaInR/'

dados <- read.table(paste0(dir,"dados_porto_alegre.txt"),
                    header = T) %>%
                    mutate(meses=seq(as.Date("2019-01-28"),
                           length.out = 12,by="month") %>% 
                           month(label = TRUE,abbr=FALSE) %>% 
                           str_to_title())%>%
                    select(meses,Tmed,Prec,Ur) 


colnames(dados) <- c("Meses","Temp. Média (°C)","Precipitação (mm)","Umid. Relativa (%)")

meu.col.style <- colnames_style(color = "white",
                          face = "bold",
                          size = 13,
                          fill = "#235ea7",
                          linewidth=1)

meu.tbody.style <- tbody_style(color = c("gray16","gray16"),
                               face = "plain",
                               size = 12,
                               fill = c("gray98","#b5cbe5"),
                               linewidth=1,
                               linecolor ="gray98")

t <- ggtexttable(dados,row=NULL,theme=ttheme(
  padding = unit(c(3, 4), "mm"),
  tbody.style = meu.tbody.style,
  colnames.style = meu.col.style
))

png(filename = paste0(dir,"figs/tabela.png"),width = 1080,height = 1080,res = 150)
t
graphics.off()


