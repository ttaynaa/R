############################################################
## script para plotar uma pir�mide et�ria animada para   ###
## o Brasil                                              ###
##                                                       ###
## Desenvolvido por Mercel Santos:                       ###
## email: contato@mercelsantos.com                       ###
## instagram: @mercelsantos                              ###
############################################################

rm(list=ls())

#########################################
####  Pacotes                       #####
#########################################

library(dplyr) # [Vers�o 0.8.5] Para manipular os dados
library(tidyr) # [vers�o 1.0.2] Para manipula��o de dados
library(ggplot2) #[vers�o 3.3.2] Para plotar os dados 
library(gganimate) #[vers�o 1.0.5] Para gerar anima��o

#Definindo diret�rio de trabalho
setwd("C:/Users/merce/OneDrive/Documents/MEGA/work/banners/piramide_etaria")

dados <- read.csv('dados/WPP2019_PopulationByAgeSex_Medium.csv') %>% # Lendo arquivo do tipo csv
  filter(Location=='Brazil') %>% #Selecionando linha de dados para regi�o Brasileira
  select(Time,AgeGrp,PopMale,PopFemale) %>% #Selecionando coluna de dados
  mutate(PopFemale=PopFemale*-1) %>%  #Tornando os valores Negativos para popula��o do Sexo Feminino 
  gather(sexo,populacao,c(PopMale,PopFemale)) %>% #Juntando os dados da popula��o dos Sexos Masculino e Feminino em uma �nica coluna
  mutate(sexo=as.factor(sexo)) #Convertendo os dados da coluna sexo para fator

#Estabelecendo os r�tulos do eixo y do gr�fico
rotulos.org <- c("0-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89","90-94","95-99","100+") 

meu.plot <- ggplot(dados,aes(populacao,AgeGrp,fill=sexo))+ #Definindo dados e colunas usados para plotar o gr�fico
  geom_bar(stat = "identity",col='grey90',lwd=.3)+ #Definindo o tipo de gr�fico (gr�fico de barras)
  geom_text(data=filter(dados,sexo=='PopFemale'), #Plotando os r�tulos na posi��o central do gr�fico
            aes(x=0,y=AgeGrp,label=AgeGrp),       
            col='grey20')+
  scale_y_discrete(limits=rotulos.org)+ #Definindo os limites do eixo y
  scale_x_continuous(limits = c(-10000,10000), #Definindo os limites do eixo x
                     breaks = c(-10000,-5000,0,5000,10000), #Definindo posi��es dos r�tulos 
                     labels = c('10000','5000','0','5000','10000'))+ #Definindo os r�tulos 
  labs(x="Popula��o", #Definindo os t�tulos dos gr�ficos e das legendas
       y=NULL,
       fill='',
       title='Distribui��o da Popula��o por Faixa Et�ria',
       subtitle = "Ano {closest_state}")+
  scale_fill_manual(values = c('#F680BE','#1F74B4'), # Definindo as cores de preenchimento das barras
                    labels =c('Feminino','Masculino'))+  # Definindo os r�tulos da legenda
  theme_minimal()+ #Definindo o tema
  theme(legend.position = 'top', #Definindo a posi��o da legenda
        legend.key.size = unit(5,'mm'), #Definindo o tamanho da legenda
        axis.text.y=element_blank())+ #Apagando os r�tulos do eixo Y
  transition_states(Time) #Definindo que a anima��o ser� gerada em fun��o do tempo (Anos)


anim <- animate(plot = meu.plot,duration = 15, #Gerando a anima��o e especificando a dura��o, dimens�es e resolu��o
                width=480,height=560,res=110)

#Salvando a anima��o
save_animation(file = 'animations/populacao_animado.gif',animation = anim)
