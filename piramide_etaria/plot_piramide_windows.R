############################################################
## script para plotar uma pirâmide etária animada para   ###
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

library(dplyr) # [Versão 0.8.5] Para manipular os dados
library(tidyr) # [versão 1.0.2] Para manipulação de dados
library(ggplot2) #[versão 3.3.2] Para plotar os dados 
library(gganimate) #[versão 1.0.5] Para gerar animação

#Definindo diretório de trabalho
setwd("C:/Users/merce/OneDrive/Documents/MEGA/work/banners/piramide_etaria")

dados <- read.csv('dados/WPP2019_PopulationByAgeSex_Medium.csv') %>% # Lendo arquivo do tipo csv
  filter(Location=='Brazil') %>% #Selecionando linha de dados para região Brasileira
  select(Time,AgeGrp,PopMale,PopFemale) %>% #Selecionando coluna de dados
  mutate(PopFemale=PopFemale*-1) %>%  #Tornando os valores Negativos para população do Sexo Feminino 
  gather(sexo,populacao,c(PopMale,PopFemale)) %>% #Juntando os dados da população dos Sexos Masculino e Feminino em uma única coluna
  mutate(sexo=as.factor(sexo)) #Convertendo os dados da coluna sexo para fator

#Estabelecendo os rótulos do eixo y do gráfico
rotulos.org <- c("0-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89","90-94","95-99","100+") 

meu.plot <- ggplot(dados,aes(populacao,AgeGrp,fill=sexo))+ #Definindo dados e colunas usados para plotar o gráfico
  geom_bar(stat = "identity",col='grey90',lwd=.3)+ #Definindo o tipo de gráfico (gráfico de barras)
  geom_text(data=filter(dados,sexo=='PopFemale'), #Plotando os rótulos na posição central do gráfico
            aes(x=0,y=AgeGrp,label=AgeGrp),       
            col='grey20')+
  scale_y_discrete(limits=rotulos.org)+ #Definindo os limites do eixo y
  scale_x_continuous(limits = c(-10000,10000), #Definindo os limites do eixo x
                     breaks = c(-10000,-5000,0,5000,10000), #Definindo posições dos rótulos 
                     labels = c('10000','5000','0','5000','10000'))+ #Definindo os rótulos 
  labs(x="População", #Definindo os títulos dos gráficos e das legendas
       y=NULL,
       fill='',
       title='Distribuição da População por Faixa Etária',
       subtitle = "Ano {closest_state}")+
  scale_fill_manual(values = c('#F680BE','#1F74B4'), # Definindo as cores de preenchimento das barras
                    labels =c('Feminino','Masculino'))+  # Definindo os rótulos da legenda
  theme_minimal()+ #Definindo o tema
  theme(legend.position = 'top', #Definindo a posição da legenda
        legend.key.size = unit(5,'mm'), #Definindo o tamanho da legenda
        axis.text.y=element_blank())+ #Apagando os rótulos do eixo Y
  transition_states(Time) #Definindo que a animação será gerada em função do tempo (Anos)


anim <- animate(plot = meu.plot,duration = 15, #Gerando a animação e especificando a duração, dimensões e resolução
                width=480,height=560,res=110)

#Salvando a animação
save_animation(file = 'animations/populacao_animado.gif',animation = anim)
