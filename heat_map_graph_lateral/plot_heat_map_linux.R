##################################################################
## script para ler dados de umidade do solo e plotar um heatmap ##
## com gráfico lateral                                          ##
##                                                              ##
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

library(dplyr)     #Para manipulação de dados
library(ggplot2)   #Para plotar dados
library(patchwork) #Para arranjamento de gráficos

#Removendo objetos da memória
rm(list = ls())

#Definindo diretório de trabalho
setwd('/home/mercel/mega/work/banners/heat_map_graph_lateral/')


#####################################
### Lendo e manipulando os dados ####
#####################################
dados <- read.table('dados/saida_modelo.txt',skip = 11, #Pulando 11 linhas destinada a descrição dos dados
                    sep=',',header = T) %>%  #sep=',' indica que os dados estão separados por vírgula
                    select(date,depth,wcontent,waterflux) %>% #Selecionando as colunas de dados
                    filter(depth != -60.0) %>% #Removendo linhas com valor de profundidade = -60.0
                    mutate(data=as.Date(date)) #Criando coluna de dados com a classe date

cor.contorno <- adjustcolor("grey30",alpha.f = .1)   #Deixando a cor transparente               

minha.paleta <- colorRampPalette(c('#8f5c16','#f3e87b', #Criando paleta de cores
                                   '#78c5ed','#224592'))  

###########################
### Plotando os dados  ####
###########################

p1 <- ggplot(dados,aes(data,depth,fill=wcontent,z=waterflux))+ #Informando e mapeando os dados
       geom_tile(col=cor.contorno)+ # Plotando o heatmap 
       coord_cartesian(expand = FALSE)+ #Removendo o espaço entre os dados e os eixos 
       scale_fill_gradientn(colours = minha.paleta(200))+ #Modificando paleta de cores padrão
       labs(y='Profundidade [cm]', #Definindo título do eixo y
             x=NULL,               #Definindo título do eixo x
             fill="Unidade \ndo solo \n[cm³/cm³]")+ ##Definindo título da escala de cores
       theme_bw()+ #Definindo o tema do gráfico
       theme(legend.position = 'right')#Definindo posição da legenda

p2 <- ggplot(dados %>% group_by(data) %>% # Definindo os dados
          summarise(u.desvPad=sum(wcontent,na.rm = T)), #Determinando a soma vertical 
          aes(data,y=u.desvPad)) + #Mapeando os dados
          geom_line(col='brown4')+ #
#          scale_y_continuous(n.breaks = 3)+ # Definindo um número máximo de marcas no eixo y (v3.3.0 ggplot2) 
          theme_minimal()+ #Definindo o tema
          labs(y="Soma\nVertical", #Definindo título do eixo y
                x=NULL)+            #Definindo título do eixo x
          coord_cartesian(expand = F)+ #Removendo o espaço entre os dados e os eixos 
          theme(axis.text.x = element_blank()) #Removendo as marcas do eixo x

p2 / p1 + plot_layout(heights = c(1,5)) #Arranjando os gráficos

############################################
#####      Salvando os dados         #######
############################################
ggsave(filename = 'figs/perfis_verticas.png',
       width = 5.68,
       height = 4.09,
       scale = .99)
