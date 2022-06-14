##################################################################
### Script para plotar um gráfico de pizza usando o pacote     ###
### ggplot2                                                    ###
###                                                            ###
## desenvolvido por: Mercel Santos                             ###
## contato: contato@mercelsantos.com                           ###
## instagram: @mercelsantos                                    ###
##################################################################

library(ggplot2)   #[3.3.2] Para plotar os dados
library(dplyr)  #[0.8.5] Para manipulação de dados

setwd('/home/mercel/mega/work/banners/graf_pizza')

dados <- read.csv('dados/dados_covid.csv', #Leitura do arquivo .csv
                  skip = 2) %>% #Pulando linha durante a leitura
         mutate(n.obitos.poc=round((n.obitos/sum(n.obitos)*100),1)) #Calculando a porcentagem do número de óbitos


#########################################
#####     Plotando os dados   ###########
#########################################
meu.graf <- ggplot(dados,aes(x="",y=n.obitos.poc,fill=Regiao))+#Definindo e mapeando os dados no gráfico
              geom_col(col="white")+ #Criando o gráfico de barras
              geom_text(aes(y=n.obitos.poc,x=1.2, #geom_text adiciona os valores percentuais no gráfico
                        label=paste0(n.obitos.poc,"%")), #Definindo os textos que serão inseridos
                         position = position_stack(vjust = 0.55),#Definindo a posição 
                         color = "black")+ #Definindo a cor do texto inserido
              coord_polar(theta = "y", start=0)+ #Convertendo coordenadas cartesianas em polares [trasforma gráfico de barras em pizzas]
              scale_fill_manual(values=c('#993767','#3C8E9D','#F3C5BC','#E16552','#E9D78E'))+ #Definindo as cores do interior das barras
              theme_void()+ #Modificando o tema
              labs(title = "Valor percentual de óbitos por Região", #Definindo títulos e subtítulos
                 subtitle = "Dados atualizados em 28/09/2020",
                 fill=NULL)+
              theme(legend.position = c(.5,.04), #Definindo a posição da legenda
                  legend.direction ="horizontal") #Definindo a orientação da legenda

########################
## Salvando os dados ###
########################
ggsave(plot = meu.graf,filename = 'figs/graf_pizza.png',scale = .9)


