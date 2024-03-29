##################################################################
### Script para plotar um gr�fico de pizza usando o pacote     ###
### ggplot2                                                    ###
###                                                            ###
## desenvolvido por: Mercel Santos                             ###
## contato: contato@mercelsantos.com                           ###
## instagram: @mercelsantos                                    ###
##################################################################

library(ggplot2)   #[3.3.2] Para plotar os dados
library(dplyr)  #[0.8.5] Para manipula��o de dados

setwd('C:/Users/merce/OneDrive/Documents/MEGA/work/banners/graf_pizza')

dados <- read.csv('dados/dados_covid.csv', #Leitura do arquivo .csv
                  skip = 2) %>% #Pulando linha durante a leitura
  mutate(n.obitos.poc=round((n.obitos/sum(n.obitos)*100),1)) #Calculando a porcentagem do n�mero de �bitos


#########################################
#####     Plotando os dados   ###########
#########################################
meu.graf <- ggplot(dados,aes(x="",y=n.obitos.poc,fill=Regiao))+#Definindo e mapeando os dados no gr�fico
  geom_col(col="white")+ #Criando o gr�fico de barras
  geom_text(aes(y=n.obitos.poc,x=1.2, #geom_text adiciona os valores percentuais no gr�fico
                label=paste0(n.obitos.poc,"%")), #Definindo os textos que ser�o inseridos
            position = position_stack(vjust = 0.55),#Definindo a posi��o 
            color = "black")+ #Definindo a cor do texto inserido
  coord_polar(theta = "y", start=0)+ #Convertendo coordenadas cartesianas em polares [trasforma gr�fico de barras em pizzas]
  scale_fill_manual(values=c('#993767','#3C8E9D','#F3C5BC','#E16552','#E9D78E'))+ #Definindo as cores do interior das barras
  theme_void()+ #Modificando o tema
  labs(title = "Valor percentual de �bitos por Regi�o", #Definindo t�tulos e subt�tulos
       subtitle = "Dados atualizados em 28/09/2020",
       fill=NULL)+
  theme(legend.position = c(.5,.04), #Definindo a posi��o da legenda
        legend.direction ="horizontal") #Definindo a orienta��o da legenda

########################
## Salvando os dados ###
########################
ggsave(plot = meu.graf,filename = 'figs/graf_pizza.png',scale = .9)


