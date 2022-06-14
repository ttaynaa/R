################################################
## script que  plota  matriz de correlacao  ####
## dos dados atmosferiocos para cidade de   ####
## Porto Alegre                             ####
##                                          ####
## desenvolvido por: Mercel Santos          ####
## contato: contato@mercelsantos.com        ####
## instagram: @mercelsantos                 ####
## P.S.: as palavras nos cometarios estao   ####
## sem  acentos  para  evitar               ####
## incompatibilidade de editores de texto   ####
################################################


####################################################
## Carregando os pacotes                         ###
##                                               ###
## certifique-se de que todos os pacotes         ###
## usados abaixo estao intalados                 ###
####################################################
library("ggplot2")
library('ggcorrplot')


##########################################
##    Lendo os dados                    ##
##########################################

dir <- '/home/mercel/mega/work/banners/marizCorrelacao/'

dados <- read.table(paste0(dir,"dados_porto_alegre.txt"),
                    header = T)


##########################################
## Determinando a matrix de correlacao  ##
## e truncando os valores              ###
##########################################


cor.data <- round(cor(dados),1)

##########################################
## Plotando e salvando os dados      #####
##########################################

meu.plot <- ggcorrplot(cor.data, hc.order = TRUE, 
               type = "lower",
               legend.title = "Coeficiente de \n Correlação",
               lab = TRUE, 
               lab_size = 3, method="circle", 
               colors = c("firebrick", "white", "dodgerblue4"), 
               title="Correlação entre Variáveis Atmosféricas ", 
               ggtheme=theme_bw)

ggsave(filename = paste0(dir,'/figs/correlacao.png'),
       plot = meu.plot,dpi=130,height =4 ,width = 5)
