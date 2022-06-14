################################################
## script para plotar grafico animado do    ####
## crescimento da populacao e ranquear      ####
## os pa'ises mais populosos do mundo       ####
##                                          ####
## desenvolvido por: Mercel Santos          ####
## contato: contato@mercelsantos.com        ####
## instagram: @mercelsantos                 ####
################################################
rm(list = ls())

library('countrycode') # determina os continentes
library('tidyverse')  # carrega pacotes p/ análise de dados
library('translateR') # traduz texto (usa uma API Key do google)
library('gganimate')  # para gerar animação
library('gapminder')

###########################################
### Algumas funções                 #######
###########################################

#traduz um texto em inglês para português
#precisa informar a api key do google
#sem a api key não haverá tradução

traduza <- function(texto){

  ##  Insira aqui sua api key
  # minha.API.KEY <- ''
  minha.API.KEY <- ''
  
      if(nchar(minha.API.KEY)>0){
      
          meu.texto <- data.frame(texto=as.character(texto),
                                  stringsAsFactors=FALSE)
            
          traduzido.google <- translate(dataset = meu.texto,
                                        content.field = 'texto',
                                        google.api.key = minha.API.KEY,
                                        source.lang = 'en',
                                        target.lang = 'pt')
          
          ok.traduzido <- traduzido.google$translatedContent
      }else{
          ok.traduzido <- texto
      }
      
    return(ok.traduzido)
}


# convert um determinado valor dado em unidades
# para mihares, milhores e bilhoes

convert.escala <- function(x){
  
  if(!is.numeric(x)){
    print("informar apenas valores numericos")
    break()    
  }
  
  resul <- vector(length = length(x)) 
  
  for(i in 1:length(x)){  
    
    if(x[i]<1000){
      resul[i] <- x[i]
    }else if(x[i]>=1000 & x[i]<1000000){
      resul[i] <-  paste(round(x[i]/1000,1),"k")
    }else if(x[i]>=1000000 &  x[i]<1000000000){
      resul[i] <-  paste(round(x[i]/1000000,1),"M")
    }else if(x[i]>=1000000000 & x[i]<1000000000000){
      resul[i] <-  paste(round(x[i]/1000000000,1),"B")
    }else{
      print("essa funcao so converte ate o bilhao")
      print("faca una nova implementacao aqui")
      break()
    }
    
  }
  return(resul)
}


###################################################
### Lendo e Processando os dados            #######
###################################################

dir <- '/home/mercel/mega/work/banners/storytellingBarPlot/'

arquivo   <- read.csv(paste0(dir,"population_total.csv"),header = T)
countries <- arquivo %>% pull(country)
paises    <- traduza(countries)
n.colunas <- arquivo %>%  select(!country) %>% ncol()
anos      <- names(arquivo)[-1] %>% substr(2,5) %>% as.integer()

valores <- arquivo  %>%
           select(!country) %>% as.matrix() %>% 
           as.vector() %>% tibble::enframe(name = NULL)

## countrycode é uma função incrível que 
## determina o continente apartir no nome do país
dados <- valores %>% mutate(ano=rep(anos,rep(length(countries),length(anos))),
                            paises=rep(paises,n.colunas),
                            countries=rep(countries,n.colunas),
                            continent=countrycode(countries,origin = "country.name",destination = "continent"),
                            pop=value) %>% 
                            group_by(ano) %>% # agrupando os dados por ano
                            mutate(rank = min_rank(-pop) * 1) %>%  # determinando o top 10/ano
                            ungroup() %>% filter(rank<=10) # desagrupando e selecionando os top 10

###########################################
### Plotando os dados               #######
###########################################

periodo <- filter(dados,ano>=1850 & ano<=2050)

p <- ggplot(periodo) +
  geom_bar(aes(y = pop,  x = rank,
               group = paises ,color = continent, fill = continent), 
           alpha = 0.75, stat = "identity",width =.6 )+#my.label1+
  coord_flip(clip = "off", expand = FALSE)+
  geom_text(aes(y = 0, x= rank,label = paste(paises, " ")),size=5,vjust = 0.2, hjust = 1)+
  geom_text(aes(y=pop,x= rank,label = paste(" ",convert.escala(pop)), hjust=0),size=5)+
  scale_x_reverse()+
  theme_minimal(15, "Avenir")+
  labs(title ="Crescimento populacional",
       y = NULL,
       x = NULL,
       colour=NULL,
       fill=NULL)+
   scale_color_manual(values = c("#7FEB00","#FFE700","#FF5872","#00D5E9"),
                      labels = c("África", "Américas", "Ásia","Europa"))+
   scale_fill_manual(values = c("#7FEB00","#FFE700","#FF5872","#00D5E9"),
                     labels = c("África", "Américas", "Ásia","Europa"))+
  theme(plot.title = element_text(size = 25,vjust = 3),
      axis.ticks.y = element_blank(),
      axis.text.y  = element_blank(),
      axis.text.x  = element_blank(),
      panel.background  = element_blank(), 
      panel.grid = element_blank(),
      plot.margin  = unit(c(1,1.5,0,4), "cm"),
      plot.background = element_blank(),
      legend.position = "top",
      panel.grid.major.x = element_blank(), 
      panel.grid.major.y = element_blank())+
  geom_text(aes(y = 0.92*max(pop), x =9.5, label = as.character(ano)),  
            size = 16, col = "#828285")+
  #####################################################
  ### Aqui onde acontece a animação           #########
  #####################################################
  transition_time(ano)+
  ease_aes('cubic-in-out')

animate(p, fps = 20, nframes = 860, end_pause = 0, width = 500, height =400)
anim_save(paste0(dir,"crescimento_populacional.gif"))



