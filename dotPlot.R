
library(RColorBrewer)
library(ggplot2)
library(dplyr)
library(stringr)

cores <- brewer.pal(8, "Spectral")


dir <- "/home/mercel/mega/work/banners/dotPlot/"

dados <- read.csv(paste0(dir,"dados/dadosIbge.csv"),
                  skip = 2) %>% select("UF....",
                                       "Densidade.demográfica...hab.km...2010.",
                                       "Rendimento.mensal.domiciliar.per.capita...R...2019.",
                                        "IDH..span.I.ndice.de.desenvolvimento.humano..span...2010.") %>%
                  setNames(c("UF","d.demo","Rend.Mensal","IDH")) %>% 
                  mutate(d.demo=log(as.numeric(str_replace_all(d.demo,",","."))),
                         IDH=as.numeric(str_replace_all(IDH,",","."))
                         )

theme_set(theme_bw())

ggplot(dados,aes(x=d.demo,y=UF,size=Rend.Mensal))+
    geom_point(aes(col=IDH))+
    scale_color_gradientn(colours = cores)+
    labs(x="Densidade Demográfica [Logaritimo dos Valores]",
         y=NULL,colour="IDH",size="Rendimento Domiciliar  \n Mensal per Capita [R$]",
         title=NULL)

ggsave(paste0(dir,'figs/dotPlot.png'),height = 4.66,width = 5.54,dpi=600) 


