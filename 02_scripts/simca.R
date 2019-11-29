##00_PAQUETES----
#Lista dos paquetes que se van usar nesta analise
paquetes_necesarios <- c(
  #Manexo de datos
  "dplyr", 
  "tidyr",
  #Uso de funcions aplicadas a listas
  "purrr",
  #Examinar e limpar os datos
  "janitor",
  #Funcions aplicadas a caracteres
  "stringr",
  "glue",
  #Funcions para aplicar a factores
  "forcats", 
  #Funcions de ordenacion e modificacion de taboas
  # "tidyr",
  #Manexo de datas
  "lubridate", 
  #Apertura e manexo de datos espaciais
  "sf", 
  #Libreria para crear e transformar unidades de medida
  "units",
  #Creacion de graficos por capas
  "ggplot2", 
  # "cowplot", 
  "ggchicklet",
  "ggforce",
  "ggalt",
  "cowplot",
  #Incluir novas cores
  "viridis", 
  #Extraccion de fontes tipograficas
  "showtext",
  #Xestion de imaxes
  "magick"
)
#Cargar paquetes
sapply(paquetes_necesarios,library,character.only=T)
#00_ESTILO_E_IMAXE----
#Importar arquivos "otf"
#QUICKSAND, letra escollida
quicksand_ruta <-
  here::here("01_datos_iniciais",
             "fontes",
             "quicksand_regular.otf")
#Engadir a fonte
font_add("quicksand", quicksand_ruta)
#Bold
quicksand_bold_ruta <-
  here::here("01_datos_iniciais",
             "fontes",
             "quicksand_bold.otf")
#Engadir a fonte
font_add("quicksand_bold", quicksand_bold_ruta)
#Light
quicksand_light_ruta <-
  here::here("01_datos_iniciais",
             "fontes",
             "quicksand_light.otf")
#Engadir a fonte
font_add("quicksand_light", quicksand_light_ruta)
#Light italic
quicksand_lightitalic_ruta <-
  here::here("01_datos_iniciais",
             "fontes",
             "quicksand_lightitalic.otf")
#Engadir a fonte
font_add("quicksand_lightitalic", quicksand_lightitalic_ruta)
#Abrir as fontes
showtext_auto()
# #Importar fontes .ttf
extrafont::font_import(pattern="Quicksand")
#CORES
#paleta de cores
cores <- c(
  #cor azulada, homes
  "#63c7dbff",
  #cor amarelada, mulleres
  "#ffd44eff")
cor_1980 <- "#ffcb6fff"
cor_2017 <- "#ffa3b5ff"
cor_gris <- "#f0f0f0ff"
##01_ABRIR_DATOS----
#Ruta do arquivo "datosr_simca.csv"
simca_ruta <- 
  here::here("01_datos_iniciais",
             "taboas",
             "datosr_simca.csv")
#Abrir arquivo
simca <- readr::read_csv2(simca_ruta) %>% 
  mutate(gidade2=
           case_when(
             gidade%in%c("00-04","05-09","10-14","15-19","20-24","25-29","30-34")~"00-34",
             gidade%in%c("35-39","40-44","45-49","50-54")~"35-54",
             gidade%in%c("55-59","60-64")~"55-64",
             gidade%in%c("65-69","70-74","75-79")~"65-79",
             gidade%in%c("80-84","85->")~">80"
           ),
         gidade2=fct_relevel(factor(gidade2),c("00-34","35-54","55-64","65-79",">80")))
#CAPA DE PROVINCIAS
#Ruta do arquivo "provincias_ign.shp"
provincias_ruta <- 
  here::here("01_datos_iniciais",
             "capas",
             "provincias_ign.shp")
#Abrir arquivo
provincias <- st_read(provincias_ruta,quiet=T) %>% 
  st_transform(crs=4326) %>% clean_names()
#CAPA DE EOXI (Estrutura Organizativa de Xestion Integrada)
#Ruta do arquivo "eoxi.shp"
eoxi_ruta <- 
  here::here("01_datos_iniciais",
             "capas",
             "eoxi.shp")
#Abrir arquivo
eoxi <- st_read(eoxi_ruta,quiet=T,options = "ENCODING=ISO-8859-1") %>% 
  st_transform(crs=4326) %>% clean_names()

#01_PROCESO----
#Datos basicos----
#Total de defuncions por sexo
simca_def_sex <- 
simca %>% filter(agrup=="Provincia") %>% 
  group_by(sexo) %>% 
  summarise(tot=sum(def)) %>% 
  ungroup()
#total de defuncions 
simca_def<- 
  format(as.numeric(simca_def_sex[3,2]),  decimal.mark=",",
         big.mark=".")
#total de defuncions en homes
simca_def_homes <- 
format(as.numeric(simca_def_sex[1,2]),  decimal.mark=",",
big.mark=".")
#total de defuncions en mulleres
simca_def_mulleres <- 
  format(as.numeric(simca_def_sex[2,2]),  decimal.mark=",",
         big.mark=".")

#Defuncions por ano e tipo de cancro----
simca_cancros_ano <- 
  simca %>% 
  filter(agrup=="Provincia"&sexo!="Total") %>% 
  group_by(cancro,ano) %>% 
  summarise(def=sum(def)) %>% 
  ungroup() 
#GRAFICO
simca_cancros_ano_grafico <- 
  simca_cancros_ano %>% 
  ggplot(.,aes(ano,reorder(cancro,def)))+
  geom_tile(aes(fill=def))+
  scale_fill_gradient2(low = "#f0f0f0ff",mid="#000337ff",high="#ffe344ff",midpoint=800)+
  scale_x_continuous(breaks=seq(1980,2017,2))+
  labs(title="",caption="Evolución das defuncións a causa de 13 tipos de cancro dende 1980 a 2017")+
  theme(# panel.grid.major = element_blank(),
    legend.position="bottom",
    legend.title = element_blank(),
    legend.direction = "horizontal",
    legend.background = element_rect(fill="transparent",colour="transparent"),
    legend.text = element_text(family="quicksand",size=10),
    legend.key = element_rect(colour = NA, fill = NA),
    legend.key.width = unit(12,"mm"),
    panel.grid =element_blank() ,
    axis.ticks=element_blank(),
    axis.title = element_blank(),
    axis.text.y = element_text(face="bold",size=17,family="quicksand"),
    axis.text.x = element_text(face="bold",size=11,family="quicksand",vjust=-1),
    plot.caption = element_text(size=20,family="quicksand_lightitalic",vjust=-2,hjust=.5),
    panel.background = element_rect(fill="transparent",colour="transparent"),
    plot.background = element_rect(fill="transparent",colour="transparent"),
    plot.margin=unit(c(2,.2,1,0.2),"cm")
  )


ggsave(here::here("04_rmd","imaxes","cancro_ano.png"), width = unit(15,"cm"), height = unit(6,"cm"), dpi = 96)
#Defuncions totais por tipo de cancro e sexo----
#Taboa de datos
simca_cancros <- 
  simca %>% filter(agrup=="Provincia"&sexo!="Total") %>% 
  group_by(cancro,sexo) %>% 
  summarise(def=sum(def)) %>% 
  ungroup() 
#TABOA
simca_cancros_pretaboa <- 
  simca_cancros %>%
  spread(sexo,def) %>% 
  mutate_at(vars("Homes","Mulleres"),
            ~case_when(is.na(.)~0,
                       TRUE~.)) %>% 
  group_by(cancro) %>% 
  mutate(total=sum(Homes,Mulleres)) %>% 
  ungroup() %>% 
  arrange(desc(total)) %>% 
  select(-total) %>% 
  rename(Cancro=cancro)

simca_cancros_taboa <- 
  formattable::formattable(simca_cancros_pretaboa,
                           align =c("l","c","c"),
                           list(
                             `Homes` = formattable::color_tile(cor_gris,cores[1]),
                             `Mulleres` = formattable::color_tile(cor_gris,cores[2])))
#GRAFICO
simca_cancros_grafico <- 
  simca_cancros %>% 
  ggplot(.,aes(reorder(cancro,def),def,fill=sexo,label=def))+
  geom_chicklet(width = 0.75)+
  scale_y_continuous(breaks=seq(0,50000,5000))+
  scale_fill_manual(values=cores,
                    guide=guide_legend( "",direction = "horizontal",
                                        keyheight = unit(2.5, units = "mm"),
                                        keywidth = unit(18,units = "mm"),
                                        title.position = 'top',
                                        title.hjust = 0.5,
                                        label.hjust = 0.5,
                                        nrow = 1,
                                        byrow = T,
                                        reverse = F,
                                        label.position = "top"))+
  coord_flip()+
  labs(x="",y="",
       title="",
       subtitle="",
       caption = "Defuncións a causa de 13 tipos de cancro en homes e mulleres.")+
  theme(
    legend.position=c(0.884,1.04),
    legend.background = element_rect(fill="transparent",colour="transparent"),
    legend.text = element_text(family="quicksand",size=13),
    legend.key = element_rect(colour = NA, fill = NA),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(colour=cor_gris),
    panel.grid.major.y = element_blank(),
    axis.ticks=element_blank(),
    axis.text.y = element_text(size=20,family="quicksand"),
    axis.text.x = element_text(size=14,family="quicksand",vjust=-3),
    plot.title = element_text(size=20,family="quicksand_bold",face="bold",hjust = .25,vjust=15),
    plot.subtitle =element_text(family="quicksand",colour="#585858ff",size=15,hjust=.25,vjust=15),
    plot.caption = element_text(size=20,family="quicksand_lightitalic",vjust=-2,hjust=.5),
    plot.background = element_rect(fill="transparent",colour="transparent"),
    panel.background= element_rect(fill="transparent",colour="transparent"),
    plot.margin=unit(c(2,.2,1,0.2),"cm")
  )

ggsave(here::here("04_rmd","imaxes","cancros_sexo.png"), width = unit(15,"cm"), height = unit(10,"cm"), dpi = 96)
#Defuncions totais por grupos de idade----
#Defuncions por grupo de idade e ano
simca_gidade_ano <- 
simca %>% 
  filter(agrup=="Provincia"&sexo=="Total"&ano%in%c(1980,2017)) %>% 
  group_by(gidade,ano) %>% 
  summarise(def=sum(def)) %>% 
  ungroup() 

#Defuncions por grupo de idade e ano b
simca_gidade_anob <- 
  simca_gidade_ano %>% 
  spread(ano,def) 

#Poboacion total por ano e grupo de idade
simca_gidade_ano_pob <-   
  simca %>% filter(agrup=="Provincia"&sexo!="Total") %>% 
  select(ano,gidade,pob) %>% unique() %>% 
  group_by(gidade,ano) %>% 
  summarise(tot_pob=sum(pob)) %>% 
  ungroup() 

#Porcentaxe de defuncions por grupos de idade segundo a poboacion total en 1980 e 2017
simca_gidade_ano_porcentaxe <- 
  simca_gidade_ano %>% 
  inner_join(simca_gidade_ano_pob,by=c("gidade","ano")) %>% 
  mutate(porcentaxe=100*(def/tot_pob)) %>% 
  select(gidade,ano,porcentaxe) %>% 
  spread(ano,porcentaxe) 

#TABOA
simca_gidade_pretaboa <- 
  simca_gidade_ano %>%
  rename(`Grupo de idade`=gidade)


simca_gidade_taboa <- 
  formattable::formattable(simca_gidade_pretaboa,
                           align =c("l","c","c"),
                           list(
                             `1980` = formattable::color_tile(cor_gris,cor_1980),
                             `2017` = formattable::color_tile(cor_gris,cor_2017)))
#GRAFICO
simca_gidade_grafico <- 
ggplot(data=simca_gidade_anob,aes( y=gidade,x=`1980`, xend=`2017`, group=gidade))+
  geom_dumbbell(size=6,size_x=8,size_xend=8,colour="#f0f0f0ff",
                colour_x=cor_1980, colour_xend=cor_2017)+
  geom_text(data=filter(simca_gidade_anob, gidade=="85->"),
            aes(x=`1980`, y=gidade, label="1980"),
            color="#ffcb6fff", size=6, vjust=-2,
            fontface="bold",family="quicksand")+
  geom_text(data=filter(simca_gidade_anob, gidade=="85->"),
            aes(x=`2017`, y=gidade, label="2017"),
            color="#ffa3b5ff",size=6, vjust=-2,
            fontface="bold",family="quicksand")+
  geom_mark_circle(aes(label = glue("{gidade}"),
                       description = "Apréciase unha gran subida nas defuncións das persoas de entre 80 e 84 anos",
                       filter=gidade=="80-84"& `2017`==909),
                   expand = unit(1, "mm"), label.family = c("quicksand", "quicksand_light"),
                   label.fontsize = 13, label.buffer = unit(80, "mm")
  )+
  geom_mark_circle(aes(label = glue("{gidade}"),
                       description = "Apréciase unha subida considerable nas defuncións das persoas con m?is de 85 anos",
                       filter=gidade=="85->"& `2017`==1224),
                   expand = unit(1, "mm"), label.family = c("quicksand", "quicksand_light"),
                   label.fontsize = 13, label.buffer = unit(1, "mm")
                   )+
  geom_mark_rect(aes(label = "0-19",group=1,
                       description = "Nestes 4 grupos de idade o n?mero de defuncións dende 1980 a 2017 reduciuse a 0",
                       filter=gidade%in%c("05-09","00-04","10-14","15-19")),
               label.family = c("quicksand", "quicksand_light"),label.fontsize = 13
  )+
  
    scale_x_continuous(breaks=seq(0,1500,100))+
  labs(x="",y="",title=" ",subtitle=" ",
       caption = "Comparación das defuncións entre 1980 e 2017 en 18 grupos de idade a causa do cancro, 
       tanto en datos absolutos como en relación á poboación total"
       )+
    coord_cartesian(clip="off")+
  theme(
    legend.position="none",
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(colour="#f0f0f0ff"),
    axis.ticks=element_blank(),
    axis.text.y = element_text(face="bold",size=17,family="quicksand"),
    axis.text.x = element_text(face="bold",size=11,family="quicksand",vjust=-3),
    plot.caption = element_text(size=20,family="quicksand_lightitalic",vjust=-2,hjust=.5),
    plot.background = element_rect(fill="transparent",colour="transparent"),
    panel.background= element_rect(fill="transparent",colour="transparent"),
    plot.margin=unit(c(2,.2,1,0.2),"cm")
  )

#Grafico de porcentaxes
simca_gidade_porcentaxe_grafico <- 
ggplot(data=simca_gidade_ano_porcentaxe,aes( y=gidade,x=`1980`, xend=`2017`, group=gidade))+
  geom_dumbbell(size=3,size_x=4,size_xend=4,colour="#f0f0f0ff",
                colour_x=cor_1980, colour_xend=cor_2017)+
  geom_text(data=filter(simca_gidade_ano_porcentaxe, gidade=="85->"),
            aes(x=`1980`, y=gidade, label="1980"),
            color="#ffcb6fff", size=5, vjust=-2,
            fontface="bold",family="quicksand")+
  geom_text(data=filter(simca_gidade_ano_porcentaxe, gidade=="85->"),
            aes(x=`2017`, y=gidade, label="2017"),
            color="#ffa3b5ff",size=5, vjust=-2,
            fontface="bold",family="quicksand")+
  scale_x_continuous(breaks=seq(0,1,.1),labels=function(x) paste0(x, "%"))+
  labs(x="Porcentaxe",y="",
       title="Porcentaxe de defuncións ",
       subtitle="segundo a poboación total "
  )+
  coord_cartesian(clip="off")+
  theme(
    legend.position="none",
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x =  element_blank(),
    axis.ticks=element_blank(),
    axis.title.x=element_text(face="bold",size=9,family="quicksand",vjust=-10),
    axis.text.y = element_text(face="bold",size=9,family="quicksand"),
    axis.text.x = element_text(face="bold",size=9,family="quicksand",vjust=-3),
    plot.title = element_text(size=12,family="quicksand_bold",face="bold",vjust=20,hjust=.3),
    plot.subtitle =element_text(family="quicksand",colour="#585858ff",size=9,vjust=20,hjust=.3),
    plot.background = element_rect(fill="white",colour="white"),
    panel.background= element_rect(fill="transparent",colour="transparent"),
    plot.margin=unit(c(2,.2,1,0.2),"cm")
  )

#Combinacion os dous graficos anteriores 
ggdraw()+
  draw_plot(simca_gidade_grafico)+
  draw_plot(simca_gidade_porcentaxe_grafico,x=0.5,y=.21,width=0.4,height=.4)
#Gardar imaxe
ggsave(here::here("04_rmd","imaxes","cancro_idades.png"), width = unit(15,"cm"), height = unit(10,"cm"), dpi = 96)
#Defuncions por grupo de idade e ano----
#Defuncions segundo gidade2
simca_gidade2_ano <-   
simca %>% filter(agrup=="Provincia"&sexo=="Total") %>% 
  group_by(gidade2,ano) %>% 
  summarise_at(c("def"),~sum(.)) %>% 
  ungroup() %>% 
    mutate(gidade2=fct_rev(gidade2))

#Poboacion total de Galicia segundo gidade2
simca_gidade2_ano_pob <-   
    simca %>% filter(agrup=="Provincia"&sexo!="Total") %>% 
    select(ano,gidade2,pob) %>% unique() %>% 
    group_by(gidade2,ano) %>% 
    summarise(tot_pob=sum(pob)) %>% 
    ungroup() %>% 
    mutate(gidade2=fct_rev(gidade2))


#GRAFICO
#Grafico cos datos absolutos por gidade2
gidade2_ano_grafico <-
  simca_gidade2_ano %>% 
    ggplot(aes(ano,def,fill=gidade2))+
    geom_area(colour="white") +
    scale_fill_manual(values = wesanderson::wes_palette("Royal2"))+
    scale_y_continuous(breaks=seq(0,5500,750))+
    scale_x_continuous(breaks=seq(1980,2017,5))+
    labs(x="",y="",caption="Defuncións por grupos de idade e poboación total de 1980 a 2017")+
    theme_minimal()+
    theme(legend.position = "none",
          panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_blank(),
          axis.ticks.y=element_blank(),
          axis.text.y = element_text(face="bold",size=14,family="quicksand"),
          axis.text.x = element_text(face="bold",size=20,family="quicksand"),
          plot.caption = element_text(size=20,family="quicksand_lightitalic",vjust=-2,hjust=.5),
          plot.margin=unit(c(2,.2,1,0.2),"cm"),
          plot.background = element_rect(fill="transparent",colour="transparent"),
          panel.background= element_rect(fill="transparent",colour="transparent")
    )
  

#GRAFICO
#Grafico da poboacion total de Galicia
simca_gidade2_ano_pob_grafico <- 
simca_gidade2_ano_pob %>% 
  ggplot(aes(ano,tot_pob,fill=gidade2))+
  geom_area(colour="white") +
  scale_fill_manual(values = wesanderson::wes_palette("Royal2"))+
  scale_x_continuous(breaks=seq(1980,2017,5))+
  labs(x="",y="",title="Poboación total")+
  theme_minimal()+
  theme(legend.position = "none",
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        axis.ticks=element_blank(),
        axis.text.x = element_text(face="bold",size=7,family="quicksand"),
        axis.text.y = element_blank(),
        plot.title = element_text(size=8,family="quicksand_bold",face="bold",hjust=.5),
        plot.margin=unit(c(.5,0,0,0),"cm"),
        plot.background = element_rect(fill="transparent",colour="grey80"),
        panel.background= element_rect(fill="transparent",colour="transparent")

  )

#Combinacion os dous graficos anteriores 
ggdraw()+
  draw_plot(gidade2_ano_grafico)+
  draw_plot(simca_gidade2_ano_pob_grafico,x=0.1,y=.71,width=0.24,height=0.24)
  
#Gardar imaxe
ggsave(here::here("04_rmd","imaxes","area_def.png"), width = unit(15,"cm"), height = unit(10,"cm"), dpi = 96)

#Datos poboacion 1980 e 2017
poboacion_1980_2017 <- 
simca_gidade_ano_pob %>% 
  filter(ano%in%c(1980,2017)) %>% 
  group_by(ano) %>% 
  summarise(tot=sum(tot_pob))
#Poboacion en 1980
pob_1980 <-
format(poboacion_1980_2017$tot[1],  decimal.mark=",",
       big.mark=".")
#Poboacion en 2017
pob_2017 <-
  format(poboacion_1980_2017$tot[2],  decimal.mark=",",
         big.mark=".")

#Porcentaxe de defuncions por gigade2
simca_gidade2_porcentaxe <- 
simca_gidade2_ano %>% 
  group_by(gidade2) %>% 
  summarise(def=sum(def)) %>% 
  ungroup() %>% mutate(tot=sum(def),
                       porcentaxe_def=round(100*(def/tot),1)) %>% 
  select(gidade2,porcentaxe_def) 

#GRAFICO
simca_gidade2_porcentaxe_grafico <- 
simca_gidade2_porcentaxe %>% 
  ggplot(.,aes(1,porcentaxe_def,1,fill=gidade2,label=porcentaxe_def))+
  geom_chicklet(width = 0.75,position = "stack")+
  geom_text(position=position_stack(vjust=0.5),
            family="quicksand_bold",colour="white",size=10)+
scale_fill_manual(values = wesanderson::wes_palette("Royal2"))+
  labs(caption="Porcentaxe de defuncións segundo os 5 grupos de idade")+
  coord_flip()+
  theme(legend.position = "none",
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.ticks=element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.title=element_blank(),
    plot.caption = element_text(size=20,family="quicksand_lightitalic",vjust=-2,hjust=.5),
    plot.margin=unit(c(2,.2,1,0.2),"cm"),
    plot.background = element_rect(fill="transparent",colour="transparent"),
    panel.background= element_rect(fill="transparent",colour="transparent")
  )

ggsave(here::here("04_rmd","imaxes","porcentaxes_idade_def.png"), width = unit(15,"cm"), height = unit(2.5,"cm"), dpi = 96)
#Defuncions totais no EOXI----

simca_cancros_eoxi <- 
  simca %>% filter(sexo!="Total"&agrup=="EOXI") %>% 
  group_by(sexo,eoxi) %>% 
  summarise(def=sum(def)) %>% 
  ungroup() %>% 
  filter(sexo!="Total") %>% 
  mutate(eoxi=toupper(eoxi)) %>% 
  inner_join(eoxi,by="eoxi") %>% 
  st_as_sf()


#Poboacion total de cada unha das EOXI
simca_sexo_eoxi_pob <-   
  simca %>% filter(agrup=="EOXI"&sexo=="Total") %>% 
  select(pob,ano,eoxi) %>% unique() %>% 
  group_by(ano,eoxi) %>% 
  summarise_at(c("pob"),~sum(.))%>% 
  ungroup() %>% 
  mutate(pob=pob/1000)
#Taboa de eoxi
eoxi <- 
  simca_cancros_eoxi %>% 
  select(eoxi,geometry) %>% unique()
#Nomes e ubicacion das etiquetas das EOXI
nomes_eoxi <- 
eoxi %>% st_centroid() %>% 
  st_coordinates %>% as.data.frame() %>% 
  cbind(eoxi$eoxi,.) %>% as.tbl() %>% 
  setNames(c("eoxi","x","y")) %>% 
  mutate(y=case_when(
                     eoxi=="A CORU?A"~ 43.25000,
                     TRUE~y))

#GRAFICO
#Mapa da distribucion das EOXI
mapa_eoxis <- 
eoxi %>% 
  ggplot(aes(label=eoxi))+
  geom_sf(fill="#c9e7caff",colour="white")+
  geom_text(data=nomes_eoxi,aes(x=x,y=y,label=eoxi),
            family="quicksand_bold",size=2.8)+
  theme_minimal()+
  labs()+
  theme(text = element_text(family="quicksand"),
    panel.grid = element_blank(),
        axis.title=element_blank(),
        axis.text = element_blank(),
        plot.caption=element_text(family="quicksand",hjust=.5),
        panel.background = element_rect(fill="transparent",colour="transparent"),
        plot.background = element_rect(fill="transparent",colour="transparent")
  )
ggsave(here::here("04_rmd","imaxes","mapa_eoxis.png"), width = unit(10,"cm"), height = unit(5,"cm"), dpi = 96) 


#GRAFICO
#Grafico de poboacion por eoxi
simca_sexo_eoxi_pob_grafico <- 
  simca_sexo_eoxi_pob %>% 
  ggplot(.,aes(ano,reorder(eoxi,pob)))+
  geom_tile(aes(fill=pob))+
  scale_fill_gradient2(low = "#f0f0f0ff",mid="#000337ff",high="#ffe344ff",midpoint=400)+
  scale_x_continuous(breaks=seq(1980,2017,2))+
  labs(title="",caption="Evolución da poboación de Galicia por EOXI dende 1980 a 2017 
       en miles de persoas")+
  theme(# panel.grid.major = element_blank(),
    legend.position="bottom",
    legend.title = element_blank(),
    legend.direction = "horizontal",
    legend.background = element_rect(fill="transparent",colour="transparent"),
    legend.text = element_text(family="quicksand",size=10),
    legend.key = element_rect(colour = NA, fill = NA),
    legend.key.width = unit(12,"mm"),
    panel.grid =element_blank() ,
    axis.ticks=element_blank(),
    axis.title = element_blank(),
    axis.text.y = element_text(face="bold",size=17,family="quicksand"),
    axis.text.x = element_text(face="bold",size=11,family="quicksand",vjust=-1),
    plot.title = element_text(size=20,family="quicksand_bold",face="bold"),
    plot.subtitle =element_text(family="quicksand",colour="#585858ff",size=15),
    plot.caption = element_text(size=20,family="quicksand_lightitalic",vjust=-2,hjust=.5),
    panel.background = element_rect(fill="transparent",colour="transparent"),
    plot.background = element_rect(fill="transparent",colour="transparent"),
    plot.margin=unit(c(2,.2,1,0.2),"cm")
  )
ggsave(here::here("04_rmd","imaxes","pob_eoxi.png"), width = unit(15,"cm"), height = unit(6,"cm"), dpi = 96)

#GRAFICO
#Mapa de defuncions por sexo e eoxi
simca_cancros_eoxi_mapa <- 
simca_cancros_eoxi %>% 
  ggplot() +
  geom_sf(aes(fill = def),colour="transparent") +
scale_fill_gradient2(low = "#f0f0f0ff",mid="#d1abb7ff",high="#6e4c55ff",midpoint=15000)+
  facet_wrap(~sexo,nrow=1)+
  theme_minimal()+
 labs()+
  theme(panel.grid = element_blank(),
          legend.position="bottom",
        legend.title = element_blank(),
          legend.background = element_rect(fill="transparent",colour="transparent"),
          legend.text = element_text(family="quicksand",size=25),
          legend.key = element_rect(colour = NA, fill = NA),
        legend.key.width = unit(15,"mm"),
          axis.text = element_blank(),
          plot.title = element_text(size=20,family="quicksand_bold",face="bold"),
          plot.subtitle =element_text(family="quicksand",colour="#585858ff",size=15),
          plot.caption=element_text(family="indigo",hjust=.5),
          panel.background = element_rect(fill="transparent",colour="transparent"),
        plot.background = element_rect(fill="transparent",colour="transparent"),
        strip.text = element_text(face="bold",size=40,family="quicksand_bold")
        )

#GRAFICO
#Imaxe en 3D do anterior grafico
  rayshader::plot_gg(simca_cancros_eoxi_mapa,
                     width = 5, height = 4, scale = 300, multicore = TRUE, windowsize = c(1200, 960),
                     fov = 70, zoom = 0.65, theta = 360, phi = 80)
  

    
dev.close()
#Gardar imaxe
rayshader::render_snapshot("mapa_3d_eoxi_sexo.png")
#Pechar a fiestra do mapa en 3D
  rayshader::render_snapshot(clear = TRUE)
rgl::rgl.quit()
rgl.pop()
#GRAFICO 
#Construir grafico completo
#Ler a imaxe do mapa en 3D
img <- image_read("mapa_3d_eoxi_sexo.png") 

#Defuncions por ano e eoxi
simca_eoxi_ano <- 
    simca %>% filter(agrup=="EOXI") %>% 
    filter(sexo!="Total") %>% 
    group_by(eoxi,sexo,ano) %>% 
    summarise(def=sum(def)) %>% 
    ungroup() 

#Grafico 
  simca_eoxi_ano_grafico <- 
    simca_eoxi_ano %>% 
    ggplot(.,aes(ano,reorder(eoxi,def)))+
    geom_tile(aes(fill=def),colour="white")+
    scale_fill_gradient("defuncións",low = "#f0f0f0ff",high="#6ecdb4ff")+
    scale_x_continuous(breaks=seq(1980,2017,2))+
    labs(title="")+
    facet_wrap(~sexo)+
    theme_minimal()+
    theme(# panel.grid.major = element_blank(),
      legend.position=c(0.5,1.12),
      legend.title = element_blank(),
      legend.direction = "horizontal",
      legend.background = element_rect(fill="transparent",colour="transparent"),
      legend.text = element_text(family="quicksand",size=8),
      legend.key = element_rect(colour = NA, fill = NA),
      legend.key.width = unit(5,"mm"),
      panel.grid =element_blank() ,
      axis.ticks=element_blank(),
      axis.title = element_blank(),
      axis.text.y = element_text(face="bold",size=17,family="quicksand"),
      axis.text.x = element_text(face="bold",size=8,family="quicksand",vjust=-1),
      plot.title = element_text(size=20,family="quicksand_bold",face="bold"),
      plot.subtitle =element_text(family="quicksand",colour="#585858ff",size=15),
      plot.caption=element_text(family="indigo",hjust=.5),
      panel.background = element_rect(fill="transparent",colour="transparent"),
      plot.background = element_rect(fill="transparent",colour="transparent"),
      strip.text = element_text(face="bold",size=17,family="quicksand")
    )
  
  
pregrafico_combinado <- 
  ggdraw() +
    draw_image(img,x=.05,y=-.15)+
    draw_plot(simca_eoxi_ano_grafico, x = 0.02, y = .7, width = 1, height = .30)

#combinacion do grafico
grafico_combinado <- 
  ggdraw()+
  draw_plot(add_sub(pregrafico_combinado,
"defuncións nas 6 EOXI (Estrutura Organizativa de Xesti?n Integrada)
                    a nivel global e por anos en ?mbolos dous sexos",
fontfamily = "quicksand_lightitalic",size=20))

#Gardar imaxe
ggsave(here::here("04_rmd","imaxes","mapa3d_mosaico_eoxi.png"), width = unit(15,"cm"), height = unit(10,"cm"), dpi = 96)
#Eliminar a imaxe mapa 3D
file.remove("mapa_3d_eoxi_sexo.png")
#Defuncions por eoxi e cancro----
#Taboa inicial
simca_eoxi_cancros <- 
  simca %>% filter(sexo!="Total"&agrup=="EOXI") %>% 
  group_by(eoxi,cancro,gidade2) %>% 
  summarise(def=sum(def)) %>% 
  ungroup() %>% 
  mutate(eoxi=toupper(eoxi)) %>% 
  inner_join(eoxi,by="eoxi") %>% 
  st_as_sf()

#GRAFICO
#Mapa de defuncions por sexo e eoxi
mapa_simca_eoxi_cancros <- 
  simca_eoxi_cancros %>% 
  ggplot() +
  geom_sf(aes(fill = def),colour="transparent") +
  scale_fill_gradient2(low = "#f0f0f0ff",mid="#000337ff",high="#ffe344ff",midpoint=2000)+
  facet_grid(gidade2~cancro)+
  theme_minimal()+
  labs(caption="Defuncións por cancro e grupos de idade nas 7 EOXI.")+
  theme(panel.grid = element_blank(),
        legend.position="bottom",
        legend.title = element_blank(),
        legend.background = element_rect(fill="transparent",colour="transparent"),
        legend.text = element_text(family="quicksand",size=12),
        legend.key = element_rect(colour = NA, fill = NA),
        legend.key.width = unit(15,"mm"),
        axis.text = element_blank(),
        plot.title = element_text(size=20,family="quicksand_bold",face="bold"),
        plot.subtitle =element_text(family="quicksand",colour="#585858ff",size=15),
        plot.caption=element_text(family="quicksand_lightitalic",size=27,hjust=.5),
        panel.background = element_rect(fill="transparent",colour="transparent"),
        plot.background = element_rect(fill="transparent",colour="transparent"),
        strip.text = element_text(face="bold",
                                  size=15,family="quicksand_bold")
  )

ggsave(here::here("04_rmd","imaxes","mapa_eoxi_cancros_idades.png") , width = unit(20,"cm"), height = unit(10,"cm"), dpi = 96)
