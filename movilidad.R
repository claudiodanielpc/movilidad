##Se carga la paquetería necesaria


if(!require('pacman')) install.packages('pacman')
pacman::p_load(tidyverse,viridis,
               dplyr,lubridate, scales,ggimage)

##Se declara la liga de donde se descargarán los datos
url<-"https://datos.cdmx.gob.mx/explore/dataset/afluencia-preliminar-en-transporte-publico/download/?format=csv&timezone=America/Mexico_City&lang=es&use_labels_for_header=true&csv_separator=%2C"


##Descarga

movilidad<-read.csv(url,header=T,encoding="UTF-8")

##Trabajar con la base de datos

movilidad<-movilidad%>%
  ###Variables en minúsculas
  rename_all(tolower)%>%
  ##Dejar solo metro y metrobús
  filter(organismo=="STC"|organismo=="Metrobús")%>%
  ##Renombrar STC por metro
  mutate(organismo=ifelse(organismo=="STC","Metro",organismo))%>%
 #Seleccionar variables y renombrar
  select(día.mes,mes,año,organismo,linea.servicio,afluencia.total..cifras.preliminares.)%>%
  rename(dia=1,
         linea=5,
         afluencia=6)%>%
  ##Modifcar variable de mes
  mutate(mes=substr(mes,6,8))%>%
  ##Construir variable de fecha
  mutate(fecha=ymd(paste(año, mes, dia,sep= ' ')))%>%
#Dejar variables requeridas
  select(organismo,linea,afluencia,fecha)%>%
  #Ordenar
arrange(fecha)%>%
  mutate(linea = factor(linea, levels=rev(c("L1", "L2", 
                                      "L3", "L4", "L5", "L6", "L7", "L8",
                                      "L9","LA","LB","L12"))))%>%
  ##Dejar todo aquello que sea menor al 13 de julio (no tiene datos)
  filter(fecha<"2020-07-13")

##Crear gráfico. LET THE MAGIC BEGIN!

movilidad%>%  
    ggplot(.,aes(x=fecha,y=linea,fill=afluencia))+
  geom_tile(colour="transparent",size=0.2)+
##Viridis y formato de leyenda
  scale_fill_viridis("Afluencia\nde\npersonas",na.value="white",
                   option = "B",direction =1,labels=scales::comma)+
  scale_x_date(expand = c(0, 0)) +
  theme_minimal()+
  ##Línea que marca inicio de Jornada Nacional de Sana Distancia
  geom_segment(x=ymd("2020-03-23"), xend=ymd("2020-03-23"), y=0,linetype="dashed", 
                                             yend=13, size=3,color="#31a354")+ 
  #Final de Jornada Nacional de Sana Distancia
  geom_segment(x=ymd("2020-05-30"), xend=ymd("2020-05-30"), y=0,linetype="dashed", 
               yend=13, size=3,color="#feb24c")+
  
  ##Pimp my plot! :)
  
  theme(legend.position="right",legend.direction="vertical",
        legend.title=element_text(colour="black"),
        legend.margin=margin(grid::unit(0,"cm")),
        legend.title.align = 0.5,
        legend.text=element_text(colour="black",size=14,face="bold"),
        legend.key.height=grid::unit(0.8,"cm"),
        legend.key.width=grid::unit(0.2,"cm"),
        axis.text.x=element_text(colour="black", size=13,angle=90),
        axis.text.y=element_text(colour="black",size=13),
        axis.ticks=element_blank(),
        plot.background=element_blank(),
        panel.border=element_blank(),
        plot.margin=margin(0.7,0.4,0.1,0.2,"cm"),
        strip.text.x = element_text(size=20, color="black",
                                    face="bold"),
        plot.title=element_text(colour="black",hjust=0,size=25,face="bold"),
        plot.subtitle = element_text(hjust = 0, size=20, face="italic"),
        plot.caption = element_text(hjust = 0,size=15),
        text=element_text(size=20))+
  ##Formalidad. Título, subtítulo y fuente
  labs(
    title = "Ciudad de México. Afluencia preliminar en transporte público. Metro y Metrobús",
    subtitle = "Del 1° de marzo al 12 de julio 2020",
    caption = "Nota: La línea vertical verde indica el inicio de la Jornada Nacional de Sana Distancia (23 de marzo)
mientras que la línea vertical amarilla indica la conclusión de ésta (30 de mayo)
Fuente: @claudiodanielpc con datos del Gobierno de la Ciudad de México. Agencia Digital de Innovación Pública (ADIP)",
    x="Fecha",
    y="Línea"
  )+
  ##Facet wrap para presentar a ambos organismos
  facet_wrap(~ organismo,ncol=1, scale = "free_y")


##Salvar el gráfico
ggsave("heatmapmm.png",height = 10,width = 30, units="in",dpi=300)