

if(!require('pacman')) install.packages('pacman')
pacman::p_load(tidyverse, gganimate,viridis,gifski,scales, lubridate)



movi = read.csv("https://datos.cdmx.gob.mx/explore/dataset/afluencia-preliminar-en-transporte-publico/download/?format=csv&timezone=America/Mexico_City&lang=es&use_labels_for_header=true&csv_separator=%2C", 
                   header=TRUE, row.names=NULL, 
                   stringsAsFactors=FALSE)%>%
  #Renombrar nombre de variables a minúsculas
  rename_all(tolower)%>%
  ##Filtrar Metro CDMX
  filter(organismo=="STC")%>%
#Renombrar variable de afluencia
    rename(afluencia=afluencia.total..cifras.preliminar.)%>%
###Cambiar variable de caracter a formato de fecha  
  mutate(fecha=as.Date.character(fecha))%>%
  #Ordenar por fecha
  arrange(fecha)%>%
  ##Ordenar las líneas de metro
  mutate(linea.servicio = factor(linea.servicio, levels=rev(c("L1", "L2", 
                                            "L3", "L4", "L5", "L6", "L7", "L8",
                                            "L9","LA","LB","L12"))))

##Heatmap
ggplot(movi, 
       aes(x = fecha, y = linea.servicio, fill = afluencia)) + 
  geom_tile()+
  #Jornada Nacional de Sana Distancia
  geom_vline(xintercept = ymd("2020-03-23"), linetype="dashed", 
             color = "black", size=1.5)+
  geom_vline(xintercept = ymd("2020-05-30"), linetype="dashed", 
             color = "black", size=1.5)+
  ##Semáforo epidemiológico de CDMX pasa de rojo a naranja
  geom_vline(xintercept = ymd("2020-06-29"), linetype="dashed", 
             color = "blue", size=1.5)+
  theme_minimal()+
  ##Títulos, subtítulos y fuente
  labs(title = "Afluencia diaria de pasajeros en el Sistema de Transporte Colectivo (Metro)",
       subtitle = "Cifras preliminares",
       y = "Líneas",
       caption = "Nota: Las líneas verticales negras marcan el inicio y conclusión de la Jornada Nacional de Sana Distancia.
Por otra parte, la línea vertical azul marca el cambio del semáforo epidemiológico en la CDMX de rojo a naranja.
Fuente: @claudiodanielpc con datos de la Agencia Digital de Innovación Pública de la Ciudad de México (ADIP).",
    fill = "Afluencia \n(personas)")+
  ##Formato del eje de la fecha
  scale_x_date("Fecha",expand = c(0, 0),
               date_labels = "%b %d", date_breaks = "10 days")+
  ##Colores del heatmap
  scale_fill_distiller(palette = "YlOrRd",direction=1,labels=comma)+
  ##Temita
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
        text=element_text(size=20))


##Salvar el gráfico
ggsave("heatmapmetro.png",height = 10,width = 30, units="in",dpi=300)