rm(list=ls())
load("datos_ordenados_13_09_2020.RData")
library(tidyr)
library(dplyr)
library(ggplot2)
library(gganimate)
names(datos_ordenados)
source("myfunctions.R")

# datos_ordenados 
## "departamento":
## Nombre del departamento Lomas de Zamora, Avellaneda, CABA, etc. 

## "fecha": fecha  es un dato tipo "Date", 
## corresponde a la fecha de diagnostico. Si esa fecha es NaN se reemplaza 
## por la fecha de apertura + 2 dias.
## El tipo de dato es Date  procesado con el paquete lubridate, pero se puede 
## ver como texto en formato yyyy-mm-dd   

## "totales_por_dia": 
## numero de confirmados en el dia. Se toma como caso confirmado aquel que 
## dia de la fecha.  totales_por_dia puede ser cero si 
## no hubo casos ese dia. 

## "totales_acum": 
## La suma de totales por dia      

## "casos_por_1000hab"
## totales_acum/poblacion del departamento multiplicado por 1000

## "pob"  
## poblacion del departamento, en PBA datos de 2010 
## de Wikipedia. En CABA datos de 2017 sacados de la web.  
##(ver levantarDatos.R)   

## "nro_semana" 
#el numero de semana a partir del 2 de marzo  de 2020        

## "semana" 
## El dia de lunes de cada semana nro_semana

##  "dia_semana" si fue lunes martes etc. 

## "nro_intervalo" se divide el tiempo cada 3 dias a partir del 2 de marzo  de 2020  
## el nro de intervalo es 1 para los primeros 3 dias, 2 para los 5 6 y 7 de marzo de 2020 etc. 


# datos_ordenados 
## "departamento":
## Nombre del departamento Lomas de Zamora, Avellaneda, CABA, etc. 

## "fecha": fecha  es un dato tipo "Date", 
## corresponde a la fecha de diagnostico. Si esa fecha es NaN se reemplaza 
## por la fecha de apertura + 2 dias.
## El tipo de dato es Date  procesado con el paquete lubridate, pero se puede 
## ver como texto en formato yyyy-mm-dd   

## "totales_por_dia": 
## numero de confirmados en el dia. Se toma como caso confirmado aquel que 
## dia de la fecha.  totales_por_dia puede ser cero si 
## no hubo casos ese dia. 

## "totales_acum": 
## La suma de totales por dia      

## "casos_por_1000hab"
## totales_acum/poblacion del departamento multiplicado por 1000

## "pob"  
## poblacion del departamento, en PBA datos de 2010 
## de Wikipedia. En CABA datos de 2017 sacados de la web.  
##(ver levantarDatos.R)   

## "nro_semana" 
#el numero de semana a partir del 2 de marzo  de 2020        

## "semana" 
## El dia de lunes de cada semana nro_semana

##  "dia_semana" si fue lunes martes etc. 

## "nro_intervalo" se divide el tiempo cada 3 dias a partir del 2 de marzo  de 2020  
## el nro de intervalo es 1 para los primeros 3 dias, 2 para los 5 6 y 7 de marzo de 2020 etc. 

AMBA=c("CABA","Almirante Brown","Avellaneda","Berazategui","Berisso",
       "Brandsen","Campana","Cańuelas","Ensenada","Escobar","Esteban Echeverría",
       "Exaltación de la Cruz","Ezeiza","Florencio Varela",
       "General Las Heras","General Rodríguez",
       "General San Martín","Hurlingham","Ituzaingó",
       "José C. Paz","La Matanza","Lanús","La Plata","Lomas de Zamora","Luján",
       "Marcos Paz","Malvinas Argentinas","Moreno","Merlo","Morón",
       "Pilar","Presidente Perón","Quilmes","San Fernando","San Isidro","San Miguel",
       "San Vicente","Tigre","Tres de Febrero","Vicente López","Zárate")

# con los datos de CABA habia un problema con muchos NAN en varias Comunas, por eso colapso todo en una. 

datAMBA_original=datos_ordenados%>%filter(departamento %in% AMBA)

#saco los ultimos 3 dias por temas la base no esta actualizada a esa fecha. 
fechaUltima=(max(datAMBA_original$fecha))
semana_ultima=max(datAMBA_original$nro_semana)-1		

datAMBA_original=datAMBA_original%>%filter(fecha<=fechaUltima)
datAMBA_original=datAMBA_original%>%group_by(departamento)%>%mutate(casos_acumulados_cada_cienmilhab=100*casos_por_1000hab,
                                                                    casos_acumulados_normalizados=totales_acum/max(totales_acum))
#                                                                    derivada=derivar(casos_acumulados_cada_cienmilhab,lag=5),
#                                                                    derivada2=derivar(derivada,lag=5))







######### cosas para testear. 
fechtest="2020-04-23";
#max(datAMBA_original$fecha-10)
#datAMBA_original<-datAMBA_original%>%filter(fecha==fechtest)
#ranked_by_year=datAMBA_original%>%filter(fecha>fechtest)%>%ungroup()%>%group_by(fecha)%>%

###################################################
###############CASOS CONFIRMADOS#############################
###################################################




ranked_by_year=datAMBA_original%>%ungroup()%>%group_by(fecha)%>%
  arrange(semana,-casos_acumulados_cada_cienmilhab)%>%
  mutate(rank=1:n())%>%
  filter(rank<=10)%>%mutate(value=casos_acumulados_cada_cienmilhab,
                            Value_rel = value/value[rank==1],
                            Value_lbl = paste0(" ",round(value)))

staticplot = ggplot(ranked_by_year, aes(rank, group = departamento, 
                                       fill = as.factor(departamento), color = as.factor(departamento))) +
  geom_tile(aes(y = value/2,
                height = value,
                width = 0.9), alpha = 0.8, color = NA) +
  geom_text(aes(y = 0, label = paste(departamento, " ")),size= 5, vjust = 0.2, hjust = 1) +
  geom_text(aes(y=value,label = Value_lbl, hjust=0)) +
  coord_flip(clip = "off", expand = TRUE) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_reverse() +
  guides(color = FALSE, fill = FALSE) 

my_theme=theme(axis.line=element_blank(),
      axis.text.x=element_blank(),
      axis.text.y=element_blank(),
      axis.ticks=element_blank(),
      axis.title.x=element_blank(),
      axis.title.y=element_blank(),
      legend.position="none",
      panel.background=element_blank(),
      panel.border=element_blank(),
      panel.grid.major=element_blank(),
      panel.grid.minor=element_blank(),
      panel.grid.major.x = element_line( size=.1, color="black" ),
      panel.grid.minor.x = element_line( size=.1, color="black" ),
      plot.title=element_text(size=18, hjust=0.5, face="bold", colour="black", vjust=1),
      plot.subtitle=element_text(size=18, hjust=.5, face="italic", color="black"),
      plot.caption =element_text(size=12, hjust=.5, face="italic", color="black"),
      plot.background=element_blank(),
      plot.margin = margin(2,2, 2, 5, "cm"))

staticplot=staticplot+my_theme #+ labs0

anim = staticplot + transition_states(fecha, transition_length = 4, state_length = 1) +
  view_follow(fixed_x = TRUE)  +
  labs(title = 'Confirmados acumulados cada cien mil Hab.',  
       subtitle  =  "Top 10 AMBA. Fecha: {paste(lubridate::day(closest_state),lubridate::month(closest_state,label=TRUE))}",
       caption   = " Fuente: Datos públicos del Ministerio de Salud  \n Reproducción del análisis: @github.com/jdgonzalezwork \n  Otras herramientas de visualización en: http://www.ic.fcen.uba.ar")+
       ease_aes('cubic-in-out') + 
       enter_fade() +
       exit_fade() 

animacion=animate(anim,nframes=500)
anim_save("animacionConfirmadosAMBA.gif", animation = animacion)



#For Video (MP4) File Format:
  # For MP4
#animate(anim, 200, fps = 20,  width = 1200, height = 1000)-> for_mp4

#for_mp4

# anim_save("animation.gif", animation = animacion)
# anim_save("animation.mp4", animation = animacion,renderer = ffmpeg_renderer() )

###################################################
###############MUERTES#############################
###################################################

ranked_by_year=datAMBA_original%>%mutate(muertesporcienmil=1e+5*muertes_acum/pob)%>%ungroup()%>%group_by(fecha)%>%
  arrange(semana,-muertesporcienmil)%>%
  mutate(rank=1:n())%>%
  filter(rank<=10)%>%mutate(value=muertesporcienmil,
                            Value_rel = value/value[rank==1],
                            Value_lbl = paste0(" ",round(value,digits=2)))


staticplot = ggplot(ranked_by_year, aes(rank, group = departamento, 
                                        fill = as.factor(departamento), color = as.factor(departamento))) +
  geom_tile(aes(y = value/2,
                height = value,
                width = 0.9), alpha = 0.8, color = NA) +
  geom_text(aes(y = 0, label = paste(departamento, " ")),size= 5, vjust = 0.2, hjust = 1) +
  geom_text(aes(y=value,label = Value_lbl, hjust=0)) +
  coord_flip(clip = "off", expand = TRUE) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_reverse() +
  guides(color = FALSE, fill = FALSE) 

my_theme=theme(axis.line=element_blank(),
               axis.text.x=element_blank(),
               axis.text.y=element_blank(),
               axis.ticks=element_blank(),
               axis.title.x=element_blank(),
               axis.title.y=element_blank(),
               legend.position="none",
               panel.background=element_blank(),
               panel.border=element_blank(),
               panel.grid.major=element_blank(),
               panel.grid.minor=element_blank(),
               panel.grid.major.x = element_line( size=.1, color="black" ),
               panel.grid.minor.x = element_line( size=.1, color="black" ),
               plot.title=element_text(size=18, hjust=0.5, face="bold", colour="black", vjust=1),
               plot.subtitle=element_text(size=18, hjust=.5, face="italic", color="black"),
               plot.caption =element_text(size=12, hjust=.5, face="italic", color="black"),
               plot.background=element_blank(),
               plot.margin = margin(2,2, 2, 5, "cm"))


#labs0=labs(title = 'Muertes acumuladas cada cien mil Hab.',  
#     subtitle  =  "Top 10 AMBA. Fecha: {paste(lubridate::day(closest_state),lubridate::month(closest_state,label=TRUE))}",
#     caption   = " Fuente de datos: MinSal \n Reproducción del análisis: @github.com/jdgonzalezwork \n  Otras herramientas de visualización en: http://www.ic.fcen.uba.ar")

staticplot=staticplot+my_theme# + labs0

anim = staticplot + transition_states(fecha, transition_length = 4, state_length = 1) +
  view_follow(fixed_x = TRUE)  +
  labs(title = 'Muertes acumuladas cada cien mil Hab.',  
       subtitle  =  "Top 10 AMBA. Fecha: {paste(lubridate::day(closest_state),lubridate::month(closest_state,label=TRUE))}",
       caption   = " Fuente: Datos públicos del Ministerio de Salud \n Reproducción del análisis: @github.com/jdgonzalezwork \n  Otras herramientas de visualización en: http://www.ic.fcen.uba.ar")+
  ease_aes('cubic-in-out') + 
  enter_fade() +
  exit_fade() 

animacion=animate(anim,nframes=500)
anim_save("animacionMuertesAMBA.gif", animation = animacion)
#animacionConfirmadosAMBA
# 
# ##########################################################
# ############### Casos  por día #################
# ##########################################################
# flim=fechaUltima-5
# 
# datAMBA_medianaMovil7dias=datAMBA_original%>%
#                           group_by(departamento)%>%
#                           mutate(totales_por_dia_suav=suavizado(totales_por_dia,n=7),pob=pob[1],
#                                  totales_por_dia_cada_cien_mil=1e+5*totales_por_dia/pob,
#                                  totales_por_dia_suav_cada_cien_mil=1e+5*totales_por_dia_suav/pob)
# datAMBA_medianaMovil7dias=datAMBA_medianaMovil7dias%>%filter(fecha<flim)
# mean(is.na(datAMBA_medianaMovil7dias$totales_por_dia_suav_cada_cien_mil))
# x=suavizado(datAMBA_original$totales_por_dia[datAMBA_original$departamento=="CABA"],n=7)
# sum(is.na(x))
# 
# ggplot(datAMBA_medianaMovil7dias%>%filter(departamento %in% c("Almirante Brown","CABA","Avellaneda")),
#        aes(x=fecha,y=totales_por_dia_suav_cada_cienmil,group=departamento,color=departamento))+
#        geom_line(size=2) + 
#        geom_point(aes(x=fecha,y=totales_por_dia_cada_cien_mil))
# 
# 
# ggplot(datAMBA_medianaMovil7dias%>%filter(departamento %in% c("Almirante Brown","CABA")),
#          aes(x=fecha,y=totales_por_dia_suav_cada_cien_mil,group=departamento,color=departamento))+
#   geom_line(size=2) + 
#   geom_point(aes(x=fecha,y=totales_por_dia_cada_cien_mil))
# 
# 
# #%>%filter(fecha==flim-10)
# ranked_by_year=datAMBA_medianaMovil7dias%>%ungroup()%>%group_by(fecha)%>%
#   arrange(semana,-totales_por_dia_suav_cada_cien_mil)%>%
#   mutate(rank=1:n())%>%
#   filter(rank<=10)%>%mutate(value=totales_por_dia_suav_cada_cien_mil,
#                             Value_rel = value/value[rank==1],
#                             Value_lbl = paste0(" ",round(value)))
# staticplot = ggplot(ranked_by_year, aes(rank, group = departamento, 
#                                         fill = as.factor(departamento), color = as.factor(departamento))) +
#   geom_tile(aes(y = value/2,
#                 height = value,
#                 width = 0.9), alpha = 0.8, color = NA) +
#   geom_text(aes(y = 0, label = paste(departamento, " ")),size= 5, vjust = 0.2, hjust = 1) +
#   geom_text(aes(y=value,label = Value_lbl, hjust=0)) +
#   coord_flip(clip = "off", expand = TRUE) +
#   scale_y_continuous(labels = scales::comma) +
#   scale_x_reverse() +
#   guides(color = FALSE, fill = FALSE) 
# 
# my_theme=theme(axis.line=element_blank(),
#                axis.text.x=element_blank(),
#                axis.text.y=element_blank(),
#                axis.ticks=element_blank(),
#                axis.title.x=element_blank(),
#                axis.title.y=element_blank(),
#                legend.position="none",
#                panel.background=element_blank(),
#                panel.border=element_blank(),
#                panel.grid.major=element_blank(),
#                panel.grid.minor=element_blank(),
#                panel.grid.major.x = element_line( size=.1, color="black" ),
#                panel.grid.minor.x = element_line( size=.1, color="black" ),
#                plot.title=element_text(size=18, hjust=0.5, face="bold", colour="black", vjust=1),
#                plot.subtitle=element_text(size=18, hjust=.5, face="italic", color="black"),
#                plot.caption =element_text(size=8, hjust=.5, face="italic", color="black"),
#                plot.background=element_blank(),
#                plot.margin = margin(2,2, 2, 5, "cm"))
# 
# 
# #labs0=labs(title = 'Confirmados Acumulados cada 100 mil Hab. {321321321}',  
# #     subtitle  =  "Top 10 AMBA",
# #     caption  = "Fuente de Datos: MinSal")
# staticplot=staticplot+my_theme #+ labs0
# 
# anim = staticplot + transition_states(fecha, transition_length = 4, state_length = 1) +
#   view_follow(fixed_x = TRUE)  +
#   labs(title = 'Casos confirmados cada cien mil Hab.',  
#        subtitle  =  "Top 10 AMBA. Fecha: {paste(lubridate::day(closest_state),lubridate::month(closest_state,label=TRUE))}",
#        caption   = " Fuente e datos: MinSal \n Reproducción del análisis: @github.com/jdgonzalezwork \n  Otras herramientas de visualización en: http://www.ic.fcen.uba.ar")+
#   ease_aes('cubic-in-out') + 
#   enter_fade() +
#   exit_fade() 
# 
#   
# animacion=animate(anim,nframes=100)
# 
# #animacion
# #anim_save("casos_diarios.gif", animation = animacion)
# 

##############################################################################################
############### Casos por SEMANA (Muy discontinuos.. no quedan bien) #########################
##############################################################################################
# flim=fechaUltima-7
# 
# datAMBA_semanal=datAMBA_original%>%group_by(departamento,semana)%>%arrange(departamento,semana,fecha)%>%
#   summarize(totales_por_semana=sum(totales_por_dia),nro_semana=first(nro_semana),pob=first(pob),
#             totales_por_semana_cada_cienmil=1e+5*sum(totales_por_dia)/pob)
# 
# datAMBA_original=datAMBA_original%>%group_by(departamento)%>%mutate(casos_por_1000hab2=1e+3*cumsum(totales_por_dia)/pob)
# datAMBA_original$casos_por_1000hab-datAMBA_original$casos_por_1000hab2
# 
# 
# datAMBA_semanal$totales_por_semana_cada_cienmil-datAMBA_semanal$totales_por_semana_cada_cienmil2
# 
# 
# 
# datAMBA_semanal=datAMBA_semanal%>%group_by(departamento)%>%
#                 mutate(labelsem=paste(lubridate::day(semana),"-",
#                                        lubridate::month(semana,label=TRUE),
#                                        " (semana nro",nro_semana,")",sep=""))%>%
#                 filter(semana<flim)%>%ungroup()
# 
# pltDAT=datAMBA_semanal%>%filter(nro_semana<109, nro_semana>1 )%>%filter(departamento %in% c("CABA"))
# ggplot(pltDAT,
#        aes(x=semana,y=totales_por_semana_cada_cienmil,group=departamento,color=departamento))+
#   geom_col(size=1,fill=NA,color="black") + 
#   geom_col(data=datAMBA_original%>%filter(departamento %in% c("CABA"),nro_semana<109, nro_semana>1),
#              aes(x=fecha,y=1e+5*totales_por_dia/pob,fill=departamento),color="black")
# 
# 
# 
# #%>%filter(fecha==flim-10)
# ranked_by_year=datAMBA_semanal%>%ungroup()%>%group_by(semana)%>%
#   arrange(semana,-totales_por_semana_cada_cienmil)%>%
#   mutate(rank=1:n())%>%
#   filter(rank<=10)%>%mutate(value=totales_por_semana_cada_cienmil,
#                             Value_rel = value/value[rank==1],
#                             Value_lbl = paste0(" ",round(value)))
# staticplot = ggplot(ranked_by_year, aes(rank, group = departamento, 
#                                         fill = as.factor(departamento), color = as.factor(departamento))) +
#   geom_tile(aes(y = value/2,
#                 height = value,
#                 width = 0.9), alpha = 0.8, color = NA) +
#   geom_text(aes(y = 0, label = paste(departamento, " ")),size= 5, vjust = 0.2, hjust = 1) +
#   geom_text(aes(y=value,label = Value_lbl, hjust=0)) +
#   coord_flip(clip = "off", expand = TRUE) +
#   scale_y_continuous(labels = scales::comma) +
#   scale_x_reverse() +
#   guides(color = FALSE, fill = FALSE) 
# 
# my_theme=theme(axis.line=element_blank(),
#                axis.text.x=element_blank(),
#                axis.text.y=element_blank(),
#                axis.ticks=element_blank(),
#                axis.title.x=element_blank(),
#                axis.title.y=element_blank(),
#                legend.position="none",
#                panel.background=element_blank(),
#                panel.border=element_blank(),
#                panel.grid.major=element_blank(),
#                panel.grid.minor=element_blank(),
#                panel.grid.major.x = element_line( size=.1, color="black" ),
#                panel.grid.minor.x = element_line( size=.1, color="black" ),
#                plot.title=element_text(size=18, hjust=0.5, face="bold", colour="black", vjust=1),
#                plot.subtitle=element_text(size=18, hjust=.5, face="italic", color="black"),
#                plot.caption =element_text(size=8, hjust=.5, face="italic", color="black"),
#                plot.background=element_blank(),
#                plot.margin = margin(2,2, 2, 5, "cm"))
# 
# 
# #labs0=labs(title = 'Confirmados Acumulados cada 100 mil Hab. {321321321}',  
# #     subtitle  =  "Top 10 AMBA",
# #     caption  = "Fuente de Datos: MinSal")
# staticplot=staticplot+my_theme #+ labs0
# 
# anim = staticplot + transition_states(semana, transition_length = 4, state_length = 1) +
#   view_follow(fixed_x = TRUE)  +
#   labs(title = 'Casos Confirmados por cien mil Hab.',  
#        subtitle  =  "Top 10 AMBA. Fecha: {closest_state}",
#        caption  = " datos: MinSal - reproduccion de análisis: @github.com/jdgonzalezwork")+
#   ease_aes('cubic-in-out') + 
#   enter_fade() +
#   exit_fade() 
# 
# animacion=animate(anim,nframes=50)
# #animacion
# #anim_save("casos_diarios.gif", animation = animacion)
# 
# 
