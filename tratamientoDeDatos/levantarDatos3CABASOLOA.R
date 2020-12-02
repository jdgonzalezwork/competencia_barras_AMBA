rm(list=ls())

### El siguiente es un script que levanta los datos 
### Esta centrado en la poblacion de la provincia y CABA. 

########################################################
#######################################################
# Lineas dedicadas a leer el archivo fuente del momento,  
# y luego guardadas en un .RData   
# file1="Covid19Casos_17062020.csv"
#file1="Covid19Casos_22_06_2020.csv"
#file1="Covid19Casos_28_06_2020.csv"
#file1="Covid19Casos_06_sep.csv"
#file1="Covid19Casos_13_sep.csv"
#file1="Covid19Casos_9_10.csv"
#file1="Covid19Casos_30_11.csv"
#valores=read.csv2(file1,sep=",",stringsAsFactors = F,encoding="UTF-8")
#save.image(file="datosCrudos13_sep_2020.RData")
#save.image(file="datosCrudos9_oct_2020.RData")
#save.image(file="datosCrudos30_nov_2020.RData")
########################################################
#######################################################

load("datosCrudos30_nov_2020.RData")
library(dplyr)
library(lubridate)
library(ggplot2)
names(valores)
n1=dim(valores)[1]
valores=as_tibble(valores)
valoresConfirmadosFall=valores%>%filter(fallecido=="SI",clasificacion_resumen %in% c("Confirmado"))
nrow(valoresConfirmadosFall)


#### Fitro datos de la Prov. de  Bs As y de CABA. 
valoresPBA=valores%>%filter(residencia_provincia_nombre=="Buenos Aires") 
valoresCABA=valores%>%filter(residencia_provincia_nombre=="CABA") 

unique(valoresCABA$residencia_departamento_nombre)
##############################################################
### Interesado en modelar CABA como un Unico departamento  ### 
###  Colapso todas las comunas a un solo valor.    ###########                                                   ## 
##############################################################
#saco esta linea, dado que ahora no lo tengo mas desagregado. 
valoresCABA$residencia_departamento_nombre="CABA" 

##########################################################

##Pego los dos data frames uno debajo del otro.  
valores=rbind(valoresPBA,valoresCABA)



##########################################################
## Leo archivos de Poblacion de la provincia de Bs. As. 
###ANTES:(Censo 2010, fuente Wikipedia)####################
###AHORA:   http://www.estadistica.ec.gba.gov.ar/dpe/images/Proyecciones_x_municipio__2010-2025.pdf                           #####################
#########################################################

#pop=read.csv2(file="pop.csv",sep=",",stringsAsFactors = F,encoding="UTF-8")
popOld=read.csv2(file="pop.csv",sep=",",stringsAsFactors = F,encoding="UTF-8")
pop=read.csv2(file="popActualizada.csv",sep=",",stringsAsFactors = F,encoding="UTF-8")
pop=as_tibble(pop[,c(1,6)])%>%rename(residencia_departamento_nombre=Partido,pob=X2020)
popOld=popOld[,2:3]
#popOld$residencia_departamento_nombre[pop$residencia_departamento_nombre=="General Madariaga"]="General Juan Madariaga"

names(popOld)
names(pop)
##Comparo los nombres del csv con los de la base minsal. 
A=sort(unique(valores$residencia_departamento_nombre))
B=sort(unique(pop$residencia_departamento_nombre))
C=sort(unique(popOld$residencia_departamento_nombre))
setdiff(A,B)
setdiff(B,A)
setdiff(B,C)
#pop=read.csv2(file="pop.csv",sep=",",stringsAsFactors = F,encoding="UTF-8")
popOld=read.csv2(file="pop.csv",sep=",",stringsAsFactors = F,encoding="UTF-8")
pop=read.csv2(file="popActualizada.csv",sep=",",stringsAsFactors = F,encoding="UTF-8")
pop=as_tibble(pop[,c(1,6)])%>%rename(residencia_departamento_nombre=Partido,pob=X2020)
popOld=popOld[,2:3]
popOld$residencia_departamento_nombre[pop$residencia_departamento_nombre=="General Madariaga"]="General Juan Madariaga"

names(popOld)
names(pop)
##Comparo los nombres del csv con los de la base minsal. 
A=sort(unique(valores$residencia_departamento_nombre))
B=sort(unique(pop$residencia_departamento_nombre))
C=sort(unique(popOld$residencia_departamento_nombre))
setdiff(A,B)
setdiff(B,A)
setdiff(B,C)
setdiff(C,B)
setdiff(B,A)
setdiff(B,A)

# Analisis de crecimiento Poblacional: 
#
#
popOld=popOld%>%rename(pob2010=pob)
popAnalisis=pop%>%inner_join(popOld,by="residencia_departamento_nombre")%>%
mutate(crecimiento_absoluto=pob - pob2010,
crecimiento_porcentual= 100*(crecimiento_absoluto/pob2010))
print(popAnalisis%>%arrange(desc(crecimiento_porcentual)),n=20)
#View(popAnalisis%>%arrange(desc(crecimiento_porcentual)))

#comentario: tordillo y lezama no tienen casos hasta el momento. 

#sum(valores$residencia_departamento_nombre=="Lezama")
#sum(valores$residencia_departamento_nombre=="Tordillo")

############################################################################
####cambio un nombre en particular para que coincida con la info del minsal 
###########################################################################
pop$residencia_departamento_nombre[pop$residencia_departamento_nombre=="General Madariaga"]="General Juan Madariaga"

##############################################################################
#############################################################################
###Levanto datos de poblacion de las comunas de CABA (2017) ################## 
###Fuente: https://data.buenosaires.gob.ar/dataset/estructura-demografica/archivo/a24fc775-ad6e-4bb0-8a7a-2283c2adea34
###########################################################################################
###############################################################################
pop2=read.csv2(file="gcba_pob_comunas_17.csv",sep=",",stringsAsFactors = F,encoding="UTF-8")
pop=rbind(pop,pop2)
filaCABA=c("CABA", sum(pop2$pob))
pop=rbind(pop,filaCABA)
#SI ES COMUNA SE BUSCA ASI grepl(aa, pattern="COMUNA")

## Pongo clasificacion en minuscula porque todos los demas campos 
## empiezan en minusculas

unique(valores$clasificacion_resumen)

#############################################
### El comando mutate genera nuevas columnas o reemplaza las existentes por la 
### asignacion que se defina.   En particular, se define la fecha como fecha diagnostico. 
### En lo siguiente, le doy formato de dato de tipo tiempo a las columnas que son fechas 
### utilizando el paquete lubridate esto permite restar dos fechas, por ejemplo.  
### la funcion coalesce, en teoria, remplaza los NaN por la indicacion 

valoresConfirmados=valores%>%filter(clasificacion_resumen== "Confirmado")
#guardo el numero de totales de confirmados para 
# ver si despes del procesamiento no pierdo casos

valoresConfirmadosFall=valores%>%filter(fallecido=="SI",clasificacion_resumen== "Confirmado")
muertesTotalesCheck0=nrow(valoresConfirmadosFall)
casosTotalesCheck0=nrow(valoresConfirmados)



valoresConfirmados=valoresConfirmados%>%
                        mutate(fecha_inicio_sintomas=ymd(fecha_inicio_sintomas),
                              fecha_apertura=ymd(fecha_apertura),
                              dif_fa_fis=fecha_apertura-fecha_inicio_sintomas,
                            #  fecha=fecha_inicio_sintomas,
                            #  fecha=coalesce(fecha_inicio_sintomas,fecha_apertura-3)
                              fecha=ymd(fecha_diagnostico),
                              fecha=coalesce(fecha,fecha_apertura+2)
                            )

########################################################################
### Si el caso en estudio es menor a un a?o, les ponene el numero de meses. 
### para facilitar las cosas les pongo 0 a?os a todos los menores dee 1.

valoresConfirmados$edad[valoresConfirmados$edad_años_meses=="Meses"]=0

########################################################################
#########################################################################


diferencia=ymd(valoresConfirmados$fecha_apertura)-ymd(valoresConfirmados$fecha_diagnostico)
ggplot(as_tibble(diferencia),aes(x=as.numeric(diferencia)))+geom_histogram()+
xlim(-10,10) +ggtitle("Diferencia entre fehca de apertura y fecha de diagnostico")


######## Me genero una categoria que incluya en que intervalo de edad cae cada obs,.
faux=function(x){cut(x,c(0,20, 40, 50,60,70,90),include.lowest = F)}
valoresConfirmados=valoresConfirmados%>%mutate(edadCat=faux(edad),
                                        dif_fa_fis=as.numeric(dif_fa_fis))


######## Veo cuanto tarda el tiempo entre que reportan sintomas y la fecha de 
######## apertura (averiguar que es fecha de apertura exactamente!). 
ggplot(valoresConfirmados,aes(x=edadCat,y=dif_fa_fis))+
geom_violin()+ylim(-3,10)+geom_boxplot(width=0.2,color="black",fill="blue",size=1)+coord_flip()+ 
ggtitle("Diferencia entre fecha de apertura y fecha de inicio de sintomas") 


#### agrupo los datos por fecha y nombre de departamento 
dat=valoresConfirmados%>%group_by(residencia_departamento_nombre,fecha)
###Cuento cuantos casos hay por dia por cada departamento y por cada nombre. 
dat=dat%>%summarise(totalespordia=n(),muertestotalespordia=sum(fallecido=="SI"))

 #datM=valoresConfirmados%>%filter(fallecido=="SI")%>%
 #group_by(residencia_departamento_nombre,fecha)%>%
 #summarise(muertespordia=n())
 #dat<-left_join(datM,by=c("residencia_departamento_nombre","fecha"))

###Calculo los casos acumulados 
dat=dat%>%mutate(totales=cumsum(totalespordia),muertestotales=cumsum(muertestotalespordia))
# Agrego datos de poblacion al data frame 
dat=dat%>%left_join(as_tibble(pop),by="residencia_departamento_nombre")
dat$pob=as.numeric(dat$pob)
# Genero una columna con los casos confirmados por cada mil habitantes. 
dat=dat%>%mutate(CasosConfirmadosCada1000hab=1000*totales/pob)

filtrar=FALSE
if(filtrar){
########### selecciono los municipios que tienen mas de 75 casos totales,
########### o al menos 1 caso confirmados cada 1000 habitantes  
seleccionados=unique(dat%>%filter(CasosConfirmadosCada1000hab>1 | totales>75,month(fecha)==6)%>%
                       select(residencia_departamento_nombre))%>%
                      ungroup()
selec=seleccionados$residencia_departamento_nombre
######################################################
##Filtro los datos para esos municipios.############### 
######################################################
dat=dat%>%filter(residencia_departamento_nombre%in%selec)
}



######################################################################
######## El dise?o de la base es tal que si la fecha x
######## no hubo infectados para un departamento, esa fecha x no aparece 
######## generando "vacios" en la estructura temporal  ##############
######## lo siguiente es para completar esos vacios #################
####### Usando la funcion complete de tidyr (esta funcion es distinta de 
####### otra que usa dplyr por eso se la llama especificamente con la sintaxis 
#######  tidyr::complete. El problema viene de que tidyr y dplyr tienen nombres
##### iguales y no se puede cargar las dos librerias al mismo tiempo. Pero el paquete 
#### tidyr debe estar instalado pero NO cargado.  

datC=dat%>%mutate(fechaCompleta=fecha)%>%
               tidyr::complete(fechaCompleta=seq(min(fecha), max(fecha), by = "day"))
datC=datC%>%select(-pob);


###datC tiene las fechas vacias con NaN agrego 
# le pego la poblacion a cada caso. 
datC=datC%>%left_join(as_tibble(pop)%>%mutate(pob=as.numeric(pob)),by="residencia_departamento_nombre")
datC=datC%>%mutate(totalespordia1=coalesce(as.numeric(totalespordia),0),
            muertestotalespordia1=coalesce(as.numeric(muertestotalespordia),0))
datC=datC%>%mutate(totales=cumsum(totalespordia1),
                   CasosConfirmadosCada1000hab=1000*totales/pob,
			 muertestotales=cumsum(muertestotalespordia1))



####################################################################
# a cada fecha le asigno el numero de semana que transcurrieron desde t0 
# todo es del paquete lubrydate

# Semana seria el lunes de inicio de la seman. 
t0=dmy("02-03-2020");
datC=datC%>%mutate(nroSemana=(interval(t0,fechaCompleta))%/%weeks(1))
datC=datC%>%mutate(Semana=t0 + weeks(nroSemana))
datC=datC%>%mutate(nroIntervalo=interval(t0,fechaCompleta)%/%days(3))

#############################################
#############################################
datosOrdenados=datC%>%mutate(totalespordia=totalespordia1,
fecha=fechaCompleta,
totalesAcum=totales,
muertesAcum=muertestotales,
muertestotalespordia=muertestotalespordia1)
datosOrdenados=datosOrdenados%>%select(-totalespordia1,-totales,
-fechaCompleta,-muertestotalespordia1)
datosOrdenados=datosOrdenados%>%mutate(diaDeLaSemana=wday(fecha,label=T))
names(datosOrdenados) 

#### Paso todo a formato guion bajo 
datos_ordenados=datosOrdenados%>%
              rename(dia_semana=diaDeLaSemana,
                     totales_por_dia=totalespordia,
                     totales_acum=totalesAcum,
			   muertes_acum=muertesAcum,
			   muertes_totales_por_dia=muertestotalespordia,
                     departamento=residencia_departamento_nombre,
                     semana=Semana,
                     casos_por_1000hab=CasosConfirmadosCada1000hab,
                     nro_semana= nroSemana,
                     nro_intervalo=nroIntervalo
                     )

casosTotalesCheck=sum(datos_ordenados$totales_por_dia)
casosTotalesCheck0==casosTotalesCheck 
muertesTotalesCheck=sum(datos_ordenados$muertes_totales_por_dia)
muertesTotalesCheck0=muertesTotalesCheck
save(file="datos_ordenados_9_10_2020.RData",datos_ordenados)


# test=grepl(datos_ordenados$departamento, pattern="COMUNA")
#  datCABA= datos_ordenados[test,]; 
# ggplot(datCABA,aes(x=semana,y= 1e+6*muertestotales/pob, color=departamento))+geom_line()

