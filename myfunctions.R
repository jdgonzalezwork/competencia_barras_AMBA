ordenar_dto_respecto_variable=function(X,varx,vary,ver_n_departamentos,dig=2,
                                       vary_creciente=FALSE){
  #rutina que ordena las variables segun varx y vary y reemplaza departamento
  # por departamento + ( valor de variable)
  X<-X%>%ungroup()
  departamentos_con_mas_casos=X%>%filter(get(varx)==(max(get(varx))-1))%>%
    ungroup()%>%arrange(-get(vary))%>%slice(1:ver_n_departamentos)%>%
    mutate(factoraux=paste(departamento," (",round(get(vary),digits=dig),")",sep=""), 
             departamento_factor=factor(factoraux,levels=factoraux[order(-get(vary))]) )%>%
    select(departamento,departamento_factor)
  
  if(vary_creciente){
         departamentos_con_mas_casos=X%>%group_by(departamento)%>%
          summarise(el_mayor=max(get(vary)))%>%
          ungroup()%>%arrange(-el_mayor)%>%slice(1:ver_n_departamentos)%>%
          mutate(factoraux=paste(departamento," (",round(el_mayor,digits=dig),")",sep=""), 
                 departamento_factor=factor(factoraux,levels=factoraux[order(-el_mayor)]) )%>%
          select(departamento,departamento_factor)
  }
  
  selec1=departamentos_con_mas_casos%>% select(departamento)%>%unique()%>%pull()
    dat2=X%>%filter(departamento %in% selec1)%>%
    left_join(departamentos_con_mas_casos,by="departamento")%>%
    mutate(departamento=departamento_factor)%>%select(-departamento_factor)
}

derivar=function(x,lag){c(x[1:lag],diff(x,lag))} # deriva en grillas equiespaciadas.

promedio_movil=function(x,lag=7){ 
  # calcula el promedio movil semanal a partir de Acum. 
  # PELIGRO, funcion no totalmente testeada
  (c(x[1:lag],diff(x,lag)))/lag
}



mediana_movil<-function(x,n=7){
 # Depende de la library(seismicRoll) (debe estar instalada previamente)
 # calcula mediana movil semanal, como pone NaNs al principio y al final, 
 # agrego un "truco", de manera que complete con los datos
  k=n %/% 2
  N=length(x)
  xx<-c(rep(x[1],k),x,rep(x[N],k))
  aux<-seismicRoll::roll_median(xx,n=n)
  s=aux[(k+1):(length(aux)-k)]
  s
}

suavizado<-function(x,n=7){
  #un suavizado "pseudo-robusto".
   k=n %/% 2 #alineado en el centro 
  N=length(x)
   xx<-c(rep(x[1],k),x,rep(x[N],k)) #alineado en el centro
  #xx<-c(rep(x[1],n),x) #alineado a derecha?  
  aux<-seismicRoll::roll_mean(mediana_movil(xx))
#  alineado en el centro: 
  s=aux[(k+1):(length(aux)-k)]
#   s=aux[(n+1):(length(aux))]
  s
}

# 
# x=sin(1:100)
# x[sample(1:length(x),8)]=100
# plot(x)
# points(seismicRoll::roll_median(x,n=7),col=3) 
# s=mediana_movil(x,n=7)
# points(s,col=6,pch=19,cex=.41)
# ss=seismicRoll::roll_mean(x)
# points(ss,col=9,pch=19,cex=.41)
# su=suavizado(x)
# points(su,col=19,pch=1,cex=1)
# length(mediana_movil(x))
# length(su)
meses=c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dec")
