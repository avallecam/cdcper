#' @title Crear mapa coropletico
#'
#' @description Generacion de mapas coropleticos por conteo y/o por tasas, segun area administrativa.
#'
#' @describeIn cdc_mapa_morb funcion para crear mapa coropletico
#'
#' @param provincia nombre de la provincia
#' @param enf nombre de la enfermedad segun CIE-10
#' @param anio anio o intervalo de anios
#' @param tipo 1=Conteo total & 2=Tasas
#' @param shape_departamento shape_departamento
#' @param shape_provincia shape_provincia
#' @param shape_distrito shape_distrito
#' @param poblacion_distrito_csv poblacion_distrito_csv
#'
#' @import magrittr
#' @import dplyr
#'
#'
#' @return Diagrama de Pareto por Area administrativa.
#'
#' @export cdc_mapa_morb
#'
#' @examples
#'
#' # library(tidyverse)
#' # library(magrittr)
#' # library(dplyr)
#' # library(sf)
#'
#' # # Extraemos la base de datos
#' # lima<-cdc_extraer("lima")
#' # # Diagrama de Pareto por departamento
#' # cdc_mapa_morb(lima, "LIMA","Anemias nutricionales",2015,2)
#' # Uso de pipe
#' # lima%>%cdc_mapa_morb(provincia = "LIMA",distrito = "Villa El Salvador",year=2016)
#'

cdc_mapa_morb<-function(data,prov,enf,anio,tip,shape_departamento="C:/Users/USER/Dropbox/cdc_peru/shape_r/Limites/departamento_geo.shp",shape_provincia="C:/Users/USER/Dropbox/cdc_peru/shape_r/Limites/provincia_geo.shp",shape_distrito="C:/Users/USER/Dropbox/cdc_peru/shape_r/Limites/DISTRITOS.shp",poblacion_distrito_csv="C:/Users/USER/Dropbox/cdc_peru/shape_r/Limites/pob_inei.csv"){
  dep1<-data
  names(dep1)[4]="iddist"
  dep1$nombprov<-toupper(dep1$nombprov)
  #Introduciomos los shapes
  departamento<-st_read(shape_departamento)
  provincia<-st_read(shape_provincia)
  distrito<-st_read(shape_distrito)
  names(departamento)<-tolower(names(departamento))
  names(provincia)<-tolower(names(provincia))
  names(distrito)<-tolower(names(distrito))
  mp1<-distrito%>%filter(provincia==prov)
  dt2<-dep1%>%
    group_by(ano,nombprov,iddist,list_12_110)%>%
    summarise(Total=sum(casos))%>%
    filter(ano%in%c(anio),nombprov==prov,list_12_110==enf)%>%
    arrange(desc(Total))%>%
    ungroup()
  #Introducimos data poblacion
  pob_inei<-read.csv(poblacion_distrito_csv)
  # Unir data
  names(pob_inei)=c("ord","iddist","nombdep","nombprov","nombdist","ano","poblacion")
  pob_inei<-pob_inei%>%mutate(iddist=as.character(iddist))
  pob_n<-pob_inei%>%
    group_by(iddist,nombdep,nombprov,nombdist,ano)%>%
    summarise(poblacion=sum(poblacion))%>%
    filter(ano%in%c(anio),nombprov==prov)%>%
    arrange(desc(poblacion))%>%
    ungroup()
  gr2<-left_join(x=dt2,y=pob_n,by="iddist")
  gr2<-gr2%>%mutate(Tasa=(Total/poblacion)*1000)
  # Unimos la base con el shape
  mapa<-left_join(x=mp1,y=gr2, by="iddist")
  if (tip==1){
    # Creamos mapa de totales
    mapa%>%ggplot()+geom_sf(aes(fill=Total))+
      scale_fill_viridis_c(alpha = 1, begin = 0, option = "B", end = 1, direction=-1)+
      ggtitle(paste( "Provincia de",prov,"-",anio),
              subtitle =paste("n? Casos de",enf))+
      xlab("Longitud") + ylab("Latitud")
  }
  else if(tip==2){
    # Creamos mapa de tasas
    mapa%>%ggplot()+geom_sf(aes(fill=Tasa))+
      scale_fill_viridis_c(alpha = 1, begin = 0, option = "D", end = 1, direction=-1)+
      ggtitle(paste( "Provincia de",prov,"-",anio),
              subtitle =paste("Tasa de",enf,"x 1000 Hab."))+
      xlab("Longitud") + ylab("Latitud")
  }
}

