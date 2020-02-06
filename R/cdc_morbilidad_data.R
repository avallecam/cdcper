#' @title Llamar base de datos de morbilidad consolidada segun Area administrativa.
#'
#' @description Llama y genera base de datos consolidada de morbilidad  segun departamento, provincia o distrito.
#'
#' @describeIn his_extraer Funcion para llamar base de datos por Area administrativa
#'
#' @param departamento nombre del departamento almacenado en cdc_peru, tipo caracter
#' @param provincia nombre de la provincia, tipo caracter
#' @param distrito nombre del distrito, tipo caracter
#' @param ruta especifica ruta
#'
#' @import magrittr
#' @import dplyr
#'
#'
#' @return Base de datos de morbilidad por Area administrativa
#'
#' @export his_extraer
#'
#'
#' @examples
#'
#' # library(tidyverse)
#' # library(magrittr)
#' # library(dplyr)
#'
#' # # Extraemos la base de datos
#' # lima<-his_extraer("lima")
#'
his_extraer<-function(departamento,provincia=NULL,distrito=NULL,ruta="C:/Users/USER/Dropbox/cdc_peru/his_r/data/his_purrr/"){
  dep1<- readRDS(paste0(ruta,departamento,".rds"))
  names(dep1)[4]="iddist"
  dep1$nombprov<-toupper(dep1$nombprov)

  if (!is.null(provincia)){
    prov1<-dep1%>%
      filter(nombprov==provincia)%>%
      arrange(desc(casos))%>%
      ungroup()

  }
  else if(!is.null(distrito)){
    dist1<-prov1%>%
      filter(nombdist==distrito)%>%
      arrange(desc(casos))%>%
      ungroup()
  }
  return(dep1)
}

