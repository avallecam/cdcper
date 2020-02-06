#' @title Crear diagrama de Pareto para datos de morbilidad
#'
#' @description GeneraciOn de diagrama de Pareto para datos de morbilidad, tablas de frecuencias y frecuecias acumuladas, por Area administrativa.
#'
#' @describeIn cdc_pareto funcion para crear diagrama de Pareto
#'
#' @param departamento nombre del departamento extraido por cdc_extraer
#' @param provincia nombre de la provincia, tipo caracter
#' @param distrito nombre del distrito, tipo caracter
#' @param year year o intervalo de years
#'
#' @import magrittr
#' @import dplyr
#'
#'
#' @return Diagrama de Pareto por Area administrativa.
#'
#' @export cdc_pareto
#'
#' @examples
#'
#' # library(tidyverse)
#' # library(magrittr)
#' # library(dplyr)
#'
#' # # Extraemos la base de datos
#' # lima<-cdc_extraer("lima")
#' # # Diagrama de Pareto por departamento
#' # cdc_pareto(lima,year=2012)
#' # # Diagrama de Pareto por provincia
#' # cdc_pareto(lima,provincia="LIMA",year=2014)
#' # # Diagrama de Pareto por distrito
#' # cdc_pareto(lima,provincia = "LIMA",distrito = "Villa El Salvador",year=2016)
#' # Uso de pipe
#' # lima%>%cdc_pareto(provincia = "LIMA",distrito = "Villa El Salvador",year=2016)
#'
cdc_pareto<-function(departamento,provincia=NULL,distrito=NULL,year){
  dep1<-departamento
  if(is.null(provincia)&is.null(distrito)){
    ej0<-dep1%>%group_by(ano,list_12_110)%>%summarise(Total=sum(casos))%>%
      filter(ano%in%c(year))%>%arrange(desc(Total))
    ej0<-ej0[,2:3]
    ej0<-ej0%>%arrange(desc(Total)) %>%
      mutate(
        cumsum = cumsum(Total),
        freq= round(Total / sum(Total), 3),
        cum_freq = cumsum(freq)
      )%>%top_n(50,Total)
    ## Grafico de barras
    pareto=barplot(ej0$Total,
                   width = 1, space = 0.2, border = NA, axes = F,
                   ylim = c(0, 1.05 * max(ej0$Total, na.rm = T)),
                   ylab = "n de Consultas externas" ,
                   xlab = "Causa",
                   cex.names = 0.7,
                   col="orange",
                   names.arg = 1:50,
                   las=1,
                   main = paste("Diagrama de Pareto","-","Nivel Regional"))
    ## Introduciendo eje izquierdo
    axis(side = 2, at = c(0, ej0$Total), las = 1, col.axis = "black", col = "black", tick = T, cex.axis = 0.6)
    ## El marco
    box( col = "black")
    ## Las lineas de frecuencia acumulada
    px <- ej0$cum_freq * max(ej0$Total, na.rm = T)
    lines(pareto, px, type = "b", cex = 0.7, pch = 19, col="red")
    ##Introducir el eje derecho
    axis(side = 4, at = c(0, px), labels = paste(c(0, round(ej0$cum_freq * 100)) ,"%",sep=""),
         las = 1, col.axis = "black", col = "black", cex.axis = 0.8, col.axis = "black")
    ## Introducir linea 80%
    i<-which(ej0$cum_freq<=0.84)
    abline(h=max(ej0$Total)*0.8, col="blue")
    abline(v=max(i),col="blue")
  }
  else if(!is.null(provincia)&is.null(distrito)){
    ej0<-dep1%>%group_by(ano,nombprov,list_12_110)%>%summarise(Total=sum(casos))%>%
      filter(ano%in%c(year),nombprov==provincia)%>%arrange(desc(Total))
    ej0<-ej0[,3:4]
    ej0<-ej0%>%arrange(desc(Total)) %>%
      mutate(
        cumsum = cumsum(Total),
        freq= round(Total / sum(Total), 3),
        cum_freq = cumsum(freq)
      )%>%top_n(50,Total)
    ## Grafico de barras
    pareto=barplot(ej0$Total,
                   width = 1, space = 0.2, border = NA, axes = F,
                   ylim = c(0, 1.05 * max(ej0$Total, na.rm = T)),
                   ylab = "n de Consultas externas" ,
                   xlab = "Causa",
                   cex.names = 0.7,
                   names.arg = 1:50,
                   las=1,
                   main = paste("Diagrama de Pareto","-","Provincia de",provincia))
    ## Introduciendo eje izquierdo
    axis(side = 2, at = c(0, ej0$Total), las = 1, col.axis = "grey62", col = "grey62", tick = T, cex.axis = 0.6)
    ## El marco
    box( col = "grey62")
    ## Las lineas de frecuencia acumulada
    px <- ej0$cum_freq * max(ej0$Total, na.rm = T)
    lines(pareto, px, type = "b", cex = 0.7, pch = 19, col="cyan4")
    ##Introducir el eje derecho
    axis(side = 4, at = c(0, px), labels = paste(c(0, round(ej0$cum_freq * 100)) ,"%",sep=""),
         las = 1, col.axis = "grey62", col = "cyan4", cex.axis = 0.8, col.axis = "cyan4")
    ## Introducir linea 80%
    i<-which(ej0$cum_freq<=0.84)
    abline(h=max(ej0$Total)*0.8, col="green")
    abline(v=max(i),col="green")
  }
  else if(!is.null(provincia)&!is.null(distrito)){
    ej0<-dep1%>%group_by(ano,nombprov,nombdist,list_12_110)%>%summarise(Total=sum(casos))%>%
      filter(ano%in%c(year),nombprov==provincia)%>%arrange(desc(Total))
    ej0<-ej0[,3:5]
    ej0<-ej0%>%group_by(nombdist,list_12_110)%>%filter(nombdist==distrito)
    ej0<-ej0[,2:3]
    ej0<-ej0%>%arrange(desc(Total)) %>%
      mutate(cumsum = cumsum(Total),
             freq= round(Total / sum(Total), 3),
             cum_freq = cumsum(freq)
      )%>%top_n(50,Total)
    ## Grafico de barras
    pareto=barplot(ej0$Total,
                   width = 1, space = 0.2, border = NA, axes = F,
                   ylim = c(0, 1.05 * max(ej0$Total, na.rm = T)),
                   ylab = "n de Consultas externas" ,
                   xlab = "Causa",
                   cex.names = 0.7,
                   col = "green",
                   names.arg = 1:50,
                   las=1,
                   main = paste("Diagrama de Pareto","-","Distrito de",distrito))
    ## Introduciendo eje izquierdo
    axis(side = 2, at = c(0, ej0$Total), las = 1, col.axis = "grey62", col = "grey62", tick = T, cex.axis = 0.6)
    ## El marco
    box( col = "grey62")
    ## Las lineas de frecuencia acumulada
    px <- ej0$cum_freq * max(ej0$Total, na.rm = T)
    lines(pareto, px, type = "b", cex = 0.7, pch = 19, col="red")
    ##Introducir el eje derecho
    axis(side = 4, at = c(0, px), labels = paste(c(0, round(ej0$cum_freq * 100)) ,"%",sep=""),
         las = 1, col.axis = "grey62", col = "grey62", cex.axis = 0.8, col.axis = "grey62")
    ## Introducir linea 80%
    i<-which(ej0$cum_freq<=0.85)
    abline(h=max(ej0$Total)*0.8, col="skyblue")
    abline(v=max(i),col="skyblue")
  }
  else if(is.null(provincia)&!is.null(distrito)){
    stop("Error-Se requiere provincia")
  }
  return(ej0)
}




