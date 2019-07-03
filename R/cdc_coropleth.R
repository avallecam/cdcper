#' Crear mapas coropleticos
#'
#' API para la generación de mapas coropleticos con información agregada por area administrativa (distrito).
#'
#' @param data data frame con conteo por area administrativa
#' @param departamento departamento (adm03) a elegir dentro del pais (adm01)
#' @param min_n número mínimo de eventos para idnetificar polígono con su nombre
#' @param title_legend Título en la legenda
#' @param shape archivo sf con los datos del polígono al nivel del area administrativa
#' @param rute_name ruta y nombre de la imagen a generar
#'
#' @return Mapa e imagen con conteo o tasas por área administrativa.
#'
#' @examples
#'
#' # librerias ---------------------------------------------------------------
#'
#' library(tidyverse)
#' library(readxl)
#' library(tmap)
#' library(sf)
#'
#' #casos ---------------------------------------------------------------
#' caso_per <- read_xls("data-raw/ubigeo_casos.xls") %>%
#'   mutate(n_depa=str_to_title(n_depa))
#'
#' #shape ---------------------------------------------------------------
#'
#' per0 <- st_read("data-raw/gis/PER_adm0.shp")
#' per1 <- st_read("data-raw/gis/PER_adm1.shp")
#' per2 <- st_read("data-raw/gis/PER_adm2.shp")
#' per4 <- st_read("data-raw/gis/Distritos.shp")
#'
#' #ejemplo ---------------------------------------------------------------
#' cdc_coropleth(data = caso_per,
#'               departamento = "Junin",
#'               min_n = 2,
#'               title_legend = "Casos\nde SGB",
#'               shape = per4,
#'               rute_name = "figure/fig04-sbg_fun-")
#'
#'
#' # loop --------------------------------------------------------------------
#'
#' d_list <- c("Lima","Junin","La Libertad","Cajamarca","Lambayeque","Piura")
#'
#' for (i in 1:length(d_list)) {
#'   cdc_coropleth(data = caso_per,
#'                 departamento = d_list[i],
#'                 min_n = 2,
#'                 title_legend = "Casos\nde SGB",
#'                 shape = per4,
#'                 rute_name = "figure/fig04-sbg_fun-")
#' }
#'
#'@export




#funcion ---------------------------------------------------------------
cdc_coropleth <- function(data,departamento,min_n,title_legend,shape,rute_name) {

  #shape file del nivel
  per4 <- shape

  #datos del nivel
  caso_per <- data

  #detallar elemento del nivel
  dep_int1 <- departamento
  dep_int2 <- departamento %>% str_to_upper()
  dep_int3 <- departamento %>%
    str_to_lower() %>%
    str_replace_all(" ","")

  #subset data de casos
  caso_lim <- caso_per %>%
    filter(n_depa==dep_int1) %>%
    #ojo: retiro de huarochiri de lima provincia (san antonio de chaclla)
    #filter(N_provincia!="HUAROCHIRI") %>%
    select(ubigeo,casos,N_distrito) %>%
    arrange(desc(casos))

  #subset shape
  if (dep_int1=="Lima") {
    lima <- per4 %>%
      filter(NM_DEPA==dep_int2) %>%
      filter(NM_PROV==dep_int2) %>%
      mutate(NM_DIST_T=str_replace_all(NM_DIST_T,"[^[:graph:]]", " ")) %>%
      mutate(ubigeo=CD_DIST) %>%
      left_join(caso_lim) %>%
      mutate(casos=if_else(is.na(casos),0,casos))
  } else{
    lima <- per4 %>%
      filter(NM_DEPA==dep_int2) %>%
      mutate(NM_DIST_T=str_replace_all(NM_DIST_T,"[^[:graph:]]", " ")) %>%
      mutate(ubigeo=CD_DIST) %>%
      left_join(caso_lim) %>%
      mutate(casos=if_else(is.na(casos),0,casos))
  }

  #subset de top casos
  caso_lim_top <- caso_lim %>%
    filter(casos>=min_n)

  #subset de labels
  lima_sub <- lima %>%
    filter(ubigeo %in% caso_lim_top$ubigeo)

  #plot
  per1_casos_map <- tm_shape(lima) +
    tm_polygons(col="casos",style="cont",
                title = title_legend) +
    tm_legend(legend.position=c("left","bottom")) +
    tm_layout(title = dep_int1,title.position = c("right","top")) +
    tm_shape(lima_sub) +
    tm_text("NM_DIST_T",size = 0.5)

  #save plot
  tmap_save(per1_casos_map,
            paste0(rute_name,
                   dep_int3,
                   ".png"),
            height = 10, width = 10)

  #return plot
  return(per1_casos_map)

}

