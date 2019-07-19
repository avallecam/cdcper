#' Crear mapas coropleticos
#'
#' API para la generación de mapas coropleticos con información agregada por area administrativa (distrito).
#'
#' @describeIn cdc_choropleth
#'
#' @param data data frame con conteo por area administrativa
#' @param value nombre de columna con el valor a mapear
#' @param departamento departamento (adm03) a elegir dentro del pais (adm01)
#' @param min_n número mínimo de eventos para idnetificar polígono con su nombre
#' @param title_legend Título en la legenda
#' @param shape archivo sf con los datos del polígono al nivel del area administrativa
#' @param rute_name ruta y nombre de la imagen a generar
#'
#' @import dplyr
#' @import rlang
#' @import tmap
#' @import tidyr
#' @import sf
#' @import spdep
#'
#' @return Mapa e imagen con conteo o tasas por área administrativa.
#'
#' @examples
#'
#' # # librerias ---------------------------------------------------------------
#' #
#' # library(tidyverse)
#' # library(readxl)
#' # library(tmap)
#' # library(sf)
#' #
#' # #casos ---------------------------------------------------------------
#' # caso_per <- read_xls("data-raw/ubigeo_casos.xls") %>%
#' #   mutate(n_depa=str_to_title(n_depa))
#' #
#' # #shape ---------------------------------------------------------------
#' #
#' # per0 <- st_read("data-raw/gis/PER_adm0.shp")
#' # per1 <- st_read("data-raw/gis/PER_adm1.shp")
#' # per2 <- st_read("data-raw/gis/PER_adm2.shp")
#' # per4 <- st_read("data-raw/gis/Distritos.shp")
#' #
#' # #ejemplo ---------------------------------------------------------------
#' # cdc_choropleth(data = edas_aggr,
#' #               value = rate,
#' #               departamento = "Cajamarca",
#' #               min_n = 5,
#' #               shape = per4,
#' #               title_legend = "Tasa cruda\nde EDA\nx 1000 hab.",
#' #               rute_name = "figure/fig22-eda_fun-")
#' #
#' # cdc_choropleth(data = edas_aggr,
#' #               value = casos,
#' #               departamento = "Cajamarca",
#' #               min_n = 200,
#' #               shape = per4,
#' #               title_legend = "Casos\nde EDA",
#' #               rute_name = "figure/fig23-eda_fun-")
#' #
#' # cdc_choropleth_facet(data = edas_aggr_semanal,
#' #                      value = casos_cumsum,
#' #                      facet = semana,
#' #                      departamento = "Piura",
#' #                      min_n = 5,
#' #                      shape = per4,title_legend = "Casos\nacumulados\nde EDA",
#' #                      rute_name = "figure/fig27-eda_fun-")
#' #
#' # cdc_morantest(data = edas_aggr,value = rate,departamento = "Lambayeque")
#' # cdc_morantest(data = edas_aggr,value = casos,departamento = "Lambayeque")
#'
#'@export cdc_choropleth
#'@export cdc_morantest
#'@export cdc_choropleth_facet
#'
cdc_choropleth <- function(data,value,departamento,min_n,shape,title_legend,rute_name) {

  #shape file del nivel
  per4 <- shape

  #datos del nivel
  caso_per <- data

  #variable to map
  c_var <- enquo(value)
  c_var_name <- c_var %>% as_name() #%>% paste0(collapse = "_")

  #detallar elemento del nivel
  dep_int1 <- departamento
  dep_int2 <- departamento %>% str_to_upper()
  dep_int3 <- departamento %>%
    str_to_lower() %>%
    str_replace_all(" ","")

  #subset data de c_var
  caso_lim <- caso_per %>%
    filter(n_depa==dep_int1) %>%
    #ojo: retiro de huarochiri de lima provincia (san antonio de chaclla)
    #filter(N_provincia!="HUAROCHIRI") %>%
    select(ubigeo,!!c_var,n_distrito) %>%
    arrange(desc(!!c_var))

  #subset shape
  if (dep_int1=="Lima") {
    lima <- per4 %>%
      filter(NM_DEPA==dep_int2) %>%
      filter(NM_PROV==dep_int2) %>%
      mutate(NM_DIST_T=str_replace_all(NM_DIST_T,"[^[:graph:]]", " ")) %>%
      mutate(ubigeo=CD_DIST) %>%
      left_join(caso_lim) %>%
      mutate(!!c_var_name := replace_na(!!c_var, 0))
    #replace_na(!!c_var,0)
    #mutate(c_var=if_else(is.na(c_var),0,c_var))
  } else{
    lima <- per4 %>%
      filter(NM_DEPA==dep_int2) %>%
      mutate(NM_DIST_T=str_replace_all(NM_DIST_T,"[^[:graph:]]", " ")) %>%
      mutate(ubigeo=CD_DIST) %>%
      left_join(caso_lim) %>%
      mutate(!!c_var_name := replace_na(!!c_var, 0))
    #replace_na(!!c_var,0)
    #mutate(c_var=if_else(is.na(c_var),0,c_var))
  }

  #subset de top c_var
  caso_lim_top <- caso_lim %>%
    filter(!!c_var>=min_n)

  #subset de labels
  lima_sub <- lima %>%
    filter(ubigeo %in% caso_lim_top$ubigeo)

  #plot
  per1_c_var_map <- tm_shape(lima) +
    tm_polygons(col=c_var_name,style="cont",
                title = title_legend) +
    tm_legend(legend.position=c("left","bottom")) +
    tm_layout(title = dep_int1,title.position = c("right","top")) +
    tm_shape(lima_sub) +
    tm_text("NM_DIST_T",size = 0.5)

  #save plot
  tmap_save(per1_c_var_map,
            paste0(rute_name,
                   dep_int3,
                   ".png"),
            height = 10, width = 10)

  #return plot
  return(per1_c_var_map)
  #return(c_var_name)

}
#' @describeIn cdc_choropleth moran test
#' @inheritParams cdc_choropleth
cdc_morantest <- function(data,value,departamento) {

  edas_aggr <- data
  casos <- enquo(value)
  depax <- departamento
  #depax <- "Lambayeque"
  #subset data de casos

  london_ref <- edas_aggr %>%
    filter(n_depa==depax) %>%
    #ojo: retiro de huarochiri de lima provincia (san antonio de chaclla)
    #filter(N_provincia!="HUAROCHIRI") %>%
    select(ubigeo,casos,n_distrito,geometry) %>%
    arrange(desc(casos)) %>%
    #from tibble to pure sf
    st_as_sf() %>%
    #from sf to spatialpolygondataframe
    as_Spatial()
  borough_nb <- poly2nb(london_ref)
  moran_remain <- moran.mc(
    london_ref$casos,
    nb2listw(borough_nb),
    nsim = 999)
  #moran_remain
  #str(moran_remain)
  moran_remain$res %>%
    enframe(name = NULL) %>%
    ggplot(aes(value)) +
    geom_histogram() +
    geom_vline(xintercept = moran_remain$statistic,colour="red") +
    labs(title = "Monte Carlo null distribution",
         subtitle = paste("Ha: ",paste(round(moran_remain$statistic,2),collapse = ", ")))
  ggsave(paste0("figure/fig17-eda-fun-moran_",depax,".png"),width = 5,height = 5)
  #
  return(moran_remain)
}
#' @describeIn cdc_choropleth moran test
#' @inheritParams cdc_choropleth
#' @param facet varaible name to make the facet by
cdc_choropleth_facet <- function(data,value,facet,departamento,min_n,shape,title_legend,rute_name) {

  #shape file del nivel
  per4 <- shape

  #datos del nivel
  caso_per <- data

  #variable to map
  c_var <- enquo(value)
  c_var_name <- c_var %>% as_name() #%>% paste0(collapse = "_")

  facet_var <- enquo(facet)
  facet_var_name <- facet_var %>% as_name()

  #detallar elemento del nivel
  dep_int1 <- departamento
  dep_int2 <- departamento %>% str_to_upper()
  dep_int3 <- departamento %>%
    str_to_lower() %>%
    str_replace_all(" ","")

  #subset data de c_var
  caso_lim <- caso_per %>%
    filter(n_depa==dep_int1) %>%
    #ojo: retiro de huarochiri de lima provincia (san antonio de chaclla)
    #filter(N_provincia!="HUAROCHIRI") %>%
    select(ubigeo,!!facet_var,!!c_var,n_distrito) %>%
    arrange(desc(!!c_var))

  #subset shape
  if (dep_int1=="Lima") {
    lima <- per4 %>%
      filter(NM_DEPA==dep_int2) %>%
      filter(NM_PROV==dep_int2) %>%
      mutate(NM_DIST_T=str_replace_all(NM_DIST_T,"[^[:graph:]]", " ")) %>%
      mutate(ubigeo=CD_DIST) %>%
      left_join(caso_lim) %>%
      mutate(!!c_var_name := replace_na(!!c_var, 0))
    #mutate(c_var=if_else(is.na(c_var),0,c_var))
  } else{
    lima <- per4 %>%
      filter(NM_DEPA==dep_int2) %>%
      mutate(NM_DIST_T=str_replace_all(NM_DIST_T,"[^[:graph:]]", " ")) %>%
      mutate(ubigeo=CD_DIST) %>%
      left_join(caso_lim) %>%
      mutate(!!c_var_name := replace_na(!!c_var, 0))
    #mutate(c_var=if_else(is.na(c_var),0,c_var))
  }

  #subset de top c_var
  caso_lim_top <- caso_lim %>%
    filter(!!c_var>=min_n)

  #subset de labels
  lima_sub <- lima %>%
    filter(ubigeo %in% caso_lim_top$ubigeo)

  #plot
  per1_c_var_map <- tm_shape(lima) +
    tm_polygons(col=c_var_name,style="cont",
                title = title_legend) +
    tm_facets(by = facet_var_name, #nrow = 2,
              free.coords = FALSE
    ) +
    tm_legend(legend.position=c("left","bottom")) +
    tm_layout(title = dep_int1,title.position = c("right","top")) #+
  #tm_shape(lima_sub) +
  #tm_text("NM_DIST_T",size = 0.5)

  #save plot
  tmap_save(per1_c_var_map,
            paste0(rute_name,
                   dep_int3,
                   ".png"),
            height = 10, width = 20)

  #return plot
  #return(per1_c_var_map)

}

