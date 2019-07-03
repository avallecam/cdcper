#' Crear mapas con patrones puntuales
#'
#' Generar de mapas con patrones puntuales.
#'
#' @describeIn cdc_point_heatmap
#'
#' @param data_shape pending
#' @param data_point pending
#' @param rute_name pending
#' @param subset_labels pending
#' @param subset_list pending
#'
#' @return Mapa e imagen con casos distribuidos espacialmente.
#'
#' @examples
#' # libreria ----------------------------------------------------------------
#'
#' library(tidyverse)
#' library(magrittr)
#' library(sf)
#' library(tmap)
#'
#' theme_set(theme_bw())
#'
#' # importar shp ------------------------------------------------------------
#'
#' #per0 <- st_read("data-raw/gis/PER_adm0.shp")
#' #per1 <- st_read("data-raw/gis/PER_adm1.shp")
#' #per2 <- st_read("data-raw/gis/PER_adm2.shp")
#' #per3 <- st_read("data-raw/gis/dip16.shp")
#' per4 <- st_read("data-raw/gis/Distritos.shp")
#' #espe <- st_read("data-raw/gis/ES_Peru.shp")
#'
#' # cases -------------------------------------------------------------------
#'
#' piur <- st_read("data-raw/gis/Piura.shp")
#' juni <- st_read("data-raw/gis/Junin.shp") %>% filter(prov_res=="HUANCAYO")
#' lima <- st_read("data-raw/gis/Lima.shp") %>% filter(prov_res=="LIMA")
#'
#' # edit polygons -------------------------------------------------------------------
#'
#' per4
#' per4 %>% glimpse()
#' per4 %>% as_tibble() %>% count(NM_DEPA) %>% print(n=Inf)
#'
#' piu4 <- per4 %>% filter(NM_DEPA=="PIURA")
#' jun4 <- per4 %>% filter(NM_DEPA=="JUNIN") %>% filter(NM_PROV=="HUANCAYO")
#' lim4 <- per4 %>% filter(NM_DEPA=="LIMA",NM_PROV=="LIMA")
#'
#' # subset ------------------------------------------------------------------
#'
#' selected_dist <- c("CASTILLA","VEINTISEIS DE OCTUBRE","PIURA",
#'                    "SULLANA","BELLAVISTA","PARIÑAS","CHULUCANAS","CATACAOS")
#'
#' piur_subset_list <- piu4 %>%
#'   select(NM_DIST,CD_DIST) %>%
#'   filter(NM_DIST %in% selected_dist | CD_DIST==200701) %>%
#'   pull(CD_DIST)
#'
#' # eval number of cases ----------------------------------------------------
#'
#' piur %>% dim()
#' lima %>% dim()
#' juni %>% dim()
#'
#' # apply -------------------------------------------------------------------
#'
#' cdc_point_heatmap(data_shape = lim4,
#'                   data_point = lima,
#'                   rute_name = "figure/fig10-fun-sgb-lima")
#'
#' cdc_point_heatmap(data_shape = piu4,
#'                   data_point = piur,
#'                   rute_name = "figure/fig10-fun-sgb-piura",
#'                   subset_labels = T,
#'                   subset_list = piur_subset_list)
#'
#' cdc_point_heatmap(data_shape = jun4,
#'                   data_point = juni,
#'                   rute_name = "figure/fig10-fun-sgb-junin") +
#'   ggtitle("JUNIN - Provincia: Huancayo")
#' ggsave(paste0("figure/fig10-fun-sgb-junin",".png"),height = 10,width = 10)
#'
#'
#' # con una marca -----------------------------------------------------------
#'
#' cdc_point_heat_mark(data_point = piur,
#'                     data_shape = piu4,
#'                     mp_xlim = c(-81.4,-80.2),
#'                     mp_ylim = c(-5.45,-4.4),
#'                     mp_dist = 10,
#'                     mp_loca = "bottomright")
#'
#'@export cdc_point_heatmap
#'@export cdc_point_heat_mark
#'
cdc_point_heatmap <- function(data_shape,data_point,rute_name,
                              subset_labels=F,subset_list) {

  heat_colors <- scale_fill_gradientn("Case\ndensity",
                                      colors = c("white", "gold",
                                                 "red"#, "darkred"
                                                 ),
                                      guide = FALSE
                                      )

  caso <- data_point #caso <- piur
  piu4 <- data_shape

  name_tit <- piu4 %>% as_tibble() %>%
    count(NM_DEPA) %>% pull(NM_DEPA) %>% as.character()

  subset_list_raw <- piu4 %>%
    select(NM_DIST,CD_DIST) %>% #count(NM_DIST) %>% print(n=Inf)
    filter(NM_DIST %in% (caso %>%
                           as_tibble() %>%
                           count(ubigeo_res,sort = T) %>%
                           filter(n>2) %>%
                           pull(ubigeo_res)) ) %>%
    pull(CD_DIST)

  caso_pnt <- caso %>%
    as_tibble() %>%
    select(cod_unico,geometry) %>%
    tidyr::extract(geometry, c('lon', 'lat'), '\\((.*),\\s(.*)\\)', convert = TRUE)

  if (subset_labels==F) {
    point_heat <- ggplot(piu4) +
      stat_density_2d(data = caso_pnt ,
                      aes(x = lon,y = lat,fill = ..level..),
                      geom = "polygon") +
      heat_colors +
      geom_sf(alpha=0) +
      geom_sf(data = caso,alpha=0.5,size=0.5) +
      geom_sf_text(data = piu4 %>%
                     mutate(NM_DIST=str_replace_all(NM_DIST,"[^[:graph:]]", " ")) %>%
                     filter(CD_DIST %in% subset_list_raw)
                   ,aes(label=NM_DIST),size=2) +
      xlab("Longitud") + ylab("Latitud") +
      ggtitle(name_tit,subtitle = "Densidad de casos")
  } else{
    point_heat <- ggplot(piu4) +
      stat_density_2d(data = caso_pnt ,
                      aes(x = lon,y = lat,fill = ..level..),
                      geom = "polygon") +
      heat_colors +
      geom_sf(alpha=0) +
      geom_sf(data = caso,alpha=0.5,size=0.5) +
      geom_sf_text(data = piu4 %>%
                     mutate(NM_DIST=str_replace_all(NM_DIST,"[^[:graph:]]", " ")) %>%
                     filter(CD_DIST %in% subset_list)
                   ,aes(label=NM_DIST),size=2) +
      xlab("Longitud") + ylab("Latitud") +
      ggtitle(name_tit,subtitle = "Densidad de casos")
  }

  ggsave(paste0(rute_name,".png"),height = 10,width = 10)

  return(point_heat)

}
#' @describeIn cdc_point_heatmap marked heat map
#' @inheritParams cdc_point_heatmap
#' @param mp_xlim pending
#' @param mp_ylim pending
#' @param mp_dist pending
#' @param mp_loca pending
cdc_point_heat_mark <- function(data_shape,data_point,
                                mp_xlim,mp_ylim,mp_dist,mp_loca) {
  caso <- data_point #piur
  piu4 <- data_shape #piu4
  mp_xlim <- mp_xlim #c(-81.4,-80.2)
  mp_ylim <- mp_ylim #c(-5.45,-4.4)
  mp_dist <- mp_dist #10
  mp_loca <- mp_loca #"bottomright"

  caso_pnt <- caso %>%
    as_tibble() %>%
    select(cod_unico,geometry,mark) %>%
    tidyr::extract(geometry, c('lon', 'lat'), '\\((.*),\\s(.*)\\)', convert = TRUE)

  selected_dist_top <- caso %>%
    as_tibble() %>%
    count(ubigeo_res,sort = T) %>%
    filter(n>2) %>%
    pull(ubigeo_res)

  subset_list_raw <- piu4 %>%
    select(NM_DIST,CD_DIST) %>% #count(NM_DIST) %>% print(n=Inf)
    filter(NM_DIST %in% (caso %>%
                           as_tibble() %>%
                           count(ubigeo_res,sort = T) %>%
                           filter(n>2) %>%
                           pull(ubigeo_res)) ) %>%
    pull(CD_DIST)

  name_tit <- piu4 %>% as_tibble() %>%
    count(NM_DEPA) %>% pull(NM_DEPA) %>% as.character()

  dep_int3 <- name_tit %>%
    str_to_lower() %>%
    str_replace_all(" ","")

  mark_plot <- caso_pnt %>%
    ggplot() +

    stat_density_2d(data = caso_pnt ,
                    aes(x = lon,y = lat,fill = ..level..),alpha=0.3, #fill="white",
                    geom = "polygon") +
    scale_fill_gradientn(#"Case\ndensity",
      colors = c("white", "gold", "red"), guide = FALSE) +

    geom_sf(data = piu4,alpha=0) +

    geom_point(aes(x = lon,y = lat,color=mark,shape=mark),lwd=3,fill="white") +
    scale_color_manual("Caso",values = c("red","black"),guide=F) +
    scale_shape_manual("Caso",values = c(20,4)) +

    #geom_sf(data = caso_pnt,aes(colour=mark,size=mark,shape=mark),size=2) +
    xlim(mp_xlim) + ylim(mp_ylim) +

    geom_sf_text(data = piu4 %>%
                   mutate(NM_DIST=str_replace_all(NM_DIST,"[^[:graph:]]", " ")) %>%
                   filter(CD_DIST %in% subset_list_raw)
                 ,aes(label=NM_DIST),size=2) +

    ggsn::scalebar(data = caso %>% filter(prov_res %in% selected_dist_top),
                   dist = mp_dist,
                   height = 0.025,st.dist = 0.02,
                   st.size = 3,dist_unit = "km",
                   transform = TRUE,
                   location = mp_loca,
                   model = "WGS84") +
    xlab("Longitud") + ylab("Latitud") +
    ggtitle(name_tit,subtitle = "Densidad de casos")

  ggsave(paste0("figure/fig11-sbg-",dep_int3,".png"),width = 10,height = 10)

  return(mark_plot)

}


