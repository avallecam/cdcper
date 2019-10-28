#' Crear mapas con patrones puntuales
#'
#' Generar de mapas con patrones puntuales.
#'
#' @describeIn cdc_heatmap
#'
#' @param data_shape pending
#' @param data_point pending
#' @param rute_name pending
#' @param subset_list pending
#' @param mp_xlim pending
#' @param mp_ylim pending
#' @param mp_dist pending
#' @param mp_loca pending
#'
#' @import dplyr
#' @import rlang
#' @import sf
#' @import ggsflabel
#'
#' @return Mapa e imagen con casos distribuidos espacialmente.
#'
#' @examples
#'
#' # per4 <- st_read("data-raw/gis/Distritos.shp")
#' # juni <- st_read("data-raw/gis/Junin.shp") %>% filter(prov_res=="HUANCAYO")
#' #
#' # # edit polygons -------------------------------------------------------------------
#' #
#' # jun4 <- per4 %>% filter(NM_DEPA=="JUNIN") %>% filter(NM_PROV=="HUANCAYO")
#' #
#' # cdc_point_heatmap(data_shape = jun4,
#' #                   data_point = juni,
#' #                   rute_name = "figure/fig10-fun-sgb-junin") +
#' #   ggtitle("JUNIN - Provincia: Huancayo")
#' # ggsave(paste0("figure/fig10-fun-sgb-junin",".png"),height = 10,width = 10)
#' #
#' #
#' # # con una marca -----------------------------------------------------------
#' #
#' # cdc_point_heat_mark(data_point = piur,
#' #                     data_shape = piu4,
#' #                     mp_xlim = c(-81.4,-80.2),
#' #                     mp_ylim = c(-5.45,-4.4),
#' #                     mp_dist = 10,
#' #                     mp_loca = "bottomright")
#'
#'@export cdc_heatmap
#'@export cdc_point_heatmap
#'@export cdc_point_heat_mark
#'
cdc_heatmap <- function(data_shape,data_point,
                        rute_name=NA,subset_list,
                        mp_xlim,mp_ylim,mp_dist,mp_loca) {

  caso <- data_point #caso <- piur
  piu4 <- data_shape
  mp_xlim <- mp_xlim #c(-81.4,-80.2)
  mp_ylim <- mp_ylim #c(-5.45,-4.4)
  mp_dist <- mp_dist #10
  mp_loca <- mp_loca #"bottomright"

  house <- caso %>% select(geometry)
  house_poly <- piu4 %>% st_buffer(dist = 0) %>% st_union() #needs to be cleaner!
  p.sp  <- as(house, "Spatial")  # Create Spatial* object
  p.ppp <- as(p.sp, "ppp")      # Create ppp object
  Window(p.ppp) <- as.owin(as(house_poly, "Spatial"))
  h_ppp <- bw.scott(p.ppp) #bw.ppl(p.ppp)

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
    select(cod_unico,mark,geometry) %>%
    tidyr::extract(geometry, c('lon', 'lat'), '\\((.*),\\s(.*)\\)', convert = TRUE)

  selected_dist_top <- caso %>%
    as_tibble() %>%
    count(ubigeo_res,sort = T) %>%
    filter(n>2) %>%
    pull(ubigeo_res)

  heat_map <- caso_pnt %>%
    ggplot() +
    stat_density_2d(data = caso_pnt ,
                    aes(x = lon,y = lat,fill = ..level..),
                    alpha=0.3,
                    geom = "polygon",h = h_ppp) +
    scale_fill_gradient2("Case\ndensity",
                         low = "yellow",mid = "gold",high = "red",
                         guide = FALSE
    )  +
    geom_sf(data = piu4,alpha=0,colour="gray") +
    # geom_sf(data = caso,alpha=0.5#,size=0.5
    # ) +

    geom_sf_text_repel(data = piu4 %>%
                         mutate(NM_DIST=str_replace_all(NM_DIST,"[^[:graph:]]", " ")) %>%
                         filter(CD_DIST %in% subset_list_raw)
                       ,aes(label=NM_DIST),size=2) +

    xlim(mp_xlim) + ylim(mp_ylim) +

    ggsn::scalebar(data = caso %>% filter(prov_res %in% selected_dist_top),
                   dist = mp_dist,
                   height = 0.025,st.dist = 0.02,
                   st.size = 3,dist_unit = "km",
                   transform = TRUE,
                   location = mp_loca,
                   model = "WGS84") +

    xlab("Longitud") + ylab("Latitud") +
    ggtitle(name_tit#,subtitle = "Densidad de casos"
    )

  if (is.na(rute_name)) {

    return(heat_map)

  } else {

    ggsave(paste0(rute_name,".png"),height = 10,width = 10)
    return(heat_map)

  }
}
#' @describeIn cdc_heatmap marked heat map
#' @inheritParams cdc_heatmap
cdc_point_heatmap <- function(data_shape,data_point,rute_name=NA,
                              subset_list,
                              mp_xlim,mp_ylim,mp_dist,mp_loca) {

  caso <- data_point #caso <- piur

  point_heat <- cdc_heatmap(data_shape,data_point,rute_name=NA,
                            subset_list,
                            mp_xlim,mp_ylim,mp_dist,mp_loca) +
    geom_sf(data = caso,alpha=0.5,shape=20,alpha=0.25,
            size=0.5,color="black") +
    scale_color_manual("Caso",guide=F)

  if (is.na(rute_name)) {

    return(point_heat)

  } else {

    ggsave(paste0(rute_name,".png"),height = 10,width = 10)

    return(point_heat)

  }

}
#' @describeIn cdc_heatmap marked heat map
#' @inheritParams cdc_heatmap
cdc_point_heat_mark <- function(data_shape,data_point,
                                mp_xlim,mp_ylim,mp_dist,mp_loca) {

  caso <- data_point #caso <- piur

  mark_plot <- cdc_heatmap(data_shape,data_point,rute_name=NA,
                           subset_list,
                           mp_xlim,mp_ylim,mp_dist,mp_loca) +
    # geom_sf(color=mark,
    #         shape=mark,
    #         size=mark,
    #         alpha=mark) +

    geom_point(aes(x = lon,y = lat,
                   color=mark,
                   alpha=mark,
                   shape=mark
    ),
    size=3,
    lwd=3,fill="white") +
    scale_color_manual("Caso",values = c("red","black"),guide=F) +
    #scale_size_manual("Caso",values = c(10,0.01)) #+ #not working!
    scale_shape_manual("Caso",values = c(20,4)) +
    scale_alpha_manual("Caso",values = c(1,0.25))

  #ggsave(paste0("figure/fig13-sbg-",dep_int3,".png"),width = 7,height = 7)

  return(mark_plot)

}


