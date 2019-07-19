#' Crear mapas coropleticos con resultados de SatScan
#'
#' Limpiar salida y generar de mapas coropleticos con resultados estadísticos de R SatScan (rsatscan).
#'
#' @describeIn cdc_rsatscan_clean funcion para limpiar salida de satscan y generar mapas
#'
#' @param rsatscan_in data de ingreso a rsatscan
#' @param rsatscan_out salida de rsatscan
#'
#' @import dplyr
#' @import rlang
#' @import tmap
#'
#' @return Limpiar salida estadística de rsatscan.
#'
#' @examples
#'
#' #
#' # library(tidyverse)
#' # library(janitor)
#' # library(rsatscan)
#' # library(sf)
#' # library(tmap)
#' # library(lubridate)
#' # library(rlang)
#' # library(cdcper)
#' # library(xlsx)
#' # theme_set(theme_bw()) #fondo blanco y negro del ggplot
#' #
#' # td = tempdir()
#' # mysatscanloc <- "C:/Program Files (x86)/SaTScan"
#' #
#' # # importar  ---------------------------------------------------------------
#' #
#' # edas_aggr <- read_rds("data/20190702-edas_ubigeo_nacional_sem15.rds")
#' # edas_aggr_semanal <- read_rds("data/20190702-edas_ubigeo_semana.rds")
#' #
#' # edas_aggr %>% glimpse()
#' # edas_aggr_semanal %>% glimpse()
#' #
#' # # 02 - weekly data --------------------------------------------------------
#' #
#' # d_list <- c("LIMA",
#' #             "Junin","La Libertad","Cajamarca","Lambayeque","Piura")
#' #
#' # name_dptx <- d_list[3]
#' #
#' # edas_aggr_dept <- edas_aggr %>%
#' #   filter(n_depa==name_dptx)
#' #   #filter(n_provincia==name_dptx)
#' #
#' # edas_aggr_semanal_dept <- edas_aggr_semanal %>%
#' #   filter(n_depa==name_dptx) %>%
#' #   #filter(n_provincia==name_dptx) %>%
#' #   filter(date>="2019-04-07") #fijar fechas desde aqui!
#' #
#' # NMcas_ <- edas_aggr_semanal_dept %>% select(ubigeo,casos,date_f) %>% as.data.frame()
#' # NMpop_ <- edas_aggr_semanal_dept %>% select(ubigeo,ano,total) %>% distinct() %>% as.data.frame()
#' # NMgeo_ <- edas_aggr_semanal_dept %>% select(ubigeo,lat_ctd,lon_ctd) %>% as.data.frame()
#' # head(NMcas_)
#' # head(NMpop_)
#' # head(NMgeo_)
#' #
#' # # analisis 02 espacial puro - datos semana ----------
#' # # analisis espacial puro
#' # # modelo poisson discreto
#' # # con datos por semana - mejor estimacion
#' #
#' # write.cas(NMcas_, td,"NM")
#' # write.geo(NMgeo_, td,"NM")
#' # write.pop(NMpop_, td,"NM")
#' #
#' # invisible(ss.options(reset=TRUE))
#' # ss.options(list(CaseFile="NM.cas",
#' #                 PopulationFile="NM.pop",
#' #                 CoordinatesFile="NM.geo",
#' #                 StartDate="2019/4/13",EndDate="2019/6/29", #days
#' #                 CoordinatesType=1,
#' #                 AnalysisType=1,
#' #                 ModelType=0,
#' #                 TimeAggregationUnits=3,
#' #                 PrecisionCaseTimes=3,
#' #                 NonCompactnessPenalty=0,
#' #                 ReportGiniClusters="n",
#' #                 LogRunToHistoryFile="n"
#' # ))
#' #
#' # write.ss.prm(td,"testnm")
#' # testnm = satscan(td,"testnm", sslocation=mysatscanloc)
#' # summary(testnm)
#' # summary.default(testnm)
#' #
#' # # output limpio!
#' # week_pure_spatial <- cdc_rsatscan_clean(rsatscan_in = edas_aggr_dept, #ojo! in dato agregado
#' #                                         rsatscan_out = testnm)
#' # week_pure_spatial
#'
#'@export cdc_rsatscan_clean
#'
cdc_rsatscan_clean <- function(rsatscan_in,rsatscan_out) {

  edas_aggr_dept <- rsatscan_in
  testnm <- rsatscan_out

  plot1_gg <- testnm$llr %>%
    as_tibble() %>%
    ggplot(aes(LLR)) +
    geom_histogram() +
    geom_vline(xintercept = testnm$col[,c("LLR")],colour="red") +
    xlim(0,25) +
    labs(title = "Monte Carlo null distribution",
         subtitle = paste("Ha: ",paste(round(testnm$col[,c("LLR")],2),collapse = ", ")))
  #xlim(min(testnm$llr$LLR),max(testnm$llr$LLR))
  #coord_cartesian(xlim = c(min(testnm$llr$LLR),max(testnm$llr$LLR)))

  testnm_cluster <- testnm$col %>%
    as_tibble() %>%
    rename_all(list(~make.names(.))) %>%
    rename_all(list(~str_to_lower(.))) %>%
    mutate(prc_cases=100*(observed/population)) %>%
    dplyr::select(cluster,loc_id,longitude,latitude,
                  radius,
                  observed:rel_risk,number_loc,prc_cases,
                  population,
                  llr,p_value) %>%
    #mutate(radius=radius/110) %>%
    st_as_sf(coords = c("longitude", "latitude"), remove = F,
             crs = 4326, agr = "constant") %>%
    st_buffer(dist = .$radius)


  testnm_df_cluster <- testnm$gis %>%
    as_tibble() %>%
    rename_all(list(~make.names(.))) %>%
    rename_all(list(~str_to_lower(.))) %>%
    dplyr::select(loc_id,cluster,starts_with("loc_"))

  testnm_df <-
    #edas_aggr_dept %>%
    #select()
    NMgeo_ %>%
    as_tibble() %>%
    left_join(NMcas_) %>%
    left_join(NMpop_) %>%
    #mutate(num_test=numcases!=numcontrols) %>% filter(num_test==F) #count(num_test)
    #rename_all(list(~make.names(.))) %>%
    rename(loc_id=ubigeo) %>%
    #mutate(loc_id=as.factor(loc_id),
    #       numcases=as.factor(nume)) %>%
    left_join(testnm_df_cluster) %>%
    left_join(testnm_cluster)

  edas_aggr_dept_cluster <- edas_aggr_dept %>%
    rename(loc_id=ubigeo) %>%
    left_join(testnm_df_cluster) %>%
    mutate(cluster=replace_na(cluster,"not"),
           cluster=as.factor(cluster),
           cluster_ctr=if_else(loc_id %in% testnm_cluster$loc_id,"center","not")
    ) %>%
    st_as_sf()

  shapeclust <- st_as_sf(testnm$shapeclust) %>%
    st_transform(4326) %>%
    mutate(CLUSTER=as.factor(CLUSTER))

  #testnm$sci %>% as_tibble() %>% select(1,4:8)
  testnm_cluster
  #edas_aggr_dept_cluster %>% tm_shape() + tm_polygons(col = "cluster_ctr")

  plot2_gg <- ggplot() +
    geom_sf(data = edas_aggr_dept_cluster,
            alpha=0,colour="gray") +
    geom_sf(data=shapeclust,
            aes(LONGITUDE,LATITUDE,
                fill=as.factor(CLUSTER)),
            alpha=0.5) +
    scale_fill_discrete("Cluster ID") +
    coord_sf()

  plot3_tm <- edas_aggr_dept_cluster %>%
    tm_shape() +
    tm_polygons(col = "cluster") +
    tm_shape(shapeclust) +
    tm_polygons(#col = "CLUSTER",
      alpha = 0.2) +
    tm_legend(legend.position=c("right","top")) +
    tm_scale_bar(position = c("left","bottom"))

  plot4_tm <- edas_aggr_dept_cluster %>%
    tm_shape() +
    tm_polygons(col = "rate",style="cont") +
    tm_shape(shapeclust) +
    tm_polygons(#col = "CLUSTER",
      alpha = 0.2) +
    tm_legend(legend.position=c("right","top")) +
    tm_scale_bar(position = c("left","bottom"))

  results_list <- list(testnm_cluster,plot1_gg,plot2_gg,plot3_tm,plot4_tm,edas_aggr_dept_cluster)

  return(results_list)

}
