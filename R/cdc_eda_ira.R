#' Limpiar bases de EDA e IRA
#'
#' Funciones para limpiar bases de EDA e IRA.
#'
#' @describeIn clean_eda_sp limpiar base EDA
#'
#' @param data data frame con conteo por area administrativa
#' @param strata nombre de columna con el valor a estatificar el conte (establecimiento o ubigeo)
#'
#' @import dplyr
#' @import rlang
#' @import tidyr
#' @import janitor
#'
#' @return Bases limpiar de EDA e IRA.
#'
#' @examples
#'
#' #
#' # # importar casos ------------------------------------------------------------------
#' #
#' # iras_i <- read.dbf("data-raw/eda_ira/20190702-IRA_SP.dbf") %>% as_tibble()
#' # edas_i <- read.dbf("data-raw/eda_ira/20190702-EDA_SP.DBF") %>% as_tibble()
#' #
#' # edas_i %>% glimpse()
#' # iras_i %>% glimpse()
#' # edas %>% count(UBIGEO)
#' #
#' # file_date <- "20190702"
#' #
#' # # 1. limpiar bases ------------------------------------------------------------
#' #
#' # edas <- edas_i %>% clean_eda_sp(strata = ubigeo)
#' # edas %>% glimpse()
#' #
#' # iras <- iras_i %>% clean_ira_sp(strata = ubigeo)
#' # iras %>% glimpse()
#' #
#' # # tarea: homogenizar denominador -------------------------------------------------
#' #
#' # #1871
#' # denomi_pe
#' # #1855
#' # edas %>% count(ubigeo)
#' #
#' # # 2. calcular casos, acumulado, tasa por ubigeo ------------------------------------
#' #
#' # #base 1
#' # edas_aggr_semanal <- edas %>%
#' #   cdc_casos_tiempo(value = eda_c_sum,
#' #                    denominador = denomi_pe) %>%
#' #   write_rds(paste0("data/",file_date,"-edas_ubigeo_semana.rds"))
#' #
#' # iras %>%
#' #   cdc_casos_tiempo(value = ira_c_sum,
#' #                    denominador = denomi_pe) %>%
#' #   write_rds(paste0("data/",file_date,"-iras_ubigeo_semana.rds"))
#' #
#' # # base 2
#' # edas_aggr <- edas %>%
#' #   filter(semana>=15) %>%
#' #   cdc_casos_nacional(value = eda_c_sum,
#' #                      denominador = denomi_pe) %>%
#' #   write_rds(paste0("data/",file_date,"-edas_ubigeo_nacional_sem15.rds"))
#' #
#' # iras %>%
#' #   filter(semana>=15) %>%
#' #   cdc_casos_nacional(value = ira_c_sum,
#' #                      denominador = denomi_pe) %>%
#' #   write_rds(paste0("data/",file_date,"-iras_ubigeo_nacional_sem15.rds"))
#'
#'@export clean_eda_sp
#'@export clean_ira_sp
#'@export cdc_casos_tiempo
#'@export cdc_casos_nacional
#'
clean_eda_sp <- function(data,strata) {

  edas_i <- data
  c_var <- enquo(strata) #ubigeo o e_salud

  edas_i %>%
    clean_names() %>%
    #create sum of cases
    mutate(daa_c=daa_c1+daa_c1_4+daa_c5,
           dis_c=dis_c1+dis_c1_4+dis_c5,
           eda_c=daa_c+dis_c,
           eda_c1=daa_c1+dis_c1,
           eda_c1_4=daa_c1_4+dis_c1_4,
           eda_c5=daa_c5+dis_c5
    ) %>%
    select(ano:!!c_var,starts_with("daa_c"),starts_with("dis_c"),starts_with("eda_c")) %>%
    #get a whole sum per unit (ubigeo or health center)
    group_by(ano,semana,!!c_var) %>%
    #summarise(eda_c_sum=sum(eda_c)) %>%
    summarise_at(.vars = vars(daa_c,dis_c,eda_c1,eda_c1_4,eda_c5,eda_c),
                 .funs = sum) %>%
    ungroup() %>%
    rename(eda_c_sum=eda_c)
}
#' @describeIn clean_eda_sp limpiar base IRA
#' @inheritParams clean_eda_sp
clean_ira_sp <- function(data,strata) {

  iras_i <- data
  c_var <- enquo(strata) #ubigeo o e_salud

  #nota:
  #se pueden recuperar más estratos por edad en IRAS

  iras_i %>%
    clean_names() %>%
    #create sum of cases
    mutate(ira_c=ira_m2+ira_2_11+ira_1_4a) %>%
    select(ano:!!c_var,ira_m2:ira_1_4a,ira_c) %>%
    #get a whole sum per unit (ubigeo or health center)
    group_by(ano,semana,!!c_var) %>%
    #summarise(ira_c_sum=sum(ira_c)) %>%
    summarise_at(.vars = vars(ira_m2,ira_2_11,ira_1_4a,ira_c),
                 .funs = sum) %>%
    ungroup() %>%
    rename(ira_c_sum=ira_c)
}
#' @describeIn clean_eda_sp crear resumen por semana
#' @inheritParams clean_eda_sp
#' @param value nombre de columna del valor a contar (e.g. eda, dis, daa)
#' @param denominador base de denominador
#' @param rate_mult factor a escalar tasa
cdc_casos_tiempo <- function(data,value,denominador,rate_mult) {

  edas <- data
  eda_c_sum <- enquo(value)
  denomi_pe <- denominador
  rate_mult <- 1000

  dates_db <-
    tibble(semana=seq(1,52)) %>%
    mutate(date=as.Date(paste(2019,semana,1,sep = "-"),"%Y-%U-%u"),
           date=date-2) %>%
    separate(date,c("year","month","day"),sep = "-",remove = F) %>%
    mutate_at(.vars = vars(year:day),.funs = as.numeric) %>%
    unite(date_f,year,month,day,sep = "/",remove = T) %>%
    mutate(date_f=as.factor(date_f))

  edas_aggr_semanal <-
    #unir bases
    full_join(edas,denomi_pe) %>%
    #tasa
    mutate(rate=(!!eda_c_sum/total)*rate_mult) %>%
    #renombrar apropiadamente
    rename(n_distrito=nm_dist,
           n_provincia=nm_prov,
           n_depa=nm_depa,
           casos=!!eda_c_sum) %>%
    #transformat tipo de variable
    mutate(casos=as.double(casos),
           rate=as.double(rate),
           n_depa=str_to_title(n_depa)) %>%
    #generar suma acumulada de casos
    arrange(ubigeo,semana) %>%
    group_by(ano,ubigeo) %>%
    mutate(casos_cumsum=cumsum(casos)) %>%
    ungroup() %>%
    left_join(dates_db) %>%
    select(ano:casos,casos_cumsum,total,rate,date_f,date,everything())

  return(edas_aggr_semanal)
}
#' @describeIn clean_eda_sp crear resumen agregado por año
#' @inheritParams cdc_casos_tiempo
cdc_casos_nacional <- function(data,value,denominador,rate_mult) {

  edas <- data
  eda_c_sum <- enquo(value)
  denomi_pe <- denominador
  rate_mult <- 1000

  edas_aggr <- edas %>%

    #requerimiento especial
    #filter(semana>=15) %>%

    group_by(ano,ubigeo) %>%
    summarise(n=sum(!!eda_c_sum)) %>%
    ungroup() %>%
    #unir
    full_join(denomi_pe) %>%
    mutate(n=replace_na(n,0),
           ano=replace_na(ano,2019)) %>%
    #tasa
    mutate(rate=(n/total)*rate_mult,
           rate=if_else(is.na(rate),0,rate)) %>% #skimr::skim(rate)
    rename_all(~str_to_lower(.)) %>%
    rename(n_distrito=nm_dist,n_provincia=nm_prov,n_depa=nm_depa,casos=n
    ) %>%
    mutate(casos=as.double(casos),
           rate=as.double(rate),
           n_depa=str_to_title(n_depa)) %>%
    select(ano:casos,total,rate,everything())

  return(edas_aggr)
}
