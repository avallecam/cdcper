#' @title Tidy up REUNIS tables
#'
#' @description Tidy up REUNIS tables of population at district level. Ver: [REUNIS](https://www.minsa.gob.pe/reunis/data/poblacion_estimada.asp).
#'
#' @describeIn read_reunis_total
#'
#' @param file nombre de archivo
#' @param year anho de archivo
#'
#' @import dplyr
#' @import tidyr
#' @import janitor
#' @import readxl
#' @import purrr
#' @import tibble
#' @import stringr
#'
#' @return tidy tables a partir de poblacion a nivel nacional de la REUNIS
#'
#' @export read_reunis_total
#' @export read_reunis_edad
#' @export poblacion_distrital_clean
#' @export read_inei_poblacion
#'
#' @examples
#'
#' # rute_1 <- "data-raw/Poblacion Peru 2020 Dpto Prov Dist Final INEI-actualizado.xlsx"
#' # read_reunis_total(file = rute_1,year = 2020)
#' # read_reunis_edad(file = rute_1, year = 2020)
#' #
#' # rute_2 <- "data-raw/PERÚ - ESTIMACIONES Y PROYECCIONES DE POBLACIÓN ( BOLETINES ESPECIALES 17,18,19,20,21,22 y 36,37)/Libro18/cuadros/"
#' # read_inei_poblacion(folder = rute_2,file_name = "c1600") %>% count(anho)
#'

read_reunis_total <- function(file,year) {
  read_xlsx(file,sheet = 1,skip = 6) %>%
    janitor::clean_names() %>%
    filter(!is.na(total)) %>%
    select(ubigeo:total) %>%
    mutate(ano=year) %>%
    select(ano,everything()) %>%
    mutate(ubigeo=case_when(
      str_length(ubigeo)==7~str_replace(ubigeo,"(......).+","\\1"),
      TRUE ~ ubigeo
    ))
}

#' @describeIn read_reunis_total priorización con dos covariables
#' @inheritParams read_reunis_total

read_reunis_edad <- function(file,year) {
  read_xlsx(file,sheet = 5,skip = 6) %>%
    janitor::clean_names() %>%
    filter(!is.na(total)) %>%
    mutate(
      "h__0_11a" = pmap(select(.,x0_7:x11_18),sum),
      "h__12_17a" = pmap(select(.,x12_19:x17_24),sum),
      "h__18_29a" = pmap(select(.,x18_25:x25_29_28),sum),
      "h__30_59a" = pmap(select(.,x30_34_29:x55_59_34),sum),
      "h__60a_mas" = pmap(select(.,x60_64_35:x80_y_39),sum),
      "m__0_11a" = pmap(select(.,x0_41:x11_52),sum),
      "m__12_17a" = pmap(select(.,x12_53:x17_58),sum),
      "m__18_29a" = pmap(select(.,x18_59:x25_29_62),sum),
      "m__30_59a" = pmap(select(.,x30_34_63:x55_59_68),sum),
      "m__60a_mas" = pmap(select(.,x60_64_69:x80_y_73),sum)
    ) %>%
    unnest() %>%
    select(ubigeo:distrito,#total,h_tot=x6,m_tot=x40,
           h__0_11a:m__60a_mas) %>%

    # #test: successfully!
    # mutate(total_new=pmap(select(.,h_0_11a:m_60a_mas),sum)) %>%
    # unnest() %>%
    # select(total,total_new) %>%
    # filter(total != total_new )

    gather(key,value,h__0_11a:m__60a_mas) %>%
    #filter(ubigeo!="000000") %>%
    separate(key,c("sex","age"),sep = "__") %>%
    arrange(ubigeo,age,sex) %>%
    mutate(ano=year) %>%
    select(ano,everything()) %>%
    mutate(ubigeo=case_when(
      str_length(ubigeo)==7~str_replace(ubigeo,"(......).+","\\1"),
      TRUE ~ ubigeo
    ))
}

#' @describeIn read_reunis_total priorización con dos covariables
#' @inheritParams read_reunis_total
#' @param path paste path to folder of files

poblacion_distrital_clean <- function(path) {
  read_excel(path,skip = 2) %>%
    janitor::clean_names() %>%
    select(ubigeo,departamento_provincia_y_distrito,starts_with("x2")) %>%
    slice(-1) %>%
    filter(!is.na(ubigeo)) %>%
    pivot_longer(cols = c(-ubigeo,-departamento_provincia_y_distrito),
                 names_to = "anho",values_to = "poblacion_total") %>%
    mutate(anho=str_replace(anho,"x","") %>% as.numeric(),
           poblacion_total=as.numeric(poblacion_total))
}

#' @describeIn read_reunis_total priorización con dos covariables
#' @inheritParams read_reunis_total
#' @param file_name name of file that corresponds to one department

read_inei_poblacion <- function(path,file_name) {

  #bulk data cleaning
  list.files(path = path) %>%
    enframe(name = NULL) %>%
    filter(str_detect(value,file_name)) %>%
    mutate(path=paste0(path,value)) %>%
    mutate(file=map(.x = path,.f = poblacion_distrital_clean)) %>%
    unnest(cols = c(file))

}

