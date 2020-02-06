#' @title Crear bases y tablas de conteo y tasas estandarizadas de mortalidad
#'
#' @description API para la generación de bases resumen para la creación de gráficos de tendencia y tablas reporte con información nominal (por sujeto).
#'
#' @describeIn cdc_mortality
#'
#' @param data data frame con información nominal (por sujeto)
#' @param denominador data frame con cantidad estimada de personas por rango de edad, sexo, departamento y año
#' @param ... lista de una o más variables
#' @param estandar data frame con proporciones otorgadas por la OMS para estandarizar tasas (fuente: https://seer.cancer.gov/stdpopulations/world.who.html)
#' @param rute_name character string con la ruta y nombre del objeto a exportar. Las bases se exportan en RDS y DTA. Las tablas en CSV y XLSX.
#'
#' @import dplyr
#' @import rlang
#' @import tidyr
#'
#' @return Base con conteo, población expuesta, tasas crudas y estandarizadas.
#'
#' @export cdc_mortality
#' @export cdc_denominator
#' @export cdc_mortality_2
#' @export cdc_mortality_to_wide
#' @export cdc_mortality_adj
#' @export cdc_mortality_raw
#'
#' @examples
#'
#'
#' # dependencies ------------------------------------------------------------
#'
#' #flexibility on dot-dot-dot
#' #https://stackoverflow.com/questions/7028385/can-i-remove-an-element-in-dot-dot-dot-and-pass-it-on
#' #https://www.burns-stat.com/the-three-dots-construct-in-r/
#'
#' library(cdcper)
#' library(tidyverse)
#' library(magrittr)
#' library(readxl)
#' library(haven)
#' library(naniar)
#' library(ggrepel)
#' library(labelled)
#' library(gridExtra)
#'
#' # importar base ----------------------------------------------------------------
#'
#' data(mortdb)
#' mortdb
#'
#' # importar denominadores -------------------------------------------------------
#'
#' data(denominadores_departamento_2000_2020_peru)
#' denomi_pe <- denominadores_departamento_2000_2020_peru
#'
#' data(edad_estandarizada_who)
#' fctwho_std <- edad_estandarizada_who
#'
#' # CDR + DAR ---------------------------------------------
#'
#' # formato de base nominal
#' mortdb %>% glimpse()
#'
#' #cuando mort y deno comparten covariables
#' cdc_mortality(data = mortdb,denominador = denomi_pe,estandar = fctwho_std, year)
#' cdc_mortality(data = mortdb,denominador = denomi_pe,estandar = fctwho_std, year,sexo)
#' cdc_mortality(data = mortdb,denominador = denomi_pe,estandar = fctwho_std, year,sexo,departamento)
#' cdc_mortality(data = mortdb,denominador = denomi_pe,estandar = fctwho_std, year,departamento)
#' cdc_mortality(data = mortdb,denominador = denomi_pe,estandar = fctwho_std, year)
#'
#' #cuando mort y deno no comparten covariables
#' deno_ys <- cdc_denominator(denominador = denomi_pe,year,sexo)
#' cdc_mortality_2(data = mortdb,
#'                 denominador = deno_ys,
#'                 estandar = fctwho_std,
#'                 year,sexo,descrip_grupo10)
#' #o todo en una
#' cdc_mortality_2(data = mortdb,
#'                 denominador = cdc_denominator(denominador = denomi_pe,year),
#'                 estandar = fctwho_std,
#'                 year,edad_q)
#' cdc_mortality_2(data = mortdb %>% filter(edad_estandar=="0-4"),
#'                 denominador = cdc_denominator(denominador = denomi_pe,year),
#'                 estandar = fctwho_std,
#'                 year,edad_estandar)
#'
#' #cuando no se requiere tasas
#' mortdb %>% count(year,edadmef) #edad_fertil
#' mortdb %>% count(year,edad_asis2015) #etapas de vida
#'
#' # opcionalmente, se puede exportar en RDS y DTA
#' # cdc_mortality(data = mortdb,denominador = denomi_pe,estandar = fctwho_std,year,sexo,
#' #               rute_name = "data/mortalidad_anual_nacional_sexo")
#'
#' # formato wide para exportar tabla resumen en CSV y XLSX
#' mortdb %>%
#'   cdc_mortality(denominador = denomi_pe,estandar = fctwho_std,year,sexo,departamento) %>%
#'   cdc_mortality_to_wide()
#' # mortdb %>%
#' #   cdc_mortality(denominador = denomi_pe,estandar = fctwho_std,year,sexo,departamento) %>%
#' #   cdc_mortality_to_wide(rute_name = "table/mortalidad_anual_nacional_sexo_departamento")
#' #
#'
cdc_mortality <- function(data,denominador,estandar,...,rute_name=NA_character_) {

  #data= base nominal
  #denominador= base agregada por year-depart-age-sexo
  #estandar= proporción estandar por quinquenios de edad hasta 80 años
  #rute_name= character string

  denomi_pe <- denominador
  fctwho_std <- estandar

  denomi_pe_sub <- denomi_pe %>%
    group_by(...,edad_estandar) %>%
    summarise(pop=sum(pop,na.rm = T)) %>%
    ungroup()

  mort_to_long <- data %>%
    #definir variables
    #count(...,edad_estandar) %>%
    #combo to add zero counts: factor,group_by,tally!
    mutate(edad_estandar = as.factor(edad_estandar)) %>%
    group_by(...,edad_estandar,.drop = FALSE) %>%
    tally(sort = T) %>%
    ungroup() %>%
    #merge denominators
    left_join(denomi_pe_sub) %>%
    #tasa bruta
    mutate(tasa_b=n/pop) %>%
    full_join(fctwho_std) %>%
    #tasa estandarizada por edad
    mutate(#tasa_e=tasa_b*pop.est,
      #producto en numerador
      # i:population ; j:strata
      #CRUDE DEATH RATE (numerator = p_ij*N_ij)
      tasa_b.pop=tasa_b*pop,
      #DIRECTLY AGE-ADJUSTED DEATH RATE (numerator = p_ij*N_ij)
      tasa_b.est=tasa_b*pop.est) %>%
    #age-adjusted rate per = tasa bruta * poblacion estandar
    group_by(...) %>%
    summarise(n_sum=sum(n),
              pop_sum=sum(pop),
              #CRUDE DEATH RATE (CDR = SUM(p_ij*N_ij) / SUM(N_ij) )
              cdr_cmil=100000*sum(tasa_b.pop)/sum(pop),
              #DIRECT AGE-ADJUSTED DEATH RATE (DAR = SUM(p_ij*N_ij) / SUM(N_ij) )
              dar_cmil=100000*sum(tasa_b.est)/100) %>%
    ungroup() %>%
    arrange(...) %>%
    mutate(country="PERU") %>%
    select(country,year,everything())

  #plan: introducir nombres de forma automática por variables
  #print(g_var)

  if (!is.na(rute_name)) {
    mort_to_long %>%
      write_dta(paste0(rute_name,".dta")) %T>%
      write_rds(paste0(rute_name,".rds"))
    return(mort_to_long)
  } else {
    #mort_to_long %>% write_dta(paste0(rute_name,".dta"))
    return(mort_to_long)
  }

}
#' @describeIn cdc_mortality create denominator
#' @inheritParams cdc_mortality
#' @param estrato estrato de edad a usar para estandarizar
cdc_denominator <- function(denominador,estrato,...){

  denomi_pe <- denominador
  edad_estandar <- enquo(estrato)

  denomi_pe_sub <- denomi_pe %>%
    group_by(...,!!edad_estandar) %>%
    summarise(pop=sum(pop,na.rm = T)) %>%
    ungroup()

  return(denomi_pe_sub)

}
#' @describeIn cdc_mortality create mortality with extra variables
#' @inheritParams cdc_mortality
cdc_mortality_2 <- function(data,denominador,estandar,...,rute_name=NA_character_) {

  #data= base nominal
  #denominador= base agregada por year-depart-age-sexo
  #estandar= proporción estandar por quinquenios de edad hasta 80 años
  #rute_name= character string

  denomi_pe_sub <- denominador
  fctwho_std <- estandar

  mort_to_long <- data %>%
    #definir variables
    #count(...,edad_estandar) %>%
    #combo to add zero counts: factor,group_by,tally!
    mutate(edad_estandar = as.factor(edad_estandar)) %>%
    group_by(...,edad_estandar,.drop = FALSE) %>%
    tally(sort = T) %>%
    ungroup() %>%
    #merge denominators
    left_join(denomi_pe_sub) %>%
    #tasa bruta
    mutate(tasa_b=n/pop) %>%
    full_join(fctwho_std) %>%
    #tasa estandarizada por edad
    mutate(#tasa_e=tasa_b*pop.est,
      #producto en numerador
      # i:population ; j:strata
      #CRUDE DEATH RATE (numerator = p_ij*N_ij)
      tasa_b.pop=tasa_b*pop,
      #DIRECTLY AGE-ADJUSTED DEATH RATE (numerator = p_ij*N_ij)
      tasa_b.est=tasa_b*pop.est) %>%
    #age-adjusted rate per = tasa bruta * poblacion estandar
    group_by(...) %>%
    summarise(n_sum=sum(n),
              pop_sum=sum(pop),
              #CRUDE DEATH RATE (CDR = SUM(p_ij*N_ij) / SUM(N_ij) )
              cdr_cmil=100000*sum(tasa_b.pop)/sum(pop),
              #DIRECT AGE-ADJUSTED DEATH RATE (DAR = SUM(p_ij*N_ij) / SUM(N_ij) )
              dar_cmil=100000*sum(tasa_b.est)/100) %>%
    ungroup() %>%
    arrange(...) %>%
    mutate(country="PERU") %>%
    select(country,year,everything())

  #plan: introducir nombres de forma automática por variables
  #print(g_var)

  if (!is.na(rute_name)) {
    mort_to_long %>%
      write_dta(paste0(rute_name,".dta")) %T>%
      write_rds(paste0(rute_name,".rds"))
    return(mort_to_long)
  } else {
    #mort_to_long %>% write_dta(paste0(rute_name,".dta"))
    return(mort_to_long)
  }

}
#' @describeIn cdc_mortality create mortality with extra variables
#' @inheritParams cdc_mortality
cdc_mortality_adj <- function(data,denominador,estandar,...,rute_name=NA_character_) {

  #data= base nominal
  #denominador= base agregada por year-depart-age-sexo
  #estandar= proporción estandar por quinquenios de edad hasta 80 años
  #rute_name= character string

  denomi_pe_sub <- denominador
  fctwho_std <- estandar

  mort_to_long <- data %>%
    #definir variables
    #count(...,edad_estandar) %>%
    #combo to add zero counts: factor,group_by,tally!
    mutate(edad_estandar = as.factor(edad_estandar)) %>%
    group_by(...,edad_estandar,.drop = FALSE) %>%
    tally(sort = T) %>%
    ungroup() %>%
    #merge denominators
    left_join(denomi_pe_sub) %>%
    #tasa bruta
    mutate(tasa_b=n/pop) %>%
    full_join(fctwho_std) %>%
    #tasa estandarizada por edad
    mutate(#tasa_e=tasa_b*pop.est,
      #producto en numerador
      # i:population ; j:strata
      #CRUDE DEATH RATE (numerator = p_ij*N_ij)
      tasa_b.pop=tasa_b*pop,
      #DIRECTLY AGE-ADJUSTED DEATH RATE (numerator = p_ij*N_ij)
      tasa_b.est=tasa_b*pop.est) %>%
    #age-adjusted rate per = tasa bruta * poblacion estandar
    group_by(...) %>%
    summarise(n_sum=sum(n),
              pop_sum=sum(pop),
              #CRUDE DEATH RATE (CDR = SUM(p_ij*N_ij) / SUM(N_ij) )
              cdr_cmil=100000*sum(tasa_b.pop)/sum(pop),
              #DIRECT AGE-ADJUSTED DEATH RATE (DAR = SUM(p_ij*N_ij) / SUM(N_ij) )
              dar_cmil=100000*sum(tasa_b.est)/100) %>%
    ungroup() %>%
    arrange(...) %>%
    mutate(country="PERU") %>%
    select(country,year,everything())

  #plan: introducir nombres de forma automática por variables
  #print(g_var)

  if (!is.na(rute_name)) {
    mort_to_long %>%
      write_dta(paste0(rute_name,".dta")) %T>%
      write_rds(paste0(rute_name,".rds"))
    return(mort_to_long)
  } else {
    #mort_to_long %>% write_dta(paste0(rute_name,".dta"))
    return(mort_to_long)
  }

}
#' @describeIn cdc_mortality create mortality with extra variables
#' @inheritParams cdc_mortality
cdc_mortality_raw <- function(data,denominador,...,rute_name=NA_character_) {

  #data= base nominal
  #denominador= base agregada por year-depart-age-sexo
  #estandar= proporción estandar por quinquenios de edad hasta 80 años
  #rute_name= character string

  denomi_pe_sub <- denominador
  #fctwho_std <- estandar

  mort_to_long <- data %>%
    #definir variables
    #count(...,edad_estandar) %>%
    #combo to add zero counts: factor,group_by,tally!
    mutate(edad_estandar = as.factor(edad_estandar)) %>%
    group_by(...,edad_estandar,.drop = FALSE) %>%
    tally(sort = T) %>%
    ungroup() %>%
    #merge denominators
    left_join(denomi_pe_sub) %>%
    #tasa bruta
    mutate(tasa_b=n/pop) %>%
    full_join(fctwho_std) %>%
    #tasa estandarizada por edad
    mutate(#tasa_e=tasa_b*pop.est,
      #producto en numerador
      # i:population ; j:strata
      #CRUDE DEATH RATE (numerator = p_ij*N_ij)
      tasa_b.pop=tasa_b*pop,
      # #DIRECTLY AGE-ADJUSTED DEATH RATE (numerator = p_ij*N_ij)
      # tasa_b.est=tasa_b*pop.est
      ) %>%
    #age-adjusted rate per = tasa bruta * poblacion estandar
    group_by(...) %>%
    summarise(n_sum=sum(n),
              pop_sum=sum(pop),
              #CRUDE DEATH RATE (CDR = SUM(p_ij*N_ij) / SUM(N_ij) )
              cdr_cmil=100000*sum(tasa_b.pop)/sum(pop),
              # #DIRECT AGE-ADJUSTED DEATH RATE (DAR = SUM(p_ij*N_ij) / SUM(N_ij) )
              # dar_cmil=100000*sum(tasa_b.est)/100
              ) %>%
    ungroup() %>%
    arrange(...) %>%
    mutate(country="PERU") %>%
    select(country,year,everything())

  #plan: introducir nombres de forma automática por variables
  #print(g_var)

  if (!is.na(rute_name)) {
    mort_to_long %>%
      write_dta(paste0(rute_name,".dta")) %T>%
      write_rds(paste0(rute_name,".rds"))
    return(mort_to_long)
  } else {
    #mort_to_long %>% write_dta(paste0(rute_name,".dta"))
    return(mort_to_long)
  }

}
#' @describeIn cdc_mortality create output table
#' @inheritParams cdc_mortality
#' @param data_long output from cdc_mortality
cdc_mortality_to_wide <- function(data_long,rute_name=NA) {
  product <- data_long %>%
    gather(key,value,n_sum,pop_sum,cdr_cmil,dar_cmil) %>%
    mutate(key=fct_relevel(key,"n_sum","pop_sum","cdr_cmil","dar_cmil")) %>%
    spread(year,value)
  if (is.na(rute_name)) {
    return(product)
  } else {
    product %>%
      write_csv(paste0(rute_name,"_wide.csv")) %T>%
      writexl::write_xlsx(paste0(rute_name,"_wide.xlsx"))
  }

}
