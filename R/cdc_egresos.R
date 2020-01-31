#' @title Crear bases y tablas de conteo y frecuencia de egresos hospitalarios
#'
#' @description Generación de bases resumen para la creación de gráficos de tendencia y tablas reporte con información nominal (por sujeto).
#'
#' @describeIn cdc_egresos
#'
#' @param data data frame con información nominal (por sujeto)
#' @param ... lista de una o más variables
#' @param percent_by (numeric vector) seleccionar las variables para generar porcentajes
#' @param save (logic) opción para grabar o no la base resumen generada
#' @param rute (character string) ruta del objeto a exportar. Las bases se exportan en RDS y DTA. Las tablas en CSV y XLSX.
#'
#' @import dplyr
#' @import rlang
#' @import tidyr
#'
#' @return Base con conteo y porcentajes por cruce de variables
#'
#' @export cdc_egresos_summary
#' @export cdc_egresos_by_year
#' @export cdc_piramide
#'
#' @examples
#'
#' # ISSUE: egresos dataset no puede ser subida al paquete!
#'
#' # library(tidyverse)
#' # library(tidyselect)
#' # library(rlang)
#' # library(naniar)
#' # library(haven)
#' # library(readxl)
#' # library(ggrepel)
#' # library(labelled)
#' # library(gridExtra)
#' # library(compareGroups)
#' # library(janitor)
#' # library(magrittr)
#' #
#' # egress <- data(egresos)
#' # #explore data
#' # egress %>% glimpse()
#' # #in one line
#' # cdc_egresos_summary(data = egress,year,nombdep)
#' # cdc_egresos_summary(data = egress,percent_by = c(1,2),year,nombdep,sexo)
#' # #cdc_egresos_summary(data = egress,percent_by = c(2,3),year,nombdep,sexo)
#' # cdc_egresos_summary(data = egress,year,nombdep,edad_asis2015,sexo)
#' # cdc_egresos_summary(data = egress,percent_by = c(1,2,3),year,nombdep,edad_asis2015,sexo)
#' # cdc_egresos_summary(data = egress,year,desc_12)
#' # cdc_egresos_summary(data = egress,percent_by = c(1,2),year,nombdep,desc_12)
#' # #with tidyverse
#' # egress %>%
#' #   filter(year==max(year)) %>%
#' #   cdc_egresos_summary(year,desc_12) %>%
#' #   arrange(desc(pct))
#' # #output
#' # egress %>%
#' #   cdc_egresos_summary(year,nombdep,save = T) #dataset
#' # egress %>%
#' #   cdc_egresos_summary(percent_by = c(1,2),
#' #                       year,nombdep,edad_asis2015) %>%
#' #   cdc_egresos_by_year() #wide table
#' # #plot piramide
#' # egress %>%
#' #   filter(year==max(year)) %>%
#' #   cdc_egresos_summary(year,e_quinq_2,sexo) %>%
#' #   cdc_piramide() #class ggplot
#'
cdc_egresos_summary <- function(data,...,percent_by=c(1),save=FALSE,rute="data/") {

  n <- percent_by
  egress <- data
  g_var <- enquos(...)
  name <- g_var %>% quos_auto_name() %>% names() %>% paste0(collapse = "_")
  rute_name <- paste0(rute,"egresos-",name,"-long")

  db_1s <- egress %>%
    filter(sexo!="0") %>%
    count(!!!g_var) %>%
    group_by(!!!g_var[n]) %>%
    mutate(pct=100*n/sum(n)#,pct=round(pct,2)
    ) %>%
    #arrange(desc(pct)) %>%
    ungroup()

  #return(db_1s)
  #return(g_var)

  if (save==TRUE) {
    db_1s %>%
      write_dta(paste0(rute_name,".dta")) %T>%
      write_rds(paste0(rute_name,".rds"))
    return(db_1s)
  } else {
    return(db_1s)
  }

}
#' @describeIn cdc_egresos create denominator
#' @inheritParams cdc_egresos
#' @param data_long output from cdc_egresos_summary
cdc_egresos_by_year <- function(data_long,rute=NA) {
  out <- data_long
  name <- out %>% select(-n,-pct) %>% colnames() %>% paste0(collapse = "_")
  rute_name <- paste0(rute,"egresos-",name,"-wide")
  product <- out %>%
    gather(key,value,n,pct) %>%
    unite("nkey",c("year","key")) %>%
    spread(nkey,value)

  if (is.na(rute)) {
    return(product)
  } else {
    product %>%
      write_csv(paste0(rute_name,".csv")) %T>%
      writexl::write_xlsx(paste0(rute_name,".xlsx"))
  }

}
#' @describeIn cdc_egresos create denominator
#' @inheritParams cdc_egresos
#' @param x variable en eje x de la pirámide
#' @param y variable en eje y de la pirámide
#' @param z variable en eje z de la pirámide
cdc_piramide <- function(data,x=pct,y=e_quinq_2,z=sexo) {

  data <- data
  pct <- enquo(x)
  e_quinq_2 <- enquo(y)
  sexo <- enquo(z)

  pyr_p <- data %>%
    mutate(pct=if_else(sexo=="F",-pct,pct)) %>%
    ggplot(aes(e_quinq_2,pct,fill=sexo)) +
    geom_col() +
    coord_flip() +
    scale_y_continuous(breaks = -10:10,
                       labels = as.character(-10:10))

  return(pyr_p)
}
