#' Acceder a bases del HIS MIS
#'
#' Limpiar salida y generar bases del HIS MIS.
#'
#' @describeIn his_create funcion para limpiar bases de HIS MIS
#'
#' @param data_index dataframe con indices de ingreso
#' @param i_info indice de consulta
#'
#' @import dplyr
#' @import zoo
#'
#' @return Bases limpias de HIS MIS.
#'
#' @examples
#'
#' # ejemplo
#' # falta
#'
#'@export his_create
#'@export his_extract
#'
his_create <- function(data_index,i_info) { #i_info=1
  his <- data_index
  a <- data_frame()
  b <- his %>% filter(info_i==i_info) %>% .$loc
  for (i in 1:length(b)) { #i=1
    a <- union_all(a,read_rds(b[i]) %>% mutate(dept=i))
  }
  a %>%
    #replace each NA with the most recent non-NA prior to it
    do(zoo::na.locf(.)) %>%
    mutate(nombdep=str_to_upper(nombdep)) %>%
    return()
}
#' @describeIn his_create unir bases por departamento
#' @inheritParams his_create
#' @param i_ord identificador
his_extract <- function(data_index,i_ord) {#i_ord=1
  his_ord <- data_index
  a <- data_frame()
  d <- data_frame()
  b <- his_ord %>% filter(ord==i_ord) %>% .$loc
  for (j in 1:length(b)) {#j=1 j=2
    if (str_detect(b[j],"HIS-MIS_2")) {
      c <- read_xlsx(b[j],sheet = 2) %>%
        rename(SEXO=sexo_ok) %>%
        select(-codlist_12_110,-cod_12morbi)
    } else {
      e <- read_xlsx(b[j],sheet = 2) %>%
        rename(
          NOMBDEP=departam,
          NOMBPROV=provincia,
          NOMBDIST=distrito
        ) %>%
        mutate_at(vars(NOMBDEP,NOMBPROV,NOMBDIST),funs(str_to_title))
      d <- e %>%
        select(RES_HAB,reg_natu) %>%
        filter(!is.na(reg_natu)) %>%
        count(RES_HAB,reg_natu) %>% select(-n)
      c <- e %>% select(-reg_natu)
    }
    a <- union_all(a,c)
  }
  a %>%
    left_join(g12) %>%
    left_join(g12_110) %>%
    left_join(d) %>%
    rename_all(funs(make.names(.))) %>%
    rename_all(funs(str_to_lower(.))) %>%
    select(-casos.) %>%
    rename(list_12=x12grumorbi,
           codlist_12=cod_12morbi) %>%
    mutate(edad_q=fct_relevel(edad_q,"0-1","2-4","5-9")) %>%
    filter(edad_q!="IGN.") %>%
    return()
}
