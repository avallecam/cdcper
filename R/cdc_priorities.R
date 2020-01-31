#' @title Priorizar listados en base a multiples covariables
#'
#' @description Mostrar qué elementos representan el 80% de la suma total
#'
#' @describeIn cdc_pareto_lista
#'
#' @param data base de datos
#' @param variable variable numérica contínua bajo la cual priorizar algún listado
#' @param pareto_cut punto de corte tipo pareto
#'
#' @import dplyr
#' @import tidyr
#' @import rlang
#'
#' @return selección por criterio pareto y coalescencia para multiples covariables
#'
#' @export cdc_pareto_lista
#' @export cdc_pareto_lista_2
#' @export cdc_carga_coalesce
#'
#' @examples
#'
#' # not yet
#'
cdc_pareto_lista <- function(data,variable,pareto_cut=85) {

  c_var <- enquo(variable)
  c_var_name_01 <- c_var %>% as_name() %>% str_c("pct_",.)
  c_var_name_02 <- c_var %>% as_name() %>% str_c("cum_",.)
  c_var_name_03 <- c_var %>% as_name() %>% str_c("cut_",.)

  data %>%
    arrange(desc(!!c_var)) %>%
    mutate(!!c_var_name_01 := 100*!!c_var/sum(!!c_var),
           !!c_var_name_02 := 100*cumsum(!!c_var)/sum(!!c_var),
           !!c_var_name_03 := if_else(100*cumsum(!!c_var)/sum(!!c_var) <= pareto_cut,str_c("dentro_del_",pareto_cut,"%"),"no"))
}

#' @describeIn cdc_pareto_lista priorización con dos covariables
#' @inheritParams cdc_pareto_lista
#' @param variable_c variable continua
#' @param variable_d variable discreta

cdc_pareto_lista_2 <- function(data,variable_c,variable_d,pareto_cut=80) {

  d_var <- enquo(variable_d)
  c_var <- enquo(variable_c)
  c_var_name_01 <- c_var %>% as_name() %>% paste(collapse = "_") %>% str_c("pct_",.)
  c_var_name_02 <- c_var %>% as_name() %>% paste(collapse = "_") %>% str_c("cum_",.)
  c_var_name_03 <- c_var %>% as_name() %>% paste(collapse = "_") %>% str_c("cut_",.)
  c_var_name_01 <- "pct_pareto"
  c_var_name_02 <- "cum_pareto"
  c_var_name_03 <- "cut_pareto"

  data %>%
    arrange(!!d_var,desc(!!c_var)) %>%
    mutate(!!c_var_name_01 := 100*!!c_var/sum(!!c_var),
           !!c_var_name_02 := 100*cumsum(!!c_var)/sum(!!c_var),
           !!c_var_name_03 := if_else(100*cumsum(!!c_var)/sum(!!c_var) <= pareto_cut,str_c("dentro_del_",pareto_cut,"%"),"no"))
}

#' @describeIn cdc_pareto_lista coalescencia de multiples covariables
#' @inheritParams cdc_pareto_lista

cdc_carga_coalesce <- function(data) {
  data %>%
    pivot_longer(cols = starts_with("cut_"),names_to = "key",values_to = "value") %>%
    mutate(value=if_else(value=="no",NA_character_,value),
           value=if_else(!is.na(value),key,value)) %>%
    pivot_wider(names_from = key,values_from = value) %>%
    mutate(cut_coalesce = coalesce(!!! select(.,starts_with("cut_")))) #%>%
    #filter(!is.na(cut_coalesce)) #%>% select(year:subcat,cut_coalesce)
}
