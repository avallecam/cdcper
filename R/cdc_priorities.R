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
#' @import ggrepel
#'
#' @return selección por criterio pareto y coalescencia para multiples covariables
#'
#' @export cdc_pareto_lista
#' @export cdc_pareto_lista_2
#' @export cdc_carga_coalesce
#' @export cdc_pareto_plot
#'
#' @examples
#'
#' library(tidyverse)
#' library(charlatan)
#'
#' n_obs <- 11
#' set.seed(n_obs)
#'
#' ch_data_wide <- tibble(
#'   #names
#'   name = ch_currency(n = n_obs),
#'   #values
#'   category = ch_integer(n = n_obs,min = 0,max = 1) %>% as.logical(),
#'   # category_02 = ch_integer(n = n_obs,min = 0,max = 1) %>% as.logical(),
#'   value_01 = ch_beta(n = n_obs,shape1 = 2,shape2 = 8),
#'   value_02 = ch_integer(n = n_obs,min = 1.2,max = 9.6)) %>%
#'   pivot_longer(cols = value_01:value_02,
#'                names_to = "variable",
#'                values_to = "numeric") %>%
#'   mutate(beta = ch_beta(n = n_obs*2,shape1 = 1,shape2 = 8))
#'
#' cdcper::cdc_pareto_lista(data = ch_data_wide,
#'                          variable = numeric,
#'                          pareto_cut = 80) %>%
#'   avallecam::print_inf()
#'
#' cdcper::cdc_pareto_lista_2(data = ch_data_wide,
#'                            variable_c = numeric,
#'                            variable_d = category,
#'                            pareto_cut = 80) %>%
#'   avallecam::print_inf()
#'
#' cdcper::cdc_pareto_lista(data = ch_data_wide,
#'                          variable = numeric,
#'                          pareto_cut = 80) %>%
#'   cdcper::cdc_pareto_lista(variable = beta,
#'                            pareto_cut = 85) %>%
#'   avallecam::print_inf()
#'
#' cdcper::cdc_pareto_lista(data = ch_data_wide,
#'                          variable = numeric,
#'                          pareto_cut = 80) %>%
#'   cdcper::cdc_pareto_lista(variable = beta,
#'                            pareto_cut = 85) %>%
#'   cdcper::cdc_carga_coalesce() %>%
#'   avallecam::print_inf()
#'
#' cdcper::cdc_pareto_lista(data = ch_data_wide,
#'                          variable = numeric,
#'                          pareto_cut = 80) %>%
#'   cdcper::cdc_pareto_plot(pct_ = pct_numeric,
#'                           cum_ = cum_numeric,
#'                           variable_value = numeric,
#'                           variable_label = name) #%>%
#'   # plotly::ggplotly()
#'
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

#' @describeIn cdc_pareto_lista grafico de pareto: % individual vs % acumulado
#' @inheritParams cdc_pareto_lista
#' @param cdc_pareto_lista resultado de cdc_pareto_lista
#' @param pct_ % individual
#' @param cum_ % acumulado
#' @param variable_value nombre de la variable numerica evaluada
#' @param variable_label nombre de la variable para las etiquetas

cdc_pareto_plot <- function(cdc_pareto_lista,pct_,cum_,
                            variable_value,variable_label) {

  cdc_pareto_lista %>%
    ggplot(aes(x = {{pct_}}, y = {{cum_}},
               size={{variable_value}})) +
    geom_point(aes(color={{variable_value}},
                   #alpha={{variable_value}}
                   )
               ) +
    scale_color_viridis_c() +
    scale_y_continuous(breaks = seq(0,100,5),
                       labels = seq(0,100,5)) +
    scale_x_continuous(breaks = seq(0,100,5),
                       labels = seq(0,100,5)) +
    coord_fixed(ratio = 1) +
    ggrepel::geom_text_repel(aes(label={{variable_label}}),
                             direction    = "y",
                             vjust        = 1,
                             hjust        = 0,
                             force        = 0.5,
                             nudge_x      = 6.85,
                             nudge_y      = 6.85,
                             segment.size = 0.2,
                             show.legend = F)
}
