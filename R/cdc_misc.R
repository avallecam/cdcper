#' @title Unclassified functions
#'
#' @description Create html table
#'
#' @describeIn cdc_datatable_html
#'
#' @param data input dataset
#'
#' @import tidyverse
#'
#' @export cdc_datatable_html
#' @export cdc_cut_integer
#'
#' @examples
#'
#' library(tidyverse)
#' mtcars %>%
#'    as_tibble() %>%
#'    cdc_cut_integer(qsec,3) %>%
#'    count(qsec_cut)
#' iris %>%
#'    group_by(Species) %>%
#'    summarise_all(.funs = mean) %>%
#'    cdc_datatable_html()
#'

cdc_datatable_html <- function(data) {
  data %>%
    mutate_if(.predicate = is.numeric,.funs = ~round(.x,2)) %>%
    DT::datatable(
      options = list(
        language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json')))
}

#' @describeIn cdc_datatable_html similar to Hmisc::cut2 (https://stackoverflow.com/a/28075632/6702544)
#' @inheritParams cdc_datatable_html
#' @param variable variable a usar
#' @param number_cuts numbers of cuts

cdc_cut_integer <- function(data,variable,number_cuts=4,end_string="_cut") {
  c_var <- enquo(variable)
  c_var_name_01 <- c_var %>% rlang::as_name() %>% str_c(.,{{end_string}})
  data %>%
    mutate(!!c_var_name_01 := cut({{variable}},
                                  breaks = quantile(data %>% pull({{variable}}), 0:{{number_cuts}}/{{number_cuts}},na.rm = T),
                                  # breaks = 4,
                                  # breaks = c(0,25,75,125,175,225,300)
                                  include.lowest = T,
                                  dig.lab = 2))
}
