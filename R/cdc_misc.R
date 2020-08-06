#' @title Unclassified functions
#'
#' @description Create html table
#'
#' @describeIn cdc_datatable_html
#'
#' @param data input dataset
#'
#' @import dplyr
#'
#' @return html table
#'
#' @export cdc_datatable_html
#'
#' @examples
#'
#' library(tidyverse)
#' iris %>% group_by(Species) %>% summarise_all(.funs = mean) %>% cdc_datatable_html()
#'

cdc_datatable_html <- function(data) {
  data %>%
    mutate_if(.predicate = is.numeric,.funs = ~round(.x,2)) %>%
    DT::datatable(
      options = list(
        language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json')))
}
