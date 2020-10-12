#' @title Weighted ECDF
#'
#' @description Create tibble with weighted ECDF
#'
#' @describeIn mutate_ewcdf
#'
#' @param data input dataset
#' @param variable input variable
#' @param weights input weights
#'
#' @import dplyr
#' @importFrom magrittr %>%
#'
#' @return tibble
#'
#' @export mutate_ewcdf
#'
#' @examples
#'
#' # source("https://raw.githubusercontent.com/NicolasWoloszko/stat_ecdf_weighted/master/stat_ecdf_weighted.R")
#' # https://stat.ethz.ch/pipermail/r-help/2012-October/337288.html
#' # https://stackoverflow.com/questions/32487457/r-ggplot-weighted-cdf
#'
#' library(tidyverse)
#'
#' x <- rnorm(100)
#' w <- runif(100)
#'
#' a <- ecdf(x = x)
#' b <- spatstat::ewcdf(x = x,weights = w)
#'
#' plot(a)
#' plot(b)
#'
#' tibble(x=x,w=w) %>%
#'   mutate_ewcdf(variable = x,weights = w) %>%
#'   ggplot() +
#'   geom_step(aes(x = x,y = ecdf_x),color="black") +
#'   geom_step(aes(x = x,y = ewcdf_x),color="red")


mutate_ewcdf <- function(data,variable,weights) {
  c_var <- enquo(variable)
  c_var_name_01 <- c_var %>% as_name() %>% str_c("ecdf_",.)
  c_var_name_02 <- c_var %>% as_name() %>% str_c("ewcdf_",.)
  data %>%
    mutate(!!c_var_name_01 := ecdf(variable)(variable) ) %>%
    mutate(!!c_var_name_02 := spatstat::ewcdf(x = variable,
                                              weights = weights)(variable) )
}
