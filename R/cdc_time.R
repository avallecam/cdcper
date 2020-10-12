#' @title Time related function
#'
#' @description Create a date from year and week variable columns
#'
#' @describeIn cdc_yearweek_to_date
#'
#' @param data input dataset
#' @param year_integer year column variable as integer
#' @param week_integer week column variable as integer
#'
#' @import dplyr
#' @import lubridate
#' @import aweek
#'
#' @return html table
#'
#' @export cdc_yearweek_to_date
#'
#' @examples
#'
#' library(tidyverse)
#' library(lubridate)
#' library(aweek)
#' data_ts <- tibble(date=seq(ymd('2012-04-07'),
#'                            ymd('2012-04-22'),
#'                            by = '5 day')) %>%
#'   mutate(#value = rnorm(n(),mean = 5),
#'     #using aweek
#'     epiweek_d = date2week(date, week_start = "Sunday"),
#'     epiweek_w = date2week(date, week_start = "Sunday", floor_day = TRUE),
#'     #using lubridate
#'     epiweek_n = epiweek(date),
#'     day_of_week = wday(date,label = T,abbr = F),
#'     month = month(date,label = F,abbr = F),
#'     year = year(date)) %>%
#'   print()
#'
#' data_ts %>%
#'   # use the function
#'   cdc_yearweek_to_date(year_integer = year,
#'                        week_integer = epiweek_n)

cdc_yearweek_to_date <- function(data,year_integer,week_integer) {

  # library(aweek)
  # aweek::set_week_start(7) #sunday -epi-
  # aweek::get_week_start()

  data %>%
    mutate(year_integer={{year_integer}},
           week_integer={{week_integer}}) %>%
    mutate(week_integer=if_else(str_length(week_integer)==1,
                                str_c("0",week_integer),
                                as.character(week_integer))) %>%
    # string
    mutate(epiweek_w=str_c(year_integer,"-W",week_integer)) %>%
    # to aweek
    mutate(epiweek_w=aweek::as.aweek(epiweek_w,week_start = "Sunday")) %>%
    # to date
    mutate(epi_date=week2date(epiweek_w, week_start = "Sunday", floor_day = TRUE))
}
