% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cdc_time.R
\name{cdc_yearweek_to_date}
\alias{cdc_yearweek_to_date}
\title{Time related function}
\usage{
cdc_yearweek_to_date(data, year_integer, week_integer)
}
\arguments{
\item{data}{input dataset}

\item{year_integer}{year column variable as integer}

\item{week_integer}{week column variable as integer}
}
\value{
html table
}
\description{
Create a date from year and week variable columns
}
\section{Functions}{
\itemize{
\item \code{cdc_yearweek_to_date}: 
}}

\examples{

library(tidyverse)
library(lubridate)
library(aweek)
data_ts <- tibble(date=seq(ymd('2012-04-07'),
                           ymd('2012-04-22'),
                           by = '5 day')) \%>\%
  mutate(#value = rnorm(n(),mean = 5),
    #using aweek
    epiweek_d = date2week(date, week_start = "Sunday"),
    epiweek_w = date2week(date, week_start = "Sunday", floor_day = TRUE),
    #using lubridate
    epiweek_n = epiweek(date),
    day_of_week = wday(date,label = T,abbr = F),
    month = month(date,label = F,abbr = F),
    year = year(date)) \%>\%
  print()

data_ts \%>\%
  # use the function
  cdc_yearweek_to_date(year_integer = year,
                       week_integer = epiweek_n)
}
