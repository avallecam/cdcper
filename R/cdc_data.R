#' CDC databases: Department population 2000-2020
#'
#' Department population 2000-2020
#'
#' @docType data
#'
#' @usage data(denominadores_departamento_2000_2020_peru)
#'
#' @format An object of class dataset as tibble.
#'
#' @keywords datasets
#'
#' @references MINSA
#' ([REUNIS](https://www.minsa.gob.pe/reunis/data/poblacion_estimada.asp))
#'
#' @source MINSA-REUNIS, <https://www.minsa.gob.pe/reunis/data/poblacion_estimada.asp> ; World (WHO 2000-2025) Standard <https://seer.cancer.gov/stdpopulations/world.who.html>
#'
#' @examples
#' library(cdcper)
#' library(tidyverse)
#' data(denominadores_departamento_2000_2020_peru)
#' denominadores_departamento_2000_2020_peru %>% glimpse()
#'
"denominadores_departamento_2000_2020_peru"

#' @rdname mortdb
"denominadores_departamento_2000_2020_peru"

#' @rdname mortdb
"edad_estandarizada_who"
