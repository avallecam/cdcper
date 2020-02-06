#' CDC databases: Mortality, Hospital outflows, and more
#'
#' Random subset of mortality (2013-2015), hospital outflows (2000-2015) databases from Peru
#'
#' @docType data
#'
#' @usage data(mortdb)
#'
#' @format An object of class dataset.
#'
#' @keywords datasets
#'
#' @references CDC-MINSA
#' ([dge.gob.pe](https://www.dge.gob.pe/portal/docs/asis/Asis_mortdb.pdf))
#'
#' @source CDC-MINSA, <https://www.dge.gob.pe/portal/> ; World (WHO 2000-2025) Standard <https://seer.cancer.gov/stdpopulations/world.who.html>
#'
#' @examples
#' library(cdcper)
#' library(tidyverse)
#' data(mortdb)
#' mortdb %>% glimpse()
#'
"mortdb"

#' @rdname mortdb
"denominadores_departamento_2000_2020_peru"

#' @rdname mortdb
"edad_estandarizada_who"

#' @rdname mortdb
"egresos_hospitalarios"
