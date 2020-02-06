#' @title Descarga de información meteorológica
#'
#' @description  Funciones que permiten la descarga de información meteorológica
#'
#' @describeIn get_data_gri función que realiza la descarga de información
#'
#' @param var Variable que será buscada. Puede ser "Prec", "Tmax" o "Tmin".
#' @param dep Nombre o código (en caracter) del departamento.
#' @param pro Nombre o código (en caracter) de la provincia.
#' @param dis Nombre o código (en caracter) del distrito.
#' @param ids Objeto que contiene la metadata.
#' @param fuente Ubicación de la base de datos. Esta puede ser "local" o "web".
#' @param ruta Dirección de la carpeta que contiene los datos. Solo funciona cuando fuente = "local".
#' @param rango Fecha inicial y final que será filtrada.
#' @param esc_temp Escala temporal en la que se descargará la información. Puede ser "Semana", "Mes", "Año" o NULL (vendría a ser escala diaria).
#' @param down Indique si la información se descargará en un archivo (TRUE) o no (FALSE).
#' @param filename Nombre del archivo donde se exportará la información.
#'
#' @import dplyr
#' @import googledrive
#'
#' @details
#'
#' Descargo de responsabilidad y usos de la información
#'
#' La información que se puede obtener con estas funciones no ha pasado por un control de calidad ni validación con mediciones en tierra. Esta información es una estimación de las condiciones reales atmosféricas en un lugar y tiempo determinado. Las condiciones reales son aquellas obtenidas a través de mediciones directas (por ejemplo, los datos de estaciones meteorológicas).
#' Ventajas
#'
#' - Información para todos los distritos del Perú. Se entiende que se puede obtener información a nivel provincial y departamental también.
#'
#' - Información continua desde el 2000 hasta el 2019. La información se actualiza a mediados de cada mes con la información del mes anterior. Por ejemplo, para mediados de febrero se tendrá información actualizada hasta enero del 2020.
#'
#' - A pesar de no haber pasado por un control de calidad, esta información sirve de muy buena referencia para entender el comportamiento de la atmósfera y el clima. Además, los eventos extremos meteorológicos pueden ser encontrados en esta información.
#'
#' - Las fuentes de información mencionadas son ampliamente utilizadas en el área de Meteorología y Climatología en muchos trabajos de investigación.
#'
#' Desventajas
#'
#' - Al ser información sin control de calidad es posible que la información tenga errores de sobrestimación o subestimación, pero el comportamiento temporal no debería presentar muchas diferencias.
#'
#' - Mientras la resolución temporal sea más alta (por ejemplo: información diaria), el error de los datos también será mayor.
#'
#' - La información distrital, provincial y departamental representa a toda el área geográfica y no a una ubiación espacial específica. Por ejemplo, la información del distrito de Chachapoyas es representativa de todo el terreno dentro del distrito y no de la ubiación de la ciudad de Chachapoyas.
#'
#' - No es posible hablar de cambio climático con los casi 20 años de información que se tienen, pero es posible describir el comportamiento climático normal.
#'
#' Usos
#'
#' - La información debe ser utilizada de manera referencial y no debe ser considerado como el dato real.
#'
#' - Utilizar la información para asociar el comportamiento temporal del clima con alguna variable a estudiar.
#'
#' - La información debe ser utilizada como representativa del área de un distrito y no de una ubicación específica.
#'
#' - La información puede ser utilizada para determinar el comportamiento espacial del clima entre distritos
#'
#' - La información puede ser utilizada para analizar eventos extremos, mas no para determinar el valor numérico del evento.
#'
#'
#' @return Tabla con información de una variable meteorológica para una lugar, tiempo y escala temporal específica.
#'
#' @export get_data_gri
#' @export get_ids
#' @export var_fun
#'
#' @examples
#'
#' library(dplyr)
#' library(googledrive)
#'
#' # Por primera vez: poner 1 -> ir a correo -> aceptar
#' ids.web <- get_ids(fuente = "web")
#'
#' prec <- get_data_gri(
#'    var = "Prec",
#' 	  dep = "LORETO",
#'    pro = "MAYNAS",
#'    dis = "IQUITOS",
#'    fuente = "web",
#'    ids = ids.web,
#'    esc_temp = "Mes",
#'    rango = c("2010-01-01","2018-12-31"))
#'
#' # final product
#' prec
#'

get_data_gri <- function(
  var,
  dep = NULL, pro = NULL, dis = NULL,
  ids = NULL, fuente = NULL, ruta = NULL,
  rango = NULL, esc_temp = NULL,
  down = FALSE, filename = paste0("Grilla_", var, ".csv")) {

  # Archivos necesarios
  if (is.null(ids)) ids <- get_ids(fuente, ruta)

  # Seleccionando lugar
  if (!is.null(dep)) ids <- filter(ids, DEPARTAMEN  == dep | IDDPTO == dep)
  if (!is.null(pro)) ids <- filter(ids, PROVINCIA == pro | IDPROV == pro)
  if (!is.null(dis)) ids <- filter(ids, DISTRITO == dis | IDDIST == dis)

  if (!is.null(dis)) ubi <- ids %>% select(IDDIST)

  # Procesando todos los archivos seleccionados
  for (i in 1:nrow(ids)) {

    # Descarga o lectura de archivos
    arc_id <- pull(ids[i, paste0(var, "_ruta")])

    if (fuente == "local") {

      temp <- arc_id

    } else if (fuente == "web") {

      temp <- tempfile()

      drive_download(
        as_id(arc_id),
        path = temp, overwrite = TRUE)

    } else {

      stop("El argumento [fuente] debe ser 'local' o 'web'")

    }

    dat <- read.csv(temp, stringsAsFactors = FALSE) %>%
      mutate(Fecha = as.Date(Fecha)) %>%
      as_tibble()

    # Juntando datos en una base
    peso <- pull(ids[i, paste0(var, "_peso")])

    if (i == 1) {

      dat.fin <- dat
      dat.fin[ ,var] <- dat.fin[ ,var] * peso

    } else {

      dat.fin[ ,var] <- dat.fin[ ,var] + dat[ ,var] * peso

    }

  }

  # Diviendo en base al peso
  dat.fin[ ,var] <- dat.fin[ ,var] / sum(ids[ ,paste0(var, "_peso")])

  # Seleccionando escala temporal
  if (!is.null(esc_temp)) {

    # Generalizando escalas
    if (esc_temp %in% c("Week","Semana","Semanal")) esc_temp <- "Semana"
    if (esc_temp %in% c("Mes","Mensual")) esc_temp <- "Month"
    if (esc_temp %in% c("Año","Anual")) esc_temp <- "Year"

    svar <- sym(var)
    year.sel <- ifelse(esc_temp == "Semana", "Year_sem", "Year")
    dat.fin <- dat.fin %>%
      group_by_at(c(year.sel, esc_temp)) %>%
      summarise(
        (!! svar) := var_fun((!! svar), var),
        Fecha = head(Fecha, 1))

  }

  # Selección del periodo de tiempo
  if (!is.null(rango)) {

    rango <- as.Date(rango)
    dat.fin <- dat.fin %>%
      filter(Fecha >= rango[1], Fecha <= rango[2])

  }

  # Detalles de la data
  dat.fin[ ,var] <- round(dat.fin[ ,var], 1)

  dat.fin$Departamento <- ids$DEPARTAMEN[1]
  if (!is.null(dis) | !is.null(pro)) dat.fin$Provincia <- ids$PROVINCIA[1]
  if (!is.null(dis)) dat.fin$Distrito <- ids$DISTRITO[1]
  if (!is.null(dis)) dat.fin$Ubigeo <- ubi$IDDIST[1]

  dat.fin <- dat.fin %>% ungroup()

  if (down) write.csv(dat.fin, filename, row.names = FALSE)

  return(dat.fin)

}


#' @describeIn get_data_gri función que contiene la ubicación de la información
#' @inheritParams get_data_gri

get_ids <- function(fuente = "local", ruta = NULL) {

	if ( !(fuente %in% c("local","web")) ) stop("El argumento [fuente] debe ser 'local' o 'web'")

    # Si los archivos están guardados localmente
	if (fuente == "local") {

		# Evaluando ruta
		if (is.null(ruta)) ruta <- getwd()

		# Lectura de metadatos
		ids <- read.csv(
				paste0(ruta, "/Metadata/meta_grilla.csv"),
				stringsAsFactors = FALSE) %>%
			mutate(
				IDDPTO = ifelse(nchar(IDDPTO) == 1, paste0("0",IDDPTO), IDDPTO),
				IDPROV = ifelse(nchar(IDPROV) == 3, paste0("0",IDPROV), IDPROV),
				IDDIST = ifelse(nchar(IDDIST) == 5, paste0("0",IDDIST), IDDIST),
				Prec_ruta = paste0(ruta, "/Datos/Prec_GPM/", IDDIST, ".csv"),
				Tmax_ruta = paste0(ruta, "/Datos/Tmax_GLDAS/", IDDIST, ".csv"),
				Tmin_ruta = paste0(ruta, "/Datos/Tmin_GLDAS/", IDDIST, ".csv")) %>%
			as_tibble()

	}

	# Si se descargará de la web
	else if (fuente == "web") {

		links.sel <- "1oCjDSAPlZeytJCgJsDM2JFrlq1Hta6NN"

		temp <- tempfile()
		drive_download(
			as_id(links.sel),
			path = temp, overwrite = TRUE)
		ids <- read.csv(temp, stringsAsFactors = FALSE) %>%
			mutate(
				IDDPTO = ifelse(nchar(IDDPTO) == 1, paste0("0",IDDPTO), IDDPTO),
				IDPROV = ifelse(nchar(IDPROV) == 3, paste0("0",IDPROV), IDPROV),
				IDDIST = ifelse(nchar(IDDIST) == 5, paste0("0",IDDIST), IDDIST)) %>%
			as_tibble()

	}

	return(ids)

}

#' @describeIn get_data_gri función para realizar suma o promedio dependiendo de la variable asignada
#' @inheritParams get_data_gri
#' @param x Datos serán sumados (precipitación) o promediados (el resto).
#' @param var Nombre de la variable

var_fun <- function(x, var) {

	if (var %in% c("Prec", "prec")) {
		res <- sum(x)
	} else {
		res <- mean(x)
	}

}
