#' @title Descarga de información meteorológica
#' 
#' @description  Funciones que permiten la descarga de información meteorológica (v0.2.1)
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

    # Seleccionando distritos
    if (!is.null(dep)) ids <- filter(ids, DEPARTAMEN  == dep | IDDPTO == dep)
    if (!is.null(pro)) ids <- filter(ids, PROVINCIA == pro | IDPROV == pro)
    if (!is.null(dis)) ids <- filter(ids, DISTRITO == dis | IDDIST == dis)

    if (!is.null(dis)) ubi <- ids %>% select(IDDIST)

    # Procesando todos los archivos seleccionados
    for (i in 1:nrow(ids)) {
    
        # Descarga o lectura de archivos
        arc_id <- pull(ids[i, "Ruta"])

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
        peso <- ids[i, c("Prec_peso", "Tmax_peso", "Tmin_peso")]

        if (i == 1) {
        
            dat.fin <- dat %>%
                mutate(
                    Prec = Prec * pull(peso[1, "Prec_peso"]),
                    Tmax = Tmax * pull(peso[1, "Tmax_peso"]),
                    Tmin = Tmin * pull(peso[1, "Tmin_peso"]))
        
        } else {
        
            dat.fin <- dat.fin %>%
                mutate(
                    Prec = Prec + dat$Prec * pull(peso[1, "Prec_peso"]),
                    Tmax = Tmax + dat$Tmax * pull(peso[1, "Tmax_peso"]),
                    Tmin = Tmin + dat$Tmin * pull(peso[1, "Tmin_peso"]))
        
        }
    
    }
    
    # Diviendo en base al peso
    dat.fin <- dat.fin %>%
        mutate(
            Prec = Prec / sum(ids$Prec_peso),
            Tmax = Tmax / sum(ids$Tmax_peso),
            Tmin = Tmin / sum(ids$Tmin_peso))

    # Seleccionando escala temporal
    if (!is.null(esc_temp)) {
        
        # Generalizando escalas
        if (esc_temp == "Week") esc_temp <- "Semana"
        if (esc_temp == "Mes") esc_temp <- "Month"
        if (esc_temp == "Año") esc_temp <- "Year"

        #svar <- sym(var)
        year.sel <- ifelse(esc_temp == "Semana", "Year_sem", "Year")
        dat.fin <- dat.fin %>%
            group_by_at(c(year.sel, esc_temp)) %>%
            summarise(
                Prec = round( var_fun(Prec, "Prec") , 1),
                Tmax = round( var_fun(Tmax, "Tmax") , 1),
                Tmin = round( var_fun(Tmin, "Tmin") , 1),
                Fecha = head(Fecha, 1))

    }
    
    # Selección del periodo de tiempo
    if (!is.null(rango)) {
    
        rango <- as.Date(rango)
        dat.fin <- dat.fin %>%
            filter(Fecha >= rango[1], Fecha <= rango[2])
            
    }

    # Selccionando variables
    dat.names <- names(dat.fin)
    dat.names <- dat.names[ !(dat.names %in% c("Prec","Tmax","Tmin")) ]
    dat.fin <- dat.fin[ , c(dat.names, var)]

    # Detalles de la data
    dat.fin$Departamento <- ids$DEPARTAMEN[1]
    if (!is.null(dis) | !is.null(pro)) dat.fin$Provincia <- ids$PROVINCIA[1]
    if (!is.null(dis)) dat.fin$Distrito <- ids$DISTRITO[1]

    if (down) write.csv(dat.fin, filename, row.names = FALSE)

    return(dat.fin)

}

#' @describeIn get_data_gri función que contiene la ubicación de la información
#' @inheritParams get_data_gri

# Función para descargar metadatos
get_ids <- function(fuente = "local", ruta = NULL) {

    # Evaluando tipo de datos
    if ( !(fuente %in% c("local","web")) ) stop("El argumento [fuente] debe ser 'local' o 'web'")

    # Si los archivos están guardados localmente
    if (fuente == "local") {

        # Evaluando ruta
        if (is.null(ruta)) ruta <- getwd()

        # Lectura inicial
        ids <- read.csv(
                paste0(ruta, "/meta_grilla.csv"), 
                stringsAsFactors = FALSE) %>%
            mutate(
                IDDPTO = ifelse(nchar(IDDPTO) == 1, paste0("0",IDDPTO), IDDPTO),
                IDPROV = ifelse(nchar(IDPROV) == 3, paste0("0",IDPROV), IDPROV),
                IDDIST = ifelse(nchar(IDDIST) == 5, paste0("0",IDDIST), IDDIST),
                Ruta = paste0(ruta, "/Grilla/", IDDIST, ".csv")) %>%
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

#' @describeIn get_data_gri
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
