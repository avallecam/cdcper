#' @title Make some special plots
#'
#' @description Make a dot-whiskers plot (a.k.a starwars TE fighter spaceship plot!)
#'
#' @describeIn cdc_dotwhiskers_plot
#'
#' @param data base de datos
#' @param var_categorical variable categórica (idealmente politómica)
#' @param var_continuous variable numérica contínua
#'
#' @import dplyr
#' @import tidyr
#' @import rlang
#'
#' @return estimacion puntual e incertidumbre de la distribución de una variable por cada nivel de categorias
#'
#' @export cdc_dotwhiskers_plot
#'
#' @examples
#'
#' # not yet
#' # but check this out: https://twitter.com/robinson_es/status/1193204992120958976/photo/1
#'

cdc_dotwhiskers_plot <- function(data,var_categorical,var_continuous) {
  #my first time with the curly curly!
  data %>%
    group_by({{var_categorical}}) %>%
    summarise(total_n=n(),
              t_test=list(t.test({{var_continuous}}))) %>%
    mutate(tidied=map(t_test,tidy)) %>%
    unnest(cols = c(tidied))
}
