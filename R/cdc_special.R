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
#' @import tidyverse
#' @import rlang
#' @import broom
#' @import charlatan
#'
#' @return estimacion puntual e incertidumbre de la distribución de una variable por cada nivel de categorias
#'
#' @export cdc_dotwhiskers_plot
#' @export cdc_dotwhiskers_plot_2
#'
#' @examples
#'
#' # inspiration: https://twitter.com/robinson_es/status/1193204992120958976/photo/1
#'
#' library(tidyverse)
#' library(charlatan)
#'
#' n_obs <- 33
#' set.seed(n_obs)
#'
#' ch_data_wide <- tibble(
#'   #values
#'   values_01 = ch_integer(n = n_obs,min = 5,max = 10),
#'   values_02 = ch_integer(n = n_obs,min = 5,max = 20),
#'   values_03 = ch_integer(n = n_obs,min = 5,max = 30),
#'   category = ch_integer(n = n_obs,min = 0,max = 1) %>% as.logical()) %>%
#'   pivot_longer(cols = values_01:values_03,
#'                names_to = "variable",
#'                values_to = "value")
#'
#' # ch_data_wide %>%
#' #   group_by(variable) %>%
#' #   skimr::skim_without_charts()
#'
#' cdcper::cdc_dotwhiskers_plot(data = ch_data_wide,
#'                              var_categorical = variable,
#'                              var_continuous = value) %>%
#'   ggplot(aes(x = estimate,y = variable)) +
#'   geom_errorbarh(aes(xmax = conf.high, xmin = conf.low)) +
#'   geom_point()
#'
#' cdcper::cdc_dotwhiskers_plot_2(data = ch_data_wide,
#'                                var_continuous = value,
#'                                category, variable)
#'
#'

cdc_dotwhiskers_plot <- function(data,var_categorical,var_continuous) {
  #my first time with the curly curly!
  data %>%
    group_by({{var_categorical}}) %>%
    summarise(total_n=n(),
              t_test=list(t.test({{var_continuous}}))) %>%
    mutate(tidied=map(.x = t_test,.f = broom::tidy)) %>%
    unnest(cols = c(tidied))
}

#' @describeIn cdc_dotwhiskers_plot priorización con dos covariables
#' @inheritParams cdc_dotwhiskers_plot
#' @param ... add multiple categorical variables

cdc_dotwhiskers_plot_2 <- function(data,var_continuous,...) {
  #how to use curly curly with multiple variables?
  by_c <- enquos(...)
  data %>%
    group_by(!!!by_c) %>%
    summarise(total_n=n(),
              t_test=list(t.test({{var_continuous}}))) %>%
    mutate(tidied=map(.x = t_test,.f = broom::tidy)) %>%
    unnest(cols = c(tidied))
}
