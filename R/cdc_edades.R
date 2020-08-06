#' @title Clean age categories
#'
#' @description Categorize continuous age into classical levels
#'
#' @describeIn cdc_edades_peru
#'
#' @param data input dataset
#' @param variable_edad edad como variable continua
#'
#' @import dplyr
#' @import forcats
#'
#' @return tidy categorical variables from one continuous age variable
#'
#' @export cdc_edades_peru
#'
#' @examples
#'
#' library(tidyverse)
#' library(charlatan)
#' library(skimr)
#' library(rlang)
#' data_edad <- tibble(age=charlatan::ch_integer(n = 100,min = 2,max = 100))
#' data_edad %>% skimr::skim_without_charts()
#' data_edad %>%
#'   cdc_edades_peru(variable_edad = age)
#' data_edad %>%
#'   cdc_edades_peru(variable_edad = age) %>%
#'   select(age,edad_quinquenal) %>%
#'   group_by(edad_quinquenal) %>%
#'   skimr::skim() %>%
#'   select(edad_quinquenal,numeric.p0:numeric.p100)
#'


cdc_edades_peru <- function(data,variable_edad) {
  data %>%
    mutate(edad={{variable_edad}}) %>%
    mutate(
      edad_etapas_de_vida_c =
        case_when(
          edad >= 0 & edad < 12 ~ "0_11a",
          edad >= 12 & edad < 18 ~ "12_17a",
          edad >= 18 & edad < 30 ~ "18_29a",
          edad >= 30 & edad < 60 ~ "30_59a",
          edad >= 60 ~ "60a_mas",
          TRUE ~ NA_character_),
      edad_etapas_de_vida_t = case_when(
        edad_etapas_de_vida_c == "0_11a" ~ "ninho",
        edad_etapas_de_vida_c == "12_17a" ~ "adolescente",
        edad_etapas_de_vida_c == "18_29a" ~ "joven",
        edad_etapas_de_vida_c == "30_59a" ~ "adulto",
        edad_etapas_de_vida_c == "60a_mas" ~ "adulto_mayor",
        TRUE ~ edad_etapas_de_vida_c),
      edad_etapas_de_vida_c =
        fct_relevel(edad_etapas_de_vida_c,
                    "0_11a",
                    "12_17a",
                    "18_29a",
                    "30_59a",
                    "60a_mas"),
      edad_etapas_de_vida_t =
        fct_relevel(edad_etapas_de_vida_t,
                    "ninho",
                    "adolescente",
                    "joven",
                    "adulto",
                    "adulto_mayor")
    ) %>%
    mutate(
      edad_grupo_x =
        case_when(
          edad >= 0 & edad<=15 ~ "0 a 14 años",
          edad>=15 & edad<25 ~ "15 a 24 años",
          edad>=25 & edad<35 ~ "25 a 34 años",
          edad>=35 & edad<45 ~ "35 a 44 años",
          edad>=45 & edad<55 ~ "45 a 54 años",
          edad>=55 & edad<65 ~ "55 a 64 años",
          edad>=65 & edad<75 ~ "65 a 74 años",
          edad>=75 ~ "75 años a más",
          TRUE ~ NA_character_),
      edad_quinquenal=cut(edad,
                          breaks=c(0, 4, 9, 14, 19, 24,
                                   29, 34, 39, 44, 49, 54,
                                   59, 64, 69, 74, 79, 84, 89, Inf),
                          labels=c("0 a 4 años","5 a 9 años","10 a 14 años", "15 a 19 años",
                                   "20 a 24 años","25 a 29 años","30 a 34 años","35 a 39 años",
                                   "40 a 44 años","45 a 49 años", "50 a 54 años","55 a 59 años",
                                   "60 a 64 años","65 a 69 años","70 a 74 años","75 a 79 años",
                                   "80 a 84 años","85 a 89 años", "90 a  mas años"),
                          include.lowest = T,ordered_result = T
                          ),
      edad_decenios=cut(edad,
                          breaks=c(0, seq(10,100,10), Inf),
                          # labels=c("0 a 4 años","5 a 9 años","10 a 14 años", "15 a 19 años",
                          #          "20 a 24 años","25 a 29 años","30 a 34 años","35 a 39 años",
                          #          "40 a 44 años","45 a 49 años", "50 a 54 años","55 a 59 años",
                          #          "60 a 64 años","65 a 69 años","70 a 74 años","75 a 79 años",
                          #          "80 a 84 años","85 a 89 años", "90 a  mas años"),
                          include.lowest = T,ordered_result = T,right = F
      ),
      edad_quinquenal_raw=cut(edad,
                        breaks=c(0, 1, seq(5,100,5), Inf),
                        # labels=c("0 a 4 años","5 a 9 años","10 a 14 años", "15 a 19 años",
                        #          "20 a 24 años","25 a 29 años","30 a 34 años","35 a 39 años",
                        #          "40 a 44 años","45 a 49 años", "50 a 54 años","55 a 59 años",
                        #          "60 a 64 años","65 a 69 años","70 a 74 años","75 a 79 años",
                        #          "80 a 84 años","85 a 89 años", "90 a  mas años"),
                        include.lowest = T,ordered_result = T,right = F
      )
      ) %>%
    mutate(
      edad_etapas_de_vida_n =
        case_when(
          edad >= 0 & edad < 1 ~ "00_00a",
          edad >= 1 & edad < 5 ~ "01_04a",
          edad >= 5 & edad < 10 ~ "05_09a",
          edad >= 10 & edad < 15 ~ "10_14a",
          TRUE ~ NA_character_),
      edad_etapas_de_vida_n =
        fct_relevel(edad_etapas_de_vida_n,
                    "00_00a",
                    "01_04a",
                    "05_09a",
                    "10_14a"))
}
