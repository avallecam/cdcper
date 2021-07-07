#' @title Forecast plot with sw_seep() output
#'
#' @description Create ggplot of forecast plot with sw_seep() output
#'
#' @describeIn gg_forecast
#'
#' @param data input dataset
#' @param axis_x variable with time axis type "yearmon"
#'
#' @import tidyverse
#' @import sweep
#' @import forecast
#' @importFrom magrittr %>%
#'
#' @return ggplot
#'
#' @export gg_forecast
#'
#' @examples
#'
#' # from: ?sw_sweep
#'
#' library(forecast)
#' library(sweep)
#' library(dplyr)
#' library(ggplot2)
#'
#' # ETS forecasts
#' a <- USAccDeaths %>%
#'   ets() %>%
#'   forecast(level = c(80, 95, 99)) %>%
#'   sw_sweep()
#'
#' # crear false observed values for predicted time units
#' b <- a %>%
#'   filter(key=="forecast") %>%
#'   mutate(key="observed") %>%
#'   mutate(value=mean(value)) %>%
#'   mutate(across(.cols = c(lo.80:hi.99),
#'          .fns = ~(.x=NA_real_)))
#'
#' # create plot
#' a %>%
#'   union_all(b) %>%
#'   # avallecam::print_inf()
#'   gg_forecast()
#'

gg_forecast <- function(data,axis_x=index) {
  data %>%
    ggplot(aes(x = {{axis_x}}, y = value, color = key)) +
    geom_ribbon(aes(ymin = lo.95, ymax = hi.95),
                fill = "#D5DBFF", linetype = 0) +
    geom_ribbon(aes(ymin = lo.80, ymax = hi.80, fill = key),
                fill = "#596DD5", linetype = 0, alpha = 0.8) +
    geom_line(size = 0.5) +
    geom_point(size = 0.5) +
    #ylim(0,600) +
    # month
    zoo::scale_x_yearmon(n = 12, format = "%b%Y") +
    #scale_x_yearmon(n = 12, format = "%Y / %m") +
    # week
    # scale_x_date(date_breaks="20 week", date_labels = "%Y-%U") + # opcion 1
    # cont
    tidyquant::scale_color_tq() +
    tidyquant::scale_fill_tq() +
    guides(color=guide_legend(override.aes=list(fill=NA))) +
    labs(title = "Casos Esperados y Observados",
         caption = ,
         x = "", y = "N° casos",
         subtitle = "Proyección mediante decomposición STL") +
    tidyquant::theme_tq() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
}

