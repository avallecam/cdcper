#' @title Crear canal endémico
#'
#' @description Generar estimaciones y detección de brotes.
#'
#' @describeIn cdc_endemic_channel_mutate
#'
#' @param disease disease surveillance datasets
#' @param population estimated population for each adm area
#' @param name free code name of diseases
#' @param method specify the method. "gmean_1sd" is geometric mean w/ 1 standard deviation (default). "gmean_2sd" is gmean w/ 2 sd. "gmean_ci" is gmean w/ 95 percent confidence intervals.
#'
#' @import dplyr
#' @import tidyr
#' @import broom
#' @import ggplot2
#'
#' @return canal endemico, union y grafico
#'
#' @export cdc_endemic_channel_mutate
#' @export cdc_endemic_channel_join
#' @export cdc_endemic_channel_ggplot
#'
#' @examples
#'
#' # not yet
#'
cdc_endemic_channel_mutate <- function(disease,population,name,method="gmean_1sd") {

  edapr_dis <- disease
  pob_long <- population

  parte_1_4 <-
    #Paso 1:  N° de casos  por semana,  7 años
    edapr_dis %>%
    complete(ubigeo,
             semana=full_seq(semana,1),
             year=full_seq(year,1),fill = list(eda=0)) %>%
    #Paso 1.2: unir con el tamaño poblacional por ubigeo-anho
    left_join(pob_long) %>%

    ##Paso 1.3: retirar ubigeos sin poblacion para ningun anho
    ##naniar::miss_var_summary()
    ##filter(is.na(pob)) %>% count(ubigeo,year) %>% count(n)
    #filter(!is.na(pob)) %>%

    #Paso 2: Cálculo de tasas y suma 1 (facilitar transformación en 0 casos)
    mutate(tasa=eda/pob*100000+1,
           #Paso 3: Transformación logarítmica de las tasas
           log_tasa=log(tasa)) %>%
    #Paso 3: agrupar
    group_by(ubigeo, semana)

  if(method=="gmean_1sd"){
    parte_final <-
      parte_1_4 %>%
      # Paso 4: Cálculo de medias, 1*DE y delimitacion del 68.26% de valores del log_tasas
      summarise(media_l=mean(log_tasa), sd_l=sd(log_tasa)) %>%
      mutate(lo_95_l=media_l-(1*sd_l), hi_95_l=media_l+(1*sd_l)) %>%
      ungroup()
  }

  if(method=="gmean_2sd"){
    parte_final <-
      parte_1_4 %>%
      # Paso 4: Cálculo de medias, 2*DE y delimitacion del 95.48% de valores del log_tasas
      summarise(media_l=mean(log_tasa), sd_l=sd(log_tasa)) %>%
      mutate(lo_95_l=media_l-(2*sd_l), hi_95_l=media_l+(2*sd_l)) %>%
      ungroup()
  }

  if(method=="gmean_ci"){
    # Paso 4: Cálculo de media e IC95% de log_tasas
    parte_final <-
      parte_1_4 %>%
      nest() %>%
      #ungroup() %>% unnest(cols = c(data))
      # #ISSUE tagged -> non-vairability stops t.test
      # count(log_tasa,sort = T) %>%
      # ungroup() %>% filter(n==7) %>%
      # count(ubigeo,sort = T)
      # count(log_tasa)
      # filter(is.na(log_tasa)) %>% count(ubigeo)
      # #SOLUTION: retirar semana con puro cero o NA
      # #PLAN 01
      mutate(sum_tasa=map_dbl(.x = data,.f = ~sum(.$log_tasa),na.rm=TRUE)) %>% #arrange(sum_tasa)
      filter(sum_tasa>0) %>%
      # mutate(t_test=map(.x = data,.f = ~t.test(.$log_tasa))) %>%
      # mutate(tidied=map(t_test,broom::tidy)) %>%
      # unnest(cols = c(tidied))
      # NOT WORKED: non-vairability  maintained after filter of values higher than 0
      #PLAN 02
      mutate(t_test=map(.x = data,.f = ~lm(log_tasa ~ 1,data=.x))) %>%
        mutate(tidied=map(t_test,broom::tidy),
               tidy_ci=map(t_test,broom::confint_tidy)) %>%
        unnest(cols = c(tidied,tidy_ci)) %>%
        rename(media_l=estimate,
               lo_95_l=conf.low,
               hi_95_l=conf.high) %>%
        ungroup()
  }


  parte_final %>%
    # Paso 5: Transformación a unidades originales (de log_tasas a tasas), menos 1 (agregado arriba)
    mutate(media_t= exp(media_l)-1, #sd_t= exp(sd_l)-1,
           lo_95_t=exp(lo_95_l)-1, hi_95_t=exp(hi_95_l)-1) %>%
    # Paso 6: Transformación a de tasas a casos (esperados)
    left_join(pob_long %>%
                group_by(ubigeo) %>%
                summarise(media_pob=mean(pob))%>%
                ungroup()) %>%
    mutate(media=media_t*media_pob/100000,
           lo_95=lo_95_t*media_pob/100000,
           hi_95=hi_95_t*media_pob/100000) %>%
    # seleccionado solo las variables a usar
    select(ubigeo, semana, media, lo_95, hi_95) %>%
    mutate(key=name)%>%
    mutate(ubigeo=as.factor(ubigeo)) %>%
    return()

}

#' @describeIn cdc_endemic_channel_mutate unir resultado con vigilancia actual
#' @inheritParams cdc_endemic_channel_mutate
#' @param disease_channel salida de cdc_*_mutate
#' @param disease_now nueva base de vigilancia

cdc_endemic_channel_join <- function(disease_channel,disease_now) {
  #unir con canal
  full_join(disease_channel,disease_now) %>%
    left_join(
      ubigeo %>% select(ubigeo,dist:dpto)
    ) %>%
    mutate_at(.vars = vars(value,lo_95,hi_95,media),replace_na,0)
}

#' @describeIn cdc_endemic_channel_mutate crear gráfico
#' @inheritParams cdc_endemic_channel_mutate
#' @param joined_channel base de datos unida

cdc_endemic_channel_ggplot <- function(joined_channel) {
  #unir con canal
  # plot_name <- joined_channel %>%
  #   select(dist,prov,dpto) %>%
  #   distinct() %>% as.character() %>% paste(collapse = ", ")
  joined_channel %>%
    mutate(new= max(c(value,hi_95),na.rm = T),
           #plot_name= str_c(dist,"\n",prov,"\n",dpto)
    ) %>%
    ggplot(aes(x = semana, y = value#, fill=key
               )) +
    geom_area(aes(x=semana, y=new), fill = "#981000", alpha=0.6, stat="identity")+
    geom_area(aes(x=semana, y=hi_95), fill = "#fee76a", stat="identity")+
    geom_area(aes(x=semana, y=media), fill = "#3e9e39",  stat="identity")+
    geom_area(aes(x=semana, y=lo_95), fill = "white", stat="identity") +
    geom_line(size = 0.8) +
    xlab("semanas") + ylab("N° de casos") #+
  #labs(title = paste0("Distrito de ",plot_name))
}
