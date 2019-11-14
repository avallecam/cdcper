#' Realiza gráficos resumen de tendencia
#'
#' Crea change, slope, heat and tree -maps.
#'
#' @describeIn cdc_changeplot create various types of plots for trend comparison
#'
#' @param data input dataframe
#' @param time nombre de la vairable año
#' @param name nombre de la variable con las enfermedades
#' @param value valor de la unidad a graficar: e.g. tasa ajsutada de mortalidad
#' @param ... conjunto de covariables a usar para estratificar los graficos finales: e.g. sexo o edad
#'
#' @import dplyr
#' @import rlang
#' @import tidyr
#' @import ggplot2
#' @import magrittr
#' @import CGPfunctions
#' @import treemapify
#'
#' @return gráficos para mortalidad y morbilidad.
#'
#' @examples
#'
#' # ejemplo
#' # falta
#'
#'@export cdc_changeplot
#'@export cdc_slopegraph
#'@export cdc_region_heatmap
#'@export cdc_treemap
#'
cdc_changeplot <- function(data,time,name,value,...,label="change") {

  mort_per1 <- data
  year <- enquo(time)
  descrip_grupo110 <- enquo(name)
  dar_cmil <- enquo(value)

  mort_change <- mort_per1 %>%
    select(!!year,!!descrip_grupo110,!!dar_cmil,...) %>%
    pivot_wider(names_from = !!year,values_from = !!dar_cmil) %>%
    magrittr::set_colnames(c(colnames(.) %>%
                               magrittr::extract(-c(length(.),(length(.)-1))),
                             "past","now")) %>%
    rename("nuname"=!!descrip_grupo110) %>%
    clean_names() %>%
    replace_na(list(now = 0, past = 0)) %>% mutate(change=now-past)

  #mort_change %>% naniar::miss_var_summary()

  if (label=="change") {
    mort_change_lab <- mort_change %>%
      mutate(nuname=case_when(
        str_detect(nuname,"respiratorias agudas") ~ "IRA",
        str_detect(nuname,"cerebrovasculares") ~ "ECV",
        TRUE ~ as.character(nuname)
      )
      ) %>%
      group_by(...) %>%
      arrange(...,desc(abs(change))) %>%
      slice(1:10) %>%
      #top_n(abs(change),5) %>%
      # filter(
      #   abs(change)>10 |
      #     now>20
      # ) %>%
      #arrange(desc(change)) %>%
      ungroup()

  } else if (label=="now") {
    mort_change_lab <- mort_change %>%
      mutate(nuname=case_when(
        str_detect(nuname,"respiratorias agudas") ~ "IRA",
        str_detect(nuname,"cerebrovasculares") ~ "ECV",
        TRUE ~ as.character(nuname)
      )
      ) %>%
      group_by(...) %>%
      arrange(...,desc(abs(now))) %>%
      slice(1:10) %>%
      #top_n(abs(change),5) %>%
      # filter(
      #   abs(change)>10 |
      #     now>20
      # ) %>%
      #arrange(desc(change)) %>%
      ungroup()

  }



  mort_change_plot <- mort_change %>%
    #filter(str_detect(!!descrip_grupo110,"isquemicas"))
    ggplot(aes(now,change)) +
    geom_point(aes(alpha=abs(change))) +
    geom_hline(yintercept=0,colour="red",lty=2) +
    geom_text_repel(data = mort_change_lab,
                    aes(label=nuname,
                        #size=x2016
                    ),
                    # vjust = 1,
                    hjust = 0,
                    direction="y",
                    show.legend = F) +
    scale_alpha_continuous(guide = "none")

  list(data=mort_change,
       plot=mort_change_plot) %>% return()
}
#' @describeIn cdc_changeplot crear slopegraph
#' @inheritParams cdc_changeplot
#' @param ranking cambiar de grafico cuantitativo a uno con ranking
cdc_slopegraph <- function(data,time,name,value,...,ranking=FALSE) {

  mort_per1 <- data
  year <- enquo(time)
  descrip_grupo110 <- enquo(name)
  dar_cmil <- enquo(value)

  # _ _top_n

  mort_top_n <- mort_per1 %>%
    group_by(...) %>%
    #top 10 al último año
    filter(!!year==max(!!year)) %>%
    top_n(n = 10,wt = !!dar_cmil) %>%
    ungroup() %>%
    select(...,!!descrip_grupo110)
  #pull(!!descrip_grupo110)

  # mort_top_n2 <- mort_per1 %>%
  #   #top 10 al último año
  #   filter(!!year==max(!!year)) %>%
  #   top_n(n = 15,wt = !!dar_cmil) %>%
  #   pull(!!descrip_grupo110)

  # _ _plot

  mort_per1_p <- mort_per1 %>%
    #filtrar top de enfermedades
    #filter(!!descrip_grupo110 %in% mort_top_n) %>%
    inner_join(mort_top_n) %>%
    #formatos para graficar
    mutate(!!year:=str_replace(!!year,"(.+)","Año \\1"),
           qvalue=round(!!dar_cmil),
           cvalue=!!descrip_grupo110) %>%
    #identificar grupos de enfermedades
    mutate(col=case_when(
      str_detect(!!descrip_grupo110,"Diabetes") ~ "no_comu",
      str_detect(descrip_grupo10,"Infecc|nutri|perinat") ~ "comu,mate,neonat,nutri",
      str_detect(descrip_grupo10,"Lesiones") ~ "lesi",
      str_detect(descrip_grupo10,"neo|sistema|aparat") ~ "no_comu"
    ),
    col_t=case_when(
      str_detect(col,"no_comu") ~ "blue",
      str_detect(col,"comu") ~ "red",
      str_detect(col,"lesi") ~ "green"
    ),
    cvalue=fct_recode(cvalue,
                      "Enfermedades crónicas del hígado"="Cirrosis y ciertas otras enfermedades crónicas del hígado")
    ) %>%
    arrange(!!year,desc(qvalue)) %>%
    group_by(...,!!year) %>%
    mutate(rank = row_number(),
           anti_rank = -rank) %>%
    #top_n(n = 10,wt = qvalue) %>%
    ungroup()

  #levels(mort_per1_p$!!descrip_grupo110)
  #levels(mort_per1_p$disease_plot)

  #mort_per1_p_col <-
  # mort_per1_p %>%
  #   select(!!descrip_grupo110,col,col_t) %>%
  #   distinct() %>%
  #   mutate(line_col=str_c("'",!!descrip_grupo110,"'='",col_t,"'")) %>%
  #   pull(line_col) #%>%
  #paste(collapse = ",")

  if (ranking==FALSE) {
    a <- mort_per1_p %>%
      #graficar
      newggslopegraph(
        Times = year,
        Measurement = qvalue,
        #Measurement = anti_rank,
        #Grouping = disease_plot,
        Grouping = cvalue,
        # Title = "Perú",
        # SubTitle = "Todas las edades. Todos los sexos.\nTasas* de mortalidad por 100 mil hab.",
        # Caption = "*Estandarizadas por edad\n\nElaboración: MINSA-CDC (Julio, 2019)",
        #Caption = "MINSA - CDC Centro Nacional de Epidemiología, Prevención y Control de Enfermedades",
        #LineColor = c("#ff6262",rep("#b1b1ff",3),"blue","#b1b1ff","#3e9e39","blue","#b1b1ff","red"),
        DataTextSize = 2,
        DataLabelFillColor = "white",
        DataLabelPadding = .1,
        DataLabelLineSize = .5,
        WiderLabels = TRUE
      )

  } else {

    a <- mort_per1_p %>%
      #graficar
      newggslopegraph(
        Times = year,
        #Measurement = dar_cmil,
        Measurement = anti_rank,
        #Grouping = disease_plot,
        Grouping = cvalue,
        # Title = "Perú",
        # SubTitle = "Todas las edades. Todos los sexos.\nTasas* de mortalidad por 100 mil hab.",
        # Caption = "*Estandarizadas por edad\n\nElaboración: MINSA-CDC (Julio, 2019)",
        #Caption = "MINSA - CDC Centro Nacional de Epidemiología, Prevención y Control de Enfermedades",
        #LineColor = c("#ff6262",rep("#b1b1ff",3),"blue","#b1b1ff","#3e9e39","blue","#b1b1ff","red"),
        DataTextSize = 2,
        DataLabelFillColor = "white",
        DataLabelPadding = .1,
        DataLabelLineSize = .5,
        WiderLabels = TRUE
      )

  }

  list(data=mort_per1_p,plot=a) %>% return()

}
#' @describeIn cdc_changeplot crear heatmap en un grilla
#' @inheritParams cdc_changeplot
cdc_region_heatmap <- function(data,time,name,value,...) {

  mort_per2 <- data
  year <- enquo(time)
  descrip_grupo110 <- enquo(name)
  dar_cmil <- enquo(value)

  heattile_data <- mort_per2 %>%
    #filtrar top de enfermedades
    filter(#!!descrip_grupo110 %in% mort_top_n2,
      !!year==max(!!year)) %>%
    arrange(departamento,desc(!!dar_cmil)) %>%
    group_by(...,departamento) %>%
    #top_n(n = 10,wt = !!dar_cmil) %>%
    mutate(rank = row_number()) %>%
    top_n(n = 10,wt = !!dar_cmil) %>%
    ungroup() %>%
    #ordenar enfermedades y departamentos por suma tasas!
    mutate(
      # !!descrip_grupo110:=reorder_within(!!descrip_grupo110, -!!dar_cmil, ...),
      # departamento=reorder_within(departamento, -!!dar_cmil, ...)
      !!descrip_grupo110:=fct_reorder(!!descrip_grupo110,-!!dar_cmil,mean),
      departamento=fct_reorder(departamento,-!!dar_cmil,mean)
    ) %>%
    mutate(rank=if_else(!!dar_cmil==0,NA_integer_,rank)) %>%
    filter(!is.na(rank))

  heattile_plot <- heattile_data %>%
    ggplot(aes(departamento,!!descrip_grupo110,
               fill=!!dar_cmil,
               #fill=rank,
               label=rank
    )
    ) +
    geom_tile() +
    geom_text(size=2) +
    scale_fill_gradient(#"Tasa*\nx 100 mil\nhab.",
      trans = "log",
      breaks = c(1,10,25,50,100,200), #c(1,5,10),
      high = "#ff0000",low = "#ffff76"#,mid="yellow",
      #midpoint = -5,#limits=c(1,10) ,#trans = "reverse"
    ) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1),
          #panel.grid.major.y = element_blank()
          #panel.grid = element_blank()
    )

  list(data=heattile_data,plot=heattile_plot) %>% return()

}
#' @describeIn cdc_changeplot crear treemap con dos niveles
#' @inheritParams cdc_changeplot
#' @param subname subcategoria de nombres de enfermedades
cdc_treemap <- function(data,time,name,subname,value,...) {

  mort_per1 <- data
  year <- enquo(time)
  descrip_grupo10 <- enquo(name)
  descrip_grupo110 <- enquo(subname)
  dar_cmil <- enquo(value)

  mort_per1_t1 <- mort_per1 %>%
    select(...,!!year,!!descrip_grupo10,!!descrip_grupo110,!!dar_cmil) %>%
    #mutate(!!year=str_replace(!!year,"(.+)","ano_\\1")) %>%
    pivot_wider(names_from = !!year,values_from = !!dar_cmil) %>%
    magrittr::set_colnames(c(colnames(.) %>%
                               magrittr::extract(-c(length(.),(length(.)-1))),
                             "past","now")) %>%
    rename("nusubname"=!!descrip_grupo110,
           "nuname"=!!descrip_grupo10) %>%
    clean_names() %>%
    #spread(!!year,!!dar_cmil) %>%
    filter(!is.na(past)) %>%
    mutate(delta_n=now-past,
           #delta_p=(100*ano_2016/ano_2000)-100
    ) %>% #skimr::skim()
    mutate(
      col=case_when(
        str_detect(nusubname,"Diabetes") ~ "no_comu",
        str_detect(nuname,"Infecc|nutri|perinat") ~ "comu,mate,neonat,nutri",
        str_detect(nuname,"Lesiones") ~ "lesi",
        str_detect(nuname,"neo|sistema|aparat") ~ "no_comu",
        TRUE ~ "other"
      ),
      palette=case_when(
        str_detect(col,"no_comu") ~ "Teal",
        str_detect(col,"comu") ~ "Red-Yellow",
        str_detect(col,"lesi") ~ "Greens",
        str_detect(col,"other") ~ "Purples"
      ),
      fill_value=delta_n
    ) %>% #print(n=Inf)
    arrange(palette) #%>% print(n=Inf)

  palette <- mort_per1_t1 %>% pull(palette)

  mort_per1_t2 <- mort_per1_t1 %>%
    mutate(
      min_fill = min(fill_value,na.rm = T),
      the_fill = fill_value+abs(min_fill)+1,
      max_fill = max(the_fill,na.rm = T),
      ratio_fill = the_fill/max_fill
    ) %>%
    #create palette
    group_by(palette) %>%
    mutate(
      #the_fill_min = min(the_fill,na.rm = T),
      #color = gradient_n_pal(sequential_hcl(6, palette = palette)[1:5])(ano_2016/ano_2000)
      color = gradient_n_pal(sequential_hcl(6, palette = palette)[1:5])(1 - ratio_fill)
      #color = gradient_n_pal(sequential_hcl(6, palette = palette)[1:5])(the_fill/the_fill_min)
    ) #%>% print(n=Inf)

  #mort_per1_t2 %>% print(n=Inf)

  mort_per1_t2_plot <- mort_per1_t2 %>%
    mutate(nusubname=str_replace_all(nusubname," ","\\\n")) %>%
    ggplot(aes(area = now,
               #fill = delta_p,
               #fill = delta_n,
               fill = color,
               label = nusubname,
               subgroup = col
    )) +
    geom_treemap() +
    scale_fill_identity() +
    geom_treemap_text(colour="white") +
    geom_treemap_subgroup_border(colour="white")

  list(data=mort_per1_t2,plot=mort_per1_t2_plot) %>% return()
}
