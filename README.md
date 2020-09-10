
<!-- README.md is generated from README.Rmd. Please edit that file -->

# cdcper

<!-- badges: start -->

<!-- badges: end -->

El objetivo de `cdcper` es agilizar la creación de variables, gráficos e
importación bases de datos de relevancia para el CDC Perú.

## Installation

<!-- You can install the released version of cdcper from [CRAN](https://CRAN.R-project.org) with: install.packages("cdcper") -->

``` r
devtools::install_github("avallecam/cdcper")
```

## Main functionalities

### General usefull functions

  - `cdc_edades_peru`: crea categorias de edades comunmente usadas
  - `read_reunis_total`: ordena base de reunis para brindar la poblacion
    del año en curso
  - `read_reunis_edad`: brinda la población estratificada por sexo y
    etapas de vida
  - `read_inei_poblacion`: lee archivos de inei de población (revisar
    ejemplo)

### Prioritization functions for Data Mining

  - `cdc_pareto_lista`: calcula porcentaje de aporte individual y aporte
    acumulado de elementos en una lista a priorizar
  - `cdc_carga_coalesce`: permite unir (logical connector OR) los listas
    priorizadas y generar una lista concenso.

### Visualization functions

  - `cdc_dotwhiskers_plot`: genera un grafico punto-bigotes con la
    estimación puntual del promedio e intervalo de confianza de una
    variable continua por niveles de una variable categórica.

### Data sets availability

  - `denominadores_departamento_2000_2020_peru` department level
    population estimates 2000-2020
  - `edad_estandarizada_who` estandardize factors to estandardize raw
    rates by age

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(cdcper)
## basic example code
```

## To-Do

( ) issue: *no visible global function definition* estas usando muchas
nombres de columna no declarados
