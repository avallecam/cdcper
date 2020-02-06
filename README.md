# cdcper

Paquete en R para automatizar la generación y exploración de reportes, tablas, gráficos y bases de datos.

## main functionalities

### ASIS: national-department level (`cdcper_asis`)

- `cdc_mortality`: total de muertes y tasas de mortalidad por combinación de covariables (*dar nombre: cdc_age_adjusted_rates)
- `cdc_denominator`: generar denominador por combinación de rangos de edad o región administrativa (*bases pendientes)
- `cdc_mortality_to_wide`: exportar base en formato extendido (wide)
- `cdc_egresos_summary`: total de egresos por combinación de covariables
- `cdc_egresos_by_year`: exportar base en formato extendido (wide)
- `his_create`: limpiar bases de HIS MIS
- `his_extract`: unir bases de HIS MIS por departamento

### ASIS: department-district level (`cdcper_asis`)

- `his_extraer`: funcion para llamar base de datos de morbilidad por Area administrativa
- `cdc_pareto`: funcion para crear diagrama de Pareto
- `cdc_mapa_morb`: funcion para crear mapa coropletico

### Climate data (`cdc_met`)

- `get_ids`: función que contiene la ubicación de la información
- `get_data_gri`: función que realiza la descarga de información
- `var_fun`: función para realizar suma o promedio dependiendo de la variable asignada

### Surveillance data management (`cdcper_sitrep`)

- `clean_eda_sp`: limpia base consolidada de EDA (vigilancia semanal)
- `clean_ira_sp`: limpia base consolidada de IRA (vigilancia semanal)
- `cdc_casos_tiempo`: resume casos por semana de EDA o IRA
- `cdc_casos_nacional`: resume casos de EDA o IRA en el presente año

### Endemic channels (`cdcper_echannel`)

- `cdc_endemic_channel_mutate`: crea canal endémico con un número de años previos
- `cdc_endemic_channel_join`: une canal endémico con la información de la vigilancia actual
- `cdc_endemic_channel_ggplot`: crea el gráfico de canal endémico con las zonas esperadas y datos observados

### Prioritization functions for Data Mining (`cdcper_asis`)

- `cdc_pareto_lista`: calcula porcentaje de aporte individual y aporte acumulado de elementos en una lista a priorizar 
- `cdc_carga_coalesce`: permite unir (logical connector OR) los listas priorizadas y generar una lista concenso.

### Spatial analysis (`cdcper_spatial`)

- `cdc_choropleth`: mapa coropletico estático
- `cdc_choropleth_facet`: mapa coropletico estático segmentado por facets
- `cdc_morantest`: Ejecuta un test de Moran para poner a prueba la hipotesis de autocorrelación espacial
- `cdc_rsatscan_clean`: Limpia la salida del rsatscan para la identificación de cluster espacial
- `cdc_geo_heatmap`: mapa de calor de datos puntuales
- `cdc_geo_point_heatmap`: mapa de calor de datos puntuales + visualización de puntos 
- `cdc_geo_point_heat_mark`: mapa de datos puntuales con dos valores a graficar (e.g. casos confirmados y PCR positivos para agente patógeno)

### Visualization functions (`cdcper_asis`)

- `cdc_piramide`: generar pirámide poblacional (*adaptarlo como un geom_piramid para usar con ggplot)
- `cdc_changeplot`: comparación del cambio (Y) vs el estado actual (X) entre dos años mediante un grafico de puntos.
- `cdc_slopegraph`: comparacion de variable continua y rankeada entre dos años mediante rectas con pendiente.
- `cdc_treemap`: comparacion del cambio de una variable continua entre dos años proporcion (area), cambio (degrade del color), categorizada por una variable politomica.
- `cdc_region_heatmap_data`: genera insumo para la creacion de un heatmap con enfoque regional.
- `cdc_region_heatmap_ggplot`: genera el heatmap con el producto de cdc_region_heatmap_data.
- `cdc_dotwhiskers_plot`: genera un grafico punto-bigotes con la estimación puntual del promedio e intervalo de confianza de una variable continua por niveles de una variable categórica.

## To-Do

( ) issue: _no visible global function definition_ estas usando muchas nombres de columna no declarados
