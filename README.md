# cdcper

Paquete en R para automatizar la generación y exploración de reportes, tablas, gráficos y bases de datos.

## main functionalities

- `cdc_mortality`: total de muertes y tasas de mortalidad por combinación de covariables (*dar nombre: cdc_age_adjusted_rates)
- `cdc_denominator`: generar denominador por combinación de rangos de edad o región administrativa (*bases pendientes)
- `cdc_mortality_to_wide`: exportar base en formato extendido (wide)
- `cdc_egresos_summary`: total de egresos por combinación de covariables
- `cdc_egresos_by_year`: exportar base en formato extendido (wide)
- `cdc_piramide`: generar pirámide poblacional (*adaptarlo como un geom_piramid para usar con ggplot)
- `clean_eda_sp`: limpia base consolidada de EDA (vigilancia semanal)
- `clean_ira_sp`: limpia base consolidada de IRA (vigilancia semanal)
- `cdc_casos_tiempo`: resume casos por semana de EDA o IRA
- `cdc_casos_nacional`: resume casos de EDA o IRA en el presente año
- `cdc_choropleth`: mapa coropletico estático
- `cdc_choropleth_facet`: mapa coropletico estático segmentado por facets
- `cdc_morantest`: Ejecuta un test de Moran para poner a prueba la hipotesis de autocorrelación espacial
- `cdc_rsatscan_clean`: Limpia la salida del rsatscan para la identificación de cluster espacial
- `cdc_heatmap`: mapa de calor de datos puntuales
- `cdc_point_heatmap`: mapa de calor de datos puntuales + visualización de puntos 
- `cdc_point_heat_mark`: mapa de datos puntuales con dos valores a graficar (e.g. casos confirmados y PCR positivos para agente patógeno)



## To-Do

( ) issue: _no visible global function definition_ estas usando muchas nombres de columna no declarados

- update mortality functions con names de argumentos ...

- agregar base de denominadores
- agregar base de estandarizacion
- agregar shapefiles

* egresos
- qué pasa con la base de egresos?
> Error: 'egresos' is not an exported object from 'namespace:cdcper'
> Ejecuci�n interrumpida
> Exited with status 1.
- respuesta: se debe usar el mismo nombre de archivo y nombre original al crear el objeto Rda
