# cdcper

Paquete en R para automatizar la generación y exploración de reportes, tablas, gráficos y bases de datos.

## main functionalities

### General usefull functions

- `cdc_edades_peru`: crea categorias de edades comunmente usadas
- `read_reunis_total`: ordena base de reunis para brindar la poblacion del año en curso
- `read_reunis_edad`: brinda la población estratificada por sexo y etapas de vida
- `read_inei_poblacion`: lee archivos de inei de población (revisar ejemplo)

### Prioritization functions for Data Mining (`cdcper_asis`)

- `cdc_pareto_lista`: calcula porcentaje de aporte individual y aporte acumulado de elementos en una lista a priorizar 
- `cdc_carga_coalesce`: permite unir (logical connector OR) los listas priorizadas y generar una lista concenso.


### Visualization functions (`cdcper_asis`)

- `cdc_dotwhiskers_plot`: genera un grafico punto-bigotes con la estimación puntual del promedio e intervalo de confianza de una variable continua por niveles de una variable categórica.

## To-Do

( ) issue: _no visible global function definition_ estas usando muchas nombres de columna no declarados
