# Función server.
# Objeto output contiene:
#' @param cantTrabEP_plot Plot de la cantidad de trabajadorxs participando de la economía popular.

# Objeto input contiene:
#' @param separar_sexos Si es verdadero, se separan las líneas de los datos en base al sexo de las personas.
#' @param regiones Las regiones a considerar cuando graficamos. En caso de que sea NULL, consideramos todas las regiones.
#' @param separar_zonas Si es verdadero, consideramos una curva distinta para cada una de las regiones consideradas.


shinyServer(function(input, output) {
  output$zonas_posibles <- renderUI({
    opciones <- individual_03.hoy %>% 
      select_at(input$variable_zona) %>%
      unique %>% unlist %>% as.character
    
    selectInput(
      inputId = "zonas",
      label = "Zonas consideradas",
      choices = opciones,
      multiple = TRUE,
      selected = c() )
  })
  
  output$cantTrabEP_plot <- renderPlot({
    # La lógica es que construimos un vector grouping_vars y otro aes_plot que nos permita indicar qué variables usar para agrupar (a la hora de construir el dataset resumido, grouping vars) y qué graficar (aes_plot)
  
    zonas <- unique(input$zonas)
    if( length(zonas) == 0 ) 
      zonas <- individual_03.hoy %>% 
        select_at(input$variable_zona) %>%
        unique %>% unlist
    
    aes_plot <- genera_aes_cantTrabEP_plot(input)
    grouping_vars <- genera_grouping_vars_cantTrabEP_plot(input)
    
    individual_03.hoy %>% ungroup() %>%
      filter(YEAR > input$slider_años[1],
             YEAR < input$slider_años[2],
             across(input$variable_zona, ~.x %in% zonas)) %>%
      group_by_at(grouping_vars) %>%
      summarise(ECON_NUCLEO = sum(ECON_NUCLEO)) %>% 
      mutate('FECHA' = as.Date(paste(YEAR,3*TRIMESTER,1,sep='-'))) %>% 
      ungroup() %>% 
      ggplot(aes_plot) +
      geom_pointline(size = 2) +
      ylab('ECONOMIA POPULAR NUCLEO [Millones de personas]')

    })

})
