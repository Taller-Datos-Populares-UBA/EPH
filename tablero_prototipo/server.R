# Función server.
# Objeto output contiene:
#' @param cantTrabEP_plot Plot de la cantidad de trabajadorxs participando de la economía popular.

# Objeto input contiene:
#' @param separar_genero Si es verdadero, se separan las líneas de los datos en base al sexo de las personas.
#' @param regiones Las regiones a considerar cuando graficamos. En caso de que sea NULL, consideramos todas las regiones.
#' @param separar_zonas Si es verdadero, consideramos una curva distinta para cada una de las regiones consideradas.


shinyServer(function(input, output) {
  #### Primer panel
  output$zonas_posibles <- renderUI({
    opciones <- individual_03.hoy %>% 
      select_at(input$variable_zona) %>%
      unique %>% unlist %>% as.character %>% setdiff('N/A')
    
    selectInput(
      inputId = "zonas",
      label = "Zonas consideradas",
      choices = opciones,
      multiple = TRUE,
      selected = c() )
  })
  
  
  output$edades_posibles <- renderUI({
    opciones <- individual_03.hoy %>% 
      select_at(paste('EDAD',input$variable_edad,sep='_')) %>%
      unique %>% unlist %>% as.character %>% discard(is.na)
    
    selectInput(
      inputId = "edades",
      label = "Edades consideradas",
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
    
    edades <- unique(input$edades)
    if( length(edades) == 0 ) 
      edades <- individual_03.hoy %>% 
        select_at(paste('EDAD',input$variable_edad,sep='_')) %>%
        unique %>% unlist 
    
    aes_plot <- genera_aes_cantTrabEP_plot(input)
    grouping_vars <- genera_grouping_vars_cantTrabEP_plot(input)
  
    ocupa_labels <- case_when(
      input$ocupaciones == 'RESTO_CUENTAPROPISTAS' ~ 'RESTO CP',
      input$ocupaciones == 'ASALARIADOS_REGISTRADOS' ~ 'Asal R',
      input$ocupaciones == 'ASALARIADOS_NOREGISTRADOS' ~ 'Asal NR',
      input$ocupaciones == 'PATRONES' ~ 'Pat',
    )
    names(ocupa_labels) <- input$ocupaciones
    ocupa_labels <- c(ocupa_labels, 'ECONOMIA_POPULAR' = 'EP')

    individual_03.hoy %>% ungroup() %>%
      filter(
        YEAR > input$slider_años[1],
        YEAR < input$slider_años[2],
        across(input$variable_zona, ~.x %in% zonas),
        across(paste('EDAD',input$variable_edad,sep='_'), ~.x %in% edades)
      ) %>%
      filter(SEXO != 'N/A') %>%
      group_by_at(grouping_vars) %>%
      summarise(ECONOMIA_POPULAR = sum(ECONOMIA_POPULAR),
                RESTO_CUENTAPROPISTAS = sum(RESTO_CUENTAPROPISTAS),
                ASALARIADOS_REGISTRADOS = sum(ASALARIADOS_REGISTRADOS),
                ASALARIADOS_NOREGISTRADOS = sum(ASALARIADOS_NOREGISTRADOS),
                PATRONES = sum(PATRONES)) %>% 
      pivot_longer(cols = c(
        'ECONOMIA_POPULAR',
        'RESTO_CUENTAPROPISTAS',
        'ASALARIADOS_REGISTRADOS',
        'ASALARIADOS_NOREGISTRADOS',
        'PATRONES'),
        names_to = 'OCUPACIONES',
        values_to = 'PERSONAS') %>% 
      ungroup() %>%
      filter(OCUPACIONES %in% c('ECONOMIA_POPULAR',input$ocupaciones)) %>%
      mutate(FECHA = as.Date(paste(YEAR,3*TRIMESTER,1,sep='-'))) %>% 
      ggplot(aes_plot) +
      geom_point() +
      geom_line() +
      ylab('Economía Popular  [Millones de personas]') +
      theme_light() +
      theme(axis.title = element_text(size=15),
            axis.text = element_text(size=12)) +
      scale_linetype(labels = function(x) ocupa_labels[x]) +
      scale_shape(name = 'GÉNERO')
    

      

    })
  
  ##### SEGUNDO PANEL
  
  output$zonas_posibles_t2 <- renderUI({
    opciones <- individual_03.hoy %>% 
      select_at(input$variable_zona_t2) %>%
      unique %>% unlist %>% as.character %>% setdiff('N/A')
    
    selectInput(
      inputId = "zonas_t2",
      label = "Zonas consideradas",
      choices = opciones,
      multiple = TRUE,
      selected = c() )
  })
  
  
  output$edades_posibles_t2 <- renderUI({
    opciones <- individual_03.hoy %>% 
      select_at(paste('EDAD',input$variable_edad_t2,sep='_')) %>%
      unique %>% unlist %>% as.character %>% discard(is.na)
    
    selectInput(
      inputId = "edades_t2",
      label = "Edades consideradas",
      choices = opciones,
      multiple = TRUE,
      selected = c() )
  })
  
  output$pobrezaEP_plot <- renderPlot({
    # La lógica es que construimos un vector grouping_vars y otro aes_plot que nos permita indicar qué variables usar para agrupar (a la hora de construir el dataset resumido, grouping vars) y qué graficar (aes_plot)
    
    zonas <- unique(input$zonas_t2)
    if( length(zonas) == 0 ) 
      zonas <- individual_03.hoy %>% 
        select_at(input$variable_zona_t2) %>%
        unique %>% unlist 
    
    edades <- unique(input$edades_t2)
    if( length(edades) == 0 ) 
      edades <- individual_03.hoy %>% 
      select_at(paste('EDAD',input$variable_edad_t2,sep='_')) %>%
      unique %>% unlist 
    
    if(input$tasa_tipo == 'POBREZA'){
      situaciones <- c('pobre','indigente')
    }else{
      situaciones <- c('indigente')
    }
    
    
    aes_plot <- genera_aes_pobrezaEP_plot(input)
    grouping_vars <- genera_grouping_vars_pobrezaEP_plot(input)
    
    individual_03.hoy %>%
      filter(
        YEAR > input$slider_años_t2[1],
        YEAR < input$slider_años_t2[2],
        across(input$variable_zona_t2, ~.x %in% zonas),
        across(paste('EDAD',input$variable_edad_t2,sep='_'), ~.x %in% edades)
      ) %>%
      group_by_at(grouping_vars) %>%
      summarise(
        tasa_EP = sum(ECONOMIA_POPULAR[situacion %in% situaciones],na.rm=TRUE)/sum(ECONOMIA_POPULAR,na.rm=TRUE),
        tasa_OCU_NEP = sum(OCUPADES_NO_EP[situacion %in% situaciones],na.rm=TRUE)/sum(OCUPADES_NO_EP,na.rm=TRUE),
        tasa_OCU = sum((ECONOMIA_POPULAR + OCUPADES_NO_EP)[situacion %in% situaciones],na.rm=TRUE)/sum((ECONOMIA_POPULAR + OCUPADES_NO_EP),na.rm=TRUE)
      ) %>%
      mutate(FECHA = as.Date(paste(YEAR,4*TRIMESTER,'1',sep='-'))) %>%
      pivot_longer(cols = c(tasa_EP,
                            tasa_OCU_NEP,
                            tasa_OCU),
                   names_to = 'tasa_tipo',
                   values_to = 'tasa') %>%
      drop_na() %>% 
      filter(is.element(tasa_tipo,c(input$ocupaciones_t2,'tasa_EP'))) %>%
      mutate(
        tasa_tipo = case_when(
          str_detect(tasa_tipo,'OCU_NEP') ~ 'OCUPADES NO EP',
          str_detect(tasa_tipo,'EP') ~ 'EP',
          TRUE ~ 'POB. TOT.')
      ) %>%
      ggplot(aes_plot) +
      geom_pointline() +
      scale_color_discrete(name = 'Población')+
      ylab('TASAS') +
      theme_light() +
      theme(axis.title = element_text(size=15),
            axis.text = element_text(size=12)) +
      theme(legend.position = c(.3,.8)) +
      scale_shape(name = 'GÉNERO')
      
  })
})
