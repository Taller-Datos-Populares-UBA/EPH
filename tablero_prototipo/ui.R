
library(shiny)

# Define UI for application that draws a histogram
shinyUI(navbarPage("La Economía Popular",
  
  tabPanel("La Economía Popular en el tiempo",
           
           
           # Sidebar with a slider input for number of bins
           sidebarLayout(
             sidebarPanel(
               
               checkboxInput("separar_sexos",
                             "¿Separar por sexo?",
                             value=FALSE),
               
               checkboxInput("separar_zonas",
                             "¿Separar por zonas?",
                             value=FALSE),
               
               sliderInput("slider_años", 
                           label = 'Rango de años',
                           min = 2003, 
                           max = 2022, 
                           value = c(2003, 2022),
                           step = 1,
                           sep = ''),
               
               selectInput(
                 inputId = "variable_zona",
                 label = "Zonas a considerar",
                 choices = c("REGION","AGLOMERADO"),
                 multiple = FALSE,
                 selected = "REGION"),
               
               uiOutput("zonas_posibles"),
               
               selectInput(
                 inputId = "variable_edad",
                 label = "Partición de la edad",
                 choices = c("QUINQUENIO","DECENIO"),
                 multiple = FALSE,
                 selected = "QUINQUENIO"),
               
               uiOutput("edades_posibles")
               
             ),
             
             
             # Gráfico de cantidad de personas participando de la economía popular
             mainPanel(
               plotOutput("cantTrabEP_plot")
             )
           )
  ),
  tabPanel("Pobreza e indigencia en la EP",
           
           
           # Sidebar with a slider input for number of bins
           sidebarLayout(
             sidebarPanel(
               selectInput(
                 inputId = "tasa_tipo",
                 label = "TASA",
                 choices = c("POBREZA","INDIGENCIA"),
                 multiple = FALSE,
                 selected = "POBREZA"),
               
               checkboxInput("separar_sexos_t2",
                             "¿Separar por sexo?",
                             value=FALSE),
               
               sliderInput("slider_años_t2", 
                           label = 'Rango de años',
                           min = 2016, 
                           max = 2022, 
                           value = c(2016, 2022),
                           step = 1,
                           sep = ''),
               
               selectInput(
                 inputId = "variable_zona_t2",
                 label = "Zonas a considerar",
                 choices = c("REGION","AGLOMERADO"),
                 multiple = FALSE,
                 selected = "REGION"),
               
               uiOutput("zonas_posibles_t2"),
               
               selectInput(
                 inputId = "variable_edad_t2",
                 label = "Partición de la edad",
                 choices = c("QUINQUENIO","DECENIO"),
                 multiple = FALSE,
                 selected = "QUINQUENIO"),
               
               uiOutput("edades_posibles_t2")
               
             ),
             
             
             # Gráfico de cantidad de personas participando de la economía popular
             mainPanel(
               plotOutput("pobrezaEP_plot")
             )
           )
  )
))
