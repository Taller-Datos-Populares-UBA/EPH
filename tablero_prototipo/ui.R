
library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("Trabajadorxs de la Economía Popular"),

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
))
