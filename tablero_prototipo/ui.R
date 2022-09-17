load('../base_ep.RData')

#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("Prototipo de tablero"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            checkboxInput("group_sexo",
                        "¿Agrupar por sexo?",
                        value=FALSE),
            checkboxInput("group_region",
                          "¿Agrupar por region?",
                          value=FALSE),
            sliderInput("slider_años", 
                        label = 'Rango de años',
                        min = 2003, 
                        max = 2022, 
                        value = c(2003, 2022),
                        step = 1,
                        sep = ''),
            
            selectInput(
              inputId = "prueba",
              label = "Seleccionar region",
              choices = unique(individual_03.hoy$REGION),
              multiple = TRUE,
              selected = c()
            )
        ),
        

        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("distPlot")
        )
    )
))
