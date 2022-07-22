#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(lemon)
load('../base_ep.RData')
source('../ep_funciones.R')
# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    output$distPlot <- renderPlot({

        # generate bins based on input$bins from ui.R
      if(input$group_sexo){
        grouping_vars <- quos('SEXO','YEAR','TRIMESTER')
        aes_plot <- aes_(x=~FECHA,
                         y = ~(CUENTAPROPISTAS_NO_PROFESIONALES+TFSR)/1e6,
                         colour = ~SEXO)
      }else{
        grouping_vars <- quos('YEAR','TRIMESTER')
        aes_plot <- aes_(x=~FECHA,
                         y = ~(CUENTAPROPISTAS_NO_PROFESIONALES+TFSR)/1e6)
      }
      individual_03.hoy %>% 
        filter(YEAR > input$slider_años[1] & YEAR < input$slider_años[2]) %>%
        group_by_at(grouping_vars) %>%
        genera_resumen() %>% 
        mutate('FECHA' = as.Date(paste(YEAR,3*TRIMESTER,1,sep='-'))) %>% 
        ungroup() %>% 
        ggplot(aes_plot) +
        geom_pointline() +
        ylab('Cuentapropistas no profesionales y T.F.S.R. [Millones]') +
        theme(legend.position=c(.2,.2)) #+
        # ylim(0,5) # Por alguna razón tira error esto

    })

})
