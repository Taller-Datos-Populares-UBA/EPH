# Linea para instalar los packages de EPH

install.packages('eph', dependencies = TRUE)

library(eph)
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library("rstudioapi")


setwd(dirname(getSourceEditorContext()$path))
getwd()

dato_21_3 <- get_microdata(year=2021, trimester=3, type='individual', destfile="EPH-3-2021.csv")
dato_21_3 <- organize_labels(df=dato_21_3, type='individual')

dato_21_3 %>% View
#PP08D1 ¿Cuánto cobró por ese mes?
#CH15 donde nació
#ESTADO 0 = Entrevista individual no realizada (norespuesta al cuestionario individual)
       #1 = Ocupado
       #2 = Desocupado
       #3 = Inactivo
       #4 = Menor de 10 años
dato_21_3
sort(sample(100))
?get_microdata
