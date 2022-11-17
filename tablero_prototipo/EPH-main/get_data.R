# Linea para instalar los packages de EPH
install.packages('eph', dependencies = TRUE)

#importo librerías
library(eph)
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library("rstudioapi")

#seteo el lugar correcto donde correr el codigo
setwd(dirname(getSourceEditorContext()$path))
getwd()

#funcion que carga la base de datos y arma el string del archivo
cargar_base <- function(año, periodo, tipo){
  string <- paste("./data/EPH", periodo, año, tipo, sep="-")
  string <- paste(string, "RDS", sep=".")
  if(año>2003 || (año==2003 && periodo>2)){#en 2003 estan solo los trimestres 3 y 4
    get_microdata(year=año, trimester=periodo, type=tipo, destfile=string)
  }else{
    get_microdata(year=año, wave=periodo, type=tipo, destfile=string)
  }
}

df <- cargar_base(2003, 4, "individual")#cargamos la base
df <- organize_labels(df=df, type='individual')#organizamos los nombres de las columnas
df %>% View #mostrarlo en un formato lindo

#funcion que se fija la fecha actual y busca la última base de datos disponible
ultima_base <- function(tipo){#incompleta
  trimestre <- lubridate::quarter(Sys.Date())
  año <- lubridate::year(Sys.Date())
}

