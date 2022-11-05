### Este script se ocupa de descargar la información desde la EPH para ser usada para la economía popular

# Carga de parquetes
library(eph)
library(tidyverse)
source('ep_funciones.R')
library(ggridges)

# Variables descargadas
## Las levantamos desde un archivo
variables_ep <- read_csv('variables_ep.csv')
#TODO: Faltaría incluir una variable "PERIODO" para asegurarse de descargar sólo cosas que funcionen

# Periodo a descargar: 
## Descargamos todos los años hasta este
years_download <- 2003:as.numeric(format(Sys.time(),'%Y'))
## Todos los trimestres
trimesters_download <- 1:4

# Descargamos primer periodo
individual_03.15 <- get_microdata(
  year = years_download[years_download<=2015],
  trimester = trimesters_download,
  type = 'individual',
  vars = variables_ep$VAR_EPH %>% setdiff(c('PONDII','PONDIIO','PONDIH'))
  )

# Descargamos segundo periodo
individual_16.hoy <- get_microdata( 
  year = years_download[years_download>2015],
  trimester = trimesters_download,
  type = 'individual',
  vars = variables_ep$VAR_EPH
)

# Combinamos los datos de ambos periodos en un solo dataset
if(is.element('microdata',colnames(individual_03.15))){
  individual_03.15 <- (individual_03.15$microdata) %>% bind_rows() 
}
if(is.element('microdata',colnames(individual_16.hoy))){
  individual_16.hoy <- (individual_16.hoy$microdata) %>% bind_rows()
}
# Pegamos los datasets
individual_03.hoy <- bind_rows(individual_03.15,individual_16.hoy)

rm(individual_03.15,individual_16.hoy)

# Renombramos variables
individual_03.hoy <- 
  individual_03.hoy %>%
  rename_with(
    .cols=variables_ep$VAR_EPH,
    .fn = function(.col) variables_ep$NOMBRE[match(.col,variables_ep$VAR_EPH)]
    )

# "Humanizamos" niveles 
## TODO: pasar esto a un formato independiente

# Orden / Niveles para variable EDAD_QUINQUENIO
niveles_quinquenio <- c(sapply(1:18, function(i) paste((i-1)*5,i*5, sep="-") ),"90+")
niveles_decenio <- c(sapply(1:9, function(i) paste((i-1)*10,i*10, sep="-") ),"90+")


# Gran operacion que convierte cada variable a lenguaje humano
individual_03.hoy <- 
  individual_03.hoy %>%
  mutate(REGION = case_when(
    REGION == 1 ~ 'GBA',
    REGION == 40 ~ 'NOA',
    REGION == 41 ~ 'NEA',
    REGION == 42 ~ 'CUYO',
    REGION == 43 ~ 'PAMPEANA',
    REGION == 44 ~ 'PATAGONIA',
    TRUE ~ 'N/A'
  )) %>% 
  mutate(AGLOMERADO = case_when(
    AGLOMERADO == 2 ~ 'GRAN LA PLATA',
    AGLOMERADO == 3 ~ 'BAHIA BLANCA - CERRI',
    AGLOMERADO == 4 ~ 'GRAN ROSARIO',
    AGLOMERADO == 5 ~ 'GRAN SANTA FE',
    AGLOMERADO == 6 ~ 'GRAN PARANA',
    AGLOMERADO == 7 ~ 'POSADAS',
    AGLOMERADO == 8 ~ 'GRAN RESISTENCIA',
    AGLOMERADO == 9 ~ 'C. RIVADAVIA - R. TILLY',
    AGLOMERADO == 10 ~ 'GRAN MENDOZA',
    AGLOMERADO == 12 ~ 'CORRIENTES',
    AGLOMERADO == 13 ~ 'GRAN CORDOBA',
    AGLOMERADO == 14 ~ 'CONCORDIA',
    AGLOMERADO == 15 ~ 'FORMOSA',
    AGLOMERADO == 17 ~ 'NEUQUEN - PLOTTIER',
    AGLOMERADO == 18 ~ 'S. DEL ESTERO - LA BANDA',
    AGLOMERADO == 19 ~ 'JUJUY - PALPALA',
    AGLOMERADO == 20 ~ 'RIO GALLEGOS',
    AGLOMERADO == 22 ~ 'GRAN CATAMARCA',
    AGLOMERADO == 23 ~ 'GRAN SALTA',
    AGLOMERADO == 25 ~ 'LA RIOJA',
    AGLOMERADO == 26 ~ 'GRAN SAN LUIS',
    AGLOMERADO == 27 ~ 'GRAN SAN JUAN',
    AGLOMERADO == 29 ~ 'GRAN TUCUMAN - TAFI V.',
    AGLOMERADO == 30 ~ 'SANTA ROSA - TOAY',
    AGLOMERADO == 31 ~ 'USUAHIA - RIO GRANDE',
    AGLOMERADO == 32 ~ 'CABA',
    AGLOMERADO == 33 ~ 'PARTIDOS GBA',
    AGLOMERADO == 34 ~ 'MAR DEL PLATA',
    AGLOMERADO == 36 ~ 'RIO CUARTO',
    AGLOMERADO == 38 ~ 'SAN NICOLAS - V. CONSTITUCION',
    AGLOMERADO == 91 ~ 'RAWSON - TRELEW',
    AGLOMERADO == 93 ~ 'VIEDMA - C. DE PATAGONES',
    TRUE ~ 'N/A'
  )) %>% 
  mutate(ROL_HOGAR = case_when(
    ROL_HOGAR == 1 ~ 'JEFE',
    ROL_HOGAR == 2 ~ 'CONYUGE',
    ROL_HOGAR == 3 ~ 'HIJE',
    ROL_HOGAR == 4 ~ 'YERNO/NUERA',
    ROL_HOGAR == 5 ~ 'NIETE',
    ROL_HOGAR == 6 ~ 'MADRE/PADRE',
    ROL_HOGAR == 7 ~ 'SUEGRE',
    ROL_HOGAR == 8 ~ 'HERMANE',
    ROL_HOGAR == 9 ~ 'OTRO FLIAR',
    ROL_HOGAR == 10 ~ 'NO FLIAR',
    TRUE ~ 'N/A'
  )) %>% 
  mutate(SEXO = case_when(
    SEXO == 1 ~ 'VARON',
    SEXO == 2 ~ 'MUJER',
    TRUE ~ 'N/A'
  )) %>% 
  mutate(ESTADO = case_when(
    ESTADO == 1 ~ 'OCUPADE',
    ESTADO == 2 ~ 'DESOCUPADE',
    ESTADO == 3 ~ 'INACTIVE',
    ESTADO == 4 ~ 'MENOR 10',
    TRUE ~ 'N/A'
  )) %>% 
  mutate(CATEGORIA_INACTIVO = case_when(
    CATEGORIA_INACTIVO == 1 ~ 'JUBILADE/PENSIONADE',
    CATEGORIA_INACTIVO == 2 ~ 'RENTISTA',
    CATEGORIA_INACTIVO == 3 ~ 'ESTUDIANTE',
    CATEGORIA_INACTIVO == 4 ~ 'AMA DE CASA',
    CATEGORIA_INACTIVO == 5 ~ 'MENOR 6',
    CATEGORIA_INACTIVO == 6 ~ 'DISCAPACITADE',
    CATEGORIA_INACTIVO == 7 ~ 'OTROS',
    TRUE ~ 'N/A'
  )) %>%
  mutate(CATEGORIA_OCUPACION = case_when(
    CATEGORIA_OCUPACION == 1 ~ 'PATRON',
    CATEGORIA_OCUPACION == 2 ~ 'CUENTAPROPISTA',
    CATEGORIA_OCUPACION == 3 ~ 'ASALARIADE',
    CATEGORIA_OCUPACION == 4 ~ 'TRABAJADORE FLIAR S.R.',
    TRUE ~ 'N/A'
  )) %>%
  mutate(ES_PROFESIONAL = case_when(
    str_ends(CODIGO_OCUPACION, "[2-4]") ~ FALSE,
    str_ends(CODIGO_OCUPACION, "[1]") ~ TRUE,
    TRUE ~ NA
  )) %>%
  mutate(ES_CUENTAPROPISTA_NO_PROFESIONAL = case_when(
    CATEGORIA_OCUPACION == 'CUENTAPROPISTA' & !ES_PROFESIONAL ~ TRUE,
    TRUE ~ FALSE
  )) %>% 
  mutate(ES_TFSR = case_when(
    CATEGORIA_OCUPACION == 'TRABAJADORE FLIAR S.R.' ~ TRUE,
    TRUE ~ FALSE
  )) %>% 
  mutate(EDAD_QUINQUENIO = case_when(
    EDAD < 0 ~ NA_character_,
    EDAD < 5 ~ "0-5",
    EDAD < 10 ~ "5-10",
    EDAD < 15 ~ "10-15",
    EDAD < 20 ~ "15-20",
    EDAD < 25 ~ "20-25",
    EDAD < 30 ~ "25-30",
    EDAD < 35 ~ "30-35",
    EDAD < 40 ~ "35-40",
    EDAD < 45 ~ "40-45",
    EDAD < 50 ~ "45-50",
    EDAD < 55 ~ "50-55",
    EDAD < 60 ~ "55-60",
    EDAD < 65 ~ "60-65",
    EDAD < 70 ~ "65-70",
    EDAD < 75 ~ "70-75",
    EDAD < 80 ~ "75-80",
    EDAD < 85 ~ "80-85",
    EDAD < 90 ~ "85-90",
    EDAD >= 90 ~ "90+",
    TRUE ~ NA_character_
  )) %>%
  mutate(EDAD_QUINQUENIO = factor(EDAD_QUINQUENIO, levels= niveles_quinquenio)) %>%
  mutate(EDAD_DECENIO = case_when(
    EDAD < 0 ~ NA_character_,
    EDAD < 10 ~ "0-10",
    EDAD < 20 ~ "10-20",
    EDAD < 30 ~ "20-30",
    EDAD < 40 ~ "30-40",
    EDAD < 50 ~ "40-50",
    EDAD < 60 ~ "50-60",
    EDAD < 70 ~ "60-70",
    EDAD < 80 ~ "70-80",
    EDAD < 90 ~ "80-90",
    EDAD >= 90 ~ "90+",
    TRUE ~ NA_character_
  )) %>%
  mutate(EDAD_DECENIO = factor(EDAD_DECENIO, levels= niveles_decenio))

#### CALCULO DE POBREZA

canastas <- read.csv('canastas_confiable.csv')

individual_03.hoy <- calcula_pobreza_individual(individual_03.hoy,basket = canastas)

save(individual_03.hoy,file='base_ep.RData')

#write.csv(individual_03.hoy, "base_ep.csv", row.names = FALSE)

rm(list=ls())
closeAllConnections()
gc()
