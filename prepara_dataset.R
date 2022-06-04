library(tidyverse,warn.conflicts = FALSE,quietly = TRUE)
library(eph,warn.conflicts = FALSE,quietly = TRUE)


variables_de_interes <-
  c('ANO4','TRIMESTRE','REGION','AGLOMERADO',
    'CODUSU','NRO_HOGAR',
    'CH03','CH04','CH05','CH06',
    'ESTADO','CAT_OCUP','CAT_INAC',
    'PP04D_COD')

individual_03to15 <- get_microdata(year=2003:2015,trimester=1:4,vars = variables_de_interes)
individual_16to22 <- get_microdata(year=2016:2022,trimester=1:4,vars = variables_de_interes)

individual_03to15 <- (individual_03to15$microdata) %>% bind_rows()
individual_16to22 <- (individual_16to22$microdata) %>% bind_rows()
individual_03to22 <- bind_rows(individual_03to15,individual_16to22)

individual_03to22 <- 
  individual_03to22 %>%
  rename(ROL_HOGAR = CH03,
         GENERO = CH04,
         FECHA_NAC = CH05,
         EDAD = CH06,
         COD_OCUP = PP04D_COD
  )

individual_03to22 <- 
  individual_03to22 %>%
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
  mutate(GENERO = case_when(
    GENERO == 1 ~ 'VARON',
    GENERO == 2 ~ 'MUJER',
    TRUE ~ 'N/A'
  )) %>% 
  mutate(FECHA_NAC = as.Date(FECHA_NAC)) %>%
  mutate(ESTADO = case_when(
    ESTADO == 1 ~ 'OCUPADO',
    ESTADO == 2 ~ 'DESOCUPADO',
    ESTADO == 3 ~ 'INACTIVO',
    ESTADO == 4 ~ 'MENOR 10',
    TRUE ~ 'N/A'
  )) %>% 
  mutate(CAT_INAC = case_when(
    CAT_INAC == 1 ~ 'JUBILADO/PENSIONADO',
    CAT_INAC == 2 ~ 'RENTISTA',
    CAT_INAC == 3 ~ 'ESTUDIANTE',
    CAT_INAC == 4 ~ 'AMA DE CASA',
    CAT_INAC == 5 ~ 'MENOR 6',
    CAT_INAC == 6 ~ 'DISCAPACITADO',
    CAT_INAC == 7 ~ 'OTROS',
    TRUE ~ 'N/A'
  )) %>%
  mutate(CAT_OCUP = case_when(
    CAT_OCUP == 1 ~ 'PATRON',
    CAT_OCUP == 2 ~ 'CUENTAPROPISTA',
    CAT_OCUP == 3 ~ 'ASALARIADE',
    CAT_OCUP == 4 ~ 'TRABAJADOR FLIAR S.R.',
    TRUE ~ 'N/A'
  )) %>%
  mutate(ES_PROF = case_when(
    str_ends(COD_OCUP, "[2-4]") ~ FALSE,
    str_ends(COD_OCUP, "[1]") ~ TRUE,
    TRUE ~ NA)
  )

individual_03to22 <- 
  individual_03to22 %>%
  mutate(FECHA_EPH =
           as.Date(
             paste(ANO4,3*TRIMESTRE,1),
             format='%Y %m %d')) %>% # Construimos la fecha en la que se hizo la encuesta
  mutate(EDAD_FECHA = round(as.numeric(FECHA_EPH - FECHA_NAC)/365)) # Armamos la edad como la resta entre ambas dividido 365 días