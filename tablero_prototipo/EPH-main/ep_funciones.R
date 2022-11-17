genera_resumen <- function(df){
  #'@param df Un dataframe, eventualmente agrupado por subgrupos
  #'@returns Un dataframe resumido, con las categorias POBLACION, ECONOMICAMENTE_ACTIVES, CUENTAPROPISTAS_PROFESIONALES, CUENTAPROPISTAS_NO_PROFESIONALES.
  #'@details El objetivo de esta funcion es agilizar la escritura de la preparación de resumenes que muestren la evolución por año y trimestre de las poblaciones de interés. Pasandole un tibble agrupado, se puede ver la evolución de subgrupos de poblaciones en términos de las variables de la EP.
  df %>%
    summarise(
      'POBLACION'=sum(PONDERA),
      'OCUPADES' = sum(
        PONDERA * 
          (ESTADO == 'OCUPADE'),na.rm=TRUE),
      'ECONOMICAMENTE_ACTIVES' = sum(
        PONDERA * 
          (ESTADO == 'OCUPADE' | ESTADO == 'DESOCUPADE'),na.rm=TRUE),
      'CUENTAPROPISTAS' = sum(
        PONDERA * 
          (CATEGORIA_OCUPACION == 'CUENTAPROPISTA'),na.rm=TRUE),
      'CUENTAPROPISTAS_PROFESIONALES' = sum(
        PONDERA * 
          (CATEGORIA_OCUPACION == 'CUENTAPROPISTA' & ES_PROFESIONAL),na.rm=TRUE),
      'CUENTAPROPISTAS_NO_PROFESIONALES' = sum(
        PONDERA * 
          (CATEGORIA_OCUPACION == 'CUENTAPROPISTA' & !ES_PROFESIONAL),na.rm=TRUE),
      'TFSR' = sum(
        PONDERA * 
          (CATEGORIA_OCUPACION == 'TRABAJADORE FLIAR S.R.'),na.rm=TRUE)
      ) %>% 
    return()
}


calcula_pobreza_hogar <- function (base, basket, print_summary = TRUE) 
{
  adulto_equi <- adulto_equivalente %>% rename(SEXO = CH04,EDAD = CH06) %>%
    mutate(SEXO = case_when(SEXO == 1 ~ 'VARON', SEXO == 2 ~ 'MUJER'))
  base <- base %>%
    mutate(periodo = paste(YEAR, TRIMESTER, sep = ".")) %>%
    left_join(., adulto_equi,
                     by = c("SEXO", "EDAD")) %>%
    left_join(., basket %>% mutate(periodo = as.character(periodo)),
                     by = c(REGION = "region", "periodo")) %>%
    group_by(CODUSU, NRO_HOGAR, periodo) %>%
    mutate(adequi_hogar = sum(adequi)) %>% 
    ungroup() %>%
    mutate(CBA_hogar = CBA * adequi_hogar,
                  CBT_hogar = CBT * adequi_hogar,
                  situacion = case_when(
                    ITF < CBA_hogar ~ "indigente",
                    ITF >= CBA_hogar &
                      ITF < CBT_hogar ~  "pobre",
                    ITF >= CBT_hogar ~ "no_pobre"),
                  situacion = case_when(
                    PONDIH == 0 ~ NA_character_,
                    TRUE ~ situacion)) 
  if (print_summary) {
    Pobreza_resumen <- base %>%
      group_by(YEAR, TRIMESTER) %>%
      summarise(
        Tasa_pobreza = sum(PONDIH[situacion %in% c("pobre", "indigente")], na.rm = TRUE)/sum(PONDIH,na.rm = TRUE), 
        Tasa_indigencia = sum(PONDIH[situacion ==  "indigente"], na.rm = TRUE)/sum(PONDIH, na.rm = TRUE))
    print(Pobreza_resumen)
  }
  return(base)
}

calcula_pobreza_individual <- function (base, basket){
  #'@param base La base a ser empleada (resultado de una descarga de EPH y posterior procesamiento interno de EP).
  #'@param basket Canastas en el mismo formato que las provistas por EPH.
  #'@return La misma base, pero con columna situación indicando la situación del a persona, y columnas adequi (indicando el consumo energético), CBA_indi y CBT_indi (indicando la canastas básica de la persona).
  library(eph)
  adulto_equi <- adulto_equivalente %>% rename(SEXO = CH04,EDAD = CH06) %>%
    mutate(SEXO = case_when(SEXO == 1 ~ 'VARON', SEXO == 2 ~ 'MUJER'))
  base <- base %>%
    mutate(periodo = paste(YEAR, TRIMESTER, sep = ".")) %>%
    left_join(., adulto_equi,
              by = c("SEXO", "EDAD")) %>%
    left_join(., basket %>% mutate(periodo = as.character(periodo)),
              by = c(REGION = "region", "periodo")) %>%
    mutate(
      CBA_indi = CBA * adequi,
      CBT_indi = CBT * adequi,
      situacion = case_when(
        is.na(TOTAL_INGRESO_INDIVIDUAL) | ( TOTAL_INGRESO_INDIVIDUAL < 0 ) ~ NA_character_,
        TOTAL_INGRESO_INDIVIDUAL < CBA_indi ~ "indigente",
        TOTAL_INGRESO_INDIVIDUAL < CBT_indi ~  "pobre",
        TRUE ~ "no_pobre"),
      situacion = case_when(
        PONDIH == 0 ~ NA_character_,
        TRUE ~ situacion),
      situacion_check = case_when(
        TOTAL_INGRESO_INDIVIDUAL < CBA_indi ~ "indigente",
        TOTAL_INGRESO_INDIVIDUAL >=CBA_indi &
          TOTAL_INGRESO_INDIVIDUAL < CBT_indi ~ 'pobre',
        TOTAL_INGRESO_INDIVIDUAL >= CBT_indi ~ 'no_pobre'
      )
    ) %>%
    select(-c(CBA,CBT,periodo))
  return(base)
}