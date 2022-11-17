## FUNCIONES PARA PROCESAR DATOS

genera_resumen <- function(df){
  #'@param df Un dataframe, eventualmente agrupado por subgrupos
  #'@returns Un dataframe resumido, con las categorias POBLACION, ECONOMICAMENTE_ACTIVES, CUENTAPROPISTAS_PROFESIONALES, CUENTAPROPISTAS_NO_PROFESIONALES.
  #'@details El objetivo de esta funcion es agilizar la escritura de la preparación de resumenes que muestren la evolución por año y trimestre de las poblaciones de interés. Pasandole un tibble agrupado, se puede ver la evolución de subgrupos de poblaciones en términos de las variables de la EP.
  df %>%
    mutate(ES_EP = ES_CUENTAPROPISTA_NO_PROFESIONAL | ES_TFSR) %>%
    summarise(
      'POBLACION'=sum(PONDERA),
      'OCUPADES' = sum(
        PONDERA * 
          (ESTADO == 'OCUPADE'),na.rm=TRUE),
      'OCUPADES_NO_EP' = sum(
        PONDERA * 
          (ESTADO == 'OCUPADE' & !ES_EP),na.rm=TRUE),
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
          (CATEGORIA_OCUPACION == 'TRABAJADORE FLIAR S.R.'),na.rm=TRUE),
      "ASALARIADOS_REGISTRADOS" = sum(
        PONDERA * (
          CATEGORIA_OCUPACION == 'ASALARIADE' & REGISTRACION == 1),na.rm=TRUE
      ),
      "ASALARIADOS_NOREGISTRADOS" = sum(
        PONDERA * (
          CATEGORIA_OCUPACION == 'ASALARIADE' & REGISTRACION == 2),na.rm=TRUE
      ),
      "PATRONES" = sum(
        PONDERA * (
          CATEGORIA_OCUPACION == 'PATRON'), na.rm=TRUE
      ),
      'ECONOMIA_POPULAR' = sum(ES_EP*PONDERA,na.rm=TRUE),
      "RESTO_CUENTAPROPISTAS" = CUENTAPROPISTAS - ECONOMIA_POPULAR
    ) %>% 
    return()
}

############################
## FUNCIONES PARA EL TABLERO
############################

genera_aes_cantTrabEP_plot <- function(input){
  #' @description Esta función se encarga de generar el objeto estético para el plot cantTrabEP_plot
  if(input$separar_genero){ # Si separamos por sexo
      if(input$separar_zonas){ # Si separamos por región
        
        aes_plot <- aes_(
          x = ~ FECHA,
          y = ~ (PERSONAS)/1e6, 
          #group = as.formula( paste('~paste( SEXO,', input$variable_zona, ',OCUPACIONES)' )), 
          color = as.formula( paste('~', input$variable_zona )),
          linetype = ~ OCUPACIONES,
          shape = ~ SEXO)
        
      }else{ # Si no separamos por región
        
        aes_plot <- aes_(
          x = ~ FECHA,
          y = ~ (PERSONAS)/1e6, 
          linetype = ~ OCUPACIONES,
          #group = ~ paste(SEXO,OCUPACIONES),  
          shape = ~ SEXO)
        
      }
    }else{ # Si no separamos por sexo
      if(input$separar_zonas){ # Si separamos por región
        
        aes_plot <- aes_(
          x = ~ FECHA,
          y = ~ (PERSONAS)/1e6, 
          linetype = ~ OCUPACIONES,
          #group = as.formula(paste('~',input$variable_zona)), 
          color = as.formula(paste('~',input$variable_zona)))
        
      }else{ # Si no separamos por región
        
        aes_plot <- aes_(
          y = ~ (PERSONAS)/1e6,
          linetype = ~ OCUPACIONES,
          x = ~ FECHA
          )
        
      }
    }
  return(aes_plot)
}

genera_grouping_vars_cantTrabEP_plot <- function(input){
  #'@description Esta función genera el agrupamiento necesario para el plot cantTrabEP_plot
  # if(input$separar_sexos){ # Si separamos por sexo
  #   if(input$separar_zonas){
  #   
  #     grouping_vars <- quos('SEXO','YEAR','TRIMESTER', input$variable_zona)
  #     
  #   }else{
  #     
  #     grouping_vars <- quos('SEXO','YEAR','TRIMESTER')
  #     
  #   }
  # }else {
  #   if(input$separar_zonas){
  #     
  #     grouping_vars <- quos('YEAR','TRIMESTER', input$variable_zona)
  #     
  #   }else{
  #     
  #     grouping_vars <- quos('YEAR','TRIMESTER')
  #     
  #   }
  # }
  
  agrupar <- c("YEAR", "TRIMESTER")
  if(input$separar_genero){
    agrupar <- c(agrupar, "SEXO")
  }
  if(input$separar_zonas){
    agrupar<- c(agrupar, input$variable_zona)
  }
  grouping_vars <- quos(agrupar)
  return(grouping_vars)
}


genera_aes_pobrezaEP_plot <- function(input){
  #' @description Esta función se encarga de generar el objeto estético para el plot cantTrabEP_plot
  if(input$separar_genero_t2){ # Si separamos por sexo
      #aes( x = FECHA, color = tasa_tipo, y = tasa)
      aes_plot <- aes_(
        x = ~ FECHA,
        y = ~ tasa, 
        color = ~ tasa_tipo,
        shape = ~ SEXO)
  }else{ # Si no separamos por sexo
      aes_plot <- aes_(
        x = ~ FECHA,
        y = ~ tasa,
        color = ~ tasa_tipo
      )
      
  }
  return(aes_plot)
}

genera_grouping_vars_pobrezaEP_plot <- function(input){
  #'@description Esta función genera el agrupamiento necesario para el plot cantTrabEP_plot
  if(input$separar_genero_t2){ # Si separamos por sexo
      grouping_vars <- quos('SEXO','YEAR','TRIMESTER')
  }else {
    grouping_vars <- quos('YEAR','TRIMESTER')
  }
  return(grouping_vars)
}
