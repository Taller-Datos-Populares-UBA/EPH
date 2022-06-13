# En este script iremos almacenando los gráficos presentados en el informe de la OCEPP para conversar con Fede.

source('prepara_ep.R')
library(lemon)
## Grafico 1: 
### Economía popular en el tiempo:
### Graficamos Cuentapropistas no profesionales en el tiempo

individual_03.hoy %>% 
  group_by(YEAR,TRIMESTER) %>%
  genera_resumen() %>%
  mutate('FECHA' = as.Date(paste(YEAR,3*TRIMESTER,1,sep='-'))) %>% 
  ungroup() %>% 
  ggplot(aes(x=FECHA,
         y = CUENTAPROPISTAS_NO_PROFESIONALES)) +
  geom_pointline() 

## Una versión con smooth
individual_03.hoy %>% 
  group_by(YEAR,TRIMESTER) %>%
  genera_resumen() %>%
  mutate('FECHA' = as.Date(paste(YEAR,3*TRIMESTER,1,sep='-'))) %>% 
  ungroup() %>% 
  ggplot(aes(x=FECHA,
             y = CUENTAPROPISTAS_NO_PROFESIONALES)) +
  geom_point() +
  geom_smooth()

# Cuentapropistas no profesionales, por sexo
individual_03.hoy %>% 
  group_by(YEAR,TRIMESTER,SEXO) %>%
  genera_resumen() %>%
  mutate('FECHA' = as.Date(paste(YEAR,3*TRIMESTER,1,sep='-'))) %>%
  ungroup() %>%
  ggplot(aes(x=FECHA,
             y = CUENTAPROPISTAS_NO_PROFESIONALES,color=SEXO)) +
  geom_point() +
  geom_smooth()



## Gráfico 2: CNP/PEA

individual_03.hoy %>% 
  group_by(YEAR,TRIMESTER) %>%
  genera_resumen() %>%
  mutate('FECHA' = as.Date(paste(YEAR,3*TRIMESTER,1,sep='-'))) %>% 
  ungroup() %>% 
  ggplot(aes(x=FECHA)) +
  geom_pointline(aes(y = CUENTAPROPISTAS_NO_PROFESIONALES/ECONOMICAMENTE_ACTIVES,color='CNP/PEA')) +
  geom_pointline(aes(y = CUENTAPROPISTAS_NO_PROFESIONALES/OCUPADES,color='CNP/PO')) +
  theme(legend.position=c(.1,.8)) + 
  ylab('PROPORCION')


individual_03.hoy %>% 
  group_by(YEAR,TRIMESTER) %>% 
  summarise(INGRESO_MEDIO_OCUP_PPAL = 
              mean(INGRESOS_OCUP_PPAL),
            INGRESO_MEDIO_CNP_OCUP_PPAL =
              mean(ifelse(ES_CUENTAPROPISTA_NO_PROFESIONAL,INGRESOS_OCUP_PPAL,NA),na.rm=TRUE),
            INGRESO_MEDIO_PO_OCUP_PPAL = 
              mean(ifelse(ESTADO=='OCUPADE',INGRESOS_OCUP_PPAL,NA),na.rm=TRUE)) %>%
  mutate('FECHA' = as.Date(paste(YEAR,3*TRIMESTER,1,sep='-'))) %>% 
  ungroup() %>% 
  ggplot(aes(x=FECHA)) +
  geom_pointline(aes(y=INGRESO_MEDIO_OCUP_PPAL,color='TOTAL'))+
  geom_pointline(aes(y=INGRESO_MEDIO_CNP_OCUP_PPAL,color='CNP')) +
  geom_pointline(aes(y=INGRESO_MEDIO_PO_OCUP_PPAL,color='OCUP.')) +
  theme(legend.position=c(.1,.8))+
  scale_color_discrete(name='Población')


individual_03.hoy %>% 
  group_by(YEAR,TRIMESTER) %>% 
  summarise(INGRESO_MEDIO_OCUP_PPAL = 
              mean(INGRESOS_OCUP_PPAL),
            INGRESO_MEDIO_CNP_OCUP_PPAL =
              mean(ifelse(ES_CUENTAPROPISTA_NO_PROFESIONAL,INGRESOS_OCUP_PPAL,NA),na.rm=TRUE),
            INGRESO_MEDIO_PO_OCUP_PPAL = 
              mean(ifelse(ESTADO=='OCUPADE',INGRESOS_OCUP_PPAL,NA),na.rm=TRUE)) %>%
  mutate('FECHA' = as.Date(paste(YEAR,3*TRIMESTER,1,sep='-'))) %>% 
  ungroup() %>% 
  ggplot(aes(x=FECHA)) +
  geom_pointline(aes(y=INGRESO_MEDIO_CNP_OCUP_PPAL/INGRESO_MEDIO_PO_OCUP_PPAL)) +
  ylab('IOP CNP/ IOP PO')
