# En este script iremos almacenando los gráficos presentados en el informe de la OCEPP para conversar con Fede.

source('prepara_ep.R')
library(lemon)

## Grafico 1: 
### Economía popular en el tiempo:
### Graficamos Cuentapropistas no profesionales en el tiempo
### Faltaría el resto
individual_03.hoy %>% 
  group_by(YEAR,TRIMESTER) %>%
  genera_resumen() %>%
  mutate('FECHA' = as.Date(paste(YEAR,3*TRIMESTER,1,sep='-'))) %>% 
  ungroup() %>% 
  ggplot(aes(x=FECHA,
         y = (CUENTAPROPISTAS_NO_PROFESIONALES+TFSR)/1e6)) +
  geom_pointline() +
  ylab('Cuentapropistas no profesionales y T.F.S.R. [Millones]') +
  expand_limits(y=c(0,5))

# A este gráfico le falta poco menos de un millon de personas en 2003, probablemente por la falta de la base del MTEySS


## Gráfico 2: CNP/PEA
# TODO: Hay un corte largo en el medio ¿ por qué es?
individual_03.hoy %>% 
  group_by(YEAR,TRIMESTER) %>%
  genera_resumen() %>%
  mutate('FECHA' = as.Date(paste(YEAR,3*TRIMESTER,1,sep='-'))) %>% 
  ungroup() %>% 
  ggplot(aes(x=FECHA)) +
  geom_pointline(aes(y = (CUENTAPROPISTAS_NO_PROFESIONALES+TFSR)/ECONOMICAMENTE_ACTIVES,color='(CNP+TFSR)/PEA')) +
  geom_pointline(aes(y = (CUENTAPROPISTAS_NO_PROFESIONALES+TFSR)/OCUPADES,color='(CNP+TFSR)/PO')) +
  theme(legend.position=c(.1,.8)) + 
  ylab('Porcentaje') +
  scale_y_continuous(labels=scales::percent_format()) +
  scale_color_discrete(name='Conjuntos')+
  expand_limits(y=c(.10,.26))

# En este los valores parecen estar bien

# Gráfico 3: gráfico de torta representando las fuentes de ingreso

individual_03.hoy %>% 
  filter(YEAR == 2021) %>% 
  group_by(TRIMESTER) %>%
  filter(ES_CUENTAPROPISTA_NO_PROFESIONAL | CATEGORIA_OCUPACION == 'TRABAJADORE FLIAR S.R.') %>%
  summarise(
    'INGRESO_LABORAL'= sum(INGRESOS_OCUP_PPAL), # FALTA INCLUIR PONDIIO
    'AYUDA_SOCIAL' = sum(INGRESO_AYUDA_SOCIAL),
    'JUB_Y_PENS' = sum(INGRESO_JUBILACION),
    'NO_LABORAL' = sum(INGRESO_TOTAL_NO_LABORAL),
      ) %>%
  mutate('RESTO_NO_LABORAL' = NO_LABORAL-JUB_Y_PENS-AYUDA_SOCIAL) %>% 
  summarise(INGRESO_LABORAL = mean(INGRESO_LABORAL),
            AYUDA_SOCIAL = mean(AYUDA_SOCIAL),
            JUB_Y_PENS = mean(JUB_Y_PENS),
            RESTO_NO_LABORAL = mean(RESTO_NO_LABORAL)) %>%
  pivot_longer(cols=everything()) %>% 
  arrange(desc(name)) %>%
  mutate(value=value/sum(value)*100) %>%
  mutate(ypos = cumsum(value)-0.5*value) %>%
  ggplot(aes(x="",y=value,fill=name)) +
  geom_bar(stat='identity',width=1,color='white')+
  coord_polar("y",start=0) +
  theme_void() +
  geom_text(
    aes(y=ypos,
        label=paste(round(value),'%',sep='')),
    color='black',size=4,x=1.6) + 
  scale_fill_brewer(name='Fuente de ingresos',palette='Set1') 
  


# Grafico 7: Tasa de pobreza de ocupades segun pertenencia a EP. 2021 (en trimestres).

individual_03.hoy %>% 
  filter(YEAR == 2021) %>%  
  group_by(TRIMESTER) %>%
  mutate(
    'POBREZA' = TOTAL_INGRESO_INDIVIDUAL > 22826, 
    'INDIGENCIA' = TOTAL_INGRESO_INDIVIDUAL > 10008,
    'FECHA' = as.Date(paste(YEAR,3*TRIMESTER,1,sep='-'))
  ) %>%  
  genera_resumen_p() %>% 
  ungroup() %>% 
  ggplot(aes(x=TRIMESTER)) +
  geom_pointline(aes(y = (CUENTAPROPISTAS_NO_PROFESIONALES_P + TFSR_P)/(CUENTAPROPISTAS_NO_PROFESIONALES + TFSR), color = 'EP')) +
  geom_pointline(aes(y = (OCUPADES_P - CUENTAPROPISTAS_NO_PROFESIONALES_P - TFSR_P)/(OCUPADES - CUENTAPROPISTAS_NO_PROFESIONALES - TFSR), color = 'No EP')) + 
  geom_pointline(aes(y = OCUPADES_P/OCUPADES, color = 'Poblacion general')) + 
  ylab('Porcentaje') +
  scale_y_continuous(labels=scales::percent_format()) +
  scale_color_discrete(name='Conjuntos')+
  expand_limits(y=c(.10,.60))




#Grafico 8

individual_03.hoy %>% 
  filter(YEAR == 2021) %>%  
  group_by(TRIMESTER) %>%
  mutate(
    'POBREZA' = TOTAL_INGRESO_INDIVIDUAL > 22826, 
    'INDIGENCIA' = TOTAL_INGRESO_INDIVIDUAL > 10008,
    'FECHA' = as.Date(paste(YEAR,3*TRIMESTER,1,sep='-'))
  ) %>%  
  genera_resumen_i() %>% 
  ungroup() %>% 
  ggplot(aes(x=TRIMESTER)) +
  geom_pointline(aes(y = (CUENTAPROPISTAS_NO_PROFESIONALES_I + TFSR_I)/(CUENTAPROPISTAS_NO_PROFESIONALES + TFSR), color = 'EP')) +
  geom_pointline(aes(y = (OCUPADES_I - CUENTAPROPISTAS_NO_PROFESIONALES_I - TFSR_I)/(OCUPADES - CUENTAPROPISTAS_NO_PROFESIONALES - TFSR), color = 'No EP')) + 
  geom_pointline(aes(y = OCUPADES_I/OCUPADES, color = 'Poblacion general')) + 
  ylab('Porcentaje') +
  scale_y_continuous(labels=scales::percent_format()) +
  scale_color_discrete(name='Conjuntos')+
  expand_limits(y=c(.10,.60))

  