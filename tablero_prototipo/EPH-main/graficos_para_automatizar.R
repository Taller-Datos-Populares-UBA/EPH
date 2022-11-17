# En este script iremos almacenando los gráficos presentados en el informe de la OCEPP para conversar con Fede.

load('base_ep.RData')
source('ep_funciones.R')
library(lemon)
library(eph)

## Grafico 1: 
### Economía popular en el tiempo:
### Graficamos Cuentapropistas no profesionales en el tiempo
### Faltaría el resto
individual_03.hoy %>% 
  group_by(YEAR,TRIMESTER) %>%
  genera_resumen() %>%
  mutate('FECHA' = as.Date(paste(YEAR,3*TRIMESTER,1,sep='-'))) %>% 
  ungroup() %>% 
  ggplot(aes(x = FECHA,
         y = (CUENTAPROPISTAS_NO_PROFESIONALES + TFSR) / 1e6)) +
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
  

# Grafico 4: ingresos de los trabajadores de la EP de la ocupacion principal y no laborales, segun sexo. 2021 (promedio de trimestres)



individual_03.hoy %>% 
  filter( YEAR == 2021 ) %>% 
  filter(ES_CUENTAPROPISTA_NO_PROFESIONAL | CATEGORIA_OCUPACION == 'TRABAJADORE FLIAR S.R.') %>%
  group_by(SEXO,TRIMESTER) %>% 
  summarise(
    'INGRESO_LABORAL'= weighted.mean(INGRESOS_OCUP_PPAL,POND_ING_OCUP_PRINC), # FALTA INCLUIR PONDIIO
    'AYUDA_SOCIAL' = weighted.mean(INGRESO_AYUDA_SOCIAL,POND_ING_OCUP_PRINC),
    'JUB_Y_PENS' = weighted.mean(INGRESO_JUBILACION,POND_ING_OCUP_PRINC),
    'NO_LABORAL' = weighted.mean(INGRESO_TOTAL_NO_LABORAL,POND_ING_OCUP_PRINC),
  ) %>% 
  mutate('RESTO_NO_LABORAL' = NO_LABORAL-JUB_Y_PENS-AYUDA_SOCIAL) %>% 
  group_by(SEXO) %>% 
  summarise(INGRESO_LABORAL = mean(INGRESO_LABORAL),
            RESTO_NO_LABORAL = mean(RESTO_NO_LABORAL) + mean(JUB_Y_PENS) + mean(AYUDA_SOCIAL)) %>% 
  pivot_longer(cols=c('INGRESO_LABORAL', 'RESTO_NO_LABORAL')) %>% 
  ggplot(aes(x=SEXO, y= value)) +  
  geom_col(aes(`fill = SEXO)) + 
  scale_fill_manual(values = c('darkolivegreen1','darkolivegreen')) + 
  ylab('') +
  ylim(0,4e4) +
  facet_grid(.~name) +
  theme_bw()
