library(tidyverse)
library(lemon)
library(eph)
source('ep_funciones.R')
load('base_ep.RData')
canastas <- read.csv('canastas_confiable.csv') %>% mutate(region = toupper(region))
individual_03.hoy <- calcula_pobreza_hogar(
  base = individual_03.hoy,
  basket = canastas,
  print_summary = FALSE
)

# individual_03.hoy %>% 
#   mutate(periodo  = paste(YEAR,TRIMESTER,sep='.')) %>%
#   mutate(INGRESOS_OCUP_PPAL_NORM = INGRESOS_OCUP_PPAL/CBT/adequi) %>% 
#   filter(ES_CUENTAPROPISTA_NO_PROFESIONAL | ES_TFSR) %>%
#   group_by(YEAR,TRIMESTER,REGION,situacion) %>%
#   summarise(IOP = weighted.mean(INGRESOS_OCUP_PPAL_NORM,POND_ING_OCUP_PRINC)) %>%
#   mutate(FECHA = as.Date(paste(YEAR,TRIMESTER,'01',sep='-'),format = '%Y-%m-%d')) %>%
#   drop_na() %>%
#   ggplot(aes(x = FECHA, y = IOP,color = situacion)) +
#   geom_pointline() +
#   facet_wrap(~ REGION) +
#   ylab ( 'ING OCUP PPAL / CBT [EP NUCLEO]') -> gg
# 
# pdf('ingresos_region.pdf')
# print(gg)
# dev.off()
# 
# 
# 
# individual_03.hoy %>% 
#   mutate(periodo  = paste(YEAR,TRIMESTER,sep='.')) %>%
#   mutate(INGRESOS_OCUP_PPAL_NORM = INGRESOS_OCUP_PPAL/CBT/adequi) %>% 
#   group_by(YEAR,TRIMESTER,REGION,situacion) %>%
#   summarise(IOP = weighted.mean(INGRESOS_OCUP_PPAL_NORM,POND_ING_OCUP_PRINC)) %>%
#   mutate(FECHA = as.Date(paste(YEAR,TRIMESTER,'01',sep='-'),format = '%Y-%m-%d')) %>%
#   drop_na() %>%
#   ggplot(aes(x = FECHA, y = IOP,color = situacion)) +
#   geom_pointline() +
#   facet_wrap(~ REGION) +
#   ylab ( 'ING OCUP PPAL / CBT [TOTAL]') -> gg
# 
# pdf('ingresos_region_todos.pdf')
# print(gg)
# dev.off()
# 
# 
# 
# individual_03.hoy %>% 
#   mutate(periodo  = paste(YEAR,TRIMESTER,sep='.')) %>%
#   mutate(INGRESOS_OCUP_PPAL_NORM = INGRESOS_OCUP_PPAL/CBT/adequi) %>% 
#   filter(ESTADO == 'OCUPADE') %>%
#   group_by(YEAR,TRIMESTER,REGION,situacion) %>%
#   summarise(IOP = weighted.mean(INGRESOS_OCUP_PPAL_NORM,POND_ING_OCUP_PRINC)) %>%
#   mutate(FECHA = as.Date(paste(YEAR,TRIMESTER,'01',sep='-'),format = '%Y-%m-%d')) %>%
#   drop_na() %>%
#   ggplot(aes(x = FECHA, y = IOP,color = situacion)) +
#   geom_pointline() +
#   facet_wrap(~ REGION) +
#   ylab ( 'ING OCUP PPAL / CBT [OCUPADES]') -> gg
# 
# pdf('ingresos_region_ocupados.pdf')
# print(gg)
# dev.off()
# 
# 
# individual_03.hoy %>% 
#   group_by(YEAR,TRIMESTER,situacion,REGION) %>%
#   summarise(personas = sum(PONDERA)) %>%
#   drop_na() %>%
#   group_by(YEAR,TRIMESTER,REGION) %>%
#   mutate(prop = personas/sum(personas)) %>%
#   mutate(FECHA = as.Date(paste(YEAR,TRIMESTER,'01',sep='-'),format = '%Y-%m-%d')) %>%
#   ggplot(aes(x = FECHA, y = prop, color = situacion)) +
#   geom_pointline() +
#   facet_wrap(~REGION) +
#   ylab('Proporción de EP nucleo en cada situacion')-> gg
# 
# pdf('prop_situaciones.pdf')
# print(gg)
# dev.off()

############################
# Plot tomado de Informe OCEPP

individual_03.hoy %>%
  mutate(ES_CP = ES_CUENTAPROPISTA_NO_PROFESIONAL | ES_TFSR) %>%
  mutate(ES_OCUP = ESTADO == 'OCUPADE') %>%
  mutate(ES_OCUP_NOEP = ES_OCUP & !ES_CP) %>%
  group_by(ES_CP,ES_OCUP_NOEP,YEAR,TRIMESTER) %>%
  summarise(
    tasa_pobreza = sum(PONDIH[situacion %in% c('pobre','indigente')],na.rm=TRUE)/sum(PONDIH,na.rm=TRUE),
    tasa_indigencia = sum(PONDIH[situacion %in% c('indigente')],na.rm=TRUE)/sum(PONDIH,na.rm=TRUE)
  ) %>%
  mutate(FECHA = as.Date(paste(YEAR,4*TRIMESTER,'1',sep='-'))) %>%
  mutate(CATE = case_when(
    ES_CP ~ 'EP',
    ES_OCUP_NOEP ~ 'Ocupados no EP',
    TRUE ~ 'RESTO'
  )) %>%
  pivot_longer(cols=c(tasa_pobreza,tasa_indigencia),names_to = 'tasa_tipo',values_to = 'tasa') %>%
  drop_na() %>% 
  ggplot(aes( x = FECHA, color = CATE, y = tasa)) +
  geom_pointline() +
  facet_wrap( ~ tasa_tipo) +
  theme_light() +
  theme(legend.position = c(.3,.8)) +
  scale_color_discrete(name = 'Población') -> gg

pdf('tasas_PONDIH.pdf')
print(gg)
dev.off()


individual_03.hoy %>%
  mutate(ES_CP = ES_CUENTAPROPISTA_NO_PROFESIONAL | ES_TFSR) %>%
  mutate(ES_OCUP = ESTADO == 'OCUPADE') %>%
  mutate(ES_OCUP_NOEP = ES_OCUP & !ES_CP) %>%
  group_by(ES_CP,ES_OCUP_NOEP,YEAR,TRIMESTER,REGION) %>%
  summarise(
    tasa_pobreza = sum(PONDIH[situacion %in% c('pobre','indigente')],na.rm=TRUE)/sum(PONDIH,na.rm=TRUE),
    tasa_indigencia = sum(PONDIH[situacion %in% c('indigente')],na.rm=TRUE)/sum(PONDIH,na.rm=TRUE)
  ) %>%
  mutate(FECHA = as.Date(paste(YEAR,4*TRIMESTER,'1',sep='-'))) %>%
  mutate(CATE = case_when(
    ES_CP ~ 'EP',
    ES_OCUP_NOEP ~ 'Ocup no EP',
    TRUE ~ 'RESTO'
  )) %>%
  filter(CATE != 'RESTO') %>%
  pivot_longer(cols=c(tasa_pobreza,tasa_indigencia),names_to = 'tasa_tipo',values_to = 'tasa') %>%
  drop_na() %>% 
  ggplot(aes( x = FECHA, shape = CATE, y = tasa,color = REGION)) +
  geom_pointline() +
  facet_wrap( ~ tasa_tipo) +
  theme_light() +
  theme(legend.position = c(.15,.62)) +
  scale_shape(name = 'Población') -> gg

pdf('tasas_PONDIH_REGION.pdf')
print(gg)
dev.off()


individual_03.hoy %>%
  mutate(ES_CP = ES_CUENTAPROPISTA_NO_PROFESIONAL | ES_TFSR) %>%
  mutate(ES_OCUP = ESTADO == 'OCUPADE') %>%
  mutate(ES_OCUP_NOEP = ES_OCUP & !ES_CP) %>%
  group_by(ES_CP,ES_OCUP_NOEP,YEAR,TRIMESTER) %>%
  summarise(
    tasa_pobreza = sum(PONDIH[situacion %in% c('pobre','indigente')],na.rm=TRUE)/sum(PONDIH,na.rm=TRUE),
    tasa_indigencia = sum(PONDERA[situacion %in% c('indigente')],na.rm=TRUE)/sum(PONDERA,na.rm=TRUE)
  ) %>%
  mutate(FECHA = as.Date(paste(YEAR,4*TRIMESTER,'1',sep='-'))) %>%
  mutate(CATE = case_when(
    ES_CP ~ 'EP',
    ES_OCUP_NOEP ~ 'Ocupados no EP',
    TRUE ~ 'RESTO'
  )) %>%
  pivot_longer(cols=c(tasa_pobreza,tasa_indigencia),names_to = 'tasa_tipo',values_to = 'tasa') %>%
  drop_na() %>% 
  filter(YEAR > 2015) %>%
  ggplot(aes( x = FECHA, color = CATE, y = tasa)) +
  geom_pointline() +
  facet_wrap( ~ tasa_tipo) +
  theme_light() +
  theme(legend.position = c(.3,.8)) +
  scale_color_discrete(name = 'Población') -> gg
pdf('tasas_PONDERA.pdf')
print(gg)
dev.off()
