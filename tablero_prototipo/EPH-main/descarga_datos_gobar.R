library(httr)
library(jsonlite)
library(tidyverse)


url <- paste0(
  'https://apis.datos.gob.ar/series/api/series?ids=',
  paste(
    '165.1_AAUH_0_0_3',
    '57.1_SMVMM_0_M_34',
    '145.3_INGNACNAL_DICI_M_15',
    '151.1_IPENDIEDAD_2012_M_39',
    '151.1_IPENDIEDAD_2012_M_48',
    sep=','
    ),
  '&format=json&metadata=full',
  '&start_date=2003-01-01',
  paste(
    '&end_date=',
    Sys.Date(),
    sep=''),
  '&limit=1000'
  )



page <- GET(url) # API request
status_code(page) # # Check that the call is successful

jsonAUHSMVM <- fromJSON(url)

datos <- jsonAUHSMVM$data  %>% as.data.frame(stringsAsFactors=FALSE)

colnames(datos) = c('Fecha', 
                    'AUH',# Asignación universal por hije
                    'SMVM',# Salario mínimo vital y movil
                    'IPC', # Indice Precios al Consumidor
                    'TIA_CE', # Trabajadorxs independientes Autonomxs con estacionalidad
                    'TIMS_CE') # Trabajadorxs independientes con monotributo social con estacionalidad

datos %>% na.omit %>% glimpse

datos %>% 
  mutate(
    Fecha = as.Date(Fecha),
    AUH = as.numeric(AUH),
    SMVM = as.numeric(SMVM),
    IPC = as.numeric(IPC),
    TIA_CE = as.numeric(TIA_CE),
    TIMS_CE = as.numeric(TIMS_CE)
  ) %>% 
  mutate(deflactor = first(na.omit(IPC))/IPC) %>%
  mutate(SSC = SMVM/2) ->
  datos

datos %>% na.omit %>% glimpse


datos %>% 
  ggplot( aes( x = Fecha, y = AUH)) +
  geom_line()

datos %>% 
  ggplot( aes( x = Fecha, y = AUH*deflactor)) +
  geom_line() +
  xlim(as.Date(c('2016-12-01','2022-01-01')))


datos %>% 
  ggplot( aes( x = Fecha, y = SSC*deflactor)) +
  geom_line() +
  xlim(as.Date(c('2016-12-01','2022-01-01')))


datos %>% 
  mutate(SSC = SSC*deflactor,
        AUH = AUH*deflactor) %>%
  mutate( SSC = 100*SSC/first(na.omit(SSC)),
          AUH = 100*AUH/first(na.omit(AUH))) %>%
  ggplot( aes( x = Fecha)) +
  geom_line(aes(y = SSC, color='SSC')) +
  geom_line(aes(y = AUH, color='AUH')) +
  xlim(as.Date(c('2016-12-01','2022-01-01'))) +
  ylab('Monto [Pesos en términos reales]') +
  theme(legend.position = c(.9,.9)) +
  scale_color_discrete(name='Asistencia social') +
  ggtitle("Base dic. 2016 = 100")


datos %>% 
  ggplot( aes( x = Fecha)) +
  geom_line(aes(y = TIA_CE, color='Autonomxs')) +
  geom_line(aes(y = TIMS_CE, color='Con MS')) +
  xlim(as.Date(c('2011-12-01','2022-01-01'))) +
  ylab('Miles de personas') +
  theme(legend.position = c(.6,.1)) +
  scale_color_discrete(name='Monotributo')
