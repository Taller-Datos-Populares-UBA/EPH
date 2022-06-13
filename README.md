# Tablero para la Economía Popular

En este repositorio, construiremos un tablero que permita visualizar de forma dinámica la evolución de la economía popular, con base en la información de la Encuesta Permanente de Hogares (EPH) y información provista por el Ministerio de Trabajo y Seguridad Social (MTEySS).

En el repositorio:

- El primer y segundo notebook son analisis exploratorios de la información disponible en la EPH.
- `ep_funciones.R` guarda las funciones a ser usadas por los otros scripts.
- `variables_ep.csv` es un diccionario que indica como renombrar las variables descargadas.
- `prepara_ep.R` descarga y renombra las variables para el analisis de la economía popular
- `graficos_para_automatizar.R` tomar fuente en los scripts anteriores para construir los gráficos del informe del a OCEPP.
