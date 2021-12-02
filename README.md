# "Examen - Analisis estadisticos en r UAH"

El presente proyecto trata sobre una análisis descriptivo de las representaciones institucionales y participativas de la democracia en Chile.

El proyecto se enmarca dentro de la evaluación final del Curso "Análisis de datos estadísticos en r" de la Universidad Alberto Hurtado impartido el segundo semestre del 2021 por la Profesora Valentina Andrade y lxs ayudantes Dafne Jaime y Nicolás Godoy.


# Descripción de la base de datos

La base de datos utilizada para este proyecto, fue la Encuesta Latinobarómetro 2020.

El n origina corresponde a 20204 casos. 

Dicha base fue procesada para poder ser analizada en función de una submuestra dada por los casos asociados solamente a Chile, excluyendo del análisis a los demás países de América Latina y El Caribe. Quedando en un n de 1200 casos.

La documentación revisada fue el Manual metodológico asociado a la muestra de Chile, para observar el diseño muestral del país.


[link](https://www.latinobarometro.org) 

# Notas sobre el procesamiento de datos

El script de procesamiento utilizado quedó alojado en la carpeta R bajo el nombre "data_proc.r"

La principal observación del procesamiento es que todas las variables utilizadas debieron ser procesadas. Esto por la existencia de categorías que ensuciaban el posterior análisis.

En cuanto a los parámetro poblacionales derivados de la submuestra, se encuentra reportados (pero ocultos) en el archivo "Examen.rmd". Esto se hizo debido a un problema detectado en el análisis cruzado, en el cual no coincidían las filas y columnas para reportar la distribución poblacional de los cruces de variables. 







