## Lectura de bases de datos 
#### paqueteria ----
library(tidyverse)
library(magrittr)
library(stringr)
library(xts)
#### establecer nombres bases ----
# Se tienen que leer 5 bases:
#   - Datos financieros ~ db_fin
#   - Datos de las metricas de moodys ~ db_EDF
#   - Datos de precios (y caracteristicas) de los bonos ~ db_bond
#   - Datos de calificaciones (de moodys) ~ db_grade
#   - Datos de los sectores de cada empresa ~ db_sector
#
#   Por ultimo, se sugiere agregar una clave al inicio de los nombres de las bases para
# identificar el proyecto (para este caso, credit risk model), por lo tanto, los nombres
# de las bases a obtener seran : CRM_db_fin, CRM_db_EDF, CRM_db_bond, CRM_db_grade, CRM_db_sector

#### lectura de las bases ----
# Las bases necesarias tendran que estar guardadas en el folder 1_datos del proyecto:
## Financieros -
## Moodys -
## Bonos -
## Calificaciones -
## Sectores -