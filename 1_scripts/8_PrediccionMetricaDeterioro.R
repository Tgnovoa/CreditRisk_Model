##    Prediccion en T+1 de la metrica de deterioro para diferentes empresas del benchmark 
##  a traves del modelo de riesgo de credito.
#### paqueteria ----
library(tidyverse)
library(magrittr)
library(stringr)
library(xts)
#### Calculo prediccion ----
# Para hacer la prediccion de la metrica de deterioro es necesario un modelo ya generado,
# es decir, las betas por grupo que se esta utilizando (objeto clase glmm o algo similar),
# ademas de las bases completas del periodo T+1
# Es importante guardar los resultados en una tabla independiente (.csv) que
# se encuentre en un folder de Resultados con la fecha/periodo para el cual se 
# hicieron las predicciones.
#### Resultados Benchmark ----
#### Resultados Portafolio ----
