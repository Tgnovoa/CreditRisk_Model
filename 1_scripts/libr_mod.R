# instalar paquetes utilizados en modelo #
list.of.packages <- c("tidyverse","magrittr","stringr","xts","xts","glmnet","lme4","lmerTest","purrr","broom","GGally","rpart","rpart.plot","lubridate","pracma")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = T)
