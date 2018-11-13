## Testing the model with portfolio data and 2018 data
#### libraries ----
library(tidyverse)
library(magrittr)
library(stringr)
library(xts)
library(glmnet)
library(lme4)
library(lmerTest)
library(purrr)
library(broom)
library(GGally)
library(rpart)
library(rpart.plot)
library(lubridate)
#### databases with 2018 information ----
db_model_s %>% 
  dplyr::filter(tat == "pred")
#### summary ----
#### graphics ----