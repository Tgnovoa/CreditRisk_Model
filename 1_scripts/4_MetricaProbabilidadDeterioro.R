## Construction of the Deterioration Probability Metric
#### libraries ----
library(tidyverse)
library(magrittr)
library(data.table)
library(stringr)
library(lubridate)
library(xts)


#### Rating ----
# read pds from transition matrix (modify matrix to work without WR) and add them to each observation in time by its rating
# Model T (= times to "jump" outside grade) as an exponential process
# Model the conditional probability of change P(new_rating|change_of_rating) as a multinomial variable (with the benchmarks historical data)
#### EWMA ----
# construct variables quarters_to_change, quarters_since_change, next_rating, previous_rating
#### Bonds valuation ----
# load read_bonds and add perc to matrix to get "new starting rating"
#### Simulation of Time to "Deterioration"
# Use BB- and B+ as barriers
