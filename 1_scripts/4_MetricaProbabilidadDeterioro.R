## Construction of the Deterioration Probability Metric
#### libraries ----
library(tidyverse)
library(magrittr)
library(data.table)
library(stringr)
library(lubridate)
library(xts)
library(imputeTS)

setwd("C:/Users/gnovoa/Desktop/Santiago/Riesgos/Proyectos/CreditRisk_Model/proyecto/1_datos")
load("1_LecturaBases/1_LecturaBases.RData")
#### Rating ---- 
# read pds from transition matrix (modify matrix to work without WR) and add them to each observation in time by its rating ----
mat <- read_csv("csv/matriz_trans_moodys.csv")
Pmat <- as.matrix(mat[2:23]/100)
P_df <- as.data.frame(Pmat) 
WR_AAA <- P_df[["WR"]][[1]]
Pmat <- P_df %>% 
    dplyr::mutate(Default = ifelse(WR<WR_AAA, Default, Default + (WR - WR_AAA)*(Default+Ca))) %>% 
    dplyr::select(-c(WR)) %>% 
    as.matrix()
Pmat <-Pmat[1:21,1:21]/rowSums(Pmat[1:21,1:21])
colnames(Pmat) <- c("AAA", 
                    "AA+", "AA", "AA-",
                    "A+", "A", "A-",
                    "BBB+", "BBB", "BBB-",
                    "BB+", "BB", "BB-",
                    "B+", "B", "B-",
                    "CCC+", "CCC", "CCC-",
                    "CC", "C")
pmat_pd <- Pmat %>% 
  as.data.frame() %>% 
  dplyr::mutate(Rating = factor(c("AAA", 
                                  "AA+", "AA", "AA-",
                                  "A+", "A", "A-",
                                  "BBB+", "BBB", "BBB-",
                                  "BB+", "BB", "BB-",
                                  "B+", "B", "B-",
                                  "CCC+", "CCC", "CCC-",
                                  "CC", "C"), 
                                levels = c("AAA", 
                                           "AA+", "AA", "AA-",
                                           "A+", "A", "A-",
                                           "BBB+", "BBB", "BBB-",
                                           "BB+", "BB", "BB-",
                                           "B+", "B", "B-",
                                           "CCC+", "CCC", "CCC-",
                                           "CC", "C")),
                                pd = CC + C) %>% 
                  dplyr::select(Rating, pd)
# change A+ and BBB+ to make relation monotone
pmat_pd <- pmat_pd %>% 
  dplyr::mutate(pd_aux = ifelse(Rating %in% c("A+","BBB+"), 
                                NA_real_, 
                                pd))
pmat_ts <- ts(pmat_pd$pd_aux)
pmat_ts <- pmat_ts %>% na.interpolation(option = "linear") %>% as.double()
pmat_pd$pd <- pmat_ts
# join read_ratings with pmat_pd to add pd to each observation
read_ratings <- read_ratings %>% 
  dplyr::left_join(pmat_pd %>% 
                     dplyr::select(Rating,pd))
# Model T (= times to "jump" outside grade) as an exponential process ----
exp_mean_cens <- read_csv("csv/media_cens.csv")

# Model the conditional probability of change P(new_rating|change_of_rating) as a multinomial variable (with the benchmarks historical data) ----
ratings_arrange <- read_ratings %>% 
  dplyr::filter(!is.na(Date)) %>% 
  dplyr::arrange(CompanyName, ISIN, CIQ_ID, Date) %>% 
  dplyr::filter(!is.na(Rating))
ratings_arrange <- ratings_arrange %>% 
  dplyr::group_by(CIQ_ID) %>% 
  dplyr::mutate(r_past = lag(Rating,1),
                r_post = lead(Rating,1)) %>% 
  dplyr::mutate(r_change = ifelse(Rating == r_past| is.na(r_past),0,1))
# multinomial transition probability matrix
multinom_trans_mat <- ratings_arrange %>% 
  dplyr::ungroup() %>% 
  dplyr::filter(r_change == 1) %>% 
  dplyr::select(r_past, Rating) %>% 
  table()
multi_tm_prob <- multinom_trans_mat/rowSums(multinom_trans_mat)
# create multinomial matrix from moodys transition matrix
n_ch <- rbind(data.frame(Rating = "AAA", n = 0),
              ratings_arrange %>% 
  dplyr::ungroup() %>% 
  dplyr::group_by(Rating) %>% 
  dplyr::filter(r_change == 1) %>% 
  dplyr::summarise(n = n()))
moodys_tm <- matrix(0, nrow = nrow(Pmat), ncol = ncol(Pmat))
for(i in 1:nrow(Pmat)){
  for(j in 1:ncol(Pmat)){
    if(!(i==j)){
      moodys_tm[i,j] <- Pmat[i,j]*100*(n_ch[i,2]+1)
    }
  }
}
moodys_tm_prob <- moodys_tm/rowSums(moodys_tm)
join_multinom_matrix <- matrix(0,nrow = nrow(multi_tm_prob), ncol = ncol(multi_tm_prob))
for(i in 1:(nrow(multi_tm_prob)-1)){
  for(j in 1:ncol(multi_tm_prob)){
    join_multinom_matrix[i,j] <- (multi_tm_prob[i,j] + moodys_tm_prob[i,j])/2
  }
}
join_multinom_matrix[21,] <- (multi_tm_prob[21,] + Pmat[21,])/2
#### EWMA ----
# construct variables quarters_to_change, quarters_since_change, next_rating, previous_rating
#### Bonds valuation ----
# load read_bonds and add perc to matrix to get "new starting rating"
load("3_MetricaSpreadBonos/3_MetricaSpreadBonos.RData")
#### Simulation of Time to "Deterioration"
# Use BB- and B+ as barriers
