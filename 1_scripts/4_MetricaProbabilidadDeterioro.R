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
names(exp_mean_cens) <- c("Rating", "t")
exp_mean_cens$Rating <-  c("AA+","AA","AA-",
                           "A+","A","A-",
                           "BBB+","BBB","BBB-",
                           "BB+","BB","BB-",
                           "B+","B","B-",
                           "CCC+","CCC","CCC-",
                           "CC","C", "NA_Rating")
exp_mean_cens <- rbind(data.frame(Rating = "AAA", t = 7*4),exp_mean_cens)
# the parameters for the distributions were extracted from the benchmark transition matrix data and using an assumption of censored observations
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
# the decision to give the same weight to moodys_tm data and historical benchmark data was made to make easier the process, 
# in the future it would be useful to analyse (with a bayesian framework) this assumptions
# Create a simulation matrix that models how the distributions of time until "Default" (in our case the barrier will be set in B ratings) behave by Rating ----
n_sim <- 100000
sim_distr <- matrix(data = NA_real_, nrow = nrow(exp_mean_cens)-1, ncol = n_sim)
# sim_distr %>% View()
set.seed(12508902)

aux_mat_post <- t(join_multinom_matrix)
for(i in 1:(nrow(exp_mean_cens)-1)){
  for(j in 1:n_sim) {
    n_iter <- 1
    sim_bool  = T
    t_aux <- 0
    unif_aux <- 0
    sim_t <- 0
    i_aux <- i
    while(sim_bool){
      k <- 1
      t_aux <- rexp(n = 1, rate = 1/exp_mean_cens$t[i_aux])
      unif_aux <- runif(n = 1, min = 0, max = 1)
      aux_prob <- aux_mat_post[k,i_aux]
      while(unif_aux>aux_prob){
        k <- k + 1
        aux_prob <- aux_prob + aux_mat_post[k,i_aux]
        if(k > 20){aux_prob <- 10}
      }
      i_aux <- k
      sim_t <- t_aux + sim_t
      n_iter <- n_iter + 1
      if(k >=15 | n_iter > 1000){
        # k = 15 is the same as having a B rating 
        sim_bool <- F
      }
    }
    sim_distr[i,j] <- sim_t
  }
} 
# transform to dataframe
df_sim_distr <- data.frame(Rating = exp_mean_cens$Rating[-22], sim = sim_distr %>% as.data.frame)
# #**
# df_sim_distr %>% 
#   tidyr::gather(value = valor, key = iteracion, -Rating) %>%
#   ggplot(aes(x = valor, colour = Rating, fill = Rating)) +
#   geom_density(show.legend = F, alpha = 0.7) + theme_bw() + facet_wrap(~Rating, scales = "free_y")
# #**
# df_sim_distr %>% 
#   tidyr::gather(value = valor, key = iteracion, -Rating) %>%
#   dplyr::group_by(Rating) %>%
#   dplyr::summarise(
#     min = min(valor),
#     # c05 = quantile(x = valor,probs = c(0.005)),
#     c1 = quantile(x = valor,probs = c(0.01)),
#     # c15 = quantile(x = valor,probs = c(0.015)),
#     # c2 = quantile(x = valor,probs = c(0.02)),
#     # c25 = quantile(x = valor,probs = c(0.025)),
#     # c3 = quantile(x = valor,probs = c(0.03)),
#     # c35 = quantile(x = valor,probs = c(0.035)),
#     # c4 = quantile(x = valor,probs = c(0.04)),
#     # c45 = quantile(x = valor,probs = c(0.045)),
#     c5 = quantile(x = valor,probs = c(0.05)),
#     c10 = quantile(x = valor,probs = c(0.1)),
#     c20 = quantile(x = valor,probs = c(0.2)),
#     c30 = quantile(x = valor,probs = c(0.3)),
#     c40 = quantile(x = valor,probs = c(0.4)),
#     median = quantile(x = valor,probs = c(0.5)),
#     c75 = quantile(x = valor,probs = c(0.75)),
#     sd = sd(valor)) %>%
#   as.data.table()


#### EWMA ----
# construct variables  next_rating, previous_rating and add diff from pd to create ewma ----
start_date_ratings <- read_ratings %>% 
  dplyr::arrange(CompanyName, ISIN, CIQ_ID, Date) %>% 
  dplyr::group_by(CompanyName) %>% 
  dplyr::filter(!is.na(Date)) %>% 
  dplyr::filter(!is.na(Rating)) %>% 
  dplyr::summarise(start_rating = min(DateQ, na.rm = T))
db_ratings_arr <- read_ratings %>% 
  dplyr::left_join(start_date_ratings) %>% 
  dplyr::filter(DateQ>=start_rating) %>% 
  dplyr::arrange(CompanyName, ISIN, CIQ_ID, Date)
db_ratings_NA <- db_ratings_arr %>% 
  dplyr::mutate(Rating_NA = factor(ifelse(is.na(Rating), "NA_Rating", Rating %>% as.character()),
                                   levels = c("AAA",
                                              "AA+","AA","AA-",
                                              "A+","A","A-",
                                              "BBB+","BBB","BBB-",
                                              "BB+","BB","BB-",
                                              "B+","B","B-",
                                              "CCC+","CCC","CCC-",
                                              "CC","C", "NA_Rating")))
pre_rating_aux <- rep("NA_Rating", nrow(db_ratings_NA))
pre_pd_aux <- rep(NA_real_, nrow(db_ratings_NA))
for(i in 2:nrow(db_ratings_NA)){
  if( db_ratings_NA$DateQ[i] > db_ratings_NA$start_rating[i] ){
    if( db_ratings_NA$Rating_NA[i] == db_ratings_NA$Rating_NA[i-1] ){
      pre_rating_aux[i] <- pre_rating_aux[i-1]
      pre_pd_aux[i] <- pre_pd_aux[i-1]
    }else{
      pre_rating_aux[i] <- db_ratings_NA$Rating_NA[i-1] %>% as.character()
      pre_pd_aux[i] <- db_ratings_NA$pd[i-1]
    } 
  }
}
db_ratings_NA$prev_rating <- pre_rating_aux
db_ratings_NA$pd_aux <- pre_pd_aux
db_ratings_NA <- db_ratings_NA %>% 
  dplyr::mutate(pd_ewma_aux = ifelse(is.na(Rating),pd_aux, pd))
db_ratings_NA <- db_ratings_NA %>% 
  dplyr::group_by(CIQ_ID) %>% 
  dplyr::mutate(pd_diff = ifelse(DateQ == start_rating, 0, pmax(pd_ewma_aux - lag(pd_ewma_aux),0)))

# calculate ewma of pd_diff with exponentialy weighting of pd_diff ----
lambda_ewma <- 0.4
# lambda_ewma is one of the parameters that should be analysed in the future to optimize the results 
db_ratings_NA$ewma <- rep(0.0,nrow(db_ratings_NA))
for(i in nrow(db_ratings_NA):1){
  sum_ewma <- 0
  dist_date_ewma <- as.integer(round(4*(db_ratings_NA$DateQ[i]-db_ratings_NA$start_rating[i]),0))+1
  for(j in (dist_date_ewma:1)){
    sum_ewma = sum_ewma + db_ratings_NA$pd_diff[i-j+1]*lambda_ewma^(j-1)*(1-lambda_ewma)
  }
  db_ratings_NA$ewma[i] <- sum_ewma
}

#### Bonds valuation ----
# load read_bonds and add perc to matrix to get "new starting rating" (db_detprob)
load("3_MetricaSpreadBonos/3_MetricaSpreadBonos.RData")
# read_bonds
# db_ratings_NA
db_detprob <- db_ratings_NA %>%
  dplyr::ungroup() %>% 
  dplyr::left_join(read_bonds %>% 
                     dplyr::ungroup() %>% 
                     dplyr::mutate(DateQ = as.yearqtr(DateQ)) %>% 
                     dplyr::select(CIQ_ID,Rating, DateQ, BondMaturity, TtM, perc_delta, dec_delta)) %>% 
  dplyr::mutate(dec_delta = ifelse(is.na(dec_delta), 0, dec_delta)) %>% 
  unique()



#### Simulation of Time to "Deterioration" ----
# Use BB- and B+ as barriers

db_detprob <- db_detprob %>% 
  rowwise() %>% 
  dplyr::mutate(pd_metric = ifelse(is.na(pd), sum(c(pd_aux,ewma), na.rm = T), sum(c(pd,ewma), na.rm = T)),
                pd_metric = ifelse(pd_metric>1,1,pd_metric)) 
# function to create diff between rating and lower rating (to add delta) ----
pd_diff_calc <- function(rating_upper, ratings = pmat_pd$Rating %>% as.character(), pds = pmat_pd$pd){
  if(rating_upper %>% is.factor()){
    r_up <- rating_upper %>% as.character()
  }else{
    r_up <- rating_upper
  }
  i_grade <- which(ratings == r_up)
  if(i_grade %>% length() == 0){
    i_grade <- 21
  }
  pd_diff <- 0
  if(i_grade < 21){
    pd_diff <- pds[i_grade+1] - pds[i_grade]
  }
  return(pd_diff)
}
db_detprob <- db_detprob  %>% 
  dplyr::mutate(delta_aux_diff = pd_diff_calc(rating_upper = Rating %>% as.character()),
                delta = dec_delta * delta_aux_diff,
                delta_pd = pd_metric + delta,
                delta_pd = ifelse(delta_pd>1,1,delta_pd)) %>% 
  dplyr::ungroup()
# function to create interpolation alpha (for simulations) and upper grades to interpolate the pds ----
r_up <- function(pd_inter, pmat = pmat_pd[,1:2], output = 1){
  i_inter <- which(round(pd_inter,5)-pmat$pd<=0)[1]
  if(i_inter == 1){
    r_down <- as.character(pmat[i_inter,1])
    adj_up <- 0
  }else{
    r_down <- as.character(pmat[i_inter-1,1])
    range_inter <- abs(pmat[i_inter-1,"pd"]-pmat[i_inter,"pd"])
    adj_up <- as.numeric((pmat[i_inter,"pd"] - pd_inter)/range_inter)
  }
  ret_obj <- NA
  if(output == 1| output == "r_down"){
    ret_obj <- r_down
  }else{
    if(output == 2|output == "adj_up"){
      ret_obj <- adj_up
      }
  }
  return(ret_obj)
}
db_detprob <- db_detprob %>% 
  rowwise() %>% 
  dplyr::mutate(r_up_sim = r_up(pd_inter = delta_pd, output = 1),
                inter_adj = 1 - r_up(pd_inter = delta_pd, output = 2)) %>% 
  dplyr::ungroup()

# function to create the actual pd from the simulation interpolation process ----
tV_median <- db_detprob$TtM %>% median(na.rm=T)
df_prob_sim_f <- df_sim_distr[,-1] %>% t() %>% as.data.frame()
colnames(df_prob_sim_f) <- df_sim_distr[,1]
# df_prob_sim_f %>% glimpse()

prob_sim <- function(rating_upper = "BBB-", inter_rating = 1, tV_aux = tV_median, db_sim_aux = df_prob_sim_f){
  if(is.na(tV_aux)){
    tV_aux <- tV_median
  }
  if(rating_upper == "NA_Rating"){
    prob_upper <- sum(db_sim_aux[["NA_Rating"]]<tV)/nrow(db_sim_aux)
    prob_lower <- prob_upper
    prob_inter <- prob_upper
  }else{
    num_upper <- which(names(db_sim_aux)==rating_upper)
    if(is.na(num_upper)){
      message("Rating not found, BBB- will be used as proxy \n")
      num_upper <- which(names(db_sim_aux)=="BBB-")
    }
    calif_aux <- names(db_sim_aux)[num_upper]
    calif_aux_lower <- names(db_sim_aux)[num_upper+1]
    prob_upper <- sum(db_sim_aux[[calif_aux]]<tV_aux)/nrow(db_sim_aux)
    prob_lower <- sum(db_sim_aux[[calif_aux_lower]]<tV_aux)/nrow(db_sim_aux)
    prob_inter <- prob_upper*(1-inter_rating) + prob_lower*(inter_rating)
  }
  prob_inter
}

db_detprob$prob_det <- 0.0
for(i in 1:nrow(db_detprob)){
  db_detprob$prob_det[i] <- prob_sim(rating_upper = db_detprob$r_up_sim[i],
                                         inter_rating = db_detprob$inter_adj[i], 
                                         tV_aux = db_detprob$TtM[i])
}

db_detprob %>% 
  dplyr::ungroup() %>% 
  dplyr::filter(Rating_NA %in% c("A-",
                                 "BBB+","BBB","BBB-",
                                 "BB+","BB","BB-",
                                 "B+","B","B-",
                                 "CCC+")) %>%
  tidyr::gather(key = det_factor, value = det_value, c(pd,ewma,delta, delta_pd, TtM,prob_det)) %>% 
  ggplot(aes(x = Date, y = det_value, colour = Rating_NA)) +
  geom_point(alpha = 0.15, size = 1.5, show.legend = F) + 
  facet_grid(det_factor~Rating_NA , scales = "free") + 
  theme_bw() + 
  ylab("") + scale_x_date(date_breaks = "1 year", date_labels = "%y") +
  theme(axis.text.x = element_text(size = 7, angle = 90), 
        strip.text = element_text(size = 5))
#### database with output complete ----
db_detprob_bis <- db_detprob %>% 
  dplyr::select(CompanyName, ParentCompany, CIQ_ID, Date, DateQ, Rating, Rating_NA, BondMaturity, TtM, prob_det, start_rating)
save(db_detprob_bis, file = "4_MetricaProbabilidadDeterioro/4_MetricaProbabilidadDeterioro.RData")
# load(file = "4_MetricaProbabilidadDeterioro/4_MetricaProbabilidadDeterioro.RData")

