## Construction of the expected default spread as a function of bond price
## #from :  
##        "Using Yield Spreads to Estimate Expected Returns on Debt and Equity"
##        - Ian A. Cooper (LBS), - Sergei A. Davydenko (LBS)

#### libraries ----
library(tidyverse)
library(magrittr)
library(stringr)
library(xts)
library(nleqslv)


setwd("C:/Users/gnovoa/Desktop/Santiago/Riesgos/Proyectos/CreditRisk_Model/proyecto/1_scripts")
#### read tables ----
na_excel <- c("#NA", "#N/A", "#VALOR!")
na_ciq <- c("NM", "(Invalid Identifier)", " (Invalid Identifier) ")
na_moodys <- c("You do not have permission to view this data.")
na_read <- c(na_excel, na_ciq, na_moodys)
path_csv <- "../1_datos/csv/Bonds/"
dir_csv <- paste0(path_csv,dir(path_csv))
## AAA yields
dir_AAA <- dir_csv[dir_csv %>% str_detect(pattern = "AAA")]
AAA_corp <- read_csv(dir_AAA, skip = 3, col_types = list(Date = col_date(format = "%m/%d/%Y")), col_names = c("Date", paste0("y",1:30)))
AAA_corp <- AAA_corp %>% dplyr::mutate(DateQ = as.yearqtr(Date))
## Benchmark bonds
dir_spread <- dir_csv[dir_csv %>% str_detect(pattern = "Spread")]
bonds <- read_csv(dir_spread[1], skip = 2, col_types = list(Date = col_date(format = "%m/%d/%Y"), BondMaturity = col_date(format = "%m/%d/%Y"),Count = col_skip(), Index = col_skip()), na = c(na_read,0))
for(i in 2:(length(dir_spread)-1)){
  bonds_aux <- read_csv(dir_spread[i], 
                        skip = 2, 
                        col_types = list(Date = col_date(format = "%m/%d/%Y"), BondMaturity = col_date(format = "%m/%d/%Y"), Count = col_skip(), Index = col_skip()), 
                        na = c(na_read,0))
  bonds <- rbind(bonds, bonds_aux)
}
names(bonds) <- bonds %>%
  names() %>% 
  gsub(pattern = " ", replacement = "")
bonds <- bonds %>% 
  dplyr::filter(!ISIN=="0")
bonds[sapply(bonds, is.character)] <- lapply(bonds[sapply(bonds, is.character)], factor)
# bonds %>% glimpse()

## CVA bonds
bonds_port <- read_csv(dir_spread[length(dir_spread)],
                       skip = 2, 
                       col_types = list(Date = col_date(format = "%m/%d/%Y"), BondMaturity = col_date("%m/%d/%Y"), Index = col_skip(), Count = col_skip()),
                       # col_names = c("Ticker", "ISIN", "CompanyName", "ParentCompany", "CIQ_ID","Date", "BondYield", "TotalDebt", "Equity", "BondMaturity", "TtM"),
                       na = c(na_read,0))
names(bonds_port) <- bonds_port %>% 
  names() %>% 
  gsub(pattern = " ", replacement = "")

bonds_port[sapply(bonds_port, is.character)] <- lapply(bonds_port[sapply(bonds_port, is.character)], factor)
bonds_port$port <- "CV"
bonds$port <- "Benchmark"

## one bonds database
bonds_agg <- rbind(bonds_port, bonds)
bonds_agg <-
  bonds_agg %>% 
  dplyr::filter(!ISIN=="0")

#### input variables : ----
# Y = yield spreads
# V = the value of the assets
# B = the value of the debt
# F = the promised debt payment
# r = treasury yield rates (and curve)
# sigma_e = equity volatility (option-implied volatility /or/ historical returns data)
dir_sigma <- dir_csv[dir_csv %>% str_detect(pattern = "Volatility")]
volatility <- read_csv(dir_sigma[1],skip = 2, col_types = list(Date = col_date(format = "%m/%d/%Y"), Index = col_skip(), Count = col_skip()), na = c(na_read,0))
for(i in 2:(length(dir_sigma)-1)){
  vol_aux <- read_csv(dir_sigma[i],
                      skip = 2,
                      col_types = list(Date = col_date(format = "%m/%d/%Y"), Index = col_skip(), Count = col_skip()), 
                      na = c(na_read,0))
  volatility <- rbind(volatility,vol_aux)
}
vol_port <- read_csv(dir_sigma[length(dir_sigma)],skip = 2, col_types = list(Date = col_date(format = "%m/%d/%Y"),X12 = col_skip(),X13 = col_skip(),X14 = col_skip(), Index = col_skip(), Count = col_skip()), na = c(na_read,0))
vol_agg <- rbind(volatility, vol_port)
vol_agg <- vol_agg %>% 
  dplyr::filter(!is.na(Date))
names(vol_agg) <- vol_agg %>% 
  names() %>% 
  gsub(pattern = " ", replacement = "")
vol_agg[sapply(vol_agg, is.character)] <- lapply(vol_agg[sapply(vol_agg, is.character)],factor) 
# vol_agg %>% glimpse()
# vol_agg %>% summary()
#### estimate AAA-rated bond spreads ----
# Construct time matched avg. AAA-rated bond spreads and subtract them from the bond spreads mentioned before
## create AAA-rated bond spreads function 
yield_to_spread <- function(yield, TtM, date_AAA){
  if(yield==0||TtM<=0||is.na(date_AAA)||is.na(TtM)||is.na(yield)){
    spread <- 0
  }
  else{
    date_qtr <- as.yearqtr(date_AAA)
    AAA_date <- AAA_corp %>% dplyr::filter(DateQ == date_qtr)
    mat_min <- max(c(min(c(as.double(floor(TtM)),29)),1))
    mat_max <- min(c(as.double(ceiling(TtM)),30))
    mat_inter <- min(c(as.double(TtM) - mat_min,1))
    AAA_short <- AAA_date[1,mat_min+1]
    AAA_long <- AAA_date[1,mat_max+1]
    AAA_inter <- AAA_long*(mat_inter) + AAA_short*(1-mat_inter)
    spread <- yield - as.double(AAA_inter)
  }
  as.double(spread)
}
# yield input in range(0,1)
bonds_agg <- bonds_agg %>% 
  rowwise() %>% 
  dplyr::mutate(spread = yield_to_spread(yield = BondYield/100, TtM = TtM, date_AAA = Date))
# Add sigma to each observation for next step
bonds_agg <- bonds_agg %>% 
  dplyr::left_join(vol_agg %>% 
                     dplyr::select(CIQ_ID, Date, StockPriceTotalReturn, EquityVolatility_3m))
# bonds_agg %>% glimpse()
# bonds_agg %>% summary()
#### variable transformation ----
## w ~ the fims levarage
#     w = B/V    ~ (TD/TD+Eq)
## s ~ the promised yield spread
#     s = 1/T ln(F/B) - r    ~  yield - r_MAT_t - s_AAAavg_t ~ yield - y_AAAavg_t
## d1 = (-ln(w) - (s - (sigma**2) / 2)*T)/sigma * sqrt(T)
## d2 = d1 - sigma*sqrt(T)
bonds_agg <- unique(bonds_agg)
# the decision to limit the ranges of outliers was based on outside assumptions and should be revisited in future revaluations of the model
bonds_agg <- bonds_agg %>% 
  # dplyr::filter(spread>0) %>% 
  dplyr::group_by(CIQ_ID) %>% 
  dplyr::mutate(
    Stock_LQ = lead(StockPriceTotalReturn, 1),
    Equity = pmax(Equity, 0),
    TtM  = pmax(TtM,0),
    s = ifelse(spread>0, spread, 0),
    sigma_e = EquityVolatility_3m/100,
    w = pmin(ifelse(is.na(Equity)|is.na(TotalDebt)|TotalDebt==0|Equity==0,NA_real_,Equity/TotalDebt),5),
    # ln_w = ifelse(w>0, log(w), NA_real_),
    # run_model = !is.na(sum(c(ln_w, spread, EquityVolatility_3m, TtM))),
    # d1 = (-ln_w-(spread-(EquityVolatility_3m**2)/2)*TtM)/EquityVolatility_3m*sqrt(TtM),
    # d2 = d1 - EquityVolatility_3m*sqrt(TtM),
    d_Stock = StockPriceTotalReturn-Stock_LQ,
    rPi_e = ifelse(Stock_LQ == 0, 0,(((StockPriceTotalReturn/Stock_LQ)-1)))*100)
bonds_agg <- bonds_agg %>% dplyr::filter(Date >= as.Date("2007-01-01"))
# bonds_agg %>% summary()
# bonds_agg %>% View()

#### calibrate merton model ----
## solve each one with 5 different values for T to obtain sigma values
# pnorm(-d1)/w + exp(s*T)*pnorm(d2) = 1
# sigma_e = sigma*pnorm(d1)/(1-w)
## use the average of the results (T,sigma) as a seed and solve simultaneously : 
# pnorm(-d1)/w + exp(s*T)*pnorm(d2) = 1
# sigma_e = sigma*pnorm(d1)/(1-w)
## create function for merton model solver that outputs T and sigma
d_fn <- function(w,s,sigma,t){
  d1 <- (-log(w)-(s - (sigma**2)/2)*t)/(sigma*sqrt(t))
  d2 <- d1 - sigma*sqrt(t)
  d <- c(d1,d2)
  return(d)
}
eq1_fn <- function(w,s,sigma,t, sigma_e){
  zero_eq1 <- sigma*(pnorm((-log(w)-(s-(sigma**2)/2)*t)/(sigma*sqrt(t))))/(1-w) - sigma_e
  return(zero_eq1)
}
eq2_fn <- function(w,s,sigma,t){
  zero_eq2 <- pnorm(-(-log(w)-(s - (sigma**2)/2)*t)/(sigma*sqrt(t)))/w + exp(s*t)*pnorm((-log(w)-(s - (sigma**2)/2)*t)/(sigma*sqrt(t))-sigma*sqrt(t)) - 1
  return(zero_eq2)
}
eq_fn <- function(w,s,sigma,t,sigma_e){
  y <- numeric(2)
  y[1] <- eq1_fn(w,s,sigma,t,sigma_e)
  y[2] <- eq2_fn(w,s,sigma,t)
  return(y)
}
merton_init_cond <- function(w, spread, sigma_e, TtM){
  index_ttm <- seq(1, TtM*2+1, TtM/5)
  index_sigma <- seq(sigma_e/2, sigma_e*3/2 , sigma_e/10)
  z <- data.frame(z1 = rep(NA_real_, length(index_ttm)),
                  z2 = rep(NA_real_, length(index_ttm)),
                  f1 = rep(NA_real_, length(index_ttm)),
                  f2 = rep(NA_real_, length(index_ttm)),
                  z1_msg = rep(NA, length(index_ttm)),
                  z2_msg = rep(NA, length(index_ttm)))
  for(i in 1:length(index_ttm)){
    eq1_aux <- function(x){
      zero_eq1_aux <- eq1_fn(w = w,s = spread,sigma = x[1],t = index_ttm[i],sigma_e = sigma_e)
      return(zero_eq1_aux)
    }
    eq2_aux <- function(x){
      zero_eq2_aux <- eq2_fn(w = w,s = spread,sigma = x[1],t = index_ttm[i])
      return(zero_eq2_aux)
    }
    x <- c(index_sigma[i])
    z1_aux <- nleqslv(x = x, fn =  eq1_aux, method="Newton", global="none", control=list(stepmax=5,allowSingular=TRUE))
    z2_aux <- nleqslv(x = x, fn =  eq2_aux, method="Newton", global="none", control=list(stepmax=5,allowSingular=TRUE))
    z[i,] <- c(z1_aux$x, z2_aux$x, z1_aux$fvec, z2_aux$fvec, z1_aux$message, z2_aux$message)
  }
  index_start <- which(z$z1_msg == "Function criterion near zero" & z$z2_msg=="Function criterion near zero")
  ttm_start <- index_ttm[index_start] %>% mean()
  z_aux <- z %>% 
    tidyr::gather(key = eq, value = value, c(z1,z2)) %>% 
    dplyr::filter(z1_msg == "Function criterion near zero", z2_msg == "Function criterion near zero") 
  sigma_start <- ifelse(nrow(z_aux)>0,z_aux%>% 
                          dplyr::select(value) %>% 
                          .$value %>% 
                          as.double() %>% 
                          mean(),index_sigma[i] + sigma_e)
  start_merton <- c(sigma_start, ttm_start)
  return(start_merton)
}
merton_solver <- function(w, spread, sigma_e, TtM){
  start_merton <- merton_init_cond(w = w, spread = spread, sigma_e = sigma_e, TtM = TtM)
  if(sum(is.na(start_merton))>0){
    start_merton <- c(sigma_e,TtM)
  }
  eq_aux <- function(x){
    zero_eq_aux <- eq_fn(w = w,s = spread,sigma = x[1],t = x[2],sigma_e = sigma_e)
    return(zero_eq_aux)
  }
  merton <- nleqslv(x = start_merton, fn =  eq_aux, method="Newton", global="dbldog", control=list(stepmax=5,allowSingular=TRUE))
  # merton
  if(merton$message=="Function criterion near zero"){
    ret_merton <- merton$x
  }else{
    ret_merton <- c(0,1)
  }
  return(ret_merton)
}
# #trial run# # 
# w <- bonds_agg$w[i]
# spread <- bonds_agg$s[i]
# sigma_e <- bonds_agg$sigma_e[i]
# TtM <- bonds_agg$TtM[i]
## create df with merton results to transform them
merton <- data.frame(Ticker = bonds_agg$Ticker, ISIN = bonds_agg$ISIN, CIQ_ID = bonds_agg$CIQ_ID, Date = bonds_agg$Date,
                     w = bonds_agg$w, s = bonds_agg$s, sigma_e = bonds_agg$sigma_e, TtM = bonds_agg$TtM, rPi_e = bonds_agg$rPi_e,
                     ESTsigma = NA_real_ , ESTt = NA_real_, 
                     d1 = NA_real_, d2 = NA_real_)

for(i in 1:nrow(merton)){
  merton_aux <- rep(NA_real_,2)
  if(!(merton$s[i] == 0|is.na(merton$w[i])|is.na(merton$s[i])|is.na(merton$sigma_e[i])|merton$TtM[i]==0|is.na(merton$TtM[i]))){
    merton_aux <- merton_solver(w = merton$w[i], spread = merton$s[i], sigma_e = merton$sigma_e[i], TtM = merton$TtM[i])
    merton$ESTsigma[i] <- max(merton_aux[1],1/100)
    merton$ESTt[i] <- min(max(merton_aux[2],1/365),100)
    d_aux <- d_fn(w = merton$w[i], s = merton$s[i], sigma = merton$ESTsigma[i], t = merton$ESTt[i])
    merton$d1[i] <- d_aux[1]
    merton$d2[i] <- d_aux[2]
  }
}
#### obtain expected default spread and default risk premium -----
## rPi ~ risk premia on assets 
#     rPi = Mu - r ~ rPi_e * sigma/sigma_e
## rPi_e ~ expected equity premium
#     rPi_e = rPi*sigma_e/sigma
## delta ~ expected default spread
#     delta = -1/T*ln((exp(rPi-s)*T)*pnorm(-d1-rPi*sqrt(T)/sigma)/w + pnorm(d2 + rPi*sqrt(T/sigma)))
## gamma ~ default risk premium
#     gamma = s - delta
expected_default_spread <- function(w, s, sigma, t, rPi, d1, d2){
  ESTdelta <- -1*(1/t)*log(exp((rPi-s)*t)*pnorm(-d1-rPi*sqrt(t)/sigma)/w + pnorm(d2 + rPi*sqrt(t)/sigma))
  return(ESTdelta)
}
merton <- merton %>% 
  dplyr::filter(!is.na(ESTt)) %>%
  dplyr::group_by(CIQ_ID) %>% 
  dplyr::mutate(ESTrPi_e = exp(mean(log(1+rPi_e), na.rm = T))-1) %>% 
  dplyr::ungroup()
merton <- merton %>% 
  rowwise() %>% 
  dplyr::mutate(rPi = ESTrPi_e * ESTsigma/sigma_e ,
                ESTdelta = expected_default_spread(w = w,s = s,sigma = ESTsigma, t = ESTt, rPi = rPi, d1 = d1, d2 = d2),
                ESTgamma = s - ESTdelta)
# ratings
path_ratings <- "../1_datos/csv/Ratings/"
dir_rating <- dir(path_ratings)
dir_rating_CIQ <- paste0(path_ratings,dir_rating[dir_rating %>% str_detect(pattern = "CIQ")])
ratingsCIQ <- read_csv(dir_rating_CIQ[1], 
                       skip = 2, 
                       col_types = list(Date = col_date(format = "%m/%d/%Y"),Count = col_skip(), Index = col_skip(), X12 = col_skip(), X13 = col_skip(), X14 = col_skip()), 
                       na = c(na_read,0))
for(i in 2:(length(dir_rating_CIQ))){
  rating_aux <- read_csv(dir_rating_CIQ[i], 
                         skip = 2, 
                         col_types = list(Date = col_date(format = "%m/%d/%Y"),Count = col_skip(), Index = col_skip(), X12 = col_skip(), X13 = col_skip(), X14 = col_skip()), 
                         na = c(na_read,0))
  ratingsCIQ <- rbind(ratingsCIQ, rating_aux)
}

dir_rating_Moodys <- paste0(path_ratings,dir_rating[dir_rating %>% str_detect(pattern = "Moodys")])
ratingsMoodys <- read_csv(dir_rating_Moodys[1], 
                       skip = 4, 
                       col_types = list(Date = col_date(format = "%m/%d/%Y"),Count = col_skip()), 
                       na = c(na_read,0))
for(i in 2:(length(dir_rating_Moodys))){
  rating_aux <- read_csv(dir_rating_Moodys[i], 
                         skip = 4, 
                         col_types = list(Date = col_date(format = "%m/%d/%Y"),Count = col_skip()), 
                         na = c(na_read,0))
  ratingsMoodys <- rbind(ratingsMoodys, rating_aux)
}

# ratingsCIQ %>% glimpse()
r_Moodys <- ratingsMoodys %>% 
  dplyr::mutate(DateQ = as.yearqtr(Date)) %>% 
  dplyr::group_by(Isin, DateQ) %>% 
  dplyr::summarise(RatingQ1 = dplyr::last(Rating, order_by = Date),
                   RatingQ2 = ifelse(is.na(RatingQ1),dplyr::nth(Rating, n=2, order_by = Date),RatingQ1),
                   RatingQ = ifelse(is.na(RatingQ2),dplyr::first(Rating, order_by = Date),RatingQ2)) %>% 
  dplyr::ungroup() 
r_Moodys <- r_Moodys %>% 
  dplyr::transmute(ISIN = gsub(Isin, pattern = "isin-", replacement = "I_"),
                   DateQ = DateQ,
                   RatingM = case_when(
                     RatingQ == "Aaa" ~ "AAA",
                     RatingQ == "Aa1" ~ "AA+",
                     RatingQ == "Aa2" ~ "AA",
                     RatingQ == "Aa3" ~ "AA-",
                     RatingQ == "A1" ~ "A+",
                     RatingQ == "A2" ~ "A",
                     RatingQ == "A3" ~ "A-",
                     RatingQ == "Baa1" ~ "BBB+",
                     RatingQ == "Baa2" ~ "BBB",
                     RatingQ == "Baa3" ~ "BBB-",
                     RatingQ == "Ba1" ~ "BB+",
                     RatingQ == "Ba2" ~ "BB",
                     RatingQ == "Ba3" ~ "BB-",
                     RatingQ == "B1" ~ "B+",
                     RatingQ == "B2" ~ "B",
                     RatingQ == "B3" ~ "B-",
                     RatingQ == "Caa1" ~ "CCC+",
                     RatingQ == "Caa2" ~ "CCC",
                     RatingQ == "Caa3" ~ "CCC-",
                     RatingQ == "Ca"~"CC",
                     RatingQ == "C"~"C",
                     T ~ NA_character_)
                   )
ratingsCIQ <- ratingsCIQ %>% 
  dplyr::mutate(DateQ = as.yearqtr(Date),
                RatingAgg = ifelse(is.na(BondRating)|BondRating == "(Capability Needed)",IssuerRating_SP, BondRating),
                RatingCIQ = ifelse(RatingAgg == "(Capability Needed)", NA_character_, RatingAgg)) %>% 
  dplyr::left_join(r_Moodys) %>% 
  dplyr::mutate(Rating = factor(ifelse(is.na(RatingCIQ), RatingM, RatingCIQ), levels = c("AAA",
                                                                                         "AA+","AA","AA-",
                                                                                         "A+","A","A-",
                                                                                         "BBB+","BBB","BBB-",
                                                                                         "BB+","BB","BB-",
                                                                                         "B+","B","B-",
                                                                                         "CCC+","CCC","CCC-",
                                                                                         "CC","C")))

merton_calif <- merton %>% 
  dplyr::mutate(DateQ = as.yearqtr(Date)) %>% 
  dplyr::left_join(ratingsCIQ %>% 
                     dplyr::select(ISIN, DateQ, Rating)) %>% 
  dplyr::ungroup()
# merton_calif %>% 
#   tidyr::gather(key = est, value = value, c(s, ESTgamma, ESTdelta)) %>% 
#   ggplot(aes(x = Date, y = value, colour = Rating)) +
#   facet_wrap(~est, scales = "free_y") + geom_point(alpha = 0.15)
# merton_calif %>% 
#   # dplyr::filter(!is.na(Rating)) %>% 
#   ggplot(aes(x = s, y = ESTdelta, colour = Rating)) +
#   facet_wrap(~Rating, scales = "free_y") + 
#   # geom_point(alpha = 0.2) + 
#   geom_density2d()
# merton_calif %>% 
#   dplyr::ungroup() %>% 
#   dplyr::mutate() %>% 
#   dplyr::group_by(Rating) %>% 
#   dplyr::summarise(min = min(ESTdelta, na.rm = T),
#                    d1 = quantile(ESTdelta, 0.1, na.rm = T),
#                    d2 = quantile(ESTdelta, 0.2, na.rm = T),
#                    d3 = quantile(ESTdelta, 0.3, na.rm = T),
#                    d4 = quantile(ESTdelta, 0.4, na.rm = T),
#                    d5 = quantile(ESTdelta, 0.5, na.rm = T),
#                    d6 = quantile(ESTdelta, 0.6, na.rm = T),
#                    d7 = quantile(ESTdelta, 0.7, na.rm = T),
#                    d8 = quantile(ESTdelta, 0.8, na.rm = T),
#                    d9 = quantile(ESTdelta, 0.9, na.rm = T),
#                    max = max(ESTdelta, na.rm = T)) 

# merton_calif %>% 
#   dplyr::group_by(Rating) %>% 
#   dplyr::summarise(n = n(),
#                    s = mean(s, na.rm = T),
#                    w = mean(w, na.rm = T),
#                    rPi_e = mean(rPi_e, na.rm = T),
#                    sigma_e = mean(sigma_e, na.rm = T),
#                    t = mean(ESTt, na.rm = T),
#                    sigma = mean(ESTsigma, na.rm = T),
#                    rPi = mean(rPi, na.rm =T),
#                    delta = mean(ESTdelta, na.rm = T),
#                    d_s = mean(ESTdelta/(s*10000), na.rm = T)) %>% View()
merton_calif <- merton_calif %>% 
  dplyr::mutate(delta = ifelse(ESTdelta>s*10000,s*10000,ESTdelta))
merton_calif <- merton_calif %>% 
  dplyr::group_by(Rating, Date) %>% 
  dplyr::mutate(Rating_s = median(s, na.rm = T)) %>% 
  dplyr::ungroup()
# the decision to include the delta metric was made arbitrary and should be worked on to better reflect the purposes that the model will try to replicate in the future
# the percentage that delta represents from the spread for each duple of Ratings, Time will be used as a linear factor to interpolate the Rating to the one that next to it. 
# merton_calif %>% 
#   names()
# bonds_agg %>% 
#   names()

df_bonds_model <- bonds_agg %>%
  dplyr::mutate(DateQ = as.yearqtr(Date)) %>% 
  dplyr::left_join(ratingsCIQ %>% 
                     dplyr::select(ISIN, DateQ, Rating)) %>% 
  dplyr::left_join(merton_calif %>% 
                     dplyr::select(Ticker, ISIN, CIQ_ID, Date, delta, Rating_s)) %>% 
  unique() %>% 
  dplyr::mutate(Rating_s = ifelse(is.na(Rating_s), 0, Rating_s),
                perc_delta = ifelse(Rating_s == 0, (delta/100) /(s*100), (delta/100)/(Rating_s*100)),
                dec_delta = case_when( perc_delta <.10 ~ 0,
                                       perc_delta <.20 ~ 0.10,
                                       perc_delta <.30 ~ 0.15,
                                       perc_delta <.40 ~ 0.20,
                                       perc_delta <.50 ~ 0.25,
                                       perc_delta <.60 ~ 0.35,
                                       perc_delta <.70 ~ 0.40,
                                       perc_delta <.80 ~ 0.45,
                                       perc_delta <.90 ~ 0.55,
                                       perc_delta >=.90 ~ 0.75,
                                       is.na(perc_delta) ~ 0))
# df_bonds_model %>% View()

### dataframe for model -----
# df_bonds_model %>% 
#   dplyr::select(Ticker, ISIN, CompanyName, Identifier, ParentCompany, CIQ_ID, DateQ, Rating, perc_delta, dec_delta) %>% 
#   write_csv(col_names = T, "../1_datos/3_MetricaSpreadBonos/df_bonds_model.csv")
#   
  
