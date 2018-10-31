## Data wrangling 
## The methods used to fill NAs or manage outliers were chosen to focus specifically on the problem at hand 
## (creating an additive Risk Model that takes into consideration the dynamics each Industry and each Company can have).
#### libraries ----
library(tidyverse)
library(magrittr)
library(data.table)
library(xts)
library(stringr)
library(Hmisc)
library(timetk)
library(tidyquant)
library(imputeTS)

#### load data -----
setwd("C:/Users/gnovoa/Desktop/Santiago/Riesgos/Proyectos/CreditRisk_Model/proyecto/1_datos")
load("1_LecturaBases/1_LecturaBases.RData")
read_fin %>% glimpse()
read_fin %>% summary()
#### eliminate empty dates ----
ind_fin <- sapply(read_fin, is.double) %>% 
  which() %>% 
  as.vector()
elim<-c()
for(i in 1:(nrow(read_fin))){
  k=0
  for(j in ind_fin){
    if(!is.na(read_fin[i,j])){k=k+1}
  } 
  if(k<16){elim<-c(elim,i)}
}
read_fin_elim <- read_fin[-elim,]
#### interpolate (linear) data ----
aux_start <- read_fin_elim %>%
  dplyr::group_by(Identifier) %>%
  dplyr::mutate(start = min(Date)) %>%
  dplyr::ungroup() %>%
  dplyr::select(CompanyName, start) %>% 
  unique()
nest_read_fin <- read_fin_elim %>%
  tidyr::nest(-c(ParentCompany,CompanyName,Identifier), .key = "data.tbl")
nest_tS <- nest_read_fin %>%
  dplyr::mutate(timeS = purrr::map(.x = data.tbl,
                                   .f = timetk::tk_ts,
                                   select = -c(Date),
                                   start = c(2006,3),
                                   freq = 4))
nest_inter <- nest_tS %>% 
  dplyr::mutate(data.imp = map(.x = timeS,
                               .f = na.interpolation,
                               option = "linear"))
nest_inter <- nest_inter %>% 
  dplyr::left_join(aux_start)

read_fin_inter <- data.frame(Identifier = nest_inter[[2]][[1]],CompanyName = nest_inter[[3]][[1]], ParentCompany = nest_inter[[1]][[1]], start = nest_inter[[7]][[1]], Date = as.yearqtr(index(nest_inter[[6]][[1]])),nest_inter[[6]][[1]])
for(i in 2:nrow(nest_inter)){
  read_fin_inter <- rbind(read_fin_inter,
                          data.frame(Identifier = nest_inter[[2]][[i]],CompanyName = nest_inter[[3]][[i]], ParentCompany = nest_inter[[1]][[i]], start = nest_inter[[7]][[i]], Date = as.yearqtr(index(nest_inter[[6]][[i]])),nest_inter[[6]][[i]]))
}
read_fin_inter$start <- as.yearqtr(read_fin_inter$start)-.25

db_fin_inter <- read_fin_inter %>%
  dplyr::filter(!is.na(start)) %>%
  dplyr::mutate(Date = as.yearqtr(Date) - start + as.yearqtr("2006 Q3")) %>% 
  dplyr::filter(Date>=start) 

ind_fin_db <- sapply(db_fin_inter, is.double) %>% 
  which() %>% 
  as.vector()
ind_sd0_fin <- db_fin_inter %>% 
  dplyr::select(Identifier, ind_fin_db, -c(Date, start)) %>% 
  tidyr::gather(key = variable, value = value, -Identifier) %>% 
  dplyr::group_by(Identifier, variable) %>% 
  dplyr::mutate(sd = sd(value, na.rm = T)) %>% 
  dplyr::ungroup() %>% 
  dplyr::group_by(Identifier) %>% 
  dplyr::mutate(sd_sum = sum(sd, na.rm = T)) %>% 
  dplyr::ungroup() %>% 
  dplyr::filter(sd_sum == 0) %>% 
  dplyr::select(Identifier) %>% 
  unique() %>% 
  .$Identifier %>% 
  as.character()
ind_sd0_fin_n <- db_fin_inter %>% 
  dplyr::filter(Identifier %in% ind_sd0_fin) %>% 
  dplyr::group_by(Identifier) %>% 
  dplyr::summarise(n = n()) %>% 
  dplyr::filter(n>10) %>% 
  dplyr::select(Identifier) %>% 
  .$Identifier %>% 
  as.character()
db_fin_inter <- db_fin_inter %>% 
  dplyr::filter(!Identifier %in% ind_sd0_fin_n)

db_fin_inter %>% 
  dplyr::mutate()

# %>% 
#   dplyr::mutate(
#     # identifier = identifier, issuer = Issuer, date = Date,
#     assets_q = totalassets_q,sales_q = totalsales_q,
#     roa = netincome_ltm/totalassets_q, roc = netincome_ltm/totalequity_q,
#     salesgrowth = ifelse(!is.na(sales_qly),(totalsales_q-sales_qly)/sales_qly,0),
#     ebitda_sales = ifelse(ebitda_ltm>0, ebitda_ltm/totalsales_ltm, 0),
#     ebitda_neg = ebitda_ltm<=0,
#     gp_sales = ifelse(grossprofit_ltm>0, grossprofit_ltm/totalsales_ltm, 0),
#     gp_neg = grossprofit_ltm<=0,
#     intangible_sales = ifelse((intangibleassets_q/totalassets_q)<=1,intangibleassets_q/totalassets_q,1),
#     inventories_sales = inventories_ltm/totalsales_ltm,
#     inventories_sales_ly = lag(inventories_sales,4),
#     inventories_sales_change = ifelse(!is.na(inventories_sales_ly),(inventories_sales-inventories_sales_ly)/inventories_sales_ly,0),
#     nwc_sales = ifelse(networkingcapital_ltm>0, networkingcapital_ltm/totalsales_q, 0),
#     nwc_neg = networkingcapital_ltm<=0,
#     currentliabilities_sales = currentliabilities_q/totalsales_q,
#     currentliabilities_aux = ifelse(is.na(currentliabilities_q), 0, 
#                                     ifelse(currentliabilities_q>=0, currentliabilities_q, 0)),
#     totaldebt_sales = ifelse(totaldebt_q-currentliabilities_aux>=0,(totaldebt_q - currentliabilities_aux)/totalsales_q, 0),
#     ebit_assets = ifelse(ebit_ltm>0, ebit_ltm/totalassets_q,0),
#     ebit_neg = ebit_ltm<=0,
#     sales_assets = totalsales_ltm/totalassets_q,
#     sales_assets_ly = lag(sales_assets,4),
#     sales_assets_ch = ifelse(!is.na(sales_assets_ly),(sales_assets-sales_assets_ly)/sales_assets_ly,0),
#     cash_assets = cash.investments_q/totalassets_q,
#     nwc_assets = ifelse(networkingcapital_ltm>0, networkingcapital_ltm/totalassets_q, 0),
#     totalliabilities_assets = totalliabilities_q/totalassets_q,
#     netdebt_ebitda = netdebt_q/ebitda_ltm,
#     totaldebt_ebitda = totaldebt_q/ebitda_ltm,
#     payables_receivables = payables_ltm/receivables_ltm,
#     payables_receivables_ly = lag(payables_receivables,4),
#     payables_receivables_ch = ifelse(!is.na(payables_receivables_ly),(payables_receivables - payables_receivables_ly)/payables_receivables_ly,0),
#     ebitda_interestexpense = ifelse(ebitda_ltm/interestexpense_ltm>0.7, ebitda_ltm/interestexpense_ltm, 0.7),
#     ebitda_intexp_neg = ebitda_ltm/interestexpense_ltm<=0.7,
#     ffo_interestexpense = ifelse(ffo_ltm/interestexpense_ltm>0.5, ffo_ltm/interestexpense_ltm, 0.5),
#     ffo_intexp_neg = ffo_ltm/interestexpense_ltm<=0.5,
#     ffo_totaldebt = ifelse(ffo_ltm/totaldebt_q>0,ffo_ltm/totaldebt_q,0),
#     mktvalueequity_bvtotalliabilities = mktvalueequity_q_avgl3m/bookvaluetotalliabilities_q,
#     td_tdeq = totaldebt_q/(totaldebt_q + totalequity_q),
#     retainedearn_currentliabilities = ifelse(retainedearnings_q>0,
#                                              ifelse(currentliabilities_aux>0,
#                                                     retainedearnings_q/currentliabilities_aux,
#                                                     1),
#                                              0),
#     retainedearn_neg = retainedearnings_q<=0,
#     cash_aux = ifelse(is.na(cash.investments_q), 0, cash.investments_q),
#     inventories_aux = ifelse(is.na(inventories_q), 0, inventories_q),
#     receivables_aux = ifelse(is.na(receivables_q), 0, receivables_q),
#     cash_ratio = ifelse(currentliabilities_aux>0,log(1+cash_aux/currentliabilities_aux),NA_real_),
#     current_ratio = ifelse(currentliabilities_aux>0,log(1+(cash_aux+inventories_aux+receivables_aux)/currentliabilities_aux),NA_real_),
#     quick_ratio = ifelse(currentliabilities_aux>0,log(1+(cash_aux+inventories_aux)/currentliabilities_aux),NA_real_)
#   )









