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

#### add sectors info ----
# first we need to add sector information to the dataframe to be able to compute some of the industry variables
db_fin_sector_aux <- db_fin_inter %>% 
  dplyr::left_join(read_sector %>% 
                     dplyr::select(ISIN, CIQ_ID, `Tresalia Industry`) %>% 
                     dplyr::transmute(Identifier = CIQ_ID, ISIN = ISIN, Sector = `Tresalia Industry`))
aux_missing_Identifiers <- db_fin_sector_aux %>% 
  dplyr::filter(is.na(Sector)) %>% 
  dplyr::select(CompanyName) %>% 
  unique() %>% 
  .$CompanyName %>% 
  as.character()

db_fin_sector_aux2 <- db_fin_inter %>% 
  dplyr::left_join(read_sector %>% 
                     dplyr::select(CompanyName, ISIN, `Tresalia Industry`, CIQ_ID) %>% 
                     dplyr::transmute(CompanyName = CompanyName, ISIN = ISIN, Sector = `Tresalia Industry`, CIQ_ID = CIQ_ID))
db_fin_sector_aux %>% 
  dplyr::filter(CompanyName %in% aux_missing_Identifiers) %>% 
  dplyr::select(Identifier, CompanyName, ParentCompany, Sector) %>% 
  unique()
db_fin_sector_aux2 %>% 
  dplyr::filter(CompanyName %in% aux_missing_Identifiers) %>% 
  dplyr::select(Identifier, CompanyName, ParentCompany, Sector, CIQ_ID) %>% 
  unique()
db_fin_sector <- db_fin_inter %>% 
  dplyr::left_join(read_sector %>% 
                     dplyr::select(CompanyName, ISIN, `Tresalia Industry`, CIQ_ID) %>% 
                     dplyr::transmute(CompanyName = CompanyName, ISIN = ISIN, Sector = `Tresalia Industry`, CIQ_ID = CIQ_ID))

#### create ratios ----
names(db_fin_sector) <- db_fin_sector %>% 
  names() %>% 
  gsub(pattern = "\\.", replacement = "")

# create LY variables for changes
db_fin_sector <- db_fin_sector %>% 
  dplyr::arrange(CompanyName, Date, Identifier) %>% 
  dplyr::group_by(Identifier) %>% 
  dplyr::mutate(TotalSalesQLY = lag(TotalSalesQ, 4),
                InventoriesLTMLY = lag(InventoriesLTM, 4),
                TotalSalesLTMLY = lag(TotalSalesLTM,4),
                InventoriesLTM_TotalSalesLTM = InventoriesLTM/TotalSalesLTM,
                InventoriesLTMLY_TotalSalesLTMLY = InventoriesLTMLY/TotalSalesLTMLY,
                PayablesLTMLY = lag(PayablesLTM,4),
                ReceivablesLTMLY = lag(ReceivablesLTM, 4),
                PayablesLTM_ReceivablesLTM = PayablesLTM/ReceivablesLTM,
                PayablesLTMLY_ReceivablesLTMLY = PayablesLTMLY/ReceivablesLTMLY
                ) %>% 
  dplyr::ungroup()

# create _ind variables for comp and groups
db_fin_sector <- db_fin_sector %>%
  dplyr::mutate(year = year(Date)) %>% 
  dplyr::group_by(Sector, year) %>% 
  dplyr::mutate(TotalAssets_ind = median(TotalAssetsQ, na.rm = T),
                TotalAssets_rat = TotalAssetsQ/TotalAssets_ind,
                TotalSalesLTM_ind = median(TotalSalesLTM, na.rm = T),
                InventoriesQ_TotalSalesQ = InventoriesQ/TotalSalesQ,
                InventoriesQ_TotalSalesQ_ind = median(InventoriesQ_TotalSalesQ, na.rm = T),
                PayablesQ_ReceivablesQ = PayablesQ/ReceivablesQ,
                PayablesQ_ReceivablesQ_ind = median(PayablesQ_ReceivablesQ, na.rm = T)
                ) %>% 
  dplyr::ungroup() 
# db_fin_sector %>% 
#   dplyr::group_by(Sector) %>% 
#   dplyr::summarise(TA_min = min(TotalAssets_rat,na.rm = T),
#                    TA_d1 = quantile(TotalAssets_rat,0.1, na.rm = T),
#                    TA_d2 = quantile(TotalAssets_rat,0.2, na.rm = T),
#                    TA_d3 = quantile(TotalAssets_rat,0.3, na.rm = T),
#                    TA_d4 = quantile(TotalAssets_rat,0.4, na.rm = T),
#                    TA_d5 = quantile(TotalAssets_rat,0.5, na.rm = T),
#                    TA_d6 = quantile(TotalAssets_rat,0.6, na.rm = T),
#                    TA_d7 = quantile(TotalAssets_rat,0.7, na.rm = T),
#                    TA_d8 = quantile(TotalAssets_rat,0.8, na.rm = T),
#                    TA_d9 = quantile(TotalAssets_rat,0.9, na.rm = T),
#                    TA_max = max(TotalAssets_rat,na.rm = T))
# db_fin_sector$TotalAssets_rat %>% 
#   summary()

# create ratios and categories  
db_fin_mut <- db_fin_sector %>% 
  dplyr::mutate(ROA = NetIncomeLTM/TotalAssetsQ,
                ROC = NetIncomeLTM/TotalEquityQ,
                SalesGrowth = TotalSalesQ/TotalSalesQLY,
                TotalAssets_cat = factor(case_when(TotalAssets_rat < 0.4 ~ "Small",
                                                   TotalAssets_rat < 2.5 ~ "Medium",
                                                   TotalAssets_rat < 10 ~ "Big",
                                                   TotalAssets_rat >= 10 ~ "2B2F",
                                                   TRUE ~ "NA"),
                                         levels = c("Small", "Medium", "Big", "2B2F")),
                TotalSales_ind = TotalSalesLTM/TotalSalesLTM_ind,
                EBITDA_Sales = pmax(pmin(EBITDALTM/TotalSalesLTM,1),0),
                D_EBITDA_Neg = EBITDALTM<0,
                GrossProfit_Sales = pmax(pmin(GrossProfitLTM/TotalSalesLTM,1),0),
                D_GP_Neg = GrossProfitLTM<0,
                IntangibleAssets_TA = pmin(IntangibleAssetsQ/TotalAssetsQ,1),
                InventoriesCh = (InventoriesLTM_TotalSalesLTM)/(InventoriesLTMLY_TotalSalesLTMLY),
                Inventories_Sales_ind = (InventoriesQ_TotalSalesQ)/(InventoriesQ_TotalSalesQ_ind),
                NetWorkingCapital_Sales = pmax(NetWorkingCapitalLTM/TotalSalesLTM,0),
                D_NWC_Neg = NetWorkingCapitalLTM<0,
                TotalLiab_TA = TotalLiabilitiesQ/TotalAssetsQ,
                NetDebt_EBITDA = pmax(NetDebtQ/EBITDALTM,0),
                TotalDebt_EBITDA = pmax(TotalDebtQ/EBITDALTM,0),
                Pay_Rec_Ch = (PayablesLTM_ReceivablesLTM)/(PayablesLTMLY_ReceivablesLTMLY),
                Pay_Rec_ind = (PayablesQ_ReceivablesQ)/(PayablesQ_ReceivablesQ_ind),
                EBITDA_IntExp = pmax(EBITDALTM/InterestExpenseLTM,0.7),
                FFO_IntExp = pmax(FFOLTM/InterestExpenseLTM,0.5),
                D_EBITDA_IntExp = EBITDA_IntExp==0.7,
                D_FFO_IntExp = FFO_IntExp==0.5,
                MktVEquity_BookVTotalLiab = MktValueEquityQ_avgL3m/BookValueTotalLiabilitiesQ,
                TD_TDEq = TotalDebtQ/(TotalDebtQ+TotalEquityQ),
                TD_TDEq_cat = factor(case_when(TD_TDEq < 0.35 ~ "Good",
                                               TD_TDEq < 0.7 ~ "Avg.",
                                               TD_TDEq >= 0.7 ~ "Bad",
                                               TRUE ~ "NA"),
                                     levels = c("Bad","Avg.","Good")),
                RetEarn_CurrLiab = pmax(RetainedEarningsQ/CurrentLiabilitiesQ,0),
                D_RetEarn_Neg = RetainedEarningsQ<0,
                CashRatioL = log(1+(sum(c(CashInvestmentsQ,0),na.rm = T)/CurrentLiabilitiesQ)),
                CurrentRatioL = log(1+(sum(c(CashInvestmentsQ,InventoriesQ,ReceivablesQ),na.rm = T)/CurrentLiabilitiesQ)),
                QuickRatioL = log(1+(sum(c(CashInvestmentsQ,InventoriesQ),na.rm = T)/CurrentLiabilitiesQ)),
                D_EBIT_neg = EBITLTM<0,
                Solv = (EBITLTM - InterestExpenseLTM)*7/NetDebtQ,
                D_GP_neg = GrossProfitLTM<0,
                D_NWC_neg = NetWorkingCapitalLTM<0
                )

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










#### save database ----
vars_select <- c("Identifier", "CIQ_ID", "CompanyName", "ParentCompany", "ISIN", "Sector", "Date", "year", 
                 "ROA", "ROC", "SalesGrowth", "TotalAssets_cat", "TotalSales_ind", 
                 "EBITDA_Sales", "D_EBITDA_Neg", "GrossProfit_Sales", "D_GP_Neg", 
                 "IntangibleAssets_TA", "InventoriesCh", "Inventories_Sales_ind", "NetWorkingCapital_Sales", "D_NWC_Neg", 
                 "TotalLiab_TA", "NetDebt_EBITDA", "TotalDebt_EBITDA", 
                 "Pay_Rec_Ch", "Pay_Rec_ind", 
                 "EBITDA_IntExp", "FFO_IntExp", "D_EBITDA_IntExp", "D_FFO_IntExp", 
                 "MktVEquity_BookVTotalLiab", "TD_TDEq_cat", "RetEarn_CurrLiab", "D_RetEarn_Neg",
                 "CashRatioL", "QuickRatioL", "CurrentRatioL",
                 "D_EBIT_neg", "Solv")
db_fin_mut %>% 
  dplyr::select(vars_select) %>% 
  glimpse()
db_fin <- db_fin_mut %>% 
  dplyr::select(vars_select)
save(db_fin, file = "2_TransformacionBases/2_TransformacionBases.RData")
# load("2_TransformacionBases/2_TransformacionBases.RData")




