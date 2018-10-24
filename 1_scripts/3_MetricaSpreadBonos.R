## Construction of the expected default spread as a function of bond price
## #from :  
##        "Using Yield Spreads to Estimate Expected Returns on Debt and Equity"
##        - Ian A. Cooper (LBS), - Sergei A. Davydenko (LBS)

#### libraries ----
library(tidyverse)
library(magrittr)
library(stringr)
library(xts)

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
bonds <- read_csv(dir_spread[1], skip = 2, col_types = list(Date = col_date(format = "%m/%d/%Y"), BondMaturity = col_date(format = "%m/%d/%Y"),Count = col_skip(), Index = col_skip()), na = na_read)
for(i in 2:(length(dir_spread)-1)){
  bonds_aux <- read_csv(dir_spread[i], 
                        skip = 2, 
                        col_types = list(Date = col_date(format = "%m/%d/%Y"), BondMaturity = col_date(format = "%m/%d/%Y"), Count = col_skip(), Index = col_skip()), 
                        na = na_read)
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
                       na = na_read)
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
volatility <- read_csv(dir_sigma[1],skip = 2, col_types = list(Date = col_date(format = "%m/%d/%Y"), X11 = col_skip(), Index = col_skip(), Count = col_skip()), na = c(na_read,0))
for(i in 2:(length(dir_sigma)-1)){
  vol_aux <- read_csv(dir_sigma[i],
                      skip = 2,
                      col_types = list(Date = col_date(format = "%m/%d/%Y"), X11 = col_skip(), Index = col_skip(), Count = col_skip()), 
                      na = c(na_read,0))
  volatility <- rbind(volatility,vol_aux)
}
vol_port <- read_csv(dir_sigma[length(dir_sigma)],skip = 2, col_types = list(Date = col_date(format = "%m/%d/%Y"), X11 = col_skip(),X12 = col_skip(),X13 = col_skip(),X14 = col_skip(), Index = col_skip(), Count = col_skip()), na = c(na_read,0))
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
  if(yield==0|TtM<=0|is.na(date_AAA)|is.na(TtM)){
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
    spread <- yield - as.double(AAA_inter)*100
  }
  as.double(spread)
}
bonds_agg <- bonds_agg %>% 
  rowwise() %>% 
  dplyr::mutate(spread = yield_to_spread(yield = BondYield, TtM = TtM, date_AAA = Date))
# Add sigma to each observation for next step
bonds_agg <- bonds_agg %>% 
  dplyr::left_join(vol_agg %>% 
                     dplyr::select(CIQ_ID, Date, EquityVolatility_3m))
bonds_agg %>% glimpse()
#### variable transformation ----
## w ~ the fims levarage
#     w = B/V    ~ (TD/TD+Eq)
## s ~ the promised yield spread
#     s = 1/T ln(F/B) - r    ~  yield - r_MAT_t - s_AAAavg_t ~ yield - y_AAAavg_t
## d1 = (-ln(w) - (s - (sigma**2) / 2)*T)/sigma * sqrt(T)
## d2 = d1 - sigma*sqrt(T)
bonds_agg <- unique(bonds_agg)
bonds_agg <- bonds_agg %>% 
  dplyr::mutate(
    Equity = pmax(Equity, 0),
    w = ifelse(is.na(Equity)|is.na(TotalDebt)|TotalDebt==0|Equity==0,0,Equity/TotalDebt),
    ln_w = ifelse(w>0, log(w), NA_real_),
    # run_model = !is.na(sum(c(ln_w, spread, EquityVolatility_3m, TtM))),
    d1 = (-ln_w-(spread-(EquityVolatility_3m**2)/2)*TtM)/EquityVolatility_3m*sqrt(TtM),
    d2 = d1 - EquityVolatility_3m*sqrt(TtM)
    )
bonds_agg %>% summary()

#### calibrate merton model ----
## solve each one with 5 different values for T to obtain sigma values
# pnorm(-d1)/w + exp(s*T)*pnorm(d2) = 1
# sigma_e = sigma*pnorm(d1)/(1-w)
## use the average of the results (T,sigma) as a seed and solve simultaneously : 
# pnorm(-d1)/w + exp(s*T)*pnorm(d2) = 1
# sigma_e = sigma*pnorm(d1)/(1-w)

#### obtain expected default spread and default risk premium -----
## rPi ~ risk premia on assets 
#     rPi = Mu - r
## rPi_e ~ risk premia on equity
#     rPi_e = rPi*sigma_e/sigma
## delta ~ expected default spread
#     delta = -1/T*ln((exp(rPi-s)*T)*pnorm(-d1-rPi*sqrt(T)/sigma)/w + pnorm(d2 + rPi*sqrt(T/sigma)))
## gamma ~ default risk premium
#     gamma = s - delta

