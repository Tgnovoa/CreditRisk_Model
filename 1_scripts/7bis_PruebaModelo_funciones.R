### Testing the model with portfolio data and 2018 data
## The following script has been written to separate each part of the prediction process into a individual subfunction. 
## Although it is not really necesary to create each subfunctions from this script, one of the purposes was to provide a clearer understanding to the reader of the script. 
#### libraries ----
list.of.packages <- c("tidyverse","magrittr","stringr","xts","xts","glmnet","lme4","lmerTest","purrr","broom","GGally","rpart","rpart.plot","lubridate","pracma","fields")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = T)
#### databases with 2018 information ----
# load databases  and create ratios----

setwd("C:/Users/gnovoa/Desktop/Santiago/Riesgos/Proyectos/CreditRisk_Model/proyecto/1_datos/")
load_db_model <- function(){
  pwd_file <- pwd() %>% str_split(pattern = "/") %>% .[[1]] %>% last()
  if(!pwd_file == "1_datos"){
    message("Incorrect path for executing the code, assign '1_datos' as your working directory")
  }else{
    load_files <- c()
    load_files <- c(load_files,"1_LecturaBases/1_LecturaBases.RData")
    load_files <- c(load_files,"1_LecturaBases/db_fin_sector_s.RData")
    load_files <- c(load_files, "2_TransformacionBases/vars_select.RData")
    load_files <- c(load_files, "6_EntrenamientoModelo_creacion/var_cap.RData")
    load_files <- c(load_files,"6_EntrenamientoModelo_creacion/glmm_par.RData")
    load_files <- c(load_files,"6_EntrenamientoModelo_creacion/tree_par.RData")
    load_files <- c(load_files,"6_EntrenamientoModelo_creacion/stopSignInd.RData")
    load_files <- c(load_files, "7bis_PruebaModelo_funciones/bench_pred.RData")
    load_files <- c(load_files, "7bis_PruebaModelo_funciones/port_pred.RData")
    message("Databases loaded")
  }
  return(load_files)
}
lapply(load_db_model(), load, .GlobalEnv)
# read financial database 
read_model_data <- function(data_path = "csv/Financials/Financials_CIQ_port.csv", skip_row = 2, data_type = "fin"){
  na_excel <- c("#NA", "#N/A", "#VALOR!")
  na_ciq <- c("NM", "(Invalid Identifier)", " (Invalid Identifier) ", "-", "NA", " -   ", " NA "," (Invalid Identifier) ")
  na_moodys <- c("You do not have permission to view this data.")
  na_read <- c(na_excel, na_ciq, na_moodys)
  data_type <- tolower(data_type)
  fin_opt <- c("fin", "financial")
  edf_opt <- c("edf", "edf1", "creditedge")
  rat_opt <- c("ratings", "rating", "s&p","moodys", "rat")
  sect_opt <- c("sector", "sec", "group", "industry", "ind")
  if(data_type %in% fin_opt){
    if(!data_path=="csv/Financials/Financials_CIQ_port.csv"){
      message("New path introduced to read financial database, remember that all variables must be called as the ones in the 'Financials_CIQ_port.csv' file.")
    }
    #skip = 2
    model_data <- read_csv(data_path,
                           skip = skip_row, na = na_read, 
                           col_types = list(Date = col_date(format = "%m/%d/%Y"), 
                                            Index = col_skip(), 
                                            Count = col_skip())
                           )
  }else{
    if(data_type %in% edf_opt){
      if(!data_path=="../1_usage/PortfolioData/EDF/Portfolio_EDF_data_paste.csv"){
        message("New path introduced to read EDF database, remember that all variables must be called as the ones in the 'Portfolio_EDF_data_paste.csv' file.")
      }
      # skip = 4
      model_data <- read_csv(data_path,
                             skip = skip_row, na = na_read,
                             col_types = list(Date = col_date(format = "%m/%d/%Y"),
                                              Date_1 =col_date(format = "%m/%d/%Y"),
                                              ID = col_skip(), 
                                              Count = col_skip())
                             )
    }else{
      if(data_type %in% rat_opt){
        if(!data_path=="../1_usage/PortfolioData/Ratings/Portfolio_Rating_data_paste.csv"){
          message("New path introduced to read Ratings database, remember that all variables must be called as the ones in the 'Portfolio_Rating_data_paste.csv' file (or at least have the identifiers and Ratings columns needed).")
        }
        #skip = 2
        model_data <- read_csv(data_path, 
                               skip = skip_row, na = na_read,
                               col_types = list(Date = col_date(format = "%m/%d/%Y"), 
                                                Index = col_skip(), 
                                                Count = col_skip())
                               )
        model_data$Rating <- factor(model_data$Rating, 
                                    levels = c("AAA", 
                                              "AA+", "AA", "AA-",
                                              "A+", "A", "A-",
                                              "BBB+", "BBB", "BBB-",
                                              "BB+", "BB", "BB-",
                                              "B+", "B", "B-",
                                              "CCC+", "CCC", "CCC-",
                                              "CC", "C"))
      }else{
        if(data_type %in% sect_opt){
          message("Reading Sector data from file CV_Portfolio_Benchmark_Sector.\nIf that file is not up to date, new issuers will be ignored from the analysis.")
          model_data <- read_csv("../1_usage/CV_Portfolio_Benchmark_Sector.csv",
                                 col_types = list(X11 = col_skip(),X12 = col_skip(),X13 = col_skip(),X14 = col_skip(), X15 = col_skip(), X16 = col_skip()))
          model_data[sapply(model_data, is.character)] <- lapply(model_data[sapply(model_data, is.character)], factor)
        }else{
          stop("data_type not inside one of the options to read data: \nfin\nedf\nrat\nTry again with another data_type.") 
        }
      }
    }
  }
  return(model_data)
}
port_fin <- read_model_data(data_type = "fin")
# read_sector <- read_model_data(data_type = "sector")
# read_edf <- read_model_data(data_path = "../1_usage/PortfolioData/EDF/Portfolio_EDF_data_paste.csv",skip_row = 4,data_type = "edf")
# read_ratings <- read_model_data(data_path = "../1_usage/PortfolioData/Ratings/Portfolio_Rating_data_paste.csv",skip_row = 2,data_type = "ratings")
# maxD <- as.yearqtr("2018 Q3")
mod_names <- function(db = port_fin){
  names_db <- names(db)%>% 
    gsub(pattern = "\\&", replacement = "") %>% 
    gsub(pattern = "\\.", replacement = "") %>% 
    gsub(pattern = " ", replacement = "") %>% 
    gsub(pattern = "\\(", replacement = "_") %>% 
    gsub(pattern = "\\)", replacement = "")
  return(names_db)
} 
data_wrangling_lag <- function(db = db_sector){
  db_ly <- db %>% 
    dplyr::arrange(CompanyName, Date, Identifier) %>% 
    dplyr::group_by(CIQ_ISIN) %>% 
    dplyr::mutate(TotalSalesQLY = lag(TotalSalesQ, 4),
                  InventoriesLTMLY = lag(InventoriesLTM, 4),
                  TotalSalesLTMLY = lag(TotalSalesLTM,4),
                  InventoriesLTM_TotalSalesLTM = InventoriesLTM/TotalSalesLTM,
                  InventoriesLTMLY_TotalSalesLTMLY = InventoriesLTMLY/TotalSalesLTMLY,
                  PayablesLTMLY = lag(PayablesLTM,4),
                  ReceivablesLTMLY = lag(ReceivablesLTM, 4),
                  PayablesLTM_ReceivablesLTM = PayablesLTM/ReceivablesLTM,
                  PayablesLTMLY_ReceivablesLTMLY = PayablesLTMLY/ReceivablesLTMLY) %>% 
    dplyr::ungroup()
  return(db_ly)
} 
data_wrangling_median <- function(db = db_fin_sector, db_aux = db_fin_sector_s){
  db_ind <- db %>%
    dplyr::mutate(year = year(Date)) %>% 
    dplyr::left_join(db_aux) %>% 
    dplyr::group_by(Sector, year) %>% 
    dplyr::mutate(TotalAssets_rat = TotalAssetsQ/TotalAssets_ind,
                  InventoriesQ_TotalSalesQ = InventoriesQ/TotalSalesQ,
                  PayablesQ_ReceivablesQ = PayablesQ/ReceivablesQ) %>% 
    dplyr::ungroup()
  return(db_ind)
}
data_wrangling_ratios <- function(db = db_fin_sector_ind){
  db_ratios <- db %>% 
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
                  D_NWC_neg = NetWorkingCapitalLTM<0)
  return(db_ratios)
}
data_wrangling_cat2num <- function(db = db_fin){
  db_num <- db %>% 
    dplyr::mutate(TotalAssets_cat = case_when(TotalAssets_cat == "Small" ~ 1,
                                              TotalAssets_cat == "Medium" ~ 2,
                                              TotalAssets_cat == "Big" ~ 3,
                                              TotalAssets_cat == "2B2F" ~ 4),
                  TD_TDEq_cat = case_when(TD_TDEq_cat == "Bad" ~ 1,
                                          TD_TDEq_cat == "Avg." ~ 2,
                                          TD_TDEq_cat == "Good" ~ 3)) %>% 
    dplyr::mutate_if(is.logical, as.numeric) %>% 
    dplyr::filter(!CIQ_ISIN=="") %>% 
    dplyr::mutate(ISIN = gsub(CIQ_ISIN,pattern="I_", replacement="")) %>% 
    dplyr::mutate(group = factor(case_when(Sector %in% c("Consumer Discretionary", "Consumer Staples") ~ "Consumer",
                                           Sector %in% c("Health Care", "Information Technology", "Telecommunications") ~ "Services",
                                           Sector %in% c("Energy", "Industrials", "Materials", "Utilities") ~ "Industrials",
                                           Sector %in% c("Real Estate") ~ "RealEstate",
                                           TRUE ~ "Other"),
                                 levels = c("Consumer", "Services", "Industrials", "RealEstate", "Other")),
                  DateQ = as.yearqtr(Date)) 
  return(db_num)
}
last_edf <- function(db = read_edf, opt = "last"){
  opt <- tolower(opt)
  opt_last <- c("last", "l", "final", "recent")
  opt_weight <- c("weight", "w", "mean", "avg")
  if(opt %in% opt_last){
    db_edf <- db %>% 
      dplyr::mutate(ISIN = gsub(Isin, pattern = "isin-", replacement = "") ,
                    DateQ = as.yearqtr(Date)) %>%
      dplyr::group_by(ISIN, DateQ) %>% 
      dplyr::summarise(EDF1_1 = dplyr::last(EDF),
                       EDF1_2 = dplyr::nth(EDF,2),
                       EDF1_3 = dplyr::first(EDF),
                       EDF1 = ifelse(!is.na(EDF1_1), EDF1_1,
                                     ifelse(!is.na(EDF1_2), EDF1_2, EDF1_3))) %>% 
      dplyr::select(ISIN, DateQ, EDF1) %>% 
      dplyr::ungroup()
  }else{
    if(opt %in% opt_weight){
      db_edf <- db %>% 
        dplyr::mutate(ISIN = gsub(Isin, pattern = "isin-", replacement = "") ,
                      DateQ = as.yearqtr(Date)) %>%
        dplyr::group_by(ISIN, DateQ) %>% 
        dplyr::summarise(EDF1_1 = dplyr::last(EDF),
                         EDF1_2 = dplyr::nth(EDF,2),
                         EDF1_3 = dplyr::first(EDF),
                         EDF1 = mean(c(EDF1_1,EDF1_1,EDF1_1,
                                       EDF1_2,EDF1_2,
                                       EDF1_3), na.rm = T)) %>% 
        dplyr::select(ISIN, DateQ, EDF1) %>% 
        dplyr::ungroup()
    }else{
      message("opt not known, please chose one of the following categories : \n'last'   \n'avg'")
      db_edf <- list()
    }
  }
  
  return(db_edf)
}
data_wrangling <- function(db = port_fin){
  db_ls <- ls(envir = .GlobalEnv)
  list.of.dbs <- c("vars_select_Port",
                   "read_model_data","mod_names","last_edf",
                   "data_wrangling_lag","data_wrangling_median","data_wrangling_ratios","data_wrangling_cat2num",
                   "read_sector", "db_fin_sector_s")
  new.dbs <- list.of.dbs[!(list.of.dbs %in% db_ls)]
  if(length(new.dbs)){
    message("Databases needed for the data wrangling process not loaded.")
  } 
  names(db) <- mod_names(db)
  db_sector <- db %>% 
    dplyr::mutate(CIQ_ISIN = ISIN) %>%
    dplyr::mutate(ISIN = gsub(CIQ_ISIN, pattern = "I_", replacement = "")) %>% 
    dplyr::left_join(read_sector %>% 
                       dplyr::select(ISIN, `Tresalia Industry`) %>% 
                       dplyr::mutate(Sector = `Tresalia Industry`) %>% 
                       dplyr::select(ISIN, Sector)) 
  ## create ratios
  # create LY variables for changes
  message("Adding LAG data")
  db_fin_sector <- data_wrangling_lag(db_sector)
  # create _ind variables for comp and groups
  message("Adding Sector data")
  db_fin_sector_ind <- data_wrangling_median(db_fin_sector)
  # create ratios and categories  
  message("Creating Ratios")
  db_fin_mut <- data_wrangling_ratios(db_fin_sector_ind)
  # select model variables only
  db_fin <- db_fin_mut %>% 
    dplyr::select(vars_select_Port)
  # turn logical  (Dummies) and categorical (TotalAssets_cat and TD_TDEq) variables into numerical variables (glmnet and glmer input as matrix)
  # and add group variable
  db_fin_num <- data_wrangling_cat2num(db_fin)
  # summarise edf monthly data into quarterly data (selecting the most recent observation available for each quarter)
  db_edf <- last_edf(read_edf)
  # join edf1 data to fin data for glmer models
  message("Adding EDF linear data")
  db_fin_num_edf <- db_fin_num %>% 
    dplyr::left_join(db_edf, by = c("DateQ"="DateQ","ISIN"="ISIN")) %>% 
    dplyr::select(ISIN,Identifier:Sector,group,Date, DateQ, year:Solv,EDF1) %>% 
    dplyr::arrange(group,CompanyName,desc(Date)) %>% 
    unique()
  return(db_fin_num_edf)
}

# #
# load(file = "2_TransformacionBases/2_TransformacionBases.RData")
# db_edf <- last_edf(read_edf)
# bench_fin <- db_fin %>%
#   dplyr::mutate(Ticker = CIQ_ID,
#                 DateQ = as.yearqtr(Date),
#                 CIQ_ISIN = paste0("I_", ISIN)) %>%
#   dplyr::select(vars_select_Port) %>%
#   data_wrangling_cat2num() %>%
#   dplyr::left_join(db_edf, by = c("DateQ"="DateQ","ISIN"="ISIN")) %>%
#   dplyr::select(ISIN,Identifier:Sector,group,Date, DateQ, year:Solv,EDF1) %>%
#   dplyr::arrange(group,CompanyName,desc(Date)) %>%
#   dplyr::filter(DateQ<=maxD) %>%
#   unique()
# #
# db_fin <- data_wrangling(port_fin)

### predicting glmm ----
glmm_cap <- function(db = db_group, db_ne = db_group_ne, lower = T){
  db_cap <- db
  aux_norm <- db %>% 
    dplyr::select(var_cap_norm)
  aux_exp <- db %>% 
    dplyr::select(var_cap_exp)
  aux_db <- cbind(aux_norm, aux_exp)
  for(i in 1:ncol(aux_db)){
    name_n_i <- names(aux_db)[i]
    name_nM_i <- which(str_detect(names(db_ne), pattern = name_n_i))[2]
    name_nS_i <- which(str_detect(names(db_ne), pattern = name_n_i))[3]
    
    db_cap[[name_n_i]] <- ifelse(db_cap[[name_n_i]]<= db_ne[[name_nM_i]] + 3* db_ne[[name_nS_i]],
                                       db_cap[[name_n_i]],
                                       db_ne[[name_nM_i]] + 3* db_ne[[name_nS_i]])
    if(lower){
      db_cap[[name_n_i]] <- ifelse(db_cap[[name_n_i]]>= db_ne[[name_nM_i]] - 3* db_ne[[name_nS_i]],
                                         db_cap[[name_n_i]],
                                         db_ne[[name_nM_i]] - 3* db_ne[[name_nS_i]])
    }
  }
  return(db_cap)
}
glmm_nas <- function(db = db_cap){
  na_list <- apply(is.na(as.matrix(db)), FUN = which, MARGIN = 2)
  na_ini <- which(names(db)=="ROA")
  na_fin <- which(names(db)=="Solv")
  db_na0 <- db
  for(k in na_ini:na_fin){
    l_col_na_list <- na_list[[k]] %>% length()
    if(l_col_na_list>0){
      db_na0[na_list[[k]],k] <- 0
    }
  }
  return(db_na0)
}
glmm_predict <- function(db = db_na0, n_groups = groups, glmm_par = glmm_par, allow_new_levels = T){
  db_glmm <- matrix(data = NA,ncol = ncol(db) + 1, nrow = 0) %>% as.data.frame()
  names(db_glmm) <- c(names(db), "pred")
  for(j in 1:n_groups){
    index_group <- levels(db$group)[j]
    message("Predicting glmm for group ", index_group, "...")
    db_group <- db %>% 
      dplyr::filter(group == index_group)
    db_group$pred <- predict(object = glmm_par[[j]]$glmm, newdata = db_group, type = "response", allow.new.levels = allow_new_levels)
    db_glmm <- rbind(db_glmm, db_group)
  }
  return(db_glmm)
}
glmm <- function(db = db_fin, groups = 4, cap = T, na_0 = T, allow_new_levels = T){
  db_ls <- ls(envir = .GlobalEnv)
  list.of.dbs <- c("var_cap_norm","var_cap_exp",
                   "glmm_cap","glmm_nas","glmm_predict",
                   "glmm_par", 
                   "db_fin")
  new.dbs <- list.of.dbs[!(list.of.dbs %in% db_ls)]
  if(length(new.dbs)){
    message("Databases needed for the glmm models not fully loaded.")
  } 
  len_glmm_par <- length(glmm_par)
  if(groups>len_glmm_par){
    message("More groups selected than those already accounted for in the glmm model. \nERROR")
  }else{
    if(groups<len_glmm_par){
      message("There are more glmm models than groups selected, only the first ",groups," will be predicted, all of the other groups will be ignored.")
    }
  }
  if(cap){
    message("Capping variables...")
    db_cap  <- matrix(data = NA,ncol = ncol(db) , nrow = 0) %>% as.data.frame()
    names(db_cap) <- names(db)
    for(i in 1:groups){
      index_group <- levels(db$group)[i]
      db_group <- db %>% 
        dplyr::filter(group == index_group)
      db_group_cap_norm <- glmm_par[[i]]$var_norm %>% unique()
      db_group_cap_exp <- glmm_par[[i]]$var_exp %>% unique()
      db_group_ne <- db_group %>% 
        dplyr::left_join(db_group_cap_norm) %>% 
        dplyr::left_join(db_group_cap_exp)
      db_cap_aux <- glmm_cap(db = db_group, db_ne = db_group_ne)
      db_cap <- rbind(db_cap, db_cap_aux)
    }
    message("Process finished")
  }else{
    db_cap <- db
  }
  if(na_0){
    message("Transforming NAs...")
    db_na0 <- glmm_nas(db = db_cap)
    message("Process finished")
  }else{
    db_na0 <- db_cap
  }
  db_glmm_pred <- glmm_predict(db = db_na0, n_groups = groups, glmm_par = glmm_par, allow_new_levels = allow_new_levels)
  message("Predictions for generalized linear mixed model complete. :)")
  return(db_glmm_pred)
}
# bench_model_glmm <- glmm(db = bench_fin, groups = 4)
# db_model_glmm <- glmm(db = db_fin, groups = 4)

# add edf correction ----
tree_edf_w <- function(db = db_model_glmm, db_edf = read_edf){
  tree_edf <- db_edf %>% 
    dplyr::mutate(ISIN = gsub(Isin, pattern = "isin-", replacement = "")) %>%
    dplyr::mutate(slope = EDF5-EDF,
                  EDFch = EDF-EDF_LY,
                  DetProbCh = DetProb - DetProb_LY,
                  DateQ = as.yearqtr(Date)) %>% 
    dplyr::group_by(ISIN,DateQ) %>% 
    dplyr::summarise(EDF1_1 = dplyr::first(EDF),
                     EDF1_2 = dplyr::nth(EDF,2),
                     EDF1_3 = dplyr::last(EDF),
                     EDF1Ch_1 = dplyr::first(EDFch),
                     EDF1Ch_2 = dplyr::nth(EDFch,2),
                     EDF1Ch_3 = dplyr::last(EDFch),
                     EDF5_1 = dplyr::first(EDF5),
                     EDF5_2 = dplyr::nth(EDF5,2),
                     EDF5_3 = dplyr::last(EDF5),
                     DetProb_1 = dplyr::first(DetProb),
                     DetProb_2 = dplyr::nth(DetProb,2),
                     DetProb_3 = dplyr::last(DetProb),
                     DetProbCh_1 = dplyr::first(DetProbCh),
                     DetProbCh_2 = dplyr::nth(DetProbCh,2),
                     DetProbCh_3 = dplyr::last(DetProbCh),
                     slope_1 = dplyr::first(slope),
                     slope_2 = dplyr::nth(slope,2),
                     slope_3 = dplyr::last(slope))
  tree_edf <- tree_edf %>% 
    dplyr::ungroup() %>% 
    dplyr::rowwise() %>% 
    dplyr::transmute(
      DateQ = DateQ,
      ISIN = ISIN,
      EDF1 = case_when((!is.na(EDF1_1)) ~ EDF1_1,
                       (!is.na(EDF1_2)) ~ EDF1_2,
                       (!is.na(EDF1_3)) ~ EDF1_3,
                       TRUE ~ NA_real_),
      EDF5 = case_when(!is.na(EDF5_1) ~ EDF5_1,
                       !is.na(EDF5_2) ~ EDF5_2,
                       !is.na(EDF5_3) ~ EDF5_3,
                       TRUE ~ NA_real_),
      DetProb = case_when(!is.na(DetProb_1) ~ DetProb_1,
                          !is.na(DetProb_2) ~ DetProb_2,
                          !is.na(DetProb_3) ~ DetProb_3,
                          TRUE ~ NA_real_),
      slope = case_when(!is.na(slope_1) ~ slope_1,
                        !is.na(slope_2) ~ slope_2,
                        !is.na(slope_3) ~ slope_3,
                        TRUE ~ NA_real_),
      EDF1_Ch = case_when(!is.na(EDF1Ch_1) ~ EDF1Ch_1,
                          !is.na(EDF1Ch_2) ~ EDF1Ch_2,
                          !is.na(EDF1Ch_3) ~ EDF1Ch_3,
                          TRUE ~ NA_real_),
      DetProb_Ch = case_when(!is.na(DetProbCh_1) ~ DetProbCh_1,
                             !is.na(DetProbCh_2) ~ DetProbCh_2,
                             !is.na(DetProbCh_3) ~ DetProbCh_3,
                             TRUE ~ NA_real_),
      D_slope = slope<0) %>% 
    dplyr::ungroup()
  tree_edf <- tree_edf %>% 
    dplyr::mutate(DateQ = as.yearqtr(DateQ))
  edf_t_detect <- which(str_detect(names(db), pattern = "EDF1"))
  if(length(edf_t_detect)){
    db <- db %>% 
      dplyr::mutate(EDF_t = EDF1) %>% 
      dplyr::select(-c(EDF1))
  }
  db_tree_edf <- db %>% 
    dplyr::left_join(tree_edf)
  return(db_tree_edf)
}
tree_rating_w <- function(db = db_tree_edf, db_rating = read_ratings){
  db_tree_ratings <- db %>% 
    dplyr::left_join(db_rating %>% 
                       dplyr::select(ISIN,DateQ,Rating)) %>% 
    unique()
  db_tree_ratings <- db_tree_ratings %>% 
    dplyr::mutate(
      D_rating = is.na(Rating)) %>% 
    dplyr::mutate(Rating_A = ifelse(Rating%in%c("AAA", 
                                                "AA+", "AA", "AA-",
                                                "A+", "A", "A-"),1,0),
                  Rating_BPort = ifelse(Rating%in%c("BBB+","BBB","BBB-",
                                                    "BB+","BB","BB-"),1,0),
                  Rating_C = ifelse(Rating%in%c("CCC+","CCC","CCC-",
                                                "CC","C"),1,0))
  return(db_tree_ratings)
}
tree_predict <- function(db = db_tree_rating, tree_par = tree_par){
  db_tree <- db
  db_tree$edf_tree_corr <- predict(tree_par, newdata = db_tree)
  db_tree <- db_tree %>% 
    rowwise() %>% 
    dplyr::mutate(pred_fin = sum(c(edf_tree_corr ,pred),na.rm = T),
                  prob_det = pmin(pmax(pred_fin,0),1)) %>% 
    dplyr::ungroup()
  db_tree <- db_tree %>% 
    dplyr::mutate(DateQ = as.yearqtr(DateQ-.25))
  return(db_tree)
}
tree <- function(db = db_model_glmm, read_edf = read_edf, read_ratings = read_ratings){
  db_ls <- ls(envir = .GlobalEnv)
  list.of.dbs <- c("tree_edf_w","tree_rating_w","tree_predict",
                   "tree_par", 
                   "db_model_glmm")
  new.dbs <- list.of.dbs[!(list.of.dbs %in% db_ls)]
  if(length(new.dbs)){
    message("Databases needed for the data wrangling for the tree model not loaded.")
  } 
  message("Adding EDF data for the random tree prediction...")
  db_tree_edf <- tree_edf_w(db = db, db_edf = read_edf)
  message("Adding Ratings data for the random tree prediction...")
  db_tree_rating <- tree_rating_w(db = db_tree_edf, db_rating = read_ratings)
  message("Calculating the random tree correction factor...")
  db_tree <- tree_predict(db = db_tree_rating, tree_par = tree_par)
  message("Final deterioration probabilities estimated! Good luck! ;)")
  return(db_tree)
}
# bench_model_tree <- tree(db = bench_model_glmm, read_edf = read_edf, read_ratings = read_ratings)
# db_model_tree <- tree(db = db_model_glmm, read_edf = read_edf, read_ratings = read_ratings)

# bench_model_tree %>% 
#   ggplot(aes(x = DateQ, y = prob_det, colour = Sector)) + 
#   geom_point(alpha = 0.35) + 
#   theme_bw() + facet_wrap(~group) + 
#   geom_smooth(se = F) + geom_smooth(col = "black")
# bench_pred <- bench_model_tree %>% 
#   dplyr::select(ISIN, Identifier, CompanyName, Sector, group, DateQ, Rating, pred, prob_det)

#### output ----

output_stopSign <- function(db = db_model_tree, stopSign = stopSign_list){
  db_output_stop <- db %>% 
    dplyr::mutate(RiskC = factor(case_when(prob_det>stopSign[4] ~ "Red",
                                    prob_det>stopSign[3] ~ "Orange",
                                    prob_det>stopSign[2] ~ "Yellow",
                                    prob_det>stopSign[1] ~ "Green",
                                    prob_det>=0 ~ "Deterioration_Probability_Near_0"), 
                                 levels = c("Red","Orange","Yellow","Green","Deterioration_Probability_Near_0")))
  return(db_output_stop)
}
output_KNN <- function(db = db_output_stopSign, benchmark = bench_pred, g_by = "group", years_back = 15){
  db <- db %>% 
    dplyr::filter(!is.na(pred), !is.na(prob_det), !is.na(ISIN), !is.na(DateQ))
  g_g_by <- unique(db[[g_by]])
  n_g_by <- g_g_by %>% length()
  date_filter <- max(benchmark$DateQ) - years_back
  bench_pred_group <- benchmark %>% 
    dplyr::filter(DateQ>=date_filter) %>% 
    dplyr::group_by(get(g_by))
  g_by_name <- paste0(g_by, "_knn")
  db[[g_by_name]] <- NA_character_
  db_knn <- matrix(data = NA, ncol = ncol(db), nrow = 0) %>% as.data.frame()
  names(db_knn) <- names(db)
  for(i in 1:n_g_by){
    bench_group <- bench_pred_group %>% 
      dplyr::filter(get(g_by)==g_g_by[i])
    db_group <- db %>% 
      dplyr::filter(get(g_by)==g_g_by[i])
    if(nrow(db_group)>0 & nrow(bench_group)>0){
      for(j in 1:nrow(db_group)){
        dist_bench <- rdist(db_group[j,c("pred","prob_det")] %>% as.matrix(), bench_group[, c("pred","prob_det")] %>% as.matrix())
        w_dist_bench <- which(dist_bench == min(dist_bench))
        if(length(w_dist_bench)){
          bench_knn <- bench_group[w_dist_bench,c("ISIN","DateQ","CompanyName")]
          db_knn_aux <- db_group[j,c("ISIN", "DateQ")]
          bench_knn_wi <- which(!bench_knn$ISIN==db_knn_aux$ISIN)
          bench_knn_wd <- which(!bench_knn$DateQ==db_knn_aux$DateQ)
          if(length(bench_knn_wi)){
            bench_knn <- bench_knn[bench_knn_wi[1],]
          }else{
            if(length(bench_knn_wd)){
              bench_knn <- bench_knn[bench_knn_wd[1],]
            }else{
              bench_knn <- data.frame(ISIN = "", DateQ = "")
            }
          }
        }else{
          bench_knn <- data.frame(ISIN = "", DateQ = "")
        }
        db_group[[g_by_name]][j] <- paste0(bench_knn, collapse = "_")
      }
    }
    db_knn <- rbind(db_knn, db_group)
  }
  return(db_knn)
    
}
output_OU <- function(db = db_knn_d, benchmark = bench_pred, g_by = "Sector", years_back = 5){
  date_filter <- max(benchmark$DateQ) - years_back  
  bench_pred_group <- benchmark %>% 
    dplyr::filter(DateQ>=date_filter) %>% 
    dplyr::group_by(get(g_by)) %>% 
    dplyr::summarise(group_pred = median(pred, na.rm = T),
                     group_prob_det = median(prob_det, na.rm = T))
  names(bench_pred_group) <- c(g_by, "group_pred", "group_prob_det")
  db_ou <- db %>% 
    dplyr::left_join(bench_pred_group) %>% 
    dplyr::mutate(pred - group_pred,
                  prob_det - group_prob_det) %>% dplyr::select(-c(group_pred, group_prob_det))
  names(db_ou)[ncol(db_ou)-1] <- paste0("ou_pred_", g_by)
  names(db_ou)[ncol(db_ou)] <- paste0("ou_prob_det_", g_by)
  return(db_ou)
}
output_model <- function(db = db_model_tree){
  message("Generating outputs from the model...")
  db_output_stopSign <- output_stopSign(db = db_model_tree, stopSign = stopSign_list)
  message("Closest observation...")
  message("by group")
  db_knn_g <- output_KNN(db = db_output_stopSign, benchmark = bench_pred, g_by = "group", years_back = 10)
  message("by sector")
  db_knn_s <- output_KNN(db = db_knn_g, benchmark = bench_pred, g_by = "Sector", years_back = 10)
  message("by rating")
  db_knn_r <- output_KNN(db = db_knn_s, benchmark = bench_pred, g_by = "Rating", years_back = 10)
  message("by date")
  db_knn_d <- output_KNN(db = db_knn_r, benchmark = bench_pred, g_by = "DateQ", years_back = 10)
  message("O/U for each prediction...")
  message("by sector")
  db_ou_s <- output_OU(db = db_knn_d, benchmark = bench_pred, g_by = "Sector", years_back = 3)
  message("by rating")
  db_ou_r <- output_OU(db = db_ou_s, benchmark = bench_pred, g_by = "Rating", years_back = 3)
  message("by date")
  db_ou_d <- output_OU(db = db_ou_r, benchmark = bench_pred, g_by = "DateQ", years_back = 3)
  message("Saving to csv...")
  return(db_ou_d)
}
# bench_pred <- bench_model_tree %>% 
#   output_stopSign() %>% 
#   dplyr::select(ISIN:group, DateQ,year,Rating,pred,prob_det,RiskC, EDF1, slope) %>% 
#   dplyr::arrange(group, Sector, CompanyName, desc(DateQ))
# write_csv(bench_pred, path = "../1_usage/BenchmarkData/bench_pred.csv")
# save(bench_pred, file = "7bis_PruebaModelo_funciones/bench_pred.RData")

# port_pred_comp <- db_model_tree %>% output_model() 

# port_pred <- port_pred_comp %>% 
#   dplyr::select(ISIN:group, DateQ,year,Rating,pred,prob_det,RiskC, EDF1, slope,
#                 group_knn:ou_prob_det_DateQ) %>% 
#   dplyr::arrange(group, Sector, CompanyName, desc(DateQ))
# write_csv(port_pred, path = "../1_usage/PortfolioData/port_pred.csv")
# save(port_pred, file = "7bis_PruebaModelo_funciones/port_pred.RData")
# save(file = "7bis_PruebaModelo_funciones/funciones_modelo.RData")
save(data_wrangling, data_wrangling_cat2num,data_wrangling_lag,data_wrangling_median,data_wrangling_ratios,read_model_data,
     glmm,glmm_cap,glmm_nas,glmm_predict,last_edf,
     load_db_model,mod_names,
     output_KNN, output_model, output_OU, output_stopSign, 
     tree, tree_edf_w, tree_predict, tree_rating_w,
     file = "7bis_PruebaModelo_funciones/funciones_modelo.RData")
     
# bench_model_tree %>% 
#   output_stopSign() %>% 
#   ggplot(aes(x = Sector,fill = RiskC)) + 
#   geom_bar(position = "stack") + 
#   facet_wrap(~group, scales = "free") + theme_bw() + scale_fill_manual(values = c("red3","orangered","yellow","springgreen2","springgreen4"))



