### Testing the model with portfolio data and 2018 data
## The following script has been written to separate each part of the prediction process into a individual subfunction. 
## Although it is not really necesary to create each subfunctions from this script, one of the purposes was to provide a clearer understanding to the reader of the script. 
#### libraries ----
list.of.packages <- c("tidyverse","magrittr","stringr","xts","xts","glmnet","lme4","lmerTest","purrr","broom","GGally","rpart","rpart.plot","lubridate","pracma")
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
    message("Databases loaded")
  }
  return(load_files)
}
lapply(load_db_model(), load, .GlobalEnv)
# read financial database 
read_model_data <- function(data_path = "csv/Financials/Financials_CIQ_port.csv", skip_row = 2, data_type = "fin"){
  na_excel <- c("#NA", "#N/A", "#VALOR!")
  na_ciq <- c("NM", "(Invalid Identifier)", " (Invalid Identifier) ", "-", "NA")
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
      if(!data_path=="..\1_usage/PortfolioData/EDF/Portfolio_EDF_data_paste.csv"){
        message("New path introduced to read financial database, remember that all variables must be called as the ones in the 'Portfolio_EDF_data_paste.csv' file.")
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
        if(!data_path=="..\1_usage/PortfolioData/Ratings/Portfolio_Rating_data_paste.csv"){
          message("New path introduced to read financial database, remember that all variables must be called as the ones in the 'Portfolio_Rating_data_paste.csv' file (or at least have the identifiers and Ratings columns needed).")
        }
        #skip = 2
        model_data <- read_csv(date_path, 
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
                                 col_types = list(X11 = col_skip(),X12 = col_skip(),X13 = col_skip(),X14 = col_skip()))
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
# read_sector <- read_model_data(data_path = ,data_type = "sector")
# read_edf <- read_model_data(data_path = ,data_type = "edf")
# read_ratings <- read_model_data(data_path = ,data_type = "ratings")
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
last_edf <- function(db = read_edf){
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
    dplyr::select(-c(ISIN)) %>% 
    dplyr::left_join(read_sector %>% 
                       dplyr::select(CIQ_ISIN, `Tresalia Industry`) %>% 
                       dplyr::mutate(Sector = `Tresalia Industry`) %>% 
                       dplyr::select(CIQ_ISIN, Sector)) 
  ## create ratios
  # create LY variables for changes
  db_fin_sector <- data_wrangling_lag(db_sector)
  # create _ind variables for comp and groups
  db_fin_sector_ind <- data_wrangling_median(db_fin_sector)
  # create ratios and categories  
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
  db_fin_num_edf <- db_fin_num %>% 
    dplyr::left_join(db_edf, by = c("DateQ"="DateQ","ISIN"="ISIN")) %>% 
    dplyr::select(ISIN,Identifier:Sector,group,Date, DateQ, year:Solv,EDF1) %>% 
    dplyr::arrange(group,CompanyName,desc(Date)) %>% 
    unique()
  return(db_fin_num_edf)
}
db_fin <- data_wrangling(port_fin)

### predicting glmm ----
db_model_glmm <- matrix(data = NA,ncol = ncol(db_fin) + 1, nrow = 0) %>% as.data.frame()
names(db_model_glmm) <- c(names(db_fin), "pred")
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
db_model_glmm <- glmm(db = db_fin, groups = 4)
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
    dplyr::left_join(db_ratings %>% 
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
db_model_tree <- tree(db = db_model_glmm, read_edf = read_edf, read_ratings = read_ratings)
#### output ----
output_stopSign <- function(){}
output_KNN <- function(){}
output_OU <- function(){}
output_model <- function(db = db_model_tree){
  "Generating outputs from the model..."
}
#### save RDs ----


