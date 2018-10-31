## Loading databases (dataframes) from csv to have 5 inputs to work with:
## 
#### libraries ----
library(tidyverse)
library(magrittr)
library(stringr)
library(xts)
#### Output ----
# 5 Databases have to be created:
#   - Financial data ~ db_fin
#   - Moodys EDF metrics data ~ db_EDF
#   - Bond prices data ~ df_bonds_model
#   - Ratings (from Moodys and S&P) data ~ db_grade
#   - Information of the Sectors each company belongs to ~ db_sector
#

#### reading the csv files ----
# All of the csv files necesary are already saved in the 1_datos folder within each of the following folders:
## Financieros - 
## Moodys -
## Bonos - 
## Calificaciones -
## Sectores -
# 
setwd("C:/Users/gnovoa/Desktop/Santiago/Riesgos/Proyectos/CreditRisk_Model/proyecto/1_datos")
na_excel <- c("#NA", "#N/A", "#VALOR!")
na_ciq <- c("NM", "(Invalid Identifier)", " (Invalid Identifier) ", "-", "NA")
na_moodys <- c("You do not have permission to view this data.")
na_read <- c(na_excel, na_ciq, na_moodys)

# Financial Data -----
path_fin <- "csv/Financials/"
dir_fin <- paste0(path_fin, dir(path_fin))

read_fin <- read_csv(dir_fin[1],
                     skip = 2, 
                     na = na_read, 
                     col_types = list(Date = col_date(format = "%m/%d/%Y"), Index = col_skip(), Count = col_skip()))
for(i in 2:(length(dir_fin)-1)){
  aux_fin <- read_csv(dir_fin[i],
                      skip = 2, 
                      na = na_read, 
                      col_types = list(Date = col_date(format = "%m/%d/%Y"), Index = col_skip(), Count = col_skip()))
  read_fin <- rbind(read_fin, aux_fin)
}
aux_fin <- read_csv(dir_fin[length(dir_fin)],
                    skip = 2, 
                    na = na_read, 
                    col_types = list(Date = col_date(format = "%m/%d/%Y"), Index = col_skip(), Count = col_skip()))
read_fin <- rbind(read_fin, aux_fin %>% dplyr::select(-c(Ticker, ISIN)))
names(read_fin) <- read_fin %>% 
  names() %>% 
  gsub(pattern = " ", replacement = "") %>% 
  gsub(pattern = "\\(", replacement = "_") %>% 
  gsub(pattern = "\\)", replacement = "")
read_fin[sapply(read_fin, is.character)] <- lapply(read_fin[sapply(read_fin, is.character)], factor)

# Moodys ----
path_edf <- "csv/EDF/"
dir_edf <- paste0(path_edf, dir(path_edf))

read_edf <- read_csv(dir_edf[1],
                     skip = 4, 
                     na = na_read, 
                     col_types = list(Date = col_date(format = "%m/%d/%Y"), Date_1 = col_date(format = "%m/%d/%Y"), Count = col_skip()))
for(i in 2:(length(dir_edf))){
  aux_edf <- read_csv(dir_edf[i],
                      skip = 4, 
                      na = na_read, 
                      col_types = list(Date = col_date(format = "%m/%d/%Y"), Date_1 = col_date(format = "%m/%d/%Y"), Count = col_skip()))
  read_edf <- rbind(read_edf, aux_edf)
}
names(read_edf) <- read_edf %>% 
  names() %>% 
  gsub(pattern = " ", replacement = "") %>% 
  gsub(pattern = "\\(", replacement = "_") %>% 
  gsub(pattern = "\\)", replacement = "")
read_edf <- read_edf %>% 
  dplyr::mutate(ISIN = gsub(Isin, pattern = "isin-", replacement=""))
read_edf[sapply(read_edf, is.character)] <- lapply(read_edf[sapply(read_edf, is.character)], factor)
read_edf <- read_edf %>% 
  dplyr::filter(!is.na(ID)) %>% 
  dplyr::select(-ID)

# Bonds Data -----
# Given that these datapoints will have to be treated differently than the rest of the dataframes, the bonds data will be loaded inside the script 3_MetricaSpreadBonos.R
# Ratings ----
path_ratings <- "csv/Ratings/"
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
read_ratings <- ratingsCIQ %>% 
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
names(read_ratings) <- read_ratings %>% 
  names() %>% 
  gsub(pattern = " ", replacement = "") %>% 
  gsub(pattern = "\\(", replacement = "_") %>% 
  gsub(pattern = "\\)", replacement = "")
read_ratings %>% 
  dplyr::mutate(ISIN = gsub(ISIN, pattern = "I_", replacement = "")) %>% 
  dplyr::select(-c(Ticker, Identifier, BondRating,IssuerRating_SP,RatingAgg,RatingCIQ,RatingM))
read_ratings <- read_ratings %>% 
  dplyr::mutate(ISIN = gsub(ISIN, pattern = "I_", replacement = "")) %>% 
  dplyr::select(ISIN, CompanyName,ParentCompany,CIQ_ID,Date,DateQ,Rating)
read_ratings[sapply(read_ratings, is.character)] <- lapply(read_ratings[sapply(read_ratings, is.character)], factor)

# Sectors ----
path_sector <- "csv/Sector/"
dir_sector <- paste0(path_sector, dir(path_sector))
read_sector <- read_csv(dir_sector[1], col_types = list(X11 = col_skip(),X12 = col_skip(),X13 = col_skip(),X14 = col_skip()))
read_sector[sapply(read_sector, is.character)] <- lapply(read_sector[sapply(read_sector, is.character)], factor)

# save objects -----
save(read_fin, read_edf, read_ratings, read_sector, file = "1_LecturaBases/1_LecturaBases.RData")
# load("1_LecturaBases/1_LecturaBases.RData")
