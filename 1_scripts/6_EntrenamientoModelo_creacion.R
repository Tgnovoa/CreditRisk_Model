## Training GLMM model to predict the probability of deterioration given fin ratios and EDFs from Moodys
#### libraries ----
library(tidyverse)
library(magrittr)
library(stringr)
library(xts)
library(glmnet)
library(lme4)
library(lmerTest)
library(purrr)
library(broom)
library(GGally)
library(rpart)
library(rpart.plot)
library(lubridate)
#### Training GLMM ----
# the first step will be to model a GLMM that only takes into consideration financial ratios, 
# after that, the residuals will be treated with a random forest to include EDF information as a second risk explanation factor
# and the model will finish trying to fit macro economic data to the results
#
# It will be extremely important to save the betas and results of the training of the models accordingly
## load dbs ----
load(file = "1_LecturaBases/1_LecturaBases.RData")
rm(list = c("read_fin", "read_ratings", "read_sector"))
load(file = "2_TransformacionBases/2_TransformacionBases.RData")
load(file = "5_SeparaBases_EntrenamientoPrueba/5_SeparaBases_EntrenamientoPrueba.RData")


# db_fin %>% glimpse()
# db_split %>% glimpse()
# 
# db_fin$CompanyName %>% unique() 
# db_split$CompanyName %>% unique()
db_edf <- read_edf %>% 
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
db_fin <- db_fin %>% 
  dplyr::left_join(db_edf, by = c("ISIN"="ISIN","Date"="DateQ"))

db_model_s <- db_split %>%
  dplyr::left_join(db_fin %>%
                     dplyr::mutate(DateQ = Date) %>%
                     dplyr::select(-c(CompanyName,ParentCompany, ISIN, Sector, Date))) %>% 
  dplyr::filter(!is.na(CurrentRatioL))%>%
  dplyr::arrange(CIQ_ID, Date) %>% unique() %>% 
  dplyr::mutate(year = factor(year))

# db_model_1 <- db_fin %>%
#   dplyr::select(-c(Sector)) %>%
#   dplyr::left_join(db_split %>%
#                      dplyr::select(-c(CompanyName,ParentCompany,Date, Rating_NA)) %>%
#                      dplyr::mutate(Date = DateQ) %>%
#                      dplyr::select(-DateQ)) %>% dplyr::filter(!is.na(tat)) %>% unique()
# db_model_2 <- db_fin %>%
#   dplyr::select(-c(Sector)) %>%
#   dplyr::left_join(db_split %>%
#                      dplyr::select(-c(ParentCompany, CIQ_ID,Date, Rating_NA)) %>%
#                      dplyr::mutate(Date = DateQ) %>%
#                      dplyr::select(-DateQ)) %>% dplyr::filter(!is.na(tat)) %>% unique()


## Feature selection via lasso ----
db_model_s %>% summary()
# transform lgl and factors to numeric (for glmnet)
db_model_s <- db_model_s %>% 
  dplyr::mutate(TotalAssets_cat = case_when(TotalAssets_cat == "Small" ~ 1,
                                            TotalAssets_cat == "Medium" ~ 2,
                                            TotalAssets_cat == "Big" ~ 3,
                                            TotalAssets_cat == "2B2F" ~ 4),
                TD_TDEq_cat = case_when(TD_TDEq_cat == "Bad" ~ 1,
                                        TD_TDEq_cat == "Avg." ~ 2,
                                        TD_TDEq_cat == "Good" ~ 3)
                ) %>% 
  dplyr::mutate_if(is.logical, as.numeric)
db_model_s$group <- factor(db_model_s$group, levels = c("Consumer", "Services", "Industrials", "RealEstate"))

var_num <- c(which(names(db_model_s)== "ROA"):(ncol(db_model_s)-1))

list_var_presel <- list()
for(i in 1:4){
  index_group <- levels(db_model_s$group)[i]
  
  db_model_g <- db_model_s %>% 
    dplyr::filter(group == index_group) %>% 
    dplyr::filter(tat == "train")
  mat_var_ratios <- db_model_g %>% 
    dplyr::select(ROA:Solv) %>% as.matrix() 
  mat_var_y <- db_model_g %>% 
    dplyr::select(y_bin_q) %>% 
    dplyr::transmute(y_bin  = ifelse(y_bin_q == 1, 1,0)) %>% 
    as.matrix()
  mat_var_ratios[is.na(mat_var_ratios)] <- 0
  # mat_var_y[is.na(mat_var_y)] <- 0
  glmnet1 <- glmnet(x = mat_var_ratios, y = mat_var_y, alpha = 1, family = "binomial", nlambda = 1000)
  # coef(glmnet1) 
  
  # par(mar = c(1,1,1,1) + 1.5)
  set.seed(12508903)
  cv_l_cl <- cv.glmnet(x = mat_var_ratios, y = mat_var_y, alpha=1, type.measure = "class", family = "binomial", nlambda = 1000)
  glmnet_cl <- glmnet(x = mat_var_ratios, y = mat_var_y, alpha=1, family = "binomial", dfmax = 20, nlambda = 1000)
  # cv_l_cl %>% plot()
  # par(mfrow=c(1,3))
  # glmnet_cl %>% plot(xvar ="norm", label = T)
  # glmnet_cl %>% plot(xvar ="lambda", label = T)
  # glmnet_cl %>% plot(xvar ="dev", label = T)
  # par(mfrow = c(1,1))
  # cv_l_cl %>% coef(s = "lambda.1se")
  # glmnet_cl %>% coef(s = cv_l_cl$lambda.1se)
  # 
  cv_l_auc <- cv.glmnet(x = mat_var_ratios, y = mat_var_y, alpha=1, type.measure = "auc", family = "binomial", nlambda = 1000)
  glmnet_auc <- glmnet(x = mat_var_ratios, y = mat_var_y, alpha=1,family = "binomial", dfmax = 20, nlambda = 1000)
  # cv_l_auc %>% plot()
  # par(mfrow=c(1,3))
  # glmnet_auc %>% plot(xvar ="norm", label = T)
  # glmnet_auc %>% plot(xvar ="lambda", label = T)
  # glmnet_auc %>% plot(xvar ="dev", label = T)
  # par(mfrow = c(1,1))
  # cv_l_auc %>% coef(s = "lambda.1se")
  # glmnet_auc %>% coef(s = cv_l_auc$lambda.1se) #correr las dos opciones, esta y con type.measure = "class"
  # glmnet_cl %>% coef(s = c(cv_l_cl$lambda.min, cv_l_auc$lambda.1se))
  # coef(glmnet_cl, s = c(cv_l_cl$lambda.min, cv_l_auc$lambda.1se))@i
  dif_num_var <- rbind(coef(glmnet_cl, s = cv_l_cl$lambda.min)@i %>% length()-1,
                       coef(glmnet_auc, s = cv_l_cl$lambda.1se)@i %>% length() -1,
                       coef(glmnet_auc, s = cv_l_auc$lambda.min)@i %>% length() -1,
                       coef(glmnet_auc, s = cv_l_auc$lambda.1se)@i %>% length() -1) -20
  dif_num_var_aux <- which(abs(dif_num_var) == min(abs(dif_num_var)))
  dif_num_var <- rbind(coef(glmnet_cl, s = cv_l_cl$lambda.min)@i ,
                       coef(glmnet_auc, s = cv_l_cl$lambda.1se)@i ,
                       coef(glmnet_auc, s = cv_l_auc$lambda.min)@i ,
                       coef(glmnet_auc, s = cv_l_auc$lambda.1se)@i ) 
  var_presel <- which(names(db_model_s) %in% names(db_model_s)[var_num][dif_num_var[dif_num_var_aux,]])
  list_var_presel[[i]] <- list(num = var_presel, names = names(db_model_s)[var_num][dif_num_var[dif_num_var_aux,]] %>% unique())
}
list_var_presel
## glmm by group ----

# glmm_par <- list()

# capping variables to have consistent output
var_cap_norm <- c("ROA","ROC","SalesGrowth","InventoriesCh","Pay_Rec_Ch", 
                  "Solv",
                  "TotalSales_ind","Inventories_Sales_ind", "Pay_Rec_ind")
var_cap_exp <- c("TotalLiab_TA","NetDebt_EBITDA","NetWorkingCapital_Sales","TotalDebt_EBITDA",
                  "RetEarn_CurrLiab","MktVEquity_BookVTotalLiab", "FFO_IntExp", "EBITDA_IntExp")

set.seed(12508904)
i_group <- 4
index_group <- levels(db_model_s$group)[i_group]

db_model_g <- db_model_s %>% 
  dplyr::filter(group == index_group) %>% 
  dplyr::filter(tat == "train") 
db_model_cap_norm <- db_model_g %>% 
  dplyr::group_by(Sector) %>% 
  dplyr::select(var_cap_norm) %>% 
  dplyr::transmute_all(funs(mean = mean, sd = sd), na.rm = T) 
db_model_cap_exp <- db_model_g %>% 
  dplyr::group_by(Sector) %>% 
  dplyr::select(var_cap_exp) %>% 
  dplyr::transmute_all(funs(mean = mean, sd = sd), na.rm = T) 


aux_norm <- db_model_g %>% 
  dplyr::select(var_cap_norm)
aux_exp <- db_model_g %>% 
  dplyr::select(var_cap_exp)
aux_db_model_g <- cbind(aux_norm,aux_exp)

for(i in 1:ncol(aux_norm)){
  name_n_i <- names(aux_norm)[i]
  name_nM_i <- which(str_detect(names(db_model_cap_norm), pattern = name_n_i))[1]
  name_nS_i <- which(str_detect(names(db_model_cap_norm), pattern = name_n_i))[2]
  
  db_model_g[[name_n_i]] <- ifelse(db_model_g[[name_n_i]]<= db_model_cap_norm[[name_nM_i]] + 3* db_model_cap_norm[[name_nS_i]],
                                   db_model_g[[name_n_i]],
                                   db_model_cap_norm[[name_nM_i]] + 3* db_model_cap_norm[[name_nS_i]])
  db_model_g[[name_n_i]] <- ifelse(db_model_g[[name_n_i]]>= db_model_cap_norm[[name_nM_i]] - 3* db_model_cap_norm[[name_nS_i]],
                                   db_model_g[[name_n_i]],
                                   db_model_cap_norm[[name_nM_i]] - 3* db_model_cap_norm[[name_nS_i]])
  }
for(i in 1:ncol(aux_exp)){
  name_e_i <- names(aux_exp)[i]
  name_eM_i <- which(str_detect(names(db_model_cap_exp), pattern = name_e_i))[1]
  name_eS_i <- which(str_detect(names(db_model_cap_exp), pattern = name_e_i))[2]
  
  db_model_g[[name_e_i]] <- ifelse(db_model_g[[name_e_i]]<= db_model_cap_exp[[name_eM_i]] + 3* db_model_cap_exp[[name_eS_i]],
                                   db_model_g[[name_e_i]],
                                   db_model_cap_exp[[name_eM_i]] + 3* db_model_cap_exp[[name_eS_i]])
}


# # function to rescale normal numeric variables (DO NOT USE ANYMORE)
# center_colmeans <- function(x, x_center = NA_real_, x_diff = NA_real_){
#   if(is.na(x_center | x_diff)[1]){
#     xcenter = colMeans(x, na.rm = T)
#     xrango <- sapply(x, range, na.rm=T)
#     xdiff <- apply(xrango,2,diff)
#   }else{
#     xcenter = x_center
#     xdiff = x_diff
#   }
#   y <- (x-rep(xcenter,rep.int(nrow(x), ncol(x))))/(rep(xdiff,rep.int(nrow(x), ncol(x))))
#   X <- list(var_resc = I(y), xcenter = xcenter, xdiff = xdiff)
#   return(X)
# }
# 
# center_var <- db_model_g %>% 
#   dplyr::select(var_norm) %>% 
#   center_colmeans()

# db_model_g[, var_norm]<- db_model_g %>% 
#   dplyr::select(var_norm) %>% 
#   center_colmeans() %>% 
#   .$var_resc

db_presel <- cbind(db_model_g[list_var_presel[[i_group]][[2]]], db_model_g[,"EDF1"], db_model_g[,c("year","Sector", "group","y_bin_q", "DateQ", "CompanyName", "CIQ_ID")])
names_presel <- names(db_presel)
(formula_presel <- as.formula(paste(paste(paste("y_bin_q ~  ", paste(names_presel[!names_presel %in% c("DateQ", "CompanyName", "CIQ_ID",
                                                                          "Sector", "Industry","group","year",
                                                                          "y_bin_q")], collapse = " + "), " + (1 + EDF1||Sector) + (1|year)")))))
## heatmap function for remaining variables
# plot_heatmap <- function(df,group = NA, type = 0, clust.type = "ward", tl.cex = 0.4, addrect = 3){
#   num_var <- df %>% sapply(is.numeric)
#   heatmap_aux <- df %>%
#     dplyr::select(names(df)[num_var])
#   if(!is.na(group)){
#     heatmap_aux <- df %>%
#       dplyr::filter(group == group) %>%
#       dplyr::select(names(df)[num_var])
#   }
#   cormat<-round(cor(heatmap_aux,use="na.or.complete"),3)
#   if(type == 1) {
#     g4 <- corrplot::corrplot(corr = cormat, order = "hclust", tl.cex = tl.cex,
#                              method = "square", type = "full",
#                              tl.pos = "lt", addrect = addrect,
#                              tl.col = "black", tl.srt = 45,
#                              title = group , hclust.method = clust.type, mar=c(0,0,1,0))
# 
#   }else{
#     cormat<-melt(cormat)
#     g4<-ggplot(data = cormat, aes(x=Var1, y=Var2, fill=value)) +
#       geom_tile()+scale_fill_gradient2(low = "red", high = "blue", mid = "white",
#                                        midpoint = 0, limit = c(-1,1), space = "Lab",
#                                        name="Pearson\nCorrelation")+
#       theme(axis.text.x = element_text(angle = 90, vjust = 1,
#                                        size = 7, hjust = 1),
#             axis.text.y = element_text(angle = 0, vjust = 1,
#                                        size = 7, hjust = 1)) +
#       xlab("") + ylab("")
#   }
#   g4
# }
db_presel %>% plot_heatmap(type = 1, clust.type = "ward.D", tl.cex = 0.5, addrect = 4)
db_presel %>% 
  dplyr::select(-c(Sector, group, CompanyName, CIQ_ID, DateQ, year)) %>% 
  dplyr::mutate(y_bin_q = factor(y_bin_q)) %>% 
  ggpairs()
db_presel %>% summary()
# nas should be predicted or accounted for (using bootstrap for example) to not throw away valiable information)
db_presel[is.na(as.matrix(db_presel))] <- 0
db_presel %>% summary()

## run model doing a manual stepwise (not entirely) to choose final model taking into account logic over everything else ----
# (take out not significant variables and those that appear to have problems with spurious correlations)
# glmm <- glmer(formula = formula_presel, data = db_presel, family = binomial(link="probit"))
glmm <- glmer(formula = formula_presel, data = db_presel, family = binomial)
# statistics
glmm %>% ranef()
glmm %>% fixef()

glmm %>% glance()
glmm %>% summary()

(formula_sel <- as.formula(
  paste(
    paste(
      paste("y_bin_q ~  ", 
            paste(
              names_presel[!names_presel %in% c("DateQ", "CompanyName", "CIQ_ID",
                                                "Sector", "Industry","group","year",
                                                "CurrentRatioL","QuickRatioL",
                                                "D_RetEarn_Neg","D_EBIT_neg",
                                                "TotalAssets_cat",
                                                "ROA","ROC",
                                                "Inventories_Sales_ind",
                                                # "InventoriesCh",
                                                "Pay_Rec_ind",
                                                "D_FFO_IntExp",
                                                "Solv",
                                                "SalesGrowth",
                                                "GrossProfit_Sales",
                                                "NetWorkingCapital_Sales",
                                                "RetEarn_CurrLiab",
                                                # "IntangibleAssets_TA",
                                                "CashRatioL","TD_TDEq_cat", "EDF1",
                                                "y_bin_q")], collapse = " + "), 
            " + (1 |TotalAssets_cat) + (1|year)")))))
# glmm <- glmer(formula = formula_sel, data = db_presel, family = binomial(link="probit"))
glmm <- glmer(formula = formula_sel, data = db_presel, family = binomial)
# db_presel %>% 
#   ggplot(aes(x = DateQ,  y = EDF1, colour = y_bin_q)) + geom_boxplot(alpha = 0.5) + facet_grid(Sector~year, scales = "free") 
# table(db_presel$TotalAssets_cat, db_presel$y_bin_q)
#
# formulas and variables ----
{
  glmm1 <- glmm
  formula_sel1 <- as.formula(
    paste(
      paste(
        paste("y_bin_q ~  ", 
              paste(
                names_presel[!names_presel %in% c("DateQ", "CompanyName", "CIQ_ID",
                                                  "Sector", "Industry","group","year",
                                                  "D_EBITDA_Neg","D_GP_Neg","D_NWC_Neg","D_RetEarn_Neg","D_EBIT_neg",
                                                  "ROC","TotalLiab_TA","TotalSales_ind","NetDebt_EBITDA","Solv",
                                                  "InventoriesCh","Pay_Rec_ind","NetWorkingCapital_Sales","CashRatioL","RetEarn_CurrLiab",
                                                  "TotalAssets_cat",
                                                  "y_bin_q")], collapse = " + "), 
              " + (1 + EDF1||Sector) + (1|year) + (1|TotalAssets_cat)"))))
  var_sel1 <- names_presel[!names_presel %in% c("DateQ", "CompanyName", "CIQ_ID",
                                                "Sector", "Industry","group","year",
                                                "D_EBITDA_Neg","D_GP_Neg","D_NWC_Neg","D_RetEarn_Neg","D_EBIT_neg",
                                                "ROC","TotalLiab_TA","TotalSales_ind","NetDebt_EBITDA","Solv",
                                                "InventoriesCh","Pay_Rec_ind","NetWorkingCapital_Sales","CashRatioL","RetEarn_CurrLiab",
                                                "TotalAssets_cat",
                                                "y_bin_q")]
  rand_sel1 <- " + (1 + EDF1||Sector) + (1|year) + (1|TotalAssets_cat)"
  }
{
  glmm2 <- glmm
  formula_sel2 <- as.formula(
    paste(
      paste(
        paste("y_bin_q ~  ", 
              paste(
                names_presel[!names_presel %in% c("DateQ", "CompanyName", "CIQ_ID",
                                                  "Sector", "Industry","group","year",
                                                  "D_EBITDA_Neg","D_NWC_Neg","D_FFO_IntExp","D_RetEarn_Neg",
                                                  "TD_TDEq_cat","RetEarn_CurrLiab","ROC","GrossProfit_Sales","TotalLiab_TA","D_EBIT_neg",
                                                  "Pay_Rec_ind","ROA","Pay_Rec_Ch","IntangibleAssets_TA",
                                                  "TotalAssets_cat",
                                                  "Solv",
                                                  "y_bin_q")], collapse = " + "), 
              " + (1 |Sector) + (1|year) + (1|TotalAssets_cat)"))))
  var_sel2 <- names_presel[!names_presel %in% c("DateQ", "CompanyName", "CIQ_ID",
                                                "Sector", "Industry","group","year",
                                                "D_EBITDA_Neg","D_NWC_Neg","D_FFO_IntExp","D_RetEarn_Neg",
                                                "TD_TDEq_cat","RetEarn_CurrLiab","ROC","GrossProfit_Sales","TotalLiab_TA","D_EBIT_neg",
                                                "Pay_Rec_ind","ROA","Pay_Rec_Ch","IntangibleAssets_TA",
                                                "TotalAssets_cat",
                                                "Solv",
                                                "y_bin_q")]
  rand_sel2 <- " + (1 |Sector) + (1|year) + (1|TotalAssets_cat)"
}
{
  glmm3 <- glmm
  formula_sel3 <- as.formula(
    paste(
      paste(
        paste("y_bin_q ~  ", 
              paste(
                names_presel[!names_presel %in% c("DateQ", "CompanyName", "CIQ_ID",
                                                  "Sector", "Industry","group","year",
                                                  "CurrentRatioL","QuickRatioL",
                                                  "D_NWC_Neg","D_FFO_IntExp","D_EBIT_neg",
                                                  "ROC","RetEarn_CurrLiab","Solv","EBITDA_Sales",
                                                  "NetDebt_EBITDA","FFO_IntExp",
                                                  "Inventories_Sales_ind",
                                                  "y_bin_q")], collapse = " + "), 
              " + (1 + EDF1||Sector)  + (1|year)"))))
  var_sel3 <- names_presel[!names_presel %in% c("DateQ", "CompanyName", "CIQ_ID",
                                                "Sector", "Industry","group","year",
                                                "CurrentRatioL","QuickRatioL",
                                                "D_NWC_Neg","D_FFO_IntExp","D_EBIT_neg",
                                                "ROC","RetEarn_CurrLiab","Solv","EBITDA_Sales",
                                                "NetDebt_EBITDA","FFO_IntExp",
                                                "Inventories_Sales_ind",
                                                "y_bin_q")]
  rand_sel3 <-  " + (1 + EDF1||Sector)  + (1|year)"
}
{
  glmm4 <- glmm
  formula_sel4 <-  as.formula(
    paste(
      paste(
        paste("y_bin_q ~  ", 
              paste(
                names_presel[!names_presel %in% c("DateQ", "CompanyName", "CIQ_ID",
                                                  "Sector", "Industry","group","year",
                                                  "CurrentRatioL","QuickRatioL",
                                                  "D_RetEarn_Neg","D_EBIT_neg",
                                                  "TotalAssets_cat",
                                                  "ROA","ROC",
                                                  "Inventories_Sales_ind","InventoriesCh",
                                                  "Pay_Rec_ind","D_FFO_IntExp","Solv","SalesGrowth",
                                                  "GrossProfit_Sales","NetWorkingCapital_Sales","RetEarn_CurrLiab",
                                                  "IntangibleAssets_TA",
                                                  "y_bin_q")], collapse = " + "), 
              " + (1 |TotalAssets_cat) + (1|year)"))))
  var_sel4 <- names_presel[!names_presel %in% c("DateQ", "CompanyName", "CIQ_ID",
                                                "Sector", "Industry","group","year",
                                                "CurrentRatioL","QuickRatioL",
                                                "D_RetEarn_Neg","D_EBIT_neg",
                                                "TotalAssets_cat",
                                                "ROA","ROC",
                                                "Inventories_Sales_ind","InventoriesCh",
                                                "Pay_Rec_ind","D_FFO_IntExp","Solv","SalesGrowth",
                                                "GrossProfit_Sales","NetWorkingCapital_Sales","RetEarn_CurrLiab",
                                                "IntangibleAssets_TA",
                                                "y_bin_q")]
  rand_sel4 <-  " + (1 |TotalAssets_cat) + (1|year)"
}
# glmm_par[[1]] <- list(glmm = glmm, formula = formula_sel1, variables = var_sel1, rand_sel = rand_sel1, var_norm = db_model_cap_norm, var_exp = db_model_cap_exp)
# glmm_par[[2]] <- list(glmm = glmm, formula = formula_sel2, variables = var_sel2, rand_sel = rand_sel2, var_norm = db_model_cap_norm, var_exp = db_model_cap_exp)
# glmm_par[[3]] <- list(glmm = glmm, formula = formula_sel3, variables = var_sel3, rand_sel = rand_sel3, var_norm = db_model_cap_norm, var_exp = db_model_cap_exp)
# glmm_par[[4]] <- list(glmm = glmm, formula = formula_sel4, variables = var_sel4, rand_sel = rand_sel4, var_norm = db_model_cap_norm, var_exp = db_model_cap_exp)
# save(glmm_par, file = "6_EntrenamientoModelo_creacion/glmm_par.RData")

### glmm performance (test database)----
# glmm_par_test <- list()

db_model_glmm <- matrix(data = NA,ncol = ncol(db_model_s) + 1, nrow = 0) %>% as.data.frame()
names(db_model_glmm) <- c(names(db_model_s), "pred")
# predict output
for(j in 1:4){
  set.seed(12508905)
  i_group <- j
  index_group <- levels(db_model_s$group)[i_group]
  
  # transform test data to be comparable to training set
  db_model_train <- db_model_s %>% 
    dplyr::filter(group == index_group) %>% 
    dplyr::filter(tat == "train") 
  db_model_test <- db_model_s %>% 
    dplyr::filter(group == index_group) %>% 
    dplyr::filter(tat == "test") 
  db_model_g_test <- db_model_s %>% 
    dplyr::filter(group == index_group) %>% 
    dplyr::filter(tat == "test") 
  db_model_cap_norm <- glmm_par[[i_group]]$var_norm %>% unique()
  db_model_cap_exp <- glmm_par[[i_group]]$var_exp %>% unique()
  db_model_g_test_ne <- db_model_g_test %>% 
    dplyr::left_join(db_model_cap_norm) %>% 
    dplyr::left_join(db_model_cap_exp)
  
  
  aux_norm <- db_model_g_test %>% 
    dplyr::select(var_cap_norm)
  aux_exp <- db_model_g_test %>% 
    dplyr::select(var_cap_exp)
  aux_db_model_g <- cbind(aux_norm,aux_exp)
  
  for(i in 1:ncol(aux_db_model_g)){
    name_n_i <- names(aux_db_model_g)[i]
    name_nM_i <- which(str_detect(names(db_model_g_test_ne), pattern = name_n_i))[2]
    name_nS_i <- which(str_detect(names(db_model_g_test_ne), pattern = name_n_i))[3]
    
    db_model_g_test[[name_n_i]] <- ifelse(db_model_g_test[[name_n_i]]<= db_model_g_test_ne[[name_nM_i]] + 3* db_model_g_test_ne[[name_nS_i]],
                                          db_model_g_test[[name_n_i]],
                                          db_model_g_test_ne[[name_nM_i]] + 3* db_model_g_test_ne[[name_nS_i]])
    db_model_g_test[[name_n_i]] <- ifelse(db_model_g_test[[name_n_i]]>= db_model_g_test_ne[[name_nM_i]] - 3* db_model_g_test_ne[[name_nS_i]],
                                          db_model_g_test[[name_n_i]],
                                          db_model_g_test_ne[[name_nM_i]] - 3* db_model_g_test_ne[[name_nS_i]])
  }
  # nas should be predicted or accounted for (using bootstrap for example) to not throw away valiable information)
  # db_model_g_test %>% summary()
  na_list <- apply(is.na(as.matrix(db_model_g_test)), FUN = which, MARGIN = 2)
  for(k in 18:49){
    l_col_na_list <- na_list[[k]] %>% length()
    if(l_col_na_list>0){
      db_model_g_test[na_list[[k]],k] <- 0
    }
  }
  
  # db_model_g_test %>% summary()
  
  db_model_train$pred <- predict(glmm_par[[i_group]]$glmm, type = "response", allow.new.levels = T)
  db_model_test$pred <- predict(glmm_par[[i_group]]$glmm, newdata = db_model_g_test, type = "response", allow.new.levels = T)
  db_model_aux <- rbind(db_model_test, db_model_train)
  db_model_glmm <- rbind(db_model_glmm, db_model_aux)
}
db_model_glmm %>% 
  dplyr::filter(!Rating_NA == "NA_Rating") %>% 
  ggplot(aes(x = Date, y = pred, colour = y_bin_q)) + 
  geom_point(alpha = 0.2, show.legend = F) + geom_smooth(col = "black", se = F, show.legend = F) +
  facet_wrap(~Rating_NA, ncol = 4) + theme_bw()
db_model_glmm %>% 
  summary()
db_model_glmm %>% 
  dplyr::mutate(
    y_num = ifelse(y_bin_q == 1, 1, 0),
    y_error = abs(y_num - pred)) %>% 
  dplyr::filter(!Rating_NA == "NA_Rating") %>% 
  ggplot(aes(x = Date, y = y_error, colour = y_bin_q)) + 
  geom_point(alpha = 0.2, show.legend = F) + geom_smooth(col = "black", se = F, show.legend = F) +
  facet_wrap(~Rating_NA, ncol = 4) + theme_bw()

## EDFs by random trees ----
db_edf <- read_edf %>% 
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
db_edf <- db_edf %>% 
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
db_edf %>% summary()
db_model_glmm_join <- db_model_glmm %>% 
  dplyr::left_join(db_fin %>% dplyr::select(ISIN,CIQ_ID) %>% unique()) %>% 
  dplyr::select(ISIN, DateQ,Sector, Rating, tat, pred, y_bin_q)
db_tree_edf <- db_edf %>% 
  dplyr::mutate(DateQ = as.yearqtr(DateQ)) %>% 
  dplyr::left_join(db_model_glmm_join) %>% 
  dplyr::mutate(
    y_num = ifelse(y_bin_q == 1, 1, 0),
    y_error = y_num - pred,
    D_rating = is.na(Rating)) %>% 
  dplyr::filter(!is.na(y_bin_q)) %>% 
  dplyr::mutate(Rating_A = ifelse(Rating%in%c("AAA", 
                                                "AA+", "AA", "AA-",
                                                "A+", "A", "A-"),1,0),
                Rating_BPort = ifelse(Rating%in%c("BBB+","BBB","BBB-",
                                              "BB+","BB","BB-"),1,0),
                Rating_C = ifelse(Rating%in%c("CCC+","CCC","CCC-",
                                              "CC","C"),1,0),
                D_year_2007 = ifelse(year(DateQ)>2007,1,0),
                D_year_2011 = ifelse(year(DateQ)>2007,1,0),
                D_year_2015 = ifelse(year(DateQ)>2007,1,0),
                group = factor(case_when(Sector %in% c("Consumer Discretionary", "Consumer Staples") ~ "Consumer",
                                         Sector %in% c("Health Care", "Information Technology", "Telecommunications") ~ "Services",
                                         Sector %in% c("Energy", "Industrials", "Materials", "Utilities") ~ "Industrials",
                                         Sector %in% c("Real Estate") ~ "RealEstate",
                                         TRUE ~ "Other"),
                               levels = c("Consumer", "Services", "Industrials", "RealEstate", "Other")))
  
# db_tree_edf$y_error %>% hist()

(formula_tree <- as.formula("y_error ~ EDF1 + EDF5 + DetProb + slope + EDF1_Ch + DetProb_Ch + D_slope + 
                            Rating_A + Rating_BPort + Rating_C + D_rating  + group"))
set.seed(12508911)
train_tree <- which(db_tree_edf$tat == "test")
tree_edf <-  rpart(formula_tree ,data = db_tree_edf, subset = train_tree,
                   method = "anova", control = rpart.control(minsplit = 20, cp = 0.0025))
tree_edf
rpart.plot(tree_edf)
db_tree_edf$edf_tree <- predict(tree_edf, newdata = db_tree_edf)
db_tree_edf <- db_tree_edf %>% 
  dplyr::mutate(pred_edf = pred + edf_tree,
                pred_edf = case_when(pred_edf>1~1,
                                     pred_edf<0~0,
                                     TRUE~pred_edf)) 
db_tree_edf %>% 
  # dplyr::filter(tat == "train") %>%
  ggplot(aes(x = DateQ, y = pred_edf, colour = y_bin_q)) + 
  geom_point(alpha = 0.4, show.legend = F) + facet_wrap(~Rating) + theme_bw()
db_tree_edf %>% summary()
table(db_tree_edf$pred_edf>0.25, db_tree_edf$y_bin_q)
table(db_tree_edf$Rating,db_tree_edf$pred_edf>0.25)


glmm_par


## model performance ----
db_tree_edf$edf_tree <- predict(tree_edf, newdata = db_tree_edf)
db_tree_edf <- db_tree_edf %>% 
  dplyr::mutate(pred_edf = pred + y_error) 
db_tree_edf %>% 
  dplyr::filter(tat == "train") %>%
  ggplot(aes(x = DateQ, y = pred_edf, colour = y_bin_q)) + 
  geom_point(alpha = 0.4, show.legend = F) + facet_wrap(~Rating) + theme_bw()
table(db_tree_edf$pred_edf>0.1, db_tree_edf$y_bin_q)
db_tree_edf %>% summary()
## macro information ----