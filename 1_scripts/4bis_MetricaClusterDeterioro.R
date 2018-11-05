# Transformation of Deterioration Probability into Boolean Variable 

#### libraries ----
library(tidyverse)
library(magrittr)
library(rpart)
library(stringr)

#### FUN ----
# split y with a tree -----
y_tree_f <- function(group = "Consumer", df, tree_form, seed = 1, perc = .2, S_g = 0){
  # split df by its group
  if(S_g>0){
    db_aux_group <- df %>% 
      dplyr::ungroup() %>% 
      dplyr::filter(group == group)
  }else{
    db_aux_group <- df
  }
  # simplify groups to have less than 53 categories (rpart)
  db_aux_group$Sector <- db_aux_group$Sector %>%
    as.character() %>%
    factor()
  # tree parameters
  set.seed(seed = seed)
  min_n <- round(nrow(db_aux_group)*0.05+1,0)
  # train tree and save results with graphic
  fit_tree <- rpart(formula = tree_form, data = db_aux_group, method = "anova", x = T, y = T, control = rpart.control(minbucket = min_n))
  fancyRpartPlot(fit_tree, main = group)
  # output
  ny <- which(names(db_aux_group)== "prob_det")
  pred <- predict(object = fit_tree, newdata = db_aux_group[,-ny])
  levels <- table(pred)/nrow(db_aux_group)
  class <- FALSE
  aux_levels <- 0
  i <- nrow(levels)
  while(!clase&i>0){
    aux_levels <- levels[i] + aux_levels
    class <- (aux_levels >= (1-perc))
    if(class||i==1){
      umbral <- as.numeric(rownames(levels)[i+1])
    }
    i <- i-1
  }
  db_aux_group$pred <- as.numeric(pred)
  db_aux_group <- db_aux_group %>%
    dplyr::mutate(y_bin_tree = ifelse(pred >= umbral, 1, 0)) 
  db_aux_group$y_bin_tree <- factor(db_aux_group$y_bin_tree)
  ret <- list(df_group = db_aux_group, fit_tree = fit_tree, cut_tree = umbral)
  ret
}
# split y with quantiles -----
y_quantile_f <- function(group = "Consumer", df, perc = 0.125, var_y = "prob_det", S_g = 0){
  # filter df by group
  if(S_g>0){
    db_aux_group <- df %>% 
      dplyr::ungroup() %>% 
      dplyr::filter(group == group)
  }else{
    db_aux_group <- df
  }
  # get barrier from quantiles of pred_det dist
  quant <- quantile(x = db_aux_group[[var_y]], probs = 1-perc, na.rm=T)
  # output
  db_aux_group <- db_aux_group %>%
    dplyr::mutate(y_bin_q = ifelse(get(var_y) >= quant, 1 ,0))
  db_aux_group$y_bin_q <- factor(db_aux_group$y_bin_q)
  ret <- list(df_group = db_aux_group, cut_q = quant)
  ret
}
# split y with its mean -----
y_mean_f <- function(group = "Consumer", df, var_y = "prob_det", S_g = 0){
  # filter df by group
  if(S_g>0){
    db_aux_group <- df %>% 
      dplyr::ungroup() %>% 
      dplyr::filter(group == group)
  }else{
    db_aux_group <- df
  }
  # output
  mean <- mean(db_aux_group[[var_y]], na.rm = T)
  db_aux_group <- db_aux_group %>%
    dplyr::mutate(y_bin_mean = ifelse(get(var_y) >= mean, 1 ,0))
  db_aux_group$y_bin_mean <- factor(db_aux_group$y_bin_mean)
  ret <- list(df_group = db_aux_group, cut_mean = mean)
  ret
}
## general function ----
y_f <- function(S_group = "Consumer", df = db_y, tree_form, seed = 1, perc = 0.125, var_y = "prob_det", f = "q"){
  # f is the parameter that indicates which function use to split the deterioration probability
  #   options are :   t ~ tree;  q ~ quantile;  m ~ mean
  f <- str_to_lower(f)
  if(!f%in%c("t","q","m")){
    message("Function not found (f parameter), defaulting to q (quantile split).")
    f <- "q"
  }
  df_g <- df %>% 
    dplyr::filter(group == S_group )
  if(f == "t"){
    ret_list <- y_tree_f(group = group, df = df_g, tree_form = tree_form, seed = seed, perc = perc, S_g = 0)
  }else{
    if(f == "q"){
      ret_list <- y_quantile_f(group = group, df = df_g, perc = perc, var_y = var_y, S_g = 0)
    }else{
      ret_list <- y_mean_f(group = group, df = df_g, var_y = var_y, S_g = 0)
    }
  }
  return(ret_list)
}
#### output ----
save(y_tree_f, y_quantile_f, y_mean_f, y_f, file = "4_MetricaProbabilidadDeterioro/4bis_MetricaClusterDeterioro.RData")
