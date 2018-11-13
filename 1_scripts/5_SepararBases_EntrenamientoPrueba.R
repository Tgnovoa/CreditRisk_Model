## Create groups to train the model and split the database into training and testing dataframes
#### libraries ----
library(tidyverse)
library(magrittr)
library(stringr)
library(xts)
library(lubridate)

setwd("C:/Users/gnovoa/Desktop/Santiago/Riesgos/Proyectos/CreditRisk_Model/proyecto/1_datos")
#### 0.0 LDA (split Sectors into groups)-----------
load("2_TransformacionBases/2_TransformacionBases.RData")
library(caret)
library(MASS)

lda <- db_fin %>%
  dplyr::filter(!is.na(Sector),!Sector%in%c("Sovereign","Financials"))
lda <- cbind(lda[,"Sector"],lda[sapply(lda,is.double)])
set.seed(1250890)
train <- sample(1:nrow(lda),size = (round(nrow(lda)*0.8,0)))
lda_train <- lda[train,]
lda_test <- lda[-train,]

lda_train %>% glimpse()

lda_train_mod <- lda_train[,-c(2,3)]
lda_test_mod <- lda_test[,-c(2,3)]
lda_train_mod$Sector <- factor(droplevels(lda_train_mod$Sector))
lda_test_mod$Sector <- factor(droplevels(lda_test_mod$Sector))
lda_ej1 <- lda(Sector~., data=lda_train_mod)
lda_ej1

lda.ej1.values <- predict(lda_ej1)
par(mar=c(1,1,1,1))
ldahist(data = lda.ej1.values$x[,1], g=lda.ej1.values$class)
ldahist(data = lda.ej1.values$x[,2], g=lda.ej1.values$class)

train_notNA <- as.double(row.names(model.frame(lda_ej1)))
(lda_train_tabla <- data.frame(pred=droplevels(lda.ej1.values$class),Sector=droplevels(lda[train_notNA,"Sector"])))
confusionMatrix(table(lda_train_tabla[,1:2]))
# lda_train_tabla %>%
#   ggplot(aes(y=pred, x=Sector, colour=factor(pred==Sector))) + geom_jitter(alpha=0.2, show.legend = F) +
#   theme_bw() +ylab("prediction") + xlab("Original Sector") + ggtitle("LDA Group Classification")

# as.data.table(table(lda_train_tabla[,1:2]))%>%
#   ggplot(aes(x=pred,y=N,colour=factor(pred==Sector))) + geom_point(size=3.5, show.legend = F)+
#   facet_wrap(~Sector,scales="free") + 
#   theme_light() + theme(axis.text.x = element_text(angle=45,size=8,hjust=1,vjust=1))

lda_train_plot <- data.frame(LD1 = lda.ej1.values$x[,1] %>% as.double(), 
              LD2 = lda.ej1.values$x[,2] %>% as.double(),
              Sector = droplevels(lda[train_notNA,"Sector"]),
              predSector = lda.ej1.values$class %>% as.vector())
lda_train_plot_names <- lda_train_plot %>% 
  dplyr::group_by(Sector) %>% 
  dplyr::summarise(LD1 = median(LD1, na.rm = T),
                   LD2 = median(LD2, na.rm = T))
# lda_train_plot %>% 
#   ggplot(aes(x = LD1, y = LD2, colour = Sector)) + 
#   geom_point(alpha = 0.2) + theme_bw() + 
#   geom_label(data = lda_train_plot_names, 
#              aes(label = Sector))
lda_train_plot_names2 <- lda_train_plot %>% 
  dplyr::group_by(predSector) %>% 
  dplyr::summarise(LD1 = median(LD1, na.rm = T),
                   LD2 = median(LD2, na.rm = T))
# lda_train_plot %>% 
#   ggplot(aes(x = LD1, y = LD2, colour = predSector)) + 
#   geom_point(alpha = 0.2) + theme_bw() + 
#   geom_label(data = lda_train_plot_names2, 
#              aes(label = predSector, colour = predSector))

lda.test1.values <- predict(lda_ej1, newdata = na.omit(lda_test_mod[,-c(1)]))
test_notNA <- as.double(row.names(na.omit(lda_test_mod[,-c(1)])))
lda_test_tabla <- data.frame(pred=droplevels(lda.test1.values$class),Sector=droplevels(na.omit(lda_test_mod)$Sector))
# lda_test_tabla %>%
#   ggplot(aes(y=pred,x=Sector,colour=factor(pred==Sector))) + geom_jitter(alpha=0.7) +
#   theme_bw() 
confusionMatrix(table(lda_test_tabla))
# as.data.table(table(lda_test_tabla[,1:2]))%>%
#   ggplot(aes(x=pred,y=N,colour=factor(pred==Sector))) + geom_point(size=3.5, show.legend = F)+
#   facet_wrap(~Sector,scales="free") + 
#   theme_light() + theme(axis.text.x = element_text(angle=45,size=8,hjust=1,vjust=1))

lda_test_plot <- data.frame(LD1 = lda.test1.values$x[,1] %>% as.double(), 
                             LD2 = lda.test1.values$x[,2] %>% as.double(),
                             Sector = droplevels(lda[test_notNA,"Sector"]),
                             predSector = lda.test1.values$class %>% as.vector())
lda_test_plot_names <- lda_test_plot %>% 
  dplyr::group_by(Sector) %>% 
  dplyr::summarise(LD1 = median(LD1, na.rm = T),
                   LD2 = median(LD2, na.rm = T))
# lda_test_plot %>% 
#   ggplot(aes(x = LD1, y = LD2, colour = Sector)) + 
#   geom_point(alpha = 0.2) + theme_bw() + 
#   geom_label(data = lda_test_plot_names, 
#              aes(label = Sector))

# create 4 groups:
#       Consumer ~ c(Consumer Discretionary, Consumer Staples)
#       Services ~ c(Health Care, Information Technology, Telecommunications)
#       Industrials ~ c(Energy, Industrials, Materials, Utilities)
#       Real Estate ~ c(Real Estate)

db_fin <- db_fin %>% 
  dplyr::mutate(group = factor(case_when(Sector %in% c("Consumer Discretionary", "Consumer Staples") ~ "Consumer",
                                         Sector %in% c("Health Care", "Information Technology", "Telecommunications") ~ "Services",
                                         Sector %in% c("Energy", "Industrials", "Materials", "Utilities") ~ "Industrials",
                                         Sector %in% c("Real Estate") ~ "RealEstate",
                                         TRUE ~ "Other"),
                               levels = c("Consumer", "Services", "Industrials", "RealEstate", "Other")))
#### load db_detprob_bis and the required functions to split prob_det into boolean ----
load("1_LecturaBases/1_LecturaBases.RData")
load("4_MetricaProbabilidadDeterioro/4_MetricaProbabilidadDeterioro.RData")
load("4_MetricaProbabilidadDeterioro/4bis_MetricaClusterDeterioro.RData")
db_y <- db_detprob_bis %>% 
  dplyr::left_join(read_sector %>% 
                     dplyr::select(CIQ_ID, `Tresalia Industry`)) %>% 
  dplyr::mutate(Sector =`Tresalia Industry`) %>% 
  dplyr::mutate(group = factor(case_when(Sector %in% c("Consumer Discretionary", "Consumer Staples") ~ "Consumer",
                                         Sector %in% c("Health Care", "Information Technology", "Telecommunications") ~ "Services",
                                         Sector %in% c("Energy", "Industrials", "Materials", "Utilities") ~ "Industrials",
                                         Sector %in% c("Real Estate") ~ "RealEstate",
                                         TRUE ~ "Other"),
                               levels = c("Consumer", "Services", "Industrials", "RealEstate", "Other"))) %>% unique()
db_y %>% 
  ggplot(aes(x = DateQ, y = prob_det, colour = Rating)) + 
  geom_point(alpha = 0.2) + 
  facet_wrap(~Rating) + theme_bw()
db_y %>% 
  ggplot(aes(x = DateQ, y = prob_det, colour = Rating)) + 
  geom_point(alpha = 0.2) + 
  facet_wrap(~group) + theme_bw()
db_y %>% 
  dplyr::filter(!group%in%c("Other"), !Rating=="NA") %>% 
  ggplot(aes(x = prob_det, fill = group)) + 
  geom_histogram(position = "stack", col = "black", bins = 20) + 
  facet_wrap(~Rating, scales = "free_y", ncol = 6) + theme_bw() + 
  scale_x_continuous(minor_breaks = seq(0,1,0.05), breaks = seq(0,1,0.2))
db_y %>% 
  dplyr::filter(group%in%c("Consumer"), !Rating=="NA") %>% 
  ggplot(aes(x = prob_det, fill = Sector)) + 
  geom_histogram(position = "stack", col = "black", bins = 20) + 
  facet_wrap(~Rating, scales = "free_y", ncol = 6) + theme_bw() + 
  scale_x_continuous(minor_breaks = seq(0,1,0.05), breaks = seq(0,1,0.2))
db_y %>% 
  dplyr::filter(group%in%c("Services"), !Rating=="NA") %>% 
  ggplot(aes(x = prob_det, fill = Sector)) + 
  geom_histogram(position = "stack", col = "black", bins = 20) + 
  facet_wrap(~Rating, scales = "free_y", ncol = 6) + theme_bw() + 
  scale_x_continuous(minor_breaks = seq(0,1,0.05), breaks = seq(0,1,0.2))
db_y %>% 
  dplyr::filter(group%in%c("Industrials"), !Rating=="NA") %>% 
  ggplot(aes(x = prob_det, fill = Sector)) + 
  geom_histogram(position = "stack", col = "black", bins = 20) + 
  facet_wrap(~Rating, scales = "free_y", ncol = 6) + theme_bw() + 
  scale_x_continuous(minor_breaks = seq(0,1,0.05), breaks = seq(0,1,0.2))
db_y %>% 
  dplyr::filter(group%in%c("RealEstate"), !Rating=="NA") %>% 
  ggplot(aes(x = prob_det, fill = Sector)) + 
  geom_histogram(position = "stack", col = "black", bins = 20) + 
  facet_wrap(~Rating, scales = "free_y", ncol = 6) + theme_bw() + 
  scale_x_continuous(minor_breaks = seq(0,1,0.05), breaks = seq(0,1,0.2))
db_y %>% 
  dplyr::group_by(group) %>% 
  dplyr::summarise(min = min(prob_det, na.rm = T),
                   q1 = quantile(prob_det, probs = c(0.25), na.rm = T),
                   q2 = quantile(prob_det, probs = c(0.5), na.rm = T),
                   d7 = quantile(prob_det, probs = c(0.7), na.rm = T),
                   q3 = quantile(prob_det, probs = c(0.75), na.rm = T),
                   d8 = quantile(prob_det, probs = c(0.8), na.rm = T),
                   max = max(prob_det, na.rm = T)) %>% View()
perc_cut <- 1-0.775
index_group <- levels(db_y$group)[1]
aux_y_f <- y_f(df = db_y , S_group = index_group, perc = perc_cut, f = "q")
db_y_agg <- aux_y_f$df_group
aux_y_f$cut_q

for(i in 2:4){
  index_group <- levels(db_y$group)[i]
  aux_y_f <- y_f(df = db_y , S_group = index_group, perc = perc_cut, f = "q")
  db_y_agg <- rbind(db_y_agg, aux_y_f$df_group)
  print(aux_y_f$cut_q)
}

# db_y_agg %>% dplyr::filter(y_bin_q == 0) %>% dplyr::select(Rating) %>% .$Rating %>% table()
# db_y_agg %>% dplyr::filter(y_bin_q == 1) %>% dplyr::select(Rating) %>% .$Rating %>% table()

#### Restrictions to split the databases (enough observations for each Sector) ----
# training databases should have, at least, 10% of their data from "risky companies" (in their respective time) 
# and include, at least, 30% of each Sectors data 
## function to split database into training set, testing set ----
tat_sector <- function(df_s, t_perc = 0.7, s_perc = 0.3, var_y = "y_bin_q", var_s = "Sector", seed = 1){
  # mutate Sector to have less factors
  df_s_aux <- df_s %>% 
    dplyr::mutate(Sector = factor(as.character(get(var_s))))
  # split dataframe to do cluster sampling
  table_s <- df_s_aux$Sector %>% table()
  n_sec <- as.integer(round(table_s*s_perc,0)+1)
  n_df_s <- as.integer(round(nrow(df_s)*t_perc,0)+1)
  if(sum(n_sec)>=n_df_s){
    message("s_perc higher than possible, defaulting to 0.1")
    n_sec <- as.integer(round(table_s*0.1,0)+1)
  }
  df_s_aux <- df_s_aux %>% 
    dplyr::arrange(CIQ_ID) %>% 
    dplyr::mutate(ind = 1:nrow(df_s_aux))
  # create training vector (making sure sectors are well distributed in sample)
  n_sample <- c()
  for(i in 1:nrow(table_s)){
    set.seed(seed+i)
    n_aux <- sample(x = 1:(table_s[i] %>% as.integer()), size = n_sec[i])
    ind_aux <- df_s_aux %>% 
      dplyr::filter(Sector == unique(df_s_aux$Sector)[i]) %>% 
      dplyr::select(ind) %>% .$ind %>% as.vector()
    n_sample <- c(n_sample,ind_aux[n_aux])
  }
  # make sure that there are sufficient 1s in the training sample
  n_y <- df_s_aux %>% 
    dplyr::filter(ind %in% n_sample) %>% 
    dplyr::filter(get(var_y)==1) %>% 
    nrow()
  rest_y <- max(c(as.integer(round(n_df_s*0.1,0)+1) - n_y,1))
  df_s_aux <- df_s_aux %>% 
    dplyr::filter(!ind%in%n_sample) 
  table_rest_y <- df_s_aux %>% 
    dplyr::filter(get(var_y)==1) %>% nrow()
  
  if(table_rest_y<rest_y){
    rest_y <- table_rest_y
  }
  set.seed(seed)
  n_aux <- sample(x = 1:table_rest_y, size = rest_y)
  ind_aux <- df_s_aux %>% 
    dplyr::filter(get(var_y)==1) %>% 
    dplyr::select(ind) %>% .$ind %>% as.vector()
  n_sample <- c(n_sample,ind_aux[n_aux])
  # complete training sample
  n_diff <- n_df_s - length(n_sample)
  df_s_aux <- df_s_aux %>% 
    dplyr::filter(!ind%in%n_sample)
  n_rest <- nrow(df_s_aux)
  n_aux <- sample(x = 1:n_rest, size = n_diff)
  ind_aux <- df_s_aux %>% 
    dplyr::select(ind) %>% .$ind %>% as.vector()
  n_sample <- c(n_sample, ind_aux[n_aux])
  return(n_sample)
}
train_and_test <- function(df = db_y_agg, t_perc = 0.7, pred = F, pred_n = 1){
  df <- df %>% 
    dplyr::mutate(ind_aux = 1:nrow(df))
  t_perc <- max(t_perc, 0.5)
  pred_n <- max(1,pred_n)
  d_Date <- df$DateQ %>% unique()
  if(pred){
    pred_Date <- df$DateQ %>% last(pred_n)
    d_Date <- df %>% dplyr::filter(!DateQ%in%pred_Date) %>% dplyr::select(DateQ) %>% .$DateQ %>% unique()
  } 
  db_tat_ret <- c()
  i_group <- df$group %>% unique()
  for(i in 1:length(i_group)){
    db_tat_aux <- df %>% 
      dplyr::filter(DateQ %in% d_Date) %>% 
      dplyr::filter(group == i_group[i]) %>% 
      dplyr::arrange(CIQ_ID)
    n_tat_aux <- tat_sector(df_s = db_tat_aux, t_perc = t_perc, seed = i)
    n_tat <- db_tat_aux[n_tat_aux,]$ind_aux
    db_tat_ret <- c(db_tat_ret,n_tat)
  }
  return(db_tat_ret)
}
# split training and testing set (and 1 observation for future predictions and as an example)----
t_perc <- 0.8
pred <- T
pred_n <- 1
pred_date <- db_y_agg$DateQ %>%  last(pred_n)
train_sample <- train_and_test(df = db_y_agg, t_perc = t_perc, pred = pred, pred_n = pred_n)

db_split <- db_y_agg %>%
  dplyr::mutate(ind = 1:nrow(db_y_agg)) %>% 
  dplyr::arrange(CIQ_ID, Date) %>%
  dplyr::select(-c(`Tresalia Industry`)) %>% 
  dplyr::mutate(tat = factor(ifelse((ind%in%train_sample), "train", 
                                    ifelse(DateQ%in%pred_date, "pred", "test")))) %>% 
  dplyr::select(-c(ind))
#### output ----
save(db_split, file = "5_SeparaBases_EntrenamientoPrueba/5_SeparaBases_EntrenamientoPrueba.RData")
