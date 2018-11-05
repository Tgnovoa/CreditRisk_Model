## Create groups to train the model and split the database into training and testing dataframes
#### libraries ----
library(tidyverse)
library(magrittr)
library(stringr)
library(xts)

setwd("C:/Users/gnovoa/Desktop/Santiago/Riesgos/Proyectos/CreditRisk_Model/proyecto/1_datos")
##### 0.0 LDA (split Sectors into groups)-----------
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
lda_train_tabla %>%
  ggplot(aes(y=pred, x=Sector, colour=factor(pred==Sector))) + geom_jitter(alpha=0.2, show.legend = F) +
  theme_bw() +ylab("prediction") + xlab("Original Sector") + ggtitle("LDA Group Classification")

as.data.table(table(lda_train_tabla[,1:2]))%>%
  ggplot(aes(x=pred,y=N,colour=factor(pred==Sector))) + geom_point(size=3.5, show.legend = F)+
  facet_wrap(~Sector,scales="free") + 
  theme_light() + theme(axis.text.x = element_text(angle=45,size=8,hjust=1,vjust=1))

lda_train_plot <- data.frame(LD1 = lda.ej1.values$x[,1] %>% as.double(), 
              LD2 = lda.ej1.values$x[,2] %>% as.double(),
              Sector = droplevels(lda[train_notNA,"Sector"]),
              predSector = lda.ej1.values$class %>% as.vector())
lda_train_plot_names <- lda_train_plot %>% 
  dplyr::group_by(Sector) %>% 
  dplyr::summarise(LD1 = median(LD1, na.rm = T),
                   LD2 = median(LD2, na.rm = T))
lda_train_plot %>% 
  ggplot(aes(x = LD1, y = LD2, colour = Sector)) + 
  geom_point(alpha = 0.2) + theme_bw() + 
  geom_label(data = lda_train_plot_names, 
             aes(label = Sector))
lda_train_plot_names2 <- lda_train_plot %>% 
  dplyr::group_by(predSector) %>% 
  dplyr::summarise(LD1 = median(LD1, na.rm = T),
                   LD2 = median(LD2, na.rm = T))
lda_train_plot %>% 
  ggplot(aes(x = LD1, y = LD2, colour = predSector)) + 
  geom_point(alpha = 0.2) + theme_bw() + 
  geom_label(data = lda_train_plot_names2, 
             aes(label = predSector, colour = predSector))

lda.test1.values <- predict(lda_ej1, newdata = na.omit(lda_test_mod[,-c(1)]))
test_notNA <- as.double(row.names(na.omit(lda_test_mod[,-c(1)])))
lda_test_tabla <- data.frame(pred=droplevels(lda.test1.values$class),Sector=droplevels(na.omit(lda_test_mod)$Sector))
lda_test_tabla %>%
  ggplot(aes(y=pred,x=Sector,colour=factor(pred==Sector))) + geom_jitter(alpha=0.7) +
  theme_bw() 
confusionMatrix(table(lda_test_tabla))
as.data.table(table(lda_test_tabla[,1:2]))%>%
  ggplot(aes(x=pred,y=N,colour=factor(pred==Sector))) + geom_point(size=3.5, show.legend = F)+
  facet_wrap(~Sector,scales="free") + 
  theme_light() + theme(axis.text.x = element_text(angle=45,size=8,hjust=1,vjust=1))

lda_test_plot <- data.frame(LD1 = lda.test1.values$x[,1] %>% as.double(), 
                             LD2 = lda.test1.values$x[,2] %>% as.double(),
                             Sector = droplevels(lda[test_notNA,"Sector"]),
                             predSector = lda.test1.values$class %>% as.vector())
lda_test_plot_names <- lda_test_plot %>% 
  dplyr::group_by(Sector) %>% 
  dplyr::summarise(LD1 = median(LD1, na.rm = T),
                   LD2 = median(LD2, na.rm = T))
lda_test_plot %>% 
  ggplot(aes(x = LD1, y = LD2, colour = Sector)) + 
  geom_point(alpha = 0.2) + theme_bw() + 
  geom_label(data = lda_test_plot_names, 
             aes(label = Sector))

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
#### Restrictions to split the databases (enough observations for each Sector) ----





