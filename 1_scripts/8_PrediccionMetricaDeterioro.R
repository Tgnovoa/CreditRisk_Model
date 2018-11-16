##    Prediccion en T+1 de la metrica de deterioro para diferentes empresas del benchmark 
##  a traves del modelo de riesgo de credito.

#### Funcion general ----
# Las especificaciones de la función se encuentran en el archivo de word del modelo.
## funciones : 
# predecir ~ 1 
# analizar ( nuevos ) ~ 2
# agregar ( nuevos ) ~ 3
# actualizar ~ 4
## archivos necesarios : 
# 1 ~ fin.csv, edf.csv, ratings.csv
# 2 ~ fin.csv, edf.csv, ratings.csv, actualizar archivo de sectores en excel.
# 3 ~ fin.csv, edf.csv, ratings.csv, port_pred, bench_pred, actualizar archivo de sectores.
# 4 ~ fin.csv, edf.csv, ratings.csv, port_pred, bench_pred
load(file = "7bis_PruebaModelo_funciones/funciones_modelo.RData")
read_data_fix <- T # si se quieren usar otras bases de datos, no las guardadas en las carpetas del modelo, cambiar a F
lapply(load_db_model(), load, .GlobalEnv)
if(read_data_fix){
  sect <- "../1_usage/CV_Portfolio_Benchmark_Sector.csv"
  fin <- "../1_usage/PortfolioData/CIQ_fin/Portfolio_CIQfin_data_paste.csv"
  edf <- "../1_usage/PortfolioData/EDF/Portfolio_EDF_data_paste.csv"
  ratings <- "../1_usage/PortfolioData/Ratings/Portfolio_Rating_data_paste.csv"
}

port_fin <- read_model_data(data_path = fin, skip = 2)
read_sector <- read_model_data(data_path = sect, data_type = "sector")
read_edf <- read_model_data(data_path = edf, data_type = "edf", skip_row = 4)
read_ratings <- read_model_data(data_path = ratings, data_type = "ratings", skip_row = 2) %>% 
  dplyr::mutate(DateQ = as.yearqtr(Date))
db_fin <- data_wrangling(port_fin)
db_model_glmm <- glmm(db_fin)
db_model_tree <- tree(db_model_glmm, read_edf = read_edf, read_ratings = read_ratings)
db_output <- output_model(db_model_tree)

