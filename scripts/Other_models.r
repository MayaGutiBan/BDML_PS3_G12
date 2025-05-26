library(pacman) 

# Cargar las librerías listadas e instalarlas en caso de ser necesario
p_load(tidyverse, # Manipular dataframes
       stringi, # Manipular cadenas de texto
       rio, # Importar datos fácilmente
       sf, # Leer/escribir/manipular datos espaciales
       tidymodels, # entrenamiento de modelos
       spatialsample) # Muestreo espacial para modelos de aprendizaje automático

library(pacman) 

p_load(tidyverse, stringi, rio, sf, tidymodels, spatialsample, caret)

p_load("caret")


setwd("/Users/carlosmanjarres/Desktop/Big_data/uniandes-bdml-2025-10-ps-3")

### separate on train and validation 
db <- readRDS("train_clean.RDS")


test <- db %>% filter(localidad == "Localidad Chapinero")
train <- db %>% filter(localidad != "Localidad Chapinero")

test <- st_as_sf(test, sf_column_name = "geometry", crs = 4326)
train <- st_as_sf(train, sf_column_name = "geometry", crs = 4326)

#### Make the folds taking into account the Neighborhood

set.seed(86936)
folds <- spatial_block_cv(train, v = 5)

# Crear una lista con los índices de entrenamiento para cada fold
index_list <- map(folds$splits, ~ .x$in_id)

fitControl <- trainControl(method = "cv", index = index_list)


### Random forest model 

colnames(train)

RF_Grid <- train(
  price ~ bedrooms+ distancia_parque + near_tm + surface_total + bathrooms + distancia_hospital + dist_to_cai 
  + surface_covered + property_type + num_restaurantes_700m + near_cai + rooms + dist_to_tm , 
  data = train,
  method = "ranger",
  trControl = fitControl,
  tuneGrid = expand.grid(
    mtry = c(2,3,4),
    splitrule = "variance",
    min.node.size = c(3, 5,8)
  ),
  metric = "MAE", 
  importance = "impurity"
) 

### Best tune mtry = 4, min node = 3 

### Predictions val set 

test$price_hat<-predict(RF_Grid,newdata = test)

mean(abs(test$price-test$price_hat))


### Kaggle Predictios 

kaggle <- read_rds("cleantest.RDS")

kaggle$price <- predict(RF_Grid,newdata = kaggle)

kaggle <- kaggle %>% select(property_id, price)

write.csv(kaggle, "submission_TREE_Model.csv", row.names = FALSE)

