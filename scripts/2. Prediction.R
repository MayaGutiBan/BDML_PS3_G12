#------------------------------------------------------------------------------#
#                     Problem Set 3: Making Money with ML                      #
#                                  Models                                      #     
#------------------------------------------------------------------------------#


#1.Preliminaries

rm(list=ls())

# Cargar pacman (contiene la función p_load)
library(pacman) 

# Cargar las librerías listadas e instalarlas en caso de ser necesario

# install.packages("rlang", repos = "https://cran.r-project.org")
# install.packages("cli", repos = "https://cran.r-project.org")

p_load(tidyverse, # Manipular dataframes
       gridExtra, # Para graficar en grid
       rio, # Import data easily
       plotly, # Gráficos interactivos
       leaflet, # Mapas interactivos
       tmaptools, # geocode_OSM()
       sf, # Leer/escribir/manipular datos espaciales
       osmdata, # Get OSM's data 
       tidymodels,
       stargazer,
       workflowsets,
       recipes,
       parsnip,
       tidyr,
       dials,
       rlang,
       dplyr,
       cli,
       visdat) #para modelos de ML



#2.  Data import
train <- read.csv("stores/train_processed.csv") # 38644 observaciones
test <- read.csv("stores/test_processed.csv") # 10286 observaciones

#3. Prediction EN


#-------------------------------
# Recetas de preprocesamiento
#-------------------------------

rec_1 <- recipe(price ~ dist_parque + dist_hospt + dist_mall + dist_pt + dist_sch + score_palabras_positivas + rooms + bathrooms + property_type_apto + surface_total + surface_covered, data = train) %>%
  step_interact(terms = ~ dist_parque:property_type_apto) %>% 
  step_zv(all_predictors()) %>% 
  step_normalize(all_predictors())

rec_2 <- recipe(price ~ dist_parque  + dist_hospt + dist_mall + dist_pt + dist_sch + score_palabras_positivas + bathrooms + property_type_apto + surface_total + surface_covered, data = train) %>%
  step_interact(terms = ~ dist_sch:property_type_apto) %>% 
  step_interact(terms = ~ dist_mall:property_type_apto) %>% 
  step_poly(dist_parque, degree = 2) %>%
  step_zv(all_predictors()) %>% 
  step_normalize(all_predictors())

#-------------------------------
# Especificación del modelo
#-------------------------------
elastic_net_spec <- linear_reg(
  penalty = tune(), 
  mixture = tune()
) %>%
  set_engine("glmnet")

# Grilla de hiperparámetros
# library(tidyr)
grid_values <- grid_regular(penalty(range = c(-2, 1)), levels = 50) %>%
  expand_grid(mixture = c(0, 0.25, 0.5, 0.75, 1))

#-------------------------------
# Validación cruzada
#-------------------------------
# library(tidymodels)
set.seed(36453856)
db_fold <- vfold_cv(train, v = 5)

#-------------------------------
# Workflows
#-------------------------------
workflow_1 <- workflow() %>% 
  add_recipe(rec_1) %>%
  add_model(elastic_net_spec)

workflow_2 <- workflow() %>% 
  add_recipe(rec_2) %>%
  add_model(elastic_net_spec)

#-------------------------------
# Entrenamiento y tuning
#-------------------------------
set.seed(36453856)
tune_res1 <- tune_grid(
  workflow_1,
  resamples = db_fold,
  grid = grid_values,
  metrics = metric_set(rmse)
)

set.seed(36453856)
tune_res2 <- tune_grid(
  workflow_2,
  resamples = db_fold,
  grid = grid_values,
  metrics = metric_set(rmse)
)

# Selección del mejor modelo
best_penalty_1 <- select_best(tune_res1, metric = "rmse")
best_penalty_2 <- select_best(tune_res2, metric = "rmse")

# Finalizar workflow con mejores hiperparámetros
EN_final1 <- finalize_workflow(workflow_1, best_penalty_1)
EN_final2 <- finalize_workflow(workflow_2, best_penalty_2)

# Ajuste final sobre todo el train
EN_final1_fit <- fit(EN_final1, data = train)
EN_final2_fit <- fit(EN_final2, data = train)

#-------------------------------
# Predicciones sobre el test
#-------------------------------
predicciones_1 <- predict(EN_final1_fit, new_data = test)
predicciones_2 <- predict(EN_final2_fit, new_data = test)

# Agregar al test
test$pred1 <- predicciones_1$.pred
test$pred2 <- predicciones_2$.pred

#4. Exportar

sub_1 <- test %>%
  select(property_id, pred1) %>%
  rename(price = pred1)


sub_2 <- test %>%
  select(property_id, pred2) %>%
  rename(price = pred2)

write.csv(sub_1, "stores/sub_1.csv", row.names = FALSE)
write.csv(sub_2, "stores/sub_2.csv", row.names = FALSE)


#4. Predicción redes neuronales

# install.packages("keras")
library(keras)


x_train <- model.matrix(price ~ . - property_id, data = train) %>% scale()
y_train <- train$price

# Solo usamos las variables disponibles para predecir
test <- test %>%
  mutate(score_palabras_positivas = if_else(is.na(score_palabras_positivas), 0L, score_palabras_positivas))

x_test <- model.matrix(~ . - property_id - price, data = test)[, -1] %>% scale(center = attr(x_train, "scaled:center"), 
                                                                               scale = attr(x_train, "scaled:scale"))



pred_nn <- predict(modnn, x_test)

# Armas el dataframe final con predicciones
submission_nn <- test %>%
  select(property_id) %>%
  mutate(price = as.vector(pred_nn))





# Definir y compilar el modelo

modnn <- keras_model_sequential() %>%
  layer_dense(units = 50, activation = "relu", input_shape = ncol(x_train)) %>%
  layer_dropout(rate = 0.4) %>%
  layer_dense(units = 1)

modnn %>% compile(
  loss = "mse",
  optimizer = optimizer_rmsprop(),
  metrics = list("mean_absolute_error")
)

# Entrenamiento

history <- modnn %>% fit(
  x_train, y_train,
  epochs = 600,
  batch_size = 32,
  validation_split = 0.2 # usa 20% del train para validar internamente
)

# Predicción

pred_nn <- predict(modnn, x_test)

# Submision 

submission_nn <- test %>%
  select(property_id) %>%
  mutate(price = as.vector(pred_nn))  # as.vector para convertir matriz a vector







# Cargar keras si no lo has hecho
library(keras)
install_keras(method = "conda")

library(tensorflow)

library(reticulate)
use_python("C:/Users/jtriana/anaconda3/python.exe", required = TRUE)


# 1. Seleccionar variables predictoras
vars_modelo <- setdiff(colnames(train), c("property_id", "price"))

# 2. Crear matriz de predictores (X) y objetivo (y)
x_train <- train[, vars_modelo] %>%
  model.matrix(~ . , data = .)[, -1] %>%
  scale()

y_train <- train$price

# 3. Crear el modelo de red neuronal
modnn <- keras_model_sequential() %>%
  layer_dense(units = 50, activation = "relu", input_shape = ncol(x_train)) %>%
  layer_dropout(rate = 0.4) %>%
  layer_dense(units = 1)

# 4. Compilar el modelo
modnn %>% compile(
  loss = "mse",
  optimizer = optimizer_rmsprop(),
  metrics = list("mean_absolute_error")
)

# 5. Entrenar el modelo
history <- modnn %>% fit(
  x_train,
  y_train,
  epochs = 600,
  batch_size = 32,
  validation_split = 0.2
)

# 6. Preparar x_test con las mismas columnas y escalamiento
x_test <- test[, vars_modelo] %>%
  model.matrix(~ . , data = .)[, -1] %>%
  scale(center = attr(x_train, "scaled:center"),
        scale = attr(x_train, "scaled:scale"))

# 7. Predecir precios
pred_nn <- predict(modnn, x_test)

# 8. Crear base final con predicciones
sub_nn <- data.frame(
  property_id = test$property_id,
  price = as.numeric(pred_nn)
)


write.csv(sub_nn, "stores/submission_nn.csv", row.names = FALSE)