library(pacman) 
library(purrr)
library(stringr)

# Cargar las librerías listadas e instalarlas en caso de ser necesario
p_load(rio, ## read datasets
       stringi, # Manipular cadenas de texto
       tidyverse, # Manipular dataframes
       tm,   # para Text Mining
       tidytext, #Para tokenización
       stopwords,  # consultar stopwords
       tidymodels, # modelos de machine learning
       sf, # datos espaciales
       spatialsample # validación cruzada espacial
) 

library(keras)

setwd("/Users/carlosmanjarres/Desktop/Big_data/uniandes-bdml-2025-10-ps-3")

#### CARGAR BASE DE DATOS LIMPIA 

db <- readRDS("train_clean_2.RDS")

#### SELECCIÓNAR VARIABLES RELEVANTES 

db <- db %>% select(property_type,dist_to_tm,num_restaurantes_700m,near_cai,localidad,
                    tiene_terraza,surface_covered,price,bathrooms,bedrooms,price)


#### SELECCIONAR LOCALIDADES SIMILARES A CHAPINERO 

db <- db %>%
  filter(localidad == "Localidad Chapinero" |
           localidad == "Localidad Usaquén" |
           localidad == "Localidad Engativá" |
           localidad == "Localidad Suba" |
           localidad == "Localidad Teusaquillo")


val_set <- db %>% filter(localidad == "Localidad Chapinero")
train <- db %>% filter(localidad != "Localidad Chapinero")

### 1 . PREPROSSES DATA FOR KERAS ### 

# 
train <- train %>%
  mutate(tiene_terraza = ifelse(is.na(tiene_terraza), 
                                0,  
                                tiene_terraza))
train <- train %>% select(-localidad)

dmy <- caret::dummyVars(
  ~ .,
  data = train,
  sep = "_", # Separador para las variables dummy
  drop = TRUE,
  fullRank = TRUE # Evitar la multicolinealidad
)

train <- as.data.frame(predict(dmy, newdata = train))

X_train <- train %>% select(-price)
y_train <- train$price

X_train <- as.matrix(train %>% select(-price))
y_train <- as.matrix(train$price)

val_set <- val_set %>% select(-localidad)

dmy2 <- caret::dummyVars(
  ~ .,
  data = val_set,
  sep = "_", # Separador para las variables dummy
  drop = TRUE,
  fullRank = TRUE # Evitar la multicolinealidad
)

val_set <- as.data.frame(predict(dmy2, newdata = val_set))

X_test <- as.matrix(val_set %>% select(-price))
y_test <- as.matrix(val_set$price)


#### 2. Keras Model Definition Function ####

model <- keras_model_sequential() %>%
  layer_dense(units = 512, activation = "relu", input_shape = c(ncol(X_train))) %>%
  layer_dropout(rate = 0.2) %>% # Adding dropout for regularization
  layer_dense(units = 256, activation = "relu") %>%
  layer_dropout(rate = 0.2) %>%
  layer_dense(units = 256, activation = "relu") %>%
  layer_dropout(rate = 0.2) %>%
  layer_dense(units = 256, activation = "relu") %>%
  layer_dropout(rate = 0.2) %>%
  layer_dense(units = 1, activation = "relu", name = "capa_salida")


### 3.  model compillation ###
model %>% compile(
  optimizer = optimizer_adam(learning_rate = 0.001),
  loss = "mse",      # Mean Squared Error for regression
  metrics = list("mae") # Mean Absolute Error
)


#### 4. train model 

history <- model %>% fit(
  X_train, y_train,
  epochs = 100,
  batch_size = 32,
  validation_split = 0.2     # o usar x_val, y_val separados
)

plot(history)

summary(model)

#### Evaluar modelo con las observaciones de chapinero 

model %>% evaluate(as.matrix(X_test), y_test)

### kaggle submission 

test <- read_rds("cleantest.RDS")

test <- test %>%
  mutate(tiene_terraza = ifelse(is.na(tiene_terraza), 
                                0,  
                                tiene_terraza))

kaggle <- test%>% select( property_type,dist_to_tm,num_restaurantes_700m,near_cai,
                          tiene_terraza,surface_covered,bathrooms,bedrooms)

dmy3 <- caret::dummyVars(
  ~ .,
  data = kaggle,
  sep = "_", # Separador para las variables dummy
  drop = TRUE,
  fullRank = TRUE # Evitar la multicolinealidad
)

kaggle <- as.data.frame(predict(dmy3, newdata = kaggle))

prob_predictions <- model %>% 
  predict(as.matrix(kaggle), batch_size = 32)

submission_df <- data.frame(
  property_id = test$property_id, 
  price = as.vector(prob_predictions)     
)

any(is.na(submission_df))

write.csv(submission_df, "submission_NNM_New.csv", row.names = FALSE)

