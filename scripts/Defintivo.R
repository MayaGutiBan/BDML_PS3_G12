---
title: "Solución Taller No. 3"
author: "Sebastián"
date: "2025-05-20"
output: html_document
---

```{r}
if(!require(pacman)) install.packages("pacman") ; require(pacman)
if(!require(janitor)) install.packages("janitor") ; require(janitor)
if(!require(readxl)) install.packages("readxl") ; require(readxl)
if(!require(writexl)) install.packages("writexl") ; require(writexl)
if(!require(sf)) install.packages("sf") ; require(sf)
if(!require(geojsonio)) install.packages("geojsonio") ; require(geojsonio)
if(!require(GGally)) install.packages("GGally") ; require(GGally)
if(!require(leaflet)) install.packages("leaflet") ; require(leaflet)
if(!require(VIM)) install.packages("VIM") ; require(VIM)
if(!require(stringr)) install.packages("stringr") ; require(stringr)
if(!require(readr)) install.packages("readr") ; require(readr)
if(!require(osmdata)) install.packages("osmdata") ; require(osmdata)

if(!require(jsonlite)) install.packages("jsonlite") ; require(jsonlite) #No
if(!require(rvest)) install.packages("rvest") ; require(rvest) #No
if(!require(httr)) install.packages("httr") ; require(httr) #No

p_load(rio, # import/export data
       tidyverse, # Manipular dataframes
       glmnet, # To implement regularization algorithms. 
       caret, # Creating predictive models
       scatterplot3d, # For 3D visualization
       plotly, # For interactive 3D plots
       skimr, #sumary data
       gridExtra, #visualizaing missing data
       corrplot, #correlation PLots
       stargazer, #tables/outpot to TEX
       MASS, #Various statistcal functions
       readxl,
       readr,
       writexl,
       stringi, #Manipular cadenas de texto
       tm,  # Para text mining
       tidytext,  #Para tokenización
       stopwords,  #consultar stopwords
       tidymodels, #modelos de machine learning
       sf, # leer/escribir/manipular datos espaciales
       spatialsample,  #Validación cruzada espacial
       janitor, 
       leaflet, # Mapas interactivos
       VIM,  # Imputar por Vercino más cercano (KNN)
       ggcorrplot,
       janitor,
       jsonlite,
       httr,
       plotly, # Gráficos interactivos
)
```


# -----------------------------------------------------------------------------------------------------------------------------
# 1. Cargando base de datos
# -----------------------------------------------------------------------------------------------------------------------------
```{r}
library(readr)

test <- read.csv("https://raw.githubusercontent.com/Jusebastru/Probelm_Set_3/refs/heads/main/test.csv", 
                 sep = ",", 
                 stringsAsFactors = TRUE)

train <- read.csv("https://raw.githubusercontent.com/Jusebastru/Probelm_Set_3/refs/heads/main/train.csv", 
                  sep = ",", 
                  stringsAsFactors = TRUE)

sumission <- read.csv("https://raw.githubusercontent.com/Jusebastru/Probelm_Set_3/refs/heads/main/submission_template.csv", 
                      sep = ",", 
                      stringsAsFactors = TRUE)

##### Se descarga dede https://github.com/Jusebastru/Probelm_Set_3/blob/main/upz-bogota.xlsx 

upz_bogota <- st_read("C:/Users/Juancho/OneDrive/Desktop/MeCA/Big Data y Maching Learning/Taller No. 3./upz-bogota.geojson",
                      quiet = TRUE)

upz_chapinero <- upz_bogota %>%
  filter(codigo_upz %in% c(88, 89, 90, 97, 99))

```

# -----------------------------------------------------------------------------------------------------------------------------
# 2. Función para variables decriptivas
# -----------------------------------------------------------------------------------------------------------------------------
```{r}

###########################################    2.1 Pasar de texto a datos 

limpiar_y_enriquecer <- function(df) {
  df <- df %>%
    # Limpieza del texto
    mutate(description = str_to_lower(description)) %>%
    mutate(description = stringi::stri_trans_general(description, "Latin-ASCII")) %>%
    mutate(description = str_replace_all(description, "[^[:alnum:]]", " ")) %>%
    mutate(description = str_trim(gsub("\\s+", " ", description))) %>%
    
    # Inferencia del tipo de propiedad
    mutate(property_type_2 = ifelse(grepl("casa", description), "Casa", property_type)) %>%
    mutate(property_type_2 = ifelse(grepl("apto|apartamento", description), "Apartamento", property_type_2)) %>%
    
    # Extracción de número de alcobas
    mutate(
      alcoba_raw = str_extract(description, "\\d+\\s*(alcobas?|cuartos?|habitaciones?)"),
      alcobas = str_extract(alcoba_raw, "\\d+") %>% as.integer(),
      bedrooms = if_else(is.na(bedrooms), alcobas, bedrooms)
    ) %>%
    
    # Extracción de si tiene terraza
    mutate(
      tiene_terraza = if_else(str_detect(description, regex("terraza", ignore_case = TRUE)), 1, 0)
    ) %>%
    
    # Extracción de número de baños
    mutate(
      baño_raw = str_extract(description, "\\d+\\s*(baños?|baño|banos|bano)"),
      baños = str_extract(baño_raw, "\\d+") %>% as.integer(),
      bathrooms = if_else(is.na(bathrooms), baños, bathrooms)
    ) %>%
    
    # Extracción de área construida y asignación a surface
    mutate(
      area_matches = str_extract_all(description,  "\\d+\\s*(salón?|saln|salon)"),
      area_nums = map(area_matches, ~ {
        nums <- str_extract_all(.x, "\\d{1,4}") %>% unlist() %>% as.integer()
        nums[!is.na(nums) & nums >= 20 & nums <= 1000]
      }),
      area_construida = map_int(area_nums, ~ if(length(.x) > 0) max(.x) else NA_integer_),
      surface_covered = if_else(is.na(surface_covered), area_construida, surface_covered),
      surface_total = if_else(is.na(surface_total), area_construida, surface_total)
    ) %>%
    
    # Extracción de metros de terraza
    mutate(
      terraza_raw = str_extract(description, "(de\\s+)?terraza|patio|balcon\\s+\\d+\\s*(m2|mts|metros)?"),
      m2_terraza = str_extract(terraza_raw, "\\d+") %>% as.integer()
    ) %>%
    
    # Ajuste final de surface_total si aún está NA
    mutate(
      surface_total = if_else(
        is.na(surface_total) & !is.na(surface_covered),
        surface_covered + coalesce(m2_terraza, 0),
        surface_total
      )
    ) %>%
    
    # Eliminación de columnas auxiliares
    dplyr::select(-alcoba_raw, -alcobas, -baño_raw, -baños, -area_construida, -terraza_raw, -m2_terraza)
  
  return(df)
}

train <- limpiar_y_enriquecer(train)
test <- limpiar_y_enriquecer(test)
```

###### 2.1. Visualizando valores

Las columnas creadas para area_matches y area_nums en el segmento de extracción de área construida y asignación de surface, permite observar unos valores atípicos. Los cuales se pueden corregir al leer la descripción de su observación. con respecto a terraza, se pueden observar 2 missing_values para la base test, mientras que para train se observan 9 missing_values los cuales se pueden eliminar o reemplazar por cero. Ambas bases carecen de información en bathrooms, rooms, surface_covered, surface_total

```{r}
glimpse(test)
skimr::skim(test)
visdat::vis_dat(test)
```

```{r}
glimpse(train)
skimr::skim(train)
visdat::vis_dat(train)
```

# -----------------------------------------------------------------------------------------------------------------------------
# 3. Función para variables espaciales
# -----------------------------------------------------------------------------------------------------------------------------
```{r}

###########################################    3.1 Restaurantes, Hospitales y Parques

# SOLUCIÓN 1: Reinstalar el paquete curl
install.packages("curl", dependencies = TRUE)

# SOLUCIÓN 2: Actualizar todos los paquetes involucrados
install.packages(c("curl", "osmdata", "sf", "dplyr"), dependencies = TRUE)

# SOLUCIÓN 3: Función corregida con manejo de errores y dependencias explícitas

agregar_vars_espaciales <- function(sf_data, ciudad = "Bogotá Colombia", Walkingdistance = 700) {
  # Cargar y verificar las bibliotecas necesarias
  required_packages <- c("dplyr", "sf", "osmdata", "curl")
  
  # Verificar e instalar paquetes faltantes
  for(pkg in required_packages) {
    if(!requireNamespace(pkg, quietly = TRUE)) {
      message(paste("Instalando paquete:", pkg))
      install.packages(pkg)
    }
    library(pkg, character.only = TRUE)
  }
  
  # Verificar que sf_data sea un objeto sf
  if(!inherits(sf_data, "sf")) {
    stop("El objeto proporcionado no es de clase sf")
  }
  
  tryCatch({
    message("Usando bbox de: ", ciudad)
    bbox <- osmdata::getbb(ciudad)
    
    # Asegurarse que el bbox es válido
    if(is.null(bbox) || any(is.na(bbox))) {
      stop("No se pudo obtener un bbox válido para la ciudad especificada")
    }
    
    #### Parques ####
    message("Obteniendo datos de parques...")
    parques <- opq(bbox = bbox) %>%
      add_osm_feature(key = "leisure", value = "park")
    
    parques_sf <- osmdata_sf(parques)
    
    # Verificar que se obtuvieron polígonos de parques
    if(is.null(parques_sf$osm_polygons) || nrow(parques_sf$osm_polygons) == 0) {
      warning("No se encontraron parques en la zona especificada")
      sf_data$distancia_parque <- NA
    } else {
      parques_poligonos <- parques_sf$osm_polygons %>% st_as_sf()
      centroides_parques <- st_centroid(parques_poligonos) %>%
        mutate(x = st_coordinates(.)[, "X"], y = st_coordinates(.)[, "Y"]) %>%
        st_as_sf(coords = c("x", "y"), crs = 4326)
      
      # Distancia a parque más cercano
      dist_matrix <- st_distance(sf_data, centroides_parques)
      sf_data$distancia_parque <- apply(dist_matrix, 1, min)
    }
    
    #### Hospitales ####
    message("Obteniendo datos de hospitales...")
    hospitales <- opq(bbox = bbox) %>%
      add_osm_feature(key = "amenity", value = "hospital")
    
    hospitales_sf <- osmdata_sf(hospitales)
    
    # Verificar que se obtuvieron polígonos de hospitales
    if(is.null(hospitales_sf$osm_polygons) || nrow(hospitales_sf$osm_polygons) == 0) {
      warning("No se encontraron hospitales en la zona especificada")
      sf_data$distancia_hospital <- NA
    } else {
      hospitales_poligonos <- hospitales_sf$osm_polygons %>% st_as_sf()
      centroides_hosp <- st_centroid(hospitales_poligonos) %>%
        mutate(x = st_coordinates(.)[, "X"], y = st_coordinates(.)[, "Y"]) %>%
        st_as_sf(coords = c("x", "y"), crs = 4326)
      
      dist_matrix_hosp <- st_distance(sf_data, centroides_hosp)
      sf_data$distancia_hospital <- apply(dist_matrix_hosp, 1, min)
    }
    
    #### Restaurantes ####
    message("Obteniendo datos de restaurantes...")
    restaurantes <- opq(bbox = bbox) %>%
      add_osm_feature(key = "amenity", value = "restaurant")
    
    restaurantes_sf <- osmdata_sf(restaurantes)
    
    # Verificar que se obtuvieron datos de restaurantes
    if((is.null(restaurantes_sf$osm_points) || nrow(restaurantes_sf$osm_points) == 0) && 
       (is.null(restaurantes_sf$osm_polygons) || nrow(restaurantes_sf$osm_polygons) == 0)) {
      warning("No se encontraron restaurantes en la zona especificada")
      sf_data$num_restaurantes_700m <- 0
    } else {
      # Unir puntos y centroides de polígonos
      puntos <- if(!is.null(restaurantes_sf$osm_points)) {
        restaurantes_sf$osm_points %>% dplyr::select(osm_id, geometry)
      } else {
        st_sf(osm_id = character(0), geometry = st_sfc(), crs = 4326)
      }
      
      poligonos <- if(!is.null(restaurantes_sf$osm_polygons)) {
        restaurantes_sf$osm_polygons %>%
          dplyr::select(osm_id, geometry) %>%
          st_centroid()
      } else {
        st_sf(osm_id = character(0), geometry = st_sfc(), crs = 4326)
      }
      
      restaurantes_all <- bind_rows(puntos, poligonos) %>%
        st_transform(4326)
      
      # Restaurantes en radio
      if(nrow(restaurantes_all) > 0) {
        restaurantes_cercanos <- st_is_within_distance(sf_data, restaurantes_all, dist = Walkingdistance)
        sf_data$num_restaurantes_700m <- lengths(restaurantes_cercanos)
      } else {
        sf_data$num_restaurantes_700m <- 0
      }
    }
    
    message("Proceso completado con éxito")
    return(sf_data)
    
  }, error = function(e) {
    # Manejo específico para el error de curl_parse_url
    if(grepl("curl_parse_url", e$message)) {
      message("Error detectado con curl_parse_url. Intentando reinstalar curl...")
      try(detach("package:curl", unload = TRUE), silent = TRUE)
      try(detach("package:osmdata", unload = TRUE), silent = TRUE)
      install.packages("curl", dependencies = TRUE)
      library(curl)
      message("Por favor, intente ejecutar la función nuevamente.")
    } else {
      message("Error en la función agregar_vars_espaciales: ", e$message)
    }
    # Devolver los datos originales si hay error
    return(sf_data)
  })
}
```

```{r}
###########################################    3.1.1. Aplicar función

# Convertir datos de puntos a objeto espacial con CRS compatible
train_sf <- train %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326)  # Sistema WGS84

test_sf <- test %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326)  # Sistema WGS84


train_sf <- agregar_vars_espaciales(train_sf)
test_sf <- agregar_vars_espaciales(test_sf)
```

```{r}

###########################################    3.2. Estaciones TM y CAIs

# Función para añadir flags de proximidad
add_proximity_flags <- function(properties_sf,
                                tm_geojson = "https://raw.githubusercontent.com/Jusebastru/Probelm_Set_3/refs/heads/main/Estaciones_Troncales_TM.geojson",
                                cai_geojson = "https://datosabiertos.bogota.gov.co/dataset/bcc51101-762b-4e13-9455-f77502c75a0f/resource/202c5810-6880-43f8-b801-df70aaf6d237/download/comandoatencioninmediata.geojson",
                                dist_threshold = 1000) {
  
  # 1. Leer capas
  tm_sf  <- st_read(tm_geojson, quiet = TRUE)
  cai_sf <- st_read(cai_geojson, quiet = TRUE)
  
  # 2. Transformar CRS para que coincida con properties_sf
  tm_sf  <- st_transform(tm_sf,  st_crs(properties_sf))
  cai_sf <- st_transform(cai_sf, st_crs(properties_sf))
  
  # 3. Calcular matriz de distancias
  d_tm  <- st_distance(properties_sf, tm_sf)   # [n_prop x n_tm]
  d_cai <- st_distance(properties_sf, cai_sf)  # [n_prop x n_cai]
  
  # 4. Distancia mínima por vivienda
  min_dist_tm  <- apply(d_tm,  1, min)
  min_dist_cai <- apply(d_cai, 1, min)
  
  # 5. Añadir columnas lógicas (TRUE si está a ≤ dist_threshold metros)
  properties_sf %>%
    mutate(
      dist_to_tm  = as.numeric(min_dist_tm),
      near_tm     = dist_to_tm  <= dist_threshold,
      dist_to_cai = as.numeric(min_dist_cai),
      near_cai    = dist_to_cai <= dist_threshold
    )
}
```

```{r}
###########################################    3.2.1 Aplicar función 

train_sf <- add_proximity_flags(train_sf)
test_sf <- add_proximity_flags(test_sf)

```

# -----------------------------------------------------------------------------------------------------------------------------
# 4. Uniendo Bases de Datos
# -----------------------------------------------------------------------------------------------------------------------------

####### 4.1. Uniendo Bases de datos
```{r}

##################################### 4.1. Primero identifico cada base con columna de mismo nombre

test_sf$origen <- "test"
train_sf$origen <- "train"

##################################### 4.2. Uniendo Bases de datos Train_sf y test_Sf 

test_sfyS <- inner_join(test_sf, sumission, by = "property_id")
Base <- bind_rows(test_sfyS, train_sf)

visdat::vis_miss(Base, warn_large_data = FALSE)

#################################### 4.3. Creando Columna PrecioUnificado
###                                       PrecioUnificado es la unión de precios en una sola columna

Base <- Base %>%
  mutate(PrecioUnificado = coalesce(price,price.y))

##################################  4.4.  Eliminando columnas que no interesan #
Base <- Base %>%
  dplyr::select(-price.x)

```

###### 4.2. Deflantando Precios a año base 2019
```{r}

################# Creo la columna 'IVPHistorico' y según el año le asigno el valor de índice Valorización Predial (IVP) #
################# Datos de IVP Histórico tomados de: https://epicainmobiliaria.com/cuanto-valoriza-inmueble-colombia/ #

Base <- Base %>%
  mutate(IVPHistorico = case_when(
    year == 2019 ~ 1,
    year == 2020 ~ 1.0422,
    year == 2021 ~ 1.0336,
    TRUE ~ NA_real_  # Para años distintos asigna NA
  ))

################## Creo columna InflacionAcumulada para deflactar precios a año base 2019 #
Base <- Base %>%
  mutate(InflacionAcumulada = case_when(
    year == 2019 ~ 1,
    year == 2020 ~ 1 * 1.0422,
    year == 2021 ~ 1.0422 * 1.0336,
    TRUE ~ NA_real_
  ))

################## Creo columna de Precios Deflactados llamada PrecioBase #
Base <- Base %>%
  mutate(
    PrecioDeflactado = PrecioUnificado/InflacionAcumulada
  )
```

# -----------------------------------------------------------------------------------------------------------------------------
# 5. Imputando Missing_Values
# -----------------------------------------------------------------------------------------------------------------------------

###### 5.1. Visualizando los missing_values
```{r}
skimr::skim(Base)
```

######  5.2. Función de imputación por KNN (K-Vecino Mäs Cercano)
```{r}

library(VIM)

# Función para imputar missing values por k-Nearest Neighbors
impute_knn <- function(df,
                       vars_objetivo    = c("bathrooms", "bedrooms", "rooms",
                                          "surface_covered", "surface_total"),
                       vars_base = c("bathrooms", "bedrooms", "rooms",
                                          "surface_covered", "surface_total",
                                          "tiene_terraza", "distancia_parque",
                                          "distancia_hospital", "num_restaurantes_700m",
                                          "dist_to_tm", "dist_to_cai", "PrecioDeflactado"),
                       k = 5) {
  
  # 1. Subconjunto con las variables de interés
  df_imp <- df %>%
    select(all_of(unique(c(vars_objetivo, vars_base))))
  
  # 2. Ejecutar kNN de VIM
  #    - variable: las columnas a imputar
  #    - dist_var: variables usadas para calcular distancias
  #    - k: número de vecinos
  #    - imp_var = FALSE: no generar columnas indicadores de imputación
  imputed_block <- kNN(df_imp,
                       variable = vars_objetivo,
                       dist_var = vars_base,
                       k = k,
                       imp_var = FALSE)
  
  # 3. Reemplazar sólo las columnas imputadas en el df original
  df2 <- df
  df2[vars_objetivo] <- imputed_block[vars_base]
  
  return(df2)
}

```

######  5.3. Imputando Missing_values
```{r}

###################################   5.3.1. Imputando miss_values en train_Sf
Base_imputada <- impute_knn(Base, k = 5)
```

```{r}
skimr::skim(Base_imputada)
```

```{r}

```

```{r}

```

```{}

```


```{r}

```

```{r}

```

```{}

```

```{r}

```

```{r}

```

```{}

```


```{r}

```

```{r}

```

```{}

```

```{r}

```

```{r}

```

```{}

```


```{r}

```

```{r}

```

```{}

```

```{r}

```

```{r}

```

```{r}

```


```{r}

```

```{r}

```

```{r}

```

```{r}

```

```{r}

```

```{r}

```


```{r}

```

```{r}

```

```{r}

```

```{r}

```

```{r}

```

```{r}

```


```{r}

```

```{r}

```

```{r}

```

```{r}

```

```{r}

```

```{r}

```


```{r}

```

```{r}

```

```{r}

```

```{r}

```

```{r}

```

```{}

```


```{r}

```

```{r}

```

```{r}

```

```{r}

```

```{r}

```

```{r}

```


```{r}

```

```{r}

```

```{}

```

```{r}

```

```{r}

```

```{r}

```


```{r}

```

```{r}

```

```{r}

```

```{r}

```

```{r}

```

```{r}

```


```{r}

```

```{r}

```

```{r}

```

```{r}

```

```{r}

```

```{r}

```


```{r}

```

```{r}

```

```{r}

```

```{r}

```

```{r}

```

```{r}

```


```{r}

```

```{r}

```

```{r}

```

```{r}

```

```{r}

```

```{r}

```


```{r}

```

```{r}

```

```{r}

```

```{r}

```

```{r}

```

```{r}

```


```{r}

```

```{r}

```

```{r}

```

```{r}

```

```{r}

```

```{r}

```


```{r}

```

```{r}

```

```{}

```

```{r}

```

```{r}

```

```{}

```


```{r}

```

```{r}

```

```{}

```
