

#----------------------------------
# 1. Configuración inicial
#----------------------------------

if (!require(pacman)) install.packages("pacman")
library(pacman)

p_load(
  # Manipulación de datos
  tidyverse, readxl, writexl, janitor, skimr, VIM, stringi,
  # Geoprocesamiento
  sf, osmdata, geojsonio, leaflet,
  # Modelado
  caret, glmnet, spatialsample, tidymodels,
  # Visualización
  GGally, corrplot, plotly, leaflet,
  # Web scraping
  httr, rvest, jsonlite
)

#----------------------------------
# 2. Cargar datos 
#----------------------------------

setwd("/Users/carlosmanjarres/Desktop/Big_data/uniandes-bdml-2025-10-ps-3")

train <- read.csv("train.csv")

#----------------------------------
# 3. Funciones personalizadas
#----------------------------------
pattern_context <- ".{0,20}\\b(\\d{1,4})\\s*(m2|mts|metros|m)\\b.{0,20}|.{0,20}\\b(m2|mts|metros|m)\\s*(\\d{1,4})\\b.{0,20}"


# Limpieza y enriquecimiento de texto en descripciones de propiedades
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
      area_matches = str_extract_all(description, pattern_context),
      area_nums = map(area_matches, ~ {
        nums <- str_extract_all(.x, "\\d{1,4}") %>% unlist() %>% as.integer()
        nums[!is.na(nums) & nums >= 35 & nums <= 1000]
      }),
      area_construida = map_int(area_nums, ~ if(length(.x) > 0) max(.x) else NA_integer_),
      surface_covered = if_else(is.na(surface_covered), area_construida, surface_covered),
      surface_total = if_else(is.na(surface_total), area_construida, surface_total)
    )%>%
    
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
    select(-alcoba_raw, -alcobas, -baño_raw, -baños, -area_construida, -terraza_raw, -m2_terraza)
  
  return(df)
}

test <- limpiar_y_enriquecer(test)
train <-limpiar_y_enriquecer(train)

# Variables espaciales utilizando OSM ( hay problemas aca)

agregar_info_espacial <- function(train, ciudad = "Bogotá Colombia") {
  
  bbox_ciudad <- getbb(ciudad)
  if (is.null(bbox_ciudad)) stop("No se pudo obtener el bbox de la ciudad.")
  
  # --- PARQUES ---
  parques_query <- opq(bbox = bbox_ciudad) %>%
    add_osm_feature(key = "leisure", value = "park")
  parques_sf <- osmdata_sf(parques_query)
  if (is.null(parques_sf$osm_polygons)) stop("No se encontraron polígonos de parques.")
  
  parques_geom <- st_as_sf(parques_sf$osm_polygons)
  centroides_parques <- st_centroid(parques_geom) %>%
    mutate(x = st_coordinates(.)[, "X"],
           y = st_coordinates(.)[, "Y"]) %>%
    st_as_sf(coords = c("x", "y"), crs = 4326)
  
  sf_db <- st_as_sf(train, coords = c("lon", "lat"), crs = 4326)
  dist_matrix <- st_distance(sf_db, centroides_parques)
  train$distancia_parque <- apply(dist_matrix, 1, min)
  
  # --- HOSPITALES ---
  hospitales_query <- opq(bbox = bbox_ciudad) %>%
    add_osm_feature(key = "amenity", value = "hospital")
  hospitales_sf <- osmdata_sf(hospitales_query)
  if (is.null(hospitales_sf$osm_polygons)) stop("No se encontraron hospitales.")
  
  hospitales_geom <- st_as_sf(hospitales_sf$osm_polygons)
  centroides_hosp <- st_centroid(hospitales_geom) %>%
    mutate(x = st_coordinates(.)[, "X"],
           y = st_coordinates(.)[, "Y"]) %>%
    st_as_sf(coords = c("x", "y"), crs = 4326)
  
  dist_matrix_hosp <- st_distance(sf_db, centroides_hosp)
  train$distancia_hospital <- apply(dist_matrix_hosp, 1, min)
  
  # --- RESTAURANTES ---
  restaurantes_query <- opq(bbox = bbox_ciudad) %>%
    add_osm_feature(key = "amenity", value = "restaurant")
  restaurantes_sf <- osmdata_sf(restaurantes_query)
  
  puntos <- restaurantes_sf$osm_points
  poligonos <- restaurantes_sf$osm_polygons
  
  if (is.null(puntos) & is.null(poligonos)) stop("No se encontraron restaurantes.")
  
  restaurantes_puntos <- puntos %>% dplyr::select(osm_id, name, geometry)
  restaurantes_poligonos <- poligonos %>%
    dplyr::select(osm_id, name, geometry) %>%
    st_centroid()
  
  restaurantes_all <- bind_rows(restaurantes_puntos, restaurantes_poligonos)
  restaurantes_all <- st_transform(restaurantes_all, 4326)
  sf_db <- st_transform(sf_db, 4326)
  
  restaurantes_cercanos <- st_is_within_distance(sf_db, restaurantes_all, dist = 700)
  train$num_restaurantes_700m <- lengths(restaurantes_cercanos)
  
  return(train)
}

train <- agregar_info_espacial(train)
test <- agregar_info_espacial(test)

train <- train %>% select(-area_matches, -area_nums) 

## convertir el data set a espacial 

train <- st_as_sf(train, coords = c("lon", "lat"), crs = 4326)

# Otras variables espaciales 

add_proximity_flags <- function(properties_sf,
                                tm_geojson = "https://raw.githubusercontent.com/Jusebastru/Probelm_Set_3/refs/heads/main/Estaciones_Troncales_TM.geojson",
                                cai_geojson = "https://datosabiertos.bogota.gov.co/dataset/bcc51101-762b-4e13-9455-f77502c75a0f/resource/202c5810-6880-43f8-b801-df70aaf6d237/download/comandoatencioninmediata.geojson",
                                dist_threshold = 700) {
  
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

train <- add_proximity_flags(train) 

### Añadir localidad a las obs 

# 1. Descarga polígonos administrativos de Bogotá 
Localidades <- opq("Bogotá Colombia") %>%
  add_osm_feature(key = "boundary", value = "administrative") %>%  
  add_osm_feature(key = "admin_level", value = "8") %>%
  osmdata_sf()


# 2. Extrae los polígonos
Localidades_sf <- Localidades$osm_multipolygons %>%
  dplyr::select(name)

# 3. Transforma al mismo CRS que tus datos
train <- st_transform(train, st_crs(Localidades_sf))

# 4. Unión espacial: ¿en qué barrio cae cada propiedad?
train <- st_join(train, Localidades_sf, left = TRUE)

train <- train %>% rename(localidad= name)


#----------------------------------
# 4. Ingeniería de características
#----------------------------------

# Deflactación de precios
inflation_rates <- tibble(
  year = c(2019, 2020, 2021),
  factor = c(1, 1.0422, 1.0336*1.0422)
)

train<- train %>%
  left_join(inflation_rates, by = "year") %>%
  mutate(price_real = price / factor)

train %>%
  summarise(across(everything(), ~ sum(is.na(.x)))) 


#----------------------------------
# 5. Imputación de valores faltantes
#----------------------------------
impute_features <- function(data) {
  data %>%
    VIM::kNN(
      variable = c("bathrooms", "rooms", "surface_covered","surface_total"),
      dist_var = c("property_type", "bedrooms", "distancia_parque", "price_real","localidad","distancia_hospital"),
      k = 5
    )
}

train<-impute_features(train)


train <- train %>% select(-bathrooms_imp,-rooms_imp,-surface_covered_imp,-surface_total)

write.csv(train, "train_clean.csv", row.names = FALSE)

