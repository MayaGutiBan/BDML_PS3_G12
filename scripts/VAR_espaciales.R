# Cargar pacman (contiene la función p_load)
library(pacman) 
library(dplyr)

# Cargar las librerías listadas e instalarlas en caso de ser necesario
p_load(tidyverse, # Manipular dataframes
       gridExtra, # Para graficar en grid
       rio, # Import data easily
       plotly, # Gráficos interactivos
       leaflet, # Mapas interactivos
       tmaptools, # geocode_OSM()
       sf, # Leer/escribir/manipular datos espaciales
       osmdata, # Get OSM's data 
       tidymodels) #para modelos de ML

##### CARGAR BASES 

train <- () 
test <- ()

#### Add spatial variables function 

agregar_vars_espaciales <- function(sf_data, ciudad = "Bogotá Colombia", Walkingdistance = 700) {
  library(dplyr)
  library(sf)
  library(osmdata)
  
  message("Usando bbox de: ", ciudad)
  bbox <- getbb(ciudad)
  
  #### Parques ####
  parques <- opq(bbox = bbox) %>%
    add_osm_feature(key = "leisure", value = "park")
  parques_sf <- osmdata_sf(parques)$osm_polygons %>% st_as_sf()
  centroides_parques <- st_centroid(parques_sf) %>%
    mutate(x = st_coordinates(.)[, "X"], y = st_coordinates(.)[, "Y"]) %>%
    st_as_sf(coords = c("x", "y"), crs = 4326)

  # Distancia a parque más cercano
  dist_matrix <- st_distance(sf_data, centroides_parques)
  sf_data$distancia_parque <- apply(dist_matrix, 1, min)

  #### Hospitales ####
  hospitales <- opq(bbox = bbox) %>%
    add_osm_feature(key = "amenity", value = "hospital")
  hospitales_sf <- osmdata_sf(hospitales)$osm_polygons %>% st_as_sf()
  centroides_hosp <- st_centroid(hospitales_sf) %>%
    mutate(x = st_coordinates(.)[, "X"], y = st_coordinates(.)[, "Y"]) %>%
    st_as_sf(coords = c("x", "y"), crs = 4326)

  dist_matrix_hosp <- st_distance(sf_data, centroides_hosp)
  sf_data$distancia_hospital <- apply(dist_matrix_hosp, 1, min)

  #### Restaurantes ####
  restaurantes <- opq(bbox = bbox) %>%
    add_osm_feature(key = "amenity", value = "restaurant")
  restaurantes_sf <- osmdata_sf(restaurantes)
  puntos <- restaurantes_sf$osm_points %>% dplyr::select(osm_id, geometry)
  poligonos <- restaurantes_sf$osm_polygons %>%
    dplyr::select(osm_id, geometry) %>%
    st_centroid()
  restaurantes_all <- bind_rows(puntos, poligonos) %>%
    st_transform(4326)

  # Restaurantes en radio
  restaurantes_cercanos <- st_is_within_distance(sf_data, restaurantes_all, dist = Walkingdistance)
  sf_data$num_restaurantes_700m <- lengths(restaurantes_cercanos)

  return(sf_data)
}

## Aplicar a train y test 

train <- agregar_vars_espaciales(train)
test <- agregar_vars_espaciales(test)

