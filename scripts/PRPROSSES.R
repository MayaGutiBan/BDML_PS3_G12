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


### Pre prosessing ####

### Missing values###
train %>%
  summarise(across(everything(), ~ sum(is.na(.x)))) 

sum(is.na(train$surface_total))

remove.packages("cli")
install.packages("cli", type = "source")

## Vizualization of missing values 
p_load( visdat)
vis_dat(train)

######## CREACIÓN VAR ESPACIALES ######### 


#### parques dist a parque ####
# Extraemos la info de todos los parques 
parques <- opq(bbox = getbb("Bogotá Colombia")) %>%
  add_osm_feature(key = "leisure" , value = "park") 
# Cambiamos el formato para que sea un objeto sf (simple features)
parques_sf <- osmdata_sf(parques)

# De las features del parque nos interesa su geomoetría y donde estan ubicados 
parques_geometria <- parques_sf$osm_polygons %>% 
  dplyr::select(osm_id, name) 

# Guardemos los poligonos de los parques 
parques_geometria <- st_as_sf(parques_sf$osm_polygons)

# Calculamos el centroide de cada parque para aproximar su ubciacion como un solo punto 
centroides <- st_centroid(parques_geometria, byid = T)

centroides <- centroides %>%
  mutate(x=st_coordinates(centroides)[, "X"]) %>%
  mutate(y=st_coordinates(centroides)[, "Y"]) 

#### MAPA PARQUES BOGOTA 

latitud_central <- 4.7110
longitud_central <- -74.0721

leaflet() %>%
  addTiles() %>%
  setView(lng = longitud_central, lat = latitud_central, zoom = 12) %>%
  addPolygons(data = parques_geometria, col = "red",weight = 10,
              opacity = 0.8, popup = parques_geometria$name) %>%
  addCircles(lng = centroides$x, 
             lat = centroides$y, 
             col = "darkblue", opacity = 0.5, radius = 1)


centroides_sf <- st_as_sf(centroides, coords = c("x", "y"), crs=4326)
sf_db<- st_as_sf(train, coords = c("lon", "lat"),  crs = 4326)

# MATRIZ DISTANCIA DE CADA SENTROIDE A CADA PROPIEDAD 
dist_matrix <- st_distance(x = sf_db, y = centroides_sf)
dim(dist_matrix)

# distancia a parque mas cercano 
dist_min <- apply(dist_matrix, 1, min)  

# La agregamos como variable a nuestra base de datos original 
train <- train %>% mutate(distancia_parque = dist_min)

#### Hospitales (distancia) ####


osmdata::available_tags("amenity") 

#### Extraer hospitales de OSM 

# Descargamos hospitales dentro de Bogotá
hospitales <- opq(bbox = getbb("Bogotá Colombia")) %>%
  add_osm_feature(key = "amenity", value = "hospital")

# Convertimos a objeto sf
hospitales_sf <- osmdata_sf(hospitales)

# Nos quedamos con los polígonos 
hospitales_geometria <- hospitales_sf$osm_polygons %>%
  dplyr::select(osm_id, name)

# Convertimos a sf explícitamente
hospitales_geometria <- st_as_sf(hospitales_geometria)

#### Calcular centroides 

# Calculamos centroides de cada hospital
centroides_hospitales <- st_centroid(hospitales_geometria)

# Extraemos coordenadas
centroides_hospitales <- centroides_hospitales %>%
  mutate(x = st_coordinates(geometry)[, "X"],
         y = st_coordinates(geometry)[, "Y"])

# Convertimos a sf con coords explícitos
centroides_hospitales_sf <- st_as_sf(centroides_hospitales, coords = c("x", "y"), crs = 4326)

#### Calcular distancias 

# Asegurar que tus datos (train) están en el mismo CRS
sf_db <- st_transform(train, crs = 4326)

# Matriz de distancias de propiedades a hospitales
dist_matrix_hosp <- st_distance(x = sf_db, y = centroides_hospitales_sf)

# Distancia al hospital más cercano
dist_min_hosp <- apply(dist_matrix_hosp, 1, min)

# Agregar al dataframe original
train <- train %>% mutate(distancia_hospital = dist_min_hosp)



#### restaurantes (puntos y polígonos) ####

restaurantes_query <- opq(bbox = getbb("Bogotá Colombia")) %>%
  add_osm_feature(key = "amenity", value = "restaurant")

restaurantes_sf <- osmdata_sf(restaurantes_query)

# Extraemos puntos y polígonos
restaurantes_puntos <- restaurantes_sf$osm_points %>%
  dplyr::select(osm_id, name, geometry)

restaurantes_poligonos <- restaurantes_sf$osm_polygons %>%
  dplyr::select(osm_id, name, geometry) %>%
  st_centroid()  # Convertimos a puntos vía centroides

# Unimos todo en un solo objeto sf
restaurantes_all <- bind_rows(restaurantes_puntos, restaurantes_poligonos)

# Aseguramos CRS
restaurantes_all <- st_transform(restaurantes_all,  4326) 
train_proj <- st_transform(train, 4326)

#### 2. Calcular número de restaurantes en 700 metros 

# Usamos st_is_within_distance para buscar vecinos en un radio
restaurantes_cercanos <- st_is_within_distance(train_proj, restaurantes_all, dist = 700)

# Contamos cuántos restaurantes hay para cada punto
num_restaurantes_700m <- lengths(restaurantes_cercanos)

# Agregamos a la base
train <- train %>%
  mutate(num_restaurantes_700m = num_restaurantes_700m)
