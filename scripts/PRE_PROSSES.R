# Cargar pacman (contiene la función p_load)
library(pacman) 
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

### Pasar de texto a datos 
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
        nums[!is.na(nums) & nums >= 30 & nums <= 1000]
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


train <- limpiar_y_enriquecer(train)
test <- limpiar_y_enriquecer(test)



p_load( visdat)
vis_dat(train)
