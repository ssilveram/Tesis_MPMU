# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# :::                                                                        :::
# :::               SCRIPT DE EQUIPAMIENTOS - ESCENARIO 4                    :::
# :::                                                                        :::
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#
# Nombre:       4_4_Amenities_Scenario_4.R
#
# Descripcion:  Este Script hará la implementación de equipamientos distribuidos en las
#               áreas correspondientes a las UPL, con el fin de igualar la media de equipamientos
#               para cada uno de los tipos de equipamientos.
#
# ####
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# :::                      Contenido de este Script                          :::
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ####
#
# 1. Inicialización
# 2. Carga de datos de origen
# 3. Creamos capa de etiquetas para poner encima de los mapas
# 4. Determinamos las áreas donde implantaremos los equipamientos
# 5. Preparación de df y sf para la implantación de equipamientos
# 6. Generamos los nuevos equipamientos en cada UPL sobre cada MNZ
# 7. Unificamos los nuevos con el Shape de puntos original (Scn1)
# 8. Recarga de datos producto de este script
# 9. Tiempos de procesamiento
#
Start_Time_S4_4_P0 <- Sys.time()

# ####
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ::: 1. Inicialización                                                      :::
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ####
#
Start_Time_S4_4_P1 <- Sys.time()

source("0_Initialization.R")

# Tiempo de procesamiento:
End_Time_S4_4_P1 <- Sys.time()
print(paste("1. Tiempo de inicialización: ", as.duration(End_Time_S4_4_P1 - Start_Time_S4_4_P1)))

# ####
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ::: 2. Carga de datos de origen                                            :::
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ####
#
# Cargamos los shapes vacíos y los de población para comenzar a hacer consultas 
# y los análisis.
#
Start_Time_S4_4_P2 <- Sys.time()

# --- 2.1. Carga de Shapes Vacíos (1_Empty_Shapes.R) ---

Shape_Municipios <- st_read("./Data/2_Processing/1_Empty_Shapes/Shape_Vacio_Municipios.gpkg")
Shape_UPLs <- st_read("./Data/2_Processing/1_Empty_Shapes/Shape_Vacio_UPLs.gpkg")
Shape_ZAT <- st_read("./Data/2_Processing/1_Empty_Shapes/Shape_Vacio_ZAT.gpkg")
Shape_Hexagonos <- st_read("./Data/2_Processing/1_Empty_Shapes/Shape_Vacio_Hexagonos.gpkg")
Shape_Manzanas <- st_read("./Data/2_Processing/1_Empty_Shapes/Shape_Vacio_Manzanas.gpkg")
Coordinates_CRS <- st_crs(Shape_Municipios)

# --- 2.2. Carga de los Shapes de la población (2_Population.R) ---

Shape_Censo_UPLs <- st_read("./Data/2_Processing/2_Population/Shape_Poblacion_UPLs.gpkg")

# --- 2.3. Carga de los Amenities del Escenario 1 para tomar como referencia ----

# Carga de puntos:
Amenities_Puntos_Scn1_sf <- st_read("./Data/2_Processing/4_Amenities/Scn1/Amenities_Puntos_Scn1_sf.gpkg")
Amenities_Puntos_Scn1_sf

# Carga de inventario por áreas:
load("./Data/2_Processing/4_Amenities/Scn1/Amenities_Inventory_by_Area.RData")
Amenities_Inventory_Scn1_HEX_sf
Amenities_Inventory_Scn1_SECC_sf
Amenities_Inventory_Scn1_SECT_sf
Amenities_Inventory_Scn1_ZAT_sf
Amenities_Inventory_Scn1_UTAM_sf
Amenities_Inventory_Scn1_UPL_sf
Amenities_Inventory_Scn1_LOC_sf
Amenities_Inventory_Scn1_BOG_sf

# Carga de categorías por área:
load("./Data/2_Processing/4_Amenities/Scn1/Amenities_Category_by_Area.RData")
Amenities_Category_Scn1_HEX_sf
Amenities_Category_Scn1_SECC_sf
Amenities_Category_Scn1_SECT_sf
Amenities_Category_Scn1_ZAT_sf
Amenities_Category_Scn1_UTAM_sf
Amenities_Category_Scn1_UPL_sf
Amenities_Category_Scn1_LOC_sf
Amenities_Category_Scn1_BOG_sf

# Carga de datos del metro
# Cargamos el Shape de UPLs
Linea_Metro <- gtfstools::convert_shapes_to_sf(read_gtfs("./Data/1_Sources/5_GTFS/Scn2/GTFS_PLMB_Con_Geom.zip"))
Estaciones_Metro <- gtfstools::convert_stops_to_sf(read_gtfs("./Data/1_Sources/5_GTFS/Scn2/GTFS_PLMB_Con_Geom.zip"))

# Tiempo de procesamiento:
End_Time_S4_4_P2 <- Sys.time()
print(paste("2. Tiempo de carga de datos de origen: ", as.duration(End_Time_S4_4_P2 - Start_Time_S4_4_P2)))

# ####
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ::: 3. Creamos capa de etiquetas para poner encima de los mapas            :::
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ####
#
Start_Time_S4_4_P3 <- Sys.time()

# Seleccionamos las UPL no rurales
Lista_No_Rural <- Shape_UPLs |> filter(ZONA_UPL != "Rural") |> as_tibble() |> select(COD_UPL)
Lista_No_Rural

# Creamos una capa de etiquetas para poner encima de los mapas.
Capa_Etiquetas_UPL <- Shape_UPLs |>
  filter(COD_UPL %in% Lista_No_Rural$COD_UPL) |>
  mutate(
    # Extrae las coordenadas del centroide de cada área
    centroides = st_centroid(geom),
    X = st_coordinates(centroides)[,1],
    Y = st_coordinates(centroides)[,2],
    # Define la columna que se usará como etiqueta
    Etiqueta_Texto = COD_UPL
  ) |>
  select(COD_UPL, NOM_UPL, X, Y, Etiqueta_Texto)
Capa_Etiquetas_UPL

# Tiempo de procesamiento:
End_Time_S4_4_P3 <- Sys.time()
print(paste("3. Tiempo de creación de la capa de etiquetas: ", as.duration(End_Time_S4_4_P3 - Start_Time_S4_4_P3)))

# ####
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ::: 4. Determinamos las áreas donde implantaremos los equipamientos        :::
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ####
# Para el Escenario 4, se implantarán equipamientos distribuidos dentro de las
# UPL, con el objetivo de aumentar el nivel de accesibilidad a estos equipamientos
# por parte de la población.
#
Start_Time_S4_4_P4 <- Sys.time()

# Se implantarán los equipamientos a lo largo de las UPLs:
Shape_UPLs_Implantacion_sf <- Shape_Censo_UPLs
Shape_UPLs_Implantacion_sf

# Verificamos las Áreas de las UPLs:
tm_basemap("OpenStreetMap") +
  tm_shape(Shape_UPLs_Implantacion_sf) +
  tm_polygons(fill = "lightblue", fill_alpha = 0.3,  col_alpha = 1, col = "darkblue") +
  tm_shape(Linea_Metro) +
  tm_lines(col = "darkgreen") +
  tm_shape(Estaciones_Metro) +
  tm_dots(fill = "blue", size = 1)

# NOTA:
# Si alcanza el tiempo, hacemos mapa bonito.

# Tiempo de procesamiento:
End_Time_S4_4_P4 <- Sys.time()
print(paste("4. Tiempo de determinación las áreas donde implantaremos los equipamientos: ", as.duration(End_Time_S4_4_P4 - Start_Time_S4_4_P4)))

# ####
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ::: 5. Preparación de df y sf para la implantación de equipamientos        :::
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ####
#
Start_Time_S4_4_P5 <- Sys.time()

# --- 5.1. Carga de df y sf necesarias ---

# Cargamos el inventario de equipamientos:
load("./Data/2_Processing/4_Amenities/Scn1/Amenities_Inventory_by_Area.RData")

# Hacemos un pivot loger porque es más facil contar así:
Amenities_Inventory_Scn1_BOG_Longer_df <- Amenities_Inventory_Scn1_BOG_sf |>
  st_drop_geometry() |>
  select(COD_CPOB, Aeropuertos:`Áreas recreativas`) |>
  pivot_longer(
    # Seleccionamos todas las columnas excepto COD_HEX
    cols = -COD_CPOB, 
    # El nombre de la nueva columna para los tipos de equipamiento
    names_to = "EQUIPAMIENTO", 
    # El nombre de la nueva columna para el conteo
    values_to = "NUMERO_DE_EQUIPAMIENTOS"
  )
Amenities_Inventory_Scn1_BOG_Longer_df

# Verificamos el CRS (Sistema de Referencia de Coordenadas)
stopifnot(
  st_crs(Shape_UPLs) == st_crs(Amenities_Puntos_Scn1_sf),
  st_crs(Shape_UPLs) == st_crs(Shape_Manzanas),
  st_crs(Shape_UPLs) == st_crs(Shape_Municipios)
)

# --- 5.2. Preparación y Cálculos Globales ---

# Revisamos los equipamientos:
Amenities_Inventory_Scn1_BOG_Longer_df$EQUIPAMIENTO

## 5.2.1: Filtrar Equipamientos no Implantables
Equipamientos_a_Excluir <- c(
  "Aeropuertos", "Bosques", "Ciclovías", "Estaciones de tren", "Estadios",
  "Prisiones", "Reservas naturales", "Uso industrial"
)
Equipamientos_a_Excluir

Amenities_Inventory_Filtrado_df <- Amenities_Inventory_Scn1_BOG_Longer_df %>%
  filter(!EQUIPAMIENTO %in% Equipamientos_a_Excluir)
Amenities_Inventory_Filtrado_df

## 5.2.2: Calcular la Población Total de la Ciudad
# Las unidades son Personas
Poblacion_Ciudad <- sum(Shape_Censo_UPLs$PERSONAS_UPL) |> as.numeric()
Poblacion_Ciudad

## 5.2.3: Calcular Densidad de Referencia para cada Equipamiento
Densidades_Referencia_df <- Amenities_Inventory_Filtrado_df %>%
  mutate(
    Densidad_Equip_Por_Habitante = NUMERO_DE_EQUIPAMIENTOS / Poblacion_Ciudad
  ) %>%
  select(EQUIPAMIENTO, Densidad_Equip_Por_Habitante)
Densidades_Referencia_df

# Mostramos un ejemplo de las densidades calculadas
print("Ejemplo de densidades de referencia (equipamientos por habitante):")
head(Densidades_Referencia_df)

# Tiempo de procesamiento:
End_Time_S4_4_P5 <- Sys.time()
print(paste("5. Tiempo de preparación de df y sf para la implantación de equipamientos: ", as.duration(End_Time_S4_4_P5 - Start_Time_S4_4_P5)))

# ####
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ::: 6. Generamos los nuevos equipamientos en cada área sobre cada MNZ      :::
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ####
#
Start_Time_S4_4_P6 <- Sys.time()

# Usaremos `map_dfr` de purrr para iterar sobre cada isocrona y unir
# los resultados en un único sf al final. Es más eficiente que un bucle for.

# Añadimos un ID único a cada isocrona para facilitar el seguimiento:
Shape_UPLs_Implantacion_sf$ID_UPL <- 1:nrow(Shape_UPLs_Implantacion_sf)

# Backup:
# Shape_UPLs_Implantacion_sf <- Shape_UPLs_Implantacion_sf_bk
# Shape_UPLs_Implantacion_sf_bk

Shape_UPLs_Implantacion_sf <- Shape_UPLs_Implantacion_sf |>
#  filter(ID_UPL < 3) |>
  st_make_valid()
Shape_UPLs_Implantacion_sf

# --- 6.1. Iteración para cada UPL y para cada tipo de equipamiento ---

# El corazón del análisis: iteramos por cada fila (UPL) del sf:
Lista_Equipamientos_Propuestos <- map(Shape_UPLs_Implantacion_sf$ID_UPL, function(id_upl) {
  
  # Seleccionamos la UPL actual
  UPL_Actual_sf <- Shape_UPLs_Implantacion_sf |> filter(ID_UPL == id_upl)
  Shape_UPLs_Implantacion_sf$PERSONAS_UPL[1]
  cat(paste("\n--- Procesando UPL ", id_upl,
            " de ", length(Shape_UPLs_Implantacion_sf$COD_UPL),
            " | COD_UPL: ", UPL_Actual_sf$COD_UPL[1],
            " | ", UPL_Actual_sf$NOM_UPL[1],
            " ---\n", sep = ""))
  
  # --- Cálculos específicos para ESTA UPL ---
  
  # Personas en la UPL actual
  Personas_UPL <- UPL_Actual_sf$PERSONAS_UPL[1] |> as.numeric()
  cat(paste(" Personas en la UPL", UPL_Actual_sf$COD_UPL[1], ": ", Personas_UPL, " personas.\n", sep = ""))
  
  # Identificar manzanas que se intersectan con la UPL
  Manzanas_En_UPL_sf <- Shape_Manzanas |>
    select(COD_UPL, COD_MNZ) |>
    filter(COD_UPL == UPL_Actual_sf$COD_UPL[1])

  # Si no hay manzanas en la UPL, no podemos hacer nada.
  if (nrow(Manzanas_En_UPL_sf) == 0) {
    cat("Advertencia: El ID", id_upl, "no intersecta con ninguna manzana. Se omite.\n")
    return(NULL) # Devolvemos NULL para que no se añada al resultado
  }
  
  # Calcular los centroides de esas manzanas (nuestras ubicaciones posibles)
  Centroides_Posibles_sf <- st_centroid(Manzanas_En_UPL_sf)
  
  # --- Bucle interno: Analizar el déficit para cada TIPO de equipamiento ---
  
  Propuestas_Para_Esta_UPL <- map_dfr(1:nrow(Densidades_Referencia_df), function(i) {
    
    # Datos del equipamiento que estamos analizando
    Tipo_Equipamiento <- Densidades_Referencia_df$EQUIPAMIENTO[i]
    Densidad_Ref <- Densidades_Referencia_df$Densidad_Equip_Por_Habitante[i]
    
    # Contar cuántos equipamientos de este tipo YA existen en la UPL
    Equip_Actuales_En_UPL <- Amenities_Puntos_Scn1_sf |>
      filter(EQUIPAMIENTO == Tipo_Equipamiento) |>
      st_filter(UPL_Actual_sf, .predicate = st_intersects)
    
    Numero_Actual <- nrow(Equip_Actuales_En_UPL)
    
    # Calcular el número OBJETIVO de equipamientos para esta isocrona
    Numero_Objetivo <- Densidad_Ref * Personas_UPL
    
    # Calcular el DÉFICIT (cuántos faltan)
    Deficit <- round(Numero_Objetivo) - Numero_Actual                           # Está rarito.
    
    # --- Implantación de Nuevos Equipamientos ---
    if (Deficit > 0) {
      cat(paste("  Déficit para ID: ", id_upl,
                " de ", length(Shape_UPLs_Implantacion_sf$COD_UPL),
                " | UPL", UPL_Actual_sf$COD_UPL[1],
                ": '", Tipo_Equipamiento, "': ",
                Deficit, " unidades.\n", sep=""))
      
      # Seleccionar aleatoriamente `deficit` centroides.
      # `replace = TRUE` permite que un mismo centroide sea elegido más de una vez
      # si el déficit fuera mayor que el número de centroides disponibles.
      Ubicaciones_Seleccionadas_sf <- Centroides_Posibles_sf |>
        sample_n(size = Deficit, replace = TRUE)
      
      # Crear el sf con los nuevos puntos propuestos
      Nuevos_Puntos_sf <- Ubicaciones_Seleccionadas_sf |>
        mutate(
          EQUIPAMIENTO = Tipo_Equipamiento,
          STATUS = "Propuesto",
          ID_UPL_ORIGEN = id_upl
        ) |>
        select(EQUIPAMIENTO, STATUS, ID_UPL_ORIGEN, geom)
      
      return(Nuevos_Puntos_sf)
    }
    
    # Si no hay déficit, no devolvemos nada
    return(NULL)
  })
  
  return(Propuestas_Para_Esta_UPL)
})

# --- 6.2. Consolidación de los puntos agregados en un solo sf ---

# `Lista_Equipamientos_Propuestos` es una lista de sf.
# Los unimos todos en un único sf final.
Equipamientos_Propuestos_sf <- bind_rows(Lista_Equipamientos_Propuestos)
Equipamientos_Propuestos_sf

# --- RESULTADO FINAL ---
if (nrow(Equipamientos_Propuestos_sf) > 0) {
  cat("\n\n --- Análisis completado. Se han generado", nrow(Equipamientos_Propuestos_sf), "nuevos equipamientos propuestos ---\n")
  
  # Imprime un resumen del resultado
  print("Resumen de equipamientos propuestos:")
  print(
    Equipamientos_Propuestos_sf %>%
      st_drop_geometry() %>%
      count(EQUIPAMIENTO, sort = TRUE)
  )
} else {
  cat("\n\n--- Análisis completado. No se encontraron déficits que requirieran nuevos equipamientos ---\n")
}

# Volvemos a un nombre conocido:
Nuevos_Equipamientos_sf <- Equipamientos_Propuestos_sf |>
  select(-ID_UPL_ORIGEN)
Nuevos_Equipamientos_sf
# Scn1: 591,267
# Scn4: 5,643 (Filtrado a 2 UPLs)
# Scn4: 129,174 (Sin filtrado)
# Scn4:  (Sin filtrado 50%)


# Creamos un vector de ubicación, para no tener que escribir esto varias veces
Campos_De_Ubicacion_LOC_MNZ <- c("COD_LOC", "COD_UPL", "COD_UTAM", "COD_ZAT", "COD_SECT", "COD_SECC", "COD_HEX", "COD_MNZ")
Campos_De_Ubicacion_LOC_MNZ

# Ubicamos los nuevos puntos espacialmente:
Puntos_Ubicados_Espacialmente_sf <- Nuevos_Equipamientos_sf |>
  #  select(-STATUS, -ID_UPL_ORIGEN) |>
  st_join(
    Shape_Manzanas[Campos_De_Ubicacion_LOC_MNZ], # Solo nos interesa la columna del código de la manzana
    join = st_intersects,      # El predicado espacial: el punto debe intersectar la manzana
    left = TRUE                # TRUE para mantener todos los puntos, incluso si no caen en ninguna manzana
  )
Puntos_Ubicados_Espacialmente_sf

# Manzanas afectadas:
Manzanas_Afectadas_sf <- Shape_Manzanas |>
  filter(COD_MNZ %in% unique(Puntos_Ubicados_Espacialmente_sf$COD_MNZ))
Manzanas_Afectadas_sf

# Verificamos la implementación de los equipamientos nuevos
tm_basemap("OpenStreetMap") +
  tm_shape(Capa_Etiquetas_UPL) +
  tm_polygons(fill = "lightblue", fill_alpha = 0.3,  col_alpha = 1, col = "darkblue") +
  tm_shape(Manzanas_Afectadas_sf) +
  tm_polygons(fill = "bisque3", fill_alpha = 0.5) +
  tm_shape(Nuevos_Equipamientos_sf) +
  tm_dots(fill = "darkgray", size = 1) +
  tm_shape(Linea_Metro) +
  tm_lines(col = "darkgreen") +
  tm_shape(Estaciones_Metro) +
  tm_dots(fill = "blue", size = 1)
  
# NOTA:
# Si alcanza el tiempo, hacemos mapa bonito.

# Guardamos:
# save(
#   Nuevos_Equipamientos_sf,
#   Manzanas_Afectadas_sf,
#   file = "./Data/2_Processing/4_Amenities/Scn4/Nuevos_Equipamientos_Scn4.RData"
# )

# Por si necesitamos cargar los datos:
load("./Data/2_Processing/4_Amenities/Scn4/Nuevos_Equipamientos_Scn4.RData")

# Tiempo de procesamiento:
End_Time_S4_4_P6 <- Sys.time()
print(paste("6. Tiempo de generación de los nuevos equipamientos en cada UPL sobre cada MNZ: ", as.duration(End_Time_S4_4_P6 - Start_Time_S4_4_P6)))

# ####
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ::: 7. Unificamos los nuevos con el Shape de puntos original (Scn1)        :::
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ####
#
Start_Time_S4_4_P7 <- Sys.time()

# Creamos un vector de ubicación, para no tener que escribir esto varias veces
Campos_De_Ubicacion_LOC_HEX <- c("COD_LOC", "COD_UPL", "COD_UTAM", "COD_ZAT", "COD_SECT", "COD_SECC", "COD_HEX")
Campos_De_Ubicacion_LOC_HEX

# Extraemos un df de Categorías del Scn1 para completar los datos del Scn3:
Categories_Of_Amenities_df <- Amenities_Puntos_Scn1_sf |>
  st_drop_geometry() |>
  select(!all_of(Campos_De_Ubicacion_LOC_HEX)) |>
  distinct() |>
  as_tibble()
Categories_Of_Amenities_df

# Creamos el sf de puntos de nuevos equipamientos (Ubicados y categorizados)
Nuevos_Equipamientos_Puntos_sf <- Puntos_Ubicados_Espacialmente_sf |>
  select(-COD_MNZ) |>
  left_join(Categories_Of_Amenities_df, by = "EQUIPAMIENTO")
Nuevos_Equipamientos_Puntos_sf

# Unificamos los dos sf: Nuevos puntos + Original:
Amenities_Puntos_Scn4_sf <- Nuevos_Equipamientos_Puntos_sf |>
  bind_rows(
    Amenities_Puntos_Scn1_sf |>
      mutate(
        STATUS = "Original_Scn1",
      )
  )
Amenities_Puntos_Scn4_sf

# Verificamso sumas:
Amenities_Puntos_Scn4_sf |> filter(STATUS == "Propuesto") |> nrow()
Amenities_Puntos_Scn4_sf |> filter(STATUS == "Original_Scn1") |> nrow()
length(Nuevos_Equipamientos_Puntos_sf$EQUIPAMIENTO) + length(Amenities_Puntos_Scn1_sf$EQUIPAMIENTO)
length(Amenities_Puntos_Scn4_sf$EQUIPAMIENTO)

# Verificamos nombres:
names(Nuevos_Equipamientos_Puntos_sf)
names(Amenities_Puntos_Scn1_sf)
names(Amenities_Puntos_Scn4_sf)

# Verificamos todos los equipamientos para el Escenario 4:
tm_basemap("OpenStreetMap") +
  tm_shape(Capa_Etiquetas_UPL) +
  tm_polygons(fill = "lightblue", fill_alpha = 0.3,  col_alpha = 1, col = "darkblue") +
  tm_shape(Manzanas_Afectadas_sf) +
  tm_polygons(fill = "bisque3", fill_alpha = 0.5) +
  tm_shape(Amenities_Puntos_Scn4_sf |> select(EQUIPAMIENTO, STATUS, geom) |> filter(STATUS == "Original_Scn1")) +
  tm_dots(fill = "darkgray", size = 1) +
  tm_shape(Amenities_Puntos_Scn4_sf |> select(EQUIPAMIENTO, STATUS, geom) |> filter(STATUS == "Propuesto")) +
  tm_dots(fill = "purple", size = 1) +
  tm_shape(Linea_Metro) +
  tm_lines(col = "darkgreen") +
  tm_shape(Estaciones_Metro) +
  tm_dots(fill = "blue", size = 1)

# Guardamos:
# save(
#   Nuevos_Equipamientos_Puntos_sf,
#   Amenities_Puntos_Scn4_sf,
#   file = "./Data/2_Processing/4_Amenities/Scn4/Shape_Amenities_Puntos_Scn4.RData"
# )

# Por si necesitamos cargar los datos:
load("./Data/2_Processing/4_Amenities/Scn4/Shape_Amenities_Puntos_Scn4.RData")

# Tiempo de procesamiento:
End_Time_S4_4_P7 <- Sys.time()
print(paste("7. Tiempo de unificación de los nuevos con el Shape de puntos original (Scn1): ", as.duration(End_Time_S4_4_P7 - Start_Time_S4_4_P7)))


# ####
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ::: 8. Recarga de datos producto de este script                            :::
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ####
#
Start_Time_S4_4_P8 <- Sys.time()

# Por si necesitamos cargar los datos:
load("./Data/2_Processing/4_Amenities/Scn4/Shape_Amenities_Puntos_Scn4.RData")

# Tiempo de procesamiento:
End_Time_S4_4_P8 <- Sys.time()
print(paste("8. Tiempo de recarga de datos producto del script: ", as.duration(End_Time_S4_4_P8 - Start_Time_S4_4_P8)))

# ####
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ::: 9. Tiempos de procesamiento                                            :::
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ####
#
# Tiempo de procesamiento:
End_Time_S4_4_P0 <- Sys.time()

print(paste("1. Tiempo de inicialización: ", as.duration(End_Time_S4_4_P1 - Start_Time_S4_4_P1)))
print(paste("2. Tiempo de carga de datos de origen: ", as.duration(End_Time_S4_4_P2 - Start_Time_S4_4_P2)))
print(paste("3. Tiempo de creación de la capa de etiquetas: ", as.duration(End_Time_S4_4_P3 - Start_Time_S4_4_P3)))
print(paste("4. Tiempo de determinación de áreas para implantación de equipamientos: ", as.duration(End_Time_S4_4_P4 - Start_Time_S4_4_P4)))
print(paste("5. Tiempo de preparación de df y sf para la implantación de equipamientos: ", as.duration(End_Time_S4_4_P5 - Start_Time_S4_4_P5)))
print(paste("6. Tiempo de generación de los nuevos equipamientos en cada UPL sobre cada MNZ: ", as.duration(End_Time_S4_4_P6 - Start_Time_S4_4_P6)))
print(paste("7. Tiempo de unificación de los nuevos con el Shape de puntos original (Scn1): ", as.duration(End_Time_S4_4_P7 - Start_Time_S4_4_P7)))
print(paste("8. Tiempo de recarga de datos producto del script: ", as.duration(End_Time_S4_4_P8 - Start_Time_S4_4_P8)))
print(paste("9. Tiempo de prcesamiento del script: ", as.duration(End_Time_S4_4_P0 - Start_Time_S4_4_P0)))
Avisar()
