# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# :::                                                                        :::
# :::               SCRIPT DE EQUIPAMIENTOS - ESCENARIO 3                    :::
# :::                                                                        :::
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#
# Nombre:       4_3_Amenities_Scenario_3.R
#
# Descripcion:  Este Script hará la implementación de equipamientos en las inmediaciones
#               de las estaciones del metro, con el fin de igualar la media de equipamientos
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
# 4. Tiempo de creación de isocronas y buffer al rededor de las estaciones
# 5. Preparación de df y sf para la implantación de equipamientos
# 6. Generamos los nuevos equipamientos en cada isocrona sobre cada MNZ
# 7. Unificamos los nuevos con el Shape de puntos original (Scn1)
# 8. Recarga de datos producto de este script y detención de entorno
# 9. Tiempos de procesamiento
#
Start_Time_S4_3_P0 <- Sys.time()

# ####
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ::: 1. Inicialización                                                      :::
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ####
#
Start_Time_S4_3_P1 <- Sys.time()

source("0_Initialization.R")

# Verificamos la versión de Java instalada, si es que hay alguna.
rJavaEnv::java_check_version_rjava()

# Si no está instalada, es necesario instalarla con el siguiente código:
# rJavaEnv::rje_consent(provided = TRUE)
# rJavaEnv::java_quick_install(version = 21) # install Java 21

# Verificamos que haya quedado bien instalada la versión 21 de Java
rJavaEnv::java_check_version_rjava()

# Establecemos el límite de RAM y Cores del CPU a usar por Java:
options(java.parameters = c("-Xmx24G", "-XX:ActiveProcessorCount=7"))

# Definimos la ruta donde se guardan los archivos del motor de r5r:
# ¡Importante!
# Aquí usamos el mismo Escenario 3 de accesibilidad, para ser consistente cuando se
# corra el modelo de accesibilidad.
data_path_scenario_3 <- "./Data/1_Sources/6_Accesibility/Scn3/r5r/"
list.files(data_path_scenario_3)

# Revisamos los archivos dentro de la carpeta:
list.files(data_path_scenario_3)
pbf_file <- list.files(data_path_scenario_3, pattern = ".pbf")[1]
gtfs_file <- list.files(data_path_scenario_3, pattern = "zip")
pbf_file
gtfs_file

# Cargamos el motor de r5r
r5r_core_scenario_3 <- setup_r5(
  data_path = data_path_scenario_3
)

# Confirmamos datos de Java:
runtime <- rJava::.jcall("java/lang/Runtime", "Ljava/lang/Runtime;", "getRuntime")
maxMemory <- rJava::.jcall(runtime, "J", "maxMemory") / (1024^2)
totalMemory <- rJava::.jcall(runtime, "J", "totalMemory") / (1024^2)
freeMemory <- rJava::.jcall(runtime, "J", "freeMemory") / (1024^2)
#cat("Cores usados para r5r_core_Scn3:",r5r_core_scenario_3$getNumberOfThreads(), "cores.\n")
cat("Máxima memoria: ", maxMemory, "MB\n")
cat("Memoria total asignada: ", totalMemory, "MB\n")
cat("Memoria libre: ", freeMemory, "MB\n")

# Tiempo de procesamiento:
End_Time_S4_3_P1 <- Sys.time()
print(paste("1. Tiempo de inicialización: ", as.duration(End_Time_S4_3_P1 - Start_Time_S4_3_P1)))

# ####
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ::: 2. Carga de datos de origen                                            :::
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ####
#
# Cargamos los shapes vacíos y los de población para comenzar a hacer consultas 
# y los análisis.
#
Start_Time_S4_3_P2 <- Sys.time()

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
Linea_Metro <- gtfstools::convert_shapes_to_sf(read_gtfs("./Data/1_Sources/6_Accesibility/Scn3/r5r/GTFS_M1-Actual.zip"))
Estaciones_Metro <- gtfstools::convert_stops_to_sf(read_gtfs("./Data/1_Sources/6_Accesibility/Scn3/r5r/GTFS_M1-Actual.zip"))

# Carga de los datos óptimos calibrados
load("./Data/2_Processing/6_Accesibility/Scn0/Variables_Calibracion_Modelo_RMSE_2025-10-22.RData")
Origenes_Calibracion_sf
Destinos_Calibracion_sf
Matriz_Tiempos_Viaje
Tiempos_Encuesta_df
Comparacion_df       # Best:
Optimo_Walk_Speed    # 2.34785
Optimo_Max_Walk_Time # 38.73269
Tiempo_de_Partida    # "2025-04-10 14:04:00 CEST"
Mejor_RMSE           # 32.29497

cat("Parámetros óptimos encontrados",
    "\n   Walk_speed:", Optimo_Walk_Speed,
    "\n   Max_walk_time:", Optimo_Max_Walk_Time,
    "\n   Time:", as.character(Tiempo_de_Partida),
    "\n   Mejor RMSE:", Mejor_RMSE
)

# Tiempo de procesamiento:
End_Time_S4_3_P2 <- Sys.time()
print(paste("2. Tiempo de carga de datos de origen: ", as.duration(End_Time_S4_3_P2 - Start_Time_S4_3_P2)))

# ####
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ::: 3. Creamos capa de etiquetas para poner encima de los mapas            :::
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ####
#
Start_Time_S4_3_P3 <- Sys.time()

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
End_Time_S4_3_P3 <- Sys.time()
print(paste("3. Tiempo de creación de la capa de etiquetas: ", as.duration(End_Time_S4_3_P3 - Start_Time_S4_3_P3)))

# ####
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ::: 4. Creamos las isocronas de estaciones con un buffer de 50m            :::
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ####
# Creamos isocronas a 15 minutos, con punto de partida las estaciones del metro
# y a ellas, les hacemos un buffer de 50 metros para garantizar la intersección
# con las manzanas adyacentes.
#
Start_Time_S4_3_P4 <- Sys.time()

# Verificamos las estaciones:
Estaciones_Metro
tm_basemap("OpenStreetMap") +
  tm_shape(Capa_Etiquetas_UPL) +
  tm_polygons(fill = "lightblue", fill_alpha = 0.3,  col_alpha = 1, col = "darkblue") +
  tm_shape(Linea_Metro) +
  tm_lines(col = "darkgreen") +
  tm_shape(Estaciones_Metro) +
  tm_dots(fill = "blue", size = 1)

# Creamos las isocronas
Isochronas_Estaciones_sf <- isochrone(
  r5r_core = r5r_core_scenario_3,
  origins = Estaciones_Metro |> mutate(id = stop_id),
  departure_datetime = Tiempo_de_Partida,
  mode = "WALK",
  cutoffs = c(15),
  polygon_output = TRUE,
  walk_speed = Optimo_Walk_Speed,
  max_walk_time = Optimo_Max_Walk_Time,
  sample_size = 1
) |>
  st_make_valid() |>
  select(id, geom = polygons)
Isochronas_Estaciones_sf

# Verificamos las Isocrhonas de las estaciones:
tm_basemap("OpenStreetMap") +
  tm_shape(Capa_Etiquetas_UPL) +
  tm_polygons(fill = "lightblue", fill_alpha = 0.3,  col_alpha = 1, col = "darkblue") +
  tm_shape(Isochronas_Estaciones_sf) +
  tm_polygons(fill = "darkred", fill_alpha = 0.25) +
  tm_shape(Linea_Metro) +
  tm_lines(col = "darkgreen") +
  tm_shape(Estaciones_Metro) +
  tm_dots(fill = "blue", size = 1)

# NOTA:
# Si alcanza el tiempo, hacemos mapa bonito.

# Creamos el buffer de 50 metros para las isocronas:
Buffer_Isochronas_Estaciones_sf <- Isochronas_Estaciones_sf |>
  st_buffer(50) |>
  st_transform(crs = Coordinates_CRS)
Buffer_Isochronas_Estaciones_sf

# Verificamos las Isocrhonas de las estaciones y los buffers:
tm_basemap("CartoDB.PositronNoLabels") +
  # tm_shape(Capa_Etiquetas_UPL) +
  # tm_polygons(fill = "lightblue", fill_alpha = 0.3,  col_alpha = 1, col = "darkblue") +
  tm_shape(Buffer_Isochronas_Estaciones_sf) +
  tm_polygons(fill = "darkgreen", fill_alpha = 0.2) +
  tm_shape(Isochronas_Estaciones_sf) +
  tm_polygons(fill = "purple", fill_alpha = 0.2) +
  tm_shape(Linea_Metro) +
  tm_lines(col = "#e6007e", lwd = 4) +
  tm_shape(Estaciones_Metro) +
  tm_bubbles(
    size = 1,             # Tamaño del círculo
    col = "white",          # Color de relleno (centro)
    border.col = "#e6007e", # Color del borde
    lwd = 4        # Grosor del borde (auméntalo si no se ve)
  )

# NOTA:
# Si alcanza el tiempo, hacemos mapa bonito.

# Tiempo de procesamiento:
End_Time_S4_3_P4 <- Sys.time()
print(paste("4. Tiempo de creación de isocronas y buffer al rededor de las estaciones: ", as.duration(End_Time_S4_3_P4 - Start_Time_S4_3_P4)))

# ####
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ::: 5. Preparación de df y sf para la implantación de equipamientos        :::
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ####
#
Start_Time_S4_3_P5 <- Sys.time()

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
  st_crs(Buffer_Isochronas_Estaciones_sf) == st_crs(Amenities_Puntos_Scn1_sf),
  st_crs(Buffer_Isochronas_Estaciones_sf) == st_crs(Shape_Manzanas),
  st_crs(Buffer_Isochronas_Estaciones_sf) == st_crs(Shape_Municipios)
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

Amenities_Inventory_Filtrado_df <- Amenities_Inventory_Scn1_BOG_Longer_df |>
  filter(!EQUIPAMIENTO %in% Equipamientos_a_Excluir)
Amenities_Inventory_Filtrado_df

## 5.2.2: Calcular Área Total de la Ciudad
# Las unidades del área, las asumimos como metros cuadrados
Area_Total_Ciudad_m2 <- st_area(Shape_Municipios) |> as.numeric()
Area_Total_Ciudad_m2

## 5.2.3: Calcular Densidad de Referencia para cada Equipamiento
Densidades_Referencia_df <- Amenities_Inventory_Filtrado_df |>
  mutate(
    Densidad_Equip_Por_m2 = NUMERO_DE_EQUIPAMIENTOS / Area_Total_Ciudad_m2
  ) |>
  select(EQUIPAMIENTO, Densidad_Equip_Por_m2)
Densidades_Referencia_df

# Mostramos un ejemplo de las densidades calculadas
print("Ejemplo de densidades de referencia (equipamientos por m2):")
head(Densidades_Referencia_df)

# Tiempo de procesamiento:
End_Time_S4_3_P5 <- Sys.time()
print(paste("5. Tiempo de preparación de df y sf para la implantación de equipamientos: ", as.duration(End_Time_S4_3_P5 - Start_Time_S4_3_P5)))

# ####
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ::: 6. Generamos los nuevos equipamientos en cada isocrona sobre cada MNZ  :::
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ####
#
Start_Time_S4_3_P6 <- Sys.time()

# Usaremos `map_dfr` de purrr para iterar sobre cada isocrona y unir
# los resultados en un único sf al final. Es más eficiente que un bucle for.

# Añadimos un ID único a cada isocrona para facilitar el seguimiento:
Buffer_Isochronas_Estaciones_sf$ID_ISOCRONA <- 1:nrow(Buffer_Isochronas_Estaciones_sf)
Buffer_Isochronas_Estaciones_sf

# --- 6.1. Iteración para cada Isocrona y para cada tipo de equipamiento ---

# El corazón del análisis: iteramos por cada fila (isocrona) del sf:
Lista_Equipamientos_Propuestos <- map(Buffer_Isochronas_Estaciones_sf$ID_ISOCRONA, function(id_iso) {
  
  # Seleccionamos la isocrona actual
  Isocrona_Actual_sf <- Buffer_Isochronas_Estaciones_sf |> filter(ID_ISOCRONA == id_iso)
  
  cat(paste("\n--- Procesando Isocrona ID:", id_iso, "|", Isocrona_Actual_sf$id[1], "---\n"))
  
  # --- Cálculos específicos para ESTA isocrona ---
  
  # Área de la isocrona actual
  Area_Isocrona_m2 <- st_area(Isocrona_Actual_sf) |> as.numeric()
  cat(paste(" Area de isocrona ", id_iso, ": ", Area_Isocrona_m2, " m2.\n", sep = ""))
  
  # Identificar manzanas que se intersectan con la isocrona
  Manzanas_En_Isocrona_sf <- Shape_Manzanas |>
    st_filter(Isocrona_Actual_sf, .predicate = st_intersects)
  
  # Si no hay manzanas en la isocrona, no podemos hacer nada.
  if (nrow(Manzanas_En_Isocrona_sf) == 0) {
    cat("Advertencia: La isocrona", id_iso, "no intersecta con ninguna manzana. Se omite.\n")
    return(NULL) # Devolvemos NULL para que no se añada al resultado
  }
  
  # Calcular los centroides de esas manzanas (nuestras ubicaciones posibles)
  Centroides_Posibles_sf <- st_centroid(Manzanas_En_Isocrona_sf)
  
  # --- Bucle interno: Analizar el déficit para cada TIPO de equipamiento ---
  
  Propuestas_Para_Esta_Isocrona <- map_dfr(1:nrow(Densidades_Referencia_df), function(i) {
    
    # Datos del equipamiento que estamos analizando
    Tipo_Equipamiento <- Densidades_Referencia_df$EQUIPAMIENTO[i]
    Densidad_Ref <- Densidades_Referencia_df$Densidad_Equip_Por_m2[i]
    
    # Contar cuántos equipamientos de este tipo YA existen en la isocrona
    Equip_Actuales_En_Isocrona <- Amenities_Puntos_Scn1_sf |>
      filter(EQUIPAMIENTO == Tipo_Equipamiento) |>
      st_filter(Isocrona_Actual_sf, .predicate = st_intersects)
    
    Numero_Actual <- nrow(Equip_Actuales_En_Isocrona)
    
    # Calcular el número OBJETIVO de equipamientos para esta isocrona
    Numero_Objetivo <- Densidad_Ref * Area_Isocrona_m2
    
    # Calcular el DÉFICIT (cuántos faltan)
    Deficit <- round(Numero_Objetivo) - Numero_Actual                           # Está rarito.
    
    # --- Implantación de Nuevos Equipamientos ---
    if (Deficit > 0) {
      cat(paste("  Déficit para Isocrona ", id_iso,": '", Tipo_Equipamiento, "': ", Deficit, " unidades.\n", sep=""))
      
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
          ID_ISOCRONA_ORIGEN = id_iso
        ) |>
        select(EQUIPAMIENTO, STATUS, ID_ISOCRONA_ORIGEN, geom)
      
      return(Nuevos_Puntos_sf)
    }
    
    # Si no hay déficit, no devolvemos nada
    return(NULL)
  })
  
  return(Propuestas_Para_Esta_Isocrona)
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

Nuevos_Equipamientos_sf <- Equipamientos_Propuestos_sf
Nuevos_Equipamientos_sf

Manzanas_Afectadas_sf <- Shape_Manzanas |>
  st_filter(Buffer_Isochronas_Estaciones_sf, .predicate = st_intersects)

# Verificamos la implementación de los equipamientos nuevos
tm_basemap("OpenStreetMap") +
  tm_shape(Capa_Etiquetas_UPL) +
  tm_polygons(fill = "lightblue", fill_alpha = 0.3,  col_alpha = 1, col = "darkblue") +
  tm_shape(Manzanas_Afectadas_sf) +
  tm_polygons(fill = "bisque3", fill_alpha = 0.5) +
  tm_shape(Isochronas_Estaciones_sf) +
  tm_polygons(fill = "darkred", fill_alpha = 0.25) +
  tm_shape(Buffer_Isochronas_Estaciones_sf) +
  tm_polygons(fill = "purple", fill_alpha = 0.25) +
  tm_shape(Linea_Metro) +
  tm_lines(col = "darkgreen") +
  tm_shape(Estaciones_Metro) +
  tm_dots(fill = "blue", size = 1) +
  tm_shape(Nuevos_Equipamientos_sf) +
  tm_dots(fill = "darkgray", size = 1)

# NOTA:
# Si alcanza el tiempo, hacemos mapa bonito.

# Guardamos:
# save(
#   Nuevos_Equipamientos_sf,
#   Manzanas_Afectadas_sf,
#   file = "./Data/2_Processing/4_Amenities/Scn3/Nuevos_Equipamientos_Scn3.RData"
# )

# Por si necesitamos cargar los datos:
load("./Data/2_Processing/4_Amenities/Scn3/Nuevos_Equipamientos_Scn3.RData")

# Tiempo de procesamiento:
End_Time_S4_3_P6 <- Sys.time()
print(paste("6. Tiempo de generación de los nuevos equipamientos en cada isocrona sobre cada MNZ: ", as.duration(End_Time_S4_3_P6 - Start_Time_S4_3_P6)))

# ####
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ::: 7. Unificamos los nuevos con el Shape de puntos original (Scn1)        :::
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ####
#
Start_Time_S4_3_P7 <- Sys.time()

# Creamos un vector de ubicación, para no tener que escribir esto varias veces
Campos_De_Ubicacion <- c("COD_LOC", "COD_UPL", "COD_UTAM", "COD_ZAT", "COD_SECT", "COD_SECC", "COD_HEX")

# Extraemos un df de Categorías del Scn1 para completar los datos del Scn3:
Categories_Of_Amenities_df <- Amenities_Puntos_Scn1_sf |>
  st_drop_geometry() |>
  select(!Campos_De_Ubicacion) |>
  distinct() |>
  as_tibble()
Categories_Of_Amenities_df

# Realizamos una unión espacial (spatial join)
Puntos_Ubicados_Espacialmente_sf <- Nuevos_Equipamientos_sf |>
#  select(-STATUS, -ID_ISOCRONA_ORIGEN) |>
  st_join(
    Manzanas_Afectadas_sf[Campos_De_Ubicacion], # Solo nos interesa la columna del código de la manzana
    join = st_intersects,      # El predicado espacial: el punto debe intersectar la manzana
    left = TRUE                # TRUE para mantener todos los puntos, incluso si no caen en ninguna manzana
  )
Puntos_Ubicados_Espacialmente_sf

# Creamos el sf de puntos de nuevos equipamientos (Ubicados y categorizados)
Nuevos_Equipamientos_Puntos_sf <- Puntos_Ubicados_Espacialmente_sf |>
  left_join(Categories_Of_Amenities_df, by = "EQUIPAMIENTO")
Nuevos_Equipamientos_Puntos_sf

# Unificamos los dos sf: Nuevos puntos + Original:
Amenities_Puntos_Scn3_sf <- Nuevos_Equipamientos_Puntos_sf |>
  bind_rows(
    Amenities_Puntos_Scn1_sf |>
      mutate(
        STATUS = "Original_Scn1",
        ID_ISOCRONA_ORIGEN = 0
      )
  )
Amenities_Puntos_Scn3_sf

# Verificamso sumas:
Amenities_Puntos_Scn3_sf |> filter(STATUS == "Propuesto") |> nrow()
Amenities_Puntos_Scn3_sf |> filter(STATUS == "Original_Scn1") |> nrow()
length(Nuevos_Equipamientos_Puntos_sf$EQUIPAMIENTO) + length(Amenities_Puntos_Scn1_sf$EQUIPAMIENTO)
length(Amenities_Puntos_Scn3_sf$EQUIPAMIENTO)

# Verificamos nombres:
names(Nuevos_Equipamientos_Puntos_sf)
names(Amenities_Puntos_Scn1_sf)
names(Amenities_Puntos_Scn3_sf)

# Verificamos todos los equipamientos para el Escenario 3:
tm_basemap("OpenStreetMap") +
  tm_shape(Capa_Etiquetas_UPL) +
  tm_polygons(fill = "lightblue", fill_alpha = 0.3,  col_alpha = 1, col = "darkblue") +
  tm_shape(Manzanas_Afectadas_sf) +
  tm_polygons(fill = "bisque3", fill_alpha = 0.5) +
  tm_shape(Isochronas_Estaciones_sf) +
  tm_polygons(fill = "darkred", fill_alpha = 0.25) +
  tm_shape(Buffer_Isochronas_Estaciones_sf) +
  tm_polygons(fill = "purple", fill_alpha = 0.25) +
  tm_shape(Linea_Metro) +
  tm_lines(col = "darkgreen") +
  tm_shape(Estaciones_Metro) +
  tm_dots(fill = "blue", size = 1) +
  tm_shape(Amenities_Puntos_Scn3_sf |> select(EQUIPAMIENTO, STATUS, geom) |> filter(STATUS == "Original_Scn1")) +
  tm_dots(fill = "darkgray", size = 1) +
  tm_shape(Amenities_Puntos_Scn3_sf |> select(EQUIPAMIENTO, STATUS, geom) |> filter(STATUS == "Propuesto")) +
  tm_dots(fill = "purple", size = 1)

# Guardamos:
# save(
#   Isochronas_Estaciones_sf,
#   Buffer_Isochronas_Estaciones_sf,
#   Nuevos_Equipamientos_Puntos_sf,
#   Amenities_Puntos_Scn3_sf,
#   file = "./Data/2_Processing/4_Amenities/Scn3/Shape_Amenities_Puntos_Scn3.RData"
# )

# Por si necesitamos cargar los datos:
load("./Data/2_Processing/4_Amenities/Scn3/Shape_Amenities_Puntos_Scn3.RData")

# Tiempo de procesamiento:
End_Time_S4_3_P7 <- Sys.time()
print(paste("7. Tiempo de unificación de los nuevos con el Shape de puntos original (Scn1): ", as.duration(End_Time_S4_3_P7 - Start_Time_S4_3_P7)))

# ####
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ::: 8. Recarga de datos producto de este script y detención de entorno     :::
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ####
#
Start_Time_S4_3_P8 <- Sys.time()

# Por si necesitamos cargar los datos:
load("./Data/2_Processing/4_Amenities/Scn3/Shape_Amenities_Puntos_Scn3.RData")

# --- Limpiamos después de usar r5r_core ---
r5r_core_scenario_3$getNumberOfThreads()
r5r::stop_r5(r5r_core_scenario_3)
rJava::.jgc(R.gc = TRUE)

# Tiempo de procesamiento:
End_Time_S4_3_P8 <- Sys.time()
print(paste("8. Tiempo de recarga de datos producto del script y detención de entorno: ", as.duration(End_Time_S4_3_P8 - Start_Time_S4_3_P8)))

# ####
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ::: 9. Tiempos de procesamiento                                            :::
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ####
#
# Tiempo de procesamiento:
End_Time_S4_3_P0 <- Sys.time()

print(paste("1. Tiempo de inicialización: ", as.duration(End_Time_S4_3_P1 - Start_Time_S4_3_P1)))
print(paste("2. Tiempo de carga de datos de origen: ", as.duration(End_Time_S4_3_P2 - Start_Time_S4_3_P2)))
print(paste("3. Tiempo de creación de la capa de etiquetas: ", as.duration(End_Time_S4_3_P3 - Start_Time_S4_3_P3)))
print(paste("4. Tiempo de creación de isocronas y buffer al rededor de las estaciones: ", as.duration(End_Time_S4_3_P4 - Start_Time_S4_3_P4)))
print(paste("5. Tiempo de preparación de df y sf para la implantación de equipamientos: ", as.duration(End_Time_S4_3_P5 - Start_Time_S4_3_P5)))
print(paste("6. Tiempo de generación de los nuevos equipamientos en cada isocrona sobre cada MNZ: ", as.duration(End_Time_S4_3_P6 - Start_Time_S4_3_P6)))
print(paste("7. Tiempo de unificación de los nuevos con el Shape de puntos original (Scn1): ", as.duration(End_Time_S4_3_P7 - Start_Time_S4_3_P7)))
print(paste("8. Tiempo de recarga de datos producto del script y detención de entorno: ", as.duration(End_Time_S4_3_P8 - Start_Time_S4_3_P8)))
print(paste("9. Tiempo de prcesamiento del script: ", as.duration(End_Time_S4_3_P0 - Start_Time_S4_3_P0)))
Avisar()