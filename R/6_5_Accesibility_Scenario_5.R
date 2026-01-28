# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# :::                                                                        :::
# :::                        SCRIPT DE ACCESIBILIDAD                         :::
# :::                      Scenario 5 - Con red férrea                       :::
# :::                                                                        :::
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#
# Nombre: 6_5_Accesibility_Scenario_5.R
#
# Descripcion: Este Script usará la librería r5r para los cálculos de isocronas
# y de accesibilidad al inventario de equipamientos de la ciudad. A diferencia del
# script Scn2, en este Scn5 se evalúa la accesibilidad con la implementación del
# sistema de transporte masivo férreo para Bogotá.
#
# Es necesario contar con los shapes de origen y destino para la correcta ejecución
# de este script.

# ####
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# :::                      Contenido de este Script                          :::
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ####
#
# 1. Inicialización
# 2. Carga de datos de origen
# 3. Cálculo de Isocronas de muestra
# 4. Cálculo de itinerarios de testeo
# 5. Cálculo de accesibilidad usando R5R
# 6. Ordenamos y normalizamos la información por MNZ
# 7. Ordenamos y normalizamos la información por UPL
# 8. Creamos los mapas de Accesibilidad por MNZ
# 9. Creamos los mapas de Accesibilidad por UPL
# 10. Creación de mapas de Funciones Sociales UPL+MNZ
# 11. Cálculo de ganancia de accesibilidad entre escenarios
# 12. Carga de datos del Script y detención de r5r y Java
# 13. Tiempo de procesamiento del script
Start_Time_S6_5_P0 <- Sys.time()

# ####
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ::: 1. Inicialización                                                      :::
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ####
#
Start_Time_S6_5_P1 <- Sys.time()

# Cargamos el script de inicialización general
source("0_Initialization.R")

# Verificamos la versión de Java instalada, si es que hay alguna.
rJavaEnv::java_check_version_rjava()

# Si no está instalada, es necesario instalarla con el siguiente código:
# rJavaEnv::rje_consent(provided = TRUE)
# rJavaEnv::java_quick_install(version = 21) # install Java 21

# Verificamos que haya quedado bien instalada la versión 21 de Java
rJavaEnv::java_check_version_rjava()

cat("🔧 Java detecta", num_cores_java, "núcleos del sistema\n")
cat("🔧 Java usará", cores_usar, "núcleos del sistema y", mem_java, "GB de RAM\n")
getOption("java.parameters")

# Definimos la ruta donde se guardan los archivos del motor de r5r:
data_path_scenario_5 <- "./Data/1_Sources/6_Accesibility/Scn5/r5r/"
list.files(data_path_scenario_5)

# Revisamos los archivos dentro de la carpeta:
list.files(data_path_scenario_5)
pbf_file <- list.files(data_path_scenario_5, pattern = ".pbf")[1]
gtfs_file <- list.files(data_path_scenario_5, pattern = "zip")
pbf_file
gtfs_file

# Cargamos el motor de r5r
r5r_core_scenario_5 <- setup_r5(
  data_path = data_path_scenario_5
)

# Confirmamos datos de Java:
runtime <- rJava::.jcall("java/lang/Runtime", "Ljava/lang/Runtime;", "getRuntime")
maxMemory <- rJava::.jcall(runtime, "J", "maxMemory") / (1024^2)
totalMemory <- rJava::.jcall(runtime, "J", "totalMemory") / (1024^2)
freeMemory <- rJava::.jcall(runtime, "J", "freeMemory") / (1024^2)
# cat("Cores usados para r5r_core_Scn5:",r5r_core_scenario_5$getNumberOfThreads(), "cores.\n")
cat("Máxima memoria: ", maxMemory, "MB\n")
cat("Memoria total asignada: ", totalMemory, "MB\n")
cat("Memoria libre: ", freeMemory, "MB\n")

# Tiempo de procesamiento:
End_Time_S6_5_P1 <- Sys.time()
print(paste("1. Tiempo de inicialización: ", as.duration(End_Time_S6_5_P1 - Start_Time_S6_5_P1)))

# ####
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ::: 2. Carga de datos de origen                                            :::
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ####
#
Start_Time_S6_5_P2 <- Sys.time()

# --- 2.1. Carga de Shapes Vacíos (1_Empty_Shapes.R) ---

# Cargamos los shapes vacíos necesarios para el trabajo:
Shape_Municipios <- st_read("./Data/2_Processing/1_Empty_Shapes/Shape_Vacio_Municipios.gpkg")
Shape_Localidades <- st_read("./Data/2_Processing/1_Empty_Shapes/Shape_Vacio_Localidades.gpkg")
Shape_UPLs <- st_read("./Data/2_Processing/1_Empty_Shapes/Shape_Vacio_UPLs.gpkg")
Shape_UTAM <- st_read("./Data/2_Processing/1_Empty_Shapes/Shape_Vacio_UTAM.gpkg")
Shape_ZAT <- st_read("./Data/2_Processing/1_Empty_Shapes/Shape_Vacio_ZAT.gpkg")
Shape_Sectores <- st_read("./Data/2_Processing/1_Empty_Shapes/Shape_Vacio_Sectores.gpkg")
Shape_Secciones <- st_read("./Data/2_Processing/1_Empty_Shapes/Shape_Vacio_Secciones.gpkg")
Shape_Hexagonos <- st_read("./Data/2_Processing/1_Empty_Shapes/Shape_Vacio_Hexagonos.gpkg")
Shape_Manzanas <- st_read("./Data/2_Processing/1_Empty_Shapes/Shape_Vacio_Manzanas.gpkg")
Coordinates_CRS <- st_crs(Shape_Municipios)

# --- 2.2. Carga de equipamientos ---

# Aquí se cargan los puntos del escenario 1, porque no hay ninguna implementación de proximidad
Amenities_Puntos_Scn1_sf <- st_read("./Data/2_Processing/4_Amenities/Scn1/Amenities_Puntos_Scn1_sf.gpkg")

# --- 2.3. Carga de los datos óptimos calibrados ---

load("./Data/2_Processing/6_Accesibility/Scn0/Variables_Calibracion_Modelo_RMSE_2025-10-22.RData")
Origenes_Calibracion_sf
Destinos_Calibracion_sf
Matriz_Tiempos_Viaje
Tiempos_Encuesta_df
Comparacion_df       # Best:
Optimo_Walk_Speed    # 2.82
Optimo_Max_Walk_Time # 10.28
Tiempo_de_Partida    # "2025-04-10 14:04:00 CEST"
Mejor_RMSE           # 32.19

cat("Parámetros óptimos encontrados",
    "\n   Walk_speed:", Optimo_Walk_Speed,
    "\n   Max_walk_time:", Optimo_Max_Walk_Time,
    "\n   Time:", as.character(Tiempo_de_Partida),
    "\n   Mejor RMSE:", Mejor_RMSE
)

# Tiempo de procesamiento:
End_Time_S6_5_P2 <- Sys.time()
print(paste("2. Tiempo de carga de datos de origen: ", as.duration(End_Time_S6_5_P2 - Start_Time_S6_5_P2)))

# ####
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ::: 3. Cálculo de Isocronas de muestra                                    :::
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ####
#
Start_Time_S6_5_P3 <- Sys.time()

# Definimos una hora de partida
Tiempo_de_Partida # Viene de los datos calibrados

# Establecemos el origen de la Isocrona de ejemplo (Arbitrariamente)
Origen_Isocrona <- Shape_UPLs |>
  filter(COD_UPL == "15") |>
  mutate(
    id = NOM_UPL # Es necesario que una variable se llame "id"
  ) |>
  select(id, NOM_UPL) |>
  st_transform(crs = 4326) |> # Si no se cambia la proyección, r5r no funcionará
  st_centroid()
Origen_Isocrona

# Calculamos la isocrona de Ejemplo usando r5r
Shape_Isocrona_Ejemplo_Scn5 <- isochrone(
  r5r_core = r5r_core_scenario_5,
  origins = Origen_Isocrona,
  departure_datetime = Tiempo_de_Partida, # Se había evaluado a las 08:00
  mode = "TRANSIT", # WALK, TRANSIT, BICYCLE
  cutoffs = c(15, 30, 45, 60), #c(15, 30)
  polygon_output = TRUE,
  walk_speed = Optimo_Walk_Speed,
  max_walk_time = Optimo_Max_Walk_Time,
  sample_size = 1
)
Shape_Isocrona_Ejemplo_Scn5
#st_area(Shape_Isocrona_Ejemplo_Scn5)

# Mostramos las Isocronas
tm_basemap("OpenStreetMap") +
  tm_shape(Shape_UPLs) +
  tm_polygons(fill = "white", fill_alpha = 0.001,  col_alpha = 0.25, col = "darkblue") +
  tm_shape(st_make_valid(Shape_Isocrona_Ejemplo_Scn5)) +
  tm_polygons(fill = "darkred", fill_alpha = 0.25) +
  tm_shape(Origen_Isocrona) +
  tm_dots(fill = "darkgreen", size = 1)

# Guardamos y recargamos la Isocrona de Ejemplo:
st_write(Shape_Isocrona_Ejemplo_Scn5, dsn = "./Data/2_Processing/6_Accesibility/Scn5/Shape_Isocrona_Ejemplo_Scn5.gpkg", driver = 'GPKG', append = FALSE)
Shape_Isocrona_Ejemplo_Scn5 <- st_read("./Data/2_Processing/6_Accesibility/Scn5/Shape_Isocrona_Ejemplo_Scn5.gpkg")
Shape_Isocrona_Ejemplo_Scn5

# Tiempo de procesamiento:
End_Time_S6_5_P3 <- Sys.time()
print(paste("3. Tiempo de cálculo de Isocronas: ", as.duration(End_Time_S6_5_P3 - Start_Time_S6_5_P3)))

# ####
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ::: 4. Cálculo de itinerarios de testeo                                    :::
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ####
#
Start_Time_S6_5_P4 <- Sys.time()

# Carga de datos del sistema férreo:
GTFS_M1_Mod <- gtfstools::read_gtfs("./Data/1_Sources/6_Accesibility/Scn5/r5r/GTFS_M1-Modificada.zip")
GTFS_M1_Ext <- gtfstools::read_gtfs("./Data/1_Sources/6_Accesibility/Scn5/r5r/GTFS_M1-Extensiones.zip")
GTFS_M2_Mod <- gtfstools::read_gtfs("./Data/1_Sources/6_Accesibility/Scn5/r5r/GTFS_M2-Modificada.zip")
GTFS_M2_Ext <- gtfstools::read_gtfs("./Data/1_Sources/6_Accesibility/Scn5/r5r/GTFS_M2-Extensiones.zip")
GTFS_M3_Boyaca <- gtfstools::read_gtfs("./Data/1_Sources/6_Accesibility/Scn5/r5r/GTFS_M3-Boyacá.zip")
GTFS_M4_Cali <- gtfstools::read_gtfs("./Data/1_Sources/6_Accesibility/Scn5/r5r/GTFS_M4-Cali.zip")
GTFS_RE_Norte <- gtfstools::read_gtfs("./Data/1_Sources/6_Accesibility/Scn5/r5r/GTFS_RE-Norte.zip")
GTFS_RE_Occidente <- gtfstools::read_gtfs("./Data/1_Sources/6_Accesibility/Scn5/r5r/GTFS_RE-Occidente.zip")
GTFS_RE_Sur <- gtfstools::read_gtfs("./Data/1_Sources/6_Accesibility/Scn5/r5r/GTFS_RE-Sur.zip")
GTFS_T1_K7 <- gtfstools::read_gtfs("./Data/1_Sources/6_Accesibility/Scn5/r5r/GTFS_T1-K7.zip")
GTFS_T2_ElDorado <- gtfstools::read_gtfs("./Data/1_Sources/6_Accesibility/Scn5/r5r/GTFS_T2-ElDorado.zip")

# Unificamos el GTFS
GTFS_Ferreo <- gtfstools::merge_gtfs(
  GTFS_M1_Mod, GTFS_M1_Ext, GTFS_M2_Mod, GTFS_M2_Ext, GTFS_M3_Boyaca, GTFS_M4_Cali,
  GTFS_RE_Norte, GTFS_RE_Occidente, GTFS_RE_Sur, GTFS_T1_K7, GTFS_T2_ElDorado
)

Lineas_Ferreas <- gtfstools::convert_shapes_to_sf(GTFS_Ferreo)
Estaciones_Ferreas <- gtfstools::convert_stops_to_sf(GTFS_Ferreo)

# Verificamos la red férrea:
tm_basemap("OpenStreetMap") +
  tm_shape(Lineas_Ferreas) +
  tm_lines(col = "darkgreen", lwd = 5) +
  tm_shape(Estaciones_Ferreas) +
  tm_dots(fill = "blue", size = 1)

# Calculamos itinerarios:
Origen_sf <- Estaciones_Ferreas |> filter(stop_id == "RE-SUR-E14") |> select(id = stop_id)
Destino_sf <- Estaciones_Ferreas |> filter(stop_id == "T1-K7-E6") |> select(id = stop_id)
Itinerarios_sf <- detailed_itineraries(r5r_core = r5r_core_scenario_5,
                                       origins = Origen_sf,
                                       destinations = Destino_sf,
                                       mode = c("WALK", "TRANSIT"),
                                       departure_datetime = Tiempo_de_Partida,
                                       max_walk_time = Optimo_Max_Walk_Time*4,
                                       walk_speed = Optimo_Walk_Speed,
                                       max_trip_duration = 240,
                                       shortest_path = FALSE,
                                       drop_geometry = FALSE
)
Itinerarios_sf |> arrange(by = "total_duration")

# Mostramos los itinerarios:
tm_basemap("OpenStreetMap") +
  tm_shape(Lineas_Ferreas) +
  tm_lines(col = "darkgreen") +
  tm_shape(Estaciones_Ferreas) +
  tm_dots(fill = "blue", size = 1) +
  tm_shape(Origen_sf) +
  tm_dots(fill = "darkred", size = 1) +
  tm_shape(Destino_sf) +
  tm_dots(fill = "darkgreen", size = 1) +
  tm_shape(Itinerarios_sf) +
  tm_lines(col = "mode", lwd = 5)

# Tiempo de procesamiento:
End_Time_S6_5_P4 <- Sys.time()
print(paste("4. Tiempo de cálculo de itinerarios de testeo: ", as.duration(End_Time_S6_5_P4 - Start_Time_S6_5_P4)))

# ####
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ::: 5. Cálculo de accesibilidad usando R5R                               :::
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ####
#
Start_Time_S6_5_P5 <- Sys.time()

# En esta sección, se calcula la accesibilidad que hay desde el centroide de cada
# una de las manzanas. Para ello, se necesitan los puntos del centroide de cada
# manzana como punto de origen, y los puntos de los equipamientos.
#
# Origen: Centroides de manzanas.
# Destino: Puntos de equipamientos.
#
# Es necesario que en el destino las columnas estén debidamente nombradas, porque
# la función de proximidad suma las coincidencias de las columnas.

# --- 4.1. Estableciendo los orígenes y los destinos ---

# Orígenes de toda la ciudad: 
Shape_Manzanas # Origenes
Origenes_Accesibilidad_City <- Shape_Manzanas |>
  #filter(COD_UPL == "15") |> # Porvenir 15 <-- Ojo!
  mutate(
    id = COD_MNZ,
  ) |>
  st_centroid() |>
  st_transform(crs = 4326) |>
  select(id, COD_MNZ)
Origenes_Accesibilidad_City # 43331 centroides (Manzanas)

# Destinos de toda la ciudad:
Amenities_Puntos_Scn1_sf # Destinos (Recordemos que son los mismos de Scn1)
Categorias_Equipamientos <- names(Amenities_Puntos_Scn1_sf)[12:29] # Columnas a tener en cuenta para la Accesibilidad
Categorias_Equipamientos
Destinos_Accesibilidad_City_Scn5 <- Amenities_Puntos_Scn1_sf |>
  #filter(COD_UPL == "18") |> # Kennedy 18
  mutate(
    id = row_number(),
  ) |>
  st_transform(crs = 4326) |>
  mutate(
    across(where(is.character), ~ ifelse(. == "VERDADERO", 1, .)), # Reemplaza todos los VERDADERO por 1.
    across(where(is.character), ~ ifelse(. == "FALSO", 0, .)),     # Reemplaza todos los FALSO por 0.
    across(Categorias_Equipamientos, ~ as.numeric(.))              # Convertioms las columnas de interés a numeric. 
  )
Destinos_Accesibilidad_City_Scn5 # 21714 / 385355 (18x)

# Calculamos la accesibilidad.
# --- ¡ATENCIÓN! ¡Esto tarda 10,00 horas en procesarse! ---
# Acceso_a_Equipamientos_Scn5 <- accessibility(
#   r5r_core = r5r_core_scenario_5,
#   origins = Origenes_Accesibilidad_City,
#   destinations = Destinos_Accesibilidad_City_Scn5,
#   opportunities_colnames = Categorias_Equipamientos,
#   mode = c("WALK", "TRANSIT"),
#   departure_datetime = Tiempo_de_Partida,
#   walk_speed = Optimo_Walk_Speed,
#   max_walk_time = Optimo_Max_Walk_Time,
#   decay_function = "step",
#   cutoffs = c(15, 30)
# )
head(Acceso_a_Equipamientos_Scn5)
Acceso_a_Equipamientos_Scn5 # 2556 / 1559916
length(Acceso_a_Equipamientos_Scn5$id) # 1559916

# Centroides x Variables x Intervalos (15 y 30)
# 43331 x 18 x 2 = 1559916 (86662)

# Guardo y cargo la tabla de acceso a equipamientos:
# fwrite(Acceso_a_Equipamientos_Scn5, "./Data/2_Processing/6_Accesibility/Scn5/Acceso_a_Equipamientos_Scn5_2025-10-22.csv", row.names = FALSE)
Acceso_a_Equipamientos_Scn5 <- as.data.frame(fread("./Data/2_Processing/6_Accesibility/Scn5/Acceso_a_Equipamientos_Scn5_2025-10-22.csv", sep=",", na = "null"))
Acceso_a_Equipamientos_Scn5

length(Acceso_a_Equipamientos_Scn5$id)
unique(Acceso_a_Equipamientos_Scn5$opportunity)

# Tiempo de procesamiento:
End_Time_S6_5_P5 <- Sys.time()
print(paste("5. Tiempo de cálculo de accesibilidad: ", as.duration(End_Time_S6_5_P5 - Start_Time_S6_5_P5)))
#Avisar()

# ####
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ::: 6. Ordenamos y normalizamos la información por MNZ                     :::
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ####
#
# Hacemos un pivot wider, para dejar las columnas como venimos haciéndolo:
#
Start_Time_S6_5_P6 <- Sys.time()

# Seleccionamos las UPL no rurales
Lista_No_Rural <- Shape_UPLs |> filter(ZONA_UPL != "Rural") |> as_tibble() |> select(COD_UPL)
Lista_No_Rural

# Hacemos un pivot Wider para que nos quede el df por Manzanas:
Acceso_Equipamientos_MNZ_Scn5_sf <- Acceso_a_Equipamientos_Scn5 |>
  select(-percentile) |>
  pivot_wider(
    names_from = "opportunity",
    values_from = "accessibility"
  ) |>
  mutate(
    across(everything(), ~replace_na(., 0)),
    Social_Functions = Living + Working + Supplying + Caring + Learning + Enjoying,
    Composite_Indicators = Well_being + Sociability + Environmental_Impact,
    Total_Amenities = rowSums(across(Education_and_Culture:Community_and_Spirituality)) # Sumamos para saber el total de Amenities
  ) |>
  rename(COD_MNZ = id) |>
  left_join(
    Shape_Manzanas |>
      select(COD_MNZ, COD_UPL, NOM_UPL),
      by = "COD_MNZ"
    ) |>
  filter(COD_UPL %in% Lista_No_Rural$COD_UPL) |>
  st_as_sf() |>
  arrange(COD_UPL)
Acceso_Equipamientos_MNZ_Scn5_sf # 43331 x 18 x 2 => 1559916/18 = 86662) <- 43331 (MNZ) x 2.

# Normalizamos entre 0 y 100 todos los valores máximos de cada columna:
Acceso_Equipamientos_MNZ_Scn5_15min_Norm_df <- Normalizacion(
  df = Acceso_Equipamientos_MNZ_Scn5_sf |> filter(cutoff == 15) |> st_drop_geometry(),
  Columna_Interes = c("COD_MNZ", "COD_UPL", "NOM_UPL", "cutoff"),
  Tipo_Resultado = "Todo")
# Calculamos las medias de las columnas normalizadas:
Acceso_Equipamientos_MNZ_Scn5_15min_Norm_Medias_df <- Normalizacion(
  df = Acceso_Equipamientos_MNZ_Scn5_sf |> filter(cutoff == 15) |> st_drop_geometry(),
  Columna_Interes = c("COD_MNZ", "COD_UPL", "NOM_UPL", "cutoff"),
  Tipo_Resultado = "Medias")

# Normalizamos entre 0 y 100 todos los valores máximos de cada columna:
Acceso_Equipamientos_MNZ_Scn5_30min_Norm_df <- Normalizacion(
  df = Acceso_Equipamientos_MNZ_Scn5_sf |> filter(cutoff == 30) |> st_drop_geometry(),
  Columna_Interes = c("COD_MNZ", "COD_UPL", "NOM_UPL", "cutoff"),
  Tipo_Resultado = "Todo")
# Calculamos las medias de las columnas normalizadas:
Acceso_Equipamientos_MNZ_Scn5_30min_Norm_Medias_df <- Normalizacion(
  df = Acceso_Equipamientos_MNZ_Scn5_sf |> filter(cutoff == 30) |> st_drop_geometry(),
  Columna_Interes = c("COD_MNZ", "COD_UPL", "NOM_UPL", "cutoff"),
  Tipo_Resultado = "Medias")

# Unimos los df de 15 y 30 normalizados para tenerlos en un solo lugar:
Acceso_Equipamientos_MNZ_Scn5_Norm_sf <- bind_rows(
  Acceso_Equipamientos_MNZ_Scn5_15min_Norm_df,
  Acceso_Equipamientos_MNZ_Scn5_30min_Norm_df
  ) |>
  left_join(
    Shape_Manzanas |> select(COD_UPL, COD_MNZ)
  )
Acceso_Equipamientos_MNZ_Scn5_Norm_sf

# Guardamos y recargamos el Shape de Acceso a equipamientos por manzana:
save(
  Acceso_Equipamientos_MNZ_Scn5_sf,
  Acceso_Equipamientos_MNZ_Scn5_Norm_sf,
  Acceso_Equipamientos_MNZ_Scn5_15min_Norm_Medias_df,
  Acceso_Equipamientos_MNZ_Scn5_30min_Norm_Medias_df,
  file = "./Data/2_Processing/6_Accesibility/Scn5/Acceso_Normalizado_MNZ_Scn5.RData"
)
load("./Data/2_Processing/6_Accesibility/Scn5/Acceso_Normalizado_MNZ_Scn5.RData")

# Tiempo de procesamiento:
End_Time_S6_5_P6 <- Sys.time()
print(paste("6. Tiempo de ordenado y normalización del acceso por MNZ: ", as.duration(End_Time_S6_5_P6 - Start_Time_S6_5_P6)))

# ####
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ::: 7. Ordenamos y normalizamos la información por UPL                     :::
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ####
#
Start_Time_S6_5_P7 <- Sys.time()

# Seleccionamos las UPL no rurales
Lista_No_Rural <- Shape_UPLs |> filter(ZONA_UPL != "Rural") |> as_tibble() |> select(COD_UPL)
Lista_No_Rural

# Acceso a Equipamientos por UPL:
Acceso_Equipamientos_UPL_Scn5_sf <- Acceso_Equipamientos_MNZ_Scn5_sf |>
  st_drop_geometry() |>
  left_join(
    Shape_Manzanas |> st_drop_geometry() |> select(COD_UPL, NOM_UPL, COD_MNZ)
  ) |>
  group_by(COD_UPL, NOM_UPL, cutoff) |>
  summarise(
    # 1. Usamos across() para aplicar la misma lógica a todas las columnas de categorías.
    #    Esto reemplaza el usar muchas líneas para sumar.
    across(
      Living:Community_and_Spirituality,        # Todas las columnas entre Living y Community_and_Spirituality
      ~ median(.x , na.rm = TRUE)                  # Sumamos sus valores.
    ),
    .groups = "drop"                            # Desagrupamos
  ) |>
  mutate(
    # 2. Usamos rowSums con across para sumar las columnas de forma más elegante.
    Social_Functions = rowSums(across(Living:Enjoying)) - Housing,
    Composite_Indicators = rowSums(across(Well_being:Environmental_Impact)),
    Total_Amenities = rowSums(across(Education_and_Culture:Community_and_Spirituality)) # Sumamos para saber el total de Amenities
   ) |>
  ungroup() |>
  left_join(
    Shape_UPLs |> select(COD_UPL)
   ) |>
  filter(COD_UPL %in% Lista_No_Rural$COD_UPL) |>
  st_as_sf()
Acceso_Equipamientos_UPL_Scn5_sf

# Normalizamos entre 0 y 100 todos los valores máximos de cada columna:
Acceso_Equipamientos_UPL_Scn5_15min_Norm_df <- Normalizacion(
  df = Acceso_Equipamientos_UPL_Scn5_sf |> filter(cutoff == 15) |> st_drop_geometry(),
  Columna_Interes = c("COD_UPL", "NOM_UPL", "cutoff"),
  Tipo_Resultado = "Todo")
# Calculamos las medias de las columnas normalizadas:
Acceso_Equipamientos_UPL_Scn5_15min_Norm_Medias_df <- Normalizacion(
  df = Acceso_Equipamientos_UPL_Scn5_sf |> filter(cutoff == 15) |> st_drop_geometry(),
  Columna_Interes = c("COD_UPL", "NOM_UPL", "cutoff"),
  Tipo_Resultado = "Medias")

# Normalizamos entre 0 y 100 todos los valores máximos de cada columna:
Acceso_Equipamientos_UPL_Scn5_30min_Norm_df <- Normalizacion(
  df = Acceso_Equipamientos_UPL_Scn5_sf |> filter(cutoff == 30) |> st_drop_geometry(),
  Columna_Interes = c("COD_UPL", "NOM_UPL", "cutoff"),
  Tipo_Resultado = "Todo")
# Calculamos las medias de las columnas normalizadas:
Acceso_Equipamientos_UPL_Scn5_30min_Norm_Medias_df <- Normalizacion(
  df = Acceso_Equipamientos_UPL_Scn5_sf |> filter(cutoff == 30) |> st_drop_geometry(),
  Columna_Interes = c("COD_UPL", "NOM_UPL", "cutoff"),
  Tipo_Resultado = "Medias")

# Unimos los df de 15 y 30 normalizados para tenerlos en un solo lugar:
Acceso_Equipamientos_UPL_Scn5_Norm_sf <- bind_rows(
  Acceso_Equipamientos_UPL_Scn5_15min_Norm_df,
  Acceso_Equipamientos_UPL_Scn5_30min_Norm_df
  ) |>
  left_join(
    Shape_UPLs |> select(COD_UPL, -NOM_UPL)
  ) |>
  filter(COD_UPL %in% Lista_No_Rural$COD_UPL) |>
  st_as_sf() |>
  arrange(COD_UPL)
Acceso_Equipamientos_UPL_Scn5_Norm_sf

# Guardamos y recargamos el Shape de Acceso a equipamientos por manzana:
save(
  Acceso_Equipamientos_UPL_Scn5_sf,
  Acceso_Equipamientos_UPL_Scn5_Norm_sf,
  Acceso_Equipamientos_UPL_Scn5_15min_Norm_Medias_df,
  Acceso_Equipamientos_UPL_Scn5_30min_Norm_Medias_df,
  file = "./Data/2_Processing/6_Accesibility/Scn5/Acceso_Normalizado_UPL_Scn5.RData"
)
load("./Data/2_Processing/6_Accesibility/Scn5/Acceso_Normalizado_UPL_Scn5.RData")

# Tiempo de procesamiento:
End_Time_S6_5_P7 <- Sys.time()
print(paste("7. Tiempo de ordenado y normalización del acceso por UPL: ", as.duration(End_Time_S6_5_P7 - Start_Time_S6_5_P7)))

# ####
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ::: 8. Creamos los mapas de Accesibilidad por MNZ                          :::
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ####
#
Start_Time_S6_5_P8 <- Sys.time()

# Evaluamos para 15 y 30 minutos mediante un ciclo para ahorrar trabajo:
for (Tiempo_Min in c(15, 30)) {
  
  # Alistamos el Shape a Plotear:
  Shape_Plotear_Acceso_MNZ <- Acceso_Equipamientos_MNZ_Scn5_sf |>
    filter(cutoff == Tiempo_Min) |>
    select(COD_MNZ, COD_UPL, NOM_UPL, Education_and_Culture:Community_and_Spirituality, Total_Amenities) |>
    mutate(
      Sin_Housing = Total_Amenities - Housing
    ) |>
    arrange(COD_MNZ)
  Shape_Plotear_Acceso_MNZ
  
  # Recorro las columas del 3 al 13 para plotear todo en una sola corrida
  names(Shape_Plotear_Acceso_MNZ)[15] # Total_Amenities = 13, Housing = 9, Sin husing = 15
  for(i in 4:13){ # Columnas 4:13 o 15
    Motivo <- names(Shape_Plotear_Acceso_MNZ)[i]
    Valor_Maximo <- max(Shape_Plotear_Acceso_MNZ[[Motivo]], na.rm = TRUE)
    print(paste("Haciendo mapa accesibilidad de MNZ, a ",Tiempo_Min," min para: ", Motivo, "...", sep = ""))
    
    # Hacemos el mapa:
    Mapa_Acceso_MNZ <- ggplot() +
      geom_sf(
        data = Shape_UPLs |> filter(COD_UPL %in% Lista_No_Rural$COD_UPL),# & COD_UPL == "15"),
        col = "white",
        linewidth = 0.8,
        fill = "white"
      ) +
      geom_sf(
        data = Shape_Plotear_Acceso_MNZ, # |> filter(COD_UPL == "15"),
        col = "white",
        linewidth = 0.1,
        aes(fill = .data[[Motivo]])
      ) + 
      scale_fill_gradient(
        low = "bisque3",
        high = "darkgreen",
        limits = c(0, Valor_Maximo)
      ) +
      theme_void()# +
    #theme(legend.position = "none")
    #Mapa_Acceso_MNZ
    
    # Guardamos el mapa como imagen:
    ggsave(
      filename = paste0("Mapa_Acceso_MNZ_",Tiempo_Min,"min_",Motivo,"_BOG_2160x3840.png"),
      plot = Mapa_Acceso_MNZ,
      device = NULL,
      path = "./Data/3_Results/6_Accesibility/Scn5/1_MNZ/",
      scale = 1,
      width = 2160, # Intercambiadas porque está en vertical
      height = 3840,
      units = c("px"),
      dpi = 300,
      limitsize = TRUE,
      bg = NULL,
      create.dir = FALSE,
    )
  }

}

# Para mostrar en Word:
## MostrarMapa(Shape_Plotear_Acceso_MNZ)

# Tiempo de procesamiento:
End_Time_S6_5_P8 <- Sys.time()
print(paste("8. Tiempo de creación de mapas de accesibilidad por MNZ: ", as.duration(End_Time_S6_5_P8 - Start_Time_S6_5_P8)))

# ####
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ::: 9. Creamos los mapas de Accesibilidad por UPL                          :::
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ####
#
Start_Time_S6_5_P9 <- Sys.time()

# Evaluamos para 15 y 30 minutos mediante un ciclo para ahorrar trabajo:
for (Tiempo_Min in c(15, 30)) {
  
  # Alistamos el Shape a Plotear:
  Shape_Plotear_Acceso_UPL <- Acceso_Equipamientos_UPL_Scn5_sf |>
    filter(cutoff == Tiempo_Min) |>
    select(COD_UPL, NOM_UPL, Education_and_Culture:Community_and_Spirituality, Total_Amenities) |>
    mutate(
      centroides = st_centroid(geom),
      X = st_coordinates(centroides)[,1],
      Y = st_coordinates(centroides)[,2],
      # Define la columna que se usará como etiqueta
      Etiqueta_Texto = COD_UPL,
      Sin_Housing = Total_Amenities - Housing
    ) |>
    arrange(Total_Amenities)
  Shape_Plotear_Acceso_UPL
  
  names(Shape_Plotear_Acceso_UPL)[18] # Housing = 9, Sin Housing = 18
  for(i in 3:12){ # Columnas 3:12 o 15
    Motivo <- names(Shape_Plotear_Acceso_UPL)[i]
    Valor_Maximo <- max(Shape_Plotear_Acceso_UPL[[Motivo]], na.rm = TRUE)
    print(paste("Haciendo mapa accesibilidad de UPL, a ",Tiempo_Min," min para: ", Motivo, "...", sep = ""))
    
    # Creamos el mapa correspondiente
    Mapa_Acceso_UPL <- Shape_Plotear_Acceso_UPL |>
      ggplot() +
      geom_sf(
        col = "white",
        linewidth = 0.8,
        aes(fill = .data[[Motivo]])
      ) + 
      scale_fill_gradient(
        low = "bisque3",
        high = "cyan4",
        limits = c(0, Valor_Maximo)
      ) +
      # Capa de etiquetas:
      geom_text(
        aes(x = X, y = Y, label = Etiqueta_Texto), # Mapea coordenadas y texto
        color = "white",        # Color del texto
        size = 6,               # Tamaño del texto
        family = "Roboto Black" # Tipo de fuente
      ) +
      theme_void() #+
    #theme(legend.position = "none")
    Mapa_Acceso_UPL
    
    # Guardamos el mapa como imagen:
    ggsave(
      filename = paste("Mapa_Acceso_UPL_", Tiempo_Min, "min_", Motivo, "_UPL_2160x3840.png", sep =""),
      plot = Mapa_Acceso_UPL,
      device = NULL,
      path = "./Data/3_Results/6_Accesibility/Scn5/2_UPL",
      scale = 1,
      width = 2160, # Intercambiadas porque está en vertical
      height = 3840,
      units = c("px"),
      dpi = 300,
      limitsize = TRUE,
      bg = NULL,
      create.dir = FALSE,
    )
  }
  
}

# Tiempo de procesamiento:
End_Time_S6_5_P9 <- Sys.time()
print(paste("9. Tiempo de creación de mapas de accesibilidad por UPL: ", as.duration(End_Time_S6_5_P9 - Start_Time_S6_5_P9)))

# ####
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ::: 10. Creación de mapas de Funciones Sociales UPL+MNZ                    :::
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ####
#
Start_Time_S6_5_P10 <- Sys.time()

Tiempo_Min <- 15

# Evaluamos para 15 y 30 minutos mediante un ciclo para ahorrar trabajo:
for (Tiempo_Min in c(15, 30)) {
  
  # Alistamos los Shapes a Plotear:
  Shape_Plotear_HSQL_MNZ <- Acceso_Equipamientos_MNZ_Scn5_sf |>
    filter(COD_UPL %in% Lista_No_Rural$COD_UPL) |>
    filter(cutoff == Tiempo_Min) |>
    select(COD_MNZ, COD_UPL, Living:Enjoying, HQSL = Social_Functions) |>
    arrange(COD_MNZ)
  Shape_Plotear_HSQL_MNZ
  
  Shape_Plotear_HSQL_UPL <- Acceso_Equipamientos_UPL_Scn5_sf |>
    filter(COD_UPL %in% Lista_No_Rural$COD_UPL) |>
    filter(cutoff == Tiempo_Min) |>
    select(COD_UPL, NOM_UPL, Living:Enjoying, HQSL = Social_Functions) |>
    mutate(
      centroides = st_centroid(geom),
      X = st_coordinates(centroides)[,1],
      Y = st_coordinates(centroides)[,2],
      # Define la columna que se usará como etiqueta
      Etiqueta_Texto = COD_UPL,
    ) |>
    select(-centroides) |>
    arrange(COD_UPL)
  Shape_Plotear_HSQL_UPL
  
  # Recorro las columas del 3 al 13 para plotear todo en una sola corrida
  names(Shape_Plotear_HSQL_MNZ)[3:9] # Total_Amenities = 13, Housing = 9, Sin housing = 15 
  names(Shape_Plotear_HSQL_UPL)[3:9] # Total_Amenities = 13, Housing = 9, Sin housing = 15 
  for(i in 3:9){ # Columnas 3:9
    #i <- 9 # Para testear
    Motivo <- names(Shape_Plotear_HSQL_MNZ)[i]
    Valor_Maximo_MNZ <- max(Shape_Plotear_HSQL_MNZ[[Motivo]], na.rm = TRUE)
    Valor_Maximo_UPL <- max(Shape_Plotear_HSQL_UPL[[Motivo]], na.rm = TRUE)
    Color_Mapa <- case_when(
      Motivo == "Living" ~ "deepskyblue4",
      Motivo == "Working" ~ "aquamarine4",
      Motivo == "Supplying" ~ "darkgoldenrod",
      Motivo == "Caring" ~ "deeppink3",
      Motivo == "Learning" ~ "darkcyan",
      Motivo == "Enjoying" ~ "darkred",
      Motivo == "HQSL_MNZ" ~ "darkgreen",
      TRUE ~ "darkgreen" # El equivalente a 'else' (si nada de lo anterior cumple)
    )
    # Un mensajito para saber cómo va la cosa:
    print(paste("Haciendo mapa accesibilidad de MNZ+UPL, a ",Tiempo_Min," min para: ", Motivo, "...", sep = ""))
    
    # Hacemos el mapa:
    Mapa_Acceso_MNZ_UPL <- ggplot() +
      # --- 1. Capa de fondo blanco ---
      geom_sf(
        data = Shape_UPLs |> filter(COD_UPL %in% Lista_No_Rural$COD_UPL),
        col = "white",
        linewidth = 0.8,
        fill = "white"
      ) +
      # --- 2. Capa de Manzanas (Primera escala) ---
      geom_sf(
        data = Shape_Plotear_HSQL_MNZ,
        col = "white",
        linewidth = 0.1,
        aes(fill = .data[[Motivo]])
      ) +
      scale_fill_gradient(
        low = "lightgray",
        high = Color_Mapa,
        limits = c(0, Valor_Maximo_MNZ),
        name = paste(Motivo, " MNZ | ", Tiempo_Min, "min", sep = ""),
        guide = guide_colorbar(
          direction = "horizontal",       
          title.position = "top",         # Título encima de la barra
          title.hjust = 0.5,              # Centrar el título
          barwidth = 10,                  # Ancho de la barra
          barheight = 0.5                 # Alto de la barra
        )
      ) +
      # --- Reinicio de escala de color ---
      new_scale_fill() + 
      # --- 3. Capa de UPL (Segunda escala con Transparencia) ---
      geom_sf(
        data = Shape_Plotear_HSQL_UPL,
        col = "white",
        linewidth = 0.1,
        alpha = 0.4, # Transparencia
        aes(fill = .data[[Motivo]])
      ) +
      scale_fill_gradient(
        low = "lightgray",
        high = Color_Mapa,
        limits = c(0, Valor_Maximo_UPL),
        name = paste(Motivo, " UPL | ", Tiempo_Min, "min", sep = ""),
        guide = guide_colorbar(
          direction = "horizontal",
          title.position = "top",
          title.hjust = 0.5,
          barwidth = 10,
          barheight = 0.5
        )
      ) +
      # --- 4. Capa de etiquetas ---
      geom_sf_text(data = Shape_Plotear_HSQL_UPL,
                   aes(label = COD_UPL),
                   color = "white",
                   fontface = "bold",
                   family = "Roboto Black", # Tipo de fuente
                   size = 5) +
      # Formateo de tema:
      theme_void() +
      theme(legend.position = "bottom")
    Mapa_Acceso_MNZ_UPL
    
    ?theme()
    
    # Guardamos el mapa como imagen:
    ggsave(
      filename = paste0("Mapa_SF-HQSL_MNZ-UPL_",Tiempo_Min,"min_",Motivo,"_BOG_2160x3840.png"),
      plot = Mapa_Acceso_MNZ_UPL,
      device = NULL,
      path = "./Data/3_Results/6_Accesibility/Scn5/3_MNZ_UPL/",
      scale = 1,
      width = 2160, # Intercambiadas porque está en vertical
      height = 3840,
      units = c("px"),
      dpi = 300,
      limitsize = TRUE,
      bg = NULL,
      create.dir = FALSE,
    )
  }
}

# Tiempo de procesamiento:
End_Time_S6_5_P10 <- Sys.time()
print(paste("10. Tiempo de creación de mapas de Funciones Sociales UPL+MNZ: ", as.duration(End_Time_S6_5_P10 - Start_Time_S6_5_P10)))

# ####
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ::: 11. Cálculo de ganancia de accesibilidad entre escenarios              :::
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ####
#
Start_Time_S6_5_P11 <- Sys.time()

# Cargamos datos del Escenario 1:
Acceso_a_Equipamientos_Scn1 <- as.data.frame(fread("./Data/2_Processing/6_Accesibility/Scn1/Acceso_a_Equipamientos_Scn1_2025-10-22.csv", sep=",", na = "null"))
Acceso_a_Equipamientos_Scn1

# Cálculamos la mejora de Acceso
Diferencia_Accesibilidad_Scn1_Scn5 <- Acceso_a_Equipamientos_Scn5 |>
  group_by(id, cutoff) |>
  summarise(
    Total_Amenities_Scn5 = sum(accessibility)
  ) |>
  full_join(
    Acceso_a_Equipamientos_Scn1 |>
      group_by(id, cutoff) |>
      summarise(
        Total_Amenities_Scn1 = sum(accessibility)
      )
  ) |>
  mutate(
    Diferencia_Scn5_Scn1 = Total_Amenities_Scn5 - Total_Amenities_Scn1
  ) |>
  ungroup() |>
  select(COD_MNZ = id, cutoff, Total_Amenities_Scn1, Total_Amenities_Scn5, Diferencia_Scn5_Scn1)
Diferencia_Accesibilidad_Scn1_Scn5

# Evaluamos para 15 y 30 minutos mediante un ciclo para ahorrar trabajo:
for (Tiempo_Min in c(15, 30)) { #15 y 30
  
  # Manzanas con diferencias:
  Manzanas_Nueva_Accesibilidad_Scn5 <- Diferencia_Accesibilidad_Scn1_Scn5 |>
    filter(cutoff == Tiempo_Min & Diferencia_Scn5_Scn1 > 0)
  Manzanas_Nueva_Accesibilidad_Scn5
  
  # Número de manzanas con Nueva Accesibilidad
  nrow(Manzanas_Nueva_Accesibilidad_Scn5)
  
  # Número total de equipamientos con nueva accesibilidad:
  sum(Manzanas_Nueva_Accesibilidad_Scn5$Diferencia_Scn5_Scn1)
  
  # Acceso medio incrementado por manzana
  Ganancia_Media_Accesibilidad_Scn5 = floor(sum(Manzanas_Nueva_Accesibilidad_Scn5$Diferencia_Scn5_Scn1)/nrow(Manzanas_Nueva_Accesibilidad_Scn5))
  Ganancia_Media_Accesibilidad_Scn5
  
  cat("\nCon la implementación del sistema de transporte masivo férreo:\n",
      nrow(Manzanas_Nueva_Accesibilidad_Scn5), "manzanas han incrementado su accesibilidad en una media de\n",
      Ganancia_Media_Accesibilidad_Scn5, "equipamientos por manzana, para viajes de", Tiempo_Min, "minutos.")
  
  # --- Agruparemos por deciles los nuevos datos de Accesibilidad ---
  
  # 1. Definimos los puntos de corte para deciles (10 grupos)
  #    La secuencia ahora va de 0 a 1 en pasos de 0.1
  breaks_deciles <- quantile(Manzanas_Nueva_Accesibilidad_Scn5$Diferencia_Scn5_Scn1, probs = seq(0, 1, by = 0.1), na.rm = TRUE)
  breaks_deciles
  
  # 2. Creamos las etiquetas para los 10 grupos.
  labels_deciles <- c("1    (Más Baja)", "2", "3", "4", "5", "6", "7", "8", "9", "10    (Más Alta)")
  labels_deciles
  
  # 3. Usarmos cut() para crear la nueva columna.
  datos_bogota_deciles <- Manzanas_Nueva_Accesibilidad_Scn5 %>%
    select(COD_MNZ, Diferencia_Scn5_Scn1) |>
    mutate(
      accesibilidad_decil = cut(
        Diferencia_Scn5_Scn1,
        breaks = breaks_deciles,
        include.lowest = TRUE,
        labels = labels_deciles
      )
    )
  datos_bogota_deciles
  
  # Creamos el shape para plotear las diferencias
  Shape_Plotear_Acceso_Scn5_Scn1 <- datos_bogota_deciles |>
    left_join(Shape_Manzanas) |>
    filter(COD_UPL %in% Lista_No_Rural$COD_UPL) |>
    st_as_sf()
  Shape_Plotear_Acceso_Scn5_Scn1
  
  # --- Hacemos el mapa ---
  
  # 1. Definimos los colores de inicio y fin que interesan
  color_inicio <- "bisque3"
  color_fin <- "darkgreen"
  
  # 2. Creamos la función que generará el gradiente entre esos dos colores
  #    El argumento es un vector de los colores que formarán los "extremos" del gradiente
  generador_gradiente <- colorRampPalette(c(color_inicio, color_fin))
  
  # 3. Usamos la nueva función para generar un número específico de colores.
  #    Por ejemplo, vamos a generar 10 colores para 10 deciles.
  numero_de_colores <- 10
  mi_paleta_gradiente <- generador_gradiente(numero_de_colores)
  
  # Ploteamos
  Mapa_Acceso_Scn5_Scn1 <- ggplot() +
    # Toca poner un fondo blanco, y para eso usamos las UPLs:
    geom_sf(
      data = Shape_UPLs |> filter(COD_UPL %in% Lista_No_Rural$COD_UPL),# & COD_UPL == "15"),
      col = "gray",
      linewidth = 0.8,
      fill = "white"
    ) +
    # Shape Manzanas vacías de fondo:
    geom_sf(
      data = Shape_Manzanas |> filter(COD_UPL %in% Lista_No_Rural$COD_UPL), # |> filter(COD_UPL == "15"),
      col = "darkgray",
      linewidth = 0.1,
      fill = "white"
      #aes(fill = Diferencia_Scn5_Scn1)
    ) + 
    # La capa de interés:
    geom_sf(
      data = Shape_Plotear_Acceso_Scn5_Scn1, # |> filter(COD_UPL == "15"),
      col = "white",
      linewidth = 0.1,
      aes(fill = accesibilidad_decil)
    ) + 
    scale_fill_manual(
      name = " ", # Accesibilidad (Decil)
      values = mi_paleta_gradiente 
    ) +
    theme_void() +
    theme(legend.position = "none")
  #Mapa_Acceso_Scn5_Scn1
  
  Nombre_Archivo <- paste0("Mapa_Diferencia_Acceso_MNZ_Scn5_Scn1_",Tiempo_Min,"min_BOG_2160x3840.png")
  Ruta_Archivo <- "./Data/3_Results/7_HQSL/2_Diferencia_Escenarios/"
  
  # Guardamos el mapa como imagen:
  ggsave(
    filename = Nombre_Archivo,
    plot = Mapa_Acceso_Scn5_Scn1,
    device = NULL,
    path = Ruta_Archivo,
    scale = 1,
    width = 2160, # Intercambiadas porque está en vertical
    height = 3840,
    units = c("px"),
    dpi = 300,
    limitsize = TRUE,
    bg = NULL,
    create.dir = FALSE,
  )
  
  # Ajustar imagen:
  image_write(image_trim(image_read(paste0(Ruta_Archivo, Nombre_Archivo))), path = paste0(Ruta_Archivo, Nombre_Archivo), format = "png")
  
}

# Tiempo de procesamiento:
End_Time_S6_5_P11 <- Sys.time()
print(paste("11. Tiempo de diferencia de accesibilidad entre escenarios: ", as.duration(End_Time_S6_5_P11 - Start_Time_S6_5_P11)))

# ####
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ::: 12. Carga de datos del Script y detención de r5r y Java                :::
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ####
#
Start_Time_S6_5_P12 <- Sys.time()

# --- ¿Necesita recargar los datos producto de este Script? ---

# Recarga de la Isocrona de Ejemplo:
Shape_Isocrona_Ejemplo_Scn5 <- st_read("./Data/2_Processing/6_Accesibility/Scn5/Shape_Isocrona_Ejemplo_Scn5.gpkg")

# Recarga del análisis de accesibilidad (el más importante):
Acceso_a_Equipamientos_Scn5 <- as.data.frame(fread("./Data/2_Processing/6_Accesibility/Scn5/Acceso_a_Equipamientos_Scn5_2025-10-22.csv", sep=",", na = "null"))

# Recarga de datos relacionados con la accesibilidad de las manzanas:
load("./Data/2_Processing/6_Accesibility/Scn5/Acceso_Normalizado_MNZ_Scn5.RData")

# Recarga de datos relacionados con la accesibilidad de las UPL:
load("./Data/2_Processing/6_Accesibility/Scn5/Acceso_Normalizado_UPL_Scn5.RData")

# --- Limpiamos después de usar r5r_core ---

# r5r_core_scenario_5$getNumberOfThreads()
r5r::stop_r5(r5r_core_scenario_5)
rJava::.jgc(R.gc = TRUE)

# Tiempo de procesamiento:
End_Time_S6_5_P12 <- Sys.time()
print(paste("12. Tiempo de detención de r5r y Java: ", as.duration(End_Time_S6_5_P12 - Start_Time_S6_5_P12)))

# ####
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ::: 13. Tiempo de procesamiento del script                                 :::
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ####
#
End_Time_S6_5_P0 <- Sys.time()
print(paste("1. Tiempo de inicialización: ", as.duration(End_Time_S6_5_P1 - Start_Time_S6_5_P1)))
print(paste("2. Tiempo de carga de datos de origen: ", as.duration(End_Time_S6_5_P2 - Start_Time_S6_5_P2)))
print(paste("3. Tiempo de cálculo de Isocronas: ", as.duration(End_Time_S6_5_P3 - Start_Time_S6_5_P3)))
print(paste("4. Tiempo de cálculo de itinerarios de testeo: ", as.duration(End_Time_S6_5_P4 - Start_Time_S6_5_P4)))
print(paste("5. Tiempo de cálculo de accesibilidad: ", as.duration(End_Time_S6_5_P5 - Start_Time_S6_5_P5)))
print(paste("6. Tiempo de ordenado y normalización del acceso por MNZ: ", as.duration(End_Time_S6_5_P6 - Start_Time_S6_5_P6)))
print(paste("7. Tiempo de ordenado y normalización del acceso por UPL: ", as.duration(End_Time_S6_5_P7 - Start_Time_S6_5_P7)))
print(paste("8. Tiempo de creación de mapas de accesibilidad por MNZ: ", as.duration(End_Time_S6_5_P8 - Start_Time_S6_5_P8)))
print(paste("9. Tiempo de creación de mapas de accesibilidad por UPL: ", as.duration(End_Time_S6_5_P9 - Start_Time_S6_5_P9)))
print(paste("10. Tiempo de creación de mapas de Funciones Sociales UPL+MNZ: ", as.duration(End_Time_S6_5_P10 - Start_Time_S6_5_P10)))
print(paste("11. Tiempo de cálculo de diferencia de accesibilidad entre escenarios: ", as.duration(End_Time_S6_5_P11 - Start_Time_S6_5_P11)))
print(paste("12. Tiempo de detención de r5r y Java: ", as.duration(End_Time_S6_5_P12 - Start_Time_S6_5_P12)))
print(paste("13. Tiempo de procesamiento del script: ", as.duration(End_Time_S6_5_P0 - Start_Time_S6_5_P0)))
Avisar()

