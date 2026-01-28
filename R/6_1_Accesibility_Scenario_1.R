# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# :::                                                                        :::
# :::                        SCRIPT DE ACCESIBILIDAD                         :::
# :::                        Scenario 1 - Actualidad                         :::
# :::                                                                        :::
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#
# Nombre: 6_1_Accesibility_Scenario_1.R
#
# Descripcion: Este Script usará la librería r5r para los cálculos de isocronas
# y de accesibilidad al inventario de equipamientos de la ciudad.
#
# Es necesario contar con los shapes de origen y destino para la correcta ejecución
# de este script.

# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# :::                      Contenido de este Script                          :::
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#####
#
# 1. Inicialización
# 2. Carga de datos de origen
# 3.  Cálculo de Isocronas de muestra
# 4. Cálculo de accesibilidad usando R5R
# 5. Ordenamos y normalizamos la información por MNZ
# 6. Ordenamos y normalizamos la información por UPL
# 7. Creamos los mapas de Accesibilidad por MNZ
# 8. Creamos los mapas de Accesibilidad por UPL
# 9. Creación de mapas de Funciones Sociales UPL+MNZ
# 10. Carga de datos del Script y detención de r5r y Java
# 11. Tiempo de procesamiento del script

Start_Time_S6_1_P0 <- Sys.time()

# ####
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ::: 1. Inicialización                                                      :::
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ####
#
Start_Time_S6_1_P1 <- Sys.time()

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
data_path_scenario_1 <- "./Data/1_Sources/6_Accesibility/Scn1/r5r/"
list.files(data_path_scenario_1)

# Revisamos los archivos dentro de la carpeta:
list.files(data_path_scenario_1)
pbf_file <- list.files(data_path_scenario_1, pattern = ".pbf")[1]
gtfs_file <- list.files(data_path_scenario_1, pattern = "zip")
pbf_file
gtfs_file

# Cargamos el motor de r5r
r5r_core_scenario_1 <- setup_r5(
  data_path = data_path_scenario_1
)

#r5r_core_scenario_1 <- build_network(data_path = "./Data/1_Sources/6_Accesibility/Scn1/r5rx/", verbose = FALSE)

# Confirmamos datos de Java:
runtime <- rJava::.jcall("java/lang/Runtime", "Ljava/lang/Runtime;", "getRuntime")
maxMemory <- rJava::.jcall(runtime, "J", "maxMemory") / (1024^2)
totalMemory <- rJava::.jcall(runtime, "J", "totalMemory") / (1024^2)
freeMemory <- rJava::.jcall(runtime, "J", "freeMemory") / (1024^2)
#cat("Cores usados para r5r_core_Scn1:",r5r_core_scenario_1$getNumberOfThreads(), "cores.\n")
cat("Máxima memoria: ", maxMemory, "MB\n")
cat("Memoria total asignada: ", totalMemory, "MB\n")
cat("Memoria libre: ", freeMemory, "MB\n")

# Tiempo de procesamiento:
End_Time_S6_1_P1 <- Sys.time()
print(paste("1. Tiempo de inicialización: ", as.duration(End_Time_S6_1_P1 - Start_Time_S6_1_P1)))

# ####
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ::: 2. Carga de datos de origen                                            :::
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ####
#
Start_Time_S6_1_P2 <- Sys.time()

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
End_Time_S6_1_P2 <- Sys.time()
print(paste("2. Tiempo de carga de datos de origen: ", as.duration(End_Time_S6_1_P2 - Start_Time_S6_1_P2)))

# ####
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ::: 3.  Cálculo de Isocronas de muestra                                    :::
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ####
#
Start_Time_S6_1_P3 <- Sys.time()

# Definimos una hora de partida
Tiempo_de_Partida # Viene de los datos calibrados

# Establecemos el origen de la Isocrona de ejemplo (Arbitrariamente)
Origen_Isocrona <- Shape_UPLs |>
  filter(COD_UPL == "15") |> # 15 - 23
  mutate(
    id = NOM_UPL # Es necesario que una variable se llame "id"
  ) |>
  select(id, NOM_UPL) |>
  st_transform(crs = 4326) |> # Si no se cambia la proyección, r5r no funcionará
  st_centroid()
Origen_Isocrona

# Calculamos la isocrona de Ejemplo usando r5r
Shape_Isocrona_Ejemplo_Scn1 <- isochrone(
  r5r_network = r5r_core_scenario_1,
  origins = Origen_Isocrona,
  departure_datetime = Tiempo_de_Partida, # Se había evaluado a las 08:00
  mode = "TRANSIT", # WALK, TRANSIT, BICYCLE
  cutoffs = c(15, 30, 45, 60), #c(15, 30)
  polygon_output = TRUE,
  walk_speed = Optimo_Walk_Speed,
  max_walk_time = Optimo_Max_Walk_Time,
  sample_size = 1
)
Shape_Isocrona_Ejemplo_Scn1
#st_area(Shape_Isocrona_Ejemplo_Scn1)

# Mostramos las Isocronas
tm_basemap("OpenStreetMap") +
  tm_shape(Shape_UPLs) +
  tm_polygons(fill = "white", fill_alpha = 0.001,  col_alpha = 0.25, col = "darkblue") +
  tm_shape(st_make_valid(Shape_Isocrona_Ejemplo_Scn1)) +
  tm_polygons(fill = "darkred", fill_alpha = 0.25) +
  tm_shape(Origen_Isocrona) +
  tm_dots(fill = "darkgreen", size = 1)

# Guardamos y recargamos la Isocrona de Ejemplo:
# st_write(Shape_Isocrona_Ejemplo_Scn1, dsn = "./Data/2_Processing/6_Accesibility/Scn1/Shape_Isocrona_Ejemplo_Scn1.gpkg", driver = 'GPKG', append = FALSE)
Shape_Isocrona_Ejemplo_Scn1 <- st_read("./Data/2_Processing/6_Accesibility/Scn1/Shape_Isocrona_Ejemplo_Scn1.gpkg")
Shape_Isocrona_Ejemplo_Scn1

# Tiempo de procesamiento:
End_Time_S6_1_P3 <- Sys.time()
print(paste("3. Tiempo de cálculo de Isocronas: ", as.duration(End_Time_S6_1_P3 - Start_Time_S6_1_P3)))

# ####
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ::: 4. Cálculo de accesibilidad usando R5R                               :::
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ####
#
Start_Time_S6_1_P4 <- Sys.time()

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
Amenities_Puntos_Scn1_sf # Destinos
Categorias_Equipamientos <- names(Amenities_Puntos_Scn1_sf)[12:29] # Columnas a tener en cuenta para la Accesibilidad
Categorias_Equipamientos
Destinos_Accesibilidad_City_Scn1 <- Amenities_Puntos_Scn1_sf |>
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
Destinos_Accesibilidad_City_Scn1 # 21714 / 385355 (18x)

# Calculamos la accesibilidad.
# --- ¡ATENCIÓN! ¡Esto tarda 6,39 horas en procesarse! ---
# Acceso_a_Equipamientos_Scn1 <- accessibility(
#   r5r_core = r5r_core_scenario_1,
#   origins = Origenes_Accesibilidad_City,
#   destinations = Destinos_Accesibilidad_City_Scn1,
#   opportunities_colnames = Categorias_Equipamientos,
#   mode = c("WALK", "TRANSIT"),
#   departure_datetime = Tiempo_de_Partida,
#   walk_speed = Optimo_Walk_Speed,
#   max_walk_time = Optimo_Max_Walk_Time,
#   decay_function = "step",
#   cutoffs = c(15, 30)
# )
head(Acceso_a_Equipamientos_Scn1)
Acceso_a_Equipamientos_Scn1 # 2556 / 1559916
length(Acceso_a_Equipamientos_Scn1$id) # 1559916

# Centroides x Variables x Intervalos (15 y 30)
# 43331 x 18 x 2 = 1559916 (86662)

# Guardo y cargo la tabla de acceso a equipamientos:
#fwrite(Acceso_a_Equipamientos_Scn1, "./Data/2_Processing/6_Accesibility/Scn1/Acceso_a_Equipamientos_Scn1_2025-10-22.csv", row.names = FALSE)
Acceso_a_Equipamientos_Scn1 <- as.data.frame(fread("./Data/2_Processing/6_Accesibility/Scn1/Acceso_a_Equipamientos_Scn1_2025-10-22.csv", sep=",", na = "null"))
Acceso_a_Equipamientos_Scn1

length(Acceso_a_Equipamientos_Scn1$id)
unique(Acceso_a_Equipamientos_Scn1$opportunity)

# Tiempo de procesamiento:
End_Time_S6_1_P4 <- Sys.time()
print(paste("4. Tiempo de cálculo de accesibilidad: ", as.duration(End_Time_S6_1_P4 - Start_Time_S6_1_P4)))
Avisar()

# ####
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ::: 5. Ordenamos y normalizamos la información por MNZ                     :::
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ####
#
# Hacemos un pivot wider, para dejar las columnas como venimos haciéndolo:
#
Start_Time_S6_1_P5 <- Sys.time()

# Seleccionamos las UPL no rurales
Lista_No_Rural <- Shape_UPLs |> filter(ZONA_UPL != "Rural") |> as_tibble() |> select(COD_UPL)
Lista_No_Rural

# Hacemos un pivot Wider para que nos quede el df por Manzanas:
Acceso_Equipamientos_MNZ_Scn1_sf <- Acceso_a_Equipamientos_Scn1 |>
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
Acceso_Equipamientos_MNZ_Scn1_sf # 43331 x 18 x 2 => 1559916/18 = 86662) <- 43331 (MNZ) x 2.

# Normalizamos entre 0 y 100 todos los valores máximos de cada columna:
Acceso_Equipamientos_MNZ_Scn1_15min_Norm_df <- Normalizacion(
  df = Acceso_Equipamientos_MNZ_Scn1_sf |> filter(cutoff == 15) |> st_drop_geometry(),
  Columna_Interes = c("COD_MNZ", "COD_UPL", "NOM_UPL", "cutoff"),
  Tipo_Resultado = "Todo")
# Calculamos las medias de las columnas normalizadas:
Acceso_Equipamientos_MNZ_Scn1_15min_Norm_Medias_df <- Normalizacion(
  df = Acceso_Equipamientos_MNZ_Scn1_sf |> filter(cutoff == 15) |> st_drop_geometry(),
  Columna_Interes = c("COD_MNZ", "COD_UPL", "NOM_UPL", "cutoff"),
  Tipo_Resultado = "Medias")

# Normalizamos entre 0 y 100 todos los valores máximos de cada columna:
Acceso_Equipamientos_MNZ_Scn1_30min_Norm_df <- Normalizacion(
  df = Acceso_Equipamientos_MNZ_Scn1_sf |> filter(cutoff == 30) |> st_drop_geometry(),
  Columna_Interes = c("COD_MNZ", "COD_UPL", "NOM_UPL", "cutoff"),
  Tipo_Resultado = "Todo")
# Calculamos las medias de las columnas normalizadas:
Acceso_Equipamientos_MNZ_Scn1_30min_Norm_Medias_df <- Normalizacion(
  df = Acceso_Equipamientos_MNZ_Scn1_sf |> filter(cutoff == 30) |> st_drop_geometry(),
  Columna_Interes = c("COD_MNZ", "COD_UPL", "NOM_UPL", "cutoff"),
  Tipo_Resultado = "Medias")

# Unimos los df de 15 y 30 normalizados para tenerlos en un solo lugar:
Acceso_Equipamientos_MNZ_Scn1_Norm_sf <- bind_rows(
  Acceso_Equipamientos_MNZ_Scn1_15min_Norm_df,
  Acceso_Equipamientos_MNZ_Scn1_30min_Norm_df
  ) |>
  left_join(
    Shape_Manzanas |> select(COD_UPL, COD_MNZ)
  )
Acceso_Equipamientos_MNZ_Scn1_Norm_sf

# Guardamos y recargamos el Shape de Acceso a equipamientos por manzana:
save(
  Acceso_Equipamientos_MNZ_Scn1_sf,
  Acceso_Equipamientos_MNZ_Scn1_Norm_sf,
  Acceso_Equipamientos_MNZ_Scn1_15min_Norm_Medias_df,
  Acceso_Equipamientos_MNZ_Scn1_30min_Norm_Medias_df,
  file = "./Data/2_Processing/6_Accesibility/Scn1/Acceso_Normalizado_MNZ_Scn1.RData"
)
load("./Data/2_Processing/6_Accesibility/Scn1/Acceso_Normalizado_MNZ_Scn1.RData")

#MostrarMapa(Acceso_Equipamientos_MNZ_Scn1_sf)

# Tiempo de procesamiento:
End_Time_S6_1_P5 <- Sys.time()
print(paste("5. Tiempo de ordenado y normalización del acceso por MNZ: ", as.duration(End_Time_S6_1_P5 - Start_Time_S6_1_P5)))

# ####
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ::: 6. Ordenamos y normalizamos la información por UPL                     :::
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ####
#
Start_Time_S6_1_P6 <- Sys.time()

# Seleccionamos las UPL no rurales
Lista_No_Rural <- Shape_UPLs |> filter(ZONA_UPL != "Rural") |> as_tibble() |> select(COD_UPL)
Lista_No_Rural

# Acceso a Equipamientos por UPL:
Acceso_Equipamientos_UPL_Scn1_sf <- Acceso_Equipamientos_MNZ_Scn1_sf |>
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
      ~ median(.x , na.rm = TRUE)                 # Sumamos sus valores y los dividimos por el número de manzanas para tener una media.
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
Acceso_Equipamientos_UPL_Scn1_sf

# Normalizamos entre 0 y 100 todos los valores máximos de cada columna:
Acceso_Equipamientos_UPL_Scn1_15min_Norm_df <- Normalizacion(
  df = Acceso_Equipamientos_UPL_Scn1_sf |> filter(cutoff == 15) |> st_drop_geometry(),
  Columna_Interes = c("COD_UPL", "NOM_UPL", "cutoff"),
  Tipo_Resultado = "Todo")
# Calculamos las medias de las columnas normalizadas:
Acceso_Equipamientos_UPL_Scn1_15min_Norm_Medias_df <- Normalizacion(
  df = Acceso_Equipamientos_UPL_Scn1_sf |> filter(cutoff == 15) |> st_drop_geometry(),
  Columna_Interes = c("COD_UPL", "NOM_UPL", "cutoff"),
  Tipo_Resultado = "Medias")

# Normalizamos entre 0 y 100 todos los valores máximos de cada columna:
Acceso_Equipamientos_UPL_Scn1_30min_Norm_df <- Normalizacion(
  df = Acceso_Equipamientos_UPL_Scn1_sf |> filter(cutoff == 30) |> st_drop_geometry(),
  Columna_Interes = c("COD_UPL", "NOM_UPL", "cutoff"),
  Tipo_Resultado = "Todo")
# Calculamos las medias de las columnas normalizadas:
Acceso_Equipamientos_UPL_Scn1_30min_Norm_Medias_df <- Normalizacion(
  df = Acceso_Equipamientos_UPL_Scn1_sf |> filter(cutoff == 30) |> st_drop_geometry(),
  Columna_Interes = c("COD_UPL", "NOM_UPL", "cutoff"),
  Tipo_Resultado = "Medias")

# Unimos los df de 15 y 30 normalizados para tenerlos en un solo lugar:
Acceso_Equipamientos_UPL_Scn1_Norm_sf <- bind_rows(
  Acceso_Equipamientos_UPL_Scn1_15min_Norm_df,
  Acceso_Equipamientos_UPL_Scn1_30min_Norm_df
  ) |>
  left_join(
    Shape_UPLs |> select(COD_UPL, -NOM_UPL)
  ) |>
  filter(COD_UPL %in% Lista_No_Rural$COD_UPL) |>
  st_as_sf() |>
  arrange(COD_UPL)
Acceso_Equipamientos_UPL_Scn1_Norm_sf

# Guardamos y recargamos el Shape de Acceso a equipamientos por manzana:
save(
  Acceso_Equipamientos_UPL_Scn1_sf,
  Acceso_Equipamientos_UPL_Scn1_Norm_sf,
  Acceso_Equipamientos_UPL_Scn1_15min_Norm_Medias_df,
  Acceso_Equipamientos_UPL_Scn1_30min_Norm_Medias_df,
  file = "./Data/2_Processing/6_Accesibility/Scn1/Acceso_Normalizado_UPL_Scn1.RData"
)
load("./Data/2_Processing/6_Accesibility/Scn1/Acceso_Normalizado_UPL_Scn1.RData")

# Tiempo de procesamiento:
End_Time_S6_1_P6 <- Sys.time()
print(paste("6. Tiempo de ordenado y normalización del acceso por UPL: ", as.duration(End_Time_S6_1_P6 - Start_Time_S6_1_P6)))

# ####
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ::: 7. Creamos los mapas de Accesibilidad por MNZ                          :::
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ####
#
Start_Time_S6_1_P7 <- Sys.time()

# Evaluamos para 15 y 30 minutos mediante un ciclo para ahorrar trabajo:
for (Tiempo_Min in c(15, 30)) {
  
  # Alistamos el Shape a Plotear:
  Shape_Plotear_Acceso_MNZ <- Acceso_Equipamientos_MNZ_Scn1_sf |>
    filter(cutoff == Tiempo_Min) |>
    select(COD_MNZ, COD_UPL, NOM_UPL, Education_and_Culture:Community_and_Spirituality, Total_Amenities) |>
    mutate(
      Sin_Housing = Total_Amenities - Housing
    ) |>
    arrange(COD_MNZ)
  Shape_Plotear_Acceso_MNZ
  
  # Recorro las columas del 3 al 13 para plotear todo en una sola corrida
  names(Shape_Plotear_Acceso_MNZ)[13] # Total_Amenities = 13, Housing = 9, Sin housing = 15 
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
      theme_void() #+
    #theme(legend.position = "none")
    #Mapa_Acceso_MNZ
    
    # Guardamos el mapa como imagen:
    ggsave(
      filename = paste0("Mapa_Acceso_MNZ_",Tiempo_Min,"min_",Motivo,"_BOG_2160x3840.png"),
      plot = Mapa_Acceso_MNZ,
      device = NULL,
      path = "./Data/3_Results/6_Accesibility/Scn1/1_MNZ/",
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
  
  # Para mostrar en Word:
  ## MostrarMapa(Shape_Plotear_Acceso_MNZ)
}

# Tiempo de procesamiento:
End_Time_S6_1_P7 <- Sys.time()
print(paste("7. Tiempo de creación de mapas de accesibilidad por MNZ: ", as.duration(End_Time_S6_1_P7 - Start_Time_S6_1_P7)))

# ####
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ::: 8. Creamos los mapas de Accesibilidad por UPL                          :::
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ####
#
Start_Time_S6_1_P8 <- Sys.time()

# Evaluamos para 15 y 30 minutos mediante un ciclo para ahorrar trabajo:
for (Tiempo_Min in c(15, 30)) {
  
  #Tiempo_Min <- 15
  
  # Alistamos el Shape a Plotear:
  Shape_Plotear_Acceso_UPL <- Acceso_Equipamientos_UPL_Scn1_sf |>
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
      path = "./Data/3_Results/6_Accesibility/Scn1/2_UPL",
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
End_Time_S6_1_P8 <- Sys.time()
print(paste("8. Tiempo de creación de mapas de accesibilidad por UPL: ", as.duration(End_Time_S6_1_P8 - Start_Time_S6_1_P8)))

# ####
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ::: 9. Creación de mapas de Funciones Sociales UPL+MNZ                     :::
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ####
#
Start_Time_S6_1_P9 <- Sys.time()

Tiempo_Min <- 15

# Evaluamos para 15 y 30 minutos mediante un ciclo para ahorrar trabajo:
for (Tiempo_Min in c(15, 30)) {
  
  # Alistamos los Shapes a Plotear:
  Shape_Plotear_HSQL_MNZ <- Acceso_Equipamientos_MNZ_Scn1_sf |>
    filter(COD_UPL %in% Lista_No_Rural$COD_UPL) |>
    filter(cutoff == Tiempo_Min) |>
    select(COD_MNZ, COD_UPL, Living:Enjoying, HQSL = Social_Functions) |>
    arrange(COD_MNZ)
  Shape_Plotear_HSQL_MNZ
  
  Shape_Plotear_HSQL_UPL <- Acceso_Equipamientos_UPL_Scn1_sf |>
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

    # Guardamos el mapa como imagen:
    ggsave(
      filename = paste0("Mapa_SF-HQSL_MNZ-UPL_",Tiempo_Min,"min_",Motivo,"_BOG_2160x3840.png"),
      plot = Mapa_Acceso_MNZ_UPL,
      device = NULL,
      path = "./Data/3_Results/6_Accesibility/Scn1/3_MNZ_UPL/",
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
End_Time_S6_1_P9 <- Sys.time()
print(paste("9. Tiempo de creación de mapas de Funciones Sociales UPL+MNZ: ", as.duration(End_Time_S6_1_P9 - Start_Time_S6_1_P9)))

# ####
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ::: 10. Carga de datos del Script y detención de r5r y Java                 :::
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ####
#
Start_Time_S6_1_P10 <- Sys.time()

# --- ¿Necesita recargar los datos producto de este Script? ---

# Recarga de la Isocrona de Ejemplo:
Shape_Isocrona_Ejemplo_Scn1 <- st_read("./Data/2_Processing/6_Accesibility/Scn1/Shape_Isocrona_Ejemplo_Scn1.gpkg")

# Recarga del análisis de accesibilidad (el más importante):
Acceso_a_Equipamientos_Scn1 <- as.data.frame(fread("./Data/2_Processing/6_Accesibility/Scn1/Acceso_a_Equipamientos_Scn1_2025-10-22.csv", sep=",", na = "null"))

# Recarga de datos relacionados con la accesibilidad de las manzanas:
load("./Data/2_Processing/6_Accesibility/Scn1/Acceso_Normalizado_MNZ_Scn1.RData")

# Recarga de datos relacionados con la accesibilidad de las UPL:
load("./Data/2_Processing/6_Accesibility/Scn1/Acceso_Normalizado_UPL_Scn1.RData")

# --- Limpiamos después de usar r5r_core ---

r5r_core_scenario_1$getNumberOfThreads()
r5r::stop_r5(r5r_core_scenario_1)
rJava::.jgc(R.gc = TRUE)

# Tiempo de procesamiento:
End_Time_S6_1_P10 <- Sys.time()
print(paste("10. Tiempo de detención de r5r y Java: ", as.duration(End_Time_S6_1_P10 - Start_Time_S6_1_P10)))

# ####
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ::: 11. Tiempo de procesamiento del script                                 :::
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ####
#
End_Time_S6_1_P0 <- Sys.time()
print(paste("1. Tiempo de inicialización: ", as.duration(End_Time_S6_1_P1 - Start_Time_S6_1_P1)))
print(paste("2. Tiempo de carga de datos de origen: ", as.duration(End_Time_S6_1_P2 - Start_Time_S6_1_P2)))
print(paste("3. Tiempo de cálculo de Isocronas: ", as.duration(End_Time_S6_1_P3 - Start_Time_S6_1_P3)))
print(paste("4. Tiempo de cálculo de accesibilidad: ", as.duration(End_Time_S6_1_P4 - Start_Time_S6_1_P4)))
print(paste("5. Tiempo de ordenado y normalización del acceso por MNZ: ", as.duration(End_Time_S6_1_P5 - Start_Time_S6_1_P5)))
print(paste("6. Tiempo de ordenado y normalización del acceso por UPL: ", as.duration(End_Time_S6_1_P6 - Start_Time_S6_1_P6)))
print(paste("7. Tiempo de creación de mapas de accesibilidad por MNZ: ", as.duration(End_Time_S6_1_P7 - Start_Time_S6_1_P7)))
print(paste("8. Tiempo de creación de mapas de accesibilidad por UPL: ", as.duration(End_Time_S6_1_P8 - Start_Time_S6_1_P8)))
print(paste("9. Tiempo de creación de mapas de Funciones Sociales UPL+MNZ: ", as.duration(End_Time_S6_1_P9 - Start_Time_S6_1_P9)))
print(paste("10. Tiempo de detención de r5r y Java: ", as.duration(End_Time_S6_1_P10 - Start_Time_S6_1_P10)))
print(paste("11. Tiempo de procesamiento del script:: ", as.duration(End_Time_S6_1_P0 - Start_Time_S6_1_P0)))
Avisar()


