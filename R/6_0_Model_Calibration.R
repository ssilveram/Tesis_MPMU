# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# :::                                                                        :::
# :::                  SCRIPT DE CALIBRACION DE MODELO                       :::
# :::                                                                        :::
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ####
# Nombre: 6_0_Model_Calibration.R
#
# Descripcion: Este Script se usará para la calibración del modelo mediante
# la librería r5r, para posteriormente hacer los cálculos de isocronas  y de
# accesibilidad al inventario de equipamientos de la ciudad.
#
# Es necesario contar el dataframe de viajes de la encuesta de Movilidad 2023
# para poder ejecutar satisfactoriamente este script.
#
# ####
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# :::                      Contenido de este Script                          :::
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ####
#
# 1. Inicialización
# 2. Carga de datos de origen
# 3. Preparación de los sf y los df necesarios para calibrar
# 4. Definimos la función de la Raíz del Error Cuadrático Medio (RMSE)
# 5. Corremos el modelo usando Algoritmos Genéticos
# 6. Testeamos, calculamos y graficamos los errores
# 7. Guardado de variables importantes
# 8. Tiempos de procesamiento
#
Start_Time_S6_0_P0 <- Sys.time()

# ####
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ::: 1. Inicialización                                                      :::
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ####
#
Start_Time_S6_0_P1 <- Sys.time()

# Cargamos el script de inicialización general
source("0_Initialization.R")

# Verificamos la versión de Java instalada, si es que hay alguna.
rJavaEnv::java_check_version_rjava()

# Si no está instalada, es necesario instalarla con el siguiente código:
# rJavaEnv::rje_consent(provided = TRUE)
# rJavaEnv::java_quick_install(version = 21) # install Java 21

# Verificamos que haya quedado bien instalada la versión 21 de Java
rJavaEnv::java_check_version_rjava()

# Establecemos el límite de RAM y Cores del CPU a usar por Java:
options(java.parameters = c("-Xmx8G", "-XX:ActiveProcessorCount=7"))

# Definimos la ruta donde se guardan los archivos del motor de r5r:
data_path_scenario_0 <- "./Data/1_Sources/6_Accesibility/Scn0/r5r/"

list.files(data_path_scenario_0)
pbf_file <- list.files(data_path_scenario_0, pattern = ".pbf")[1]
gtfs_file <- list.files(data_path_scenario_0, pattern = "zip")
pbf_file
gtfs_file

# Cargamos el motor de r5r
r5r_core_calibration <- setup_r5(
  data_path = data_path_scenario_0
)

# Confirmamos datos de Java:
runtime <- rJava::.jcall("java/lang/Runtime", "Ljava/lang/Runtime;", "getRuntime")
maxMemory <- rJava::.jcall(runtime, "J", "maxMemory") / (1024^2)
totalMemory <- rJava::.jcall(runtime, "J", "totalMemory") / (1024^2)
freeMemory <- rJava::.jcall(runtime, "J", "freeMemory") / (1024^2)
# cat("Cores usados para r5r_core_calibration:",r5r_core_calibration$getNumberOfThreads(), "cores.\n")
cat("Máxima memoria: ", maxMemory, "MB\n")
cat("Memoria total asignada: ", totalMemory, "MB\n")
cat("Memoria libre: ", freeMemory, "MB\n")

# Tiempo de procesamiento:
End_Time_S6_0_P1 <- Sys.time()
print(paste("1. Tiempo de inicialización: ", as.duration(End_Time_S6_0_P1 - Start_Time_S6_0_P1)))

# ####
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ::: 2. Carga de datos de origen                                            :::
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ####

Start_Time_S6_0_P2 <- Sys.time()

# Cargamos datos de la Encuesta de Movilidad 2023:
load("./Data/2_Processing/3_EM2023/DataFrames_EM2023.RData")
Viajes_Estandarizado_df
Viajes_Calibracion_Modelo_df

# Cargamos los shapes necesarios:
Shape_UPLs <- st_read("./Data/2_Processing/1_Empty_Shapes/Shape_Vacio_UPLs.gpkg")
Shape_UTAM <- st_read("./Data/2_Processing/1_Empty_Shapes/Shape_Vacio_UTAM.gpkg")
Shape_ZAT <- st_read("./Data/2_Processing/1_Empty_Shapes/Shape_Vacio_ZAT.gpkg")

# Tiempo de procesamiento:
End_Time_S6_0_P2 <- Sys.time()
print(paste("2. Tiempo de carga de datos de origen: ", as.duration(End_Time_S6_0_P2 - Start_Time_S6_0_P2)))

# ####
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ::: 3. Preparación de los sf y los df necesarios para calibrar             :::
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ####
#
Start_Time_S6_0_P3 <- Sys.time()

# --- Preparamos los sf de Orígenes (sf) ---

# Establecemos el medio:
unique(Viajes_Calibracion_Modelo_df$VEHICULO_ESTANDAR)
Vehiculo <- "Transporte Público"
UPL_Origen_Seleccionada <- "15"

# Preparamos los sf de Origen y Destino, pero tenemos en cuenta únicamente la muestra de la EM2023:
Origenes_Calibracion_sf <- Viajes_Calibracion_Modelo_df |>
  ungroup() |>
  filter(UPL_ORIGEN == UPL_Origen_Seleccionada) |> # Siempre la UPL de partida
  filter(VEHICULO_ESTANDAR == Vehiculo) |>
  mutate(COD_ZAT = ZAT_ORIGEN) |>
  select(COD_ZAT, VEHICULO_ESTANDAR) |>
  left_join(
    Shape_ZAT |> select(COD_UPL, COD_ZAT)
  ) |>
  mutate(id = COD_ZAT) |>
  st_as_sf() |>
  st_centroid() |>
  st_transform(crs = 4326) |>
  distinct() |>
  select(id, COD_UPL, VEHICULO_ESTANDAR)
Origenes_Calibracion_sf

Destinos_Calibracion_sf <- Viajes_Calibracion_Modelo_df |>
  ungroup() |>
  filter(UPL_ORIGEN == UPL_Origen_Seleccionada) |> # Siempre la UPL de partida
  filter(VEHICULO_ESTANDAR == Vehiculo) |>
  mutate(COD_ZAT = ZAT_DESTINO) |>
  select(COD_ZAT, VEHICULO_ESTANDAR) |>
  left_join(
    Shape_ZAT |> select(COD_UPL, COD_ZAT)
  ) |>
  mutate(id = COD_ZAT) |>
  st_as_sf() |>
  st_centroid() |>
  st_transform(crs = 4326) |>
  distinct() |>
  select(id, COD_UPL, VEHICULO_ESTANDAR)
Destinos_Calibracion_sf

# Miramos solo por chismosear en el mapa:
Shape_UPLs |>
  ggplot() +
  geom_sf(linewidth = 0.6) +
  geom_sf(data = Destinos_Calibracion_sf, size = 0.5, color = "green") +
  geom_sf(data = Origenes_Calibracion_sf, size = 0.5, color = "red") +
  theme_void()

# Establecemos el df de referencia de la Encuesta de Movilidad 2023:
Tiempos_Encuesta_df <- Viajes_Calibracion_Modelo_df |>
  ungroup() |>
  filter(UPL_ORIGEN == UPL_Origen_Seleccionada) |> # Siempre la UPL de partida
  filter(VEHICULO_ESTANDAR == Vehiculo) |>
  select(
    from_id = ZAT_ORIGEN,
    to_id = ZAT_DESTINO,
    UPL_ORIGEN,
    UPL_DESTINO,
    Tiempo_EM2023 = AVRG_TIME
  ) |>
  distinct()
Tiempos_Encuesta_df

# Tiempo de procesamiento:
End_Time_S6_0_P3 <- Sys.time()
print(paste("3. Tiempo de prepararción de los sf y df necesarios para calibrar: ", as.duration(End_Time_S6_0_P3 - Start_Time_S6_0_P3)))

# ####
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ::: 4. Definimos la función de la Raíz del Error Cuadrático Medio (RMSE)   :::
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ####
#
Start_Time_S6_0_P4 <- Sys.time()

Fecha_De_Salida <- "2025-04-10"

# Creamos la función para el cálculo del RMSE:
Calcular_RMSE <- function(Parametros) {
  # 1. Extraemos los parámetros que se están probando en esta iteración
  Velocidad_Caminata <- Parametros[1]
  Tiempo_Max_Caminata <- Parametros[2]
  Hora_De_Salida <- floor(Parametros[3])                                         # Tomamos la parte entera como la hora
  Minuto_De_Salida <- round((Parametros[3]-floor(Parametros[3]))*60, digits = 0) # El residuo lo pasamos a minutos.
  Tiempo_de_Partida <- as.POSIXct(paste(Fecha_De_Salida, " ", Hora_De_Salida, ":", Minuto_De_Salida, ":00", sep = "")) # Fecha Origina: 2024-07-10

  # 2. Ejecutamos el cálculo de la matriz de tiempos de viaje con r5r
  Matriz_Tiempos_Modelo <- r5r::travel_time_matrix(
    r5r_core = r5r_core_calibration,
    origins = Origenes_Calibracion_sf,
    destinations = Destinos_Calibracion_sf,
    mode = c("WALK", "TRANSIT"),
    departure_datetime = Tiempo_de_Partida,
    walk_speed = Velocidad_Caminata,
    max_walk_time = Tiempo_Max_Caminata,
    percentiles = 50 # Usar la mediana (percentil 50) es una buena práctica
  )
  
  # 3. Unir los resultados del modelo con los de la encuesta
  Comparacion <- merge(Matriz_Tiempos_Modelo, Tiempos_Encuesta_df, 
                       by.x = c("from_id", "to_id"), 
                       by.y = c("from_id", "to_id"))
  
  # 4. Calcular el error (RMSE)
  # Asegúrate de que los nombres de las columnas de tiempo coincidan
  RMSE <- sqrt(mean((Comparacion$travel_time_p50 - Comparacion$Tiempo_EM2023)^2, na.rm = TRUE))
  
  # Imprimir el progreso (opcional, pero útil)
  cat("Probando modelo con:",
    "\n   Walk_speed: ", Velocidad_Caminata, " | Max_walk_time: ", Tiempo_Max_Caminata, " | Hora de salida: ", Hora_De_Salida, ":", Minuto_De_Salida, " -> RMSE: ", RMSE, "\n", sep = "")
  
  return(RMSE)
}

# Tiempo de procesamiento:
End_Time_S6_0_P4 <- Sys.time()
print(paste("4. Tiempo de definición de la función de la Raíz del Error Cuadrático Medio (RMSE): ", as.duration(End_Time_S6_0_P4 - Start_Time_S6_0_P4)))

# ####
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ::: 5. Corremos el modelo usando Algoritmos Genéticos                      :::
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ####
#
Start_Time_S6_0_P5 <- Sys.time()

# Instalamos el paquete GA, pues sive para la solución del problema, ya que exploran
# el espacio de soluciones de manera muy eficiente, imitando el proceso de evolución natural
# y esn menos propenso a quedarse atorado en mínimos locales.

# install.packages("GA")
# library(GA)

# Definir los límites para los parámetros
# Límite inferior y superior para walk_speed (en m/s)
Limite_Inferior <- c(1.8, 15, 6)  # walk_speed > 0.5 m/s (1,8 km/h), max_walk > 10 min, Hora mínima: 6:00
Limite_Superior <- c(7.2, 60, 23)  # walk_speed < 2.0 m/s (7,2 km/h), max_walk < 60 min, Hora máxima: 23:00

# Ejecutar el algoritmo genético
# GA maximiza, por lo que debemos minimizar el negativo de nuestra función de error
Resultado_GA <- ga(
  type = "real-valued",
  fitness = function(params) -Calcular_RMSE(params), # Minimizar RMSE es maximizar -RMSE
  lower = Limite_Inferior,
  upper = Limite_Superior,
  popSize = 20,    # Número de "individuos" (soluciones) por generación
  maxiter = 100,   # 50. Número máximo de generaciones
  run = 50,        # 20. Criterio de parada si no hay mejora
  monitor = TRUE   # Muestra el progreso
)
summary(Resultado_GA)

# Guardamos para no tener que recalibrar si se corre el script más de una vez:
Fecha_Actual <- as.character(format(Sys.time(), "%Y-%m-%d"))
Fecha_Actual # 2025-10-21
# save(
#   Resultado_GA,
#   file = paste("./Data/2_Processing/6_Accesibility/Scn0/Resultado_GA_2025-10-22.RData", sep = "")
# )
# Por si necesitamos cargar los datos:
load(paste("./Data/2_Processing/6_Accesibility/Scn0/Resultado_GA_2025-10-22.RData", sep = ""))

# 5. Analizar los resultados
summary(Resultado_GA)

# Los parámetros óptimos ahora serán 4 valores
Parametros_Optimos <- Resultado_GA@solution
Optimo_Walk_Speed <- mean(Parametros_Optimos[,1])
Optimo_Max_Walk_Time <- mean(Parametros_Optimos[,2])
Hora_De_Salida <- floor(mean(Parametros_Optimos[,3]))                                         # Tomamos la parte entera como la hora
Minuto_De_Salida <- round((mean(Parametros_Optimos[,3])-floor(mean(Parametros_Optimos[,3])))*60, digits = 0) # El residuo lo pasamos a minutos.
Tiempo_de_Partida <- as.POSIXct(paste(Fecha_De_Salida, " ", Hora_De_Salida, ":", Minuto_De_Salida, ":00", sep = ""))
Mejor_RMSE <- -Resultado_GA@fitnessValue

cat("Parámetros óptimos encontrados",
    "\n   Walk_speed:", Optimo_Walk_Speed,
    "\n   Max_walk_time:", Optimo_Max_Walk_Time,
    "\n   Time:", as.character(Tiempo_de_Partida),
    "\n   Mejor RMSE:", Mejor_RMSE # 31.07138
    )

# Tiempo de procesamiento:
End_Time_S6_0_P5 <- Sys.time()
print(paste("5. Tiempo de calibración del modelo usando Algoritmos Genéticos: ", as.duration(End_Time_S6_0_P5 - Start_Time_S6_0_P5)))

# ####
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ::: 6. Testeamos, calculamos y graficamos los errores                      :::
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ####
#
Start_Time_S6_0_P6 <- Sys.time()

# --- 6.1. Testeamos una matriz Origen-Destino con los parámetros óptimos ---

# Los parámetros óptimos ahora serán 4 valores
Parametros_Optimos <- Resultado_GA@solution
Optimo_Walk_Speed <- mean(Parametros_Optimos[,1])
Optimo_Max_Walk_Time <- mean(Parametros_Optimos[,2])
Hora_De_Salida <- floor(mean(Parametros_Optimos[,3]))                                         # Tomamos la parte entera como la hora
Minuto_De_Salida <- round((mean(Parametros_Optimos[,3])-floor(mean(Parametros_Optimos[,3])))*60, digits = 0) # El residuo lo pasamos a minutos.
Tiempo_de_Partida <- as.POSIXct(paste(Fecha_De_Salida, " ", Hora_De_Salida, ":", Minuto_De_Salida, ":00", sep = ""))
Mejor_RMSE <- -Resultado_GA@fitnessValue

# Corremos una matriz Origen-Destino con los datos óptimos
Matriz_Tiempos_Viaje <- r5r::travel_time_matrix(
  r5r_core = r5r_core_calibration,
  origins = Origenes_Calibracion_sf,
  destinations = Destinos_Calibracion_sf,
  mode = c("WALK", "TRANSIT"),
  departure_datetime = Tiempo_de_Partida,
  walk_speed = Optimo_Walk_Speed,
  max_walk_time = Optimo_Max_Walk_Time,
  percentiles = 50 # Usar la mediana (percentil 50) es una buena práctica
)
Matriz_Tiempos_Viaje
Tiempos_Encuesta_df

# Unimos los resultados del modelo con los de la encuesta
Comparacion_df <- merge(Matriz_Tiempos_Viaje, Tiempos_Encuesta_df, 
                     by.x = c("from_id", "to_id"), 
                     by.y = c("from_id", "to_id")) |>
  mutate(
    # Error absoluto: la diferencia directa en minutos:
    Error_Absoluto = travel_time_p50 - Tiempo_EM2023,
    # Error porcentual: útil para comparar errores en viajes cortos y largos:
    Error_Porcentual = (Error_Absoluto / Tiempo_EM2023) * 100
  )
Comparacion_df

# Calculamos manualmente el error (RMSE) con los datos manuales para compararlo con el de la calibración:
sqrt(mean((Comparacion_df$travel_time_p50 - Comparacion_df$Tiempo_EM2023)^2, na.rm = TRUE))

# Gráfico 1: Tiempos del Modelo vs. Tiempos de la Encuesta (Scatter Plot)
ggplot(Comparacion_df, aes(x = Tiempo_EM2023, y = travel_time_p50)) +
  geom_point(alpha = 0.5) + # Puntos semitransparentes para ver la densidad
  geom_abline(color = "darkred", linetype = "dashed", size = 1) + # Línea de predicción perfecta (Y=X)
  labs(
    title = "Tiempos del Modelo vs. Tiempos Reales",
    x = "Tiempo de Viaje de la Encuesta (min)",
    y = "Tiempo de Viaje del Modelo (min)",
    caption = "La línea roja representa una predicción perfecta.",
    xlim = c(0,150),
    ylim = c(0,150)
  ) +
  theme_minimal()

# Gráfico 2: Distribución de los Errores Absolutos (Histograma)
ggplot(Comparacion_df, aes(x = Error_Absoluto)) +
  geom_histogram(binwidth = 5, fill = "darkgray", color = "black") +
  geom_vline(xintercept = 0, color = "darkred", linetype = "dashed", size = 1) +
  labs(
    title = "Distribución del Error de Predicción",
    x = "Error Absoluto (Modelo - Encuesta) en minutos",
    y = "Frecuencia (Nº de pares OD)"
  ) +
  theme_minimal()

# Tiempo de procesamiento:
End_Time_S6_0_P6 <- Sys.time()
print(paste("6. Tiempo de testeo, cálculo y gráfico de errores: ", as.duration(End_Time_S6_0_P6 - Start_Time_S6_0_P6)))

# ####
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ::: 7. Guardado de variables importantes                                   :::
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ####
#
Start_Time_S6_0_P7 <- Sys.time()

# Guardamos los df importantes:
Fecha_Actual <- as.character(format(Sys.time(), "%Y-%m-%d"))
Fecha_Actual # 2025-10-22
save(
  Origenes_Calibracion_sf,
  Destinos_Calibracion_sf,
  Matriz_Tiempos_Viaje,
  Tiempos_Encuesta_df,
  Comparacion_df,
  Optimo_Walk_Speed,
  Optimo_Max_Walk_Time,
  Tiempo_de_Partida,
  Mejor_RMSE,
  file = paste("./Data/2_Processing/6_Accesibility/Scn0/Variables_Calibracion_Modelo_RMSE_2025-10-22.RData", sep = "")
)

# Por si necesitamos cargar los datos:
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

# --- Limpiamos después de usar r5r_core ---

r5r_core_calibration$getNumberOfThreads()
r5r::stop_r5(r5r_core_calibration)
rJava::.jgc(R.gc = TRUE)

# Tiempo de procesamiento:
End_Time_S6_0_P7 <- Sys.time()
print(paste("7. Tiempo de testeo, cálculo y gráfico de errores: ", as.duration(End_Time_S6_0_P7 - Start_Time_S6_0_P7)))

# ####
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ::: 8. Tiempos de procesamiento                                            :::
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ####
#
End_Time_S6_0_P0 <- Sys.time()

# Tiempo de procesamiento:
print(paste("1. Tiempo de inicialización: ", as.duration(End_Time_S6_0_P1 - Start_Time_S6_0_P1)))
print(paste("2. Tiempo de carga de datos de origen: ", as.duration(End_Time_S6_0_P2 - Start_Time_S6_0_P2)))
print(paste("3. Tiempo de prepararción de los sf y df necesarios para calibrar: ", as.duration(End_Time_S6_0_P3 - Start_Time_S6_0_P3)))
print(paste("4. Tiempo de definición de la función de la Raíz del Error Cuadrático Medio (RMSE): ", as.duration(End_Time_S6_0_P4 - Start_Time_S6_0_P4)))
print(paste("5. Tiempo de calibración del modelo usando Algoritmos Genéticos: ", as.duration(End_Time_S6_0_P5 - Start_Time_S6_0_P5)))
print(paste("6. Tiempo de testeo, cálculo y gráfico de errores: ", as.duration(End_Time_S6_0_P6 - Start_Time_S6_0_P6)))
print(paste("7. Tiempo de testeo, cálculo y gráfico de errores: ", as.duration(End_Time_S6_0_P7 - Start_Time_S6_0_P7)))
print(paste("Tiempo de procesamiento del script de calibración:: ", as.duration(End_Time_S6_0_P0 - Start_Time_S6_0_P0)))

