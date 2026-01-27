# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# :::                                                                        :::
# :::                           SCRIPT DE GTFS                               :::
# :::          Script para la generación de GTFS de forma dinámica           :::
# :::                                                                        :::
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#
# Nombre: 5_0_GTFS_Generator.R
#
# Descripcion: Este Script creará los GTFS necesarios para la implementación del
#              sistema de transporte masivo férreo de Bogotá

# ####
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# :::                       Contenido de este Script                         :::
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ####
#
# 1. Inicialización
# 2. Declaración de variables de entrada del GFTS
# 3. Inicio de la generación de tablas para el GTFS
# 4. Creación de archivos agency, routes, calendar y feed_info
# 5. Creación de trips y stop_times
# 6. Guardado de archivos y zip en disco
# 7. Carga del GTFS y entorno para iniciar tests
# 8. Test del GTFS
# 9. Tiempos de procesamiento del script
#
Start_Time_S5_0_P0 <- Sys.time()

# ####
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ::: 1. Inicialización                                                      :::
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ####
#
Start_Time_S5_0_P1 <- Sys.time()

# Cargamos el script de inicialización general
source("0_Initialization.R")

# Mostramos los tiempos de procesamiento:
End_Time_S5_0_P1 <- Sys.time()
print(paste("1. Tiempo de inicialización: ", as.duration(End_Time_S5_0_P1 - Start_Time_S5_0_P1)))

# ####
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ::: 2. Declaración de variables de entrada del GFTS                        :::
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ####
#
# Declaramos e Importamos toda la información de entrada y necesaria para la
# cración del GTFS de manera dinámica.
#
Start_Time_S5_0_P2 <- Sys.time()

# En teoría, con solo modificar los datos de esta sección, se debería poder
# crear cualquier GFTS.
#
# Para crear dinámicamente los GTFS de diferentes líneas, debemos identificarlas por
# algunos identificadores únicos que definiremos a continuación:

ID_De_Ruta <- "Regio-Occidente"
Nombre_Corto <- "RE-Occidente"
Nombre_Largo <- "Tren Regional del Occidente"
Origen_Linea <- "Museo Nacional"
Destino_Linea <- "CATAM - Carrera 116"
Frecuencias_LS = c(720, 300, 480, 300, 480) # Metro: 8, 3, 5, 3, 5 min. LRT: 5, 2, 4, 2, 4 min. Tren: 12, 5, 8, 5, 8 min.
Frecuencias_DF = c(480)  # Hora valle de LS
Velocidad_Promedio <- 60 # 35 Para Metro, 25 para LRT y 60 para Trenes
Tiempo_de_Parada <- 30   # 30 Para todos
Ruta_Archivos <- Nombre_Corto

# Mostramos los tiempos de procesamiento:
End_Time_S5_0_P2 <- Sys.time()
print(paste("2. Tiempo de declaración de variables de entrada del GFTS: ", as.duration(End_Time_S5_0_P2 - Start_Time_S5_0_P1)))

# ####
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ::: 3. Inicio de la generación de tablas para el GTFS                      :::
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ####
#
Start_Time_S5_0_P3 <- Sys.time()

# 3.1. INFORMACIÓN GENERAL DEL FEED
feed_info_data <- list(
  feed_publisher_name = "Santiago Silvera",
  feed_contact_email = "ssilvera@gmail.com",
  feed_contact_url = "https://www.silvera.com.co",
  feed_publisher_url = "https://www.silvera.com.co",
  feed_lang = "es",
  feed_start_date = "20000101",
  feed_end_date = "20991231",
  feed_version = "1.0"
)

# 3.2. INFORMACIÓN DE LA AGENCIA (agency.txt)
agency_data <- list(
  agency_id = "EM_BOG",
  agency_name = "Metro de Bogotá S.A.",
  agency_url = "https://www.metrodebogota.gov.co/",
  agency_timezone = "America/Bogota",
  agency_lang = "es"
)

# 3.3. INFORMACIÓN DE LA RUTA (routes.txt)
Descripcion <- paste(Nombre_Largo, " | ", Origen_Linea, " - ", Destino_Linea, sep = "")
route_data <- list(
  agency_id = "EM_BOG",
  route_id = ID_De_Ruta,
  route_short_name = Nombre_Corto,
  route_long_name = Nombre_Largo,
  route_desc = Descripcion,
  route_type = 1 # 1 = Metro, tranvía.
)

# 3.4. INFORMACIÓN DEL CALENDARIO (calendar.txt)

# Definimos los servicios como lo hicimos para la PLMB; Un calendario para los
# servicios de Lunes a sábado, y otro para los servicios de domingos y festivos.
calendar_data <- data.frame(
  service_id = c("SERVICIO-LS","SERVICIO-DF"),
  start_date = c("20000101","20000101"),
  end_date = c("20991231","20991231"),
  monday = c(1,0),
  tuesday = c(1,0),
  wednesday = c(1,0),
  thursday = c(1,0),
  friday = c(1,0),
  saturday = c(1,0),
  sunday = c(0,1)
)

# 3.5. PARÁMETROS DE OPERACIÓN

# Velocidad promedio estimada del vehículo en km/h (incluyendo aceleración/desaceleración):
average_speed_kmh <- Velocidad_Promedio
# Tiempo de detención en cada estación (segundos):
dwell_time_secs <- Tiempo_de_Parada 

# 3.6. FRECUENCIAS DE PASO (frequencies.txt):

# Definimos los intervalos de operación para cada TIPO de servicio (LS y DF)
# El script asignará estas frecuencias a todos los viajes que usen el service_id correspondiente.

# Frecuencias para el servicio Lunes-Sábado ("SERVICIO-LS")
frequencies_ls <- data.frame(
  start_time = c("04:30:00","05:30:00","09:00:00","16:00:00","20:00:00"),
  end_time   = c("05:29:59","08:59:59","15:59:59","19:59:59","23:59:00"),
  headway_secs = Frecuencias_LS # 8min, 3min, 5min, 3min, 5min
)

# Frecuencias para el servicio Domingo-Festivo ("SERVICIO-DF")
frequencies_df <- data.frame(
  start_time = c("05:00:00"),
  end_time   = c("23:59:00"),
  headway_secs = Frecuencias_DF # 5 min constante
)

# 3.7. INFORMACIÓN DE VIAJES Y SENTIDOS (trips.txt)

# Le ponemos nombre a los sentidos de cada viaje (headsigns o letreros).
trip_data <- list(
  direction_0_headsign = Destino_Linea,  # Sentido 0
  direction_1_headsign = Origen_Linea    # Sentido 1
)

# 3.8. ARCHIVOS DE ENTRADA Y SALIDA

# Ruta al archivo KML  que contiene las paradas y el trazado.
kml_file_path <- paste("./Data/1_Sources/5_GTFS/Scn0/", Ruta_Archivos, ".kml", sep = "")
kml_file_path

# Carpeta donde se guardarán los archivos GTFS generados:
output_dir <- paste("./Data/2_Processing/5_GTFS/GTFS_", Ruta_Archivos, "_Generado", sep = "")
output_dir

# Mostramos los tiempos de procesamiento:
End_Time_S5_0_P3 <- Sys.time()
print(paste("3. Tiempo de Inicio de la generación de tablas para el GTFS: ", as.duration(End_Time_S5_0_P3 - Start_Time_S5_0_P3)))

# ####
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ::: 4. Creación de archivos agency, routes, calendar y feed_info           :::
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ####
#
Start_Time_S5_0_P4 <- Sys.time()

# Crearmos el directorio de salida si no existe:
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# 4.1. LEER Y PROCESAR EL ARCHIVO KML

stops_sf <- st_collection_extract(st_read(kml_file_path), "POINT")
rownames(stops_sf) <- NULL
shape_sf <- st_collection_extract(st_read(kml_file_path), "LINESTRING")
stops_sf
shape_sf

# Verificamos Que haya cargado bien el trazado y las estaciones:
tm_basemap("OpenStreetMap") +
  tm_shape(shape_sf) +
  tm_lines(col = "darkgreen", lwd = 5) +
  tm_shape(stops_sf) +
  tm_dots(fill = "blue", size = 1)

# 4.2. CREAR stops.txt

stops_df <- stops_sf %>%
  mutate(
    stop_lon = st_coordinates(.)[,1],
    stop_lat = st_coordinates(.)[,2],
    stop_id = paste0(toupper(route_data$route_short_name),"-E", 1:n()),
    stop_name = Name,
    location_type = 0
  ) |>
  st_drop_geometry() |>
  select(stop_id, stop_name, stop_lat, stop_lon, location_type)
stops_df

print("stops.txt creado exitosamente.")

# 4.3. CREAR shapes.txt PARA CADA SENTIDO

# Extraemos las coordenadas de cada punto del trazado:
shape_points <- st_coordinates(shape_sf) %>% as.data.frame()

# Crear shape para el sentido IDA (0)
shape_df_ida <- shape_points %>%
  rename(shape_pt_lon = X, shape_pt_lat = Y) %>%
  mutate(
    shape_id = paste0("SHP-", toupper(route_data$route_short_name), "-IDA"),
    shape_pt_sequence = 1:n(),
    dist_to_prev_m = geosphere::distGeo(
      p1 = cbind(lag(shape_pt_lon, 1), lag(shape_pt_lat, 1)),
      p2 = cbind(shape_pt_lon, shape_pt_lat)
    ),
    dist_to_prev_m = ifelse(is.na(dist_to_prev_m), 0, dist_to_prev_m),
    dist_diff_km = dist_to_prev_m / 1000,
    shape_dist_traveled = cumsum(dist_diff_km)
  )
shape_df_ida

# Crear shape para el sentido VUELTA (1) invirtiendo el orden
shape_df_vuelta <- shape_df_ida %>%
  arrange(desc(shape_pt_sequence)) %>%
  mutate(
    shape_id = paste0("SHP-", toupper(route_data$route_short_name), "-VUELTA"),
    shape_pt_sequence = 1:n(),
    dist_to_prev_m = geosphere::distGeo(
      p1 = cbind(lag(shape_pt_lon, 1), lag(shape_pt_lat, 1)),
      p2 = cbind(shape_pt_lon, shape_pt_lat)
    ),
    dist_to_prev_m = ifelse(is.na(dist_to_prev_m), 0, dist_to_prev_m),
    dist_diff_km = dist_to_prev_m / 1000,
    shape_dist_traveled = cumsum(dist_diff_km)
  )
shape_df_vuelta

# Combinar ambos shapes en un único dataframe
shape_df <- bind_rows(shape_df_ida, shape_df_vuelta) %>%
  select(shape_id, shape_pt_lat, shape_pt_lon, shape_pt_sequence, shape_dist_traveled)
shape_df

print("shapes.txt creado exitosamente con shapes para IDA y VUELTA.")

# 4.4. ORDENAR PARADAS Y CALCULAR DISTANCIAS

# En caso de que los puntos de las estaciones no estén ordenados, los ordenamos
# en función del trazado de la línea:

# Usamos el shape de IDA como referencia para el orden geográfico de las paradas.
shape_vertices_sf <- shape_df_ida %>%
  st_as_sf(coords = c("shape_pt_lon", "shape_pt_lat"), crs = st_crs(stops_sf))
shape_vertices_sf

# Buscamos los vértices más cercanos:
closest_vertex_indices <- st_nearest_feature(stops_sf, shape_vertices_sf)
closest_vertex_indices

# Ordenamos las paradas:
stops_df_ordered <- stops_sf %>%
  mutate(
    closest_vertex_seq = closest_vertex_indices,
    stop_lon = st_coordinates(.)[,1],
    stop_lat = st_coordinates(.)[,2]
  ) %>%
  st_drop_geometry() %>%
  arrange(closest_vertex_seq) %>%
  left_join(stops_df, by = c("stop_lon", "stop_lat")) %>%
  left_join(
    select(shape_df_ida, shape_pt_sequence, shape_dist_traveled), 
    by = c("closest_vertex_seq" = "shape_pt_sequence")
  ) %>%
  mutate(stop_dist_traveled_m = shape_dist_traveled * 1000)
stops_df_ordered

# --- PASO 5: CREAR ARCHIVOS SIMPLES ---
agency_df <- as.data.frame(agency_data)
routes_df <- as.data.frame(route_data)
calendar_df <- as.data.frame(calendar_data)
feed_info_df <- as.data.frame(feed_info_data)

print("Archivos agency, routes, calendar y feed_info creados.")

# Mostramos los tiempos de procesamiento:
End_Time_S5_0_P4 <- Sys.time()
print(paste("4. Tiempo de creación de archivos agency, routes, calendar y feed_info: ", as.duration(End_Time_S5_0_P4 - Start_Time_S5_0_P4)))

# ####
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ::: 5. Creación de trips y stop_times                                      :::
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ####
#
#
Start_Time_S5_0_P5 <- Sys.time()

# 4.1. GENERAR TODOS LOS VIAJES (TRIPS) BASADO EN HORARIOS

all_trips <- list()

# Función para convertir "HH:MM:SS" a segundos
time_to_seconds <- function(time_str) {
  parts <- as.numeric(strsplit(time_str, ":")[[1]])
  return(parts[1] * 3600 + parts[2] * 60 + parts[3])
}

for (service_idx in 1:nrow(calendar_data)) {
  service_id <- calendar_data$service_id[service_idx]
  
  if (service_id == "SERVICIO-LS") {
    freq_table <- frequencies_ls
  } else {
    freq_table <- frequencies_df
  }
  
  for (direction in c(0, 1)) {
    for (i in 1:nrow(freq_table)) {
      start_secs <- time_to_seconds(freq_table$start_time[i])
      end_secs <- time_to_seconds(freq_table$end_time[i])
      headway <- freq_table$headway_secs[i]
      
      departure_times_secs <- seq(start_secs, end_secs, by = headway)
      
      for (dep_time in departure_times_secs) {
        dep_time_str <- format(hms::hms(dep_time), "%H%M%S")
        
        new_trip <- data.frame(
          route_id = route_data$route_id,
          service_id = service_id,
          trip_id = paste(route_data$route_short_name, if_else(direction == 0, "IDA", "VUELTA"), service_id, dep_time_str, sep = "-"),
          trip_headsign = if_else(direction == 0, trip_data$direction_0_headsign, trip_data$direction_1_headsign),
          direction_id = direction,
          shape_id = if_else(
            direction == 0, 
            paste0("SHP-", toupper(route_data$route_short_name), "-IDA"),
            paste0("SHP-", toupper(route_data$route_short_name), "-VUELTA")
          )
        )
        all_trips <- append(all_trips, list(new_trip))
      }
    }
  }
}
trips_df <- bind_rows(all_trips)
trips_df

print(paste("trips.txt creado con", nrow(trips_df), "viajes individuales."))

# 4.2. CREAR stop_times.txt CON HORAS ABSOLUTAS

# Creamos una función para cambiar de segundos a formato de tiempo de forma rápida
format_time <- function(seconds) {
  seconds <- round(seconds)
  h <- floor(seconds / 3600); m <- floor((seconds %% 3600) / 60); s <- seconds %% 60
  sprintf("%02d:%02d:%02d", h, m, s)
}

# Lógica de cálculo para la plantilla del Sentido 0 (Ida)
stop_times_template_0 <- stops_df_ordered %>%
  mutate(
    # 1. Calcular diferencia de distancia con la parada anterior.
    dist_diff_m = stop_dist_traveled_m - lag(stop_dist_traveled_m),
    
    # 2. Calcular tiempo de viaje, reemplazando el NA del primer lag con 0.
    travel_time_secs = (ifelse(is.na(dist_diff_m), 0, dist_diff_m) / 1000) / average_speed_kmh * 3600,
    
    # 3. El tiempo de parada en la primera estación no afecta la línea de tiempo.
    effective_lagged_dwell = lag(if_else(row_number() == 1, 0, dwell_time_secs), default = 0),
    
    # 4. La llegada es la suma acumulada de los tiempos de viaje y las paradas anteriores.
    arrival_time_offset = cumsum(travel_time_secs + effective_lagged_dwell),
    
    # 5. La salida es la llegada + el tiempo de parada en la estación actual.
    departure_time_offset = arrival_time_offset + dwell_time_secs,

    # 6. Ajuste final: La última parada no tiene salida (departure = arrival).
    departure_time_offset = if_else(row_number() == n(), arrival_time_offset, departure_time_offset),
    
    # 7. Ajuste final: La primera parada tiene llegada y salida en 0.
    arrival_time_offset = if_else(row_number() == 1, 0, arrival_time_offset),
    departure_time_offset = if_else(row_number() == 1, 0, departure_time_offset)
  ) %>%
  select(stop_id, stop_sequence = closest_vertex_seq, arrival_time_offset, departure_time_offset)
stop_times_template_0

# Lógica de cálculo para la plantilla del Sentido 1 (Vuelta)
stop_times_template_1 <- stops_df_ordered %>%
  arrange(desc(closest_vertex_seq)) %>%
  mutate(
    # 1. Calcular diferencia de distancia con la parada anterior.
    dist_diff_m = abs(stop_dist_traveled_m - lag(stop_dist_traveled_m)),
    
    # 2. Calcular tiempo de viaje, reemplazando el NA del primer lag con 0.
    travel_time_secs = (ifelse(is.na(dist_diff_m), 0, dist_diff_m) / 1000) / average_speed_kmh * 3600,
    
    # 3. El tiempo de parada en la primera estación no afecta la línea de tiempo.
    effective_lagged_dwell = lag(if_else(row_number() == 1, 0, dwell_time_secs), default = 0),
    
    # 4. La llegada es la suma acumulada de los tiempos de viaje y las paradas anteriores.
    arrival_time_offset = cumsum(travel_time_secs + effective_lagged_dwell),
    
    # 5. La salida es la llegada + el tiempo de parada en la estación actual.
    departure_time_offset = arrival_time_offset + dwell_time_secs,
    
    # 6. Ajuste final: La última parada no tiene salida (departure = arrival).
    departure_time_offset = if_else(row_number() == n(), arrival_time_offset, departure_time_offset),
    
    # 7. Ajuste final: La primera parada tiene llegada y salida en 0.
    arrival_time_offset = if_else(row_number() == 1, 0, arrival_time_offset),
    departure_time_offset = if_else(row_number() == 1, 0, departure_time_offset)
  ) %>%
  select(stop_id, stop_sequence = closest_vertex_seq, arrival_time_offset, departure_time_offset)
stop_times_template_1

all_stop_times <- list()

for (i in 1:nrow(trips_df)) {
  trip <- trips_df[i, ]
  trip_start_time_str <- sub(".*-", "", trip$trip_id)
  
  h <- as.numeric(substr(trip_start_time_str, 1, 2))
  m <- as.numeric(substr(trip_start_time_str, 4, 5))
  s <- as.numeric(substr(trip_start_time_str, 7, 8))
  trip_start_time_secs <- h * 3600 + m * 60 + s
  
  template <- if (trip$direction_id == 0) stop_times_template_0 else stop_times_template_1
  
  stop_times_for_trip <- template %>%
    mutate(
      trip_id = trip$trip_id,
      arrival_time = format_time(trip_start_time_secs + arrival_time_offset),
      departure_time = format_time(trip_start_time_secs + departure_time_offset),
      stop_sequence = 1:n(),
      pickup_type = 0,
      drop_off_type = 0
    ) %>%
    select(trip_id, arrival_time, departure_time, stop_id, stop_sequence, pickup_type, drop_off_type)
  
  all_stop_times <- append(all_stop_times, list(stop_times_for_trip))
}
stop_times_df <- bind_rows(all_stop_times)
stop_times_df

print(paste("stop_times.txt creado con", nrow(stop_times_df), "registros."))

# Mostramos los tiempos de procesamiento:
End_Time_S5_0_P5 <- Sys.time()
print(paste("5. Tiempo de creación de trips y stop_times: ", as.duration(End_Time_S5_0_P5 - Start_Time_S5_0_P5)))

# ####
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ::: 6. Guardado de archivos y zip en disco                                 :::
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ####
#
Start_Time_S5_0_P6 <- Sys.time()

# 5.1. ESCRIBIR TODOS LOS ARCHIVOS CSV
# No se tiene en cuenta frequencies.txt ya que no es necesario en un GTFS basado en horarios.
write.csv(agency_df, file.path(output_dir, "agency.txt"), row.names = FALSE, quote = TRUE, na = "")
write.csv(calendar_df, file.path(output_dir, "calendar.txt"), row.names = FALSE, quote = TRUE, na = "")
write.csv(feed_info_df, file.path(output_dir, "feed_info.txt"), row.names = FALSE, quote = TRUE, na = "")
write.csv(routes_df, file.path(output_dir, "routes.txt"), row.names = FALSE, quote = TRUE, na = "")
write.csv(shape_df, file.path(output_dir, "shapes.txt"), row.names = FALSE, quote = TRUE, na = "")
write.csv(stops_df, file.path(output_dir, "stops.txt"), row.names = FALSE, quote = TRUE, na = "")
write.csv(stop_times_df, file.path(output_dir, "stop_times.txt"), row.names = FALSE, quote = TRUE, na = "")
write.csv(trips_df, file.path(output_dir, "trips.txt"), row.names = FALSE, quote = TRUE, na = "")

print(paste("¡Proceso completado! Los 8 archivos GTFS han sido guardados en la carpeta:", output_dir))

# 5.2. COMPRIMIR ARCHIVOS EN UN ZIP
print("Iniciando la compresión de archivos GTFS...")

# Nombre del archivo zip de salida.
zip_filename <- paste("GTFS_", Ruta_Archivos, ".zip", sep = "")
zip_filename

# Guardar el directorio de trabajo actual para poder restaurarlo después.
original_wd <- getwd()
original_wd

# Cambiar al directorio de salida. Esto es crucial para que el zip
# no contenga la estructura de carpetas, solo los archivos.
setwd(output_dir)

# Listar los archivos .txt que se van a comprimir.
files_to_zip <- list.files(pattern = "\\.txt$")
files_to_zip

# Usamos un bloque tryCatch para asegurarnos de que el directorio de trabajo
# se restaure incluso si ocurre un error durante la compresión.
tryCatch({
  # Crear el archivo zip usando la función base de R.
  zip(zipfile = zip_filename, files = files_to_zip)
  
  print(paste("¡Archivos comprimidos exitosamente en:", file.path(getwd(), zip_filename)))
  
}, error = function(e) {
  # Mensaje en caso de error.
  print(paste("Ocurrió un error durante la compresión:", e$message))
  
}, finally = {
  # Volver al directorio de trabajo original.
  setwd(original_wd)
})

# Mostramos los tiempos de procesamiento:
End_Time_S5_0_P6 <- Sys.time()
print(paste("6. Tiempo de guardado de archivos txt y zip en disco: ", as.duration(End_Time_S5_0_P6 - Start_Time_S5_0_P6)))

# ####
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ::: 7. Carga del GTFS y entorno para iniciar tests                         :::
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ####
#
Start_Time_S5_0_P7 <- Sys.time()

# ---- Testeamos con una ruta cualquiera que obligue a tomar la línea ---

# Carga de datos de la línea:
GTFS_a_Testear <- paste(output_dir, "/", zip_filename, sep = "")
Linea <- gtfstools::convert_shapes_to_sf(read_gtfs(GTFS_a_Testear))
Estaciones <- gtfstools::convert_stops_to_sf(read_gtfs(GTFS_a_Testear))

# Verificamos Que haya cargado bien el trazado y las estaciones:
tm_basemap("OpenStreetMap") +
  tm_shape(Linea) +
  tm_lines(col = "darkgreen", lwd = 4) +
  tm_shape(Estaciones) +
  tm_dots(fill = "blue", size = 1)

# Seleccionamos las estaciones de origen y destino:
Estacion_Origen_sf <- Estaciones[1,1]
Estacion_Destino_sf <- Estaciones[nrow(Estaciones),1]

# Cambiamos la proyección para poder hacer el buffer en metros:
Estacion_Origen_utm_sf <- st_transform(Estacion_Origen_sf, crs = 32618)
Estacion_Destino_utm_sf <- st_transform(Estacion_Destino_sf, crs = 32618)

# Hacemos un buffer de 300m
Buffer_Origen_sf <- st_buffer(Estacion_Origen_utm_sf, dist = 300)
Buffer_Destino_sf <- st_buffer(Estacion_Destino_utm_sf, dist = 300)

# Creamos los puntos de manera aleatoria:
Origen_Aleatorio_utm_sf <- st_sample(Buffer_Origen_sf, size = 1)
Destino_Aleatorio_utm_sf <- st_sample(Buffer_Destino_sf, size = 1)

# Transformamos los puntos aleatorios a WGS84:
Origen_Aleatorio_wgs84_sf <- st_transform(Origen_Aleatorio_utm_sf, crs = 4326)  |>
  st_as_sf() |>
  mutate(id = "Origen_1")
Destino_Aleatorio_wgs84_sf <- st_transform(Destino_Aleatorio_utm_sf, crs = 4326) |>
  st_as_sf() |>
  mutate(id = "Destino_1")

# Verificamos dónde quedaron los puntos:
tm_basemap("OpenStreetMap") +
  tm_shape(Linea) +
  tm_lines(col = "darkgreen") +
  tm_shape(Estaciones) +
  tm_dots(fill = "blue", size = 1) +
  tm_shape(Origen_Aleatorio_wgs84_sf) +
  tm_dots(fill = "darkred", size = 1) +
  tm_shape(Destino_Aleatorio_wgs84_sf) +
  tm_dots(fill = "darkgreen", size = 1)

# --- Preparamos la carpeta de tests ---
Carpeta_Tests <- paste(output_dir,"/Test_GTFS/", sep = "")
Carpeta_Tests

if (!dir.exists(Carpeta_Tests)) {
  # Si la carpeta NO existe, créala
  dir.create(Carpeta_Tests)
  print(paste("Carpeta de Tests creada:", Carpeta_Tests))
} else {
  # Si ya existe, solo avisa
  print(paste("La carpeta de Tests ya existe:", Carpeta_Tests))
}

# Vaciamos la carpeta completa:
unlink(paste(Carpeta_Tests, "*", sep = ""))

# Copiamos el GTFS generado, el GTFS Original de Bogotá, y la red de Bogotá a la carpeta de testeo:

# GTFS Generado:
file.copy(
  from = GTFS_a_Testear,
  to = paste(Carpeta_Tests, zip_filename, sep = ""),
  overwrite = TRUE
)
# GTFS Original:
file.copy(
  from = "./Data/1_Sources/5_GTFS/Scn0/GTFS-2025-07-28_Original_Sin_Freq.zip",
  to = paste(output_dir,"/Test_GTFS/GTFS-2025-07-28_Original_Sin_Freq.zip", sep = ""),
  overwrite = TRUE
)
# Red de Bogotá:
file.copy(
  from = "./Data/1_Sources/5_GTFS/Scn0/OSM_Bogota_Extendida.osm.pbf",
  to = paste(output_dir,"/Test_GTFS/OSM_Bogota_Extendida.osm.pbf", sep = ""),
  overwrite = TRUE
)

# Inicializamos Java para r5r de testeo:
# Verificamos la versión de Java instalada, si es que hay alguna.
rJavaEnv::java_check_version_rjava()

# Definimos la ruta donde se guardan los archivos del motor de r5r:
data_path_test_gtfs <- paste(output_dir,"/Test_GTFS/", sep = "")
list.files(data_path_test_gtfs)

# Cargamos el motor de r5r
r5r_core_test_gtfs <- setup_r5(
  data_path = data_path_test_gtfs,
)

# Confirmamos datos de Java:
runtime <- rJava::.jcall("java/lang/Runtime", "Ljava/lang/Runtime;", "getRuntime")
maxMemory <- rJava::.jcall(runtime, "J", "maxMemory") / (1024^2)
totalMemory <- rJava::.jcall(runtime, "J", "totalMemory") / (1024^2)
freeMemory <- rJava::.jcall(runtime, "J", "freeMemory") / (1024^2)
#cat("Cores usados para el test:",r5r_core_test_gtfs$getNumberOfThreads(), "cores.\n")
cat("Máxima memoria: ", maxMemory, "MB\n")
cat("Memoria total asignada: ", totalMemory, "MB\n")
cat("Memoria libre: ", freeMemory, "MB\n")

cat("🔧 Java detecta", num_cores_java, "núcleos del sistema\n")
cat("🔧 Java usará", cores_usar, "núcleos del sistema y", mem_java, "GB de RAM\n")
getOption("java.parameters")

# Mostramos los tiempos de procesamiento:
End_Time_S5_0_P7 <- Sys.time()
print(paste("7. Tiempo de Carga del GTFS y entorno para iniciar tests: ", as.duration(End_Time_S5_0_P7 - Start_Time_S5_0_P7)))

# ####
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ::: 8. Test del GTFS                                                       :::
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ####
#
Start_Time_S5_0_P8 <- Sys.time()

# Carga de datos de calibración
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

# Definimos una hora de partida
Tiempo_de_Partida # Viene de los datos calibrados

# 7.1. Calculo de Isocrona de prueba

# Establecemos el origen de la Isocrona de ejemplo (Arbitrariamente)
Origen_Isocrona <- Estacion_Destino_sf |>
  mutate(
    id = stop_id # Es necesario que una variable se llame "id"
  ) |>
  select(id) |>
  st_transform(crs = 4326)
Origen_Isocrona

# Calculamos la isocrona de Ejemplo usando r5r
Shape_Isocrona_Test_GTFS <- isochrone(
  r5r_network = r5r_core_test_gtfs,
  origins = Origen_Isocrona,
  departure_datetime = Tiempo_de_Partida,
  mode = "TRANSIT", # WALK, TRANSIT, BICYCLE
  cutoffs = c(15,30), #c(15, 30)
  polygon_output = TRUE,
  walk_speed = Optimo_Walk_Speed,
  max_walk_time = Optimo_Max_Walk_Time,
  sample_size = 1,
  n_threads = cores_usar
)
Shape_Isocrona_Test_GTFS

# Mostramos las Isocronas
tm_basemap("OpenStreetMap") +
  tm_shape(st_make_valid(Shape_Isocrona_Test_GTFS)) +
  tm_polygons(fill = "darkred", fill_alpha = 0.25) +
  tm_shape(Origen_Isocrona) +
  tm_dots(fill = "darkgreen", size = 1)

# 7.1. Calculo de Isocrona de prueba

Shape_UPLs <- st_read("./Data/2_Processing/1_Empty_Shapes/Shape_Vacio_UPLs.gpkg")
Itinerarios_sf <- detailed_itineraries(
  r5r_network = r5r_core_test_gtfs,
  origins = Origen_Aleatorio_wgs84_sf,
  destinations = Destino_Aleatorio_wgs84_sf,
# destinations = Shape_UPLs |> st_transform(crs = 4326) |> filter(NOM_UPL == "San Cristóbal") |> st_centroid() |> select(id = COD_UPL),
  mode = c("WALK", "TRANSIT"),
  departure_datetime = Tiempo_de_Partida,
  max_walk_time = Optimo_Max_Walk_Time*4,
  walk_speed = Optimo_Walk_Speed,
  max_trip_duration = 240,
  shortest_path = FALSE,
  drop_geometry = FALSE,
  n_threads = cores_usar
  )
Itinerarios_sf

# Mostramos los itinerarios:
tm_basemap("OpenStreetMap") +
  tm_shape(Linea) +
  tm_lines(col = "darkgreen") +
  tm_shape(Estaciones) +
  tm_dots(fill = "blue", size = 1) +
  tm_shape(Origen_Aleatorio_wgs84_sf) +
  tm_dots(fill = "darkred", size = 1) +
  tm_shape(Destino_Aleatorio_wgs84_sf) +
  tm_dots(fill = "darkgreen", size = 1) +
  tm_shape(Itinerarios_sf) +
  tm_lines(col = "mode", lwd = 5)

# Confirmamos datos de Java:
runtime <- rJava::.jcall("java/lang/Runtime", "Ljava/lang/Runtime;", "getRuntime")
maxMemory <- rJava::.jcall(runtime, "J", "maxMemory") / (1024^2)
totalMemory <- rJava::.jcall(runtime, "J", "totalMemory") / (1024^2)
freeMemory <- rJava::.jcall(runtime, "J", "freeMemory") / (1024^2)
#cat("Cores usados para el test:",r5r_core_test_gtfs$getNumberOfThreads(), "cores.\n")
cat("Máxima memoria: ", maxMemory, "MB\n")
cat("Memoria total asignada: ", totalMemory, "MB\n")
cat("Memoria libre: ", freeMemory, "MB\n")

# Detenemos r5r:
#r5r_core_test_gtfs$getNumberOfThreads()
r5r::stop_r5(r5r_core_test_gtfs)
rJava::.jgc(R.gc = TRUE)

# Mostramos los tiempos de procesamiento:
End_Time_S5_0_P8 <- Sys.time()
print(paste("8. Tiempo testeo del GTFS: ", as.duration(End_Time_S5_0_P8 - Start_Time_S5_0_P8)))

# ####
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ::: 9. Tiempos de procesamiento del script                                 :::
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ####
#
End_Time_S5_0_P0 <- Sys.time()

print(paste("1. Tiempo de inicialización: ", as.duration(End_Time_S5_0_P1 - Start_Time_S5_0_P1)))
print(paste("2. Tiempo de declaración de variables de entrada del GFTS: ", as.duration(End_Time_S5_0_P2 - Start_Time_S5_0_P1)))
print(paste("3. Tiempo de Inicio de la generación de tablas para el GTFS: ", as.duration(End_Time_S5_0_P3 - Start_Time_S5_0_P3)))
print(paste("4. Tiempo de creación de archivos agency, routes, calendar y feed_info: ", as.duration(End_Time_S5_0_P4 - Start_Time_S5_0_P4)))
print(paste("5. Tiempo de creación de trips y stop_times: ", as.duration(End_Time_S5_0_P5 - Start_Time_S5_0_P5)))
print(paste("6. Tiempo de guardado de archivos txt y zip en disco: ", as.duration(End_Time_S5_0_P6 - Start_Time_S5_0_P6)))
print(paste("7. Tiempo de Carga del GTFS y entorno para iniciar tests: ", as.duration(End_Time_S5_0_P7 - Start_Time_S5_0_P7)))
print(paste("8. Tiempo testeo del GTFS: ", as.duration(End_Time_S5_0_P8 - Start_Time_S5_0_P8)))
print(paste("9. Tiempo de procesamiento del script:: ", as.duration(End_Time_S5_0_P0 - Start_Time_S5_0_P0)))
Avisar()

