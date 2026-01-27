# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# :::                                                                        :::
# :::                       SCRIPT DE RED FERREA                             :::
# :::                                                                        :::
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#
# Nombre: 8_Red_Ferrea.R
#
# Descripcion: Este Script muestra la Red Férrea
#

# ####
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# :::                      Contenido de este Script                          :::
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ####
#
# 1. Inicialización
# 2. Carga de datos de origen
# 4. Cálculo de itinerarios de testeo


# 12. Carga de datos del Script y detención de r5r y Java
# 13. Tiempo de procesamiento del script
#
Start_Time_S8_P0 <- Sys.time()

# ####
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ::: 1. Inicialización                                                      :::
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ####
#
Start_Time_S8_P1 <- Sys.time()

# Cargamos el script de inicialización general
source("0_Initialization.R")

# Tiempo de procesamiento:
End_Time_S8_P1 <- Sys.time()
print(paste("1. Tiempo de inicialización: ", as.duration(End_Time_S8_P1 - Start_Time_S8_P1)))

# ####
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ::: 2. Carga de datos de origen                                            :::
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ####
#
Start_Time_S8_P2 <- Sys.time()

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

Shape_Municipio_Original <- st_read("./Data/1_Sources/2_Population/Shapes/MGN2021_URB_AREA_CENSAL/MGN_URB_AREA_CENSAL.shp") # Municipio
Shape_Soacha <- Shape_Municipio_Original |>
  filter(NOM_CPOB == "SOACHA") |>
  select(NOM_CPOB)

# Tiempo de procesamiento:
End_Time_S8_P2 <- Sys.time()
print(paste("2. Tiempo de carga de datos de origen: ", as.duration(End_Time_S8_P2 - Start_Time_S8_P2)))

# ####
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ::: 3. Alistamiento de los datos a plotear                                 :::
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ####
#
Start_Time_S8_P3 <- Sys.time()

# Carga de datos del sistema férreo:
GTFS_Bogota <- gtfstools::read_gtfs("./Data/1_Sources/5_GTFS/GTFS_Bogota_2024-07-10_stop_times.zip")
GTFS_M1_Mod <- gtfstools::read_gtfs("./Data/1_Sources/6_Accesibility/Scn7/r5r/GTFS_M1-Modificada.zip")
GTFS_M1_Ext <- gtfstools::read_gtfs("./Data/1_Sources/6_Accesibility/Scn7/r5r/GTFS_M1-Extensiones.zip")
GTFS_M2_Mod <- gtfstools::read_gtfs("./Data/1_Sources/6_Accesibility/Scn7/r5r/GTFS_M2-Modificada.zip")
GTFS_M2_Ext <- gtfstools::read_gtfs("./Data/1_Sources/6_Accesibility/Scn7/r5r/GTFS_M2-Extensiones.zip")
GTFS_M3_Boyaca <- gtfstools::read_gtfs("./Data/1_Sources/6_Accesibility/Scn7/r5r/GTFS_M3-Boyacá.zip")
GTFS_M4_Cali <- gtfstools::read_gtfs("./Data/1_Sources/6_Accesibility/Scn7/r5r/GTFS_M4-Cali.zip")
GTFS_RE_Norte <- gtfstools::read_gtfs("./Data/1_Sources/6_Accesibility/Scn7/r5r/GTFS_RE-Norte.zip")
GTFS_RE_Occidente <- gtfstools::read_gtfs("./Data/1_Sources/6_Accesibility/Scn7/r5r/GTFS_RE-Occidente.zip")
GTFS_RE_Sur <- gtfstools::read_gtfs("./Data/1_Sources/6_Accesibility/Scn7/r5r/GTFS_RE-Sur.zip")
GTFS_T1_K7 <- gtfstools::read_gtfs("./Data/1_Sources/6_Accesibility/Scn7/r5r/GTFS_T1-K7.zip")
GTFS_T2_ElDorado <- gtfstools::read_gtfs("./Data/1_Sources/6_Accesibility/Scn7/r5r/GTFS_T2-ElDorado.zip")

# Extraemos los componentes de cada GTFS
Lineas_GTFS_M1_Mod <- gtfstools::convert_shapes_to_sf(GTFS_M1_Mod) |> st_transform(crs = 3857)
Estaciones_GTFS_M1_Mod <- gtfstools::convert_stops_to_sf(GTFS_M1_Mod) |> st_transform(crs = 3857)
Lineas_GTFS_M1_Ext <- gtfstools::convert_shapes_to_sf(GTFS_M1_Ext) |> st_transform(crs = 3857)
Estaciones_GTFS_M1_Ext <- gtfstools::convert_stops_to_sf(GTFS_M1_Ext) |> st_transform(crs = 3857)
Lineas_GTFS_M2_Mod <- gtfstools::convert_shapes_to_sf(GTFS_M2_Mod) |> st_transform(crs = 3857)
Estaciones_GTFS_M2_Mod <- gtfstools::convert_stops_to_sf(GTFS_M2_Mod) |> st_transform(crs = 3857)
Lineas_GTFS_M2_Ext <- gtfstools::convert_shapes_to_sf(GTFS_M2_Ext) |> st_transform(crs = 3857)
Estaciones_GTFS_M2_Ext <- gtfstools::convert_stops_to_sf(GTFS_M2_Ext) |> st_transform(crs = 3857)
Lineas_GTFS_M3_Boyaca <- gtfstools::convert_shapes_to_sf(GTFS_M3_Boyaca) |> st_transform(crs = 3857)
Estaciones_GTFS_M3_Boyaca <- gtfstools::convert_stops_to_sf(GTFS_M3_Boyaca) |> st_transform(crs = 3857)
Lineas_GTFS_M4_Cali <- gtfstools::convert_shapes_to_sf(GTFS_M4_Cali) |> st_transform(crs = 3857)
Estaciones_GTFS_M4_Cali <- gtfstools::convert_stops_to_sf(GTFS_M4_Cali) |> st_transform(crs = 3857)
Lineas_GTFS_RE_Norte <- gtfstools::convert_shapes_to_sf(GTFS_RE_Norte) |> st_transform(crs = 3857)
Estaciones_GTFS_RE_Norte <- gtfstools::convert_stops_to_sf(GTFS_RE_Norte) |> st_transform(crs = 3857)
Lineas_GTFS_RE_Norte <- gtfstools::convert_shapes_to_sf(GTFS_RE_Norte) |> st_transform(crs = 3857)
Estaciones_GTFS_RE_Norte <- gtfstools::convert_stops_to_sf(GTFS_RE_Norte) |> st_transform(crs = 3857)
Lineas_GTFS_RE_Occidente <- gtfstools::convert_shapes_to_sf(GTFS_RE_Occidente) |> st_transform(crs = 3857)
Estaciones_GTFS_RE_Occidente <- gtfstools::convert_stops_to_sf(GTFS_RE_Occidente) |> st_transform(crs = 3857)
Lineas_GTFS_RE_Sur <- gtfstools::convert_shapes_to_sf(GTFS_RE_Sur) |> st_transform(crs = 3857)
Estaciones_GTFS_RE_Sur <- gtfstools::convert_stops_to_sf(GTFS_RE_Sur) |> st_transform(crs = 3857)
Lineas_GTFS_T1_K7 <- gtfstools::convert_shapes_to_sf(GTFS_T1_K7) |> st_transform(crs = 3857)
Estaciones_GTFS_T1_K7 <- gtfstools::convert_stops_to_sf(GTFS_T1_K7) |> st_transform(crs = 3857)
Lineas_GTFS_T2_ElDorado <- gtfstools::convert_shapes_to_sf(GTFS_T2_ElDorado) |> st_transform(crs = 3857)
Estaciones_GTFS_T2_ElDorado <- gtfstools::convert_stops_to_sf(GTFS_T2_ElDorado) |> st_transform(crs = 3857)

# --- Extraemos la información de Transmilenio del GTFS Oficial: ---
# Transmilenio
Lineas_GTFS_Bogota <- gtfstools::convert_shapes_to_sf(GTFS_Bogota) |> st_transform(crs = 3857)
Estaciones_GTFS_Bogota <- gtfstools::convert_stops_to_sf(GTFS_Bogota) |> st_transform(crs = 3857)

GTFS_Bogota$agency

# Algunas rutas al mostrarse en el mapa, están medio crazy. Las descartamos.
Rutas_Locas <- c("T_2157", "T_2121", "T_2316", "T_2314", "T_2315", "T_2317")
Rutas_Locas

# Extraemos las rutas de Transmi:
Rutas_Transmi <- GTFS_Bogota$routes |> filter(agency_id == "1" & !route_id %in% Rutas_Locas) |> select(route_id)
Rutas_Transmi

# Extraemos las líneas
Lineas_GTFS_Transmi <- Lineas_GTFS_Bogota |> filter(shape_id %in% Rutas_Transmi$route_id)
Lineas_GTFS_Transmi

# Extraemos las paradas de Transmi
GTFS_Bogota$routes #route_id
GTFS_Bogota$trips #trip_id, route_id
GTFS_Bogota$stop_times #trip_id, stop_id
GTFS_Bogota$stops # stop_id, lat, lon

Trips_Transmi <- GTFS_Bogota$trips |> filter(route_id %in% Rutas_Transmi$route_id & !route_id %in% Rutas_Locas) |> select(trip_id)
Trips_Transmi
Stop_Ids_Transmi <- GTFS_Bogota$stop_times |> filter(trip_id %in% Trips_Transmi$trip_id) |> select(stop_id)
Stop_Ids_Transmi
Stops_Transmi <- GTFS_Bogota$stops |> filter(stop_id %in% Stop_Ids_Transmi$stop_id) |>
  st_as_sf(coords = c("stop_lon", "stop_lat"), crs = 4326) |> # CRS Original: 4326
  st_transform(crs = 9377) # CRS en Metros
Stops_Transmi

# Fusionamos las estaciones a menos de 200m:
Matriz_Distancias <- st_distance(Stops_Transmi)
Matriz_Distancias

# Creamos un cluster. La función 'as.dist()' convierte la matriz para que hclust() la entienda
Cluster <- hclust(as.dist(Matriz_Distancias), method = "complete")
Cluster

#    h = 200 significa "agrupa todo lo que esté a menos de 200m"
Grupos <- cutree(Cluster, h = 200) # 200 de distancia

# Fusionar los datos:
Estaciones_Fusionadas <- Stops_Transmi |>
  mutate(cluster_id = Grupos) |>
  group_by(cluster_id) |>
  summarise(
    # Calculamos el centroide del grupo de puntos
    geometry = st_centroid(st_union(geometry)),
    # Opcional: Contar cuántas paradas se fusionaron
    num_paradas = n(),
    # Opcional: Concatenar nombres si tienes una columna 'nombre'
    # nombres_agrupados = paste(nombre_estacion, collapse = ", ")
  ) |>
  # (Opcional) Volver a coordenadas geográficas si lo necesitas para web maps
  st_transform(crs = 4326)
Estaciones_Fusionadas

# Terminamos:
Estaciones_GTFS_Transmi <- Estaciones_Fusionadas
Estaciones_GTFS_Transmi

# --- Fin de extracción de líneas y estaciones de Transmi ---

# Tiempo de procesamiento:
End_Time_S8_P3 <- Sys.time()
print(paste("3. Tiempo de alistamiento de los datos a plotear: ", as.duration(End_Time_S8_P3 - Start_Time_S8_P3)))

# ####
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ::: 4. Ploteo de los mapas de los trazados                                 :::
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ####
#
Start_Time_S8_P4 <- Sys.time()

# Verificamos la red férrea:
tm_basemap("OpenStreetMap") +
  tm_shape(Lineas_GTFS_Transmi) +
  tm_lines(col = "darkgreen", lwd = 3) +
  tm_shape(Estaciones_GTFS_Transmi) +
  tm_dots(fill = "darkgreen", size = 0.8)

# Seleccionamos solo las manzanas del ID_Mapa de interés:
Shape_UPL_Fondo <- Shape_UPLs |>
  filter(ZONA_UPL != "Rural") |>
  st_transform(crs = 3857) # 3857: Proyección CartoDB/Google
Shape_UPL_Fondo

# Plot principal
Plot_Mapa <- ggplot() +
  # Capa de fondo blanco
  geom_sf(
    data = Shape_UPL_Fondo,
    col = "white",
    linewidth = 0.8,
    fill = "white"
  ) +
  
  # Capa de manzanas grises
  geom_sf(
    data = Shape_Manzanas |> filter(COD_UPL %in% Shape_UPL_Fondo$COD_UPL), # No rurales
    col = "white",
    linewidth = 0.1,
    fill = "darkgray"
  ) +
  
  # La capa de Accesibilidad:
  # geom_sf(
  #   data = Shape_Plotear_Acceso_Scn6_Scn1, # |> filter(COD_UPL == "15"),
  #   col = "white",
  #   linewidth = 0.1,
  #   aes(fill = accesibilidad_decil)
  # ) + 
  # scale_fill_manual(
  #   name = " ", # Accesibilidad (Decil)
  #   values = mi_paleta_gradiente 
  # ) +
  # 
  # # Red Transmilenio:
  # geom_sf(
  #   data = Lineas_GTFS_Transmi,
  #   color = "darkred",  # Color del borde de las manzanas
  #   size = 0.5,        # Grosor del borde
  #   alpha = 0.3        # Transparencia
  # ) +
  # # Estaciones:
  # geom_sf(
  #   data = Estaciones_GTFS_Transmi,
  #   col = "darkred",
  #   size = 1.5,
  #   alpha = 0.3
  # ) +
  # geom_sf(
  #   data = Estaciones_GTFS_Transmi,
  #   col = "white",
  #   size = 0.75,
  #   alpha = 1
  # ) +
  # 
  # # Línea férrea: M1_Mod
  # geom_sf(
  #   data = Lineas_GTFS_M1_Mod,
  #   color = "#e6007e",  # Color del borde de las manzanas
  #   size = 1.5,        # Grosor del borde
  #   alpha = 1        # Transparencia
  # ) +
  # # Estaciones:
  # geom_sf(
  #   data = Estaciones_GTFS_M1_Mod,
  #   col = "#e6007e",
  #   size = 3
  # ) +
  # geom_sf(
  #   data = Estaciones_GTFS_M1_Mod,
  #   col = "white",
  #   size = 1
  # ) +
  # 
  # # Línea férrea: M1_Ext
  # geom_sf(
  #   data = Lineas_GTFS_M1_Ext,
  #   color = "#e6007e",  # Color del borde de las manzanas
  #   size = 1.5,        # Grosor del borde
  #   alpha = 1        # Transparencia
  # ) +
  # # Estaciones:
  # geom_sf(
  #   data = Estaciones_GTFS_M1_Ext,
  #   col = "#e6007e",
  #   size = 3
  # ) +
  # geom_sf(
  #   data = Estaciones_GTFS_M1_Ext,
  #   col = "white",
  #   size = 1
  # ) +
  # 
  # # Línea férrea: M3_Boyaca
  # geom_sf(
  #   data = Lineas_GTFS_M3_Boyaca,
  #   color = "#006633",  # Color del borde de las manzanas
  #   size = 1.5,        # Grosor del borde
  #   alpha = 1        # Transparencia
  # ) +
  # # Estaciones:
  # geom_sf(
  #   data = Estaciones_GTFS_M3_Boyaca,
  #   col = "#006633",
  #   size = 3
  # ) +
  # geom_sf(
  #   data = Estaciones_GTFS_M3_Boyaca,
  #   col = "white",
  #   size = 1
  # ) +
  # 
  # # # Línea férrea: M4_Cali
  # geom_sf(
  #   data = Lineas_GTFS_M4_Cali,
  #   color = "#b17f4a",  # Color del borde de las manzanas
  #   size = 1.5,        # Grosor del borde
  #   alpha = 1        # Transparencia
  # ) +
  # # Estaciones:
  # geom_sf(
  #   data = Estaciones_GTFS_M4_Cali,
  #   col = "#b17f4a",
  #   size = 3
  # ) +
  # geom_sf(
  #   data = Estaciones_GTFS_M4_Cali,
  #   col = "white",
  #   size = 1
  # ) +
  # 
  #   # Línea férrea: M2_Mod
  # geom_sf(
  #   data = Lineas_GTFS_M2_Mod,
  #   color = "#1d71b8",  # Color del borde de las manzanas
  #   size = 1.5,        # Grosor del borde
  #   alpha = 1        # Transparencia
  # ) +
  # # Estaciones:
  # geom_sf(
  #   data = Estaciones_GTFS_M2_Mod,
  #   col = "#1d71b8",
  #   size = 3
  # ) +
  # geom_sf(
  #   data = Estaciones_GTFS_M2_Mod,
  #   col = "white",
  #   size = 1
  # ) +
  # 
  # # # # Línea férrea: M2_Ext
  # geom_sf(
  #   data = Lineas_GTFS_M2_Ext,
  #   color = "#1d71b8",  # Color del borde de las manzanas
  #   size = 1.5,        # Grosor del borde
  #   alpha = 1        # Transparencia
  # ) +
  # # Estaciones:
  # geom_sf(
  #   data = Estaciones_GTFS_M2_Ext,
  #   col = "#1d71b8",
  #   size = 3
  # ) +
  # geom_sf(
  #   data = Estaciones_GTFS_M2_Ext,
  #   col = "white",
  #   size = 1
  # ) +
  # 
  # # # Línea férrea: RE_Norte
  # geom_sf(
  #   data = Lineas_GTFS_RE_Norte,
  #   color = "#009640",  # Color del borde de las manzanas
  #   size = 1.5,        # Grosor del borde
  #   alpha = 1        # Transparencia
  # ) +
  # # Estaciones:
  # geom_sf(
  #   data = Estaciones_GTFS_RE_Norte,
  #   col = "#009640",
  #   size = 3
  # ) +
  # geom_sf(
  #   data = Estaciones_GTFS_RE_Norte,
  #   col = "white",
  #   size = 1
  # ) +
  # 
  # # # Línea férrea: RE_Occidente
  # geom_sf(
  #   data = Lineas_GTFS_RE_Occidente,
  #   color = "#f39200",  # Color del borde de las manzanas
  #   size = 1.5,        # Grosor del borde
  #   alpha = 1        # Transparencia
  # ) +
  # # Estaciones:
  # geom_sf(
  #   data = Estaciones_GTFS_RE_Occidente,
  #   col = "#f39200",
  #   size = 3
  # ) +
  # geom_sf(
  #   data = Estaciones_GTFS_RE_Occidente,
  #   col = "white",
  #   size = 1
  # ) +
  # 
  # # # Línea férrea: RE_Sur
  # geom_sf(
  #   data = Shape_Soacha,
  #   col = "darkgray",
  #   linewidth = 0.8,
  #   fill = "white"
  # )+
  # geom_sf(
  #   data = Lineas_GTFS_RE_Sur,
  #   color = "#662483",  # Color del borde de las manzanas
  #   size = 1.5,        # Grosor del borde
  #   alpha = 1        # Transparencia
  # ) +
  # # Estaciones:
  # geom_sf(
  #   data = Estaciones_GTFS_RE_Sur,
  #   col = "#662483",
  #   size = 3
  # ) +
  # geom_sf(
  #   data = Estaciones_GTFS_RE_Sur,
  #   col = "white",
  #   size = 1
  # ) +
  # 
  # # # Línea férrea: T1_K7
  # geom_sf(
  #   data = Lineas_GTFS_T1_K7,
  #   color = "#951b81",  # Color del borde de las manzanas
  #   size = 1.5,        # Grosor del borde
  #   alpha = 1        # Transparencia
  # ) +
  # # Estaciones:
  # geom_sf(
  #   data = Estaciones_GTFS_T1_K7,
  #   col = "#951b81",
  #   size = 3
  # ) +
  # geom_sf(
  #   data = Estaciones_GTFS_T1_K7,
  #   col = "white",
  #   size = 1
  # ) +
  # 
  # # # Línea férrea: T2_ElDorado
  # geom_sf(
  #   data = Lineas_GTFS_T2_ElDorado,
  #   color = "#3aaa35",  # Color del borde de las manzanas
  #   size = 1.5,        # Grosor del borde
  #   alpha = 1        # Transparencia
  # ) +
  # # Estaciones:
  # geom_sf(
  #   data = Estaciones_GTFS_T2_ElDorado,
  #   col = "#3aaa35",
  #   size = 3
  # ) +
  # geom_sf(
  #   data = Estaciones_GTFS_T2_ElDorado,
  #   col = "white",
  #   size = 1
  # ) +
  
  # Estética (Quitar ejes y fondo gris):
  theme_void() +
  theme(
    legend.position = "none", # Ajusta esto para mover la leyenda (0 a 1)
    plot.margin = margin(0,0,0,0)   # Márgenes cero
  )
Plot_Mapa

Nombre_Linea <- "Portada_Gris"

# Guardamos el mapa como imagen:
ggsave(
  filename = paste0("Mapa_Trazado_", Nombre_Linea, ".png"),
  plot = Plot_Mapa,
  device = NULL,
  path = "./Data/3_Results/8_Red_Ferrea/",
  scale = 1,
  width = 2190,
  height = 3840,
  units = c("px"),
  dpi = 150,
  limitsize = TRUE,
  bg = NULL,
  create.dir = FALSE,
)
# Ajustar imagen:
image_write(image_trim(image_read(paste0("./Data/3_Results/8_Red_Ferrea/Mapa_Trazado_", Nombre_Linea, ".png"))), path = paste0("./Data/3_Results/8_Red_Ferrea/Mapa_Trazado_", Nombre_Linea, ".png"), format = "png")

# Tiempo de procesamiento:
End_Time_S8_P4 <- Sys.time()
print(paste("4. Tiempo Ploteo de los mapas de los trazados: ", as.duration(End_Time_S8_P4 - Start_Time_S8_P4)))

# ####
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ::: 5. Diferencia en accesibilidad por estrato                             :::
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ####
#
Start_Time_S8_P5 <- Sys.time()

# No rurales:
No_Rural <- Shape_UPLs |> st_drop_geometry() |> filter(ZONA_UPL != "Rural") |> select(COD_UPL)

# Cargamos datos de los Escenarios:
load("./Data/2_Processing/6_Accesibility/Scn1/Acceso_Normalizado_MNZ_Scn1.RData")
load("./Data/2_Processing/6_Accesibility/Scn2/Acceso_Normalizado_MNZ_Scn2.RData")
load("./Data/2_Processing/6_Accesibility/Scn3/Acceso_Normalizado_MNZ_Scn3.RData")
load("./Data/2_Processing/6_Accesibility/Scn4/Acceso_Normalizado_MNZ_Scn4.RData")
load("./Data/2_Processing/6_Accesibility/Scn5/Acceso_Normalizado_MNZ_Scn5.RData")
load("./Data/2_Processing/6_Accesibility/Scn6/Acceso_Normalizado_MNZ_Scn6.RData")
load("./Data/2_Processing/6_Accesibility/Scn7/Acceso_Normalizado_MNZ_Scn7.RData")
Acceso_Equipamientos_MNZ_Scn1_sf
Acceso_Equipamientos_MNZ_Scn2_sf
Acceso_Equipamientos_MNZ_Scn3_sf
Acceso_Equipamientos_MNZ_Scn4_sf
Acceso_Equipamientos_MNZ_Scn5_sf
Acceso_Equipamientos_MNZ_Scn6_sf
Acceso_Equipamientos_MNZ_Scn7_sf

# Cargamos Shape de Estratificación:
Shape_Estratificacion_Original <- st_read("./Data/1_Sources/1_Shapes/Custom/Shape_Estratificacion_Original.gpkg")

# Unimos espacialmente para tener un sf estratificado por MNZ (Se hacer espacialmente porque los ids de las fuentes no coinciden):
# Se demora! No correr si no es necesario:
# Estratos_MNZ_sf <- st_join(
#   Shape_Manzanas |> filter(COD_UPL %in% No_Rural$COD_UPL) |> select(COD_MNZ, COD_UPL), 
#   Shape_Estratificacion_Original |> select(ESTRATO) |> st_transform(crs = st_crs(Shape_Manzanas)), 
#   largest = TRUE
# )
# Estratos_MNZ_sf

# Guardamos para no repetir el proceso
# save(
#   Estratos_MNZ_sf,
#   file = "./Data/2_Processing/1_Empty_Shapes/Estratos_MNZ_sf.RData"
# )
load("./Data/2_Processing/1_Empty_Shapes/Estratos_MNZ_sf.RData")
Estratos_MNZ_sf

# Creamos el df de Diferencia de Accesibilidad.
Diferencia_Accesibilidad_MNZ_df <- Acceso_Equipamientos_MNZ_Scn1_sf |>
  # Hacemos un left_join para tener a todos juntitos:
  st_drop_geometry() |>
  filter(COD_UPL %in% No_Rural$COD_UPL) |>
  select(COD_MNZ, cutoff, Total_Amenities_Scn1 = Total_Amenities) |>
  # Unimos al Scn2:
  left_join(
    Acceso_Equipamientos_MNZ_Scn2_sf |>
      st_drop_geometry() |>
      filter(COD_UPL %in% No_Rural$COD_UPL) |>
      select(COD_MNZ, cutoff, Total_Amenities_Scn2 = Total_Amenities)
  ) |>
  # Unimos al Scn3:
  left_join(
    Acceso_Equipamientos_MNZ_Scn3_sf |>
      st_drop_geometry() |>
      filter(COD_UPL %in% No_Rural$COD_UPL) |>
      select(COD_MNZ, cutoff, Total_Amenities_Scn3 = Total_Amenities)
  ) |>
  # Unimos al Scn4:
  left_join(
    Acceso_Equipamientos_MNZ_Scn4_sf |>
      st_drop_geometry() |>
      filter(COD_UPL %in% No_Rural$COD_UPL) |>
      select(COD_MNZ, cutoff, Total_Amenities_Scn4 = Total_Amenities)
  ) |>
  # Unimos al Scn5:
  left_join(
    Acceso_Equipamientos_MNZ_Scn5_sf |>
      st_drop_geometry() |>
      filter(COD_UPL %in% No_Rural$COD_UPL) |>
      select(COD_MNZ, cutoff, Total_Amenities_Scn5 = Total_Amenities)
  ) |>
  # Unimos al Scn6:
  left_join(
    Acceso_Equipamientos_MNZ_Scn6_sf |>
      st_drop_geometry() |>
      filter(COD_UPL %in% No_Rural$COD_UPL) |>
      select(COD_MNZ, cutoff, Total_Amenities_Scn6 = Total_Amenities)
  ) |>
  # Unimos al Scn7:
  left_join(
    Acceso_Equipamientos_MNZ_Scn7_sf |>
      st_drop_geometry() |>
      filter(COD_UPL %in% No_Rural$COD_UPL) |>
      select(COD_MNZ, cutoff, Total_Amenities_Scn7 = Total_Amenities)
  ) |>
  # Calculamos las diferencias:
  mutate(
    Diff_Scn2 = Total_Amenities_Scn2 - Total_Amenities_Scn1,
    Diff_Scn3 = Total_Amenities_Scn3 - Total_Amenities_Scn1,
    Diff_Scn4 = Total_Amenities_Scn4 - Total_Amenities_Scn1,
    Diff_Scn5 = Total_Amenities_Scn5 - Total_Amenities_Scn1,
    Diff_Scn6 = Total_Amenities_Scn6 - Total_Amenities_Scn1,
    Diff_Scn7 = Total_Amenities_Scn7 - Total_Amenities_Scn1
  ) |>
  # Agregamos los estratos
  left_join(Estratos_MNZ_sf |> st_drop_geometry()) |>
  select(COD_UPL, COD_MNZ, ESTRATO, cutoff, Total_Amenities_Scn1:Total_Amenities_Scn7, Diff_Scn2:Diff_Scn7) |>
  as_tibble()
Diferencia_Accesibilidad_MNZ_df

# Test del por qué el estrato 3 en 15 y 30 minuto tienen el mismo porcentaje (?):
Diferencia_Accesibilidad_MNZ_df |>
  select(ESTRATO, cutoff, Total_Amenities_Scn1, Total_Amenities_Scn4) |>
  filter(ESTRATO == 3) |>
  group_by(cutoff) |>
  summarise(
    Valor_Scn1 = sum(Total_Amenities_Scn1),
    Valor_Scn4 = sum(Total_Amenities_Scn4),
    Numerador = sum(Total_Amenities_Scn4) - sum(Total_Amenities_Scn1),
    Porcentaje = 100 * Numerador/Valor_Scn1
  )
# Conclusión: ¡Coincidencia!

# --- Cálculo de la diferencia porcentual ---

# Diferencia porcentual acumulada por estrato, en formato ancho:
Diferencias_Porcent_df <- Diferencia_Accesibilidad_MNZ_df |>
  filter(!is.na(ESTRATO)) |>
  group_by(ESTRATO, cutoff) |>
  summarise(
    Numero_Manzanas = n(),
    Diff_Percent_Scn2 = round(100 * (sum(Total_Amenities_Scn2) - sum(Total_Amenities_Scn1)) / sum(Total_Amenities_Scn1), digits = 2),
    Diff_Percent_Scn3 = round(100 * (sum(Total_Amenities_Scn3) - sum(Total_Amenities_Scn1)) / sum(Total_Amenities_Scn1), digits = 2),
    Diff_Percent_Scn4 = round(100 * (sum(Total_Amenities_Scn4) - sum(Total_Amenities_Scn1)) / sum(Total_Amenities_Scn1), digits = 2),
    Diff_Percent_Scn5 = round(100 * (sum(Total_Amenities_Scn5) - sum(Total_Amenities_Scn1)) / sum(Total_Amenities_Scn1), digits = 2),
    Diff_Percent_Scn6 = round(100 * (sum(Total_Amenities_Scn6) - sum(Total_Amenities_Scn1)) / sum(Total_Amenities_Scn1), digits = 2),
    Diff_Percent_Scn7 = round(100 * (sum(Total_Amenities_Scn7) - sum(Total_Amenities_Scn1)) / sum(Total_Amenities_Scn1), digits = 2),
  ) |>
  select(ESTRATO, cutoff, Diff_Percent_Scn2:Diff_Percent_Scn7)
Diferencias_Porcent_df

# La tabla en formato largo
df_long <- Diferencias_Porcent_df |>
  pivot_longer(
    cols = starts_with("Diff"), 
    names_to = "Escenario", 
    values_to = "Diferencia"
  ) |>
  mutate(
    # Limpiamos los nombres para que en el eje X salga "Scn 2" en vez de "Diff_Percent_Scn2"
    Escenario = str_remove(Escenario, "Diff_Percent_"),
    # Convertimos Estrato a Factor para que no lo trate como número continuo
    ESTRATO = factor(ESTRATO)
  )
df_long

# Para cargar Roboto:
showtext_auto()

# Para tests:
Tiempo_Min <- 15

# Evaluamos para 15 y 30 minutos mediante un ciclo para ahorrar trabajo:
for (Tiempo_Min in c(15, 30)) {

  # Creamos mapas de Calor
  Plot_Grafico <- df_long |>
    filter(cutoff == Tiempo_Min) |>
    select(-cutoff) |>
    # Graficamos
    ggplot(
      aes(x = Escenario, y = ESTRATO, fill = Diferencia)
      ) +
    # Bordes blancos:
    geom_tile(color = "white") + # El borde blanco ayuda a separar los cuadros
    # Agregamos textos
    geom_text(aes(
      # 1. accuracy = 0.01 -> Asegura 2 decimales
      # 2. decimal.mark = "," -> Usa coma para decimales (12,34)
      # 3. suffix = "%" -> Le pone el porcentaje al final
      label = scales::number(Diferencia, accuracy = 0.01, decimal.mark = ",", suffix = "%"),
#        label = paste0(sprintf("%.2f", Diferencia), "%"), 
        color = ifelse(Diferencia > 30, "white", "#595959")
        ), 
      family = "Roboto", size = 6, fontface = "bold") +
    # Establecemos escala de color
    scale_fill_gradientn(
      colors = c("lightgray", "darkgreen"),
      values = scales::rescale(c(0, 100))
    ) +
    scale_color_identity() +
    labs(
      x = "Escenario",
      y = "Estrato Socioeconómico",
      fill = paste("% Mejora para", Tiempo_Min, "minutos")
    ) +
    theme_minimal() +
    theme(
      legend.position = "none",
      # Negrita a las etiquetas:
      axis.text = element_text(face = "bold", size = 14), 
      # Negrita a los títulos:
      axis.title = element_text(face = "bold", size = 20),
      panel.background = element_blank(), # Quita el fondo del área de dibujo
      plot.background = element_blank(),  # Quita el fondo de todo el recuadro
      legend.background = element_blank() # Asegura que la leyenda no tenga fondo
    )
  Plot_Grafico
  
  print(paste0("Creando grafica de diferencia porcentual para ", Tiempo_Min, "min..."))
  
  Nombre_Archivo <- paste0("Diferencia_Porcentual_Acceso_",Tiempo_Min,"min.png")
  Ruta_Archivo <- "./Data/3_Results/7_HQSL/2_Diferencia_Escenarios/"
  
  # Guardamos el mapa como imagen:
  ggsave(
    filename = Nombre_Archivo,
    plot = Plot_Grafico,
    device = NULL,
    path = Ruta_Archivo,
    scale = 1,
    width = 1080,
    height = 1080,
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
End_Time_S8_P5 <- Sys.time()
print(paste("5. Tiempo de cálculo en la diferencia en la accesibilidad por estrato: ", as.duration(End_Time_S8_P5 - Start_Time_S8_P5)))

# ####
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ::: 6. Cálculo de la población beneficiada con las intervenciones          :::
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ####
#
Start_Time_S8_P6 <- Sys.time()

# --- Cálculo de población beneficiada ---

# Cargamos información del censo:
Shape_Censo_Manzanas <- st_read("./Data/2_Processing/2_Population/Shape_Poblacion_Manzanas.gpkg")

# Calculamos las personas afectadas:
# Diferencia_Accesibilidad_MNZ_df viene del paso anterior
Personas_Afectadas_df <- Diferencia_Accesibilidad_MNZ_df |>
  filter(!is.na(ESTRATO)) |>
  left_join(
    Shape_Censo_Manzanas |> st_drop_geometry() |> filter(COD_UPL %in% No_Rural$COD_UPL) |> select(COD_MNZ, PERSONAS_MNZ)
  ) |>
  select(COD_MNZ, ESTRATO, cutoff, PERSONAS_MNZ, Diff_Scn2, Diff_Scn3, Diff_Scn4, Diff_Scn5, Diff_Scn6, Diff_Scn7) |>
  mutate(
    PERSONAS_Scn2 = ifelse(Diff_Scn2 > 0, 1, 0) * ifelse(is.na(PERSONAS_MNZ), 0, PERSONAS_MNZ),
    PERSONAS_Scn3 = ifelse(Diff_Scn3 > 0, 1, 0) * ifelse(is.na(PERSONAS_MNZ), 0, PERSONAS_MNZ),
    PERSONAS_Scn4 = ifelse(Diff_Scn4 > 0, 1, 0) * ifelse(is.na(PERSONAS_MNZ), 0, PERSONAS_MNZ),
    PERSONAS_Scn5 = ifelse(Diff_Scn5 > 0, 1, 0) * ifelse(is.na(PERSONAS_MNZ), 0, PERSONAS_MNZ),
    PERSONAS_Scn6 = ifelse(Diff_Scn6 > 0, 1, 0) * ifelse(is.na(PERSONAS_MNZ), 0, PERSONAS_MNZ),
    PERSONAS_Scn7 = ifelse(Diff_Scn7 > 0, 1, 0) * ifelse(is.na(PERSONAS_MNZ), 0, PERSONAS_MNZ),
  ) |>
  group_by(ESTRATO, cutoff) |>
  summarise(
    PERSONAS_Scn2 = sum(PERSONAS_Scn2),
    PERSONAS_Scn3 = sum(PERSONAS_Scn3),
    PERSONAS_Scn4 = sum(PERSONAS_Scn4),
    PERSONAS_Scn5 = sum(PERSONAS_Scn5),
    PERSONAS_Scn6 = sum(PERSONAS_Scn6),
    PERSONAS_Scn7 = sum(PERSONAS_Scn7)
  )
Personas_Afectadas_df

# La tabla en formato largo
Personas_Afectadas_Largo_df <- Personas_Afectadas_df |>
  pivot_longer(
    cols = starts_with("PERSONAS_"), 
    names_to = "Escenario", 
    values_to = "Beneficiadas"
  ) |>
  mutate(
    # Limpiamos los nombres para que en el eje X salga "Scn 2" en vez de "Diff_Percent_Scn2"
    Escenario = str_remove(Escenario, "PERSONAS_"),
    # Convertimos Estrato a Factor para que no lo trate como número continuo
    ESTRATO = factor(ESTRATO)
  )
Personas_Afectadas_Largo_df

# Para cargar Roboto:
showtext_auto()

# Para tests:
Tiempo_Min <- 15

# Evaluamos para 15 y 30 minutos mediante un ciclo para ahorrar trabajo:
for (Tiempo_Min in c(15, 30)) {
  
  # Creamos mapas de Calor
  Plot_Grafico <- Personas_Afectadas_Largo_df |>
    filter(cutoff == Tiempo_Min) |>
    select(-cutoff) |>
    # Graficamos
    ggplot(
      aes(x = Escenario, y = ESTRATO, fill = Beneficiadas)
    ) +
    # Bordes blancos:
    geom_tile(color = "white") + # El borde blanco ayuda a separar los cuadros
    # Agregamos textos
    geom_text(aes(
      # big.mark = "," hará que salga 1,000,000
      # big.mark = "." hará que salga 1.000.000 (Formato Colombia usual)
      # big.mark = "'" hará que salga 1'000'000 (Formato Suiza/Técnico)
      label = scales::comma(Beneficiadas, big.mark = ".", accuracy = 1), #Beneficiadas, 
      color = ifelse(Beneficiadas > 1000000, "white", "#595959")
    ), 
    family = "Roboto", size = 6, fontface = "bold") +
    # Establecemos escala de color
    scale_fill_gradientn(
      colors = c("lightgray", "darkgreen"),
      values = scales::rescale(c(0, 3000000))
    ) +
    scale_color_identity() +
    labs(
      x = "Escenario",
      y = "Estrato Socioeconómico",
      fill = paste("Personas beneficiadas en viajes de", Tiempo_Min, "minutos")
    ) +
    theme_minimal() +
    theme(
      legend.position = "none",
      # Negrita a las etiquetas:
      axis.text = element_text(face = "bold", size = 14), 
      # Negrita a los títulos:
      axis.title = element_text(face = "bold", size = 20),
      panel.background = element_blank(), # Quita el fondo del área de dibujo
      plot.background = element_blank(),  # Quita el fondo de todo el recuadro
      legend.background = element_blank() # Asegura que la leyenda no tenga fondo
    )
  Plot_Grafico
  
  print(paste0("Creando grafica de población beneficiada por estratos para ", Tiempo_Min, "min..."))
  
  Nombre_Archivo <- paste0("Población_Beneficiada_Por_Estratos_",Tiempo_Min,"min.png")
  Ruta_Archivo <- "./Data/3_Results/7_HQSL/2_Diferencia_Escenarios/"
  
  # Guardamos el mapa como imagen:
  ggsave(
    filename = Nombre_Archivo,
    plot = Plot_Grafico,
    device = NULL,
    path = Ruta_Archivo,
    scale = 1,
    width = 1080,
    height = 1080,
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
End_Time_S8_P6 <- Sys.time()
print(paste("6. Tiempo de cálculo de la población beneficiada con las intervenciones: ", as.duration(End_Time_S8_P6 - Start_Time_S8_P6)))

# ####
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ::: 7.  Cálculo de Isocronas comparativas                                  :::
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ####
#
Start_Time_S8_P7 <- Sys.time()

# Cargamos datos de origen:
Shape_Manzanas <- st_read("./Data/2_Processing/1_Empty_Shapes/Shape_Vacio_Manzanas.gpkg")
load("./Data/2_Processing/6_Accesibility/Scn0/Variables_Calibracion_Modelo_RMSE_2025-10-22.RData")
Optimo_Walk_Speed    # 2.82
Optimo_Max_Walk_Time # 10.28
Tiempo_de_Partida    # "2025-04-10 14:04:00 CEST"
cat("Parámetros óptimos encontrados",
    "\n   Walk_speed:", Optimo_Walk_Speed,
    "\n   Max_walk_time:", Optimo_Max_Walk_Time,
    "\n   Time:", as.character(Tiempo_de_Partida)
)

# Java:
cat("🔧 Java detecta", num_cores_java, "núcleos del sistema\n")
cat("🔧 Java usará", cores_usar, "núcleos del sistema y", mem_java, "GB de RAM\n")
getOption("java.parameters")

# Definimos la ruta donde se guardan los archivos del motor de r5r para cada escenario:
data_path_scenario_1 <- "./Data/1_Sources/6_Accesibility/Scn1/r5r/"
data_path_scenario_2 <- "./Data/1_Sources/6_Accesibility/Scn2/r5r/"
data_path_scenario_5 <- "./Data/1_Sources/6_Accesibility/Scn5/r5r/"
list.files(data_path_scenario_1)
list.files(data_path_scenario_2)
list.files(data_path_scenario_5)

# Establecemos el origen de la Isocrona de ejemplo (Arbitrariamente)
Origen_Isocrona <- Shape_UPLs |>
  filter(COD_UPL != "") |> # 15: Porvenir, 16: Eden, 14: Patio Bonito, 23: Centro Histórico
  mutate(
    id = NOM_UPL # Es necesario que una variable se llame "id"
  ) |>
  select(id, NOM_UPL) |>
  st_transform(crs = 4326) |> # Si no se cambia la proyección, r5r no funcionará
  st_centroid()
Origen_Isocrona

# Calculamos las isocronas para los Escenarios

#Escenario 1: Base.
r5r_core_scenario_1 <- setup_r5(data_path = data_path_scenario_1)
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
) |> st_make_valid() |> mutate(AREA = st_area(polygons))
Shape_Isocrona_Ejemplo_Scn1
r5r::stop_r5(r5r_core_scenario_1)

#Escenario 2: Metro.
r5r_core_scenario_2 <- setup_r5(data_path = data_path_scenario_2)
Shape_Isocrona_Ejemplo_Scn2 <- isochrone(
  r5r_network = r5r_core_scenario_2,
  origins = Origen_Isocrona,
  departure_datetime = Tiempo_de_Partida, # Se había evaluado a las 08:00
  mode = "TRANSIT", # WALK, TRANSIT, BICYCLE
  cutoffs = c(15, 30, 45, 60), #c(15, 30)
  polygon_output = TRUE,
  walk_speed = Optimo_Walk_Speed,
  max_walk_time = Optimo_Max_Walk_Time,
  sample_size = 1
) |> st_make_valid() |> mutate(AREA = st_area(polygons))
Shape_Isocrona_Ejemplo_Scn2
r5r::stop_r5(r5r_core_scenario_2)

#Escenario 5: Red Férrea.
r5r_core_scenario_5 <- setup_r5(data_path = data_path_scenario_5)
Shape_Isocrona_Ejemplo_Scn5 <- isochrone(
  r5r_network = r5r_core_scenario_5,
  origins = Origen_Isocrona,
  departure_datetime = Tiempo_de_Partida, # Se había evaluado a las 08:00
  mode = "TRANSIT", # WALK, TRANSIT, BICYCLE
  cutoffs = c(15, 30, 45, 60), #c(15, 30)
  polygon_output = TRUE,
  walk_speed = Optimo_Walk_Speed,
  max_walk_time = Optimo_Max_Walk_Time,
  sample_size = 1
) |> st_make_valid() |> mutate(AREA = st_area(polygons))
Shape_Isocrona_Ejemplo_Scn5
r5r::stop_r5(r5r_core_scenario_5)

# Mostramos las Isocronas
tm_basemap("OpenStreetMap") +
  tm_shape(Shape_UPLs) +
  tm_polygons(fill = "white", fill_alpha = 0.001,  col_alpha = 0.25, col = "darkblue") +
  tm_shape(st_make_valid(Shape_Isocrona_Ejemplo_Scn5 |> filter(isochrone == 60))) +
  tm_polygons(fill = "darkred", fill_alpha = 0.25) +
  tm_shape(st_make_valid(Shape_Isocrona_Ejemplo_Scn2 |> filter(isochrone == 60))) +
  tm_polygons(fill = "darkblue", fill_alpha = 0.25) +
  tm_shape(st_make_valid(Shape_Isocrona_Ejemplo_Scn1 |> filter(isochrone == 60))) +
  tm_polygons(fill = "darkgreen", fill_alpha = 0.25) +
  tm_shape(Origen_Isocrona) +
  tm_dots(fill = "yellow", size = 1)


# Seleccionamos solo las manzanas del ID_Mapa de interés:
Shape_UPL_Fondo <- Shape_UPLs |>
  filter(ZONA_UPL != "Rural") |>
  st_transform(crs = 3857) # 3857: Proyección CartoDB/Google
Shape_UPL_Fondo

# Plot principal
Plot_Mapa <- ggplot() +
  # Capa de fondo blanco
  geom_sf(
    data = Shape_UPL_Fondo,
    col = "white",
    linewidth = 0.8,
    fill = "white"
  ) +
  
  # Capa de manzanas grises
  geom_sf(
    data = Shape_Manzanas |> filter(COD_UPL %in% Shape_UPL_Fondo$COD_UPL), # No rurales
    col = "white",
    linewidth = 0.1,
    fill = "darkgray"
  ) +
  
  # Isochronas:
  geom_sf(
    data = Shape_Isocrona_Ejemplo_Scn5 |> filter(isochrone == 30) |> st_intersection(st_union(Shape_UPL_Fondo |> st_transform(4326))),
    color = "darkgreen",  # Color del borde
    fill = "lightgreen",
    size = 0.5,        # Grosor del borde
    alpha = 0.3        # Transparencia
  ) +

  # Punto de origen:
  geom_sf(
    data = Origen_Isocrona,
    col = "darkred",
    size = 4,
    alpha = 1
  ) +
  geom_sf(
    data = Origen_Isocrona,
    col = "white",
    size = 1.5,
    alpha = 1
  ) +
  
  # Estética (Quitar ejes y fondo gris):
  theme_void() +
  theme(
    legend.position = "none", # Ajusta esto para mover la leyenda (0 a 1)
    plot.margin = margin(0,0,0,0)   # Márgenes cero
  )
#Plot_Mapa

Nombre_Archivo <- "Alcance_Isocronas_Portada.png"
Ruta_Archivo <- "./Data/3_Results/7_HQSL/2_Diferencia_Escenarios/"

# Guardamos el mapa como imagen:
ggsave(
  filename = Nombre_Archivo,
  plot = Plot_Mapa,
  device = NULL,
  path = Ruta_Archivo,
  scale = 1,
  width = 1080,
  height = 1920,
  units = c("px"),
  dpi = 150,
  limitsize = TRUE,
  bg = NULL,
  create.dir = FALSE,
)
# Ajustar imagen:
image_write(image_trim(image_read(paste0(Ruta_Archivo, Nombre_Archivo))), path = paste0(Ruta_Archivo, Nombre_Archivo), format = "png")

# Tiempo de procesamiento:
End_Time_S8_P7 <- Sys.time()
print(paste("7. Tiempo de cálculo de isocronas comparativas: ", as.duration(End_Time_S8_P7 - Start_Time_S8_P7)))









# ####
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ::: ñ. Cálculo de las matrices OD para todos los Scn                       :::
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ####
#
Start_Time_S8_Pñ <- Sys.time()

# --- Alistamos Java ---

# Confirmamos datos de Java:
rJavaEnv::java_check_version_rjava()

# Autoconfiguración de Memoria y núcleos
ram_sistema <- round(as.numeric(memuse::Sys.meminfo()$totalram / (1024^3)))
cores_sistema <- parallel::detectCores()

# Recursos a usar:
mem_java <- floor(ram_sistema * 0.75) # El 50% de la RAM
cores_usar <- cores_sistema - 1 # Todos los cores, menos 1.

options(java.parameters = c(
  paste0("-Xmx", mem_java, "G"),
  paste0("-XX:ActiveProcessorCount=", cores_usar)
))

cat("🔧 Java detecta", num_cores_java, "núcleos del sistema\n")
cat("🔧 Java usará", cores_usar, "núcleos del sistema y", mem_java, "GB de RAM\n")
getOption("java.parameters")

# Definimos las rutas donde se guardan los archivos del motor de r5r:
data_path_scenario_1 <- "./Data/1_Sources/6_Accesibility/Scn1/r5r/"
data_path_scenario_2 <- "./Data/1_Sources/6_Accesibility/Scn2/r5r/"
data_path_scenario_3 <- "./Data/1_Sources/6_Accesibility/Scn3/r5r/"
data_path_scenario_4 <- "./Data/1_Sources/6_Accesibility/Scn4/r5r/"
data_path_scenario_5 <- "./Data/1_Sources/6_Accesibility/Scn5/r5r/"
data_path_scenario_6 <- "./Data/1_Sources/6_Accesibility/Scn6/r5r/"
data_path_scenario_7 <- "./Data/1_Sources/6_Accesibility/Scn7/r5r/"
list.files(data_path_scenario_1)
list.files(data_path_scenario_2)
list.files(data_path_scenario_3)
list.files(data_path_scenario_4)
list.files(data_path_scenario_5)
list.files(data_path_scenario_6)
list.files(data_path_scenario_7)

# Cargamos los datos del escenario base y los resultados de la calibración:
load("./Data/2_Processing/6_Accesibility/Scn0/Variables_Calibracion_Modelo_RMSE_2025-10-22.RData")
Origenes_ZAT <- Origenes_Calibracion_sf
Destinos_ZAT <- Destinos_Calibracion_sf
cat("Parámetros óptimos resultado de la calibración del modelo:",
    "\n   Walk_speed:", Optimo_Walk_Speed,
    "\n   Max_walk_time:", Optimo_Max_Walk_Time,
    "\n   Time:", as.character(Tiempo_de_Partida),
    "\n   Mejor RMSE:", Mejor_RMSE
)

# Cálculo de matrices OD de los escenarios
Origenes_ZAT <- Shape_ZAT |> st_centroid() |> select(id = COD_ZAT) |> st_transform(crs = 4326)
Destinos_ZAT <- Origenes_ZAT

# Scn1:
r5r_core_scenario_1 <- setup_r5(data_path = data_path_scenario_1)
Matriz_OD_Scn1 <- travel_time_matrix(
  r5r_network = r5r_core_scenario_1,
  origins = Origenes_ZAT,
  destinations = Destinos_ZAT,
  mode = "TRANSIT",
  departure_datetime = Tiempo_de_Partida,
  walk_speed = Optimo_Walk_Speed,
  max_walk_time = Optimo_Max_Walk_Time,
)
r5r::stop_r5(r5r_core_scenario_1)
Matriz_OD_Scn1

# Scn2:
r5r_core_scenario_2 <- setup_r5(data_path = data_path_scenario_2)
Matriz_OD_Scn2 <- travel_time_matrix(
  r5r_network = r5r_core_scenario_2,
  origins = Origenes_ZAT,
  destinations = Destinos_ZAT,
  mode = "TRANSIT",
  departure_datetime = Tiempo_de_Partida,
  walk_speed = Optimo_Walk_Speed,
  max_walk_time = Optimo_Max_Walk_Time,
)
r5r::stop_r5(r5r_core_scenario_2)
Matriz_OD_Scn2

# Scn3:
r5r_core_scenario_3 <- setup_r5(data_path = data_path_scenario_3)
Matriz_OD_Scn3 <- travel_time_matrix(
  r5r_network = r5r_core_scenario_3,
  origins = Origenes_ZAT,
  destinations = Destinos_ZAT,
  mode = "TRANSIT",
  departure_datetime = Tiempo_de_Partida,
  walk_speed = Optimo_Walk_Speed,
  max_walk_time = Optimo_Max_Walk_Time,
)
r5r::stop_r5(r5r_core_scenario_3)
Matriz_OD_Scn3

# Scn4:
r5r_core_scenario_4 <- setup_r5(data_path = data_path_scenario_4)
Matriz_OD_Scn4 <- travel_time_matrix(
  r5r_network = r5r_core_scenario_4,
  origins = Origenes_ZAT,
  destinations = Destinos_ZAT,
  mode = "TRANSIT",
  departure_datetime = Tiempo_de_Partida,
  walk_speed = Optimo_Walk_Speed,
  max_walk_time = Optimo_Max_Walk_Time,
)
r5r::stop_r5(r5r_core_scenario_4)
Matriz_OD_Scn4

# Scn5:
r5r_core_scenario_5 <- setup_r5(data_path = data_path_scenario_5)
Matriz_OD_Scn5 <- travel_time_matrix(
  r5r_network = r5r_core_scenario_5,
  origins = Origenes_ZAT,
  destinations = Destinos_ZAT,
  mode = "TRANSIT",
  departure_datetime = Tiempo_de_Partida,
  walk_speed = Optimo_Walk_Speed,
  max_walk_time = Optimo_Max_Walk_Time,
)
r5r::stop_r5(r5r_core_scenario_5)
Matriz_OD_Scn5

# Scn6:
r5r_core_scenario_6 <- setup_r5(data_path = data_path_scenario_6)
Matriz_OD_Scn6 <- travel_time_matrix(
  r5r_network = r5r_core_scenario_6,
  origins = Origenes_ZAT,
  destinations = Destinos_ZAT,
  mode = "TRANSIT",
  departure_datetime = Tiempo_de_Partida,
  walk_speed = Optimo_Walk_Speed,
  max_walk_time = Optimo_Max_Walk_Time,
)
r5r::stop_r5(r5r_core_scenario_6)
Matriz_OD_Scn6

# Scn7:
r5r_core_scenario_7 <- setup_r5(data_path = data_path_scenario_7)
Matriz_OD_Scn7 <- travel_time_matrix(
  r5r_network = r5r_core_scenario_7,
  origins = Origenes_ZAT,
  destinations = Destinos_ZAT,
  mode = "TRANSIT",
  departure_datetime = Tiempo_de_Partida,
  walk_speed = Optimo_Walk_Speed,
  max_walk_time = Optimo_Max_Walk_Time,
)
r5r::stop_r5(r5r_core_scenario_7)
Matriz_OD_Scn7

# Unimos todas las matrices OD en una sola
Matriz_OD_All <- bind_rows(
  Matriz_OD_Scn1 |> mutate(SCN = "Scn1"),
  Matriz_OD_Scn2 |> mutate(SCN = "Scn2"),
  Matriz_OD_Scn3 |> mutate(SCN = "Scn3"),
  Matriz_OD_Scn4 |> mutate(SCN = "Scn4"),
  Matriz_OD_Scn5 |> mutate(SCN = "Scn5"),
  Matriz_OD_Scn6 |> mutate(SCN = "Scn6"),
  Matriz_OD_Scn7 |> mutate(SCN = "Scn7"),
)
Matriz_OD_All

# Guardamos para no repetir el proceso:
save(
  Matriz_OD_Scn1,
  Matriz_OD_Scn2,
  Matriz_OD_Scn3,
  Matriz_OD_Scn4,
  Matriz_OD_Scn5,
  Matriz_OD_Scn6,
  Matriz_OD_Scn7,
  Matriz_OD_All,
  file = "./Data/2_Processing/8_Results/Matrices_OD.RData"
)
load("./Data/2_Processing/8_Results/Matrices_OD.RData")

# --- Limpiamos después de usar r5r_core ---
rJava::.jgc(R.gc = TRUE)

# Tiempo de procesamiento:
End_Time_S8_P5 <- Sys.time()
print(paste("5. Tiempo de cálculo de las matrices OD para todos los Scn: ", as.duration(End_Time_S8_P5 - Start_Time_S8_P5)))


# Boxplot:
ggplot(data = Matriz_OD_All, aes(x = SCN, y = travel_time_p50)) +
  
  # 1. El Boxplot
  # El 'fill = SCN' es opcional, pero ayuda a diferenciar visualmente con colores
  geom_boxplot(aes(fill = SCN), outlier.alpha = 0.2) + 
  
  # 2. Etiquetas y Títulos
  labs(
    title = "Distribución de Tiempos de Viaje por Escenario",
    subtitle = "Comparación de percentil 50",
    x = "Escenario",
    y = "Tiempo de Viaje (minutos)",
    fill = "Escenario"
  ) +
  
  # 3. Tema limpio
  theme_minimal() +
  
  # Opcional: Si los nombres de los escenarios se solapan abajo
  theme(legend.position = "bottom")

Matriz_OD_All |>
  group_by(SCN) |>
  summarise(
    Media = mean(travel_time_p50, na.rm = TRUE),
    Mediana = median(travel_time_p50, na.rm = TRUE), # Dato extra útil
    Desv_Std = sd(travel_time_p50, na.rm = TRUE)     # Dato extra útil
  )
summary(Matriz_OD_Scn1$travel_time_p50)
Avisar()







# ---


# ####
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ::: x. xxx                                                                 :::
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ####
#
Start_Time_S8_Px <- Sys.time()

# Mi contenido aquí

# Tiempo de procesamiento:
End_Time_S8_Px <- Sys.time()
print(paste("x. Tiempo de xxx: ", as.duration(End_Time_S8_Px - Start_Time_S8_Px)))
