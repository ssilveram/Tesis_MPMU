# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# :::                                                                        :::
# :::               SCRIPT DE EQUIPAMIENTOS - ESCENARIO 1                    :::
# :::                                                                        :::
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#
# Nombre:       4_1_Amenities_Scenario_1.R
#
# Descripcion:  Este Script hará la consulta en OSM acerca de diferentes tipos de
#               equipamientos en la ciudad, y posteriormente clasificará y creará
#               los diferentes df y los combinará con los sf de la ciudad.
#
# ####
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# :::                      Contenido de este Script                          :::
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ####
#
# 1. Inicialización
# 2. Carga de datos de origen
# 3. Consulta de equipamientos
# 4. Creamos el inventario de equipamientos por área (Area vs Equip)
# 5. Creamos los sf de categorías de inventario (area vs categoria)
# 6. Creamos mapa de capa de UPL con etiquetas para sobreponer
# 7. Creamos mapa de puntos de equipamientos (Todos)
# 8. Creamos mapa de Equipamientos por HEX (Todos)
# 9. Creamos mapa de Equipamientos por UPL (Todos)
# 10. Creamos mapa de Equipamientos por UPL, por cada 100k Habitantes
# 11. Creamos mapa de Equipamientos por Educación Superior por UPL
# 12. Extra - Creamos los mapas 2D y 3D para las posibles combinaciones
# 13. Extra - ¿Necesita recargar los resultados de este script?
# 14. Tiempos de procesamiento
#
Start_Time_S4_1_P0 <- Sys.time()

# ####
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ::: 1. Inicialización                                                      :::
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ####
#
Start_Time_S4_1_P1 <- Sys.time()

source("0_Initialization.R")

# Tiempo de procesamiento:
End_Time_S4_1_P1 <- Sys.time()
print(paste("1. Tiempo de inicialización: ", as.duration(End_Time_S4_1_P1 - Start_Time_S4_1_P1)))

# ####
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ::: 2. Carga de datos de origen                                            :::
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ####
#
# Cargamos los shapes vacíos y los de población para comenzar a hacer consultas 
# y los análisis.
#
Start_Time_S4_1_P2 <- Sys.time()

# --- 2.1. Carga de Shapes Vacíos (1_Empty_Shapes.R) ---

Shape_Municipios <- st_read("./Data/2_Processing/1_Empty_Shapes/Shape_Vacio_Municipios.gpkg")
Shape_Localidades <- st_read("./Data/2_Processing/1_Empty_Shapes/Shape_Vacio_Localidades.gpkg")
Shape_UPLs <- st_read("./Data/2_Processing/1_Empty_Shapes/Shape_Vacio_UPLs.gpkg")
Shape_UTAM <- st_read("./Data/2_Processing/1_Empty_Shapes/Shape_Vacio_UTAM.gpkg")
Shape_ZAT <- st_read("./Data/2_Processing/1_Empty_Shapes/Shape_Vacio_ZAT.gpkg")
Shape_Sectores <- st_read("./Data/2_Processing/1_Empty_Shapes/Shape_Vacio_Sectores.gpkg")
Shape_Secciones <- st_read("./Data/2_Processing/1_Empty_Shapes/Shape_Vacio_Secciones.gpkg")
Shape_Hexagonos <- st_read("./Data/2_Processing/1_Empty_Shapes/Shape_Vacio_Hexagonos.gpkg")
Shape_Manzanas <- st_read("./Data/2_Processing/1_Empty_Shapes/Shape_Vacio_Manzanas.gpkg")

# --- 2.2. Carga de los Shapes de la población (2_Population.R) ---

Shape_Censo_UPLs <- st_read("./Data/2_Processing/2_Population/Shape_Poblacion_UPLs.gpkg")

# --- 2.3. Carga de información de viviendas (Para analizar el Housing) ---

Censo_Viviendas <- as.data.frame(fread("./Data/2_Processing/2_Population/Censo_Viviendas.csv", sep=",", na = "null", colClasses=c("COD_LOC"="character", "COD_UPL"="character", "COD_UTAM"="character", "COD_ZAT"="character", "COD_SECT"="character", "COD_SECC"="character", "COD_HEX"="character", "COD_MNZ"="character", "COD_VIVIENDA"="character")))

# --- 2.4. Establecimiento del BBox para consulta ---

Coordinates_CRS <- st_crs(Shape_Municipios)
BBox_Municipios <- st_bbox(Shape_Municipios)
BBox_Municipios

# Tiempo de procesamiento:
End_Time_S4_1_P2 <- Sys.time()
print(paste("2. Tiempo de carga de datos de origen: ", as.duration(End_Time_S4_1_P2 - Start_Time_S4_1_P2)))

# ####
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ::: 3. Consulta de equipamientos                                           :::
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ####
#
# Hacemos las consultas correspondientes a los equipamientos, ya sea extrayendo
# la información de conjuntos de datos locales, como de OSM.
#
Start_Time_S4_1_P3 <- Sys.time()

# Importamos las Categorías, Equipamientos, Claves, Valores y Traducciones
Equipamientos_OSM <- as.data.frame(fread("./Data/1_Sources/4_Amenities/Scn1/OSM_Categories_CSV.csv", sep=";", na = "null")) # Categorias para hacer las consultas a OSM
Equipamientos_OSM # 102 Filas
length(Equipamientos_OSM$EQUIPAMIENTO)
names(Equipamientos_OSM)

# Creamos un sf casi vacío, donde se irán copiando los sf que se van solicitando.
Amenities_from_OSM <- st_sf(EQUIPAMIENTO = "Borrador",
                      geometry = st_sfc(st_point(1:2)),
                      crs = Coordinates_CRS
)
Amenities_from_OSM

# Recorremos el sf de Categorias_OSM y hacemos las solicitudes a medida que vamos recorriendo.
Total_Consultas_OSM <- length(Equipamientos_OSM$EQUIPAMIENTO)
Total_Consultas_OSM
unique(Equipamientos_OSM$EQUIPAMIENTO)

# Realizamos las consultas una a una.
# for(i in 1:Total_Consultas_OSM){
#   Equipamiento <- Equipamientos_OSM$EQUIPAMIENTO[i]
#   Clave <- Equipamientos_OSM$CLAVE[i]
#   Valor <- Equipamientos_OSM$VALOR[i]
#   print(paste(i, "/", Total_Consultas_OSM, ". Solicitando ", Equipamiento, "...", sep = ""))
#   Temp_sf <- Solicitud_OSM_v02(Shape_Hexagonos, Equipamiento, Clave, Valor)
#   Amenities_from_OSM <- Amenities_from_OSM |>
#     bind_rows(Temp_sf) |>
#     filter(EQUIPAMIENTO != "Borrador") |>
#     select(EQUIPAMIENTO, COD_LOC, COD_UPL, COD_UTAM, COD_ZAT, COD_SECT, COD_SECC, COD_HEX)
# }
# Amenities_from_OSM # 386385 Equipamientos
# unique(Amenities_from_OSM$EQUIPAMIENTO)

# Guardamos el Amenities_form_OSM para no repetir el proceso en el futuro:
#st_write(Amenities_from_OSM, dsn = "./Data/2_Processing/3_Amenities/Sc1/Amenities_from_OSM.gpkg", driver = 'GPKG', append = FALSE)
Amenities_from_OSM <- st_read("./Data/2_Processing/4_Amenities/Scn1/Amenities_from_OSM.gpkg")
Amenities_from_OSM

# Agregamos las columnas de clasificación del equipamiento:
Amenities_Con_Clasificacion_sf <- Amenities_from_OSM |>
  left_join(
    Equipamientos_OSM |>
      select(-c('No.', CLAVE, VALOR,)) |>
      distinct(),
    by = "EQUIPAMIENTO",
    multiple = "first"
  ) |>
  mutate(
    Social_Functions = 0,
    Composite_Indicators = 0,
    Category = 0
  )
Amenities_Con_Clasificacion_sf # 386385
names(Amenities_Con_Clasificacion_sf)

# --- Corrección de los datos de Housing y Living ---

# Los datos de Housing de OSM nos arrojan datos atípicos,
# así que tomamos los datos oficiales del Censo de Población.
Amenities_Viviendas_Censo <- Shape_Manzanas |>
  st_centroid() |>
  right_join(
    Censo_Viviendas |>
      select(COD_MNZ, COD_VIVIENDA),
    by = "COD_MNZ"
  ) |>
  select(-COD_MNZ, -COD_VIVIENDA) |>
  mutate(
    EQUIPAMIENTO = "Viviendas",
    AMENITY = "Housing",
    DESCRIPCION = "Viviendas tomadas del Censo",
    DESCRIPTION = "Housing taken from Census",
    Living = "VERDADERO",
    Working = "FALSO",
    Supplying = "FALSO",
    Caring = "FALSO",
    Learning = "FALSO",
    Enjoying = "FALSO",
    Well_being = "VERDADERO",
    Sociability = "FALSO",
    Environmental_Impact = "FALSO",
    Education_and_Culture = "FALSO",
    Health_and_Care = "FALSO",
    Commerce_and_Supply = "FALSO",
    Sport_and_Public_Spaces = "FALSO",
    Work_and_Economic_Activities = "FALSO",
    Administratives_and_Financial_Services = "FALSO",
    Housing = "VERDADERO",
    Transportation = "FALSO",
    Community_and_Spirituality = "FALSO",
    Social_Functions = 0,
    Composite_Indicators = 0,
    Category = 0,
  ) |>
  select(names(Amenities_Con_Clasificacion_sf))
Amenities_Viviendas_Censo # 366707 viviendas con geom incluida

Amenities_Con_Clasificacion_sf # 386385
#Unimos los sf, descartando los anteriores resultados de equipamientos:
Amenities_Puntos_Scn1_sf <- Amenities_Con_Clasificacion_sf |> # 386385 elementos iniciales
  filter(!EQUIPAMIENTO %in% c("Apartamentos", "Casas", "Residencial", "Dormitorios", "Residencial")) |> # Quedan 224560 elementos
  bind_rows(Amenities_Viviendas_Censo) # Se agregan 366707 elementos
Amenities_Puntos_Scn1_sf # Finalizan 591267 elementos

# Revisamos que queden las 366707 viviendas:
Amenities_Puntos_Scn1_sf |>
  filter(EQUIPAMIENTO == "Viviendas")

# Testeamos:
#PlotMapa(Amenities_Puntos_Scn1_sf)

# Guardamos y recargamos el sf de todos los Amenities:
#st_write(Amenities_Puntos_Scn1_sf, dsn = "./Data/2_Processing/4_Amenities/Scn1/Amenities_Puntos_Scn1_sf.gpkg", driver = 'GPKG', append = FALSE)
Amenities_Puntos_Scn1_sf <- st_read("./Data/2_Processing/4_Amenities/Scn1/Amenities_Puntos_Scn1_sf.gpkg")
Amenities_Puntos_Scn1_sf

# Tiempo de procesamiento de solicitud de puntos a OSM:
End_Time_S4_1_P3 <- Sys.time()
print(paste("3. Tiempo de consulta y corrección housing de equipamientos: ", as.duration(End_Time_S4_1_P3 - Start_Time_S4_1_P3)))

# ####
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ::: 4. Creamos el inventario de equipamientos por área (Area vs Equip)     :::
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ####
#
# Creamos el inventario de equipamientos por área, es decir que al final la tabla nos muestra el área en las filas
# y el tipo de equipamiento en las columnas. El cruce es la suma de elementos encontrados por cada área.
#
# Se hace para cada uno de estos:
#
#  HEX - Cantidad de <Equipamiento>
#  SECC - Cantidad de <Equipamiento>
#  SECT - Cantidad de <Equipamiento>
#  ZAT - Cantidad de <Equipamiento>
#  UTAM - Cantidad de <Equipamiento>
#  UPL - Cantidad de <Equipamiento>
#  LOC - Cantidad de <Equipamiento>
#  BOG - Cantidad de <Equipamiento>
#
Start_Time_S4_1_P4 <- Sys.time()

Amenities_Puntos_Scn1_sf # Este sf viene del paso anterior, y es el inventario en puntos bruto de OSM + Viviendas.

# --- 4.1. HEX ---

# Creamos el df de inventario para el área correspondiente:
Amenities_Inventory_Scn1_HEX_df <- Amenities_Puntos_Scn1_sf |>
  as_tibble() |>
  # Agrupamos por HEX y Tipo de equipamiento, y contamos las ocurrencias:
  count(COD_HEX, AMENITY, name = "Conteo") |>
  
  # Pivotamos la tabla para tener una columna por cada tipo de equipamiento.
  pivot_wider(
    names_from = AMENITY,  # Las nuevas columnas vendrán de los valores de "EQUIPAMIENTO"
    values_from = Conteo,       # Los valores de esas columnas serán los conteos
    values_fill = 0             # Rellena con 0 si un HEX no tiene un tipo de equipamiento
  ) |>
  mutate(Total_Equipamientos = rowSums(across(where(is.numeric))))
Amenities_Inventory_Scn1_HEX_df


# Unimos al shape correspondiente:
Amenities_Inventory_Scn1_HEX_sf <- Shape_Hexagonos |>
  # Unimos el Shape de HEX con el df de Inventario de HEX
  left_join(Amenities_Inventory_Scn1_HEX_df, by = "COD_HEX", n) |>
  # Reemplazamos los NA con 0 en las columnas de conteo para los HEX que no tengan ningún equipamiento.
  mutate(across(where(is.numeric), ~replace_na(., 0)))
Amenities_Inventory_Scn1_HEX_sf

# Testeamos:
#MostrarMapa(Amenities_Inventory_Scn1_HEX_sf)

# --- 4.2. SECC ---

# Creamos el df de inventario para el área correspondiente:
Amenities_Inventory_Scn1_SECC_df <- Amenities_Puntos_Scn1_sf |>
  as_tibble() |>
  # Agrupamos por SECC y Tipo de equipamiento, y contamos las ocurrencias:
  count(COD_SECC, AMENITY, name = "Conteo") |>
  
  # Pivotamos la tabla para tener una columna por cada tipo de equipamiento.
  pivot_wider(
    names_from = AMENITY,  # Las nuevas columnas vendrán de los valores de "EQUIPAMIENTO"
    values_from = Conteo,       # Los valores de esas columnas serán los conteos
    values_fill = 0             # Rellena con 0 si una SECC no tiene un tipo de equipamiento
  ) |>
  mutate(Total_Equipamientos = rowSums(across(where(is.numeric))))
Amenities_Inventory_Scn1_SECC_df

# Unimos al shape correspondiente:
Amenities_Inventory_Scn1_SECC_sf <- Shape_Secciones |>
  # Unimos el Shape de SECC con el df de Inventario de SECC
  left_join(Amenities_Inventory_Scn1_SECC_df, by = "COD_SECC", n) |>
  # Reemplazamos los NA con 0 en las columnas de conteo para las SECC que no tengan ningún equipamiento.
  mutate(across(where(is.numeric), ~replace_na(., 0)))
Amenities_Inventory_Scn1_SECC_sf

# Testeamos:
#MostrarMapa(Amenities_Inventory_Scn1_SECC_sf)

# --- 4.3. SECT ---

# Creamos el df de inventario para el área correspondiente:
Amenities_Inventory_Scn1_SECT_df <- Amenities_Puntos_Scn1_sf |>
  as_tibble() |>
  # Agrupamos por SECT y Tipo de equipamiento, y contamos las ocurrencias:
  count(COD_SECT, AMENITY, name = "Conteo") |>
  
  # Pivotamos la tabla para tener una columna por cada tipo de equipamiento.
  pivot_wider(
    names_from = AMENITY,  # Las nuevas columnas vendrán de los valores de "EQUIPAMIENTO"
    values_from = Conteo,       # Los valores de esas columnas serán los conteos
    values_fill = 0             # Rellena con 0 si una SECT no tiene un tipo de equipamiento
  ) |>
  mutate(Total_Equipamientos = rowSums(across(where(is.numeric))))
Amenities_Inventory_Scn1_SECT_df

# Unimos al shape correspondiente:
Amenities_Inventory_Scn1_SECT_sf <- Shape_Sectores |>
  # Unimos el Shape de SECT con el df de Inventario de SECT
  left_join(Amenities_Inventory_Scn1_SECT_df, by = "COD_SECT", n) |>
  # Reemplazamos los NA con 0 en las columnas de conteo para los SECT que no tengan ningún equipamiento.
  mutate(across(where(is.numeric), ~replace_na(., 0)))
Amenities_Inventory_Scn1_SECT_sf

# Testeamos:
#MostrarMapa(Amenities_Inventory_Scn1_SECT_sf)

# --- 4.4. ZAT ---

# Creamos el df de inventario para el área correspondiente:
Amenities_Inventory_Scn1_ZAT_df <- Amenities_Puntos_Scn1_sf |>
  as_tibble() |>
  # Agrupamos por ZAT y Tipo de equipamiento, y contamos las ocurrencias:
  count(COD_ZAT, AMENITY, name = "Conteo") |>
  
  # Pivotamos la tabla para tener una columna por cada tipo de equipamiento.
  pivot_wider(
    names_from = AMENITY,  # Las nuevas columnas vendrán de los valores de "EQUIPAMIENTO"
    values_from = Conteo,       # Los valores de esas columnas serán los conteos
    values_fill = 0             # Rellena con 0 si una ZAT no tiene un tipo de equipamiento
  ) |>
  mutate(Total_Equipamientos = rowSums(across(where(is.numeric))))
Amenities_Inventory_Scn1_ZAT_df

# Unimos al shape correspondiente:
Amenities_Inventory_Scn1_ZAT_sf <- Shape_ZAT |>
  # Unimos el Shape de ZAT con el df de Inventario de ZAT
  left_join(Amenities_Inventory_Scn1_ZAT_df, by = "COD_ZAT", n) |>
  # Reemplazamos los NA con 0 en las columnas de conteo para las ZAT que no tengan ningún equipamiento.
  mutate(across(where(is.numeric), ~replace_na(., 0)))
Amenities_Inventory_Scn1_ZAT_sf

# Testeamos:
#MostrarMapa(Amenities_Inventory_Scn1_ZAT_sf)

# --- 4.5. UTAM ---

# Creamos el df de inventario para el área correspondiente:
Amenities_Inventory_Scn1_UTAM_df <- Amenities_Puntos_Scn1_sf |>
  as_tibble() |>
  # Agrupamos por UTAM y Tipo de equipamiento, y contamos las ocurrencias:
  count(COD_UTAM, AMENITY, name = "Conteo") |>
  
  # Pivotamos la tabla para tener una columna por cada tipo de equipamiento.
  pivot_wider(
    names_from = AMENITY,  # Las nuevas columnas vendrán de los valores de "EQUIPAMIENTO"
    values_from = Conteo,       # Los valores de esas columnas serán los conteos
    values_fill = 0             # Rellena con 0 si una UTAM no tiene un tipo de equipamiento
  ) |>
  mutate(Total_Equipamientos = rowSums(across(where(is.numeric))))
Amenities_Inventory_Scn1_UTAM_df

# Unimos al shape correspondiente:
Amenities_Inventory_Scn1_UTAM_sf <- Shape_UTAM |>
  # Unimos el Shape de UTAM con el df de Inventario de UTAM
  left_join(Amenities_Inventory_Scn1_UTAM_df, by = "COD_UTAM", n) |>
  # Reemplazamos los NA con 0 en las columnas de conteo para las UTAM que no tengan ningún equipamiento.
  mutate(across(where(is.numeric), ~replace_na(., 0)))
Amenities_Inventory_Scn1_UTAM_sf

# Testeamos:
#MostrarMapa(Amenities_Inventory_Scn1_UTAM_sf)

# --- 4.6. UPL ---

# Creamos el df de inventario para el área correspondiente:
Amenities_Inventory_Scn1_UPL_df <- Amenities_Puntos_Scn1_sf |>
  as_tibble() |>
  # Agrupamos por UPL y Tipo de equipamiento, y contamos las ocurrencias:
  count(COD_UPL, AMENITY, name = "Conteo") |>
  
  # Pivotamos la tabla para tener una columna por cada tipo de equipamiento.
  pivot_wider(
    names_from = AMENITY,  # Las nuevas columnas vendrán de los valores de "EQUIPAMIENTO"
    values_from = Conteo,       # Los valores de esas columnas serán los conteos
    values_fill = 0             # Rellena con 0 si una UPL no tiene un tipo de equipamiento
  ) |>
  mutate(Total_Equipamientos = rowSums(across(where(is.numeric))))
Amenities_Inventory_Scn1_UPL_df

# Unimos al shape correspondiente:
Amenities_Inventory_Scn1_UPL_sf <- Shape_UPLs |>
  # Unimos el Shape de UPL con el df de Inventario de UPL
  left_join(Amenities_Inventory_Scn1_UPL_df, by = "COD_UPL", n) |>
  # Reemplazamos los NA con 0 en las columnas de conteo para las UPL que no tengan ningún equipamiento.
  mutate(across(where(is.numeric), ~replace_na(., 0)))
Amenities_Inventory_Scn1_UPL_sf

# Testeamos:
#MostrarMapa(Amenities_Inventory_Scn1_UPL_sf)

# --- 4.7. LOC ---

# Creamos el df de inventario para el área correspondiente:
Amenities_Inventory_Scn1_LOC_df <- Amenities_Puntos_Scn1_sf |>
  as_tibble() |>
  # Agrupamos por LOC y Tipo de equipamiento, y contamos las ocurrencias:
  count(COD_LOC, AMENITY, name = "Conteo") |>
  
  # Pivotamos la tabla para tener una columna por cada tipo de equipamiento.
  pivot_wider(
    names_from = AMENITY,  # Las nuevas columnas vendrán de los valores de "EQUIPAMIENTO"
    values_from = Conteo,       # Los valores de esas columnas serán los conteos
    values_fill = 0             # Rellena con 0 si una LOC no tiene un tipo de equipamiento
  ) |>
  mutate(Total_Equipamientos = rowSums(across(where(is.numeric))))
Amenities_Inventory_Scn1_LOC_df

# Unimos al shape correspondiente:
Amenities_Inventory_Scn1_LOC_sf <- Shape_Localidades |>
  # Unimos el Shape de LOC con el df de Inventario de LOC
  left_join(Amenities_Inventory_Scn1_LOC_df, by = "COD_LOC", n) |>
  # Reemplazamos los NA con 0 en las columnas de conteo para las LOC que no tengan ningún equipamiento.
  mutate(across(where(is.numeric), ~replace_na(., 0)))
Amenities_Inventory_Scn1_LOC_sf

# Testeamos:
#MostrarMapa(Amenities_Inventory_Scn1_LOC_sf)

# --- 4.8. BOG ---

# Creamos el df de inventario para el área correspondiente:
Amenities_Inventory_Scn1_BOG_df <- Amenities_Puntos_Scn1_sf |>
  as_tibble() |>
  mutate(COD_CPOB = "11001000") |>
  # Agrupamos por COD_CPOB y Tipo de equipamiento, y contamos las ocurrencias:
  count(COD_CPOB, AMENITY, name = "Conteo") |>
  
  # Pivotamos la tabla para tener una columna por cada tipo de equipamiento.
  pivot_wider(
    names_from = AMENITY,  # Las nuevas columnas vendrán de los valores de "EQUIPAMIENTO"
    values_from = Conteo,       # Los valores de esas columnas serán los conteos
    values_fill = 0             # Rellena con 0 si una LOC no tiene un tipo de equipamiento
  ) |>
  mutate(Total_Equipamientos = rowSums(across(where(is.numeric))))
Amenities_Inventory_Scn1_BOG_df

# Unimos al shape correspondiente:
Amenities_Inventory_Scn1_BOG_sf <- Shape_Municipios |>
  # Unimos el Shape de LOC con el df de Inventario de LOC
  left_join(Amenities_Inventory_Scn1_BOG_df, by = "COD_CPOB", n) |>
  # Reemplazamos los NA con 0 en las columnas de conteo cuando no tengan ningún equipamiento.
  mutate(across(where(is.numeric), ~replace_na(., 0)))
Amenities_Inventory_Scn1_BOG_sf

# Testeamos:
MostrarMapa(Amenities_Inventory_Scn1_BOG_sf)

# --- 4.9. Guardamos y recargamos los inventarios sf de áreas de inventario ---

# Guardamos todo en una variable de R:
save(
  Amenities_Inventory_Scn1_HEX_sf,
  Amenities_Inventory_Scn1_SECC_sf,
  Amenities_Inventory_Scn1_SECT_sf,
  Amenities_Inventory_Scn1_ZAT_sf,
  Amenities_Inventory_Scn1_UTAM_sf,
  Amenities_Inventory_Scn1_UPL_sf,
  Amenities_Inventory_Scn1_LOC_sf,
  Amenities_Inventory_Scn1_BOG_sf,
  file = "./Data/2_Processing/4_Amenities/Scn1/Amenities_Inventory_by_Area_Eng.RData"
)
load("./Data/2_Processing/4_Amenities/Scn1/Amenities_Inventory_by_Area_Eng.RData")

# --- 4.10. Mostramos todo mapa para verificar ---

# tm_basemap("OpenStreetMap") +
#   tm_shape(Amenities_Inventory_Scn1_BOG_sf) +
#   tm_polygons(fill = "Total_Equipamientos", fill_alpha = 0.5, col_alpha = 0.1) +
#   tm_shape(Amenities_Inventory_Scn1_LOC_sf) +
#   tm_polygons(fill = "Total_Equipamientos", fill_alpha = 0.5, col_alpha = 0.1) +
#   tm_shape(Amenities_Inventory_Scn1_UPL_sf) +
#   tm_polygons(fill = "Total_Equipamientos", fill_alpha = 0.5, col_alpha = 0.1) +
#   tm_shape(Amenities_Inventory_Scn1_UTAM_sf) +
#   tm_polygons(fill = "Total_Equipamientos", fill_alpha = 0.5, col_alpha = 0.1) +
#   tm_shape(Amenities_Inventory_Scn1_ZAT_sf) +
#   tm_polygons(fill = "Total_Equipamientos", fill_alpha = 0.5, col_alpha = 0.1) +
#   tm_shape(Amenities_Inventory_Scn1_SECT_sf) +
#   tm_polygons(fill = "Total_Equipamientos", fill_alpha = 0.5, col_alpha = 0.1) +
#   tm_shape(Amenities_Inventory_Scn1_SECC_sf) +
#   tm_polygons(fill = "Total_Equipamientos", fill_alpha = 0.5, col_alpha = 0.1) +
#   tm_shape(Amenities_Inventory_Scn1_HEX_sf) +
#   tm_polygons(fill = "Total_Equipamientos", fill_alpha = 0.5, col_alpha = 0.1)

# Tiempo de procesamiento de Inventario por áreas:
End_Time_S4_1_P4 <- Sys.time()
print(paste("4. Tiempo de procesamiento de inventario por áreas: ", as.duration(End_Time_S4_1_P4 - Start_Time_S4_1_P4)))

# ####
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ::: 5. Creamos los sf de categorías de inventario (area vs categoria)      :::
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ####
#
# Buscamos encontrar la suma las clasificaciones  o categorías de los equipamientos
# por cada una de las áreas.
#
# Se hace para cada uno de estos:
#
#  HEX - Cantidad de <Clasificación/Categoría>
#  SECC - Cantidad de <Clasificación/Categoría>
#  SECT - Cantidad de <Clasificación/Categoría>
#  ZAT - Cantidad de <Clasificación/Categoría>
#  UTAM - Cantidad de <Clasificación/Categoría>
#  UPL - Cantidad de <Clasificación/Categoría>
#  LOC - Cantidad de <Clasificación/Categoría>
#  BOG - Cantidad de <Clasificación/Categoría>

Start_Time_S4_1_P5 <- Sys.time()

# Todos los equipamientos
Amenities_Puntos_Scn1_sf
Equipamientos_OSM

# --- 5.1. HEX ---

# Creamos el df de categorias para el área correspondiente:
Amenities_Category_Scn1_HEX_df <- Amenities_Puntos_Scn1_sf |>
  as_tibble() |>
  # simplificamos la elección de columnas:
  select(COD_HEX, Living:Community_and_Spirituality) |>
  filter(COD_HEX != "<NA>") |>
  group_by(COD_HEX) |>
  summarise(
    # 1. Usamos across() para aplicar la misma lógica a todas las columnas de categorías.
    #    Esto reemplaza el usar muchas líneas para sumar.
    across(
      Living:Community_and_Spirituality,        # Todas las columnas entre Living y Community_and_Spirituality
      ~ sum(.x == "VERDADERO", na.rm = TRUE)    # Sumamos sus valores.
    ),
    Total_Amenities = n(),                      # Sumamos para saber el total de Amenities
    .groups = "drop" # Desagrupamos
  ) |>
  mutate(
    # 2. Usamos rowSums con across para sumar las columnas de forma más elegante.
    Social_Functions = rowSums(across(Living:Enjoying)),
    Composite_Indicators = rowSums(across(Well_being:Environmental_Impact))
  )
Amenities_Category_Scn1_HEX_df                  # Echamos un vistazo que se haya hecho bien

# Unimos el Shape correspondiente:
Amenities_Category_Scn1_HEX_sf <- Shape_Hexagonos |>
  select(COD_LOC, NOM_LOC, COD_UPL, NOM_UPL, COD_UTAM, NOM_UTAM, COD_ZAT, COD_SECT, COD_SECC, COD_HEX) |>
  left_join(Amenities_Category_Scn1_HEX_df) |>
  mutate(across(where(is.numeric), ~ ifelse(is.na(.), 0, .)))
Amenities_Category_Scn1_HEX_sf

# Testeamos:
MostrarMapa(Amenities_Category_Scn1_HEX_sf)

# --- 5.2. SECC ---

# Creamos el df de categorias para el área correspondiente:
Amenities_Category_Scn1_SECC_df <- Amenities_Puntos_Scn1_sf |>
  as_tibble() |>
  # simplificamos la elección de columnas:
  select(COD_SECC, Living:Community_and_Spirituality) |>
  filter(COD_SECC != "<NA>") |>
  group_by(COD_SECC) |>
  summarise(
    # 1. Usamos across() para aplicar la misma lógica a todas las columnas de categorías.
    #    Esto reemplaza el usar muchas líneas para sumar.
    across(
      Living:Community_and_Spirituality,        # Todas las columnas entre Living y Community_and_Spirituality
      ~ sum(.x == "VERDADERO", na.rm = TRUE)    # Sumamos sus valores.
    ),
    Total_Amenities = n(),                      # Sumamos para saber el total de Amenities
    .groups = "drop" # Desagrupamos
  ) |>
  mutate(
    # 2. Usamos rowSums con across para sumar las columnas de forma más elegante.
    Social_Functions = rowSums(across(Living:Enjoying)),
    Composite_Indicators = rowSums(across(Well_being:Environmental_Impact))
  )
Amenities_Category_Scn1_SECC_df                 # Echamos un vistazo que se haya hecho bien

# Unimos el Shape correspondiente:
Amenities_Category_Scn1_SECC_sf <- Shape_Secciones |>
  select(COD_LOC, NOM_LOC, COD_UPL, NOM_UPL, COD_UTAM, NOM_UTAM, COD_ZAT, COD_SECT, COD_SECC) |>
  left_join(Amenities_Category_Scn1_SECC_df) |>
  mutate(across(where(is.numeric), ~ ifelse(is.na(.), 0, .)))
Amenities_Category_Scn1_SECC_sf

# Testeamos:
MostrarMapa(Amenities_Category_Scn1_SECC_sf)

# --- 5.3. SECT ---

# Creamos el df de categorias para el área correspondiente:
Amenities_Category_Scn1_SECT_df <- Amenities_Puntos_Scn1_sf |>
  as_tibble() |>
  # simplificamos la elección de columnas:
  select(COD_SECT, Living:Community_and_Spirituality) |>
  filter(COD_SECT != "<NA>") |>
  group_by(COD_SECT) |>
  summarise(
    # 1. Usamos across() para aplicar la misma lógica a todas las columnas de categorías.
    #    Esto reemplaza el usar muchas líneas para sumar.
    across(
      Living:Community_and_Spirituality,        # Todas las columnas entre Living y Community_and_Spirituality
      ~ sum(.x == "VERDADERO", na.rm = TRUE)    # Sumamos sus valores.
    ),
    Total_Amenities = n(),                      # Sumamos para saber el total de Amenities
    .groups = "drop" # Desagrupamos
  ) |>
  mutate(
    # 2. Usamos rowSums con across para sumar las columnas de forma más elegante.
    Social_Functions = rowSums(across(Living:Enjoying)),
    Composite_Indicators = rowSums(across(Well_being:Environmental_Impact))
  )
Amenities_Category_Scn1_SECT_df                 # Echamos un vistazo que se haya hecho bien

# Unimos el Shape correspondiente:
Amenities_Category_Scn1_SECT_sf <- Shape_Sectores |>
  select(COD_LOC, NOM_LOC, COD_UPL, NOM_UPL, COD_UTAM, NOM_UTAM, COD_ZAT, COD_SECT) |>
  left_join(Amenities_Category_Scn1_SECT_df) |>
  mutate(across(where(is.numeric), ~ ifelse(is.na(.), 0, .)))
Amenities_Category_Scn1_SECT_sf

# Testeamos:
MostrarMapa(Amenities_Category_Scn1_SECT_sf)

# --- 5.4. ZAT ---

# Creamos el df de categorias para el área correspondiente:
Amenities_Category_Scn1_ZAT_df <- Amenities_Puntos_Scn1_sf |>
  as_tibble() |>
  # simplificamos la elección de columnas:
  select(COD_ZAT, Living:Community_and_Spirituality) |>
  filter(COD_ZAT != "<NA>") |>
  group_by(COD_ZAT) |>
  summarise(
    # 1. Usamos across() para aplicar la misma lógica a todas las columnas de categorías.
    #    Esto reemplaza el usar muchas líneas para sumar.
    across(
      Living:Community_and_Spirituality,        # Todas las columnas entre Living y Community_and_Spirituality
      ~ sum(.x == "VERDADERO", na.rm = TRUE)    # Sumamos sus valores.
    ),
    Total_Amenities = n(),                      # Sumamos para saber el total de Amenities
    .groups = "drop" # Desagrupamos
  ) |>
  mutate(
    # 2. Usamos rowSums con across para sumar las columnas de forma más elegante.
    Social_Functions = rowSums(across(Living:Enjoying)),
    Composite_Indicators = rowSums(across(Well_being:Environmental_Impact))
  )
Amenities_Category_Scn1_ZAT_df                 # Echamos un vistazo que se haya hecho bien

# Unimos el Shape correspondiente:
Amenities_Category_Scn1_ZAT_sf <- Shape_ZAT |>
  select(COD_LOC, NOM_LOC, COD_UPL, NOM_UPL, COD_UTAM, NOM_UTAM, COD_ZAT) |>
  left_join(Amenities_Category_Scn1_ZAT_df) |>
  mutate(across(where(is.numeric), ~ ifelse(is.na(.), 0, .)))
Amenities_Category_Scn1_ZAT_sf

# Testeamos:
MostrarMapa(Amenities_Category_Scn1_ZAT_sf)

# --- 5.5. UTAM ---

# Creamos el df de categorias para el área correspondiente:
Amenities_Category_Scn1_UTAM_df <- Amenities_Puntos_Scn1_sf |>
  as_tibble() |>
  # simplificamos la elección de columnas:
  select(COD_UTAM, Living:Community_and_Spirituality) |>
  filter(COD_UTAM != "<NA>") |>
  group_by(COD_UTAM) |>
  summarise(
    # 1. Usamos across() para aplicar la misma lógica a todas las columnas de categorías.
    #    Esto reemplaza el usar muchas líneas para sumar.
    across(
      Living:Community_and_Spirituality,        # Todas las columnas entre Living y Community_and_Spirituality
      ~ sum(.x == "VERDADERO", na.rm = TRUE)    # Sumamos sus valores.
    ),
    Total_Amenities = n(),                      # Sumamos para saber el total de Amenities
    .groups = "drop" # Desagrupamos
  ) |>
  mutate(
    # 2. Usamos rowSums con across para sumar las columnas de forma más elegante.
    Social_Functions = rowSums(across(Living:Enjoying)),
    Composite_Indicators = rowSums(across(Well_being:Environmental_Impact))
  )
Amenities_Category_Scn1_UTAM_df                 # Echamos un vistazo que se haya hecho bien

# Unimos el Shape correspondiente:
Amenities_Category_Scn1_UTAM_sf <- Shape_UTAM |>
  select(COD_LOC, NOM_LOC, COD_UPL, NOM_UPL, COD_UTAM, NOM_UTAM) |>
  left_join(Amenities_Category_Scn1_UTAM_df) |>
  mutate(across(where(is.numeric), ~ ifelse(is.na(.), 0, .)))
Amenities_Category_Scn1_UTAM_sf

# Testeamos:
MostrarMapa(Amenities_Category_Scn1_UTAM_sf)

# --- 5.6. UPL ---

# Creamos el df de categorias para el área correspondiente:
Amenities_Category_Scn1_UPL_df <- Amenities_Puntos_Scn1_sf |>
  as_tibble() |>
  # simplificamos la elección de columnas:
  select(COD_UPL, Living:Community_and_Spirituality) |>
  filter(COD_UPL != "<NA>") |>
  group_by(COD_UPL) |>
  summarise(
    # 1. Usamos across() para aplicar la misma lógica a todas las columnas de categorías.
    #    Esto reemplaza el usar muchas líneas para sumar.
    across(
      Living:Community_and_Spirituality,        # Todas las columnas entre Living y Community_and_Spirituality
      ~ sum(.x == "VERDADERO", na.rm = TRUE)    # Sumamos sus valores.
    ),
    Total_Amenities = n(),                      # Sumamos para saber el total de Amenities
    .groups = "drop" # Desagrupamos
  ) |>
  mutate(
    # 2. Usamos rowSums con across para sumar las columnas de forma más elegante.
    Social_Functions = rowSums(across(Living:Enjoying)),
    Composite_Indicators = rowSums(across(Well_being:Environmental_Impact))
  )
Amenities_Category_Scn1_UPL_df                 # Echamos un vistazo que se haya hecho bien

# Unimos el Shape correspondiente:
Amenities_Category_Scn1_UPL_sf <- Shape_UPLs |>
  select(COD_UPL, NOM_UPL) |>
  left_join(Amenities_Category_Scn1_UPL_df) |>
  mutate(across(where(is.numeric), ~ ifelse(is.na(.), 0, .)))
Amenities_Category_Scn1_UPL_sf

# Testeamos:
MostrarMapa(Amenities_Category_Scn1_UPL_sf)

# --- 5.7. LOC ---

# Creamos el df de categorias para el área correspondiente:
Amenities_Category_Scn1_LOC_df <- Amenities_Puntos_Scn1_sf |>
  as_tibble() |>
  # simplificamos la elección de columnas:
  select(COD_LOC, Living:Community_and_Spirituality) |>
  filter(COD_LOC != "<NA>") |>
  group_by(COD_LOC) |>
  summarise(
    # 1. Usamos across() para aplicar la misma lógica a todas las columnas de categorías.
    #    Esto reemplaza el usar muchas líneas para sumar.
    across(
      Living:Community_and_Spirituality,        # Todas las columnas entre Living y Community_and_Spirituality
      ~ sum(.x == "VERDADERO", na.rm = TRUE)    # Sumamos sus valores.
    ),
    Total_Amenities = n(),                      # Sumamos para saber el total de Amenities
    .groups = "drop" # Desagrupamos
  ) |>
  mutate(
    # 2. Usamos rowSums con across para sumar las columnas de forma más elegante.
    Social_Functions = rowSums(across(Living:Enjoying)),
    Composite_Indicators = rowSums(across(Well_being:Environmental_Impact))
  )
Amenities_Category_Scn1_LOC_df                 # Echamos un vistazo que se haya hecho bien

# Unimos el Shape correspondiente:
Amenities_Category_Scn1_LOC_sf <- Shape_Localidades |>
  select(COD_LOC, NOM_LOC) |>
  left_join(Amenities_Category_Scn1_LOC_df) |>
  mutate(across(where(is.numeric), ~ ifelse(is.na(.), 0, .)))
Amenities_Category_Scn1_LOC_sf

# Testeamos:
MostrarMapa(Amenities_Category_Scn1_LOC_sf)

# --- 5.8. BOG ---

# Creamos el df de categorias para el área correspondiente:
Amenities_Category_Scn1_BOG_df <- Amenities_Puntos_Scn1_sf |>
  as_tibble() |>
  mutate(COD_CPOB = "11001000") |>
  # simplificamos la elección de columnas:
  select(COD_CPOB, Living:Community_and_Spirituality) |>
  group_by(COD_CPOB) |>
  summarise(
    # 1. Usamos across() para aplicar la misma lógica a todas las columnas de categorías.
    #    Esto reemplaza el usar muchas líneas para sumar.
    across(
      Living:Community_and_Spirituality,        # Todas las columnas entre Living y Community_and_Spirituality
      ~ sum(.x == "VERDADERO", na.rm = TRUE)    # Sumamos sus valores.
    ),
    Total_Amenities = n(),                      # Sumamos para saber el total de Amenities
    .groups = "drop" # Desagrupamos
  ) |>
  mutate(
    # 2. Usamos rowSums con across para sumar las columnas de forma más elegante.
    Social_Functions = rowSums(across(Living:Enjoying)),
    Composite_Indicators = rowSums(across(Well_being:Environmental_Impact))
  )
Amenities_Category_Scn1_BOG_df                 # Echamos un vistazo que se haya hecho bien

# Unimos el Shape correspondiente:
Amenities_Category_Scn1_BOG_sf <- Shape_Municipios |>
  select(COD_CPOB, NOM_CPOB) |>
  left_join(Amenities_Category_Scn1_BOG_df) |>
  mutate(across(where(is.numeric), ~ ifelse(is.na(.), 0, .)))
Amenities_Category_Scn1_BOG_sf

#Testeamos:
MostrarMapa(Amenities_Category_Scn1_BOG_sf)

# --- 5.9. Guardamos y recargamos los sf de inventarios por áreas por categorías ---

# Guardamos todo en una variable de R:
save(
  Amenities_Category_Scn1_HEX_sf,
  Amenities_Category_Scn1_SECC_sf,
  Amenities_Category_Scn1_SECT_sf,
  Amenities_Category_Scn1_ZAT_sf,
  Amenities_Category_Scn1_UTAM_sf,
  Amenities_Category_Scn1_UPL_sf,
  Amenities_Category_Scn1_LOC_sf,
  Amenities_Category_Scn1_BOG_sf,
  file = "./Data/2_Processing/4_Amenities/Scn1/Amenities_Category_by_Area_Eng.RData"
)
load("./Data/2_Processing/4_Amenities/Scn1/Amenities_Category_by_Area_Eng.RData")

# --- Mostramos todo mapa para verificar ---

tm_basemap("OpenStreetMap") +
  tm_shape(Amenities_Category_Scn1_BOG_sf) +
  tm_polygons(fill = "Social_Functions", fill_alpha = 0.5, col_alpha = 0.1) +
  tm_shape(Amenities_Category_Scn1_LOC_sf) +
  tm_polygons(fill = "Social_Functions", fill_alpha = 0.5, col_alpha = 0.1) +
  tm_shape(Amenities_Category_Scn1_UPL_sf) +
  tm_polygons(fill = "Social_Functions", fill_alpha = 0.5, col_alpha = 0.1) +
  tm_shape(Amenities_Category_Scn1_UTAM_sf) +
  tm_polygons(fill = "Social_Functions", fill_alpha = 0.5, col_alpha = 0.1) +
  tm_shape(Amenities_Category_Scn1_ZAT_sf) +
  tm_polygons(fill = "Social_Functions", fill_alpha = 0.5, col_alpha = 0.1) +
  tm_shape(Amenities_Category_Scn1_SECT_sf) +
  tm_polygons(fill = "Social_Functions", fill_alpha = 0.5, col_alpha = 0.1) +
  tm_shape(Amenities_Category_Scn1_SECC_sf) +
  tm_polygons(fill = "Social_Functions", fill_alpha = 0.5, col_alpha = 0.1) +
  tm_shape(Amenities_Category_Scn1_HEX_sf) +
  tm_polygons(fill = "Social_Functions", fill_alpha = 0.5, col_alpha = 0.1)

# Tiempo de procesamiento de Inventario por áreas:
End_Time_S4_1_P5 <- Sys.time()
print(paste("5. Tiempo de procesamiento de las categorías por áreas: ", as.duration(End_Time_S4_1_P5 - Start_Time_S4_1_P5)))

# ####
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ::: 6. Creamos mapa de capa de UPL con etiquetas para sobreponer           :::
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ####
#
Start_Time_S4_1_P6 <- Sys.time()

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

# Probamos que haya quedado bien.
Test_Plot <- Capa_Etiquetas_UPL |>
  ggplot() +
  geom_sf(
    data = Capa_Etiquetas_UPL,
    col = "gray",
    linewidth = 0.8,
    fill = "white",
    #aes(fill = "blue"),
    alpha = 0.01
  ) + 
  geom_text(
    data = Capa_Etiquetas_UPL,
    aes(x = X, y = Y, label = Etiqueta_Texto), # Mapea coordenadas y texto
    color = "gray",        # Color del texto
    size = 6,               # Tamaño del texto
    family = "Roboto Black" # Tipo de fuente
  ) +
  theme_void() +
  theme(legend.position = "none")
Test_Plot

# Tiempo de procesamiento de Inventario por áreas:
End_Time_S4_1_P6 <- Sys.time()
print(paste("6. Tiempo de creación de mapa de capa de ULP con etiquetas: ", as.duration(End_Time_S4_1_P6 - Start_Time_S4_1_P6)))

# ####
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ::: 7. Creamos mapa de puntos de equipamientos (Todos)                     :::
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ####
#
Start_Time_S4_1_P7 <- Sys.time()

# Basados en el shape original de puntos, creamos un df en el cual las categorías
# se encuentren en una sola columna. Como cada equipamiento hace parte de una sola
# categoría, no se repetirán filas. Cada punto, tendrá su categoría asignada.

Amenities_Puntos_Scn1_sf # Shape Original de puntos.

# Atención! Este proceso es muy lento! Se omite y se cargan los datos directamente.
# Shape_Puntos_Plotear_Scn1 <- Amenities_Puntos_Scn1_sf |>
#   pivot_longer(
#     cols = Education_and_Culture:Community_and_Spirituality, # Las columnas que queremos transformar
#     names_to = "CATEGORY",                                   # La nueva columna para los NOMBRES de las columnas
#     values_to = "valor"                                      # La nueva columna para los VALORES ("VERDADERO"/"FALSO")
#   ) |>
#   filter(valor == "VERDADERO") |>                            # Nos quedamos solo con las filas que nos interesan
#   st_transform(crs = Coordinates_CRS) |>                     # Cambiamos las coordenadas para hacer la intersección
#   st_intersection(Shape_UPLs |> select(-COD_UPL)) |>         # Al hacer la intersección, se viene el NOM_UPL
#   left_join(Shape_Localidades |> as_tibble() |> select(COD_LOC, NOM_LOC)) |> # Nos traemos el nombre de la LOC
#   left_join(Shape_UTAM |> as_tibble() |> select(COD_UTAM, NOM_UTAM)) |>      # Nos traemos el nombre de la UTAM
#   select(COD_LOC, NOM_LOC, COD_UPL, NOM_UPL, COD_UTAM, NOM_UTAM, COD_ZAT, COD_SECT, COD_SECC, COD_HEX, AMENITY, CATEGORY, names(Equipamientos_OSM)[8:16]) # Seleccionamos las columnas finales
# Shape_Puntos_Plotear_Scn1
# 
# # Guardamos para no repetir el proceso de intersección y cambio de coordenadas porque es un proceso muy demorado:
# save(
#   Shape_Puntos_Plotear_Scn1,
#   file = "./Data/2_Processing/4_Amenities/Scn1/Shape_Puntos_Plotear_Scn1.RData"
# )
load("./Data/2_Processing/4_Amenities/Scn1/Shape_Puntos_Plotear_Scn1.RData")
Shape_Puntos_Plotear_Scn1
names(Shape_Puntos_Plotear_Scn1)

# --- Ploteamos el mapa general de puntos con todos los equipamientos ---
#
# Este mapa se va a hacer en tres etapas: Base, Puntos y Bordes.

# Mapa base gris:
Plot_Base_UPL <- ggplot() +
  geom_sf(
    data = Shape_UPLs |> filter(COD_UPL %in% Lista_No_Rural$COD_UPL),
    col = "white",
    linewidth = 0.8,
    fill = "gray", # darkgray
    #aes(fill = "blue"),
    alpha = 1
  ) + 
  theme_void() +
  theme(
    legend.position = "none",
  )
#Plot_Base_UPL

# Mapa de puntos:
Mapa_Puntos <- Plot_Base_UPL +
  geom_sf(
    # Ordenamos en orden inverso para que los Housing no se noten tanto:
    data = Shape_Puntos_Plotear_Scn1 |> filter(COD_UPL %in% Lista_No_Rural$COD_UPL) |> mutate(row_num = row_number()) |> arrange(desc(row_num)) |> select(-row_num),
    size = 0.01,
    aes(col = CATEGORY)
  ) +
  scale_color_discrete(labels = function(x) gsub("_", " ", x))
#Mapa_Puntos

# Mapa de bordes de las UPLs:
Mapa_Plotear <- Mapa_Puntos +
  geom_sf(
    data = Capa_Etiquetas_UPL,
    col = "white",
    linewidth = 1,
    fill = "white",
    #aes(fill = "blue"),
    alpha = 0.01
  ) + 
  geom_text(
    data = Capa_Etiquetas_UPL,
    aes(x = X, y = Y, label = Etiqueta_Texto), # Mapea coordenadas y texto
    color = "white",        # Color del texto
    size = 4,               # Tamaño del texto
    family = "Roboto Black" # Tipo de fuente
  ) +
  theme_void() +
  # Texto blanco en la leyenda: 
  theme(
    legend.text = element_text(color = "lightgray"),
    legend.title = element_text(color = "lightgray")
  )# +
  #theme(legend.position = "none")
#Mapa_Plotear

# Guardamos el plot como imagen:
ggsave(
  filename = "Mapa_Puntos_Todos_Los_Equipamientos_Por_Categorias_2160x3840_WhiteText.png",
  plot = Mapa_Plotear,
  device = NULL,
  path = "./Data/3_Results/4_Amenities/Scn1/1_Points/",
  scale = 1,
  width = 2160, # Intercambiadas porque está en vertical
  height = 3840,
  units = c("px"),
  dpi = 300,
  limitsize = TRUE,
  bg = NULL,
  create.dir = FALSE,
)

# --- Hacemos los mapas individuales para todos las CATEGORIAS de equipamientos ---

for(i in 1:length(unique(Shape_Puntos_Plotear_Scn1$CATEGORY))){
  
  # Voy recorriendo los tipos de amenities uno por uno:
  Categoria_Actual <- sort(unique(Shape_Puntos_Plotear_Scn1$CATEGORY))[i]
  Titulo_Leyenda <- gsub("_", " ", Categoria_Actual)

  # Creo un mapa de progreso para saber cómo va la cosa:
  cat(paste("Generando mapa de categoría ", i, " de ", length(unique(Shape_Puntos_Plotear_Scn1$CATEGORY)), " para: '", Categoria_Actual, "'...\n", sep = ""))
  
  # Filtramos el sf solo para los actuales
  Datos_Para_Mapa_Actual <- Shape_Puntos_Plotear_Scn1 |> 
    filter(CATEGORY == Categoria_Actual) |>
    select(COD_UPL, CATEGORY, AMENITY)

  # Creamos el Mapa de puntos (reciclamos el mapa base de UPLs):
  # Mapa de puntos:
  Mapa_Puntos <- Plot_Base_UPL +
    geom_sf(
      data = Datos_Para_Mapa_Actual |> filter(COD_UPL %in% Lista_No_Rural$COD_UPL),
      size = 0.01,
      aes(col = AMENITY)
    ) +
    scale_color_discrete(labels = function(x) gsub("_", " ", x)) +
    labs(color = Titulo_Leyenda)
  #Mapa_Puntos
  
  # Mapa de bordes de las UPLs:
  Mapa_Plotear <- Mapa_Puntos +
    geom_sf(
      data = Capa_Etiquetas_UPL,
      col = "white",
      linewidth = 1,
      fill = "white",
      #aes(fill = "blue"),
      alpha = 0.01
    ) + 
    geom_text(
      data = Capa_Etiquetas_UPL,
      aes(x = X, y = Y, label = Etiqueta_Texto), # Mapea coordenadas y texto
      color = "white",        # Color del texto
      size = 4,               # Tamaño del texto
      family = "Roboto Black" # Tipo de fuente
    ) +
    theme_void() +
    # Texto blanco en la leyenda: 
    theme(
      legend.text = element_text(color = "lightgray"),
      legend.title = element_text(color = "lightgray")
    )# +
  #theme(legend.position = "none")
  #Mapa_Plotear
  
  # Guardamos el plot como imagen:
  ggsave(
    filename = paste("Map_Point_Amenities-", Categoria_Actual, ".png", sep = ""),
    plot = Mapa_Plotear,
    device = NULL,
    path = "./Data/3_Results/4_Amenities/Scn1/1_Points/Detailed_Maps/",
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

# --- Hacemos los mapas individuales para todos los AMENITIES de equipamientos ---

for(i in 1:length(unique(Shape_Puntos_Plotear_Scn1$AMENITY))){
  
  # Voy recorriendo los tipos de amenities uno por uno:
  Amenity_Actual <- sort(unique(Shape_Puntos_Plotear_Scn1$AMENITY))[i]
  Titulo_Leyenda <- gsub("_", " ", Amenity_Actual)
  
  # Creo un mapa de progreso para saber cómo va la cosa:
  cat(paste("Generando mapa de equipamientos ", i, " de ", length(unique(Shape_Puntos_Plotear_Scn1$AMENITY)), " para: '", Amenity_Actual, "'...\n", sep = ""))
  
  # Filtramos el sf solo para los actuales
  Datos_Para_Mapa_Actual <- Shape_Puntos_Plotear_Scn1 |> 
    filter(AMENITY == Amenity_Actual) |>
    select(COD_UPL, CATEGORY, AMENITY)
  
  Categoria_Actual <- sort(unique(Datos_Para_Mapa_Actual$CATEGORY))[1]
  
  # Creamos el Mapa de puntos (reciclamos el mapa base de UPLs):
  # Mapa de puntos:
  Mapa_Puntos <- Plot_Base_UPL +
    geom_sf(
      data = Datos_Para_Mapa_Actual |> filter(COD_UPL %in% Lista_No_Rural$COD_UPL),
      size = 0.01,
      aes(col = AMENITY)
    ) +
    scale_color_discrete(labels = function(x) gsub("_", " ", x)) +
    labs(color = Titulo_Leyenda)
  #Mapa_Puntos
  
  # Mapa de bordes de las UPLs:
  Mapa_Plotear <- Mapa_Puntos +
    geom_sf(
      data = Capa_Etiquetas_UPL,
      col = "white",
      linewidth = 1,
      fill = "white",
      #aes(fill = "blue"),
      alpha = 0.01
    ) + 
    geom_text(
      data = Capa_Etiquetas_UPL,
      aes(x = X, y = Y, label = Etiqueta_Texto), # Mapea coordenadas y texto
      color = "white",        # Color del texto
      size = 4,               # Tamaño del texto
      family = "Roboto Black" # Tipo de fuente
    ) +
    theme_void() +
    # Texto blanco en la leyenda: 
    theme(
      legend.text = element_text(color = "lightgray"),
      legend.title = element_text(color = "lightgray")
    )# +
  #theme(legend.position = "none")
  #Mapa_Plotear
  
  # Guardamos el plot como imagen:
  ggsave(
    filename = paste("Map_Point_Amenities-", Categoria_Actual,"-", Amenity_Actual, ".png", sep = ""),
    plot = Mapa_Plotear,
    device = NULL,
    path = "./Data/3_Results/4_Amenities/Scn1/1_Points/Detailed_Maps/",
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

# Tiempo de procesamiento de Inventario por áreas:
End_Time_S4_1_P7 <- Sys.time()
print(paste("7. Tiempo de creación de Mapa de puntos de equipamientos (todos): ", as.duration(End_Time_S4_1_P7 - Start_Time_S4_1_P7)))

# ####
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ::: 8. Creamos mapa de Equipamientos por HEX (Todos)                       :::
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ####
#
Start_Time_S4_1_P8 <- Sys.time()

# Seleccionamos las UPL no rurales
Lista_No_Rural <- Shape_UPLs |> filter(ZONA_UPL != "Rural") |> as_tibble() |> select(COD_UPL)
Lista_No_Rural

# Tomamos el Inventario de equipamientos de HEX, y descartamos los rurales:
Equipamientos_Por_HEX_Todos_sf <- Amenities_Inventory_Scn1_HEX_sf |>
  filter(COD_UPL %in% Lista_No_Rural$COD_UPL) |>
  select(COD_UPL, NOM_UPL, COD_HEX, Total_Equipamientos) |>
  arrange(desc(Total_Equipamientos))
Equipamientos_Por_HEX_Todos_sf

# Creamos el mapa de Equipamientos por HEX
Mapa_Equipamientos_Por_HEX_Todos <- Equipamientos_Por_HEX_Todos_sf |>
  st_intersection(Capa_Etiquetas_UPL) |>
  ggplot() +
  # Capa de equipamientos por HEX:
  geom_sf(
    col = "white",
    linewidth = 0.2, # 0.6
    aes(fill = Total_Equipamientos)
  ) +
  scale_fill_gradient(
    low = "gray",
    high = "darkblue",
    limits = c(0,2000)
  ) +
  # Capa de mapa de UPLs con etiquetas:
  geom_sf(
    data = Capa_Etiquetas_UPL,
    col = "white",
    linewidth = 1,
    fill = "white",
    alpha = 0.10
  ) + 
  geom_text(
    data = Capa_Etiquetas_UPL,
    aes(x = X, y = Y, label = Etiqueta_Texto), # Mapea coordenadas y texto
    color = "gray20",        # Color del texto
    size = 6,               # Tamaño del texto - Original: 6
    family = "Roboto Black" # Tipo de fuente
  ) +
  theme_void() +
  theme(legend.position = "none")
Mapa_Equipamientos_Por_HEX_Todos

# Guardamos el plot como imagen:
ggsave(
  filename = "Mapa_Equipamientos_Por_HEX_2160x3840.png",
  plot = Mapa_Equipamientos_Por_HEX_Todos,
  device = NULL,
  path = "./Data/3_Results/4_Amenities/Scn1/2_HEX/",
  scale = 1,
  width = 2160, # Intercambiadas porque está en vertical
  height = 3840,
  units = c("px"),
  dpi = 300,
  limitsize = TRUE,
  bg = NULL,
  create.dir = FALSE,
)

# Tiempo de procesamiento de Inventario por UPL:
End_Time_S4_1_P8 <- Sys.time()
print(paste("8. Tiempo de creación de gráfico y mapa de equipamientos por HEX (todos): ", as.duration(End_Time_S4_1_P8 - Start_Time_S4_1_P8)))

# ####
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ::: 9. Creamos mapa de Equipamientos por UPL (Todos)                       :::
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ####
#
Start_Time_S4_1_P9 <- Sys.time()

# --- Ploteamos el mapa de áreas con todos los equipamientos ---

# Seleccionamos las UPL no rurales
Lista_No_Rural <- Shape_UPLs |> filter(ZONA_UPL != "Rural") |> as_tibble() |> select(COD_UPL)
Lista_No_Rural

# Tomamos el Inventario de equipamientos de la UPL, y descartamos los rurales,
# y agregamos las coordenadas de las etiquetas:
Equipamientos_Por_UPL_Todos_sf <- Amenities_Inventory_Scn1_UPL_sf |>
  filter(COD_UPL %in% Lista_No_Rural$COD_UPL) |>
  select(COD_UPL, NOM_UPL, Total_Equipamientos) |>
  arrange(desc(Total_Equipamientos)) |>
  mutate(
    # Extrae las coordenadas del centroide de cada área
    centroides = st_centroid(geom),
    X = st_coordinates(centroides)[,1],
    Y = st_coordinates(centroides)[,2],
    # Define la columna que se usará como etiqueta
    Etiqueta_Texto = COD_UPL 
  )
Equipamientos_Por_UPL_Todos_sf

# Reordenar  otra vez el factor para que las barras vayan de menor a mayor
Equipamientos_Por_UPL_Todos_sf$COD_UPL <- factor(Equipamientos_Por_UPL_Todos_sf$COD_UPL, levels = Equipamientos_Por_UPL_Todos_sf$COD_UPL[order(Equipamientos_Por_UPL_Todos_sf$Total_Equipamientos)])
Equipamientos_Por_UPL_Todos_sf

# Gráfica de barras:
Plot_Equipamientos_Por_UPL_Todos_sf <- Equipamientos_Por_UPL_Todos_sf |>
  ggplot() +
  # Barras horizontales
  geom_col(aes(x = COD_UPL, y = Total_Equipamientos, fill = Total_Equipamientos), width = 0.9) +
  # Nombre de la UPL dentro de la barra, al inicio
  geom_text(aes(x = COD_UPL, y = 0, label = paste("UPL ", COD_UPL, sep = "")),
            hjust = 0,             # muy cerca del borde izquierdo
            nudge_y = max(Equipamientos_Por_UPL_Todos_sf$Total_Equipamientos) * 0.02,
            color = "white", size = 4, family = "Roboto", fontface = "bold") +
  # Valor de horas dentro de la barra, al final
  geom_text(aes(x = COD_UPL, y = Total_Equipamientos, label = Total_Equipamientos),
            hjust = 1.2,             # muy cerca del borde derecho (1.2 es el mejor)
            color = "white", size = 4, family = "Roboto") +
  # Voltear ejes para barras horizontales
  coord_flip() +
  # Asignar colores:
  scale_fill_gradient(
    low = "gray",
    high = "darkblue"
  ) +
  # Eliminar ejes, cuadrículas y leyenda
  theme_void() +
  theme(legend.position = "none") +
  # Añadir espacio a la derecha para que no se corte el texto
  scale_y_continuous(limits = c(0, max(Equipamientos_Por_UPL_Todos_sf$Total_Equipamientos) * 1.1), expand = c(0,0))
Plot_Equipamientos_Por_UPL_Todos_sf

# Guardamos como imagen el gráfico de barras:
ggsave(
  filename = "Grafica_Equipamientos_Por_UPL.png",
  plot = Plot_Equipamientos_Por_UPL_Todos_sf,
  device = NULL,
  path = "./Data/3_Results/4_Amenities/Scn1/4_UPL/",
  scale = 1,
  width = 1500,
  height = 1500,
  units = c("px"),
  dpi = 150,
  limitsize = TRUE,
  bg = NULL,
  create.dir = FALSE,
)

Mapa_Equipamientos_Por_UPL_Todos <- Equipamientos_Por_UPL_Todos_sf |>
  ggplot() +
  geom_sf(
    col = "white",
    linewidth = 0.8,
    aes(fill = Total_Equipamientos)
  ) + 
  scale_fill_gradient(
    low = "gray",
    high = "darkblue"
  ) +
  # Capa de etiquetas:
  geom_text(
    aes(x = X, y = Y, label = Etiqueta_Texto), # Mapea coordenadas y texto
    color = "white",        # Color del texto
    size = 6,               # Tamaño del texto
    family = "Roboto Black" # Tipo de fuente
  ) +
  theme_void() +
  theme(legend.position = "none")
Mapa_Equipamientos_Por_UPL_Todos

# Guardamos el plot como imagen:
ggsave(
  filename = "Mapa_Equipamientos_Por_UPL_2160x3840.png",
  plot = Mapa_Equipamientos_Por_UPL_Todos,
  device = NULL,
  path = "./Data/3_Results/4_Amenities/Scn1/4_UPL/",
  scale = 1,
  width = 2160, # Intercambiadas porque está en vertical
  height = 3840,
  units = c("px"),
  dpi = 300,
  limitsize = TRUE,
  bg = NULL,
  create.dir = FALSE,
)

# Tiempo de procesamiento de Inventario por UPL:
End_Time_S4_1_P9 <- Sys.time()
print(paste("9. Tiempo de creación de gráfico y mapa de equipamientos por UPL (todos): ", as.duration(End_Time_S4_1_P9 - Start_Time_S4_1_P9)))

# ####
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ::: 10. Creamos mapa de Equipamientos por UPL, por cada 100k Habitantes     :::
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ####
#
Start_Time_S4_1_P10 <- Sys.time()

# --- Ploteamos el mapa de áreas con todos los equipamientos ---

# Seleccionamos las UPL no rurales
Lista_No_Rural <- Shape_UPLs |> filter(ZONA_UPL != "Rural") |> as_tibble() |> select(COD_UPL)
Lista_No_Rural

# Tomamos el Inventario de equipamientos de la UPL, y extraemos:
Censo_UPLs_df <- as.data.frame(fread("./Data/2_Processing/2_Population/Censo_UPLs.csv", sep=",", na = "null", colClasses=c("COD_UPL"="character")))
Censo_UPLs_df

Equipamientos_Por_Habitantes_UPL_sf <- Amenities_Inventory_Scn1_UPL_sf |>
  filter(COD_UPL %in% Lista_No_Rural$COD_UPL) |>
  select(COD_UPL, NOM_UPL, Total_Equipamientos) |>
  left_join(
    Censo_UPLs_df |> select(COD_UPL, PERSONAS_UPL),
    by = "COD_UPL"
  ) |>
  mutate(
    Equip_10000_Hab = if_else(COD_UPL == "7", NA, (Total_Equipamientos/PERSONAS_UPL)*10000),
    # Extrae las coordenadas del centroide de cada área
    centroides = st_centroid(geom),
    X = st_coordinates(centroides)[,1],
    Y = st_coordinates(centroides)[,2],
    # Define la columna que se usará como etiqueta
    Etiqueta_Texto = COD_UPL
  )
Equipamientos_Por_Habitantes_UPL_sf

# Reordenar  otra vez el factor para que las barras vayan de menor a mayor
Equipamientos_Por_Habitantes_UPL_sf$COD_UPL <- factor(Equipamientos_Por_Habitantes_UPL_sf$COD_UPL, levels = Equipamientos_Por_Habitantes_UPL_sf$COD_UPL[order(Equipamientos_Por_Habitantes_UPL_sf$Equip_10000_Hab)])
Equipamientos_Por_Habitantes_UPL_sf

# Gráfica de barras:
Plot_Equipamientos_Por_Habitantes_UPL <- Equipamientos_Por_Habitantes_UPL_sf |>
  filter(COD_UPL != "7") |>
  ggplot() +
  # Barras horizontales
  geom_col(aes(x = COD_UPL, y = Equip_10000_Hab, fill = Equip_10000_Hab), width = 0.9) +
  # Nombre de la UPL dentro de la barra, al inicio
  geom_text(aes(x = COD_UPL, y = 0, label = paste("UPL ", COD_UPL, sep = "")),
            hjust = 0,             # muy cerca del borde izquierdo
            nudge_y = max(Equipamientos_Por_Habitantes_UPL_sf$Equip_10000_Hab, na.rm = TRUE) * 0.02,
            color = "white", size = 4, family = "Roboto", fontface = "bold") +
  # Valor de equipamientos por personas dentro de la barra, al final
  geom_text(aes(x = COD_UPL, y = Equip_10000_Hab, label = round(Equip_10000_Hab, 2)),
            hjust = 1.2,             # muy cerca del borde derecho (1.2 es el mejor)
            color = "white", size = 4, family = "Roboto") +
  # Voltear ejes para barras horizontales
  coord_flip() +
  # Asignar colores:
  scale_fill_gradient(
    low = "gray",
    high = "darkblue"
  ) +
  # Eliminar ejes, cuadrículas y leyenda
  theme_void() +
  theme(legend.position = "none") +
  # Añadir espacio a la derecha para que no se corte el texto
  scale_y_continuous(limits = c(0, max(Equipamientos_Por_Habitantes_UPL_sf$Equip_10000_Hab) * 1.1), expand = c(0,0))
Plot_Equipamientos_Por_Habitantes_UPL

# Guardamos como imagen el gráfico de barras:
ggsave(
  filename = "Grafica_Equipamientos_Por_Habitante_UPL.png",
  plot = Plot_Equipamientos_Por_Habitantes_UPL,
  device = NULL,
  path = "./Data/3_Results/4_Amenities/Scn1/4_UPL/",
  scale = 1,
  width = 1500,
  height = 1500,
  units = c("px"),
  dpi = 150,
  limitsize = TRUE,
  bg = NULL,
  create.dir = FALSE,
)

# Creamos el mapa correspondiente:
Mapa_Equipamientos_Por_Habitantes_UPL <- Equipamientos_Por_Habitantes_UPL_sf |>
  ggplot() +
  geom_sf(
    col = "white",
    linewidth = 0.8,
    aes(fill = Equip_10000_Hab)
  ) + 
  scale_fill_gradient(
    low = "gray",
    high = "darkblue",
    breaks = c(0, 500, 1000, 1500, 2000, 2500),
    limits = c(0, 2500)
  ) +
  # Capa de etiquetas:
  geom_text(
    aes(x = X, y = Y, label = Etiqueta_Texto), # Mapea coordenadas y texto
    color = "white",        # Color del texto
    size = 6,               # Tamaño del texto
    family = "Roboto Black" # Tipo de fuente
  ) +
  theme_void() +
  theme(legend.position = "none")
Mapa_Equipamientos_Por_Habitantes_UPL

# Guardamos el plot como imagen:
ggsave(
  filename = "Mapa_Equipamientos_Por_Habitante_UPL_2160x3840.png",
  plot = Mapa_Equipamientos_Por_Habitantes_UPL,
  device = NULL,
  path = "./Data/3_Results/4_Amenities/Scn1/4_UPL/",
  scale = 1,
  width = 2160, # Intercambiadas porque está en vertical
  height = 3840,
  units = c("px"),
  dpi = 300,
  limitsize = TRUE,
  bg = NULL,
  create.dir = FALSE,
)

# Tiempo de procesamiento de Inventario por UPL:
End_Time_S4_1_P10 <- Sys.time()
print(paste("10. Tiempo de creación de gráfico y mapa de equipamientos por UPL (por 100k habitantes): ", as.duration(End_Time_S4_1_P10 - Start_Time_S4_1_P10)))

# ####
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ::: 11. Creamos mapa de Equipamientos por Educación Superior por UPL       :::
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ####
#
Start_Time_S4_1_P11 <- Sys.time()

# Seleccionamos las UPL no rurales
Lista_No_Rural <- Shape_UPLs |> filter(ZONA_UPL != "Rural") |> as_tibble() |> select(COD_UPL)
Lista_No_Rural

# Tomamos el Inventario de equipamientos de la UPL, y extraemos:
Censo_UPLs_df <- as.data.frame(fread("./Data/2_Processing/2_Population/Censo_UPLs.csv", sep=",", na = "null", colClasses=c("COD_UPL"="character")))
Censo_UPLs_df

# Definimos la lista de equipamientos que pertenecen a la Educación superior:
Lista_Educacion_Superior <- c("Universidades", "Centros de formación técnica", "Institutos de investigación")

# Preparamos el sf de Educación superior por UPL:
Educacion_Superior_Por_UPL_sf <- Amenities_Inventory_Scn1_UPL_sf |>
  filter(COD_UPL %in% Lista_No_Rural$COD_UPL) |>
  select(COD_UPL, NOM_UPL, ZONA_UPL, all_of(Lista_Educacion_Superior)) |>
  mutate(
    Total_Centros_de_Educacion_Superior = `Universidades` + `Centros de formación técnica` + `Institutos de investigación`,
    # Extrae las coordenadas del centroide de cada área
    centroides = st_centroid(geom),
    X = st_coordinates(centroides)[,1],
    Y = st_coordinates(centroides)[,2],
    # Define la columna que se usará como etiqueta
    Etiqueta_Texto = COD_UPL
  ) |>
  arrange(desc(Total_Centros_de_Educacion_Superior)) |>
  st_transform(crs = Coordinates_CRS)
Educacion_Superior_Por_UPL_sf

# Reordenar  otra vez el factor para que las barras vayan de menor a mayor
Educacion_Superior_Por_UPL_sf$COD_UPL <- factor(Educacion_Superior_Por_UPL_sf$COD_UPL, levels = Educacion_Superior_Por_UPL_sf$COD_UPL[order(Educacion_Superior_Por_UPL_sf$Total_Centros_de_Educacion_Superior)])
Educacion_Superior_Por_UPL_sf

Color_Trabajo <- "darkred"
Color_Estudio <- "deepskyblue4"
Color_Tramites <- "darkgreen"
Color_Otros <- "darkgray"

# Gráfica de barras:
Plot_Educacion_Superior_Por_UPL <- Educacion_Superior_Por_UPL_sf |>
  #filter(COD_UPL != "7") |>
  ggplot() +
  # Barras horizontales
  geom_col(aes(x = COD_UPL, y = Total_Centros_de_Educacion_Superior, fill = Total_Centros_de_Educacion_Superior), width = 0.9) +
  # Nombre de la UPL dentro de la barra, al inicio
  # geom_text(aes(x = COD_UPL, y = 0, label = paste("UPL ", COD_UPL, sep = "")),
  #           hjust = 0,             # muy cerca del borde izquierdo
  #           nudge_y = max(Educacion_Superior_Por_UPL_sf$Total_Centros_de_Educacion_Superior, na.rm = TRUE) * 0.02,
  #           color = "white", size = 4, family = "Roboto", fontface = "bold") +
  # Valor de equipamientos por personas dentro de la barra, al final
  # geom_text(aes(x = COD_UPL, y = Total_Centros_de_Educacion_Superior, label = round(Total_Centros_de_Educacion_Superior, 2)),
  #           hjust = 1.2,             # muy cerca del borde derecho (1.2 es el mejor)
  #           color = "white", size = 4, family = "Roboto") +
  # Voltear ejes para barras horizontales
  coord_flip() +
  # Asignar colores:
  scale_fill_gradient(
    low = "gray",
    high = Color_Estudio
  ) +
  # Eliminar ejes, cuadrículas y leyenda
  theme_void() +
  theme(legend.position = "none") +
  # Añadir espacio a la derecha para que no se corte el texto
  scale_y_continuous(limits = c(0, max(Educacion_Superior_Por_UPL_sf$Total_Centros_de_Educacion_Superior) * 1.1), expand = c(0,0))
Plot_Educacion_Superior_Por_UPL

# Guardamos como imagen el gráfico de barras:
ggsave(
  filename = "Grafica_Educacion_Superior_Por_UPL.png",
  plot = Plot_Educacion_Superior_Por_UPL,
  device = NULL,
  path = "./Data/3_Results/4_Amenities/Scn1/4_UPL/",
  scale = 1,
  width = 1500,
  height = 1500,
  units = c("px"),
  dpi = 150,
  limitsize = TRUE,
  bg = NULL,
  create.dir = FALSE,
)

# Creamos el mapa correspondiente:
Mapa_Educacion_Superior_Por_UPL <- Educacion_Superior_Por_UPL_sf |>
  ggplot() +
  geom_sf(
    col = "white",
    linewidth = 0.8,
    aes(fill = Total_Centros_de_Educacion_Superior)
  ) + 
  scale_fill_gradient(
    low = "gray",
    high = Color_Estudio,
    # breaks = c(0, 500, 1000, 1500, 2000, 2500),
    # limits = c(0, 2500)
  ) +
  # Capa de etiquetas:
  geom_text(
    aes(x = X, y = Y, label = Etiqueta_Texto), # Mapea coordenadas y texto
    color = "white",        # Color del texto
    size = 6,               # Tamaño del texto
    family = "Roboto Black" # Tipo de fuente
  ) +
  theme_void() +
  theme(legend.position = "none")
Mapa_Educacion_Superior_Por_UPL

# Guardamos el plot como imagen:
ggsave(
  filename = "Mapa_Educacion_Superior_Por_UPL_2160x3840.png",
  plot = Mapa_Educacion_Superior_Por_UPL,
  device = NULL,
  path = "./Data/3_Results/4_Amenities/Scn1/4_UPL/",
  scale = 1,
  width = 2160, # Intercambiadas porque está en vertical
  height = 3840,
  units = c("px"),
  dpi = 300,
  limitsize = TRUE,
  bg = NULL,
  create.dir = FALSE,
)

# Datos para pegar en Word:
Temp <- Educacion_Superior_Por_UPL_sf |>
  st_drop_geometry() |>
  select(COD_UPL, Total_Centros_de_Educacion_Superior)

# Tiempo de procesamiento de Inventario por UPL:
End_Time_S4_1_P11 <- Sys.time()
print(paste("11. Tiempo de creación de gráfico y mapa de Universidades por UPL: ", as.duration(End_Time_S4_1_P11 - Start_Time_S4_1_P11)))


## --- EXTRAS - Los mapas adicionales y detallados ya son de adorno ---

# ####
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ::: 12. Extra - Creamos los mapas 2D y 3D para las posibles combinaciones  :::
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ####
#
# Se deben realizar los siguientes grupos de mapas en 2D:
#
#  7.1               7.2               7.3                7.4
#  Puntos            Hexágonos         UPL                LOC
#   └- Puntos 2D      ├- Total Equip    ├- Total Equip    ├- Total Equip
#                     ├- Categoria      ├- Categoria      ├- Categoria
#                     └- Equipamiento   ├- Equipamiento   ├- Equipamiento
#                                       ├- Equip/Habit    ├- Equip/Habit
#                                       ├- Puestos/Trab   ├- Puestos/Trab
#                                       └- Univer/Estud   └- Univer/Estud
#
Start_Time_S4_1_P12 <- Sys.time()

# --- 6.1 Mapas 2D para puntos ---

Resolucion_2D <- "2160p"

# ...::: Nivel 1 - Point :::...
# Mapas Generales de todos los puntos de los Equipamientos en un solo mapa [ES/EN]

# Mostramos avance:
print(paste("Creando mapa de puntos de Nivel 1: 1/1...", sep = ""))

Plot_Puntos_en_Shape(
  Shape_Base = Shape_UPLs,
  Datos_Puntos = Amenities_sf,
  Color = Amenities_sf$CATEGORIA, #En Español
  Leyenda = "Categoría de Equipamiento",
  Output_Path = "./Data/3_Results/Maps/3_Amenities/1_Points/Detailed_Maps",
  Output_Name = "ES_Mapa_Puntos_Todos los Equipamientos",
  Resolution = Resolucion_2D
)
Plot_Puntos_en_Shape(
  Shape_Base = Shape_UPLs,
  Datos_Puntos = Amenities_sf,
  Color = Amenities_sf$CATEGORY, #En Inglés
  Leyenda = "Amenity Categories",
  Output_Path = "./Data/3_Results/Maps/3_Amenities/1_Points/Detailed_Maps",
  Output_Name = "EN_Map_Points_All Amenities",
  Resolution = Resolucion_2D
)

# ...::: Nivel 2 - Point :::...
# Creamos los mapas de puntos en 2D para los totales de cada una de las Categorías [ES/EN]
for(i in 1:length(unique(Amenities_sf$CATEGORIA))){
  # Nos paramos en la categoria deseada:
  Temp_Categoria_ES <- unique(Amenities_sf$CATEGORIA)[i]
  Temp_Categoria_EN <- unique(Amenities_sf$CATEGORY)[i]
  
  # Filtramos por la categoria deseada:
  Filtered_sf <- Amenities_sf |>
    filter(CATEGORIA == Temp_Categoria_ES) |>
    select(EQUIPAMIENTO, AMENITY)
  
  # Mostramos avance:
  print(paste("Creando mapa de puntos de Nivel 2: ", i, "/", length(unique(Amenities_sf$CATEGORIA)), "...", sep = ""))

  # Generamos los mapas de puntos para cada una de las categorías
  Plot_Puntos_en_Shape(
    Shape_Base = Shape_UPLs,
    Datos_Puntos = Filtered_sf,
    Color = Filtered_sf$EQUIPAMIENTO, #En Español
    Leyenda = Temp_Categoria_ES,
    Output_Path = "./Data/3_Results/Maps/3_Amenities/1_Points/Detailed_Maps",
    Output_Name = paste("ES_Mapa_Puntos_", Temp_Categoria_ES, sep = ""),
    Resolution = Resolucion_2D
  )
  Plot_Puntos_en_Shape(
    Shape_Base = Shape_UPLs,
    Datos_Puntos = Filtered_sf,
    Color = Filtered_sf$AMENITY, #En Inglés
    Leyenda = Temp_Categoria_EN,
    Output_Path = "./Data/3_Results/Maps/3_Amenities/1_Points/Detailed_Maps",
    Output_Name = paste("EN_Map_Points_", Temp_Categoria_EN, sep = ""),
    Resolution = Resolucion_2D
  )
}

# ...::: Nivel 3 - Point :::...
# Creamos los mapas de puntos en 2D para cada uno de los tipos de equipamientos [ES/EN]
for(i in 1:length(unique(Amenities_sf$EQUIPAMIENTO))){
  # Nos paramos en el equipamiento deseado:
  Temp_Equipamiento_ES <- unique(Amenities_sf$EQUIPAMIENTO)[i]
  Temp_Equipamiento_EN <- unique(Amenities_sf$AMENITY)[i]
  Temp_sf <- Amenities_sf |>
    as_tibble() |>
    select(CATEGORIA, CATEGORY, EQUIPAMIENTO, AMENITY) |>
    filter(EQUIPAMIENTO == Temp_Equipamiento_ES) |>
    unique()
  Temp_Categoria_ES <- Temp_sf[1]
  Temp_Categoria_EN <- Temp_sf[2]
  
  # Filtramos por el equipamiento deseado:
  Filtered_sf <- Amenities_sf |>
    filter(EQUIPAMIENTO == Temp_Equipamiento_ES) |>
    select(EQUIPAMIENTO, AMENITY)
  
  # Mostramos avance:
  print(paste("Creando mapa de puntos de Nivel 3: ", i, "/", length(unique(Amenities_sf$EQUIPAMIENTO)), "...", sep = ""))

  # Generamos los mapas de puntos para cada una de las categorías
  Plot_Puntos_en_Shape(
    Shape_Base = Shape_UPLs,
    Datos_Puntos = Filtered_sf,
    Color = Filtered_sf$EQUIPAMIENTO, #En Español
    Leyenda = Temp_Equipamiento_ES,
    Output_Path = "./Data/3_Results/Maps/3_Amenities/1_Points/Detailed_Maps",
    Output_Name = paste("ES_Mapa_Puntos_", Temp_Categoria_ES, "_", Temp_Equipamiento_ES, sep = ""),
    Resolution = Resolucion_2D
  )
  Plot_Puntos_en_Shape(
    Shape_Base = Shape_UPLs,
    Datos_Puntos = Filtered_sf,
    Color = Filtered_sf$AMENITY, #En Inglés
    Leyenda = Temp_Equipamiento_EN,
    Output_Path = "./Data/3_Results/Maps/3_Amenities/1_Points/Detailed_Maps",
    Output_Name = paste("EN_Map_Points_", Temp_Categoria_EN, "_", Temp_Equipamiento_EN, sep = ""),
    Resolution = Resolucion_2D
  )
}

# --- 6.2 Mapas para áreas de Hexágonos 2D [ES/EN] y 3D ---

# Extraemos la lista de categorías y equipamientos en un df aparte.
Categorias_ES <- names(Shape_Equipamientos_Hexagonos)[str_which(names(Shape_Equipamientos_Hexagonos), "Categoria: ")]
Categorias_ES
Categorias_EN <- names(Shape_Equipamientos_Hexagonos)[str_which(names(Shape_Equipamientos_Hexagonos), "Category: ")]
Equipamientos_ES <- names(Shape_Equipamientos_Hexagonos)[str_which(names(Shape_Equipamientos_Hexagonos), "Equipamiento: ")]
Equipamientos_EN <- names(Shape_Equipamientos_Hexagonos)[str_which(names(Shape_Equipamientos_Hexagonos), "Amenity: ")]
Letras_Viridis <- LETTERS[1:8]

Resolucion_2D <- "2160p"
Resolucion_3D <- "720p"
Escala <- 100
Ancho_Linea <- 0.2
Shape_Plotear <- Shape_Equipamientos_Hexagonos

# ...::: Nivel 1 - HEX :::...
# Mapas Generales de todos los Equipamientos

# Seteamos las variables para el renderizado y creación imagen:
Criterio <- Shape_Equipamientos_Hexagonos$TOTAL_EQUIPAMIENTOS
Titulo_ES <- "Equipamientos de Bogotá DC"
Titulo_EN <- "Amenities of Bogota DC"
Output_Path <- "./Data/3_Results/Maps/3_Amenities/2_HEX/Detailed_Maps"
Output_Name_ES <- paste("HEX_Todos los equipamientos", sep = "")
Output_Name_EN <- paste("HEX_All amenities", sep = "")
#Paleta_Color <- scale_fill_gradient(low="bisque3", high="purple4")
Paleta_Color <- scale_fill_viridis_c(option = Letras_Viridis[7])

print(paste("Creando mapa HEX de Nivel 1: 1/1...", sep = ""))

# 2D Español/Inglés:
Plot_Color(
  Shape_Plotear = Shape_Plotear,
  Criterio_Color = Criterio,
  Paleta_Color = Paleta_Color,
  Resolution = Resolucion_2D,
  Line_Width = Ancho_Linea,
  Output_Path = Output_Path,
  Output_Name_ES = paste("ES_Mapa_2D_", Output_Name_ES, sep = ""),
  Output_Name_EN = paste("EN_Map_2D_", Output_Name_EN, sep = "")
)
# 3D Español/Inglés:
PlotMapa3D(
  Shape_Plotear = Shape_Plotear,
  Criterio_Extrusion = Criterio,
  Paleta_Color = Paleta_Color,
  Resolution = Resolucion_3D,
  Scale = Escala,
  Output_Path = Output_Path,
  Output_Name_ES = paste("ES_Mapa_3D_", Output_Name_ES, sep = ""),
  Output_Name_EN = paste("EN_Map_3D_", Output_Name_EN, sep = "")
)

# ...::: Nivel 2 - HEX :::...
# Mapas para los equipamientos cada categoría

for(i in 1:length(Categorias_ES)){
  # Seteamos las variables para el plot
  Shape_Plotear <- Shape_Equipamientos_Hexagonos |>
    select(COD_HEX, Categorias_ES[i])
  Criterio <- Shape_Plotear[[Categorias_ES[i]]]
  Titulo_ES <- Categorias_ES[i]
  Titulo_EN <- Categorias_EN[i]
  Output_Path <- "./Data/3_Results/Maps/3_Amenities/2_HEX/Detailed_Maps"
  Output_Name_ES <- paste("HEX_", str_replace(Titulo_ES, "Categoria: ", ""), sep = "")
  Output_Name_EN <- paste("HEX_", str_replace(Titulo_EN, "Category: ", ""), sep = "")
  Paleta_Color <- scale_fill_viridis_c(option = Letras_Viridis[i])
  
  # Mostramos avance:
  print(paste("Creando mapa HEX de Nivel 2: ", i, "/", length(Categorias_ES), "...", sep = ""))
  
  # 2D Español/Inglés:
  Plot_Color(
    Shape_Plotear = Shape_Plotear,
    Criterio_Color = Criterio,
    Paleta_Color = Paleta_Color,
    Resolution = Resolucion_2D,
    Line_Width = Ancho_Linea,
    Output_Path = Output_Path,
    Output_Name_ES = paste("ES_Mapa_2D_", Output_Name_ES, sep = ""),
    Output_Name_EN = paste("EN_Map_2D_", Output_Name_EN, sep = "")
  )
  # 3D Español/Inglés:
  PlotMapa3D(
    Shape_Plotear = Shape_Plotear,
    Criterio_Extrusion = Criterio,
    Paleta_Color = Paleta_Color,
    Resolution = Resolucion_3D,
    Scale = Escala,
    Output_Path = Output_Path,
    Output_Name_ES = paste("ES_Mapa_3D_", Output_Name_ES, sep = ""),
    Output_Name_EN = paste("EN_Map_3D_", Output_Name_EN, sep = "")
  )
}

# ...::: Nivel 3 - HEX :::...
# Mapas para los equipamientos cada tipo de equipamiento

# Extraemos los nombres de categorías y equipamientos:
Nombres_Categoria_Equipamiento <- Amenities_sf |>
  as_tibble() |>
  select(CATEGORIA, CATEGORY, EQUIPAMIENTO, AMENITY) |>
  group_by(CATEGORIA, CATEGORY, EQUIPAMIENTO, AMENITY) |>
  count() |>
  mutate(
    NOM_ES = paste(CATEGORIA, " - ", EQUIPAMIENTO, sep = ""),
    NOM_EN = paste(CATEGORY, " - ", AMENITY, sep = ""),
  ) |>
  as_tibble() |>
  select(-n)

for(i in 1:length(Equipamientos_ES)){
  Shape_Plotear <- Shape_Equipamientos_Hexagonos |>
    select(COD_HEX, Equipamientos_ES[i])
  Criterio <- Shape_Plotear[[Equipamientos_ES[i]]]
  Indice_Nombre <- which(Nombres_Categoria_Equipamiento$EQUIPAMIENTO == str_replace(Equipamientos_ES[i], "Equipamiento: ", ""))
  Titulo_ES <- Nombres_Categoria_Equipamiento$NOM_ES[Indice_Nombre]
  Titulo_EN <- Nombres_Categoria_Equipamiento$NOM_EN[Indice_Nombre]
  Output_Path <- "./Data/3_Results/Maps/3_Amenities/2_HEX/Detailed_Maps"
  Output_Name_ES <- paste("HEX_", Titulo_ES, sep = "")
  Output_Name_EN <- paste("HEX_", Titulo_EN, sep = "")
  Indice_Categoria <- which(Categorias_ES == paste("Categoria: ", Nombres_Categoria_Equipamiento$CATEGORIA[Indice_Nombre], sep = ""))
  Paleta_Color <- scale_fill_viridis_c(option = Letras_Viridis[Indice_Categoria])
  
  # Mostramos avance:
  print(paste("Creando mapa HEX de Nivel 3: ", i, "/", length(Equipamientos_ES), "...", sep = ""))

  # 2D Español/Inglés:
  Plot_Color(
    Shape_Plotear = Shape_Plotear,
    Criterio_Color = Criterio,
    Paleta_Color = Paleta_Color,
    Resolution = Resolucion_2D,
    Line_Width = Ancho_Linea,
    Output_Path = Output_Path,
    Output_Name_ES = paste("ES_Mapa_2D_", Output_Name_ES, sep = ""),
    Output_Name_EN = paste("EN_Map_2D_", Output_Name_EN, sep = "")
    )
  # 3D Español/Inglés:
  PlotMapa3D(
    Shape_Plotear = Shape_Plotear,
    Criterio_Extrusion = Criterio,
    Paleta_Color = Paleta_Color,
    Resolution = Resolucion_3D,
    Scale = Escala,
    Output_Path = Output_Path,
    Output_Name_ES = paste("ES_Mapa_3D_", Output_Name_ES, sep = ""),
    Output_Name_EN = paste("EN_Map_3D_", Output_Name_EN, sep = "")
  )
}

# --- 6.3 Mapas para áreas de UPLs 2D [ES/EN] y 3D ---

# Extraemos la lista de categorías y equipamientos en un df aparte.
Categorias_ES <- names(Shape_Equipamientos_UPLs)[str_which(names(Shape_Equipamientos_UPLs), "Categoria: ")]
Categorias_EN <- names(Shape_Equipamientos_UPLs)[str_which(names(Shape_Equipamientos_UPLs), "Category: ")]
Equipamientos_ES <- names(Shape_Equipamientos_UPLs)[str_which(names(Shape_Equipamientos_UPLs), "Equipamiento: ")]
Equipamientos_EN <- names(Shape_Equipamientos_UPLs)[str_which(names(Shape_Equipamientos_UPLs), "Amenity: ")]
Letras_Viridis <- LETTERS[1:8]

Resolucion_2D <- "2160p"
Resolucion_3D <- "720p"
Escala <- 50
Ancho_Linea <- 0.8

# ...::: Nivel 1 - UPL :::...
# Mapas Generales de todos los Equipamientos

# Seteamos las variables para el renderizado y creación imagen:
Shape_Plotear <- Shape_Equipamientos_UPLs
Criterio <- Shape_Plotear$TOTAL_EQUIPAMIENTOS
Titulo_ES <- "Equipamientos de Bogotá DC"
Titulo_EN <- "Amenities of Bogota DC"
Output_Path <- "./Data/3_Results/Maps/3_Amenities/3_UPL/Detailed_Maps"
Output_Name_ES <- paste("UPL_Todos los equipamientos", sep = "")
Output_Name_EN <- paste("UPL_All amenities", sep = "")
#Paleta_Color <- scale_fill_gradient(low="bisque3", high="purple4")
Paleta_Color <- scale_fill_viridis_c(option = Letras_Viridis[7])

print(paste("Creando mapa UPL de Nivel 1: 1/1...", sep = ""))

# 2D Español/Inglés:
Plot_Color(
  Shape_Plotear = Shape_Plotear,
  Criterio_Color = Criterio,
  Paleta_Color = Paleta_Color,
  Resolution = Resolucion_2D,
  Line_Width = Ancho_Linea,
  Output_Path = Output_Path,
  Output_Name_ES = paste("ES_Mapa_2D_", Output_Name_ES, sep = ""),
  Output_Name_EN = paste("EN_Map_2D_", Output_Name_EN, sep = "")
)
# 3D Español/Inglés:
PlotMapa3D(
  Shape_Plotear = Shape_Plotear,
  Criterio_Extrusion = Criterio,
  Paleta_Color = Paleta_Color,
  Resolution = Resolucion_3D,
  Scale = Escala,
  Output_Path = Output_Path,
  Output_Name_ES = paste("ES_Mapa_3D_", Output_Name_ES, sep = ""),
  Output_Name_EN = paste("EN_Map_3D_", Output_Name_EN, sep = "")
)

# ...::: Nivel 2 - UPL :::...
# Mapas para los equipamientos cada categoría

for(i in 1:length(Categorias_ES)){
  # Seteamos las variables para el plot
  Shape_Plotear <- Shape_Equipamientos_UPLs |>
    select(COD_UPL, Categorias_ES[i])
  Criterio <- Shape_Plotear[[Categorias_ES[i]]]
  Titulo_ES <- Categorias_ES[i]
  Titulo_EN <- Categorias_EN[i]
  Output_Path <- "./Data/3_Results/Maps/3_Amenities/3_UPL/Detailed_Maps"
  Output_Name_ES <- paste("UPL_", str_replace(Titulo_ES, "Categoria: ", ""), sep = "")
  Output_Name_EN <- paste("UPL_", str_replace(Titulo_EN, "Category: ", ""), sep = "")
  Paleta_Color <- scale_fill_viridis_c(option = Letras_Viridis[i])
  
  # Mostramos avance:
  print(paste("Creando mapa UPL de Nivel 2: ", i, "/", length(Categorias_ES), "...", sep = ""))
  
  # 2D Español/Inglés:
  Plot_Color(
    Shape_Plotear = Shape_Plotear,
    Criterio_Color = Criterio,
    Paleta_Color = Paleta_Color,
    Resolution = Resolucion_2D,
    Line_Width = Ancho_Linea,
    Output_Path = Output_Path,
    Output_Name_ES = paste("ES_Mapa_2D_", Output_Name_ES, sep = ""),
    Output_Name_EN = paste("EN_Map_2D_", Output_Name_EN, sep = "")
  )
  # 3D Español/Inglés:
  PlotMapa3D(
    Shape_Plotear = Shape_Plotear,
    Criterio_Extrusion = Criterio,
    Paleta_Color = Paleta_Color,
    Resolution = Resolucion_3D,
    Scale = Escala,
    Output_Path = Output_Path,
    Output_Name_ES = paste("ES_Mapa_3D_", Output_Name_ES, sep = ""),
    Output_Name_EN = paste("EN_Map_3D_", Output_Name_EN, sep = "")
  )
}

# ...::: Nivel 3 - UPL :::...
# Mapas para los equipamientos cada tipo de equipamiento

# Extraemos los nombres de categorías y equipamientos:
Nombres_Categoria_Equipamiento <- Amenities_sf |>
  as_tibble() |>
  select(CATEGORIA, CATEGORY, EQUIPAMIENTO, AMENITY) |>
  group_by(CATEGORIA, CATEGORY, EQUIPAMIENTO, AMENITY) |>
  count() |>
  mutate(
    NOM_ES = paste(CATEGORIA, " - ", EQUIPAMIENTO, sep = ""),
    NOM_EN = paste(CATEGORY, " - ", AMENITY, sep = ""),
  ) |>
  as_tibble() |>
  select(-n)

for(i in 1:length(Equipamientos_ES)){
  Shape_Plotear <- Shape_Equipamientos_UPLs |>
    select(COD_UPL, Equipamientos_ES[i])
  Criterio <- Shape_Plotear[[Equipamientos_ES[i]]]
  Indice_Nombre <- which(Nombres_Categoria_Equipamiento$EQUIPAMIENTO == str_replace(Equipamientos_ES[i], "Equipamiento: ", ""))
  Titulo_ES <- Nombres_Categoria_Equipamiento$NOM_ES[Indice_Nombre]
  Titulo_EN <- Nombres_Categoria_Equipamiento$NOM_EN[Indice_Nombre]
  Output_Path <- "./Data/3_Results/Maps/3_Amenities/3_UPL/Detailed_Maps"
  Output_Name_ES <- paste("UPL_", Titulo_ES, sep = "")
  Output_Name_EN <- paste("UPL_", Titulo_EN, sep = "")
  Indice_Categoria <- which(Categorias_ES == paste("Categoria: ", Nombres_Categoria_Equipamiento$CATEGORIA[Indice_Nombre], sep = ""))
  Paleta_Color <- scale_fill_viridis_c(option = Letras_Viridis[Indice_Categoria])
  
  # Mostramos avance:
  print(paste("Creando mapa UPL de Nivel 3: ", i, "/", length(Equipamientos_ES), "...", sep = ""))
  
  # 2D Español/Inglés:
  Plot_Color(
    Shape_Plotear = Shape_Plotear,
    Criterio_Color = Criterio,
    Paleta_Color = Paleta_Color,
    Resolution = Resolucion_2D,
    Line_Width = Ancho_Linea,
    Output_Path = Output_Path,
    Output_Name_ES = paste("ES_Mapa_2D_", Output_Name_ES, sep = ""),
    Output_Name_EN = paste("EN_Map_2D_", Output_Name_EN, sep = "")
  )
  # 3D Español/Inglés:
  PlotMapa3D(
    Shape_Plotear = Shape_Plotear,
    Criterio_Extrusion = Criterio,
    Paleta_Color = Paleta_Color,
    Resolution = Resolucion_3D,
    Scale = Escala,
    Output_Path = Output_Path,
    Output_Name_ES = paste("ES_Mapa_3D_", Output_Name_ES, sep = ""),
    Output_Name_EN = paste("EN_Map_3D_", Output_Name_EN, sep = "")
  )
}

# ...::: Mapas de Equipamientos por cada 1000 habitantes - UPL :::...

Shape_Plotear <- Shape_Equipamientos_UPLs |>
  select(COD_UPL, EQUIPAMIENTOS_PERSONAS)
Criterio <- Shape_Plotear$EQUIPAMIENTOS_PERSONAS
Titulo_ES <- "Equipamientos por cada 1000 habitantes"
Titulo_EN <- "Facilities per 1000 inhabitants"
Output_Path <- "./Data/3_Results/Maps/3_Amenities/3_UPL/Detailed_Maps"
Output_Name_ES <- paste("UPL_", Titulo_ES, sep = "")
Output_Name_EN <- paste("UPL_", Titulo_EN, sep = "")
Paleta_Color <- scale_fill_viridis_c(option = "H")

# Mostramos avance:
print(paste("Creando mapas de: ", Titulo_ES, " - UPL...", sep = ""))

# 2D Español/Inglés:
Plot_Color(
  Shape_Plotear = Shape_Plotear,
  Criterio_Color = Criterio,
  Paleta_Color = Paleta_Color,
  Resolution = Resolucion_2D,
  Line_Width = Ancho_Linea,
  Output_Path = Output_Path,
  Output_Name_ES = paste("ES_Mapa_2D_", Output_Name_ES, sep = ""),
  Output_Name_EN = paste("EN_Map_2D_", Output_Name_EN, sep = "")
)
# 3D Español/Inglés:
PlotMapa3D(
  Shape_Plotear = Shape_Plotear,
  Criterio_Extrusion = Criterio,
  Paleta_Color = Paleta_Color,
  Resolution = Resolucion_3D,
  Scale = Escala,
  Output_Path = Output_Path,
  Output_Name_ES = paste("ES_Mapa_3D_", Output_Name_ES, sep = ""),
  Output_Name_EN = paste("EN_Map_3D_", Output_Name_EN, sep = "")
)


# ...::: Mapas de Puestos de trabajo, por trabajadores - UPL :::...

Shape_Plotear <- Shape_Equipamientos_UPLs |>
  select(COD_UPL, PUESTOS_TRABAJADORES)
Criterio <- Shape_Plotear$PUESTOS_TRABAJADORES
Titulo_ES <- "Lugares de trabajo por cada 1000 trabajadores"
Titulo_EN <- "Workplaces per 1,000 workers"
Output_Path <- "./Data/3_Results/Maps/3_Amenities/3_UPL/Detailed_Maps"
Output_Name_ES <- paste("UPL_", Titulo_ES, sep = "")
Output_Name_EN <- paste("UPL_", Titulo_EN, sep = "")
Paleta_Color <- scale_fill_viridis_c(option = "H")

# Mostramos avance:
print(paste("Creando mapas de: ", Titulo_ES, " - UPL...", sep = ""))

# 2D Español/Inglés:
Plot_Color(
  Shape_Plotear = Shape_Plotear,
  Criterio_Color = Criterio,
  Paleta_Color = Paleta_Color,
  Resolution = Resolucion_2D,
  Line_Width = Ancho_Linea,
  Output_Path = Output_Path,
  Output_Name_ES = paste("ES_Mapa_2D_", Output_Name_ES, sep = ""),
  Output_Name_EN = paste("EN_Map_2D_", Output_Name_EN, sep = "")
)
# 3D Español/Inglés:
PlotMapa3D(
  Shape_Plotear = Shape_Plotear,
  Criterio_Extrusion = Criterio,
  Paleta_Color = Paleta_Color,
  Resolution = Resolucion_3D,
  Scale = Escala,
  Output_Path = Output_Path,
  Output_Name_ES = paste("ES_Mapa_3D_", Output_Name_ES, sep = ""),
  Output_Name_EN = paste("EN_Map_3D_", Output_Name_EN, sep = "")
)

# ...::: Mapas de Universidades por universitarios - UPL :::...

Shape_Plotear <- Shape_Equipamientos_UPLs |>
  select(COD_UPL, UNIVERSIDADES_ESTUDIANTES)
Criterio <- Shape_Plotear$UNIVERSIDADES_ESTUDIANTES
Titulo_ES <- "Universidades por cada 1000 universitarios"
Titulo_EN <- "Universities per 1000 university students"
Output_Path <- "./Data/3_Results/Maps/3_Amenities/3_UPL/Detailed_Maps"
Output_Name_ES <- paste("UPL_", Titulo_ES, sep = "")
Output_Name_EN <- paste("UPL_", Titulo_EN, sep = "")
Paleta_Color <- scale_fill_viridis_c(option = "H")

# Mostramos avance:
print(paste("Creando mapas de: ", Titulo_ES, " - UPL...", sep = ""))

# 2D Español/Inglés:
Plot_Color(
  Shape_Plotear = Shape_Plotear,
  Criterio_Color = Criterio,
  Paleta_Color = Paleta_Color,
  Resolution = Resolucion_2D,
  Line_Width = Ancho_Linea,
  Output_Path = Output_Path,
  Output_Name_ES = paste("ES_Mapa_2D_", Output_Name_ES, sep = ""),
  Output_Name_EN = paste("EN_Map_2D_", Output_Name_EN, sep = "")
)
# 3D Español/Inglés:
PlotMapa3D(
  Shape_Plotear = Shape_Plotear,
  Criterio_Extrusion = Criterio,
  Paleta_Color = Paleta_Color,
  Resolution = Resolucion_3D,
  Scale = Escala,
  Output_Path = Output_Path,
  Output_Name_ES = paste("ES_Mapa_3D_", Output_Name_ES, sep = ""),
  Output_Name_EN = paste("EN_Map_3D_", Output_Name_EN, sep = "")
)


# --- 6.4 Mapas para áreas de Localidades 2D [ES/EN] y 3D ---

# Extraemos la lista de categorías y equipamientos en un df aparte.
Categorias_ES <- names(Shape_Equipamientos_Localidades)[str_which(names(Shape_Equipamientos_Localidades), "Categoria: ")]
Categorias_EN <- names(Shape_Equipamientos_Localidades)[str_which(names(Shape_Equipamientos_Localidades), "Category: ")]
Equipamientos_ES <- names(Shape_Equipamientos_Localidades)[str_which(names(Shape_Equipamientos_Localidades), "Equipamiento: ")]
Equipamientos_EN <- names(Shape_Equipamientos_Localidades)[str_which(names(Shape_Equipamientos_Localidades), "Amenity: ")]
Letras_Viridis <- LETTERS[1:8]

Resolucion_2D <- "2160p"
Resolucion_3D <- "720p"
Escala <- 50
Ancho_Linea <- 0.8

# ...::: Nivel 1 - LOC :::...
# Mapas Generales de todos los Equipamientos

# Seteamos las variables para el renderizado y creación imagen:
Shape_Plotear <- Shape_Equipamientos_Localidades
Criterio <- Shape_Plotear$TOTAL_EQUIPAMIENTOS
Titulo_ES <- "Equipamientos de Bogotá DC"
Titulo_EN <- "Amenities of Bogota DC"
Output_Path <- "./Data/3_Results/Maps/3_Amenities/4_LOC/Detailed_Maps"
Output_Name_ES <- paste("LOC_Todos los equipamientos", sep = "")
Output_Name_EN <- paste("LOC_All amenities", sep = "")
#Paleta_Color <- scale_fill_gradient(low="bisque3", high="purple4")
Paleta_Color <- scale_fill_viridis_c(option = Letras_Viridis[7])

print(paste("Creando mapa LOC de Nivel 1: 1/1...", sep = ""))

# 2D Español/Inglés:
Plot_Color(
  Shape_Plotear = Shape_Plotear,
  Criterio_Color = Criterio,
  Paleta_Color = Paleta_Color,
  Resolution = Resolucion_2D,
  Line_Width = Ancho_Linea,
  Output_Path = Output_Path,
  Output_Name_ES = paste("ES_Mapa_2D_", Output_Name_ES, sep = ""),
  Output_Name_EN = paste("EN_Map_2D_", Output_Name_EN, sep = "")
)
# 3D Español/Inglés:
PlotMapa3D(
  Shape_Plotear = Shape_Plotear,
  Criterio_Extrusion = Criterio,
  Paleta_Color = Paleta_Color,
  Resolution = Resolucion_3D,
  Scale = Escala,
  Output_Path = Output_Path,
  Output_Name_ES = paste("ES_Mapa_3D_", Output_Name_ES, sep = ""),
  Output_Name_EN = paste("EN_Map_3D_", Output_Name_EN, sep = "")
)

# ...::: Nivel 2 - LOC :::...
# Mapas para los equipamientos cada categoría

for(i in 1:length(Categorias_ES)){
  # Seteamos las variables para el plot
  Shape_Plotear <- Shape_Equipamientos_Localidades |>
    select(COD_LOC, Categorias_ES[i])
  Criterio <- Shape_Plotear[[Categorias_ES[i]]]
  Titulo_ES <- Categorias_ES[i]
  Titulo_EN <- Categorias_EN[i]
  Output_Path <- "./Data/3_Results/Maps/3_Amenities/4_LOC/Detailed_Maps"
  Output_Name_ES <- paste("LOC_", str_replace(Titulo_ES, "Categoria: ", ""), sep = "")
  Output_Name_EN <- paste("LOC_", str_replace(Titulo_EN, "Category: ", ""), sep = "")
  Paleta_Color <- scale_fill_viridis_c(option = Letras_Viridis[i])
  
  # Mostramos avance:
  print(paste("Creando mapa LOC de Nivel 2: ", i, "/", length(Categorias_ES), "...", sep = ""))
  
  # 2D Español/Inglés:
  Plot_Color(
    Shape_Plotear = Shape_Plotear,
    Criterio_Color = Criterio,
    Paleta_Color = Paleta_Color,
    Resolution = Resolucion_2D,
    Line_Width = Ancho_Linea,
    Output_Path = Output_Path,
    Output_Name_ES = paste("ES_Mapa_2D_", Output_Name_ES, sep = ""),
    Output_Name_EN = paste("EN_Map_2D_", Output_Name_EN, sep = "")
  )
  # 3D Español/Inglés:
  PlotMapa3D(
    Shape_Plotear = Shape_Plotear,
    Criterio_Extrusion = Criterio,
    Paleta_Color = Paleta_Color,
    Resolution = Resolucion_3D,
    Scale = Escala,
    Output_Path = Output_Path,
    Output_Name_ES = paste("ES_Mapa_3D_", Output_Name_ES, sep = ""),
    Output_Name_EN = paste("EN_Map_3D_", Output_Name_EN, sep = "")
  )
}

# ...::: Nivel 3 - LOC :::...
# Mapas para los equipamientos cada tipo de equipamiento

# Extraemos los nombres de categorías y equipamientos:
Nombres_Categoria_Equipamiento <- Amenities_sf |>
  as_tibble() |>
  select(CATEGORIA, CATEGORY, EQUIPAMIENTO, AMENITY) |>
  group_by(CATEGORIA, CATEGORY, EQUIPAMIENTO, AMENITY) |>
  count() |>
  mutate(
    NOM_ES = paste(CATEGORIA, " - ", EQUIPAMIENTO, sep = ""),
    NOM_EN = paste(CATEGORY, " - ", AMENITY, sep = ""),
  ) |>
  as_tibble() |>
  select(-n)

for(i in 1:length(Equipamientos_ES)){
  Shape_Plotear <- Shape_Equipamientos_Localidades |>
    select(COD_LOC, Equipamientos_ES[i])
  Criterio <- Shape_Plotear[[Equipamientos_ES[i]]]
  Indice_Nombre <- which(Nombres_Categoria_Equipamiento$EQUIPAMIENTO == str_replace(Equipamientos_ES[i], "Equipamiento: ", ""))
  Titulo_ES <- Nombres_Categoria_Equipamiento$NOM_ES[Indice_Nombre]
  Titulo_EN <- Nombres_Categoria_Equipamiento$NOM_EN[Indice_Nombre]
  Output_Path <- "./Data/3_Results/Maps/3_Amenities/4_LOC/Detailed_Maps"
  Output_Name_ES <- paste("LOC_", Titulo_ES, sep = "")
  Output_Name_EN <- paste("LOC_", Titulo_EN, sep = "")
  Indice_Categoria <- which(Categorias_ES == paste("Categoria: ", Nombres_Categoria_Equipamiento$CATEGORIA[Indice_Nombre], sep = ""))
  Paleta_Color <- scale_fill_viridis_c(option = Letras_Viridis[Indice_Categoria])
  
  # Mostramos avance:
  print(paste("Creando mapa LOC de Nivel 3: ", i, "/", length(Equipamientos_ES), "...", sep = ""))
  
  # 2D Español/Inglés:
  Plot_Color(
    Shape_Plotear = Shape_Plotear,
    Criterio_Color = Criterio,
    Paleta_Color = Paleta_Color,
    Resolution = Resolucion_2D,
    Line_Width = Ancho_Linea,
    Output_Path = Output_Path,
    Output_Name_ES = paste("ES_Mapa_2D_", Output_Name_ES, sep = ""),
    Output_Name_EN = paste("EN_Map_2D_", Output_Name_EN, sep = "")
  )
  # 3D Español/Inglés:
  PlotMapa3D(
    Shape_Plotear = Shape_Plotear,
    Criterio_Extrusion = Criterio,
    Paleta_Color = Paleta_Color,
    Resolution = Resolucion_3D,
    Scale = Escala,
    Output_Path = Output_Path,
    Output_Name_ES = paste("ES_Mapa_3D_", Output_Name_ES, sep = ""),
    Output_Name_EN = paste("EN_Map_3D_", Output_Name_EN, sep = "")
  )
}

# ...::: Mapas de Equipamientos por cada 1000 habitantes - LOC :::...

Shape_Plotear <- Shape_Equipamientos_Localidades |>
  select(COD_LOC, EQUIPAMIENTOS_PERSONAS)
Criterio <- Shape_Plotear$EQUIPAMIENTOS_PERSONAS
Titulo_ES <- "Equipamientos por cada 1000 habitantes"
Titulo_EN <- "Facilities per 1000 inhabitants"
Output_Path <- "./Data/3_Results/Maps/3_Amenities/4_LOC/Detailed_Maps"
Output_Name_ES <- paste("LOC_", Titulo_ES, sep = "")
Output_Name_EN <- paste("LOC_", Titulo_EN, sep = "")
Paleta_Color <- scale_fill_viridis_c(option = "H")

# Mostramos avance:
print(paste("Creando mapas de: ", Titulo_ES, " - LOC...", sep = ""))

# 2D Español/Inglés:
Plot_Color(
  Shape_Plotear = Shape_Plotear,
  Criterio_Color = Criterio,
  Paleta_Color = Paleta_Color,
  Resolution = Resolucion_2D,
  Line_Width = Ancho_Linea,
  Output_Path = Output_Path,
  Output_Name_ES = paste("ES_Mapa_2D_", Output_Name_ES, sep = ""),
  Output_Name_EN = paste("EN_Map_2D_", Output_Name_EN, sep = "")
)
# 3D Español/Inglés:
PlotMapa3D(
  Shape_Plotear = Shape_Plotear,
  Criterio_Extrusion = Criterio,
  Paleta_Color = Paleta_Color,
  Resolution = Resolucion_3D,
  Scale = Escala,
  Output_Path = Output_Path,
  Output_Name_ES = paste("ES_Mapa_3D_", Output_Name_ES, sep = ""),
  Output_Name_EN = paste("EN_Map_3D_", Output_Name_EN, sep = "")
)

# ...::: Mapas de Puestos de trabajo, por trabajadores - LOC :::...

Shape_Plotear <- Shape_Equipamientos_Localidades |>
  select(COD_LOC, PUESTOS_TRABAJADORES)
Criterio <- Shape_Plotear$PUESTOS_TRABAJADORES
Titulo_ES <- "Lugares de trabajo por cada 1000 trabajadores"
Titulo_EN <- "Workplaces per 1,000 workers"
Output_Path <- "./Data/3_Results/Maps/3_Amenities/4_LOC/Detailed_Maps"
Output_Name_ES <- paste("LOC_", Titulo_ES, sep = "")
Output_Name_EN <- paste("LOC_", Titulo_EN, sep = "")
Paleta_Color <- scale_fill_viridis_c(option = "H")

# Mostramos avance:
print(paste("Creando mapas de: ", Titulo_ES, " - LOC...", sep = ""))

# 2D Español/Inglés:
Plot_Color(
  Shape_Plotear = Shape_Plotear,
  Criterio_Color = Criterio,
  Paleta_Color = Paleta_Color,
  Resolution = Resolucion_2D,
  Line_Width = Ancho_Linea,
  Output_Path = Output_Path,
  Output_Name_ES = paste("ES_Mapa_2D_", Output_Name_ES, sep = ""),
  Output_Name_EN = paste("EN_Map_2D_", Output_Name_EN, sep = "")
)
# 3D Español/Inglés:
PlotMapa3D(
  Shape_Plotear = Shape_Plotear,
  Criterio_Extrusion = Criterio,
  Paleta_Color = Paleta_Color,
  Resolution = Resolucion_3D,
  Scale = Escala,
  Output_Path = Output_Path,
  Output_Name_ES = paste("ES_Mapa_3D_", Output_Name_ES, sep = ""),
  Output_Name_EN = paste("EN_Map_3D_", Output_Name_EN, sep = "")
)

# ...::: Mapas de Universidades por universitarios - LOC :::...

Shape_Plotear <- Shape_Equipamientos_Localidades |>
  select(COD_LOC, UNIVERSIDADES_ESTUDIANTES)
Criterio <- Shape_Plotear$UNIVERSIDADES_ESTUDIANTES
Titulo_ES <- "Universidades por cada 1000 universitarios"
Titulo_EN <- "Universities per 1000 university students"
Output_Path <- "./Data/3_Results/Maps/3_Amenities/4_LOC/Detailed_Maps"
Output_Name_ES <- paste("LOC_", Titulo_ES, sep = "")
Output_Name_EN <- paste("LOC_", Titulo_EN, sep = "")
Paleta_Color <- scale_fill_viridis_c(option = "H")

# Mostramos avance:
print(paste("Creando mapas de: ", Titulo_ES, " - LOC...", sep = ""))

# 2D Español/Inglés:
Plot_Color(
  Shape_Plotear = Shape_Plotear,
  Criterio_Color = Criterio,
  Paleta_Color = Paleta_Color,
  Resolution = Resolucion_2D,
  Line_Width = Ancho_Linea,
  Output_Path = Output_Path,
  Output_Name_ES = paste("ES_Mapa_2D_", Output_Name_ES, sep = ""),
  Output_Name_EN = paste("EN_Map_2D_", Output_Name_EN, sep = "")
)
# 3D Español/Inglés:
PlotMapa3D(
  Shape_Plotear = Shape_Plotear,
  Criterio_Extrusion = Criterio,
  Paleta_Color = Paleta_Color,
  Resolution = Resolucion_3D,
  Scale = Escala,
  Output_Path = Output_Path,
  Output_Name_ES = paste("ES_Mapa_3D_", Output_Name_ES, sep = ""),
  Output_Name_EN = paste("EN_Map_3D_", Output_Name_EN, sep = "")
)

# --- Fin de la creación de mapas de Puntos, y de mapas 2D y 3D para HEX, UPL y LOC ---

# Tiempo de creación de mapas de Puntos, y de mapas 2D y 3D para HEX, UPL y LOC:
End_Time_S4_1_P12 <- Sys.time()
print(paste("12. Tiempo de duración de creación de mapas 2D y 3D: ", as.duration(End_Time_S4_1_P12 - Start_Time_S4_1_P12)))

# ####
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ::: 13. Extra - ¿Necesita recargar los resultados de este script?          :::
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ####
#
Start_Time_S4_1_P13 <- Sys.time()

# Recargamos el sf con los puntos de todos los Amenities:
Amenities_sf <- st_read("./Data/2_Processing/3_Amenities/Amenities.gpkg")

# Recargamos todos los shapes de áreas con las sumas de todos los equipamientos
Shape_Equipamientos_Hexagonos <- st_read("./Data/2_Processing/3_Amenities/Shape_Amenities_HEX.gpkg")
Shape_Equipamientos_UPLs <- st_read("./Data/2_Processing/3_Amenities/Shape_Amenities_UPL.gpkg")
Shape_Equipamientos_Localidades <- st_read("./Data/2_Processing/3_Amenities/Shape_Amenities_LOC.gpkg")
load("./Data/2_Processing/3_Amenities/Names_Equipamientos.RData")
names(Shape_Equipamientos_Hexagonos) <- Names_Equipamientos_HEX
names(Shape_Equipamientos_UPLs) <- Names_Equipamientos_UPL
names(Shape_Equipamientos_Localidades) <- Names_Equipamientos_LOC

# Mostramos los tiempos de procesamiento:
End_Time_S4_1_P13 <- Sys.time()
print(paste("13. Tiempo de recarga de los resultados del script: ", as.duration(End_Time_S4_1_P13 - Start_Time_S4_1_P13)))

# ####
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ::: 14. Tiempos de procesamiento                                           :::
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ####
#
# Mostramos los tiempos de procesamiento:
#
End_Time_S4_1_P0 <- Sys.time()
print(paste("1. Tiempo de inicialización: ", as.duration(End_Time_S4_1_P1 - Start_Time_S4_1_P1)))
print(paste("2. Tiempo de carga de datos de origen: ", as.duration(End_Time_S4_1_P2 - Start_Time_S4_1_P2)))
print(paste("3. Tiempo de consulta y corrección housing de equipamientos: ", as.duration(End_Time_S4_1_P3 - Start_Time_S4_1_P3)))
print(paste("4. Tiempo de procesamiento de inventario por áreas: ", as.duration(End_Time_S4_1_P4 - Start_Time_S4_1_P4)))
print(paste("5. Tiempo de procesamiento de las categorías por áreas: ", as.duration(End_Time_S4_1_P5 - Start_Time_S4_1_P5)))
print(paste("6. Tiempo de creación de mapa de capa de ULP con etiquetas: ", as.duration(End_Time_S4_1_P6 - Start_Time_S4_1_P6)))
print(paste("7. Tiempo de creación de Mapa de puntos de equipamientos (todos): ", as.duration(End_Time_S4_1_P7 - Start_Time_S4_1_P7)))
print(paste("8. Tiempo de creación de gráfico y mapa de equipamientos por HEX (todos): ", as.duration(End_Time_S4_1_P8 - Start_Time_S4_1_P8)))
print(paste("9. Tiempo de creación de gráfico y mapa de equipamientos por UPL (todos): ", as.duration(End_Time_S4_1_P9 - Start_Time_S4_1_P9)))
print(paste("10. Tiempo de creación de gráfico y mapa de equipamientos por UPL (por 100k habitantes): ", as.duration(End_Time_S4_1_P10 - Start_Time_S4_1_P10)))
print(paste("11. Tiempo de creación de gráfico y mapa de Universidades por UPL: ", as.duration(End_Time_S4_1_P11 - Start_Time_S4_1_P11)))
print(paste("12. Extra - Tiempo de duración de creación de mapas 2D y 3D: ", as.duration(End_Time_S4_1_P12 - Start_Time_S4_1_P12)))
print(paste("13. Extra - Tiempo de recarga de los resultados del script: ", as.duration(End_Time_S4_1_P13 - Start_Time_S4_1_P13)))
print(paste("Tiempo total de procesamiento del script: ", as.duration(End_Time_S4_1_P0 - Start_Time_S4_1_P0)))

