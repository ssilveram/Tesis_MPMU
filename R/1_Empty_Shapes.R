# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# :::                                                                        :::
# :::                       SCRIPT DE SHAPES VACIOS                          :::
# :::                                                                        :::
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#
# Nombre:      1_Empty_Shapes.R
#
# Descripcion: La función de este script es la de organizar los shapes originales,
#              en shapes de facil entendimiento, los cuales estarán relacionados con
#              los datos de identificación de cada una de las Localidades, UPL, Sectores,
#              Manzanas y Hexágonos, para posteriormente ubicar geo-espacialmente los datos
#              que se logren conseguir de la ciudad (población, servicios, viajes, etc.).

# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# :::                      Contenido de este Script                          :::
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#
# 1. Inicialización
# 1.1. Carga de librerías.
# 1.2. Creación de funciones propias.
# 2. Carga de datos de origen.
# 2.1. Carga de los Shapes de la Ciudad.
# 2.2. Carga de los CSV con datos del censo (DANE).
# 3. Alistamiento de Shapes Temporales.
# 4. Creación del Grid del Municipio.
# 5. Consolidación de Shapes con información de ubicación por tipo.
# 5.1. Consolidación del Shape Manzanas.
# 5.2. Consolidación del Shape Hexágonos.
# 5.3. Consolidación del Shape Secciones.
# 5.4. Consolidación del Shape Sectores.
# 5.5. Consolidación del Shape ZAT.
# 5.6. Consolidación del Shape UTAM.
# 5.7. Consolidación del Shape UPLs.
# 5.8. Consolidación del Shape Localidades.
# 6. Ploteo de resultados (Shapes vacíos).
# 7. Guardado de Shapes Vacíos.
# 8. Recarga de resultados del Script.
# 9. Tiempos de procesamiento.
#
Start_Time_S1_P0 <- Sys.time()

# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ::: 1. Inicialización                                                      :::
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#
Start_Time_S1_P1 <- Sys.time()

# --- 1.1. Cargamos las librerías necesarias para correr este script ---

# --- 1.2. Defino funciones propias ---

source("0_Initialization.R")

# Tiempo de procesamiento:
End_Time_S1_P1 <- Sys.time()
print(paste("1. Tiempo de inicialización: ", as.duration(End_Time_S1_P1 - Start_Time_S1_P1)))

# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ::: 2. Carga de datos de origen                                            :::
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# Cargamos los shapes y dataframes originales para comenzar a filtrar y poner
# todo en orden para futuros análisis
#
# ---- Cargamos los datos originales del origen de datos ---
#
Start_Time_S1_P2 <- Sys.time()

# --- 2.1. Carga de Shapes ---
# Las fuentes de los archivos shp y gpkg son: mapas.bogota.com

Shape_Municipio_Original <- st_read("./Data/1_Sources/2_Population/Shapes/MGN2021_URB_AREA_CENSAL/MGN_URB_AREA_CENSAL.shp") # Municipio
Shape_Localidad_Original <- st_read("./Data/1_Sources/1_Shapes/Custom/localidad.gpkg")                                # Localidad
Shape_UPL_Original <- st_read("./Data/1_Sources/1_Shapes/Custom/UPL.gpkg")                                           # UPLs
Shape_Sector_Original <- st_read("./Data/1_Sources/2_Population/Shapes/MGN2021_URB_SECTOR/MGN_URB_SECTOR.shp")              # Sectores
Shape_Seccion_Original <- st_read("./Data/1_Sources/2_Population/Shapes/MGN2021_URB_SECCION/MGN_URB_SECCION.shp")           # Secciones
Shape_Manzana_Original <- st_read("./Data/1_Sources/2_Population/Shapes/MGN2021_URB_MANZANA/MGN_URB_MANZANA.shp")           # Manzana
Shape_UTAM_Original <- st_read("./Data/1_Sources/3_EM2023/EODH/03_Zonificacion/UTAM2023/UTAM2023.shp")                  # Shape EM2023
Shape_ZAT_Original <- st_read("./Data/1_Sources/3_EM2023/EODH/03_Zonificacion/ZAT2023/ZAT2023.shp")                     # Shape EM2023
Coordinates_CRS <- st_crs(Shape_Municipio_Original)

# Tiempo de procesamiento:
End_Time_S1_P2 <- Sys.time()
print(paste("2. Tiempo de carga de datos de origen: ", as.duration(End_Time_S1_P2 - Start_Time_S1_P2)))

# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ::: 3. Filtrado y alistamiento de Shapes Temporales                        :::
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# El objetivo es dejar lo más simplificados posibles los shapes originales
# de los mapas de la ciudad.
#
Start_Time_S1_P3 <- Sys.time()

# Filtro Municipios y dejo solo NOM_CPOB y COD_CPOB
Shape_Municipio_Original
Shape_Municipios <- Shape_Municipio_Original %>%
  st_transform(Coordinates_CRS) %>%
  filter(NOM_CPOB == "BOGOTÁ, DISTRITO CAPITAL") %>%
  mutate(NOM_CPOB = str_to_title(NOM_CPOB, locale = "es")) %>%
  select(COD_CPOB, NOM_CPOB) %>%
  st_transform(Coordinates_CRS)
Shape_Municipios
#view(Shape_Municipios)
#MostrarMapa(Shape_Municipios)
#COD_CPOB_Array <- Shape_Municipios$COD_CPOB
#COD_CPOB_Array

# Filtro localidades y dejo solo NOM_LOC y COD_LOC
Shape_Localidad_Original
Shape_Localidades_Temp <- Shape_Localidad_Original %>%
  st_transform(Coordinates_CRS) %>%
  st_intersection(Shape_Municipios) %>%
  mutate(NOM_LOC = LocNombre) %>%
  mutate(COD_LOC = as.character(as.numeric(LocCodigo))) %>%
  mutate(NOM_LOC = str_to_title(NOM_LOC, locale = "es")) %>%
  mutate(ROW_LOC_NO = row_number()) %>%
  select(ROW_LOC_NO, COD_LOC, NOM_LOC)
Shape_Localidades_Temp
#view(Shape_Localidades_Temp)
#MostrarMapa(Shape_Localidades_Temp)

# Filtro UPLs y dejo solo NOM_UPL, COD_UPL y ZONA (Zona de la ciudad)
Shape_UPL_Original
Shape_UPLs_Temp <- Shape_UPL_Original %>%
  #filter(SECTOR != "Sector Rural") %>%
  st_transform(Coordinates_CRS) %>%
  st_intersection(Shape_Municipios) %>%
  separate(CODIGO_UPL, into = c("X", "COD_UPL"), sep = "L") %>%
  mutate(COD_UPL = as.character(as.numeric(COD_UPL))) %>%
  mutate(ZONA = str_replace(SECTOR, "Sector ", "")) %>%
  mutate(NOM_UPL = NOMBRE) %>%
  mutate(NOM_UPL = str_to_title(NOM_UPL, locale = "es")) %>%
  mutate(ROW_UPL_NO = row_number()) %>%
  select(ROW_UPL_NO, COD_UPL, NOM_UPL, ZONA)
Shape_UPLs_Temp
#view(Shape_UPLs_Temp)
#MostrarMapa(Shape_UPLs_Temp)

# Filtro UTAM y solo dejo COD_LOC, COD_UTAM y NOM_UTAM
Shape_UTAM_Original # 142 UTAMs Originales
Shape_UTAM_Temp <- Shape_UTAM_Original |>
  st_transform(Coordinates_CRS) |>
  # Dejo solamente la info básica del Shape de UTAMs:
  filter(MUNNombre == "BOGOTA" & LOCNombre != "SUMAPAZ") |> # 111 UTAMs
  mutate(
    NOM_LOC = str_to_title(LOCNombre, locale = "es"),
    COD_UTAM = UTAM,
    NOM_UTAM = str_to_title(UTAMNombre , locale = "es"),
    NOM_LOC = str_replace_all(NOM_LOC, "Narino", "Nariño")
  ) |>
  select(NOM_LOC, COD_UTAM, NOM_UTAM) |>
  # Fusiono con el Shape de Localidades:
  full_join(
    Shape_Localidades_Temp |>
      as_tibble() |>
      select(COD_LOC, NOM_LOC),
    by = "NOM_LOC"
  ) |>
  # Creamos el índice de la primera columna, y seleccionamos solo lo necesario;
  mutate(ROW_UTAM_NO = row_number()) |>
  select(ROW_UTAM_NO, COD_LOC, COD_UTAM, NOM_UTAM)
Shape_UTAM_Temp
#view(Shape_UTAM_Temp)
#MostrarMapa(Shape_UTAM_Temp)

# Filtro ZAT y solo dejo lo necesario
Shape_ZAT_Original # 1215 ZATs
Shape_ZAT_Temp <- Shape_ZAT_Original |>
  st_transform(Coordinates_CRS) |>
  # Dejo solamente lo necesario
  filter(NOMMun == "Bogotá") |> # 940 ZATs
  mutate(
    COD_ZAT = ZAT,
    COD_UTAM = UTAM
  )  |>
  filter(COD_UTAM %in% Shape_UTAM_Temp$COD_UTAM) |> # 892 ZATs
  select(COD_UTAM, COD_ZAT) |>
  # Fusiono con Shape de UTAMs (Que ya incluye Localidades):
  full_join(
    Shape_UTAM_Temp |>
      as_tibble() |>
      select(COD_LOC, COD_UTAM),
    by = "COD_UTAM"
  ) |>
  mutate(ROW_ZAT_NO = row_number()) |>
  select(ROW_ZAT_NO, COD_LOC, COD_UTAM, COD_ZAT)
Shape_ZAT_Temp  
#view(Shape_ZAT_Temp)
#MostrarMapa(Shape_ZAT_Temp)

# Filtro Sector y dejo solo COD_SECT
Shape_Sector_Original
Shape_Sectores_Temp <- Shape_Sector_Original %>%
  st_transform(Coordinates_CRS) %>%
  filter(COD_CPOB %in% Shape_Municipios$COD_CPOB) %>%
  mutate(ROW_SECT_NO = row_number()) %>%
  select(ROW_SECT_NO, COD_SECT)
Shape_Sectores_Temp
#view(Shape_Sectores)
#MostrarMapa(Shape_Sectores_Temp)
#COD_SECT_Array <- Shape_Sectores$COD_SECT
#COD_SECT_Array

# Filtro Secciones y dejo solo COD_SECT y COD_SECC
Shape_Seccion_Original
Shape_Secciones_Temp <- Shape_Seccion_Original %>%
  st_transform(Coordinates_CRS) %>%
  filter(COD_SECT %in% Shape_Sectores_Temp$COD_SECT) %>%
  mutate(ROW_SECC_NO = row_number()) %>%
  select(ROW_SECC_NO, COD_SECT, COD_SECC)
Shape_Secciones_Temp
#view(Shape_Secciones_Temp)
#MostrarMapa(Shape_Secciones_Temp)
#COD_SECC_Array <- Shape_Secciones$COD_SECC
#COD_SECC_Array

# Filtro Manzanas y dejo solo COD_SECC y COD_MNZ
Shape_Manzana_Original
Shape_Manzanas_Temp <- Shape_Manzana_Original %>%
  st_transform(Coordinates_CRS) %>%
  # Solo manzanas del municipio de interés:
  filter(COD_CPOB %in% Shape_Municipios$COD_CPOB) %>% #Bogotá: 43331 manzanas
  mutate(
    COD_MNZ = COD_DANE,
    ROW_MNZ_NO = row_number()
    ) |>
  select(ROW_MNZ_NO, COD_SECC, COD_MNZ)
Shape_Manzanas_Temp
#view(Shape_Manzanas_Temp)
#MostrarMapa(Shape_Manzanas_Temp)

# Tiempo de procesamiento:
End_Time_S1_P3 <- Sys.time()
print(paste("3. Tiempo de filtrado y alistamiento de shapes temporales: ", as.duration(End_Time_S1_P3 - Start_Time_S1_P3)))

# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ::: 4. Creación del Grid del Municipio                                     :::
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#
# Se crea el shape con de los hexágonos.
#
Start_Time_S1_P4 <- Sys.time()

#PlotMapa(Shape_Municipios)
BBox_Municipios <- st_bbox(Shape_Municipios)
BBox_Municipios

# --- Para calcular las distancias máximas y establecer tamaño del grid ---
Punto_BL <- st_point(c(BBox_Municipios[["xmin"]], BBox_Municipios[["ymin"]]), dim = "XYZ")
Punto_UL <- st_point(c(BBox_Municipios[["xmin"]], BBox_Municipios[["ymax"]]), dim = "XYZ")
Punto_UR <- st_point(c(BBox_Municipios[["xmax"]], BBox_Municipios[["ymax"]]), dim = "XYZ")
Punto_BR <- st_point(c(BBox_Municipios[["xmax"]], BBox_Municipios[["ymin"]]), dim = "XYZ")
Poligono_BB <- st_convex_hull(c(Punto_BL, Punto_UL, Punto_UR, Punto_BR)) # Creo un Polígono con los puntos

Puntos_BB <- Poligono_BB %>%
  st_coordinates() %>%
  as_tibble() %>%
  st_as_sf(coords = c("X","Y"), crs = Coordinates_CRS) %>%
  select(geometry)
Puntos_BB

Distancia_NS <- st_distance(Puntos_BB[[1]][1], Puntos_BB[[1]][2]) # Distancia Norte-Sur
Distancia_OO <- st_distance(Puntos_BB[[1]][2], Puntos_BB[[1]][3]) # Distancia Oriente-Occidente
Distancia_NS
Distancia_OO
Distancia_UB <- st_distance(Punto_BL, Punto_UL)
Distancia_RL <- st_distance(Punto_BL, Punto_BR)
Distancia_UB
Distancia_RL

Grid_Size <- 500 # Ancho en metros
Grid_Factor <- Distancia_RL * Grid_Size / as.numeric(Distancia_OO)

# Group in hexagons
Hexa_Grid <- st_make_grid(Shape_Municipios, square = FALSE, cellsize = Grid_Factor)
Hexa_Grid

Hexa_Municipios <- Hexa_Grid[Shape_Municipios] # Intersección con Shape_Municipios
Hexa_Municipios
#PlotMapa(Hexa_Municipios)

Shape_Hexagonos_Temp <- Hexa_Municipios %>%
  st_as_sf() %>%
  mutate(ROW_HEX_NO = row_number()) %>%
  mutate(COD_HEX = row_number()) %>%
  select(ROW_HEX_NO, COD_HEX)
st_geometry(Shape_Hexagonos_Temp) <- "geometry"
Shape_Hexagonos_Temp  # Total Hexágonos: 2185 (500m).
#MostrarMapa(Shape_Hexagonos_Temp)

# Tiempo de procesamiento:
End_Time_S1_P4 <- Sys.time()
print(paste("4. Tiempo de creación del grid del municipio: ", as.duration(End_Time_S1_P4 - Start_Time_S1_P4)))

# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ::: 5. Consolidación de Shapes con información de ubicación por tipo       :::
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#
# Aquí comenzamos a detectar colisiones de los Shapes sueltos y a consolidar la información de
# ubicación de cada uno de los tipos de geometrías. La estructura será la siguiente:
#
# 5.1          5.2        5.3         5.4         5.5         5.6        5.7      5.8
# Manzanas     Hexágonos  Secciones   Sectores    ZAT         UTAM       UPL      LOC
#   ├- LOC      ├- LOC      ├- LOC      ├- LOC      ├- LOC      ├- LOC    └- UPL   └- LOC
#   ├- UPL      ├- UPL      ├- UPL      ├- UPL      ├- UPL      ├- UPL
#   ├- UTAM     ├- UTAM     ├- UTAM     ├- UTAM     ├- UTAM     └- UTAM
#   ├- ZAT      ├- ZAT      ├- ZAT      ├- ZAT      └- ZAT
#   ├- SECT     ├- SECT     ├- SECT     └- SECT
#   ├- SECC     ├- SECC     └- SECC
#   ├- HEX      └- HEX
#   └- MNZ
#
Start_Time_S1_P5 <- Sys.time()

# ::: 5.1. Consolidación del Shape Manzanas :::
#
# Manzanas
#   ├- LOC
#   ├- UPL
#   ├- UTAM
#   ├- ZAT
#   ├- SECT
#   ├- SECC
#   ├- HEX
#   └- MNZ
#
# Creamos el Shape de Centroides para encontrar las intersecciones
Shape_Centroides_Manzanas_Temp <- st_centroid(Shape_Manzanas_Temp)
Shape_Centroides_Manzanas_Temp

# Detectamos Intersección de Manzanas y Localidades
Interseccion_MNZ_LOC <- as.data.frame(st_intersects(Shape_Centroides_Manzanas_Temp, Shape_Localidades_Temp)) %>%
  mutate(ROW_MNZ_NO = row.id) %>%
  mutate(ROW_LOC_NO = col.id) %>%
  group_by(ROW_MNZ_NO) %>%
  select(ROW_MNZ_NO, ROW_LOC_NO)
Interseccion_MNZ_LOC

# Detectamos Intersección de Manzanas y UPLs
Interseccion_MNZ_UPL <- as.data.frame(st_intersects(Shape_Centroides_Manzanas_Temp, Shape_UPLs_Temp)) %>%
  mutate(ROW_MNZ_NO = row.id) %>%
  mutate(ROW_UPL_NO = col.id) %>%
  group_by(ROW_MNZ_NO) %>%
  select(ROW_MNZ_NO, ROW_UPL_NO)
Interseccion_MNZ_UPL

# Detectamos Intersección de Manzanas y UTAM
Interseccion_MNZ_UTAM <- as.data.frame(st_intersects(Shape_Centroides_Manzanas_Temp, Shape_UTAM_Temp)) %>%
  mutate(ROW_MNZ_NO = row.id) %>%
  mutate(ROW_UTAM_NO = col.id) %>%
  group_by(ROW_MNZ_NO) %>%
  select(ROW_MNZ_NO, ROW_UTAM_NO)
Interseccion_MNZ_UTAM

# Detectamos Intersección de Manzanas y ZAT
Interseccion_MNZ_ZAT <- as.data.frame(st_intersects(Shape_Centroides_Manzanas_Temp, Shape_ZAT_Temp)) %>%
  mutate(ROW_MNZ_NO = row.id) %>%
  mutate(ROW_ZAT_NO = col.id) %>%
  group_by(ROW_MNZ_NO) %>%
  select(ROW_MNZ_NO, ROW_ZAT_NO)
Interseccion_MNZ_ZAT

# ¿Por qué no se consolidan con Sectores y Secciones? R:/ Porque el Shape de MNZ ya tiene la info de SECC, y el
# Shape de SECC, ya contiene la info de SECT. Solo debe hacerse un left_join y ya se tiene la info ce SECT en MNZ.

# Detectamos Intersección de Manzanas y HEX
Interseccion_MNZ_HEX <- as.data.frame(st_intersects(Shape_Centroides_Manzanas_Temp, Shape_Hexagonos_Temp)) %>%
  mutate(ROW_MNZ_NO = row.id) %>%
  mutate(ROW_HEX_NO = col.id) %>%
  group_by(ROW_MNZ_NO) %>%
  select(ROW_MNZ_NO, ROW_HEX_NO)
Interseccion_MNZ_HEX

# Consolidamos Shape Manzanas con info de Localidades, UPLs, UTAM, ZAT, Sectores, Secciones y Hexágonos
Shape_Manzanas <- Shape_Manzanas_Temp %>%
  # Localidades
  left_join(Interseccion_MNZ_LOC, by = "ROW_MNZ_NO") %>%
  left_join(as_tibble(Shape_Localidades_Temp) %>%
               select(ROW_LOC_NO, COD_LOC, NOM_LOC),
             by = "ROW_LOC_NO"
  ) %>%
  # UPLs
  left_join(Interseccion_MNZ_UPL, by = "ROW_MNZ_NO") %>%
  left_join(as_tibble(Shape_UPLs_Temp) %>%
               select(ROW_UPL_NO, COD_UPL, NOM_UPL),
             by = "ROW_UPL_NO"
  ) %>%
  # UTAM
  left_join(Interseccion_MNZ_UTAM, by = "ROW_MNZ_NO") %>%
  left_join(as_tibble(Shape_UTAM_Temp) %>%
              select(ROW_UTAM_NO, COD_UTAM, NOM_UTAM),
            by = "ROW_UTAM_NO"
  ) %>%
  # ZAT
  left_join(Interseccion_MNZ_ZAT, by = "ROW_MNZ_NO") %>%
  left_join(as_tibble(Shape_ZAT_Temp) %>%
              select(ROW_ZAT_NO, COD_ZAT),
            by = "ROW_ZAT_NO"
  ) %>%
  # Sectores (Incluye Sectores)
  left_join(as_tibble(Shape_Secciones_Temp) %>%
               select(COD_SECT, COD_SECC),
             by = "COD_SECC"
  ) %>%
  # HEX
  left_join(Interseccion_MNZ_HEX, by = "ROW_MNZ_NO") %>%
  left_join(as_tibble(Shape_Hexagonos_Temp) %>%
              select(ROW_HEX_NO, COD_HEX),
            by = "ROW_HEX_NO"
  ) %>%
  mutate(
    COD_LOC = as.character(COD_LOC),
    COD_UPL = as.character(COD_UPL),
    COD_UTAM = as.character(COD_UTAM),
    COD_ZAT = as.character(COD_ZAT),
    COD_SECT = as.character(COD_SECT),
    COD_SECC = as.character(COD_SECC),
    COD_HEX = as.character(COD_HEX),
    COD_MNZ = as.character(COD_MNZ),
  ) %>%
  select(COD_LOC, NOM_LOC, COD_UPL, NOM_UPL, COD_UTAM, NOM_UTAM, COD_ZAT, COD_SECT, COD_SECC, COD_HEX, COD_MNZ)
Shape_Manzanas
#length(Shape_Manzanas$COD_MNZ)
#view(Shape_Manzanas)
#MostrarMapa(Shape_Manzanas)

# ::: 5.2. Consolidación del Shape Hexagonos :::
#
# Hexágonos
#   ├- LOC
#   ├- UPL
#   ├- UTAM
#   ├- ZAT
#   ├- SECT
#   ├- SECC
#   └- HEX
#
# Creamos el Shape de Centroides para encontrar las intersecciones
Shape_Centroides_Hexagonos_Temp <- st_centroid(Shape_Hexagonos_Temp)
Shape_Centroides_Hexagonos_Temp # 2185

# Detectamos Intersección de Hexágonos y Localidades
Interseccion_HEX_LOC <- as.data.frame(st_intersects(Shape_Centroides_Hexagonos_Temp, Shape_Localidades_Temp)) %>%
  mutate(ROW_HEX_NO = row.id) %>%
  mutate(ROW_LOC_NO = col.id) %>%
  group_by(ROW_HEX_NO) %>%
  select(ROW_HEX_NO, ROW_LOC_NO)
Interseccion_HEX_LOC  # 1904

# Detectamos Intersección de Hexágonos y UPLs
Interseccion_HEX_UPL <- as.data.frame(st_intersects(Shape_Centroides_Hexagonos_Temp, Shape_UPLs_Temp)) %>%
  mutate(ROW_HEX_NO = row.id) %>%
  mutate(ROW_UPL_NO = col.id) %>%
  group_by(ROW_HEX_NO) %>%
  select(ROW_HEX_NO, ROW_UPL_NO)
Interseccion_HEX_UPL # 1894

# Detectamos Intersección de Hexágonos y UTAM
Interseccion_HEX_UTAM <- as.data.frame(st_intersects(Shape_Centroides_Hexagonos_Temp, Shape_UTAM_Temp)) %>%
  mutate(ROW_HEX_NO = row.id) %>%
  mutate(ROW_UTAM_NO = col.id) %>%
  group_by(ROW_HEX_NO) %>%
  select(ROW_HEX_NO, ROW_UTAM_NO)
Interseccion_HEX_UTAM # 1865

# Detectamos Intersección de Hexágonos y ZAT
Interseccion_HEX_ZAT <- as.data.frame(st_intersects(Shape_Centroides_Hexagonos_Temp, Shape_ZAT_Temp)) %>%
  mutate(ROW_HEX_NO = row.id) %>%
  mutate(ROW_ZAT_NO = col.id) %>%
  group_by(ROW_HEX_NO) %>%
  select(ROW_HEX_NO, ROW_ZAT_NO)
Interseccion_HEX_ZAT # 1879

# Detectamos Intersección de Hexágonos y Sectores
Interseccion_HEX_SECT <- as.data.frame(st_intersects(Shape_Centroides_Hexagonos_Temp, Shape_Sectores_Temp)) %>%
  mutate(ROW_HEX_NO = row.id) %>%
  mutate(ROW_SECT_NO = col.id) %>%
  group_by(ROW_HEX_NO) %>%
  select(ROW_HEX_NO, ROW_SECT_NO)
Interseccion_HEX_SECT # 1922

# Detectamos Intersección de Hexágonos y Secciones
Interseccion_HEX_SECC <- as.data.frame(st_intersects(Shape_Centroides_Hexagonos_Temp, Shape_Secciones_Temp)) %>%
  mutate(ROW_HEX_NO = row.id) %>%
  mutate(ROW_SECC_NO = col.id) %>%
  group_by(ROW_HEX_NO) %>%
  select(ROW_HEX_NO, ROW_SECC_NO)
Interseccion_HEX_SECC # 1922

# Consolidamos Shape Hexágonos con info de Localidades, UPLs, UTAM, ZAT, Sectores y Secciones
Shape_Hexagonos_Temp
Shape_Hexagonos <- Shape_Hexagonos_Temp %>%
  # Localidades
  left_join(Interseccion_HEX_LOC, by = "ROW_HEX_NO") %>%
  left_join(as_tibble(Shape_Localidades_Temp) %>%
              select(ROW_LOC_NO, COD_LOC, NOM_LOC),
            by = "ROW_LOC_NO"
  ) %>%
  # UPLs
  left_join(Interseccion_HEX_UPL, by = "ROW_HEX_NO") %>%
  left_join(as_tibble(Shape_UPLs_Temp) %>%
              select(ROW_UPL_NO, COD_UPL, NOM_UPL),
            by = "ROW_UPL_NO"
  ) %>%
  # UTAM
  left_join(Interseccion_HEX_UTAM, by = "ROW_HEX_NO") %>%
  left_join(as_tibble(Shape_UTAM_Temp) %>%
              select(ROW_UTAM_NO, COD_UTAM, NOM_UTAM),
            by = "ROW_UTAM_NO"
  ) %>%
  # ZAT
  left_join(Interseccion_HEX_ZAT, by = "ROW_HEX_NO") %>%
  left_join(as_tibble(Shape_ZAT_Temp) %>%
              select(ROW_ZAT_NO, COD_ZAT),
            by = "ROW_ZAT_NO"
  ) %>%
  # SECT
  left_join(Interseccion_HEX_SECT, by = "ROW_HEX_NO") %>%
  left_join(as_tibble(Shape_Sectores_Temp) %>%
              select(ROW_SECT_NO, COD_SECT),
            by = "ROW_SECT_NO"
  ) %>%
  # SECC
  left_join(Interseccion_HEX_SECC, by = "ROW_HEX_NO") %>%
  left_join(as_tibble(Shape_Secciones_Temp) %>%
              select(ROW_SECC_NO, COD_SECC),
            by = "ROW_SECC_NO"
  ) %>%
  mutate(
    COD_LOC = as.character(COD_LOC),
    COD_UPL = as.character(COD_UPL),
    COD_UTAM = as.character(COD_UTAM),
    COD_ZAT = as.character(COD_ZAT),
    COD_SECT = as.character(COD_SECT),
    COD_SECC = as.character(COD_SECC),
    COD_HEX = as.character(COD_HEX),
  ) %>%
  select(COD_LOC, NOM_LOC, COD_UPL, NOM_UPL, COD_UTAM, NOM_UTAM, COD_ZAT, COD_SECT, COD_SECC, COD_HEX)
Shape_Hexagonos
#length(Shape_Hexagonos$COD_HEX)
#view(Shape_Hexagonos)
#MostrarMapa(Shape_Hexagonos)

# ::: 5.3. Consolidación del Shape Secciones :::
#
# Secciones
#   ├- LOC
#   ├- UPL
#   ├- UTAM
#   ├- ZAT
#   ├- SECT
#   └- SECC
#
# Creamos el Shape de Centroides para encontrar las intersecciones
Shape_Centroides_Secciones_Temp <- st_centroid(Shape_Secciones_Temp)
Shape_Centroides_Secciones_Temp # 2796

# Detectamos Intersección de Secciones y Localidades
Interseccion_SECC_LOC <- as.data.frame(st_intersects(Shape_Centroides_Secciones_Temp, Shape_Localidades_Temp)) %>%
  mutate(ROW_SECC_NO = row.id) %>%
  mutate(ROW_LOC_NO = col.id) %>%
  group_by(ROW_SECC_NO) %>%
  select(ROW_SECC_NO, ROW_LOC_NO)
Interseccion_SECC_LOC  # 2792

# Detectamos Intersección de Secciones y UPLs
Interseccion_SECC_UPL <- as.data.frame(st_intersects(Shape_Centroides_Secciones_Temp, Shape_UPLs_Temp)) %>%
  mutate(ROW_SECC_NO = row.id) %>%
  mutate(ROW_UPL_NO = col.id) %>%
  group_by(ROW_SECC_NO) %>%
  select(ROW_SECC_NO, ROW_UPL_NO)
Interseccion_SECC_UPL # 2792

# Detectamos Intersección de Secciones y UTAM
Interseccion_SECC_UTAM <- as.data.frame(st_intersects(Shape_Centroides_Secciones_Temp, Shape_UTAM_Temp)) %>%
  mutate(ROW_SECC_NO = row.id) %>%
  mutate(ROW_UTAM_NO = col.id) %>%
  group_by(ROW_SECC_NO) %>%
  select(ROW_SECC_NO, ROW_UTAM_NO)
Interseccion_SECC_UTAM # 2865

# Detectamos Intersección de Secciones y ZAT
Interseccion_SECC_ZAT <- as.data.frame(st_intersects(Shape_Centroides_Secciones_Temp, Shape_ZAT_Temp)) %>%
  mutate(ROW_SECC_NO = row.id) %>%
  mutate(ROW_ZAT_NO = col.id) %>%
  group_by(ROW_SECC_NO) %>%
  select(ROW_SECC_NO, ROW_ZAT_NO)
Interseccion_SECC_ZAT # 2754

# ¿Por qué no hay detección con Sectores?
# R:/ Porque el Shape_Secciones ya tiene la información de los sectores.

# Consolidamos Shape Secciones con info de Localidades, UPLs, UTAM, ZAT
Shape_Secciones_Temp
Shape_Secciones <- Shape_Secciones_Temp %>%
  # Localidades
  left_join(Interseccion_SECC_LOC, by = "ROW_SECC_NO") %>%
  left_join(as_tibble(Shape_Localidades_Temp) %>%
              select(ROW_LOC_NO, COD_LOC, NOM_LOC),
            by = "ROW_LOC_NO"
  ) %>%
  # UPLs
  left_join(Interseccion_SECC_UPL, by = "ROW_SECC_NO") %>%
  left_join(as_tibble(Shape_UPLs_Temp) %>%
              select(ROW_UPL_NO, COD_UPL, NOM_UPL),
            by = "ROW_UPL_NO"
  ) %>%
  # UTAM
  left_join(Interseccion_SECC_UTAM, by = "ROW_SECC_NO") %>%
  left_join(as_tibble(Shape_UTAM_Temp) %>%
              select(ROW_UTAM_NO, COD_UTAM, NOM_UTAM),
            by = "ROW_UTAM_NO"
  ) %>%
  # ZAT
  left_join(Interseccion_SECC_ZAT, by = "ROW_SECC_NO") %>%
  left_join(as_tibble(Shape_ZAT_Temp) %>%
              select(ROW_ZAT_NO, COD_ZAT),
            by = "ROW_ZAT_NO"
  ) %>%
  mutate(
    COD_LOC = as.character(COD_LOC),
    COD_UPL = as.character(COD_UPL),
    COD_UTAM = as.character(COD_UTAM),
    COD_ZAT = as.character(COD_ZAT),
    COD_SECT = as.character(COD_SECT),
    COD_SECC = as.character(COD_SECC),
  ) %>%
  select(COD_LOC, NOM_LOC, COD_UPL, NOM_UPL, COD_UTAM, NOM_UTAM, COD_ZAT, COD_SECT, COD_SECC)
Shape_Secciones # 2796
#length(Shape_Secciones$COD_SECC)
#view(Shape_Secciones)
#MostrarMapa(Shape_Secciones)

# ::: 5.4. Consolidación del Shape Sectores :::
#
# Secciones
#   ├- LOC
#   ├- UPL
#   ├- UTAM
#   ├- ZAT
#   └- SECT
#
# Creamos el Shape de Centroides para encontrar las intersecciones
Shape_Centroides_Sectores_Temp <- st_centroid(Shape_Sectores_Temp)
Shape_Centroides_Sectores_Temp # 631

# Detectamos Intersección de Sectores y Localidades
Interseccion_SECT_LOC <- as.data.frame(st_intersects(Shape_Centroides_Sectores_Temp, Shape_Localidades_Temp)) %>%
  mutate(ROW_SECT_NO = row.id) %>%
  mutate(ROW_LOC_NO = col.id) %>%
  group_by(ROW_SECT_NO) %>%
  select(ROW_SECT_NO, ROW_LOC_NO)
Interseccion_SECT_LOC  # 627

# Detectamos Intersección de Sectores y UPLs
Interseccion_SECT_UPL <- as.data.frame(st_intersects(Shape_Centroides_Sectores_Temp, Shape_UPLs_Temp)) %>%
  mutate(ROW_SECT_NO = row.id) %>%
  mutate(ROW_UPL_NO = col.id) %>%
  group_by(ROW_SECT_NO) %>%
  select(ROW_SECT_NO, ROW_UPL_NO)
Interseccion_SECT_UPL # 627

# Detectamos Intersección de Sectores y UTAM
Interseccion_SECT_UTAM <- as.data.frame(st_intersects(Shape_Centroides_Sectores_Temp, Shape_UTAM_Temp)) %>%
  mutate(ROW_SECT_NO = row.id) %>%
  mutate(ROW_UTAM_NO = col.id) %>%
  group_by(ROW_SECT_NO) %>%
  select(ROW_SECT_NO, ROW_UTAM_NO)
Interseccion_SECT_UTAM # 617

# Detectamos Intersección de Sectores y ZAT
Interseccion_SECT_ZAT <- as.data.frame(st_intersects(Shape_Centroides_Sectores_Temp, Shape_ZAT_Temp)) %>%
  mutate(ROW_SECT_NO = row.id) %>%
  mutate(ROW_ZAT_NO = col.id) %>%
  group_by(ROW_SECT_NO) %>%
  select(ROW_SECT_NO, ROW_ZAT_NO)
Interseccion_SECT_ZAT # 617

# Consolidamos Shape Sectores con info de Localidades, UPLs, UTAM, ZAT
Shape_Sectores_Temp
Shape_Sectores <- Shape_Sectores_Temp %>%
  # Localidades
  left_join(Interseccion_SECT_LOC, by = "ROW_SECT_NO") %>%
  left_join(as_tibble(Shape_Localidades_Temp) %>%
              select(ROW_LOC_NO, COD_LOC, NOM_LOC),
            by = "ROW_LOC_NO"
  ) %>%
  # UPLs
  left_join(Interseccion_SECT_UPL, by = "ROW_SECT_NO") %>%
  left_join(as_tibble(Shape_UPLs_Temp) %>%
              select(ROW_UPL_NO, COD_UPL, NOM_UPL),
            by = "ROW_UPL_NO"
  ) %>%
  # UTAM
  left_join(Interseccion_SECT_UTAM, by = "ROW_SECT_NO") %>%
  left_join(as_tibble(Shape_UTAM_Temp) %>%
              select(ROW_UTAM_NO, COD_UTAM, NOM_UTAM),
            by = "ROW_UTAM_NO"
  ) %>%
  # ZAT
  left_join(Interseccion_SECT_ZAT, by = "ROW_SECT_NO") %>%
  left_join(as_tibble(Shape_ZAT_Temp) %>%
              select(ROW_ZAT_NO, COD_ZAT),
            by = "ROW_ZAT_NO"
  ) %>%
  mutate(
    COD_LOC = as.character(COD_LOC),
    COD_UPL = as.character(COD_UPL),
    COD_UTAM = as.character(COD_UTAM),
    COD_ZAT = as.character(COD_ZAT),
    COD_SECT = as.character(COD_SECT),
  ) %>%
  select(COD_LOC, NOM_LOC, COD_UPL, NOM_UPL, COD_UTAM, NOM_UTAM, COD_ZAT, COD_SECT)
Shape_Sectores # 631
#length(Shape_Sectores$COD_SECT)
#view(Shape_Sectores)
#MostrarMapa(Shape_Sectores)

# ::: 5.5. Consolidación del Shape ZAT :::
#
# ZAT
#  ├- LOC
#  ├- UPL
#  ├- UTAM
#  └- ZAT
#
# Creamos el Shape de Centroides para encontrar las intersecciones
Shape_Centroides_ZAT_Temp <- st_centroid(Shape_ZAT_Temp)
Shape_Centroides_ZAT_Temp # 892

# ¿Por qué no hay detección entre ZAT y LOC y UTAM?
# R:/ Porque el Shape de ZAT ya contiene la info de LOC y UTAM.

# Detectamos Intersección de ZAT y UPLs
Interseccion_ZAT_UPL <- as.data.frame(st_intersects(Shape_Centroides_ZAT_Temp, Shape_UPLs_Temp)) %>%
  mutate(ROW_ZAT_NO = row.id) %>%
  mutate(ROW_UPL_NO = col.id) %>%
  group_by(ROW_ZAT_NO) %>%
  select(ROW_ZAT_NO, ROW_UPL_NO)
Interseccion_ZAT_UPL # 871

# Consolidamos Shape ZAT con info de Localidades, UPLs y UTAM
Shape_ZAT_Temp
Shape_ZAT <- Shape_ZAT_Temp %>%
  # LOC
  left_join(
    Shape_Localidades_Temp |> as_tibble() |> select(COD_LOC, NOM_LOC)
  ) %>%
  # UPLs
  left_join(Interseccion_ZAT_UPL, by = "ROW_ZAT_NO") %>%
  left_join(as_tibble(Shape_UPLs_Temp) %>%
              select(ROW_UPL_NO, COD_UPL, NOM_UPL),
            by = "ROW_UPL_NO"
  ) %>%
  # UTAM
  left_join(
    Shape_UTAM_Temp |> as_tibble() |> select(COD_UTAM, NOM_UTAM)
  ) %>%
  mutate(
    COD_LOC = as.character(COD_LOC),
    COD_UPL = as.character(COD_UPL),
    COD_UTAM = as.character(COD_UTAM),
    COD_ZAT = as.character(COD_ZAT),
  ) %>%
  select(COD_LOC, NOM_LOC, COD_UPL, NOM_UPL, COD_UTAM, NOM_UTAM, COD_ZAT)
Shape_ZAT # 892
#length(Shape_ZAT$COD_ZAT)
#view(Shape_ZAT)
# MostrarMapa(Shape_ZAT)

# Paréntesis: Relación de ZAT, UTAM, UPL y LOC para exportar y usar en la tabla de la EM2023
# Temp_df <- Shape_ZAT |>
#   as_tibble() |>
#   select(-geometry)
# fwrite(Temp_df, "./Data/1_Sources/2_EM2023/EODH/05_Base datos procesada/XLSX/LOC_UPL_UTAM_ZAT.csv", row.names = FALSE)

# ::: 5.6. Consolidación del Shape UTAM :::
#
# UTAM
#  ├- LOC
#  ├- UPL
#  └- UTAM
#
# Creamos el Shape de Centroides para encontrar las intersecciones
Shape_Centroides_UTAM_Temp <- st_centroid(Shape_UTAM_Temp)
Shape_Centroides_UTAM_Temp # 111

# ¿Por qué no hay detección entre UTAM y LOC?
# R:/ Porque el Shape de UTAM ya contiene la info de LOC.

# Detectamos Intersección de ZAT y UPLs
Interseccion_UTAM_UPL <- as.data.frame(st_intersects(Shape_Centroides_UTAM_Temp, Shape_UPLs_Temp)) %>%
  mutate(ROW_UTAM_NO = row.id) %>%
  mutate(ROW_UPL_NO = col.id) %>%
  group_by(ROW_UTAM_NO) %>%
  select(ROW_UTAM_NO, ROW_UPL_NO)
Interseccion_UTAM_UPL # 108

# Consolidamos Shape UTAM con info de UPLs.
Shape_UTAM_Temp
Shape_UTAM <- Shape_UTAM_Temp %>%
  # LOC
  left_join(
    Shape_Localidades_Temp |> as_tibble() |> select(COD_LOC, NOM_LOC)
  ) %>%
  # UPLs
  left_join(Interseccion_UTAM_UPL, by = "ROW_UTAM_NO") %>%
  left_join(as_tibble(Shape_UPLs_Temp) %>%
              select(ROW_UPL_NO, COD_UPL, NOM_UPL),
            by = "ROW_UPL_NO"
  ) %>%
  mutate(
    COD_LOC = as.character(COD_LOC),
    COD_UPL = as.character(COD_UPL),
    COD_UTAM = as.character(COD_UTAM),
  ) %>%
  select(COD_LOC, NOM_LOC, COD_UPL, NOM_UPL, COD_UTAM, NOM_UTAM)
Shape_UTAM # 892
#length(Shape_UTAM$COD_UTAM)
#view(Shape_UTAM)
#MostrarMapa(Shape_UTAM)

# ::: 5.7. Consolidación del Shape UPL :::
#
# UPL
#  └- UPL
#
# Debido a que UPL no es dependiente de LOC, no cruzamos la info de los Shapes, ya que hay UPLs que pertenecen
# a ás de una LOC.

Shape_UPLs_Temp # 32
Shape_UPLs <- Shape_UPLs_Temp %>%
  mutate(ZONA_UPL = ZONA) %>%
  mutate(
    COD_UPL = as.character(COD_UPL),
  ) %>%
  select(COD_UPL, NOM_UPL, ZONA_UPL)
Shape_UPLs # 32
#length(Shape_UPLs$COD_UPL)
#view(Shape_UPLs)
#MostrarMapa(Shape_UPLs)

# ::: 5.8. Consolidación del Shape LOC :::
#
# LOC
#  └- LOC
#
# LOC se encuentra en el nivel más alto (después de MCPIO). Por eso no se relaciona con ningún superior.

Shape_Localidades_Temp
Shape_Localidades <- Shape_Localidades_Temp %>%
  mutate(
    COD_LOC = as.character(COD_LOC),
  ) %>%
  select(COD_LOC, NOM_LOC)
Shape_Localidades
#length(Shape_Localidades$COD_LOC)
#view(Shape_Localidades)
#MostrarMapa(Shape_Localidades)

# Tiempo de procesamiento:
End_Time_S1_P5 <- Sys.time()
print(paste("5. Tiempo consolidación de shapes con la información de ubicación: ", as.duration(End_Time_S1_P5 - Start_Time_S1_P5)))

# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ::: 6. Ploteo de resultados (Shapes vacíos)                                :::
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#
# Revisión de los resultados: Shapes finales sin información adicional (Vacios)
#
Start_Time_S1_P6 <- Sys.time()

Shape_Municipios
#MostrarMapa(Shape_Municipios)

Shape_Localidades
#MostrarMapa(Shape_Localidades)

Shape_UPLs
#MostrarMapa(Shape_UPLs)

Shape_Sectores
#MostrarMapa(Shape_Sectores)

Shape_Secciones
#MostrarMapa(Shape_Secciones)

Shape_Hexagonos
#MostrarMapa(Shape_Hexagonos)

Shape_Manzanas
#MostrarMapa(Shape_Manzanas)

P1 <- PlotMapa(Shape_Municipios)
P2 <- PlotMapa(Shape_Localidades)
P3 <- PlotMapa(Shape_UPLs)
P4 <- PlotMapa(Shape_Sectores)
P5 <- PlotMapa(Shape_Secciones)
P6 <- PlotMapa(Shape_Hexagonos)
P7 <- PlotMapa(Shape_UTAM)
P8 <- PlotMapa(Shape_ZAT)
P9 <- PlotMapa(Shape_Manzanas)
# En orden de Mayor a menor, disposición vertical:
# P1+P2+P3+P4+P5+P6+P7+P8+P9
# En orden de Mayor a menor, disposición horizontal:
# P5+P7+P1+P6+P4+P2+P9+P8+P3

# ::: Shape Municipio como imagen :::
Shape_Municipios
Plot_Vacio_Municipios <- ggplot(Shape_Municipios) +
  geom_sf(
    col = "white",
    linewidth = 0.8,  # Original: 0.8
    aes(fill = COD_CPOB)
  ) + 
  theme_void() +
  theme(
    legend.position = "none",
  )
#Plot_Vacio_Municipios

# Guardamos el plot como imagen:
ggsave(
  filename = "Plot_Vacio_Municipios.png",
  plot = Plot_Vacio_Municipios,
  device = NULL,
  path = "./Data/3_Results/1_Empty_Shapes/",
  scale = 1,
  width = Alto, # Intercambiadas porque está en vertical
  height = Ancho,
  units = c("px"),
  dpi = 300,
  limitsize = TRUE,
  bg = NULL,
  create.dir = FALSE,
)

# ::: Shape Localidades como imagen :::
Plot_Vacio_Localidades <- ggplot(Shape_Localidades |> filter(COD_LOC != "<NA>")) +
  geom_sf(
    col = "white",
    linewidth = 0.8,  # Original: 0.8
    aes(fill = NOM_LOC)
  ) + 
  theme_void() +
  theme(
    legend.position = "none",
  )
#Plot_Vacio_Localidades

# Guardamos el plot como imagen:
ggsave(
  filename = "Plot_Vacio_Localidades.png",
  plot = Plot_Vacio_Localidades,
  device = NULL,
  path = "./Data/3_Results/1_Empty_Shapes/",
  scale = 1,
  width = Alto, # Intercambiadas porque está en vertical
  height = Ancho,
  units = c("px"),
  dpi = 300,
  limitsize = TRUE,
  bg = NULL,
  create.dir = FALSE,
)

# ::: Shape UPL como imagen :::
Shape_UPLs
Plot_Vacio_UPLs <- ggplot(Shape_UPLs |> filter(COD_UPL != "<NA>")) +
  geom_sf(
    col = "white",
    linewidth = 0.8,  # Original: 0.8
    aes(fill = NOM_UPL)
  ) + 
  theme_void() +
  theme(
    legend.position = "none",
  )
#Plot_Vacio_UPLs

# Guardamos el plot como imagen:
ggsave(
  filename = "Plot_Vacio_UPLs.png",
  plot = Plot_Vacio_UPLs,
  device = NULL,
  path = "./Data/3_Results/1_Empty_Shapes/",
  scale = 1,
  width = Alto, # Intercambiadas porque está en vertical
  height = Ancho,
  units = c("px"),
  dpi = 300,
  limitsize = TRUE,
  bg = NULL,
  create.dir = FALSE,
)

# ::: Shape UTAM como imagen :::
Shape_UTAM
Plot_Vacio_UTAM <- ggplot(Shape_UTAM |> filter(COD_UPL != "<NA>")) +
  geom_sf(
    col = "white",
    linewidth = 0.6, # Original: 0.6
    aes(fill = COD_UPL)
  ) + 
  theme_void() +
  theme(
    legend.position = "none",
  )
#Plot_Vacio_UTAM

# Guardamos el plot como imagen:
ggsave(
  filename = "Plot_Vacio_UTAM.png",
  plot = Plot_Vacio_UTAM,
  device = NULL,
  path = "./Data/3_Results/1_Empty_Shapes/",
  scale = 1,
  width = Alto, # Intercambiadas porque está en vertical
  height = Ancho,
  units = c("px"),
  dpi = 300,
  limitsize = TRUE,
  bg = NULL,
  create.dir = FALSE,
)

# ::: Shape ZAT como imagen :::
Shape_ZAT
Plot_Vacio_ZAT <- ggplot(Shape_ZAT |> filter(COD_UPL != "<NA>")) +
  geom_sf(
    col = "white",
    linewidth = 0.6, # Original: 0.6
    aes(fill = COD_UPL)
  ) + 
  theme_void() +
  theme(
    legend.position = "none",
  )
#Plot_Vacio_ZAT

# Guardamos el plot como imagen:
ggsave(
  filename = "Plot_Vacio_ZAT.png",
  plot = Plot_Vacio_ZAT,
  device = NULL,
  path = "./Data/3_Results/1_Empty_Shapes/",
  scale = 1,
  width = Alto, # Intercambiadas porque está en vertical
  height = Ancho,
  units = c("px"),
  dpi = 300,
  limitsize = TRUE,
  bg = NULL,
  create.dir = FALSE,
)

# ::: Shape Sectores como imagen :::
Shape_Sectores
Plot_Vacio_Sectores <- ggplot(Shape_Sectores) +
  geom_sf(
    col = "white",
    linewidth = 0.6, # Original: 0.6
    aes(fill = COD_UPL)
  ) + 
  theme_void() +
  theme(
    legend.position = "none",
  )
#Plot_Vacio_Sectores

# Guardamos el plot como imagen:
ggsave(
  filename = "Plot_Vacio_Sectores.png",
  plot = Plot_Vacio_Sectores,
  device = NULL,
  path = "./Data/3_Results/1_Empty_Shapes/",
  scale = 1,
  width = Alto, # Intercambiadas porque está en vertical
  height = Ancho,
  units = c("px"),
  dpi = 300,
  limitsize = TRUE,
  bg = NULL,
  create.dir = FALSE,
)

# ::: Shape Secciones como imagen :::
Shape_Secciones
Plot_Vacio_Secciones <- ggplot(Shape_Secciones |> filter(COD_UPL != "<NA>")) +
  geom_sf(
    col = "white",
    linewidth = 0.6, # Original: 0.6
    aes(fill = COD_UPL)
  ) + 
  theme_void() +
  theme(
    legend.position = "none",
  )
#Plot_Vacio_Secciones

# Guardamos el plot como imagen:
ggsave(
  filename = "Plot_Vacio_Secciones.png",
  plot = Plot_Vacio_Secciones,
  device = NULL,
  path = "./Data/3_Results/1_Empty_Shapes/",
  scale = 1,
  width = Alto, # Intercambiadas porque está en vertical
  height = Ancho,
  units = c("px"),
  dpi = 300,
  limitsize = TRUE,
  bg = NULL,
  create.dir = FALSE,
)

# ::: Shape Secciones como imagen :::
Shape_Hexagonos
Plot_Vacio_Hexagonos <- ggplot(Shape_Hexagonos |> filter(COD_UPL != "<NA>")) +
  geom_sf(
    col = "white",
    linewidth = 0.6, # Original: 0.6
    aes(fill = COD_UPL)
  ) + 
  theme_void() +
  theme(
    legend.position = "none",
  )
#Plot_Vacio_Hexagonos

# Guardamos el plot como imagen:
ggsave(
  filename = "Plot_Vacio_Hexagonos.png",
  plot = Plot_Vacio_Hexagonos,
  device = NULL,
  path = "./Data/3_Results/1_Empty_Shapes/",
  scale = 1,
  width = Alto, # Intercambiadas porque está en vertical
  height = Ancho,
  units = c("px"),
  dpi = 300,
  limitsize = TRUE,
  bg = NULL,
  create.dir = FALSE,
)

# ::: Shape Manzanas como imagen :::
Shape_Manzanas |> slice_sample(n = 5)
Plot_Vacio_Manzanas <- ggplot(Shape_Manzanas |> filter(COD_UPL != "<NA>")) +
  geom_sf(
    col = "white",
    linewidth = 0.1,  # Original: 0.1
    aes(fill = COD_UPL)
  ) + 
  theme_void() +
  theme(
    legend.position = "none",
  )
#Plot_Vacio_Manzanas

# Guardamos el plot como imagen:
ggsave(
  filename = "Plot_Vacio_Manzanas.png",
  plot = Plot_Vacio_Manzanas,
  device = NULL,
  path = "./Data/3_Results/1_Empty_Shapes/",
  scale = 1,
  width = Alto, # Intercambiadas porque está en vertical
  height = Ancho,
  units = c("px"),
  dpi = 300,
  limitsize = TRUE,
  bg = NULL,
  create.dir = FALSE,
)

# ::: Todos los mapas en una sola imagen :::
Plot_Vacio_Todo <- Plot_Vacio_Secciones + Plot_Vacio_UTAM + Plot_Vacio_Municipios +
  Plot_Vacio_Hexagonos + Plot_Vacio_ZAT + Plot_Vacio_Localidades +
  Plot_Vacio_Manzanas + Plot_Vacio_Sectores + Plot_Vacio_UPLs
Plot_Vacio_Todo

# Guardamos el plot como imagen:
ggsave(
  filename = "Plot_Vacio_Todo.png",
  plot = Plot_Vacio_Todo,
  device = NULL,
  path = "./Data/3_Results/1_Empty_Shapes/",
  scale = 1,
  width = Alto, # Intercambiadas porque está en vertical
  height = Ancho,
  units = c("px"),
  dpi = 300,
  limitsize = FALSE,
  bg = NULL,
  create.dir = FALSE,
)

# Liberamos RAM
rm(Shape_Localidades_Temp, Shape_UPLs_Temp, Shape_UTAM_Temp, Shape_ZAT_Temp, Shape_Sectores_Temp, Shape_Secciones_Temp, Shape_Hexagonos_Temp, Shape_Manzanas_Temp)
rm(Interseccion_MNZ_LOC, Interseccion_MNZ_UPL, Interseccion_MNZ_UTAM, Interseccion_MNZ_ZAT, Interseccion_MNZ_HEX)
rm(Interseccion_HEX_LOC, Interseccion_HEX_UPL, Interseccion_HEX_UTAM, Interseccion_HEX_ZAT, Interseccion_HEX_SECT, Interseccion_HEX_SECC)
rm(Interseccion_SECC_LOC, Interseccion_SECC_UPL, Interseccion_SECC_UTAM, Interseccion_SECC_ZAT)
rm(Interseccion_SECT_LOC, Interseccion_SECT_UPL, Interseccion_SECT_UTAM, Interseccion_SECT_ZAT)
rm(Interseccion_ZAT_UPL)
rm(Interseccion_UTAM_UPL)
rm(Shape_Centroides_Manzanas_Temp, Shape_Centroides_Hexagonos_Temp, Shape_Centroides_Secciones_Temp, Shape_Centroides_Sectores_Temp, Shape_Centroides_ZAT_Temp, Shape_Centroides_UTAM_Temp)
rm(Shape_Manzana_Original, Shape_Seccion_Original, Shape_Sector_Original, Shape_ZAT_Original, Shape_UTAM_Original, Shape_UPL_Original, Shape_Localidad_Original)

# Tiempo de procesamiento:
End_Time_S1_P6 <- Sys.time()
print(paste("6. Tiempo de ploteo de shapes vacíos: ", as.duration(End_Time_S1_P6 - Start_Time_S1_P6)))

# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ::: 7. Guardado de Shapes Vacíos                                           :::
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#
# Guardamos los Shapes Vacíos
#
Start_Time_S1_P7 <- Sys.time()
st_write(Shape_Municipios, dsn = "./Data/2_Processing/1_Empty_Shapes/Shape_Vacio_Municipios.gpkg", driver = 'GPKG', append = FALSE)
st_write(Shape_Localidades, dsn = "./Data/2_Processing/1_Empty_Shapes/Shape_Vacio_Localidades.gpkg", driver = 'GPKG', append = FALSE)
st_write(Shape_UPLs, dsn = "./Data/2_Processing/1_Empty_Shapes/Shape_Vacio_UPLs.gpkg", driver = 'GPKG', append = FALSE)
st_write(Shape_UTAM, dsn = "./Data/2_Processing/1_Empty_Shapes/Shape_Vacio_UTAM.gpkg", driver = 'GPKG', append = FALSE)
st_write(Shape_ZAT, dsn = "./Data/2_Processing/1_Empty_Shapes/Shape_Vacio_ZAT.gpkg", driver = 'GPKG', append = FALSE)
st_write(Shape_Sectores, dsn = "./Data/2_Processing/1_Empty_Shapes/Shape_Vacio_Sectores.gpkg", driver = 'GPKG', append = FALSE)
st_write(Shape_Secciones, dsn = "./Data/2_Processing/1_Empty_Shapes/Shape_Vacio_Secciones.gpkg", driver = 'GPKG', append = FALSE)
st_write(Shape_Hexagonos, dsn = "./Data/2_Processing/1_Empty_Shapes/Shape_Vacio_Hexagonos.gpkg", driver = 'GPKG', append = FALSE)
st_write(Shape_Manzanas, dsn = "./Data/2_Processing/1_Empty_Shapes/Shape_Vacio_Manzanas.gpkg", driver = 'GPKG', append = FALSE)

# Tiempo de procesamiento:
End_Time_S1_P7 <- Sys.time()
print(paste("7. Tiempo de guardado de shapes vacíos: ", as.duration(End_Time_S1_P7 - Start_Time_S1_P7)))

# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ::: 8. ¿Necesita cargar los resultados de este Script?                     :::
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#
# El objetivo de esta sección, es recargar los resultados de este script, de manera que
# no sea necesario correrlo de nuevo si se llegan a necesitar sus resultados:
#
Start_Time_S1_P8 <- Sys.time()

Shape_Municipios <- st_read("./Data/2_Processing/1_Empty_Shapes/Shape_Vacio_Municipios.gpkg")
Shape_Localidades <- st_read("./Data/2_Processing/1_Empty_Shapes/Shape_Vacio_Localidades.gpkg")
Shape_UPLs <- st_read("./Data/2_Processing/1_Empty_Shapes/Shape_Vacio_UPLs.gpkg")
Shape_UTAM <- st_read("./Data/2_Processing/1_Empty_Shapes/Shape_Vacio_UTAM.gpkg")
Shape_ZAT <- st_read("./Data/2_Processing/1_Empty_Shapes/Shape_Vacio_ZAT.gpkg")
Shape_Sectores <- st_read("./Data/2_Processing/1_Empty_Shapes/Shape_Vacio_Sectores.gpkg")
Shape_Secciones <- st_read("./Data/2_Processing/1_Empty_Shapes/Shape_Vacio_Secciones.gpkg")
Shape_Hexagonos <- st_read("./Data/2_Processing/1_Empty_Shapes/Shape_Vacio_Hexagonos.gpkg")
Shape_Manzanas <- st_read("./Data/2_Processing/1_Empty_Shapes/Shape_Vacio_Manzanas.gpkg")
MostrarMapa(Shape_Manzanas)
# Tiempo de procesamiento:
End_Time_S1_P8 <- Sys.time()
print(paste("8. Tiempo de recarga de resultados: ", as.duration(End_Time_S1_P8 - Start_Time_S1_P8)))

# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ::: 9. Tiempos de procesamiento                                            :::
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#
# Mostramos los tiempos de procesamiento:
#
End_Time_S1_P0 <- Sys.time()
print(paste("1. Tiempo de inicialización: ", as.duration(End_Time_S1_P1 - Start_Time_S1_P1)))
print(paste("2. Tiempo de carga de datos de origen: ", as.duration(End_Time_S1_P2 - Start_Time_S1_P2)))
print(paste("3. Tiempo de filtrado y alistamiento de shapes temporales: ", as.duration(End_Time_S1_P3 - Start_Time_S1_P3)))
print(paste("4. Tiempo de creación del grid del municipio: ", as.duration(End_Time_S1_P4 - Start_Time_S1_P4)))
print(paste("5. Tiempo consolidación de shapes con la información de ubicación: ", as.duration(End_Time_S1_P5 - Start_Time_S1_P5)))
print(paste("6. Tiempo de ploteo de shapes vacíos: ", as.duration(End_Time_S1_P6 - Start_Time_S1_P6)))
print(paste("7. Tiempo de guardado de shapes vacíos: ", as.duration(End_Time_S1_P7 - Start_Time_S1_P7)))
print(paste("8. Tiempo de recarga de resultados: ", as.duration(End_Time_S1_P8 - Start_Time_S1_P8)))
print(paste("Tiempo total de procesamiento: ", as.duration(End_Time_S1_P0 - Start_Time_S1_P0)))

