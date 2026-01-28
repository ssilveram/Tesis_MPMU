# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# :::                                                                        :::
# :::                      SCRIPT DE ORIGENES-DESTINO                        :::
# :::                                                                        :::
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#
# Nombre:        3_EM2023.R
#
# Descripcion: Este Script cargará los datos de la encuesta de movilidad 2023
#              y la procesará para encontrar datos de interés a fin de procesarlos
#              y obtener gráficas y matrices de viajes, tiempos, etc.

# ####
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# :::                      Contenido de este Script                          :::
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ####
#
# 1. Inicialización
# 2. Carga de datos de origen.
# 3. Filtrado y Tratamiento de dataframes de viajes
# 4. Relacionamiento entre Shapes de ZAT y UPL con el df de Viajes
# 5. Iniciamos procesos de impresión de mapa de Origenes-Destino
# 6. Gráfico de cuerdas Origen-Destino entre UPLs
# 7. Estandarización del df de viajes
# 8. Establecer colores para los principales motivos de viaje
# 9. Principales motivos de viaje de la encuesta
# 10. Distribución modal y Gráfico de barras polares
# 11. Distribución de modos sustentables y no sustentables
# 12. Tiempos medios de las tres actividades principales
# 13. Tiempos medios de las 3 actividades principales | Solo Universidad
# 14. Tiempos de viaje por UPL y por Categoría
# 15. Tiempos de viaje por estrato socioeconómico
# 16. Tiempo de viaje por género
# 17. Tiempos de viaje generales por UPL
# 18. Simplificación de la tabla de viajes a mostrar
# 19. Alistamiento de tabla para calibración del modelo
# 20. Tabla de UPLs que más viajes reciben
# 21. Mapa de UPLs con etiquetas sin colores solo para referencia
# 22. Totalizamos los tiempos de procesamiento del Script

Start_Time_S3_P0 <- Sys.time()

# ####
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ::: 1. Inicialización                                                      :::
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ####
#
Start_Time_S3_P1 <- Sys.time()

source("0_Initialization.R")

# Tiempo de procesamiento:
End_Time_S3_P1 <- Sys.time()
print(paste("1. Tiempo de inicialización: ", as.duration(End_Time_S3_P1 - Start_Time_S3_P1)))

# ####
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ::: 2. Carga de datos de origen                                            :::
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ####
#
# Cargamos los shapes y dataframes originales para comenzar a filtrar y poner
# todo en orden para futuros análisis
#
Start_Time_S3_P2 <- Sys.time()

# --- 2.1. Carga de Shapes Vacíos  ---

# Vienen de 1_Empty_Shapes.r
Shape_Municipios <- st_read("./Data/2_Processing/1_Empty_Shapes/Shape_Vacio_Municipios.gpkg")
Shape_Localidades <- st_read("./Data/2_Processing/1_Empty_Shapes/Shape_Vacio_Localidades.gpkg")
Shape_UPLs <- st_read("./Data/2_Processing/1_Empty_Shapes/Shape_Vacio_UPLs.gpkg")
Shape_UTAM <- st_read("./Data/2_Processing/1_Empty_Shapes/Shape_Vacio_UTAM.gpkg")
Shape_ZAT <- st_read("./Data/2_Processing/1_Empty_Shapes/Shape_Vacio_ZAT.gpkg")
Shape_Sectores <- st_read("./Data/2_Processing/1_Empty_Shapes/Shape_Vacio_Sectores.gpkg")
Shape_Hexagonos <- st_read("./Data/2_Processing/1_Empty_Shapes/Shape_Vacio_Hexagonos.gpkg")
Shape_Manzanas <- st_read("./Data/2_Processing/1_Empty_Shapes/Shape_Vacio_Manzanas.gpkg")
Coordinates_CRS <- st_crs(Shape_Municipios)

# Cargamos Shape de Estratificación:
Shape_Estratificacion_Original <- st_read("./Data/1_Sources/1_Shapes/Custom/Shape_Estratificacion_Original.gpkg")

# --- 2.2. Carga de Datos de la encuesta ---

# Método rápido:
CSV_EH_Hogares_Original <- fread("./Data/1_Sources/3_EM2023/EODH/05_Base datos procesada/CSV/a. Modulo hogares.csv", check.names=FALSE, sep=";", dec=",", na = "null", colClasses=c("cod_dane_manzana_hg"="character"))
CSV_EH_Vehiculos_Original <- fread("./Data/1_Sources/3_EM2023/EODH/05_Base datos procesada/CSV/b. Modulo vehiculos.csv", check.names=FALSE, sep=";", dec=",", na = "null")
CSV_EH_Personas_Original <- fread("./Data/1_Sources/3_EM2023/EODH/05_Base datos procesada/CSV/c. Modulo personas.csv", check.names=FALSE, sep=";", dec=",", na = "null")
CSV_EH_Viajes_Original <- fread("./Data/1_Sources/3_EM2023/EODH/05_Base datos procesada/CSV/d. Modulo viajes.csv", check.names=FALSE, sep=";", dec=",", na = "null")
CSV_EH_Etapas_Original <- fread("./Data/1_Sources/3_EM2023/EODH/05_Base datos procesada/CSV/e. Modulo etapas.csv", check.names=FALSE, sep=";", dec=",", na = "null")
CSV_EI_Interceptacion_Original <- fread("./Data/1_Sources/3_EM2023/EODI/04_Base datos/DB_Interceptacion.csv", check.names=FALSE, sep=";", dec=",", na = "null")

# --- 2.3 Recarga de datos de población ---
Censo_Municipios <- as.data.frame(fread("./Data/2_Processing/2_Population/Censo_Municipios.csv", sep=",", na = "null", colClasses=c("COD_CPOB"="character")))
# Censo_Localidades <- as.data.frame(fread("./Data/2_Processing/2_Population/Censo_Localidades.csv", sep=",", na = "null", colClasses=c("COD_LOC"="character")))
# Censo_UPLs <- as.data.frame(fread("./Data/2_Processing/2_Population/Censo_UPLs.csv", sep=",", na = "null", colClasses=c("COD_UPL"="character")))
# Censo_UTAM <- as.data.frame(fread("./Data/2_Processing/2_Population/Censo_UTAM.csv", sep=",", na = "null", colClasses=c("COD_UTAM"="character")))
# Censo_ZAT <- as.data.frame(fread("./Data/2_Processing/2_Population/Censo_ZAT.csv", sep=",", na = "null", colClasses=c("COD_ZAT"="character")))
# Censo_Sectores <- as.data.frame(fread("./Data/2_Processing/2_Population/Censo_Sectores.csv", sep=",", na = "null", colClasses=c("COD_SECT"="character")))
# Censo_Secciones <- as.data.frame(fread("./Data/2_Processing/2_Population/Censo_Secciones.csv", sep=",", na = "null", colClasses=c("COD_SECC"="character")))
# Censo_Hexagonos <- as.data.frame(fread("./Data/2_Processing/2_Population/Censo_Hexagonos.csv", sep=",", na = "null", colClasses=c("COD_HEX"="character")))
# Censo_Manzanas <- as.data.frame(fread("./Data/2_Processing/2_Population/Censo_Manzanas.csv", sep=",", na = "null", colClasses=c("COD_LOC"="character", "COD_UPL"="character", "COD_UTAM"="character", "COD_ZAT"="character", "COD_SECT"="character", "COD_SECC"="character", "COD_HEX"="character", "COD_MNZ"="character")))
# Censo_Viviendas <- as.data.frame(fread("./Data/2_Processing/2_Population/Censo_Viviendas.csv", sep=",", na = "null", colClasses=c("COD_LOC"="character", "COD_UPL"="character", "COD_UTAM"="character", "COD_ZAT"="character", "COD_SECT"="character", "COD_SECC"="character", "COD_HEX"="character", "COD_MNZ"="character", "COD_VIVIENDA"="character")))
# Censo_Familias <- as.data.frame(fread("./Data/2_Processing/2_Population/Censo_Familias.csv", sep=",", na = "null", colClasses=c("COD_LOC"="character", "COD_UPL"="character", "COD_UTAM"="character", "COD_ZAT"="character", "COD_SECT"="character", "COD_SECC"="character", "COD_HEX"="character", "COD_MNZ"="character", "COD_FAMILIA"="character")))
# Censo_Personas <- as.data.frame(fread("./Data/2_Processing/2_Population/Censo_Personas.csv", sep=",", na = "null", colClasses=c("COD_LOC"="character", "COD_UPL"="character", "COD_UTAM"="character", "COD_ZAT"="character", "COD_SECT"="character", "COD_SECC"="character", "COD_HEX"="character", "COD_MNZ"="character", "COD_FAMILIA"="character", "COD_PERSONA"="character")))

# Tiempo de procesamiento:
End_Time_S3_P2 <- Sys.time()
print(paste("2. Tiempo de carga de datos de origen: ", as.duration(End_Time_S3_P2 - Start_Time_S3_P2)))

# ####
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ::: 3. Filtrado y Tratamiento de dataframes de viajes                      :::
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ####
#
# El objetivo es dejar lo más simplificados posibles los dataframes originales
# con la información sobre los viajes.
#
Start_Time_S3_P3 <- Sys.time()

# --- 3.1. Tratamiento del df de hogares de la Encuesta de Hogares (EH) ---

CSV_EH_Hogares_Original
unique(CSV_EH_Hogares_Original$nom_mpio_hg)

CSV_EH_Hogares <- CSV_EH_Hogares_Original |>
  filter(nom_mpio_hg == "Bogotá D.C.") |>
  mutate(
    # Creamos los campos en formato Estándar:
    COD_HOGAR = str_c("HOG_EH_", cod_hog, sep = ""),
    COD_LOC_HOGAR = cod_loc_hg,
    COD_UPL_HOGAR = cod_upl_hg,
    COD_UTAM_HOGAR = cod_utam_hg,
    COD_ZAT_HOGAR = "",
    COD_MNZ_HOGAR = cod_dane_manzana_hg,
    BARRIO_HOGAR = nom_barrio_vereda_hg,
    ESTRATO_HOGAR = estrato_hg
  ) |>
  # Tratamiento y relacionamiento con otras fuentes de datos:
  # Seleccionamos solamente las variables necesarias:
  select(
    COD_HOGAR,
    COD_LOC_HOGAR,
    COD_UPL_HOGAR,
    COD_UTAM_HOGAR,
    COD_ZAT_HOGAR,
    COD_MNZ_HOGAR,
    BARRIO_HOGAR,
    ESTRATO_HOGAR
  ) |>
  as.data.frame()
CSV_EH_Hogares

# --- 3.2. Tratamiento del df de personas de la Encuesta de Hogares (EH) ---

CSV_EH_Personas_Original |>
  mutate(
    COD_PERSONA = str_c("PER_EH_", cod_per, sep = ""),
    EDAD = edad,
    SEXO = sexo,
    ESTRATO = as.character(estra_hg),
    COD_HOGAR = str_c("HOG_EH_", cod_hg, sep = ""),
    GENERO = genero,
    ORIENTACION = orien_sexual,
    ETNIA = identidad_etnica,
    MADRE_CABEZA = madre_cab_familia,
    EDUCACION = max_nivel_edu,
    OCUPACION = ocupacion_principal,
    ACTIVIDAD_ECO = actividad_economica,
    DISCAPACIDAD = condicion_discapacidad,
  ) |>
  select(
    COD_PERSONA,
    EDAD,
    SEXO,
    ESTRATO,
    COD_HOGAR,
    GENERO,
    ORIENTACION,
    ETNIA,
    MADRE_CABEZA,
    EDUCACION,
    OCUPACION,
    ACTIVIDAD_ECO,
    DISCAPACIDAD,
  )

# --- 3.3. Tratamiento del df de viajes de la Encuesta de Hogares (EH) ---

CSV_EH_Viajes_Original <- CSV_EH_Viajes_Original |>
#  select(localidad_ori, upl_ori, utam_ori, zat_ori) |>
  mutate(COD_ZAT = as.character(zat_ori)) |>
  left_join(Shape_ZAT |> as_tibble() |> select(COD_UPL, COD_ZAT)) |>
  mutate(upl_ori = COD_UPL) |>
  select(-COD_UPL, -COD_ZAT)

Viajes_EH_df <- CSV_EH_Viajes_Original |>
  filter(nom_mun_ori == "Bogotá" & nom_mun_des == "Bogotá") |>
  # Creamos los campos necesarios:
  mutate(
    DATA_ORIGEN = "EH",
    # Demográficos:
    COD_VIAJE = str_c("VI_EH_", cod_vj, sep = ""),
    COD_PERSONA = str_c("PER_EH_", cod_pers, sep = ""),
    EDAD = edad,
    SEXO = sexo,
    ESTRATO = as.character(estra_hg),
    COD_HOGAR = str_c("HOG_EH_", cod_hg, sep = ""),
    ZAT_HOGAR = as.character(zat_hg),
    OCUPACION = ocupacion_principal,
    # Origen-Destino
    LOC_ORIGEN = localidad_ori,
    LOC_DESTINO = localidad_des,
    UPL_ORIGEN = upl_ori,
    UPL_DESTINO = upl_des,
    UTAM_ORIGEN = utam_ori,
    UTAM_DESTINO = utam_des,
    ZAT_ORIGEN = zat_ori,
    ZAT_DESTINO = zat_des,
    MNZ_ORIGEN = "",
    MNZ_DESTINO = "",
    LON_ORIGEN = 0,
    LAT_ORIGEN = 0,
    LON_DESTINO = 0,
    LAT_DESTINO = 0,
    ACTIVIDAD_ORIGEN = "",
    ACTIVIDAD_DESTINO = motivo_viaje,
    VEHICULO = modo_principal_agrupado,
    # Horarios y tiempos
    FRANJA_HORARIA = "",
    HORA_INICIO_VIAJE = as.numeric(str_replace_all(if_else(hora_ini == " - ", "0", str_replace_all(hora_ini, " ", "")),",",".")),
    HORA_FIN_VIAJE = as.numeric(str_replace_all(if_else(hora_fin == " - ", "0", str_replace_all(hora_fin, " ", "")),",",".")),
    TIEMPO_VIAJE = as.numeric(str_replace_all(if_else(duracion_min == " - ", "0", str_replace_all(duracion_min, " ", "")),",",".")),
    TIEMPO_ACCESO = as.numeric(str_replace_all(if_else(t_acceso_min == " - ", "0", str_replace_all(t_acceso_min, " ", "")),",",".")),
    TIEMPO_ESPERA = as.numeric(str_replace_all(if_else(t_espera_min == " - ", "0", str_replace_all(t_espera_min, " ", "")),",",".")),
    TIEMPO_EGRESO = as.numeric(str_replace_all(if_else(t_egreso_min == " - ", "0", str_replace_all(t_egreso_min, " ", "")),",",".")),
    TIEMPO_TOTAL = TIEMPO_ACCESO + TIEMPO_ESPERA + TIEMPO_VIAJE + TIEMPO_EGRESO,
    HORA_ORIGEN = 0,
    HORA_DESTINO = 0,
    # Validación
    DIST_LON = LON_ORIGEN - LON_DESTINO,
    DIST_LAT = LAT_ORIGEN - LAT_DESTINO,
  ) |>
  # Tratamiento de campos y relleno de faltantes
  # Cambiamos los textos del nombre de la Localidad, por el Código de la Localidad
  mutate(LOC_ORIGEN = str_replace_all(LOC_ORIGEN, "ó", "o")) |>
  mutate(LOC_DESTINO = str_replace_all(LOC_DESTINO, "ó", "o")) |>
  inner_join(
    Shape_Localidades |>
      as_tibble() |>
      mutate(LOC_ORIGEN = NOM_LOC, COD_LOC_ORIGEN = COD_LOC) |>
      select(LOC_ORIGEN, COD_LOC_ORIGEN),
    by = "LOC_ORIGEN"
  ) |>
  mutate(LOC_ORIGEN = COD_LOC_ORIGEN) |>
  inner_join(
    Shape_Localidades |>
      as_tibble() |>
      mutate(LOC_DESTINO = NOM_LOC, COD_LOC_DESTINO = COD_LOC) |>
      select(LOC_DESTINO, COD_LOC_DESTINO),
    by = "LOC_DESTINO"
  ) |>
  mutate(LOC_DESTINO = COD_LOC_DESTINO) |>
  # Selección final de variables
  select(
    DATA_ORIGEN,
    # Demográficos:
    COD_VIAJE,
    COD_PERSONA,
    EDAD,
    SEXO,
    ESTRATO,
    COD_HOGAR,
    ZAT_HOGAR,
    OCUPACION,
    # Origen-Destino
    LOC_ORIGEN,
    LOC_DESTINO,
    UPL_ORIGEN,
    UPL_DESTINO,
    UTAM_ORIGEN,
    UTAM_DESTINO,
    ZAT_ORIGEN,
    ZAT_DESTINO,
    MNZ_ORIGEN,
    MNZ_DESTINO,
    LON_ORIGEN,
    LAT_ORIGEN,
    LON_DESTINO,
    LAT_DESTINO,
    ACTIVIDAD_ORIGEN,
    ACTIVIDAD_DESTINO,
    VEHICULO,
    # Horarios y tiempos
    FRANJA_HORARIA,
    HORA_INICIO_VIAJE,
    HORA_FIN_VIAJE,
    TIEMPO_VIAJE,
    TIEMPO_ACCESO,
    TIEMPO_ESPERA,
    TIEMPO_EGRESO,
    TIEMPO_TOTAL,
    HORA_ORIGEN,
    HORA_DESTINO,
    # Validación
    DIST_LON,
    DIST_LAT,
  )
Viajes_EH_df

Shape_Localidades
Shape_Localidades |>
  as_tibble() |>
  mutate(LOC_ORIGEN = NOM_LOC, COD_LOC_ORIGEN = COD_LOC) |>
  select(LOC_ORIGEN, COD_LOC_ORIGEN)

# Cambiamos los textos del nombre de la Localidad, por el Código de la Localidad
Viajes_EH_df |>
#  select(LOC_ORIGEN, LOC_DESTINO) |>
  # Cambiamos los textos del nombre de la Localidad, por el Código de la Localidad
  mutate(LOC_ORIGEN = str_replace_all(LOC_ORIGEN, "ó", "o")) |>
  mutate(LOC_DESTINO = str_replace_all(LOC_DESTINO, "ó", "o")) |>
  inner_join(
    Shape_Localidades |>
      as_tibble() |>
      mutate(LOC_ORIGEN = NOM_LOC, COD_LOC_ORIGEN = COD_LOC) |>
      select(LOC_ORIGEN, COD_LOC_ORIGEN),
    by = "LOC_ORIGEN"
  ) |>
  mutate(LOC_ORIGEN = COD_LOC_ORIGEN) |>
  inner_join(
    Shape_Localidades |>
      as_tibble() |>
      mutate(LOC_DESTINO = NOM_LOC, COD_LOC_DESTINO = COD_LOC) |>
      select(LOC_DESTINO, COD_LOC_DESTINO),
    by = "LOC_DESTINO"
  ) |>
  mutate(LOC_DESTINO = COD_LOC_DESTINO)

str_replace_all(Viajes_EH_df$LOC_ORIGEN, "ó", "o") |>
  unique()

# --- 3.4. Tratamiento de viajes por Encuesta de Iterceptación ---

CSV_EI_Interceptacion_Original
names(CSV_EI_Interceptacion_Original)

Viajes_EI_df <- CSV_EI_Interceptacion_Original |>
  filter(origen_ciudad_municipio == "BOGOTÁ, D.C." & destino_ciudad_municipio == "BOGOTÁ, D.C.") |>
  # Creamos los campos necesarios:
  mutate(
    DATA_ORIGEN = "EI",
    # Demográficos
    COD_VIAJE = str_c("EI_",row_number(),sep = ""),
    COD_PERSONA = str_c("PER_EI_", row_number(), sep = ""),
    EDAD = edad,
    SEXO = sexo,
    ESTRATO = as.character(estrato),
    COD_HOGAR = str_c("HOG_EI_",row_number(),sep = ""),
    ZAT_HOGAR = "",
    OCUPACION = "",
    # Origen-Destino:
    LOC_ORIGEN = "",
    LOC_DESTINO = "",
    UPL_ORIGEN = "",
    UPL_DESTINO = "",
    UTAM_ORIGEN = "",
    UTAM_DESTINO = "",
    ZAT_ORIGEN = ZAT_Origen,
    ZAT_DESTINO = ZAT_Destino,
    MNZ_ORIGEN = "",
    MNZ_DESTINO = "",
    LON_ORIGEN = Longitud_Org,
    LAT_ORIGEN = Latitud_Org,
    LON_DESTINO = Longitud_Des,
    LAT_DESTINO = Latitud_Des,
    ACTIVIDAD_ORIGEN = origen_actividad,
    ACTIVIDAD_DESTINO = destino_actividad,
    VEHICULO = `tipo_vehiculo/servicio`,
    # Horarios y tiempos
    FRANJA_HORARIA = Periodo,
    HORA_INICIO_VIAJE = 0,
    HORA_FIN_VIAJE = 0,
    TIEMPO_VIAJE = 0,
    TIEMPO_ACCESO = 0,
    TIEMPO_ESPERA = 0,
    TIEMPO_EGRESO = 0,
    TIEMPO_TOTAL = TIEMPO_ACCESO + TIEMPO_ESPERA + TIEMPO_VIAJE + TIEMPO_EGRESO,
    HORA_ORIGEN = 0,
    HORA_DESTINO = 0,
    # Validación
    DIST_LON = LON_ORIGEN - LON_DESTINO,
    DIST_LAT = LAT_ORIGEN - LAT_DESTINO
  ) |>
  # Tratamiento de campos y relleno de faltantes
  filter(DIST_LON != 0 & DIST_LAT != 0) |>
  # Selección final de variables
  select(
    DATA_ORIGEN,
    # Demográficos
    COD_VIAJE,
    COD_PERSONA,
    EDAD,
    SEXO,
    ESTRATO,
    COD_HOGAR,
    ZAT_HOGAR,
    OCUPACION,
    # Origen-Destino:
    LOC_ORIGEN,
    LOC_DESTINO,
    UPL_ORIGEN,
    UPL_DESTINO,
    UTAM_ORIGEN,
    UTAM_DESTINO,
    ZAT_ORIGEN,
    ZAT_DESTINO,
    MNZ_ORIGEN,
    MNZ_DESTINO,
    LON_ORIGEN,
    LAT_ORIGEN,
    LON_DESTINO,
    LAT_DESTINO,
    ACTIVIDAD_ORIGEN,
    ACTIVIDAD_DESTINO,
    VEHICULO,
    # Horarios y tiempos
    FRANJA_HORARIA,
    HORA_INICIO_VIAJE,
    HORA_FIN_VIAJE,
    TIEMPO_VIAJE,
    TIEMPO_ACCESO,
    TIEMPO_ESPERA,
    TIEMPO_EGRESO,
    TIEMPO_TOTAL,
    HORA_ORIGEN,
    HORA_DESTINO,
    # Validación
    DIST_LON,
    DIST_LAT
  )
Viajes_EI_df

# Stacking de Viajes de Encuesta Hogar, y de Interceptación
Viajes_df <- Viajes_EH_df |>
  bind_rows(Viajes_EI_df) |>
  mutate(
    ESTRATO = as.character(ESTRATO),
    LOC_ORIGEN = as.character(LOC_ORIGEN),
    LOC_DESTINO = as.character(LOC_DESTINO),
    UPL_ORIGEN = as.character(UPL_ORIGEN),
    UPL_DESTINO = as.character(UPL_DESTINO),
    UTAM_ORIGEN = as.character(UTAM_ORIGEN),
    UTAM_DESTINO = as.character(UTAM_DESTINO),
    ZAT_ORIGEN = as.character(ZAT_ORIGEN),
    ZAT_DESTINO = as.character(ZAT_DESTINO),
    MNZ_ORIGEN = as.character(MNZ_ORIGEN),
    MNZ_DESTINO = as.character(MNZ_DESTINO)
  )
Viajes_df

# Guardamos para no repetir el proceso
save(
  Viajes_df,
  file = "./Data/2_Processing/3_EM2023/Viajes_df.RData"
)
load("./Data/2_Processing/3_EM2023/Viajes_df.RData")
Viajes_df

No_Rural <- Shape_UPLs |> st_drop_geometry() |> filter(ZONA_UPL != "Rural") |> select(COD_UPL)
ZAT_Validos <- Shape_ZAT |> st_drop_geometry() |> filter(COD_UPL %in% No_Rural$COD_UPL) |> select(COD_ZAT)
Viajes_df |> filter(ZAT_ORIGEN %in% ZAT_Validos$COD_ZAT & ZAT_DESTINO %in% ZAT_Validos$COD_ZAT)

length(Viajes_EH_df$DATA_ORIGEN)
length(Viajes_EI_df$DATA_ORIGEN)
length(Viajes_df$DATA_ORIGEN)
length(Viajes_EH_df$DATA_ORIGEN) + length(Viajes_EI_df$DATA_ORIGEN) == length(Viajes_df$DATA_ORIGEN)

# --- Fin de tratamiento de df de Viajes ---

# Tiempo de procesamiento:
End_Time_S3_P3 <- Sys.time()
print(paste("3. Tiempo de tratamiento de df de viajes: ", as.duration(End_Time_S3_P3 - Start_Time_S3_P3)))

# ####
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ::: 4. Relacionamiento entre Shapes de ZAT y UPL con el df de Viajes       :::
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ####
#
# Vincular los shapes de ZAT y UPL con la información del data frame de viajes
# viajes y origen destino
#
Start_Time_S3_P4 <- Sys.time()

# Viajes por ZATs
Shape_ZAT
Viajes_ZAT_df <- Viajes_df |>
  select(COD_VIAJE, ZAT_ORIGEN, ZAT_DESTINO) |>
  mutate(COD_ZAT = ZAT_ORIGEN) |>
  inner_join(
    Shape_ZAT |>
      st_centroid(),
    by = "COD_ZAT") |>
  as_tibble() |>
  select(COD_VIAJE, ZAT_ORIGEN, ZAT_DESTINO, geom) |>
  extract(geom, c('LON_ORIGEN', 'LAT_ORIGEN'), '\\((.*), (.*)\\)', convert = TRUE) |>
  mutate(COD_ZAT = ZAT_DESTINO) |>
  inner_join(
    Shape_ZAT |>
      st_centroid(),
    by = "COD_ZAT") |>
  as_tibble() |>
  select(COD_VIAJE, ZAT_ORIGEN, ZAT_DESTINO, LON_ORIGEN, LAT_ORIGEN, geom) |>
  extract(geom, c('LON_DESTINO', 'LAT_DESTINO'), '\\((.*), (.*)\\)', convert = TRUE) |>
  as.data.frame()
Viajes_ZAT_df

# Viajes por UPLs
Shape_UPLs
Viajes_UPL_df <- Viajes_df |>
  select(COD_VIAJE, ZAT_ORIGEN, ZAT_DESTINO) |>
  mutate(COD_ZAT = ZAT_ORIGEN) |>
  inner_join(Shape_ZAT, by = "COD_ZAT") |>
  select(COD_VIAJE, ZAT_ORIGEN, ZAT_DESTINO, COD_UPL) |>
  inner_join(
    Shape_UPLs |>
      st_centroid(),
    by = "COD_UPL") |>
  mutate(UPL_ORIGEN = COD_UPL) |>
  extract(geom, c('LON_ORIGEN', 'LAT_ORIGEN'), '\\((.*), (.*)\\)', convert = TRUE) |>
  select(COD_VIAJE, UPL_ORIGEN, ZAT_DESTINO, LON_ORIGEN, LAT_ORIGEN) |>
  mutate(COD_ZAT = ZAT_DESTINO) |>
  inner_join(Shape_ZAT, by = "COD_ZAT") |>
  select(COD_VIAJE, UPL_ORIGEN, LON_ORIGEN, LAT_ORIGEN, COD_UPL) |>
  inner_join(
    Shape_UPLs |>
      st_centroid(),
    by = "COD_UPL") |>
  mutate(UPL_DESTINO = COD_UPL) |>
  extract(geom, c('LON_DESTINO', 'LAT_DESTINO'), '\\((.*), (.*)\\)', convert = TRUE) |>
  select(COD_VIAJE, UPL_ORIGEN, UPL_DESTINO, LON_ORIGEN, LAT_ORIGEN, LON_DESTINO, LAT_DESTINO)
Viajes_UPL_df

# Separo los Origenes
Origenes_sf <- Viajes_UPL_df |>
  mutate(COD_ORIGEN = str_c(COD_VIAJE,"_ORIGEN", sep = "")) |>
  mutate(lon = as.numeric(LON_ORIGEN), lat = as.numeric(LAT_ORIGEN)) |>
  select(COD_VIAJE, COD_ORIGEN, lon, lat) |>
  sf::st_as_sf(
    coords = c("lon", "lat"),
    crs = Coordinates_CRS,
    remove = FALSE
  ) |>
  sf::st_transform(Coordinates_CRS) |>
  #sf::st_intersection(Shape_Municipios) |>
  select(COD_VIAJE, COD_ORIGEN, lon, lat)
Origenes_sf

# Separo los Destinos
Destinos_sf <- Viajes_UPL_df |>
  filter(COD_VIAJE %in% Origenes_sf$COD_VIAJE) |>
  mutate(COD_DESTINO = str_c(COD_VIAJE,"_DESTINO", sep = "")) |>
  mutate(lon = as.numeric(LON_DESTINO), lat = as.numeric(LAT_DESTINO)) |>
  select(COD_VIAJE, COD_DESTINO, lon, lat) |>
  sf::st_as_sf(
    coords = c("lon", "lat"),
    crs = Coordinates_CRS,
    remove = FALSE
  ) |>
  sf::st_transform(Coordinates_CRS) |>
#  sf::st_intersection(Shape_Municipios) |>
  select(COD_VIAJE, COD_DESTINO, lon, lat)
Destinos_sf

Origenes_sf <- Origenes_sf |>
  filter(COD_VIAJE %in% Destinos_sf$COD_VIAJE)
Origenes_sf

# Verificamos tamaños
length(Origenes_sf$COD_VIAJE) == length(Destinos_sf$COD_VIAJE)

# Echamos un vistazo en el mapa
# tm_shape(Shape_Municipios) +                # Plot the shape
#  tm_polygons(alpha = 0.6) +                 # Puting transparent mode
#  tm_shape(Origenes_sf) +
#  tm_dots(col = "red", size = 0.01) +
#  tm_shape(Destinos_sf) +
#  tm_dots(col = "green", size = 0.01)

st_write(Origenes_sf, dsn = "./Data/2_Processing/3_EM2023/Origenes_EM2023_sf.gpkg", driver = 'GPKG', append = FALSE)
st_write(Destinos_sf, dsn = "./Data/2_Processing/3_EM2023/Destinos_EM2023_sf.gpkg", driver = 'GPKG', append = FALSE)
Origenes_sf <- st_read("./Data/2_Processing/3_EM2023/Origenes_EM2023_sf.gpkg")
Destinos_sf <- st_read("./Data/2_Processing/3_EM2023/Destinos_EM2023_sf.gpkg")

# Tiempo de procesamiento:
End_Time_S3_P4 <- Sys.time()
print(paste("4. Tiempo de relacionamiento de Shapes de ZAT y UPLs con el df de viajes: ", as.duration(End_Time_S3_P4 - Start_Time_S3_P4)))

# ####
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ::: 5. Iniciamos procesos de impresión de mapa de Origenes-Destino         :::
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ####
#
# El objetivo es plotear un mapa de los Orígenes-Destino de las ZAT, basados en
# una muestra del total de viajes (por tiempos de procesamiento)
#
Start_Time_S3_P5 <- Sys.time()

Muestra <- 0.01 # Porcentaje de viajes a mostrar en el mapa (Recomendado: 1% / 2%)
length(Viajes_df$COD_VIAJE)

# Orígenes-Destino de ZATs

Pares_OD_Mapa_df <- Viajes_df |>
  select(ZAT_ORIGEN, ZAT_DESTINO) |>
  slice_sample(n = as.integer(length(Viajes_df$COD_VIAJE) * Muestra)) |>
  as.data.frame()
Pares_OD_Mapa_df

Shape_ZAT
Vertices_OD_Mapa_df <- Shape_ZAT |>
  st_centroid() |>
  filter(COD_ZAT %in% Pares_OD_Mapa_df$ZAT_ORIGEN) |>
  filter(COD_ZAT %in% Pares_OD_Mapa_df$ZAT_DESTINO) |>
  select(COD_ZAT, geom) |>
  extract(geom, c('lon', 'lat'), '\\((.*), (.*)\\)', convert = TRUE) |>
  as.data.frame()
Vertices_OD_Mapa_df

Pares_OD_Mapa_df <- Pares_OD_Mapa_df |>
  filter(ZAT_ORIGEN %in% Vertices_OD_Mapa_df$COD_ZAT) |>
  filter(ZAT_DESTINO %in% Vertices_OD_Mapa_df$COD_ZAT) |>
  filter(ZAT_ORIGEN != ZAT_DESTINO) |>
  distinct()
Pares_OD_Mapa_df

# Creamos la Network Graph
Graph_from_df <- igraph::graph_from_data_frame(
  d = Pares_OD_Mapa_df,
  directed = TRUE,
  vertices = Vertices_OD_Mapa_df
)

Graph_tbl <- tidygraph::as_tbl_graph(
  Graph_from_df
)

# Creamos el mapa con los viajes en fucsia
Mapa_OD <- ggraph::ggraph(
  Graph_tbl,
  x = lon,
  y = lat
) +
  geom_sf(
    data = Shape_ZAT,
    fill = "grey10",
    color = "white",
    linewidth = 0.1
  ) +
  ggraph::geom_edge_bundle_path(
    color = "#FA57B1",
    width = 0.025,
    alpha = 0.8, # Mejor resultado: 0.8/0.4
  ) +
  coord_sf(crs = sf::st_crs(Shape_UPLs)) +
  theme_void()

# Muestro el mapa en R:
print(paste("Mostrando el ",Muestra*100,"% de ",length(Viajes_df$COD_VIAJE)," viajes. Número de líneas: ",length(Pares_OD_Mapa_df$ZAT_ORIGEN), sep = ""))
#Mapa_OD

# Defino nombre de archivo:
Date_and_Time = str_replace_all(str_replace_all(as.character(as_datetime(Sys.time())), ":", ""), " ", "_")
Filename <- paste("Mapa_Viajes_OD_ZAT ", Date_and_Time, ".png", sep = "")
print(paste("Guardando archivo ", Filename, "...", sep = ""))

ggsave(
  filename = Filename,
  plot = last_plot(),
  device = NULL,
  path = "./Data/3_Results/3_EM2023/",
  scale = 1,
  width = 2160,
  height = 3840,
  units = c("px"),
  dpi = 300,
  limitsize = TRUE,
#  bg = "white",
  create.dir = FALSE,
)

# Tiempo de procesamiento del mapa:
End_Time_S3_P5 <- Sys.time()
print(paste("5. Tiempo de procesamiento del mapa de Origen-Destino (ZAT): ", as.duration(End_Time_S3_P5 - Start_Time_S3_P5)))

# ####
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ::: 6. Gráfico de cuerdas Origen-Destino entre UPLs                        :::
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ####
#
Start_Time_S3_P6 <- Sys.time()

length(unique(Viajes_df$COD_PERSONA)) # Personas únicas
Censo_Municipios$PERSONAS_MPIO # Personas en la ciudad
length(unique(Viajes_df$COD_PERSONA))/Censo_Municipios$PERSONAS_MPIO # Porcentaje de la muestra

# Creamos la Matriz de viajes para el diagrama de cuerdas
Matriz_Viajes_Cuerdas <- Viajes_UPL_df |>
  group_by(UPL_ORIGEN, UPL_DESTINO) |> # Agrupo por UPL, que es lo que interesa
  summarise(
    # Extrapolo al total habitantes y paso a una magnitud de 100k
    TRAVELS = n()/(100000*length(unique(Viajes_df$COD_PERSONA))/Censo_Municipios$PERSONAS_MPIO)
  ) |>
  # Convierto a formato de matriz para hacer el Gráfico de cuerdas
  mutate(
    UPL_ORIGEN = factor(UPL_ORIGEN, levels = unique(c(UPL_ORIGEN, UPL_DESTINO))),
    UPL_DESTINO = factor(UPL_DESTINO, levels = unique(c(UPL_ORIGEN, UPL_DESTINO)))
  ) %>%
  pivot_wider(
    names_from = UPL_DESTINO,
    values_from = TRAVELS,
    values_fill = 0
  ) %>%
  column_to_rownames("UPL_ORIGEN") %>%
  as.matrix()
Matriz_Viajes_Cuerdas

# Hacemos varias gráficas para escoger la más linda:
Limite <- 1
for(i in 1:Limite) {
  # Mostramos algo para no desesperar a la gente
  print(paste("Generando diagrama ", i, " de ", Limite, ".", sep = ""))
  
  # Definimos la ruta de salida cambiante
  ruta_archivo <- paste("./Data/3_Results/3_EM2023/Grafica_Viajes_Cuerdas_Entre_UPL_", round(seconds(Sys.time())),".png", sep = "")
  
  # Crea el directorio recursivamente si no existe para evitar errores
  dir.create(dirname(ruta_archivo), recursive = TRUE, showWarnings = FALSE)
  
  # Abre el dispositivo PNG
  png(ruta_archivo, width = 1800, height = 1800, res = 300, bg = "transparent")
  
  circos.clear()
  circos.par(gap.after = 2)
  
  chordDiagram(
    Matriz_Viajes_Cuerdas,
    annotationTrack = "grid",
    preAllocateTracks = list(track.height = 0.1)
  )
  
  circos.track(
    track.index = 1, 
    panel.fun = function(x, y) {
      circos.axis(
        h = "bottom",
        major.at = seq(from = 0, to = get.cell.meta.data("xlim")[2], by = 2),
        col = "grey40",        # Sets the color of the axis line and tick marks
        #family = "Roboto",    # Change the font family
        labels.font = 2,       # Change the font style to bold
        labels.col = "grey40", # Set the font color
        labels.cex = 0.5,      # Define el tamaño de la fuente de las cuentas. Def: 0.5
        labels.niceFacing = TRUE
      )
      circos.text(
        x = mean(get.cell.meta.data("xlim")),
        y = 1,
        labels = get.cell.meta.data("sector.index"),
        facing = "inside",
        family = "Roboto ExtraBold",     # Change the font family
        col = "gray40",
  #      font = "bold",
  #      labels.font = 2,       # Change the font style to bold
        srt = 0,
        adj = c(0.5, 0.5), # Adjust justification for better alignment. Def: (0.5, 0)
        cex = 1                # Ajusta el tamaño de la fuente. Def: 1.2
      )
    }, 
    bg.border = NA
  )

  # Cerramos el dispositivo para guardar el archivo
  dev.off()

}

# Tiempo de procesamiento:
End_Time_S3_P6 <- Sys.time()
print(paste("6. Tiempo de procesamiento del gráfico de cuerdas Origen-Destino (UPL): ", as.duration(End_Time_S3_P6 - Start_Time_S3_P6)))

# ####
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ::: 7. Estandarización del df de viajes                                    :::
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ####
#
Start_Time_S3_P7 <- Sys.time()

# --- 7.1. Estandatizamos los motivos de viaje ---

Viajes_df
unique(Viajes_df$ACTIVIDAD_DESTINO)

Viajes_Estandarizado_df <- Viajes_df %>%
  mutate(
    SEXO = case_when(
      SEXO == "Hombre" ~ "Masculino",
      SEXO == "Mujer" ~ "Femenino",
      SEXO == "Masculino" ~ "Masculino",
      SEXO == "Femenino" ~ "Femenino",
      SEXO == "Intersexual" ~ "Diverso"
    )
  ) |>
  mutate(
    # Primero, limpiamos las actividades base quitando los números de las que lo tienen:
    ACTIVIDAD_LIMPIA = str_remove(ACTIVIDAD_DESTINO, "^\\d{2}\\.\\s*"),
    # Ahora, recodificamos para crear categorías finales y limpias
    CATEGORIA = case_when(
      grepl("hogar", ACTIVIDAD_LIMPIA, ignore.case = TRUE) ~ "Hogar",
      grepl("trabajar|Conduzco", ACTIVIDAD_LIMPIA, ignore.case = TRUE) ~ "Trabajo",
      grepl("Estudiar", ACTIVIDAD_LIMPIA, ignore.case = TRUE) ~ "Estudio",
      grepl("compras", ACTIVIDAD_LIMPIA, ignore.case = TRUE) ~ "Compras",
      grepl("médicos", ACTIVIDAD_LIMPIA, ignore.case = TRUE) ~ "Salud",
      grepl("trámite", ACTIVIDAD_LIMPIA, ignore.case = TRUE) ~ "Trámites",
      grepl("acompañar|llevar y/o dejar", ACTIVIDAD_LIMPIA, ignore.case = TRUE) ~ "Acompañar/Llevar",
      grepl("recreativas|deportivas", ACTIVIDAD_LIMPIA, ignore.case = TRUE) ~ "Ocio y Deporte",
      grepl("visitar a alguien", ACTIVIDAD_LIMPIA, ignore.case = TRUE) ~ "Visita Social",
      grepl("religioso", ACTIVIDAD_LIMPIA, ignore.case = TRUE) ~ "Religión",
      grepl("buscar trabajo", ACTIVIDAD_LIMPIA, ignore.case = TRUE) ~ "Búsqueda de empleo",
      grepl("NS/NR", ACTIVIDAD_LIMPIA, ignore.case = TRUE) ~ "No Sabe/No Responde",
      TRUE ~ "Otro" # Asigna "Otro" a cualquier cosa que no coincida con lo anterior
    ),
    # Ahora lo mismo para inglés, por si acaso:
    CATEGORY = case_when(
      grepl("hogar", ACTIVIDAD_LIMPIA, ignore.case = TRUE) ~ "Home",
      grepl("trabajar|Conduzco", ACTIVIDAD_LIMPIA, ignore.case = TRUE) ~ "Work",
      grepl("Estudiar", ACTIVIDAD_LIMPIA, ignore.case = TRUE) ~ "Education",
      grepl("compras", ACTIVIDAD_LIMPIA, ignore.case = TRUE) ~ "Shopping",
      grepl("médicos", ACTIVIDAD_LIMPIA, ignore.case = TRUE) ~ "Medical",
      grepl("trámite", ACTIVIDAD_LIMPIA, ignore.case = TRUE) ~ "Personal Errands",
      grepl("acompañar|llevar y/o dejar", ACTIVIDAD_LIMPIA, ignore.case = TRUE) ~ "Escort / Drop-off",
      grepl("recreativas|deportivas", ACTIVIDAD_LIMPIA, ignore.case = TRUE) ~ "Leisure & Sports",
      grepl("visitar a alguien", ACTIVIDAD_LIMPIA, ignore.case = TRUE) ~ "Social Visit",
      grepl("religioso", ACTIVIDAD_LIMPIA, ignore.case = TRUE) ~ "Religion",
      grepl("buscar trabajo", ACTIVIDAD_LIMPIA, ignore.case = TRUE) ~ "Job Seeking",
      grepl("NS/NR", ACTIVIDAD_LIMPIA, ignore.case = TRUE) ~ "Not Stated / No Response",
      TRUE ~ "Other" # Condición por defecto para el resto
    )
  ) |>
  # --- 7.2. Estandarizamos el tipo de vevículo ---
  mutate(
    # str_to_lower() convierte todo a minúsculas para facilitar la búsqueda
    VEHICULO_lower = str_to_lower(VEHICULO), 
    # Estandarizamos el tipo de vehículo de las dos encuestas
    VEHICULO_ESTANDAR = case_when(
      # Usamos str_detect() que es similar a grepl()
      str_detect(VEHICULO_lower, "auto|moto")                      ~ "Automóvil",
      str_detect(VEHICULO_lower, "bici")                      ~ "Bicicleta",
      str_detect(VEHICULO_lower, "pie")                       ~ "A Pie",
      #str_detect(VEHICULO_lower, "moto")                      ~ "Motocicleta",
      str_detect(VEHICULO_lower, "taxi")                      ~ "Taxi",
      str_detect(VEHICULO_lower, "público|sitp|brt|interm")    ~ "Transporte Público",
      str_detect(VEHICULO_lower, "especial")                  ~ "Transporte Especial",
      str_detect(VEHICULO_lower, "escolar")                   ~ "Transporte Escolar",
      str_detect(VEHICULO_lower, "informal")                  ~ "Transporte Informal",
      # Si ninguna de las reglas anteriores se cumple, se asigna "Otro"
      TRUE                                                    ~ "Otro" 
    ),
    VEHICLE_STANDARD = case_when(
      # Usamos str_detect() que es similar a grepl()
      str_detect(VEHICULO_lower, "auto|moto")                      ~ "Car",
      str_detect(VEHICULO_lower, "bici")                      ~ "Bicycle",
      str_detect(VEHICULO_lower, "pie")                       ~ "Walking",
      #str_detect(VEHICULO_lower, "moto")                      ~ "Motorcycle",
      str_detect(VEHICULO_lower, "taxi")                      ~ "Taxi",
      str_detect(VEHICULO_lower, "público|sitp|brt|interm")    ~ "Public transport",
      str_detect(VEHICULO_lower, "especial")                  ~ "Special transport",
      str_detect(VEHICULO_lower, "escolar")                   ~ "School transport",
      str_detect(VEHICULO_lower, "informal")                  ~ "Informal transport",
      # Si ninguna de las reglas anteriores se cumple, se asigna "Otro"
      TRUE                                                    ~ "Other" 
    )
  )

# --- 7.3. Rellenamos los datos faltantes Origen-Destino (LOC, UPL, UTAM, ZAT, LON y LAT) ---

names(Viajes_Estandarizado_df)

# - 7.3.1. Buscamos ZAT y UPL a partir de las Coordenadas LON y LAT -

# Primero tratamos de encontrar la LOC, UPL, UTAM y ZAT, por medio de LON y LAT:

# Convierto las coordenadas de origen y destino, a sf:
Puntos_Origen_sf <- Viajes_Estandarizado_df |>
  select(COD_VIAJE, LON_ORIGEN, LAT_ORIGEN) |>
  filter(!is.na(LON_ORIGEN) & LON_ORIGEN != "" & LON_ORIGEN != 0 & !is.na(LAT_ORIGEN) & LAT_ORIGEN != "" & LAT_ORIGEN != 0) |>
  st_as_sf(coords = c("LON_ORIGEN", "LAT_ORIGEN"), crs = st_crs(Shape_ZAT))
Puntos_Origen_sf

Puntos_Destino_sf <- Viajes_Estandarizado_df |>
  select(COD_VIAJE, LON_DESTINO, LAT_DESTINO) |>
  filter(!is.na(LON_DESTINO) & LON_DESTINO != "" & LON_DESTINO != 0 & !is.na(LAT_DESTINO) & LAT_DESTINO != "" & LAT_DESTINO != 0) |>
  st_as_sf(coords = c("LON_DESTINO", "LAT_DESTINO"), crs = st_crs(Shape_ZAT))
Puntos_Destino_sf

# Hacemos la unión espacial (point-in-polygon)
Puntos_Origen_Ubicados_df <- st_join(Puntos_Origen_sf, Shape_ZAT, join = st_intersects) |>
  # Nos quedamos con el ID y los códigos que acabamos de encontrar
  select(COD_VIAJE, LOC_ORIGEN_ENCONTRADO = COD_LOC, UPL_ORIGEN_ENCONTRADO = COD_UPL, UTAM_ORIGEN_ENCONTRADO = COD_UTAM, ZAT_ORIGEN_ENCONTRADO = COD_ZAT) |>
  st_drop_geometry() # Quitamos la geometría porque ya no la necesitamos
Puntos_Origen_Ubicados_df

Puntos_Destino_Ubicados_df <- st_join(Puntos_Destino_sf, Shape_ZAT, join = st_intersects) |>
  # Nos quedamos con el ID y los códigos que acabamos de encontrar
  select(COD_VIAJE, LOC_DESTINO_ENCONTRADO = COD_LOC, UPL_DESTINO_ENCONTRADO = COD_UPL, UTAM_DESTINO_ENCONTRADO = COD_UTAM, ZAT_DESTINO_ENCONTRADO = COD_ZAT) |>
  st_drop_geometry() # Quitamos la geometría porque ya no la necesitamos
Puntos_Destino_Ubicados_df

# - 7.3.2. Buscar LOC, UPL, UTAM, LON y LAT, a partid del ZAT -

# LOC, UPL y UTAM se sacan por tabla, pero LON y LAT, toca por Centroides:
Info_por_ZAT_Origen_df <- Shape_ZAT |>
  mutate(
    CENTROIDE = st_centroid(geom),
    LON_ZAT_ORIGEN = st_coordinates(CENTROIDE)[,1],
    LAT_ZAT_ORIGEN = st_coordinates(CENTROIDE)[,2],
    LOC_ORIGEN = COD_LOC,
    UPL_ORIGEN = COD_UPL,
    UTAM_ORIGEN = COD_UTAM,
    ZAT_ORIGEN = COD_ZAT
  ) |>
  st_drop_geometry() |>
  select(LOC_ORIGEN, UPL_ORIGEN, UTAM_ORIGEN, ZAT_ORIGEN, LON_ZAT_ORIGEN, LAT_ZAT_ORIGEN)
Info_por_ZAT_Origen_df

Info_por_ZAT_Destino_df <- Shape_ZAT |>
  mutate(
    CENTROIDE = st_centroid(geom),
    LON_ZAT_DESTINO = st_coordinates(CENTROIDE)[,1],
    LAT_ZAT_DESTINO = st_coordinates(CENTROIDE)[,2],
    LOC_DESTINO = COD_LOC,
    UPL_DESTINO = COD_UPL,
    UTAM_DESTINO = COD_UTAM,
    ZAT_DESTINO = COD_ZAT
  ) |>
  st_drop_geometry() |>
  select(LOC_DESTINO, UPL_DESTINO, UTAM_DESTINO, ZAT_DESTINO, LON_ZAT_DESTINO, LAT_ZAT_DESTINO)
Info_por_ZAT_Destino_df

# - 7.3.3. Buscar UPL, cuando no hay más que UPL -

# Aquí solo extraemos la UPL, y las coordenadas LON y LAT del centroide:
Info_por_UPL_Origen_df <- Shape_ZAT |>
  group_by(COD_UPL) |>
  summarise(geom = st_union(geom)) |> # Unimos las geometrías
  mutate(
    CENTROIDE = st_centroid(geom),
    LON_UPL_ORIGEN = st_coordinates(CENTROIDE)[,1],
    LAT_UPL_ORIGEN = st_coordinates(CENTROIDE)[,2],
    UPL_ORIGEN = COD_UPL,
  ) |>
  st_drop_geometry() |>
  select(UPL_ORIGEN, LON_UPL_ORIGEN, LAT_UPL_ORIGEN)
Info_por_UPL_Origen_df

Info_por_UPL_Destino_df <- Shape_ZAT |>
  group_by(COD_UPL) |>
  summarise(geom = st_union(geom)) |> # Unimos las geometrías
  mutate(
    CENTROIDE = st_centroid(geom),
    LON_UPL_DESTINO = st_coordinates(CENTROIDE)[,1],
    LAT_UPL_DESTINO = st_coordinates(CENTROIDE)[,2],
    UPL_DESTINO = COD_UPL,
  ) |>
  st_drop_geometry() |>
  select(UPL_DESTINO, LON_UPL_DESTINO, LAT_UPL_DESTINO)
Info_por_UPL_Destino_df

# - 7.3.4. Completamos la tabla con los datos anteriores -

Viajes_Estandarizado_df <- Viajes_Estandarizado_df |>
  # Unimos los resultados del Caso 1 (puntos ubicados)
  left_join(Puntos_Origen_Ubicados_df, by = "COD_VIAJE") |>
  left_join(Puntos_Destino_Ubicados_df, by = "COD_VIAJE") |>
  # Unimos las tablas de referencia
  left_join(Info_por_ZAT_Origen_df, by = "ZAT_ORIGEN", suffix = c("", "_ref_zat")) |>
  left_join(Info_por_ZAT_Destino_df, by = "ZAT_DESTINO", suffix = c("", "_ref_zat")) |>
  left_join(Info_por_UPL_Origen_df, by = "UPL_ORIGEN", suffix = c("", "_ref_upl")) |>
  left_join(Info_por_UPL_Destino_df, by = "UPL_DESTINO", suffix = c("", "_ref_upl")) |>
  # Usamos coalesce() para rellenar los NA de forma ordenada
  mutate(
    # Para columnas de texto: reemplaza "" con NA
    across(where(is.character), ~na_if(., "")),
    # Para columnas numéricas: reemplaza 0 con NA
    across(where(is.numeric), ~na_if(., 0)),
    LOC_ORIGEN = coalesce(LOC_ORIGEN, LOC_ORIGEN_ENCONTRADO, LOC_ORIGEN_ref_zat),
    LOC_DESTINO = coalesce(LOC_DESTINO, LOC_DESTINO_ENCONTRADO, LOC_DESTINO_ref_zat),
    UPL_ORIGEN = coalesce(UPL_ORIGEN, UPL_ORIGEN_ENCONTRADO, UPL_ORIGEN_ref_zat),
    UPL_DESTINO = coalesce(UPL_DESTINO, UPL_DESTINO_ENCONTRADO, UPL_DESTINO_ref_zat),
    UTAM_ORIGEN = coalesce(UTAM_ORIGEN, UTAM_ORIGEN_ENCONTRADO, UTAM_ORIGEN_ref_zat),
    UTAM_DESTINO = coalesce(UTAM_DESTINO, UTAM_DESTINO_ENCONTRADO, UTAM_DESTINO_ref_zat),
    ZAT_ORIGEN = coalesce(ZAT_ORIGEN, ZAT_ORIGEN_ENCONTRADO),
    ZAT_DESTINO = coalesce(ZAT_DESTINO, ZAT_DESTINO_ENCONTRADO),
    LON_ORIGEN = coalesce(LON_ORIGEN, LON_ZAT_ORIGEN, LON_UPL_ORIGEN),
    LAT_ORIGEN = coalesce(LAT_ORIGEN, LAT_ZAT_ORIGEN, LAT_UPL_ORIGEN),
    LON_DESTINO = coalesce(LON_DESTINO, LON_ZAT_DESTINO, LON_UPL_DESTINO),
    LAT_DESTINO = coalesce(LAT_DESTINO, LAT_ZAT_DESTINO, LAT_UPL_DESTINO),
    UTAM_ORIGEN = str_replace_all(UTAM_ORIGEN, "M0", "M"),
    UTAM_DESTINO = str_replace_all(UTAM_DESTINO, "M0", "M")
  ) |>
  # Seleccionamos y renombramos las columnas finales
  select(DATA_ORIGEN:VEHICLE_STANDARD)
Viajes_Estandarizado_df

# Tiempo de Acceso al TP
Viajes_Estandarizado_df |>
  filter(VEHICULO_ESTANDAR == "Transporte Público") |>
  select(VEHICULO_ESTANDAR, TIEMPO_ACCESO) |>
  group_by(VEHICULO_ESTANDAR) |>
  summarise(
    PROMEDIO = mean(TIEMPO_ACCESO, na.rm = TRUE)
  )
# Tiempo de acceso promedio al transporte público: 9,59 minutos

Viajes_Estandarizado_df |>
#  filter(VEHICULO_ESTANDAR == "Transporte Público") |>
  filter(CATEGORIA != "Home") |>
  select(VEHICULO_ESTANDAR, HORA_INICIO_VIAJE) |>
  group_by(VEHICULO_ESTANDAR) |>
  summarise(
    PROMEDIO = mean(HORA_INICIO_VIAJE, na.rm = TRUE)
  )
# Tiempo de acceso promedio al transporte público: 9,59 minutos

# save(
#   Viajes_Estandarizado_df,
#   file = "./Data/2_Processing/3_EM2023/Viajes_Estandarizado_df.RData"
# )
load("./Data/2_Processing/3_EM2023/Viajes_Estandarizado_df.RData")


# Tiempo de procesamiento:
End_Time_S3_P7 <- Sys.time()
print(paste("7. Tiempo de estandarización del df de viajes: ", as.duration(End_Time_S3_P7 - Start_Time_S3_P7)))

# ####
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ::: 8. Establecer colores para los principales motivos de viaje            :::
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ####
#
# Aquí se establecen los colores de los principales motivos de viaje. Sin embargo,
# en este punto del script no se conocen los principales motivos, sino hasta la
# siguiente sección.
#
Start_Time_S3_P8 <- Sys.time()

Color_Trabajo <- "darkred"
Color_Estudio <- "deepskyblue4"
Color_Tramites <- "darkgreen"
Color_Otros <- "darkgray"

# Tiempo de procesamiento:
End_Time_S3_P8 <- Sys.time()
print(paste("8. Tiempo de establecimiento del color: ", as.duration(End_Time_S3_P8 - Start_Time_S3_P8)))

# ####
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ::: 9. Principales motivos de viaje de la encuesta                         :::
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ####
#
Start_Time_S3_P9 <- Sys.time()

Viajes_df
unique(Viajes_df$ACTIVIDAD_DESTINO)

# Destaco los tipos de categoría que más viajes tienen:
Porcentajes_Motivo_Viaje_df <- Viajes_Estandarizado_df |>
  group_by(CATEGORIA, CATEGORY) |>
  summarise(
    VIAJES = n()
  ) |>
  arrange(desc(VIAJES)) |>
  ungroup() |>
  # Aquí es donde destaco:
  mutate(
    PORCENTAJE = round(VIAJES/length(Viajes_Estandarizado_df$COD_VIAJE)*100, digits = 2),
    #DESTACADO = ifelse(CATEGORIA == "Hogar", "Hogar", ifelse(row_number() <= 4, "Principales", "Otros")),
    DESTACADO = case_when(
      CATEGORIA == "Trabajo" ~ "Trabajo",
      CATEGORIA == "Estudio" ~ "Estudio",
      CATEGORIA == "Trámites" ~ "Trámites",
      TRUE ~ "Otro"
    ),
  )
Porcentajes_Motivo_Viaje_df

# Reordenar  otra vez el factor para que las barras vayan de menor a mayor
Porcentajes_Motivo_Viaje_df$CATEGORIA <- factor(Porcentajes_Motivo_Viaje_df$CATEGORIA, levels = Porcentajes_Motivo_Viaje_df$CATEGORIA[order(Porcentajes_Motivo_Viaje_df$PORCENTAJE)])
Porcentajes_Motivo_Viaje_df

Plot_Porcentaje_Viajes_EM2023 <- Porcentajes_Motivo_Viaje_df |>
  ggplot() +
  # Barras horizontales
  geom_col(aes(x = CATEGORIA, y = PORCENTAJE, fill = DESTACADO), width = 0.9) +
  # Solo vamos a hacer las barras porque quedó fea la gráfica con los textos en R.
  # Voltear ejes para barras horizontales
  coord_flip() +
  # Asignar colores: verde a Bogotá, gris al resto
  scale_fill_manual(values = c(
    Trabajo = Color_Trabajo,
    Estudio = Color_Estudio,
    Trámites = Color_Tramites,
    Otro = Color_Otros
  )) +
  # Eliminar ejes, cuadrículas y leyenda
  theme_void() +
  theme(legend.position = "none") +
  # Añadir espacio a la derecha para que no se corte el texto
  scale_y_continuous(limits = c(0, max(Porcentajes_Motivo_Viaje_df$PORCENTAJE) * 1.1), expand = c(0,0))
Plot_Porcentaje_Viajes_EM2023

ggsave(
  filename = "Grafica_Principales_Motivos_Viaje_EM2023.png",
  plot = Plot_Porcentaje_Viajes_EM2023,
  device = NULL,
  path = "./Data/3_Results/3_EM2023/",
  scale = 1,
  width = 1500,
  height = 1000,
  units = c("px"),
  dpi = 150,
  limitsize = TRUE,
  bg = NULL,
  create.dir = FALSE,
)

# Para sacar los datos y pegarlos en Word:
Porcentajes_Motivo_Viaje_df |>
  mutate(PORCENTAJE = paste(PORCENTAJE, "%", sep = " ")) |>
  select(CATEGORIA, CATEGORY, PORCENTAJE)

# Tiempo de procesamiento:
End_Time_S3_P9 <- Sys.time()
print(paste("9. Tiempo de procesar principales motivos de viaje: ", as.duration(End_Time_S3_P9 - Start_Time_S3_P9)))

# ####
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ::: 10. Distribución modal y Gráfico de barras polares                     :::
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ####
#
Start_Time_S3_P10 <- Sys.time()

Viajes_Estandarizado_df
Muestra_EM2023 <- length(unique(Viajes_Estandarizado_df$COD_PERSONA))
Poblacion_Ciudad <- Censo_Municipios$PERSONAS_MPIO
Factor_De_Expansion <- 1/(Muestra_EM2023/Poblacion_Ciudad)

# Motos fusionado con autos:
Distribucion_Modal_df <- Viajes_Estandarizado_df |>
  select(VEHICULO_ESTANDAR, VEHICLE_STANDARD) |>
  mutate(
    VEHICULO_ESTANDAR = ifelse(VEHICULO_ESTANDAR == "Automóvil", "Vehículo privado", VEHICULO_ESTANDAR),
    VEHICLE_STANDARD = ifelse(VEHICLE_STANDARD == "Car", "Private vehicle", VEHICLE_STANDARD)
  ) |>
  group_by(VEHICULO_ESTANDAR, VEHICLE_STANDARD) |>
  summarise(
    VIAJES = n(),
    PORCENTAJE = paste(round(100*n()/length(Viajes_Estandarizado_df$COD_VIAJE), digits = 2), " %", sep = "")
  ) |>
  arrange(desc(VIAJES))
Distribucion_Modal_df

# Ploteamos en barras polares:
Plot_Distribucion_Modal <- Distribucion_Modal_df |>
  ggplot() +
  # Make custom panel grid
  geom_hline(
    aes(yintercept = y), 
    data.frame(y = c(0:4) * 25000),
    color = "lightgrey"
  ) + 
  # Add bars to represent the cumulative track lengths
  # str_wrap(region, 5) wraps the text so each line has at most 5 characters
  # (but it doesn't break long words!)
  geom_col(
    aes(
      x = reorder(str_wrap(VEHICULO_ESTANDAR, 5), VIAJES),
      y = VIAJES,
      fill = VIAJES
    ),
    position = "dodge2",
    show.legend = TRUE,
    alpha = .9
  ) +
  
  # Lollipop shaft for mean gain per region
  geom_segment(
    aes(
      x = reorder(str_wrap(VEHICULO_ESTANDAR, 5), VIAJES),
      y = 0,
      xend = reorder(str_wrap(VEHICULO_ESTANDAR, 5), VIAJES),
      yend = 100000
    ),
    linetype = "dashed",
    color = "gray"
  ) + 
  
  # Make it circular!
  coord_polar() +
  
  # Scale y axis so bars don't start in the center
  scale_y_continuous(
    limits = c(-50000, 100000),
    expand = c(0, 0),
    breaks = c(0, 25000, 50000, 75000, 100000)
  ) + 
  
  # New fill and legend title for number of tracks per region
  scale_fill_gradientn(
    "Amount of Tracks",
    colours = c( "#6C5B7B","#C06C84","#F67280","#F8B195")
  ) +
  
  # Make the guide for the fill discrete
  guides(
    fill = guide_colorsteps(
      barwidth = 15, barheight = .5, title.position = "top", title.hjust = .5
    )
  ) +
  theme(
    # Remove axis ticks and text
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    axis.text.y = element_blank(),
    # Use gray text for the region names
    axis.text.x = element_text(color = "grey40", size = 12, family = "Roboto Black"),
    #axis.text.x = element_blank(),
    # Move the legend to the bottom
    legend.position = "none",
  ) +
  theme(
    
    # Set default color and font family for the text
    text = element_text(color = "grey40", family = "Roboto Bold"),
    
    # Remove extra grid lines
    plot.background = element_rect(fill = "transparent", colour = NA),
    panel.background = element_rect(fill = "transparent", colour = NA),
    panel.grid = element_blank(),
    panel.grid.major.x = element_blank()
  )
Plot_Distribucion_Modal

# Guardamos diagrama
ggsave(
  filename = "Grafica_Distribucion_Modal_EM2023.png",
  plot = Plot_Distribucion_Modal,
  device = NULL,
  path = "./Data/3_Results/3_EM2023/",
  scale = 1,
  width = 1400,
  height = 1400,
  units = c("px"),
  dpi = 150,
  limitsize = TRUE,
  bg = NULL,
  create.dir = FALSE,
)

# Para poner los valores manualmente en Word:
Distribucion_Modal_df |>
  mutate(
    VIAJES_REALES = round(VIAJES * Factor_De_Expansion, digits = 0)
  )

# Tiempo de procesamiento:
End_Time_S3_P10 <- Sys.time()
print(paste("10. Tiempo de procesamiento de la distribución modal: ", as.duration(End_Time_S3_P10 - Start_Time_S3_P10)))

# ####
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ::: 11. Distribución de modos sustentables y no sustentables               :::
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ####
#
Start_Time_S3_P11 <- Sys.time()

Muestra_EM2023 <- length(unique(Viajes_Estandarizado_df$COD_PERSONA))
Poblacion_Ciudad <- Censo_Municipios$PERSONAS_MPIO
Factor_De_Expansion <- 1/(Muestra_EM2023/Poblacion_Ciudad)

# Modos sustentables:
Medios_Sustentables <- c("Transporte Público", "Bicicleta", "A Pie", "Transporte Escolar", "Transporte Informal")

# Crear el nuevo data frame de Distribución Sustentable
Distribucion_Sustentable <- Distribucion_Modal_df %>%
  mutate(TIPO_TRANSPORTE = ifelse(VEHICULO_ESTANDAR %in% Medios_Sustentables, "Sustentable", "No Sustentable")) %>%
  group_by(TIPO_TRANSPORTE) %>%
  summarise(TOTAL_VIAJES = sum(VIAJES)) |>
  mutate(
    PORCENTAJE = paste(round(100*TOTAL_VIAJES/length(Viajes_Estandarizado_df$COD_VIAJE), digits = 2), " %", sep = ""),
    COLOR = ifelse(TIPO_TRANSPORTE == "Sustentable", "darkgreen", "darkred")
    ) |>
  arrange(desc(TOTAL_VIAJES))
Distribucion_Sustentable

# Visualizar el nuevo data frame resumido
# Compute percentages
Distribucion_Sustentable$fraction <- Distribucion_Sustentable$TOTAL_VIAJES / sum(Distribucion_Sustentable$TOTAL_VIAJES)
Distribucion_Sustentable

# Compute the cumulative percentages (top of each rectangle)
Distribucion_Sustentable$ymax <- cumsum(Distribucion_Sustentable$fraction)

# Compute the bottom of each rectangle
Distribucion_Sustentable$ymin <- c(0, head(Distribucion_Sustentable$ymax, n=-1))

# Compute label position
Distribucion_Sustentable$labelPosition <- (Distribucion_Sustentable$ymax + Distribucion_Sustentable$ymin) / 2
Distribucion_Sustentable

# Compute a good label
Distribucion_Sustentable$label <- paste(Distribucion_Sustentable$TIPO_TRANSPORTE, "\n", Distribucion_Sustentable$PORCENTAJE)
Distribucion_Sustentable

# Ploteamos con colores manuales
Plot_Sustentable <- ggplot(Distribucion_Sustentable, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=TIPO_TRANSPORTE)) +
  geom_rect() +
  
  # Corrección: 'color' debe ir dentro de aes() para mapear a la variable
  #geom_text(aes(x=4, y=labelPosition, label=label, color=TIPO_TRANSPORTE), size=6, family = "Roboto Black") +
  
  # 1. Controlar el color de relleno de las áreas (Sustentable / No Sustentable)
  scale_fill_manual(values = c("Sustentable" = "#99cc99", "No Sustentable" = "#993366")) +
  
  # 2. Controlar el color del texto
  scale_color_manual(values = c("Sustentable" = "gray40", "No Sustentable" = "gray40")) +
  
  coord_polar(theta="y") +
  xlim(c(1, 5)) +
  theme_void() +
  theme(
    legend.position = "none",
    # Set default color and font family for the text
    text = element_text(color = "grey40", family = "Roboto Bold"),
    
    # Remove extra grid lines
    plot.background = element_rect(fill = "transparent", colour = NA),
    panel.background = element_rect(fill = "transparent", colour = NA),
    panel.grid = element_blank(),
    panel.grid.major.x = element_blank()
  )
Plot_Sustentable

# Guardamos como imagen:
ggsave(
  filename = "Grafica_Distribucion_Sustentable_EM2023.png",
  plot = Plot_Sustentable,
  device = NULL,
  path = "./Data/3_Results/3_EM2023/",
  scale = 1,
  width = 1400,
  height = 1400,
  units = c("px"),
  dpi = 150,
  limitsize = TRUE,
  bg = NULL,
  create.dir = FALSE,
)

# Datos para poner manualmente en Word:
Distribucion_Sustentable |>
  mutate(
    VIAJES_REALES = round(TOTAL_VIAJES * Factor_De_Expansion, digits = 0)
  )
  

# Tiempo de procesamiento:
End_Time_S3_P11 <- Sys.time()
print(paste("11. Tiempo de procesamiento de la distribución sustentable: ", as.duration(End_Time_S3_P11 - Start_Time_S3_P11)))

# ####
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ::: 12. Tiempos medios de las tres actividades principales                 :::
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ####
#
Start_Time_S3_P12 <- Sys.time()

names(Viajes_Estandarizado_df)
unique(Viajes_Estandarizado_df$VEHICULO)

# Tiempos de viaje de toda la ciudad, en las 3 principales categorías:
Tiempos_Principales_Motivos_Ciudad_df <- Viajes_Estandarizado_df |>
  select(DATA_ORIGEN, UPL_ORIGEN, TIEMPO_VIAJE, CATEGORIA) |>
  filter(DATA_ORIGEN == "EH") |>
  filter(UPL_ORIGEN != "" & !is.na(TIEMPO_VIAJE) & TIEMPO_VIAJE > 0 & CATEGORIA %in% c("Trabajo", "Estudio", "Trámites")) |>
  group_by(CATEGORIA) |>
  summarise(
    AVRG_TIME = mean(TIEMPO_VIAJE)
  ) |>
  ungroup() |>
  arrange(desc(AVRG_TIME)) |>
  mutate(COLOR = case_when(
    CATEGORIA == "Trabajo" ~ Color_Trabajo,
    CATEGORIA == "Estudio" ~ Color_Estudio,
    CATEGORIA == "Trámites" ~ Color_Tramites),
    CATEGORIA = paste(row_number(), ". ", CATEGORIA, sep = "")
  )
Tiempos_Principales_Motivos_Ciudad_df

# Reordenar  otra vez el factor para que las barras vayan de menor a mayor
Tiempos_Principales_Motivos_Ciudad_df$CATEGORIA <- factor(Tiempos_Principales_Motivos_Ciudad_df$CATEGORIA, levels = Tiempos_Principales_Motivos_Ciudad_df$CATEGORIA[order(Tiempos_Principales_Motivos_Ciudad_df$AVRG_TIME)])
Tiempos_Principales_Motivos_Ciudad_df

# Graficamos tiempos promedio de viaje de las principales actividades para toda la ciudad:
Plot_Tiempos_Principales_Motivos_Ciudad <- Tiempos_Principales_Motivos_Ciudad_df |>
  ggplot() +
  # Barras horizontales
  geom_col(aes(x = CATEGORIA, y = AVRG_TIME, fill = CATEGORIA), width = 0.9) +
  # Nombre de la categoría dentro de la barra, al inicio
  geom_text(aes(x = CATEGORIA, y = 0, label = CATEGORIA ),
            hjust = 0,             # muy cerca del borde izquierdo
            nudge_y = max(Tiempos_Principales_Motivos_Ciudad_df$AVRG_TIME) * 0.02,
            color = "white", size = 6, family = "Roboto ExtraBold", fontface = "bold") +
  # Valor del tiempo dentro de la barra, al final
  geom_text(aes(x = CATEGORIA, y = AVRG_TIME, label = paste(round(AVRG_TIME, digits = 2), " min", sep = "")),
            hjust = 1.2,             # muy cerca del borde derecho (1.2 es el mejor)
            color = "white", size = 6, family = "Roboto") +
  # Voltear ejes para barras horizontales
  coord_flip() +
  # Asignar colores:
  scale_fill_manual(values = c(
    "1. Trabajo" = Color_Trabajo,
    "2. Trámites" = Color_Tramites,
    "3. Estudio" = Color_Estudio)
    ) +
  # Eliminar ejes, cuadrículas y leyenda
  theme_void() +
  theme(legend.position = "none") +
  # Añadir espacio a la derecha para que no se corte el texto
  scale_y_continuous(limits = c(0, max(Tiempos_Principales_Motivos_Ciudad_df$AVRG_TIME) * 1.1), expand = c(0,0))
Plot_Tiempos_Principales_Motivos_Ciudad

# Guardamos como imagen:
ggsave(
  filename = "Grafica_Tiempos_Principales_Motivos_EM2023.png",
  plot = Plot_Tiempos_Principales_Motivos_Ciudad,
  device = NULL,
  path = "./Data/3_Results/3_EM2023/",
  scale = 1,
  width = 1500,
  height = 250,
  units = c("px"),
  dpi = 150,
  limitsize = TRUE,
  bg = NULL,
  create.dir = FALSE,
)

# Datos para poner manualmente en Word:
Tiempos_Principales_Motivos_Ciudad_df

# Tiempo de procesamiento:
End_Time_S3_P12 <- Sys.time()
print(paste("12. Tiempo de procesamiento de tiempos de principales motivos de viaje: ", as.duration(End_Time_S3_P12 - Start_Time_S3_P12)))

# ####
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ::: 13. Tiempos medios de las 3 actividades principales | Solo Universidad :::
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ####
#
Start_Time_S3_P13 <- Sys.time()

Viajes_Estandarizado_df

# Tiempos de viaje de toda la ciudad, en las 3 principales categorías, pero en estudio, solo Universitarios:
Tiempos_Principales_Motivos_Uni_Ciudad_df <- Viajes_Estandarizado_df |>
  select(DATA_ORIGEN, UPL_ORIGEN, TIEMPO_VIAJE, CATEGORIA) |>
  filter(DATA_ORIGEN == "EH") |>
  filter(UPL_ORIGEN != "" & !is.na(TIEMPO_VIAJE) & TIEMPO_VIAJE > 0 & CATEGORIA %in% c("Trabajo", "Trámites")) |>
  group_by(CATEGORIA) |>
  summarise(
    AVRG_TIME = mean(TIEMPO_VIAJE)
  ) |>
  bind_rows(
    Viajes_Estandarizado_df |>
      select(DATA_ORIGEN, UPL_ORIGEN, TIEMPO_VIAJE, CATEGORIA, OCUPACION) |>
      filter(DATA_ORIGEN == "EH") |>
      filter(UPL_ORIGEN != "" & !is.na(TIEMPO_VIAJE) & TIEMPO_VIAJE > 0 & CATEGORIA == "Estudio" & OCUPACION %in% c("Universidad – Pregrado", "Universidad – Posgrado", "Inst Técnico / Tecnológico")) |>
      group_by(CATEGORIA) |>
      summarise(
        AVRG_TIME = mean(TIEMPO_VIAJE)
      )
  ) |>
  ungroup() |>
  arrange(desc(AVRG_TIME)) |>
  mutate(COLOR = case_when(
    CATEGORIA == "Trabajo" ~ Color_Trabajo,
    CATEGORIA == "Estudio" ~ Color_Estudio,
    CATEGORIA == "Trámites" ~ Color_Tramites),
    CATEGORIA = ifelse(CATEGORIA == "Estudio", "Estudio (Educación Superior)", CATEGORIA),
    CATEGORIA = paste(row_number(), ". ", CATEGORIA, sep = "")
  )
Tiempos_Principales_Motivos_Uni_Ciudad_df

# Reordenar  otra vez el factor para que las barras vayan de menor a mayor
Tiempos_Principales_Motivos_Uni_Ciudad_df$CATEGORIA <- factor(Tiempos_Principales_Motivos_Uni_Ciudad_df$CATEGORIA, levels = Tiempos_Principales_Motivos_Uni_Ciudad_df$CATEGORIA[order(Tiempos_Principales_Motivos_Uni_Ciudad_df$AVRG_TIME)])
Tiempos_Principales_Motivos_Uni_Ciudad_df

# Graficamos tiempos promedio de viaje de las principales actividades (Educación superior) para toda la ciudad:
Plot_Tiempos_Principales_Motivos_Uni_Ciudad <- Tiempos_Principales_Motivos_Uni_Ciudad_df |>
  ggplot() +
  # Barras horizontales
  geom_col(aes(x = CATEGORIA, y = AVRG_TIME, fill = CATEGORIA), width = 0.9) +
  # Nombre de la categoría dentro de la barra, al inicio
  geom_text(aes(x = CATEGORIA, y = 0, label = CATEGORIA ),
            hjust = 0,             # muy cerca del borde izquierdo
            nudge_y = max(Tiempos_Principales_Motivos_Uni_Ciudad_df$AVRG_TIME) * 0.02,
            color = "white", size = 6, family = "Roboto ExtraBold", fontface = "bold") +
  # Valor del tiempo dentro de la barra, al final
  geom_text(aes(x = CATEGORIA, y = AVRG_TIME, label = paste(round(AVRG_TIME, digits = 2), " min", sep = "")),
            hjust = 1.2,             # muy cerca del borde derecho (1.2 es el mejor)
            color = "white", size = 6, family = "Roboto") +
  # Voltear ejes para barras horizontales
  coord_flip() +
  # Asignar colores:
  scale_fill_manual(values = c(
    "1. Estudio (Educación Superior)" = Color_Estudio,
    "2. Trabajo" = Color_Trabajo,
    "3. Trámites" = Color_Tramites)
  ) +
  # Eliminar ejes, cuadrículas y leyenda
  theme_void() +
  theme(legend.position = "none") +
  # Añadir espacio a la derecha para que no se corte el texto
  scale_y_continuous(limits = c(0, max(Tiempos_Principales_Motivos_Uni_Ciudad_df$AVRG_TIME) * 1.1), expand = c(0,0))
Plot_Tiempos_Principales_Motivos_Uni_Ciudad

# Guardamos como imagen:
ggsave(
  filename = "Graficas_Tiempos_Principales_Motivos_Uni_EM2023.png",
  plot = Plot_Tiempos_Principales_Motivos_Uni_Ciudad,
  device = NULL,
  path = "./Data/3_Results/3_EM2023/",
  scale = 1,
  width = 1500,
  height = 250,
  units = c("px"),
  dpi = 150,
  limitsize = TRUE,
  bg = NULL,
  create.dir = FALSE,
)

# Datos para poner manualmente en Word:
Tiempos_Principales_Motivos_Uni_Ciudad_df

# Tiempo de procesamiento:
End_Time_S3_P13 <- Sys.time()
print(paste("13. Tiempo de procesamiento de tiempos de principales motivos de viaje (Universidad): ", as.duration(End_Time_S3_P13 - Start_Time_S3_P13)))

# ####
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ::: 14. Tiempos de viaje por UPL y por Categoría                           :::
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ####
#
Start_Time_S3_P14 <- Sys.time()

# En esta sección, primero se hacen las gráficas de barras, y después el mapa.

# Usamos esta variable para filtrar y hacer la gráfica y el mapa en función de esto.
Motivo_Viaje <- "Trabajo"

# Tiempos de viaje de por UPL y por categoría:
Tiempos_Principales_Motivos_Uni_UPL_sf <- Viajes_Estandarizado_df |>
  select(DATA_ORIGEN, UPL_ORIGEN, TIEMPO_VIAJE, CATEGORIA) |>
  filter(DATA_ORIGEN == "EH") |>
  filter(UPL_ORIGEN != "" & !is.na(TIEMPO_VIAJE) & TIEMPO_VIAJE > 0 & CATEGORIA %in% c("Trabajo", "Trámites")) |>
  mutate(COD_UPL = UPL_ORIGEN) |>
  group_by(COD_UPL, CATEGORIA) |>
  summarise(
    AVRG_TIME = mean(TIEMPO_VIAJE)
  ) |>
  bind_rows(
    Viajes_Estandarizado_df |>
      select(DATA_ORIGEN, UPL_ORIGEN, TIEMPO_VIAJE, CATEGORIA, OCUPACION) |>
      filter(DATA_ORIGEN == "EH") |>
      filter(UPL_ORIGEN != "" & !is.na(TIEMPO_VIAJE) & TIEMPO_VIAJE > 0 & CATEGORIA == "Estudio" & OCUPACION %in% c("Universidad – Pregrado", "Universidad – Posgrado", "Inst Técnico / Tecnológico")) |>
      mutate(COD_UPL = UPL_ORIGEN) |>
      group_by(COD_UPL, CATEGORIA) |>
      summarise(
        AVRG_TIME = mean(TIEMPO_VIAJE)
      )
  ) |>
  filter(CATEGORIA == Motivo_Viaje) |> # <-- Aquí cambio entre "Estudio", "Trabajo", o "Trámites"
  left_join(Shape_UPLs) |>
  filter(ZONA_UPL != "Rural") |>
  arrange(desc(AVRG_TIME)) |>
  st_as_sf()
Tiempos_Principales_Motivos_Uni_UPL_sf

# Reordenar  otra vez el factor para que las barras vayan de menor a mayor
Tiempos_Principales_Motivos_Uni_UPL_sf$COD_UPL <- factor(Tiempos_Principales_Motivos_Uni_UPL_sf$COD_UPL, levels = Tiempos_Principales_Motivos_Uni_UPL_sf$COD_UPL[order(Tiempos_Principales_Motivos_Uni_UPL_sf$AVRG_TIME)])
Tiempos_Principales_Motivos_Uni_UPL_sf

# Gráfica de barras para el motivo seleccionado
Plot_Tiempos_Principales_Motivos_Uni_UPL <- Tiempos_Principales_Motivos_Uni_UPL_sf |>
  ggplot() +
  # Barras horizontales
  geom_col(aes(x = COD_UPL, y = AVRG_TIME, fill = AVRG_TIME), width = 0.9) +
  # Nombre de la UPL dentro de la barra, al inicio
  geom_text(aes(x = COD_UPL, y = 0, label = paste("UPL ", COD_UPL, sep = "")),
            hjust = 0,             # muy cerca del borde izquierdo
            nudge_y = max(Tiempos_Principales_Motivos_Uni_UPL_sf$AVRG_TIME) * 0.02,
            color = "white", size = 4, family = "Roboto", fontface = "bold") +
  # Valor de horas dentro de la barra, al final
  geom_text(aes(x = COD_UPL, y = AVRG_TIME, label = paste(round(AVRG_TIME, digits = 2), " min", sep = "")),
            hjust = 1.2,             # muy cerca del borde derecho (1.2 es el mejor)
            color = "white", size = 4, family = "Roboto") +
  # Voltear ejes para barras horizontales
  coord_flip() +
  # Asignar colores:
  scale_fill_gradient(
    low = "bisque3",
    high =  case_when(
      Motivo_Viaje == "Trabajo" ~ Color_Trabajo,
      Motivo_Viaje == "Estudio" ~ Color_Estudio,
      Motivo_Viaje == "Trámites" ~ Color_Tramites)
  ) +
  # Eliminar ejes, cuadrículas y leyenda
  theme_void() +
  theme(legend.position = "none") +
  # Añadir espacio a la derecha para que no se corte el texto
  scale_y_continuous(limits = c(0, max(Tiempos_Principales_Motivos_Uni_UPL_sf$AVRG_TIME) * 1.1), expand = c(0,0))
Plot_Tiempos_Principales_Motivos_Uni_UPL

# Guardamos como imagen el gráfico de barras:
ggsave(
  filename = paste("Grafica_Tiempos_Por_UPL_", Motivo_Viaje, "_EM2023.png", sep = ""),
  plot = Plot_Tiempos_Principales_Motivos_Uni_UPL,
  device = NULL,
  path = "./Data/3_Results/3_EM2023/",
  scale = 1,
  width = 1500,
  height = 1500,
  units = c("px"),
  dpi = 150,
  limitsize = TRUE,
  bg = NULL,
  create.dir = FALSE,
)

# Para plotear el mapa con etiquetas, se den sacar los centroides y definir el contenido de la etiqueta:
Shape_Plotear_Mapa <- Tiempos_Principales_Motivos_Uni_UPL_sf |>
  mutate(
    # Extrae las coordenadas del centroide de cada área
    centroides = st_centroid(geom),
    X = st_coordinates(centroides)[,1],
    Y = st_coordinates(centroides)[,2],
    # Define la columna que se usará como etiqueta
    Etiqueta_Texto = COD_UPL 
  )

Plot_Temporal <- Shape_Plotear_Mapa |>
  ggplot() +
  geom_sf(
    col = "white",
    linewidth = 0.6,
    aes(fill = AVRG_TIME)
  ) + 
  # Asignar colores:
  scale_fill_gradient(
    low = "bisque3",
    high =  case_when(
      Motivo_Viaje == "Trabajo" ~ Color_Trabajo,
      Motivo_Viaje == "Estudio" ~ Color_Estudio,
      Motivo_Viaje == "Trámites" ~ Color_Tramites)
  ) +
  # Capa de etiquetas:
  geom_text(
    aes(x = X, y = Y, label = Etiqueta_Texto), # Mapea coordenadas y texto
    color = "white",        # Color del texto
    size = 6,               # Tamaño del texto
    family = "Roboto Black" # Tipo de fuente
  ) +
  theme_void() +
  theme(
    legend.position = "none",
  )
Plot_Temporal

# Guardamos como imagen del mapa:
ggsave(
  filename = paste("Mapa_Tiempos_Por_UPL_", Motivo_Viaje, "_EM2023.png", sep = ""),
  plot = Plot_Temporal,
  device = NULL,
  path = "./Data/3_Results/3_EM2023/",
  scale = 1,
  width = 1080,
  height = 1920,
  units = c("px"),
  dpi = 150,
  limitsize = TRUE,
  bg = NULL,
  create.dir = FALSE,
)

# Datos para poner manualmente en Word:
Tiempos_Principales_Motivos_Uni_UPL_sf

# Tiempo de procesamiento:
End_Time_S3_P14 <- Sys.time()
print(paste("14. Tiempo de procesamiento de tiempos de viaje por UPL y Categoría: ", as.duration(End_Time_S3_P14 - Start_Time_S3_P14)))

# ####
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ::: 15. Tiempos de viaje por estrato socioeconómico                        :::
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ####
#
Start_Time_S3_P15 <- Sys.time()

# Seleccionamos las UPL no rurales
Lista_No_Rural <- Shape_UPLs |> filter(ZONA_UPL != "Rural") |> as_tibble() |> select(COD_UPL)
Lista_No_Rural

Viajes_Estandarizado_df

# Los tiempos medios por estratos:
Tiempos_Viajes_Estrato_df <- Viajes_Estandarizado_df |>
  filter(!is.na(ESTRATO) & !is.na(TIEMPO_VIAJE) & TIEMPO_VIAJE > 0) |> # ESTRATO != "No aplica"
  mutate(
    ESTRATO = if_else(ESTRATO == "No aplica", "0", ESTRATO),
    ESTRATO = paste("Estrato ", ESTRATO, sep = "")
    ) |>
  group_by(ESTRATO) |>
  summarise(
    AVRG_TIME = mean(TIEMPO_VIAJE)
  )
Tiempos_Viajes_Estrato_df <- Tiempos_Viajes_Estrato_df |>
  mutate(
    ETIQUETA = paste(round(AVRG_TIME, digits = 2), " min (+", round((AVRG_TIME/min(Tiempos_Viajes_Estrato_df$AVRG_TIME)-1)*100, 2), "%)", sep = "")
  )
Tiempos_Viajes_Estrato_df

# Reordenar  otra vez el factor para que las barras vayan de mayor a menor
Tiempos_Viajes_Estrato_df$ESTRATO <- factor(Tiempos_Viajes_Estrato_df$ESTRATO, levels = Tiempos_Viajes_Estrato_df$ESTRATO[order(Tiempos_Viajes_Estrato_df$AVRG_TIME)])
Tiempos_Viajes_Estrato_df

# Diagrama de barras:
Plot_Tiempos_Viajes_Estrato <- Tiempos_Viajes_Estrato_df |>
  ggplot() +
  # Barras 
  geom_col(aes(x = ESTRATO, y = AVRG_TIME, fill = AVRG_TIME), width = 0.9) +
  # Nombre de la categoría dentro de la barra, al inicio
  geom_text(aes(x = ESTRATO, y = 0, label = ESTRATO ),
            hjust = 0,             # muy cerca del borde izquierdo
            nudge_y = max(Tiempos_Viajes_Estrato_df$AVRG_TIME) * 0.02,
            color = "white", size = 6, family = "Roboto ExtraBold", fontface = "bold") +
  # Valor del tiempo dentro de la barra, al final
  geom_text(aes(x = ESTRATO, y = AVRG_TIME, label = ETIQUETA),
            hjust = 1.2,             # muy cerca del borde derecho (1.2 es el mejor)
            color = "white", size = 6, family = "Roboto") +
  # Voltear ejes para barras horizontales
  coord_flip() +
  # Asignar colores:
  scale_fill_gradient(
    low = "darkolivegreen",
    high =  "darkred"
  ) +
  # Eliminar ejes, cuadrículas y leyenda
  theme_void() +
  theme(legend.position = "none") +
  # Añadir espacio a la derecha para que no se corte el texto
  scale_y_continuous(limits = c(0, max(Tiempos_Viajes_Estrato_df$AVRG_TIME) * 1.1), expand = c(0,0))
Plot_Tiempos_Viajes_Estrato

# Guardamos como imagen el gráfico de barras:
ggsave(
  filename = "Grafica_Tiempos_Viajes_Por_Estrato_EM2023.png",
  plot = Plot_Tiempos_Viajes_Estrato,
  device = NULL,
  path = "./Data/3_Results/3_EM2023/",
  scale = 1,
  width = 1000,
  height = 560,
  units = c("px"),
  dpi = 150,
  limitsize = TRUE,
  bg = NULL,
  create.dir = FALSE,
)

# Limpiamos el Shape de Estratificación
Shape_Estratificacion_MNZ <- Shape_Estratificacion_Original |>
  select(CODIGO_MANZANA, ESTRATO, CODIGO_ZONA_ESTRATO) #43765

# Creamos el Shape de ZAT a plotear para los estratos
Shape_Plotear_Estratificacion_ZAT <- Shape_Estratificacion_MNZ |>
  select(CODIGO_MANZANA, ESTRATO) |>
  st_centroid() |>
  st_join(
    Shape_ZAT |> select(COD_ZAT),
    join = st_intersects
  ) |>
  filter(!is.na(COD_ZAT)) |>
  as_tibble() |>
  select(COD_ZAT, ESTRATO) |>
  distinct(COD_ZAT, .keep_all = TRUE) |>
  right_join(
    Shape_ZAT
  ) |>
  filter(!is.na(ESTRATO)) |>
  mutate(
    ESTRATO = paste("Estrato ", ESTRATO, sep = "")
  ) |>
  left_join(
    Tiempos_Viajes_Estrato_df |> select(ESTRATO, AVRG_TIME)
  ) |>
  left_join(
    Shape_UPLs |> as_tibble() |> select(COD_UPL, ZONA_UPL)
  ) |>
  # Para deshacernos del estrato cero que está dentro de la ciudad (Parques, patrimonio, etc)
  filter(!(COD_UPL %in% c("18", "22", "23", "24", "25", "27", "29", "30", "31", "32", "33") & ESTRATO == "Estrato 0")) |>
  filter(COD_UPL %in% Lista_No_Rural$COD_UPL) |>
  st_as_sf()
Shape_Plotear_Estratificacion_ZAT

MostrarMapa(Shape_Plotear_Estratificacion_ZAT)

# Ploteamos el mapa de estratificación, con vacíos donde el Estrato cero esté cerca del centro de la ciudad.
Plot_Estratificacion_ZAT <- ggplot() + # Iniciamos el lienzo en blanco
  # CAPA 1: El mapa base completo (Shape_ZAT)
  # Lo dibujamos con un relleno gris y un borde blanco.
  geom_sf(
    data = Shape_ZAT |> filter(COD_UPL %in% Lista_No_Rural$COD_UPL),
    fill = "darkgray", # Relleno neutro para las zonas sin datos
    color = "white",   # Borde de las zonas base
    linewidth = 0.6
  ) +
  # CAPA 2: Los polígonos con datos, dibujados ENCIMA de la base
  geom_sf(
    data = Shape_Plotear_Estratificacion_ZAT,
    color = "white", # Borde para las zonas con datos
    linewidth = 0.6,
    aes(fill = AVRG_TIME) # Relleno según la variable
  ) +
  # Asignar colores para la CAPA 2:
  scale_fill_gradient(
    low = "darkolivegreen",
    high = "darkred"
  ) +
  # Temas y leyendas
  theme_void() +
  theme(
    legend.position = "none"
  )
Plot_Estratificacion_ZAT

# Guardamos como imagen del mapa:
ggsave(
  filename = "Mapa_Estratificacion_ZAT.png",
  plot = Plot_Estratificacion_ZAT,
  device = NULL,
  path = "./Data/3_Results/3_EM2023/",
  scale = 1,
  width = 1080,
  height = 1920,
  units = c("px"),
  dpi = 150,
  limitsize = TRUE,
  bg = NULL,
  create.dir = FALSE,
)

# Datos para poner manualmente en Word:
Tiempos_Viajes_Estrato_df

# Tiempo de procesamiento:
End_Time_S3_P15 <- Sys.time()
print(paste("15. Tiempo de procesamiento de tiempos de viaje por estrato: ", as.duration(End_Time_S3_P15 - Start_Time_S3_P15)))

# ####
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ::: 16. Tiempo de viaje por género                                         :::
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ####
#
Start_Time_S3_P16 <- Sys.time()

unique(Viajes_Estandarizado_df$SEXO)

# Los tiempos medios por estratos:
Tiempos_Viaje_Sexo_df <- Viajes_Estandarizado_df |>
  filter(!is.na(SEXO) & !is.na(TIEMPO_VIAJE) & TIEMPO_VIAJE > 0) |>
  group_by(SEXO) |>
  summarise(
    AVRG_TIME = mean(TIEMPO_VIAJE)
  ) |>
  mutate(
    ETIQUETA = paste(round(AVRG_TIME, digits = 2), " min", sep = "")
  )
Tiempos_Viaje_Sexo_df

# Reordenar  otra vez el factor para que las barras vayan de mayor a menor
Tiempos_Viaje_Sexo_df$ETIQUETA <- factor(Tiempos_Viaje_Sexo_df$ETIQUETA, levels = Tiempos_Viaje_Sexo_df$ETIQUETA[order(Tiempos_Viaje_Sexo_df$AVRG_TIME)])
Tiempos_Viaje_Sexo_df

# Creamos el plot de tiempos de género:
Plot_Tiempos_Sexo <- Tiempos_Viaje_Sexo_df |>
  ggplot() +
  # Barras horizontales
  geom_col(aes(x = SEXO, y = AVRG_TIME, fill = SEXO), width = 0.9) +
  # Nombre de la categoría dentro de la barra, al inicio
  geom_text(aes(x = SEXO, y = 0, label = SEXO ),
            hjust = 0,             # muy cerca del borde izquierdo
            nudge_y = max(Tiempos_Viaje_Sexo_df$AVRG_TIME) * 0.02,
            color = "white", size = 6, family = "Roboto ExtraBold", fontface = "bold") +
  # Valor del tiempo dentro de la barra, al final
  geom_text(aes(x = SEXO, y = AVRG_TIME, label = ETIQUETA),
            hjust = 1.2,             # muy cerca del borde derecho (1.2 es el mejor)
            color = "white", size = 6, family = "Roboto") +
  # Voltear ejes para barras horizontales
  coord_flip() +
  # Asignar colores:
  scale_fill_manual(values = c(
    # Usamos str_detect() que es similar a grepl()
    Masculino = "deepskyblue4",
    Femenino = "deeppink4",
    Diverso = "darkorchid4")
  ) +
  # Eliminar ejes, cuadrículas y leyenda
  theme_void() +
  theme(legend.position = "none") +
  # Añadir espacio a la derecha para que no se corte el texto
  scale_y_continuous(limits = c(0, max(Tiempos_Viaje_Sexo_df$AVRG_TIME) * 1.1), expand = c(0,0))
Plot_Tiempos_Sexo

# Guardamos como imagen del mapa:
ggsave(
  filename = "Grafica_Tiempos_Por_Genero.png",
  plot = Plot_Tiempos_Sexo,
  device = NULL,
  path = "./Data/3_Results/3_EM2023/",
  scale = 1,
  width = 1500,
  height = 250,
  units = c("px"),
  dpi = 150,
  limitsize = TRUE,
  bg = NULL,
  create.dir = FALSE,
)

# Datos para poner manualmente en Word:
Tiempos_Viaje_Sexo_df

# Tiempo de procesamiento:
End_Time_S3_P16 <- Sys.time()
print(paste("16. Tiempo de procesamiento de tiempos de viaje por género: ", as.duration(End_Time_S3_P16 - Start_Time_S3_P16)))

# ####
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ::: 17. Tiempos de viaje generales por UPL                                 :::
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ####
#
Start_Time_S3_P17 <- Sys.time()

# En esta sección, primero se hacen las gráficas de barras, y después el mapa.

unique(Viajes_Estandarizado_df$VEHICULO_ESTANDAR)

# Tiempos de viaje de por UPL y por categoría:
Tiempos_Viaje_Por_UPL_sf <- Viajes_Estandarizado_df |>
  select(UPL_ORIGEN, TIEMPO_VIAJE) |>
  filter(UPL_ORIGEN != "" & !is.na(TIEMPO_VIAJE) & TIEMPO_VIAJE > 0) |>
  mutate(COD_UPL = UPL_ORIGEN) |>
  group_by(COD_UPL) |>
  summarise(
    AVRG_TIME = mean(TIEMPO_VIAJE)
  ) |>
  arrange(desc(AVRG_TIME)) |>
  left_join(Shape_UPLs) |>
  filter(ZONA_UPL != "Rural") |>
  st_as_sf()
Tiempos_Viaje_Por_UPL_sf

# Reordenar  otra vez el factor para que las barras vayan de menor a mayor
Tiempos_Viaje_Por_UPL_sf$COD_UPL <- factor(Tiempos_Viaje_Por_UPL_sf$COD_UPL, levels = Tiempos_Viaje_Por_UPL_sf$COD_UPL[order(Tiempos_Viaje_Por_UPL_sf$AVRG_TIME)])
Tiempos_Viaje_Por_UPL_sf

# Gráfica de barras para el motivo seleccionado
Plot_Tiempos_Viaje_Por_UPL <- Tiempos_Viaje_Por_UPL_sf |>
  ggplot() +
  # Barras horizontales
  geom_col(aes(x = COD_UPL, y = AVRG_TIME, fill = AVRG_TIME), width = 0.9) +
  # Nombre de la UPL dentro de la barra, al inicio
  geom_text(aes(x = COD_UPL, y = 0, label = paste("UPL ", COD_UPL, sep = "")),
            hjust = 0,             # muy cerca del borde izquierdo
            nudge_y = max(Tiempos_Viaje_Por_UPL_sf$AVRG_TIME) * 0.02,
            color = "white", size = 4, family = "Roboto", fontface = "bold") +
  # Valor de horas dentro de la barra, al final
  geom_text(aes(x = COD_UPL, y = AVRG_TIME, label = paste(round(AVRG_TIME, digits = 2), " min", sep = "")),
            hjust = 1.2,             # muy cerca del borde derecho (1.2 es el mejor)
            color = "white", size = 4, family = "Roboto") +
  # Voltear ejes para barras horizontales
  coord_flip() +
  # Asignar colores:
  scale_fill_gradient(
    low = "darkgray",
    high = "darkolivegreen"
  ) +
  # Eliminar ejes, cuadrículas y leyenda
  theme_void() +
  theme(legend.position = "none") +
  # Añadir espacio a la derecha para que no se corte el texto
  scale_y_continuous(limits = c(0, max(Tiempos_Viaje_Por_UPL_sf$AVRG_TIME) * 1.1), expand = c(0,0))
Plot_Tiempos_Viaje_Por_UPL

# Guardamos como imagen el gráfico de barras:
ggsave(
  filename = paste("Grafica_Tiempos_Por_UPL_EM2023.png", sep = ""),
  plot = Plot_Tiempos_Viaje_Por_UPL,
  device = NULL,
  path = "./Data/3_Results/3_EM2023/",
  scale = 1,
  width = 1000,
  height = 1500,
  units = c("px"),
  dpi = 150,
  limitsize = TRUE,
  bg = NULL,
  create.dir = FALSE,
)

# Para plotear el mapa con etiquetas, se den sacar los centroides y definir el contenido de la etiqueta:
Shape_Plotear_Mapa <- Tiempos_Viaje_Por_UPL_sf |>
  mutate(
    # Extrae las coordenadas del centroide de cada área
    centroides = st_centroid(geom),
    X = st_coordinates(centroides)[,1],
    Y = st_coordinates(centroides)[,2],
    # Define la columna que se usará como etiqueta
    Etiqueta_Texto = COD_UPL 
  )

Plot_Temporal <- Shape_Plotear_Mapa |>
  ggplot() +
  geom_sf(
    col = "white",
    linewidth = 0.6,
    aes(fill = AVRG_TIME)
  ) + 
  # Asignar colores:
  scale_fill_gradient(
    low = "bisque3",
    high = "darkolivegreen"
  ) +
  # Capa de etiquetas:
  geom_text(
    aes(x = X, y = Y, label = Etiqueta_Texto), # Mapea coordenadas y texto
    color = "white",        # Color del texto
    size = 6,               # Tamaño del texto
    family = "Roboto Black" # Tipo de fuente
  ) +
  theme_void() +
  theme(
    legend.position = "none",
  )
Plot_Temporal

# Guardamos como imagen del mapa:
ggsave(
  filename = paste("Mapa_Tiempos_Por_UPL_EM2023.png", sep = ""),
  plot = Plot_Temporal,
  device = NULL,
  path = "./Data/3_Results/3_EM2023/",
  scale = 1,
  width = 1080,
  height = 1920,
  units = c("px"),
  dpi = 150,
  limitsize = TRUE,
  bg = NULL,
  create.dir = FALSE,
)

# Datos para poner manualmente en Word:
Tiempos_Principales_Motivos_Uni_UPL_sf

# Tiempo de procesamiento:
End_Time_S3_P17 <- Sys.time()
print(paste("17. Tiempo de procesamiento de tiempos de viaje por UPL generales: ", as.duration(End_Time_S3_P17 - Start_Time_S3_P17)))

# ####
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ::: 18. Simplificación de la tabla de viajes a mostrar                     :::
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ####
#
Start_Time_S3_P18 <- Sys.time()

# Origen, Destino, Motivo, Ocupación, Medio, Género, Estrato, Tiempo de viaje

names(Viajes_Estandarizado_df)

# Tabla para mostrar
Tabla_EM2023_Mostrar <- Viajes_Estandarizado_df |>
  select(
    DATA_ORIGEN, COD_VIAJE, COD_PERSONA, EDAD, SEXO, ESTRATO, OCUPACION,
    LOC_ORIGEN, UPL_ORIGEN, UTAM_ORIGEN, ZAT_ORIGEN, LON_ORIGEN, LAT_ORIGEN,
    LOC_DESTINO, UPL_DESTINO, UTAM_DESTINO, ZAT_DESTINO, LON_DESTINO, LAT_DESTINO,
    TIEMPO_VIAJE, CATEGORIA, CATEGORY, VEHICULO_ESTANDAR, VEHICLE_STANDARD
    )
Tabla_EM2023_Mostrar

# Tiempo de procesamiento:
End_Time_S3_P18 <- Sys.time()
print(paste("18. Tiempo de procesamiento de simplificación de tabla de viajes a mostrar: ", as.duration(End_Time_S3_P18 - Start_Time_S3_P18)))

# ####
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ::: 19. Alistamiento de tabla para calibración del modelo                  :::
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ####
#
Start_Time_S3_P19 <- Sys.time()

Viajes_Calibracion_Modelo_df <- Viajes_Estandarizado_df |>
  filter(DATA_ORIGEN == "EH") |>
  group_by(UPL_ORIGEN, UPL_DESTINO, ZAT_ORIGEN, ZAT_DESTINO, VEHICULO_ESTANDAR) |>
  summarise(
    VIAJES = n(),
    AVRG_TIME = mean(TIEMPO_VIAJE)
  ) |>
  filter(VEHICULO_ESTANDAR %in% c("Transporte Público", "Automóvil", "Bicicleta", "A Pie"))
#  filter(VEHICULO_ESTANDAR == "Transporte Público") |>
#  filter(UPL_ORIGEN == "16")
Viajes_Calibracion_Modelo_df

# Guardamos los df importantes:
save(
  Viajes_Estandarizado_df,
  Viajes_Calibracion_Modelo_df,
  file = "./Data/2_Processing/3_EM2023/DataFrames_EM2023.RData"
)
load("./Data/2_Processing/3_EM2023/DataFrames_EM2023.RData")

# Tiempo de procesamiento:
End_Time_S3_P19 <- Sys.time()
print(paste("19. Tiempo de procesamiento de alistamiento de tabla para calibración del modelo: ", as.duration(End_Time_S3_P19 - Start_Time_S3_P19)))

# ####
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ::: 20. Tabla de UPLs que más viajes reciben                               :::
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ####
#
Start_Time_S3_P20 <- Sys.time()

Muestra_EM2023 <- length(unique(Viajes_Estandarizado_df$COD_PERSONA))
Poblacion_Ciudad <- Censo_Municipios$PERSONAS_MPIO
Factor_De_Expansion <- 1/(Muestra_EM2023/Poblacion_Ciudad)

Viajes_A_UPL_df <- Viajes_Estandarizado_df |>
  filter(CATEGORIA != "Hogar") |>
  group_by(UPL_DESTINO) |>
  summarise(
    Viajes_A_UPL = n()
  ) |>
  select(COD_UPL = UPL_DESTINO, Viajes_A_UPL) |>
  arrange(desc(Viajes_A_UPL))
Viajes_A_UPL_df <- Viajes_A_UPL_df |>
  mutate(
    Porcentaje = paste(round(Viajes_A_UPL/sum(Viajes_A_UPL_df$Viajes_A_UPL)*100, digits =  2), " %", sep = ""),
    Viajes_Reales_A_UPL = round(Viajes_A_UPL * Factor_De_Expansion, digits = 0)
  ) |>
  left_join(
    Shape_UPLs |> as_tibble() |> select(COD_UPL, NOM_UPL, ZONA_UPL)
  ) |>
  filter(!is.na(COD_UPL) & !is.na(ZONA_UPL)) |>
  select(COD_UPL, NOM_UPL, ZONA_UPL, Viajes_Reales_A_UPL, Porcentaje)
Viajes_A_UPL_df

# Tiempo de procesamiento:
End_Time_S3_P20 <- Sys.time()
print(paste("20. Tiempo de procesamiento de tabla de UPLs que más viajes reciben: ", as.duration(End_Time_S3_P20 - Start_Time_S3_P20)))

# ####
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ::: 21. Mapa de UPLs con etiquetas sin colores solo para referencia        :::
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ####
#
Start_Time_S3_P21 <- Sys.time()

Shape_Plotear_Mapa <- Shape_UPLs |>
  filter(ZONA_UPL != "Rural") |>
  mutate(
    # Extrae las coordenadas del centroide de cada área
    centroides = st_centroid(geom),
    X = st_coordinates(centroides)[,1],
    Y = st_coordinates(centroides)[,2],
    # Define la columna que se usará como etiqueta
    Etiqueta_Texto = COD_UPL 
  )

Plot_Mapa <- Shape_Plotear_Mapa |>
  ggplot() +
  geom_sf(
    col = "white",
    linewidth = 1,
    fill = "lightgray"
  ) + 
  # # Asignar colores:
  # scale_fill_gradient(
  #   low = "bisque3",
  #   high = "darkolivegreen"
  # ) +
  # Capa de etiquetas:
  geom_text(
    aes(x = X, y = Y, label = Etiqueta_Texto), # Mapea coordenadas y texto
    color = "#595959",        # Color del texto
    size = 10,               # Tamaño del texto
    family = "Roboto Black" # Tipo de fuente
  ) +
  theme_void() +
  theme(
    legend.position = "none",
  )
Plot_Mapa

Nombre_Archivo <- "Mapa_UPL_Referencia.png"
Ruta_Archivo <- "./Data/3_Results/3_EM2023/"

# Guardamos como imagen del mapa:
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
End_Time_S3_P21 <- Sys.time()
print(paste("21. Tiempo de creación de mapa de UPLs con etiquetas para referencia: ", as.duration(End_Time_S3_P21 - Start_Time_S3_P21)))










# ####
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ::: 22. Totalizamos los tiempos de procesamiento del Script                :::
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ####
#
End_Time_S3_P0 <- Sys.time()
print(paste("1. Tiempo de inicialización: ", as.duration(End_Time_S3_P1 - Start_Time_S3_P1)))
print(paste("2. Tiempo de carga de datos de origen: ", as.duration(End_Time_S3_P2 - Start_Time_S3_P2)))
print(paste("3. Tiempo de tratamiento de df de viajes: ", as.duration(End_Time_S3_P3 - Start_Time_S3_P3)))
print(paste("4. Tiempo de relacionamiento de Shapes de ZAT y UPLs con el df de viajes: ", as.duration(End_Time_S3_P4 - Start_Time_S3_P4)))
print(paste("5. Tiempo de procesamiento del mapa de Origen-Destino (ZAT): ", as.duration(End_Time_S3_P5 - Start_Time_S3_P5)))
print(paste("6. Tiempo de procesamiento del gráfico de cuerdas Origen-Destino (UPL): ", as.duration(End_Time_S3_P6 - Start_Time_S3_P6)))
print(paste("7. Tiempo de estandarización del df de viajes: ", as.duration(End_Time_S3_P7 - Start_Time_S3_P7)))
print(paste("8. Tiempo de establecimiento del color: ", as.duration(End_Time_S3_P8 - Start_Time_S3_P8)))
print(paste("9. Tiempo de procesar principales motivos de viaje: ", as.duration(End_Time_S3_P9 - Start_Time_S3_P9)))
print(paste("10. Tiempo de procesamiento de la distribución modal: ", as.duration(End_Time_S3_P10 - Start_Time_S3_P10)))
print(paste("11. Tiempo de procesamiento de la distribución sustentable: ", as.duration(End_Time_S3_P11 - Start_Time_S3_P11)))
print(paste("12. Tiempo de procesamiento de tiempos de principales motivos de viaje: ", as.duration(End_Time_S3_P12 - Start_Time_S3_P12)))
print(paste("13. Tiempo de procesamiento de tiempos de principales motivos de viaje (Universidad): ", as.duration(End_Time_S3_P13 - Start_Time_S3_P13)))
print(paste("14. Tiempo de procesamiento de tiempos de viaje por UPL y Categoría: ", as.duration(End_Time_S3_P14 - Start_Time_S3_P14)))
print(paste("15. Tiempo de procesamiento de tiempos de viaje por estrato: ", as.duration(End_Time_S3_P15 - Start_Time_S3_P15)))
print(paste("16. Tiempo de procesamiento de tiempos de viaje por género: ", as.duration(End_Time_S3_P16 - Start_Time_S3_P16)))
print(paste("17. Tiempo de procesamiento de tiempos de viaje por UPL generales: ", as.duration(End_Time_S3_P17 - Start_Time_S3_P17)))
print(paste("18. Tiempo de procesamiento de simplificación de tabla de viajes a mostrar: ", as.duration(End_Time_S3_P18 - Start_Time_S3_P18)))
print(paste("19. Tiempo de procesamiento de alistamiento de tabla para calibración del modelo: ", as.duration(End_Time_S3_P19 - Start_Time_S3_P19)))
print(paste("20. Tiempo de procesamiento de tabla de UPLs que más viajes reciben: ", as.duration(End_Time_S3_P20 - Start_Time_S3_P20)))
print(paste("21. Tiempo de creación de mapa de UPLs con etiquetas para referencia: ", as.duration(End_Time_S3_P21 - Start_Time_S3_P21)))
print(paste("Tiempo total de procesamiento del Script: ", as.duration(End_Time_S3_P0 - Start_Time_S3_P0)))

