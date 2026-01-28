# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# :::                                                                        :::
# :::                            SCRIPT DE HQSL                              :::
# :::                                                                        :::
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#
# Nombre: 7_HQSL.R
#
# Descripcion: Este Script calcula el HQSL para todos los Escenarios, basado en
#              los resultados del cálculo de Accesibilidad.
#
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# :::                      Contenido de este Script                          :::
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#####
#
# 1. Inicialización
# 2. Carga de datos de origen

Start_Time_S7_P0 <- Sys.time()

# ####
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ::: 1. Inicialización                                                      :::
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ####
#
Start_Time_S7_P1 <- Sys.time()

# Cargamos el script de inicialización general
source("0_Initialization.R")

# Tiempo de procesamiento:
End_Time_S7_P1 <- Sys.time()
print(paste("1. Tiempo de inicialización: ", as.duration(End_Time_S7_P1 - Start_Time_S7_P1)))

# ####
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ::: 2. Carga de datos de origen                                            :::
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ####
#
Start_Time_S7_P2 <- Sys.time()

# --- 2.1. Carga de Shapes Vacíos (1_Empty_Shapes.R) ---

# Cargamos los shapes población:
Shape_Censo_Municipios <- st_read("./Data/2_Processing/2_Population/Shape_Poblacion_Municipios.gpkg")
Shape_Censo_UPLs <- st_read("./Data/2_Processing/2_Population/Shape_Poblacion_UPLs.gpkg")
Shape_Censo_Manzanas <- st_read("./Data/2_Processing/2_Population/Shape_Poblacion_Manzanas.gpkg")
Coordinates_CRS <- st_crs(Shape_Censo_Municipios)

# --- 2.2. Carga de datos de accesibilidad ---

# Datos para el Escenario 1:
load("./Data/2_Processing/6_Accesibility/Scn1/Acceso_Normalizado_MNZ_Scn1.RData")
load("./Data/2_Processing/6_Accesibility/Scn1/Acceso_Normalizado_UPL_Scn1.RData")
# Verificamos la carga:
Acceso_Equipamientos_MNZ_Scn1_sf
Acceso_Equipamientos_MNZ_Scn1_Norm_sf
Acceso_Equipamientos_MNZ_Scn1_15min_Norm_Medias_df
Acceso_Equipamientos_MNZ_Scn1_30min_Norm_Medias_df
Acceso_Equipamientos_UPL_Scn1_sf
Acceso_Equipamientos_UPL_Scn1_Norm_sf
Acceso_Equipamientos_UPL_Scn1_15min_Norm_Medias_df
Acceso_Equipamientos_UPL_Scn1_30min_Norm_Medias_df

# Datos para el Escenario 2:
load("./Data/2_Processing/6_Accesibility/Scn2/Acceso_Normalizado_MNZ_Scn2.RData")
load("./Data/2_Processing/6_Accesibility/Scn2/Acceso_Normalizado_UPL_Scn2.RData")
# Verificamos la carga:
Acceso_Equipamientos_MNZ_Scn2_sf
Acceso_Equipamientos_MNZ_Scn2_Norm_sf
Acceso_Equipamientos_MNZ_Scn2_15min_Norm_Medias_df
Acceso_Equipamientos_MNZ_Scn2_30min_Norm_Medias_df
Acceso_Equipamientos_UPL_Scn2_sf
Acceso_Equipamientos_UPL_Scn2_Norm_sf
Acceso_Equipamientos_UPL_Scn2_15min_Norm_Medias_df
Acceso_Equipamientos_UPL_Scn2_30min_Norm_Medias_df

# Datos para el Escenario 3:
load("./Data/2_Processing/6_Accesibility/Scn3/Acceso_Normalizado_MNZ_Scn3.RData")
load("./Data/2_Processing/6_Accesibility/Scn3/Acceso_Normalizado_UPL_Scn3.RData")
# Verificamos la carga:
Acceso_Equipamientos_MNZ_Scn3_sf
Acceso_Equipamientos_MNZ_Scn3_Norm_sf
Acceso_Equipamientos_MNZ_Scn3_15min_Norm_Medias_df
Acceso_Equipamientos_MNZ_Scn3_30min_Norm_Medias_df
Acceso_Equipamientos_UPL_Scn3_sf
Acceso_Equipamientos_UPL_Scn3_Norm_sf
Acceso_Equipamientos_UPL_Scn3_15min_Norm_Medias_df
Acceso_Equipamientos_UPL_Scn3_30min_Norm_Medias_df

# Datos para el Escenario 4:
load("./Data/2_Processing/6_Accesibility/Scn4/Acceso_Normalizado_MNZ_Scn4.RData")
load("./Data/2_Processing/6_Accesibility/Scn4/Acceso_Normalizado_UPL_Scn4.RData")
# Verificamos la carga:
Acceso_Equipamientos_MNZ_Scn4_sf
Acceso_Equipamientos_MNZ_Scn4_Norm_sf
Acceso_Equipamientos_MNZ_Scn4_15min_Norm_Medias_df
Acceso_Equipamientos_MNZ_Scn4_30min_Norm_Medias_df
Acceso_Equipamientos_UPL_Scn4_sf
Acceso_Equipamientos_UPL_Scn4_Norm_sf
Acceso_Equipamientos_UPL_Scn4_15min_Norm_Medias_df
Acceso_Equipamientos_UPL_Scn4_30min_Norm_Medias_df

# Datos para el Escenario 5:
load("./Data/2_Processing/6_Accesibility/Scn5/Acceso_Normalizado_MNZ_Scn5.RData")
load("./Data/2_Processing/6_Accesibility/Scn5/Acceso_Normalizado_UPL_Scn5.RData")
# Verificamos la carga:
Acceso_Equipamientos_MNZ_Scn5_sf
Acceso_Equipamientos_MNZ_Scn5_Norm_sf
Acceso_Equipamientos_MNZ_Scn5_15min_Norm_Medias_df
Acceso_Equipamientos_MNZ_Scn5_30min_Norm_Medias_df
Acceso_Equipamientos_UPL_Scn5_sf
Acceso_Equipamientos_UPL_Scn5_Norm_sf
Acceso_Equipamientos_UPL_Scn5_15min_Norm_Medias_df
Acceso_Equipamientos_UPL_Scn5_30min_Norm_Medias_df

# Datos para el Escenario 6:
load("./Data/2_Processing/6_Accesibility/Scn6/Acceso_Normalizado_MNZ_Scn6.RData")
load("./Data/2_Processing/6_Accesibility/Scn6/Acceso_Normalizado_UPL_Scn6.RData")
# Verificamos la carga:
Acceso_Equipamientos_MNZ_Scn6_sf
Acceso_Equipamientos_MNZ_Scn6_Norm_sf
Acceso_Equipamientos_MNZ_Scn6_15min_Norm_Medias_df
Acceso_Equipamientos_MNZ_Scn6_30min_Norm_Medias_df
Acceso_Equipamientos_UPL_Scn6_sf
Acceso_Equipamientos_UPL_Scn6_Norm_sf
Acceso_Equipamientos_UPL_Scn6_15min_Norm_Medias_df
Acceso_Equipamientos_UPL_Scn6_30min_Norm_Medias_df

# Datos para el Escenario 7:
load("./Data/2_Processing/6_Accesibility/Scn7/Acceso_Normalizado_MNZ_Scn7.RData")
load("./Data/2_Processing/6_Accesibility/Scn7/Acceso_Normalizado_UPL_Scn7.RData")
# Verificamos la carga:
Acceso_Equipamientos_MNZ_Scn7_sf
Acceso_Equipamientos_MNZ_Scn7_Norm_sf
Acceso_Equipamientos_MNZ_Scn7_15min_Norm_Medias_df
Acceso_Equipamientos_MNZ_Scn7_30min_Norm_Medias_df
Acceso_Equipamientos_UPL_Scn7_sf
Acceso_Equipamientos_UPL_Scn7_Norm_sf
Acceso_Equipamientos_UPL_Scn7_15min_Norm_Medias_df
Acceso_Equipamientos_UPL_Scn7_30min_Norm_Medias_df

# Tiempo de procesamiento:
End_Time_S7_P2 <- Sys.time()
print(paste("2. Tiempo de carga de datos de origen: ", as.duration(End_Time_S7_P2 - Start_Time_S7_P2)))

# ####
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ::: 3. Cálculo del HQSL para cada Escenario                                :::
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ####
#
Start_Time_S7_P3 <- Sys.time()

# Establecemos valores globales:
AREA_BOG <- Shape_Censo_Municipios$AREA_MPIO
PERSONAS_BOG <- Shape_Censo_Municipios$PERSONAS_MPIO
DENSIDAD_BOG <- Shape_Censo_Municipios$DENSIDAD_MPIO
Social_Functions <- c("Living", "Working", "Supplying", "Caring", "Learning", "Enjoying")
Composite_Indicators <- c("Well_being", "Sociability", "Environmental_Impact")

# HQSL Para Escenario 1
HQSL_Scn1 <- Acceso_Equipamientos_UPL_Scn1_Norm_sf |>
  select(
    COD_UPL:Environmental_Impact
  ) |>
  mutate(
    HQSL_SF_Norm = round(Living + Working + Supplying + Caring + Learning + Enjoying, digits = 2),
    SCN = "Scn1"
  ) |>
  # Incorporación de las medias para cada Función Social e Indicador Compuesto:
  left_join(
    bind_rows(
      bind_rows(Acceso_Equipamientos_UPL_Scn1_15min_Norm_Medias_df) |>
        select(Living, Working, Supplying, Caring, Learning, Enjoying, Well_being, Sociability, Environmental_Impact) |>
        rename_with(~ paste0(.x, "_Media")) |>
        mutate(
          SCN = "Scn1",
          cutoff = 15
        ),
      bind_rows(Acceso_Equipamientos_UPL_Scn1_30min_Norm_Medias_df) |>
        select(Living, Working, Supplying, Caring, Learning, Enjoying, Well_being, Sociability, Environmental_Impact) |>
        rename_with(~ paste0(.x, "_Media")) |>
        mutate(
          SCN = "Scn1",
          cutoff = 30
        )
    )
  ) |>
  # Unimos con los datos brutos, para obtener las sumas de Social Functions y Composite Indicators
  left_join(
    Acceso_Equipamientos_UPL_Scn1_sf |>
      st_drop_geometry() |>
      select(COD_UPL, cutoff, HQSL_SF = Social_Functions, HQSL_CI = Composite_Indicators)
  )
HQSL_Scn1

# HQSL Para Escenario 2
HQSL_Scn2 <- Acceso_Equipamientos_UPL_Scn2_Norm_sf |>
  select(
    COD_UPL:Environmental_Impact
  ) |>
  mutate(
    HQSL_SF_Norm = round(Living + Working + Supplying + Caring + Learning + Enjoying, digits = 2),
    SCN = "Scn2"
  ) |>
  # Incorporación de las medias para cada Función Social e Indicador Compuesto:
  left_join(
    bind_rows(
      bind_rows(Acceso_Equipamientos_UPL_Scn2_15min_Norm_Medias_df) |>
        select(Living, Working, Supplying, Caring, Learning, Enjoying, Well_being, Sociability, Environmental_Impact) |>
        rename_with(~ paste0(.x, "_Media")) |>
        mutate(
          SCN = "Scn2",
          cutoff = 15
        ),
      bind_rows(Acceso_Equipamientos_UPL_Scn2_30min_Norm_Medias_df) |>
        select(Living, Working, Supplying, Caring, Learning, Enjoying, Well_being, Sociability, Environmental_Impact) |>
        rename_with(~ paste0(.x, "_Media")) |>
        mutate(
          SCN = "Scn2",
          cutoff = 30
        )
     )
  ) |>
  # Unimos con los datos brutos, para obtener las sumas de Social Functions y Composite Indicators
  left_join(
    Acceso_Equipamientos_UPL_Scn2_sf |>
      st_drop_geometry() |>
      select(COD_UPL, cutoff, HQSL_SF = Social_Functions, HQSL_CI = Composite_Indicators)
  )
HQSL_Scn2

# HQSL Para Escenario 3
HQSL_Scn3 <- Acceso_Equipamientos_UPL_Scn3_Norm_sf |>
  select(
    COD_UPL:Environmental_Impact
  ) |>
  mutate(
    HQSL_SF_Norm = round(Living + Working + Supplying + Caring + Learning + Enjoying, digits = 2),
    SCN = "Scn3"
  ) |>
  # Incorporación de las medias para cada Función Social e Indicador Compuesto:
  left_join(
    bind_rows(
      bind_rows(Acceso_Equipamientos_UPL_Scn3_15min_Norm_Medias_df) |>
        select(Living, Working, Supplying, Caring, Learning, Enjoying, Well_being, Sociability, Environmental_Impact) |>
        rename_with(~ paste0(.x, "_Media")) |>
        mutate(
          SCN = "Scn3",
          cutoff = 15
        ),
      bind_rows(Acceso_Equipamientos_UPL_Scn3_30min_Norm_Medias_df) |>
        select(Living, Working, Supplying, Caring, Learning, Enjoying, Well_being, Sociability, Environmental_Impact) |>
        rename_with(~ paste0(.x, "_Media")) |>
        mutate(
          SCN = "Scn3",
          cutoff = 30
        )
     )
  ) |>
  # Unimos con los datos brutos, para obtener las sumas de Social Functions y Composite Indicators
  left_join(
    Acceso_Equipamientos_UPL_Scn3_sf |>
      st_drop_geometry() |>
      select(COD_UPL, cutoff, HQSL_SF = Social_Functions, HQSL_CI = Composite_Indicators)
  )
HQSL_Scn3

# HQSL Para Escenario 4
HQSL_Scn4 <- Acceso_Equipamientos_UPL_Scn4_Norm_sf |>
  select(
    COD_UPL:Environmental_Impact
  ) |>
  mutate(
    HQSL_SF_Norm = round(Living + Working + Supplying + Caring + Learning + Enjoying, digits = 2),
    SCN = "Scn4"
  ) |>
  # Incorporación de las medias para cada Función Social e Indicador Compuesto:
  left_join(
    bind_rows(
      bind_rows(Acceso_Equipamientos_UPL_Scn4_15min_Norm_Medias_df) |>
        select(Living, Working, Supplying, Caring, Learning, Enjoying, Well_being, Sociability, Environmental_Impact) |>
        rename_with(~ paste0(.x, "_Media")) |>
        mutate(
          SCN = "Scn4",
          cutoff = 15
        ),
      bind_rows(Acceso_Equipamientos_UPL_Scn4_30min_Norm_Medias_df) |>
        select(Living, Working, Supplying, Caring, Learning, Enjoying, Well_being, Sociability, Environmental_Impact) |>
        rename_with(~ paste0(.x, "_Media")) |>
        mutate(
          SCN = "Scn4",
          cutoff = 30
        )
     )
  ) |>
  # Unimos con los datos brutos, para obtener las sumas de Social Functions y Composite Indicators
  left_join(
    Acceso_Equipamientos_UPL_Scn4_sf |>
      st_drop_geometry() |>
      select(COD_UPL, cutoff, HQSL_SF = Social_Functions, HQSL_CI = Composite_Indicators)
  )
HQSL_Scn4

# HQSL Para Escenario 5
HQSL_Scn5 <- Acceso_Equipamientos_UPL_Scn5_Norm_sf |>
  select(
    COD_UPL:Environmental_Impact
  ) |>
  mutate(
    HQSL_SF_Norm = round(Living + Working + Supplying + Caring + Learning + Enjoying, digits = 2),
    SCN = "Scn5"
  ) |>
  # Incorporación de las medias para cada Función Social e Indicador Compuesto:
  left_join(
    bind_rows(
      bind_rows(Acceso_Equipamientos_UPL_Scn5_15min_Norm_Medias_df) |>
        select(Living, Working, Supplying, Caring, Learning, Enjoying, Well_being, Sociability, Environmental_Impact) |>
        rename_with(~ paste0(.x, "_Media")) |>
        mutate(
          SCN = "Scn5",
          cutoff = 15
        ),
      bind_rows(Acceso_Equipamientos_UPL_Scn5_30min_Norm_Medias_df) |>
        select(Living, Working, Supplying, Caring, Learning, Enjoying, Well_being, Sociability, Environmental_Impact) |>
        rename_with(~ paste0(.x, "_Media")) |>
        mutate(
          SCN = "Scn5",
          cutoff = 30
        )
     )
  ) |>
  # Unimos con los datos brutos, para obtener las sumas de Social Functions y Composite Indicators
  left_join(
    Acceso_Equipamientos_UPL_Scn5_sf |>
      st_drop_geometry() |>
      select(COD_UPL, cutoff, HQSL_SF = Social_Functions, HQSL_CI = Composite_Indicators)
  )
HQSL_Scn5

# HQSL Para Escenario 6
HQSL_Scn6 <- Acceso_Equipamientos_UPL_Scn6_Norm_sf |>
  select(
    COD_UPL:Environmental_Impact
  ) |>
  mutate(
    HQSL_SF_Norm = round(Living + Working + Supplying + Caring + Learning + Enjoying, digits = 2),
    SCN = "Scn6"
  ) |>
  # Incorporación de las medias para cada Función Social e Indicador Compuesto:
  left_join(
    bind_rows(
      bind_rows(Acceso_Equipamientos_UPL_Scn6_15min_Norm_Medias_df) |>
        select(Living, Working, Supplying, Caring, Learning, Enjoying, Well_being, Sociability, Environmental_Impact) |>
        rename_with(~ paste0(.x, "_Media")) |>
        mutate(
          SCN = "Scn6",
          cutoff = 15
        ),
      bind_rows(Acceso_Equipamientos_UPL_Scn6_30min_Norm_Medias_df) |>
        select(Living, Working, Supplying, Caring, Learning, Enjoying, Well_being, Sociability, Environmental_Impact) |>
        rename_with(~ paste0(.x, "_Media")) |>
        mutate(
          SCN = "Scn6",
          cutoff = 30
        )
     )
  ) |>
  # Unimos con los datos brutos, para obtener las sumas de Social Functions y Composite Indicators
  left_join(
    Acceso_Equipamientos_UPL_Scn6_sf |>
      st_drop_geometry() |>
      select(COD_UPL, cutoff, HQSL_SF = Social_Functions, HQSL_CI = Composite_Indicators)
  )
HQSL_Scn6

# HQSL Para Escenario 7
HQSL_Scn7 <- Acceso_Equipamientos_UPL_Scn7_Norm_sf |>
  select(
    COD_UPL:Environmental_Impact
  ) |>
  mutate(
    HQSL_SF_Norm = round(Living + Working + Supplying + Caring + Learning + Enjoying, digits = 2),
    SCN = "Scn7"
  ) |>
  # Incorporación de las medias para cada Función Social e Indicador Compuesto:
  left_join(
    bind_rows(
      bind_rows(Acceso_Equipamientos_UPL_Scn7_15min_Norm_Medias_df) |>
        select(Living, Working, Supplying, Caring, Learning, Enjoying, Well_being, Sociability, Environmental_Impact) |>
        rename_with(~ paste0(.x, "_Media")) |>
        mutate(
          SCN = "Scn7",
          cutoff = 15
        ),
      bind_rows(Acceso_Equipamientos_UPL_Scn7_30min_Norm_Medias_df) |>
        select(Living, Working, Supplying, Caring, Learning, Enjoying, Well_being, Sociability, Environmental_Impact) |>
        rename_with(~ paste0(.x, "_Media")) |>
        mutate(
          SCN = "Scn7",
          cutoff = 30
        )
     )
  ) |>
  # Unimos con los datos brutos, para obtener las sumas de Social Functions y Composite Indicators
  left_join(
    Acceso_Equipamientos_UPL_Scn7_sf |>
      st_drop_geometry() |>
      select(COD_UPL, cutoff, HQSL_SF = Social_Functions, HQSL_CI = Composite_Indicators)
  )
HQSL_Scn7

# Tiempo de procesamiento:
End_Time_S7_P3 <- Sys.time()
print(paste("3. Tiempo de cálculo del HQSL para todos los escenarios: ", as.duration(End_Time_S7_P3 - Start_Time_S7_P3)))

# ####
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ::: 4. Creación de tablas de ScoreCards                                    :::
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ####
#
Start_Time_S7_P4 <- Sys.time()

# Establecemos datos comunes de las UPL en el ScoreCard:
Datos_ScoreCard_UPL <- Shape_Censo_UPLs |>
  st_drop_geometry() |>
  select(COD_UPL, PERSONAS_UPL, AREA_UPL, DENSIDAD_UPL) |>
  mutate(
    PORCENTAJE_PERSONAS_BOG = round(PERSONAS_UPL/PERSONAS_BOG * 100, digits = 2),
    PORCENTAJE_AREA_BOG = round(AREA_UPL/AREA_BOG * 100, digits = 2),
    AREA_UPL = round(AREA_UPL, digits = 2),
    DENSIDAD_UPL = round(DENSIDAD_UPL)
  )
Datos_ScoreCard_UPL

# Generamos tabla general de ScoreCard:
ScoreCards_sf <- bind_rows(
  HQSL_Scn1,
  HQSL_Scn2,
  HQSL_Scn3,
  HQSL_Scn4,
  HQSL_Scn5,
  HQSL_Scn6,
  HQSL_Scn7
  ) |>
  left_join(Datos_ScoreCard_UPL) |>
  mutate(
    cutoff = as.character(cutoff),
    ID = str_c(SCN, "_", cutoff, "min_UPL", COD_UPL)
  )
ScoreCards_sf

# Filtramos para limitar el procesamiento:
ScoreCards_sf <- ScoreCards_sf |> head(n = 5)

# Tiempo de procesamiento:
End_Time_S7_P4 <- Sys.time()
print(paste("4. Tiempo de creación de info para ScoreCards: ", as.duration(End_Time_S7_P4 - Start_Time_S7_P4)))

# ####
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ::: 5. Generación de gráficos de radar                                     :::
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ####
#
Start_Time_S7_P5 <- Sys.time()

# Mandamos a volar la geometría para no tener problemas en la creación del radar:
ScoreCards_df <- ScoreCards_sf |> st_drop_geometry()

# Guardamos estas líneas para el filtro dentro de la función:
# select(Caring, Enjoying, Learning, Living, Supplying, Working)
# select(Caring_Media, Enjoying_Media, Learning_Media, Living_Media, Supplying_Media, Working_Media)

# Activamos showtext para usar la fuente personalizada en el gráfico.
showtext_auto()

crear_radar_fmsb <- function(fila_datos, id_col = "ID") {
  
  # --- 1. PREPARACIÓN DE DATOS (Formato especial fmsb) ---
  
  # A. Extraemos los valores de la UPL
  datos_caso <- fila_datos %>%
    select(Caring, Enjoying, Learning, Living, Supplying, Working)
  
  # B. Extraemos los valores de la media (y le quitamos el "_Media" al nombre)
  datos_media <- fila_datos %>%
    select(Caring_Media, Enjoying_Media, Learning_Media, Living_Media, Supplying_Media, Working_Media) |>
    rename_with(~ str_remove(., "(?i)_Media"))
  
  # C. Construimos el Data Frame mágico para fmsb
  # Necesitamos que las columnas coincidan, así que usamos los nombres de 'datos_caso'
  columnas <- names(datos_caso)
  
  # Creamos el DF con 4 filas:
  # Fila 1: Máximo (100)
  # Fila 2: Mínimo (0)
  # Fila 3: La Media (para que quede al fondo)
  # Fila 4: El Caso de la UPL (para que quede encima)
  
  df_radar <- rbind(
    rep(100, length(columnas)), # Fila Max
    rep(0, length(columnas)),   # Fila Min
    datos_media,                # Fila Media
    datos_caso                  # Fila Caso
  )
  
  # Aseguramos que sea Data Frame numérico y sin NAs (reemplazar NA con 0)
  df_radar[is.na(df_radar)] <- 0
  df_radar <- as.data.frame(df_radar)
  
  # --- 2. GRAFICAR ---
  
  # Definimos colores con transparencia (alpha)
  # rgb(red, green, blue, alpha) -> alpha va de 0 a 1
  color_media_borde <- "#595959"
  color_media_relleno <- NA  # Transparente total
  
  color_caso_borde <- "#69b3a2" 
  color_caso_relleno <- rgb(0.4, 0.7, 0.6, 0.4) # Un verde azulado con transparencia
  
  # Título
  titulo <- paste("Radar:", fila_datos[[id_col]])
  COD_UPL <- fila_datos[["COD_UPL"]]
  NOM_UPL <- fila_datos[["NOM_UPL"]]
  par(family = "Inter 18pt", xpd = TRUE)
  
  # Función radarchart
  radarchart(df_radar,
             
             # --- Estilo de los Polígonos ---
             pcol = c(color_media_borde, color_caso_borde),     # Color de línea (Media, Caso)
             pfcol = c(color_media_relleno, color_caso_relleno),# Color de relleno
             plwd = c(2, 3),                                    # Grosor de línea
             plty = c(2, 1),                                    # Tipo de línea (2=punteada, 1=sólida)
             
             # --- Estilo de la Cuadrícula (Grid) ---
             cglcol = "grey",       # Color de la telaraña
             cglty = 1,             # Tipo de línea de telaraña
             axislabcol = "grey30", # Color de los números del eje
             caxislabels = c("0", "25", "50", "75", "100"), # Etiquetas manuales
             axistype = 1,          # Mostrar eje numérico
             
             # --- Títulos ---
             title = NULL,
             vlcex = 0.8            # Tamaño de fuente de las etiquetas de variables
  )
  
  # Añadir Leyenda (fmsb requiere ponerla aparte)
  legend(x = "bottom", legend = c("Mean in Bogota", paste("UPL ", COD_UPL, ": " , NOM_UPL, sep = "")), horiz = TRUE,
         bty = "n", pch = 20, col = c("black", "#69b3a2"),
         text.col = "#595959", cex = 1, pt.cex = 3)
}

# Probamos en RStudio:
crear_radar_fmsb(ScoreCards_df[1,])

# Creamos todos los gráficos de Radar:
for(i in 1:nrow(ScoreCards_df)) {
  
  nombre_archivo <- paste0("./Data/2_Processing/7_HQSL/1_Radares/Radar_", ScoreCards_df$ID[i], ".png")
  
  # 1. Abrir archivo PNG
  png(filename = nombre_archivo, 
      width = 700, height = 800, res = 150, 
      bg = "transparent") 
  
  # Ajuste de márgenes (opcional, pero ayuda a que no corte textos)
  # mar = c(bottom, left, top, right)
  par(mar = c(1, 1, 2, 1))
  
  # 2. Ejecutar la gráfica
  crear_radar_fmsb(ScoreCards_df[i, ])
  
  # 3. Cerrar archivo
  dev.off()
  
  # Para ver el progreso:
  print(paste("Generando gráfico de radar para las Funciones Sociales ", i, " de ", nrow(ScoreCards_df), ": Radar_", ScoreCards_df$ID[i], ".png", sep = ""))
}

# Tiempo de procesamiento:
End_Time_S7_P5 <- Sys.time()
print(paste("5. Tiempo de generación de gráficos de radar: ", as.duration(End_Time_S7_P5 - Start_Time_S7_P5)))

# ####
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ::: 6. Creación de gráfica de Indicadores Compuestos                       :::
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ####
#
Start_Time_S7_P6 <- Sys.time()

# Mandamos a volar la geometría para no tener problemas:
ScoreCards_df <- ScoreCards_sf |> st_drop_geometry()

# Activamos showtext para usar la fuente personalizada en el gráfico.
showtext_auto()

# Guardamos estas líneas para el filtro dentro de la función:
# select(Caring, Enjoying, Learning, Living, Supplying, Working)
# select(Caring_Media, Enjoying_Media, Learning_Media, Living_Media, Supplying_Media, Working_Media)

# --- 1. DEFINIR LOS RANGOS ---
rangos <- data.frame(
  xmin = c(0, 20, 40, 60, 80),
  xmax = c(20, 40, 60, 80, 100),
  fill = c("#FF3B30", "#FF9500", "#FFCC00", "#4CD964", "#007A33"),
  label = c("VERY POOR", "POOR", "FAIR", "GOOD", "EXCELLENT")
)

# --- 2. CREAR LA FUNCIÓN ---
# Esta función acepta: 'una fila del df) y 'nombre_archivo' (para guardar)
grafico_indicadores <- function(fila) {
  
  # Para tests:
  #fila <- ScoreCards_df[1, ]
  
  # A. TRANSFORMACIÓN DE DATOS (El paso clave)
  # Convertimos la fila ancha al formato que pide tu ggplot
  datos <- data.frame(
    Variable = c("ENVIRONMENTAL IMPACT", "SOCIABILITY", "WELL-BEING"),
    Valor = c(fila$Environmental_Impact, fila$Sociability, fila$Well_being),
    Media = c(fila$Environmental_Impact_Media, fila$Sociability_Media, fila$Well_being_Media)
  )
  
  # Aseguramos el orden de los factores
  datos$Variable <- factor(datos$Variable, levels = c("ENVIRONMENTAL IMPACT", "SOCIABILITY", "WELL-BEING"))
  
  # B. TU CÓDIGO DEL GRÁFICO (Casi intacto)
  plot_barras <- ggplot() +
    # --- CAPA 1: FONDO ---
    geom_rect(data = merge(datos, rangos), 
              aes(ymin = as.numeric(Variable) - 0.2, 
                  ymax = as.numeric(Variable) + 0.2,
                  xmin = xmin, xmax = xmax, fill = fill)) +
    
    # --- CAPA 2: LA MEDIA ---
    geom_segment(data = datos, 
                 aes(x = Media, xend = Media, 
                     y = as.numeric(Variable) - 0.3,
                     yend = as.numeric(Variable) + 0.3),
                 color = "#595959", size = 1.2) +
    
    # --- CAPA 3: ETIQUETAS RANGOS ---
    geom_text(data = rangos, aes(x = (xmin + xmax)/2, y = 3.6, label = label),
              size = 5, fontface = "bold", color = "grey40") +
    
    # --- CAPA 4: TEXTO "AVG" ---
    geom_text(data = datos, aes(x = Media, y = as.numeric(Variable) + 0.4, label = "Avg"), 
              size = 4, fontface = "italic", color = "#595959") +
    
    # --- CAPA 5: VALOR (TRIÁNGULO RELLENO) ---
    geom_point(data = datos, aes(x = Valor, y = as.numeric(Variable) - 0.2), 
               shape = 17, size = 8, color = "darkslategrey") +
    
    # --- CAPA 6: VALOR (TRIÁNGULO HUECO) ---
    geom_point(data = datos, aes(x = Valor, y = as.numeric(Variable) - 0.2), 
               shape = 2, size = 8, color = "darkgray", stroke = 1) +
    
    # --- ESTILOS ---
    scale_fill_identity() +
    scale_y_continuous(breaks = 1:3, labels = levels(datos$Variable)) +
    scale_x_continuous(limits = c(0, 100), expand = c(0, 0)) +
    theme_minimal() +
    labs(title = NULL, x = NULL, y = NULL) +
    theme(
      panel.background = element_rect(fill = "transparent", color = NA), 
      plot.background = element_rect(fill = "transparent", color = NA), 
      panel.grid = element_blank(),
      axis.text.y = element_text(face = "bold", size = 12, color = "#595959", hjust = 0),
      axis.text.x = element_blank(),
      axis.ticks = element_blank()
    )
  
  return(plot_barras)
}

grafico_indicadores(ScoreCards_df[1, ])

# --- 3. EL CICLO (LOOP) ---
# Asumiendo que tu dataframe grande se llama 'mi_df_grande'
for (i in 1:nrow(ScoreCards_df)) {
  
  # Llamamos a la función pasando la fila i
  plot_barras <-grafico_indicadores(ScoreCards_df[i, ])

  # Alistamos para guardar
  SCN <- ScoreCards_df[["SCN"]][i]
  cutoff <- ScoreCards_df[["cutoff"]][i]
  UPL <- ScoreCards_df[["COD_UPL"]][i]
  nombre <- paste0("Barras_Indicadores_", SCN, "_", cutoff,"min_UPL", UPL, ".png")
  ggsave(
    filename = nombre,
    plot = plot_barras,
    device = NULL,
    path = "./Data/2_Processing/7_HQSL/2_Indicadores",
    scale = 1,
    width = 900,
    height = 300,
    units = c("px"),
    dpi = 100,
    limitsize = TRUE,
    bg = NULL,
    create.dir = FALSE,
  )
  
  print(paste("Generando gráfico de barras para los Indicadores Compuestos ", i, " de ", nrow(ScoreCards_df), ": ", nombre, sep = "")) # Para ver el progreso
}

# Tiempo de procesamiento:
End_Time_S7_P6 <- Sys.time()
print(paste("6. Tiempo de creación de gráfica de Indicadores Compuestos: ", as.duration(End_Time_S7_P6 - Start_Time_S7_P6)))

# ####
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ::: 7. Creación de mapa principal de la UPL                                :::
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ####
#
Start_Time_S7_P7 <- Sys.time()

# Preparamos un sf de Accesibilidad a nivel manzanas para todos los Scenarios:
Shape_Accesibilidad_Mapas_sf <- bind_rows(
  # Sf para manzanas del Scn1:
  Acceso_Equipamientos_MNZ_Scn1_sf |>
    mutate(
      cutoff = as.character(cutoff),
      SCN = "Scn1",
      ID = str_c(SCN, "_", cutoff, "min_UPL", COD_UPL),
      HQSL_SF = Social_Functions,
      HQSL_CI = Composite_Indicators
    ) |>
    select(ID, COD_MNZ, COD_UPL, NOM_UPL, cutoff, SCN, Living:Enjoying, Well_being:Environmental_Impact, Total_Amenities, HQSL_SF, HQSL_CI),
  # Sf para manzanas del Scn2:
  Acceso_Equipamientos_MNZ_Scn2_sf |>
    mutate(
      cutoff = as.character(cutoff),
      SCN = "Scn2",
      ID = str_c(SCN, "_", cutoff, "min_UPL", COD_UPL),
      HQSL_SF = Social_Functions,
      HQSL_CI = Composite_Indicators
    ) |>
    select(ID, COD_MNZ, COD_UPL, NOM_UPL, cutoff, SCN, Living:Enjoying, Well_being:Environmental_Impact, Total_Amenities, HQSL_SF, HQSL_CI),
  # Sf para manzanas del Scn3:
  Acceso_Equipamientos_MNZ_Scn3_sf |>
    mutate(
      cutoff = as.character(cutoff),
      SCN = "Scn3",
      ID = str_c(SCN, "_", cutoff, "min_UPL", COD_UPL),
      HQSL_SF = Social_Functions,
      HQSL_CI = Composite_Indicators
    ) |>
    select(ID, COD_MNZ, COD_UPL, NOM_UPL, cutoff, SCN, Living:Enjoying, Well_being:Environmental_Impact, Total_Amenities, HQSL_SF, HQSL_CI),
  # Sf para manzanas del Scn4:
  Acceso_Equipamientos_MNZ_Scn4_sf |>
    mutate(
      cutoff = as.character(cutoff),
      SCN = "Scn4",
      ID = str_c(SCN, "_", cutoff, "min_UPL", COD_UPL),
      HQSL_SF = Social_Functions,
      HQSL_CI = Composite_Indicators
    ) |>
    select(ID, COD_MNZ, COD_UPL, NOM_UPL, cutoff, SCN, Living:Enjoying, Well_being:Environmental_Impact, Total_Amenities, HQSL_SF, HQSL_CI),
  # Sf para manzanas del Scn5:
  Acceso_Equipamientos_MNZ_Scn5_sf |>
    mutate(
      cutoff = as.character(cutoff),
      SCN = "Scn5",
      ID = str_c(SCN, "_", cutoff, "min_UPL", COD_UPL),
      HQSL_SF = Social_Functions,
      HQSL_CI = Composite_Indicators
    ) |>
    select(ID, COD_MNZ, COD_UPL, NOM_UPL, cutoff, SCN, Living:Enjoying, Well_being:Environmental_Impact, Total_Amenities, HQSL_SF, HQSL_CI),
  # Sf para manzanas del Scn6:
  Acceso_Equipamientos_MNZ_Scn6_sf |>
    mutate(
      cutoff = as.character(cutoff),
      SCN = "Scn6",
      ID = str_c(SCN, "_", cutoff, "min_UPL", COD_UPL),
      HQSL_SF = Social_Functions,
      HQSL_CI = Composite_Indicators
    ) |>
    select(ID, COD_MNZ, COD_UPL, NOM_UPL, cutoff, SCN, Living:Enjoying, Well_being:Environmental_Impact, Total_Amenities, HQSL_SF, HQSL_CI),
  # Sf para manzanas del Scn7:
  Acceso_Equipamientos_MNZ_Scn7_sf |>
    mutate(
      cutoff = as.character(cutoff),
      SCN = "Scn7",
      ID = str_c(SCN, "_", cutoff, "min_UPL", COD_UPL),
      HQSL_SF = Social_Functions,
      HQSL_CI = Composite_Indicators
    ) |>
    select(ID, COD_MNZ, COD_UPL, NOM_UPL, cutoff, SCN, Living:Enjoying, Well_being:Environmental_Impact, Total_Amenities, HQSL_SF, HQSL_CI),
)
Shape_Accesibilidad_Mapas_sf


# --- EL CICLO (LOOP) ---

# Antes, creamos una carpeta Caché, para guardar mapas descargados:
Directorio_Cache <- "./Data/2_Processing/7_HQSL/3_Mapas_UPL/Mapas_Cache/"

# Recorremos el sf para hacer los mapas:
for (i in 1:nrow(ScoreCards_df)) {
  
  ID_Mapa <- ScoreCards_sf[i,]$ID # Para pruebas
  UPL_Actual <- ScoreCards_sf[i,]$COD_UPL
  
  # Mensajito para ver algo:
  print(paste("Generando mapa principal de UP para ScoreCard ", i, " de ", nrow(ScoreCards_df), ": Mapa_HQSL_", ID_Mapa, ".png", sep = "")) # Para ver el progreso
  
# Seleccionamos solo las manzanas del ID_Mapa de interés:
  Shape_Mapa_UPL_Color <- Shape_Accesibilidad_Mapas_sf |>
    filter(ID == ID_Mapa) |>
    st_transform(crs = 3857) # 3857: Proyección CartoDB/Google
  Shape_Mapa_UPL_Color
  
  # 1. Obtenemos el bbox y lo convertimos a números puros
  BBox <- st_bbox(Shape_Mapa_UPL_Color)
  
  Ancho <- BBox["xmax"] - BBox["xmin"]
  Alto  <- BBox["ymax"] - BBox["ymin"]
  
  Lado_Final <- max(Ancho, Alto) * 1.2
  
  Centro_x <- as.numeric((BBox["xmin"] + BBox["xmax"]) / 2)
  Centro_y <- as.numeric((BBox["ymin"] + BBox["ymax"]) / 2)
  
  # Asegurar tamaño mínimo (en metros, EPSG:3857)
  Centro_x
  Centro_y
  Lado_Final
  
  BBox_Cuadrado <- st_bbox(
    c(
      xmin = Centro_x - Lado_Final / 2,
      xmax = Centro_x + Lado_Final / 2,
      ymin = Centro_y - Lado_Final / 2,
      ymax = Centro_y + Lado_Final / 2
    ),
    crs = st_crs(Shape_Mapa_UPL_Color)
  )
  
  # 5. Convertir a polígono para el "velo blanco" o recorte
  Zona_Cuadrada <- st_as_sfc(BBox_Cuadrado)
  
  Plot_Mapa <- ggplot() +
    
    # 1. FONDO: CartoDB Positron No Labels
    # Usamos la URL directa para asegurarnos que sea la versión sin etiquetas
    annotation_map_tile(
      type = "https://a.basemaps.cartocdn.com/light_nolabels/${z}/${x}/${y}.png",
      zoom = 15, # La mejor resolución es 15
      alpha = 1,
      cachedir = Directorio_Cache
    ) +
    
    # 2. CAPA 1: La geometría:
    geom_sf(data = Shape_Mapa_UPL_Color, 
            aes(fill = HQSL_SF), 
            color = NA,        # Color del borde de las manzanas
            lwd = 0.01,        # Grosor del borde
            alpha = 0.9) +     # Transparencia
    
    # 3. Alicamos los colores
    scale_fill_gradient(
      low = "bisque3", 
      high = "darkgreen", 
      name = "Access to amenities"
    ) +
    
    # 4. EL RECORTE CUADRADO 
    coord_sf(
      xlim = c(BBox_Cuadrado["xmin"], BBox_Cuadrado["xmin"] + (Lado_Final * 1.57)),
      ylim = c(BBox_Cuadrado["ymin"], BBox_Cuadrado["ymax"]),
      expand = FALSE
    ) +
    
    # 5. ESTÉTICA (Quitar ejes y fondo gris)
    theme_void() +
    theme(
      legend.position = "none",
      plot.margin = margin(0,0,0,0)
    )
  Plot_Mapa
  
  # Guardamos el mapa como imagen:
  ggsave(
    filename = paste("Mapa_HQSL_", ID_Mapa, ".png", sep =""),
    plot = Plot_Mapa,
    device = NULL,
    path = "./Data/2_Processing/7_HQSL/3_Mapas_UPL/",
    scale = 1,
    width = 940,
    height = 600,
    units = c("px"),
    dpi = 150,
    limitsize = TRUE,
    bg = NULL,
    create.dir = FALSE,
  )
  
}

# Tiempo de procesamiento:
End_Time_S7_P7 <- Sys.time()
print(paste("7. Tiempo de creación de mapa principal de la UPL: ", as.duration(End_Time_S7_P7 - Start_Time_S7_P7)))

# ####
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ::: 8. Creación de mapa de ubicación de UPL                                :::
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ####
#
Start_Time_S7_P8 <- Sys.time()

# Alistamos el shape a plotear:
Shape_Plotear <- Shape_Censo_UPLs |>
  filter(ZONA_UPL != "Rural") |>
  mutate(
    centroides = st_centroid(geom),
    X = st_coordinates(centroides)[,1],
    Y = st_coordinates(centroides)[,2]
  ) |>
  select(COD_UPL, X, Y)
Shape_Plotear

# Iniciamos el ciclo para recorrerlo:
for (i in 1:nrow(Shape_Plotear)) {
  
  UPL_Actual <- Shape_Plotear[i,]$COD_UPL
  #UPL_Actual = "10"

  # Mensajito para ver algo mientras:
  cat(paste("\nGenerando mapa de ubicación ", i, "/", nrow(Shape_Plotear), ": UPL", UPL_Actual, "... ", sep = ""))

  Plot_Mapa <- ggplot() +
    # CAPA 1: Mapa base (Todas las UPLs en gris)
    geom_sf(data = Shape_Plotear, 
            fill = "gray",      # Color de relleno gris
            color = "white",      # Bordes blancos
            size = 0.5) +         # Grosor de línea
    
    # CAPA 2: La UPL seleccionada (Color escogido)
    geom_sf(data = Shape_Plotear |> filter(COD_UPL == UPL_Actual), 
            fill = "#69b3a2", 
            color = "white",      # Borde negro para resaltar más
            size = 0.5) +
    
    # CAPA 3: La etiqueta (Última capa para que quede encima de todo)
    geom_sf_text(data = Shape_Plotear,
                 aes(label = COD_UPL),
                 color = "white",
                 fontface = "bold",
                 family = "Roboto Black", # Tipo de fuente
                 size = 5) +
    
    # Capa de etiquetas:
    # geom_text(
    #   aes(x = X, y = Y, label = COD_UPL), # Mapea coordenadas y texto
    #   color = "white",        # Color del texto
    #   size = 6,               # Tamaño del texto
    #   family = "Roboto Black" # Tipo de fuente
    # ) +
    # 
    # Estética limpia (sin ejes ni fondo gris de ggplot)
    theme_void()
  Plot_Mapa
  
  # Guardamos el mapa como imagen:
  ggsave(
    filename = paste("Mapa_Ubicacion_UPL", UPL_Actual, ".png", sep =""),
    plot = Plot_Mapa,
    device = NULL,
    path = "./Data/2_Processing/7_HQSL/4_Mapas_Ubicacion_UPL/",
    scale = 1,
    width = 500,
    height = 800,
    units = c("px"),
    dpi = 150,
    limitsize = TRUE,
    bg = NULL,
    create.dir = FALSE,
  )
  cat("Done.")

}

# Tiempo de procesamiento:
End_Time_S7_P8 <- Sys.time()
print(paste("8. Tiempo de creación de mapa de ubicación de UPL: ", as.duration(End_Time_S7_P8 - Start_Time_S7_P8)))

# ####
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ::: 9. Compilación del ScoreCard                                           :::
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ####
#
Start_Time_S7_P9 <- Sys.time()

# Función personalizada para dibujar rectángulos en un pipeline:
dibujar_rectangulo <- function(imagen, x_izq, y_arr, x_der, y_aba, 
                               color_borde = "gray", 
                               grosor = 2, 
                               color_fondo = "transparent") {
  
  # 1. Abrimos el modo de dibujo
  img_temp <- image_draw(imagen)
  
  # 2. Dibujamos usando la función base rect()
  rect(xleft = x_izq, 
       ytop = y_arr, 
       xright = x_der, 
       ybottom = y_aba,
       border = color_borde,  # Color de la línea
       lwd = grosor,          # Grosor de la línea
       col = color_fondo)     # Color de relleno (fill)
  
  # 3. Cerramos para guardar cambios
  dev.off()
  
  # 4. Retornamos la imagen para seguir el pipeline
  return(img_temp)
}

# 1. Creamos el lienzo vacío (el fondo):
Lienzo <- image_blank(width = 980, height = 1315, color = "white") |> #980x1255
  image_border(color = "#595959", geometry = "10x10", operator = "copy")
Lienzo

# 2. Cargaamos los iconos e imagenes estáticas:
Icono_Habitantes <- image_read("./Data/1_Sources/7_HQSL/Habitantes.png")
Icono_Densidad <- image_read("./Data/1_Sources/7_HQSL/Densidad.png")
Icono_Area <- image_read("./Data/1_Sources/7_HQSL/Area.png")
Barra_Escala <- image_read("./Data/1_Sources/7_HQSL/Escala.png")
Brujula <- image_read("./Data/1_Sources/7_HQSL/Compass.png")
Logo_TUB <- image_read("./Data/1_Sources/7_HQSL/UBA.png")
Logo_UBA <- image_read("./Data/1_Sources/7_HQSL/TUB.png")

# Para tests:
i <- 1

# --- 3. EL CICLO (LOOP) ---
for (i in 1:nrow(ScoreCards_sf)) {
  
  ID_Actual <- ScoreCards_sf[i,]$ID
  SCN_Actual <- ScoreCards_sf[i,]$SCN
  cutoff_Actual <- ScoreCards_sf[i,]$cutoff
  COD_UPL_Actual <- ScoreCards_sf[i,]$COD_UPL
  NOM_UPL_Actual <- ScoreCards_sf[i,]$NOM_UPL
  HQSL_SCORE_UPL_Actual <- paste("", round(ScoreCards_sf[i,]$HQSL_SF), sep = "") 
  # Para tests:
  #SCN_Actual <- "Scn6"
  #HQSL_SCORE_UPL_Actual <- "Score: 28879" # El Máximo Score es 28879
  
  # Mensajito para ver algo mientras:
  cat(paste("\nCompilando ScoreCard ", i, "/", length(ScoreCards_sf$ID), ": Score_Card_", ScoreCards_sf[i,]$ID, ".png ... ", sep = ""))
  
  # Textos del encabezado:
  Titulo <- "HQSL ScoreCard"
  Titulo_UPL <- paste("UPL ", COD_UPL_Actual, ": ", NOM_UPL_Actual, sep ="")
  Subtitulo_Escenario <- paste("Scenario ", str_replace(SCN_Actual, "Scn", ""), ":", sep = "")
  Descripcion_Escenario <- case_when(
    SCN_Actual == "Scn1" ~ "Current status",
    SCN_Actual == "Scn2" ~ "Bogotá First Metro Line",
    SCN_Actual == "Scn3" ~ "Proximity in the immediate vicinity of the PLMB",
    SCN_Actual == "Scn4" ~ "PLMB + Full proximity",
    SCN_Actual == "Scn5" ~ "Implementation of the railway network",
    SCN_Actual == "Scn6" ~ "Proximity in the immediate vicinity of the railway network",
    SCN_Actual == "Scn7" ~ "Railway network + Full proximity",
    TRUE ~ "Unknown scenario" # El equivalente a 'else' (si nada de lo anterior cumple)
  )
  Subtitulo_Accesibilidad <- paste("", cutoff_Actual, "-min accesibility", sep ="")
  
  # Títulos internos:
  Titulo_Basic_Information <- "Basic Information"
  Titulo_HQSL <- "HQSL Index"
  Titulo_Composite <- "Overall Well-being Impact"
  Titulo_City_Map <- "City Map"
  Titulo_UPL_Map <- "UPL Map"
  
  # Textos dentro del ScoreCard:
  Texto_Habitantes_1 <- paste(ScoreCards_sf[i,]$PERSONAS_UPL, " inhabitants", sep = "")
  Texto_Habitantes_2 <- paste("(", ScoreCards_sf[i,]$PORCENTAJE_PERSONAS_BOG, "% of Bogota)", sep = "")
  Texto_Densidad_1 <- paste(ScoreCards_sf[i,]$DENSIDAD_UPL, " inhabitants/km2", sep = "")
  Texto_Densidad_2 <- paste("(", round(Shape_Censo_Municipios$DENSIDAD_MPIO), " inhabitants/km2 for Bogota)", sep = "")
  Texto_Area_1 <- paste(ScoreCards_sf[i,]$AREA_UPL, " km2", sep = "")
  Texto_Area_2 <- paste("(", ScoreCards_sf[i,]$PORCENTAJE_AREA_BOG, "% of Bogota)", sep = "")
  Texto_Anotacion_Radar <- "*Percentage of the\nhighest Social Function\nscore in the same scenario."
  Texto_Leyenda_Min <- round(Shape_Accesibilidad_Mapas_sf |> st_drop_geometry() |> filter(ID == ID_Actual) |> select(HQSL_SF) |> min())
  Texto_Leyenda_Max <- round(Shape_Accesibilidad_Mapas_sf |> st_drop_geometry() |> filter(ID == ID_Actual) |> select(HQSL_SF) |> max())
  Texto_Leyenda_Med <- round((Texto_Leyenda_Max + Texto_Leyenda_Min)/2)
  
  # Imagenes creadas en procesos anteriores
  Grafico_Indicadores_Compuestos <- image_read(paste0("./Data/2_Processing/7_HQSL/2_Indicadores/Barras_Indicadores_", ID_Actual, ".png"))
  Grafico_Radar <- image_read(paste0("./Data/2_Processing/7_HQSL/1_Radares/Radar_", ID_Actual, ".png"))
  Mapa_UPL <- image_read(paste0("./Data/2_Processing/7_HQSL/3_Mapas_UPL/Mapa_HQSL_", ID_Actual, ".png"))
  Mapa_Ubicacion_UPL <- image_read(paste0("./Data/2_Processing/7_HQSL/4_Mapas_Ubicacion_UPL/Mapa_Ubicacion_UPL", COD_UPL_Actual, ".png"))
  
  # 3. Componer la imagen sobre el lienzo
  # 'offset' define la posición "+X+Y" desde la esquina superior izquierda
  Imagen_Final <- Lienzo |>
    
    # Agregamos texto de encabezados
    dibujar_rectangulo(x_izq = 0, y_arr = 0, x_der = 1000, y_aba = 70, color_borde = "#595959", grosor = 1, color_fondo = "#595959") |>
    image_annotate(Titulo, size = 40, location = "+20+5", color = "white", weight = 800, kerning = -3) |>
    image_annotate(Titulo_UPL, gravity = "northeast", size = 25, location = "+20+15", color = "white", weight = 800, kerning = -2) |>
    image_annotate(Subtitulo_Escenario, size = 25, location = "+30+80", color = "#595959", weight = 800, kerning = -2) |>
    image_annotate(Descripcion_Escenario, size = 25, location = "+30+105", color = "#595959", weight = 800, kerning = -2) |>
    image_annotate(Subtitulo_Accesibilidad, size = 20, location = "+30+133", color = "#595959", weight = 800, kerning = -2) |>
    
    # Agregamos información de estadísticas
    dibujar_rectangulo(x_izq = 0, y_arr = 175, x_der = 260, y_aba = 200, color_borde = "#595959", grosor = 1, color_fondo = "#595959") |>
    image_annotate(Titulo_Basic_Information, size = 20, location = "+50+172", color = "white", weight = 800, kerning = -2) |>
    image_composite(image_scale(Icono_Habitantes, "40"), offset = "+100+225") |>
    image_composite(image_scale(Icono_Densidad, "40"), offset = "+100+275") |>
    image_composite(image_scale(Icono_Area, "40"), offset = "+100+325") |>
    image_annotate(Texto_Habitantes_1, size = 20, location = "+170+230", color = "#595959", weight = 700) |>
    image_annotate(Texto_Habitantes_2, size = 12, location = "+170+250", color = "#595959", weight = 400) |>
    image_annotate(Texto_Densidad_1, size = 20, location = "+170+280", color = "#595959", weight = 700) |>
    image_annotate(Texto_Densidad_2, size = 12, location = "+170+300", color = "#595959", weight = 400) |>
    image_annotate(Texto_Area_1, size = 20, location = "+170+330", color = "#595959", weight = 700) |>
    image_annotate(Texto_Area_2, size = 12, location = "+170+350", color = "#595959", weight = 400) |>
    
    # Agregamos Gráfica de Radar
    dibujar_rectangulo(x_izq = 800, y_arr = 100, x_der = 1000, y_aba = 125, color_borde = "#595959", grosor = 1, color_fondo = "#595959") |>
    image_annotate(Titulo_HQSL, size = 20, gravity = "northeast", location = "+50+97", color = "white", weight = 800, kerning = -2) |>
    image_composite(image_scale(Grafico_Radar, "450"), offset = "+515+70") |>
    image_annotate("HQSL Score:", size = 18, gravity = "northeast", location = "+50+140", color = "#595959", weight = 800, kerning = -2) |>
    image_annotate(HQSL_SCORE_UPL_Actual, size = 24, gravity = "northeast", location = "+50+160", color = "#69b3a2", weight = 800, kerning = -2) |>
    image_annotate(Texto_Anotacion_Radar, size = 10, gravity = "northeast", location = "+30+480", color = "#595959", weight = 500, kerning = 0) |>
    
    # Agregamos Información de Indicadores Compuestos:
    dibujar_rectangulo(x_izq = 0, y_arr = 385, x_der = 330, y_aba = 410, color_borde = "#595959", grosor = 1, color_fondo = "#595959") |>
    image_annotate(Titulo_Composite, size = 20, location = "+50+382", color = "white", weight = 800, kerning = -2) |>
    image_composite(image_scale(Grafico_Indicadores_Compuestos, "450"), offset = "+30+425") |>

    # Agregamos el mapa principal
    dibujar_rectangulo(x_izq = 0, y_arr = 600, x_der = 180, y_aba = 625, color_borde = "#595959", grosor = 1, color_fondo = "#595959") |>
    image_annotate(Titulo_UPL_Map, size = 20, location = "+50+597", color = "white", weight = 800, kerning = -2) |>
    dibujar_rectangulo(x_izq = 30, y_arr = 645, x_der = 970, y_aba = 1245, color_borde = "lightgray", grosor = 10, color_fondo = "transparent") |>
    image_composite(image_scale(Mapa_UPL, "940"), offset = "+30+645") |>
    image_composite(image_scale(Barra_Escala, "x100"), offset = "+600+1123") |>
    image_annotate("Amenity access", size = 15, location = "+600+1100", color = "#595959", weight = 700, kerning = -1) |>
    image_annotate(Texto_Leyenda_Max, size = 12, location = "+620+1120", color = "#595959", weight = 700, kerning = -1) |>
    image_annotate(Texto_Leyenda_Med, size = 12, location = "+620+1165", color = "#595959", weight = 700, kerning = -1) |>
    image_annotate(Texto_Leyenda_Min, size = 12, location = "+620+1210", color = "#595959", weight = 700, kerning = -1) |>
    
    # Agregamos mapa de Ubicación
    image_composite(image_scale(Mapa_Ubicacion_UPL, "280"), gravity = "southeast", offset = "+50+200") |>
    image_composite(image_scale(Brujula, "30"), gravity = "southeast", offset = "+50+110") |>
    
    # Pie de pagina
    # dibujar_rectangulo(x_izq = 620, y_arr = 670, x_der = 950, y_aba = 820, color_borde = "gray", grosor = 2, color_fondo = "white") |>
    image_composite(image_scale(Logo_UBA, "x50"), offset = "+50+1260") |>
    image_composite(image_scale(Logo_TUB, "x50"), offset = "+135+1260") |>
    image_annotate("Stadt- und Regionalplanung, M.Sc. Dual Degree", size = 15, location = "+200+1260", color = "#595959", weight = 700, kerning = -1) |>
    image_annotate("Institut für Stadt- und Regionalplanung | Berlin", size = 15, location = "+200+1275", color = "#595959", weight = 700, kerning = -1) |>
    image_annotate("Facultad de Arquitectura, Diseño y Urbanismo | Buenos Aires", size = 15, location = "+200+1291", color = "#595959", weight = 700, kerning = -1) |>
    image_annotate("Santiago Silvera M.", size = 20, gravity = "northeast", location = "+50+1260", color = "#595959", weight = 700, kerning = -1) |>
    image_annotate("Bogotá | 2026", size = 20, gravity = "northeast", location = "+50+1280", color = "#595959", weight = 700, kerning = -1) |>

    # Volvemos transparente:
    image_fill("white", point = "+980+1240", fuzz = 5)
  Imagen_Final
  
  # 5. Guardar la imagen nueva:
  image_write(Imagen_Final, path = paste("./Data/3_Results/7_HQSL/1_ScoreCard/Score_Card_", ID_Actual, ".png", sep = ""), format = "png")

  cat("Done.")
}

# Tiempo de procesamiento:
End_Time_S7_P9 <- Sys.time()
print(paste("9. Tiempo de compilación del ScoreCard: ", as.duration(End_Time_S7_P9 - Start_Time_S7_P9)))


# ####
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ::: 10. Creación de gráficas de evolución del HQSL                         :::
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ####
#
Start_Time_S7_P10 <- Sys.time()

# Para garantizar el orden de escenarios
Orden_Escenarios <- unique(ScoreCards_sf$SCN)

# Para la fuente
showtext_auto()

# Para tests:
Tiempo_Min <- 15

# Evaluamos para 15 y 30 minutos mediante un ciclo para ahorrar trabajo:
for (Tiempo_Min in c(15, 30)) {
  
  # Apartamos el df a plotear
  df_plotear <- ScoreCards_sf |>
    st_drop_geometry() |>
    filter(cutoff == Tiempo_Min) |>
    mutate(
      SCN = factor(SCN, levels = Orden_Escenarios),
      Tiempo_Min = cutoff
      ) |>
    select(COD_UPL, SCN, HQSL_SF, Tiempo_Min)
  df_plotear
  
  # Creamos el gráfico de líneas para el puntaje de HQSL:
  Plot_Graficas <- ggplot(df_plotear, aes(x = SCN, y = HQSL_SF, group = COD_UPL)) +
    # Línea conectando los puntos para ver la evolución
    geom_line(color = "#0073C2", linewidth = 1) + 
    # Puntos para marcar el valor exacto en cada escenario
    geom_point(color = "darkblue", size = 1.5) + 
    # Facet_wrap crea un gráfico por cada UPL. 
    # 'scales = "free_y"' permite que cada UPL tenga su propia escala si los rangos son muy distintos.
    facet_wrap(~ COD_UPL, scales = "free_y", ncol = 6) + 
    labs(
      # title = "Evolución del Indicador HQSL por UPL",
      # subtitle = "Comparación a través de los 7 escenarios",
      x = "Escenario",
      y = "Puntaje HQSL"
    ) +
    theme_minimal(base_family = "Roboto", base_size = 7) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1), # Rotar etiquetas del eje X
      strip.text = element_text(face = "bold") # Negrita en los títulos de cada UPL
    )
  Plot_Graficas
  
  # Gráfico de Calor por si queda mejor:
  Plot_Graficas_Calor <- ggplot(df_plotear, aes(x = SCN, y = as.factor(COD_UPL), fill = HQSL_SF)) +
    # Bordes blancos para separar
    geom_tile(color = "white") +
    # Textos encimade los cuadritos
    geom_text(
      aes(label = round(HQSL_SF, 0), 
          color = ifelse(
            Tiempo_Min == 15,
            ifelse(HQSL_SF > 350, "white", "#595959"),
            ifelse(HQSL_SF > 5000, "white", "#595959")
          )
        ),
      size = 2.5,
      family = "Roboto",
      fontface = "bold",
      show.legend = FALSE # Para no crear una leyenda de colores de texto
    ) +
    scale_color_identity() + # Necesario para que R entienda que "white"/"black" son colores reales
    # Establecemos escala de color
    scale_fill_gradientn(
      colors = c("gray90", "darkcyan")
#      values = scales::rescale(c(0, 3000000))
    ) +
    labs(
      # title = "Mapa de Calor: HQSL por UPL y Escenario",
      x = "Escenario",
      y = "Código UPL",
      fill = "HQSL"
    ) +
    theme_minimal(base_family = "Roboto", base_size = 7) +
    theme(
      legend.position = "none",
      # Texto de los títulos de ejes
      axis.title = element_text(
        family = "Roboto",  # Tu fuente personalizada
        color = "#595959",  # El color (puedes usar nombres o HEX como "#FF5733")
        size = 10,          # Tamaño de la letra
        face = "bold"       # "bold" para negrita, "italic" para cursiva, o "plain"
      ),
      # Negrita a las etiquetas:
      axis.text.x = element_text(angle = 45, hjust = 1),
      axis.text = element_text(face = "bold", size = 8), 
      # Negrita a los títulos:
      panel.background = element_blank(), # Quita el fondo del área de dibujo
      plot.background = element_blank(),  # Quita el fondo de todo el recuadro
      legend.background = element_blank() # Asegura que la leyenda no tenga fondo
      )
  Plot_Graficas_Calor
  
  # Establecemos ruta y nombre al archivo:
  Nombre_Archivo <- paste("Grafica_Evolucion_HQSL_", Tiempo_Min, "min.png", sep ="")
  Nombre_Archivo_Calor <- paste("Grafica_Calor_Evolucion_HQSL_", Tiempo_Min, "min.png", sep ="")
  Ruta_Archivo <- "./Data/3_Results/7_HQSL/2_Diferencia_Escenarios/"
  
  # Guardamos el gráficos como imagen:
  ggsave(
    filename = Nombre_Archivo,
    plot = Plot_Graficas,
    device = NULL,
    path = Ruta_Archivo,
    scale = 1,
    width = 1080,
    height = 1080,
    units = c("px"),
    dpi = 150,
    limitsize = TRUE,
    bg = NULL,
    create.dir = FALSE,
  )
  # Grafico de Calor:
  ggsave(
    filename = Nombre_Archivo_Calor,
    plot = Plot_Graficas_Calor,
    device = NULL,
    path = Ruta_Archivo,
    scale = 1,
    width = 1080,
    height = 1280,
    units = c("px"),
    dpi = 200,
    limitsize = TRUE,
    bg = NULL,
    create.dir = FALSE,
  )
  
  # Ajustar imagenes:
  image_write(image_trim(image_read(paste0(Ruta_Archivo, Nombre_Archivo))), path = paste0(Ruta_Archivo, Nombre_Archivo), format = "png")
  image_write(image_trim(image_read(paste0(Ruta_Archivo, Nombre_Archivo_Calor))), path = paste0(Ruta_Archivo, Nombre_Archivo_Calor), format = "png")
}

# Tiempo de procesamiento:
End_Time_S7_P10 <- Sys.time()
print(paste("10. Tiempo de creación de gráficas de evolución del HQSL: ", as.duration(End_Time_S7_P10 - Start_Time_S7_P10)))

# ####
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ::: 10. Tiempos de procesamiento                                           :::
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ####
#

# Tiempo de procesamiento:
End_Time_S7_P0 <- Sys.time()

print(paste("1. Tiempo de inicialización: ", as.duration(End_Time_S7_P1 - Start_Time_S7_P1)))
print(paste("2. Tiempo de carga de datos de origen: ", as.duration(End_Time_S7_P2 - Start_Time_S7_P2)))
print(paste("3. Tiempo de cálculo del HQSL para todos los escenarios: ", as.duration(End_Time_S7_P3 - Start_Time_S7_P3)))
print(paste("4. Tiempo de creación de info para ScoreCards: ", as.duration(End_Time_S7_P4 - Start_Time_S7_P4)))
print(paste("5. Tiempo de generación de gráficos de radar: ", as.duration(End_Time_S7_P5 - Start_Time_S7_P5)))
print(paste("6. Tiempo de creación de gráfica de Indicadores Compuestos: ", as.duration(End_Time_S7_P6 - Start_Time_S7_P6)))
print(paste("7. Tiempo de creación de mapa principal de la UPL: ", as.duration(End_Time_S7_P7 - Start_Time_S7_P7)))
print(paste("8. Tiempo de creación de mapa de ubicación de UPL: ", as.duration(End_Time_S7_P8 - Start_Time_S7_P8)))
print(paste("9. Tiempo de compilación del ScoreCard: ", as.duration(End_Time_S7_P9 - Start_Time_S7_P9)))
print(paste("10. Tiempo de creación de gráficas de evolución del HQSL: ", as.duration(End_Time_S7_P10 - Start_Time_S7_P10)))
print(paste("11. Tiempo total de procesamiento: ", as.duration(End_Time_S7_P0 - Start_Time_S7_P0)))
