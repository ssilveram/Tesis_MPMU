# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# :::                                                                        :::
# :::                           SCRIPT MISCELANEO                            :::
# :::                                                                        :::
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#
# Nombre: 9_Misc.R
#
# Descripcion: Este Script tiene como objetivo realizar pequeñas tareas para las
# cuales no se justifica la creación de un script completo.

# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# :::                      Contenido de este Script                          :::
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#####
#
# ####
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ::: 1. Inicialización                                                      :::
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ####
#

# Cargamos el script de inicialización general
source("0_Initialization.R")

# ####
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ::: 2. Gráfica INRIX                                                       :::
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ####

# 1. Cargar fuentes
loadfonts(device = "win")

# --- Grafica de INRIX 2023 ---

# 2. Datos de gráfica
Inrix_2023 <- data.frame(
  CIUDAD = c("Londres", "Chicago", "París", "Boston", "Bogotá", "Palermo", "Toronto", "Nueva York", "Monterrey", "Filadelfia"),
  CITY = c("London", "Chicago", "Paris", "Boston", "Bogota", "Palermo", "Toronto", "New York", "Monterrey", "Philadelfia"),
  HORAS  = c(156, 155, 138, 134, 122, 121, 118, 117, 116, 114)
)
Inrix_2023

# 3. Crear variable de resalte: Bogotá vs. resto
Inrix_2023$HIGHLIGHT <- ifelse(Inrix_2023$CIUDAD == "Bogotá", "Destacado", "Otros")
Inrix_2023

# 4. Reordenar factor para que las barras vayan de menor a mayor
Inrix_2023$CIUDAD <- factor(Inrix_2023$CIUDAD, levels = Inrix_2023$CIUDAD[order(Inrix_2023$HORAS)])
Inrix_2023$CITY <- factor(Inrix_2023$CITY, levels = Inrix_2023$CITY[order(Inrix_2023$HORAS)])

# 5. Agregamos el número de posición al nombre de la ciudad
Inrix_2023 <- Inrix_2023 |>
  mutate(CIUDAD = paste(row_number(), ". ", CIUDAD, sep = ""),
         CITY = paste(row_number(), ". ", CITY, sep = ""))

# 6. Reordenar  otra vez el factor para que las barras vayan de menor a mayor
Inrix_2023$CIUDAD <- factor(Inrix_2023$CIUDAD, levels = Inrix_2023$CIUDAD[order(Inrix_2023$HORAS)])
Inrix_2023$CITY <- factor(Inrix_2023$CITY, levels = Inrix_2023$CITY[order(Inrix_2023$HORAS)])
Inrix_2023

# 7. Gráfica con estética con Bogotá resaltado”
Grafica_Inrix_2023_ES <- ggplot(Inrix_2023) +
  # Barras horizontales
  geom_col(aes(x = CIUDAD, y = HORAS, fill = HIGHLIGHT), width = 0.9) +
  # Nombre de la ciudad dentro de la barra, al inicio
  geom_text(aes(x = CIUDAD, y = 0, label = CIUDAD),
            hjust = 0,             # muy cerca del borde izquierdo
            nudge_y = max(Inrix_2023$HORAS) * 0.02,
            color = "white", size = 6, family = "Roboto", fontface = "bold") +
  # Valor de horas dentro de la barra, al final
  geom_text(aes(x = CIUDAD, y = HORAS, label = paste(HORAS, " hr", sep = "")),
            hjust = 1.2,             # muy cerca del borde derecho (1.2 es el mejor)
            color = "white", size = 6, family = "Roboto") +
  # Voltear ejes para barras horizontales
  coord_flip() +
  # Asignar colores: verde a Bogotá, gris al resto
  scale_fill_manual(values = c(
    Destacado = "darkred", #"#2E7D32",  # verde oscuro
    Otros      = "darkgray"  # "#B0BEC5"   # gris claro
  )) +
  # Eliminar ejes, cuadrículas y leyenda
  theme_void() +
  theme(legend.position = "none") +
  # Añadir espacio a la derecha para que no se corte el texto
  scale_y_continuous(limits = c(0, max(Inrix_2023$HORAS) * 1.1), expand = c(0,0))
Grafica_Inrix_2023_ES

Grafica_Inrix_2023_EN <- ggplot(Inrix_2023) +
  # Barras horizontales
  geom_col(aes(x = CITY, y = HORAS, fill = HIGHLIGHT), width = 0.9) +
  # Nombre de la ciudad dentro de la barra, al inicio
  geom_text(aes(x = CITY, y = 0, label = CITY),
            hjust = 0,             # muy cerca del borde izquierdo
            nudge_y = max(Inrix_2023$HORAS) * 0.02,
            color = "white", size = 6, family = "Roboto", fontface = "bold") +
  # Valor de horas dentro de la barra, al final
  geom_text(aes(x = CITY, y = HORAS, label = paste(HORAS, " hr", sep = "")),
            hjust = 1.2,             # muy cerca del borde derecho (1.2 es el mejor)
            color = "white", size = 6, family = "Roboto") +
  # Voltear ejes para barras horizontales
  coord_flip() +
  # Asignar colores: verde a Bogotá, gris al resto
  scale_fill_manual(values = c(
    Destacado = "darkred", #"#2E7D32",  # verde oscuro
    Otros      = "darkgray"  # "#B0BEC5"   # gris claro
  )) +
  # Eliminar ejes, cuadrículas y leyenda
  theme_void() +
  theme(legend.position = "none") +
  # Añadir espacio a la derecha para que no se corte el texto
  scale_y_continuous(limits = c(0, max(Inrix_2023$HORAS) * 1.1), expand = c(0,0))
Grafica_Inrix_2023_EN

# 8. Guardamos las gráficas como imágenes
ggsave(
  filename = "Graph_01_INRIX2023_ES_1280x720.png",
  plot = Grafica_Inrix_2023_ES,
  device = NULL,
  path = "./Data/3_Results/Graphs/",
  scale = 1,
  width = 1280,
  height = 720,
  units = c("px"),
  dpi = 150,
  limitsize = TRUE,
  bg = NULL,
  create.dir = FALSE,
)
ggsave(
  filename = "Graph_01_INRIX2023_EN_1280x720.png",
  plot = Grafica_Inrix_2023_EN,
  device = NULL,
  path = "./Data/3_Results/Graphs/",
  scale = 1,
  width = 1280,
  height = 720,
  units = c("px"),
  dpi = 150,
  limitsize = TRUE,
  bg = NULL,
  create.dir = FALSE,
)


# ####
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ::: 3. Gráfica Financial Times                                             :::
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ####

# 1. Datos de gráfica
Financial_Times_2023 <- data.frame(
  CIUDAD = c("Bogotá", "Bucarest", "Manila", "Bengaluru", "Lima", "Recife", "Mumbai", "Estambul", "Ciudad de México", "Puna"),
  CITY = c("Bogota", "Bucarest", "Manila", "Bengaluru", "Lima", "Recife", "Mumbai", "Istambul", "México City", "Pune"),
  HORAS  = c(132, 107, 103, 102, 101, 97, 91, 84, 80, 79)
)
Financial_Times_2023

# 2. Crear variable de resalte: Bogotá vs. resto
Financial_Times_2023$HIGHLIGHT <- ifelse(Financial_Times_2023$CIUDAD == "Bogotá", "Destacado", "Otros")
Financial_Times_2023

# 3. Reordenar factor para que las barras vayan de menor a mayor
Financial_Times_2023$CIUDAD <- factor(Financial_Times_2023$CIUDAD, levels = Financial_Times_2023$CIUDAD[order(Financial_Times_2023$HORAS)])
Financial_Times_2023$CITY <- factor(Financial_Times_2023$CITY, levels = Financial_Times_2023$CITY[order(Financial_Times_2023$HORAS)])
Financial_Times_2023

# 4. Agregamos el número de posición al nombre de la ciudad
Financial_Times_2023 <- Financial_Times_2023 |>
  mutate(CIUDAD = paste(row_number(), ". ", CIUDAD, sep = ""),
         CITY = paste(row_number(), ". ", CITY, sep = ""))

# 5. Reordenar  otra vez el factor para que las barras vayan de menor a mayor
Financial_Times_2023$CIUDAD <- factor(Financial_Times_2023$CIUDAD, levels = Financial_Times_2023$CIUDAD[order(Financial_Times_2023$HORAS)])
Financial_Times_2023$CITY <- factor(Financial_Times_2023$CITY, levels = Financial_Times_2023$CITY[order(Financial_Times_2023$HORAS)])
Financial_Times_2023

# 6. Gráfica con estética tipo con Bogotá resaltada
Grafica_Financial_Times_2023_ES <- ggplot(Financial_Times_2023) +
  # Barras horizontales
  geom_col(aes(x = CIUDAD, y = HORAS, fill = HIGHLIGHT), width = 0.9) +
  # Nombre de la ciudad dentro de la barra, al inicio
  geom_text(aes(x = CIUDAD, y = 0, label = CIUDAD),
            hjust = 0,             # muy cerca del borde izquierdo
            nudge_y = max(Financial_Times_2023$HORAS) * 0.02,
            color = "white", size = 6, family = "Roboto ExtraBold", fontface = "bold") +
  # Valor de horas dentro de la barra, al final
  geom_text(aes(x = CIUDAD, y = HORAS, label = paste(HORAS, " hr", sep = "")),
            hjust = 1.2,             # muy cerca del borde derecho (1.2 es el mejor)
            color = "white", size = 6, family = "Roboto") +
  # Voltear ejes para barras horizontales
  coord_flip() +
  # Asignar colores: verde a Bogotá, gris al resto
  scale_fill_manual(values = c(
    Destacado = "darkred", #"#2E7D32",  # verde oscuro
    Otros      = "darkgray"  # "#B0BEC5"   # gris claro
  )) +
  # Eliminar ejes, cuadrículas y leyenda
  theme_void() +
  theme(legend.position = "none") +
  # Añadir espacio a la derecha para que no se corte el texto
  scale_y_continuous(limits = c(0, max(Financial_Times_2023$HORAS) * 1.1), expand = c(0,0))
Grafica_Financial_Times_2023_ES

Grafica_Financial_Times_2023_EN <- ggplot(Financial_Times_2023) +
  # Barras horizontales
  geom_col(aes(x = CITY, y = HORAS, fill = HIGHLIGHT), width = 0.9) +
  # Nombre de la ciudad dentro de la barra, al inicio
  geom_text(aes(x = CITY, y = 0, label = CITY),
            hjust = 0,             # muy cerca del borde izquierdo
            nudge_y = max(Financial_Times_2023$HORAS) * 0.02,
            color = "white", size = 6, family = "Roboto ExtraBold", fontface = "bold") +
  # Valor de horas dentro de la barra, al final
  geom_text(aes(x = CITY, y = HORAS, label = paste(HORAS, " hr", sep = "")),
            hjust = 1.2,             # muy cerca del borde derecho (1.2 es el mejor)
            color = "white", size = 6, family = "Roboto") +
  # Voltear ejes para barras horizontales
  coord_flip() +
  # Asignar colores: verde a Bogotá, gris al resto
  scale_fill_manual(values = c(
    Destacado = "darkred", #"#2E7D32",  # verde oscuro
    Otros      = "darkgray"  # "#B0BEC5"   # gris claro
  )) +
  # Eliminar ejes, cuadrículas y leyenda
  theme_void() +
  theme(legend.position = "none") +
  # Añadir espacio a la derecha para que no se corte el texto
  scale_y_continuous(limits = c(0, max(Financial_Times_2023$HORAS) * 1.1), expand = c(0,0))
Grafica_Financial_Times_2023_EN

# 7. Guardamos las gráficas como imágenes
ggsave(
  filename = "Graph_02_FinancialTimes2023_ES_1280x720.png",
  plot = Grafica_Financial_Times_2023_ES,
  device = NULL,
  path = "./Data/3_Results/Graphs/",
  scale = 1,
  width = 1280,
  height = 720,
  units = c("px"),
  dpi = 150,
  limitsize = TRUE,
  bg = NULL,
  create.dir = FALSE,
)
ggsave(
  filename = "Graph_02_FinancialTimes2023_EN_1280x720.png",
  plot = Grafica_Financial_Times_2023_EN,
  device = NULL,
  path = "./Data/3_Results/Graphs/",
  scale = 1,
  width = 1280,
  height = 720,
  units = c("px"),
  dpi = 150,
  limitsize = TRUE,
  bg = NULL,
  create.dir = FALSE,
)

# ####
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ::: 4. Gráfica para estadísticas de población - Por sexo                   :::
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ####

# Cargamos datos de población
Censo_Personas <- as.data.frame(fread("./Data/2_Processing/2_Population/Censo_Personas.csv", sep=",", na = "null", colClasses=c("COD_LOC"="character", "COD_UPL"="character", "COD_UTAM"="character", "COD_ZAT"="character", "COD_SECT"="character", "COD_SECC"="character", "COD_HEX"="character", "COD_MNZ"="character", "COD_FAMILIA"="character", "COD_PERSONA"="character")))
Censo_Personas

# Población por género:
Distribución_Sexo <- Censo_Personas |>
  select(SEXO) |>
  group_by(SEXO) |>
  summarise(
    PERSONAS_SEXO = n()
  ) |>
  mutate(
    PORCENTAJE = paste(round(100*PERSONAS_SEXO/length(Censo_Personas$COD_PERSONA), digits = 2), " %", sep = ""),
  ) |>
  arrange(desc(PERSONAS_SEXO))

Distribución_Sexo$fraction <- Distribución_Sexo$PERSONAS_SEXO / length(Censo_Personas$COD_PERSONA)
Distribución_Sexo$ymax <- cumsum(Distribución_Sexo$fraction)
Distribución_Sexo$ymin <- c(0, head(Distribución_Sexo$ymax, n=-1))
Distribución_Sexo$labelPosition <- (Distribución_Sexo$ymax + Distribución_Sexo$ymin) / 2
Distribución_Sexo$label <- paste(Distribución_Sexo$PORCENTAJE) # Usamos solo el porcentaje para la etiqueta

# Ploteamos:
Plot_Sexo <- ggplot(Distribución_Sexo, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=SEXO)) +
  geom_rect() +
  
  # 1. Usar la paleta de colores manual para el relleno
  scale_fill_manual(values = c("M" = "deepskyblue4", "F" = "#993366")) +
  
  # 2. Por si queremos las etiquetas de texto que ya calculaste
  #geom_text(aes(x = 3.5, y = labelPosition, label = label), color = "white", size = 5) +
  
  coord_polar(theta="y") +
  xlim(c(1, 5)) +
  theme_void() +
  theme(
    legend.position = "none",
    text = element_text(color = "grey40", family = "Roboto Bold")
  )
Plot_Sexo

# Guardamos como imagen:
ggsave(
  filename = "Grafica_Distribucion_Sexo.png",
  plot = Plot_Sexo,
  device = NULL,
  path = "./Data/3_Results/9_Misc/",
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
Distribución_Sexo |>
  mutate(
    SEXO = if_else(SEXO == "M", "Masculino", "Femenino")
  ) |>
  select(SEXO, PERSONAS_SEXO, PORCENTAJE)


# ####
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ::: 5. Gráfica para estadísticas de población - Por edad                   :::
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ####

# Cargamos datos de población
Censo_Personas <- as.data.frame(fread("./Data/2_Processing/2_Population/Censo_Personas.csv", sep=",", na = "null", colClasses=c("COD_LOC"="character", "COD_UPL"="character", "COD_UTAM"="character", "COD_ZAT"="character", "COD_SECT"="character", "COD_SECC"="character", "COD_HEX"="character", "COD_MNZ"="character", "COD_FAMILIA"="character", "COD_PERSONA"="character")))
Censo_Personas

# Población por grupos de edad:
Distribucion_Edades <- Censo_Personas |>
  select(EDAD) |>
  group_by(EDAD) |>
  summarise(
    PERSONAS_EDAD = n()
  ) |>
  mutate(
    GRUPO_EDAD = case_when(
      EDAD %in% c("0-4", "5-9") ~ "1. Infantes",
      EDAD %in% c("10-14", "15-19") ~ "2. Adolescentes",
      EDAD %in% c("20-24", "25-29", "30-34") ~ "3. Jóvenes",
      EDAD %in% c("35-39", "40-44", "45-49", "50-54", "55-59", "60-64") ~ "4. Adultos",
      EDAD %in% c("65-69", "70-74", "75-79", "80-84", "85-89", "90-94", "95-99", "100+") ~ "5. Adultos mayores",
    ),
  ) |>
  group_by(GRUPO_EDAD) |>
  summarise(
    PERSONAS_EDAD = sum(PERSONAS_EDAD)
  ) |>
  mutate(
    PORCENTAJE = paste(round(100*PERSONAS_EDAD/length(Censo_Personas$COD_PERSONA), digits = 2), "%", sep = ""),
  ) |>
  arrange(GRUPO_EDAD)
Distribucion_Edades

# Ordenamos y asignamos factores
Distribucion_Edades$GRUPO_EDAD <- factor(Distribucion_Edades$GRUPO_EDAD, levels = Distribucion_Edades$GRUPO_EDAD[order(Distribucion_Edades$GRUPO_EDAD)])
Distribucion_Edades

# Creamos el plot para los grupos de edades:
Plot_Distribucion_Edades <- Distribucion_Edades |>
  ggplot() +
  # Barras horizontales
  geom_col(aes(x = fct_rev(GRUPO_EDAD), y = PERSONAS_EDAD), fill = "darkgray", width = 0.9) +
  # Valor de horas dentro de la barra, al final
  geom_text(aes(x = GRUPO_EDAD, y = PERSONAS_EDAD, label = PORCENTAJE),
            hjust = 1.2,             # muy cerca del borde derecho (1.2 es el mejor)
            color = "white", size = 6, family = "Roboto") +
  coord_flip() +
  # Eliminar ejes, cuadrículas y leyenda
  theme_void() +
  theme(legend.position = "none") +
  # Añadir espacio a la derecha para que no se corte el texto
  scale_y_continuous(limits = c(0, max(Distribucion_Edades$PERSONAS_EDAD) * 1.1), expand = c(0,0))
Plot_Distribucion_Edades

# # Guardamos como imagen:
ggsave(
  filename = "Grafica_Distribucion_Edades.png",
  plot = Plot_Distribucion_Edades,
  device = NULL,
  path = "./Data/3_Results/9_Misc/",
  scale = 1,
  width = 1000,
  height = 400,
  units = c("px"),
  dpi = 150,
  limitsize = TRUE,
  bg = NULL,
  create.dir = FALSE,
)

# Datos para poner manualmente en Word:
Distribucion_Edades


# ####
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ::: 6. Gráfica para estadísticas de población - Por oficio                 :::
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ####

# Cargamos datos de población
Censo_Personas <- as.data.frame(fread("./Data/2_Processing/2_Population/Censo_Personas.csv", sep=",", na = "null", colClasses=c("COD_LOC"="character", "COD_UPL"="character", "COD_UTAM"="character", "COD_ZAT"="character", "COD_SECT"="character", "COD_SECC"="character", "COD_HEX"="character", "COD_MNZ"="character", "COD_FAMILIA"="character", "COD_PERSONA"="character")))
Censo_Personas

# Distribución por oficio
Distribucion_Oficios <- Censo_Personas |>
  select(OFICIO) |>
  group_by(OFICIO) |>
  summarise(
    PERSONAS_OFICIO = n()
  ) |>
  mutate(
  PORCENTAJE = paste(round(100*PERSONAS_OFICIO/length(Censo_Personas$COD_PERSONA), digits = 2), "%", sep = ""),
  ) |>
  arrange(desc(PERSONAS_OFICIO))
Distribucion_Oficios

# Ordenamos y asignamos factores
Distribucion_Oficios$OFICIO <- factor(Distribucion_Oficios$OFICIO, levels = Distribucion_Oficios$OFICIO[order(Distribucion_Oficios$PERSONAS_OFICIO)])
Distribucion_Oficios

# Creamos el plot para los grupos de edades:
Plot_Distribucion_Oficios <- Distribucion_Oficios |>
  ggplot() +
  # Barras horizontales (Agregamos 200k para alargar las barras manualmente)
  geom_col(aes(x = OFICIO, y = 330000+PERSONAS_OFICIO), fill = "darkgray", width = 0.9) +
  # Valor de horas dentro de la barra, al final
  geom_text(aes(x = OFICIO, y = 330000+PERSONAS_OFICIO, label = PORCENTAJE),
            hjust = 1.2,             # muy cerca del borde derecho (1.2 es el mejor)
            color = "white", size = 6, family = "Roboto") +
  coord_flip() +
  # Eliminar ejes, cuadrículas y leyenda
  theme_void() +
  theme(legend.position = "none") +
  # Añadir espacio a la derecha para que no se corte el texto
  scale_y_continuous(limits = c(0, max(Distribucion_Oficios$PERSONAS_OFICIO) * 1.1), expand = c(0,0))
Plot_Distribucion_Oficios

# # Guardamos como imagen:
ggsave(
  filename = "Grafica_Distribucion_Oficios.png",
  plot = Plot_Distribucion_Oficios,
  device = NULL,
  path = "./Data/3_Results/9_Misc/",
  scale = 1,
  width = 1000,
  height = 550,
  units = c("px"),
  dpi = 150,
  limitsize = TRUE,
  bg = NULL,
  create.dir = FALSE,
)

# Datos para poner manualmente en Word:
Distribucion_Oficios |> select(PERSONAS_OFICIO)

# ####
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ::: Test de equipamientos del Escenario 3                                  :::
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ####

Isochronas_Estaciones_sf <- isochrone(
  r5r_core = r5r_core_scenario_3,
  origins = Estaciones_Metro |> mutate(id = stop_id),
  departure_datetime = Tiempo_de_Partida, # Se había evaluado a las 08:00
  mode = "WALK",
  cutoffs = c(15),
  polygon_output = TRUE,
  walk_speed = Optimo_Walk_Speed,
  max_walk_time = Optimo_Max_Walk_Time,
  sample_size = 1
) |>
  st_make_valid()
Isochronas_Estaciones_sf

Buffer_Isochronas_Estaciones_sf <- Isochronas_Estaciones_sf |>
  st_buffer(50) |>
  select(id, geom=polygons)
Buffer_Isochronas_Estaciones_sf

# Verificamos las Isocrhonas de las estaciones y los buffers:
tm_basemap("OpenStreetMap") +
  tm_shape(Capa_Etiquetas_UPL) +
  tm_polygons(fill = "lightblue", fill_alpha = 0.3,  col_alpha = 1, col = "darkblue") +
  tm_shape(Isochronas_Estaciones_sf) +
  tm_polygons(fill = "darkred", fill_alpha = 0.25) +
  tm_shape(Buffer_Isochronas_Estaciones_sf) +
  tm_polygons(fill = "purple", fill_alpha = 0.25) +
  tm_shape(Linea_Metro) +
  tm_lines(col = "darkgreen") +
  tm_shape(Estaciones_Metro) +
  tm_dots(fill = "blue", size = 1)


# Implantación de equipamientos en déficit dentro de las áreas de las Isocronas:

#-----------------------------------------------------------------------
# PASO 0: Simplificamos nombres
#-----------------------------------------------------------------------

# Simplificamos nombres:
ciudad_sf <- Shape_Municipios
ciudad_sf
Coordinates_CRS <- st_crs(Shape_Municipios)
Coordinates_CRS

# Isocronas:
isochrones_sf <- Buffer_Isochronas_Estaciones_sf |>
  st_transform(crs = Coordinates_CRS)
isochrones_sf

# Puntos de equipamientos:
existing_amenities_sf <- Amenities_Puntos_Scn1_sf
existing_amenities_sf

# Las manzanas:
manzanas_sf <- Shape_Manzanas
manzanas_sf

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
amenities_inventory_df <- Amenities_Inventory_Scn1_BOG_Longer_df

# Verificamos el CRS (Sistema de Referencia de Coordenadas)
stopifnot(
  st_crs(isochrones_sf) == st_crs(existing_amenities_sf),
  st_crs(isochrones_sf) == st_crs(manzanas_sf),
  st_crs(isochrones_sf) == st_crs(ciudad_sf)
)


#-----------------------------------------------------------------------
# PASO 1: Preparación y Cálculos Globales
#-----------------------------------------------------------------------

# Revisamos los equipamientos:
Amenities_Inventory_Scn1_BOG_Longer_df$EQUIPAMIENTO

## 1.1: Filtrar Equipamientos no Implantables
equipamientos_a_excluir <- c(
  "Aeropuertos", "Bosques", "Ciclovías", "Estaciones de tren", "Estadios",
  "Prisiones", "Reservas naturales", "Uso industrial"
)
equipamientos_a_excluir

amenities_inventory_filtrado_df <- amenities_inventory_df %>%
  filter(!EQUIPAMIENTO %in% equipamientos_a_excluir)
amenities_inventory_filtrado_df

## 1.2: Calcular Área Total de la Ciudad
# Aseguramos que la unidad sea metros cuadrados
area_total_ciudad_m2 <- st_area(ciudad_sf) %>% as.numeric()
area_total_ciudad_m2

## 1.3: Calcular Densidad de Referencia para cada Equipamiento
densidades_referencia_df <- amenities_inventory_filtrado_df %>%
  mutate(
    densidad_equip_por_m2 = NUMERO_DE_EQUIPAMIENTOS / area_total_ciudad_m2
  ) %>%
  select(EQUIPAMIENTO, densidad_equip_por_m2)
densidades_referencia_df

# Mostramos un ejemplo de las densidades calculadas
print("Ejemplo de densidades de referencia (equipamientos por m2):")
head(densidades_referencia_df)


#-----------------------------------------------------------------------
# PASO 2, 3 y 4: Bucle Principal - Analizar cada Isocrona
#-----------------------------------------------------------------------

# Usaremos `map_dfr` de purrr para iterar sobre cada isocrona y unir
# los resultados en un único sf al final. Es más eficiente que un bucle for.

# Añadimos un ID único a cada isocrona para facilitar el seguimiento:
isochrones_sf$ID_ISOCRONA <- 1:nrow(isochrones_sf)

# El corazón del análisis: iteramos por cada fila (isocrona) del sf:
lista_equipamientos_propuestos <- map(isochrones_sf$ID_ISOCRONA, function(id_iso) {
  
  # Seleccionamos la isocrona actual
  isocrona_actual_sf <- isochrones_sf %>% filter(ID_ISOCRONA == id_iso)
  
  cat(paste("\n--- Procesando Isocrona ID:", id_iso, "|", isocrona_actual_sf$id[1], "---\n"))
  
  # --- Cálculos específicos para ESTA isocrona ---
  
  # Área de la isocrona actual
  area_isocrona_m2 <- st_area(isocrona_actual_sf) %>% as.numeric()
  cat(paste(" Area de isocrona ", id_iso, ": ", area_isocrona_m2, " m2.\n", sep = ""))
  
  # Identificar manzanas que se intersectan con la isocrona
  manzanas_en_isocrona_sf <- manzanas_sf %>%
    st_filter(isocrona_actual_sf, .predicate = st_intersects)
  
  # Si no hay manzanas en la isocrona, no podemos hacer nada.
  if (nrow(manzanas_en_isocrona_sf) == 0) {
    cat("Advertencia: La isocrona", id_iso, "no intersecta con ninguna manzana. Se omite.\n")
    return(NULL) # Devolvemos NULL para que no se añada al resultado
  }
  
  # Calcular los centroides de esas manzanas (nuestras ubicaciones posibles)
  centroides_posibles_sf <- st_centroid(manzanas_en_isocrona_sf)
  
  # --- Bucle interno: Analizar el déficit para cada TIPO de equipamiento ---
  
  propuestas_para_esta_isocrona <- map_dfr(1:nrow(densidades_referencia_df), function(i) {
    
    # Datos del equipamiento que estamos analizando
    tipo_equipamiento <- densidades_referencia_df$EQUIPAMIENTO[i]
    densidad_ref <- densidades_referencia_df$densidad_equip_por_m2[i]
    
    # Contar cuántos equipamientos de este tipo YA existen en la isocrona
    equip_actuales_en_isocrona <- existing_amenities_sf %>%
      filter(EQUIPAMIENTO == tipo_equipamiento) %>%
      st_filter(isocrona_actual_sf, .predicate = st_intersects)
    
    numero_actual <- nrow(equip_actuales_en_isocrona)
    
    # Calcular el número OBJETIVO de equipamientos para esta isocrona
    numero_objetivo <- densidad_ref * area_isocrona_m2
    
    # Calcular el DÉFICIT (cuántos faltan)
    deficit <- round(numero_objetivo) - numero_actual
    
    # --- Implantación de Nuevos Equipamientos ---
    if (deficit > 0) {
      cat(paste("  Déficit para Isocrona ", id_iso,": '", tipo_equipamiento, "': ", deficit, " unidades.\n", sep=""))
      
      # Seleccionar aleatoriamente `deficit` centroides.
      # `replace = TRUE` permite que un mismo centroide sea elegido más de una vez
      # si el déficit fuera mayor que el número de centroides disponibles.
      ubicaciones_seleccionadas_sf <- centroides_posibles_sf %>%
        sample_n(size = deficit, replace = TRUE)
      
      # Crear el sf con los nuevos puntos propuestos
      nuevos_puntos_sf <- ubicaciones_seleccionadas_sf %>%
        mutate(
          EQUIPAMIENTO = tipo_equipamiento,
          STATUS = "Propuesto",
          ID_ISOCRONA_ORIGEN = id_iso
        ) %>%
        select(EQUIPAMIENTO, STATUS, ID_ISOCRONA_ORIGEN, geom)
      
      return(nuevos_puntos_sf)
    }
    
    # Si no hay déficit, no devolvemos nada
    return(NULL)
  })
  
  return(propuestas_para_esta_isocrona)
})

#-----------------------------------------------------------------------
# PASO 5: Consolidación Final
#-----------------------------------------------------------------------

# `lista_equipamientos_propuestos` es una lista de sf.
# Los unimos todos en un único sf final.
equipamientos_propuestos_sf <- bind_rows(lista_equipamientos_propuestos)
equipamientos_propuestos_sf

# --- RESULTADO FINAL ---
if (nrow(equipamientos_propuestos_sf) > 0) {
  cat("\n\n --- Análisis completado. Se han generado", nrow(equipamientos_propuestos_sf), "nuevos equipamientos propuestos ---\n")
  
  # Imprime un resumen del resultado
  print("Resumen de equipamientos propuestos:")
  print(
    equipamientos_propuestos_sf %>%
      st_drop_geometry() %>%
      count(EQUIPAMIENTO, sort = TRUE)
  )
  
  # Visualización rápida (requiere el paquete 'mapview')
  # library(mapview)
  # mapview(isochrones_sf) + mapview(equipamientos_propuestos_sf, zcol = "EQUIPAMIENTO")
  
} else {
  cat("\n\n--- Análisis completado. No se encontraron déficits que requirieran nuevos equipamientos ---\n")
}

Nuevos_Equipamientos_sf <- equipamientos_propuestos_sf
Nuevos_Equipamientos_sf

manzanas_afectadas_sf <- manzanas_sf |>
  st_filter(isochrones_sf, .predicate = st_intersects)

# Verificamos la implementación de los equipamientos nuevos
tm_basemap("OpenStreetMap") +
  tm_shape(Capa_Etiquetas_UPL) +
  tm_polygons(fill = "lightblue", fill_alpha = 0.3,  col_alpha = 1, col = "darkblue") +
  tm_shape(manzanas_afectadas_sf) +
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


















