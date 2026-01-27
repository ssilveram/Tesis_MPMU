# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# :::                                                                        :::
# :::                       SCRIPT DE INICIALIZACIÓN                         :::
# :::                                                                        :::
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#
# Nombre:      0_Initialization.r
#
# Descripcion: La función de este script es la de inicializar las librerías y
#              las funciones necesarias para correr los scripts de este proyecto
#              de Tesis de Maestría.
#              Se recomienda correr este script siempre antes de cualquier otro
#              que haga parte del proyecto.

# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# :::                      Contenido de este Script                          :::
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#
# 1. Inicialización
# 1.1. Carga de librerías.
# 1.2. Definición de funciones propias.

Start_Time_S0_P0 <- Sys.time()

# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ::: 1. Inicialización                                                      :::
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

Start_Time_S0_P1 <- Sys.time()

# --- 1.1. Cargamos las librerías necesarias para correr este script ---
#

#install.packages("pacman")
pacman::p_load(
  doParallel, # Para computación paralela.
  dplyr, # Para procesamiento de datos.
  jsonlite, # Para lectura de JSON.
  lubridate, # Para procesamiento de fechas.
  osmdata, # Para descarga de datos de OSM.
  patchwork, # Para plotear un arreglo de mapas.
  sf, # Para procesar los simple features.
  spData, # No me acuerdo.
  tidytransit, # Para procesamiento de datos de tránsito.
  tidyverse, # Conjunto de utilidades para manejo de data.
  tmap, # Para ver los mapas.
  units, # Para procesar unidades.
  osmdata, # Para importar datos de OSM.
  viridisLite, # Para hacer que los gráficos se vean mejor.
  viridis, # Para hacer que los gráficos se vean mejor.
  rayshader, # Para hacer los mapas en 3D
  rayrender, # Para hacer renderizados de alta calidad.
  RColorBrewer, # Para manejo del color
  colorspace, # Para manejo del color
  igraph, # Para hacer gráficas más bonitas
  ggraph, # Para hacer gráficas más bonitas.
  tidygraph, # Como el Tidyverse, pero para gráficas
  doParallel, # Computación paralela
  data.table, # Carga y guardado de CSV de manera rápida
  magick, # Para manipulación de imágenes
  remotes, # Para instalaciones remotas
  scales, # Para las escalas de color
  cowplot, # Para extraer leyendas
  accessibility, # Para el cálculo de accesibilidad
  aopdata, # 
  data.table, # Para manipular tablas de forma más rápida
  downlit, # Vinculación del código R de una manera que se pueda usar fácilmente desde los paquetes RMarkdown
  fs, # Proporciona una interfaz uniforme y multiplataforma para las operaciones del sistema de archivos
  ggplot2, # Para hacer las gráficas
  gtfstools, # Herramientas para el tratamiento de GTFS
  httr, # Proporcionar un contenedor para el paquete curl
  knitr, # Sirve como motor para la generación dinámica de reportes
  patchwork, # Sirve para el apilamiento de gráficas de ggplot
  processx, # Ejecuta procesos del sistema en segundo plano
  quantreg, # Para la estimación de regresión cuantil y métodos de inferencia
  quarto, # Sirve para hacer reportes y dashboards
  r5r, # Motor para la accesibilidad basado en Java
  rJava, # Java para R
  rmarkdown, # Para Markdown en R
  scales, # Para las escalas de las gráficas. Integrada en ggplot
  sf, # Para el tratamiento de sf
  testthat, # Es un marco de pruebas para R
  tibble, # Para el manejo de tablas
  xml2, # Para el acceso a XML
  yaml, # Para el manejo de YAML, que es un lenguaje de marcado legible
  zip, # Para el manejo de ZIP
  openrouteservice, # Para acceder a los servicios de OpenRouteService
  osrm, # Para acceder a los servicios de OSRM
  rJavaEnv, # Para el entorno de Java en R
  osmextract, # Para extraer información de paquetes de OSM
  extrafont, # Para integración de fuentes adicionales
  polyglotr, # Para hacer traducciones a los textos
  hms, # Para el manejo de Horas, Minutos y Segundos
  fmsb, # Para las gráficas de radar (arañita)
  gridGraphics, # For 
  maptiles, # For 
  circlize, # Para diagramas de cuerdas
  networkD3,
  ragg, # Para el renderizado de texto
  GA, # Para correr y calibrar el modelo mediante Algoritmos Genéticos
  beepr, # Reproduce sonidos en R. Util para avisar que ha terminado de ejecutarse un script
  purrr, # Parte de tidyverse que sirve para el trabajo con funciones y vectores
  geosphere, # Sirve para calcular distancias en coordenadas LON-LAT
  memuse, # Para extraer los datos de RAM
  ggradar, # Para gráficos de radar
  showtext, # Para cargar fuentes personalizadas
  prettymapr, # Para mapas lindos en ggplot
  ggspatial, # Para descargar el fondo de mapas en ggplot
  ggnewscale, # Para agregar dos escalas en ggplot
  # OpenStreetMap, # Librería de OSM
  # BiocManager, # Manager para librerías
  # maptiles, # Para cargar fragmentos de mapa
  # tmaptools, # Herramientas adicionales de tmap
  # webshot2, # Toma capturas en tmap
  # rosm, # Para descargar el raster del mapa de OSM
)

# --- AUTO-CONFIGURACIÓN DE MEMORIA Y NUCLEOS---
ram_sistema <- round(as.numeric(memuse::Sys.meminfo()$totalram / (1024^3)))
cores_sistema <- parallel::detectCores()

# Recursos a usar:
mem_java <- floor(ram_sistema * 0.75) # El 50% de la RAM
cores_usar <- cores_sistema - 1 # Todos los cores, menos 1.

options(java.parameters = c(
  paste0("-Xmx", mem_java, "G"),
  paste0("-XX:ActiveProcessorCount=", cores_usar)
))

# --- 1.2. Creamos variables de Java ---

# Si no está instalada, es necesario instalarla con el siguiente código:
# rJavaEnv::rje_consent(provided = TRUE)
# rJavaEnv::java_quick_install(version = 21) # install Java 21

# Reiniciar JVM y asignar memoria
# rJava::.jexit()
rJava::.jinit(parameters = c("-Xmx8g"))
runtime <- rJava::.jcall("java/lang/Runtime", "Ljava/lang/Runtime;", "getRuntime")
num_cores_java <- rJava::.jcall(runtime, "I", "availableProcessors")

cat("🔧 Java detecta", num_cores_java, "núcleos del sistema\n")
cat("🔧 Java usará", cores_usar, "núcleos del sistema y", mem_java, "GB de RAM\n")
getOption("java.parameters")

# Confirmar cuánta memoria se asignó

runtime <- rJava::.jcall("java/lang/Runtime", "Ljava/lang/Runtime;", "getRuntime")
maxMemory <- rJava::.jcall(runtime, "J", "maxMemory") / (1024^2)
totalMemory <- rJava::.jcall(runtime, "J", "totalMemory") / (1024^2)
freeMemory <- rJava::.jcall(runtime, "J", "freeMemory") / (1024^2)
cat("Máxima memoria: ", maxMemory, "MB\n")
cat("Memoria total asignada: ", totalMemory, "MB\n")
cat("Memoria libre: ", freeMemory, "MB\n")

# --- 1.3. Defino funciones propias ---
#
# Establezco las preferencias para mostrar mapas en modo Viewer
tmap_mode("view")
tmap_options(basemap.server = "OpenStreetMap")

# Función para setear las resoluciones de imágenes
Set_Resolution = function(Id_Resolution){
  # Inicializamos tabla de Resoluciones
  Resolutions <- tibble(
    Id_Res = c("2160p", "1440p", "1080p", "720p", "480p","240p"),
    Ancho = c(3840, 2560, 1920, 1280, 853, 427),
    Alto = c(2160, 1440, 1080, 720, 480, 240)
  )
  # Buscamos Resolución
  Res_Found = FALSE
  for(i in 1:length(Resolutions$Id_Res)){
    if(Id_Resolution == Resolutions$Id_Res[i]){
      Res_Found = TRUE
    }
  }
  # Si se encuentra, se envía esa fila. De lo contrario, la más baja
  if(Res_Found){
    return(Resolutions |> filter(Id_Res == Id_Resolution))
  }else{
    return(Resolutions |> filter(Id_Res == "240p"))
  }
}

# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ::: 2. Creamos funciones propias                                           :::
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# Función para plotear en modo sencillo
PlotMapa <- function(CosoAPlotear){
  ggplot(CosoAPlotear) +
    geom_sf() + 
    theme(axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank())
}

# Función para mostrar mapa en modo interactivo:
MostrarMapa <- function(cosoAMostrar) { # create a function with the name my_functiontmap_mode("view")
  tmap_mode("view")
  #tm_check_fix()
  tmap_options(check_and_fix = TRUE)        # Solve errors
  tm_shape(cosoAMostrar) +                 # Plot the shape
    tm_polygons(fill_alpha = 0.6)                # Puting transparent mode
  
}

# Función para plotear a color y guardar imagen PNG en una ruta determinada
Plot_Color <- function(Shape_Plotear, Criterio_Color, Paleta_Color, Resolution, Line_Width, Output_Path, Output_Name_ES, Output_Name_EN, Check = FALSE){
  # Seteamos la resolución:
  Ancho <- Set_Resolution(Resolution)$Ancho
  Alto <- Set_Resolution(Resolution)$Alto
  # Creamos el Plot
  Plot_Temporal <- ggplot(Shape_Plotear) +
    geom_sf(
      col = "white",
      linewidth = Line_Width,
      aes(fill = Criterio_Color)
    ) + 
    Paleta_Color +
    # geom_text(
    #   aes(x = X, y = Y, label = etiqueta_texto), # Mapea coordenadas y texto
    #   color = "black",      # Cambia el color ("black", "#FF5733", etc.)
    #   size = 3,             # Cambia el tamaño del texto
    #   family = "sans"       # Cambia el tipo de fuente ("serif", "mono", "Arial", etc.)
    # ) +
    theme_void() +
    theme(
      legend.position = "none",
    )
  # Guardamos el plot como imagen:
  ggsave(
    filename = paste(Output_Name_ES, " ", Alto, "x", Ancho, ".png", sep = ""),
    plot = Plot_Temporal,
    device = NULL,
    path = Output_Path,
    scale = 1,
    width = Alto,
    height = Ancho,
    units = c("px"),
    dpi = 300,
    limitsize = TRUE,
    bg = NULL,
    create.dir = FALSE,
  )

  # Abrimos la imagen y la guardamos en español
  Sys.sleep(1)
  Imagen <- image_read(paste(Output_Path, Output_Name_ES, " ", Alto, "x", Ancho, ".png", sep = ""))
  image_write(Imagen, paste(Output_Path, Output_Name_ES, " ", Alto, "x", Ancho, ".png", sep = ""))
  #image_write(Imagen, paste(Output_Path, Output_Name_EN, " ", Ancho, "x", Alto, ".png", sep = ""))
  
  # Si Check = TRUE, se muestra el plot:
  if(Check == TRUE){
    Plot_Temporal
  }
}

# Función para combinar Shapes de datos, con Shapes de Areas:
Join_Hexagonos_Datos <- function(Shape_Datos, Shape_Area){     # Shape con datos (puntos), Shape de áreas, Nombre Campo de Areas 
  # Hacemos un left_join entre los dos sf. Se unen en las coincidencias
  Joined_Hex <- st_join(Shape_Datos, Shape_Area, left = FALSE)  
  # Sumamos el número de hexágonos que tuvieron coincidencias.
  Sum_Joined_Hex <- Joined_Hex %>%
    group_by(COD_HEX) %>%
    count()
  # Extraemos los Hexagonos y la Suma de la geometría.
  Less_Geom_Hex <- Sum_Joined_Hex %>%
    as_tibble() %>%
    dplyr::select(-geom) # Probaremos con geom
  # Hacemos un left_join entre el Shape de Hexágonos y los datos de acumulación.
  Hex_Counts <- left_join(Shape_Area, Less_Geom_Hex, by = "COD_HEX")
  Hex_Counts[is.na(Hex_Counts)] <- 0  # Como los que no coinciden quedan NA, los ponemos en cero.
  # Extraemos los hexagonos que no se usaron y los combinamos.
  Unused_Hex <- Hex_Counts %>%
    filter(n == 0) %>%
    st_union() %>%
    st_as_sf() %>%
    mutate(COD_HEX = 0) %>%
    mutate(n = 0) %>%
    dplyr::select(COD_HEX, n)
  st_geometry(Unused_Hex) <- "geom"
  # Combinamos los Hexagonos usados, con el Area sin usar.
  Final_Hex <- Hex_Counts %>%
    filter(n != 0) %>%
    bind_rows(Unused_Hex)
  
  return <- Final_Hex
}

# Función para hacer solicitudes a OSM de modo sencillo
Solicitud_OSM <- function(Shape_Limite, Categoria, Category, Equipamiento, Amenity, Clave, Valor){ #ShapeLimite, Categoria [ES], Category [EN], Equipamiento [ES], Amenity [EN], ClaveOSM, ValorOSM
  sf_use_s2(FALSE)
  BBox = st_bbox(Shape_Limite)
  Coordinates_CRS <- st_crs(Shape_Limite)
  # Creamos un sf vacío para inicializar:
  Retornar_sf <- st_sf(CATEGORIA = "Borrador",
                       CATEGORY = "Borrador",
                       EQUIPAMIENTO = "Borrador",
                       AMENITY = "Borrador",
                       geometry = st_sfc(st_point(1:2)),
                       crs = Coordinates_CRS
  )
  # Solicitamos a OSM:
  Respuesta_OSM <- opq(BBox) |>
    add_osm_feature(key = Clave, value = Valor) |>
    osmdata_sf()
  # Convertimos en puntos:
  if(length(Respuesta_OSM$osm_points) > 0){ # Cuando haya puntos
    Retornar_sf <- Retornar_sf |> bind_rows(
      Respuesta_OSM$osm_points |>
        st_transform(Coordinates_CRS) |>
        mutate(CATEGORIA = Categoria,
               CATEGORY = Category,
               EQUIPAMIENTO = Equipamiento,
               AMENITY = Amenity,
               geometry = st_centroid(geometry)
        ) |>
        select(CATEGORIA, CATEGORY, EQUIPAMIENTO, AMENITY)
    )
  }
  if(length(Respuesta_OSM$osm_lines) > 0){ # Cuando haya lineas
    Retornar_sf <- Retornar_sf |> bind_rows(
      Respuesta_OSM$osm_lines |>
        st_transform(Coordinates_CRS) |>
        mutate(CATEGORIA = Categoria,
               CATEGORY = Category,
               EQUIPAMIENTO = Equipamiento,
               AMENITY = Amenity,
               geometry = st_centroid(geometry)
        ) |>
        select(CATEGORIA, CATEGORY, EQUIPAMIENTO, AMENITY)
    )
  }
  if(length(Respuesta_OSM$osm_polygons) > 0){ # Cuando haya poligonos
    Retornar_sf <- Retornar_sf |> bind_rows(
      Respuesta_OSM$osm_polygons |>
        st_transform(Coordinates_CRS) |>
        mutate(CATEGORIA = Categoria,
               CATEGORY = Category,
               EQUIPAMIENTO = Equipamiento,
               AMENITY = Amenity,
               geometry = st_centroid(geometry)
        ) |>
        select(CATEGORIA, CATEGORY, EQUIPAMIENTO, AMENITY)
    )
  }
  if(length(Respuesta_OSM$osm_multilines) > 0){ # Cuando haya multilineas
    Retornar_sf <- Retornar_sf |> bind_rows(
      Respuesta_OSM$osm_multilines |>
        st_transform(Coordinates_CRS) |>
        mutate(CATEGORIA = Categoria,
               CATEGORY = Category,
               EQUIPAMIENTO = Equipamiento,
               AMENITY = Amenity,
               geometry = st_centroid(geometry)
        ) |>
        select(CATEGORIA, CATEGORY, EQUIPAMIENTO, AMENITY)
    )
  }
  if(length(Respuesta_OSM$osm_multipolygons) > 0){ # Cuando haya multipoligonos
    Retornar_sf <- Retornar_sf |> bind_rows(
      Respuesta_OSM$osm_multipolygons |>
        st_transform(Coordinates_CRS) |>
        mutate(CATEGORIA = Categoria,
               CATEGORY = Category,
               EQUIPAMIENTO = Equipamiento,
               AMENITY = Amenity,
               geometry = st_centroid(geometry)
        ) |>
        select(CATEGORIA, CATEGORY, EQUIPAMIENTO, AMENITY)
    )
  }
  Retornar_sf <- Retornar_sf |>
    filter(CATEGORIA != "Borrador") |>
    st_intersection(Shape_Limite) # Solo dentro del Shape deseado
  rownames(Retornar_sf) <- NULL
  
  return(Retornar_sf)
}

# Función para hacer solicitudes a OSM de modo sencillo v02
Solicitud_OSM_v02 <- function(Shape_Limite, Equipamiento, Clave, Valor){ #ShapeLimite, Categoria [ES], Category [EN], Equipamiento [ES], Amenity [EN], ClaveOSM, ValorOSM
  sf_use_s2(FALSE)
  BBox = st_bbox(Shape_Limite)
  Coordinates_CRS <- st_crs(Shape_Limite)
  # Creamos un sf vacío para inicializar:
  Retornar_sf <- st_sf(EQUIPAMIENTO = "Borrador",
                       geometry = st_sfc(st_point(1:2)),
                       crs = Coordinates_CRS
  )
  
  # Solicitamos a OSM:
  Respuesta_OSM <- opq(BBox) |>
    add_osm_feature(key = Clave, value = Valor) |>
    osmdata_sf()
  
  # Convertimos en puntos:
  if(length(Respuesta_OSM$osm_points) > 0){ # Cuando haya puntos
    Retornar_sf <- Retornar_sf |> bind_rows(
      Respuesta_OSM$osm_points |>
        st_transform(Coordinates_CRS) |>
        mutate(EQUIPAMIENTO = Equipamiento,
               geometry = st_centroid(geometry)
        ) |>
        select(EQUIPAMIENTO)
    )
  }
  if(length(Respuesta_OSM$osm_lines) > 0){ # Cuando haya lineas
    Retornar_sf <- Retornar_sf |> bind_rows(
      Respuesta_OSM$osm_lines |>
        st_transform(Coordinates_CRS) |>
        mutate(EQUIPAMIENTO = Equipamiento,
               geometry = st_centroid(geometry)
        ) |>
        select(EQUIPAMIENTO)
    )
  }
  if(length(Respuesta_OSM$osm_polygons) > 0){ # Cuando haya poligonos
    Retornar_sf <- Retornar_sf |> bind_rows(
      Respuesta_OSM$osm_polygons |>
        st_transform(Coordinates_CRS) |>
        mutate(EQUIPAMIENTO = Equipamiento,
               geometry = st_centroid(geometry)
        ) |>
        select(EQUIPAMIENTO)
    )
  }
  if(length(Respuesta_OSM$osm_multilines) > 0){ # Cuando haya multilineas
    Retornar_sf <- Retornar_sf |> bind_rows(
      Respuesta_OSM$osm_multilines |>
        st_transform(Coordinates_CRS) |>
        mutate(EQUIPAMIENTO = Equipamiento,
               geometry = st_centroid(geometry)
        ) |>
        select(EQUIPAMIENTO)
    )
  }
  if(length(Respuesta_OSM$osm_multipolygons) > 0){ # Cuando haya multipoligonos
    Retornar_sf <- Retornar_sf |> bind_rows(
      Respuesta_OSM$osm_multipolygons |>
        st_transform(Coordinates_CRS) |>
        mutate(EQUIPAMIENTO = Equipamiento,
               geometry = st_centroid(geometry)
        ) |>
        select(EQUIPAMIENTO)
    )
  }
  Retornar_sf <- Retornar_sf |>
    filter(EQUIPAMIENTO != "Borrador") |>
    st_intersection(Shape_Limite) # Solo dentro del Shape deseado
  rownames(Retornar_sf) <- NULL
  
  return(Retornar_sf)
}

# Función para crear un mapa de puntos con un shape de base ---
Plot_Puntos_en_Shape <- function(Shape_Base, Datos_Puntos, Color, Line_Width = 0.6, Leyenda, Output_Path, Output_Name, Resolution = "2160p"){
  # Seteamos la resolución:
  Ancho <- Set_Resolution(Resolution)$Ancho
  Alto <- Set_Resolution(Resolution)$Alto
  Output_Name <- paste(Output_Name, " ", Ancho, "x", Alto, ".png", sep = "")
  # Creamos el plot
  Temp_Plot_Puntos <- Shape_Base |>
    ggplot() +
    geom_sf(linewidth = Line_Width) +
    geom_sf(data = Datos_Puntos, size = 0.1, aes(col = Color)) +
    theme_void() +
    labs(
      colour = Leyenda
    )
  # Guardamos el plot como imagen
  ggsave(
    filename = Output_Name,
    plot = Temp_Plot_Puntos,
    device = NULL,
    path = Output_Path,
    scale = 1,
    width = Ancho,
    height = Alto,
    units = c("px"),
    dpi = 300,
    limitsize = TRUE,
    bg = "white",
    create.dir = FALSE,
  )
  # Cargamos la imagen para recortarla y guardarla
  Imagen <- image_read(paste(Output_Path, Output_Name, sep = ""))
  Imagen |>
    image_trim() |>
    image_write(paste(Output_Path, Output_Name, sep = ""))
}

# Función para mapas 3D
PlotMapa3D <- function(Shape_Plotear, Criterio_Extrusion, Paleta_Color, Output_Path, Output_Name_ES, Output_Name_EN, Resolution, Scale, Titulo_ES = "", Titulo_EN = "", Check = FALSE){
  
  # Seteamos la resolución:
  Ancho <- Set_Resolution(Resolution)$Ancho
  Alto <- Set_Resolution(Resolution)$Alto
  
  # Creamos el 2D Plot:
  bbox <- sf::st_bbox(Shape_Plotear)
  
  # Expandimos en un factor (ej. 1.1 = 10% más grande)
  exp_factor <- 1.5
  xrange <- diff(bbox[c("xmin", "xmax")])
  yrange <- diff(bbox[c("ymin", "ymax")])
  
  Shape_Plotear <- Shape_Plotear |>
    dplyr::filter(!is.na(sf::st_is_valid(geom)), sf::st_is_valid(geom)) |> # Elimina los Shapes vacíos
    sf::st_make_valid()
  
  # Generamos el plot:
  Ploteo <- Shape_Plotear |>
    ggplot() +
    geom_sf(
      aes(fill = Criterio_Extrusion),
      color = NA,
    ) +
    coord_sf( # Esto es para ampliar en 1,5 veces la base y poder recortar después
      xlim = c(bbox["xmin"] - xrange * (exp_factor - 1) / 2,
               bbox["xmax"] + xrange * (exp_factor - 1) / 2),
      ylim = c(bbox["ymin"] - yrange * (exp_factor - 1) / 2,
               bbox["ymax"] + yrange * (exp_factor - 1) / 2)
    ) +
    Paleta_Color +
    theme(
      line = element_blank(),
      legend.position = "none",
      axis.text = element_blank(),
      axis.line = element_blank(),
      axis.ticks = element_blank(),
      panel.background = element_blank(),
      panel.grid = element_blank(),
    ) +
    labs(x = NULL,
         y = NULL,
         borders = NULL,)
  
  # Muestro el Plot en la ventanita de Plots
  #Ploteo
  
  # Creamos el 3D Plot
  rgl::close3d()
  plot_gg(
    ggobj = Ploteo,
    multicore = TRUE,
    windowsize = c(Ancho*1.5, Alto*1.5),
    offset_edges = FALSE,
    scale = Scale, #50
  )
  
  # Ajustamos la cámara de la vista 3D
  rayshader::render_camera(
    phi = 35,
    zoom = .32,
    theta = -70
  )

  # Renderizamos con RayTracing y guardamos el archivo
  rayshader::render_highquality(
     filename = paste(Output_Path, Output_Name_ES, " ", Ancho, "x", Alto, ".png", sep = ""),
     preview = T,
     interactive = F,
     light = T,
     lightdirection = c(
       315, 310, 315, 310
     ),
     lightintensity = c(
       1000, 1500, 150, 100
     ),
     lightaltitude = c(
       20, 20, 80, 80 #15,15,80,80
     ),
     ground_material =
       rayrender::microfacet(
         roughness = .1
       ),
     width = Ancho*1.5,
     height = Alto*1.5,
   )
  
  # Abrimos la imagen, la rotamos y recortamos al tamaño solicitado
  Sys.sleep(1)
  #
  Imagen_Mapa <- image_read(paste(Output_Path, Output_Name_ES, " ", Ancho, "x", Alto, ".png", sep = ""))
  Fondo_Blanco <- image_scale(image_read("./Data/3_Results/2_Population/Fondo_Blanco 3840x2160.png"), paste(Ancho, "x", Alto, sep = "")) #|>
    #image_border("darkblue", "1x1")
  
  Ancho_Imagen <- image_info(Imagen_Mapa)$width # Esta dimensión debe ser 1.5 veces el ancho
  Alto_Imagen <- image_info(Imagen_Mapa)$height  # Esta imensión debe ser 1.5 veces el alto
  Origen_Corte_X <- (Ancho_Imagen - Ancho)*2/3  # No centrado, sino desplazado un poco a la izquierda
  Origen_Corte_Y <- (Alto_Imagen - Alto)*1/3    # No centrado, sino desplazado un poco hacia arriba
  
  Imagen_Cortada <- Imagen_Mapa |>
    image_rotate(-12) |>
    image_crop(paste(Ancho, "x", Alto, "+", Origen_Corte_X, "+", Origen_Corte_Y, sep = "")) |>
    image_rotate(12) |>
    image_trim() |>
  # image_border("darkred", "1x1") |>
    image_scale(Ancho)
   Posicion_X <- 0
   Posicion_Y <- (Alto-image_info(Imagen_Cortada)$height)/2
   Mapa_Listo <- image_composite(Fondo_Blanco,
                                 Imagen_Cortada,
                                 offset = paste("+", Posicion_X, "+", Posicion_Y , sep = "")) # Centrado en altura
  
   # Agregamos los títulos (Opcional)
   Color_Titulo <- Paleta_Color$palette(1)
   Mapa_Listo_ES <- Mapa_Listo |>
    image_annotate(text = Titulo_ES,
                  size = 12*(Alto/240),
                  gravity = "Northeast",
                  font = "Georgia",
                  color = Color_Titulo
    )
   Mapa_Listo_EN <- Mapa_Listo |>
     image_annotate(text = Titulo_EN,
                  size = 12*(Alto/240),
                  gravity = "Northeast",
                  font = "Georgia",
                  color = Color_Titulo
    )

  # Guardamos en Español e Inglés
  image_write(Mapa_Listo_ES, paste(Output_Path, Output_Name_ES, " ", Ancho, "x", Alto, ".png", sep = ""))
  #image_write(Mapa_Listo_EN, paste(Output_Path, Output_Name_EN, " ", Ancho, "x", Alto, ".png", sep = ""))
  
  # Si Check = TRUE, se muestra el plot:
  if(Check == TRUE){
    Mapa_Listo
  }
}

# Función de normalización para la Accesibilidad a equipamientos.
# Como parámetros van: DataFrame, Columna: COD_LOC, COD_UPL, COD_HEX; Tipo: "Todo", "Medias".
Normalizacion <- function(df, Columna_Interes, Tipo_Resultado = "Todo"){
  
  # 1. Separamos la columna de interés de los datos numericos a normalizar
  # ---------------------------------------------
  columna_id <- df[Columna_Interes]
  df_numeric <- df[, !(names(df) %in% c(Columna_Interes))]
  
  # 2. Calculamos los máximos (techo) y las medias
  # ---------------------------------------------
  maximos <- sapply(df_numeric, max, na.rm = TRUE)
  medias <- colMeans(df_numeric, na.rm = TRUE)
  
  # 3. Definimos una nueva función para la normalización por el valor máximo
  # ---------------------------------------------
  normalize_by_max <- function(x) {
    max_val <- max(x, na.rm = TRUE)
    # Si el máximo es 0, todos los valores son 0 para evitar división por cero.
    if (max_val == 0) {
      return(rep(0, length(x)))
    }
    # Calculamos el porcentaje respecto al máximo.
    return(100 * x / max_val)
  }
  
  if (Tipo_Resultado == "Medias") {
    # Normalizamos las medias usando el mismo método.
    medias_normalizadas <- 100 * medias / maximos
    # Reemplazar posibles NaN si maximo era 0.
    medias_normalizadas[is.nan(medias_normalizadas)] <- 0
    resultado <- medias_normalizadas
    
  }else{
    
    # Aplicamos la función para cada columna del dataframe.
    df_normalizado <- as.data.frame(lapply(df_numeric, normalize_by_max))
    
    # Reconstruimos el dataframe original
    resultado <- cbind(columna_id, df_normalizado) #decia columna upl
  }
  resultado
}

########## FUNCION DE GRAFICO DE ESTRELLITA ################

Estrellita_SocialFunctions_UPL <- function(df = Acceso_Equipamientos_UPL_Scn1_15min_Norm_df, Medias = Acceso_Equipamientos_UPL_Scn1_15min_Medias_Norm_df, COD){
  
  # Dataframe 1: Grupo A
  grupo_A <- Medias[1:6]
  
  # Dataframe 2: Grupo B
  grupo_B <- df |>
    filter(COD_UPL == COD) |>
    select(Categorias_Equipamientos[1:6])
  
  # --- Formatear los datos para la gráfica ---
  
  # Define los valores máximos y mínimos para cada eje
  # Puedes ajustarlos según la escala real de tus datos
  max_vals <- c(100, 100, 100, 100, 100, 100)
  min_vals <- c(0, 0, 0, 0, 0, 0)
  
  # Combina los datos en un solo dataframe
  # Primero las filas de max/min, luego los datos de cada grupo
  datos_para_grafica <- rbind(max_vals, min_vals, grupo_A, grupo_B)
  
  # Renombrar las filas para mayor claridad (opcional)
  NOM <- as.character(Shape_UPLs |> as_tibble() |> filter(COD_UPL == COD) |> select(NOM_UPL))
  rownames(datos_para_grafica) <- c("Max", "Min", "Promedio Bogotá", paste("UPL ", COD, ": ", NOM, sep = ""))
  
  # --- Crear la gráfica de estrella ---
  
  # Define los colores para cada grupo. 
  # Usaremos colores con transparencia para que se vean ambas áreas.
  colores_area <- c(rgb(0.2, 0.5, 0.5, 0.4), rgb(0.8, 0.2, 0.5, 0.4))
  colores_linea <- c(rgb(0.2, 0.5, 0.5, 0.9), rgb(0.8, 0.2, 0.5, 0.9))
  
  
  # Generar la gráfica
  Grafico_Estrella <- wrap_elements(panel = ~ {
    
    # Bloque de código que quiero envolver:
    
    radarchart(
      datos_para_grafica,
      axistype = 1, # Estilo de las etiquetas de los ejes
      
      #--- Propiedades del polígono ---
      pcol = colores_linea,       # Color de la línea
      pfcol = colores_area,       # Color del relleno
      plwd = 2,                   # Grosor de la línea
      plty = 1,                   # Tipo de línea (sólida)
      
      #--- Propiedades de la rejilla ---
      cglcol = "grey",            # Color de la rejilla
      cglty = 1,                  # Tipo de línea de la rejilla
      axislabcol = "grey",        # Color de las etiquetas de los ejes
      caxislabels = seq(0, 25, 5), # Etiquetas numéricas de los ejes
      cglwd = 0.8,                # Grosor de la rejilla
      
      #--- Propiedades de las etiquetas de las variables ---
      vlcex = 0.8, # Tamaño de la fuente de las etiquetas (Fuerza, Velocidad...)
      
      title = paste("Social functions for UPL ", COD, ": ", NOM, sep = "")
    )
    
    # Añadir una leyenda para identificar a cada grupo
    legend(
      x = "topright",
      legend = rownames(datos_para_grafica)[-c(1, 2)], # Nombres de los grupos
      bty = "n", # Sin caja alrededor de la leyenda
      pch = 20,  # Símbolo
      col = colores_area,
      text.col = "black",
      cex = 1,   # Tamaño del texto de la leyenda
      pt.cex = 3 # Tamaño de los puntos en la leyenda
    )
    
  }) # Cierro mi envoltura
  Grafico_Estrella
}


################ FUNCION DE BARRITAS HORIZONTALES #####################

Puntitos_Composite_UPL <- function(COD){
  # 1. Asignamos los df a las variables locales
  # Primero con los datos actuales o de interés
  datos_actuales <- Acceso_Equipamientos_UPL_Scn1_15min_Norm_df |>
    filter(COD_UPL == COD) |>
    select(Categorias_Equipamientos[7:9])
  
  # Después con lod datos de referencia o promedio
  datos_referencia <- Acceso_Equipamientos_UPL_Scn1_15min_Medias_Norm_df[7:9] |>
    as.list() |> as.data.frame()
  
  # 2. Procesamos el primer dataframe (valores actuales o de interés)
  # Lo "pivotamos" para poner las categorías en una columna y los valores en otra.
  df_actual_largo <- datos_actuales %>%
    pivot_longer(
      cols = everything(), # Selecciona todas las columnas
      names_to = "categoria",
      values_to = "valor_actual"
    )
  
  # 3. Procesamos el segundo dataframe (valores de referencia o promedio)
  # Hacemos lo mismo para los datos de referencia o promedio.
  df_referencia_largo <- datos_referencia %>%
    pivot_longer(
      cols = everything(),
      names_to = "categoria",
      values_to = "valor_referencia"
    )
  
  # 4. Unimos los dos dataframes procesados
  # Usamos la columna "categoria" como la clave para unir las dos tablas.
  df_final <- full_join(df_actual_largo, df_referencia_largo, by = "categoria")
  #df_final
  
  # 2. Creamos el gráfico
  Grafico_de_bolitas <- ggplot(df_final) +
    
    # Línea vertical de referencia:
    geom_errorbarh(
      aes(y = categoria, xmin = valor_referencia, xmax = valor_referencia),
      height = 0.2,      # Controla la altura de la línea vertical
      color = "gray40",  # Un gris más oscuro para que se vea bien
      linewidth = 0.8    # Grosor de la línea
    ) +
    
    # Línea gris de la escala
    geom_segment(
      aes(x = 0, xend = 100, y = categoria, yend = categoria), # <-- Escala de 0 a 100
      color = "gray",    # Color de la línea
      linewidth = 0.5    # Grosor de la línea
    ) +
    
    # Punto principal (sin cambios en la lógica)
    geom_point(
      aes(x = valor_actual, y = categoria, color = valor_actual,),
      size = 15           # Tamaño del punto
    ) +
    
    # Añadimos el texto "Referencia"
    geom_text(
      aes(x = valor_referencia, y = categoria, label = "Mean in Bogota"),
      nudge_y = 0.3,     # Mueve la etiqueta un poco hacia arriba para que no se solape
      size = 5,          # Tamaño del texto
      color = "gray20"   # Color del texto
    ) +
    
    # 3. Personalización y estética
    
    # Escala de colores ajustada
    scale_color_gradient2(
      low = "#e74c3c", mid = "#f1c40f", high = "#2ecc71",
      midpoint = 50,      # <-- CAMBIO: El punto medio ahora es 50
      limits = c(0, 100)  # <-- CAMBIO: Los límites son 0 y 100
    ) +
    
    # Límites y etiquetas del eje X ajustados
    scale_x_continuous(
      limits = c(0, 100),              # <-- CAMBIO: Límites del eje
      breaks = seq(0, 100, by = 10)    # <-- CAMBIO: Marcas en 0, 25, 50, 75, 100
    ) +
    
    # Tema blanquito:
    theme_minimal() +
    theme(
      panel.grid.major.y = element_blank(),
      panel.grid.minor.x = element_blank(),
      legend.position = "none",
      axis.title.y = element_blank(),
      text = element_text(family = "sans", size = 20)
    ) +
    
    # Títulos
    labs(
      title = "Composite Indicators",
      subtitle = " ", # <-- Mejor lo dejo vacío
      x = " "         # <-- Por si quiero poner un texto al eje X.
    )
  Grafico_de_bolitas
}

###### Mapa de ubicación de la UPL #############

UPL_en_Mapa <- function(COD){
  
  # 1. Alistamos los datos:
  Acceso_Equipamientos_UPL_Scn1_15min_Norm_sf <- Acceso_Equipamientos_UPL_Scn1_15min_Norm_df |>
    right_join(Shape_UPLs) |>
    select(COD_UPL, NOM_UPL, Social_Functions, geom) |>
    st_as_sf()
  # Acceso_Equipamientos_UPL_Scn1_15min_Norm_sf
  
  # 2. Cargo mis datos en la variable local:
  datos_upl <- Acceso_Equipamientos_UPL_Scn1_15min_Norm_sf
  
  # 3. Selecciono la UPL a resaltar:
  upl_a_resaltar <- Acceso_Equipamientos_UPL_Scn1_15min_Norm_sf |>
    as_tibble() |>
    filter(COD_UPL == COD) |>
    select(NOM_UPL) |>
    as.character()
  
  # 4. PREPARAR LOS DATOS
  # Usamos dplyr para añadir una nueva columna llamada "resaltado".
  # Esta columna tendrá el valor "Sí" si el nombre de la UPL coincide con nuestra selección,
  # y "No" en caso contrario.
  # Asegúrate de que el nombre de la columna `NOMBRE_UPL` coincida con el de tu archivo.
  datos_upl_mapa <- datos_upl %>%
    mutate(resaltado = ifelse(NOM_UPL == upl_a_resaltar, "Sí", "No"))
  
  # Miremos cómo quedó la tabla
  # print(head(datos_upl_mapa))
  
  # 5. CREAR EL MAPA CON GGPLOT
  Mapa_UPL_en_Bogota <- ggplot(data = datos_upl_mapa) +
    # Usamos geom_sf y le decimos que el color de relleno (fill) depende de la columna "resaltado"
    geom_sf(aes(fill = resaltado), 
            color = "white", # Color del borde de cada polígono
            lwd = 1) +      # Grosor del borde
    
    # Añadimos etiquetas con los nombres de las UPL para mejor visualización
    # geom_sf_text(aes(label = NOM_UPL), size = 3, color = "black") +
    
    # Definimos manualmente los colores que queremos usar
    scale_fill_manual(values = c("Sí" = "darkred", "No" = "grey80")#,
                      #name = "Resaltado", # Título de la leyenda
                      #labels = c("No", "Sí")
    ) +
    
    # Títulos y tema limpio
    labs(
      title = "Mapa de Unidades de Planeación Local (UPL)",
      subtitle = paste("Resaltando la UPL:", upl_a_resaltar),
      caption = "Fuente: Elaboración propia"
    ) +
    theme_void() + # Un tema minimalista ideal para mapas
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5),
      legend.position = "none" # Ubicamos la leyenda abajo
    )
  
  Mapa_UPL_en_Bogota
}

############ Croquis dentro de hexágono #############

Manzanas_en_UPL <- function(COD){
  # 1. Alisto el sf de manzanas
  manzanas_sf <- Acceso_Equipamientos_MNZ_Scn1_15min_Norm_df |>
    select(COD_MNZ, Social_Functions) |>
    left_join(
      Shape_Manzanas |>
        select(COD_UPL, COD_MNZ)
    ) |>
    select(COD_UPL, COD_MNZ, Social_Functions, geom) |>
    st_as_sf()
  manzanas_sf
  
  # Seleccionar la UPL que quieres visualizar y filtrar los datos
  codigo_upl_seleccionado <- COD
  nombre_columna_accesibilidad <- "Social_Functions" # <- ¡IMPORTANTE: Cambia esto!
  
  manzanas_upl_filtradas <- manzanas_sf %>%
    filter(COD_UPL == codigo_upl_seleccionado)
  
  # Paso 4: Crear el mapa con ggplot2
  Mapa_Accesibilidad_Manzanas <- ggplot() +
    # Añadir la capa de las manzanas filtradas
    geom_sf(data = manzanas_upl_filtradas, 
            aes(fill = !!sym(nombre_columna_accesibilidad)),
            color = "gray50", # Color del borde de las manzanas, puedes cambiarlo o quitarlo
            lwd = 0.1) +      # Grosor del borde de las manzanas
    
    # Personalizar la paleta de colores (ejemplo con 'viridis')
    scale_fill_gradient(low="bisque3", high="darkred", name = "Índice de\nAccesibilidad") +
    #scale_fill_viridis_c(option = "magma", name = "Índice de\nAccesibilidad") +
    
    # Añadir títulos y etiquetas
    labs(title = paste("Accesibilidad a Equipamientos por manzana en la UPL:", codigo_upl_seleccionado),
         subtitle = "Visualización por Manzana",
         caption = " ") + # Quizá ponga el crédito si me queda espacio
    
    # Usar un tema limpio para el mapa
    theme_void() +
    
    # Mejorar la apariencia de la leyenda y los títulos
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
      plot.subtitle = element_text(hjust = 0.5, size = 12),
      legend.position = "right"
    )
  
  Mapa_Accesibilidad_Manzanas
}

# Función para avisar con sonido, que terminó de ejecutarse un script
Avisar <- function(Repeticiones = 1){
  i <- 1
  Avisos <- Repeticiones
  while(i <= Avisos){
    if(Repeticiones == 1){
      print("¡Script terminado!")
    }else{
      print(paste("Aviso ", i, " de ", Repeticiones, ": ¡Script terminado!", sep = ""))
    }
    for(j in 1:2){
      if(j == 1){
        beep(8)
        Sys.sleep(5)
      }else{
        beep(7)
        Sys.sleep(0.5)
        beep(7)
        Sys.sleep(0.5)
        beep(7)
        Sys.sleep(2)
      }
    }
    if(i == Avisos & Repeticiones > 1){
      print(paste("¡Se avisó ", Avisos, " veces y nadie vino a revisar!", sep = ""))
      break
    }
    i <- i + 1
  }
}

############# FIN DE FUNCIONES PROPIAS ###################

# Tiempo de procesamiento:
End_Time_S0_P1 <- Sys.time()
print(paste("1. Tiempo de inicialización: ", as.duration(End_Time_S0_P1 - Start_Time_S0_P1)))
