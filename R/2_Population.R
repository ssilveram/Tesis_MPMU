# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# :::                                                                        :::
# :::                         SCRIPT DE POBLACIÓN                            :::
# :::                                                                        :::
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#
# Nombre:       2_Population.R
#
# Descripcion:  Este Script nos ayuda a entender dónde viven las personas en Bogotá,
#               cruzando los datos del censo oficial del 2018, con la información
#               de los shapes procesados en 1_Empty_Shapes.

# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# :::                      Contenido de este Script                          :::
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#
# 1. Inicialización
# 1.1. Carga de librerías.
# 1.2. Creación de funciones propias.
# 2. Carga de datos de origen.
# 2.1. Carga de los Shapes de la Ciudad ordenados en 1_Empty_Shapes.
# 2.2. Carga de los CSV con datos del censo (DANE).
# 3. Filtrado y Tratamiento de dataframes de población.
# 4. Guardado de datos del censo filtrados y procesados.
# 5. Combinación de datos de población con los shapes en diferentes escalas.
# 6. Ploteo de resultados (Shapes con población).
# 7. Guardado de Shapes con info de población.
# 8. Tiempos de procesamiento.
#
Start_Time_S2_P0 <- Sys.time()

# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ::: 1. Inicialización                                                      :::
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#
Start_Time_S2_P1 <- Sys.time()

source("0_Initialization.R")

# Tiempo de procesamiento:
End_Time_S2_P1 <- Sys.time()
print(paste("1. Tiempo de inicialización: ", as.duration(End_Time_S2_P1 - Start_Time_S2_P1)))

# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ::: 2. Carga de datos de origen                                            :::
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# Cargamos los shapes y dataframes originales para comenzar a filtrar y poner
# todo en orden para futuros análisis
#
# ---- Cargamos los datos originales del origen de datos ---
#
Start_Time_S2_P2 <- Sys.time()

# --- 2.1. Carga de Shapes Vacíos (1_Empty_Shapes.r) ---
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

# --- 2.2. Carga de Datos del Censo ---
# Antiguo método:
#Personas_BOG_Original <- read.csv2("./Data/1_Sources/1_Census/DANE/CSV/CNPV2018_5PER_A2_11.CSV", sep=",", na = "null")       # Datos de personas
#MNG_BOG_Original <- read.csv2("./Data/1_Sources/1_Census/DANE/CSV/CNPV2018_MGN_A2_11.CSV", sep=",", na = "null", colClasses=c("COD_DANE_ANM"="character")) # Datos de ubicación de encuesta

# Nuevo método:
Personas_BOG_Original <- as.data.frame(fread("./Data/1_Sources/2_Population/DANE/CSV/CNPV2018_5PER_A2_11.CSV", sep=",", na = "null"))       # Datos de personas
MNG_BOG_Original <- as.data.frame(fread("./Data/1_Sources/2_Population/DANE/CSV/CNPV2018_MGN_A2_11.CSV", sep=",", na = "null", colClasses=c("COD_DANE_ANM"="character"))) # Datos de ubicación de encuesta

head(Personas_BOG_Original)
head(Censo_Personas)

# Tiempo de procesamiento:
End_Time_S2_P2 <- Sys.time()
print(paste("2. Tiempo de carga de datos de origen: ", as.duration(End_Time_S2_P2 - Start_Time_S2_P2)))

# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ::: 3. Filtrado y Tratamiento de dataframes de población                   :::
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#
# El objetivo es dejar lo más simplificados posibles los dataframes originales
# con la información sobre la población.
#
Start_Time_S2_P3 <- Sys.time()

# --- Creamos los diccionarios para las personas ---

# Diccionario de edades:
Diccionario_Edades <- data.frame(
  P_EDADR = seq(1:21),
  Intervalo_Edad = str_c( (seq(0:20)-1)*5 , "-" , (seq(0:20)-1)*5+4, sep = "")
)
Diccionario_Edades$Intervalo_Edad <- Diccionario_Edades$Intervalo_Edad %>% str_replace("100-104","100+")
Diccionario_Edades

# Diccionario de oficios:
Diccionario_Oficio = data.frame(
  P_TRABAJO = seq(0:10)-1,
  OFICIO = c("NA/NI", "Trabaja", "Desempleado", "Trabaja", "Desempleado", "Pensionado", "Estudia", "Hogar", "Desempleado", "Otro", "NA/NI")
)
Diccionario_Oficio

# --- Tratamiento de Data-Frames ---

# Extraemos la relación entre Manzanas y Familias (Cód. Encuestas):
MNG_BOG_Original
names(MNG_BOG_Original)

# Extraemos los códigos de Manzana, Vivienda y Familia
MNZ_FAMILIA <- MNG_BOG_Original %>%
  mutate(
    COD_MNZ = as.character(COD_DANE_ANM),
    COD_VIVIENDA = paste(COD_MNZ, as.character(U_VIVIENDA), sep = "_"),
    COD_FAMILIA = as.character(COD_ENCUESTAS)
  ) %>%
  filter(COD_MNZ %in% Shape_Manzanas$COD_MNZ) %>%
  select(COD_MNZ, COD_VIVIENDA, COD_FAMILIA)
MNZ_FAMILIA

# Procesamos el dataframe de personas con sus detalles:
Personas_BOG_Original
names(Personas_BOG_Original)
Censo_Personas <- Personas_BOG_Original %>%
  select(COD_ENCUESTAS, P_SEXO, P_EDADR, PA_LUG_NAC, CONDICION_FISICA, P_TRABAJO) %>%
  filter(COD_ENCUESTAS %in% MNZ_FAMILIA$COD_FAMILIA) %>%
  mutate(
    COD_FAMILIA = as.character(COD_ENCUESTAS),
    SEXO = as.character(P_SEXO),
    SEXO = str_replace(SEXO, "1", "M"),
    SEXO = str_replace(SEXO, "2", "F")
  ) %>%
  inner_join(Diccionario_Edades, by = "P_EDADR") %>%
  mutate(
    EDAD = as.character(Intervalo_Edad),
    LUG_NAC = if_else(PA_LUG_NAC == "1", "Bogota", "Otro"),
    DISCAPACIDAD = if_else(CONDICION_FISICA == "1", "Si", "No")
  ) %>%
  full_join(Diccionario_Oficio, by = "P_TRABAJO") %>%
  mutate(
    OFICIO = replace_na(OFICIO, "NA/NI"),
    COD_PERSONA = row_number()
  ) %>%
  inner_join(MNZ_FAMILIA, by = "COD_FAMILIA") %>%
  select(COD_MNZ, COD_VIVIENDA, COD_FAMILIA, COD_PERSONA, SEXO, EDAD, LUG_NAC, DISCAPACIDAD, OFICIO) %>%
  inner_join(Shape_Manzanas, by = "COD_MNZ") %>%
  mutate(
    COD_LOC = as.character(COD_LOC),
    COD_UPL = as.character(COD_UPL),
    COD_UTAM = as.character(COD_UTAM),
    COD_ZAT = as.character(COD_ZAT),
    COD_SECT = as.character(COD_SECT),
    COD_SECC = as.character(COD_SECC),
    COD_HEX = as.character(COD_HEX),
    COD_MNZ = as.character(COD_MNZ),
    COD_VIVIENDA = as.character(COD_VIVIENDA),
    COD_FAMILIA = as.character(COD_FAMILIA),
    COD_PERSONA = as.character(COD_PERSONA)
  ) %>%
  select(COD_LOC, COD_UPL, COD_UTAM, COD_ZAT, COD_SECT, COD_SECC, COD_HEX, COD_MNZ, COD_VIVIENDA, COD_FAMILIA, COD_PERSONA, SEXO, EDAD, LUG_NAC, DISCAPACIDAD, OFICIO)
Censo_Personas
length(Censo_Personas$COD_PERSONA) # Total Ciudadanos: 7130646

# Creamos el nuevo MNG con la info de las personas.
Censo_MNG <- Censo_Personas %>%
  select(COD_MNZ, COD_VIVIENDA, COD_FAMILIA, COD_PERSONA)
Censo_MNG
length(Censo_MNG$COD_PERSONA) # 7130646 Personas.

# Población por Familias:
Censo_Familias <- Censo_Personas %>%
  mutate(HOMBRES = if_else(SEXO == "M", 1, 0)) %>%
  mutate(MUJERES = if_else(SEXO == "F", 1, 0)) %>%
  mutate(DISCAPACITADOS = if_else(DISCAPACIDAD == "Si", 1, 0)) %>%
  mutate(TRABAJADORES = if_else(OFICIO == "Trabaja", 1, 0)) %>%
  mutate(ESTUDIANTES = if_else(OFICIO == "Estudia", 1, 0)) %>%
  group_by(COD_LOC, COD_UPL, COD_UTAM, COD_ZAT, COD_SECT, COD_SECC, COD_HEX, COD_MNZ, COD_VIVIENDA, COD_FAMILIA) %>%
  summarise(
    PERSONAS_FAMILIA = n(),
    HOMBRES_FAMILIA = sum(HOMBRES),
    MUJERES_FAMILIA = sum(MUJERES),
    DISCAPACITADOS_FAMILIA = sum(DISCAPACITADOS),
    TRABAJADORES_FAMILIA = sum(TRABAJADORES),
    ESTUDIANTES_FAMILIA = sum(ESTUDIANTES)
  )
Censo_Familias
length(Censo_Familias$COD_FAMILIA)   # 2339310 Familias.
sum(Censo_Familias$PERSONAS_FAMILIA) # 7130646 Personas.

# Población por Vivendas:
Censo_Viviendas <- Censo_Familias %>%
  group_by(COD_LOC, COD_UPL, COD_UTAM, COD_ZAT, COD_SECT, COD_SECC, COD_HEX, COD_MNZ, COD_VIVIENDA) %>%
  summarise(
    FAMILIAS_VIVIENDA = n(),
    PERSONAS_VIVIENDA = sum(PERSONAS_FAMILIA),
    HOMBRES_VIVIENDA = sum(HOMBRES_FAMILIA),
    MUJERES_VIVIENDA = sum(MUJERES_FAMILIA),
    DISCAPACITADOS_VIVIENDA = sum(DISCAPACITADOS_FAMILIA),
    TRABAJADORES_VIVIENDA = sum(TRABAJADORES_FAMILIA),
    ESTUDIANTES_VIVIENDA = sum(ESTUDIANTES_FAMILIA)
  )
Censo_Viviendas
length(Censo_Viviendas$COD_VIVIENDA)   # 366707 viviendas (Son únicas)
sum(Censo_Viviendas$PERSONAS_VIVIENDA) # 7130646 Personas.

# Población por Manzanas:
Censo_Manzanas <- Censo_Viviendas %>%
  group_by(COD_LOC, COD_UPL, COD_UTAM, COD_ZAT, COD_SECT, COD_SECC, COD_HEX, COD_MNZ) %>%
  summarise(
    VIVIENDAS_MNZ = n(),
    FAMILIAS_MNZ = sum(FAMILIAS_VIVIENDA),
    PERSONAS_MNZ = sum(PERSONAS_VIVIENDA),
    HOMBRES_MNZ = sum(HOMBRES_VIVIENDA),
    MUJERES_MNZ = sum(MUJERES_VIVIENDA),
    DISCAPACITADOS_MNZ = sum(DISCAPACITADOS_VIVIENDA),
    TRABAJADORES_MNZ = sum(TRABAJADORES_VIVIENDA),
    ESTUDIANTES_MNZ = sum(ESTUDIANTES_VIVIENDA)
  ) 
Censo_Manzanas
length(Censo_Manzanas$COD_MNZ)   # 38384 Manzanas (Son únicas)
sum(Censo_Manzanas$PERSONAS_MNZ) # 7130646 Personas.
length(Shape_Manzanas$COD_MNZ) # Numero de manzanas en el shape :o !
names(Censo_Manzanas) # Verificamos que estén todos los campos

# Población por Hexagonos:
Censo_Hexagonos <- Censo_Manzanas %>%
  group_by(COD_HEX) %>%
  summarise(
    MANZANAS_HEX = n(),
    VIVIENDAS_HEX = sum(VIVIENDAS_MNZ),
    FAMILIAS_HEX = sum(FAMILIAS_MNZ),
    PERSONAS_HEX = sum(PERSONAS_MNZ),
    HOMBRES_HEX = sum(HOMBRES_MNZ),
    MUJERES_HEX = sum(MUJERES_MNZ),
    DISCAPACITADOS_HEX = sum(DISCAPACITADOS_MNZ),
    TRABAJADORES_HEX = sum(TRABAJADORES_MNZ),
    ESTUDIANTES_HEX = sum(ESTUDIANTES_MNZ)
  )
Censo_Hexagonos
length(Censo_Hexagonos$COD_HEX)   # 1773 Hexagonos.
sum(Censo_Hexagonos$PERSONAS_HEX) # 7130646 Personas.

# Población por Secciones:
Censo_Secciones <- Censo_Manzanas %>%
  group_by(COD_SECC) %>%
  summarise(
    MANZANAS_SECC = n(),
    VIVIENDAS_SECC = sum(VIVIENDAS_MNZ),
    FAMILIAS_SECC = sum(FAMILIAS_MNZ),
    PERSONAS_SECC = sum(PERSONAS_MNZ),
    HOMBRES_SECC = sum(HOMBRES_MNZ),
    MUJERES_SECC = sum(MUJERES_MNZ),
    DISCAPACITADOS_SECC = sum(DISCAPACITADOS_MNZ),
    TRABAJADORES_SECC = sum(TRABAJADORES_MNZ),
    ESTUDIANTES_SECC = sum(ESTUDIANTES_MNZ)
  )
Censo_Secciones
length(Censo_Secciones$COD_SECC)   # 2767 Secciones
sum(Censo_Secciones$PERSONAS_SECC) # 7130646 Personas

# Población por Sectores:
Censo_Sectores <- Censo_Manzanas %>%
  group_by(COD_SECT) %>%
  summarise(
    MANZANAS_SECT = n(),
    VIVIENDAS_SECT = sum(VIVIENDAS_MNZ),
    FAMILIAS_SECT = sum(FAMILIAS_MNZ),
    PERSONAS_SECT = sum(PERSONAS_MNZ),
    HOMBRES_SECT = sum(HOMBRES_MNZ),
    MUJERES_SECT = sum(MUJERES_MNZ),
    DISCAPACITADOS_SECT = sum(DISCAPACITADOS_MNZ),
    TRABAJADORES_SECT = sum(TRABAJADORES_MNZ),
    ESTUDIANTES_SECT = sum(ESTUDIANTES_MNZ)
  )
Censo_Sectores
length(Censo_Sectores$COD_SECT)   # 626 Sectores
sum(Censo_Sectores$PERSONAS_SECT) # 7130646 personas

# Población por ZAT:
Censo_ZAT <- Censo_Manzanas %>%
  group_by(COD_ZAT) %>%
  summarise(
    MANZANAS_ZAT = n(),
    VIVIENDAS_ZAT = sum(VIVIENDAS_MNZ),
    FAMILIAS_ZAT = sum(FAMILIAS_MNZ),
    PERSONAS_ZAT = sum(PERSONAS_MNZ),
    HOMBRES_ZAT = sum(HOMBRES_MNZ),
    MUJERES_ZAT = sum(MUJERES_MNZ),
    DISCAPACITADOS_ZAT = sum(DISCAPACITADOS_MNZ),
    TRABAJADORES_ZAT = sum(TRABAJADORES_MNZ),
    ESTUDIANTES_ZAT = sum(ESTUDIANTES_MNZ)
  )
Censo_ZAT
length(Censo_ZAT$COD_ZAT)   # 839 ZAT
sum(Censo_ZAT$PERSONAS_ZAT) # 7130646 Personas.

# Población por UTAM:
Censo_UTAM <- Censo_Manzanas %>%
  group_by(COD_UTAM) %>%
  summarise(
    MANZANAS_UTAM = n(),
    VIVIENDAS_UTAM = sum(VIVIENDAS_MNZ),
    FAMILIAS_UTAM = sum(FAMILIAS_MNZ),
    PERSONAS_UTAM = sum(PERSONAS_MNZ),
    HOMBRES_UTAM = sum(HOMBRES_MNZ),
    MUJERES_UTAM = sum(MUJERES_MNZ),
    DISCAPACITADOS_UTAM = sum(DISCAPACITADOS_MNZ),
    TRABAJADORES_UTAM = sum(TRABAJADORES_MNZ),
    ESTUDIANTES_UTAM = sum(ESTUDIANTES_MNZ)
  )
Censo_UTAM
length(Censo_UTAM$COD_UTAM)   # 112 UTAM (!)
sum(Censo_UTAM$PERSONAS_UTAM) # 7130646 personas.

# Población por UPL:
Censo_UPLs <- Censo_Manzanas %>%
  group_by(COD_UPL) %>%
  summarise(
    MANZANAS_UPL = n(),
    VIVIENDAS_UPL = sum(VIVIENDAS_MNZ),
    FAMILIAS_UPL = sum(FAMILIAS_MNZ),
    PERSONAS_UPL = sum(PERSONAS_MNZ),
    HOMBRES_UPL = sum(HOMBRES_MNZ),
    MUJERES_UPL = sum(MUJERES_MNZ),
    DISCAPACITADOS_UPL = sum(DISCAPACITADOS_MNZ),
    TRABAJADORES_UPL = sum(TRABAJADORES_MNZ),
    ESTUDIANTES_UPL = sum(ESTUDIANTES_MNZ)
  )
Censo_UPLs
length(Censo_UPLs$COD_UPL)   # 33 (!)
sum(Censo_UPLs$PERSONAS_UPL) # 7130646 personas.

# Población por Localidad:
Censo_Localidades <- Censo_Manzanas %>%
  group_by(COD_LOC) %>%
  summarise(
    MANZANAS_LOC = n(),
    VIVIENDAS_LOC = sum(VIVIENDAS_MNZ),
    FAMILIAS_LOC = sum(FAMILIAS_MNZ),
    PERSONAS_LOC = sum(PERSONAS_MNZ),
    HOMBRES_LOC = sum(HOMBRES_MNZ),
    MUJERES_LOC = sum(MUJERES_MNZ),
    DISCAPACITADOS_LOC = sum(DISCAPACITADOS_MNZ),
    TRABAJADORES_LOC = sum(TRABAJADORES_MNZ),
    ESTUDIANTES_LOC = sum(ESTUDIANTES_MNZ)
  )
Censo_Localidades
length(Censo_Localidades$COD_LOC) # 20 (!)
sum(Censo_Localidades$PERSONAS_LOC) # 7130646 personas

# Población por municipio:
Censo_Municipios <- Censo_Manzanas %>%
  mutate(COD_CPOB = as.character("11001000")) %>%
  group_by(COD_CPOB) %>%
  summarise(
    MANZANAS_MPIO = n(),
    VIVIENDAS_MPIO = sum(VIVIENDAS_MNZ),
    FAMILIAS_MPIO = sum(FAMILIAS_MNZ),
    PERSONAS_MPIO = sum(PERSONAS_MNZ),
    HOMBRES_MPIO = sum(HOMBRES_MNZ),
    MUJERES_MPIO = sum(MUJERES_MNZ),
    DISCAPACITADOS_MPIO = sum(DISCAPACITADOS_MNZ),
    TRABAJADORES_MPIO = sum(TRABAJADORES_MNZ),
    ESTUDIANTES_MPIO = sum(ESTUDIANTES_MNZ)
  )
Censo_Municipios
length(Censo_Municipios$COD_CPOB)   # 1
sum(Censo_Municipios$PERSONAS_MPIO) # 7130646

# Verificamos que las sumas den igual:
length(Censo_MNG$COD_PERSONA)
length(Censo_Personas$COD_PERSONA)   # 7130646
sum(Censo_Familias$PERSONAS_FAMILIA)
sum(Censo_Viviendas$PERSONAS_VIVIENDA)
sum(Censo_Manzanas$PERSONAS_MNZ)
sum(Censo_Hexagonos$PERSONAS_HEX)
sum(Censo_Secciones$PERSONAS_SECC)
sum(Censo_Sectores$PERSONAS_SECT)
sum(Censo_ZAT$PERSONAS_ZAT)
sum(Censo_UTAM$PERSONAS_UTAM)
sum(Censo_UPLs$PERSONAS_UPL)
sum(Censo_Localidades$PERSONAS_LOC)
sum(Censo_Municipios$PERSONAS_MPIO)
length(Censo_Manzanas$PERSONAS_MNZ) # Manzanas al final de Df Process: 38384 de 43331.
length(Personas_BOG_Original$COD_ENCUESTAS) # Personas originales: 7181469.

# Tiempo de procesamiento:
End_Time_S2_P3 <- Sys.time()
print(paste("3. Tiempo de filtrado y tratamiento de datos del censo: ", as.duration(End_Time_S2_P3 - Start_Time_S2_P3)))

# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ::: 4. Guardado de datos del censo filtrados y procesados                  :::
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#
# El objetivo es guardar el avance de los nuevos shapes y data frames para
# no tener que volver a filtrar toooodo lo anterior porque es muy lento.
#
Start_Time_S2_P4 <- Sys.time()

# Guardo los nuevos CSV filtrados y ordenados (Nuevo método):
fwrite(Censo_Municipios, "./Data/2_Processing/2_Population/Censo_Municipios.csv", row.names = FALSE)
fwrite(Censo_Localidades, "./Data/2_Processing/2_Population/Censo_Localidades.csv", row.names = FALSE)
fwrite(Censo_UPLs, "./Data/2_Processing/2_Population/Censo_UPLs.csv", row.names = FALSE)
fwrite(Censo_UTAM, "./Data/2_Processing/2_Population/Censo_UTAM.csv", row.names = FALSE)
fwrite(Censo_ZAT, "./Data/2_Processing/2_Population/Censo_ZAT.csv", row.names = FALSE)
fwrite(Censo_Sectores, "./Data/2_Processing/2_Population/Censo_Sectores.csv", row.names = FALSE)
fwrite(Censo_Secciones, "./Data/2_Processing/2_Population/Censo_Secciones.csv", row.names = FALSE)
fwrite(Censo_Hexagonos, "./Data/2_Processing/2_Population/Censo_Hexagonos.csv", row.names = FALSE)
fwrite(Censo_Manzanas, "./Data/2_Processing/2_Population/Censo_Manzanas.csv", row.names = FALSE)
fwrite(Censo_Viviendas, "./Data/2_Processing/2_Population/Censo_Viviendas.csv", row.names = FALSE)
fwrite(Censo_Familias, "./Data/2_Processing/2_Population/Censo_Familias.csv", row.names = FALSE)
fwrite(Censo_Personas, "./Data/2_Processing/2_Population/Censo_Personas.csv", row.names = FALSE)

# Recarga de CSV (Nuevo método):
Censo_Municipios <- as.data.frame(fread("./Data/2_Processing/2_Population/Censo_Municipios.csv", sep=",", na = "null", colClasses=c("COD_CPOB"="character")))
Censo_Localidades <- as.data.frame(fread("./Data/2_Processing/2_Population/Censo_Localidades.csv", sep=",", na = "null", colClasses=c("COD_LOC"="character")))
Censo_UPLs <- as.data.frame(fread("./Data/2_Processing/2_Population/Censo_UPLs.csv", sep=",", na = "null", colClasses=c("COD_UPL"="character")))
Censo_UTAM <- as.data.frame(fread("./Data/2_Processing/2_Population/Censo_UTAM.csv", sep=",", na = "null", colClasses=c("COD_UTAM"="character")))
Censo_ZAT <- as.data.frame(fread("./Data/2_Processing/2_Population/Censo_ZAT.csv", sep=",", na = "null", colClasses=c("COD_ZAT"="character")))
Censo_Sectores <- as.data.frame(fread("./Data/2_Processing/2_Population/Censo_Sectores.csv", sep=",", na = "null", colClasses=c("COD_SECT"="character")))
Censo_Secciones <- as.data.frame(fread("./Data/2_Processing/2_Population/Censo_Secciones.csv", sep=",", na = "null", colClasses=c("COD_SECC"="character")))
Censo_Hexagonos <- as.data.frame(fread("./Data/2_Processing/2_Population/Censo_Hexagonos.csv", sep=",", na = "null", colClasses=c("COD_HEX"="character")))
Censo_Manzanas <- as.data.frame(fread("./Data/2_Processing/2_Population/Censo_Manzanas.csv", sep=",", na = "null", colClasses=c("COD_LOC"="character", "COD_UPL"="character", "COD_UTAM"="character", "COD_ZAT"="character", "COD_SECT"="character", "COD_SECC"="character", "COD_HEX"="character", "COD_MNZ"="character")))
Censo_Viviendas <- as.data.frame(fread("./Data/2_Processing/2_Population/Censo_Viviendas.csv", sep=",", na = "null", colClasses=c("COD_LOC"="character", "COD_UPL"="character", "COD_UTAM"="character", "COD_ZAT"="character", "COD_SECT"="character", "COD_SECC"="character", "COD_HEX"="character", "COD_MNZ"="character", "COD_VIVIENDA"="character")))
Censo_Familias <- as.data.frame(fread("./Data/2_Processing/2_Population/Censo_Familias.csv", sep=",", na = "null", colClasses=c("COD_LOC"="character", "COD_UPL"="character", "COD_UTAM"="character", "COD_ZAT"="character", "COD_SECT"="character", "COD_SECC"="character", "COD_HEX"="character", "COD_MNZ"="character", "COD_VIVIENDA"="character", "COD_FAMILIA"="character")))
Censo_Personas <- as.data.frame(fread("./Data/2_Processing/2_Population/Censo_Personas.csv", sep=",", na = "null", colClasses=c("COD_LOC"="character", "COD_UPL"="character", "COD_UTAM"="character", "COD_ZAT"="character", "COD_SECT"="character", "COD_SECC"="character", "COD_HEX"="character", "COD_MNZ"="character", "COD_VIVIENDA"="character", "COD_FAMILIA"="character", "COD_PERSONA"="character")))

# Tiempo de procesamiento:
End_Time_S2_P4 <- Sys.time()
print(paste("4. Tiempo de guardado y carga de los nuevos CSV de población: ", as.duration(End_Time_S2_P4 - Start_Time_S2_P4)))

# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ::: 5. Combinación de datos de población con los shapes en diferentes escalas :::
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#
# El objetivo es crear combinar los Shapes con los dataframes del Censo en
# todas las escalas disponibles.
#
Start_Time_S2_P5 <- Sys.time()

# --- Comenzamos a integrar los datos de población, con los shapes ---

# Fusionamos el Shape Municipios:
Shape_Municipios # 1
Censo_Municipios # 1
Shape_Censo_Municipios <- Shape_Municipios %>%
  mutate(COD_CPOB = as.character(COD_CPOB)) %>%
  left_join(Censo_Municipios, by = "COD_CPOB") %>%
  # Calculo de densidades:
  mutate(
    AREA_MPIO = set_units(st_area(geom), NULL)/1000000, # km^2
    DENSIDAD_MPIO = PERSONAS_MPIO/AREA_MPIO
    ) %>%
  select(COD_CPOB, NOM_CPOB, MANZANAS_MPIO, VIVIENDAS_MPIO, FAMILIAS_MPIO, PERSONAS_MPIO, AREA_MPIO, DENSIDAD_MPIO, HOMBRES_MPIO, MUJERES_MPIO, DISCAPACITADOS_MPIO, TRABAJADORES_MPIO, ESTUDIANTES_MPIO)
Shape_Censo_Municipios[is.na(Shape_Censo_Municipios)] <- 0
Shape_Censo_Municipios
length(Shape_Censo_Municipios$COD_CPOB)   # 1
sum(Shape_Censo_Municipios$PERSONAS_MPIO) # 7130646 personas.

# Fusionamos el Shape Localidades:
Shape_Localidades # 19
Censo_Localidades # 20
Shape_Censo_Localidades <- Shape_Localidades %>%
  mutate(COD_LOC = as.character(COD_LOC)) %>%
  left_join(Censo_Localidades, by = "COD_LOC") %>%
  # Calculo de densidades:
  mutate(
    AREA_LOC = set_units(st_area(geom), NULL)/1000000, # km^2
    DENSIDAD_LOC = PERSONAS_LOC/AREA_LOC
  ) %>%
  select(COD_LOC, NOM_LOC, MANZANAS_LOC, VIVIENDAS_LOC, FAMILIAS_LOC, PERSONAS_LOC, AREA_LOC, DENSIDAD_LOC, HOMBRES_LOC, MUJERES_LOC, DISCAPACITADOS_LOC, TRABAJADORES_LOC, ESTUDIANTES_LOC)
Shape_Censo_Localidades[is.na(Shape_Censo_Localidades)] <- 0
Shape_Censo_Localidades
length(Shape_Censo_Localidades$COD_LOC)    # 19
sum(Shape_Censo_Localidades$PERSONAS_LOC)  # 7130131

# Fusionamos el Shape UPLs:
Shape_UPLs # 32
Censo_UPLs # 33
Shape_Censo_UPLs <- Shape_UPLs %>%
  mutate(COD_UPL = as.character(COD_UPL)) %>%
  left_join(Censo_UPLs, by = "COD_UPL") %>%
  # Calculo de densidades:
  mutate(
    AREA_UPL = set_units(st_area(geom), NULL)/1000000, # km^2
    DENSIDAD_UPL = PERSONAS_UPL/AREA_UPL
  ) %>%
  dplyr::filter(!is.na(sf::st_is_valid(geom)), sf::st_is_valid(geom)) %>% # Elimina los Shapes vacíos
  sf::st_make_valid() %>%
  select(COD_UPL, NOM_UPL, ZONA_UPL, MANZANAS_UPL, VIVIENDAS_UPL, FAMILIAS_UPL, PERSONAS_UPL, AREA_UPL, DENSIDAD_UPL, HOMBRES_UPL, MUJERES_UPL, DISCAPACITADOS_UPL, TRABAJADORES_UPL, ESTUDIANTES_UPL)
Shape_Censo_UPLs[is.na(Shape_Censo_UPLs)] <- 0
Shape_Censo_UPLs
length(Shape_Censo_UPLs$COD_UPL)   # 32
sum(Shape_Censo_UPLs$PERSONAS_UPL) # 7130131 Personas

# Fusionamos el Shape UTAM:
Shape_UTAM # 111
Censo_UTAM # 112
Shape_Censo_UTAM <- Shape_UTAM %>%
  mutate(COD_UTAM = as.character(COD_UTAM)) %>%
  left_join(Censo_UTAM, by = "COD_UTAM") %>%
  # Calculo de densidades:
  mutate(
    AREA_UTAM = set_units(st_area(geom), NULL)/1000000, # km^2
    DENSIDAD_UTAM = PERSONAS_UTAM/AREA_UTAM
  ) %>%
  select(COD_LOC, COD_UPL, COD_UTAM, MANZANAS_UTAM, VIVIENDAS_UTAM, FAMILIAS_UTAM, PERSONAS_UTAM, AREA_UTAM, DENSIDAD_UTAM, HOMBRES_UTAM, MUJERES_UTAM, DISCAPACITADOS_UTAM, TRABAJADORES_UTAM, ESTUDIANTES_UTAM)
Shape_Censo_UTAM[is.na(Shape_Censo_UTAM)] <- 0
Shape_Censo_UTAM
length(Shape_Censo_UTAM$COD_UTAM)   # 111
sum(Shape_Censo_UTAM$PERSONAS_UTAM) # 7094309 Personas

# Verificar diferencias
length(Shape_UTAM$COD_UTAM) #111
length(Censo_UTAM$COD_UTAM) #112

# Fusionamos el Shape ZAT:
Shape_ZAT # 892
Censo_ZAT # 839
Shape_Censo_ZAT <- Shape_ZAT %>%
  mutate(COD_ZAT = as.character(COD_ZAT)) %>%
  left_join(Censo_ZAT, by = "COD_ZAT") %>%
  # Calculo de densidades:
  mutate(
    AREA_ZAT = set_units(st_area(geom), NULL)/1000000, # km^2
    DENSIDAD_ZAT = PERSONAS_ZAT/AREA_ZAT
  ) %>%
  select(COD_LOC, COD_UPL, COD_UTAM, COD_ZAT, MANZANAS_ZAT, VIVIENDAS_ZAT, FAMILIAS_ZAT, PERSONAS_ZAT, AREA_ZAT, DENSIDAD_ZAT, HOMBRES_ZAT, MUJERES_ZAT, DISCAPACITADOS_ZAT, TRABAJADORES_ZAT, ESTUDIANTES_ZAT)
Shape_Censo_ZAT[is.na(Shape_Censo_ZAT)] <- 0
Shape_Censo_ZAT
length(Shape_Censo_ZAT$COD_ZAT) # 892
sum(Shape_Censo_ZAT$PERSONAS_ZAT) # 7102761 Personas

# Verificar diferencias
length(Shape_ZAT$COD_ZAT)
length(Censo_ZAT$COD_ZAT)

# Fusionamos el Shape Sectores:
Shape_Sectores # 631
Censo_Sectores # 669
Shape_Censo_Sectores <- Shape_Sectores %>%
  mutate(COD_SECT = as.character(COD_SECT)) %>%
  left_join(Censo_Sectores, by = "COD_SECT") %>%
  # Calculo de densidades:
  mutate(
    AREA_SECT = set_units(st_area(geom), NULL)/1000000, # km^2
    DENSIDAD_SECT = PERSONAS_SECT/AREA_SECT
  ) %>%
  select(COD_LOC, COD_UPL, COD_UTAM, COD_ZAT, COD_SECT, MANZANAS_SECT, VIVIENDAS_SECT, FAMILIAS_SECT, PERSONAS_SECT,AREA_SECT, DENSIDAD_SECT, HOMBRES_SECT, MUJERES_SECT, DISCAPACITADOS_SECT, TRABAJADORES_SECT, ESTUDIANTES_SECT)
Shape_Censo_Sectores[is.na(Shape_Censo_Sectores)] <- 0
Shape_Censo_Sectores # 669 sectores
length(Shape_Censo_Sectores$COD_SECT) # 626
sum(Shape_Censo_Sectores$PERSONAS_SECT) # 7130646 Personas

# Fusionamos el Shape Secciones:
Shape_Secciones # 2796
Censo_Secciones # 2767
Shape_Censo_Secciones <- Shape_Secciones %>%
  mutate(COD_SECC = as.character(COD_SECC)) %>%
  left_join(Censo_Secciones, by = "COD_SECC") %>%
  # Calculo de densidades:
  mutate(
    AREA_SECC = set_units(st_area(geom), NULL)/1000000, # km^2
    DENSIDAD_SECC = PERSONAS_SECC/AREA_SECC
  ) %>%
  select(COD_LOC, COD_UPL, COD_UTAM, COD_ZAT, COD_SECT, COD_SECC, MANZANAS_SECC, VIVIENDAS_SECC, FAMILIAS_SECC, PERSONAS_SECC, AREA_SECC, DENSIDAD_SECC, HOMBRES_SECC, MUJERES_SECC, DISCAPACITADOS_SECC, TRABAJADORES_SECC, ESTUDIANTES_SECC)
Shape_Censo_Secciones[is.na(Shape_Censo_Secciones)] <- 0
Shape_Censo_Secciones
length(Shape_Censo_Secciones$COD_SECC)   # 2767
sum(Shape_Censo_Secciones$PERSONAS_SECC) # 7130646

# Fusionamos el Shape Hexágonos:
Shape_Hexagonos # 2185
Censo_Hexagonos # 1773
Shape_Censo_Hexagonos <- Shape_Hexagonos %>%
  mutate(COD_HEX = COD_HEX) %>%
  left_join(Censo_Hexagonos, by = "COD_HEX") %>%
  # Calculo de densidades:
  mutate(
    AREA_HEX = set_units(st_area(geom), NULL)/1000000, # km^2
    DENSIDAD_HEX = PERSONAS_HEX/AREA_HEX
  ) %>%
  select(COD_LOC, COD_UPL, COD_UTAM, COD_ZAT, COD_SECT, COD_SECC, COD_HEX, MANZANAS_HEX, VIVIENDAS_HEX, FAMILIAS_HEX, PERSONAS_HEX, AREA_HEX, DENSIDAD_HEX, HOMBRES_HEX, MUJERES_HEX, DISCAPACITADOS_HEX, TRABAJADORES_HEX, ESTUDIANTES_HEX)
Shape_Censo_Hexagonos[is.na(Shape_Censo_Hexagonos)] <- 0
Shape_Censo_Hexagonos
length(Shape_Censo_Hexagonos$COD_HEX)   # 1773
sum(Shape_Censo_Hexagonos$PERSONAS_HEX) # 7130646

# Fusionamos el Shape Manzanas:
Shape_Manzanas # 43331
Censo_Manzanas # 38384
Shape_Censo_Manzanas <- Shape_Manzanas %>%
  select(COD_MNZ) %>%
  mutate(COD_MNZ = as.character(COD_MNZ)) %>%
  left_join(Censo_Manzanas, by = "COD_MNZ") %>%
  # Calculo de densidades:
  mutate(
    AREA_MNZ = set_units(st_area(geom), NULL)/1000000, # km^2
    DENSIDAD_MNZ = PERSONAS_MNZ/AREA_MNZ
  ) %>%
  select(COD_LOC, COD_UPL, COD_UTAM, COD_ZAT, COD_SECT, COD_SECC, COD_HEX, COD_MNZ, VIVIENDAS_MNZ, FAMILIAS_MNZ, PERSONAS_MNZ, AREA_MNZ, DENSIDAD_MNZ, HOMBRES_MNZ, MUJERES_MNZ, DISCAPACITADOS_MNZ, TRABAJADORES_MNZ, ESTUDIANTES_MNZ)
Shape_Censo_Manzanas[is.na(Shape_Censo_Manzanas)] <- 0
Shape_Censo_Manzanas
length(Shape_Censo_Manzanas$COD_MNZ) # 43331
sum(Shape_Censo_Manzanas$PERSONAS_MNZ) # 7130646

# Verificamos que no se pierdan personas
sum(Shape_Censo_Municipios$PERSONAS_MPIO) # 7130646
sum(Shape_Censo_Localidades$PERSONAS_LOC) # 7130131
sum(Shape_Censo_UPLs$PERSONAS_UPL)        # 7130131
sum(Shape_Censo_UTAM$PERSONAS_UTAM)       # 7094309 !
sum(Shape_Censo_ZAT$PERSONAS_ZAT)         # 7102761 !
sum(Shape_Censo_Sectores$PERSONAS_SECT)   # 7130646
sum(Shape_Censo_Secciones$PERSONAS_SECC)  # 7130646
sum(Shape_Censo_Hexagonos$PERSONAS_HEX)   # 7130646
sum(Shape_Censo_Manzanas$PERSONAS_MNZ)    # 7130646

# Tiempo de procesamiento:
End_Time_S2_P5 <- Sys.time()
print(paste("5. Tiempo de combinación de Shapes y Población: ", as.duration(End_Time_S2_P5 - Start_Time_S2_P5)))

# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ::: 6. Ploteo de resultados (Shapes con población) tanto en 2D como 3D     :::
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#
# Revisión de los resultados: Shapes finales sin información adicional (Vacios)
#
Start_Time_S2_P6 <- Sys.time()

# Vemos en mapa
P1 <- Shape_Censo_Municipios %>%
  ggplot(aes(fill = PERSONAS_MPIO)) +
  geom_sf()
P2 <- Shape_Censo_Localidades %>%
  ggplot(aes(fill = PERSONAS_LOC)) +
  geom_sf()
P3 <- Shape_Censo_UPLs %>%
  ggplot(aes(fill = PERSONAS_UPL)) +
  geom_sf()
P4 <- Shape_Censo_UTAM %>%
  ggplot(aes(fill = PERSONAS_UTAM)) +
  geom_sf()
P5 <- Shape_Censo_ZAT %>%
  ggplot(aes(fill = PERSONAS_ZAT)) +
  geom_sf()
P6 <- Shape_Censo_Sectores %>%
  ggplot(aes(fill = PERSONAS_SECT)) +
  geom_sf()
P7 <- Shape_Censo_Secciones %>%
  ggplot(aes(fill = PERSONAS_SECC)) +
  geom_sf()
P8 <- Shape_Censo_Hexagonos %>%
  ggplot(aes(fill = PERSONAS_HEX)) +
  geom_sf()
P9 <- Shape_Censo_Manzanas %>%
  ggplot(aes(fill = PERSONAS_MNZ)) +
  geom_sf()

Resolucion_2D <- "2160p"
Resolucion_3D <- "240p"
Escala <- 50

# ...::: Mapas de población :::...

# --- 2D Población ---
Plot_Color(
  Shape_Plotear = Shape_Censo_Municipios,
  Criterio_Color = Shape_Censo_Municipios$PERSONAS_MPIO,
  Paleta_Color = scale_fill_gradient(low="bisque3", high="darkred"),
  Resolution = Resolucion_2D,
  Line_Width = 0.8,
  Output_Path = "./Data/3_Results/2_Population/1_Pop/",
  Output_Name_ES = "ES_Mapa_2D_Poblacion_Municipios",
  Output_Name_EN = "EN_Map_2D_Population_Municipios"
)
Plot_Color(
  Shape_Plotear = Shape_Censo_Localidades,
  Criterio_Color = Shape_Censo_Localidades$PERSONAS_LOC,
  Paleta_Color = scale_fill_gradient(low="bisque3", high="darkred"),
  Resolution = Resolucion_2D,
  Line_Width = 0.8,
  Output_Path = "./Data/3_Results/2_Population/1_Pop/",
  Output_Name_ES = "ES_Mapa_2D_Poblacion_Localidades",
  Output_Name_EN = "EN_Map_2D_Population_Localidades"
)
Plot_Color(
  Shape_Plotear = Shape_Censo_UPLs,
  Criterio_Color = Shape_Censo_UPLs$PERSONAS_UPL,
  Paleta_Color = scale_fill_gradient(low="bisque3", high="darkred"),
  Resolution = Resolucion_2D,
  Line_Width = 0.8,
  Output_Path = "./Data/3_Results/2_Population/1_Pop/",
  Output_Name_ES = "ES_Mapa_2D_Poblacion_UPLs",
  Output_Name_EN = "EN_Map_2D_Population_UPLs"
)
Plot_Color(
  Shape_Plotear = Shape_Censo_UTAM,
  Criterio_Color = Shape_Censo_UTAM$PERSONAS_UTAM,
  Paleta_Color = scale_fill_gradient(low="bisque3", high="darkred"),
  Resolution = Resolucion_2D,
  Line_Width = 0.6,
  Output_Path = "./Data/3_Results/2_Population/1_Pop/",
  Output_Name_ES = "ES_Mapa_2D_Poblacion_UTAM",
  Output_Name_EN = "EN_Map_2D_Population_UTAM"
)
Plot_Color(
  Shape_Plotear = Shape_Censo_ZAT,
  Criterio_Color = Shape_Censo_ZAT$PERSONAS_ZAT,
  Paleta_Color = scale_fill_gradient(low="bisque3", high="darkred"),
  Resolution = Resolucion_2D,
  Line_Width = 0.6,
  Output_Path = "./Data/3_Results/2_Population/1_Pop/",
  Output_Name_ES = "ES_Mapa_2D_Poblacion_ZAT",
  Output_Name_EN = "EN_Map_2D_Population_ZAT"
)
Plot_Color(
  Shape_Plotear = Shape_Censo_Sectores,
  Criterio_Color = Shape_Censo_Sectores$PERSONAS_SECT,
  Paleta_Color = scale_fill_gradient(low="bisque3", high="darkred"),
  Resolution = Resolucion_2D,
  Line_Width = 0.6,
  Output_Path = "./Data/3_Results/2_Population/1_Pop/",
  Output_Name_ES = "ES_Mapa_2D_Poblacion_Sectores",
  Output_Name_EN = "EN_Map_2D_Population_Sectores"
)
Plot_Color(
  Shape_Plotear = Shape_Censo_Secciones,
  Criterio_Color = Shape_Censo_Secciones$PERSONAS_SECC,
  Paleta_Color = scale_fill_gradient(low="bisque3", high="darkred"),
  Resolution = Resolucion_2D,
  Line_Width = 0.2,
  Output_Path = "./Data/3_Results/2_Population/1_Pop/",
  Output_Name_ES = "ES_Mapa_2D_Poblacion_Secciones",
  Output_Name_EN = "EN_Map_2D_Population_Secciones"
)
Plot_Color(
  Shape_Plotear = Shape_Censo_Hexagonos,
  Criterio_Color = Shape_Censo_Hexagonos$PERSONAS_HEX,
  Paleta_Color = scale_fill_gradient(low="bisque3", high="darkred"),
  Resolution = Resolucion_2D,
  Line_Width = 0.2,
  Output_Path = "./Data/3_Results/2_Population/1_Pop/",
  Output_Name_ES = "ES_Mapa_2D_Poblacion_Hexagonos",
  Output_Name_EN = "EN_Map_2D_Population_Hexagonos"
)
Plot_Color(
  Shape_Plotear = Shape_Censo_Manzanas,
  Criterio_Color = Shape_Censo_Manzanas$PERSONAS_MNZ,
  Paleta_Color = scale_fill_gradient(low="bisque3", high="darkred"),
  Resolution = Resolucion_2D,
  Line_Width = 0.1,
  Output_Path = "./Data/3_Results/2_Population/1_Pop/",
  Output_Name_ES = "ES_Mapa_2D_Poblacion_Manzanas",
  Output_Name_EN = "EN_Map_2D_Population_Manzanas"
)

# --- 3D Población ---
# PlotMapa3D(
#   Shape_Plotear = Shape_Censo_Municipios,
#   Criterio_Extrusion = Shape_Censo_Municipios$PERSONAS_MPIO,
#   Paleta_Color = scale_fill_gradient(low="bisque3", high="darkred"),
#   Resolution = Resolucion_3D,
#   Scale = Escala,
#   Output_Path = "./Data/3_Results/2_Population/1_Pop/",
#   Output_Name_ES = "ES_Mapa_3D_Poblacion_Municipios",
#   Output_Name_EN = "EN_Map_3D_Population_Municipios"
# )
# PlotMapa3D(
#   Shape_Plotear = Shape_Censo_Localidades,
#   Criterio_Extrusion = Shape_Censo_Localidades$PERSONAS_LOC,
#   Paleta_Color = scale_fill_gradient(low="bisque3", high="darkred"),
#   Resolution = Resolucion_3D,
#   Scale = Escala,
#   Output_Path = "./Data/3_Results/2_Population/1_Pop/",
#   Output_Name_ES = "ES_Mapa_3D_Poblacion_Localidades",
#   Output_Name_EN = "EN_Map_3D_Population_Localidades"
# )
# PlotMapa3D(
#   Shape_Plotear = Shape_Censo_UPLs,
#   Criterio_Extrusion = Shape_Censo_UPLs$PERSONAS_UPL,
#   Paleta_Color = scale_fill_gradient(low="bisque3", high="darkred"),
#   Resolution = Resolucion_3D,
#   Scale = Escala,
#   Output_Path = "./Data/3_Results/2_Population/1_Pop/",
#   Output_Name_ES = "ES_Mapa_3D_Poblacion_UPLs",
#   Output_Name_EN = "EN_Map_3D_Population_UPLs",
# )
# PlotMapa3D(
#   Shape_Plotear = Shape_Censo_UTAM,
#   Criterio_Extrusion = Shape_Censo_UTAM$PERSONAS_UTAM,
#   Paleta_Color = scale_fill_gradient(low="bisque3", high="darkred"),
#   Resolution = Resolucion_3D,
#   Scale = Escala,
#   Output_Path = "./Data/3_Results/2_Population/1_Pop/",
#   Output_Name_ES = "ES_Mapa_3D_Poblacion_UTAM",
#   Output_Name_EN = "EN_Map_3D_Population_UTAM"
# )
# PlotMapa3D(
#   Shape_Plotear = Shape_Censo_ZAT,
#   Criterio_Extrusion = Shape_Censo_ZAT$PERSONAS_ZAT,
#   Paleta_Color = scale_fill_gradient(low="bisque3", high="darkred"),
#   Resolution = Resolucion_3D,
#   Scale = Escala,
#   Output_Path = "./Data/3_Results/2_Population/1_Pop/",
#   Output_Name_ES = "ES_Mapa_3D_Poblacion_ZAT",
#   Output_Name_EN = "EN_Map_3D_Population_ZAT"
# )
# PlotMapa3D(
#   Shape_Plotear = Shape_Censo_Sectores,
#   Criterio_Extrusion = Shape_Censo_Sectores$PERSONAS_SECT,
#   Paleta_Color = scale_fill_gradient(low="bisque3", high="darkred"),
#   Resolution = Resolucion_3D,
#   Scale = Escala,
#   Output_Path = "./Data/3_Results/2_Population/1_Pop/",
#   Output_Name_ES = "ES_Mapa_3D_Poblacion_Sectores",
#   Output_Name_EN = "EN_Map_3D_Population_Sectores"
# )
# PlotMapa3D(
#   Shape_Plotear = Shape_Censo_Secciones,
#   Criterio_Extrusion = Shape_Censo_Secciones$PERSONAS_SECC,
#   Paleta_Color = scale_fill_gradient(low="bisque3", high="darkred"),
#   Resolution = Resolucion_3D,
#   Scale = Escala,
#   Output_Path = "./Data/3_Results/2_Population/1_Pop/",
#   Output_Name_ES = "ES_Mapa_3D_Poblacion_Secciones",
#   Output_Name_EN = "EN_Map_3D_Population_Secciones"
# )
# PlotMapa3D(
#   Shape_Plotear = Shape_Censo_Hexagonos,
#   Criterio_Extrusion = Shape_Censo_Hexagonos$PERSONAS_HEX,
#   Paleta_Color = scale_fill_gradient(low="bisque3", high="darkred"),
#   Resolution = Resolucion_3D,
#   Scale = Escala,
#   Output_Path = "./Data/3_Results/2_Population/1_Pop/",
#   Output_Name_ES = "ES_Mapa_3D_Poblacion_Hexagonos",
#   Output_Name_EN = "EN_Map_3D_Population_Hexagonos"
# )
PlotMapa3D(
  Shape_Plotear = Shape_Censo_Manzanas,
  Criterio_Extrusion = Shape_Censo_Manzanas$PERSONAS_MNZ,
  Paleta_Color = scale_fill_gradient(low="bisque3", high="darkred"),
  Resolution = Resolucion_3D,
  Scale = Escala,
  Output_Path = "./Data/3_Results/2_Population/1_Pop/",
  Output_Name_ES = "ES_Mapa_3D_Poblacion_Manzanas",
  Output_Name_EN = "EN_Map_3D_Population_Manzanas"
)

# ...::: Mapas de densidad :::...
# --- 1D Densidad ---
Plot_Color(
  Shape_Plotear = Shape_Censo_Municipios,
  Criterio_Color = Shape_Censo_Municipios$DENSIDAD_MPIO,
  Paleta_Color = scale_fill_gradient(low="bisque3", high="purple4"),
  Resolution = Resolucion_2D,
  Line_Width = 0.8,
  Output_Path = "./Data/3_Results/2_Population/2_Den/",
  Output_Name_ES = "ES_Mapa_2D_Densidad_Municipios",
  Output_Name_EN = "EN_Map_2D_Density_Municipios"
)
Plot_Color(
  Shape_Plotear = Shape_Censo_Localidades,
  Criterio_Color = Shape_Censo_Localidades$DENSIDAD_LOC,
  Paleta_Color = scale_fill_gradient(low="bisque3", high="purple4"),
  Resolution = Resolucion_2D,
  Line_Width = 0.8,
  Output_Path = "./Data/3_Results/2_Population/2_Den/",
  Output_Name_ES = "ES_Mapa_2D_Densidad_Localidades",
  Output_Name_EN = "EN_Map_2D_Density_Localidades"
)
Plot_Color(
  Shape_Plotear = Shape_Censo_UPLs,
  Criterio_Color = Shape_Censo_UPLs$DENSIDAD_UPL,
  Paleta_Color = scale_fill_gradient(low="bisque3", high="purple4"),
  Resolution = Resolucion_2D,
  Line_Width = 0.8,
  Output_Path = "./Data/3_Results/2_Population/2_Den/",
  Output_Name_ES = "ES_Mapa_2D_Densidad_UPLs",
  Output_Name_EN = "EN_Map_2D_Density_UPLs"
)
Plot_Color(
  Shape_Plotear = Shape_Censo_UTAM,
  Criterio_Color = Shape_Censo_UTAM$DENSIDAD_UTAM,
  Paleta_Color = scale_fill_gradient(low="bisque3", high="purple4"),
  Resolution = Resolucion_2D,
  Line_Width = 0.6,
  Output_Path = "./Data/3_Results/2_Population/2_Den/",
  Output_Name_ES = "ES_Mapa_2D_Densidad_UTAM",
  Output_Name_EN = "EN_Map_2D_Density_UTAM"
)
Plot_Color(
  Shape_Plotear = Shape_Censo_ZAT,
  Criterio_Color = Shape_Censo_ZAT$DENSIDAD_ZAT,
  Paleta_Color = scale_fill_gradient(low="bisque3", high="purple4"),
  Resolution = Resolucion_2D,
  Line_Width = 0.6,
  Output_Path = "./Data/3_Results/2_Population/2_Den/",
  Output_Name_ES = "ES_Mapa_2D_Densidad_ZAT",
  Output_Name_EN = "EN_Map_2D_Density_ZAT"
)
Plot_Color(
  Shape_Plotear = Shape_Censo_Sectores,
  Criterio_Color = Shape_Censo_Sectores$DENSIDAD_SECT,
  Paleta_Color = scale_fill_gradient(low="bisque3", high="purple4"),
  Resolution = Resolucion_2D,
  Line_Width = 0.6,
  Output_Path = "./Data/3_Results/2_Population/2_Den/",
  Output_Name_ES = "ES_Mapa_2D_Densidad_Sectores",
  Output_Name_EN = "EN_Map_2D_Density_Sectores"
)
Plot_Color(
  Shape_Plotear = Shape_Censo_Secciones,
  Criterio_Color = Shape_Censo_Secciones$DENSIDAD_SECC,
  Paleta_Color = scale_fill_gradient(low="bisque3", high="purple4"),
  Resolution = Resolucion_2D,
  Line_Width = 0.2,
  Output_Path = "./Data/3_Results/2_Population/2_Den/",
  Output_Name_ES = "ES_Mapa_2D_Densidad_Secciones",
  Output_Name_EN = "EN_Map_2D_Density_Secciones"
)
Plot_Color(
  Shape_Plotear = Shape_Censo_Hexagonos,
  Criterio_Color = Shape_Censo_Hexagonos$DENSIDAD_HEX,
  Paleta_Color = scale_fill_gradient(low="bisque3", high="purple4"),
  Resolution = Resolucion_2D,
  Line_Width = 0.2,
  Output_Path = "./Data/3_Results/2_Population/2_Den/",
  Output_Name_ES = "ES_Mapa_2D_Densidad_Hexagonos",
  Output_Name_EN = "EN_Map_2D_Density_Hexagonos"
)
Plot_Color(
  Shape_Plotear = Shape_Censo_Manzanas,
  Criterio_Color = Shape_Censo_Manzanas$DENSIDAD_MNZ,
  Paleta_Color = scale_fill_gradient(low="bisque3", high="purple4"),
  Resolution = Resolucion_2D,
  Line_Width = 0.1,
  Output_Path = "./Data/3_Results/2_Population/2_Den/",
  Output_Name_ES = "ES_Mapa_2D_Densidad_Manzanas",
  Output_Name_EN = "EN_Map_2D_Density_Manzanas"
)

# --- 3D Densidad ---
# PlotMapa3D(
#   Shape_Plotear = Shape_Censo_Municipios,
#   Criterio_Extrusion = Shape_Censo_Municipios$DENSIDAD_MPIO,
#   Paleta_Color = scale_fill_gradient(low="bisque3", high="purple4"),
#   Resolution = Resolucion_3D,
#   Scale = Escala,
#   Output_Path = "./Data/3_Results/2_Population/2_Den/",
#   Output_Name_ES = "ES_Mapa_3D_Densidad_Municipios",
#   Output_Name_EN = "EN_Map_3D_Density_Municipios"
# )
# PlotMapa3D(
#   Shape_Plotear = Shape_Censo_Localidades,
#   Criterio_Extrusion = Shape_Censo_Localidades$DENSIDAD_LOC,
#   Paleta_Color = scale_fill_gradient(low="bisque3", high="purple4"),
#   Resolution = Resolucion_3D,
#   Scale = Escala,
#   Output_Path = "./Data/3_Results/2_Population/2_Den/",
#   Output_Name_ES = "ES_Mapa_3D_Densidad_Localidades",
#   Output_Name_EN = "EN_Map_3D_Density_Localidades"
# )
# PlotMapa3D(
#   Shape_Plotear = Shape_Censo_UPLs,
#   Criterio_Extrusion = Shape_Censo_UPLs$DENSIDAD_UPL,
#   Paleta_Color = scale_fill_gradient(low="bisque3", high="purple4"),
#   Resolution = Resolucion_3D,
#   Scale = Escala,
#   Output_Path = "./Data/3_Results/2_Population/2_Den/",
#   Output_Name_ES = "ES_Mapa_3D_Densidad_UPLs",
#   Output_Name_EN = "EN_Map_3D_Density_UPLs"
# )
# PlotMapa3D(
#   Shape_Plotear = Shape_Censo_UTAM,
#   Criterio_Extrusion = Shape_Censo_UTAM$DENSIDAD_UTAM,
#   Paleta_Color = scale_fill_gradient(low="bisque3", high="purple4"),
#   Resolution = Resolucion_3D,
#   Scale = Escala,
#   Output_Path = "./Data/3_Results/2_Population/2_Den/",
#   Output_Name_ES = "ES_Mapa_3D_Densidad_UTAM",
#   Output_Name_EN = "EN_Map_3D_Density_UTAM"
# )
# PlotMapa3D(
#   Shape_Plotear = Shape_Censo_ZAT,
#   Criterio_Extrusion = Shape_Censo_ZAT$DENSIDAD_ZAT,
#   Paleta_Color = scale_fill_gradient(low="bisque3", high="purple4"),
#   Resolution = Resolucion_3D,
#   Scale = Escala,
#   Output_Path = "./Data/3_Results/2_Population/2_Den/",
#   Output_Name_ES = "ES_Mapa_3D_Densidad_ZAT",
#   Output_Name_EN = "EN_Map_3D_Density_ZAT"
# )
# PlotMapa3D(
#   Shape_Plotear = Shape_Censo_Sectores,
#   Criterio_Extrusion = Shape_Censo_Sectores$DENSIDAD_SECT,
#   Paleta_Color = scale_fill_gradient(low="bisque3", high="purple4"),
#   Resolution = Resolucion_3D,
#   Scale = Escala,
#   Output_Path = "./Data/3_Results/2_Population/2_Den/",
#   Output_Name_ES = "ES_Mapa_3D_Densidad_Sectores",
#   Output_Name_EN = "EN_Map_3D_Density_Sectores"
# )
# PlotMapa3D(
#   Shape_Plotear = Shape_Censo_Secciones,
#   Criterio_Extrusion = Shape_Censo_Secciones$DENSIDAD_SECC,
#   Paleta_Color = scale_fill_gradient(low="bisque3", high="purple4"),
#   Resolution = Resolucion_3D,
#   Scale = Escala,
#   Output_Path = "./Data/3_Results/2_Population/2_Den/",
#   Output_Name_ES = "ES_Mapa_3D_Densidad_Secciones",
#   Output_Name_EN = "EN_Map_3D_Density_Secciones"
# )
# PlotMapa3D(
#   Shape_Plotear = Shape_Censo_Hexagonos,
#   Criterio_Extrusion = Shape_Censo_Hexagonos$DENSIDAD_HEX,
#   Paleta_Color = scale_fill_gradient(low="bisque3", high="purple4"),
#   Resolution = Resolucion_3D,
#   Scale = Escala,
#   Output_Path = "./Data/3_Results/2_Population/2_Den/",
#   Output_Name_ES = "ES_Mapa_3D_Densidad_Hexagonos",
#   Output_Name_EN = "EN_Map_3D_Density_Hexagonos"
# )
# PlotMapa3D(
#   Shape_Plotear = Shape_Censo_Manzanas,
#   Criterio_Extrusion = Shape_Censo_Manzanas$DENSIDAD_MNZ,
#   Paleta_Color = scale_fill_gradient(low="bisque3", high="purple4"),
#   Resolution = Resolucion_3D,
#   Scale = Escala,
#   Output_Path = "./Data/3_Results/2_Population/2_Den/",
#   Output_Name_ES = "ES_Mapa_3D_Densidad_Manzanas",
#   Output_Name_EN = "EN_Map_3D_Density_Manzanas"
# )

# --- Gráficas de barras ---

# ####
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ::: Creamos mapas de capas de LOC y UPL con etiquetas para sobreponer      :::
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ####
#
# ####
# ...::: Capas de etiquetas :::...
# ####

# Seleccionamos las UPL no rurales
Lista_No_Rural <- Shape_UPLs |> filter(ZONA_UPL != "Rural") |> as_tibble() |> select(COD_UPL)
Lista_No_Rural

# --- Capa de etiquetas para LOC ---

# Creamos una capa de etiquetas para poner encima de los mapas para las LOC.
Capa_Etiquetas_LOC <- Shape_Localidades |>
  mutate(
    # Extrae las coordenadas del centroide de cada área
    centroides = st_centroid(geom),
    X = st_coordinates(centroides)[,1],
    Y = st_coordinates(centroides)[,2],
    # Define la columna que se usará como etiqueta
    Etiqueta_Texto = COD_LOC
  ) |>
  select(COD_LOC, NOM_LOC, X, Y, Etiqueta_Texto)
Capa_Etiquetas_LOC

# Probamos que haya quedado bien.
Plot_Capa_Etiquetas_LOC <- Capa_Etiquetas_LOC |>
  ggplot() +
  geom_sf(
    data = Capa_Etiquetas_LOC,
    col = "lightgray",
    linewidth = 0.1, #0.8
    fill = "white",
    #aes(fill = "blue"),
    alpha = 0.01
  ) + 
  geom_text(
    data = Capa_Etiquetas_LOC,
    aes(x = X, y = Y, label = Etiqueta_Texto), # Mapea coordenadas y texto
    color = "white",        # Color del texto
    size = 6,               # Tamaño del texto
    family = "Roboto Black" # Tipo de fuente
  ) +
  theme_void() +
  theme(legend.position = "none")
Plot_Capa_Etiquetas_LOC

ggsave(
  filename = paste("Capa_Etiquetas_LOC.png", sep = ""),
  plot = Plot_Capa_Etiquetas_LOC,
  device = NULL,
  path = "./Data/3_Results/2_Population/",
  scale = 1,
  width = 1080,
  height = 1920,
  units = c("px"),
  dpi = 160,
  limitsize = TRUE,
  bg = NULL,
  create.dir = FALSE,
)

# --- Capa de etiquetas para LOC ---

# Creamos una capa de etiquetas para poner encima de los mapas para las UPL.
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
Plot_Capa_Etiquetas_UPL <- Capa_Etiquetas_UPL |>
  ggplot() +
  geom_sf(
    data = Capa_Etiquetas_UPL,
    col = "lightgray",
    linewidth = 0.1, #0.8
    fill = "white",
    #aes(fill = "blue"),
    alpha = 0.01
  ) + 
  geom_text(
    data = Capa_Etiquetas_UPL,
    aes(x = X, y = Y, label = Etiqueta_Texto), # Mapea coordenadas y texto
    color = "white",        # Color del texto
    size = 6,               # Tamaño del texto
    family = "Roboto Black" # Tipo de fuente
  ) +
  theme_void() +
  theme(legend.position = "none")
Plot_Capa_Etiquetas_UPL

ggsave(
  filename = paste("Capa_Etiquetas_UPL.png", sep = ""),
  plot = Plot_Capa_Etiquetas_UPL,
  device = NULL,
  path = "./Data/3_Results/2_Population/",
  scale = 1,
  width = 1080,
  height = 1920,
  units = c("px"),
  dpi = 160,
  limitsize = TRUE,
  bg = NULL,
  create.dir = FALSE,
)

# ####
# ...::: Graficas de barras para Población y Densidad :::...
# ####

# --- Grafica para la población por localidad ---

Shape_Censo_Localidades
# Gráfica de barras para LOC
# Reordenar  otra vez el factor para que las barras vayan de menor a mayor
Shape_Censo_Localidades_Order <- Shape_Censo_Localidades |> select(COD_LOC, NOM_LOC, PERSONAS_LOC, DENSIDAD_LOC) |> st_drop_geometry() |> as_tibble() |> arrange(desc(PERSONAS_LOC))
Shape_Censo_Localidades_Order$COD_LOC <- factor(Shape_Censo_Localidades_Order$COD_LOC, levels = Shape_Censo_Localidades_Order$COD_LOC[order(Shape_Censo_Localidades_Order$PERSONAS_LOC)])
Shape_Censo_Localidades_Order

Plot_Barras_Poblacion_LOC <- Shape_Censo_Localidades_Order |>
  ggplot() +
  # Barras horizontales
  geom_col(aes(x = COD_LOC, y = PERSONAS_LOC + 550000, fill = PERSONAS_LOC), width = 0.9) +
  # Nombre del área dentro de la barra, al inicio
  geom_text(aes(x = COD_LOC, y = 0, label = paste("Localidad ", COD_LOC, ": ", NOM_LOC, sep = "")),
            hjust = 0,             # muy cerca del borde izquierdo
            nudge_y = max(Shape_Censo_Localidades_Order$PERSONAS_LOC) * 0.02,
            color = "white", size = 4, family = "Roboto", fontface = "bold") +
  # Valor de población dentro de la barra, al final
  geom_text(aes(x = COD_LOC, y = PERSONAS_LOC + 550000, label = paste(PERSONAS_LOC, " hab", sep = "")),
            hjust = 1.2,             # muy cerca del borde derecho (1.2 es el mejor)
            color = "white", size = 4, family = "Roboto") +
  # Voltear ejes para barras horizontales
  coord_flip() +
  # Asignar colores:
  scale_fill_gradient(
    low = "bisque3",
    high =  "darkred"
  ) +
  # Eliminar ejes, cuadrículas y leyenda
  theme_void() +
  theme(legend.position = "none") +
  # Añadir espacio a la derecha para que no se corte el texto
  scale_y_continuous(limits = c(0, max(Shape_Censo_Localidades_Order$PERSONAS_LOC) * 1.5), expand = c(0,0)) # Original 1.1
Plot_Barras_Poblacion_LOC

# Guardamos como imagen el gráfico de barras:
ggsave(
  filename = paste("Grafica_Poblacion_por_LOC.png", sep = ""),
  plot = Plot_Barras_Poblacion_LOC,
  device = NULL,
  path = "./Data/3_Results/2_Population/1_Pop/",
  scale = 1,
  width = 1500,
  height = 1000,
  units = c("px"),
  dpi = 160,
  limitsize = TRUE,
  bg = NULL,
  create.dir = FALSE,
)

# --- Grafica para la población por UPL ---

Shape_Censo_UPLs
# Gráfica de barras para UPL
# Reordenar  otra vez el factor para que las barras vayan de menor a mayor
Shape_Censo_UPL_Order <- Shape_Censo_UPLs |> select(COD_UPL, NOM_UPL, PERSONAS_UPL, DENSIDAD_UPL) |> st_drop_geometry() |> as_tibble() |> arrange(desc(PERSONAS_UPL))
Shape_Censo_UPL_Order$COD_UPL <- factor(Shape_Censo_UPL_Order$COD_UPL, levels = Shape_Censo_UPL_Order$COD_UPL[order(Shape_Censo_UPL_Order$PERSONAS_UPL)])
Shape_Censo_UPL_Order

Plot_Barras_Poblacion_UPL <- Shape_Censo_UPL_Order |>
  ggplot() +
  # Barras horizontales
  geom_col(aes(x = COD_UPL, y = PERSONAS_UPL + 200000, fill = PERSONAS_UPL), width = 0.9) +
  # Nombre del área dentro de la barra, al inicio
  geom_text(aes(x = COD_UPL, y = 0, label = paste("UPL ", COD_UPL, ": ", NOM_UPL, sep = "")),
            hjust = 0,             # muy cerca del borde izquierdo
            nudge_y = max(Shape_Censo_UPL_Order$PERSONAS_UPL) * 0.02,
            color = "white", size = 4, family = "Roboto", fontface = "bold") +
  # Valor de población dentro de la barra, al final
  geom_text(aes(x = COD_UPL, y = PERSONAS_UPL + 200000, label = paste(PERSONAS_UPL, " hab", sep = "")),
            hjust = 1.2,             # muy cerca del borde derecho (1.2 es el mejor)
            color = "white", size = 4, family = "Roboto") +
  # Voltear ejes para barras horizontales
  coord_flip() +
  # Asignar colores:
  scale_fill_gradient(
    low = "bisque3",
    high =  "darkred"
  ) +
  # Eliminar ejes, cuadrículas y leyenda
  theme_void() +
  theme(legend.position = "none") +
  # Añadir espacio a la derecha para que no se corte el texto
  scale_y_continuous(limits = c(0, max(Shape_Censo_UPL_Order$PERSONAS_UPL) * 1.6), expand = c(0,0)) # Original 1.1
Plot_Barras_Poblacion_UPL

# Guardamos como imagen el gráfico de barras:
ggsave(
  filename = paste("Grafica_Poblacion_por_UPL.png", sep = ""),
  plot = Plot_Barras_Poblacion_UPL,
  device = NULL,
  path = "./Data/3_Results/2_Population/1_Pop/",
  scale = 1,
  width = 1500,
  height = 1500,
  units = c("px"),
  dpi = 150,
  limitsize = TRUE,
  bg = NULL,
  create.dir = FALSE,
)

# --- Grafica para la densidad por LOC ---

Shape_Censo_LOC
# Gráfica de barras para LOC
# Reordenar  otra vez el factor para que las barras vayan de menor a mayor
Shape_Censo_LOC_Order <- Shape_Censo_Localidades |> select(COD_LOC, NOM_LOC, PERSONAS_LOC, DENSIDAD_LOC) |> st_drop_geometry() |> as_tibble() |> arrange(desc(DENSIDAD_LOC))
Shape_Censo_LOC_Order$COD_LOC <- factor(Shape_Censo_LOC_Order$COD_LOC, levels = Shape_Censo_LOC_Order$COD_LOC[order(Shape_Censo_LOC_Order$DENSIDAD_LOC)])
Shape_Censo_LOC_Order

Plot_Barras_Densidad_LOC <- Shape_Censo_LOC_Order |>
  ggplot() +
  # Barras horizontales
  geom_col(aes(x = COD_LOC, y = DENSIDAD_LOC + 4000, fill = DENSIDAD_LOC), width = 0.9) +
  # Nombre del área dentro de la barra, al inicio
  geom_text(aes(x = COD_LOC, y = 0, label = paste("Localidad ", COD_LOC, ": ", NOM_LOC, sep = "")),
            hjust = 0,             # muy cerca del borde izquierdo
            nudge_y = max(Shape_Censo_LOC_Order$DENSIDAD_LOC) * 0.02,
            color = "white", size = 4, family = "Roboto", fontface = "bold") +
  # Valor de población dentro de la barra, al final
  geom_text(aes(x = COD_LOC, y = DENSIDAD_LOC + 4000, label = paste(round(DENSIDAD_LOC), " hab/km2", sep = "")),
            hjust = 1.1,             # muy cerca del borde derecho (1.2 es el mejor)
            color = "white", size = 4, family = "Roboto") +
  # Voltear ejes para barras horizontales
  coord_flip() +
  # Asignar colores:
  scale_fill_gradient(
    low = "bisque3",
    high =  "purple4"
  ) +
  # Eliminar ejes, cuadrículas y leyenda
  theme_void() +
  theme(legend.position = "none") +
  # Añadir espacio a la derecha para que no se corte el texto
  scale_y_continuous(limits = c(0, max(Shape_Censo_LOC_Order$DENSIDAD_LOC) * 1.2), expand = c(0,0)) # Original 1.1
Plot_Barras_Densidad_LOC

# Guardamos como imagen el gráfico de barras:
ggsave(
  filename = paste("Grafica_Densidad_por_LOC.png", sep = ""),
  plot = Plot_Barras_Densidad_LOC,
  device = NULL,
  path = "./Data/3_Results/2_Population/2_Den/",
  scale = 1,
  width = 1500,
  height = 1000,
  units = c("px"),
  dpi = 150,
  limitsize = TRUE,
  bg = NULL,
  create.dir = FALSE,
)

# --- Grafica para la densidad por UPL ---

Shape_Censo_UPLs
# Gráfica de barras para UPL
# Reordenar  otra vez el factor para que las barras vayan de menor a mayor
Shape_Censo_UPL_Order <- Shape_Censo_UPLs |> select(COD_UPL, NOM_UPL, PERSONAS_UPL, DENSIDAD_UPL) |> st_drop_geometry() |> as_tibble() |> arrange(desc(DENSIDAD_UPL))
Shape_Censo_UPL_Order$COD_UPL <- factor(Shape_Censo_UPL_Order$COD_UPL, levels = Shape_Censo_UPL_Order$COD_UPL[order(Shape_Censo_UPL_Order$DENSIDAD_UPL)])
Shape_Censo_UPL_Order

Plot_Barras_Densidad_UPL <- Shape_Censo_UPL_Order |>
  ggplot() +
  # Barras horizontales
  geom_col(aes(x = COD_UPL, y = DENSIDAD_UPL + 20000, fill = DENSIDAD_UPL), width = 0.9) +
  # Nombre del área dentro de la barra, al inicio
  geom_text(aes(x = COD_UPL, y = 0, label = paste("UPL ", COD_UPL, ": ", NOM_UPL, sep = "")),
            hjust = 0,             # muy cerca del borde izquierdo
            nudge_y = max(Shape_Censo_UPL_Order$DENSIDAD_UPL) * 0.02,
            color = "white", size = 4, family = "Roboto", fontface = "bold") +
  # Valor de población dentro de la barra, al final
  geom_text(aes(x = COD_UPL, y = DENSIDAD_UPL + 20000, label = paste(round(DENSIDAD_UPL), " hab/km2", sep = "")),
            hjust = 1.1,             # muy cerca del borde derecho (1.2 es el mejor)
            color = "white", size = 4, family = "Roboto") +
  # Voltear ejes para barras horizontales
  coord_flip() +
  # Asignar colores:
  scale_fill_gradient(
    low = "bisque3",
    high =  "purple4"
  ) +
  # Eliminar ejes, cuadrículas y leyenda
  theme_void() +
  theme(legend.position = "none") +
  # Añadir espacio a la derecha para que no se corte el texto
  scale_y_continuous(limits = c(0, max(Shape_Censo_UPL_Order$DENSIDAD_UPL) * 1.5), expand = c(0,0)) # Original 1.1
Plot_Barras_Densidad_UPL

# Guardamos como imagen el gráfico de barras:
ggsave(
  filename = paste("Grafica_Densidad_por_UPL.png", sep = ""),
  plot = Plot_Barras_Densidad_UPL,
  device = NULL,
  path = "./Data/3_Results/2_Population/2_Den/",
  scale = 1,
  width = 1500,
  height = 1500,
  units = c("px"),
  dpi = 150,
  limitsize = TRUE,
  bg = NULL,
  create.dir = FALSE,
)

# ...::: Gráficos de pirámides para trabajadores y estudiantes :::...

# --- Estudiantes ---

# Reordenar  otra vez el factor para que las barras vayan de menor a mayor
Shape_Censo_UPLs
Porcentajes_Oficios_UPL_df <- Shape_Censo_UPLs |>
  mutate(
    AREA_UPL = as.numeric(set_units(st_area(geom), km^2)),
    PORCENTAJE_TRABAJADORES_UPL = round(TRABAJADORES_UPL/PERSONAS_UPL, digits = 4) * 100,
    DENSIDAD_TRABAJADORES_UPL = round(TRABAJADORES_UPL/AREA_UPL),
    PORCENTAJE_ESTUDIANTES_UPL = round(ESTUDIANTES_UPL/PERSONAS_UPL, digits = 4) * 100,
    DENSIDAD_ESTUDIANTES_UPL = round(ESTUDIANTES_UPL/AREA_UPL),
  ) |>
  st_drop_geometry() |>
  as_tibble() |>
  arrange(as.numeric(COD_UPL))
Porcentajes_Oficios_UPL_df$COD_UPL <- factor(Porcentajes_Oficios_UPL_df$COD_UPL, levels = Porcentajes_Oficios_UPL_df$COD_UPL[order(as.numeric(Porcentajes_Oficios_UPL_df$COD_UPL))])
Porcentajes_Oficios_UPL_df

min(Porcentajes_Oficios_UPL_df$DENSIDAD_TRABAJADORES_UPL)

# Grafica para estudiantes (Lado Izquierdo)

Plot_Barras_Estudiantes_UPL <- Porcentajes_Oficios_UPL_df |>
  ggplot() +
  # Barras horizontales
  geom_col(aes(x = fct_rev(COD_UPL), y = -ESTUDIANTES_UPL + -18000, fill = DENSIDAD_ESTUDIANTES_UPL), width = 0.9) +
  # Nombre del área dentro de la barra, al inicio
  # geom_text(aes(x = COD_UPL, y = 0, label = paste("UPL ", COD_UPL, ": ", NOM_UPL, sep = "")),
  #           hjust = 0,             # muy cerca del borde izquierdo
  #           nudge_y = max(Porcentajes_Oficios_UPL_df$ESTUDIANTES_UPL) * 0.02,
  #           color = "white", size = 4, family = "Roboto", fontface = "bold") +
  # Valor de población dentro de la barra, al final
  geom_text(aes(x = COD_UPL, y = -ESTUDIANTES_UPL + 0, label = paste(ESTUDIANTES_UPL, " (", PORCENTAJE_ESTUDIANTES_UPL, "%)", sep = "")),
            hjust = 1.0,             # muy cerca del borde derecho (1.2 es el mejor)
            nudge_y = -max(Porcentajes_Oficios_UPL_df$ESTUDIANTES_UPL) * 0.02,
            color = "white", size = 4, family = "Roboto") +
  # Voltear ejes para barras horizontales
  coord_flip() +
  # Asignar colores:
  scale_fill_gradient(
    low = "bisque3",
    high =  "deepskyblue4"
  ) +
  # Eliminar ejes, cuadrículas y leyenda
  theme_void() +
  theme(legend.position = "none") +
  # Añadir espacio a la derecha para que no se corte el texto
  scale_y_continuous(limits = c(-max(Porcentajes_Oficios_UPL_df$ESTUDIANTES_UPL) * 1.65, 0), expand = c(0,0)) # Original 1.1
Plot_Barras_Estudiantes_UPL

# Guardamos como imagen el gráfico de barras:
ggsave(
  filename = paste("Grafica_Estudiantes_por_UPL.png", sep = ""),
  plot = Plot_Barras_Estudiantes_UPL,
  device = NULL,
  path = "./Data/3_Results/2_Population/2_Den",
  scale = 1,
  width = 1000,
  height = 1500,
  units = c("px"),
  dpi = 150,
  limitsize = TRUE,
  bg = NULL,
  create.dir = FALSE,
)

# --- Trabajadores ---

# Grafica para trabajadores (Lado derecho)

Plot_Barras_Trabajadores_UPL <- Porcentajes_Oficios_UPL_df |>
  ggplot() +
  # Barras horizontales
  geom_col(aes(x = fct_rev(COD_UPL), y = TRABAJADORES_UPL + 50000, fill = DENSIDAD_TRABAJADORES_UPL), width = 0.9) +
  # Nombre del área dentro de la barra, al inicio
  # geom_text(aes(x = COD_UPL, y = 0, label = paste("UPL ", COD_UPL, ": ", NOM_UPL, sep = "")),
  #           hjust = 0,             # muy cerca del borde izquierdo
  #           nudge_y = max(Porcentajes_Oficios_UPL_df$TRABAJADORES_UPL) * 0.02,
  #           color = "white", size = 4, family = "Roboto", fontface = "bold") +
  # Valor de población dentro de la barra, al final
  geom_text(aes(x = COD_UPL, y = TRABAJADORES_UPL + 50000, label = paste(TRABAJADORES_UPL, " (", PORCENTAJE_TRABAJADORES_UPL, "%)", sep = "")),
            hjust = 1.1,             # muy cerca del borde derecho (1.2 es el mejor)
            color = "white", size = 4, family = "Roboto") +
  # Voltear ejes para barras horizontales
  coord_flip() +
  # Asignar colores:
  scale_fill_gradient(
    low = "bisque3",
    high =  "darkred"
  ) +
  # Eliminar ejes, cuadrículas y leyenda
  theme_void() +
  theme(legend.position = "none") +
  # Añadir espacio a la derecha para que no se corte el texto
  scale_y_continuous(limits = c(0, max(Porcentajes_Oficios_UPL_df$TRABAJADORES_UPL) * 1.3), expand = c(0,0)) # Original 1.1
Plot_Barras_Trabajadores_UPL

# Guardamos como imagen el gráfico de barras:
ggsave(
  filename = paste("Grafica_Trabajadores_por_UPL.png", sep = ""),
  plot = Plot_Barras_Trabajadores_UPL,
  device = NULL,
  path = "./Data/3_Results/2_Population/2_Den",
  scale = 1,
  width = 1000,
  height = 1500,
  units = c("px"),
  dpi = 150,
  limitsize = TRUE,
  bg = NULL,
  create.dir = FALSE,
)




# Inglés:
# Aquí abrimos, ponemos títulos y guardamos la copia en inglés

# Tiempo de procesamiento:
End_Time_S2_P6 <- Sys.time()
print(paste("6. Tiempo de ploteo de mapas de población: ", as.duration(End_Time_S2_P6 - Start_Time_S2_P6)))

# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ::: 7. Guardado de Shapes con info de población                            :::
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#
# El objetivo es guardar el avance de los nuevos shapes
# no tener que volver a filtrar toooodo lo anterior porque es muy lento.
#
Start_Time_S2_P7 <- Sys.time()

# Guardo los Shapes de Población:
st_write(Shape_Censo_Municipios, dsn = "./Data/2_Processing/2_Population/Shape_Poblacion_Municipios.gpkg", driver = 'GPKG', append = FALSE)
st_write(Shape_Censo_Localidades, dsn = "./Data/2_Processing/2_Population/Shape_Poblacion_Localidades.gpkg", driver = 'GPKG', append = FALSE)
st_write(Shape_Censo_UPLs, dsn = "./Data/2_Processing/2_Population/Shape_Poblacion_UPLs.gpkg", driver = 'GPKG', append = FALSE)
st_write(Shape_Censo_UTAM, dsn = "./Data/2_Processing/2_Population/Shape_Poblacion_UTAM.gpkg", driver = 'GPKG', append = FALSE)
st_write(Shape_Censo_ZAT, dsn = "./Data/2_Processing/2_Population/Shape_Poblacion_ZAT.gpkg", driver = 'GPKG', append = FALSE)
st_write(Shape_Censo_Sectores, dsn = "./Data/2_Processing/2_Population/Shape_Poblacion_Sectores.gpkg", driver = 'GPKG', append = FALSE)
st_write(Shape_Censo_Secciones, dsn = "./Data/2_Processing/2_Population/Shape_Poblacion_Secciones.gpkg", driver = 'GPKG', append = FALSE)
st_write(Shape_Censo_Hexagonos, dsn = "./Data/2_Processing/2_Population/Shape_Poblacion_Hexagonos.gpkg", driver = 'GPKG', append = FALSE)
st_write(Shape_Censo_Manzanas, dsn = "./Data/2_Processing/2_Population/Shape_Poblacion_Manzanas.gpkg", driver = 'GPKG', append = FALSE)

# Tiempo de procesamiento:
End_Time_S2_P7 <- Sys.time()
print(paste("7. Tiempo de guardado de Shapes de Población: ", as.duration(End_Time_S2_P7 - Start_Time_S2_P7)))

# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ::: 8. ¿Necesita cargar los resultados de este Script?                     :::
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#
# El objetivo de esta sección, es recargar los resultados de este script, de manera que
# no sea necesario correrlo de nuevo si se llegan a necesitar sus resultados:

Start_Time_S2_P8 <- Sys.time()

# Recarga de CSV (Nuevo método):
Censo_Municipios <- as.data.frame(fread("./Data/2_Processing/2_Population/Censo_Municipios.csv", sep=",", na = "null", colClasses=c("COD_CPOB"="character")))
Censo_Localidades <- as.data.frame(fread("./Data/2_Processing/2_Population/Censo_Localidades.csv", sep=",", na = "null", colClasses=c("COD_LOC"="character")))
Censo_UPLs <- as.data.frame(fread("./Data/2_Processing/2_Population/Censo_UPLs.csv", sep=",", na = "null", colClasses=c("COD_UPL"="character")))
Censo_UTAM <- as.data.frame(fread("./Data/2_Processing/2_Population/Censo_UTAM.csv", sep=",", na = "null", colClasses=c("COD_UTAM"="character")))
Censo_ZAT <- as.data.frame(fread("./Data/2_Processing/2_Population/Censo_ZAT.csv", sep=",", na = "null", colClasses=c("COD_ZAT"="character")))
Censo_Sectores <- as.data.frame(fread("./Data/2_Processing/2_Population/Censo_Sectores.csv", sep=",", na = "null", colClasses=c("COD_SECT"="character")))
Censo_Secciones <- as.data.frame(fread("./Data/2_Processing/2_Population/Censo_Secciones.csv", sep=",", na = "null", colClasses=c("COD_SECC"="character")))
Censo_Hexagonos <- as.data.frame(fread("./Data/2_Processing/2_Population/Censo_Hexagonos.csv", sep=",", na = "null", colClasses=c("COD_HEX"="character")))
Censo_Manzanas <- as.data.frame(fread("./Data/2_Processing/2_Population/Censo_Manzanas.csv", sep=",", na = "null", colClasses=c("COD_LOC"="character", "COD_UPL"="character", "COD_UTAM"="character", "COD_ZAT"="character", "COD_SECT"="character", "COD_SECC"="character", "COD_HEX"="character", "COD_MNZ"="character")))
Censo_Viviendas <- as.data.frame(fread("./Data/2_Processing/2_Population/Censo_Viviendas.csv", sep=",", na = "null", colClasses=c("COD_LOC"="character", "COD_UPL"="character", "COD_UTAM"="character", "COD_ZAT"="character", "COD_SECT"="character", "COD_SECC"="character", "COD_HEX"="character", "COD_MNZ"="character", "COD_VIVIENDA"="character")))
Censo_Familias <- as.data.frame(fread("./Data/2_Processing/2_Population/Censo_Familias.csv", sep=",", na = "null", colClasses=c("COD_LOC"="character", "COD_UPL"="character", "COD_UTAM"="character", "COD_ZAT"="character", "COD_SECT"="character", "COD_SECC"="character", "COD_HEX"="character", "COD_MNZ"="character", "COD_FAMILIA"="character")))
Censo_Personas <- as.data.frame(fread("./Data/2_Processing/2_Population/Censo_Personas.csv", sep=",", na = "null", colClasses=c("COD_LOC"="character", "COD_UPL"="character", "COD_UTAM"="character", "COD_ZAT"="character", "COD_SECT"="character", "COD_SECC"="character", "COD_HEX"="character", "COD_MNZ"="character", "COD_FAMILIA"="character", "COD_PERSONA"="character")))

# Recarga de Shapes:
Shape_Censo_Municipios <- st_read("./Data/2_Processing/2_Population/Shape_Poblacion_Municipios.gpkg")
Shape_Censo_Localidades <- st_read("./Data/2_Processing/2_Population/Shape_Poblacion_Localidades.gpkg")
Shape_Censo_UPLs <- st_read("./Data/2_Processing/2_Population/Shape_Poblacion_UPLs.gpkg")
Shape_Censo_UTAM <- st_read("./Data/2_Processing/2_Population/Shape_Poblacion_UTAM.gpkg")
Shape_Censo_ZAT <- st_read("./Data/2_Processing/2_Population/Shape_Poblacion_ZAT.gpkg")
Shape_Censo_Sectores <- st_read("./Data/2_Processing/2_Population/Shape_Poblacion_Sectores.gpkg")
Shape_Censo_Secciones <- st_read("./Data/2_Processing/2_Population/Shape_Poblacion_Secciones.gpkg")
Shape_Censo_Hexagonos <- st_read("./Data/2_Processing/2_Population/Shape_Poblacion_Hexagonos.gpkg")
Shape_Censo_Manzanas <- st_read("./Data/2_Processing/2_Population/Shape_Poblacion_Manzanas.gpkg")

# Tiempo de procesamiento:
End_Time_S2_P8 <- Sys.time()
print(paste("8. Tiempo de recarga de resultados: ", as.duration(End_Time_S2_P8 - Start_Time_S2_P8)))

# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ::: 9. Tiempos de procesamiento                                            :::
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#
# Mostramos los tiempos de procesamiento:
#
End_Time_S2_P0 <- Sys.time()
print(paste("1. Tiempo de inicialización: ", as.duration(End_Time_S2_P1 - Start_Time_S2_P1)))
print(paste("2. Tiempo de carga de datos de origen: ", as.duration(End_Time_S2_P2 - Start_Time_S2_P2)))
print(paste("3. Tiempo de filtrado y tratamiento de datos del censo: ", as.duration(End_Time_S2_P3 - Start_Time_S2_P3)))
print(paste("4. Tiempo de guardado de los nuevos CSV del Censo: ", as.duration(End_Time_S2_P4 - Start_Time_S2_P4)))
print(paste("5. Tiempo de combinación de Shapes y Población: ", as.duration(End_Time_S2_P5 - Start_Time_S2_P5)))
print(paste("6. Tiempo de ploteo de mapas de población: ", as.duration(End_Time_S2_P6 - Start_Time_S2_P6)))
print(paste("7. Tiempo de guardado de Shapes de Población: ", as.duration(End_Time_S2_P7 - Start_Time_S2_P7)))
print(paste("8. Tiempo de recarga de resultados: ", as.duration(End_Time_S2_P8 - Start_Time_S2_P8)))
print(paste("Tiempo total de procesamiento: ", as.duration(End_Time_S2_P0 - Start_Time_S2_P0)))

################## TEST ####################
