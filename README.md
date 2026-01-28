
# Transformando la Movilidad Urbana de Bogotá: Implementación e Integración del Sistema de Transporte Masivo Férreo y la Ciudad de Proximidad - Escala de Unidad de Planeación Local - 🚈🏙️

![Badge de R](https://img.shields.io/badge/Language-R-blue)
![Badge de Estado](https://img.shields.io/badge/Status-Completed-green)
![Badge de Licencia](https://img.shields.io/badge/License-MIT-yellow)

> **Repositorio oficial de código para la Tesis de Maestría:**
> *"Transformando la Movilidad Urbana de Bogotá: Implementación e Integración del Sistema de Transporte Masivo Férreo y la Ciudad de Proximidad - Escala de Unidad de Planeación Local -"*

<p align="center">
  <img src="./Data/3_Results/8_Red_Ferrea/Mapa_Trazado_Portada.png" alt="Mapa de Accesibilidad y Trazado de Red Férrea" style="max-width: 50%; height: auto;">
</p>

## 📄 Descripción
Este repositorio contiene el flujo de trabajo computacional completo para modelar, simular y evaluar el impacto en la accesibilidad y el Índice de Alta Calidad de Vida Social (derivado del concepto de la "Ciudad de los 15 minutos"), como resultado de la implementación de 6 escenarios futuros respecto al estado actual de la ciudad (Escenario base):

| Escenario | Descripción |
| :--- | :--- |
| **Escenario 1** | Estado actual (Base) |
| **Escenario 2** | Escenario 1 + PLMB |
| **Escenario 3** | Escenario 2 + Implantación de equipamientos en inmediaciones de las estaciones de la PLMB |
| **Escenario 4** | Escenario 2 + Implantación plena de equipamientos a nivel de UPL |
| **Escenario 5** | Escenario 1 + Red Férrea |
| **Escenario 6** | Escenario 5 + Implantación de equipamientos en inmediaciones de las estaciones de la Red Férrea |
| **Escenario 7** | Escenario 5 + Implantación plena de equipamientos a nivel de UPL |

El proyecto utiliza **R** y el motor de enrutamiento **R5 (r5r)** para calcular matrices de tiempos de viaje multimodales y generar el índice **HQSL (High Quality Social Life)**.

## 📂 Estructura del Repositorio

El código está organizado secuencialmente para garantizar la reproducibilidad de los resultados:

| Script | Descripción |
| :--- | :--- |
| **`0_Initialization`** | Limpieza y estandarización de datos espaciales (Manzanas, UPLs, Red Vial). |
| **`1_Empty_Shapes`** | Limpieza y estandarización de datos espaciales (Manzanas, UPLs, Red Vial). |
| **`2_Population`** | Procesamiento de datos censales y proyecciones de densidad poblacional. |
| **`3_EM2023`** | Análisis de la Encuesta de Movilidad 2023 (Patrones de viaje y reparto modal). |
| **`4_x_Amenities`** | Inventario, clasificación y georreferenciación de equipamientos urbanos. |
| **`5_0_GTFS_Generator`** | **[Key Tool]** Algoritmo para generar archivos GTFS sintéticos de las líneas de metro y tren proyectadas. |
| **`6_x_Accessibility`** | Ejecución del motor `r5r` para el cálculo de matrices de tiempo (Línea Base y. Escenarios Futuros). |
| **`7_ScoreCard`** | Cálculo del Índice HQSL y generación ScoreCards de diagnóstico (Gráficos de Radar, mapas, etc.) por UPL. |
| **`8_Red_Ferrea`** | Generación de visualizaciones de la red férrea. |
| **`9_Misc`** | Generación de gráficos auxilares. |

## Estructura de carpetas archivos fuente, de procesamiento y resultaos

Este directorio (`./Data/`) almacena los insumos y resultados del proyecto, estructurados según el flujo de trabajo en R:

| Carpeta | Descripción | Contenido Típico |
| :--- | :--- | :--- |
| **`./Data/1_Sources/`** | Datos fuente del Proyecto | Encuestas (DANE, Movilidad), Shapes, CSVs originales. |
| **`./Data/2_Processing/`** | Datos intermedios | Archivos `.gpkg`, `.csv`, tablas limpias, uniones espaciales. |
| **`./Data/3_Results/`** | Salidas finales | Mapas (`.png`), Gráficas (`.png`) y Tablas resumen. |

### 📥 Instrucciones para la Reproducibilidad

Debido al tamaño de los archivos (limitaciones de GitHub), esta carpeta no contiene archivos alojados en este repositorio. Los archivos fuente, han sido alojados en una carpeta externa en Google Drive.

**Para ejecutar el código correctamente:**
1. Descargue los archivos pesados desde el siguiente repositorio en la nube:
   👉 **[Clic aquí para acceder a Google Drive](https://drive.google.com/drive/folders/1fvW1JFfZM1tjaLQHEzeKaqh4McILtFRQ?usp=sharing)** (Es necesario solititar permisos para ver).

3. Descomprima/copie los archivos descargados dentro de las carpetas correspondientes (`1_Sources`, `2_Processing`, o `3_Results`) en su copia local de este repositorio.

4. Asegúrese de mantener los nombres de archivo originales para que los scripts de R los reconozcan.

## 🛠️ Requisitos e Instalación

**[R](https://cran.rstudio.com/)**: Versión 4.4.3.

**[RStudio](https://posit.co/download/rstudio-desktop/)**: Versión 2026.01.0.

Para ejecutar estos scripts, es necesario tener instalado **Java Development Kit (JDK 21)** instalado en su sistema para el funcionamiento de R5.

Las librerías necesarias para la ejecución del proyecto, se instalarán y cargarán automáticamente a través del **`Script 0_Initialization`**

## 📝 Citación
Si utiliza este código o metodología para su investigación, por favor cite:
Silvera, Santiago (2026). Transformando la Movilidad Urbana de Bogotá: Implementación e Integración del Sistema de Transporte Masivo Férreo y la Ciudad de Proximidad - Escala de Unidad de Planeación Local -. [Technische Universität Berlin - Universidad de Buenos Aires]. Repositorio GitHub: https://github.com/ssilveram/Tesis_MPMU

## 📜 Licencia
Este proyecto está bajo la Licencia MIT - vea el archivo LICENSE.md para más detalles.
Desarrollado como parte de la Maestría Dual en Planificación y Movilidad Urbana de la Technische Universität Berlin y la Universidad de Buenos Aires.

