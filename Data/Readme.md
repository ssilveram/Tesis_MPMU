## ./Data/

# Datos del Proyecto

Este directorio (`./Data/`) almacena los insumos y resultados del proyecto, estructurados según el flujo de trabajo en R:

| Carpeta | Descripción | Contenido Típico |
| :--- | :--- | :--- |
| **`1_Sources`** | Datos fuente del Proyecto | Encuestas (DANE, Movilidad), Shapes, CSVs originales. |
| **`2_Processing`** | Datos intermedios | Archivos `.gpkg`, `.csv`, tablas limpias, uniones espaciales. |
| **`3_Results`** | Salidas finales | Mapas (`.png`), Gráficas (`.png`) y Tablas resumen. |

---

### 📥 Instrucciones para la Reproducibilidad

Debido al tamaño de los archivos (limitaciones de GitHub), esta carpeta no contiene archivos alojados en este repositorio. Los archivos fuente, han sido alojados en una carpeta externa en Google Drive.

**Para ejecutar el código correctamente:**
1. Descargue los archivos pesados desde el siguiente repositorio en la nube:
   👉 **[Clic aquí para acceder a Google Drive](https://drive.google.com/drive/folders/1fvW1JFfZM1tjaLQHEzeKaqh4McILtFRQ?usp=sharing)**

3. Descomprima/copie los archivos descargados dentro de las carpetas correspondientes (`1_Sources`, `2_Processing`, o `3_Results`) en su copia local de este repositorio.

4. Asegúrese de mantener los nombres de archivo originales para que los scripts de R los reconozcan.





