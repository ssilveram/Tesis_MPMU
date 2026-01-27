## ./Data/

# Datos del Proyecto

Este directorio (`/Data`) almacena los insumos y resultados del proyecto, estructurados según el flujo de trabajo en R:

| Carpeta | Descripción | Contenido Típico |
| :--- | :--- | :--- |
| **`1_Source`** | Datos crudos de origen | Encuestas (DANE), Shapes, CSVs originales. |
| **`2_Processing`** | Datos intermedios | Archivos `.rds`, tablas limpias, uniones espaciales. |
| **`3_Results`** | Salidas finales | Mapas (`.png`, `.pdf`), Gráficas y Tablas resumen. |

---

### 📥 Instrucciones para la Reproducibilidad

Debido al tamaño de los archivos (limitaciones de GitHub), este repositorio solo contiene la estructura de carpetas y archivos ligeros.

**Para ejecutar el código correctamente:**
1. Descargue los archivos pesados desde el siguiente repositorio en la nube:
   👉 **[Clic aquí para acceder a Google Drive](URL_AQUI)**
2. Descomprima/copie los archivos descargados dentro de las carpetas correspondientes (`1_Source`, etc.) en su copia local de este repositorio.
3. Asegúrese de mantener los nombres de archivo originales para que los scripts de R los reconozcan.