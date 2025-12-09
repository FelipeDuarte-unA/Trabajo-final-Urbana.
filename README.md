# Trabajo-final-Urbana.
README: Calidad Educativa y Localización Residencial en Bogotá

## Descripción del Proyecto

Este proyecto simula y analiza el efecto de la calidad educativa en las decisiones de localización residencial y los precios de la vivienda en Bogotá, utilizando un marco teórico de equilibrio espacial Rosen-Roback. El estudio combina métodos econométricos avanzados (DID, SFD, modelos de elección discreta) con datos simulados que replican características realistas de la estructura urbana de Bogotá.

## Contenido del Repositorio

### Scripts Principales:
- **`Rtrabajofinal.R`**: Script principal que ejecuta toda la simulación y análisis
  - Genera datos simulados de 120 barrios en 21 periodos
  - Realiza análisis descriptivos y gráficos
  - Estima modelos econométricos (DID, SFD, elección discreta)
  - Genera mapas y visualizaciones espaciales

### Archivos de Salida Generados:
- **`tabla_descriptivas_paper.tex`** / **`.html`**: Tabla de estadísticas descriptivas
- **`tabla_resultados_regresion.tex`** / **`.html`**: Resultados completos de regresión
- **`event_study_completo_iplot.png`**: Gráfico de event study
- **`mapa_tratamiento_delimitado_real.png`**: Mapa de distribución del tratamiento
- **`mapa_calidad_delimitado_real.png`**: Mapa de calidad educativa y precios
- **`mapa_efectos_bogota.png`**: Mapa de efectos post-tratamiento

## Requisitos del Sistema

### Paquetes R Requeridos:
```r
install.packages(c("tidyverse", "ggplot2", "sf", "spdep", "leaflet", "plm", 
                   "fixest", "kableExtra", "haven", "modelsummary", "ggthemes", 
                   "patchwork", "rnaturalearth", "rnaturalearthdata", "osmdata", 
                   "geodata", "htmlwidgets", "viridis", "survival"))
```

## Estructura de la Simulación

### 1. **Unidades y Periodos**
- 120 barrios simulados en Bogotá
- 21 periodos temporales (10 pre-tratamiento + tratamiento + 10 post-tratamiento)
- Asignación aleatoria de tratamiento a 30 barrios (25%)

### 2. **Variables Generadas**
- **Precio de vivienda**: Log-lineal con autocorrelación espacial
- **Calidad educativa**: SAR con ρ=0.35, correlacionada con estrato e ingreso
- **Covariables**: Ingreso, distancia al centro, estrato, tipo de colegio
- **Estructura de errores**: Heterocedástica con clustering espacial

### 3. **Modelos Econométricos**
- **DID Básico y con Controles**: Efecto promedio del tratamiento
- **Event Study**: Dinámica temporal del efecto
- **Spatial First Differences**: Control por heterogeneidad espacial
- **Modelo de Elección Discreta**: Preferencias heterogéneas de hogares

## Ejecución del Código

### Opción 1: Ejecución Completa
```r
# Cargar el script completo
source("Rtrabajofinal.R")
```

### Opción 2: Ejecución por Secciones
```r
# 1. Instalar paquetes
# (Ejecutar solo si es necesario)

# 2. Cargar librerías
# (Ya incluido en script)

# 3. Simulación de datos
# (Se ejecuta automáticamente)

# 4. Análisis descriptivo
# (Genera tablas y gráficos automáticamente)

# 5. Estimación de modelos
# (Resultados guardados en archivos)
```


*Nota: Este README corresponde al archivo de código R proporcionado y describe el proyecto de simulación completo.*
