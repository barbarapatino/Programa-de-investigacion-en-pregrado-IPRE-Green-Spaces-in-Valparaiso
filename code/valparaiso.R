# Cargar librerías
library(dplyr)
library(ggplot2)
library(readr)
library(stringi)
library(sf)
library(stats)

# Cargar datos corregidos de NDVI
df_summer <- read_csv("code/green_spaces_data/summerNDVI_corregido.csv")
df_winter <- read_csv("code/green_spaces_data/winterNDVI_corregido.csv")

# Filtrar por Región de Valparaíso
df_summer_valparaiso <- df_summer %>% filter(NOM_REG == "Valparaíso")
df_winter_valparaiso <- df_winter %>% filter(NOM_REG == "Valparaíso")


## Para facilitar la visualizacion y uso ordenamos las columnas por orden cronologico (0_NDVI, 1_NDVI, 2_NDVI...)
# Identificar columnas NDVI
columnas_ndvi <- grep("_NDVI", names(df_summer), value = TRUE)

# Función para extraer el número de la columna NDVI
extraer_numero <- function(col) {
  as.integer(strsplit(col, "_")[[1]][1])}

# Ordenar las columnas NDVI
ordered_ndvi_columns <- columnas_ndvi[order(sapply(columnas_ndvi, extraer_numero))]

# Reordenar las columnas en los dataframes
df_summer_valparaiso <- df_summer_valparaiso %>% select(all_of(ordered_ndvi_columns), everything())
df_winter_valparaiso <- df_winter_valparaiso %>% select(all_of(ordered_ndvi_columns), everything())

# Cambiar etiquetas de columnas NDVI a años (reemplaza 0_NDVI con 2002_NDVI)
nueva_etiqueta_ndvi <- setNames(ordered_ndvi_columns, paste0(2002:2022, "_NDVI"))
df_summer_valparaiso <- df_summer_valparaiso %>% rename(!!!nueva_etiqueta_ndvi)
df_winter_valparaiso <- df_winter_valparaiso %>% rename(!!!nueva_etiqueta_ndvi)

# Verificar nombres de columnas antes de aplicar cálculos
print(names(df_summer_valparaiso))

# Obtener nombres de columnas NDVI renombradas
ndvi_cols <- names(df_summer_valparaiso)[names(df_summer_valparaiso) %in% paste0(2002:2022, "_NDVI")]

# Calcular estadísticas: mínimo, máximo y promedio por fila con redondeo a 2 decimales, manteniendo formato numérico
df_summer_valparaiso <- df_summer_valparaiso %>% 
  mutate(
    MIN = round(apply(select(., all_of(ndvi_cols)), 1, min, na.rm = TRUE), 2),
    MAX = round(apply(select(., all_of(ndvi_cols)), 1, max, na.rm = TRUE), 2),
    MEAN = round(rowMeans(select(., all_of(ndvi_cols)), na.rm = TRUE), 2)
  )

df_winter_valparaiso <- df_winter_valparaiso %>% 
  mutate(
    MIN = round(apply(select(., all_of(ndvi_cols)), 1, min, na.rm = TRUE), 2),
    MAX = round(apply(select(., all_of(ndvi_cols)), 1, max, na.rm = TRUE), 2),
    MEAN = round(rowMeans(select(., all_of(ndvi_cols)), na.rm = TRUE), 2)
  )

# Visualización del DataFrame con mejor formato
print(df_summer_valparaiso %>% select(NOM_COM, MIN, MAX, MEAN) %>% head(10))
print(df_winter_valparaiso %>% select(NOM_COM, MIN, MAX, MEAN) %>% head(10))

# Mostrar estructura del dataframe para verificar tipos de datos
glimpse(df_summer_valparaiso)

