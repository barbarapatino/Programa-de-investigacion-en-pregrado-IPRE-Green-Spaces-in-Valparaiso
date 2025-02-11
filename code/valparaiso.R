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

# Calcular estadísticas: mínimo, máximo y promedio por fila con redondeo a 2 decimales, por si se necesitan a futuro
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

## Índice de vulnerabilidad
# Cargar base de datos SOVI
df_sovi <- read_csv("code/SOVI/sovi_datasets.csv", locale = locale(encoding = "UTF-8"))

# Visualizar los primeros datos de SOVI
print(head(df_sovi, 10))

# Renombramos la columna 'cod_com' en df_sovi a 'COD_COMUNA' para merge posterior
df_sovi <- df_sovi %>% rename(COD_COMUNA = cod_com)

# Hacemos merge de los DataFrane de NDVI con los de SOVI
df_summer_valparaiso_sovi <- df_sovi %>% inner_join(df_summer_valparaiso, by = "COD_COMUNA")
df_winter_valparaiso_sovi <- df_sovi %>% inner_join(df_winter_valparaiso, by = "COD_COMUNA")

# la columna name_comuna es redundante, ya tenemos NOM_COM, por lo que la eliminamos
df_summer_valparaiso_sovi <- df_summer_valparaiso_sovi %>% select(-name_comuna)
df_winter_valparaiso_sovi <- df_winter_valparaiso_sovi %>% select(-name_comuna)

# Cargar base de datos de nacimientos
df_nacimientos <- read_csv("code/births_valparaiso_1992_2020/births_valparaiso.csv", locale = locale(encoding = "UTF-8"))

# Convertir 'year_nac' a numérico y filtrar datos desde 2002
df_nacimientos <- df_nacimientos %>% mutate(year_nac = as.numeric(year_nac)) %>% filter(year_nac >= 2002)

# Visualizar DataFrame de nacimientos, vemos la primeras 10 filas
print(head(df_nacimientos, 10))

# Renombramos la columna 'name_com' en df_nacimientos a 'NOM_COM' para merge futuro
df_nacimientos <- df_nacimientos %>% rename(NOM_COM = name_com)

# Filtramos para dejar los nacimientos a término (birth_term == 1)
df_nacimientos_termino <- df_nacimientos %>% filter(birth_term == 1)

# Nos aseguramos que las columnas con fecha esten en formato Date (las convertimos a este)
df_nacimientos_termino <- df_nacimientos_termino %>% mutate(date_nac = as_date(date_nac),date_week1 = as_date(date_week1))

# Calcular la fecha de inicio del tercer trimestre (sumando 27 semanas a la primera semana de gestación)
df_nacimientos_termino <- df_nacimientos_termino %>% mutate(semana_tercer_trimestre = date_week1 + weeks(27))

# Visualizar DataFrame de nacimientosa a término, vemos la primeras 10 filas
print(head(df_nacimientos_termino, 10))

