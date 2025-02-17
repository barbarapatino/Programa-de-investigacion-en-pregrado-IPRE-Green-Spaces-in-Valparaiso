# Rscript code/valparaiso.R
# Cargar librerías
library(dplyr)
library(readr)
library(stringi)
library(purrr)
library(lubridate)
library(stringr)


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
# print(names(df_summer_valparaiso))

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
# print(df_summer_valparaiso %>% select(NOM_COM, MIN, MAX, MEAN) %>% head(10))
# print(df_winter_valparaiso %>% select(NOM_COM, MIN, MAX, MEAN) %>% head(10))

## Índice de vulnerabilidad
# Cargar base de datos SOVI
df_sovi <- read_csv("code/SOVI/sovi_datasets.csv", locale = locale(encoding = "UTF-8"))

# Visualizar los primeros datos de SOVI
# print(head(df_sovi, 10))

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

# Renombramos la columna 'name_com' en df_nacimientos a 'NOM_COM' para merge futuro
df_nacimientos <- df_nacimientos %>% rename(NOM_COM = name_com)

# Filtramos para dejar los nacimientos a término (birth_term == 1)
df_nacimientos_termino <- df_nacimientos %>% filter(birth_term == 1)

# Nos aseguramos que las columnas con fecha esten en formato Date (las convertimos a este)
df_nacimientos_termino <- df_nacimientos_termino %>% mutate(date_nac = as.Date(date_nac), date_week1 = as.Date(date_week1))

# Calcular la fecha de inicio del tercer trimestre (sumando 27 semanas a la primera semana de gestación)
df_nacimientos_termino <- df_nacimientos_termino %>% mutate(semana_tercer_trimestre = date_week1 + weeks(27))

# Nos aseguramos la columna tenga el formato Date (las convertimos a este)
df_nacimientos_termino <- df_nacimientos_termino %>% mutate(semana_tercer_trimestre = as.Date(semana_tercer_trimestre),date_ends_week_gest = as.Date(date_ends_week_gest))


# Asignar NDVI segun invierno o verano
# Asignamos la estación en la que cae el tercer trimestre
# Verano: 21/12 del año anterior hasta 21/03 del año actual.
# Invierno: 21/06 al 21/09 del mismo año.

## Inicialmente asignamos la estación si el inicio del tercer trimestre cae en 
# el mes de inicio de la estación y la ultima semana de gestación en el último mes de la estación
# Creamos la función que asigna la estación
# Función para asignar estación con base en el trimestre de gestación
asignar_estacion_trimestre <- function(inicio, fin) {
  # Verificar valores nulos
  if (is.na(inicio) | is.na(fin)) {
    return("Fuera de estación")
  }
  # Obtener el mes de cada fecha
  inicio_mes <- month(inicio)
  fin_mes <- month(fin)
  # Verano: Si el inicio es en diciembre o enero y el fin en febrero o marzo
  if (inicio_mes %in% c(12, 1) & fin_mes %in% c(2, 3)) {
    return("Verano")
  }
  # Invierno: Si el inicio es en junio o julio y el fin en agosto o septiembre
  if (inicio_mes %in% c(6, 7) & fin_mes %in% c(8, 9)) {
    return("Invierno")
  }

  return("Fuera de estación")  # Si no calza completamente en ninguna estación
}

# Aplicamos la función a los nacimientos a término
df_nacimientos_termino <- df_nacimientos_termino %>% 
  mutate(estacion_trimestre = mapply(asignar_estacion_trimestre, semana_tercer_trimestre, date_ends_week_gest))

# Esto muestra cuantos hay de cada estación
# table(df_nacimientos_termino$estacion_trimestre, useNA = "always")

# Filtramos y dejamos las filas que son de Verano o Invierno
df_nacimientos_termino <- df_nacimientos_termino %>% filter(str_starts(estacion_trimestre, "Verano") | str_starts(estacion_trimestre, "Invierno"))

# Los nombres de las comunas vienen distintos
df_summer_valparaiso_sovi <- df_summer_valparaiso_sovi %>% mutate(NOM_COM = stri_trans_general(NOM_COM, "Latin-ASCII"))
df_winter_valparaiso_sovi <- df_winter_valparaiso_sovi %>% mutate(NOM_COM = stri_trans_general(NOM_COM, "Latin-ASCII"))

# En los DataFrame eliminamos espacios en blanco de "NOM_COM" antes del merge para asegirarnos de que sean compatibles
df_summer_valparaiso_sovi <- df_summer_valparaiso_sovi %>% mutate(NOM_COM = str_trim(NOM_COM))
df_winter_valparaiso_sovi <- df_winter_valparaiso_sovi %>% mutate(NOM_COM = str_trim(NOM_COM))
df_nacimientos_termino <- df_nacimientos_termino %>% mutate(NOM_COM = str_trim(NOM_COM))

# Merge con df_summer_valparaiso_sovi
df_nacimientos_termino_summer <- df_nacimientos_termino %>% left_join(df_summer_valparaiso_sovi %>% select(NOM_COM, COD_COMUNA, sovi, vulnerablidad), by = "NOM_COM")

# Merge con df_winter_valparaiso_sovi
df_nacimientos_termino_winter <- df_nacimientos_termino %>% left_join(df_winter_valparaiso_sovi %>% select(NOM_COM, COD_COMUNA, sovi, vulnerablidad), by = "NOM_COM")

# Ahora queremos asignar los valores de NDVI
# Crear un "diccionario" donde la clave es NOM_COM (nombre de la comuna)
# y el valor es un data frame con los valores NDVI por año.
# Esto nos permitirá luego asignar el NDVI correspondiente a cada nacimiento según su comuna y año.

ndvi_dict_summer <- df_summer_valparaiso_sovi %>% 
  select(NOM_COM, contains("_NDVI")) %>%  # Selecciona la columna NOM_COM y las columnas NDVI
  split(.$NOM_COM)  # Divide el DataFrame en una lista donde cada comuna es una clave

ndvi_dict_winter <- df_winter_valparaiso_sovi %>% 
  select(NOM_COM, contains("_NDVI")) %>% 
  split(.$NOM_COM)  # Lo mismo, pero para invierno

# Función para asignar NDVI basado en comuna y año
asignar_ndvi <- function(comuna, year, ndvi_dict) {
  # Verificar si la comuna existe en el diccionario de NDVI
  if (!comuna %in% names(ndvi_dict)) return(NA) # Si la comuna no está en el diccionario, devuelve NA
  # Construir el nombre de la columna que contiene el NDVI para el año dado (Ej: "2002_NDVI")
  col_name <- paste0(year, "_NDVI")
  # Verificar si la columna de NDVI para ese año existe en la comuna
  if (!(col_name %in% names(ndvi_dict[[comuna]]))) return(NA)
  # Si la comuna y el año existen se retorna el valor de NDVI correspondiente
  return(ndvi_dict[[comuna]][[col_name]])
}

# Asignar el valor de NDVI correspondiente a cada nacimiento según su comuna y año.
# A df_nacimientos_termino_summer
df_nacimientos_termino_summer <- df_nacimientos_termino_summer %>% 
  mutate(
    #  Usa la función asignar_ndvi para obtener el NDVI basado en NOM_COM (comuna) y year_nac (año de nacimiento)
    NDVI_summer = map2_dbl(NOM_COM, year_nac, ~ asignar_ndvi(.x, .y, ndvi_dict_summer))
  ) %>% # Filtra los registros y elimina los que no tienen un NDVI asignado
  filter(!is.na(NDVI_summer))

# A df_nacimientos_termino_winter
df_nacimientos_termino_winter <- df_nacimientos_termino_winter %>% 
  mutate(
    NDVI_winter = map2_dbl(NOM_COM, year_nac, ~ asignar_ndvi(.x, .y, ndvi_dict_winter))
  ) %>%
  filter(!is.na(NDVI_winter))

# Convertir "tbw" a numérico para ambos DataFrame
df_nacimientos_termino_summer <- df_nacimientos_termino_summer %>% 
  mutate(tbw = as.numeric(tbw))
df_nacimientos_termino_winter <- df_nacimientos_termino_winter %>% 
  mutate(tbw = as.numeric(tbw))

# Renombrar la columna NDVI para que sea compatible
df_nacimientos_termino_summer <- df_nacimientos_termino_summer %>% rename(NDVI = NDVI_summer)
df_nacimientos_termino_winter <- df_nacimientos_termino_winter %>% rename(NDVI = NDVI_winter)

# Concatenar ambos DataFrames
df_nacimientos_termino_total <- bind_rows(df_nacimientos_termino_summer, df_nacimientos_termino_winter)

# Verificar estructura después de la unión
print(head(df_nacimientos_termino_total, 10), width = Inf)
