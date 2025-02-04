# Cargar librerías
library(dplyr)
library(ggplot2)
library(readr)
library(stringi)
library(sf)
library(stats)

# Cargar datos corregidos de NDVI
df_summer <- read_csv("green_spaces_data/summerNDVI_corregido.csv")
df_winter <- read_csv("green_spaces_data/winterNDVI_corregido.csv")

# Filtrar por Región de Valparaíso
df_summer_valparaiso <- df_summer %>% filter(NOM_REG == "Valparaíso")
df_winter_valparaiso <- df_winter %>% filter(NOM_REG == "Valparaíso")