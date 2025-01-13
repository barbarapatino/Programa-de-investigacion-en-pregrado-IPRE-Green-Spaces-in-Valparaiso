import pandas as pd
import ftfy

# Generar DataFrames
df_summer = pd.read_csv("green_spaces_data/summerNDVI_MOD_com_2002-2022_EBlanco.csv", encoding='Windows-1252')
df_winter = pd.read_csv("green_spaces_data/winterNDVI_MOD_com_2002-2022_EBlanco.csv", encoding='Windows-1252')

def corregir_caracteres(df):
    for col in df.select_dtypes(include=['object']).columns:
        df[col] = df[col].apply(ftfy.fix_text)  # Corregir texto con ftfy
    return df

# Aplicar la corrección a ambos DataFrames
df_summer = corregir_caracteres(df_summer)
df_winter = corregir_caracteres(df_winter)


# Identificar columnas NDVI en ambos DataFrames
columnas_ndvi = [col for col in df_summer.columns if '_NDVI' in col]

# Desescalar valores dividiendo por 10,000
df_summer[columnas_ndvi] = df_summer[columnas_ndvi] / 10000
df_winter[columnas_ndvi] = df_winter[columnas_ndvi] / 10000

df_summer.to_csv("green_spaces_data/summerNDVI_corregido.csv", index=False, encoding='utf-8')
df_winter.to_csv("green_spaces_data/winterNDVI_corregido.csv", index=False, encoding='utf-8')

print("green_spaces_data cargada")