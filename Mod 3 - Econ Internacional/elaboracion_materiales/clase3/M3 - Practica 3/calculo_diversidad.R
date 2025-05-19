# Calcular diversidad y ubicuidad desde BACI
rm(list = ls())

#Librerias
library(tidyverse)
library(openxlsx)

# Limpiamos números científicos
options(scipen=999)

# Seteamos ruta a la carpeta donde ubican su base de BACI
ruta_bases <- "C:/BASES/BACI/"

# Levantamos la bases en hs17
comercio_mundial <- read_csv(paste0(ruta_bases,"BACI_HS92_Y2023_V202501.csv"))

# DICCIONARIO
# t -> time (año)
# i -> país exportador
# j -> país receptor
# k -> producto (a 6 dígitos)
# v -> value
# q -> cantidad

# Sacamos cantidad y tiempo
comercio_mundial <- comercio_mundial %>%
  select(i, j, k, v)

# Agrupamos según destinatario
comercio_mundial <- comercio_mundial %>%
  group_by(i, k) %>%
  summarise(v = sum(v))

# Levantamos el diccionario de países
paises_baci <- read_csv(paste0(ruta_bases, "/country_codes_V202501.csv"))

# Pegamos código de país
comercio_mundial <- comercio_mundial %>%
  left_join(paises_baci %>% select(country_code, country_name),
            by = c("i" = "country_code")) %>% 
  ungroup()

# Limpiamos i
comercio_mundial <- comercio_mundial %>%
  select(-i)

# Renombramos
comercio_mundial <- comercio_mundial %>%
  rename(
    pais = country_name,
    producto = k,
    valor_miles_usd = v)

# Creamos nueva columna con el total por país
comercio_mundial <- comercio_mundial %>%
  group_by(pais) %>%
  mutate(expo_pais = sum(valor_miles_usd)) %>%
  ungroup()

# Creamos nueva columna con el total por producto
comercio_mundial <- comercio_mundial %>%
  group_by(producto) %>%
  mutate(expo_producto = sum(valor_miles_usd)) %>%
  ungroup()

# Creamos nueva columna con el total de exportaciones
comercio_mundial <- comercio_mundial %>%
  mutate(expo_totales = sum(valor_miles_usd)) %>%
  ungroup()

# Creamos nueva columna replicando la formula de VCR
comercio_mundial <- comercio_mundial %>%
  mutate(vcr = ((valor_miles_usd/expo_pais)/(expo_producto/expo_totales))) %>%
  ungroup()

# Creamos una variable dicotómica que recuente si las VCR existen por país y producto
comercio_mundial <- comercio_mundial %>%
  mutate(hay_vcr = ifelse(vcr>1,1,0)) %>%
  ungroup()

# Recontamos cantidad de productos con VCR por país para obtener diversidad
diversidad <- comercio_mundial %>%
  group_by(pais) %>%
  summarize(diversidad = sum(hay_vcr))

# Nos quedamos los 10 de arriba
top10_diversidad <- diversidad %>% 
  slice_max(order_by = diversidad, n = 10)

# Nos quedamos los 10 de abajo
nottop10_diversidad <- diversidad %>% 
  slice_min(order_by = diversidad, n = 10)

# Para guardar en excel:
# Crear el archivo
wb <- createWorkbook()

# Agregar la pestaña del top 10
addWorksheet(wb, "Top 10 diversidad")
writeData(wb, sheet = "Top 10 diversidad", top10_diversidad)

# Agregar la pestaña del not top 10
addWorksheet(wb, "Not top 10 diversidad")
writeData(wb, sheet = "Not top 10 diversidad", nottop10_diversidad)

# Save workbook
saveWorkbook(wb, "diversidad_paises.xlsx", overwrite = TRUE)