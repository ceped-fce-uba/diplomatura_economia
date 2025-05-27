library(tidyverse)
options(scipen = 999)

cadena_litio <- read_csv("~/repos/diplomatura_economia/Mod 3 - Econ Internacional/elaboracion_materiales/clase2/M3 - Practica 2/cadena_litio.csv")

datos_mundiales <- cadena_litio %>%
  filter(country == "WORLD") %>%
  select(producto, value) %>%
  rename(valor_mundial = value)

datos_totales <- cadena_litio %>% 
  filter(producto == "expo_totales") %>%
  select(country, valor_total = value)

datos_paises <- cadena_litio %>%
  filter(country != "WORLD", producto != "expo_totales")

total_mundial <- datos_mundiales %>% 
  filter(producto == "expo_totales") %>% 
  pull(valor_mundial)

datos <- datos_paises %>% 
  left_join(datos_totales, by = "country") %>% 
  left_join(datos_mundiales, by = "producto") %>% 
  mutate(expo_totales_mundiales = total_mundial)

vcrs <- datos %>% 
  mutate(vcr = (value / valor_total) / (valor_mundial / expo_totales_mundiales))

# Vi en algunos casos que calcularon algo del estilo de:
# groupby() %>% mutate(exportaciones_totales_pais = sum(value, na.rm = TRUE))
# Lo que pasa es que la base ya traía las exportaciones totales por año
# (no podemos sumar todos los valores porque solo están las exportaciones
# de la cadena del litio en la base y además se suma al total de expo)


ivcrs <- vcrs %>%
  group_by(country) %>%
  mutate(ivcr = (vcr - 1)/(vcr + 1)) %>% 
  select(producto, country, ivcr) %>% 
  ungroup()

ivcrs_tabla <- ivcrs %>%
  pivot_wider(
    id_cols = country,
    names_from = producto,
    values_from = ivcr
  )

write_xlsx(ivcrs_tabla, "ventaja_comparativa_por_pais.xlsx")
