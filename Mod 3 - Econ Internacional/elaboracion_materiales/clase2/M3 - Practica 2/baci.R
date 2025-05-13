# BACI
# 
# Version: 202501
# 
# Release Date: 2025 01 30
# 
# Weblink: http://www.cepii.fr/CEPII/en/bdd_modele/bdd_modele_item.asp?id=37
# 
# Content:
#   Trade flows at the year - exporter - importer - product level.
# Products in Harmonized System 6-digit nomenclature.
# Values in thousand USD and quantities in metric tons.
# 
# List of Variables:
#   t: year
# i: exporter
# j: importer
# k: product
# v: value
# q: quantity
# 
# Reference:
#   Gaulier, G. and Zignago, S. (2010)
# BACI: International Trade Database at the Product-Level. The 1994-2007 Version.
# CEPII Working Paper, N°2010-23

library(tidyverse)

# DE AHORA EN MÁS USO HS92

codigos_pais <- read_csv("country_codes_V202501.csv")
head(codigos_pais)

# repasito de R base:
# dataframe[fila,columna]
# dataframe[dataframe$columna, NADA]
codigos_pais[codigos_pais$country_name == "Germany",]
codigos_pais[codigos_pais$country_name == "Brazil",]

codigos_producto <- read_csv("product_codes_HS92_V202501.csv")
head(codigos_producto)

codigos_producto[codigos_producto$code == 283691,]

base_2022 <- read_csv("BACI_HS92_Y2022_V202501.csv") 
base_2023 <- read_csv("BACI_HS92_Y2023_V202501.csv")

base_2022_y_2023 <- bind_rows(base_2022, base_2023)

# o, equivalentemente

base_2022_y_2023 <- base_2022 %>% 
  bind_rows(base_2023)

# si nos queremos quedar unicamente con un solo país, por ejemplo, Brasil:

brasil_2022_y_2023 <- base_2022_y_2023 %>% 
  filter(i == 76 | j == 76) # como importador o como exportador

codigos_pais <- codigos_pais %>% 
  select(country_code, country_iso3)

brasil_2022_y_2023 <- brasil_2022_y_2023 %>%
  left_join(codigos_pais, by = c("i" = "country_code")) %>%
  rename(i_iso3 = country_iso3) %>%

  left_join(codigos_pais, by = c("j" = "country_code")) %>%
  rename(j_iso3 = country_iso3) %>% 
  
  select(t, i_iso3, j_iso3, k, v, q)

base_2021 <- base_2022_y_2023 %>% 
  filter(t == 2021)

