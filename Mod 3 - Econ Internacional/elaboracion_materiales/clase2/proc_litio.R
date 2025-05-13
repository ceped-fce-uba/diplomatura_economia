library(tidyverse)
library(readxl)

options(scipen = 999)
cadena_litio <- read_excel("Mod 3 - Econ Internacional/elaboracion_materiales/clase2/VCR_cadena_litio.xlsx", 
                               sheet = "Sheet1")

cadena_litio_long <- cadena_litio %>%
  # Rename the first column to something more descriptive
  rename(producto = `2021`) %>%
  # Convert all country columns to rows
  pivot_longer(
    cols = c(WORLD, CHL, CHN, KOR, DEU, USA, JPN, POL, ARG),
    names_to = "country",
    values_to = "value"
  )

write_csv(cadena_litio_long, "Mod 3 - Econ Internacional/elaboracion_materiales/clase2/bases/cadena_litio.csv")
