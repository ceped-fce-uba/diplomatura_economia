library(tidyverse)

# Schteingart, D., Sonzogni, P. y Pascuariello, G. (2024). Estructura productiva. Argendata. Fundar.
base <- read_csv("Mod 2 - Intro a programacion/elaboracion_materiales/proc_bases/empleo_sectores.csv")

head(base)

base$iso3 %>% unique()

base <- base %>% 
  filter(iso3 %in% c("ARG","BOL","CHL","COL","MEX","PER"),
         anio >= 1960 )

diccionario_variables <- tibble(
  variable = c(
    "iso3", 
    "anio", 
    "gran_sector", 
    "sector_codigo",
    "sector_desc",
    "empleo_miles",
    "share_empleo"
  ),
  
  etiqueta = c(
    "Código identificador de país (ISO 3166-1 alfa-3)",
    "Año al que corresponde el dato",
    "Gran sector: Bienes o Servicios",
    "Código del sector productivo",
    "Nombre del sector productivo",
    "Empleo en el sector, en miles",
    "Proporción del empleo total del país en dicho año que representa el sector (entre 0 y 1)"
  )
)

write_csv(base, "Mod 2 - Intro a programacion/Actividad Integradora/bases/empleo_sectores_base.csv")
write_csv(diccionario_variables, "Mod 2 - Intro a programacion/Actividad Integradora/bases/diccionario_empleo_sectores_vars.csv")

