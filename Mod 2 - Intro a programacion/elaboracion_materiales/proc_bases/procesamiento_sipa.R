library(tidyverse)
library(readxl)
library(lubridate)

# Carga de excels ####

# La base viene con las fechas formateadas horribles, con inconsistencias en el almacenado
# de los datos. Lo solucioné directo en el excel de origen.

# A.5.1.Personas con empleo asalariado registrado en el sector privado,
# según provincia. Con estacionalidad. En miles.

data1 <- read_excel("Mod 2 - Intro a programacion/elaboracion_materiales/proc_bases/sipa_seleccion.xlsx",
                 sheet = "A.5.1",
                 range = "A2:Y93"
                 )

# T.2.1. Personas con trabajo registrado según modalidad ocupacional principal.
# Con estacionalidad. Total país. En miles.

data2 <- read_excel(
  "Mod 2 - Intro a programacion/elaboracion_materiales/proc_bases/sipa_seleccion.xlsx",
  sheet = "T.2.1",
  range = "A2:H157"
) %>%
  mutate(
    Periodo = ymd(Periodo) # Parsea fechas en formato "Año-Mes-Día"
  )


# T.3.1. Personas con trabajo registrado según modalidad ocupacional principal. 
# Con estacionalidad. Total país. Índice base 100 = Ene-12

data3 <- read_excel("Mod 2 - Intro a programacion/elaboracion_materiales/proc_bases/sipa_seleccion.xlsx",
                   sheet = "T.3.1",
                   range = "A2:H157"
) %>%
  mutate(
    Periodo = ymd(Periodo) # Parsea fechas en formato "Año-Mes-Día"
  )



#A.4. Remuneración de las personas con empleo asalariado registrado en el sector privado.
# Por todo concepto (**),  en pesos, a valores corrientes. Total país.
# ** Es la remuneración bruta (previa a las deducciones por cargas sociales) declarada por la empresa  para cada mes. Incluye adicionales de periodicidad no mensual, horas extraordinarias, viáticos, sueldo anual complementario y bonificación por vacaciones. No incluye indemnizaciones.				

data4 <- read_excel("Mod 2 - Intro a programacion/elaboracion_materiales/proc_bases/sipa_seleccion.xlsx",
                   sheet = "A.4",
                   range = "A2:E193"
) %>%
  mutate(
    Periodo = ymd(Periodo) # Parsea fechas en formato "Año-Mes-Día"
  )

# Pivot Longer ####

data1_long <- data1 %>%
  pivot_longer(
    cols = -Periodo,
    names_to = "Geografia", 
    values_to = "Valor"
  ) %>% 
  mutate(Variable = "Asalariados registrados en el sector privado")

data2_long <- data2 %>%
  pivot_longer(
    cols = -Periodo,
    names_to = "Variable", 
    values_to = "Valor" 
  ) %>% 
  mutate(Variable = gsub("\\r\\n", "", Variable), # porque los encabesados traen saltos de línea
         Geografia = "Total Pais")

data3_long <- data3 %>%
  pivot_longer(
    cols = -Periodo,
    names_to = "Variable",
    values_to = "Valor" 
  ) %>% 
  mutate(Variable = gsub("\\r\\n", "", Variable), # porque los encabesados traen saltos de línea
         Geografia = "Total Pais")

data4_long <- data4 %>%
  pivot_longer(
    cols = -Periodo, 
    names_to = "Variable",
    values_to = "Valor" 
  ) %>% 
  mutate(Variable = gsub("\\r\\n", "", Variable), # porque los encabesados traen saltos de línea
         Geografia = "Total Pais")

# Ahora uso bind_rows() de dplyr que es más flexible que rbind() de R Base.

base_sipa <- bind_rows(data1_long, data2_long, data3_long, data4_long) %>%
  select(Periodo, Variable, Geografia, Valor)

# Guardado ####

write_csv(base_sipa, "Mod 2 - Intro a programacion/elaboracion_materiales/proc_bases/base_sipa.csv")

# Si hubiera que guardarlas por separado
# write_csv(data1_long, "Mod 2 - Intro a programacion/bases/asalariados_registrados_provincia.csv")
# write_csv(data2_long, "Mod 2 - Intro a programacion/bases/trabajadores_registrados_categoria_ocupacional.csv")
# write_csv(data3_long, "Mod 2 - Intro a programacion/bases/trabajadores_registrados_categoria_ocupacional_ind.csv")
# write_csv(data4_long, "Mod 2 - Intro a programacion/bases/remuneracion_asalariados_registrados_privados.csv")
