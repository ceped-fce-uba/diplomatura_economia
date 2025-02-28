library(tidyverse)
library(readxl)

# Carga de excels ####

# La base viene con las fechas formateadas horribles, con inconsistencias en el almacenado
# de los datos. Lo solucioné directo en el excel de origen.

# A.5.1.Personas con empleo asalariado registrado en el sector privado,
# según provincia. Con estacionalidad. En miles.

data1 <- read_excel("Mod 1 - Fundamentos/bases/sipa_seleccion.xlsx",
                 sheet = "A.5.1",
                 range = "A2:Y93"
                 )


# T.2.1. Personas con trabajo registrado según modalidad ocupacional principal.
# Con estacionalidad. Total país. En miles.

data2 <- read_excel("Mod 1 - Fundamentos/bases/sipa_seleccion.xlsx",
                   sheet = "T.2.1",
                   range = "A2:H157"
)


# T.3.1. Personas con trabajo registrado según modalidad ocupacional principal. 
# Con estacionalidad. Total país. Índice base 100 = Ene-12

data3 <- read_excel("Mod 1 - Fundamentos/bases/sipa_seleccion.xlsx",
                   sheet = "T.3.1",
                   range = "A2:H157"
)

#A.4. Remuneración de las personas con empleo asalariado registrado en el sector privado.
# Por todo concepto (**),  en pesos, a valores corrientes. Total país.
# ** Es la remuneración bruta (previa a las deducciones por cargas sociales) declarada por la empresa  para cada mes. Incluye adicionales de periodicidad no mensual, horas extraordinarias, viáticos, sueldo anual complementario y bonificación por vacaciones. No incluye indemnizaciones.				

data4 <- read_excel("Mod 1 - Fundamentos/bases/sipa_seleccion.xlsx",
                   sheet = "A.4",
                   range = "A2:E193"
)

# Pivot Longer ####

data1_long <- data1 %>%
  pivot_longer(
    cols = -Período, # Usamos Período como la columna ID
    names_to = "Provincia", # Nueva columna con las provincias
    values_to = "Valor" # Columna a la que va el valor
  )

data2_long <- data2 %>%
  pivot_longer(
    cols = -Período, # Usamos Período como la columna ID
    names_to = "Categoría Ocupacional", # Nueva columna con las provincias
    values_to = "Valor" # Columna a la que va el valor
  )

data3_long <- data3 %>%
  pivot_longer(
    cols = -Período, # Usamos Período como la columna ID
    names_to = "Categoría Ocupacional", # Nueva columna con las provincias
    values_to = "Valor" # Columna a la que va el valor
  )

# data4 no necesita ser pivoteada

# Guardado ####

write_csv(data1_long, "Mod 1 - Fundamentos/bases/asalariados_registrados_provincia.csv")
write_csv(data2_long, "Mod 1 - Fundamentos/bases/trabajadores_registrados_categoria_ocupacional.csv")
write_csv(data3_long, "Mod 1 - Fundamentos/bases/trabajadores_registrados_categoria_ocupacional_ind.csv")
write_csv(data4, "Mod 1 - Fundamentos/bases/remuneracion_asalariados_registrados_privados.csv")
