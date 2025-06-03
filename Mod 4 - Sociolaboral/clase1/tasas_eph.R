# Tasas básicas del mercado laboral en EPH
rm(list = ls())

# Librerias
library(tidyverse)
library(openxlsx)

# Instalar la librería de EPH
#install.packages('eph')

# Levantar librería EPH
library(eph)

# Limpiamos números científicos
options(scipen=999)

# Levantamos la bases del último trimestre disponible
base_eph <- get_microdata(
  year = 2024,
  period = 4,
  type = "individual"
)

# Seleccionamos sólo algunas columnas, y renombramos parte de ellas
eph_seleccion <- base_eph %>%
  select(CODUSU, NRO_HOGAR, COMPONENTE, REGION, PONDERA, CH04, 
         CH06, NIVEL_ED, ESTADO, CAT_OCUP, P21) %>%
  rename(
    genero = CH04,
    edad = CH06,
    ingresos = P21)

# Transformamos variables codificadas a su resultado legible
eph_seleccion <- eph_seleccion %>% mutate(
  genero = factor(genero, levels = c(1, 2), labels = c("Varón", "Mujer")),
  region = factor(REGION, levels = c(1, 40, 41, 42, 43, 44),
                  labels = c("Gran Buenos Aires", "NOA", "NEA", "Cuyo", "Pampeana", "Patagónica")),
  estado = factor(ESTADO, levels = 0:4, labels = c(
    "Entrevista no realizada", "Ocupado", "Desocupado", "Inactivo", "Menor de 10 años"
  )),
  cat_ocup = factor(CAT_OCUP, levels = c(0, 1, 2, 3, 4, 9),
                    labels = c("Inactivos", "Patrón", "Cuenta propia", "Obrero o empleado",
                               "Familiar sin remuneración", "Ns/Nr")),
  nivel_ed = factor(NIVEL_ED, levels = c(1:7, 9),
                    labels = c("Primaria incompleta", "Primaria completa", "Secundaria incompleta",
                               "Secundaria completa", "Universitario incompleto", "Universitario completo",
                               "Sin instrucción", "Ns/Nr")))

# Eliminamos las que quedaron viejas
eph_seleccion <- eph_seleccion %>% select(!c(REGION, ESTADO, CAT_OCUP, NIVEL_ED))

# Tenemos toda la base. Bien formateada. Para calcular las tasas, marquemos PEA, ocupados y desocupados:
eph_seleccion <- eph_seleccion %>% mutate(
  ocupado = ifelse(estado == "Ocupado", 1, 0),
  desocupado = ifelse(estado == "Desocupado", 1, 0),
  activo = ifelse(estado == "Ocupado" | estado == "Desocupado", 1, 0))

# Creamos las tasas
tasas_eph <- eph_seleccion %>%
  summarise(
    tasa_actividad = mean(activo),
    tasa_empleo = mean(ocupado),
    tasa_desocupacion = mean(desocupado) / mean(activo))

# ¿Qué pasa? ¿No coinciden con las del informe?

# Hay que usar los ponderadores. Hay que hacer promedios ponderados
tasas_eph <- eph_seleccion %>%
  summarise(
    tasa_actividad = weighted.mean(activo, PONDERA),
    tasa_empleo = weighted.mean(ocupado, PONDERA),
    tasa_desocupacion = (weighted.mean(desocupado, PONDERA) / weighted.mean(activo, PONDERA)))

# Podemos dejarlo en términos porcentuales, a un decimal
tasas_eph <- tasas_eph %>%
  mutate(
    tasa_actividad = round(tasa_actividad * 100, 1),
    tasa_empleo = round(tasa_empleo * 100, 1),
    tasa_desocupacion = round(tasa_desocupacion * 100, 1))

# Podemos ir más allá de lo que muestra el informe: calcular tasas por género
tasas_por_genero <- eph_seleccion %>%
  group_by(genero) %>%
  summarise(
    tasa_actividad = round(weighted.mean(activo, PONDERA)*100, 1),
    tasa_empleo = round(weighted.mean(ocupado, PONDERA)*100, 1),
    tasa_desocupacion = round((weighted.mean(desocupado, PONDERA) / weighted.mean(activo, PONDERA))*100, 1))

# O por región!
tasas_por_region <- eph_seleccion %>%
  group_by(region) %>%
  summarise(
    tasa_actividad = round(weighted.mean(activo, PONDERA)*100, 1),
    tasa_empleo = round(weighted.mean(ocupado, PONDERA)*100, 1),
    tasa_desocupacion = round((weighted.mean(desocupado, PONDERA) / weighted.mean(activo, PONDERA))*100, 1))

# Por edad tambien puede ser. Pero tenemos que categorizar la variable, que está como continua
eph_seleccion <- eph_seleccion %>%
  mutate(rango_edad = case_when(
    edad <= 25 ~ "25 o menos",
    edad >= 26 & edad < 40 ~ "26 a 39",
    edad >= 40 & edad < 60 ~ "40 a 59",
    edad >= 60 ~ "60 o más"))

# Ahora sí, tasas por edad
tasas_por_edad <- eph_seleccion %>%
  group_by(rango_edad) %>%
  summarise(
    tasa_actividad = round(weighted.mean(activo, PONDERA)*100, 1),
    tasa_empleo = round(weighted.mean(ocupado, PONDERA)*100, 1),
    tasa_desocupacion = round((weighted.mean(desocupado, PONDERA) / weighted.mean(activo, PONDERA))*100, 1))

# Podemos unificar con la función bind_rows
tasas_unificadas <- bind_rows(
  tasas_eph,
  tasas_por_genero,
  tasas_por_region,
  tasas_por_edad)

# Para guardar en excel:
# Crear el archivo
wb <- createWorkbook()

# Agregar la pestaña de tasas generales
addWorksheet(wb, "Tasas generales")
writeData(wb, sheet = "Tasas generales", tasas_eph)

# Agregar la pestaña de tasas por género
addWorksheet(wb, "Tasas por género")
writeData(wb, sheet = "Tasas por género", tasas_por_genero)

# Agregar la pestaña de tasas por edad
addWorksheet(wb, "Tasas por edad")
writeData(wb, sheet = "Tasas por edad", tasas_por_edad)

# Agregar la pestaña de tasas por región
addWorksheet(wb, "Tasas por región")
writeData(wb, sheet = "Tasas por región", tasas_por_region)

# Save workbook
saveWorkbook(wb, "tasas_básicas_mercado_laboral.xlsx", overwrite = TRUE)