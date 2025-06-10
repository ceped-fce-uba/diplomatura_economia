# Tasas básicas del mercado laboral en EPH
rm(list = ls())

# Limpiamos números científicos
options(scipen=999)

# Librerias
library(tidyverse)
library(openxlsx)
library(eph)
library(rstudioapi)

# Directorio automaticamente en la carpeta donde se está corriendo el script
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Levantamos la bases del último trimestre disponible
base_eph <- get_microdata(
  year = 2024,
  period = 4,
  type = "individual"
)

# Seleccionamos sólo algunas columnas, y renombramos parte de ellas
eph_seleccion <- base_eph %>%
  select(CODUSU, NRO_HOGAR, COMPONENTE, REGION, PONDERA, PONDIIO, ESTADO, EMPLEO, SECTOR,
         CAT_OCUP, CH04, CH06, NIVEL_ED, P21, PP3E_TOT, PP03I, PP07C, PP07H) %>%
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

# Hasta ahora se analizaba sólo la condición de registro de los asalariados
# Nos quedamos sólo con asalariados
eph_asalariados <- eph_seleccion %>% filter(estado == "Ocupado" & cat_ocup == "Obrero o empleado")

# Vamos a clasificarlos a través de la pregunta PP07H. Una rápida revisada a su contenido:
table(eph_asalariados$PP07H)

# Reemplazamos su contenido usando mutate y factor.
eph_asalariados <- eph_asalariados %>% mutate(
  registro = factor(PP07H, levels = c(1, 2), labels = c("Registrado", "No registrado"))) %>% 
  select(!c(PP07H))

# Obtenemos la tasa de no registro por región
asalariados_registro <- eph_asalariados %>%
  group_by(region, registro) %>%
  summarise(total = sum(PONDERA))

# Expandimos la base de datos para calcular participaciones
asalariados_registro <- asalariados_registro %>% 
  pivot_wider(names_from = registro, values_from = total)

# Creamos un total a partir de una suma y luego calculamos participaciones de no registrados
asalariados_registro <- asalariados_registro %>% mutate(
  Total = rowSums(across(where(is.numeric)), na.rm = TRUE),
  `Tasa no registro` = round(((`No registrado`/Total)*100), 1))

# Vamos a repetir considerando ahora también sector de empleo. Adecuamos la variable
eph_asalariados <- eph_asalariados %>% mutate(
  sector = factor(SECTOR, levels = c(1, 2, 3, 9), labels = c("Formal", "Informal", "Hogares", "No clasificado"))) %>% 
  select(!c(SECTOR))

# Agrupamos y pivoteamos
asalariados_sector <- eph_asalariados %>%
  group_by(region, registro, sector) %>% 
  summarise(total = sum(PONDERA)) %>% 
  pivot_wider(names_from = c(sector, registro), values_from = total)

# Transformamos en realtivos con across
asalariados_sector <- asalariados_sector %>% rowwise() %>%
  mutate(total = sum(across(where(is.numeric))), # Le pido que me cree totales sumando todas las columnas numéricas
         across(where(is.numeric), ~ .x / total)) %>% # Le pido que divide todas las columnas numéricas contra el total
  select(-total)

# Volvemos a eph_asalariados. Para trabajra ingresos, necesitamos filtrar a quienes no declaran
# Creamos un nuevo objeto quitando ingresos > 0
eph_asal_ingresos <- eph_asalariados %>% filter(ingresos > 0)

# Agrupamos pero promediando ingresos
asalariados_ingresos <- eph_asal_ingresos %>%
  group_by(sector, registro) %>%
  summarise(salario_promedio = weighted.mean(ingresos, PONDIIO)) %>% 
  arrange(-salario_promedio)

# Graficamos nuestras tres tablas resultantes. Primero la de las tasas de no registro
ggplot(asalariados_registro,
       aes(x = reorder(region, `Tasa no registro`), 
           y = `Tasa no registro`)) +
  geom_col(fill = "darkgreen") +
  labs(title = "Tasa de no registro por región. 4t24", x = "Región", y = "Tasa de no registro (%)") +
  coord_flip()
ggsave("asalariados_registro.png", width = 8, height = 5, dpi = 300)

# Luego la participación de cada combinación de empleo/sector. 
# Primero le tenemos que dar un retoque para que vuelva a ser long
asalariados_sector_long <- asalariados_sector %>%
  pivot_longer(
    cols = -region,
    names_to = "categoria",
    values_to = "proporcion"
  )

# Ahora sí el gráfico
ggplot(asalariados_sector_long, 
       aes(x = region, 
           y = proporcion, 
           fill = categoria)) +
  geom_col(position = "fill") +
  labs(x = "Región", y = "Participación (%)", fill = "Categoría") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_minimal()
ggsave("asalariados_sector.png", width = 8, height = 5, dpi = 300)

# Finalmente, el de salario promedio por empleo/sector
ggplot(asalariados_ingresos,
       aes(x = reorder(interaction(sector, registro), -salario_promedio), 
           y = salario_promedio)) +
  geom_col(fill = "darkgreen") +
  labs(title = "Ingreso promedio de asalariados por formalidad de sector y empleo. En pesos del 4t24", 
       x = "Tipo de sector y empleo", y = "Ingreso promedio")
ggsave("asalariados_ingresos.png", width = 8, height = 5, dpi = 300)

# Ahora sacamos en un sólo excel, en tres pestañas

# Crear el archivo
wb <- createWorkbook()

# Agregar la pestaña de tasas generales
addWorksheet(wb, "No registro por región")
writeData(wb, sheet = "No registro por región", asalariados_registro)

# Agregar la pestaña de tasas por género
addWorksheet(wb, "Registro y sector por región")
writeData(wb, sheet = "Registro y sector por región", asalariados_sector)

# Agregar la pestaña de tasas por edad
addWorksheet(wb, "Ingresos por registro y sector")
writeData(wb, sheet = "Ingresos por registro y sector", asalariados_ingresos)

# Save workbook
saveWorkbook(wb, "asalaraidos_informalidad_precariedad.xlsx", overwrite = TRUE)