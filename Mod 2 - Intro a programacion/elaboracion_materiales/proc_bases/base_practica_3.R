library(tidyverse)
library(readxl)

sipa <- read_csv("Mod 2 - Intro a programacion/Clase 3 - Tidyverse avanzado y visualizacion/bases/base_sipa.csv")
regiones <- read_csv("Mod 2 - Intro a programacion/Clase 3 - Tidyverse avanzado y visualizacion/bases/regiones_arg.csv")

sipa <- sipa %>%
  mutate(Periodo = as.Date(Periodo),
         Geografia = str_replace_all(Geografia, "TUCUMÁN", "TUCUMAN")
         ) %>% 
  filter(Variable == "Asalariados registrados en el sector privado",
         Periodo == as.Date('2015-01-01')
         ) 

sipa_regiones_group_by <- sipa %>%
  left_join(regiones, by = "Geografia") %>%
  group_by(Region) %>% 
  summarize(asalariados_privados = sum(Valor) * 1000)

ggplot(sipa_regiones_group_by, aes(x = Region, y = asalariados_privados)) +
  geom_col(fill = "steelblue")