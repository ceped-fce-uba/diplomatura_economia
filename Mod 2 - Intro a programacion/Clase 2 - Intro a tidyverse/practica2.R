library(tidyverse)
library(gapminder)

base <- read_csv("bases/adam_smith.csv")

names(base)

base <- base %>%
  filter(dias_trabajo == 1) %>% 
  rename(trabajadoras = trabajadores) %>% 
  mutate(productividad = alfileres_producidos / trabajadoras) %>% 
  select(organizacion_trabajo, productividad)

base <- read_csv("bases/gapminder_2000.csv")

base <- gapminder

resumen_gapminder <- base %>% 
  group_by(year, continent) %>%
  summarise("Promedio de esperanza de vida" = mean(lifeExp),
            "Población de los países de la base" = sum(pop))
  
