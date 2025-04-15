library(tidyverse)
library(readxl)

base_sipa <- read_csv("bases/base_sipa.csv")

base_ipc <- read_xlsx("bases/ipc_ceped_data.xlsx")

head(base_sipa)

tail(base_ipc)


base_regiones <- read_csv("bases/regiones_arg")



# ADAM SMITH

library(tidyverse)

adam_smith <- read_csv("bases/adam_smith.csv")

adam_smith <- adam_smith %>%
  filter(dias_trabajo == 1)

adam_smith <- adam_smith %>%
  rename(trabajadoras = trabajadores)

adam_smith <- adam_smith %>%
  mutate(prod_per_cap = alfileres_producidos / trabajadoras)


adam_smith <- adam_smith %>%
  select(organizacion_trabajo, prod_per_cap)

adam_smith %>% summarise('Producción promedio' = mean(prod_per_cap))


############################################################3

library(tidyverse)

gapminder_2000 <- read_csv("bases/gapminder_2000.csv")

gapminder_2000$region %>% unique()

table(gapminder_2000$region)

america_2000 <- gapminder_2000 %>%
  filter(region == "America")

america_2000 %>% summary() # ó summary(america_2000)

america_2000 %>% 
  filter(is.na(child_mortality))


america_2000 <- america_2000 %>% 
  filter(!is.na(child_mortality))

poblacion_por_region <- gapminder_2000 %>%
  group_by(region) %>%
  summarize(poblacion_media = mean(population)) %>% 
  arrange(desc(poblacion_media))

            