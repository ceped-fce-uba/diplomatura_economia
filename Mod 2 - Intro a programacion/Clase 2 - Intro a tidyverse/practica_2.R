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


estado_datos <- as.data.frame(state.x77)
estado_datos$Estado <- rownames(estado_datos)