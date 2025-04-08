# install.packages("tidyverse")
# library(tidyverse)
# 

library(tidyverse)


OCUPADOS  <- c(6105953, 2543717, 10931300,
               6103382, 2598154, 10982740,
               6112372, 2602057, 10992215)
FECHA  <-  c("Ene-12", "Ene-12", "Ene-12",
             "Feb-12", "Feb-12", "Feb-12",
             "Mar-12", "Mar-12", "Mar-12")
SECTOR  <-  c("Privado_Registrado","Público","Total",
              "Privado_Registrado","Público","Total",
              "Privado_Registrado","Público","Total")
Datos <- data.frame(OCUPADOS, FECHA, SECTOR)

Datos %>% 
 filter(SECTOR == "Total")

Datos %>% 
  filter(SECTOR == "Total", OCUPADOS > 10931300)

Datos %>% 
  filter(OCUPADOS > 10931300 | SECTOR == "Privado_Registrado")

Datos %>% 
  rename(Periodo = FECHA)

Datos <- Datos %>% 
  mutate(Promedio = mean(OCUPADOS))
Datos

Datos <- Datos %>% 
  mutate(caso_cuando = case_when(SECTOR == "Privado_Registrado"    ~ OCUPADOS*2,
                                 SECTOR == "Público"               ~ OCUPADOS*3,
                                 SECTOR == "Privado_No_Registrado" ~ OCUPADOS*5 ))
Datos

Datos2 <- Datos %>% 
  select(OCUPADOS, FECHA, SECTOR)
Datos2

Datos <- Datos %>% 
  arrange(SECTOR, OCUPADOS)
Datos

Datos %>% 
  summarise(Indprom = mean(OCUPADOS))

Datos %>% 
  group_by(FECHA) %>%
  summarise("Ocupados por habitante" = OCUPADOS/45000000)

knitr::include_graphics("imgs/joins.png")

INDICE  <- c(100,   100,   100,
             101.8, 101.2, 100.73,
             102.9, 102.4, 103.2)
FECHA  <-  c("Oct-16", "Oct-16", "Oct-16",
             "Nov-16", "Nov-16", "Nov-16",
             "Dic-16", "Dic-16", "Dic-16")
GRUPO  <-  c("Privado_Registrado","Público","Privado_No_Registrado",
             "Privado_Registrado","Público","Privado_No_Registrado",
             "Privado_Registrado","Público","Privado_No_Registrado")
IS <- data.frame(INDICE, FECHA, GRUPO)

Ponderadores <- data.frame(GRUPO = c("Privado_Registrado","Público","Privado_No_Registrado"),
                            PONDERADOR = c(50.16,29.91,19.93))
IS_join <- IS %>% 
  left_join(.,Ponderadores, by = "GRUPO")
IS_join

IS_Indice_Gral <- IS_join %>% 
  group_by(FECHA) %>% 
  summarise(Indice_Gral = weighted.mean(INDICE,w = PONDERADOR))
IS_Indice_Gral

knitr::include_graphics("imgs/wide_long.png")



summary(Datos)

unique(Datos$SECTOR)

sample_n(tbl = Datos,size = 9)

table(Datos$SECTOR)
