library(tidyverse)
library(readxl)
library(lubridate)      # Para manejar fechas
library(plotly)         # Gráficos interactivos
library(gt)             # Para mostrar las tablas mejor

SIPA <- read_csv("bases/base_sipa.csv", 
                 show_col_types = FALSE)

class(SIPA$Periodo)

SIPA <- SIPA %>% 
  mutate(Periodo = as.Date(Periodo))

class(SIPA$Periodo)
  

names(SIPA)

table(SIPA$Variable)

SIPA <- SIPA %>% 
  mutate(Anio = year(Periodo))

flujo1 <- SIPA %>% 
 group_by(Anio, Variable) %>% 
 summarise(Promedio = mean(Valor))

flujo1

flujo2 <- SIPA %>%
  filter(Variable == "Empleo asalariado en el sector privado") %>% 
  group_by(Anio) %>% 
  summarise(Promedio = mean(Valor))

flujo2

flujo3 <- SIPA %>%
  filter(Variable == "Empleo en casas particulares") %>% 
  group_by(Anio) %>% 
  summarise(Promedio = mean(Valor)) %>% 
  arrange(-Anio)

flujo3

SIPA <- SIPA %>% 
  mutate(Mes = month(Periodo),
         Trimestre = quarter(Periodo))

ipc_mensual <- read_xlsx("../clase2/bases/ipc_ceped_data.xlsx")

class(ipc_mensual$fecha)

ipc_mensual <- ipc_mensual %>% 
  mutate(fecha = as.Date(fecha))

class(ipc_mensual$fecha)

remuneracion_media <- SIPA %>% 
  filter(Variable == "Remuneración promedio - sin estacionalidad") %>% 
  mutate(indice_remuneraciones = Valor/Valor[Periodo == "2009-01-01"]*100)

ipc_mensual <- ipc_mensual %>% 
  mutate(indice_ipc_2009 = valor/valor[fecha == "2009-01-01"]*100)

remuneracion_real <- remuneracion_media %>%
  left_join(ipc_mensual, by = c("Periodo" = "fecha"))

remuneracion_real <- remuneracion_real %>% 
  mutate(indice_real = indice_remuneraciones/indice_ipc_2009*100,
         Trimestre        = quarter(Periodo)) %>% 
  select(Periodo, Anio = ANO4, Trimestre, Mes = sub, indice_real) 

remuneracion_real_anual <- remuneracion_real %>% 
  group_by(Anio) %>% 
  summarise(Promedio_Anual = mean(indice_real))

remuneracion_real_anual <- remuneracion_real_anual %>% 
  mutate(Variacion = Promedio_Anual/lag(Promedio_Anual)-1)

remuneracion_real_anual <- remuneracion_real_anual %>% 
  mutate(Variacion = round((Promedio_Anual/lag(Promedio_Anual)-1)*100, digits = 2))

# write_excel_csv(remuneracion_real_anual, "Remuneración Real Anual 2009-2024.xlsx")

library(ggplot2)

temp_arg <- read.csv("bases/city_temperature_arg.csv") 
#hay que filtrar las bases por que tienen datos mising computados como -99

temp_mex <- read.csv("bases/city_temperature_mex.csv")

temp_arg <- temp_arg %>% 
 mutate(Month = as.factor(Month))
temp_mex <- temp_mex %>% 
 mutate(Month = as.factor(Month))

summary(temp_arg$temp_prom)
summary(temp_mex$temp_prom)

temp_arg <- temp_arg %>% 
  filter(temp_prom > 0)

temp_mex <- temp_mex %>% 
  filter(temp_prom > 0)

ggplot(temp_arg)

ggplot(temp_arg, aes(x = Month, y = temp_prom)) +
  geom_point()

ggplot(temp_arg, aes(x = Month, y = temp_prom)) +
  geom_boxplot()

temp_arg <- temp_arg %>% 
  mutate(day_of_year = yday(paste(Year, Month, Day, sep = "-")))

temp_mex <- temp_mex %>% 
  mutate(day_of_year = yday(paste(Year, Month, Day, sep = "-")))


flujo4 <- temp_arg %>% 
  group_by(day_of_year) %>% 
  summarise(promedio_dia = mean(temp_prom))

ggplot(flujo4, aes(x = day_of_year, y = promedio_dia)) +
  geom_line()


flujo5 <- temp_mex %>% 
  group_by(City, day_of_year) %>% 
  summarise(promedio_dia = mean(temp_prom))

ggplot(flujo5, aes(x = day_of_year, y = promedio_dia, color = City)) +
  geom_line()

flujo6 <- flujo5 %>% 
  rename(Temperatura = promedio_dia)

ggplot(flujo6, aes(x = day_of_year, y = City, color = Temperatura)) +
  geom_point()

ggplot(temp_mex, aes(x = Month, y = temp_prom, fill = City)) +
  geom_boxplot()

remuneracion_real %>% 
  ggplot(aes(x = Periodo, y = indice_real)) +
  geom_line()

remuneracion_real %>% 
  ggplot(aes(x = Periodo, y = indice_real)) +
  geom_line() +
  labs(title = "Remuneración Real Asal. Reg. del Sec. Privado",
       subtitle = "Total País. Sin estacionalidad.",
       y = "Nivel",
       x = "", #de este modo no aparece el nombre de la variable X
       caption = "Fuente: Sistema Integrado Previsional Argentino (SIPA)") 

remuneracion_real %>% 
  ggplot(aes(x = Periodo, y = indice_real)) +
  geom_line(color = "#104E8B", size = 1) +
  theme_classic() +
  labs(title = "Remuneración Real Asal. Reg. del Sec. Privado",
       subtitle = "Total País. Sin estacionalidad.",
       y = "Nivel",
       x = "", #de este modo no aparece el nombre de la variable X
       caption = "Fuente: Sistema Integrado Previsional Argentino (SIPA)") 
