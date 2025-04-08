library(tidyverse)
library(readxl)
library(lubridate)      # Para manejar fechas
library(plotly)         # Interactive plots

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


ggplot(temp_arg)

ggplot(temp_arg, aes(x = Month, y = temp_prom)) +
  geom_point()

ggplot(SIPA_viz, aes(x = Periodo, y = Valor, group = 1)) +
  geom_point()

ggplot(SIPA_viz, aes(x = Mes, y = Valor)) +
  geom_boxplot()

ggplot(SIPA_viz, aes(x = Mes, y = Valor, color = Anio)) +
  geom_line()

#SIPA_viz <- SIPA %>% 
#  filter(Variable == "Empleo asalariado en el sector privado", Valor > 200) %>% 
#  mutate(Periodo = paste0(Anio,".", Mes),
#         Mes = as.factor(Mes)) %>% 
#  select(Periodo, Valor, Anio, Mes)

#PROBANDO

library(plotly)
library(htmlwidgets)

plot_remun <- remuneracion_real %>%
  filter(year(Periodo) >= 2019) %>%
  ggplot(aes(x = Periodo, y = indice_real)) +
  geom_line(color = 'steelblue', size = 1) +
  geom_point(color = 'steelblue', size = 2.5) +
  scale_x_date(date_breaks = "2 month",
               date_labels = "%b %Y") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45,
                                  hjust = 1)
        )
  

plotly_remun <- plot_remun %>%
  ggplotly()

htmlwidgets::saveWidget(plotly_remun, "remuneracion_desde_2019.html")

plotly_remun
