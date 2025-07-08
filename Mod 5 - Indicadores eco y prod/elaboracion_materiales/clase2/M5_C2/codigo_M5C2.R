# Clase 2 Módulo 5 - Práctica
# Diplomatura Problemas Actuales de la Economía, el Empleo y el Comercio (CEPED FCE - UBA)
# DOCENTES: Ezequiel Monteforte, Juan Camilo Gutman

# Carga de librerías
library(tidyverse)
library(readxl)
library(ggthemes)
library(plotly)
library(scales)

options(scipen=999)

# Indicadores de Tipo de Cambio Real

## Descarga y lectura de datos

# La URL que copiamos del sitio web
url <- "https://www.bcra.gob.ar/Pdfs/PublicacionesEstadisticas/ITCRMSerie.xlsx"

# la ruta relativa, que voy a usarlo para la descarga y luego la carga
destino <- "bases/ITCRMSerie.xlsx"

download.file(url,
              destino,
              mode = "wb", 
              method = "curl",
              extra = "-k")

# prestenle atención al argumento skip, clave para los archivos institucionales de excel
data <- read_xlsx(destino, 
                  skip = 1,
                  col_types = c("text", rep("numeric", 15))) # cargamos la fecha como texto y el resto como numéricas
                  

# Transformamos la columna de fechas
data$Período <- as.Date(as.numeric(data$Período), origin = "1899-12-30")

# Clave para todo lo que viene de excel los trims
names(data) <- trimws(names(data))

# Descartamos filas con NA's en la columna Período:
data <- data %>% 
  drop_na(Período)

## Visualización

### ITCRM

# Ojo que acá dejamos el 100 como lo pone el BCRA
graf1 <- ggplot(data, aes(x = Período, y = ITCRM)) +
  geom_line(color = "4E79A7", linewidth = 1) +
  labs(title = "Índice de Tipo de Cambio Real Multilateral (ITCRM)",
       subtitle = "Base 17-12-15 = 100",
       x = "Fecha",
       y = "Índice ITCRM") +
  theme_fivethirtyeight() +
  scale_color_tableau() +
  scale_x_date(date_labels = "%m-%Y", date_breaks = "4 years") +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5))

graf1

### ITCRB con EEUU, China y Brasil + ITCRM

# Para graficar varias series juntas, primero tenemos que pasar los datos a formato largo.
data_long <- data %>%
  select(Período, ITCRM, `ITCRB Estados Unidos`, `ITCRB China`, `ITCRB Brasil`) %>%
  pivot_longer(cols = -Período, names_to = "Índice", values_to = "Valor")

graf2 <- ggplot(data_long, aes(x = Período, y = Valor, color = Índice)) +
  geom_line(size = 1) +
  labs(title = "Índices de Tipo de Cambio Real - Argentina",
       subtitle = "ITCRM e ITRCB con EEUU, China y Brasil (Base 17-12-15 = 100)",
       x = "Fecha",
       y = "Valor del Índice") +

  scale_x_date(
    date_labels = "%Y", 
    date_breaks = "2 years",
    expand = c(0, 0)
  ) +
  
  theme_fivethirtyeight() +
  scale_color_tableau() +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "bottom")

graf2

# Y, podemos poner el 100 en alguna fecha base (y de paso volverlo interactivo con ggplotly):

# Si quisieran poner el 100 al comienzo de la serie, algo que ya hemos hecho:
# data_long_bis <- data_long %>%
#   group_by(Índice) %>% 
#   mutate(Valor = (Valor / first(Valor, order_by = Período)) * 100) %>%
#   ungroup()

fecha_base <- as.Date("2006-01-01")

valores_base <- data_long %>%
  filter(Período == fecha_base) %>%
  select(Índice, valor_base = Valor)

data_long_bis <- data_long %>%
  left_join(valores_base, by = "Índice") %>%
  group_by(Índice) %>%
  mutate(Valor = (Valor / valor_base) * 100) %>%
  ungroup() %>%
  select(-valor_base)

graf3 <- ggplot(data_long_bis, aes(x = Período, y = Valor, color = Índice)) +
  geom_line(size = 1) +
  labs(title = "Índices de Tipo de Cambio Real - Argentina",
       subtitle = "ITCRM e ITRCB con EEUU y China (Base 17-12-15 = 100)",
       x = "Fecha",
       y = "Valor del Índice") +
  
  scale_y_continuous(breaks = seq(40, 160, by = 20)) +

  scale_x_date(
    date_labels = "%Y", 
    date_breaks = "2 years",
    expand = c(0, 0)
  ) +  

  theme_fivethirtyeight() +
  scale_color_tableau() +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "bottom")

graf3 %>% ggplotly()