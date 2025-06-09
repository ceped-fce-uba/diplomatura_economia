library(tidyverse)
library(readxl)
library(ggthemes)
library(plotly)

url <- "https://www.bcra.gob.ar/Pdfs/PublicacionesEstadisticas/ITCRMSerie.xlsx"
destino <- "Mod 5 - Indicadores eco y prod/elaboracion_materiales/clase1/bases/ITCRMSerie.xlsx"

download.file(url, destino, mode = "wb", method = "curl", extra = "-k")

data <- read_xlsx(destino, 
                  skip = 1,  # Skip the first row
                  col_types = c("text", rep("numeric", 15)))  # Cargamos como texto para luego transformar

# Transformamos la columna de fechas
data$Período <- as.Date(as.numeric(data$Período), origin = "1899-12-30")

# Clave para todo lo que viene de excel los trims
names(data) <- trimws(names(data))

# Descartamos filas con NA's en la columna Período:
data <- data %>% 
  drop_na(Período)

# Gráfico 1: el ITCRM
graf1 <- ggplot(data, aes(x = Período, y = ITCRM)) +
  geom_line(color = "blue", size = 1) +
  labs(title = "Índice de Tipo de Cambio Real Multilateral (ITCRM)",
       subtitle = "Base 17-12-15 = 100",
       x = "Fecha",
       y = "Índice ITCRM") +
  theme_fivethirtyeight() +
  scale_color_fivethirtyeight() +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5))

graf1

# Para graficar varias series juntas usamos el formato largo
data_long <- data %>%
  select(Período, ITCRM, `ITCRB Estados Unidos`, `ITCRB China`) %>%
  pivot_longer(cols = -Período, names_to = "Índice", values_to = "Valor")

# Gráfico 2: EEUU y China
graf2 <- ggplot(data_long, aes(x = Período, y = Valor, color = Índice)) +
  geom_line(size = 1) +
  labs(title = "Índices de Tipo de Cambio Real - Argentina",
       subtitle = "ITCRM e ITRCB con EEUU y China (Base 17-12-15 = 100)",
       x = "Fecha",
       y = "Valor del Índice") +
  theme_fivethirtyeight() +
  scale_color_tableau() +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "bottom")

graf2

graf2 %>% ggplotly()

# Filtro de los últimos 5 años
data_reciente <- data %>%
  filter(Período >= as.Date("2019-01-01")) %>% 
  arrange(desc(Período))

# Resumen estadístico
cat("Resumen del ITCRM:\n")
summary(data$ITCRM)
cat("\nÚltimo valor disponible:\n")
tail(data[c("Período", "ITCRM")], 1)