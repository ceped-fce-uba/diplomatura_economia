library(httr)
library(jsonlite)
library(tidyverse)
library(lubridate)
library(ggthemes)
library(scales)


# Base URL for BCRA API v3.0
url <- "https://api.bcra.gob.ar/estadisticas/v3.0/monetarias"

response <- GET(url, config(ssl_verifypeer = FALSE))

data <- fromJSON(content(response, "text"))

df <- data$results

df

url_reservas = "https://api.bcra.gob.ar/estadisticas/v3.0/monetarias/1"

response_reservas <- GET(url_reservas, config(ssl_verifypeer = FALSE))

data_reservas <- fromJSON(content(response_reservas, "text"))

df_reservas <- data_reservas$results
df_reservas

df_reservas$fecha <- as.Date(df_reservas$fecha)

df_reservas <- df_reservas %>% 
  filter(fecha >= as.Date("2024-06-25"))


ggplot(df_reservas, aes(x = fecha, y = valor)) +
  geom_line(color = "#1f77b4", size = 1.2) +
  geom_point(color = "#1f77b4", size = 2.5) +
  theme_fivethirtyeight() +
  labs(
    title = "Reservas BCRA",
    subtitle = "Reservas diarias en millones de dólares",
    caption = "Fuente: BCRA"
  ) +
  scale_y_continuous(
    labels = scales::comma_format(),
    breaks = scales::pretty_breaks(n = 6)
  ) +
  scale_x_date(
    date_labels = "%d-%m",
    date_breaks = "15 days"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  )

##################
# BASE MONETARIA #
##################

url_base_mon = "https://api.bcra.gob.ar/estadisticas/v3.0/monetarias/15"

response_base_mon <- GET(url_base_mon, config(ssl_verifypeer = FALSE))

data_base_mon <- fromJSON(content(response_base_mon, "text"))

df_base_mon <- data_base_mon$results
df_base_mon

df_base_mon$fecha <- as.Date(df_base_mon$fecha)

df_base_mon <- df_base_mon %>% 
  filter(fecha >= as.Date("2024-06-25"))


ggplot(df_base_mon, aes(x = fecha, y = valor)) +
  geom_line(color = "#1f77b4", size = 1.2) +
  geom_point(color = "#1f77b4", size = 2.5) +
  theme_fivethirtyeight() +
  labs(
    title = "Base monetaria",
    subtitle = "Base monetaria - Total (en millones de pesos)",
    caption = "Fuente: BCRA"
  ) +
  scale_y_continuous(
    labels = scales::comma_format(),
    breaks = scales::pretty_breaks(n = 6)
  ) +
  scale_x_date(
    date_labels = "%d-%m",
    date_breaks = "15 days"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  )
