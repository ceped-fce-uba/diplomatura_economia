# Carga de librerías
library(httr)       # para hacer las solicitudes a la API
library(jsonlite)   # para procesar el formato JSON
library(tidyverse)  # nuestro querido tidyverse
library(lubridate)  # para manejar fechas
library(ggthemes)   # para la estética de los gráficos
library(scales)     # para formatear las etiquetas de los gráficos

options(scipen=999) # para evitar la notación científica


# URL base de la API
url <- "https://api.bcra.gob.ar/estadisticas/v3.0/monetarias"

# Con la función GET del paquete httr realizamos la solicitud
response <- GET(url, config(ssl_verifypeer = FALSE))

# Procesamos el JSON a un formato de R
data <- fromJSON(content(response, "text"))

# Filtramos para quedarnos con las Principales Variables
df_series <- data$results %>% 
  filter(categoria == 'Principales Variables')

# Imprimimos la tabla de series disponibles
print(df_series)


#----------------------------------------------------------------
# GRÁFICO DE BASE MONETARIA
#----------------------------------------------------------------

url_base_mon = "https://api.bcra.gob.ar/estadisticas/v3.0/monetarias/15"
response_base_mon <- GET(url_base_mon, config(ssl_verifypeer = FALSE))
data_base_mon <- fromJSON(content(response_base_mon, "text"))
df_base_mon <- data_base_mon$results
df_base_mon$fecha <- as.Date(df_base_mon$fecha)

df_base_mon <- df_base_mon %>% 
  filter(fecha >= as.Date("2024-06-30")) %>% 
  arrange(fecha) %>% 
  select(fecha, valor)

plot_base_monetaria <- ggplot(df_base_mon, aes(x = fecha, y = valor)) +
  geom_line(color = "#F28E2B", size = 1.5) +
  theme_fivethirtyeight() +
  labs(
    title = "Base monetaria (último año)",
    subtitle = "Total (en millones de pesos)",
    caption = "Fuente: BCRA"
  ) +
  scale_y_continuous(labels = scales::label_dollar(
    prefix = "ARS ",
    suffix = " M",
    big.mark = ".",
    decimal.mark = ","
      ),
    breaks = scales::pretty_breaks(n = 6)
  ) +
  scale_x_date(
    date_labels = "%d-%m-%y",
    date_breaks = "1 month"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  )

print(plot_base_monetaria)

#----------------------------------------------------------------
# GRÁFICO DE RESERVAS
#----------------------------------------------------------------

url_reservas = "https://api.bcra.gob.ar/estadisticas/v3.0/monetarias/1"
response_reservas <- GET(url_reservas, config(ssl_verifypeer = FALSE))
data_reservas <- fromJSON(content(response_reservas, "text"))
df_reservas <- data_reservas$results
df_reservas$fecha <- as.Date(df_reservas$fecha)

df_reservas <- df_reservas %>% 
  filter(fecha >= as.Date("2023-06-30")) %>% 
  arrange(fecha) %>% 
  select(fecha, valor)

plot_reservas <- ggplot(df_reservas, aes(x = fecha, y = valor)) +
  geom_line(color = "#4E79A7", size = 1.5) +
  theme_fivethirtyeight() +
  labs(
    title = "Reservas BCRA (últimos dos años)",
    subtitle = "Reservas diarias en millones de dólares",
    caption = "Fuente: BCRA"
  ) +
  scale_y_continuous(labels = scales::label_dollar(
    prefix = "USD ",
    suffix = " M",
    big.mark = ".",
    decimal.mark = ","
      ),
    breaks = scales::pretty_breaks(n = 6)
  ) +
  scale_x_date(
    date_labels = "%d-%m-%y",
    date_breaks = "1 month"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  )

print(plot_reservas)

#----------------------------------------------------------------
# GRÁFICO DE CIRCULACIÓN MONETARIA + RESERVAS
#----------------------------------------------------------------

# Descargamos datos de circulación monetaria
url_circulacion = "https://api.bcra.gob.ar/estadisticas/v3.0/monetarias/16"
response_circulacion <- GET(url_circulacion, config(ssl_verifypeer = FALSE))
data_circulacion <- fromJSON(content(response_circulacion, "text"))
df_circulacion <- data_circulacion$results
df_circulacion$fecha <- as.Date(df_circulacion$fecha)
df_circulacion <- df_circulacion %>% 
  filter(fecha >= as.Date("2023-06-30")) %>% 
  arrange(fecha) %>% 
  select(fecha, circulacion = valor)

# Descargamos datos de tipo de cambio
url_tc = "https://api.bcra.gob.ar/estadisticas/v3.0/monetarias/5"
response_tc <- GET(url_tc, config(ssl_verifypeer = FALSE))
data_tc <- fromJSON(content(response_tc, "text"))
df_tc <- data_tc$results
df_tc$fecha <- as.Date(df_tc$fecha)
df_tc <- df_tc %>% 
  filter(fecha >= as.Date("2023-06-30")) %>% 
  arrange(fecha) %>% 
  select(fecha, tc_mayorista = valor)

# Unimos y calculamos la circulación en dólares
df_circ_tc <- left_join(df_circulacion, df_tc, by = "fecha")
df_circ_tc <- df_circ_tc %>% 
  mutate(circulacion_dolares = circulacion / tc_mayorista)

# Renombramos la columna valor de df_reservas para el join
df_reservas_join <- df_reservas %>%
  rename(reservas = valor)

# Unimos los dos dataframes
df_plot <- left_join(df_circ_tc, df_reservas_join, by = "fecha")

# Pasamos a formato long para ggplot

df_plot_long <- df_plot %>%
  select(fecha, `Circulación Monetaria` = circulacion_dolares, `Reservas` = reservas) %>%
  pivot_longer(cols = -fecha, names_to = "variable", values_to = "valor")

# Graficamos ambas series
plot_circ_reservas <- ggplot(df_plot_long, aes(x = fecha, y = valor, color = variable)) +
  geom_line(size = 1.5) +
  theme_fivethirtyeight() +
  labs(
    title = "Circulación Monetaria y Reservas",
    subtitle = "En millones de dólares",
    caption = "Fuente: BCRA",
    color = ""
  ) +
  scale_y_continuous(
    labels = scales::label_dollar(
      prefix = "USD ",
      suffix = " M",
      big.mark = ".",
      decimal.mark = ","
      ),
    breaks = scales::pretty_breaks(n = 6)
  ) +
  scale_x_date(
    date_labels = "%d-%m-%y",
    date_breaks = "1 month"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    legend.position = "top"
  ) +
  scale_color_manual(values = c("Circulación Monetaria" = "#4E79A7", "Reservas" = "#F28E2B"))

print(plot_circ_reservas)

#----------------------------------------------------------------
# EXTRA: PROGRAMACIÓN FUNCIONAL
#----------------------------------------------------------------

api_bcra <- function(id) {
  
  # Librerías necesarias
  library(httr)
  library(jsonlite)
  library(lubridate)
  
  # 1. Se construye la URL con el id provisto
  url <- paste0("https://api.bcra.gob.ar/estadisticas/v3.0/monetarias/", id)
  
  # 2. Solicitud GET a la API
  response <- GET(url, config(ssl_verifypeer = FALSE))

  # 3. Chequeo de código de estado
  if (status_code(response) != 200) {
    stop("La solicitud falló con código: ", status_code(response))
   }
  
  # 4. Extraemos los datos del JSON
  data <- fromJSON(content(response, "text"))

  # 5. Creamos el dataframe
  df <- data$results
  df$fecha <- as.Date(df$fecha)
  
  # 6. Vayan a estudiar si quieren saber qué hace el return!
  return(df)
}

#----------------------------------------------------------------
# EJEMPLO DE USO DE LA FUNCIÓN
#----------------------------------------------------------------
prueba_función_reservas <- api_bcra(1)
prueba_función_reservas


