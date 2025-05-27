library(tidyverse)
library(ggthemes)
library(scales)

options(scipen = 999)

# Cambien las rutas, obviamente:
base <- read_csv("bases/dataset_2025-05-26T03_18_36.490488675Z_DEFAULT_INTEGRATION_IMF.STA_BOP_AGG_9.0.1.csv")

indicadores <- base$INDICATOR %>% 
  unique() %>% as.tibble()

cpi <- read_csv("bases/CPI-U_BLS.csv") %>% 
  filter(Period == "M13", # Este es el dato anual, en el mes 13, obviamente inexistente
         Year >= 2005
  ) %>% 
  select(Year, cpi = Value) %>% 
  arrange(Year) %>% # Por si las dudas, pero ya viene ordenado
  mutate(cpi = (cpi / first(cpi)) * 100) # Cambiamos la base del índice

base_porc <- base %>% 
  filter(TYPE_OF_TRANSFORMATION == "Percent of GDP") # Si quisieran graficar algún dato como porción del PBI

base <- base %>% 
  filter(TYPE_OF_TRANSFORMATION == "US dollar") %>% 
  left_join(cpi, by = c("TIME_PERIOD" = "Year")) %>% 
  mutate(valor_corr = OBS_VALUE / 1000000) %>%  # Para verlo en millones
  mutate(valor_const = ((valor_corr / cpi) * 100))

cuent_corr <- indicadores[13,] %>% as.character()
cuent_fin <- indicadores[36,] %>% as.character()
reserv <- indicadores[23,] %>% as.character()

cuenta_corriente <- base %>% 
  filter(INDICATOR == cuent_corr) %>%
  mutate(nombre_limpio = "Saldo Cuenta Corriente")

cuenta_financiera <- base %>% 
  filter(INDICATOR == cuent_fin) %>%
  mutate(nombre_limpio = "Saldo Cuenta Financiera",
         valor_const = -valor_const, # Para graficarlo como contraparte de la Cuenta Corriente
         valor_corr = -valor_corr)

reservas <- base %>% 
  filter(INDICATOR == reserv) %>%
  mutate(nombre_limpio = "Reservas",
         var_anual = valor_const - lag(valor_const) # ojo que R Base tiene una función lag, pero nosotros necesitamos la de dplyr)
         ) %>% 
  drop_na(var_anual) # porque generamos un valor faltante en la primer fila
  
# Combinamos todos los datos para el gráfico combinado
series_unidas <- bind_rows(
  cuenta_corriente %>% select(TIME_PERIOD, valor_const, valor_corr, nombre_limpio),
  cuenta_financiera %>% select(TIME_PERIOD, valor_const, valor_corr, nombre_limpio),
  reservas %>% select(TIME_PERIOD, valor_const, valor_corr, var_anual, nombre_limpio)
)

# Y esto para el gráfico final
series_cuentas <- bind_rows(
  cuenta_corriente %>% select(TIME_PERIOD, valor_const, valor_corr, nombre_limpio),
  cuenta_financiera %>% select(TIME_PERIOD, valor_const, valor_corr, nombre_limpio)
)

colors <- c("#2E8B57", "#FF6B35", "#4169E1") # para que sirvan de paleta

# Gráfico 1: Saldo de Cuenta Corriente
p1 <- ggplot(cuenta_corriente, aes(x = TIME_PERIOD, y = valor_const)) +
  geom_line(color = colors[1], size = 1.2, alpha = 0.8) +
  geom_point(color = colors[1], size = 2.5, alpha = 0.9) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", alpha = 0.7) +
  labs(
    title = "Argentina: Saldo de Cuenta Corriente",
    subtitle = "Datos anuales de la Balanza de Pagos en millones de dólares de 2005",
    x = "Año",
    y = "Millones de USD",
    caption = "Fuente: FMI"
  ) +
  theme_fivethirtyeight() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_continuous(breaks = 2005:2023) +
  scale_y_continuous(breaks = seq(-25000, 5000, 5000), 
                     labels = function(x) paste0(x/1000, "M USD"))

p1

# Gráfico 2: Saldo de la Cuenta Financiera
p2 <- ggplot(cuenta_financiera, aes(x = TIME_PERIOD, y = valor_const)) +
  geom_line(color = colors[2], size = 1.2, alpha = 0.8) +
  geom_point(color = colors[2], size = 2.5, alpha = 0.9) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", alpha = 0.7) +
  labs(
    title = "Argentina: Saldo de Cuenta Financiera",
    subtitle = "Datos anuales de la Balanza de Pagos en millones de dólares de 2005",
    x = "Año",
    y = "Millones de USD",
    caption = "Fuente: FMI"
  ) +
  theme_fivethirtyeight() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_continuous(breaks = 2005:2023) +
  scale_y_continuous(breaks = seq(-10000, 25000, 5000), 
                     labels = function(x) paste0(x/1000, "M USD"))

p2

# Gráfico 3: Reservas
p3 <- ggplot(reservas, aes(x = TIME_PERIOD, y = valor_const)) +
  geom_line(color = colors[3], size = 1.2, alpha = 0.8) +
  geom_point(color = colors[3], size = 2.5, alpha = 0.9) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", alpha = 0.7) +
  labs(
    title = "Argentina: Reservas",
    subtitle = "Datos anuales de la Balanza de Pagos en millones de dólares de 2005",
    x = "Año", 
    y = "Millones de USD",
    caption = "Fuente: FMI"
  ) +
  theme_fivethirtyeight() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_continuous(breaks = 2005:2023) +
  scale_y_continuous(breaks = seq(-15000, 15000, 5000), 
                     labels = function(x) paste0(x/1000, "M USD"))

p3

# Gráfico 4: Variación de reservas:

p4 <- ggplot(reservas, aes(x = TIME_PERIOD)) +
  geom_col(aes(y = var_anual, color = nombre_limpio, fill = nombre_limpio)) +
  scale_color_manual(values = colors) +
  scale_fill_manual(values = colors) +
  labs(
    title = "Argentina: Variación de Reservas",
    subtitle = "Datos anuales de la balanza de pagos",
    x = "Año",
    y = "Millones de USD", 
    color = "Indicador",
    caption = "Fuente: Estadísticas de Balanza de Pagos del FMI"
  ) +
  theme_fivethirtyeight() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none") + # porque sino queda horrible
  scale_x_continuous(breaks = 2005:2023) +
  scale_y_continuous(breaks = seq(-25000, 15000, 5000), 
                     labels = function(x) paste0(x/1000, "M USD"))

p4

# Gráfico final: muy lindo ejemplo de apilar muchas capas de geometrías

reservas$nombre_limpio <- "Variación de reservas" # Para que quede bien la etiqueta del gráfico

grafico_final <- ggplot() +
  # Columnas para la variación de reservas (van primero si queremos que queden atrás)
  geom_col(data = reservas, 
           aes(x = TIME_PERIOD, y = var_anual, fill = nombre_limpio),
           alpha = 0.6, width = 0.7) +
  
  # Líneas para cuenta corriente y financiera  
  geom_line(data = series_cuentas, 
            aes(x = TIME_PERIOD, y = valor_const, color = nombre_limpio, group = nombre_limpio),
            size = 1.4, alpha = 0.9) +

  geom_point(data = series_cuentas, 
             aes(x = TIME_PERIOD, y = valor_const, color = nombre_limpio),
             size = 2.5, alpha = 0.95) +
  
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40", alpha = 0.8) +
  
  scale_color_manual(values = colors, name = "") +
  scale_fill_manual(values = colors, name = "") +
  labs(
    title = "Argentina: Balanza de Pagos y Variación de Reservas",
    subtitle = "Cuenta Corriente y Financiera vs. Variación Anual de Reservas en millones de dólares de 2005",
    x = "",
    y = "", 
    caption = "Fuente: Estadísticas de Balanza de Pagos del FMI"
  ) +
  theme_fivethirtyeight() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_continuous(breaks = 2005:2023) +
  scale_y_continuous(breaks = seq(-25000, 25000, 5000), 
                     labels = function(x) paste0(x/1000, "M USD"))

print(grafico_final)
