library(tidyverse)
library(readxl)
library(ggthemes)

options(scipen = 999)

base <- read_xlsx(path = "Mod 3 - Econ Internacional/elaboracion_materiales/clase1/bases/total_e_industria.xlsx",
                  sheet = 'Sheet 1',
                  range = 'A1:AU1571')

base <- base %>% 
  select(Anio, País, Regiones.economicas, Ocup_TOTAL_ECONOMIA, Ocup_INDUSTRIA) %>% 
  
  filter(Anio >= 1978,
         Anio <= 2018,
         Anio != 1999    # faltan datos de china para el 99'
  ) %>% 
  
  mutate(across(
    c(Ocup_TOTAL_ECONOMIA, Ocup_INDUSTRIA), # en miles para estos países
    ~ case_when(
      País %in% c("DEU", "BEL", "DNK", "FIN", "MYS", "NDL", "GBR", "SWE") ~ .x * 1000,
      TRUE ~ .x
    ))
  ) %>%
  
  mutate(pp_empleo_industrial = Ocup_INDUSTRIA / Ocup_TOTAL_ECONOMIA)

############################
# ACÁ ARRANCA EL EJERCICIO #
############################
# mutate(pp_empleo_industrial = Ocup_INDUSTRIA / Ocup_TOTAL_ECONOMIA)

datos_filtrados <- base %>%
  filter(País %in% c("DEU", "ARG", "CHN", "MEX")) %>%
  
  arrange(País, Anio) # Se ordena para que no haya problemas con lineas que van y vienen

lineas_empleo_industrial <- datos_filtrados %>% 
  ggplot(aes(x = Anio, y = pp_empleo_industrial, color = País, group = País)) +
  
  geom_line(linewidth = 1.2, alpha = 0.9) +
  
  # Sumamos puntos para enfatizar los datos disponibles
  geom_point(size = 2.5, alpha = 0.7) +
  
  scale_x_continuous(breaks = seq(from = 1978, to = 2018, by = 5)) +
  
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = "Evolución de la participación del empleo industrial",
    subtitle = "Alemania, Argentina, China y México (1978-2018)",
    x = "",
    y = "",
    caption = "Fuente: Graña y Terranova (2022)"
  ) +
  
  theme_fivethirtyeight() +
  
  scale_color_tableau() +
  
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, color = "darkgrey"),
    axis.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

plotly::ggplotly(lineas_empleo_industrial)

# DE EXTRA QUE PONGAN EL 100 DONDE SE LES CANTE, CAPAZ AL COMIENZO DE LA CLASE 2
# MUESTRO EL EXTRA

# Que escriban un parrafito explicando qué ven.

# POR SI ALGUIEN SE ANIMA

# tabla auxiliar
#
# ultimos_puntos <- datos_filtrados %>%
#   group_by(País) %>%
#   filter(Anio == max(Anio)) %>%
#   ungroup()

# geom_text_repel(
#   data = ultimos_puntos,
#   aes(label = País),
#   direction = "y",          # Prioriza separación vertical
#   hjust = -0.2,             # Desplaza texto a la derecha
#   segment.size = 0.5,       # Grosor de la línea que conecta
#   nudge_x = 1,              # Empujar etiquetas a la derecha 
#   box.padding = 0.5,        # Espaciado alrededor de etiquetas
#   size = 4.5,               # Tamaño de texto
#   fontface = "bold"         # Texto en negrita
# ) +

# y cambiarle:

# theme(
#   legend.position = "none")

# scale_x_continuous(limits = c(min(datos_filtrados$Anio), max(datos_filtrados$Anio) + 3.5))