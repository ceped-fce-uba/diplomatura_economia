library(tidyverse)
library(readxl)
library(ggthemes)
library(patchwork)

va_ind <- read_xlsx("Mod 3 - Econ Internacional/elaboracion_materiales/clase1/bases/datos_graficos.xlsx", sheet = "grafico 3", range = "A1:D121")

graf_va <- va_ind %>%
  select(Año,Regiones.economicas,`Industrial Value Added`) %>%
  ggplot(aes(x = Año, y = `Industrial Value Added`, fill = Regiones.economicas)) +
  geom_col(position = "stack") +
  scale_x_continuous(breaks = seq(1978, 2018, by = 5)) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  labs(
    title = "Promedio ponderado de la participación del valor agregado manufacturero por región",
    subtitle = "Ponderado por la participación de cada país en el valor agregado en PPA de la muestra",
    x = "",
    y = "",
    caption = "Fuentes: EUROSTAT, Graña (2017), Graña and Terranova (2020), Kennedy, Pacífico and Sánchez (2018), University
of Groningen, ILO-Stats, OECD-Stats, LA-KLEMS, UNIDO, World Bank, y cuentas nacionales de cada país. Notas: Debido a la falta
de datos de empleo total para China en 1999 decidimos no incluir ese año en los cálculos",
    fill = "Región"
  ) +
  theme_classic() +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    plot.title.position = "plot",
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  scale_fill_tableau()

graf_va

