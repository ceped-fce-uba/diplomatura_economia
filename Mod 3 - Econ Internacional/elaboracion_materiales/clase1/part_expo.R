library(tidyverse)
library(readxl)
library(ggthemes)
library(patchwork)
library(RColorBrewer)

part_expo <- read_xlsx("Mod 3 - Econ Internacional/elaboracion_materiales/clase1/bases/datos_graficos.xlsx", sheet = "grafico 1", range = "A1:D1102")

# # Creamos una columna con intervalos de años:
# part_expo$grupo_anios <- cut(part_expo$Year, 
#                             breaks = c(1970, 1975, 1980, 1985, 1990, 1995, 2000, 2005, 2010, 2015, 2020),
#                             labels = c("1970-1974", "1975-1979", "1980-1984", "1985-1989", 
#                                        "1990-1994", "1995-1999", "2000-2004", "2005-2009",
#                                        "2010-2014", "2015-2019"),
#                             right = FALSE)  # Crea intervalos cerrados a la izquierda, abiertos a la derecha, para no incluir 2020

# Separamos según región (esto supongo que sería mejor hacerlo con un 'for loop' pero no lo vimos):
part_expo_latam <- part_expo %>% 
  filter(Regiones.economicas == "Latin American")
  
part_expo_este_asiatico <- part_expo %>% 
  filter(Regiones.economicas == "East Asia")

part_expo_desarrollados <- part_expo %>% 
  filter(Regiones.economicas == "Developed")

plot_latam <- ggplot(part_expo_latam, aes(x = Year, 
                      y = Proporcion.mundial,
                      fill = Paises)) +
  geom_col(position = "stack") +
  labs(
    title = "América Latina",
    subtitle = "",
    x = "",
    y = "",
    fill = "Países LATAM"
  ) +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right",
    panel.grid.major.x = element_blank()
  ) +
  scale_y_continuous(limits = c(0, 100)) +
  scale_fill_few()

plot_este_asiatico <- ggplot(part_expo_este_asiatico, aes(x = Year, 
                            y = Proporcion.mundial,
                            fill = Paises)) +
  geom_col(position = "stack") +
  labs(
    title = "Este Asiático",
    subtitle = "",
    x = "",
    y = "",
    fill = "Paises Este Asiático"
  ) +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right",
    panel.grid.major.x = element_blank()
  ) +
  scale_y_continuous(limits = c(0, 100)) +
  scale_fill_economist()

plot_desarrollados <- ggplot(part_expo_desarrollados, aes(x = Year, 
                            y = Proporcion.mundial,
                            fill = Paises)) +
  geom_col(position = "stack") +
  labs(
    title = "Países Desarrollados",
    subtitle = "",
    x = "",
    y = "",
    fill = "Países Desarrollados"
  ) +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right",
    panel.grid.major.x = element_blank()
  ) +
  scale_y_continuous(limits = c(0, 100)) +
  scale_fill_tableau("Tableau 20") 

grafico_combinado <- (plot_desarrollados | plot_este_asiatico | plot_latam) +
  plot_layout(guides = "collect") +
  plot_annotation(
    title = "Participación en las exportaciones mundiales de manufacturas por país, 1970-2019",
    theme = theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))
  )

# Mostramos el gráfico final
grafico_combinado

# Alternativamente...

library(plotly)

plotly_desarrollados <- ggplotly(plot_desarrollados)
plotly_este_asiatico <- ggplotly(plot_este_asiatico)
plotly_latam <- ggplotly(plot_latam)

grafico_combinado_interactivo <- subplot(
  plotly_desarrollados, 
  plotly_este_asiatico, 
  plotly_latam,
  nrows = 1,
  margin = 0.05,
  titleY = TRUE,
  titleX = TRUE
) %>% 
  layout(
    title = list(
      text = "Participación en las exportaciones mundiales de manufacturas por país, 1970-2019",
      y = 0.95,
      x = 0.5,
      xanchor = 'center',
      yanchor = 'top'
    ),
    margin = list(t = 100)
  )

grafico_combinado_interactivo
