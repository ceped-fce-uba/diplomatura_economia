library(tidyverse)
library(ggthemes)

options(scipen = 999)

base <- read_csv("empleo_industrial.csv")

base <- base %>% 
  select(Anio, País, Regiones.economicas, Ocup_TOTAL_ECONOMIA, Ocup_INDUSTRIA) %>% 
  
  filter(Anio >= 1991,
         Anio <= 2018,
         Anio != 1999    # faltan datos de china para el 99'
  ) %>% 

  mutate(pp_empleo_industrial = Ocup_INDUSTRIA / Ocup_TOTAL_ECONOMIA)

datos_filtrados <- base %>%
  filter(País %in% c("DEU", "ARG", "CHN", "MEX")) %>%
  
  arrange(País, Anio) # Se ordena para que no haya problemas con lineas que van y vienen

base_1991 <- datos_filtrados %>% 
  group_by(País) %>% 
  mutate(base1991 = pp_empleo_industrial[Anio==1991],
         indice = (pp_empleo_industrial / base1991) * 100 ) %>% 
  ungroup()

base_1991  %>% 
  ggplot(aes(x = Anio, y = indice, color = País, group = País)) +
  
  geom_line(linewidth = 1.2, alpha = 0.9) +

  geom_point(size = 2.5, alpha = 0.7) +
  
  scale_x_continuous(breaks = seq(from = 1991, to = 2018, by = 1)) +

  labs(
    title = "Índices de participación del empleo industrial (1991 = 100)",
    subtitle = "Alemania, Argentina, China y México (1991-2018)",
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
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
