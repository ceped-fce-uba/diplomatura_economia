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
    )
  ))

write_csv(base, "Mod 3 - Econ Internacional/elaboracion_materiales/clase1/bases/empleo_industrial.csv")

# Van dos flujos de group_by %>% ungroup() separados para mayor legibilidad:

participacion_por_region <- base %>%
  group_by(Anio, Regiones.economicas) %>% 
  summarize(
    empleo_total_region = sum(Ocup_TOTAL_ECONOMIA, na.rm = TRUE),
    empleo_industria_region = sum(Ocup_INDUSTRIA, na.rm = TRUE)
  ) %>% 
  ungroup()

participacion_por_region <- participacion_por_region %>% 
  group_by(Anio) %>% 
  mutate(empleo_total_muestra = sum(empleo_total_region),
         pp_empleo_industrial_region = empleo_industria_region / empleo_total_muestra * 100
         ) %>%
  ungroup()

graf_empleo_ind <- participacion_por_region %>% 
  ggplot(aes(x = Anio, y = pp_empleo_industrial_region, fill = Regiones.economicas)) +
  geom_col(position = "stack") +
  scale_x_continuous(breaks = 1978:2018) +
  scale_y_continuous(breaks = seq(0, 30, by = 5),
                     labels = scales::percent_format(scale = 1)) +
  labs(
    title = "Empleo Industrial por Región (1978-2018)",
    subtitle = "Participación del empleo industrial en el empleo total de la muestra",
    x = "",
    y = "",
    caption = "Datos: blabla"
  ) +
  theme_classic() +
  scale_fill_tableau() +
  theme(
    legend.position = "bottom",
    plot.title.position = "plot",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

graf_empleo_ind %>% plotly::ggplotly()
