library(tidyverse)
library(readxl)

SIPA <- read_csv("bases/base_sipa.csv", show_col_types = FALSE)
ipc_mensual <- read_xlsx("bases/ipc_ceped_data.xlsx")

SIPA <- SIPA %>% 
  mutate(Periodo = as.Date(Periodo),
         Anio = year(Periodo),
         Mes = month(Periodo),
         Trimestre = quarter(Periodo))

ipc_mensual <- ipc_mensual %>% 
  mutate(fecha = as.Date(fecha))

remuneracion_media <- SIPA %>% 
  filter(Variable == "Remuneración promedio - sin estacionalidad") %>% 
  mutate(indice_remuneraciones = Valor/Valor[Periodo == "2009-01-01"]*100)

ipc_mensual <- ipc_mensual %>% 
  mutate(indice_ipc_2009 = valor/valor[fecha == "2009-01-01"]*100)

remuneracion_real <- remuneracion_media %>%
  left_join(ipc_mensual, by = c("Periodo" = "fecha"))

remuneracion_real <- remuneracion_real %>% 
  mutate(indice_real = indice_remuneraciones/indice_ipc_2009*100,
         Trimestre        = quarter(Periodo)) %>% 
  select(Periodo, Anio = ANO4, Trimestre, Mes = sub, indice_real) 

graf_remun <- remuneracion_real %>% 
  ggplot(aes(x = Periodo, y = indice_real)) +
  geom_line(color = "#104E8B", size = 1) +
  theme_classic() +
  labs(title = "Remuneración Real Asal. Reg. del Sec. Privado",
       subtitle = "Total País. Sin estacionalidad. (ene-2009 = 100)",
       y = "",
       x = "",
       caption = "Fuente: Sistema Integrado Previsional Argentino (SIPA)")

graf_remun

library(ggthemes)

graf_remun <- graf_remun +
  theme_fivethirtyeight() 

graf_remun

graf_remun <- graf_remun +
  
  geom_hline(yintercept = 100, linetype = "dashed", color = "gray50", size = 0.8) +
  
  geom_vline(xintercept = as.Date("2020-03-20"), linetype = "dotted", color = "red", size = 0.7)

graf_remun

graf_remun <- graf_remun +
  annotate("text", x = as.Date("2020-03-20"), y = max(remuneracion_real$indice_real, na.rm = TRUE), 
         label = "Inicio Pandemia", hjust = -0.1, vjust = 7, size = 3)

graf_remun

graf_remun <- graf_remun +
scale_x_date(date_breaks = "1 year", date_labels = "%Y")

graf_remun

graf_remun <- graf_remun +
  theme(axis.text.x = element_text(angle = 45,
                                  hjust = 1)
        )
  
graf_remun

graf_remun <- graf_remun +
  
annotate("rect", 
       xmin = as.Date("2015-09-01"), xmax = as.Date("2020-02-01"),
       ymin = -Inf, ymax = Inf, 
       fill = "#8B3A3A", alpha = 0.2) +
  
annotate("text", x = as.Date("2017-12-10"), y = max(remuneracion_real$indice_real, na.rm = TRUE), 
         label = "Retroceso previo a la pandemia", vjust = 0, size = 3)

graf_remun

library(gapminder)  
library(ggplot2)    

gapminder_2007 <- gapminder %>% filter(year == 2007)

ggplot(gapminder_2007, aes(x = gdpPercap, y = lifeExp)) +
  geom_point() +  
  labs(
    title = "Esperanza de vida vs. PBI per Capita (2007)",
    x = "PBI per Capita (en escala logarítmica)",
    y = "Esperanza de vida (en años)"
  ) +
  scale_x_log10() +  # Escala logarítmica para el PBI
  theme_classic()

ggplot(gapminder_2007, aes(x = gdpPercap, y = lifeExp, 
                          size = pop, color = continent)) +
  geom_point(alpha = 0.7) +  # Puntos con transparencia, clave por la superposición
  scale_x_log10() +  # Escala logarítmica para GDP per capita
  labs(
    title = "Esperanza de vida vs. PBI per Capita (2007)",
    x = "PBI per Capita (en escala logarítmica)",
    y = "Esperanza de vida (en años)",
    color = "Continente"
  ) +
  theme_classic()


# Leer los datos
expo_mineras <- read_csv("bases/expo_mineras_por_grupo.csv")


expo_mineras <- expo_mineras %>% 
  group_by(grupo_nuevo) %>%
  mutate(total = sum(expo_grupo)) %>%
  ungroup() %>%
  mutate(grupo_nuevo = fct_reorder(grupo_nuevo, total, .desc = TRUE))

ggplot(expo_mineras, aes(x = anio, y = expo_grupo, fill = grupo_nuevo)) +
  # bordes finos para distinguir los grupos
  geom_area(color = "white", size = 0.1, alpha = 0.85) +
  
  # paleta de colores
  scale_fill_tableau() +
  
    scale_y_continuous(
    labels = function(x) paste0("$", x / 1000000, "M"),
    expand = c(0, 0)
  ) +

  scale_x_continuous(
    breaks = seq(1994, 2022, by = 4),
    expand = c(0, 0)
  ) +

  labs(
    title = "Exportaciones mineras argentinas por grupo de minerales (1994-2022)",
    x = "Año",
    y = "Miles de millones USD",
    fill = "Mineral"
  ) -> graf_expo_mine_area # Esta forma de asignar puede que también se la encuentren

graf_expo_mine_area

library(plotly)
ggplotly(graf_expo_mine_area)

ggplot(expo_mineras, aes(x = anio, y = expo_grupo, fill = grupo_nuevo)) +
  # Usamos geom_col() en lugar de geom_area() para crear barras apiladas
  geom_col(color = "white", size = 0.1, alpha = 0.85) +
  
  # Mantenemos la misma paleta de colores
  scale_fill_tableau() +
  
  scale_y_continuous(
    labels = function(x) paste0("$", x / 1000000, "M"),
    expand = c(0, 0)
  ) +
  scale_x_continuous(
    breaks = seq(1994, 2022, by = 4),
    expand = c(0, 0)
  ) +
  labs(
    title = "Exportaciones mineras argentinas por grupo de minerales (1994-2022)",
    x = "Año",
    y = "Miles de millones USD",
    fill = "Mineral"
  ) -> graf_expo_mine_barras

graf_expo_mine_barras

ggplotly(graf_expo_mine_barras)

library(htmlwidgets)

mine_interac_area <- ggplotly(graf_expo_mine_barras)

htmlwidgets::saveWidget(mine_interac_area, "exportaciones_mineras.html")

eph <- read.table("bases/usu_individual_T424.txt",
                  header = TRUE,
                  sep = ";",
                  dec = ".")

eph <- eph %>% 
  mutate(MAS_500 = case_when(MAS_500 == "N" ~ "Menos de 500 mil",
                             MAS_500 == "S" ~ "500 mil y más"),
         sexo    = case_when(CH04 == 1 ~ "Varón",
                             CH04 == 2 ~ "Mujer"),
         NIVEL_ED = as.factor(NIVEL_ED))


graf <- eph %>% 
  filter(P47T > 0,
         P21 > 0) 

perc_99 <- quantile(eph$P47T, probs = c(99/100), na.rm = TRUE)

ggplot(graf, aes(x = P47T, fill = MAS_500, weight = PONDII)) +
  geom_histogram(bins = 100) +
  scale_fill_brewer(palette = 9) +
  scale_x_continuous(limits = c(0, perc_99)) +
  theme_minimal() +
  labs(title= "Ingreso Total Individual según tamaño de aglomerado",
       subtitle = "4to trimestre 2024",
       caption = "Fuente: Encuesta Permanente de Hogares",
       x = "Ingreso Total Individual",
       y = "Casos",
       fill = "Aglomerado"
       )
  

ggplot(graf, aes(x = as.integer(NIVEL_ED), weight = PONDERA)) +
  geom_histogram(bins = 30, fill = "steelblue4") +
  facet_wrap(~ as.factor(MAS_500)) +
  theme_classic() +
  labs(title= "Nivel Educativo según tamaño de aglomerado",
       subtitle = "4to trimestre 2024",
       caption = "Fuente: Encuesta Permanente de Hogares",
       x = "Nivel Educativo",
       y = "Casos"
       
       )

perc_99_p21 <-  quantile(eph$P21, probs = c(99/100), na.rm = TRUE)

ggplot(graf, aes(x= NIVEL_ED, y = P21, group = NIVEL_ED, fill = NIVEL_ED)) +
  geom_boxplot()+
  scale_fill_brewer(palette = 9) +
  scale_y_continuous(limits = c(0, perc_99_p21))+
  facet_wrap(~ sexo) +
  theme_minimal() + 
  labs(title= "Distribución Ingreso de la Ocupación Principal según sexo y nivel educativo",
       subtitle = "4to trimestre 2024",
       caption = "Fuente: Encuesta Permanente de Hogares",
       x = "",
       y = "",
       fill = "Nivel ed."
       ) 

trimestre <- "4to trimestre 2024"

cols <- c("#698B22", "#36648B")

ggplot(graf, aes(x = P21, fill = sexo, weight = PONDIIO)) +
  geom_density(alpha = 0.5, color = NA) +
  scale_fill_manual(values = cols) +
  theme_minimal() +
  scale_x_continuous(limits = c(0, perc_99_p21)) +
  labs(title= "Distribución Ingreso Ocupación Principal según sexo",
       subtitle = `trimestre`,
       caption = "Fuente: Encuesta Permanente de Hogares",
       x = "Ingreso Ocupación Principal",
       y = "",
       fill = "Sexo") + 
  theme(axis.text.y = element_blank())

ggplot(graf, aes(y = P21, x = sexo, fill = sexo)) +
  geom_boxplot() +
  scale_y_continuous(limits = c(0, perc_99_p21)) +
  scale_fill_manual(values = cols) +
  theme_minimal() +
  labs(title = "Distribución del Ingreso Ocupación Principal según sexo",
       subtitle = `trimestre`,
       y = "Ingreso Ocupación Principal",
       x = "",
       fill = "Sexo",
       caption = "Fuente: Encuesta Permanente de Hogares"
       ) 

ggplot(graf, aes(y = P21, x = sexo, fill = sexo, weight = PONDIIO)) +
  geom_violin() +
  scale_y_continuous(limits = c(0, perc_99_p21)) +
  scale_fill_manual(values = cols) +
  theme_minimal() +
   labs(title = "Distribución del Ingreso Ocupación Principal según sexo",
       subtitle = `trimestre`,
       y = "Ingreso Ocupación Principal",
       x = "",
       fill = "Sexo",
       caption = "Fuente: Encuesta Permanente de Hogares"
       ) 

ggplot(graf, aes(y = P21, x = sexo, fill = sexo)) +
  geom_violin() +
  scale_y_continuous(limits = c(0, perc_99_p21)) +
  scale_fill_manual(values = cols) +
  theme_minimal() +
  labs(title = "Distribución del Ingreso Ocupación Principal según sexo",
       subtitle = `trimestre`,
       y = "Ingreso Ocupación Principal",
       x = "",
       fill = "Sexo",
       caption = "Fuente: Encuesta Permanente de Hogares"
       ) +
  geom_boxplot(width = 0.2)
