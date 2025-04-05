library(rvest)    # Para análisis de HTML
library(dplyr)    # Para manipulación de datos
library(lubridate) # Para manipulación de fechas

# Función para analizar paquetes CRAN desde una URL
procesam_paquetes_cran <- function(url) {
  # Leer el contenido HTML directamente desde la URL
  html_content <- read_html(url)
  
  # Extraer la tabla del HTML
  packages_table <- html_content %>%
    html_element("table") %>%
    html_table()
  
  # Limpiar nombres de columnas (eliminar espacios al inicio/final)
  names(packages_table) <- trimws(names(packages_table))
  
  # Extraer nombres de paquetes desde los enlaces HTML
  package_names <- html_content %>%
    html_elements("table tr td:nth-child(2) a span") %>%
    html_text()
  
  # Reemplazar los nombres de paquetes en la tabla
  if (length(package_names) > 0) {
    for (i in 1:min(nrow(packages_table), length(package_names))) {
      packages_table$Package[i] <- package_names[i]
    }
  }
  
  # Convertir la columna de fecha a formato Date adecuado
  packages_table$Date <- as.Date(packages_table$Date)
  
  return(packages_table)
}

# URL de la página de paquetes CRAN
url_cran <- "https://cran.r-project.org/web/packages/available_packages_by_date.html"

# Obtener y procesar los datos
paquetes_cran <- procesam_paquetes_cran(url_cran)

# Calcular el crecimiento acumulado de paquetes
growth_data <- paquetes_cran %>%
  arrange(Date) %>%
  group_by(Date) %>%
  summarize(Packages = n()) %>%
  mutate(paq_acum = cumsum(Packages))

# Crear un rango completo de fechas para rellenar los días faltantes
date_range <- seq(min(growth_data$Date), max(growth_data$Date), by = "day")
data_completa <- data.frame(Date = date_range) %>%
  left_join(growth_data, by = "Date") %>%
  mutate(Packages = ifelse(is.na(Packages), 0, Packages)) %>%
  mutate(paq_acum = cumsum(Packages))

# Si se quisieran guardar los datos procesados
# write.csv(data_completa, "cran_acumulados.csv", row.names = FALSE)

# Función para generar la visualización con diferentes temas
crear_plot_cran <- function(theme_type = "light") {
  # Definir colores basados en el tema
  if(theme_type == "light") {
    # Colores para tema claro
    bg_color <- "white"
    text_color <- "#333333"
    line_color <- "#1A476F"
    gradient_fill1 <- "#4D8DC9"  # Azul R más claro
    gradient_fill2 <- "#2165B6"  # Azul del logo de R
    title <- "Crecimiento de los paquetes de R en CRAN (acumulados)"
    filename <- "Mod 2 - Intro a programacion/elaboracion_materiales/clase1/misc/paquetes_cran_acum_light.png"
  } else {
    # Colores para tema oscuro
    bg_color <- "#1A1A1A"
    text_color <- "#E0E0E0"
    line_color <- "#70A8FF"
    gradient_fill1 <- "#4D8DC9"  # Azul R más claro
    gradient_fill2 <- "#1A3C78"  # Azul más oscuro para contraste con fondo oscuro
    title <- "Crecimiento de los paquetes de R en CRAN (acumulados)"
    filename <- "Mod 2 - Intro a programacion/elaboracion_materiales/clase1/misc/paquetes_cran_acum_dark.png"
  }
  
  # Obtener años presentes en los datos
  años_datos <- unique(format(complete_data$Date, "%Y"))
  
  # Crear fechas para el primer día de cada año como breaks
  breaks_años <- as.Date(paste0(años_datos, "-01-01"))
  
  # Crear gráfico base
  p <- ggplot(complete_data, aes(x = Date, y = CumulativePackages)) + 
    # Área con patrón de gradiente
    geom_area_pattern(
      pattern = "gradient", 
      fill = "#00000000", # Relleno base transparente
      pattern_fill = gradient_fill1,
      pattern_fill2 = gradient_fill2,
      alpha = 0.9
    ) + 
    # Línea refinada - ligeramente más gruesa para compensar la ausencia de puntos
    geom_line(colour = line_color, linewidth = 0.8) +
    # Configurar escala del eje Y para mostrar ticks cada 2000 unidades
    scale_y_continuous(breaks = seq(0, max(complete_data$CumulativePackages, na.rm = TRUE), by = 2000)) +
    # Configurar escala del eje X con breaks explícitos para cada año en los datos
    scale_x_date(
      breaks = breaks_años,
      date_labels = "%Y",
      expand = c(0.01, 0)  # Mínima expansión a la izquierda, ninguna a la derecha
    ) +
    # Título y etiquetas
    ggtitle(title) + 
    labs(x = "Fecha", y = "Paquetes acumulados") +
    # Personalización del tema
    theme(
      # Colores de fondo
      plot.background = element_rect(fill = bg_color, color = NA),
      panel.background = element_rect(fill = bg_color, color = NA),
      panel.grid.major = element_line(color = ifelse(theme_type == "light", "#ECECEC", "#333333"), linewidth = 0.2),
      panel.grid.minor = element_line(color = ifelse(theme_type == "light", "#F8F8F8", "#2A2A2A"), linewidth = 0.1),
      # Eliminar bordes
      panel.border = element_blank(),
      # Añadir líneas de eje sutiles
      axis.line = element_line(color = ifelse(theme_type == "light", "#999999", "#666666"), linewidth = 0.3),
      # Formato de texto
      plot.title = element_text(size = 16, face = "bold", color = text_color, 
                                margin = margin(b = 15)),
      axis.title = element_text(size = 12, color = text_color, 
                                margin = margin(t = 10, b = 10)),
      # Texto del eje Y normal
      axis.text.y = element_text(size = 10, color = text_color),
      # Texto del eje X en diagonal
      axis.text.x = element_text(angle = 45, hjust = 1, size = 10, color = text_color),
      # Eliminar marcas de graduación
      axis.ticks = element_blank(),
      # Añadir algo de relleno
      plot.margin = margin(15, 15, 15, 15)
    )
  
  # Guardar el gráfico
  dir.create(dirname(filename), recursive = TRUE, showWarnings = FALSE)
  ggsave(filename, p, width = 10, height = 6, dpi = 300)
  
  return(p)
}

# Crear y guardar ambas versiones
dark_plot <- crear_plot_cran("dark")
light_plot <- crear_plot_cran("light")