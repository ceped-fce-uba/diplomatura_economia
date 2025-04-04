library(rvest)
library(dplyr)

parse_cran_packages <- function(file_path) {
  html_content <- read_html(file_path)

  packages_table <- html_content %>%
    html_element("table") %>%
    html_table()
  
  names(packages_table) <- trimws(names(packages_table))
  
  package_names <- html_content %>%
    html_elements("table tr td:nth-child(2) a span") %>%
    html_text()
  
  if (length(package_names) > 0) {
    for (i in 1:min(nrow(packages_table), length(package_names))) {
      packages_table$Package[i] <- package_names[i]
    }
  }
  
  packages_table$Date <- as.Date(packages_table$Date)
  
  return(packages_table)
}

cran_packages <- parse_cran_packages("Mod 2 - Intro a programacion/elaboracion_materiales/clase1/misc/cran_paquetes.html")

head(cran_packages)

write.csv(cran_packages, "Mod 2 - Intro a programacion/elaboracion_materiales/clase1/misc/cran_paquetes.csv", row.names = FALSE)


# Cuando tengamos grafico
# ggsave("Mod 2 - Intro a programacion/elaboracion_materiales/clase1/misc/cran_packages_growth.png", p, width = 10, height = 6, dpi = 300)
