library(tidyverse)
library(readxl)
library(ggthemes)

options(scipen = 999)

base <- read_xlsx(path = "Mod 3 - Econ Internacional/elaboracion_materiales/clase1/bases/total_e_industria.xlsx",
                  sheet = 'Sheet 1',
                  range = 'A1:AU1571')

base <- base %>% 
  select(Anio, País, Regiones.economicas, PBIpm_pr_corr_TOTAL_ECONOMIA, PBIpm_pr_corr_INDUSTRIA) %>%
  
  filter(Anio >= 1978,
         Anio <= 2018,
         Anio != 1999    # faltan datos de china para el 99'
  )

base
