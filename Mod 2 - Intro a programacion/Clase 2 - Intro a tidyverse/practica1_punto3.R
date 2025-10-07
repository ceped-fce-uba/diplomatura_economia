library(readxl)
library(tidyverse)

base <- read_csv("bases/gapminder_2000.csv")

head(base, n = 10)
tail(base, n = 10)

names(base)

base$Country

mean(base$life)

min(base$population)

base$life %>%
  mean() %>% 
  func2() %>% 
  func3()
