library(tidyverse)
library(readxl)

base_sipa <- read_csv("bases/base_sipa.csv")

base_ipc <- read_xlsx("bases/ipc_ceped_data.xlsx")

head(base_sipa)

tail(base_ipc)

asalariados <- 