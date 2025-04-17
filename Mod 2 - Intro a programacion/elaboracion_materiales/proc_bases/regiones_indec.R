library("readr")

provincias_argentina <- data.frame(
  Geografia = c(
    "CABA",
    "BUENOS AIRES",
    "CATAMARCA",
    "JUJUY",
    "LA RIOJA",
    "SALTA",
    "SANTIAGO DEL ESTERO",
    "TUCUMAN",
    "CORRIENTES",
    "CHACO",
    "FORMOSA",
    "MISIONES",
    "MENDOZA",
    "SAN JUAN",
    "SAN LUIS",
    "CORDOBA",
    "ENTRE RIOS",
    "LA PAMPA",
    "SANTA FE",
    "RIO NEGRO",
    "NEUQUEN",
    "CHUBUT",
    "SANTA CRUZ",
    "TIERRA DEL FUEGO"
  ),
  Region = c(
    "Gran Buenos Aires",
    "Gran Buenos Aires",
    "NOA",
    "NOA",
    "NOA",
    "NOA",
    "NOA",
    "NOA",
    "NEA",
    "NEA",
    "NEA",
    "NEA",
    "Cuyo",
    "Cuyo",
    "Cuyo",
    "Pampeana",
    "Pampeana",
    "Pampeana",
    "Pampeana",
    "Patagonia",
    "Patagonia",
    "Patagonia",
    "Patagonia",
    "Patagonia"
  )
)

write_csv(provincias_argentina, "Mod 2 - Intro a programacion/Clase 2 - Intro a tidyverse/bases/regiones_arg.csv")