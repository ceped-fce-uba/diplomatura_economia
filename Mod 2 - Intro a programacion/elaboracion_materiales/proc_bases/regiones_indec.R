library("readr")

provincias_argentina <- data.frame(
  provincia = c(
    "CABA",
    "BUENOS AIRES",
    "CATAMARCA",
    "JUJUY",
    "LA RIOJA",
    "SALTA",
    "SANTIAGO DEL ESTERO",
    "TUCUMÁN",
    "CORRIENTES",
    "CHACO",
    "FORMOSA",
    "MISIONES",
    "MENDOZA",
    "SAN JUAN",
    "SAN LUIS",
    "CÓRDOBA",
    "ENTRE RÍOS",
    "LA PAMPA",
    "SANTA FE",
    "RIO NEGRO",
    "NEUQUÉN",
    "CHUBUT",
    "SANTA CRUZ",
    "TIERRA DEL FUEGO"
  ),
  region = c(
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

write_csv(provincias_argentina, "../Clase 2 - Intro a tidyverse/bases/regiones_arg")