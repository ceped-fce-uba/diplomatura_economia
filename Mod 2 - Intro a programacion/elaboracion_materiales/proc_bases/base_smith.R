library(readr)

fabrica_alfileres <- data.frame(
  organizacion_trabajo = c(
    "Trabajador no especializado",
    "Trabajador especializado",
    "División del trabajo (10 trabajadores)",
    "Trabajador no especializado",
    "Trabajador especializado",
    "División del trabajo (10 trabajadores)"
  ),
  tareas = c(
    "Todas las tareas, sin conocimiento previo del proceso y de la utilización de los medios de producción",
    "Todas las tareas, con conocimiento previo del proceso y de la utilización de los medios de producción",
    "Realización de tareas específicas por parte de cada trabajador/a (estirado del alambre, cortado, afilado, limado, empaquetado, etc.)",
    "Todas las tareas, sin conocimiento previo del proceso y de la utilización de los medios de producción",
    "Todas las tareas, con conocimiento previo del proceso y de la utilización de los medios de producción",
    "Realización de tareas específicas por parte de cada trabajador/a (estirado del alambre, cortado, afilado, limado, empaquetado, etc.)"
  ),
  dias_trabajo = c(1, 1, 1, 2, 2, 2),
  trabajadores = c(1, 1, 10, 1, 1, 10),
  alfileres_producidos = c(1, 20, 48000, 1 * 2, 20 * 2, 48000 * 2)
)

write_csv(fabrica_alfileres, file = "Mod 2 - Intro a programacion/Clase 2 - Intro a tidyverse/bases/adam_smith.csv")