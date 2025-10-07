library(tidyverse)
library(eph)

# Estructuras Condicionales ####

edad <- 20

if (edad >= 18) {
  print("la persona es mayor de edad.")
}

# Si la condición es falsa, no se ejecuta nada:
edad <- 15
if (edad >= 18) {
  print("este mensaje no se va a imprimir.")
}

temperatura <- 15

if (temperatura > 25) {
  print("hace calor.")
} else {
  print("no hace calor.")
}

nota <- -9

if (nota >= 9) {
  print("sobresaliente")
} else if (nota >= 7) {
  print("muy bien")
} else if (nota >= 4) {
  print("aprueba")
} else {
  print("desaprueba")
}

# Loops o estructuras de control ####

## loop for ####

### ejemplo numérico ####

secuencia = seq(from = 4, to = 40, by = 2)

for (par in secuencia) {
  print(par * 2)
}

### ejemplo con cadenas de caracteres ####
ciudades <- c("Buenos Aires", "Córdoba", "Rosario", "Mendoza")

for (ciudad in ciudades) {
  print(ciudad)
}

### cálculos repetitivos ####
suma_total <- 0

for (numero in 1:10) {
  suma_total <- suma_total + numero
}

print(suma_total)

### loop for anidado (uno dentro de otro) ####
# Podemos poner un loop dentro de otro para crear combinaciones.
for (i in 1:3) {
  for (j in c("a", "b")) {
    print(paste(i, j))
  }
}

### loop for con una condición (if) ####
for (numero in 1:10) {
  if (numero %% 2 == 0) {
    print(paste(numero, "es un número par."))
  } else {
    print(paste(numero, "es un número impar."))
  }
}

### loop for con next ####

for (i in 1:10) {
  if (i %% 2 != 0) {
    next
  }
  print(i)
}


### loop for con break ####
for (ciudad in ciudades) {
  message(paste("Evaluando:", ciudad))
  if (startsWith(ciudad, "R")) {
    print(paste("ciudad encontrada:", ciudad))
    break
  }
}

## loop while ####

### primer ejemplo while ####

# contador <- 2
# 
# while (contador > 1) {
#   print(paste("numero:",contador))
#   contador <- contador + 1
# }

### descarga automatizada de bases con loop for ####
lista_bases <- list()

for (year in 2019:2021) {
  
  message(paste("descargando", year))
  
  data_anual <- eph::get_microdata(
    year = year,
    trimester = 1,
    type = "individual"
  )
  
  # Esto crea una variable con nombre base_20xx
  assign(paste0("base_", year), data_anual)
  
  # Se agrega la base a la lista instanciada para el loop
  lista_bases[[as.character(paste0("año_",year))]] <- data_anual
}


for (base in names(lista_bases)) {
  print(lista_bases[names(lista_bases)])
}

