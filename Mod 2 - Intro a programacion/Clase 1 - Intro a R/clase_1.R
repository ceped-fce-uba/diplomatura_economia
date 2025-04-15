
Y <- 1 # Creamos una variable llamada Y cuyo contenido será el número 1

Y

X = 2 # Funciona a la perfección pero no es convencional.

X

Y <- X # Sobreescribimos el contenido de Y con el contenido de X

Y

Y -> X

X # En este caso X fue redefinida con el contenido de Y

A <- 2 + 2 # Suma
A

B <- 10 - 5 # Resta
B

C <- 15/3 # División
C

D <- 4*3 # Multiplicación
D

A <- 1 # Asignamos un número cualquiera
class(A)
typeof(A)

A <- 1.5 # Un ejemplo con decimales
class(A)
typeof(A)

A <- 10L  # La L indica que queremos un entero
class(A)
typeof(A)

A <- "osea, digamos" # Asignamos texto
class(A)

verdadero <- TRUE # Verdadero en inglés, todo en mayúsculas
falso <- FALSE    # Falso en inglés, todo en mayúsculas
class(verdadero)

A <- as.Date("2017-01-01")
class(A)

A <- factor("Alto", "Medio", "Bajo") 
class(A)

Y <- 1
X <- 2

Y > X # Y es mayor que X

Y >= X # Y es mayor or igual que X

Y == X # Y es igual a X

Y != X # Y es diferente de X

Z <- Y != X
Z

C <- c(1, 3, 4)
C

C <- C + 1
C

D <- C + 1:3 #esto es equivalente a hacer 3+1, 5+2, 6+9 
D

E <- c("Ingreso", "Variación", "Indice")

elemento2 <- E[2] # Accedemos al segundo elemento del vector para asignarlo
elemento2

E[2] <- "var" # Sobreescribimos el segundo elemento del vector

INDICE  <- c(100,   100,   100,
             101.8, 101.2, 100.73,
             102.9, 102.4, 103.2)
FECHA  <-  c("Oct-16", "Oct-16", "Oct-16",
             "Nov-16", "Nov-16", "Nov-16",
             "Dic-16", "Dic-16", "Dic-16")
SECTOR  <-  c("Privado_Registrado","Público","Privado_No_Registrado",
             "Privado_Registrado","Público","Privado_No_Registrado",
             "Privado_Registrado","Público","Privado_No_Registrado")
             
Datos <- data.frame(INDICE, FECHA, SECTOR)
Datos

Datos$FECHA

Datos[3,2]

try({
Datos$FECHA[3,2]
})

lista1 <- list(1, 2, "tres")
lista1

# library(readr)
# 
# SIPA <- read_csv("bases/base_sipa.csv")
# names(SIPA)




head(SIPA)

# install.packages("readxl")

# library(readxl)
# 
# ipc_mensual <- read_xlsx("bases/ipc_ceped_data.xlsx")
# names(ipc_mensual)



head(ipc_mensual)

# eph_T324 <- read_delim(file = "bases/usu_individual_T324.txt",
#                   delim = ";",
#                   col_names = TRUE)



head(eph_T324)
