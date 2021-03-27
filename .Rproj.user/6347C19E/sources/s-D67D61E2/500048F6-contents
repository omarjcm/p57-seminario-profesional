
# Expresiones -------------------------------------------------------------
5 + 10
2 -30


# Funciones matemáticas -------------------------------------------------
pi
pi^2


# Secuencias, repeticiones  --------------------------------------------------------------

#Secuencias
1:100
seq(from=0, to=1000, by=100)
seq(from=5, by=5, length.out=100)

# Repeticiones
rep(x=3, times=1000)

# Datos generados con distribución normal
lista <- rnorm(n=10, mean = 100, sd=10)
# Datos generados con distribución uniforme
runif(n=10, min = 10, max = 100)
rnbinom(n=10, prob = 0.5, size = 10)


# Ambiente de Trabajo -----------------------------------------------------

ls()
objects()
ls( envir = globalenv() )
save.image(file = "01_fundamentos.RData")
load(file = "01_fundamentos01.RData")

savehistory(file = "01_fundamentos.Rhistory")


# Manejo de Paquetes ------------------------------------------------------

install.packages('tidyverse')
library(tidyverse)

# Tipos de datos ----------------------------------------------------------
a <- 23; b <- 10; c <- 20; d <- 30;


# Estructura de Datos -----------------------------------------------------

# Vectores
x <- 1
vectores <- c(1:100)
vectores <- c(rnorm(n=10, mean = 100, sd=10))

a <- c('a', 'b', 'c')
b <- c(a, 10, 20, a)

rep(a, times=10)
rep(a, each=10)

x <- c(10, 20, 30, 40)
y <- c(10, 20, 30, 40)
x + 3 * y - 1

# Dataframes
nombre <- c('Guillermo', 'Karina', 'Catalina', 'Sofía')
apellidos <- c('Pizarro', 'Ascencio', 'Pizarro', 'Pizarro')
ciudad <- factor(c('GYE', 'CUE', 'UIO', 'UIO'))
df_1 <- data.frame(nombre, apellidos, ciudad)
df_2 <- data.frame(a=nombre, b=apellidos, c=ciudad)
df_3 <- data.frame(
  nombre = c('Guillermo', 'Karina', 'Catalina', 'Sofía'),
  apellidos = c('Pizarro', 'Ascencio', 'Pizarro', 'Pizarro'),
  ciudad = factor(c('GYE', 'CUE', 'UIO', 'UIO'))
)

rownames(df_3) <- paste('id_', 1:4, sep = '')

names(df_3) <- c('Nombre(s)', 'Apellido(s)', 'Ciudad')

head(df_3, n=2)
tail(df_3, n=1)

str(df_3)

# Listas
list(1, c(2, 3), df_1)
lista_1 <- list(A = 1, B=c(2, 3), C=df_1)
lista_1$C

# Fechas
fecha <- lubridate::ymd("2021-03-23")

# Fechas
data_serie <- ts(1:24, start = 2014)
data_serie

# Factores
data_factor <- factor(x=c('alto', 'bajo', 'alto', 'alto', 'alto', 'alto'), levels = c('alto', 'mediano', 'bajo'))
data_factor



# Estructuras de Control --------------------------------------------------


# Funciones ---------------------------------------------------------------


