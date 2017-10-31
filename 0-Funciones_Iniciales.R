################ Lectura de Datos ################ 

library(tidyverse)
library(readxl)

read_and_check <- function() {
  datos <- read_excel(path = "00 Prueba Data Scientist.xlsx")
  missing <- sum(is.na(datos))
  repeated <- nrow(datos) - nrow(unique(datos))
  cat("El número de observaciones son: ", nrow(datos), "\n")
  cat("El número de columnas es: ", ncol(datos), "\n")
  cat("El numero de datos missing es:  ", missing, "\n")
  cat("El numero de observaciones repetidas son:  ", repeated, "\n")
  datos <- datos %>% mutate(rio = factor(rio))
}


