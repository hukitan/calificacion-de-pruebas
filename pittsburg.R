#Librerias 
library(tidyverse)
library(readr)

#carga de datos
pittsburg <- read_csv("data/pittsburg.csv")


#zona de cosas que si funcionan pero aun no se implementan 
hms(pittsburg[14,6])
shell()

abs(as.integer(pittsburg[14,6] - pittsburg[14,8])/3600)

if (pittsburg[10,10] == "Menos de una vez a la semana") {
    print("pan")
} 

#resultados (?)
res <- tibble()
