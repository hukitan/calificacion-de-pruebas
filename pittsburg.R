#Librerias 
library(tidyverse)
library(readr)

#carga de datos
pittsburg <- read_csv("data/pittsburg.csv")


#zona de cosas que si funcionan pero aun no se implementan 
hms(pittsburg[14,6])
shell("curl")