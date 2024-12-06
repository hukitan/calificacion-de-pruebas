#Librerias 
library(tidyverse)
library(readr)

#carga de datos
pittsburg <- read_csv("data/pittsburg.csv")


#zona de cosas que si funcionan pero aun no se implementan 
hms(pittsburg[14,6])
shell()

abs(as.integer(pittsburg[14,6] - pittsburg[14,8])/3600) #en caso de que haya que hacer restas de horas dormidas

#base para los condicionales para convertir a puntajes
if (pittsburg[10,10] == "Ninguna vez en el último mes") {
    print(0)
} else if  (pittsburg[10,10] == "Menos de una vez a la semana") {
    print(1)
}else if  (pittsburg[10,10] == "Una o dos veces a la semana") {
    print(2)
}else if  (pittsburg[10,10] == "Tres o más veces a la semana") {
    print(3)
} else {
    print("Valor no reconocido")
}

tbl_colnames <- c("holi","uwu", "pao", "mermelada")

#resultados (?)
res <- tbl_colnames %>% purrr::map_dfc(setNames, object = list(logical())) # hace una tible vacia con nombres en tbl_colnames y la nombra res
t <- (0) # Creamos un vector vacio para probar las iteraciones


for (p in 1:nrow(pittsburg)) {
    res[p,2] <- pittsburg[p,10]
}


for (p in 0:nrow(pittsburg)) {
    t[p] <- pittsburg[p,10]
}



for (p in 1:nrow(pittsburg)) {
    if (pittsburg[p,10] == "Ninguna vez en el último mes") {
        t[p] <- 0
    } else if  (pittsburg[p,10] == "Menos de una vez a la semana") {
        t[p] <- 1
    }else if  (pittsburg[p,10] == "Una o dos veces a la semana") {
        t[p] <- 2
    }else if  (pittsburg[p,10] == "Tres o más veces a la semana") {
        t[p] <- 3
    } else {
        t[p] <- 9
    }
}

