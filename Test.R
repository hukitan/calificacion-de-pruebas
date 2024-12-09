# Librerias
library(tidyverse)
library(gsheet)
# library(readr) #si hay que leer del csv, se comenta ya que no se usa


# carga de datos
# pittsburg <- read_csv("data/pittsburg.csv") #con csv descargado
pittsburg <- gsheet2tbl("docs.google.com/spreadsheets/d/1uwcQvyIvxhIO0tNeykoPK8f1p7v7b9DIlUNf-1kozYc")

# zona de cosas que si funcionan pero aun no se implementan
hms(pittsburg[14, 6])
shell()
abs(as.integer(pittsburg[14, 6] - pittsburg[14, 8]) / 3600) # en caso de que haya que hacer restas de horas dormidas

# base para los condicionales para convertir a puntajes
if (pittsburg[10, 10] == "Ninguna vez en el último mes") {
    print(0)
} else if (pittsburg[10, 10] == "Menos de una vez a la semana") {
    print(1)
} else if (pittsburg[10, 10] == "Una o dos veces a la semana") {
    print(2)
} else if (pittsburg[10, 10] == "Tres o más veces a la semana") {
    print(3)
} else {
    print("Valor no reconocido")
}



# resultados (?)
tbl_colnames <- c("holi", "uwu", "pao", "mermelada")
res <- tbl_colnames %>% purrr::map_dfc(setNames, object = list(logical())) # hace una tible vacia con nombres en tbl_colnames y la nombra res
t <- (0)

# se puebla la tabla res generada
for (p in 1:nrow(pittsburg)) {
    res[p, 2] <- pittsburg[p, 10]
}


# Se hacen las conversiones de putnaje
for (p in 1:nrow(pittsburg)) {
    if (pittsburg[p, 10] == "Ninguna vez en el último mes") {
        t[p] <- 0
    } else if (pittsburg[p, 10] == "Menos de una vez a la semana") {
        t[p] <- 1
    } else if (pittsburg[p, 10] == "Una o dos veces a la semana") {
        t[p] <- 2
    } else if (pittsburg[p, 10] == "Tres o más veces a la semana") {
        t[p] <- 3
    } else {
        t[p] <- 9
    }
    rm(p)
}


# no sirve pero si
populate <- function(data, col_o, col_d) {
    for (p in 1:nrow(data)) {
        proc_fct[p, col_d] <- data[p, col_o]
    }
}


# hroas en cama
for (p in 1:nrow(pittsburg)) {
    if (pittsburg[p, 6] > hms("16:00:00")) {
        print(abs(as.integer((pittsburg[p, 6] - ddays(1)) - pittsburg[p, 8]) / 3600))
    } else {
        print(abs(as.integer(pittsburg[p, 6] - pittsburg[p, 8]) / 3600))
    }
}
rm(p)
