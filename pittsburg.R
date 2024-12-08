#Librerias 
library(tidyverse)

source('get_Data.R', chdir = TRUE)


tbl_colnames <- c("id","sexo", "edad", "1","2","3","4", "5a", "5b","5c","5d","5e", "5f","5g","5h","5i","Sum_aj","c5","7_c6","8","9","c7","6_c1", "c2","c3","c4","sumatoria_total")
proc_fct <- tbl_colnames %>% purrr::map_dfc(setNames, object = list(logical())) # hace una tible vacia con nombres en tbl_colnames y la nombra res
rm(tbl_colnames)

#id
for (p in 1:nrow(pittsburg)) {
    proc_fct[p,1] <- pittsburg[p,2]
}

#sexo
for (p in 1:nrow(pittsburg)) {
    proc_fct[p,2] <- pittsburg[p,3]
}

#edad
for (p in 1:nrow(pittsburg)) {
    proc_fct[p,3] <- pittsburg[p,4]
}

# 1.
for (p in 1:nrow(pittsburg)) {
    proc_fct[p,4] <- pittsburg[p,6]
}

# 2.
for (p in 1:nrow(pittsburg)) {
    if (pittsburg[p,7] <= 15) {
        proc_fct[p,5] <- 0
    } else if  (pittsburg[p,7] <= 30) {
        proc_fct[p,5] <- 1 
    }else if  (pittsburg[p,7] <= 60) {
        proc_fct[p,5] <- 2
    }else if  (pittsburg[p,7] > 60) {
        proc_fct[p,5] <- 3
    } else {
        proc_fct[p,5] <- 999
    }
    rm(p)
}

#3. hay que verificar que los datos si sean almacenados bien
for (p in 1:nrow(pittsburg)) {
    proc_fct[p,6] <- pittsburg[p,8]
}

# 4.
for (p in 1:nrow(pittsburg)) {
    proc_fct[p,7] <- pittsburg[p,9]
}

#5a-5j 
orig <- 10:18
for (p in 1:nrow(pittsburg)) {
    proc_fct[p,17] <- 0
    for (o in orig) {
        r <- o-2
        if (pittsburg[p,o] == "Ninguna vez en el último mes") {
            proc_fct[p,r] <- 0
        } else if  (pittsburg[p,o] == "Menos de una vez a la semana") {
            proc_fct[p,r] <- 1
        }else if  (pittsburg[p,o] == "Una o dos veces a la semana") {
            proc_fct[p,r] <- 2
        }else if  (pittsburg[p,o] == "Tres o más veces a la semana") {
            proc_fct[p,r] <- 3
        } else {
            proc_fct[p,r] <- 999
        }
        proc_fct[p,17] <- proc_fct[p,17] + proc_fct[p,r]
    }
}
rm(p,o,r,orig)

#caluclo del puntaje de 5
for (p in 1:nrow(proc_fct)) {
    if (proc_fct[p,17] == 0) {
        proc_fct[p,18] <- 0
    } else if  (proc_fct[p,17] <= 9) {
        proc_fct[p,18] <- 1
    }else if  (proc_fct[p,17] <= 18) {
        proc_fct[p,18] <- 2
    }else if  (proc_fct[p,17] <= 27) {
        proc_fct[p,18] <- 3
    } else {
        proc_fct[p,18] <- 999
    }
}
rm(p)


# 7-8
orig <- 21:22
for (p in 1:nrow(pittsburg)) {
    for (o in orig) {
        r <- o-2
        if (pittsburg[p,o] == "Ninguna vez en el último mes") {
            proc_fct[p,r] <- 0
        } else if  (pittsburg[p,o] == "Menos de una vez a la semana") {
            proc_fct[p,r] <- 1
        }else if  (pittsburg[p,o] == "Una o dos veces a la semana") {
            proc_fct[p,r] <- 2
        }else if  (pittsburg[p,o] == "Tres o más veces a la semana") {
            proc_fct[p,r] <- 3
        } else {
            proc_fct[p,r] <- 999
        }
    }
}
rm(p,o,r,orig)

#9
for (p in 1:nrow(pittsburg)) {
    if (pittsburg[p,23] == "Ningún problema") {
        proc_fct[p,21] <- 0
    } else if  (pittsburg[p,23] == "Un problema muy ligero") {
        proc_fct[p,21] <- 1 
    }else if  (pittsburg[p,23] == "Algo de problema") {
        proc_fct[p,21] <- 2
    }else if  (pittsburg[p,23] == "Un gran problema") {
        proc_fct[p,21] <- 3
    } else {
        proc_fct[p,21] <- 999
    }
    rm(p)
}

#componente 7
for (p in 1:nrow(pittsburg)) {
    sum<- proc_fct[p,21] + proc_fct[p,20]
    if (sum == 0 ) {
        proc_fct[p,22] <- 0
    }else if (sum <= 2) {
        proc_fct[p,22] <- 1
    }else if (sum <= 4) {
        proc_fct[p,22] <- 2
    }else if (sum <= 6) {
        proc_fct[p,22] <- 3
    }
    rm(sum)
}
rm(p)


#se hacen las calificaciones de los componentes restantes 
## componente 1
for (p in 1:nrow(pittsburg)) {
    if (pittsburg[p,20] == "Bastante buena") {
        proc_fct[p,23] <- 0
    } else if  (pittsburg[p,20] == "Buena") {
        proc_fct[p,23] <- 1 
    }else if  (pittsburg[p,20] == "Mala") {
        proc_fct[p,23] <- 2
    }else if  (pittsburg[p,20] == "Bastante mala") {
        proc_fct[p,23] <- 3
    } else {
        proc_fct[p,23] <- 999
    }
    rm(p)
}

#componente 2
for (p in 1:nrow(pittsburg)) {
    sum<- proc_fct[p,5] + proc_fct[p,8]
    if (sum == 0 ) {
        proc_fct[p,24] <- 0
    }else if (sum <= 2) {
        proc_fct[p,24] <- 1
    }else if (sum <= 4) {
        proc_fct[p,24] <- 2
    }else if (sum <= 6) {
        proc_fct[p,24] <- 3
    }
    rm(sum)
}
rm(p)

#componente 3 & 4
##c3
for (p in 1:nrow(pittsburg)) {
    if (pittsburg[p,9] <5) {
        proc_fct[p,25] <- 3
    } else if  (pittsburg[p,9] <= 6) {
        proc_fct[p,25] <- 2
    }else if  (pittsburg[p,9] <= 7) {
        proc_fct[p,25] <- 1
    }else if  (pittsburg[p,9] > 7 ) {
        proc_fct[p,25] <- 0
    } else {
        proc_fct[p,] <- 999
    }
    rm(p)
}

##c4
##hroas en cama
for (p in 1:nrow(pittsburg)) {
    if (pittsburg[p,6] > hms("16:00:00")) {
        hr_cm <- abs(as.integer((pittsburg[p,6] - ddays(1)) - pittsburg[p,8])/3600)
    } else {
        hr_cm <- abs(as.integer(pittsburg[p,6] - pittsburg[p,8])/3600)
    }
    efi <- (proc_fct[p,7]/hr_cm)*100
    
    if (efi >= 85) {
        proc_fct[p,26] <- 0
    } else if  (efi >= 75 ) {
        proc_fct[p,26] <- 1
    }else if  (efi >= 65) {
        proc_fct[p,26] <- 2
    }else if  (efi < 65 ) {
        proc_fct[p,26] <- 3
    } else {
        proc_fct[p,26] <- 999
    }
    rm(p,hr_cm,efi)
}



