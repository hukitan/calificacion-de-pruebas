#Librerias 
library(tidyverse)

source('get_Data.R', chdir = TRUE)


tbl_colnames <- c("id","sexo", "edad", "1","2","3","4", "5a", "5b","5c","5d","5e", "5f","5g","5h","5i", "5j","6","7","8","9" )
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
    proc_fct[p,5] <- pittsburg[p,7]
}

#hay que corregir unas cosas 3.
for (p in 1:nrow(pittsburg)) {
    proc_fct[p,6] <- pittsburg[p,8]
}

#hay que corregir unas cosas 4.
for (p in 1:nrow(pittsburg)) {
    proc_fct[p,7] <- pittsburg[p,9]
}

#5a-5j 
orig <- 10:18
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
            proc_fct[p,r] <- 9
        }
    }
}
rm(p,o,r,orig,dest)
