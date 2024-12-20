# Librerias
library(tidyverse)

source("get_Data.R", chdir = TRUE)

tbl_colnames <- c("id", "sexo", "edad", "1", "2", "3", "4", "5a", "5b", "5c", "5d", "5e", "5f", "5g", "5h", "5i", "Sum_aj", "c5", "7_c6", "8", "9", "c7", "6_c1", "c2", "c3", "c4", "sumatoria_total")
proc_fct <- tbl_colnames %>% purrr::map_dfc(setNames, object = list(logical())) # hace una tible vacia con nombres en tbl_colnames y la nombra res

# Lista de asignaciones
asignaciones <- list(
  c(1, 2), # id
  c(2, 3), # sexo
  c(3, 4), # edad
  c(4, 6), # 1
  c(6, 8), # 6
  c(7, 9)  # 7
)

# Asignar valores y realizar transformaciones en un solo bucle
for (p in 1:nrow(pittsburg)) {
  for (asignacion in asignaciones) {
    proc_fct[p, asignacion[1]] <- pittsburg[p, asignacion[2]]
  }
  
  # Asignación condicional para la columna 5
  proc_fct[p, 5] <- case_when(
    pittsburg[p, 7] <= 15 ~ 0,
    pittsburg[p, 7] <= 30 ~ 1,
    pittsburg[p, 7] <= 60 ~ 2,
    pittsburg[p, 7] > 60 ~ 3,
    TRUE ~ 999
  )
  
  # Asignación condicional para las columnas 5a-5j
  proc_fct[p, 17] <- 0
  for (o in 10:18) {
    r <- o - 2
    proc_fct[p, r] <- case_when(
      pittsburg[p, o] == "Ninguna vez en el último mes" ~ 0,
      pittsburg[p, o] == "Menos de una vez a la semana" ~ 1,
      pittsburg[p, o] == "Una o dos veces a la semana" ~ 2,
      pittsburg[p, o] == "Tres o más veces a la semana" ~ 3,
      TRUE ~ 999
    )
    proc_fct[p, 17] <- proc_fct[p, 17] + proc_fct[p, r]
  }
  
  # Calculo del puntaje de 5
  proc_fct[p, 18] <- case_when(
    proc_fct[p, 17] == 0 ~ 0,
    proc_fct[p, 17] <= 9 ~ 1,
    proc_fct[p, 17] <= 18 ~ 2,
    proc_fct[p, 17] <= 27 ~ 3,
    TRUE ~ 999
  )
  
  # Asignación condicional para las columnas 7-8
  for (o in 21:22) {
    r <- o - 2
    proc_fct[p, r] <- case_when(
      pittsburg[p, o] == "Ninguna vez en el último mes" ~ 0,
      pittsburg[p, o] == "Menos de una vez a la semana" ~ 1,
      pittsburg[p, o] == "Una o dos veces a la semana" ~ 2,
      pittsburg[p, o] == "Tres o más veces a la semana" ~ 3,
      TRUE ~ 999
    )
  }
  
  # Asignación condicional para la columna 9
  proc_fct[p, 21] <- case_when(
    pittsburg[p, 23] == "Ningún problema" ~ 0,
    pittsburg[p, 23] == "Un problema muy ligero" ~ 1,
    pittsburg[p, 23] == "Algo de problema" ~ 2,
    pittsburg[p, 23] == "Un gran problema" ~ 3,
    TRUE ~ 999
  )
  
  # Componente 7
  sum <- proc_fct[p, 21] + proc_fct[p, 20]
  proc_fct[p, 22] <- case_when(
    sum == 0 ~ 0,
    sum <= 2 ~ 1,
    sum <= 4 ~ 2,
    sum <= 6 ~ 3,
    TRUE ~ 999
  )
  
  # Componente 1
  proc_fct[p, 23] <- case_when(
    pittsburg[p, 20] == "Bastante buena" ~ 0,
    pittsburg[p, 20] == "Buena" ~ 1,
    pittsburg[p, 20] == "Mala" ~ 2,
    pittsburg[p, 20] == "Bastante mala" ~ 3,
    TRUE ~ 999
  )
  
  # Componente 2
  sum <- proc_fct[p, 5] + proc_fct[p, 8]
  proc_fct[p, 24] <- case_when(
    sum == 0 ~ 0,
    sum <= 2 ~ 1,
    sum <= 4 ~ 2,
    sum <= 6 ~ 3,
    TRUE ~ 999
  )
  
  # Componente 3
  proc_fct[p, 25] <- case_when(
    pittsburg[p, 9] < 5 ~ 3,
    pittsburg[p, 9] <= 6 ~ 2,
    pittsburg[p, 9] <= 7 ~ 1,
    pittsburg[p, 9] > 7 ~ 0,
    TRUE ~ 999
  )
  
  # Componente 4
  hr_cm <- if (pittsburg[p, 6] > hms("16:00:00")) {
    abs(as.integer((pittsburg[p, 6] - ddays(1)) - pittsburg[p, 8]) / 3600)
  } else {
    abs(as.integer(pittsburg[p, 6] - pittsburg[p, 8]) / 3600)
  }
  efi <- (proc_fct[p, 7] / hr_cm) * 100
  proc_fct[p, 26] <- case_when(
    efi >= 85 ~ 0,
    efi >= 75 ~ 1,
    efi >= 65 ~ 2,
    efi < 65 ~ 3,
    TRUE ~ 999
  )
  
  # Sumatoria de los factores
  proc_fct[p, 27] <- sum(proc_fct[p, 22:26], proc_fct[p, 7], proc_fct[p, 18:19])
}

# Eliminar variables temporales
rm(tbl_colnames, asignaciones, sum, hr_cm, efi, asignacion, p, o, r)

source("to_excel.R", chdir = TRUE)
