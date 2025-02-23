library(gsheet) #solo cuando se use sheets#
library(readxl) #solo cuando se use excel#
library(rstatix)
library(writexl)
library(tidyverse)


##### CARGAR BASE DE DATOS #####
IDARE <- read_xlsx("IDARE BIEN.xlsx",sheet = "Respuestas")

tbl_colnames <- c("Marca temporal", "ID", "edad", "sexo", "email") 
Resultados_IDARE <- tbl_colnames %>% purrr::map_dfc(setNames, object = list(logical())) # hace una tible vacia con nombres en tbl_colnames y la nombra res
rm(tbl_colnames)

# Marca temporal
for (p in 1:nrow(IDARE)) {
  Resultados_IDARE[p, 1] <- IDARE[p, 1]
}

# id
for (p in 1:nrow(IDARE)) {
  Resultados_IDARE[p, 2] <- IDARE[p, 2]
}

# sexo
for (p in 1:nrow(IDARE)) {
  Resultados_IDARE[p, 3] <- IDARE[p, 3]
}

# edad
for (p in 1:nrow(IDARE)) {
  Resultados_IDARE[p, 4] <- IDARE[p, 4]
}

# email
for (p in 1:nrow(IDARE)) {
  Resultados_IDARE[p, 5] <- IDARE[p, 5]
}

##### SE CLASIFICA ENTRE RASGO Y ESTADO #####

estadoA <- cbind(IDARE$`3. Estoy tenso/a`,
                 IDARE$`4. Estoy contrariado/a`,
                 IDARE$`6. Me siento alterado/a`,
                 IDARE$`7. Estoy alterado/a por un posible contratiempo`,
                 IDARE$`9. Me siento ansioso/a`,
                 IDARE$`12. Me siento nervioso/a`,
                 IDARE$`13. Estoy agitado/a`,
                 IDARE$`14. Me siento "a punto de explotar"`,
                 IDARE$`17. Me siento preocupado/a`,
                 IDARE$`18. Me siento muy excitado/a y aturdido/a`)

estadoB <- cbind(IDARE$`1. Me siento calmado/a`,
                 IDARE$`2. Me siento seguro/a`,
                 IDARE$`5. Me siento a gusto`,
                 IDARE$`8. Me siento descansado/a`,
                 IDARE$`10. Me siento cómodo/a`,
                 IDARE$`11. Me siento con confianza en mí mismo/a`,
                 IDARE$`15. Me siento relajado/a`,
                 IDARE$`16. Me siento satisfecho/a`,
                 IDARE$`19. Me siento alegre`,
                 IDARE$`20. Me siento bien`)

rasgoA <- cbind(IDARE$`22. Me canso rápidamente`,
                IDARE$`23. Siento ganas de llorar`,
                IDARE$`24. Quisiera ser tan feliz`,
                IDARE$`25. Me pierdo cosas por no poder decidirme rápidamente`,
                IDARE$`28. Siento que las dificultades se amontonan al punto de no poder soportarlas`,
                IDARE$`29. Me preocupo demasiado por cosas sin importancia`,
                IDARE$`31. Me inclino a tomar las cosas muy a pecho`,
                IDARE$`32. Me falta confianza en mí mismo/a`,
                IDARE$`34. Trato de evitar enfrentar una crisis o dificultad`,
                IDARE$`35. Me siento melancólico/a`,
                IDARE$`37. Algunas ideas poco importantes pasan por mi mente`,
                IDARE$`38. Me afectan tanto los desengaños que no me los puedo quitar de la cabeza`,
                IDARE$`40. Cuando pienso en los asuntos que tengo entre manos me pongo tenso/a y alterado/a`)

rasgoB <- cbind(IDARE$`21. Me siento bien`,
                IDARE$`26. Me siento descansado/a`,
                IDARE$`27. Soy una persona "tranquila, serena y sosegada"`,
                IDARE$`30. Soy feliz`,
                IDARE$`33. Me siento seguro/a`,
                IDARE$`36. Estoy satisfecho/a`,
                IDARE$`39. Soy una persona estable`)

#####  SE PASA DE PALABRAS A NUMEROS ##### 
estadoA <- as.data.frame(estadoA)
estadoB <- as.data.frame(estadoB)
rasgoA <- as.data.frame(rasgoA)
rasgoB <- as.data.frame(rasgoB)

estadoA_n <- estadoA %>%
  mutate(across(all_of(colnames(estadoA)), ~case_when(
      . == "No" ~ 1,
      . == "Un poco" ~ 2,
      . == "Bastante" ~ 3,
      . == "Mucho" ~ 4,
      TRUE ~ NA_real_
    )))
estadoB_n <- estadoB %>%
  mutate(across(all_of(colnames(estadoB)), ~case_when(
    . == "No" ~ 1,
    . == "Un poco" ~ 2,
    . == "Bastante" ~ 3,
    . == "Mucho" ~ 4,
    TRUE ~ NA_real_
  )))

rasgoA_n <- rasgoA %>%
  mutate(across(all_of(colnames(rasgoA)), ~case_when(
    . == "Casi nunca" ~ 1,
    . == "Algunas veces" ~ 2,
    . == "Frecuentemente" ~ 3,
    . == "Casi siempre" ~ 4,
    TRUE ~ NA_real_
  )))
rasgoB_n <- rasgoB %>%
  mutate(across(all_of(colnames(rasgoB)), ~case_when(
    . == "Casi nunca" ~ 1,
    . == "Algunas veces" ~ 2,
    . == "Frecuentemente" ~ 3,
    . == "Casi siempre" ~ 4,
    TRUE ~ NA_real_
  )))


##### SE CALCULAN LOS PUNTUAJES PARA RASGO Y ESTADO #####

sumaEA <- rowSums(estadoA_n)
sumaEB <- rowSums(estadoB_n)
sumaRA <- rowSums(rasgoA_n)
sumaRB <- rowSums(rasgoB_n)

PtjeE <- (sumaEA-sumaEB)+50
PtjeR <- (sumaRA-sumaRB)+35

PtjeE_N <- PtjeE
PtjeR_N <- PtjeR

##### SE INTERPRETAN LOS PUNTUAJES PARA RASGO Y ESTADO #####

PtjeE <- as.data.frame(PtjeE)
PtjeE_N <- PtjeE %>%
  mutate(across(where(is.numeric), ~case_when(
    . >= 45 ~ "HIGH",
    . > 30 & . < 45 ~ "MEDIUM",
    . <= 30 ~ "LOW",
    TRUE ~ NA_character_
  )))

PtjeR <- as.data.frame(PtjeR)
PtjeR_N <- PtjeR %>%
  mutate(across(where(is.numeric), ~case_when(
    . >= 45 ~ "HIGH",
    . > 30 & . < 45 ~ "MEDIUM",
    . <= 30 ~ "LOW",
    TRUE ~ NA_character_
  )))


##### EXPORTAR BASE DE DATOS #####
Resultados_IDARE_BIEN<- cbind(Resultados_IDARE, PtjeE, PtjeE_N, PtjeR, PtjeR_N)

write_xlsx(Resultados_IDARE_BIEN, "Resultados_IDARE.xlsx")


