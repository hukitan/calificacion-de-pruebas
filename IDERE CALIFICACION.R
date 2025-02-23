library(gsheet) #solo cuando se use sheets#
library(readxl) #solo cuando se use excel#
library(rstatix)
library(writexl)
library(tidyverse)


##### CARGAR BASE DE DATOS #####
IDERE <- read_xlsx("IDERE Respuestas BIEN.xlsx",sheet = "Respuestas")

tbl_colnames <- c("Marca temporal", "ID", "edad", "sexo", "email") 
Resultados_IDERE <- tbl_colnames %>% purrr::map_dfc(setNames, object = list(logical())) # hace una tible vacia con nombres en tbl_colnames y la nombra res
rm(tbl_colnames)

# Marca temporal
for (p in 1:nrow(IDERE)) {
  Resultados_IDERE[p, 1] <- IDERE[p, 1]
}

# id
for (p in 1:nrow(IDERE)) {
  Resultados_IDERE[p, 2] <- IDERE[p, 2]
}

# sexo
for (p in 1:nrow(IDERE)) {
  Resultados_IDERE[p, 3] <- IDERE[p, 3]
}

# edad
for (p in 1:nrow(IDERE)) {
  Resultados_IDERE[p, 4] <- IDERE[p, 4]
}

# email
for (p in 1:nrow(IDERE)) {
  Resultados_IDERE[p, 5] <- IDERE[p, 5]
}

##### SE CLASIFICA ENTRE RASGO Y ESTADO #####

estadoA <- cbind(IDERE$`1. Pienso que mi futuro es desesperado y no mejorará mi situación.`,
                 IDERE$`2. Estoy preocupado/a.`,
                 IDERE$`6. Siento deseos de quitarme la vida.`,
                 IDERE$`8. Deseo desentenderme de todos los problemas que tengo.`,
                 IDERE$`9. Me canso más pronto que antes.`,
                 IDERE$`10. Me inclino a ver el lado bueno de las cosas....15`,
                 IDERE$`13. He perdido la confianza en mí mismo/a.`,
                 IDERE$`15. Siento que nada me alegra como antes.`,
                 IDERE$`18. Me siento incapaz de hacer cualquier trabajo por pequeño que sea.`,
                 IDERE$`20. Me despierto más temprano que antes y me cuesta trabajo volverme a dormir.`)

estadoB <- cbind(IDERE$`3. Me siento con confianza en mí mismo/a.`,
                 IDERE$`4. Siento que no me canso con facilidad.`,
                 IDERE$`5. Creo que no tengo nada de que arrepentirme.`,
                 IDERE$`7. Me siento seguro/a.`,
                 IDERE$`11. Me siento bien sexualmente.`,
                 IDERE$`12. Ahora no tengo ganas de llorar.`,
                 IDERE$`14. Siento necesidad de vivir.`,
                 IDERE$`16. No tengo sentimientos de culpa.`,
                 IDERE$`17. Duermo perfectamente.`,
                 IDERE$`19. Tengo gran confianza en el porvenir.`)

rasgoA <- cbind(IDERE$`1. Quisiera ser tan feliz como otras personas parecen serlo.`,
                IDERE$`3. Pienso que las cosas me van a salir mal.`,
                IDERE$`5. Sufro cuando no me siento reconocido por los demás.`,
                IDERE$`7. Sufro por no haber alcanzado mis mayores aspiraciones.`,
                IDERE$`8. Me deprimo por pequeñas cosas.`,
                IDERE$`11. Me siento aburrido/a.`,
                IDERE$`14. Soy indiferente ante las situaciones emocionales.`,
                IDERE$`16. Me afectan tanto los desengaños que no me los puedo quitar de la cabeza.`,
                IDERE$`17. Me falta confianza en mí mismo/a.`,
                IDERE$`20. Me ahogo en un vaso de agua.`,
                IDERE$`22. Pienso que la gente no me reconoce las cosas buenas que hago.`)

rasgoB <- cbind(IDERE$`2. Creo no haber fracasado más que otras personas.`,
                IDERE$`4. Creo que he tenido suerte en la vida.`,
                IDERE$`6. Pienso que todo saldrá bien en el futuro.`,
                IDERE$`9. Tengo confianza en mí mismo/a.`,
                IDERE$`10. Me inclino a ver el lado bueno de las cosas....35`,
                IDERE$`12. Los problemas no me preocupan más de lo que se merecen.`,
                IDERE$`13. He logrado cumplir mis propósitos fundamentales.`,
                IDERE$`15. Todo me resulta de interés.`,
                IDERE$`18. Me siento lleno/a de fuerzas y energía.`,
                IDERE$`19. Pienso que los demás me motivan adecuadamente.`,
                IDERE$`21. Soy una persona alegre.`)

#####  SE PASA DE PALABRAS A NUMEROS ##### 
estadoA <- as.data.frame(estadoA)
estadoB <- as.data.frame(estadoB)
rasgoA <- as.data.frame(rasgoA)
rasgoB <- as.data.frame(rasgoB)

estadoA_n <- estadoA %>%
  mutate(across(all_of(colnames(estadoA)), ~case_when(
    . == "Nada" ~ 1,
    . == "Un poco" ~ 2,
    . == "Mucho" ~ 3,
    . == "Demasiado" ~ 4,
    TRUE ~ NA_real_
  )))
estadoB_n <- estadoB %>%
  mutate(across(all_of(colnames(estadoB)), ~case_when(
    . == "Nada" ~ 1,
    . == "Un poco" ~ 2,
    . == "Mucho" ~ 3,
    . == "Demasiado" ~ 4,
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
PtjeR <- (sumaRA-sumaRB)+50

PtjeE_N <- PtjeE
PtjeR_N <- PtjeR

##### SE INTERPRETAN LOS PUNTUAJES PARA RASGO Y ESTADO #####

PtjeE <- as.data.frame(PtjeE)
PtjeE_N <- PtjeE %>%
  mutate(across(where(is.numeric), ~case_when(
    . >= 43 ~ "HIGH",
    . > 34 & . < 43 ~ "MEDIUM",
    . <= 34 ~ "LOW",
    TRUE ~ NA_character_
  )))

PtjeR <- as.data.frame(PtjeR)
PtjeR_N <- PtjeR %>%
  mutate(across(where(is.numeric), ~case_when(
    . >= 47 ~ "HIGH",
    . > 35 & . < 47 ~ "MEDIUM",
    . <= 35 ~ "LOW",
    TRUE ~ NA_character_
  )))


##### EXPORTAR BASE DE DATOS #####
Resultados_IDERE_BIEN<- cbind(Resultados_IDERE, PtjeE, PtjeE_N, PtjeR, PtjeR_N)

write_xlsx(Resultados_IDERE_BIEN, "Resultados_IDERE.xlsx")
