library(tidyverse)
set.seed(724)
tiempo <- seq(
  as.POSIXct("2026-01-01 06:00"),
  as.POSIXct("2026-01-03 22:00"),
  by = "3 min"
  )
puntos <- c("Centro", "Norte", "Sur", "Oriente", "Occidente")
# Simulación
 datos <- expand.grid(
   tiempo = tiempo,
   punto = puntos
   ) %>%
   as_tibble() %>%
   arrange(tiempo, punto) %>%
   mutate(
     intervalo = as.numeric(difftime(tiempo, min(tiempo), units = "mins")) / 10,
     hora = hour(tiempo) + minute(tiempo) / 60,
     intensidad = case_when(
       hora >= 6 & hora < 10 ~
         1 + 2.5 * sin((hora - 6) / 4 * pi),
       hora >= 16 & hora < 20 ~
         1 + 3 * sin((hora - 16) / 4 * pi),
       TRUE ~ 1
       ),
     vehiculos = pmax(1, round(intensidad + rnorm(n(), 20, 2))),
     velocidad_kmh = round(pmax(10, 60 - vehiculos * runif(1, 0.2, 0.6)),1),
     congestion = case_when(
       vehiculos < 50 ~ "Baja",
       vehiculos < 100 ~ "Media",
       vehiculos < 150 ~ "Alta",
       TRUE ~ "Critica"
       )
     )
