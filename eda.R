
# ============================================================
# EJERCICIO 1 - ANALISIS EXPLORATORIO DE DATOS (EDA)
# TRAFICO Y MOVILIDAD URBANA
# ============================================================
# hay algunas cosas por corregir
# 1. INSTALAR Y CARGAR LIBRERIAS
paquetes <- c("dplyr", "ggplot2", "tidyr")

faltantes <- paquetes[
  !paquetes %in% rownames(installed.packages())
]

if (length(faltantes) > 0) {
  install.packages(faltantes)
}

library(dplyr)
library(ggplot2)
library(tidyr)

# ============================================================
# 2. GENERAR DATOS SIMULADOS DE TRAFICO
# ============================================================

set.seed(123)

n <- 10000

datos <- data.frame(
  id_vehiculo = 1:n,
  
  tipo_vehiculo = sample(
    c("Auto", "Moto", "Bus", "Camion", "Taxi"),
    n,
    replace = TRUE
  ),
  
  zona = sample(
    c("Centro", "Norte", "Sur", "Este", "Oeste"),
    n,
    replace = TRUE
  ),
  
  hora = sample(0:23, n, replace = TRUE),
  
  velocidad_kmh = round(
    pmax(rnorm(n, mean = 45, sd = 15), 5),
    2
  ),
  
  distancia_km = round(
    runif(n, min = 1, max = 30),
    2
  ),
  
  nivel_congestion = sample(
    c("Bajo", "Medio", "Alto"),
    n,
    replace = TRUE,
    prob = c(0.30, 0.45, 0.25)
  )
)

# Calcular tiempo de viaje en minutos
datos$tiempo_viaje_min <- round(
  (datos$distancia_km / datos$velocidad_kmh) * 60,
  2
)

# Introducir algunos valores faltantes para el ejercicio
indices_na <- sample(1:n, 100)
datos$velocidad_kmh[indices_na] <- NA

# ============================================================
# 3. DIMENSIONES DEL DATASET
# ============================================================

cat("\n========== DIMENSIONES ==========\n")

cat("Cantidad de observaciones:", nrow(datos), "\n")
cat("Cantidad de columnas:", ncol(datos), "\n")

cat("\nNOMBRES DE LAS COLUMNAS:\n")
print(names(datos))

# ============================================================
# 4. EXPLORACION INICIAL
# ============================================================

cat("\n========== PRIMEROS 10 REGISTROS ==========\n")
print(head(datos, 10))

cat("\n========== ESTRUCTURA ==========\n")
str(datos)

cat("\n========== RESUMEN ESTADISTICO ==========\n")
print(summary(datos))

# ============================================================
# 5. TIPOS DE DATOS
# ============================================================

tipos <- data.frame(
  columna = names(datos),
  tipo = sapply(datos, function(x) class(x)[1]),
  row.names = NULL
)

cat("\n========== TIPOS DE DATOS ==========\n")
print(tipos)

# ============================================================
# 6. VALORES FALTANTES
# ============================================================

nulos <- data.frame(
  columna = names(datos),
  cantidad_nulos = colSums(is.na(datos)),
  porcentaje_nulos = round(
    colMeans(is.na(datos)) * 100,
    2
  ),
  row.names = NULL
)

cat("\n========== VALORES FALTANTES ==========\n")
print(nulos)

# ============================================================
# 7. REGISTROS DUPLICADOS
# ============================================================

duplicados <- sum(duplicated(datos))

cat("\n========== DUPLICADOS ==========\n")
cat("Registros duplicados:", duplicados, "\n")

# ============================================================
# 8. ESTADISTICAS DESCRIPTIVAS
# ============================================================

# Excluir el identificador de las estadisticas
numericas <- datos %>%
  select(
    hora,
    velocidad_kmh,
    distancia_km,
    tiempo_viaje_min
  )

estadisticas <- data.frame(
  variable = names(numericas),
  
  media = sapply(
    numericas,
    mean,
    na.rm = TRUE
  ),
  
  mediana = sapply(
    numericas,
    median,
    na.rm = TRUE
  ),
  
  desviacion = sapply(
    numericas,
    sd,
    na.rm = TRUE
  ),
  
  minimo = sapply(
    numericas,
    min,
    na.rm = TRUE
  ),
  
  maximo = sapply(
    numericas,
    max,
    na.rm = TRUE
  ),
  
  row.names = NULL
)

cat("\n========== ESTADISTICAS ==========\n")
print(estadisticas)

# ============================================================
# 9. DISTRIBUCION POR TIPO DE VEHICULO
# ============================================================

cat("\n========== TIPOS DE VEHICULO ==========\n")

print(
  datos %>%
    count(tipo_vehiculo, name = "cantidad") %>%
    arrange(desc(cantidad))
)

grafico_vehiculos <- ggplot(
  datos,
  aes(x = tipo_vehiculo)
) +
  geom_bar(fill = "steelblue") +
  theme_minimal() +
  labs(
    title = "Cantidad de vehiculos por tipo",
    x = "Tipo de vehiculo",
    y = "Cantidad"
  )

print(grafico_vehiculos)

# ============================================================
# 10. DISTRIBUCION POR ZONA
# ============================================================

cat("\n========== TRAFICO POR ZONA ==========\n")

print(
  datos %>%
    count(zona, name = "cantidad") %>%
    arrange(desc(cantidad))
)

grafico_zonas <- ggplot(
  datos,
  aes(x = zona)
) +
  geom_bar(fill = "darkorange") +
  theme_minimal() +
  labs(
    title = "Distribucion del trafico por zona",
    x = "Zona",
    y = "Cantidad de vehiculos"
  )

print(grafico_zonas)

# ============================================================
# 11. DISTRIBUCION DE VELOCIDAD
# ============================================================

grafico_velocidad <- ggplot(
  datos,
  aes(x = velocidad_kmh)
) +
  geom_histogram(
    bins = 30,
    fill = "steelblue",
    color = "white",
    na.rm = TRUE
  ) +
  theme_minimal() +
  labs(
    title = "Distribucion de velocidades",
    x = "Velocidad (km/h)",
    y = "Frecuencia"
  )

print(grafico_velocidad)

# ============================================================
# 12. TRAFICO POR HORA
# ============================================================

trafico_hora <- datos %>%
  group_by(hora) %>%
  summarise(
    cantidad_vehiculos = n(),
    .groups = "drop"
  )

cat("\n========== TRAFICO POR HORA ==========\n")
print(trafico_hora)

grafico_hora <- ggplot(
  trafico_hora,
  aes(
    x = hora,
    y = cantidad_vehiculos
  )
) +
  geom_line(
    color = "blue",
    linewidth = 1
  ) +
  geom_point() +
  scale_x_continuous(breaks = 0:23) +
  theme_minimal() +
  labs(
    title = "Cantidad de vehiculos por hora",
    x = "Hora del dia",
    y = "Cantidad de vehiculos"
  )

print(grafico_hora)

# ============================================================
# 13. NIVEL DE CONGESTION
# ============================================================

congestion <- datos %>%
  count(nivel_congestion, name = "cantidad") %>%
  mutate(
    porcentaje = round(
      cantidad / sum(cantidad) * 100,
      2
    )
  )

cat("\n========== NIVELES DE CONGESTION ==========\n")
print(congestion)

grafico_congestion <- ggplot(
  datos,
  aes(x = nivel_congestion)
) +
  geom_bar(fill = "tomato") +
  theme_minimal() +
  labs(
    title = "Niveles de congestion vehicular",
    x = "Nivel de congestion",
    y = "Cantidad de registros"
  )

print(grafico_congestion)

# ============================================================
# 14. RELACION DISTANCIA VS TIEMPO DE VIAJE
# ============================================================

grafico_distancia <- ggplot(
  datos,
  aes(
    x = distancia_km,
    y = tiempo_viaje_min
  )
) +
  geom_point(
    alpha = 0.25,
    color = "steelblue"
  ) +
  theme_minimal() +
  labs(
    title = "Distancia vs tiempo de viaje",
    x = "Distancia (km)",
    y = "Tiempo de viaje (minutos)"
  )

print(grafico_distancia)

# ============================================================
# 15. CORRELACIONES
# ============================================================

correlaciones <- cor(
  numericas,
  use = "pairwise.complete.obs"
)

cat("\n========== CORRELACIONES ==========\n")
print(round(correlaciones, 3))

# ============================================================
# 16. RESUMEN DEL TRAFICO POR ZONA
# ============================================================

resumen_zonas <- datos %>%
  group_by(zona) %>%
  summarise(
    vehiculos = n(),
    
    velocidad_promedio = round(
      mean(velocidad_kmh, na.rm = TRUE),
      2
    ),
    
    tiempo_promedio = round(
      mean(tiempo_viaje_min, na.rm = TRUE),
      2
    ),
    
    .groups = "drop"
  )

cat("\n========== RESUMEN POR ZONA ==========\n")
print(resumen_zonas)

# ============================================================
# 17. CONCLUSIONES GENERALES
# ============================================================

cat("\n========== CONCLUSIONES DEL EDA ==========\n")

cat("Total de observaciones:", nrow(datos), "\n")
cat("Total de columnas:", ncol(datos), "\n")

cat(
  "Velocidad promedio:",
  round(mean(datos$velocidad_kmh, na.rm = TRUE), 2),
  "km/h\n"
)

cat(
  "Tiempo promedio de viaje:",
  round(mean(datos$tiempo_viaje_min, na.rm = TRUE), 2),
  "minutos\n"
)

cat(
  "Total de valores faltantes:",
  sum(is.na(datos)),
  "\n"
)

cat(
  "Registros duplicados:",
  duplicados,
  "\n"
)

cat("\nEDA FINALIZADO CORRECTAMENTE\n")


# ============================================================
# 18. INCREMENTO DEL TRAFICO URBANO
# ============================================================

# Crear datos de trafico por hora y zona
datos_trafico <- datos %>%
  group_by(hora, zona) %>%
  summarise(
    vehiculos = n(),
    .groups = "drop"
  ) %>%
  rename(
    tiempo = hora,
    punto = zona
  ) %>%
  arrange(punto, tiempo)

# Visualizar datos
print(head(datos_trafico, 10))

# Grafico de lineas
ggplot(
  datos_trafico,
  aes(
    x = tiempo,
    y = vehiculos,
    color = punto,
    group = punto
  )
) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  labs(
    title = "Incremento del tráfico urbano",
    x = "Tiempo (horas)",
    y = "Vehículos",
    color = "Punto de medición"
  ) +
  scale_x_continuous(breaks = 0:23) +
  theme_minimal()
