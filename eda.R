
# ============================================================
# EJERCICIO 1 - EDA BASICO
# TRAFICO Y MOVILIDAD URBANA
# ============================================================

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
# 2. CARGAR DATOS GENERADOS POR datos.R
# ============================================================

# Cambia la ruta segun la ubicacion de tu archivo
source("datos.R")

# IMPORTANTE:
# Reemplaza 'trafico' por el nombre del data.frame
# generado en datos.R.

datos <- trafico

# ============================================================
# 3. DIMENSIONES DEL DATASET
# ============================================================

cat("\n========== DIMENSIONES ==========\n")

cat("Cantidad de observaciones:", nrow(datos), "\n")
cat("Cantidad de columnas:", ncol(datos), "\n")

# Nombres de las columnas
cat("\nCOLUMNAS DEL DATASET:\n")
print(names(datos))

# ============================================================
# 4. EXPLORACION INICIAL
# ============================================================

cat("\n========== PRIMEROS REGISTROS ==========\n")
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
  tipo = sapply(datos, function(x) class(x)[1])
)

print(tipos)

# ============================================================
# 6. VALORES NULOS
# ============================================================

nulos <- data.frame(
  columna = names(datos),
  cantidad_nulos = colSums(is.na(datos)),
  porcentaje_nulos = round(
    colMeans(is.na(datos)) * 100, 2
  )
)

print(nulos)

# ============================================================
# 7. REGISTROS DUPLICADOS
# ============================================================

duplicados <- sum(duplicated(datos))

cat("\nRegistros duplicados:", duplicados, "\n")

# ============================================================
# 8. ESTADISTICAS DE VARIABLES NUMERICAS
# ============================================================

numericas <- datos %>%
  select(where(is.numeric))

if (ncol(numericas) > 0) {
  
  estadisticas <- data.frame(
    variable = names(numericas),
    media = sapply(numericas, mean, na.rm = TRUE),
    mediana = sapply(numericas, median, na.rm = TRUE),
    desviacion = sapply(numericas, sd, na.rm = TRUE),
    minimo = sapply(numericas, min, na.rm = TRUE),
    maximo = sapply(numericas, max, na.rm = TRUE)
  )
  
  print(estadisticas)
}

# ============================================================
# 9. HISTOGRAMAS
# ============================================================

if (ncol(numericas) > 0) {
  
  datos_largos <- numericas %>%
    pivot_longer(
      cols = everything(),
      names_to = "variable",
      values_to = "valor"
    )
  
  grafico <- ggplot(
    datos_largos,
    aes(x = valor)
  ) +
    geom_histogram(
      bins = 30,
      fill = "steelblue",
      color = "white"
    ) +
    facet_wrap(
      ~variable,
      scales = "free"
    ) +
    theme_minimal() +
    labs(
      title = "Distribucion de variables numericas",
      x = "Valor",
      y = "Frecuencia"
    )
  
  print(grafico)
}

# ============================================================
# 10. CORRELACION ENTRE VARIABLES
# ============================================================

if (ncol(numericas) >= 2) {
  
  correlaciones <- cor(
    numericas,
    use = "pairwise.complete.obs"
  )
  
  print(round(correlaciones, 3))
}

# ============================================================
# 11. CONCLUSIONES DEL EDA
# ============================================================

cat("\n========== CONCLUSIONES ==========\n")

cat("Total de registros:", nrow(datos), "\n")
cat("Total de variables:", ncol(datos), "\n")
cat("Total de valores nulos:", sum(is.na(datos)), "\n")
cat("Total de duplicados:", duplicados, "\n")

cat("\nEDA FINALIZADO CORRECTAMENTE\n")
