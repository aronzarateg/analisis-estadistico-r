# ============================================================
# GENERACION DE DATOS FINANCIEROS SIMULADOS
# PostgreSQL + R
# ============================================================

library(DBI)
library(RPostgres)
library(data.table)
library(lubridate)

set.seed(123)

# ============================================================
# 1. SE ASUME QUE YA EXISTE LA CONEXION
# ============================================================

# Ejemplo, NO es necesario ejecutarlo si ya estas conectado:

# ============================================================
# 1. CONEXIÓN A POSTGRESQL
# ============================================================

con <- dbConnect(
  RPostgres::Postgres(),
  host = "base_de_datos",
  port = 5432,
  dbname = "bd_caso_estudio_3",
  user = "postgres",
  password = "postgres"
)

# ============================================================
# 2. CREAR TABLAS
# ============================================================

dbExecute(con, "
CREATE TABLE IF NOT EXISTS clientes (
    id_cliente BIGINT PRIMARY KEY,
    documento VARCHAR(20),
    edad INTEGER,
    ciudad VARCHAR(50),
    segmento VARCHAR(30),
    ingreso_mensual NUMERIC(14,2),
    antiguedad_cliente INTEGER,
    fecha_registro DATE
);
")


dbExecute(con, "
CREATE TABLE IF NOT EXISTS creditos (
    id_credito BIGINT PRIMARY KEY,
    id_cliente BIGINT NOT NULL,
    tipo_credito VARCHAR(40),
    monto_original NUMERIC(14,2),
    tasa_interes NUMERIC(8,4),
    plazo_meses INTEGER,
    fecha_inicio DATE,
    refinanciado INTEGER DEFAULT 0,

    CONSTRAINT fk_credito_cliente
        FOREIGN KEY (id_cliente)
        REFERENCES clientes(id_cliente)
);
")


dbExecute(con, "
CREATE TABLE IF NOT EXISTS periodos_credito (
    id_periodo BIGSERIAL PRIMARY KEY,
    id_credito BIGINT NOT NULL,
    numero_periodo INTEGER,
    fecha_periodo DATE,

    ingreso NUMERIC(14,2),
    saldo NUMERIC(14,2),
    cuota NUMERIC(14,2),

    utilizacion NUMERIC(10,4),
    mora_dias INTEGER,
    mora_historica INTEGER,

    refinanciado INTEGER,
    tasa_interes NUMERIC(8,4),

    pago_realizado NUMERIC(14,2),
    ratio_pago NUMERIC(10,4),

    riesgo NUMERIC(10,6),
    incumplimiento INTEGER,

    CONSTRAINT fk_periodo_credito
        FOREIGN KEY (id_credito)
        REFERENCES creditos(id_credito)
);
")



# ============================================================
# 3. GENERAR CLIENTES
# ============================================================

n_clientes <- 50000


clientes <- data.table(
  
  id_cliente = 1:n_clientes,
  
  documento = sprintf(
    "CLI%08d",
    1:n_clientes
  ),
  
  edad = sample(
    18:75,
    n_clientes,
    replace = TRUE
  ),
  
  ciudad = sample(
    c(
      "Lima",
      "Arequipa",
      "Cusco",
      "Trujillo",
      "Piura",
      "Chiclayo"
    ),
    n_clientes,
    replace = TRUE,
    prob = c(
      0.45,
      0.12,
      0.10,
      0.12,
      0.11,
      0.10
    )
  ),
  
  segmento = sample(
    c(
      "Bajo",
      "Medio",
      "Alto"
    ),
    n_clientes,
    replace = TRUE,
    prob = c(
      0.30,
      0.55,
      0.15
    )
  ),
  
  ingreso_mensual = round(
    rlnorm(
      n_clientes,
      log(3500),
      0.55
    ),
    2
  ),
  
  antiguedad_cliente = sample(
    1:180,
    n_clientes,
    replace = TRUE
  ),
  
  fecha_registro =
    as.Date("2018-01-01") +
    sample(
      0:2500,
      n_clientes,
      replace = TRUE
    )
)


# Limitar ingresos excesivamente altos
clientes[
  ingreso_mensual > 30000,
  ingreso_mensual := 30000
]


head(clientes)
# ============================================================
# 4. INSERTAR CLIENTES
# ============================================================

dbWriteTable(
  con,
  "clientes",
  clientes,
  append = TRUE,
  row.names = FALSE
)

cat(
  "Clientes insertados:",
  nrow(clientes),
  "\n"
)
# ============================================================
# 5. GENERAR CREDITOS
# ============================================================

cantidad_creditos <- sample(
  1:5,
  n_clientes,
  replace = TRUE,
  prob = c(
    0.35,
    0.30,
    0.20,
    0.10,
    0.05
  )
)


# Repetimos ID de cliente segun cantidad de creditos
id_clientes_creditos <- rep(
  clientes$id_cliente,
  cantidad_creditos
)


n_creditos <- length(
  id_clientes_creditos
)


cat(
  "Total de creditos:",
  n_creditos,
  "\n"
)




creditos <- data.table(
  
  id_credito = 1:n_creditos,
  
  id_cliente = id_clientes_creditos,
  
  tipo_credito = sample(
    c(
      "Consumo",
      "Tarjeta",
      "Vehicular",
      "Personal",
      "Hipotecario"
    ),
    n_creditos,
    replace = TRUE,
    prob = c(
      0.30,
      0.30,
      0.12,
      0.20,
      0.08
    )
  ),
  
  monto_original = round(
    rlnorm(
      n_creditos,
      log(15000),
      0.8
    ),
    2
  ),
  
  tasa_interes = round(
    runif(
      n_creditos,
      0.08,
      0.45
    ),
    4
  ),
  
  plazo_meses = sample(
    12:72,
    n_creditos,
    replace = TRUE
  ),
  
  fecha_inicio =
    as.Date("2021-01-01") +
    sample(
      0:1500,
      n_creditos,
      replace = TRUE
    ),
  
  refinanciado = rbinom(
    n_creditos,
    1,
    0.08
  )
)


# Evitar montos exagerados
creditos[
  monto_original > 200000,
  monto_original := 200000
]


head(creditos)



# ============================================================
# 6. INSERTAR CREDITOS
# ============================================================

dbWriteTable(
  con,
  "creditos",
  creditos,
  append = TRUE,
  row.names = FALSE
)


cat(
  "Creditos insertados:",
  nrow(creditos),
  "\n"
)

# ============================================================
# 7. FUNCION LOGISTICA
# ============================================================

logistic <- function(x) {
  
  1 / (
    1 + exp(-x)
  )
  
}

# ============================================================
# 8. EFECTO DEL TIPO DE CREDITO
# ============================================================

efecto_tipo <- function(tipo) {
  
  resultado <- rep(
    0,
    length(tipo)
  )
  
  resultado[
    tipo == "Tarjeta"
  ] <- 0.35
  
  resultado[
    tipo == "Personal"
  ] <- 0.25
  
  resultado[
    tipo == "Consumo"
  ] <- 0.15
  
  resultado[
    tipo == "Vehicular"
  ] <- -0.10
  
  resultado[
    tipo == "Hipotecario"
  ] <- -0.30
  
  resultado
}

# ============================================================
# 9. GENERAR PERIODOS POR LOTES
# ============================================================

tamano_lote <- 2000
total_creditos <- nrow(creditos)

for (inicio in seq(1, total_creditos, by = tamano_lote)) {
  
  fin <- min(
    inicio + tamano_lote - 1,
    total_creditos
  )
  
  cat(
    "\nProcesando creditos",
    inicio,
    "a",
    fin,
    "de",
    total_creditos,
    "\n"
  )
  
  # ==========================================================
  # 9.1 OBTENER LOTE DE CREDITOS
  # ==========================================================
  
  lote_creditos <- creditos[inicio:fin]
  
  
  # ==========================================================
  # 9.2 NUMERO DE PERIODOS POR CREDITO
  # Cada credito tendra entre 12 y 72 observaciones
  # ==========================================================
  
  numero_periodos <- sample(
    12:72,
    nrow(lote_creditos),
    replace = TRUE
  )
  
  
  # ==========================================================
  # 9.3 REPETIR CADA CREDITO SEGUN SUS PERIODOS
  # ==========================================================
  
  historial <- lote_creditos[
    rep(
      seq_len(.N),
      numero_periodos
    )
  ]
  
  
  # ==========================================================
  # 9.4 NUMERO DE PERIODO
  # ==========================================================
  
  historial[
    ,
    numero_periodo := sequence(numero_periodos)
  ]
  
  
  # ==========================================================
  # 9.5 FECHA DEL PERIODO
  # ==========================================================
  
  historial[
    ,
    fecha_periodo :=
      fecha_inicio %m+%
      months(numero_periodo - 1)
  ]
  
  
  # ==========================================================
  # 9.6 AGREGAR INFORMACION DEL CLIENTE
  # ==========================================================
  
  historial <- merge(
    historial,
    clientes[
      ,
      .(
        id_cliente,
        ingreso_mensual,
        antiguedad_cliente,
        ciudad,
        segmento
      )
    ],
    by = "id_cliente",
    all.x = TRUE
  )
  
  
  # ==========================================================
  # 9.7 INGRESO DEL PERIODO
  # Se permite una variacion mensual aproximada de +/- 15 %
  # ==========================================================
  
  historial[
    ,
    ingreso :=
      ingreso_mensual *
      runif(
        .N,
        0.85,
        1.15
      )
  ]
  
  
  # ==========================================================
  # 9.8 SALDO DEL CREDITO
  # Disminuye conforme avanzan las cuotas
  # ==========================================================
  
  historial[
    ,
    saldo :=
      monto_original *
      pmax(
        0.02,
        1 - numero_periodo / plazo_meses
      )
  ]
  
  
  # pequena variacion
  historial[
    ,
    saldo :=
      saldo *
      runif(
        .N,
        0.95,
        1.05
      )
  ]
  
  
  historial[
    saldo < 0,
    saldo := 0
  ]
  
  
  # ==========================================================
  # 9.9 CUOTA MENSUAL APROXIMADA
  # ==========================================================
  
  historial[
    ,
    cuota :=
      (
        monto_original / plazo_meses
      ) *
      (
        1 + tasa_interes / 12
      )
  ]
  
  
  # ==========================================================
  # 9.10 UTILIZACION
  # ==========================================================
  
  historial[
    ,
    utilizacion :=
      saldo / monto_original
  ]
  
  
  historial[
    ,
    utilizacion :=
      utilizacion +
      rnorm(
        .N,
        mean = 0,
        sd = 0.08
      )
  ]
  
  
  # Limitar entre 0 y 1
  historial[
    ,
    utilizacion :=
      pmin(
        pmax(utilizacion, 0),
        1
      )
  ]
  
  
  # ==========================================================
  # 9.11 PROBABILIDAD BASE DE MORA
  # ==========================================================
  
  prob_mora <-
    0.05 +
    0.18 * historial$utilizacion +
    0.12 * historial$refinanciado
  
  
  prob_mora <- pmin(
    pmax(prob_mora, 0),
    0.80
  )
  
  
  # ==========================================================
  # 9.12 DETERMINAR SI TIENE MORA
  # ==========================================================
  
  tiene_mora <- rbinom(
    nrow(historial),
    size = 1,
    prob = prob_mora
  )
  
  
  # ==========================================================
  # 9.13 GENERAR DIAS DE MORA
  # ==========================================================
  
  historial[
    ,
    mora_dias :=
      ifelse(
        tiene_mora == 1,
        sample(
          1:90,
          .N,
          replace = TRUE
        ),
        0
      )
  ]
  
  
  # ==========================================================
  # 9.14 ORDENAR POR CREDITO Y PERIODO
  # ==========================================================
  
  setorder(
    historial,
    id_credito,
    numero_periodo
  )
  
  
  # ==========================================================
  # 9.15 MORA HISTORICA
  # Cantidad de periodos anteriores con mora
  # ==========================================================
  
  historial[
    ,
    mora_historica :=
      shift(
        cumsum(mora_dias > 0),
        fill = 0
      ),
    by = id_credito
  ]
  
  
  # ==========================================================
  # 9.16 PAGO REALIZADO
  # ==========================================================
  
  historial[
    ,
    pago_realizado :=
      cuota *
      runif(
        .N,
        0.75,
        1.10
      )
  ]
  
  
  # Si tiene mora alta, el pago tiende a ser menor
  historial[
    mora_dias > 30,
    pago_realizado :=
      cuota *
      runif(
        .N,
        0.20,
        0.80
      )
  ]
  
  
  # ==========================================================
  # 9.17 RATIO DE PAGO
  # ==========================================================
  
  historial[
    ,
    ratio_pago :=
      pago_realizado / cuota
  ]
  
  
  historial[
    ,
    ratio_pago :=
      pmin(
        pmax(ratio_pago, 0),
        1.50
      )
  ]
  
  
  # ==========================================================
  # 9.18 EFECTO DEL TIPO DE CREDITO
  # ==========================================================
  
  efecto <- efecto_tipo(
    historial$tipo_credito
  )
  
  
  # ==========================================================
  # 9.19 MORA MAYOR A 30 DIAS
  # ==========================================================
  
  mora_grave <- ifelse(
    historial$mora_dias > 30,
    1,
    0
  )
  
  
  # ==========================================================
  # 9.20 FUNCION LATENTE DE RIESGO
  # ==========================================================
  
  z <-
    -4 +
    
    # Mora actual
    1.45 * mora_grave +
    
    # Utilizacion alta
    2.20 * historial$utilizacion +
    
    # Refinanciacion
    1.25 * historial$refinanciado +
    
    # Cuota respecto al ingreso
    1.10 *
    (
      historial$cuota /
        historial$ingreso
    ) +
    
    # Saldo pendiente
    0.000025 *
    historial$saldo -
    
    # Mayor ingreso reduce riesgo
    0.00012 *
    historial$ingreso -
    
    # Mayor antiguedad reduce ligeramente el riesgo
    0.006 *
    historial$antiguedad_cliente +
    
    # Historial de mora
    0.12 *
    historial$mora_historica +
    
    # Pagar menos de la cuota aumenta riesgo
    1.20 *
    (
      1 -
        pmin(
          historial$ratio_pago,
          1
        )
    ) +
    
    # Efecto del tipo de credito
    efecto
  
  
  # ==========================================================
  # 9.21 TRANSFORMAR A PROBABILIDAD
  # ==========================================================
  
  historial[
    ,
    riesgo := logistic(z)
  ]
  
  
  # Evitar probabilidades exactas 0 o 1
  historial[
    ,
    riesgo :=
      pmin(
        pmax(
          riesgo,
          0.001
        ),
        0.95
      )
  ]
  
  
  # ==========================================================
  # 9.22 GENERAR VARIABLE OBJETIVO
  # 0 = No incumple
  # 1 = Incumple
  # ==========================================================
  
  historial[
    ,
    incumplimiento :=
      rbinom(
        .N,
        size = 1,
        prob = riesgo
      )
  ]
  
  
  # ==========================================================
  # 9.23 PREPARAR COLUMNAS PARA POSTGRESQL
  # ==========================================================
  
  periodos_db <- historial[
    ,
    .(
      id_credito,
      numero_periodo,
      fecha_periodo,
      
      ingreso = round(
        ingreso,
        2
      ),
      
      saldo = round(
        saldo,
        2
      ),
      
      cuota = round(
        cuota,
        2
      ),
      
      utilizacion = round(
        utilizacion,
        4
      ),
      
      mora_dias,
      
      mora_historica,
      
      refinanciado,
      
      tasa_interes,
      
      pago_realizado = round(
        pago_realizado,
        2
      ),
      
      ratio_pago = round(
        ratio_pago,
        4
      ),
      
      riesgo = round(
        riesgo,
        6
      ),
      
      incumplimiento
    )
  ]
  
  
  # ==========================================================
  # 9.24 INSERTAR LOTE EN POSTGRESQL
  # ==========================================================
  
  dbWriteTable(
    con,
    "periodos_credito",
    periodos_db,
    append = TRUE,
    row.names = FALSE
  )
  
  
  cat(
    "Registros insertados:",
    nrow(periodos_db),
    "\n"
  )
  
  
  # ==========================================================
  # 9.25 LIBERAR MEMORIA
  # ==========================================================
  
  rm(
    historial,
    periodos_db
  )
  
  gc()
}


# ============================================================
# 10. VALIDACION GENERAL
# ============================================================

cat("\n====================================\n")
cat("VALIDACION DE DATOS\n")
cat("====================================\n")


# Clientes
resultado_clientes <- dbGetQuery(
  con,
  "
  SELECT COUNT(*) AS total_clientes
  FROM clientes;
  "
)

print(resultado_clientes)


# Creditos
resultado_creditos <- dbGetQuery(
  con,
  "
  SELECT COUNT(*) AS total_creditos
  FROM creditos;
  "
)

print(resultado_creditos)


# Periodos
resultado_periodos <- dbGetQuery(
  con,
  "
  SELECT COUNT(*) AS total_periodos
  FROM periodos_credito;
  "
)

print(resultado_periodos)

periodos_por_credito <- dbGetQuery(
  con,
  "
  SELECT
      MIN(cantidad) AS minimo_periodos,
      MAX(cantidad) AS maximo_periodos,
      AVG(cantidad) AS promedio_periodos
  FROM (
      SELECT
          id_credito,
          COUNT(*) AS cantidad
      FROM periodos_credito
      GROUP BY id_credito
  ) t;
  "
)

print(periodos_por_credito)


creditos_por_cliente <- dbGetQuery(
  con,
  "
  SELECT
      MIN(cantidad) AS minimo_creditos,
      MAX(cantidad) AS maximo_creditos,
      AVG(cantidad) AS promedio_creditos
  FROM (
      SELECT
          id_cliente,
          COUNT(*) AS cantidad
      FROM creditos
      GROUP BY id_cliente
  ) t;
  "
)

print(creditos_por_cliente)


distribucion_incumplimiento <- dbGetQuery(
  con,
  "
  SELECT
      incumplimiento,
      COUNT(*) AS cantidad,
      ROUND(
          COUNT(*) * 100.0 /
          SUM(COUNT(*)) OVER (),
          2
      ) AS porcentaje
  FROM periodos_credito
  GROUP BY incumplimiento
  ORDER BY incumplimiento;
  "
)

print(distribucion_incumplimiento)


validacion_riesgo <- dbGetQuery(
  con,
  "
  SELECT
      incumplimiento,
      ROUND(AVG(riesgo), 4) AS riesgo_promedio,
      ROUND(AVG(mora_dias), 2) AS mora_promedio,
      ROUND(AVG(utilizacion), 4) AS utilizacion_promedio,
      ROUND(AVG(ratio_pago), 4) AS ratio_pago_promedio,
      ROUND(AVG(ingreso), 2) AS ingreso_promedio,
      ROUND(AVG(saldo), 2) AS saldo_promedio
  FROM periodos_credito
  GROUP BY incumplimiento
  ORDER BY incumplimiento;
  "
)

print(validacion_riesgo)


clientes_total <- dbGetQuery(con, "
    SELECT COUNT(*) AS total_clientes
    FROM clientes;
")

print(clientes_total)

creditos_tipo <- dbGetQuery(con, "
    SELECT
        tipo_credito,
        COUNT(*) AS cantidad_creditos
    FROM creditos
    GROUP BY tipo_credito
    ORDER BY cantidad_creditos DESC;
")

print(creditos_tipo)

creditos_periodos <- dbGetQuery(con, "
    SELECT
        id_credito,
        COUNT(*) AS periodos_observados
    FROM periodos_credito
    GROUP BY id_credito
    ORDER BY periodos_observados DESC
    LIMIT 10;
")

print(creditos_periodos)

distribucion_incumplimiento <- dbGetQuery(con, "
    SELECT
        incumplimiento,
        COUNT(*) AS cantidad,
        ROUND(
            COUNT(*) * 100.0 / SUM(COUNT(*)) OVER (),
            2
        ) AS porcentaje
    FROM periodos_credito
    GROUP BY incumplimiento
    ORDER BY incumplimiento;
")

print(distribucion_incumplimiento)


library(ggplot2)
library(dplyr)
grafico_incumplimiento <- dbGetQuery(con, "
    SELECT
        incumplimiento,
        COUNT(*) AS cantidad
    FROM periodos_credito
    GROUP BY incumplimiento
    ORDER BY incumplimiento;
")
grafico_incumplimiento <- grafico_incumplimiento %>%
  mutate(
    estado = ifelse(
      incumplimiento == 0,
      "No incumple",
      "Incumple"
    ),
    porcentaje = cantidad / sum(cantidad) * 100
  )
ggplot(
  grafico_incumplimiento,
  aes(
    x = estado,
    y = porcentaje,
    fill = estado
  )
) +
  geom_col(width = 0.6) +
  geom_text(
    aes(
      label = paste0(
        round(porcentaje, 2),
        "%"
      )
    ),
    vjust = -0.5,
    size = 5
  ) +
  labs(
    title = "Distribución del incumplimiento",
    x = "Estado del crédito",
    y = "Porcentaje de observaciones"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none"
  )


grafico_mora <- dbGetQuery(con, "
    SELECT
        incumplimiento,
        AVG(mora_dias) AS mora_promedio,
        AVG(mora_historica) AS mora_historica_promedio
    FROM periodos_credito
    GROUP BY incumplimiento
    ORDER BY incumplimiento;
")
grafico_mora <- grafico_mora %>%
  mutate(
    estado = ifelse(
      incumplimiento == 0,
      "No incumple",
      "Incumple"
    )
  )
ggplot(
  grafico_mora,
  aes(
    x = estado,
    y = mora_promedio,
    fill = estado
  )
) +
  geom_col(width = 0.6) +
  geom_text(
    aes(
      label = round(
        mora_promedio,
        2
      )
    ),
    vjust = -0.5,
    size = 5
  ) +
  labs(
    title = "Mora promedio según incumplimiento",
    subtitle = "Comparación del comportamiento de pago",
    x = "Estado del crédito",
    y = "Días promedio de mora"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none"
  )

# ============================================================
# Base de datos analitica
# ============================================================
dbExecute(con, "
DROP TABLE IF EXISTS ml_creditos;
")

dbExecute(con, "
CREATE TABLE ml_creditos AS
SELECT
    p.id_periodo,
    p.id_credito,
    c.id_cliente,

    -- Variable temporal
    p.fecha_periodo,
    EXTRACT(YEAR FROM p.fecha_periodo)::INTEGER AS anio,
    EXTRACT(MONTH FROM p.fecha_periodo)::INTEGER AS mes,

    -- Caracteristicas del cliente
    cl.edad,
    cl.ciudad,
    cl.segmento,
    cl.antiguedad_cliente,

    -- Caracteristicas del credito
    c.tipo_credito,
    c.monto_original,
    c.plazo_meses,

    -- Informacion disponible en el periodo
    p.ingreso,
    p.saldo,
    p.cuota,
    p.utilizacion,
    p.mora_dias,
    p.mora_historica,
    p.refinanciado,
    p.tasa_interes,
    p.pago_realizado,
    p.ratio_pago,

    -- Variables derivadas
    CASE
        WHEN p.ingreso > 0
        THEN p.cuota / p.ingreso
        ELSE 0
    END AS ratio_cuota_ingreso,

    CASE
        WHEN c.monto_original > 0
        THEN p.saldo / c.monto_original
        ELSE 0
    END AS ratio_saldo_original,

    -- Variable objetivo
    p.incumplimiento

FROM periodos_credito p

INNER JOIN creditos c
    ON c.id_credito = p.id_credito

INNER JOIN clientes cl
    ON cl.id_cliente = c.id_cliente;
")

dbExecute(con, "
CREATE INDEX IF NOT EXISTS idx_ml_fecha
ON ml_creditos(fecha_periodo);
")

dbExecute(con, "
CREATE INDEX IF NOT EXISTS idx_ml_incumplimiento
ON ml_creditos(incumplimiento);
")

total_ml <- dbGetQuery(con, "
SELECT COUNT(*) AS total_registros
FROM ml_creditos;
")

print(total_ml)

muestra_ml <- dbGetQuery(con, "
SELECT *
FROM ml_creditos
LIMIT 10;
")

print(muestra_ml)

estructura_ml <- dbGetQuery(con, "
SELECT
    column_name,
    data_type
FROM information_schema.columns
WHERE table_name = 'ml_creditos'
ORDER BY ordinal_position;
")

print(estructura_ml)




distribucion_anios <- dbGetQuery(con, "
SELECT
    anio,
    COUNT(*) AS registros,
    SUM(incumplimiento) AS incumplimientos,
    ROUND(
        AVG(incumplimiento) * 100,
        2
    ) AS porcentaje_incumplimiento
FROM ml_creditos
GROUP BY anio
ORDER BY anio;
")

print(distribucion_anios)


library(sparklyr)
library(dplyr)

# Conectar a Spark
sc <- spark_connect(
  master = "local",
  version = "3.5.0"
)

datos_ml_muestra <- dbGetQuery(con, "
SELECT
    anio::INTEGER,
    edad::INTEGER,
    antiguedad_cliente::INTEGER,
    monto_original::DOUBLE PRECISION,
    plazo_meses::INTEGER,
    ingreso::DOUBLE PRECISION,
    saldo::DOUBLE PRECISION,
    cuota::DOUBLE PRECISION,
    utilizacion::DOUBLE PRECISION,
    mora_dias::INTEGER,
    mora_historica::INTEGER,
    refinanciado::INTEGER,
    tasa_interes::DOUBLE PRECISION,
    pago_realizado::DOUBLE PRECISION,
    ratio_pago::DOUBLE PRECISION,
    ratio_cuota_ingreso::DOUBLE PRECISION,
    ratio_saldo_original::DOUBLE PRECISION,
    incumplimiento::INTEGER
FROM ml_creditos
WHERE anio IN (2024, 2025, 2026)
LIMIT 10000;
")
str(datos_ml_muestra)
datos_spark <- copy_to(
  sc,
  datos_ml_muestra,
  "ml_creditos_spark",
  overwrite = TRUE
)
datos_spark %>%
  count() %>%
  collect()

train <- datos_spark %>%
  filter(anio == 2024)

valid <- datos_spark %>%
  filter(anio == 2025)

test <- datos_spark %>%
  filter(anio == 2026)

train %>% count() %>% collect()
valid %>% count() %>% collect()
test %>% count() %>% collect()

formula_ml <- incumplimiento ~
  edad +
  antiguedad_cliente +
  monto_original +
  plazo_meses +
  ingreso +
  saldo +
  cuota +
  utilizacion +
  mora_dias +
  mora_historica +
  refinanciado +
  tasa_interes +
  pago_realizado +
  ratio_pago +
  ratio_cuota_ingreso +
  ratio_saldo_original

modelo_logistico <- ml_logistic_regression(
  train,
  formula_ml
)
pred_logistico <- ml_predict(
  modelo_logistico,
  test
)
pred_logistico %>%
  select(
    incumplimiento,
    prediction,
    probability
  ) %>%
  head() %>%
  collect()
modelo_rf <- ml_random_forest_classifier(
  train,
  formula_ml,
  num_trees = 100,
  max_depth = 8,
  seed = 123
)
pred_rf <- ml_predict(
  modelo_rf,
  test
)
pred_rf %>%
  select(
    incumplimiento,
    prediction,
    probability
  ) %>%
  head() %>%
  collect()

matriz_logistica <- pred_logistico %>%
  group_by(
    incumplimiento,
    prediction
  ) %>%
  summarise(
    cantidad = n(),
    .groups = "drop"
  ) %>%
  collect()

print(matriz_logistica)

matriz_rf <- pred_rf %>%
  group_by(
    incumplimiento,
    prediction
  ) %>%
  summarise(
    cantidad = n(),
    .groups = "drop"
  ) %>%
  collect()

print(matriz_rf)
