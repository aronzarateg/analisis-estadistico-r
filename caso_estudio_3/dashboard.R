# ============================================================
# DASHBOARD SHINY - RIESGO DE CREDITO
# RESUMEN EJECUTIVO + EXPLORACION DE CARTERA
# ============================================================

library(shiny)
library(shinydashboard)
library(DBI)
library(RPostgres)
library(dplyr)
library(ggplot2)


# ============================================================
# 1. CONEXION POSTGRESQL
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
# 2. DATOS GENERALES PARA LOS FILTROS
# ============================================================

anios <- dbGetQuery(con, "
    SELECT DISTINCT anio
    FROM ml_creditos
    WHERE anio BETWEEN 2024 AND 2026
    ORDER BY anio;
")$anio


ciudades <- dbGetQuery(con, "
    SELECT DISTINCT ciudad
    FROM ml_creditos
    WHERE ciudad IS NOT NULL
    ORDER BY ciudad;
")$ciudad


segmentos <- dbGetQuery(con, "
    SELECT DISTINCT segmento
    FROM ml_creditos
    WHERE segmento IS NOT NULL
    ORDER BY segmento;
")$segmento


tipos_credito <- dbGetQuery(con, "
    SELECT DISTINCT tipo_credito
    FROM ml_creditos
    WHERE tipo_credito IS NOT NULL
    ORDER BY tipo_credito;
")$tipo_credito


rangos <- dbGetQuery(con, "
    SELECT
        FLOOR(MIN(ingreso)) AS ingreso_min,
        CEILING(MAX(ingreso)) AS ingreso_max,
        FLOOR(MIN(saldo)) AS saldo_min,
        CEILING(MAX(saldo)) AS saldo_max
    FROM ml_creditos
    WHERE anio BETWEEN 2024 AND 2026;
")


# ============================================================
# 3. RESUMEN GENERAL
# ============================================================

resumen <- dbGetQuery(con, "
    SELECT
        COUNT(DISTINCT id_credito) AS creditos_analizados,
        COUNT(DISTINCT id_cliente) AS clientes_analizados,

        ROUND(
            AVG(incumplimiento) * 100,
            2
        ) AS porcentaje_incumplimiento

    FROM ml_creditos

    WHERE anio BETWEEN 2024 AND 2026;
")


# ============================================================
# 4. PORCENTAJE CON SENAL DE RIESGO
# ============================================================
# Esta es una regla descriptiva.
# No corresponde al resultado del modelo ML.
# ============================================================

riesgo <- dbGetQuery(con, "
    SELECT
        ROUND(
            AVG(
                CASE
                    WHEN mora_dias > 30
                      OR refinanciado = 1
                      OR utilizacion > 0.80
                    THEN 1
                    ELSE 0
                END
            ) * 100,
            2
        ) AS porcentaje_riesgo

    FROM ml_creditos

    WHERE anio BETWEEN 2024 AND 2026;
")


# ============================================================
# 5. RESUMEN POR ANIO
# ============================================================

resumen_anual <- dbGetQuery(con, "
    SELECT
        anio,

        COUNT(*) AS observaciones,

        COUNT(DISTINCT id_credito) AS creditos,

        COUNT(DISTINCT id_cliente) AS clientes,

        ROUND(
            AVG(incumplimiento) * 100,
            2
        ) AS porcentaje_incumplimiento

    FROM ml_creditos

    WHERE anio BETWEEN 2024 AND 2026

    GROUP BY anio

    ORDER BY anio;
")


# ============================================================
# 6. METRICAS DEL MODELO
# ============================================================
# Cuando obtengamos las metricas reales de Spark ML,
# reemplazaremos estos valores.
# ============================================================

auc_modelo <- NA_real_
f1_modelo <- NA_real_
modelo_seleccionado <- "En evaluacion"


# ============================================================
# 7. INTERFAZ
# ============================================================

ui <- dashboardPage(
  
  # ----------------------------------------------------------
  # HEADER
  # ----------------------------------------------------------
  
  dashboardHeader(
    title = "Riesgo de Credito"
  ),
  
  
  # ----------------------------------------------------------
  # MENU LATERAL
  # ----------------------------------------------------------
  
  dashboardSidebar(
    
    sidebarMenu(
      
      menuItem(
        "Resumen Ejecutivo",
        tabName = "resumen",
        icon = icon("dashboard")
      ),
      
      menuItem(
        "Exploracion de cartera",
        tabName = "exploracion",
        icon = icon("search")
      )
      
    )
    
  ),
  
  
  # ----------------------------------------------------------
  # BODY
  # ----------------------------------------------------------
  
  dashboardBody(
    
    tabItems(
      
      # ======================================================
      # TAB 1 - RESUMEN EJECUTIVO
      # ======================================================
      
      tabItem(
        
        tabName = "resumen",
        
        
        fluidRow(
          
          box(
            width = 12,
            title = "Resumen Ejecutivo",
            status = "primary",
            solidHeader = TRUE,
            
            p(
              paste(
                "Indicadores principales del analisis",
                "de riesgo crediticio para los periodos",
                "2024, 2025 y 2026."
              )
            )
          )
          
        ),
        
        
        # ----------------------------------------------------
        # INDICADORES
        # ----------------------------------------------------
        
        fluidRow(
          
          valueBoxOutput(
            "creditos",
            width = 4
          ),
          
          valueBoxOutput(
            "clientes",
            width = 4
          ),
          
          valueBoxOutput(
            "riesgo",
            width = 4
          )
          
        ),
        
        
        fluidRow(
          
          valueBoxOutput(
            "incumplimiento",
            width = 4
          ),
          
          valueBoxOutput(
            "auc",
            width = 4
          ),
          
          valueBoxOutput(
            "f1",
            width = 4
          )
          
        ),
        
        
        # ----------------------------------------------------
        # MODELO
        # ----------------------------------------------------
        
        fluidRow(
          
          box(
            width = 12,
            title = "Modelo seleccionado",
            status = "primary",
            solidHeader = TRUE,
            
            h3(
              textOutput("modelo")
            )
          )
          
        ),
        
        
        # ----------------------------------------------------
        # DISTRIBUCION TEMPORAL
        # ----------------------------------------------------
        
        fluidRow(
          
          box(
            width = 12,
            title = "Distribucion temporal de los datos",
            status = "primary",
            solidHeader = TRUE,
            
            tableOutput(
              "tabla_anual"
            )
          )
          
        )
        
      ),
      
      
      # ======================================================
      # TAB 2 - EXPLORACION DE CARTERA
      # ======================================================
      
      tabItem(
        
        tabName = "exploracion",
        
        fluidRow(
          
          # ==================================================
          # PANEL DE FILTROS
          # ==================================================
          
          box(
            width = 3,
            title = "Filtros",
            status = "primary",
            solidHeader = TRUE,
            
            selectInput(
              "filtro_anio",
              "Periodo:",
              choices = anios,
              selected = anios,
              multiple = TRUE
            ),
            
            selectInput(
              "filtro_ciudad",
              "Ciudad:",
              choices = c(
                "Todas",
                ciudades
              ),
              selected = "Todas"
            ),
            
            selectInput(
              "filtro_segmento",
              "Segmento:",
              choices = c(
                "Todos",
                segmentos
              ),
              selected = "Todos"
            ),
            
            selectInput(
              "filtro_tipo",
              "Tipo de credito:",
              choices = c(
                "Todos",
                tipos_credito
              ),
              selected = "Todos"
            ),
            
            sliderInput(
              "filtro_ingreso",
              "Rango de ingresos:",
              min = rangos$ingreso_min,
              max = rangos$ingreso_max,
              value = c(
                rangos$ingreso_min,
                rangos$ingreso_max
              ),
              step = 100
            ),
            
            sliderInput(
              "filtro_saldo",
              "Rango de saldo:",
              min = rangos$saldo_min,
              max = rangos$saldo_max,
              value = c(
                rangos$saldo_min,
                rangos$saldo_max
              ),
              step = 100
            ),
            
            selectInput(
              "filtro_riesgo",
              "Nivel de riesgo:",
              choices = c(
                "Todos",
                "Bajo",
                "Medio",
                "Alto"
              ),
              selected = "Todos"
            )
            
          ),
          
          
          # ==================================================
          # RESULTADOS DE FILTROS
          # ==================================================
          
          box(
            width = 9,
            title = "Indicadores de la cartera seleccionada",
            status = "primary",
            solidHeader = TRUE,
            
            fluidRow(
              
              valueBoxOutput(
                "f_creditos",
                width = 4
              ),
              
              valueBoxOutput(
                "f_clientes",
                width = 4
              ),
              
              valueBoxOutput(
                "f_incumplimiento",
                width = 4
              )
              
            ),
            
            fluidRow(
              
              column(
                width = 6,
                plotOutput(
                  "grafico_tipo"
                )
              ),
              
              column(
                width = 6,
                plotOutput(
                  "grafico_incumplimiento"
                )
              )
              
            )
            
          )
          
        ),
        
        
        # ----------------------------------------------------
        # TABLA DE RESULTADOS
        # ----------------------------------------------------
        
        fluidRow(
          
          box(
            width = 12,
            title = "Muestra de creditos filtrados",
            status = "primary",
            solidHeader = TRUE,
            
            tableOutput(
              "tabla_creditos"
            )
            
          )
          
        )
        
      )
      
    )
    
  )
  
)


# ============================================================
# 8. SERVIDOR
# ============================================================

server <- function(input, output, session) {
  
  
  # ==========================================================
  # RESUMEN EJECUTIVO
  # ==========================================================
  
  output$creditos <- renderValueBox({
    
    valueBox(
      format(
        resumen$creditos_analizados,
        big.mark = ",",
        scientific = FALSE
      ),
      "Creditos analizados",
      icon = icon("credit-card"),
      color = "blue"
    )
    
  })
  
  
  output$clientes <- renderValueBox({
    
    valueBox(
      format(
        resumen$clientes_analizados,
        big.mark = ",",
        scientific = FALSE
      ),
      "Clientes analizados",
      icon = icon("users"),
      color = "aqua"
    )
    
  })
  
  
  output$riesgo <- renderValueBox({
    
    valueBox(
      paste0(
        riesgo$porcentaje_riesgo,
        "%"
      ),
      "Observaciones con senal de riesgo",
      icon = icon("exclamation-triangle"),
      color = "yellow"
    )
    
  })
  
  
  output$incumplimiento <- renderValueBox({
    
    valueBox(
      paste0(
        resumen$porcentaje_incumplimiento,
        "%"
      ),
      "Incumplimiento real",
      icon = icon("chart-line"),
      color = "red"
    )
    
  })
  
  
  output$auc <- renderValueBox({
    
    valor_auc <- if (is.na(auc_modelo)) {
      "Pendiente"
    } else {
      round(
        auc_modelo,
        3
      )
    }
    
    valueBox(
      valor_auc,
      "ROC-AUC",
      icon = icon("chart-area"),
      color = "green"
    )
    
  })
  
  
  output$f1 <- renderValueBox({
    
    valor_f1 <- if (is.na(f1_modelo)) {
      "Pendiente"
    } else {
      round(
        f1_modelo,
        3
      )
    }
    
    valueBox(
      valor_f1,
      "F1 Score",
      icon = icon("calculator"),
      color = "purple"
    )
    
  })
  
  
  output$modelo <- renderText({
    
    modelo_seleccionado
    
  })
  
  
  output$tabla_anual <- renderTable({
    
    resumen_anual %>%
      
      rename(
        "Año" = anio,
        "Observaciones" = observaciones,
        "Creditos" = creditos,
        "Clientes" = clientes,
        "% Incumplimiento" = porcentaje_incumplimiento
      )
    
  })
  
  
  # ==========================================================
  # CONSTRUIR FILTROS SQL
  # ==========================================================
  
  condicion_sql <- reactive({
    
    req(input$filtro_anio)
    req(input$filtro_ingreso)
    req(input$filtro_saldo)
    
    
    condiciones <- c()
    
    
    # --------------------------------------------------------
    # ANIO
    # --------------------------------------------------------
    
    anios_sql <- paste(
      as.integer(input$filtro_anio),
      collapse = ","
    )
    
    condiciones <- c(
      condiciones,
      paste0(
        "anio IN (",
        anios_sql,
        ")"
      )
    )
    
    
    # --------------------------------------------------------
    # CIUDAD
    # --------------------------------------------------------
    
    if (input$filtro_ciudad != "Todas") {
      
      ciudad_sql <- dbQuoteString(
        con,
        input$filtro_ciudad
      )
      
      condiciones <- c(
        condiciones,
        paste0(
          "ciudad = ",
          ciudad_sql
        )
      )
      
    }
    
    
    # --------------------------------------------------------
    # SEGMENTO
    # --------------------------------------------------------
    
    if (input$filtro_segmento != "Todos") {
      
      segmento_sql <- dbQuoteString(
        con,
        input$filtro_segmento
      )
      
      condiciones <- c(
        condiciones,
        paste0(
          "segmento = ",
          segmento_sql
        )
      )
      
    }
    
    
    # --------------------------------------------------------
    # TIPO DE CREDITO
    # --------------------------------------------------------
    
    if (input$filtro_tipo != "Todos") {
      
      tipo_sql <- dbQuoteString(
        con,
        input$filtro_tipo
      )
      
      condiciones <- c(
        condiciones,
        paste0(
          "tipo_credito = ",
          tipo_sql
        )
      )
      
    }
    
    
    # --------------------------------------------------------
    # INGRESOS
    # --------------------------------------------------------
    
    condiciones <- c(
      condiciones,
      
      paste0(
        "ingreso BETWEEN ",
        input$filtro_ingreso[1],
        " AND ",
        input$filtro_ingreso[2]
      )
    )
    
    
    # --------------------------------------------------------
    # SALDO
    # --------------------------------------------------------
    
    condiciones <- c(
      condiciones,
      
      paste0(
        "saldo BETWEEN ",
        input$filtro_saldo[1],
        " AND ",
        input$filtro_saldo[2]
      )
    )
    
    
    # --------------------------------------------------------
    # NIVEL DE RIESGO
    # --------------------------------------------------------
    #
    # BAJO:
    #   mora <= 0
    #   utilizacion <= 60 %
    #   no refinanciado
    #
    # MEDIO:
    #   mora entre 1 y 30 dias
    #   o utilizacion entre 60 y 80 %
    #
    # ALTO:
    #   mora > 30 dias
    #   o refinanciado
    #   o utilizacion > 80 %
    # --------------------------------------------------------
    
    if (input$filtro_riesgo == "Alto") {
      
      condiciones <- c(
        condiciones,
        "
        (
            mora_dias > 30
            OR refinanciado = 1
            OR utilizacion > 0.80
        )
        "
      )
      
    }
    
    
    if (input$filtro_riesgo == "Medio") {
      
      condiciones <- c(
        condiciones,
        "
        (
            (
                mora_dias BETWEEN 1 AND 30
                OR utilizacion > 0.60
            )
            AND mora_dias <= 30
            AND refinanciado = 0
            AND utilizacion <= 0.80
        )
        "
      )
      
    }
    
    
    if (input$filtro_riesgo == "Bajo") {
      
      condiciones <- c(
        condiciones,
        "
        (
            mora_dias = 0
            AND refinanciado = 0
            AND utilizacion <= 0.60
        )
        "
      )
      
    }
    
    
    paste(
      condiciones,
      collapse = " AND "
    )
    
  })
  
  
  # ==========================================================
  # RESUMEN SEGUN FILTROS
  # ==========================================================
  
  resumen_filtrado <- reactive({
    
    where_sql <- condicion_sql()
    
    consulta <- paste0(
      "
      SELECT

          COUNT(DISTINCT id_credito)
              AS creditos,

          COUNT(DISTINCT id_cliente)
              AS clientes,

          ROUND(
              AVG(incumplimiento) * 100,
              2
          ) AS porcentaje_incumplimiento

      FROM ml_creditos

      WHERE ",
      where_sql,
      ";
      "
    )
    
    
    dbGetQuery(
      con,
      consulta
    )
    
  })
  
  
  # ==========================================================
  # CREDITOS FILTRADOS
  # ==========================================================
  
  output$f_creditos <- renderValueBox({
    
    datos <- resumen_filtrado()
    
    valueBox(
      format(
        datos$creditos,
        big.mark = ","
      ),
      "Creditos",
      icon = icon("credit-card"),
      color = "blue"
    )
    
  })
  
  
  # ==========================================================
  # CLIENTES FILTRADOS
  # ==========================================================
  
  output$f_clientes <- renderValueBox({
    
    datos <- resumen_filtrado()
    
    valueBox(
      format(
        datos$clientes,
        big.mark = ","
      ),
      "Clientes",
      icon = icon("users"),
      color = "aqua"
    )
    
  })
  
  
  # ==========================================================
  # INCUMPLIMIENTO FILTRADO
  # ==========================================================
  
  output$f_incumplimiento <- renderValueBox({
    
    datos <- resumen_filtrado()
    
    valueBox(
      paste0(
        datos$porcentaje_incumplimiento,
        "%"
      ),
      "Incumplimiento",
      icon = icon("exclamation-triangle"),
      color = "red"
    )
    
  })
  
  
  # ==========================================================
  # GRAFICO POR TIPO DE CREDITO
  # ==========================================================
  
  output$grafico_tipo <- renderPlot({
    
    where_sql <- condicion_sql()
    
    
    consulta <- paste0(
      "
      SELECT

          tipo_credito,

          COUNT(DISTINCT id_credito)
              AS cantidad

      FROM ml_creditos

      WHERE ",
      where_sql,
      "

      GROUP BY tipo_credito

      ORDER BY cantidad DESC;
      "
    )
    
    
    datos <- dbGetQuery(
      con,
      consulta
    )
    
    
    ggplot(
      datos,
      aes(
        x = reorder(
          tipo_credito,
          cantidad
        ),
        y = cantidad
      )
    ) +
      
      geom_col() +
      
      coord_flip() +
      
      labs(
        title = "Creditos por tipo",
        x = "Tipo de credito",
        y = "Cantidad"
      ) +
      
      theme_minimal()
    
  })
  
  
  # ==========================================================
  # GRAFICO DE INCUMPLIMIENTO
  # ==========================================================
  
  output$grafico_incumplimiento <- renderPlot({
    
    where_sql <- condicion_sql()
    
    
    consulta <- paste0(
      "
      SELECT

          incumplimiento,

          COUNT(*) AS cantidad

      FROM ml_creditos

      WHERE ",
      where_sql,
      "

      GROUP BY incumplimiento

      ORDER BY incumplimiento;
      "
    )
    
    
    datos <- dbGetQuery(
      con,
      consulta
    )
    
    
    datos <- datos %>%
      
      mutate(
        
        Estado = ifelse(
          incumplimiento == 1,
          "Incumple",
          "No incumple"
        ),
        
        porcentaje =
          cantidad /
          sum(cantidad) *
          100
        
      )
    
    
    ggplot(
      datos,
      aes(
        x = Estado,
        y = porcentaje
      )
    ) +
      
      geom_col(
        width = 0.6
      ) +
      
      geom_text(
        aes(
          label = paste0(
            round(
              porcentaje,
              2
            ),
            "%"
          )
        ),
        vjust = -0.5
      ) +
      
      labs(
        title = "Distribucion de incumplimiento",
        x = "",
        y = "Porcentaje"
      ) +
      
      theme_minimal()
    
  })
  
  
  # ==========================================================
  # TABLA DE DATOS
  # ==========================================================
  
  output$tabla_creditos <- renderTable({
    
    where_sql <- condicion_sql()
    
    
    consulta <- paste0(
      "
      SELECT

          id_credito,
          id_cliente,
          anio,
          ciudad,
          segmento,
          tipo_credito,

          ROUND(ingreso, 2)
              AS ingreso,

          ROUND(saldo, 2)
              AS saldo,

          mora_dias,

          ROUND(utilizacion * 100, 2)
              AS utilizacion_pct,

          refinanciado,

          incumplimiento

      FROM ml_creditos

      WHERE ",
      where_sql,
      "

      ORDER BY id_credito

      LIMIT 20;
      "
    )
    
    
    dbGetQuery(
      con,
      consulta
    )
    
  })
  
}


# ============================================================
# 9. CERRAR CONEXION CUANDO TERMINE SHINY
# ============================================================

onStop(
  function() {
    
    if (
      DBI::dbIsValid(con)
    ) {
      
      DBI::dbDisconnect(con)
      
    }
    
  }
)


# ============================================================
# 10. EJECUTAR DASHBOARD
# ============================================================

shinyApp(
  ui = ui,
  server = server
)
