# ============================================================
# server_01_core_api.R
# Purpose: Database connection, station state, navigation, help modals, and session-only ANA token/download state.
# ============================================================
# BEGIN ORIGINAL BODY
  con <- connect_shiny_database()
  session$onSessionEnded(function() disconnect_shiny_database(con))
  
  station_index <- load_station_index(con)
  spatial_layers <- load_spatial_layers(app_config$spatial_layers_path)
  source_metadata <- load_source_metadata(con)
  available_tables <- tibble::tibble(table_or_view = app_db_tables(con))
  
  default_map_station_layers <- c("flu_stations", "rainfall_stations")
  
  selectable_map_station_layers <- c(
    "flu_stations",
    "rainfall_stations"
  )
  
  normalize_map_station_layers <- function(selected_layers = default_map_station_layers) {
    if (is.null(selected_layers)) {
      return(character(0))
    }
    
    selected_layers <- as.character(selected_layers)
    
    if (length(selected_layers) == 0) {
      return(character(0))
    }
    
    intersect(selected_layers, selectable_map_station_layers)
  }
  
  map_logical_field <- function(data, field) {
    if (!field %in% names(data)) {
      return(rep(FALSE, nrow(data)))
    }
    
    dplyr::coalesce(as.logical(data[[field]]), FALSE)
  }
  
  map_date_field_available <- function(data, field) {
    if (!field %in% names(data)) {
      return(rep(FALSE, nrow(data)))
    }
    
    !is.na(data[[field]])
  }
  
  map_station_type_text <- function(data) {
    if (!"station_type" %in% names(data)) {
      return(rep("", nrow(data)))
    }
    
    stringr::str_to_lower(as.character(dplyr::coalesce(data$station_type, "")))
  }
  
  map_is_flu_station <- function(data) {
    station_type_text <- map_station_type_text(data)
    
    stringr::str_detect(station_type_text, "fluvi") |
      map_logical_field(data, "has_inventory_flu_data") |
      map_logical_field(data, "has_product_flu_data") |
      map_logical_field(data, "has_product_discharge_summary") |
      map_logical_field(data, "has_product_rating_curves") |
      map_logical_field(data, "has_product_cross_sections") |
      map_date_field_available(data, "discharge_start_date") |
      map_date_field_available(data, "stage_start_date")
  }
  
  map_is_rainfall_station <- function(data) {
    station_type_text <- map_station_type_text(data)
    
    stringr::str_detect(station_type_text, "pluvi") |
      map_logical_field(data, "has_inventory_rainfall_data") |
      map_logical_field(data, "has_product_rainfall_data") |
      map_logical_field(data, "has_rainfall_data") |
      map_date_field_available(data, "rainfall_start_date")
  }
  
  map_has_flu_data <- function(data) {
    map_logical_field(data, "has_product_discharge_summary") |
      map_logical_field(data, "has_product_rating_curves") |
      map_logical_field(data, "has_product_cross_sections") |
      map_logical_field(data, "has_product_flu_data") |
      map_logical_field(data, "has_discharge_data") |
      map_logical_field(data, "has_discharge_measurements") |
      map_date_field_available(data, "discharge_start_date")
  }
  
  map_has_rainfall_data <- function(data) {
    map_logical_field(data, "has_rainfall_data") |
      map_logical_field(data, "has_product_rainfall_data") |
      map_date_field_available(data, "rainfall_start_date")
  }
  
  station_matches_map_layers <- function(data, selected_layers) {
    selected_layers <- normalize_map_station_layers(selected_layers)
    
    if (length(selected_layers) == 0) {
      return(rep(FALSE, nrow(data)))
    }
    
    is_flu_station <- map_is_flu_station(data)
    is_rainfall_station <- map_is_rainfall_station(data)
    
    include_flu <- "flu_stations" %in% selected_layers
    include_rainfall <- "rainfall_stations" %in% selected_layers
    
    (include_flu & is_flu_station) |
      (include_rainfall & is_rainfall_station)
  }
  
  assign_station_display_layer <- function(data, selected_layers) {
    selected_layers <- normalize_map_station_layers(selected_layers)
    
    if (length(selected_layers) == 0) {
      return(rep(NA_character_, nrow(data)))
    }
    
    is_flu_station <- map_is_flu_station(data)
    is_rainfall_station <- map_is_rainfall_station(data)
    has_flu_data <- map_has_flu_data(data)
    has_rainfall_data <- map_has_rainfall_data(data)
    
    include_flu <- "flu_stations" %in% selected_layers
    include_rainfall <- "rainfall_stations" %in% selected_layers
    
    dplyr::case_when(
      include_flu & include_rainfall & is_flu_station & is_rainfall_station &
        (has_flu_data | has_rainfall_data) ~ "flu_rainfall_with_data",
      
      include_flu & include_rainfall & is_flu_station & is_rainfall_station ~
        "flu_rainfall_registration",
      
      include_flu & is_flu_station & has_flu_data ~
        "flu_with_data",
      
      include_flu & is_flu_station ~
        "flu_registration",
      
      include_rainfall & is_rainfall_station & has_rainfall_data ~
        "rainfall_with_data",
      
      include_rainfall & is_rainfall_station ~
        "rainfall_registration",
      
      TRUE ~ NA_character_
    )
  }
  
  station_choices_for_map_visibility <- function(selected_map_layers = default_map_station_layers) {
    selected_layers <- normalize_map_station_layers(selected_map_layers)
    
    if (length(selected_layers) == 0) {
      return(stats::setNames(character(0), character(0)))
    }
    
    choices_data <- station_index
    choices_data <- choices_data[station_matches_map_layers(choices_data, selected_layers), , drop = FALSE]
    choices_data$display_map_layer <- assign_station_display_layer(choices_data, selected_layers)
    
    choices_data <- choices_data |>
      dplyr::filter(!is.na(display_map_layer)) |>
      dplyr::arrange(map_product_layer_priority_value(display_map_layer), station_code)
    
    stats::setNames(
      choices_data$station_code,
      choices_data$station_search_label
    )
  }
  
  station_choices <- station_choices_for_map_visibility(default_map_station_layers)
  
  first_station <- station_index %>%
    dplyr::filter(has_product_rating_curves | has_product_discharge_summary) %>%
    dplyr::arrange(dplyr::desc(has_product_rating_curves), station_code) %>%
    dplyr::pull(station_code) %>%
    head(1)
  
  if (length(first_station) == 0) {
    station_choices <- stats::setNames(
      station_index$station_code,
      station_index$station_search_label
    )
    first_station <- station_index$station_code[[1]]
  }
  
  updateSelectizeInput(
    session,
    inputId = "station_select",
    choices = station_choices,
    selected = first_station,
    server = TRUE
  )
  
  selected_station_code <- reactiveVal(as.character(first_station))
  
  pending_station_code <- reactiveVal(NULL)
  
  loaded_session_station_code <- reactiveVal(NULL)
  loaded_session_data_type <- reactiveVal(NULL)
  
  station_label_for_code <- function(code) {
    label <- station_index |>
      dplyr::filter(station_code == as.character(code)) |>
      dplyr::slice(1) |>
      dplyr::pull(station_search_label)
    
    if (length(label) == 0 || is.na(label) || label == "") {
      return(as.character(code))
    }
    
    as.character(label)
  }
  
  station_flag_value <- function(station_row, field) {
    if (!field %in% names(station_row)) {
      return(FALSE)
    }
    
    value <- station_row[[field]][1]
    isTRUE(as.logical(value))
  }
  
  station_default_acquisition_module <- function(code) {
    station_row <- station_index |>
      dplyr::filter(station_code == as.character(code)) |>
      dplyr::slice(1)
    
    if (nrow(station_row) == 0) {
      return("flu")
    }
    
    has_flu <- station_flag_value(station_row, "has_inventory_flu_data") ||
      station_flag_value(station_row, "has_product_flu_data")
    
    has_plu <- station_flag_value(station_row, "has_inventory_rainfall_data") ||
      station_flag_value(station_row, "has_product_rainfall_data")
    
    if (isTRUE(has_plu) && !isTRUE(has_flu)) {
      return("plu")
    }
    
    "flu"
  }
  
  station_choices_with_selected <- function(code) {
    choices <- station_choices_for_map_visibility(input$map_station_layers)
    code <- as.character(code)
    
    if (!code %in% unname(choices)) {
      choices <- c(stats::setNames(code, station_label_for_code(code)), choices)
    }
    
    choices
  }
  
  session_has_loaded_series <- function() {
    !is.null(loaded_session_station_code())
  }
  
  go_to_acquisition_for_station <- function(code) {
    target <- station_default_acquisition_module(code)
    
    if (identical(target, "plu")) {
      updateTabsetPanel(session, "main_tabs", selected = "Análise de dados pluviométricos")
      updateTabsetPanel(session, "pluviometric_tabs", selected = "Obtenção de dados")
    } else {
      updateTabsetPanel(session, "main_tabs", selected = "Análise de dados fluviométricos")
      updateTabsetPanel(session, "fluviometric_tabs", selected = "Obtenção de dados")
    }
  }
  
  show_station_change_modal <- function(new_code) {
    old_code <- loaded_session_station_code()
    if (is.null(old_code)) {
      old_code <- selected_station_code()
    }
    
    showModal(
      modalDialog(
        title = "Trocar estação selecionada?",
        tags$p(
          "Há dados carregados na sessão para o posto atualmente selecionado:"
        ),
        tags$div(
          class = "table-status warning",
          tags$strong(station_label_for_code(old_code))
        ),
        tags$p(
          "O novo posto selecionado é:"
        ),
        tags$div(
          class = "table-status empty",
          tags$strong(station_label_for_code(new_code))
        ),
        tags$p(
          "Se continuar, os dados carregados na sessão serão descartados e será necessário obter ou enviar novamente os dados para o novo posto."
        ),
        footer = tagList(
          actionButton(
            inputId = "cancel_station_change",
            label = "Não, manter posto atual",
            class = "btn-default"
          ),
          actionButton(
            inputId = "confirm_station_change",
            label = "Sim, trocar posto",
            class = "btn-primary"
          )
        ),
        easyClose = FALSE
      )
    )
  }
  
  request_station_change <- function(new_code, update_selector = FALSE) {
    new_code <- as.character(new_code)
    current_code <- as.character(selected_station_code())
    
    if (length(new_code) == 0 || is.na(new_code) || new_code == "") {
      return(invisible(NULL))
    }
    
    if (identical(new_code, current_code)) {
      return(invisible(NULL))
    }
    
    if (session_has_loaded_series()) {
      pending_station_code(new_code)
      
      updateSelectizeInput(
        session,
        inputId = "station_select",
        choices = station_choices_with_selected(current_code),
        selected = current_code,
        server = TRUE
      )
      
      show_station_change_modal(new_code)
      return(invisible(NULL))
    }
    
    selected_station_code(new_code)
    
    if (isTRUE(update_selector)) {
      updateSelectizeInput(
        session,
        inputId = "station_select",
        choices = station_choices_with_selected(new_code),
        selected = new_code,
        server = TRUE
      )
    }
    
    invisible(NULL)
  }
  
  observeEvent(input$station_select, {
    req(input$station_select)
    request_station_change(input$station_select, update_selector = FALSE)
  }, ignoreInit = TRUE)
  
  selected_code <- reactive({
    req(selected_station_code())
    as.character(selected_station_code())
  })
  
  observeEvent(input$map_station_layers, {
    selected_layers <- normalize_map_station_layers(input$map_station_layers)
    
    if (is.null(input$map_station_layers) || length(input$map_station_layers) == 0) {
      updateCheckboxGroupInput(
        session,
        inputId = "map_station_layers",
        selected = selected_layers
      )
    }
    
    visible_choices <- station_choices_for_map_visibility(selected_layers)
    visible_codes <- unname(visible_choices)
    
    if (length(visible_codes) == 0) {
      return(NULL)
    }
    
    selected_for_dropdown <- selected_station_code()
    
    if (!(selected_for_dropdown %in% visible_codes)) {
      selected_label <- station_index %>%
        dplyr::filter(station_code == selected_for_dropdown) %>%
        dplyr::slice(1) %>%
        dplyr::pull(station_search_label)
      
      if (length(selected_label) == 0 || is.na(selected_label)) {
        selected_label <- selected_for_dropdown
      }
      
      current_choice <- stats::setNames(selected_for_dropdown, selected_label)
      visible_choices <- c(current_choice, visible_choices)
    }
    
    updateSelectizeInput(
      session,
      inputId = "station_select",
      choices = visible_choices,
      selected = selected_for_dropdown,
      server = TRUE
    )
  }, ignoreInit = TRUE)
  
  output$map_spatial_layer_controls <- renderUI({
    available_layer_keys <- intersect(spatial_map_layer_order, names(spatial_layers))
    
    if (length(available_layer_keys) == 0) {
      return(tags$div(
        class = "map-layer-empty",
        "Camadas espaciais ainda não carregadas. Quando o arquivo exports/spatial_layers/shiny_spatial_layers.rds existir, os controles aparecerão aqui."
      ))
    }
    
    checkboxGroupInput(
      inputId = "map_spatial_layers",
      label = NULL,
      choices = stats::setNames(
        available_layer_keys,
        unlist(spatial_map_groups[available_layer_keys])
      ),
      selected = intersect(spatial_map_default_layers, available_layer_keys)
    )
  })
  
  observeEvent(input$go_to_map, {
    updateTabsetPanel(session, "main_tabs", selected = "Mapa")
  }, ignoreInit = TRUE)
  
  observeEvent(input$open_about_modal, {
    showModal(
      modalDialog(
        title = "Sobre o HydroStat Data Explorer",
        uiOutput("source_limitations"),
        easyClose = TRUE,
        size = "l",
        footer = modalButton("Fechar")
      )
    )
  }, ignoreInit = TRUE)
  
  observeEvent(input$open_diagnostic_help_modal, {
    showModal(
      modalDialog(
        title = "Ajuda para interpretação dos diagnósticos",
        uiOutput("diagnostic_help_content"),
        easyClose = TRUE,
        size = "l",
        footer = modalButton("Fechar")
      )
    )
  }, ignoreInit = TRUE)
  
  observeEvent(input$open_fluviometric_help_modal, {
    showModal(
      modalDialog(
        title = "Ajuda — Análise de dados fluviométricos",
        uiOutput("fluviometric_help_content"),
        easyClose = TRUE,
        size = "l",
        footer = modalButton("Fechar")
      )
    )
  }, ignoreInit = TRUE)
  
  observeEvent(input$open_pluviometric_help_modal, {
    showModal(
      modalDialog(
        title = "Ajuda — Análise de dados pluviométricos",
        uiOutput("pluviometric_help_content"),
        easyClose = TRUE,
        size = "l",
        footer = modalButton("Fechar")
      )
    )
  }, ignoreInit = TRUE)
  
  output$diagnostic_help_content <- renderUI({
    tagList(
      div(
        class = "diagnostic-help-modal",
        
        h4("O que esta análise representa"),
        p(
          "A análise diagnóstica foi criada para auxiliar a triagem e a revisão visual de estações hidrométricas ",
          "com medições de descarga e curvas-chave disponíveis no aplicativo."
        ),
        p(
          "Os resultados devem ser interpretados como sinais de atenção. Eles ajudam a identificar medições, ",
          "curvas-chave ou períodos que merecem revisão visual, mas não representam uma classificação oficial ",
          "de qualidade da estação."
        ),
        
        tags$hr(),
        
        tags$details(
          class = "diagnostic-help-details",
          open = "open",
          tags$summary("Variáveis usadas nos índices"),
          tags$table(
            class = "diagnostic-help-table",
            tags$thead(
              tags$tr(
                tags$th("Símbolo"),
                tags$th("Significado")
              )
            ),
            tags$tbody(
              tags$tr(tags$td("H"), tags$td("Cota observada na medição.")),
              tags$tr(tags$td("Qobs"), tags$td("Vazão medida em campo.")),
              tags$tr(tags$td("Qcc"), tags$td("Vazão estimada pela curva-chave.")),
              tags$tr(tags$td("N"), tags$td("Número total de medições de descarga avaliadas.")),
              tags$tr(tags$td("Nv"), tags$td("Número de medições válidas, com cota e vazão positivas.")),
              tags$tr(tags$td("Np"), tags$td("Número de medições válidas pareadas com algum segmento de curva-chave."))
            )
          )
        ),
        
        tags$details(
          class = "diagnostic-help-details",
          open = "open",
          tags$summary("Índices de medições"),
          
          h5("Fração de cotas ≤ 0"),
          p(tags$strong("Sigla sugerida: "), "FH0"),
          p("Indica a proporção de medições de descarga com cota menor ou igual a zero."),
          div(class = "diagnostic-help-equation", "FH0 = n(H ≤ 0) / N"),
          p(
            "Valores próximos de zero indicam que praticamente não há medições sinalizadas por esse critério. ",
            "Valores maiores que zero indicam medições que devem ser verificadas."
          ),
          
          h5("Fração de vazões ≤ 0"),
          p(tags$strong("Sigla sugerida: "), "FQ0"),
          p("Indica a proporção de medições de descarga com vazão menor ou igual a zero."),
          div(class = "diagnostic-help-equation", "FQ0 = n(Qobs ≤ 0) / N"),
          p(
            "Vazões menores ou iguais a zero são sinais fortes de atenção em medições de descarga. ",
            "Essas medições devem ser verificadas antes de qualquer interpretação hidrológica."
          ),
          
          h5("Fração de cotas repetidas com vazão variável"),
          p(tags$strong("Sigla sugerida: "), "FHRQ"),
          p("Identifica medições que pertencem a grupos com mesma cota, mas com vazões diferentes."),
          div(class = "diagnostic-help-equation", "FHRQ = n(medições em grupos de cota repetida com vazão variável) / N"),
          p(
            "Valores elevados podem indicar arredondamento excessivo, erro de preenchimento, alteração hidráulica, ",
            "histerese ou comportamento complexo da seção."
          ),
          
          h5("Fração de vazões repetidas com cota variável"),
          p(tags$strong("Sigla sugerida: "), "FQRH"),
          p("Identifica medições que pertencem a grupos com mesma vazão, mas com cotas diferentes."),
          div(class = "diagnostic-help-equation", "FQRH = n(medições em grupos de vazão repetida com cota variável) / N"),
          p(
            "Valores elevados podem indicar repetição artificial de vazões, arredondamento excessivo, ",
            "inconsistência de preenchimento ou mudanças reais na relação cota-vazão."
          )
        ),
        
        tags$details(
          class = "diagnostic-help-details",
          open = "open",
          tags$summary("Índices de curva-chave"),
          
          h5("Fração pareada com curva-chave"),
          p(tags$strong("Sigla sugerida: "), "FPC"),
          p(
            "Mede a proporção de medições válidas que puderam ser pareadas com algum segmento de curva-chave. ",
            "Uma medição é pareada quando sua data e sua cota estão dentro da janela de validade e da faixa de cotas ",
            "de um segmento de curva-chave."
          ),
          div(class = "diagnostic-help-equation", "FPC = Np / Nv"),
          tags$table(
            class = "diagnostic-help-table",
            tags$thead(
              tags$tr(tags$th("Valor de FPC"), tags$th("Interpretação"))
            ),
            tags$tbody(
              tags$tr(tags$td("FPC ≥ 0,90"), tags$td("Alta cobertura das medições pelas curvas-chave.")),
              tags$tr(tags$td("0,60 ≤ FPC < 0,90"), tags$td("Cobertura moderada.")),
              tags$tr(tags$td("FPC < 0,60"), tags$td("Baixa cobertura; muitas medições não estão cobertas por curvas-chave válidas."))
            )
          ),
          
          h5("Mediana do resíduo logarítmico absoluto"),
          p(tags$strong("Sigla sugerida: "), "RLA"),
          p("Mede a diferença típica entre a vazão medida e a vazão estimada pela curva-chave."),
          div(class = "diagnostic-help-equation", "r = log(Qobs) − log(Qcc)"),
          div(class = "diagnostic-help-equation", "RLA = mediana(|r|)"),
          tags$table(
            class = "diagnostic-help-table",
            tags$thead(
              tags$tr(
                tags$th("Valor de RLA"),
                tags$th("Diferença relativa aproximada"),
                tags$th("Interpretação")
              )
            ),
            tags$tbody(
              tags$tr(tags$td("< 0,10"), tags$td("menor que ~10%"), tags$td("Baixo desvio típico.")),
              tags$tr(tags$td("0,10 a 0,25"), tags$td("~10% a ~28%"), tags$td("Desvio moderado.")),
              tags$tr(tags$td("0,25 a 0,50"), tags$td("~28% a ~65%"), tags$td("Desvio alto.")),
              tags$tr(tags$td("≥ 0,50"), tags$td("maior que ~65%"), tags$td("Desvio muito alto."))
            )
          ),
          p("A diferença relativa aproximada pode ser estimada por:"),
          div(class = "diagnostic-help-equation", "diferença relativa ≈ exp(RLA) − 1"),
          
          h5("Fração fora do envelope empírico"),
          p(tags$strong("Sigla sugerida: "), "FEE"),
          p("Indica a proporção de medições pareadas que ficaram fora do envelope empírico de resíduos."),
          div(class = "diagnostic-help-equation", "FEE = n(medições fora do envelope) / n(medições com envelope calculado)"),
          p(
            "Medições fora do envelope devem ser revisadas visualmente. O envelope é uma referência empírica ",
            "calculada a partir dos próprios resíduos observados e não representa intervalo oficial de confiança."
          )
        ),
        
        tags$details(
          class = "diagnostic-help-details",
          tags$summary("Regimes temporais"),
          
          h5("Evidência de regimes temporais nos resíduos"),
          p(tags$strong("Sigla sugerida: "), "ERT"),
          p(
            "Avalia se os resíduos da relação cota-vazão apresentam mudanças persistentes ao longo do tempo. ",
            "A análise usa uma curva de referência do tipo:"
          ),
          div(class = "diagnostic-help-equation", "Q = a(H − h0)^b"),
          tags$table(
            class = "diagnostic-help-table",
            tags$thead(
              tags$tr(tags$th("Classe"), tags$th("Interpretação"))
            ),
            tags$tbody(
              tags$tr(tags$td("Sem evidência"), tags$td("Não há sinal claro de mudança temporal nos resíduos.")),
              tags$tr(tags$td("Evidência fraca"), tags$td("Há algum sinal, mas ele deve ser interpretado com cautela.")),
              tags$tr(tags$td("Evidência moderada"), tags$td("Há sinal relevante de mudança temporal; recomenda-se inspeção visual.")),
              tags$tr(tags$td("Evidência forte"), tags$td("Há sinal forte de mudança temporal; a estação deve ser priorizada para revisão."))
            )
          ),
          p(
            "Regimes temporais podem estar associados a alterações na seção, mudanças de controle hidráulico, ",
            "mudanças de método, inconsistências nos dados ou lacunas nas curvas-chave."
          )
        ),
        
        tags$details(
          class = "diagnostic-help-details",
          tags$summary("Escore de atenção diagnóstica"),
          
          h5("Escore de atenção diagnóstica"),
          p(tags$strong("Sigla sugerida: "), "EAD"),
          p(
            "Resume vários sinais de atenção em um único valor. Ele serve para priorizar a revisão visual da estação ",
            "e não deve ser interpretado como nota de qualidade hidrológica."
          ),
          p("O escore aumenta quando são encontrados sinais como:"),
          tags$ul(
            tags$li("fração relevante de cotas ≤ 0;"),
            tags$li("fração relevante de vazões ≤ 0;"),
            tags$li("fração relevante de grupos repetidos;"),
            tags$li("baixa fração de pareamento com curva-chave;"),
            tags$li("resíduos elevados;"),
            tags$li("fração relevante de medições fora do envelope empírico.")
          ),
          tags$table(
            class = "diagnostic-help-table",
            tags$thead(
              tags$tr(tags$th("Classe"), tags$th("Significado"))
            ),
            tags$tbody(
              tags$tr(tags$td("Atenção baixa"), tags$td("Poucos sinais de atenção foram detectados.")),
              tags$tr(tags$td("Atenção moderada"), tags$td("Há sinais que justificam revisão visual.")),
              tags$tr(tags$td("Atenção alta"), tags$td("A estação deve ser priorizada para inspeção detalhada."))
            )
          )
        ),
        
        tags$details(
          class = "diagnostic-help-details",
          tags$summary("Como usar os diagnósticos"),
          
          p("Uma sequência recomendada de leitura é:"),
          tags$ol(
            tags$li("verificar se há medições de descarga e curvas-chave disponíveis;"),
            tags$li("observar o número de medições válidas;"),
            tags$li("verificar a fração pareada com curva-chave;"),
            tags$li("analisar o gráfico de curvas-chave e medições;"),
            tags$li("observar os resíduos e os envelopes empíricos;"),
            tags$li("verificar grupos repetidos e medições sinalizadas;"),
            tags$li("avaliar se há evidência de regimes temporais;"),
            tags$li("interpretar o escore de atenção diagnóstica como prioridade de revisão.")
          ),
          p(tags$strong("Use termos como: "), "sinal de triagem, atenção diagnóstica, medição sinalizada, possível mudança temporal e revisão visual recomendada."),
          p(tags$strong("Evite interpretar como: "), "erro definitivo, estação boa ou ruim, classificação oficial ou rejeição automática de dados.")
        )
      )
    )
  })
  
  output$fluviometric_help_content <- renderUI({
    tagList(
      div(
        class = "diagnostic-help-modal",
        
        h4("O que este módulo faz"),
        p(
          "Este módulo analisa séries diárias de vazão carregadas durante a sessão. ",
          "Os dados podem ser enviados pelo usuário ou obtidos por download público quando disponível. ",
          "As séries não são gravadas no banco local do aplicativo."
        ),
        
        tags$hr(),
        
        tags$details(
          class = "diagnostic-help-details",
          open = "open",
          tags$summary("Obtenção de dados"),
          p(
            "A subaba de obtenção aceita arquivos do HidroWeb, arquivos CSV de vazão, XML do WebService da ANA ",
            "e JSON previamente obtido pelo usuário. O código da estação no arquivo deve coincidir com o posto selecionado."
          ),
          p(
            "Quando possível, o app também tenta obter cotas associadas à série de vazão para apoiar as análises de consistência ",
            "entre cota, vazão e curva-chave."
          )
        ),
        
        tags$details(
          class = "diagnostic-help-details",
          open = "open",
          tags$summary("Séries de vazões"),
          p(
            "A subaba de séries mostra o hidrograma diário da vazão carregada na sessão. ",
            "O gráfico é exploratório e serve para inspeção visual de valores, períodos sem dados e comportamento geral da série."
          )
        ),
        
        tags$details(
          class = "diagnostic-help-details",
          open = "open",
          tags$summary("Falhas e consistência"),
          p(
            "A análise de consistência combina vazão diária, cota diária quando disponível e curvas-chave cadastradas no aplicativo. ",
            "Ela identifica falhas, períodos sem cota ou sem vazão, vazões fora da cobertura de curva-chave, cotas fora da faixa de validade ",
            "e diferenças entre vazão informada e vazão estimada pela curva-chave."
          ),
          p(
            "Os resultados são sinais de atenção para revisão visual. Eles não excluem automaticamente nenhum dado."
          )
        ),
        
        tags$details(
          class = "diagnostic-help-details",
          open = "open",
          tags$summary("Estatísticas mensais e anuais"),
          p(
            "Esta subaba calcula estatísticas descritivas da série diária, incluindo médias anuais, regime mensal médio, curvas de permanência, ",
            "curva de regularização e diagrama de Rippl para a janela crítica quando há dados suficientes."
          ),
          p(
            "Alguns produtos exigem uma extensão mínima de série válida. Quando essa condição não é atendida, o app informa a limitação."
          )
        ),
        
        tags$details(
          class = "diagnostic-help-details",
          open = "open",
          tags$summary("Eventos extremos"),
          p(
            "A subaba de eventos extremos extrai máximos anuais, mínimas anuais para durações fixas e eventos POT. ",
            "As análises são descritivas e incluem flags de triagem para auxiliar a revisão dos eventos."
          ),
          p(
            "Não há ajuste de distribuição de probabilidade, estimativa de tempo de retorno ou modelagem de frequência nesta versão."
          )
        )
      )
    )
  })
  
  output$pluviometric_help_content <- renderUI({
    tagList(
      div(
        class = "diagnostic-help-modal",
        
        h4("O que este módulo faz"),
        p(
          "Este módulo analisa séries diárias de precipitação carregadas durante a sessão. ",
          "Os dados podem ser enviados pelo usuário ou obtidos por download público quando disponível. ",
          "As séries não são gravadas no banco local do aplicativo."
        ),
        
        tags$hr(),
        
        tags$details(
          class = "diagnostic-help-details",
          open = "open",
          tags$summary("Obtenção de dados"),
          p(
            "A subaba de obtenção aceita arquivos ZIP do HidroWeb, CSV de chuvas, XML do WebService da ANA ",
            "e JSON previamente obtido pelo usuário. O código da estação no arquivo deve coincidir com o posto selecionado."
          ),
          p(
            "A série diária analítica combina níveis de consistência quando aplicável, priorizando dados consistidos sobre dados brutos ",
            "para a mesma data."
          )
        ),
        
        tags$details(
          class = "diagnostic-help-details",
          open = "open",
          tags$summary("Séries de precipitação"),
          p(
            "A subaba de séries mostra o hietograma diário da precipitação carregada na sessão. ",
            "O gráfico permite revisar visualmente dias chuvosos, períodos secos, falhas e valores extremos."
          )
        ),
        
        tags$details(
          class = "diagnostic-help-details",
          open = "open",
          tags$summary("Falhas e consistência"),
          p(
            "A análise de falhas e consistência resume a cobertura da série diária, identifica dias sem dados, anos incompletos ",
            "e ocorrências que merecem revisão visual."
          ),
          p(
            "Esses indicadores são descritivos. Eles não classificam oficialmente a estação e não removem dados automaticamente."
          )
        ),
        
        tags$details(
          class = "diagnostic-help-details",
          open = "open",
          tags$summary("Estatísticas mensais e anuais"),
          p(
            "Esta subaba calcula totais mensais e índices anuais de precipitação, incluindo acumulado anual, número de dias com chuva ",
            "e métricas de concentração ou distribuição temporal quando disponíveis."
          ),
          p(
            "As estatísticas dependem da cobertura da série. Anos ou meses incompletos devem ser interpretados com cautela."
          )
        ),
        
        tags$details(
          class = "diagnostic-help-details",
          open = "open",
          tags$summary("Eventos extremos"),
          p(
            "A subaba de eventos extremos extrai máximos anuais de precipitação por ano hidrológico outubro/setembro. ",
            "Nesta versão, a duração é fixa em 1 dia."
          ),
          p(
            "Os resultados são descritivos. Não há ajuste de distribuição, tempo de retorno ou modelagem de frequência."
          )
        )
      )
    )
  })
  
  selected_station <- reactive({
    req(selected_code())
    station_index %>%
      dplyr::filter(station_code == selected_code()) %>%
      dplyr::slice(1)
  })
  
  ana_api_token <- reactiveVal(NULL)
  ana_api_token_created_at <- reactiveVal(NULL)
  ana_api_token_expires_at <- reactiveVal(NULL)
  ana_api_token_message <- reactiveVal("Nenhum token obtido nesta sessão.")
  ana_download_state <- reactiveVal(NULL)
  
  ana_api_token_is_valid <- function() {
    token <- ana_api_token()
    expires_at <- ana_api_token_expires_at()
    
    !is.null(token) &&
      !is.na(token) &&
      token != "" &&
      !is.null(expires_at) &&
      Sys.time() < expires_at
  }
  
  render_ana_api_token_status <- function() {
    token <- ana_api_token()
    expires_at <- ana_api_token_expires_at()
    
    if (is.null(token) || token == "") {
      return(div(
        class = "table-status empty",
        "Nenhum token válido obtido nesta sessão."
      ))
    }
    
    div(
      class = "table-status available",
      tags$strong("Token obtido com sucesso."),
      tags$p("As credenciais foram removidas dos campos de entrada e não foram armazenadas."),
      tags$p(paste0("Validade aproximada até: ", format(expires_at, "%Y-%m-%d %H:%M:%S"))),
      tags$details(
        tags$summary("Mostrar token"),
        tags$code(token)
      )
    )
  }
  
  output$fluviometric_api_token_status <- renderUI({
    render_ana_api_token_status()
  })
  
  output$pluviometric_api_token_status <- renderUI({
    render_ana_api_token_status()
  })
  
  observeEvent(input$fluviometric_api_get_token, {
    tryCatch({
      token_info <- ana_api_authenticate_session(
        identificador = input$fluviometric_api_identificador,
        senha = input$fluviometric_api_senha
      )
      
      ana_api_token(token_info$token)
      ana_api_token_created_at(token_info$created_at)
      ana_api_token_expires_at(token_info$expires_at)
      ana_api_token_message("Token obtido com sucesso.")
      
      updateTextInput(session, "fluviometric_api_identificador", value = "")
      updateTextInput(session, "fluviometric_api_senha", value = "")
      
      showNotification(
        "Token obtido com sucesso. As credenciais foram removidas dos campos e não foram armazenadas.",
        type = "message"
      )
    }, error = function(e) {
      showNotification(clean_error_message(conditionMessage(e)), type = "error", duration = 8)
    })
  }, ignoreInit = TRUE)
  
  observeEvent(input$pluviometric_api_get_token, {
    tryCatch({
      token_info <- ana_api_authenticate_session(
        identificador = input$pluviometric_api_identificador,
        senha = input$pluviometric_api_senha
      )
      
      ana_api_token(token_info$token)
      ana_api_token_created_at(token_info$created_at)
      ana_api_token_expires_at(token_info$expires_at)
      ana_api_token_message("Token obtido com sucesso.")
      
      updateTextInput(session, "pluviometric_api_identificador", value = "")
      updateTextInput(session, "pluviometric_api_senha", value = "")
      
      showNotification(
        "Token obtido com sucesso. As credenciais foram removidas dos campos e não foram armazenadas.",
        type = "message"
      )
    }, error = function(e) {
      showNotification(clean_error_message(conditionMessage(e)), type = "error", duration = 8)
    })
  }, ignoreInit = TRUE)
  
  ana_inventory_start_year <- function(module) {
    station <- selected_station()
    
    get_year <- function(field) {
      if (!field %in% names(station)) {
        return(NA_integer_)
      }
      
      value <- suppressWarnings(as.Date(station[[field]][1]))
      
      if (is.na(value)) {
        return(NA_integer_)
      }
      
      as.integer(format(value, "%Y"))
    }
    
    if (identical(module, "flu")) {
      years <- c(
        get_year("discharge_start_date"),
        get_year("stage_start_date")
      )
      years <- years[!is.na(years)]
      
      if (length(years) == 0) {
        return(NA_integer_)
      }
      
      return(min(years))
    }
    
    get_year("rainfall_start_date")
  }
  
  ana_requested_period <- function(module) {
    current_year <- as.integer(format(Sys.Date(), "%Y"))
    
    if (identical(module, "flu")) {
      mode <- input$fluviometric_api_period_mode
      
      if (identical(mode, "manual")) {
        start_year <- as.integer(input$fluviometric_api_start_year)
        end_year <- as.integer(input$fluviometric_api_end_year)
      } else {
        start_year <- ana_inventory_start_year("flu")
        end_year <- current_year
      }
    } else {
      mode <- input$pluviometric_api_period_mode
      
      if (identical(mode, "manual")) {
        start_year <- as.integer(input$pluviometric_api_start_year)
        end_year <- as.integer(input$pluviometric_api_end_year)
      } else {
        start_year <- ana_inventory_start_year("plu")
        end_year <- current_year
      }
    }
    
    if (is.na(start_year) || is.na(end_year)) {
      stop("Não foi possível definir o período pelo inventário. Informe ano inicial e final manualmente.", call. = FALSE)
    }
    
    if (start_year > end_year) {
      stop("O ano inicial não pode ser maior que o ano final.", call. = FALSE)
    }
    
    if (end_year > current_year) {
      stop("O ano final não pode ser maior que o ano atual.", call. = FALSE)
    }
    
    list(start_year = start_year, end_year = end_year)
  }
  
  ana_prepare_or_resume_state <- function(module) {
    state <- ana_download_state()
    
    if (
      !is.null(state) &&
      identical(state$status, "paused_auth") &&
      identical(state$module, module) &&
      identical(as.character(state$station_code), selected_code())
    ) {
      state$status <- "running"
      ana_download_state(state)
      return(state)
    }
    
    period <- ana_requested_period(module)
    
    tasks <- ana_api_make_year_tasks(
      module = module,
      station_code = selected_code(),
      start_year = period$start_year,
      end_year = period$end_year
    )
    
    state <- list(
      module = module,
      station_code = selected_code(),
      start_year = period$start_year,
      end_year = period$end_year,
      tasks = tasks,
      pending_tasks = tasks,
      partial_data = list(),
      download_report = tibble::tibble(),
      status = "running",
      paused_message = NA_character_
    )
    
    ana_download_state(state)
    state
  }
  
  ana_download_report_for_module <- function(module) {
    state <- ana_download_state()
    
    if (is.null(state) || !identical(state$module, module)) {
      return(tibble::tibble())
    }
    
    state$download_report
  }
  
  render_ana_download_status <- function(module) {
    state <- ana_download_state()
    
    if (is.null(state) || !identical(state$module, module)) {
      return(div(
        class = "table-status empty",
        "Nenhum download pela API ANA iniciado para este módulo."
      ))
    }
    
    report <- state$download_report
    total_tasks <- nrow(state$tasks)
    pending_tasks <- nrow(state$pending_tasks)
    completed_tasks <- total_tasks - pending_tasks
    
    if (identical(state$status, "paused_auth")) {
      return(div(
        class = "table-status warning",
        tags$strong("Download pausado por autorização/token."),
        tags$p(state$paused_message),
        tags$p(
          paste0(
            "Período original: ",
            state$start_year,
            "–",
            state$end_year,
            ". Tarefas concluídas: ",
            completed_tasks,
            " de ",
            total_tasks,
            "."
          )
        ),
        tags$p("Obtenha novo token e clique novamente em Processar dados para retomar do ponto em que parou.")
      ))
    }
    
    if (identical(state$status, "completed")) {
      n_success <- sum(report$status == "success", na.rm = TRUE)
      n_empty <- sum(report$status == "empty", na.rm = TRUE)
      n_failed <- sum(report$status == "failed_after_3_attempts", na.rm = TRUE)
      
      report_button_id <- if (identical(module, "flu")) {
        "fluviometric_api_download_report_download"
      } else {
        "pluviometric_api_download_report_download"
      }
      
      return(
        div(
          class = if (n_failed > 0) "table-status warning" else "table-status available",
          div(
            style = "display: flex; justify-content: space-between; align-items: center; gap: 12px;",
            
            div(
              style = "min-width: 0;",
              tags$strong("Download pela API ANA concluído."),
              tags$p(
                style = "margin: 4px 0 0 0;",
                paste0(
                  "Período: ",
                  state$start_year,
                  "–",
                  state$end_year,
                  ". Sucesso: ",
                  n_success,
                  "; sem registros: ",
                  n_empty,
                  "; falhas após 3 tentativas: ",
                  n_failed,
                  "."
                )
              )
            ),
            
            div(
              style = "flex: 0 0 auto;",
              downloadButton(
                outputId = report_button_id,
                label = "Relatório",
                class = "btn-primary",
                style = "padding: 6px 12px; font-size: 12px; border-radius: 8px; min-width: 110px;"
              )
            )
          )
        )
      )
    }
    
    div(
      class = "table-status empty",
      paste0("Download em preparação. Tarefas concluídas: ", completed_tasks, " de ", total_tasks, ".")
    )
  }
  
  output$fluviometric_api_download_status <- renderUI({
    render_ana_download_status("flu")
  })
  
  output$pluviometric_api_download_status <- renderUI({
    render_ana_download_status("plu")
  })
  
  ana_run_download_loop <- function(module) {
    if (!ana_api_token_is_valid()) {
      stop("Obtenha um token válido antes de processar os dados pela API ANA.", call. = FALSE)
    }
    
    state <- ana_prepare_or_resume_state(module)
    
    total_tasks <- nrow(state$tasks)
    
    while (nrow(state$pending_tasks) > 0) {
      task <- state$pending_tasks[1, , drop = FALSE]
      done_before <- total_tasks - nrow(state$pending_tasks)
      
      incProgress(
        amount = 1 / max(total_tasks, 1),
        detail = paste0(
          "Ano ",
          task$year,
          " — ",
          task$route_name,
          " (",
          done_before + 1,
          " de ",
          total_tasks,
          ")"
        )
      )
      
      task_result <- ana_api_download_task_with_retries(
        task = task,
        token = ana_api_token(),
        max_attempts = app_config$ana_api_max_attempts_per_task
      )
      
      if (identical(task_result$status, "paused_auth")) {
        state$status <- "paused_auth"
        state$paused_message <- task_result$report$message[[1]]
        state$download_report <- dplyr::bind_rows(state$download_report, task_result$report)
        ana_download_state(state)
        
        return(list(status = "paused_auth", result = NULL))
      }
      
      state$pending_tasks <- state$pending_tasks[-1, , drop = FALSE]
      state$download_report <- dplyr::bind_rows(state$download_report, task_result$report)
      
      if (identical(task_result$status, "success") && nrow(task_result$data) > 0) {
        state$partial_data[[length(state$partial_data) + 1L]] <- task_result$data
      }
      
      ana_download_state(state)
    }
    
    raw_data <- dplyr::bind_rows(state$partial_data)
    report <- state$download_report
    
    if (nrow(raw_data) == 0) {
      state$status <- "completed"
      ana_download_state(state)
      stop(
        "O download foi concluído, mas nenhuma série diária foi retornada. Consulte o relatório de download.",
        call. = FALSE
      )
    }
    
    if (identical(module, "flu")) {
      result <- build_fluviometric_result_from_ana_api(raw_data, report)
    } else {
      result <- build_pluviometric_result_from_ana_api(raw_data, report)
    }
    
    state$status <- "completed"
    ana_download_state(state)
    
    list(status = "completed", result = result)
  }
  

